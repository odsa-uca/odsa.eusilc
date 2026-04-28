#' Title
#'
#' @param .datos Conjunto de datos P a imputar.
#' @param .variables Grupo de variables a imputar.
#' @param ... ...
#'
#' @returns Conjunto de datos P con variables imputadas.
#' @export
imputar_personas <- function(
  .datos,
  .variables,
  ...
) {
  # Chequeos args ------------------------------------------------------------

  # Construccion flags -------------------------------------------------------
  # Numero negativo indica grupo a imputar, mismo numero positivo indica grupo
  # de referencia para entrenamiento
  .datos <- dplyr::mutate(
    .datos,
    .f_maa = dplyr::case_when(
      PY010N != 0 & (is.na(PL073 + PL074) | PL073 + PL074 == 0) ~ -1,
      PY010N != 0 & !is.na(PL073 + PL074) & PL073 + PL074 != 0 ~ 1,
      .default = 0
    ),
    .f_man = dplyr::case_when(
      PY050N != 0 & (is.na(PL075 + PL076) | PL075 + PL076 == 0) ~ -1,
      PY050N != 0 & !is.na(PL075 + PL076) & PL075 + PL076 != 0 ~ 1,
      .default = 0
    ),
    .f_PL060 = dplyr::if_else(PL060_F %in% c(-1, 1), PL060_F, 0),
    .f_PL040A = dplyr::if_else(PL040A_F %in% c(-1, 1), PL040A_F, 0),
    .f_PL051A = dplyr::if_else(PL051A_F %in% c(-1, 1), PL051A_F, 0),
    .f_PL111A = dplyr::if_else(PL111A_F %in% c(-1, 1), PL111A_F, 0),
    .f_PL040B = dplyr::case_when(
      PY010N + PY050N != 0 & (PL032 != 1 | is.na(PL032)) & PL040B_F %in% c(-1, -2) ~ -1,
      PY010N + PY050N != 0 & PL040B_F == 1 ~ 1,
      .default = 0
    ),
    .f_PL051B = dplyr::case_when(
      PY010N + PY050N != 0 & (PL032 != 1 | is.na(PL032)) & PL051B_F %in% c(-1, -2) ~ -1,
      PY010N + PY050N != 0 & PL051B_F == 1 ~ 1,
      .default = 0
    ),
    .f_PL111B = dplyr::case_when(
      PY010N + PY050N != 0 & (PL032 != 1 | is.na(PL032)) & PL111B_F %in% c(-1, -2) ~ -1,
      PY010N + PY050N != 0 & PL111B_F == 1 ~ 1,
      .default = 0
    ),
    .f_PL130 = dplyr::case_when(
      PL130 == 14 ~ -1,
      PL130 < 10 ~ 1,
      PL130 == 15 ~ -2,
      PL130 >= 10 ~ 2,
      .default = 0
    ),
    .f_PL230 = dplyr::case_when(
      PL032 == 1 & PL040A == 3 & PL230 == 99 ~ -1,
      PL230_F %in% c(-1, 1) ~ PL230_F,
      .default = 0
    )
  )

  # Construccion vbles -------------------------------------------------------
  .datos <- dplyr::mutate(
    .datos,
    maa = dplyr::case_when(
      .f_maa == -1 ~ NA_integer_,
      .default = PL073 + PL074
    ),
    man = dplyr::case_when(
      .f_man == -1 ~ NA_integer_,
      .default = PL075 + PL076
    )
  )

  # Imputacion ---------------------------------------------------------------
  # Random Forests para lidiar con la distribución atípica de los meses
  # TODO: Selección de hiperparámetros
  # Meses asalariados ------------------------
  datos_imp_maa <- .datos |>
    dplyr::select(PB010, PB020, PB030, PY010N, PY050N, PB140, PB150, PE041, maa, .f_maa) |>
    dplyr::filter(.f_maa %in% c(-1, 1))

  imp_maa <- missRanger::missRanger(
    data = datos_imp_maa,
    formula = maa + PE041 ~ PY010N + PY050N + PB140 + PB150 + PE041,
    num.trees = 100,
    pmm.k = 10
  )

  # Meses no asalariados ---------------------
  datos_imp_man <- .datos |>
    dplyr::select(PB010, PB020, PB030, PY010N, PY050N, PB140, PB150, PE041, man, .f_man) |>
    dplyr::filter(.f_man %in% c(-1, 1))

  imp_man <- missRanger::missRanger(
    data = datos_imp_man,
    formula = man + PE041 ~ PY010N + PY050N + PB140 + PB150 + PE041,
    num.trees = 100,
    pmm.k = 10
  )

  # Horas habituales semanales ---------------
  datos_imp_PL060 <- .datos |>
    dplyr::select(PB010, PB020, PB030, PY010N, PY050N, maa, man, PB140, PB150, PL060, .f_PL060) |>
    dplyr::filter(.f_PL060 %in% c(-1, 1))

  imp_PL060 <- missRanger::missRanger(
    data = datos_imp_PL060,
    formula = PL060 ~ PY010N + PY050N + PB140 + PB150 + maa + man,
    num.trees = 100,
    pmm.k = 10
  )

  # Categoria, ocupacion y rama A ------------
  datos_imp_corA <- .datos |>
    dplyr::select(PB010, PB020, PB030, PL040A, PL051A, PL111A, PY010N, PY050N,
                  PB140, PB150, PE041, .f_PL040A, .f_PL051A, .f_PL111A) |>
    dplyr::filter(.f_PL040A %in% c(-1, 1)) |>
    dplyr::mutate(PL040A = factor(PL040A), PL051A = factor(PL051A), PE041 = factor(PE041))

  imp_corA <- missRanger::missRanger(
    data = datos_imp_corA,
    formula = PL040A + PL051A + PL111A + PE041 ~ PY010N + PY050N + PB140 + PB150 + PL040A + PL051A + PL111A + PE041,
    num.trees = 100,
    pmm.k = 10
  )

  # Categoria, ocupacion y rama B ------------
  datos_imp_corB <- .datos |>
    dplyr::select(PB010, PB020, PB030, PL040B, PL051B, PL111B, PY010N, PY050N,
                  PB140, PB150, PE041, .f_PL040B, .f_PL051B, .f_PL111B) |>
    dplyr::filter(.f_PL040B %in% c(-1, 1)) |>
    dplyr::mutate(PL040B = factor(PL040B), PL051B = factor(PL051B), PE041 = factor(PE041))

  imp_corB <- missRanger::missRanger(
    data = datos_imp_corB,
    formula = PL040B + PL051B + PL111B + PE041 ~ PY010N + PY050N + PB140 + PB150 + PL040B + PL051B + PL111B + PE041,
    num.trees = 100,
    pmm.k = 10
  )

  # Tamaño del establecimiento ---------------

  # Sector publico-privado -------------------

  # Datos finales ------------------------------------------------------------
  imps <- list(
    imp_maa = imp_maa |>
      dplyr::filter(.f_maa == -1) |>
      dplyr::select(PB010, PB020, PB030, maa),
    imp_man = imp_man |>
      dplyr::filter(.f_man == -1) |>
      dplyr::select(PB010, PB020, PB030, man),
    imp_PL060 = imp_PL060 |>
      dplyr::filter(.f_PL060 == -1) |>
      dplyr::select(PB010, PB020, PB030, PL060),
    imp_PL040A = imp_corA |>
      dplyr::filter(.f_PL040A == -1) |>
      dplyr::select(PB010, PB020, PB030, PL040A) |>
      dplyr::mutate(PL040A = as.numeric(as.character(PL040A))),
    imp_PL051A = imp_corA |>
      dplyr::filter(.f_PL051A == -1) |>
      dplyr::select(PB010, PB020, PB030, PL051A) |>
      dplyr::mutate(PL051A = as.numeric(as.character(PL051A))),
    imp_PL111A = imp_corA |>
      dplyr::filter(.f_PL111A == -1) |>
      dplyr::select(PB010, PB020, PB030, PL111A),
    imp_PL040B = imp_corB |>
      dplyr::filter(.f_PL040B == -1) |>
      dplyr::select(PB010, PB020, PB030, PL040B) |>
      dplyr::mutate(PL040B = as.numeric(as.character(PL040B))),
    imp_PL051B = imp_corB |>
      dplyr::filter(.f_PL051B == -1) |>
      dplyr::select(PB010, PB020, PB030, PL051B) |>
      dplyr::mutate(PL051B = as.numeric(as.character(PL051B))),
    imp_PL111B = imp_corB |>
      dplyr::filter(.f_PL111B == -1) |>
      dplyr::select(PB010, PB020, PB030, PL111B)
  )

  for (.imp in imps) {
    .datos <- dplyr::left_join(
      x = .datos,
      y = .imp,
      by = dplyr::join_by(PB010, PB020, PB030),
      suffix = c("", "_imp")
    )
  }

  # Devolver -----------------------------------------------------------------
  attr(.datos, "imputada") <- TRUE

  return(.datos)
}
