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
    .f_PL060 = PL060_F,
    .f_PL040A = PL040A_F,
    .f_PL040B = dplyr::case_when(
      PL040B_F == -1 ~ -1,
      PY010N + PY050N != 0 & (PL032 != 1 | is.na(PL032)) & PL040B_F == -2 ~ -1,
      PY010N + PY050N != 0 & PL040B_F == 1 ~ 1,
      .default = 0
    ),
    .f_PL051A = PL051A_F,
    .f_PL051B = dplyr::case_when(
      PL051B_F == -1 ~ -1,
      PY010N + PY050N != 0 & (PL032 != 1 | is.na(PL032)) & PL051B_F == -2 ~ -1,
      PY010N + PY050N != 0 & PL051B_F == 1 ~ 1,
      .default = 0
    ),
    .f_PL111A = PL111A_F,
    .f_PL111B = dplyr::case_when(
      PL111B_F == -1 ~ -1,
      PY010N + PY050N != 0 & (PL032 != 1 | is.na(PL032)) & PL111B_F == -2 ~ -1,
      PY010N + PY050N != 0 & PL111B_F == 1 ~ 1,
      .default = 0
    ),
    .f_PL130 = dplyr::case_when(
      PL130 == 14 ~ -1,
      PL130 < 10 ~ 1,
      PL130 == 15 ~ -2,
      PL130 >= 10 ~ 2,
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
    formula = maa ~ PY010N + PY050N + PB140 + PB150 + PE041,
    num.trees = 100,
    pmm.k = 10
  )

  # Meses no asalariados ---------------------
  datos_imp_man <- .datos |>
    dplyr::select(PB010, PB020, PB030, PY010N, PY050N, PB140, PB150, PE041, man, .f_man) |>
    dplyr::filter(.f_man %in% c(-1, 1))

  imp_man <- missRanger::missRanger(
    data = datos_imp_man,
    formula = man ~ PY010N + PY050N + PB140 + PB150 + PE041,
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

  # Categoria ocupacional --------------------
  datos_imp_PL040A <- .datos |>
    dplyr::select(PB010, PB020, PB030, PY010N, PY050N, PL040A, .f_PL040A) |>
    dplyr::filter(.f_PL040A %in% c(-1, 1))

  imp_PL040A <- missRanger::missRanger(
    data = datos_imp_PL040A,
    formula = PL040A ~ PY010N + PY050N,
    num.trees = 100,
    pmm.k = 10
  )

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
    imp_PL040A = imp_PL040A |>
      dplyr::filter(.f_PL040A == -1) |>
      dplyr::select(PB010, PB020, PB030, PL040A)
  )

  for (.imp in imps) {
    .datos <- .datos |>
      dplyr::left_join(
        .imp, by = dplyr::join_by(PB010, PB020, PB030), suffix = c("", "_imp")
      )
  }

  # Devolver -----------------------------------------------------------------
  attr(.datos, "Imputada") <- TRUE

  return(.datos)
}
