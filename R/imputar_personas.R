#' Title
#'
#' @param .datos Conjunto de datos P a imputar.
#' @param .anio anio
#' @param .lmh lmh
#' @param ... ...
#'
#' @returns Conjunto de datos P con variables imputadas.
#' @export
imputar_personas <- function(
  .datos,
  .anio,
  .lmh,
  ...
) {
  # TODO: Reorganizar esta función!! Es una monstruosidad!!
  # TODO: Chequear cuánto hay que imputar e incorporar al control de flujos
  # Construccion flags -------------------------------------------------------
  # Numero negativo indica grupo a imputar, mismo numero positivo indica grupo
  # de referencia para entrenamiento
  .datos <- calc_flags_imputacion(.datos, .anio, .lmh)

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

  if (.lmh) {
    .datos <- dplyr::mutate(
      .datos,
      PL130_ = dplyr::if_else(PL130 %in% 14:15, NA_integer_, PL130)
    )
  }

  # Imputacion ---------------------------------------------------------------
  # Random Forests para lidiar con la distribución atípica de los meses
  # TODO: Selección de hiperparámetros
  # Meses asalariados ------------------------
  datos_imp_maa <- armar_imputables(
    .datos,
    .imputadas   = "maa",
    .predictoras = c("PY010N", "PY050N", "PB140", "PB150", "PE041"),
    .flag        = ".f_maa"
  )

  imp_maa <- missRanger::missRanger(
    data = datos_imp_maa,
    formula = maa + PE041 ~ PY010N + PY050N + PB140 + PB150 + PE041,
    num.trees = 100,
    pmm.k = 10
  )

  imp_maa <- imp_maa |>
    dplyr::filter(.f_maa == -1) |>
    dplyr::select(PB010, PB020, PB030, maa)

  # Meses no asalariados ---------------------
  datos_imp_man <- armar_imputables(
    .datos,
    .imputadas   = "man",
    .predictoras = c("PY010N", "PY050N", "PB140", "PB150", "PE041"),
    .flag        = ".f_man"
  )

  imp_man <- missRanger::missRanger(
    data = datos_imp_man,
    formula = man + PE041 ~ PY010N + PY050N + PB140 + PB150 + PE041,
    num.trees = 100,
    pmm.k = 10
  )

  imp_man <- imp_man |>
    dplyr::filter(.f_man == -1) |>
    dplyr::select(PB010, PB020, PB030, man)

  # Horas habituales semanales ---------------
  datos_imp_PL060 <- armar_imputables(
    .datos,
    .imputadas   = "PL060",
    .predictoras = c("PY010N", "PY050N", "PB140", "PB150", "maa", "man"),
    .flag        = ".f_PL060"
  )

  imp_PL060 <- missRanger::missRanger(
    data = datos_imp_PL060,
    formula = PL060 ~ PY010N + PY050N + PB140 + PB150 + maa + man,
    num.trees = 100,
    pmm.k = 10
  )

  imp_PL060 <- imp_PL060 |>
    dplyr::filter(.f_PL060 == -1) |>
    dplyr::select(PB010, PB020, PB030, PL060)

  # Categoria, ocupacion y rama A ------------
  datos_imp_corA <- armar_imputables(
    .datos,
    .imputadas   = c("PL040A", "PL051A", "PL111A"),
    .predictoras = c("PY010N", "PY050N", "PB140", "PB150", "PE041"),
    .flags       = c(".f_PL040A", ".f_PL051A", ".f_PL111A"),
    .factores    = c("PL040A", "PL051A", "PE041")
  )

  imp_corA <- missRanger::missRanger(
    data = datos_imp_corA,
    formula = PL040A + PL051A + PL111A + PE041 ~ PY010N + PY050N + PB140 + PB150 + PL040A + PL051A + PL111A + PE041,
    num.trees = 100,
    pmm.k = 10
  )

  imp_PL040A <- imp_corA |>
    dplyr::filter(.f_PL040A == -1) |>
    dplyr::select(PB010, PB020, PB030, PL040A) |>
    dplyr::mutate(PL040A = as.numeric(as.character(PL040A)))
  imp_PL051A <- imp_corA |>
    dplyr::filter(.f_PL051A == -1) |>
    dplyr::select(PB010, PB020, PB030, PL051A) |>
    dplyr::mutate(PL051A = as.numeric(as.character(PL051A)))
  imp_PL111A <- imp_corA |>
    dplyr::filter(.f_PL111A == -1) |>
    dplyr::select(PB010, PB020, PB030, PL111A)

  # Categoria, ocupacion y rama B ------------
  if (.anio < 2021) {
    datos_imp_corB <- armar_imputables(
      .datos,
      .imputadas   = c("PL040B", "PL051B"),
      .predictoras = c("PY010N", "PY050N", "PB140", "PB150", "PE041"),
      .flags       = c(".f_PL040B", ".f_PL051B"),
      .factores    = c("PL040B", "PL051B", "PE041")
    )

    imp_corB <- missRanger::missRanger(
      data = datos_imp_corB,
      formula = PL040B + PL051B + PE041 ~ PY010N + PY050N + PB140 + PB150 + PL040B + PL051B + PE041,
      num.trees = 100,
      pmm.k = 10
    )
  } else {
    datos_imp_corB <- armar_imputables(
      .datos,
      .imputadas   = c("PL040B", "PL051B", "PL111B"),
      .predictoras = c("PY010N", "PY050N", "PB140", "PB150", "PE041"),
      .flags       = c(".f_PL040B", ".f_PL051B", ".f_PL111B"),
      .factores    = c("PL040B", "PL051B", "PE041")
    )

    imp_corB <- missRanger::missRanger(
      data = datos_imp_corB,
      formula = PL040B + PL051B + PL111B + PE041 ~ PY010N + PY050N + PB140 + PB150 + PL040B + PL051B + PL111B + PE041,
      num.trees = 100,
      pmm.k = 10
    )

    imp_PL111B <- imp_corB |>
      dplyr::filter(.f_PL111B == -1) |>
      dplyr::select(PB010, PB020, PB030, PL111B)
  }

  imp_PL040B <- imp_corB |>
    dplyr::filter(.f_PL040B == -1) |>
    dplyr::select(PB010, PB020, PB030, PL040B) |>
    dplyr::mutate(PL040B = as.numeric(as.character(PL040B)))
  imp_PL051B <- imp_corB |>
    dplyr::filter(.f_PL051B == -1) |>
    dplyr::select(PB010, PB020, PB030, PL051B) |>
    dplyr::mutate(PL051B = as.numeric(as.character(PL051B)))

  if (.anio < 2021 | .lmh) {
    # Tamaño del establecimiento ---------------
    datos_imp_PL130a <- armar_imputables(
      .datos,
      .imputadas   = c("PL130_", "PE041", "PL111A"),
      .predictoras = c("PY010N", "PY050N", "PB140", "PB150", "PE041", "PL111A"),
      .flags       = ".fa_PL130"
    )
    datos_imp_PL130b <- armar_imputables(
      .datos,
      .imputadas   = c("PL130_", "PE041", "PL111A"),
      .predictoras = c("PY010N", "PY050N", "PB140", "PB150", "PE041", "PL111A"),
      .flags       = ".fb_PL130",
      .factores    = c("PL130_", "PE041")
    )
    datos_imp_PL130c <- armar_imputables(
      .datos,
      .imputadas   = c("PL130", "PE041", "PL111A"),
      .predictoras = c("PY010N", "PY050N", "PB140", "PB150", "PE041", "PL111A"),
      .flags       = ".fc_PL130",
      .factores    = c("PL130", "PE041")
    )

    imp_PL130a <- missRanger::missRanger(
      data = datos_imp_PL130a,
      formula = PL130_ + PE041 + PL111A ~ PY010N + PY050N + PB140 + PB150 + PE041 + PL111A,
      num.trees = 100,
      pmm.k = 10
    )
    imp_PL130b <- missRanger::missRanger(
      data = datos_imp_PL130b,
      formula = PL130_ + PE041 + PL111A ~ PY010N + PY050N + PB140 + PB150 + PE041 + PL111A,
      num.trees = 100,
      pmm.k = 10
    )
    imp_PL130c <- missRanger::missRanger(
      data = datos_imp_PL130c,
      formula = PL130 + PE041 + PL111A ~ PY010N + PY050N + PB140 + PB150 + PE041 + PL111A,
      num.trees = 100,
      pmm.k = 10
    )

    imp_PL130 <- dplyr::bind_rows(
      imp_PL130a |>
        dplyr::filter(.fa_PL130 == -1) |>
        dplyr::select(PB010, PB020, PB030, PL130_) |>
        dplyr::rename(PL130 = PL130_),
      imp_PL130b |>
        dplyr::filter(.fb_PL130 == -1) |>
        dplyr::select(PB010, PB020, PB030, PL130_) |>
        dplyr::mutate(PL130 = as.numeric(as.character(PL130_))),
      imp_PL130c |>
        dplyr::filter(.fc_PL130 == -1) |>
        dplyr::select(PB010, PB020, PB030, PL130) |>
        dplyr::mutate(PL130 = as.numeric(as.character(PL130))),
    )
  }

  if (.lmh) {
    # Sector publico-privado -------------------
    datos_imp_PL230 <- armar_imputables(
      .datos,
      .imputadas   = c("PL230", "PE041"),
      .predictoras = c("PY010N", "PY050N", "PB140", "PB150", "PE041"),
      .flag        = ".f_PL230",
      .factores    = c("PL230", "PE041")
    )

    imp_PL230 <- missRanger::missRanger(
      data = datos_imp_PL230,
      formula = PL230 + PE041 ~ PY010N + PY050N + PB140 + PB150 + PE041,
      num.trees = 100,
      pmm.k = 10
    )

    imp_PL230 <- imp_PL230 |>
      dplyr::filter(.f_PL230 == -1) |>
      dplyr::select(PB010, PB020, PB030, PL230) |>
      dplyr::mutate(PL230 = as.numeric(as.character(PL230)))

  }

  # Datos finales ------------------------------------------------------------
  imps <- list(
    imp_maa,
    imp_man,
    imp_PL060,
    imp_PL040A,
    imp_PL051A,
    imp_PL111A,
    imp_PL040B,
    imp_PL051B
  )

  if (.anio >= 2021) imps <- c(imps, list(imp_PL111B))

  if (.anio < 2021 | .lmh) imps <- c(imps, list(imp_PL130))

  if (.lmh) imps <- c(imps, list(imp_PL230))

  for (.imp in imps) {
    .datos <- dplyr::left_join(
      x = .datos,
      y = .imp,
      by = dplyr::join_by(PB010, PB020, PB030),
      suffix = c("", "_imp")
    )
  }

  .datos <- aplicar_imputaciones(.datos, .anio, .lmh)

  # Devolver -----------------------------------------------------------------
  attr(.datos, "imputada") <- TRUE

  return(.datos)
}

# ============================================================================
#' Title
#'
#' @param .datos .datos
#' @param .anio .anio
#' @param .lmh lmh
#' @param ... ...
#'
#' @returns .datos con flags de imputacion
calc_flags_imputacion <- function(.datos, .anio, .lmh, ...) {
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
    )
  )

  if (.anio >= 2021) {
    .datos <- dplyr::mutate(
      .datos,
      .f_PL111B = dplyr::case_when(
        PY010N + PY050N != 0 & (PL032 != 1 | is.na(PL032)) & PL111B_F %in% c(-1, -2) ~ -1,
        PY010N + PY050N != 0 & PL111B_F == 1 ~ 1,
        .default = 0
      )
    )
  }

  if (.anio < 2021 | .lmh) {
    .datos <- dplyr::mutate(
      .datos,
      .fa_PL130 = dplyr::case_when(
        PL130 == 14 ~ -1,
        PL130 < 10 ~ 1,
        .default = 0
      ),
      .fb_PL130 = dplyr::case_when(
        PL130 == 15 ~ -1,
        PL130 %in% 10:13 ~ 1,
        .default = 0
      ),
      .fc_PL130 = dplyr::case_when(
        PL130_F == -1 ~ -1,
        PL130_F == 1 & PL130 != 14 & PL130 != 15 ~ 1,
        .default = 0
      )
    )
  }

  if (.lmh) {
    .datos <- dplyr::mutate(
      .datos,
      .f_PL230 = dplyr::case_when(
        PL032 == 1 & PL040A == 3 & PL230 == 99 ~ -1,
        PL230_F %in% c(-1, 1) ~ PL230_F,
        .default = 0
      )
    )
  }

  return(.datos)
}

# ============================================================================
#' Title
#'
#' @param .datos .datos
#' @param .imputadas .imputadas
#' @param .predictoras .predictoras
#' @param .flags .flags
#' @param .factores .factores
#'
#' @returns conjunto de datos para imputar
armar_imputables <- function(
  .datos,
  .imputadas,
  .predictoras,
  .flags,
  .factores = NULL
) {
  datos_imp <- .datos |>
    dplyr::select(dplyr::all_of(c("PB010", "PB020", "PB030", .predictoras, .imputadas, .flags))) |>
    dplyr::filter(!!rlang::sym(.flags[1]) %in% c(-1, 1)) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(.factores), factor))

  return(datos_imp)
}

# ============================================================================
#' Title
#'
#' @param .datos .datos
#' @param .anio .anio
#' @param .lmh .lmh
#'
#' @returns Conjunto de datos estandarizado con imputaciones aplicadas
aplicar_imputaciones <- function(.datos, .anio, .lmh) {
  .datos <- dplyr::mutate(
    .datos,
    maa = dplyr::if_else(.f_maa == -1, maa_imp, maa),
    man = dplyr::if_else(.f_man == -1, man_imp, man),
    PL060 = dplyr::if_else(.f_PL060 == -1, PL060_imp, PL060),
    PL040A = dplyr::if_else(.f_PL040A == -1, PL040A_imp, PL040A),
    PL051A = dplyr::if_else(.f_PL051A == -1, PL051A_imp, PL051A),
    PL111A = dplyr::if_else(.f_PL111A == -1, PL111A_imp, PL111A),
    PL040B = dplyr::if_else(.f_PL040B == -1, PL040B_imp, PL040B),
    PL051B = dplyr::if_else(.f_PL051B == -1, PL051B_imp, PL051B),
  )

  if (.anio >= 2021) {
    .datos <- dplyr::mutate(
      .datos,
      PL111B = dplyr::if_else(.f_PL111B, PL111B_imp, PL111B)
    )
  }

  if (.anio < 2021 | .lmh) {
    # TODO: PL130 tiene tres flags ...
    .datos <- dplyr::mutate(
      .datos,
      PL130 = dplyr::if_else(!is.na(PL130_imp), PL130_imp, PL130),
    )
  }

  if (.lmh) {
    .datos <- dplyr::mutate(
      .datos,
      PL230 = dplyr::if_else(.f_PL230 == -1, PL230_imp, PL230),
    )
  }

  return(.datos)
}
