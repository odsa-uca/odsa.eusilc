#' Construir variables adicionales en los conjuntos de datos P de la EU-SILC
#'
#' @param .datos Conjunto de datos P de la EU-SILC.
#' @param ... ...
#' @param .D Conjunto de datos D de la EU-SILC.
#' @param .R Conjunto de datos R de la EU-SILC.
#' @param .expandir Conservar las variables originales en el conjunto de datos final o eliminarlas.
#'
#' @returns Conjunto de datos de la EU-SILC con variables adicionales de uso habitual.
#' @export
expandir_personas <- function(
    .datos,
    ...,
    .D = NULL,
    .R = NULL,
    .expandir = FALSE
) {
  # Chequeos args ------------------------------------------------------------
  errores <- NULL

  if (!is.data.frame(.datos)) {
    errores <- c(errores, "*" = "`.datos` debe ser un data.frame o tibble.")
  }
  if (!is.null(.D) & !is.data.frame(.D)) {
    errores <- c(errores, "*" = "`.D` debe ser un data.frame o tibble.")
  }
  if (!is.null(.R) & !is.data.frame(.R)) {
    errores <- c(errores, "*" = "`.R` debe ser un data.frame o tibble.")
  }
  if (!is.logical(.expandir)) {
    errores <- c(errores, "*" = "`.expandir` debe ser `TRUE` o `FALSE`.")
  }

  if(!is.null(errores)) rlang::abort(c("Problemas en los argumentos:", errores))

  # Estandarizacion ----------------------------------------------------------
  anio <- unique(.datos$PB010)
  lmh <- "PL230" %in% names(.datos)

  datos_estandar <- estandarizar_personas(.datos, anio, .D, .R, lmh)
  .datos <- datos_estandar$datos
  mensajes <- datos_estandar$mensajes

  # Lookup tables ------------------------------------------------------------
  .datos <- dplyr::left_join(x  = .datos,
                             y  = tabla_ppa,
                             by = dplyr::join_by(PB010, PB020))
  .datos <- dplyr::mutate(
    .datos,
    pd03  = dplyr::recode_values(
      PE041,
      from = tabla_pd03$PE041,
      to = tabla_pd03$pd03,
      default = NA_integer_
    ),
    pl02  = dplyr::recode_values(
      PL032,
      from = tabla_pl02$PL032,
      to = tabla_pl02$pl02,
      default = NA_integer_
    ),
    pl05a = dplyr::recode_values(
      PL111A,
      from = tabla_pl05$PL111,
      to = tabla_pl05$pl05,
      default = NA_integer_
    ),
    pl05b = dplyr::recode_values(
      PL111B,
      from = tabla_pl05$PL111,
      to = tabla_pl05$pl05,
      default = NA_integer_
    ),
    pl06a = dplyr::recode_values(
      PL130,
      from = tabla_pl06$PL130,
      to = tabla_pl06$pl06a,
      default = NA_integer_
    ),
    pl06b = dplyr::recode_values(
      PL130,
      from = tabla_pl06$PL130,
      to = tabla_pl06$pl06b,
      default = NA_integer_
    ),
    pl08a = dplyr::recode_values(
      PL051A,
      from = tabla_isco$PL051,
      to = tabla_isco$pl08a,
      default = NA_integer_
    ),
    pl08b = dplyr::recode_values(
      PL051B,
      from = tabla_isco$PL051,
      to = tabla_isco$pl08b,
      default = NA_integer_
    ),
  )

  # Arreglos imputaciones ----------------------------------------------------
  if (is.null(attr(.datos, "imputada"))) {
    .datos <- dplyr::mutate(
      .datos,
      maa = PL073 + PL074,
      man = PL075 + PL076
    )
  } else {
    .datos <- .datos |>
    dplyr::mutate(
      maa = dplyr::case_when(
        .f_maa == -1 ~ maa_imp,
        .default = maa
      ),
      man = dplyr::case_when(
        .f_man == -1 ~ man_imp,
        .default = man
      ),
      PL060 = dplyr::case_when(
        .f_PL060 == -1 ~ PL060_imp,
        .default = PL060,
      ),
      PL040A = dplyr::case_when(
        .f_PL040A == -1 ~ PL040A_imp,
        .default = PL040A
      )
    )
    mensajes <- c(mensajes, "i" = "El conjunto de datos fue imputado...")
  }

  # Calcular vbles -----------------------------------------------------------
  .datos <- calc_personas(.datos, .lmh = lmh)

  # Arreglos y devolver ------------------------------------------------------
  if (!.expandir) {
    .datos <- dplyr::select(.datos, dplyr::all_of(names(etq$P$variables)))
  } else {
    .datos <- dplyr::relocate(.datos, dplyr::all_of(names(etq$P$variables)))
  }

  attr(.datos, "base")       <- "P"
  attr(.datos, "pre. 2021")  <- anio < 2021
  attr(.datos, "vbles. D")   <- !is.null(.D)
  attr(.datos, "vbles. R")   <- !is.null(.R)
  attr(.datos, "vbles. LMH") <- lmh
  attr(.datos, "expandida")  <- .expandir
  attr(.datos, "imputada")   <- !is.null(attr(.datos, "imputada"))

  if (!is.null(mensajes)) rlang::warn(c("Ojo!", mensajes))

  return(.datos)
}

# ============================================================================
#' Title
#'
#' @param .datos .datos
#' @param .anio .anio
#' @param .D .D
#' @param .R .R
#' @param .lmh lmh
#'
#' @returns conjunto de datos estandarizado para [calc_personas()].
estandarizar_personas <- function(.datos, .anio, .D, .R, .lmh) {
  mensajes <- NULL

  if (.anio <= 2021) {
    .datos <- dplyr::mutate(
      .datos,
      RB080 = PB140,
      RB081 = PB010 - PB140 - 1,
      RB082 = PB110 - PB140 - (PB130 > PB100),
      RB280 = PB210,
      RB290 = PB220A,
      PE041 = PE040,
      PL032 = dplyr::case_when(
        PL031 %in% 1:4 ~ 1,
        PL031 %in% 5 ~ 2,
        PL031 %in% 6:11 ~ 3,
        .default = NA_integer_
      ),
      PL040A = PL040,
      PL051A = PL051,
      PL111A = PL111
    )
    mensajes <- c(mensajes, "i" = "La base es anterior a 2021.")
  } else if (is.null(.R)) {
    .datos <- dplyr::mutate(
      .datos,
      RB080 = PB010 - PX020 - 1,
      RB081 = PX020,
      RB082 = NA_integer_,
      RB280 = NA_integer_,
      RB290 = NA_integer_
    )
    mensajes <- c(mensajes, "i" = "No se proporciono el conjunto R. Se pierden: `pd01a`, `pd04`, `pd05`.")
  } else {
    .datos <- dplyr::left_join(
      x  = .datos,
      y  = dplyr::select(.R, RB010, RB020, RB030, RB080, RB081, RB082, RB280, RB290),
      by = dplyr::join_by(PB010 == RB010, PB020 == RB020, PB030 == RB030)
    )
  }

  if (is.null(.D)) {
    .datos <- dplyr::mutate(.datos, DB040 = NA_character_)
    mensajes <- c(mensajes, "i" = "No se proporciono el conjunto D. Se pierden: `pi03`.")
  } else {
    .datos <- dplyr::left_join(
      x  = .datos,
      y  = dplyr::select(.D, DB010, DB020, DB030, DB040),
      by = dplyr::join_by(PB010 == DB010, PB020 == DB020, PX030 == DB030)
    )
  }

  if (.anio < 2021 & !.lmh) {
    .datos <- dplyr::mutate(.datos, PL230 = NA_integer_)
    mensajes <- c(mensajes, "i" = "No se encontro `PL230`. Se pierden: `pl07`, `pl09a`, `pl09b`, `py13`, `py14`, `py15`.")
  } else if (!.lmh) {
    .datos <- dplyr::mutate(.datos, PL130 = NA_integer_, PL230 = NA_integer_)
    mensajes <- c(mensajes, "i" = "No se encontro `PL130` o `PL230`. Se pierden: `pl06a`, `pl06b`, `pl07`, `pl09a`, `pl09b`, `py13`, `py14`, `py15`.")
  }

  return(list(datos = .datos, mensajes = mensajes))
}

# ============================================================================
#' Construye variables en la base P de la EU-SILC.
#'
#' @param .datos Conjunto P de la EU-SILC.
#' @param .lmh Si el conjunto de datos tiene el modulo LMH.
#' @param ... ...
#'
#' @returns Conjunto de datos P de la EU-SILC con variables adicionales.
calc_personas <- function(
    .datos,
    .lmh = FALSE,
    ...
) {
  datos <- .datos |>
    dplyr::mutate(
      # Bloque I -----------------------
      pi01 = PB010,
      pi02 = PB020,
      pi03 = DB040,
      pi04 = PX030,
      pi05 = PB030,
      pi06 = PB040,
      # Bloque D -----------------------
      pd01a = RB082,
      pd01b = dplyr::if_else(!is.na(RB081), RB081, PB010 - RB080 - 1),
      pd01c = PB010 - agrupar_nac(PB010, RB080) - 1,
      pd02  = PB150,
      #pd03  = dplyr::recode_values(PE041, from = tabla_pd03$PE041,
      #                             to = tabla_pd03$pd03, default = NA_integer_),
      pd04  = dplyr::if_else(RB280 == pi02, 1, 2),
      pd05  = dplyr::if_else(RB290 == pi02, 1, 2),
      # Bloque L -----------------------
      pl01  = NA_integer_,
      #pl02  = dplyr::recode_values(PL032, from = tabla_pl02$PL032,
      #                             to = tabla_pl02$pl02, default = NA_integer_),
      pl03a = PL051A,
      pl03b = PL051A %/% 10,
      pl04  = PL040A,
      #pl05a = dplyr::recode_values(PL111A, from = tabla_pl05$PL111,
      #                             to = tabla_pl05$pl05, default = NA_integer_),
      #pl05b = dplyr::recode_values(PL111B, from = tabla_pl05$PL111,
      #                             to = tabla_pl05$pl05, default = NA_integer_),
      pl05c = dplyr::case_when(
        PL032 == 1 ~ pl05a,
        PL032 != 1 ~ pl05b,
        .default = NA_integer_
      ),
      #pl06a = calc_testablecimiento(PL130, "a", .lmh),
      #pl06b = calc_testablecimiento(PL130, "b", .lmh),
      pl07  = dplyr::if_else(PL230 != 99, PL230, NA_integer_),
      #pl08a = dplyr::recode_values(PL051A, from = tabla_isco$PL051,
      #                             to = tabla_isco$pl08a, default = NA_integer_),
      #pl08b = dplyr::recode_values(PL051B, from = tabla_isco$PL051,
      #                             to = tabla_isco$pl08b, default = NA_integer_),
      pl09a = calc_heterogeneidad(PL040A, PL032, pl05a, pl06b, pl07, pl08b, "a", .lmh),
      pl09b = calc_heterogeneidad(PL040A, PL032, pl05a, pl06b, pl07, pl08b, "b", .lmh),
      pl10  = calc_egp(PL051A, PL040A, PL150),
      pl11a = calc_informalidad(PL040A, PY030G, PY035G, "a"),
      pl11b = calc_informalidad(PL040A, PY030G, PY035G, "b"),
      # Bloque Y -----------------------
      py00 = PY010N + PY050N + PY090N + PY110N + PY120N + PY130N + PY140N + PY100N + PY080N,
      py10 = PY010N + PY050N,
      py11 = PY010N,
      py12 = PY050N,
      py13 = calc_y_sector(py10, pl09b, 1, .lmh),
      py14 = calc_y_sector(py10, pl09b, 2, .lmh),
      py15 = calc_y_sector(py10, pl09b, 3, .lmh),
      py20 = PY090N + PY110N + PY120N + PY130N + PY140N + PY100N + PY080N,
      py21 = PY100N + PY080N,
      py22 = PY100N,
      py23 = PY080N,
      py24 = PY090N,
      py25 = PY110N + PY120N + PY130N + PY140N,
      haa  = dplyr::if_else(pl02 == 1 & py11 != 0, maa * PL060 * 4.2, NA_real_),
      han  = dplyr::if_else(pl02 == 1 & py12 != 0, man * PL060 * 4.2, NA_real_),
      py11h = dplyr::if_else(py11 != 0, (py11 * PX010) / haa, 0),
      py12h = dplyr::if_else(py12 != 0, (py12 * PX010) / han, 0),
      dplyr::across(py00:py25, \(y) (y * PX010) / 12),
      dplyr::across(c(py00:py25, py11h, py12h), \(y) y / ppa, .names = "{.col}ppa"),
      .keep = "all"
    )

  # ------------------------------------------
  return(datos)
}
