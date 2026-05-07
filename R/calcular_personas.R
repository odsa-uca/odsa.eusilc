#' Title
#'
#' @param .datos .datos
#' @param .anio anio
#' @param .lmh .lmh
#' @param ... ...
#'
#' @returns .datos con varibles recodificadas
lookup_personas <- function(.datos, .anio, .lmh = FALSE, ...) {
  .datos <- dplyr::left_join(x  = .datos,
                             y  = tabla_ppa,
                             by = dplyr::join_by(PB010, PB020))
  .datos <- dplyr::mutate(
    .datos,
    pd03  = dplyr::recode_values(
      PE041,
      from = tabla_pd03$PE041,
      to   = tabla_pd03$pd03,
      default = NA_integer_
    ),
    pl01  = dplyr::recode_values(
      PL032,
      from = tabla_pl01$PL032,
      to   = tabla_pl01$pl01,
      default = NA_integer_
    ),
    pl12a = dplyr::recode_values(
      PL051A,
      from = tabla_isco$PL051,
      to   = tabla_isco$pl12,
      default = NA_integer_
    ),
    pl12b = dplyr::recode_values(
      PL051B,
      from = tabla_isco$PL051,
      to   = tabla_isco$pl12,
      default = NA_integer_
    ),
    pl13a = dplyr::recode_values(
      PL051A,
      from = tabla_isco$PL051,
      to   = tabla_isco$pl13,
      default = NA_integer_
    ),
    pl13b = dplyr::recode_values(
      PL051B,
      from = tabla_isco$PL051,
      to   = tabla_isco$pl13,
      default = NA_integer_
    ),
    pl20a = dplyr::recode_values(
      PL111A,
      from = tabla_pl20$PL111,
      to   = tabla_pl20$pl20,
      default = NA_integer_
    ),
    pl20b = dplyr::recode_values(
      PL111B,
      from = tabla_pl20$PL111,
      to   = tabla_pl20$pl20,
      default = NA_integer_
    ),
  )

  if (.lmh) {
    .datos <- dplyr::mutate(
      .datos,
      pl21a = dplyr::recode_values(
        PL130,
        from = tabla_pl21$PL130,
        to   = tabla_pl21$pl21a,
        default = NA_integer_
      ),
      pl21b = dplyr::recode_values(
        PL130,
        from = tabla_pl21$PL130,
        to   = tabla_pl21$pl21b,
        default = NA_integer_
      )
    )
  } else {
    .datos <- dplyr::mutate(.datos, pl21a = NA_integer_, pl21b = NA_integer_)
  }

  return(.datos)
}

# ============================================================================
#' Construye variables en la base P de la EU-SILC.
#'
#' @param .datos Conjunto P de la EU-SILC.
#' @param .lmh Si el conjunto de datos tiene el modulo LMH.
#' @param ... ...
#'
#' @returns Conjunto de datos P de la EU-SILC con variables adicionales.
calcular_personas <- function(
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
      pd04  = dplyr::if_else(RB280 == pi02, 1, 2),
      pd05  = dplyr::if_else(RB290 == pi02, 1, 2),
      pd06  = NA_integer_,
      # Bloque L -----------------------
      pl02a = PL040A,
      pl02b = PL040B,
      pl02c = dplyr::case_when(
        PL032 == 1 ~ pl02a,
        PL032 != 1 ~ pl02b,
        .default = NA_integer_
      ),
      pl10a = PL051A,
      pl10b = PL051B,
      pl10c = dplyr::case_when(
        PL032 == 1 ~ pl10a,
        PL032 != 1 ~ pl10b,
        .default = NA_integer_
      ),
      pl11a = PL051A %/% 10,
      pl11b = PL051B %/% 10,
      pl11c = dplyr::case_when(
        PL032 == 1 ~ pl11a,
        PL032 != 1 ~ pl11b,
        .default = NA_integer_
      ),
      pl12c = dplyr::case_when(
        PL032 == 1 ~ pl12a,
        PL032 != 1 ~ pl12b,
        .default = NA_integer_
      ),
      pl13c = dplyr::case_when(
        PL032 == 1 ~ pl13a,
        PL032 != 1 ~ pl13b,
        .default = NA_integer_
      ),
      pl20c = dplyr::case_when(
        PL032 == 1 ~ pl20a,
        PL032 != 1 ~ pl20b,
        .default = NA_integer_
      ),
      pl22  = dplyr::if_else(PL230 != 99, PL230, NA_integer_),
      pl30 = calc_heterogeneidad(PL040A, PL032, pl20a, pl21b, pl22, pl13a, "a", .lmh),
      pl31 = calc_heterogeneidad(PL040A, PL032, pl20a, pl21b, pl22, pl13a, "b", .lmh),
      pl50  = calc_egp(PL051A, PL040A, PL150),
      pl40a = calc_informalidad(PL040A, PY030G, PY035G, "a"),
      pl40b = calc_informalidad(PL040A, PY030G, PY035G, "b"),
      # Bloque Y -----------------------
      py00 = PY010N + PY050N + PY090N + PY110N + PY120N + PY130N + PY140N + PY100N + PY080N,
      py10 = PY010N + PY050N,
      py11 = PY010N,
      py12 = PY050N,
      py13 = calc_y_sector(py10, pl31, 1, .lmh),
      py14 = calc_y_sector(py10, pl31, 2, .lmh),
      py15 = calc_y_sector(py10, pl31, 3, .lmh),
      py20 = PY090N + PY110N + PY120N + PY130N + PY140N + PY100N + PY080N,
      py21 = PY100N + PY080N,
      py22 = PY100N,
      py23 = PY080N,
      py24 = PY090N,
      py25 = PY110N + PY120N + PY130N + PY140N,
      haa  = dplyr::if_else(pl01 == 1 & py11 != 0, maa * PL060 * 4.2, NA_real_),
      han  = dplyr::if_else(pl01 == 1 & py12 != 0, man * PL060 * 4.2, NA_real_),
      py11h = dplyr::if_else(py11 != 0, (py11 * PX010) / haa, 0),
      py12h = dplyr::if_else(py12 != 0, (py12 * PX010) / han, 0),
      dplyr::across(py00:py25, \(y) (y * PX010) / 12),
      dplyr::across(c(py00:py25, py11h, py12h), \(y) y / ppa, .names = "{.col}ppa"),
      .keep = "all"
    )

  # ------------------------------------------
  return(datos)
}

# ============================================================================
#' Agrupa los años de nacimiento en grupos de cinco años
#'
#' La función agrupa los años según el criterio que se aplica en el conjunto de
#' datos de Alemania. Los años de nacimiento se registran desde 81 años atrás
#' hasta el presente; aquellos que nacieron antes se agrupan en el primer año.
#' Los grupos de cinco años se arman a partir del primer año registrado. P.e.,
#' en 2023 el primer año registrado fue 2023 - 81 = 1942, por lo cual los
#' grupos resultan ser 1942-1946, 1947-1951, 1951-1956, etc.
#'
#' @param .anio Año de la encuesta.
#' @param .nac Vector de años de nacimiento.
#'
#' @returns Vector de años de nacimiento agrupados.
agrupar_nac <- function(.anio, .nac) {
  desf <- (.anio - 1) %% 5
  nac_agrup <- .nac + (desf - .nac) %% 5
  nac_agrup <- dplyr::if_else(nac_agrup < .anio, nac_agrup, .anio)
  return(nac_agrup)
}

# ============================================================================
#' Title
#'
#' @param .PL130 PL130
#' @param .nivel Nivel de agregación.
#' @param .lmh Módulo LMH
#'
#' @returns Tamaño del establecimiento
calc_testablecimiento <- function(.PL130, .nivel, .lmh = TRUE) {
  rlang::arg_match(.nivel, c("a", "b"))

  if (!.lmh) {
    pl21 <- NA_integer_
  } else if (.nivel == "a") {
    pl21 <- dplyr::recode_values(
      .PL130, from = tabla_pl21$PL130, to = tabla_pl21$pl21a, default = NA_integer_
    )
  } else {
    pl21 <- dplyr::recode_values(
      .PL130, from = tabla_pl21$PL130, to = tabla_pl21$pl21b, default = NA_integer_
    )
  }

  return(pl21)
}

# ============================================================================
#' Title
#'
#' @param .PL040A PL040A
#' @param .PL032 PL032
#' @param .pl20 pl20
#' @param .pl21b pl21b
#' @param .pl22 pl22
#' @param .pl13 pl13
#' @param .nivel Nivel de agregación.
#' @param .lmh lmh
#'
#' @returns heterogeneidad
calc_heterogeneidad <- function(.PL040A, .PL032, .pl20, .pl21b, .pl22, .pl13, .nivel, .lmh = TRUE) {
  rlang::arg_match(.nivel, c("a", "b"))

  if (!.lmh) {
    pl3x <- NA_integer_
  } else if (.nivel == "a") {
    pl3x <- dplyr::case_when(
      .PL040A == 1 & .pl21b > 1 ~ 1,
      .PL040A == 2 & .pl13 == 1 ~ 2,
      .PL032 == 1 & .pl22 == 1 ~ 3,
      .PL040A == 3 & .pl22 == 2 & .pl21b == 3 ~ 4,
      .PL040A == 3 & .pl22 == 2 & .pl21b == 2 ~ 5,
      .PL040A == 1 & .pl21b == 1 ~ 6,
      .PL040A == 2 & .pl13 == 2 ~ 7,
      .PL040A == 3 & .pl22 == 2 & .pl21b == 1 ~ 8,
      .PL040A == 4 ~ 8,
      .PL032 == 1 & .pl20 == 8 ~ 9,
      .default = NA_integer_
    )
  } else {
    pl3x <- dplyr::case_when(
      .PL040A == 1 & .pl21b > 1 ~ 2,
      .PL040A == 2 & .pl13 == 1 ~ 2,
      .PL032 == 1 & .pl22 == 1 ~ 1,
      .PL040A == 3 & .pl22 == 2 & .pl21b %in% 2:3 ~ 2,
      .PL040A == 1 & .pl21b == 1 ~ 3,
      .PL040A == 2 & .pl13 == 2 ~ 3,
      .PL040A == 3 & .pl22 == 2 & .pl21b == 1 ~ 3,
      .PL040A == 4 ~ 3,
      .PL032 == 1 & .pl20 == 8 ~ 2,
      .default = NA_integer_
    )
  }

  return(pl3x)
}

# ============================================================================
#' Title
#'
#' @param .PL051A PL051A
#' @param .PL040A PL040A
#' @param .PL150 PL150
#'
#' @returns egp
calc_egp <- function(.PL051A, .PL040A, .PL150) {
  .pl50 <- dplyr::recode_values(
    .PL051A, from = tabla_isco$PL051, to = tabla_isco$.pl50, default = NA_integer_
  )
  pl50 <-  dplyr::case_when(
    .pl50 == 8 & .PL040A != 1 ~ 8,
    .pl50 > 1 & .PL040A == 1 ~ 2,
    .pl50 > 1 & .PL040A == 2 ~ 6,
    .pl50 > 1 & is.na(.PL040A) ~ NA_integer_,
    .pl50 > 2 & .PL150 == 1 ~ 7,
    .pl50 > 2 & is.na(.PL150) ~ NA_integer_,
    .default = .pl50
  )

  return(pl50)
}

# ============================================================================
#' Title
#'
#' @param .PL040A PL040A
#' @param .PY030G PY030G
#' @param .PY035G PY035G
#' @param .nivel nivel de agregación
#'
#' @returns informalidad
calc_informalidad <- function(.PL040A, .PY030G, .PY035G, .nivel) {
  rlang::arg_match(.nivel, c("a", "b"))

  if (.nivel == "a") {
    pl40 <- dplyr::case_when(
      .PL040A == 3 & .PY030G != 0 ~ 1,
      .PL040A == 3 & .PY030G == 0 ~ 2,
      .PL040A %in% 1:2 & !(.PY030G == 0 & .PY035G == 0) ~ 3,
      .PL040A %in% 1:2 & .PY030G == 0 & .PY035G == 0 ~ 4,
      .PL040A == 4 ~ 4,
      .default = NA_integer_
    )
  } else {
    pl40 <- dplyr::case_when(
      .PL040A == 3 & .PY030G != 0 ~ 1,
      .PL040A == 3 & .PY030G == 0 ~ 2,
      .PL040A %in% 1:2 & !(.PY030G == 0 & .PY035G == 0) ~ 1,
      .PL040A %in% 1:2 & .PY030G == 0 & .PY035G == 0 ~ 2,
      .PL040A == 4 ~ 2,
      .default = NA_integer_
    )
  }

  return(pl40)
}

# ============================================================================
#' Title
#'
#' @param .py10 py10
#' @param .pl31 pl31
#' @param .sector sector
#' @param .lmh lmh
#'
#' @returns py13, py14 o py15
calc_y_sector <- function(.py10, .pl31, .sector, .lmh = TRUE) {
  if (!.lmh) {
    py1x <- NA_real_
  } else {
    py1x <- dplyr::case_when(
      .py10 != 0 & is.na(.pl31) ~ NA_real_,
      .py10 != 0 & .pl31 == .sector ~ .py10,
      .default = 0
    )
  }

  return(py1x)
}
