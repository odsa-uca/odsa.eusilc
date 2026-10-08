# ----------------------------------------------------------------------------
#' Construye variables del conjunto P longitudinal de la EU-SILC (interna)
#'
#' @description
#' Núcleo interno de cálculo de [expandir_personas_panel()]. Incorpora los
#' factores PPA, recodifica mediante tablas y construye variables de
#' identificación, demográficas, laborales e ingresos. Supone observaciones
#' de 2021 en adelante.
#'
#' @param .P `data.frame` o `tibble`. Conjunto P longitudinal preparado
#'   por [estandarizar_personas_panel_()], incluidos los auxiliares `maa` y `man`.
#'
#' @returns Conjunto P con todas las columnas de entrada y las variables
#'   armonizadas, conservando las filas y su orden. Reemplaza factores PPA
#'   homónimos y variables armonizadas preexistentes.
#'
#' @details
#' Conserva las reglas nacionales y las limitaciones de detalle publicadas.
#' En PT, el código ocupacional 14 agrupa 11–14: `pl12a/b` quedan como `NA`,
#' antes de construir las variantes C y el empleo de calidad. Los códigos
#' militares 1, 2 y 3 representan 01, 02 y 03 y pertenecen al grupo principal 0.
#' [calc_informalidad()] y [calc_calidad()] conservan sus reglas transversales.
#'
#' Los ingresos anuales netos se convierten a moneda nacional mensual con
#' `PX010 / 12`. Los horarios se calculan desde los importes anuales con
#' `PX010`, antes de mensualizar. Las variantes PPA se obtienen con
#' `ingreso / ppa_factor * ppa_factor_us`; sin factores disponibles quedan
#' como `NA`. Se conservan ingresos negativos y se propagan los faltantes.
#'
#' Para las definiciones y limitaciones de cada variable, ver
#' [expandir_personas_panel()]. `pd01c` permanece pendiente.
calcular_personas_panel_ <- function(.P) {
  # Factores de conversión a PPA ---------------------------------------------
  .P <- dplyr::left_join(
    x = dplyr::select(.P, -dplyr::any_of(c("ppa_factor", "ppa_factor_us"))),
    y = tabla_ppa_,
    by = dplyr::join_by(PB010, PB020),
    relationship = "many-to-one"
  )

  # Recodificación mediante tablas de lookup --------------------------------
  .P <- dplyr::mutate(
    .P,
    pi07 = dplyr::recode_values(
      DB100,
      from = tabla_pi07$DB100,
      to = tabla_pi07$pi07,
      default = NA_integer_
    ),
    pd03 = dplyr::recode_values(
      PE041,
      from = tabla_pd03$PE041,
      to = tabla_pd03$pd03,
      default = NA_integer_
    ),
    pl01 = dplyr::recode_values(
      PL032,
      from = tabla_pl01$PL032,
      to = tabla_pl01$pl01,
      default = NA_integer_
    ),
    pl12a = dplyr::recode_values(
      PL051A,
      from = tabla_isco$PL051,
      to = tabla_isco$pl12,
      default = NA_integer_
    ),
    pl12b = dplyr::recode_values(
      PL051B,
      from = tabla_isco$PL051,
      to = tabla_isco$pl12,
      default = NA_integer_
    ),
    pl13a = dplyr::recode_values(
      PL051A,
      from = tabla_isco$PL051,
      to = tabla_isco$pl13,
      default = NA_integer_
    ),
    pl13b = dplyr::recode_values(
      PL051B,
      from = tabla_isco$PL051,
      to = tabla_isco$pl13,
      default = NA_integer_
    )
  )

  # Construcción de nuevas variables y recodificación -------------------------
  .P <- dplyr::mutate(
    .P,
    # Bloque I --------------------------------------------------------------
    pi01 = PB010,
    pi02 = PB020,
    pi03 = DB040,
    pi04 = PX030,
    pi05 = PB030,
    pi06a = RB062,
    pi06b = RB063,
    pi06c = RB064,
    pi06d = RB065,
    pi06e = RB066,
    pi08a = DB075,
    pi08b = DB076,
    # Bloque D --------------------------------------------------------------
    pd01a = dplyr::coalesce(RB082, RX010),
    pd01b = dplyr::coalesce(
      RB081,
      RX020,
      PX020,
      dplyr::case_when(
        PB020 %in%
          c("ES", "IT", "PL") |
          (PB020 == "PT" & dplyr::coalesce(RB080, PB140) > PB010 - 80) ~
          PB010 - dplyr::coalesce(RB080, PB140) - 1,
        .default = NA_real_
      )
    ),
    pd02 = PB150,
    pd04 = dplyr::case_when(
      RB280 == "LOC" ~ 1L,
      RB280 %in% c("EU", "OTH") ~ 2L,
      .default = NA_integer_
    ),
    pd05 = dplyr::case_when(
      RB290 == "LOC" ~ 1L,
      RB290 %in% c("EU", "OTH") ~ 2L,
      .default = NA_integer_
    ),
    # Bloque L --------------------------------------------------------------
    pl02a = PL040A,
    pl02b = PL040B,
    pl02c = dplyr::case_when(
      PL032 == 1 ~ pl02a,
      PL032 != 1 ~ pl02b,
      .default = NA_real_
    ),
    pl10a = PL051A,
    pl10b = PL051B,
    pl10c = dplyr::case_when(
      PL032 == 1 ~ pl10a,
      PL032 != 1 ~ pl10b,
      .default = NA_real_
    ),
    pl11a = dplyr::if_else(
      PL051A %in% tabla_isco$PL051,
      PL051A %/% 10,
      NA_real_
    ),
    pl11b = dplyr::if_else(
      PL051B %in% tabla_isco$PL051,
      PL051B %/% 10,
      NA_real_
    ),
    pl11c = dplyr::case_when(
      PL032 == 1 ~ pl11a,
      PL032 != 1 ~ pl11b,
      .default = NA_real_
    ),
    pl12a = dplyr::if_else(PB020 == "PT" & PL051A == 14, NA_real_, pl12a),
    pl12b = dplyr::if_else(PB020 == "PT" & PL051B == 14, NA_real_, pl12b),
    pl12c = dplyr::case_when(
      PL032 == 1 ~ pl12a,
      PL032 != 1 ~ pl12b,
      .default = NA_real_
    ),
    pl13c = dplyr::case_when(
      PL032 == 1 ~ pl13a,
      PL032 != 1 ~ pl13b,
      .default = NA_real_
    ),
    pl40a = calc_informalidad(PL040A, PY030G, PY035G, "a"),
    pl40b = calc_informalidad(PL040A, PY030G, PY035G, "b"),
    toc = dplyr::case_when(
      PL141 %in% c(11, 21) ~ 1L,
      PL141 %in% c(12, 22) ~ 2L,
      .default = NA_integer_
    ),
    pomj = dplyr::case_when(
      PL141 %in% c(21, 22) ~ 1L,
      PL141 %in% c(11, 12) ~ 2L,
      .default = NA_integer_
    ),
    pl41 = calc_calidad(pl40a, pl12a, pomj),
    # Bloque Y --------------------------------------------------------------
    py00 = PY010N +
      PY050N +
      PY090N +
      PY110N +
      PY120N +
      PY130N +
      PY140N +
      PY100N +
      PY080N,
    py10 = PY010N + PY050N,
    py11 = PY010N,
    py12 = PY050N,
    py20 = PY090N + PY110N + PY120N + PY130N + PY140N + PY100N + PY080N,
    py21 = PY100N + PY080N,
    py22 = PY100N,
    py23 = PY080N,
    py24 = PY090N,
    py25 = PY110N + PY120N + PY130N + PY140N,
    haa = dplyr::if_else(
      pl01 == 1 & py11 != 0 & maa > 0 & PL060 > 0,
      maa * PL060 * 4.2,
      NA_real_
    ),
    han = dplyr::if_else(
      pl01 == 1 & py12 != 0 & man > 0 & PL060 > 0,
      man * PL060 * 4.2,
      NA_real_
    )
  )

  # Ingresos horarios, mensuales y PPA ---------------------------------------
  .P <- dplyr::mutate(
    .P,
    py11h = dplyr::if_else(py11 != 0, (py11 * PX010) / haa, 0),
    py12h = dplyr::if_else(py12 != 0, (py12 * PX010) / han, 0),
    dplyr::across(
      c(py00, py10, py11, py12, py20, py21, py22, py23, py24, py25),
      \(y) (y * PX010) / 12
    ),
    dplyr::across(
      c(
        py00,
        py10,
        py11,
        py12,
        py20,
        py21,
        py22,
        py23,
        py24,
        py25,
        py11h,
        py12h
      ),
      \(y) y / ppa_factor * ppa_factor_us,
      .names = "{.col}ppa"
    )
  )

  return(.P)
}
