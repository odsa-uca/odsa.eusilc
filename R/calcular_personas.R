#' Construye variables nuevas a partir del conjunto P de la EU-SILC
#'
#' @description
#' Construye variables nuevas de nivel individual a partir del conjunto P de la
#' EU-SILC. Las variables se organizan en cuatro bloques: I de identificación,
#' D de demográficos, L de laborales e Y de ingresos. Dependiendo del año, el
#' país de la encuesta y si se proporcionaron los conjuntos D y R algunas de
#' las variables pueden estar perdidas (`NA`).
#'
#' @param .P `data.frame` o `tibble`. Conjunto de datos P de la EU-SILC estandarizado con [estandarizar_personas()].
#' @param .expandir `TRUE` o `FALSE` (por defecto). ¿Mantener variables originales?
#'
#' @returns `tibble`. Conjunto de datos P de la EU-SILC estandarizado con variables armonizadas.
#'
#' @details
#' A continuación se listan las variables construidas según bloque
#'
#' ## (I) Identificación
#'
#' - pi01. Año de la encuesta
#' - pi02. Pais
#' - pi03. Región
#' - pi04. Identificador del hogar
#' - pi05. Identificador de la persona
#' - pi06. Ponderador
#'
#' ## (D) Demográficos
#'
#' - pd01a. Edad al momento de la entrevista
#' - pd01b. Edad al final del período de referencia de ingresos
#' - pd01c. Edad aproximada agrupada en quinquenios
#' - pd02. Sexo
#' - pd03. Nivel educativo
#' - pd04. Estatus migratorio
#' - pd05. Estatus ciudadanía
#' - pd06. Jefatura del Hogar
#'
#' ## (L) Laborales
#'
#' - pl01. Condición de actividad
#' - pl02a. Categoría ocupacional (CRP)
#' - pl02b. Categoría ocupacional (Último trabajo)
#' - pl02c. Categoría ocupacional (CRP / Último trabajo)
#' - pl10a. Ocupación (ISCO-08) CRP
#' - pl10b. Ocupación (ISCO-08) Último trabajo
#' - pl10c. Ocupación (ISCO-08) CRP / Último trabajo
#' - pl11a. Ocupación, grupo principal (ISCO-08) CRP
#' - pl11b. Ocupación, grupo principal (ISCO-08) Último trabajo
#' - pl11c. Ocupación, grupo principal (ISCO-08) CRP / Último trabajo
#' - pl12a. Calificación (CRP)
#' - pl12b. Calificación (Último trabajo)
#' - pl12c. Calificación (CRP / Último trabajo)
#' - pl13a. Calificación profesional (CRP)
#' - pl13b. Calificación profesional (Último trabajo)
#' - pl13c. Calificación profesional (CRP / Último trabajo)
#' - pl20a. Rama de actividad (CRP)
#' - pl20b. Rama de actividad (Último trabajo)
#' - pl20c. Rama de actividad (CRP / Último trabajo)
#' - pl21a<sup>1</sup>. Tamaño del establecimiento
#' - pl21b<sup>1</sup>. Estrato de productividad
#' - pl22<sup>1</sup>. Sector público/privado
#' - pl30<sup>1</sup>. Heterogeneidad sectorial
#' - pl31<sup>1</sup>. Sector de inserción
#' - pl40a. Informalidad laboral (4 categorías)
#' - pl40b. Informalidad laboral (2 categorías)
#' - pl50. EGP
#'
#' Nota 1: Dependen de las variables PL130 y PL230 del módulo LMH
#'
#' ## (Y) Ingresos
#'
#' - py00<sup>2</sup>. Ingreso total
#' - py10<sup>2</sup>. Ingreso total por fuentes laborales
#' - py11. Ingreso por trabajo asalariado
#' - py12<sup>2</sup>. Ingreso por trabajo no asalariado
#' - py13<sup>3</sup>. Ingreso por trabajo en el sector público
#' - py14<sup>3</sup>. Ingreso por trabajo en el sector privado formal
#' - py15<sup>3</sup>. Ingreso por trabajo en el sector microinformal
#' - py20. Ingreso total por fuentes no laborales
#' - py21. Ingreso total por jubilaciones y pensiones privadas
#' - py22. Ingreso por jubilación
#' - py23. Ingreso por pensión privada
#' - py24. Ingreso por desempleo
#' - py25. Ingreso por otras ayudas
#'
#' Nota 2: Puede tener valores negativos!
#' Nota 3: Dependen de las variables PL130 y PL230 del módulo LMH
#' 
#' Las variables de ingreso se presentan en la moneda nacional del país
#' correspondiente mensualizados (sin sufijo) y en dólares paridad de poder
#' adquisitivo (sufijo `ppa`). Para más detalles ver [tabla_ppa].
#'
#' ## Auxiliares
#'
#' Además de las variables incluidas en los cuatro bloques principales, se
#' incluyen algunas auxiliares. Estas son insumos de otras pero se conservan
#' en el conjunto final por si son de utilidad.
#'
#' - ppa_factor. Factor de conversión a PPA de la Unión Europea 27 países
#' - ppa_factor_us. Factor de conversión de dólares a PPA de la Unión Europea 27 países
#' - haa. Horas habitualmente trabajadas anuales, trabajadores asalariados
#' - han. Horas habitualmente trabajadas anuales, trabajadores no asalariados
#' - maa. Meses con ingresos por trabajo asalariado en el IRP
#' - man. Meses con ingresos por trabajo no asalariado en el IRP
#' - .f_(variable)<sup>3</sup>. Flag de imputación de la variable. Para más detalle, ver [imputar_personas()]
#'
#' Nota 3: Estas variables están presentes sólo si se imputaron los datos insumo.
#'
#' @export
calcular_personas <- function(.P, .expandir = FALSE) {
  if (!is.data.frame(.P)) {
    cli::cli_abort(
      c(".P debe ser un data.frame o tibble.",
        "x" = "Se paso un {class(.P)}"
      ),
      class = "no_data_frame"
    )
  }
  if (is.null(attr(.P, "estandar"))) {
    cli::cli_abort(
      ".P debe ser una base P estandarizada con estandarizar_personas().",
      class = "no_estandar"
    )
  }
  if (attr(.P, "base") != "P") {
    cli::cli_abort(
      ".P debe ser una base P.",
      class = "no_p"
    )
  }
  
  if (!is.logical(.expandir)) {
    cli::cli_abort(
      c(".expandir debe ser TRUE o FALSE.",
        "x" = "Se paso un {class(.expandir)}"
      ),
      class = "no_logical"
    )
  }
  
  # --------------------------------------------------------------------------
  cli::cli_h1("Calcular variables nuevas")
  .P <- calcular_personas_(.P)
  
  chequear_perdidas(.P, "P")
  
  if (!.expandir) {
    .P <- dplyr::select(.P, dplyr::any_of(names(etq$P$variables)))
  } else {
    .P <- dplyr::relocate(.P, dplyr::any_of(names(etq$P$variables)))
  }
  
  attr(.P, "expandida") <- .expandir
  
  return(.P)
}

# ============================================================================
#' Construye variables nuevas a partir del conjunto P de la EU-SILC (interna)
#'
#' @description
#' ¡Esta función es interna! Construye variables nuevas de nivel individual a
#' partir del conjunto P de la EU-SILC. Las variables se organizan en cuatro
#' bloques: I de identificación, D de demográficos, L de laborales e Y de
#' ingresos. Dependiendo del año, el país de la encuesta y si se proporcionaron
#' los conjuntos D y R algunas de las variables pueden estar perdidas (`NA`).
#' 
#' @details
#' Esta función es el núcleo interno de [calcular_personas()]. Para más detalles
#' ver la documentación de esa función.
#'
#' @param .P `data.frame` o `tibble`. Conjunto de datos P de la EU-SILC estandarizado con [estandarizar_personas()].
#'
#' @returns `tibble`. Conjunto de datos P de la EU-SILC estandarizado con variables armonizadas.
calcular_personas_ <- function(.P) {
  # PPA --------------------------------------
  .P <- dplyr::left_join(
    x  = .P,
    y  = tabla_ppa,
    by = dplyr::join_by(PB010, PB020)
  )

  # Lookup -----------------------------------
  .P <- dplyr::mutate(
    .P,
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

  # Núcleo -----------------------------------
  .P <- dplyr::mutate(
    .data = .P,
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
    pd04  = dplyr::if_else(RB280 == "LOC", 1, 2),
    pd05  = dplyr::if_else(RB290 == "LOC", 1, 2),
    pd06  = NA_integer_,
    # Bloque L -----------------------
    pl02a = PL040A,
    pl02b = PL040B,
    pl02c = calc_variante_c(PL032, pl02a, pl02b),
    pl10a = PL051A,
    pl10b = PL051B,
    pl10c = calc_variante_c(PL032, pl10a, pl10b),
    pl11a = PL051A %/% 10,
    pl11b = PL051B %/% 10,
    pl11c = calc_variante_c(PL032, pl11a, pl11b),
    pl12c = calc_variante_c(PL032, pl12a, pl12b),
    pl13c = calc_variante_c(PL032, pl13a, pl13b),
    pl20c = calc_variante_c(PL032, pl20a, pl20b),
    pl50  = calc_egp(PL051A, PL040A, PL150),
    pl40a = calc_informalidad(PL040A, PY030G, PY035G, "a"),
    pl40b = calc_informalidad(PL040A, PY030G, PY035G, "b"),
    # Bloque Y -----------------------
    py00 = PY010N + PY050N + PY090N + PY110N + PY120N + PY130N + PY140N + PY100N + PY080N,
    py10 = PY010N + PY050N,
    py11 = PY010N,
    py12 = PY050N,
    py20 = PY090N + PY110N + PY120N + PY130N + PY140N + PY100N + PY080N,
    py21 = PY100N + PY080N,
    py22 = PY100N,
    py23 = PY080N,
    py24 = PY090N,
    py25 = PY110N + PY120N + PY130N + PY140N,
    haa  = dplyr::if_else(pl01 == 1 & py11 != 0 & maa != 0,
                          maa * PL060 * 4.2, NA_real_),
    han  = dplyr::if_else(pl01 == 1 & py12 != 0 & man != 0,
                          man * PL060 * 4.2, NA_real_),
    py11h = dplyr::if_else(py11 != 0, (py11 * PX010) / haa, 0),
    py12h = dplyr::if_else(py12 != 0, (py12 * PX010) / han, 0),
    .keep = "all"
  )

  if ("PL130" %in% names(.P)) {
    .P <- dplyr::mutate(
      .data = .P,
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
      ),
     .keep = "all"
    )
  } else {
    .P <- dplyr::mutate(
      .data = .P,
      pl21a = NA_integer_,
      pl21b = NA_integer_,
      pl30  = NA_integer_,
      pl31  = NA_integer_,
      py13  = NA_real_,
      py14  = NA_real_,
      py15  = NA_real_,
     .keep  = "all"
    )

  }

  if("PL230" %in% names(.P)) {
    .P <- dplyr::mutate(
      .data = .P,
      pl22 = dplyr::if_else(PL230 != 99, PL230, NA_integer_)
    )
  } else {
    .P <- dplyr::mutate(
      .data = .P,
      pl22  = NA_integer_,
      pl30  = NA_integer_,
      pl31  = NA_integer_,
      py13  = NA_real_,
      py14  = NA_real_,
      py15  = NA_real_,
     .keep  = "all"
    )
  }

  if(all(c("PL130", "PL230") %in% names(.P))) {
    .P <- dplyr::mutate(
      .data = .P,
      pl30 = calc_heterogeneidad(PL040A, PL032, pl20a, pl21b, pl22, pl13a, "a"),
      pl31 = calc_heterogeneidad(PL040A, PL032, pl20a, pl21b, pl22, pl13a, "b"),
      py13 = calc_y_sector(py10, pl31, 1),
      py14 = calc_y_sector(py10, pl31, 2),
      py15 = calc_y_sector(py10, pl31, 3),
     .keep = "all"
    )
  }

  # Ingresos mensuales y ppa -----------------
  .P <- dplyr::mutate(
    .data = .P,
    dplyr::across(
      c(py00:py25, py13:py15),
      \(y) (y * PX010) / 12
    ),
    dplyr::across(
      c(py00:py25, py13:py15, py11h, py12h),
      \(y) y / ppa_factor * ppa_factor_us,
      .names = "{.col}ppa"
    ),
    .keep = "all"
  )

  # ------------------------------------------
  return(.P)
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
#' @param .anio `numeric`. Año de la encuesta.
#' @param .nac `numeric`. Vector de años de nacimiento.
#'
#' @returns `numeric`. Vector de años de nacimiento agrupados.
agrupar_nac <- function(.anio, .nac) {
  desf <- (.anio - 1) %% 5
  nac_agrup <- .nac + (desf - .nac) %% 5
  nac_agrup <- dplyr::if_else(nac_agrup < .anio, nac_agrup, .anio)
  return(nac_agrup)
}

# ============================================================================
#' Calcula el clasificador de heterogeneidad estructural
#' 
#' @param .PL040A `numeric`. Categoría ocupacional
#' @param .PL032 `numeric`. Condición de actividad
#' @param .pl20  `numeric`. Rama de actividad
#' @param .pl21b `numeric`. Estrato de productividad
#' @param .pl22  `numeric`. Sector público-privado
#' @param .pl13  `numeric`. Calificación profesional
#' @param .nivel `numeric`. Nivel de agregación
#'
#' @returns `numeric`. Clasificador de heterogeneidad estructural
calc_heterogeneidad <- function(.PL040A, .PL032, .pl20, .pl21b, .pl22, .pl13, .nivel) {
  if (.nivel == "a") {
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
#' Calcula el clasificador de clase social de Erikson, Goldthorpe y Pontocarero
#'
#' @param .PL051A `numeric`. Ocupación (ISCO-08)
#' @param .PL040A `numeric`. Categoría ocupacional
#' @param .PL150  `numeric`. Responsabilidades de supervisión
#'
#' @returns `numeric`. Clasificador de clase de EGP
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
#' Clacula el clasificador de informalidad laboral según aportes a la seguridad social
#'
#' @param .PL040A `numeric`. Categoría ocupacional
#' @param .PY030G `numeric`. Contribuciones a la seguridad social del empleador
#' @param .PY035G `numeric`. Contribuciones a pensiones privadas individuales
#' @param .nivel  `numeric`. Nivel de agregación
#'
#' @returns `numeric`. Clasificador de informalidad laboral
calc_informalidad <- function(.PL040A, .PY030G, .PY035G, .nivel) {
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
#' Censura ingresos que no provienen de determinado sector de inserción
#'
#' @param .py10   `numeric`. Ingresos laborales
#' @param .pl31   `numeric`. Sector de inserción de los individuos
#' @param .sector `numeric`. Sector de inserción a seleccionar
#'
#' @returns `numeric`. Ingresos laborales provenientes de determinado sector, 0 el resto
calc_y_sector <- function(.py10, .pl31, .sector) {
  py1x <- dplyr::case_when(
    .py10 != 0 & is.na(.pl31) ~ NA_real_,
    .py10 != 0 & .pl31 == .sector ~ .py10,
    .default = 0
  )

  return(py1x)
}

# ============================================================================
#' Calcula híbrido entre características de la ocupación de los ocupados y los desocupados
#'
#' @param .PL032 `numeric`. Condición de actividad
#' @param .a `numeric`. Características de la ocupación de los ocupados en el CRP
#' @param .b `numeric`. Características de la última ocupación de los desocupados en el CRP
#'
#' @returns `numeric`. Características de la ocupación actual o de la última ocupación
calc_variante_c <- function(.PL032, .a, .b) {
  plxxc <- dplyr::case_when(
    .PL032 == 1 ~ .a,
    .PL032 != 1 ~ .b,
    .default = NA_integer_
  )

  return(plxxc)
}
