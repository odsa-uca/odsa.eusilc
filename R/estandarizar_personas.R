# ----------------------------------------------------------------------------
#' Estandariza el conjunto P de la EU-SILC para el proceso de armonización
#'
#' @description
#' Aplica transformaciones sobre las variables del conjunto P según el año, el
#' país y si se proveyeron los conjuntos D y R. El conjunto final tiene todas
#' las variables necesarias para aplicar [imputar_personas()] y
#' [calcular_personas()]. Las variables que no están disponibles quedan como
#' `NA`.
#'
#' @param .P `data.frame` o `tibble`. Conjunto de datos P de la EU-SILC.
#' @param .D `data.frame` o `tibble`. Conjunto de datos D de la EU-SILC.
#' @param .R `data.frame` o `tibble`. Conjunto de datos R de la EU-SILC.
#' @param .flags `TRUE` o `FALSE` (por defecto). ¿Construir los flags de imputación en este paso?
#'
#' @returns `tibble`. Conjunto de datos P estandarizado para [imputar_personas()] y [calcular_personas()].
#'
#' @details
#' Los conjuntos de datos de la EU-SILC presentan cierta heterogeneidad
#' dependiendo del año y el país al que correspondan. Algunas variables pueden
#' no estar disponibles en ciertos paises o años, o pueden tener valores
#' diferentes (en relación a este problema, conviene consultar los documentos _methodological
#' guidelines_ y _differences between original database ..._ de EUROSTAT).
#'
#' A los propósitos de la armonización, esta heterogeneidad tiene efecto sobre:
#'
#' * Variables demográficas: Región de residencia, edad, país de nacimiento,
#'   país de ciudadanía y nivel educativo.
#' * Variables laborales: Condición de actividad, categoría ocupacional,
#'   ocupación, rama de actividad, tamaño del establecimiento, sector público
#'   o privado, tipo de contrato y permanencia del trabajo principal.
#' * Variables de ingreso: transferencias por enfermedad.
#'
#' Más en particular, la función se encarga de los siguientes problemas. Si el
#' conjunto de datos corresponde al año 2020 o anterior, entonces:
#'
#' * La edad al momento de la entrevista se puede construir con el conjunto P y
#'   no hace falta el conjunto R.
#' * El país de nacimiento y de ciudadanía están en el conjunto P en lugar del
#'   R, y sus nombres son distintos a los que tienen a partir de 2021.
#' * El nivel educativo tiene nombre distinto.
#' * La condición de actividad tiene nombre y categorías disintas.
#' * La categoría ocupacional, la ocupación y la rama de actividad son variables
#'   únicas. A partir de 2021 se dividen en A (ocupados) y B (no ocupados).
#' * El tamaño del establecimiento está disponible todos los años.
#' * El sector público privado está disponbile los años en los que se
#'   relevó el módulo _labor market and housing conditions (LMH)_ y queda como `NA`
#'   los años en los que no.
#' * No hay información acerca del tipo de contrato (verbal o por escrito) y
#'   queda como `NA`.
#'
#' Si el conjunto de datos corresponde al año 2021 o posterior, entonces:
#'
#' * Edad al momento de la entrevista, país de nacimiento y ciudadanía están en
#'   el conjunto R y quedan como `NA` si este no se proporciona.
#' * El tamaño del establecimiento y el sector público privado están disponibles
#'   sólo en los años en los que se relevó el módulo _LMH_ y quedan como `NA`
#'   los años en los que no.
#' * El tipo de contrato y la permanencia del trabajo principal se incluyen
#'   en una única variable.
#'
#' Cualquiera sea el año, si no se proporciona el conjunto D, entonces la
#' región de residencia queda como `NA`. Además, si el país es Italia, entonces
#' la variable PY120N (_sickness benefits_) queda en cero dado que el monto se
#' incluye en otras variables.
#'
#' La función modifica los conjuntos de datos de forma tal que tengan las mismas
#' variables (potencialmente con `NA`) con los nombres y categorías con las que
#' aparecen luego de 2021. Esto simplifica el trabajo de las funciones
#' [imputar_personas()] y [calcular_personas()].
#'
#' @export
estandarizar_personas <- function(
  .P,
  .D = NULL,
  .R = NULL,
  .flags = FALSE
) {
  chequear_bases_personas(.P, .D, .R)

  rlang::check_bool(.flags, class = "no_logical")

  # --------------------------------------------------------------------------
  anio <- unique(.P$PB010)
  pais <- unique(.P$PB020)

  cli::cli_h1("Estandarizacion")
  advertencias <- obtener_advertencias("P", anio, pais)
  informar_insumos_personas(.P, .D, .R, anio)

  .P <- estandarizar_personas_(.P, .R, .D, anio, pais)

  if (.flags) {
    .P <- calc_flags_imputacion(.P, anio, pais)
  }

  .P <- structure(
    .P,
    "base" = "P",
    "estandar" = TRUE,
    "pre. 2021" = anio < 2021,
    "vbles. D" = !is.null(.D),
    "vbles. R" = !is.null(.R),
    "vble. PL130" = "PL130" %in% names(.P),
    "vble. PL230" = "PL230" %in% names(.P),
    "flags imp." = .flags,
    "advertencias" = advertencias
  )

  return(.P)
}

# ============================================================================
#' Estandariza el conjunto P de la EU-SILC para el proceso de armonización (interna)
#'
#' @description
#' ¡Esta función es interna! Aplica transformaciones sobre las variables del
#' conjunto P según el año, el país y si se proveyeron los conjuntos D y R. El
#' conjunto final tiene todas las variables necesarias para aplicar
#' [imputar_personas()] y [calcular_personas()]. Las variables que no están
#' disponibles quedan como `NA`.
#'
#' @details
#' Esta función es el núcleo interno de [estandarizar_personas()]. Para más
#' detalles ver la documentación de esa función.
#'
#' @param .P `data.frame` o `tibble`. Conjunto de datos P de la EU-SILC.
#' @param .R `data.frame` o `tibble`. Conjunto de datos R de la EU-SILC.
#' @param .D `data.frame` o `tibble`. Conjunto de datos D de la EU-SILC.
#' @param .anio `numeric`. Año de la encuesta.
#' @param .pais `character`. País de la encuesta.
#'
#' @returns `tibble`. Conjunto de datos P estandarizado para [imputar_personas()] y [calcular_personas()].
estandarizar_personas_ <- function(.P, .R, .D, .anio, .pais) {
  .P <- estandarizar_anio_personas(.P, .anio)

  if (.anio >= 2021) {
    .P <- agregar_r_personas(.P, .R)
  }

  .P <- agregar_d_personas(.P, .D)
  .P <- estandarizar_paises_personas(.P, .anio, .pais)
  .P <- calcular_auxiliares_personas(.P, .anio)

  return(.P)
}

# ============================================================================
#' Convierte algunas variables de los conjuntos previos a 2021 al formato que
#' tienen a partir de ese año
#'
#' @param .P `data.frame` o `tibble`. Conjunto de datos P de la EU-SILC.
#' @param .anio `numeric`. Año de la encuesta.
#'
#' @returns `tibble`. Conjunto de datos P con variables transformadas
estandarizar_anio_personas <- function(.P, .anio) {
  if (.anio < 2021) {
    .P <- dplyr::mutate(
      .P,
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
      PL040A = dplyr::if_else(PL032 == 1, PL040, NA),
      PL051A = dplyr::if_else(PL032 == 1, PL051, NA),
      PL111A = dplyr::if_else(PL032 == 1, PL111, NA),
      PL040B = dplyr::if_else(PL032 != 1 | is.na(PL032), PL040, NA),
      PL051B = dplyr::if_else(PL032 != 1 | is.na(PL032), PL051, NA),
      PL111B = NA_character_,
      # Los flags hacen falta si después se imputa
      PL040A_F = dplyr::if_else(PL032 == 1, PL040_F, -2),
      PL051A_F = dplyr::if_else(PL032 == 1, PL051_F, -2),
      PL111A_F = dplyr::if_else(PL032 == 1, PL111_F, -2),
      PL040B_F = dplyr::if_else(PL032 != 1 | is.na(PL032), PL040_F, -2),
      PL051B_F = dplyr::if_else(PL032 != 1 | is.na(PL032), PL051_F, -2),
      PL111B_F = -2,
    )
  }

  return(.P)
}

# ============================================================================
#' Traspasa variables del conjunto R de la EU-SILC al conjunto P
#'
#' @param .P `data.frame` o `tibble`. Conjunto de datos P de la EU-SILC.
#' @param .R `data.frame` o `tibble`. Conjunto de datos R de la EU-SILC.
#'
#' @returns `tibble`. Conjunto P de la EU-SILC con algunas varibles del conjunto R
agregar_r_personas <- function(.P, .R) {
  if (is.null(.R)) {
    .P <- dplyr::mutate(
      .P,
      RB080 = PB010 - PX020 - 1,
      RB081 = PX020,
      RB082 = NA_integer_,
      RB280 = NA_integer_,
      RB290 = NA_integer_
    )
  } else {
    .P <- dplyr::left_join(
      x = .P,
      y = dplyr::select(
        .R,
        RB010,
        RB020,
        RB030,
        RB080,
        RB081,
        RB082,
        RB280,
        RB290
      ),
      by = dplyr::join_by(PB010 == RB010, PB020 == RB020, PB030 == RB030)
    )
  }

  return(.P)
}

# ============================================================================
#' Traspasa variables del conjunto D de la EU-SILC al conjunto P
#'
#' @param .P `data.frame` o `tibble`. Conjunto de datos P de la EU-SILC.
#' @param .D `data.frame` o `tibble`. Conjunto de datos D de la EU-SILC.
#'
#' @returns `tibble`. Conjunto P de la EU-SILC con algunas variables del conjunto D
agregar_d_personas <- function(.P, .D) {
  if (is.null(.D)) {
    .P <- dplyr::mutate(.P, DB040 = NA_character_, DB100 = NA_integer_)
  } else {
    .P <- dplyr::left_join(
      x = .P,
      y = dplyr::select(.D, DB010, DB020, DB030, DB040, DB100),
      by = dplyr::join_by(PB010 == DB010, PB020 == DB020, PX030 == DB030)
    )
  }

  return(.P)
}

# ============================================================================
#' Ajustes y advertencias específicas para ciertos países y años
#'
#' @param .P `data.frame` o `tibble`. Conjunto de datos P de la EU-SILC.
#' @param .anio `numeric`. Año de la encuesta.
#' @param .pais `numeric`. País de la encuesta.
#'
#' @returns `tibble`. Conjunto P de la EU-SILC con ajustes específicos por país
estandarizar_paises_personas <- function(.P, .anio, .pais) {
  if (.pais == "IT" & all(.P$PY120N_F == -4)) {
    .P <- dplyr::mutate(.P, PY120N = 0)
  }

  # Puede haber sido leído como logical
  if (.pais == "DE" & .anio < 2021 & all(is.na(.P$DB100))) {
    .P <- dplyr::mutate(.P, DB100 = NA_integer_)
  }

  return(.P)
}

# ============================================================================
#' Calcula algunas variables auxiliares
#'
#' @param .P `data.frame` o `tibble`. Conjunto de datos P de la EU-SILC.
#' @param .anio `numeric`. Año de la encuesta.
#'
#' @returns `tibble`. Conjunto P de la EU-SILC con variables auxiliares extra.
calcular_auxiliares_personas <- function(.P, .anio) {
  if (.anio < 2021) {
    .P <- dplyr::mutate(
      .P,
      toc = NA_integer_,
      pomj = PL140,
      .before = PL040A_F
    )
  } else {
    .P <- dplyr::mutate(
      .P,
      toc = dplyr::case_when(
        PL141 == 21 | PL141 == 11 ~ 1,
        PL141 == 22 | PL141 == 12 ~ 2,
        .default = NA_integer_
      ),
      pomj = dplyr::case_when(
        PL141 == 21 | PL141 == 22 ~ 1,
        PL141 == 11 | PL141 == 12 ~ 2,
        .default = NA_integer_
      ),
      .before = RB080
    )
  }

  .P <- dplyr::mutate(.P, maa = PL073 + PL074, man = PL075 + PL076)

  return(.P)
}
