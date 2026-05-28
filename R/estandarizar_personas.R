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
#'   ocupación, rama de actividad, tamaño del establecimiento y sector público
#'   o privado.
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
#'
#' Si el conjunto de datos corresponde al año 2021 o posterior, entonces:
#'
#' * Edad al momento de la entrevista, país de nacimiento y ciudadanía están en
#'   el conjunto R y quedan como `NA` si este no se proporciona.
#' * El tamaño del establecimiento y el sector público privado están disponibles
#'   sólo en los años en los que se relevó el módulo _LMH_ y quedan como `NA`
#'   los años en los que no.
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
    .R = NULL,
    .D = NULL
) {
  # TODO: chequeos de argumentos
  anio <- unique(.P$PB010)
  pais <- unique(.P$PB020)
  
  estandarizar_personas_(.P, .R, .D, anio, pais)
}

# ============================================================================
#' Estandariza el conjunto P de la EU-SILC para el proceso de armonización (interna)
#'
#' @param .P `data.frame` o `tibble`. Conjunto de datos P de la EU-SILC.
#' @param .R `data.frame` o `tibble`. Conjunto de datos R de la EU-SILC.
#' @param .D `data.frame` o `tibble`. Conjunto de datos D de la EU-SILC.
#' @param .anio `numeric`. Año de la encuesta.
#' @param .pais `character`. País de la encuesta.
#'
#' @returns `tibble`. Conjunto de datos P estandarizado para [imputar_personas()] y [calcular_personas()].
estandarizar_personas_ <- function(.P, .R, .D, .anio, .pais) {
  # Anterior a 2021 --------------------------
  if (.anio <= 2021) {
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

    cli::cli_bullets(c(
      "!" = "La base corresponde al {(.anio)}, anterior a 2021",
      " " = "No hace falta el conjunto R",
      " " = "Se pierde PL111B"
    ))
  # Posterior a 2021 sin R -------------------
  } else if (is.null(.R)) {
    .P <- dplyr::mutate(
      .P,
      RB080 = PB010 - PX020 - 1,
      RB081 = PX020,
      RB082 = NA_integer_,
      RB280 = NA_integer_,
      RB290 = NA_integer_
    )

    cli::cli_bullets(c(
      "!" = "No se proporciono el conjunto R",
      " " = "Se pierden: pd01a, pd04, pd05"
    ))
  # Posterior a 2021 con R -------------------
  } else {
    .P <- dplyr::left_join(
      x  = .P,
      y  = dplyr::select(.R, RB010, RB020, RB030, RB080, RB081, RB082, RB280, RB290),
      by = dplyr::join_by(PB010 == RB010, PB020 == RB020, PB030 == RB030)
    )

    cli::cli_bullets(c(
      "v" = "La base es posterior a 2021 y se proporciono el conjunto R"
    ))
  }

  # Sin D ------------------------------------
  if (is.null(.D)) {
    .P <- dplyr::mutate(.P, DB040 = NA_character_)

    cli::cli_bullets(c(
      "!" = "No se proporciono el conjunto D",
      " " = "Se pierden: pi03"
    ))
  # Con D ------------------------------------
  } else {
    .P <- dplyr::left_join(
      x  = .P,
      y  = dplyr::select(.D, DB010, DB020, DB030, DB040),
      by = dplyr::join_by(PB010 == DB010, PB020 == DB020, PX030 == DB030)
    )

    cli::cli_bullets(c(
      "v" = "Se proporciono el conjunto D"
    ))
  }

  if (!("PL230" %in% names(.P))) {
    cli::cli_bullets(c(
      "!" = "No se encontro PL230",
      " " = "Se pierden: pl22, pl30, pl31, py13, py14, py15."
    ))
  } else {
    cli::cli_bullets(c(
      "v" = "Se encontro la variable PL230"
    ))
  }

  if (!("PL130" %in% names(.P))) {
    cli::cli_bullets(c(
      "!" = "No se encontro PL130",
      " " = "Se pierden: pl21a, pl21b, pl30, pl31, py13, py14, py15."
    ))
  } else {
    cli::cli_bullets(c(
      "v" = "Se encontro la variable PL130"
    ))

  }

  if (.pais == "IT" & all(.P$PY120N_F == -4)) {
    .P <- dplyr::mutate(.P, PY120N = 0)

    cli::cli_bullets(c(
      "!" = "El pais es Italia y PY120N (sickness benefits) se incluye en otro monto",
      " " = "Se deja en cero"
    ))
  }

  return(.P)
}
