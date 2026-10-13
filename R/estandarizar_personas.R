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
#' * Los insumos de edad, país de nacimiento y ciudadanía se incorporan desde
#'   R. Sin R se conservan los ya presentes en P y se completan los ausentes
#'   con `NA`. Las edades se construyen en [calcular_personas()] con los
#'   insumos disponibles en orden de prioridad.
#' * El tamaño del establecimiento y el sector público privado están disponibles
#'   sólo en los años en los que se relevó el módulo _LMH_ y quedan como `NA`
#'   los años en los que no.
#' * El tipo de contrato y la permanencia del trabajo principal se incluyen
#'   en una única variable.
#'
#' Cualquiera sea el año, sin D se conservan DB040 y DB100 si ya están en P y
#' se completan las columnas ausentes con `NA`. Si se proporciona D, sus
#' valores reemplazan esas columnas, incluso cuando faltan o no hay
#' coincidencia. Lo mismo se aplica a los insumos de R desde 2021.
#' Además, en Italia PY120N (_sickness benefits_) queda en cero cuando todos
#' los valores de PY120N_F son -4, dado que el monto se incluye en otras
#' variables. Una flag ausente o indeterminada no activa ese ajuste.
#'
#' La función modifica los conjuntos de datos de forma tal que tengan las mismas
#' variables (potencialmente con `NA`) con los nombres y categorías con las que
#' aparecen luego de 2021. Esto simplifica el trabajo de las funciones
#' [imputar_personas()] y [calcular_personas()].
#'
#' Se completan también los insumos opcionales de imputación y sus flags
#' originales. Esto no modifica los criterios de imputación. Los auxiliares
#' `maa` y `man` se construyen en este paso y pueden imputarse antes del
#' cálculo de indicadores; el cálculo conserva sus valores imputados.
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
  vble_PL130 <- "PL130" %in% names(.P)
  vble_PL230 <- "PL230" %in% names(.P)

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
    "vble. PL130" = vble_PL130,
    "vble. PL230" = vble_PL230,
    "flags imp." = .flags,
    "advertencias" = advertencias
  )

  return(.P)
}

# ----------------------------------------------------------------------------
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
  # Insumos para el cálculo y la imputación ----------------------------------
  insumos_p <- list(
    PB040 = NA_real_,
    PB140 = NA_integer_,
    PB150 = NA_integer_,
    PX010 = NA_real_,
    PX020 = NA_integer_,
    PE041 = NA_integer_,
    PL032 = NA_integer_,
    PL040A = NA_integer_,
    PL040B = NA_integer_,
    PL051A = NA_integer_,
    PL051B = NA_integer_,
    PL111A = NA_character_,
    PL111B = NA_character_,
    PL060 = NA_real_,
    PL073 = NA_integer_,
    PL074 = NA_integer_,
    PL075 = NA_integer_,
    PL076 = NA_integer_,
    PL130 = NA_integer_,
    PL230 = NA_integer_,
    PL141 = NA_integer_,
    PL150 = NA_integer_,
    PY010N = NA_real_,
    PY050N = NA_real_,
    PY080N = NA_real_,
    PY090N = NA_real_,
    PY100N = NA_real_,
    PY110N = NA_real_,
    PY120N = NA_real_,
    PY130N = NA_real_,
    PY140N = NA_real_,
    PY030G = NA_real_,
    PY035G = NA_real_,
    PY120N_F = NA_integer_,
    PL060_F = NA_integer_,
    PL040A_F = NA_integer_,
    PL040B_F = NA_integer_,
    PL051A_F = NA_integer_,
    PL051B_F = NA_integer_,
    PL111A_F = NA_integer_,
    PL111B_F = NA_integer_,
    PL130_F = NA_integer_,
    PL230_F = NA_integer_
  )
  insumos_r <- list(
    RB080 = NA_integer_,
    RB081 = NA_integer_,
    RB082 = NA_integer_,
    RX010 = NA_integer_,
    RX020 = NA_integer_,
    RB280 = NA_character_,
    RB290 = NA_character_
  )
  insumos_d <- list(DB040 = NA_character_, DB100 = NA_integer_)

  if (.anio < 2021) {
    insumos_p <- c(
      insumos_p,
      list(
        PB100 = NA_integer_,
        PB110 = NA_integer_,
        PB130 = NA_integer_,
        PB210 = NA_character_,
        PB220A = NA_character_,
        PE040 = NA_integer_,
        PL031 = NA_integer_,
        PL040 = NA_integer_,
        PL051 = NA_integer_,
        PL111 = NA_character_,
        PL140 = NA_integer_,
        PL040_F = NA_integer_,
        PL051_F = NA_integer_,
        PL111_F = NA_integer_
      )
    )
  }
  for (variable in setdiff(names(insumos_p), names(.P))) {
    .P[[variable]] <- rep(insumos_p[[variable]], nrow(.P))
  }
  if (.anio >= 2021 && !is.null(.R)) {
    for (variable in setdiff(names(insumos_r), names(.R))) {
      .R[[variable]] <- rep(insumos_r[[variable]], nrow(.R))
    }
  } else {
    for (variable in setdiff(names(insumos_r), names(.P))) {
      .P[[variable]] <- rep(insumos_r[[variable]], nrow(.P))
    }
  }
  if (is.null(.D)) {
    for (variable in setdiff(names(insumos_d), names(.P))) {
      .P[[variable]] <- rep(insumos_d[[variable]], nrow(.P))
    }
  } else {
    for (variable in setdiff(names(insumos_d), names(.D))) {
      .D[[variable]] <- rep(insumos_d[[variable]], nrow(.D))
    }
  }

  # Conversión de los insumos históricos -------------------------------------
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
      PL040A_F = dplyr::if_else(PL032 == 1, PL040_F, -2),
      PL051A_F = dplyr::if_else(PL032 == 1, PL051_F, -2),
      PL111A_F = dplyr::if_else(PL032 == 1, PL111_F, -2),
      PL040B_F = dplyr::if_else(PL032 != 1 | is.na(PL032), PL040_F, -2),
      PL051B_F = dplyr::if_else(PL032 != 1 | is.na(PL032), PL051_F, -2),
      PL111B_F = -2,
    )
  }

  # Traspaso de variables desde R y D ----------------------------------------
  if (.anio >= 2021 && !is.null(.R)) {
    .P <- dplyr::left_join(
      x = dplyr::select(.P, -dplyr::any_of(names(insumos_r))),
      y = dplyr::select(
        .R,
        RB010,
        RB020,
        RB030,
        dplyr::all_of(names(insumos_r))
      ),
      by = dplyr::join_by(PB010 == RB010, PB020 == RB020, PB030 == RB030)
    )
  }

  if (!is.null(.D)) {
    .P <- dplyr::left_join(
      x = dplyr::select(.P, -dplyr::any_of(names(insumos_d))),
      y = dplyr::select(
        .D,
        DB010,
        DB020,
        DB030,
        dplyr::all_of(names(insumos_d))
      ),
      by = dplyr::join_by(PB010 == DB010, PB020 == DB020, PX030 == DB030)
    )
  }

  # Ajustes nacionales -------------------------------------------------------
  if (.pais == "IT" && isTRUE(all(.P$PY120N_F == -4))) {
    .P <- dplyr::mutate(.P, PY120N = 0)
  }

  if (.pais == "DE" & .anio < 2021 & all(is.na(.P$DB100))) {
    .P <- dplyr::mutate(.P, DB100 = NA_integer_)
  }

  # Variables auxiliares previas a la imputación ------------------------------
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
