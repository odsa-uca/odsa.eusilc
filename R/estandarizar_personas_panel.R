# ----------------------------------------------------------------------------
#' Estandariza el conjunto P longitudinal de la EU-SILC (interna)
#'
#' @description
#' Núcleo interno de estandarización de [expandir_personas_panel()]. Completa
#' los insumos ausentes con `NA`, aplica las transformaciones nacionales e
#' incorpora variables desde R y D longitudinales. Supone observaciones de
#' 2021 en adelante y no agrega atributos de armonización.
#'
#' @param .P `data.frame` o `tibble`. Conjunto P longitudinal en formato largo.
#' @param .D `data.frame` o `tibble`, o `NULL`. Conjunto D longitudinal.
#' @param .R `data.frame` o `tibble`, o `NULL`. Conjunto R longitudinal.
#'
#' @returns Conjunto P estandarizado, con las filas y el orden originales.
#'   Conserva los insumos añadidos como `NA` y construye `maa` y `man`.
#'
#' @details
#' Los cruces incluyen año y país, además de persona para R y hogar actual
#' para D. Cada clave debe identificar una única fila en el auxiliar. Si se
#' suministra un auxiliar, sus valores reemplazan los insumos correspondientes
#' de P; si no, se conservan los ya incorporados. Sin coincidencia quedan `NA`.
#' No se balancea el panel ni se crean observaciones.
#'
#' En Italia, `PY120N` se establece en cero cuando `PY120N_F` vale -4.
#' Esta regla contable no depende de la imputación. Sin la flag se conserva
#' el valor publicado. No se revierten agrupaciones ni perturbaciones.
#'
#' `maa` y `man` se calculan con [calcular_meses_actividad()] antes de la
#' futura imputación. Para las reglas y limitaciones de los insumos, ver
#' [expandir_personas_panel()].
estandarizar_personas_panel_ <- function(.P, .D = NULL, .R = NULL) {
  # Estandarización de los conjuntos -----------------------------------------
  insumos_r <- list(
    RB062 = NA_real_,
    RB063 = NA_real_,
    RB064 = NA_real_,
    RB065 = NA_real_,
    RB066 = NA_real_,
    RB080 = NA_integer_,
    RB081 = NA_integer_,
    RB082 = NA_integer_,
    RX010 = NA_integer_,
    RX020 = NA_integer_,
    RB280 = NA_character_,
    RB290 = NA_character_
  )
  insumos_d <- list(
    DB075 = NA_integer_,
    DB076 = NA_integer_,
    DB040 = NA_character_,
    DB100 = NA_integer_
  )
  if (is.null(.R)) {
    for (variable in setdiff(names(insumos_r), names(.P))) {
      .P[[variable]] <- rep(insumos_r[[variable]], nrow(.P))
    }
  } else {
    for (variable in setdiff(names(insumos_r), names(.R))) {
      .R[[variable]] <- rep(insumos_r[[variable]], nrow(.R))
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

  insumos <- c(
    "PY010N",
    "PY050N",
    "PY080N",
    "PY090N",
    "PY100N",
    "PY110N",
    "PY120N",
    "PY130N",
    "PY140N",
    "PX010",
    "PB140",
    "PX020",
    "PE041",
    "PL032",
    "PL040A",
    "PL040B",
    "PL051A",
    "PL051B",
    "PY030G",
    "PY035G",
    "PL141",
    "PL060",
    paste0("PL211", LETTERS[1:12])
  )
  for (variable in setdiff(insumos, names(.P))) {
    .P[[variable]] <- rep(NA_real_, nrow(.P))
  }

  if ("PY120N_F" %in% names(.P)) {
    .P <- dplyr::mutate(
      .P,
      PY120N = dplyr::if_else(
        PB020 == "IT" & PY120N_F == -4,
        0,
        PY120N,
        missing = PY120N
      )
    )
  }

  # Traspaso de variables desde conjuntos auxiliares -------------------------
  if (!is.null(.R)) {
    .P <- dplyr::left_join(
      x = dplyr::select(.P, -dplyr::any_of(names(insumos_r))),
      y = dplyr::select(
        .R,
        RB010,
        RB020,
        RB030,
        dplyr::all_of(names(insumos_r))
      ),
      by = dplyr::join_by(PB010 == RB010, PB020 == RB020, PB030 == RB030),
      relationship = "many-to-one"
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
      by = dplyr::join_by(PB010 == DB010, PB020 == DB020, PX030 == DB030),
      relationship = "many-to-one"
    )
  }
  
  # Variables auxiliares para la imputación -----------------------------------
  .P <- dplyr::mutate(
    .P,
    maa = calcular_meses_actividad(.P, c(1, 2)),
    man = calcular_meses_actividad(.P, c(3, 4))
  )

  return(.P)
}
