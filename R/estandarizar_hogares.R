# ----------------------------------------------------------------------------
#' Estandariza el conjunto H de la EU-SILC para el proceso de armonización
#'
#' @description
#' Aplica transformaciones sobre las variables del conjunto H según el año, el
#' país y si se proveyó el conjunto D. El conjunto final tiene las variables
#' necesarias para aplicar [calcular_hogares()]. Las variables que no están
#' disponibles quedan como `NA`. Sin D se conservan sus variables ya
#' incorporadas en H. Si se proporciona D, sus valores las reemplazan,
#' incluso cuando faltan columnas opcionales o no hay coincidencia.
#'
#' @param .H `data.frame`o `tibble`. Conjunto de datos H de la EU-SILC
#' @param .D `data.frame`o `tibble`. Conjunto de datos D de la EU-SILC
#'
#' @returns `tibble`. Conjunto de datos H de la EU-SILC estandarizado
#' @export
estandarizar_hogares <- function(.H, .D = NULL) {
  chequear_bases_hogares(.H, NULL, .D)

  anio <- unique(.H$HB010)
  pais <- unique(.H$HB020)

  # --------------------------------------------------------------------------
  cli::cli_h1("Estandarizacion")
  advertencias <- obtener_advertencias("H", anio, pais)
  informar_insumos_hogares(.D)

  .H <- estandarizar_hogares_(.H, .D, anio, pais)

  .H <- structure(
    .H,
    "base" = "H",
    "estandar" = TRUE,
    "vbles. D" = !is.null(.D),
    "advertencias" = advertencias
  )

  return(.H)
}

# ----------------------------------------------------------------------------
#' Estandariza el conjunto H de la EU-SILC para el proceso de armonización
#'
#' @description
#' Aplica transformaciones sobre las variables del conjunto H según el año, el
#' país y si se proveyó el conjunto D. El conjunto final tiene las variables
#' necesarias para aplicar [calcular_hogares()]. Las variables que no están
#' disponibles quedan como `NA`.
#'
#' @param .H `data.frame`o `tibble`. Conjunto de datos H de la EU-SILC
#' @param .D `data.frame`o `tibble`. Conjunto de datos D de la EU-SILC
#' @param .anio `numeric`. Año al que corresponde el conjunto H
#' @param .pais `character`. País al que corresponde el conjunto H
#'
#' @returns `tibble`. Conjunto de datos H de la EU-SILC estandarizado
estandarizar_hogares_ <- function(.H, .D, .anio, .pais) {
  insumos_h <- c(
    "HY040N",
    "HY050N",
    "HY060N",
    "HY070N",
    "HY080N",
    "HY090N",
    "HY110N",
    "HX040"
  )
  for (variable in setdiff(insumos_h, names(.H))) {
    .H[[variable]] <- rep(NA_real_, nrow(.H))
  }
  insumos_d <- list(
    DB040 = NA_character_,
    DB090 = NA_real_,
    DB100 = NA_integer_
  )
  if (is.null(.D)) {
    for (variable in setdiff(names(insumos_d), names(.H))) {
      .H[[variable]] <- rep(insumos_d[[variable]], nrow(.H))
    }
  } else {
    for (variable in setdiff(names(insumos_d), names(.D))) {
      .D[[variable]] <- rep(insumos_d[[variable]], nrow(.D))
    }
  }

  # Traspaso de variables desde D --------------------------------------------
  if (!is.null(.D)) {
    .H <- dplyr::left_join(
      x = dplyr::select(.H, -dplyr::any_of(names(insumos_d))),
      y = dplyr::select(
        .D,
        DB010,
        DB020,
        DB030,
        dplyr::all_of(names(insumos_d))
      ),
      by = dplyr::join_by(HB010 == DB010, HB020 == DB020, HB030 == DB030)
    )
  }

  if (.pais == "DE" & .anio < 2021 & all(is.na(.H$DB100))) {
    .H <- dplyr::mutate(.H, DB100 = NA_integer_)
  }

  return(.H)
}
