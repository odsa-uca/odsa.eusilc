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

# ============================================================================
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
  if (!is.null(.D)) {
    .H <- dplyr::left_join(
      x = .H,
      y = dplyr::select(.D, DB010, DB020, DB030, DB040, DB090, DB100),
      by = dplyr::join_by(HB010 == DB010, HB020 == DB020, HB030 == DB030)
    )
  } else {
    .H <- dplyr::mutate(
      .H,
      DB040 = NA_integer_,
      DB090 = NA_real_,
      DB100 = NA_integer_
    )
  }
  
  if (.pais == "DE" & .anio < 2021 & all(is.na(.H$DB100))) {
    .H <- dplyr::mutate(.H, DB100 = NA_integer_)
  }

  return(.H)
}
