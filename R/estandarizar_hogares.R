#' Estandariza el conjunto H de la EU-SILC para el proceso de armonización
#' 
#' @description
#' Aplica transformaciones sobre las variables de los conjuntos H y P (expandido)
#' según el año, el país y si se proveyó el conjunto D. El conjunto final tiene
#' las variables necesarias para aplicar [calcular_hogares()]. Las variables
#' que no están disponibles quedan como `NA`.
#'
#' @param .H `data.frame`o `tibble`. Conjunto de datos H de la EU-SILC
#' @param .P `data.frame`o `tibble`. Conjunto de datos P de la EU-SILC expandido por [expandir_personas()]
#' @param .D `data.frame`o `tibble`. Conjunto de datos D de la EU-SILC
#' 
#' @returns `tibble`. Conjunto de datos H de la EU-SILC con variables adicionales armonizadas
#' @export
estandarizar_hogares <- function(
    .H,
    .P,
    .D = NULL
) {
  chequear_bases_hogares(.H, .P, .D)
  
  anio <- unique(.H$HB010)
  pais <- unique(.H$HB020)

  .H <- estandarizar_hogares_(.H, .P, .D, anio, pais)
  
  .H <- structure(
    .H,
    "base"      = "H",
    "estandar"  = TRUE,
    "vbles. D"  = !is.null(.D),
    "vbles. LMH"= attr(.P, "vble. PL230")
  )
  
  return(.H)
}

# ============================================================================
#' Estandariza el conjunto H de la EU-SILC para el proceso de armonización
#' 
#' @description
#' Aplica transformaciones sobre las variables de los conjuntos H y P (expandido)
#' según el año, el país y si se proveyó el conjunto D. El conjunto final tiene
#' las variables necesarias para aplicar [calcular_hogares()]. Las variables
#' que no están disponibles quedan como `NA`.
#'
#' @param .H `data.frame`o `tibble`. Conjunto de datos H de la EU-SILC
#' @param .P `data.frame`o `tibble`. Conjunto de datos P de la EU-SILC expandido por [expandir_personas()]
#' @param .D `data.frame`o `tibble`. Conjunto de datos D de la EU-SILC
#' @param .anio `numeric`. Año al que corresponde el conjunto H
#' @param .pais `character`. País al que corresponde el conjunto H
#' 
#' @returns `tibble`. Conjunto de datos H de la EU-SILC con variables adicionales armonizadas
estandarizar_hogares_ <- function(.H, .P, .D, .anio, .pais) {
  if (!is.null(.D)) {
    .H <- dplyr::left_join(
      x = .H,
      y = dplyr::select(.D, DB010, DB020, DB030, DB040, DB090),
      by = dplyr::join_by(HB010 == DB010, HB020 == DB020, HB030 == DB030)
    )

    cli::cli_bullets(c(
      "v" = "Se proporciono el conjunto D"
    ))
  } else {
    .H <- dplyr::mutate(.H, DB090 = NA)

    cli::cli_bullets(c(
      "!" = "No se proporciono el conjunto D",
      " " = "Se pierde: hi06"
    ))
  }

  if (!attr(.P, "vble. PL230")) {
    cli::cli_bullets(c(
      "!" = "No se encontro PL230 en el conjunto P",
      " " = "Se pierden: py13, py14, py15"
    ))
  }

  return(.H)
}
