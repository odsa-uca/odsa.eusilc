#' Armoniza el conjunto de datos H de la EU-SILC
#' 
#' @description
#' Aplica una serie de transformaciones sobre los conjuntos de datos H y P de
#' la EU-SILC y devuelve un conjunto de datos de nivel hogar con variables
#' armonizadas. Las transformaciones tienen en cuenta el país y el año de la
#' encuesta y si se proporciona el conjunto de datos D.
#'
#' @param .H `data.frame`o `tibble`. Conjunto de datos H de la EU-SILC
#' @param .P `data.frame`o `tibble`. Conjunto de datos P de la EU-SILC expandido por [expandir_personas()]
#' @param .D `data.frame`o `tibble`. Conjunto de datos D de la EU-SILC
#' @param .expandir `TRUE` o `FALSE` (por defecto). ¿Conservar las variables originales en el conjunto de datos final?
#' @param .etiquetar `TRUE` (por defecto) o `FALSE`. ¿Aplicar etiquetas a las variables y sus valores?
#'
#' @returns `tibble`. Conjunto de datos de la EU-SILC con variables adicionales armonizadas
#' @export
expandir_hogares <- function(
    .H,
    .P,
    .D = NULL,
    .expandir = FALSE,
    .etiquetar = TRUE
) {
  # Chequeos args ------------------------------------------------------------
  chequear_bases_hogares(.H, .P, .D)

  if (!is.logical(.expandir)) {
    cli::cli_abort(
      c(".etiquetar debe ser TRUE o FALSE.",
        "x" = "Se paso un {class(.expandir)}"
      ),
      class = "no_logical"
    )
  }
  if (!is.logical(.etiquetar)) {
    cli::cli_abort(
      c(".etiquetar debe ser TRUE o FALSE.",
        "x" = "Se paso un {class(.etiquetar)}"
      ),
      class = "no_logical"
    )
  }
  
  # --------------------------------------------------------------------------
  anio <- unique(.H$HB010)
  pais <- unique(.H$HB020)

  cli::cli_h1("Estandarizacion")
  .H <- estandarizar_hogares_(.H, .D, anio, pais)

  cli::cli_h1("Calcular variables nuevas")
  P <- agregar_personas(.P)
  .H <- dplyr::left_join(
    x = .H, y = P,
    by = dplyr::join_by(HB010 == pi01, HB020 == pi02, HB030 == pi04)
  )
  .H <- calcular_hogares_(.H)
  
  chequear_perdidas(.H, "H")

  if (!.expandir) {
    .H <- dplyr::select(.H, dplyr::any_of(names(etq$H$variables)))
  } else {
    .H <- dplyr::relocate(.H, dplyr::any_of(names(etq$H$variables)))
  }

  if (.etiquetar) {
    .H <- etiquetar_eusilc_(.H, .base = "H")
  }
  
  .H <- structure(
    .H,
    "base"      = "H",
    "vbles. D"  = !is.null(.D),
    "vbles. LMH"= attr(.P, "vble. PL230"),
    "expandida" = .expandir
  )

  return(.H)
}