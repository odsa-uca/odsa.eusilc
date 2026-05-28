#' Title
#'
#' @param .datos datos
#'
#' @returns datos etiquetados
#' @export
etiquetar_eusilc <- function(.datos) {
  if (is.null(attr(.datos, "base"))) {
    rlang::abort("`.datos` debe ser un conjunto de datos EUSILC expandido.")
  }
  base <- attr(.datos, "base")
  
  etiquetar_eusilc_(.datos, base)
}

# ============================================================================
#' Title
#'
#' @param .datos datos
#' @param .base base
#'
#' @returns datos etiquetados
etiquetar_eusilc_ <- function(.datos, .base) {

  .datos <- labelled::set_variable_labels(
    .datos,
    .labels = etq[[.base]]$variables,
    .strict = FALSE
  )
  .datos <- labelled::set_value_labels(
    .datos,
    .labels = etq[[.base]]$valores,
    .strict = FALSE
  )

  return(.datos)
}
