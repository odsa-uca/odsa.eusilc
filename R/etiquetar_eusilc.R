#' Title
#'
#' @param .datos datos
#' @param .base base
#' @param ... ...
#'
#' @returns datos etiquetados
#' @export
etiquetar_eusilc <- function(.datos, .base = NULL, ...) {
  if (is.null(.base) & is.null(attr(.datos, "base"))) {
    rlang::abort("`.datos` debe ser un conjunto de datos EUSILC expandido.")
  } else if (is.null(.base)) {
    .base <- attr(.datos, "base")
  }

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
