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

# ============================================================================
#' Title
#'
#' @param .etq df de xlsx de etiquetas
#'
#' @returns lista anidada con etiquetas para etiquetar_eusilc
armar_etiquetas <- function(.etq) {
  etq <- tidyr::nest(.etq, valores = c(etiqueta, valor))
  etq$valores <- purrr::map(etq$valores, tibble::deframe)

  etq_p <- dplyr::filter(etq, conjunto == "P")
  etq_h <- dplyr::filter(etq, conjunto == "H")

  etiquetas <- list()
  etiquetas$P <- list()
  etiquetas$H <- list()

  etiquetas$P$variables <- as.list(tibble::deframe(etq_p[, c("variable", "descripcion")]))
  etiquetas$H$variables <- as.list(tibble::deframe(etq_h[, c("variable", "descripcion")]))

  etiquetas$P$valores <- tibble::deframe(etq_p[!is.na(etq_p$valores), c("variable", "valores")])
  etiquetas$H$valores <- tibble::deframe(etq_h[!is.na(etq_h$valores), c("variable", "valores")])

  return(etiquetas)
}
