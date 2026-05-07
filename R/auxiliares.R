#' Title
#'
#' @param .etq df de xlsx de etiquetas
#'
#' @returns lista anidada con etiquetas para etiquetar_eusilc
aux_etiquetas <- function(.etq) {
  etq <- tidyr::nest(.etq, valores = c(etiqueta, valor))
  etq$valores <- purrr::map(etq$valores, tibble::deframe)

  etq_p <- dplyr::filter(etq, conjunto == "P")
  etq_h <- dplyr::filter(etq, conjunto == "H")

  etiquetas <- list()
  etiquetas$P <- list()
  etiquetas$H <- list()

  etiquetas$P$variables <- as.list(tibble::deframe(etq_p[, c("variable_n2", "descripcion")]))
  etiquetas$H$variables <- as.list(tibble::deframe(etq_h[, c("variable_n2", "descripcion")]))

  etiquetas$P$valores <- tibble::deframe(etq_p[!is.na(etq_p$valores), c("variable_n2", "valores")])
  etiquetas$H$valores <- tibble::deframe(etq_h[!is.na(etq_h$valores), c("variable_n2", "valores")])

  return(etiquetas)
}

#' Title
#'
#' @param .imp df de xlsx de imputaciones
#'
#' @returns df de imputaciones para imputar_eusilc
aux_imputaciones <- function(.imp) {
  cols <- c("predictoras_na", "predictoras_full", "parametros")
  .imp <- dplyr::mutate(
    .imp, dplyr::across(dplyr::all_of(cols), function(x) strsplit(x, ","))
  )

  return(.imp)
}
