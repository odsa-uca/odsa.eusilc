#' Construir variables adicionales en los conjuntos de datos P de la EU-SILC
#'
#' @param .datos Conjunto de datos P de la EU-SILC.
#' @param .D Conjunto de datos D de la EU-SILC.
#' @param .R Conjunto de datos R de la EU-SILC.
#' @param .expandir Conservar las variables originales en el conjunto de datos final o eliminarlas.
#' @param ... ...
#'
#' @returns Conjunto de datos de la EU-SILC con variables adicionales de uso habitual.
#' @export
expandir_personas <- function(
    .datos,
    .D = NULL,
    .R = NULL,
    .expandir = FALSE,
    ...
) {
  # Chequeos args ------------------------------------------------------------
  errores <- NULL

  if (!is.data.frame(.datos)) {
    errores <- c(errores, "*" = "`.datos` debe ser un data.frame o tibble.")
  }
  if (!is.null(.D) & !is.data.frame(.D)) {
    errores <- c(errores, "*" = "`.D` debe ser un data.frame o tibble.")
  }
  if (!is.null(.R) & !is.data.frame(.R)) {
    errores <- c(errores, "*" = "`.R` debe ser un data.frame o tibble.")
  }
  if (!is.logical(.expandir)) {
    errores <- c(errores, "*" = "`.expandir` debe ser `TRUE` o `FALSE`.")
  }

  if(!is.null(errores)) rlang::abort(c("Problemas en los argumentos:", errores))

  # Estandarizacion ----------------------------------------------------------
  anio <- unique(.datos$PB010)
  lmh  <- "PL230" %in% names(.datos)

  datos_estandar <- estandarizar_personas(.datos, anio, .D, .R, lmh)

  .datos   <- datos_estandar$datos
  mensajes <- datos_estandar$mensajes

  # Imputaciones -------------------------------------------------------------

  # Calcular vbles -----------------------------------------------------------
  if (!all(c("maa", "man") %in% names(.datos))) {
    .datos <- dplyr::mutate(
      .datos,
      maa = PL073 + PL074,
      man = PL075 + PL076
    )
  }
  .datos <- lookup_personas(.datos, lmh)
  .datos <- calcular_personas(.datos, lmh)

  # Arreglos y devolver ------------------------------------------------------
  if (!.expandir) {
    .datos <- dplyr::select(.datos, dplyr::any_of(names(etq$P$variables)))
  } else {
    .datos <- dplyr::relocate(.datos, dplyr::any_of(names(etq$P$variables)))
  }

  attr(.datos, "base")       <- "P"
  attr(.datos, "pre. 2021")  <- anio < 2021
  attr(.datos, "vbles. D")   <- !is.null(.D)
  attr(.datos, "vbles. R")   <- !is.null(.R)
  attr(.datos, "vbles. LMH") <- lmh
  attr(.datos, "expandida")  <- .expandir
  attr(.datos, "imputada")   <- !is.null(attr(.datos, "imputada"))

  if (!is.null(mensajes)) rlang::warn(c("Ojo!", mensajes))

  return(.datos)
}
