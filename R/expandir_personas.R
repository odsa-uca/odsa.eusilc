#' Construir variables adicionales en los conjuntos de datos P de la EU-SILC
#'
#' @param .datos Conjunto de datos P de la EU-SILC.
#' @param .D Conjunto de datos D de la EU-SILC.
#' @param .R Conjunto de datos R de la EU-SILC.
#' @param .imputar Imputar algunas variables insumo.
#' @param .expandir Conservar las variables originales en el conjunto de datos final o eliminarlas.
#' @param .etiquetar Aplicar etiquetas a las variables y sus valores
#' @param ... ...
#'
#' @returns Conjunto de datos de la EU-SILC con variables adicionales de uso habitual.
#' @export
expandir_personas <- function(
    .datos,
    .D = NULL,
    .R = NULL,
    .imputar = FALSE,
    .expandir = FALSE,
    .etiquetar = TRUE,
    ...
) {
  # Chequeos args ------------------------------------------------------------
  errores <- NULL

  if (!is.data.frame(.datos)) {
    errores <- c(errores, "x" = "`.datos` debe ser un data.frame o tibble.")
  }
  if (!is.null(.D) & !is.data.frame(.D)) {
    errores <- c(errores, "x" = "`.D` debe ser un data.frame o tibble.")
  }
  if (!is.null(.R) & !is.data.frame(.R)) {
    errores <- c(errores, "x" = "`.R` debe ser un data.frame o tibble.")
  }
  if (!is.logical(.expandir)) {
    errores <- c(errores, "x" = "`.expandir` debe ser `TRUE` o `FALSE`.")
  }

  if(!is.null(errores)) cli::cli_abort(c("Problemas en los argumentos:", errores))

  anio <- unique(.datos$PB010)
  pais <- unique(.datos$PB020)

  if (length(anio) > 1) {
    cli::cli_abort(c(
      "Solo se aceptan bases de un unico anio",
      "x" = "Se proporciono una base para {anio}."
    ))
  }
  if (length(pais) > 1) {
    cli::cli_abort(c(
      "Solo se aceptan bases de un unico pais",
      "x" = "Se proporciono una base para {pais}"
    ))
  }

  # Estandarizacion ----------------------------------------------------------
  cli::cli_h1("Estandarizacion")

  lmh  <- "PL230" %in% names(.datos)
  .datos <- estandarizar_personas(.datos, anio, pais, .D, .R, lmh)

  # Imputaciones -------------------------------------------------------------
  if (.imputar) {
    cli::cli_h1("Imputacion")

    .datos <- imputar_personas(.datos, anio, lmh)
  }

  # Calcular vbles -----------------------------------------------------------
  cli::cli_h1("Calcular variables nuevas")

  if (!all(c("maa", "man") %in% names(.datos))) {
    .datos <- dplyr::mutate(
      .datos,
      maa = PL073 + PL074,
      man = PL075 + PL076
    )
  }

  .datos <- calcular_personas(.datos, anio, lmh)

  # Arreglos y devolver ------------------------------------------------------
  if (!.expandir) {
    .datos <- dplyr::select(.datos, dplyr::any_of(names(etq$P$variables)))
  } else {
    .datos <- dplyr::relocate(.datos, dplyr::any_of(names(etq$P$variables)))
  }

  if (.etiquetar) {
    .datos <- etiquetar_eusilc(.datos, .base = "P")
  }

  attr(.datos, "base")       <- "P"
  attr(.datos, "pre. 2021")  <- anio < 2021
  attr(.datos, "vbles. D")   <- !is.null(.D)
  attr(.datos, "vbles. R")   <- !is.null(.R)
  attr(.datos, "vbles. LMH") <- lmh
  attr(.datos, "expandida")  <- .expandir
  attr(.datos, "imputada")   <- !is.null(attr(.datos, "imputada"))

  return(.datos)
}
