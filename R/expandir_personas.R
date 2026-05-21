#' Armoniza el conjunto de datos P de la EU-SILC
#'
#' @description Aplica una serie de transformaciones sobre el conjunto de datos
#' P de la EU_SILC y lo devuelve con variables armonizadas. Las transformaciones
#' tienen en cuenta el país y el año de la encuesta y si se proporcionaron los
#' conjuntos D y R.
#'
#' @param .datos `data.frame` o `tibble`. Conjunto de datos P de la EU-SILC.
#' @param .D `data.frame` o `tibble`. Conjunto de datos D de la EU-SILC.
#' @param .R `data.frame` o `tibble`. Conjunto de datos R de la EU-SILC.
#' @param .imputar `TRUE` o `FALSE` (por defecto). ¿Se aplican imputaciones
#'   sobre las variables insumo?
#' @param .expandir `TRUE` o `FALSE` (por defecto). ¿Conservar las variables
#'   originales en el conjunto de datos final?
#' @param .etiquetar `TRUE` (por defecto) o `FALSE`. ¿Aplicar etiquetas a las
#'   variables y sus valores?
#'
#' @returns `tibble`. Conjunto de datos de la EU-SILC con variables adicionales
#'   armonizadas.
#'
#' @details La función encadena cuatro grandes operaciones sobre los datos:
#'
#' ## Estandarización
#'
#' Se aplican ciertas transformaciones a las variables dependiendo del país, el
#' año y si se proveyeron los conjuntos D y R. Su propósito es que el conjunto P
#' se tenga un formato estándar, similar al de las EU-SILC posteriores a 2021,
#' para facilitar los pasos siguientes. Para más detalles, ver
#' [estandarizar_personas()].
#'
#' ## Imputación (opcional)
#'
#' Se imputan valores nulos o faltantes de variables relacionadas con
#'
#' * las horas semanales de trabajo,
#' * la cantidad de meses con ingresos en el período de referencia correspondiente y
#' * algunas características de la ocupación y el lugar de trabajo.
#'
#' Para más detalles, ver [imputar_personas()].
#'
#' ## Cálculo de nuevas variables
#'
#' Se construyen nuevas variables a partir de
#' las preexistentes. Estas se agrupan en cuatro bloques:
#'
#' * I: Identificación
#' * D: Demográficos
#' * L: Laborales
#' * Y: Ingresos
#'
#' Para más detalles, ver [calcular_personas()] o examinar el conjunto [etiquetas].
#'
#' ## Etiquetado (opcional)
#'
#' Se asignan etiquetas a las variables y a sus categorías cuando son factores.
#' Las etiquetas correspondientes a cada variable se pueden examinar en el
#' conjunto [etiquetas].
#'
#' El conjunto de datos final debería tener las mismas variables construidas sin
#' importar el año, el país o si se proporcionaron los conjuntos D y R.
#'
#' @export
expandir_personas <- function(
    .datos,
    .D = NULL,
    .R = NULL,
    .imputar = FALSE,
    .expandir = FALSE,
    .etiquetar = TRUE
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

  lmh <- "PL230" %in% names(.datos)
  .datos <- estandarizar_personas(.datos, .D, .R, anio, pais, lmh)

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
  attr(.datos, "imputada")   <- .imputar

  return(.datos)
}
