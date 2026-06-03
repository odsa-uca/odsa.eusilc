#' Armoniza el conjunto de datos P de la EU-SILC
#'
#' @description Aplica una serie de transformaciones sobre el conjunto de datos
#' P de la EU_SILC y lo devuelve con variables armonizadas. Las transformaciones
#' tienen en cuenta el país y el año de la encuesta y si se proporcionaron los
#' conjuntos D y R.
#'
#' @param .P `data.frame` o `tibble`. Conjunto de datos P de la EU-SILC.
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
    .P,
    .D = NULL,
    .R = NULL,
    .imputar = FALSE,
    .expandir = FALSE,
    .etiquetar = TRUE
) {
  # Chequeos args ------------------------------------------------------------
  chequear_bases_personas(.P, .D, .R)

  if (!is.logical(.imputar)) {
    cli::cli_abort(
      c(".imputar debe ser TRUE o FALSE.",
        "x" = "Se paso un {class(.imputar)}"
      ),
      class = "no_logical"
    )
  }
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
  anio <- unique(.P$PB010)
  pais <- unique(.P$PB020)
  
  vble_PL130 <- "PL130" %in% names(.P)
  vble_PL230 <- "PL230" %in% names(.P)

  cli::cli_h1("Estandarizacion")
  .P <- estandarizar_personas_(.P, .R, .D, anio, pais)

  if (.imputar) {
    cli::cli_h1("Imputacion")
    
    .P <- calc_flags_imputacion(.P, anio, pais)
  
    .P <-  imputar_meses(.P)
    .P <-  imputar_horas(.P)
    .P <-  imputar_laboral_a(.P)
    .P <-  imputar_laboral_b(.P, anio)
    if (vble_PL130) {
      .P <- imputar_tamanio(.P)
    }
    if (vble_PL230) {
      .P <- imputar_sectorpp(.P)
    }
  }

  cli::cli_h1("Calcular variables nuevas")
  .P <- calcular_personas_(.P)
  
  chequear_perdidas(.P, "P")

  if (!.expandir) {
    .P <- dplyr::select(.P, dplyr::any_of(names(etq$P$variables)))
  } else {
    .P <- dplyr::relocate(.P, dplyr::any_of(names(etq$P$variables)))
  }

  if (.etiquetar) {
    .P <- etiquetar_eusilc_(.P, .base = "P")
  }
  
  .P <- structure(
    .P,
    "base"        = "P",
    "pre. 2021"   = anio < 2021,
    "vbles. D"    = !is.null(.D),
    "vbles. R"    = !is.null(.R),
    "vble. PL130" = vble_PL130,
    "vble. PL230" = vble_PL230,
    "expandida"   = .expandir,
    "imputada"    = .imputar
  )

  return(.P)
}