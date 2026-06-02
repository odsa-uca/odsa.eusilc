#' Etiqueta un conjunto de datos EU-SILC armonizado
#' 
#' @description
#' Aplica etiquetas a las variables y los valores de un conjunto de datos
#' EU-SILC armonizado con [estandarizar_personas()] y [calcular_personas()].
#' Sólo etiqueta las variables nuevas, no etiqueta las originales. Las
#' etiquetas aplicadas se pueden ver en [etiquetas].
#'
#' @param .datos `data.frame` o `tibble`. Conjunto de datos armonizado P o H de la EU-SILC
#'
#' @returns `tibble`. Conjunto de datos armonizado P o H con variables y valores etiquetados
#' @export
etiquetar_eusilc <- function(.datos) {
  if (is.null(attr(.datos, "expandida"))) {
    cli::cli_abort(".datos debe ser un conjunto de datos EUSILC expandido.")
  }
  base <- attr(.datos, "base")

  etiquetar_eusilc_(.datos, base)
}

# ============================================================================
#' Etiqueta un conjunto de datos EU-SILC armonizado (interna)
#' 
#' @description
#' ¡Esta función es interna! Aplica etiquetas a las variables y los valores de
#' un conjunto de datos EU-SILC armonizado con [estandarizar_personas()] y
#' [calcular_personas()]. Sólo etiqueta las variables nuevas, no etiqueta las
#' originales. Las etiquetas aplicadas se pueden ver en [etiquetas].
#' 
#' @details
#' Esta función es el núcleo interno de [etiquetar_eusilc()]. Para más detalles,
#' consultar la documentación de esa función.
#'
#' @param .datos `data.frame` o `tibble`. Conjunto de datos armonizado P o H de la EU-SILC
#' @param .base `character`, "P" o "H". ¿Qué tipo de conjunto se debe etiquetar?
#'
#' @returns `tibble`. Conjunto de datos armonizado P o H con variables y valores etiquetados
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
#' Convierte un data frame de etiquetas en una lista anidada
#' 
#' @description
#' Toma un data frame con etiquetas de variables y valores y lo convierte en
#' una lista anidada de etiquetas para aplicar con [etiquetar_eusilc()]. La
#' función tiene un rol auxiliar de desarrollo.
#'
#' @param .etq `tibble`. Data frame con etiquetas de variables y valores
#'
#' @returns `list`. Lista anidada con etiquetas para [etiquetar_eusilc()].
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
