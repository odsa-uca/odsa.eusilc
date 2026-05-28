#' Construir variables adicionales en los conjuntos de datos H de la EU-SILC
#'
#' @param .H Conjunto de datos H de la EU-SILC.
#' @param .P Conjunto de datos P de la EU-SILC expandido por [expandir_personas()].
#' @param .D Conjunto de datos D de la EU-SILC.
#' @param .expandir Conservar las variables originales en el conjunto de datos final o eliminarlas.
#' @param .etiquetar Aplicar etiquetas a las variables y sus valores
#' @param ... ...
#'
#' @returns Conjunto de datos de la EU-SILC con variables adicionales de uso habitual
#' @export
expandir_hogares <- function(
    .H,
    .P,
    .D = NULL,
    .expandir = FALSE,
    .etiquetar = TRUE,
    ...
) {
  # Chequeos args ------------------------------------------------------------
  errores <- NULL

  if (!is.data.frame(.H)) {
    errores <- c(errores, "x" = "`.H` debe ser un data.frame o tibble.")
  }
  if (!is.data.frame(.P)) {
    errores <- c(errores, "x" = "`.P` debe ser un data.frame o tibble.")
  } else if (is.null(attr(.P, "base"))) {
    errores <- c(errores, "x" = "`.P` debe ser una base P expandida con expandir_personas().")
  } else if (attr(.P, "base") != "P") {
    errores <- c(errores, "x" = "`.P` debe ser una base P.")
  }
  if (!is.null(.D) & !is.data.frame(.D)) {
    errores <- c(errores, "x" = "`.D` debe ser un data.frame o tibble.")
  }
  if (!is.logical(.expandir)) {
    errores <- c(errores, "x" = "`.expandir` debe ser `TRUE` o `FALSE`.")
  }
  if (!is.logical(.etiquetar)) {
    errores <- c(errores, "x" = "`.etiquetar` debe ser `TRUE` o `FALSE`.")
  }

  if(!is.null(errores)) cli::cli_abort(c("Problemas en los argumentos:", errores))

  anio <- unique(.H$HB010)
  pais <- unique(.H$HB020)

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

  # Estandarización ----------------------------------------------------------
  cli::cli_h1("Estandarizacion")

  .H <- estandarizar_hogares_(.H, .P, .D, anio, pais)

  # Calcular vbles -----------------------------------------------------------
  cli::cli_h1("Calcular variables nuevas")

  cli::cli_h2("Agregando ingresos personales")
  P <- agregar_personas(.P)
  .H <- dplyr::left_join(
    x = .H, y = P,
    by = dplyr::join_by(HB010 == pi01, HB020 == pi02, HB030 == pi04)
  )

  cli::cli_h2("Calculando variables nuevas")
  .H <- dplyr::left_join(
    x = .H,
    y = tabla_ppa,
    by = dplyr::join_by(HB010 == PB010, HB020 == PB020)
  )

  .H <- calcular_hogares(.H)

  # Arreglos y devolver ------------------------------------------------------
  attr(.H, "base") <- "H"
  attr(.H, "vbles. D") <- !is.null(.D)
  attr(.H, "vbles. LMH") <- attr(.P, "vble. PL230")
  attr(.H, "expandida") <- .expandir

  if (!.expandir) {
    .H <- dplyr::select(.H, dplyr::any_of(names(etq$H$variables)))
  } else {
    .H <- dplyr::relocate(.H, dplyr::any_of(names(etq$H$variables)))
  }

  if (.etiquetar) {
    .H <- etiquetar_eusilc(.H, .base = "H")
  }

  return(.H)
}
