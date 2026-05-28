#' Title
#'
#' @param .datos Conjunto de datos H
#' @param .anio Año de la encuesta
#' @param .P Conjunto de datos P
#' @param .D Conjunto de datos D
#' @param .pais Pais de la encuesta
#'
#' @returns Conjunto de datos H estandarizado
#' @export
estandarizar_hogares <- function(.datos, .P, .D, .anio, .pais) {
  if (!is.null(.D)) {
    .datos <- dplyr::left_join(
      x = .datos,
      y = dplyr::select(.D, DB010, DB020, DB030, DB040, DB090),
      by = dplyr::join_by(HB010 == DB010, HB020 == DB020, HB030 == DB030)
    )

    cli::cli_bullets(c(
      "v" = "Se proporciono el conjunto D"
    ))
  } else {
    .datos <- dplyr::mutate(.datos, DB090 = NA)

    cli::cli_bullets(c(
      "!" = "No se proporciono el conjunto D",
      " " = "Se pierde: hi06"
    ))
  }

  if (!attr(.P, "vble. PL230")) {
    cli::cli_bullets(c(
      "!" = "No se encontro PL230 en el conjunto P",
      " " = "Se pierden: py13, py14, py15"
    ))
  }

  return(.datos)
}
