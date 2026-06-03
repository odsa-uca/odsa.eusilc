#' Tabla de factores de conversión a unidades de Paridad de Poder Adquisitivo
#'
#' @description
#' Indica el valor del factor de conversión de unidades de la moneda nacional a
#' unidades de paridad de poder adquisitivo (PPA) relativo a la Unión Europea 27
#' países.
#' 
#' @details
#' La tabla se utiliza para convertir los montos de ingreso de los diferentes
#' países a unidades de PPA en dólares. [calcular_personas()] y
#' [calcular_hogares()] utilizan las variables PX010 y HX010 (_change rate euro
#' to national currency_) para convertir los montos a moneda nacional,
#' `ppa_factor` para convertirlos a unidades de PPA en euros de la Unión
#' Europea 27 países y finalmente `ppa_factor_us` para convertirlos a unidades
#' de PPA en dólares estadounidenses.
#' 
#' \out{a + b}
#'
#' @format tabla_ppa
#' A tibble: 56 x 3
#' \describe{
#'   \item{PB010}{Año}
#'   \item{PB020}{País}
#'   \item{ppa_factor}{Factor de conversión a unidades de PPA de la Unión Europea 27 países}
#'   \item{ppa_factor_us}{Factor de conversión de dólares a unidades de PPA de la Unión Europea 27 países}
#' }
#' @source <https://ec.europa.eu/eurostat/web/purchasing-power-parities/database>
"tabla_ppa"
