#' Advertencias documentadas para la estandarizacion de EU-SILC
#'
#' Contiene perdidas de informacion, problemas de la UDB y diferencias de
#' comparabilidad que afectan variables utilizadas por el paquete.
#' La revisión documental es parcial; la ausencia de registros no garantiza
#' la ausencia de particularidades.
#'
#' @format ## `tabla_advertencias`
#' Un tibble con una fila por advertencia, rango de años, pais y variable:
#' \describe{
#'   \item{id_advertencia}{Identificador estable de la advertencia.}
#'   \item{anio_desde}{Año inicial de la advertencia.}
#'   \item{anio_hasta}{Año final de la advertencia.}
#'   \item{pais}{Codigo de pais de dos letras.}
#'   \item{base}{Flujo de estandarizacion afectado: P o H.}
#'   \item{conjunto_origen}{Conjunto donde se encuentra la variable.}
#'   \item{variable}{Variable original afectada.}
#'   \item{tipo}{Clase de perdida o diferencia documentada.}
#'   \item{advertencia}{Descripcion breve del problema.}
#'   \item{consecuencia}{Resultados del paquete afectados.}
#'   \item{accion_paquete}{Tratamiento que aplica el paquete.}
#'   \item{fuente}{Documento general utilizado como fuente.}
#' }
#' @source Documentacion de EU-SILC disponible con las entregas UDB de
#'   Eurostat.
"tabla_advertencias"
