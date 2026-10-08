# ----------------------------------------------------------------------------
#' Armoniza variables del conjunto P longitudinal de la EU-SILC
#'
#' @description
#' Construye variables de identificación, demográficas, laborales e ingresos del
#' conjunto P longitudinal de la EU-SILC, suponiendo observaciones de 2021 en
#' adelante. Incorpora ponderadores y demográficos desde R, y región,
#' urbanización y metadatos panel desde D cuando se suministran.
#'
#' @param .P `data.frame` o `tibble`. Conjunto P longitudinal en formato largo,
#'   de un único país, con una fila por persona y año.
#' @param .D `data.frame` o `tibble`, o `NULL` (por defecto). Conjunto D
#'   longitudinal del mismo país y de los años correspondientes.
#' @param .R `data.frame` o `tibble`, o `NULL` (por defecto). Conjunto R
#'   longitudinal del mismo país y de los años correspondientes.
#' @param .imputar `TRUE` o `FALSE` (por defecto). ¿Imputar valores faltantes o
#'   inconsistentes dentro de cada ola? Todavía no tiene efecto.
#' @param .expandir `TRUE` o `FALSE` (por defecto). ¿Conservar las columnas
#'   originales en el resultado? Todavía no tiene efecto: se conservan siempre.
#' @param .etiquetar `TRUE` (por defecto) o `FALSE`. ¿Aplicar etiquetas a las
#'   variables y sus valores? Todavía no tiene efecto.
#'
#' @returns `data.frame` o `tibble` con las variables armonizadas, las columnas
#'   originales y los insumos añadidos durante la estandarización. Conserva las
#'   filas y su orden, incluidos paneles desbalanceados. No agrega atributos de
#'   armonización, imputación o etiquetado.
#'
#' @details
#' A continuación se listan las variables construidas según bloque.
#'
#' ## (I) Identificación
#'
#' - `pi01`. Año de la encuesta.
#' - `pi02`. País.
#' - `pi03`. Región.
#' - `pi04`. Identificador del hogar actual.
#' - `pi05`. Identificador de la persona.
#' - `pi06a`. Ponderador longitudinal de dos años.
#' - `pi06b`. Ponderador longitudinal de tres años.
#' - `pi06c`. Ponderador longitudinal de cuatro años.
#' - `pi06d`. Ponderador longitudinal de cinco años.
#' - `pi06e`. Ponderador longitudinal de seis años.
#' - `pi07`. Urbanización.
#' - `pi08a`. Grupo de rotación.
#' - `pi08b`. Número de encuesta dentro del panel.
#'
#' ## (D) Demográficos
#'
#' - `pd01a`. Edad al momento de la entrevista.
#' - `pd01b`. Edad al final del período de referencia de ingresos.
#' - `pd02`. Sexo.
#' - `pd03`. Nivel educativo.
#' - `pd04`. Estatus migratorio: nacido en el país o en el exterior.
#' - `pd05`. Estatus de ciudadanía: nacional o extranjero.
#'
#' ## (L) Laborales
#'
#' Las variantes A corresponden a ocupados; las B, a no ocupados con ocupación
#' anterior, y las C combinan A para ocupados y B para no ocupados. En B se
#' describe la última ocupación. El sufijo de informalidad distingue categorías,
#' no estos grupos.
#'
#' - `pl01`. Condición de actividad.
#' - `pl02a`, `pl02b`, `pl02c`. Categoría ocupacional (A, B y C).
#' - `pl10a`, `pl10b`, `pl10c`. Ocupación ISCO-08 (A, B y C).
#' - `pl11a`, `pl11b`, `pl11c`. Grupo principal ISCO-08 (A, B y C).
#' - `pl12a`, `pl12b`, `pl12c`. Calificación de la ocupación (A, B y C).
#' - `pl13a`, `pl13b`, `pl13c`. Calificación profesional (A, B y C).
#' - `pl40a`. Informalidad laboral, cuatro categorías.
#' - `pl40b`. Informalidad laboral, dos categorías.
#' - `pl41`. Empleo de calidad.
#'
#' Ninguna de las variables actualmente construidas requiere el módulo
#' *labor market and housing conditions* (LMH).
#'
#' ## (Y) Ingresos
#'
#' Los siguientes ingresos se expresan en moneda nacional por mes, excepto
#' `py11h` y `py12h`, que se expresan en moneda nacional por hora.
#'
#' - `py00`. Ingreso total.
#' - `py10`. Ingreso total por fuentes laborales.
#' - `py11`. Ingreso por trabajo asalariado.
#' - `py12`. Ingreso por trabajo no asalariado.
#' - `py20`. Ingreso total por fuentes no laborales.
#' - `py21`. Ingreso por jubilaciones y pensiones privadas.
#' - `py22`. Ingreso por jubilación.
#' - `py23`. Ingreso por pensión privada.
#' - `py24`. Ingreso por desempleo.
#' - `py25`. Ingreso por otras ayudas.
#' - `py11h`. Ingreso horario por trabajo asalariado.
#' - `py12h`. Ingreso horario por trabajo no asalariado.
#'
#' Para cada uno se construye una variante con sufijo `ppa`, expresada en
#' dólares de paridad de poder adquisitivo (PPA) por mes o por hora, según
#' corresponda. Para más detalles sobre las unidades PPA, ver [tabla_ppa].
#'
#' ## Auxiliares
#'
#' - `ppa_factor`. Factor de conversión de moneda nacional a PPA.
#' - `ppa_factor_us`. Factor de conversión de dólares estadounidenses a PPA.
#' - `maa`. Meses de actividad principal asalariada en el período de referencia
#'   de ingresos.
#' - `man`. Meses de actividad principal no asalariada en ese período,
#'   incluidos trabajadores familiares.
#' - `haa`. Horas anuales aproximadas de trabajo asalariado.
#' - `han`. Horas anuales aproximadas de trabajo no asalariado.
#' - `toc`. Tipo de contrato: escrito o verbal.
#' - `pomj`. Permanencia del trabajo principal: permanente o temporal.
#'
#' ## Disponibilidad y valores faltantes
#'
#' El conjunto de variables armonizadas es siempre el mismo, se proporcionen
#' o no R y D. El conjunto completo de columnas puede variar porque se conservan
#' los insumos originales y los añadidos como `NA` durante la estandarización.
#'
#' Si no están disponibles los insumos necesarios ni una fuente alternativa,
#' las variables dependientes quedan como `NA`. Cuando la ausencia afecta a
#' todo el conjunto, pueden quedar completamente perdidas. Los faltantes se
#' propagan en las sumas de ingresos, sin reemplazarlos por cero.
#'
#' Algunas edades tienen fuentes alternativas. Las variantes laborales C
#' conservan los valores disponibles de A o B según la actividad de cada
#' observación. La informalidad y el empleo de calidad quedan completamente
#' perdidos si algún insumo requerido está completamente perdido, incluso
#' cuando hay información parcial en otros insumos.
#'
#' ## Ingresos horarios aproximados
#'
#' Las horas anuales se aproximan multiplicando los meses de actividad principal
#' correspondiente por las horas habitualmente trabajadas por semana y por 4,2.
#' Los meses de actividad no equivalen a meses de cobro comprobado. Las horas
#' semanales son las actuales: no se reconstruyen las horas históricas de cada
#' empleo durante el período de referencia de ingresos.
#'
#' La aproximación se aplica sólo a ocupados actuales con ingreso correspondiente
#' distinto de cero, meses de actividad positivos y horas semanales positivas.
#' El ingreso horario divide el ingreso anual en moneda nacional por esas horas.
#' Si el ingreso es cero, el horario queda en cero aunque falten meses u horas;
#' si el ingreso es distinto de cero y faltan horas válidas, queda como `NA`.
#'
#' ## Alcance y limitaciones
#'
#' La implementación supone datos desde 2021. Las diferencias nacionales
#' revisadas corresponden a ES, IT, DE, PL y PT. No se recupera el detalle
#' eliminado por agrupaciones, supresiones o perturbaciones de los datos
#' publicados. Esto afecta al nivel educativo en IT y PT y a la clasificación
#' ocupacional en PT; puede limitar también el cálculo del empleo de calidad.
#'
#' No se elige automáticamente un ponderador: debe usarse el adecuado a la
#' duración del seguimiento analizado. Se conserva la estructura de entrada,
#' sin balancear el panel ni restringirlo a personas con ponderador positivo.
#'
#' Se conservan los ingresos negativos. Los factores PPA se incorporan por país
#' y año de encuesta; la tabla interna cubre 2016–2025. Sin coincidencia, los
#' factores y los ingresos PPA quedan como `NA`, sin extrapolación.
#'
#' ## Etapas de armonización
#'
#' La estandarización completa los insumos ausentes, aplica las transformaciones
#' nacionales e incorpora desde R ponderadores y demográficos, y desde D región,
#' urbanización y metadatos panel. Sus reglas de traspaso y tratamiento de
#' insumos se describen en [estandarizar_personas_panel_()].
#'
#' El cálculo construye las variables de los bloques anteriores y convierte
#' los ingresos a unidades mensuales, horarias y PPA. Las reglas específicas se
#' describen en [calcular_personas_panel_()].
#'
#' Quedan pendientes la imputación dentro de cada ola, la selección de columnas
#' mediante `.expandir`, el etiquetado mediante `.etiquetar` y la definición de
#' `pd01c`. Las tres flags todavía no modifican el resultado.
#'
#' @seealso [estandarizar_personas_panel_()], [calcular_personas_panel_()],
#'   [expandir_hogares_panel()]
#' @export
expandir_personas_panel <- function(
  .P,
  .D = NULL,
  .R = NULL,
  .imputar = FALSE,
  .expandir = FALSE,
  .etiquetar = TRUE
) {
  # Estandarización de los conjuntos -----------------------------------------
  .P <- estandarizar_personas_panel_(.P, .D = .D, .R = .R)

  # Imputación dentro de cada ola --------------------------------------------
  if (.imputar) {
    # Pendiente: imputar valores faltantes o inconsistentes dentro de cada ola.
  }

  # Construcción de nuevas variables y recodificación -------------------------
  .P <- calcular_personas_panel_(.P)

  # Selección y ordenamiento de variables ------------------------------------
  # Pendiente: conservar columnas originales según .expandir.

  # Etiquetado de variables y valores ----------------------------------------
  if (.etiquetar) {
    # Pendiente: aplicar las etiquetas correspondientes al panel.
  }

  # Devolución del conjunto panel --------------------------------------------
  return(.P)
}
