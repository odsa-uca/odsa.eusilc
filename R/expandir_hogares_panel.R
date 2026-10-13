# ----------------------------------------------------------------------------
#' Armoniza variables del conjunto H longitudinal de la EU-SILC
#'
#' @description
#' Construye variables de identificación, tamaño, ingresos y perceptores del
#' conjunto H longitudinal de la EU-SILC, suponiendo observaciones de 2021 en
#' adelante. Incorpora ponderador, región y urbanización desde D, y agrega los
#' ingresos de las personas cubiertas por P por país, año y hogar actual.
#'
#' @param .H `data.frame` o `tibble`. Conjunto H longitudinal en formato largo,
#'   de un único país, con una fila por hogar y año.
#' @param .P `data.frame` o `tibble`. Conjunto P longitudinal armonizado por
#'   [expandir_personas_panel()], del mismo país y de los años correspondientes.
#' @param .D `data.frame` o `tibble`, o `NULL` (por defecto). Conjunto D
#'   longitudinal del mismo país y de los años correspondientes.
#' @param .imputar `TRUE` o `FALSE` (por defecto). ¿Imputar valores faltantes o
#'   inconsistentes dentro de cada ola? Todavía no tiene efecto.
#' @param .expandir `TRUE` o `FALSE` (por defecto). ¿Conservar las columnas
#'   originales en el resultado?
#' @param .etiquetar `TRUE` (por defecto) o `FALSE`. ¿Aplicar etiquetas a las
#'   variables y sus valores?
#'
#' @returns `data.frame` o `tibble` con las variables armonizadas.
#'
#' @details
#' A continuación se listan las variables construidas según bloque.
#'
#' ## (I) Identificación
#'
#' - `hi01`. Año de la encuesta.
#' - `hi02`. País.
#' - `hi03`. Región.
#' - `hi04`. Identificador del hogar.
#' - `hi06`. Ponderador longitudinal del hogar (`DB095`).
#' - `hi07`. Urbanización.
#'
#' ## (D) Demográficos
#'
#' - `hd01`. Tamaño total del hogar.
#'
#' ## (Y) Ingresos
#'
#' Todos los ingresos se expresan en moneda nacional por mes. Los `py*` son
#' sumas de los ingresos de las personas cubiertas por P.
#'
#' - `py00`. Ingreso total de las personas.
#' - `py10`. Ingreso personal por fuentes laborales.
#' - `py11`. Ingreso personal por trabajo asalariado.
#' - `py12`. Ingreso personal por trabajo no asalariado.
#' - `py20`. Ingreso personal por fuentes no laborales.
#' - `py21`. Ingreso personal por jubilaciones y pensiones privadas.
#' - `py22`. Ingreso personal por jubilación.
#' - `py23`. Ingreso personal por pensión privada.
#' - `py24`. Ingreso personal por desempleo.
#' - `py25`. Ingreso personal por otras ayudas.
#' - `hy00`. Ingreso total del hogar.
#' - `hy20`. Ingreso del hogar por fuentes no laborales.
#' - `hy21`. Ingreso por inversiones y otras transferencias.
#' - `hy22`. Ingreso por inversiones.
#' - `hy23`. Ingreso por otras transferencias.
#' - `hy24`. Ingreso total por política social.
#' - `hy25`. Ingreso total por transferencias.
#' - `hy26`. Ingreso por asistencia social.
#'
#' Para cada ingreso se construyen variantes con sufijos `pc` (moneda nacional
#' por persona y mes), `ppa` (dólares PPA por mes) y `pcppa` (dólares PPA por
#' persona y mes). Para más detalles sobre las unidades PPA, ver [tabla_ppa].
#'
#' ## (P) Perceptores
#'
#' - `hp00`. Perceptores de ingreso total.
#' - `hp10`. Perceptores de ingreso por fuentes laborales.
#' - `hp11`. Perceptores de ingreso por trabajo asalariado.
#' - `hp12`. Perceptores de ingreso por trabajo no asalariado.
#' - `hp20`. Perceptores de ingreso por fuentes no laborales.
#' - `hp21`. Perceptores de jubilaciones y pensiones privadas.
#' - `hp22`. Perceptores de jubilación.
#' - `hp23`. Perceptores de pensión privada.
#' - `hp24`. Perceptores de ingreso por desempleo.
#' - `hp25`. Perceptores de ingreso por otras ayudas.
#'
#' Se cuenta a las personas con ingreso correspondiente distinto de cero,
#' incluidos ingresos negativos. Los conteos no están ponderados.
#'
#' ## Auxiliares
#'
#' - `ppa_factor`. Factor de conversión de moneda nacional a PPA.
#' - `ppa_factor_us`. Factor de conversión de dólares estadounidenses a PPA.
#'
#' Ninguna variable actualmente construida requiere el módulo *labor market
#' and housing conditions* (LMH).
#'
#' ## Disponibilidad y valores faltantes
#'
#' El conjunto de variables armonizadas es siempre el mismo, se proporcione
#' o no D. Con `.expandir = TRUE`, el conjunto completo de columnas puede variar
#' porque se conservan los insumos originales y los añadidos como `NA` durante
#' la estandarización.
#' Los identificadores `HB010`, `HB020`, `HB030` en H y `pi01`, `pi02`, `pi04`
#' en P deben estar presentes.
#'
#' Las columnas de insumo opcionales ausentes se completan con `NA`. Los
#' faltantes se propagan en las sumas y conteos: un ingreso personal faltante
#' deja como `NA` su agregado y su número de perceptores en ese hogar-año.
#' Si no hay personas coincidentes, incluso si P está vacío, ambos quedan
#' como `NA`, no como cero. No se reconstruyen componentes ausentes desde un
#' total oficial ni se sustituye `hy00` por el ingreso disponible `HY020`.
#'
#' ## Cobertura de personas
#'
#' P se agrega por `pi01`, `pi02` y `pi04`, y luego se incorpora a H por país,
#' año y hogar. El hogar actual de una persona puede cambiar entre olas. No se
#' filtra P por ponderadores positivos ni por trayectorias completas. La
#' cobertura de los miembros pertinentes debe evaluarla el usuario: las sumas
#' corresponden a las personas disponibles y no acreditan cobertura total.
#' `hd01` procede del tamaño publicado, no del número de filas de P.
#'
#' ## Alcance y limitaciones
#'
#' La implementación supone datos desde 2021, en continuidad con personas
#' panel y los países revisados ES, IT, DE, PL y PT. Se conserva el detalle
#' regional y de urbanización publicado, sin equiparar niveles regionales
#' nacionales ni recuperar categorías agrupadas o suprimidas. Por ejemplo,
#' DE agrupa categorías de urbanización en algunas entregas y suprime región
#' para determinados hogares. No se reconstruyen hogares suprimidos.
#'
#' `hi06` conserva el peso longitudinal de D; no usa el ponderador transversal
#' `DB090`. No se balancea el panel ni se crean observaciones.
#'
#' ## Etapas de armonización
#'
#' La estandarización completa los insumos ausentes e incorpora desde D
#' ponderación, región y urbanización. Sus reglas de preparación y traspaso
#' se describen en [estandarizar_hogares_panel_()].
#'
#' La expansión agrega los ingresos personales y los conteos de perceptores
#' por país, año y hogar actual, y los incorpora a H. El cálculo construye las
#' variables del hogar y las variantes de ingresos per cápita y PPA. Las
#' recodificaciones y conversiones se describen en [calcular_hogares_panel_()].
#'
#' Con `.expandir = FALSE` se seleccionan las columnas presentes contempladas
#' en el catálogo de [etiquetas] para H. Con `.expandir = TRUE` se conservan
#' todas las columnas y se colocan primero las contempladas en ese catálogo.
#' La selección no crea variables pendientes de construcción; si alguna ya
#' estaba en H y figura en el catálogo, puede conservarse en el resultado.
#'
#' Con `.etiquetar = TRUE` se asignan etiquetas a las variables y sus valores
#' según [etiquetas]. El etiquetado no modifica los códigos de las categorías.
#'
#' Quedan pendientes la tipología del hogar, las variables sectoriales y la
#' imputación dentro de cada ola. La flag `.imputar` todavía no tiene efecto.
#'
#' @seealso [estandarizar_hogares_panel_()], [calcular_hogares_panel_()],
#'   [expandir_personas_panel()]
#' @export
expandir_hogares_panel <- function(
  .H,
  .P,
  .D = NULL,
  .imputar = FALSE,
  .expandir = FALSE,
  .etiquetar = TRUE
) {
  # Estandarización de los conjuntos -----------------------------------------
  .H <- estandarizar_hogares_panel_(.H, .P, .D)
  
  # Agregación de la información personal ------------------------------------
  ingresos_personas <- c(
    "py00",
    "py10",
    "py11",
    "py12",
    "py20",
    "py21",
    "py22",
    "py23",
    "py24",
    "py25"
  )
  for (variable in setdiff(ingresos_personas, names(.P))) {
    .P[[variable]] <- rep(NA_real_, nrow(.P))
  }
  
  personas <- dplyr::select(
    .P,
    pi01,
    pi02,
    pi04,
    dplyr::all_of(ingresos_personas)
  )
  if (nrow(personas) > 0L) {
    personas <- agregar_personas(personas)
  } else {
    personas <- dplyr::mutate(
      personas,
      dplyr::across(
        dplyr::all_of(ingresos_personas),
        \(y) as.integer(y != 0),
        .names = "x{.col}"
      )
    )
    personas <- dplyr::rename_with(
      personas,
      .fn = \(nombre) sub("xpy", "hp", nombre),
      .cols = dplyr::starts_with("xpy")
    )
  }

  # Incorporación de los agregados personales --------------------------------
  .H <- dplyr::left_join(
    x = dplyr::select(
      .H,
      -dplyr::any_of(setdiff(names(personas), c("pi01", "pi02", "pi04")))
    ),
    y = personas,
    by = dplyr::join_by(HB010 == pi01, HB020 == pi02, HB030 == pi04),
    relationship = "many-to-one"
  )

  # Imputación dentro de cada ola --------------------------------------------
  if (.imputar) {
    # Pendiente: imputar valores faltantes o inconsistentes dentro de cada ola.
  }

  # Construcción de nuevas variables y recodificación -------------------------
  .H <- calcular_hogares_panel_(.H)

  # Selección y ordenamiento de variables ------------------------------------
  if (!.expandir) {
    .H <- dplyr::select(.H, dplyr::any_of(names(etiquetas_$H$variables)))
  } else {
    .H <- dplyr::relocate(.H, dplyr::any_of(names(etiquetas_$H$variables)))
  }

  # Etiquetado de variables y valores ----------------------------------------
  if (.etiquetar) {
    .H <- etiquetar_eusilc_(.H, .base = "H")
  }

  # Devolución del conjunto panel --------------------------------------------
  return(.H)
}
