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
#'   originales en el resultado? Todavía no tiene efecto: se conservan siempre.
#' @param .etiquetar `TRUE` (por defecto) o `FALSE`. ¿Aplicar etiquetas a las
#'   variables y sus valores? Todavía no tiene efecto.
#'
#' @returns `data.frame` o `tibble` con las variables armonizadas, las columnas
#'   originales y los insumos añadidos durante la estandarización. Conserva las
#'   filas y su orden, incluidos paneles desbalanceados. Reemplaza agregados,
#'   factores PPA y variables armonizadas homónimos preexistentes. No agrega
#'   atributos de armonización, imputación o etiquetado.
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
#' sumas de los ingresos de las personas cubiertas por P, no promedios.
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
#' o no D. El conjunto completo de columnas puede variar porque se conservan
#' los insumos originales y los añadidos como `NA` durante la estandarización.
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
#' ## Traspaso y cobertura de personas
#'
#' Desde D se incorporan `DB095`, `DB040` y `DB100` por país, año y hogar:
#' `HB010/HB020/HB030` en H y `DB010/DB020/DB030` en D. Cada clave debe
#' identificar una única fila en D. Si se suministra D, sus valores reemplazan
#' los insumos correspondientes de H, incluso cuando faltan o no hay
#' coincidencia; en esos casos quedan como `NA`. Sin D, se conservan los
#' insumos ya incorporados en H y se completan los ausentes.
#'
#' P se agrega por `pi01`, `pi02` y `pi04`, y luego se incorpora a H por país,
#' año y hogar. El hogar actual de una persona puede cambiar entre olas. No se
#' filtra P por ponderadores positivos ni por trayectorias completas. La
#' cobertura de los miembros pertinentes debe evaluarla el usuario: las sumas
#' corresponden a las personas disponibles y no acreditan cobertura total.
#' `hd01` procede del tamaño publicado, no del número de filas de P.
#'
#' ## Conversión de ingresos
#'
#' Los componentes netos propios de H, publicados en euros anuales, se
#' convierten con `HX010 / 12` antes de sumarlos a los ingresos personales.
#' P ya está en moneda nacional mensual y no se convierte nuevamente. Se
#' conservan ingresos negativos y no se imputan componentes faltantes.
#'
#' Las variantes per cápita dividen el ingreso por `hd01` sólo cuando el tamaño
#' es positivo y está disponible; en otro caso quedan como `NA`. Las variantes
#' PPA usan `ingreso / ppa_factor * ppa_factor_us`. Los factores se incorporan
#' por país y año de encuesta, reemplazando factores homónimos preexistentes.
#' La tabla interna cubre 2016–2025; sin coincidencia, factores y variantes PPA
#' quedan como `NA`, sin extrapolación.
#'
#' ## Alcance y etapas pendientes
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
#' Quedan pendientes la tipología del hogar, las variables sectoriales, la
#' imputación dentro de cada ola, la selección mediante `.expandir` y el
#' etiquetado mediante `.etiquetar`. Las tres flags todavía no modifican el
#' resultado.
#'
#' @seealso [expandir_personas_panel()]
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
  insumos_h <- c(
    "HY040N",
    "HY050N",
    "HY060N",
    "HY070N",
    "HY080N",
    "HY090N",
    "HY110N",
    "HX010",
    "HX040"
  )
  for (variable in setdiff(insumos_h, names(.H))) {
    .H[[variable]] <- rep(NA_real_, nrow(.H))
  }
  insumos_d <- list(
    DB095 = NA_real_,
    DB040 = NA_character_,
    DB100 = NA_integer_
  )
  if (is.null(.D)) {
    for (variable in setdiff(names(insumos_d), names(.H))) {
      .H[[variable]] <- rep(insumos_d[[variable]], nrow(.H))
    }
  } else {
    for (variable in setdiff(names(insumos_d), names(.D))) {
      .D[[variable]] <- rep(insumos_d[[variable]], nrow(.D))
    }
  }
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

  # Traspaso de variables desde D --------------------------------------------
  if (!is.null(.D)) {
    .H <- dplyr::left_join(
      x = dplyr::select(.H, -dplyr::any_of(names(insumos_d))),
      y = dplyr::select(
        .D,
        DB010,
        DB020,
        DB030,
        dplyr::all_of(names(insumos_d))
      ),
      by = dplyr::join_by(HB010 == DB010, HB020 == DB020, HB030 == DB030),
      relationship = "many-to-one"
    )
  }

  # Agregación de la información personal ------------------------------------
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
  .H <- dplyr::mutate(
    .H,
    # Bloque I --------------------------------------------------------------
    hi01 = HB010,
    hi02 = HB020,
    hi03 = DB040,
    hi04 = HB030,
    hi06 = DB095,
    hi07 = dplyr::recode_values(
      DB100,
      from = tabla_pi07$DB100,
      to = tabla_pi07$pi07,
      default = NA_integer_
    ),
    # Bloque D --------------------------------------------------------------
    hd01 = HX040,
    # Bloque Y --------------------------------------------------------------
    hy00 = py00 +
      (HY040N + HY050N + HY060N + HY070N + HY080N + HY090N + HY110N) *
        HX010 /
        12,
    hy20 = py20 +
      (HY040N + HY050N + HY060N + HY070N + HY080N + HY090N + HY110N) *
        HX010 /
        12,
    hy21 = (HY040N + HY080N + HY090N + HY110N) * HX010 / 12,
    hy22 = (HY040N + HY090N) * HX010 / 12,
    hy23 = (HY080N + HY110N) * HX010 / 12,
    hy24 = py21 + py24 + py25 + (HY050N + HY060N + HY070N) * HX010 / 12,
    hy25 = py24 + py25 + (HY050N + HY060N + HY070N) * HX010 / 12,
    hy26 = (HY050N + HY060N + HY070N) * HX010 / 12
  )

  # Ingresos per cápita y en unidades de PPA ----------------------------------
  ingresos <- c(
    ingresos_personas,
    "hy00",
    "hy20",
    "hy21",
    "hy22",
    "hy23",
    "hy24",
    "hy25",
    "hy26"
  )
  .H <- dplyr::left_join(
    x = dplyr::select(.H, -dplyr::any_of(c("ppa_factor", "ppa_factor_us"))),
    y = tabla_ppa_,
    by = dplyr::join_by(HB010 == PB010, HB020 == PB020),
    relationship = "many-to-one"
  )
  .H <- dplyr::mutate(
    .H,
    dplyr::across(
      dplyr::all_of(ingresos),
      \(y) dplyr::if_else(!is.na(hd01) & hd01 > 0, y / hd01, NA_real_),
      .names = "{.col}pc"
    ),
    dplyr::across(
      dplyr::all_of(ingresos),
      \(y) y / ppa_factor * ppa_factor_us,
      .names = "{.col}ppa"
    ),
    dplyr::across(
      dplyr::all_of(paste0(ingresos, "pc")),
      \(y) y / ppa_factor * ppa_factor_us,
      .names = "{.col}ppa"
    )
  )

  # Selección y ordenamiento de variables ------------------------------------
  # Pendiente: conservar columnas originales según .expandir.

  # Etiquetado de variables y valores ----------------------------------------
  if (.etiquetar) {
    # Pendiente: aplicar las etiquetas correspondientes al panel.
  }

  # Devolución del conjunto panel --------------------------------------------
  return(.H)
}
