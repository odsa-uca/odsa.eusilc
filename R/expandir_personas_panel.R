# ----------------------------------------------------------------------------
#' Armoniza variables mínimas del conjunto P longitudinal de la EU-SILC
#'
#' @description
#' Construye variables de identificación, sexo e ingresos netos del conjunto P
#' longitudinal de la EU-SILC, suponiendo observaciones de 2021 en adelante.
#' Incorpora ponderadores desde R y metadatos panel desde D cuando se suministran.
#' Las flags todavía no tienen efecto.
#'
#' @param .P `data.frame` o `tibble`. Conjunto P longitudinal en formato largo,
#'   de un único país y varios años, con una fila por persona y año.
#' @param .D `data.frame` o `tibble`, o `NULL` (por defecto). Conjunto D
#'   longitudinal del cual se incorporan `DB075` y `DB076`.
#' @param .R `data.frame` o `tibble`, o `NULL` (por defecto). Conjunto R
#'   longitudinal del cual se incorporan los ponderadores `RB062`–`RB066`.
#' @param .imputar `TRUE` o `FALSE` (por defecto). ¿Imputar valores faltantes o
#'   inconsistentes dentro de cada ola? Pendiente de implementación.
#' @param .expandir `TRUE` o `FALSE` (por defecto). ¿Conservar las columnas
#'   originales en el resultado? Pendiente de implementación.
#' @param .etiquetar `TRUE` (por defecto) o `FALSE`. ¿Aplicar etiquetas a las
#'   variables y sus valores? Pendiente de implementación.
#'
#' @returns `data.frame` o `tibble` con las variables originales y las variables
#'   armonizadas. Conserva las filas y su orden, incluidos paneles desbalanceados.
#'   Los ponderadores y metadatos panel se renombran y quedan como `NA` cuando
#'   sus insumos no están disponibles.
#'
#' @details
#' Se construyen `pi01`, `pi02`, `pi04`, `pi05` y `pd02` desde `PB010`, `PB020`,
#' `PX030`, `PB030` y `PB150`, respectivamente. Estos insumos deben estar presentes.
#'
#' Los ingresos `py00`, `py10`, `py11`, `py12`, `py20`, `py21`, `py22`, `py23`,
#' `py24` y `py25` siguen las definiciones de [calcular_personas()]. Se expresan
#' en moneda nacional por mes, convirtiendo los importes netos anuales en euros
#' mediante `PX010 / 12`. Se conservan los ingresos negativos y los faltantes se
#' propagan en las sumas. Si falta una columna de ingresos netos o `PX010`, las
#' variables que la requieren quedan como `NA`, sin impedir los otros cálculos.
#'
#' En Italia, `PY120N` se establece en cero por observación cuando su flag
#' `PY120N_F` vale -4, indicando su integración en otros componentes. Esta regla
#' contable se aplica independientemente de `.imputar`. Sin la flag se conserva
#' el valor publicado. No se revierten agrupaciones ni perturbaciones publicadas.
#'
#' Los insumos `RB062`, `RB063`, `RB064`, `RB065` y `RB066`
#' se renombran como `pi06a`, `pi06b`, `pi06c`, `pi06d` y `pi06e`, para paneles de
#' dos a seis años, respectivamente. `DB075` y `DB076` se renombran como `pi08a`
#' (grupo de rotación) y `pi08b` (número de encuesta). No se elige un ponderador
#' automáticamente. Las siete variables armonizadas están siempre presentes;
#' los insumos ausentes se completan con `NA` durante la estandarización.
#'
#' Los cruces con `.R` incluyen país, año y persona; con `.D`, país, año y hogar
#' actual, que puede cambiar entre olas. Cada clave debe identificar una única
#' fila en el auxiliar. Sin coincidencia, las variables traspasadas quedan como
#' `NA`. Si se suministra un auxiliar, sus valores reemplazan los insumos
#' correspondientes de `.P`; si no se suministra, se conservan los insumos ya
#' incorporados en `.P`. No se balancea el panel ni se crean observaciones.
#'
#' Los insumos y ponderadores longitudinales requieren un tratamiento propio:
#' no debe suponerse la disponibilidad de las variables transversales ni elegirse
#' automáticamente un ponderador, cuya duración de seguimiento debe considerarse.
#' Las agrupaciones, supresiones y perturbaciones documentadas por Eurostat no
#' se tratarán automáticamente como inconsistencias imputables. La imputación
#' prevista utilizará únicamente información dentro de cada ola.
#'
#' Quedan pendientes el traspaso de otras variables desde `.D` y `.R`, la imputación,
#' la selección mediante `.expandir`, el etiquetado mediante `.etiquetar` y las
#' variantes PPA. Esta versión conserva todas las columnas originales, salvo
#' los renombrados y la transformación contable indicada, y no agrega atributos
#' de armonización, imputación o etiquetado.
#'
#' @seealso [expandir_hogares_panel()]
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
  ponderadores <- c("RB062", "RB063", "RB064", "RB065", "RB066")
  metadatos_panel <- c("DB075", "DB076")
  if (is.null(.R)) {
    for (variable in setdiff(ponderadores, names(.P))) {
      .P[[variable]] <- rep(NA_real_, nrow(.P))
    }
  } else {
    for (variable in setdiff(ponderadores, names(.R))) {
      .R[[variable]] <- rep(NA_real_, nrow(.R))
    }
  }
  if (is.null(.D)) {
    for (variable in setdiff(metadatos_panel, names(.P))) {
      .P[[variable]] <- rep(NA_integer_, nrow(.P))
    }
  } else {
    for (variable in setdiff(metadatos_panel, names(.D))) {
      .D[[variable]] <- rep(NA_integer_, nrow(.D))
    }
  }

  insumos <- c(
    "PY010N",
    "PY050N",
    "PY080N",
    "PY090N",
    "PY100N",
    "PY110N",
    "PY120N",
    "PY130N",
    "PY140N",
    "PX010"
  )
  insumos_ausentes <- setdiff(insumos, names(.P))
  for (variable in insumos_ausentes) {
    .P[[variable]] <- rep(NA_real_, nrow(.P))
  }

  if ("PY120N_F" %in% names(.P)) {
    .P <- dplyr::mutate(
      .P,
      PY120N = dplyr::if_else(
        PB020 == "IT" & PY120N_F == -4,
        0,
        PY120N,
        missing = PY120N
      )
    )
  }

  # Traspaso de variables desde conjuntos auxiliares -------------------------
  if (!is.null(.R)) {
    .P <- dplyr::left_join(
      x = dplyr::select(.P, -dplyr::any_of(ponderadores)),
      y = dplyr::select(.R, RB010, RB020, RB030, dplyr::all_of(ponderadores)),
      by = dplyr::join_by(PB010 == RB010, PB020 == RB020, PB030 == RB030),
      relationship = "many-to-one"
    )
  }
  if (!is.null(.D)) {
    .P <- dplyr::left_join(
      x = dplyr::select(.P, -dplyr::any_of(metadatos_panel)),
      y = dplyr::select(
        .D,
        DB010,
        DB020,
        DB030,
        dplyr::all_of(metadatos_panel)
      ),
      by = dplyr::join_by(PB010 == DB010, PB020 == DB020, PX030 == DB030),
      relationship = "many-to-one"
    )
  }
  # Pendiente: incorporar otras variables auxiliares de D y R.

  # Imputación dentro de cada ola --------------------------------------------
  if (.imputar) {
    # Pendiente: imputar valores faltantes o inconsistentes dentro de cada ola.
  }

  # Construcción de nuevas variables y recodificación -------------------------
  .P <- dplyr::mutate(
    .P,
    # Bloque I --------------------------------------------------------------
    pi01 = PB010,
    pi02 = PB020,
    pi04 = PX030,
    pi05 = PB030,
    pi06a = RB062,
    pi06b = RB063,
    pi06c = RB064,
    pi06d = RB065,
    pi06e = RB066,
    pi08a = DB075,
    pi08b = DB076,
    # Bloque D --------------------------------------------------------------
    pd02 = PB150,
    # Bloque Y --------------------------------------------------------------
    py00 = PY010N +
      PY050N +
      PY090N +
      PY110N +
      PY120N +
      PY130N +
      PY140N +
      PY100N +
      PY080N,
    py10 = PY010N + PY050N,
    py11 = PY010N,
    py12 = PY050N,
    py20 = PY090N + PY110N + PY120N + PY130N + PY140N + PY100N + PY080N,
    py21 = PY100N + PY080N,
    py22 = PY100N,
    py23 = PY080N,
    py24 = PY090N,
    py25 = PY110N + PY120N + PY130N + PY140N
  )

  .P <- dplyr::mutate(
    .P,
    dplyr::across(
      c(py00, py10, py11, py12, py20, py21, py22, py23, py24, py25),
      \(y) (y * PX010) / 12
    )
  )

  # Selección y ordenamiento de variables ------------------------------------
  .P <- dplyr::select(.P, -dplyr::any_of(insumos_ausentes))
  # Pendiente: conservar columnas originales según .expandir.

  # Etiquetado de variables y valores ----------------------------------------
  if (.etiquetar) {
    # Pendiente: aplicar las etiquetas correspondientes al panel.
  }

  # Devolución del conjunto panel --------------------------------------------
  return(.P)
}
