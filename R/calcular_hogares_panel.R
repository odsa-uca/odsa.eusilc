# ----------------------------------------------------------------------------
#' Construye variables del conjunto H longitudinal de la EU-SILC (interna)
#'
#' @description
#' Construye variables armonizadas de identificación, tamaño e ingresos del
#' hogar a partir del conjunto H longitudinal estandarizado y de los agregados
#' personales ya incorporados. Recodifica la urbanización e incorpora factores
#' PPA para construir variantes de ingresos per cápita y PPA. Supone
#' observaciones de 2021 en adelante. Es una función interna.
#'
#' @param .H `data.frame` o `tibble`. Conjunto H longitudinal de la EU-SILC
#'   estandarizado por [estandarizar_hogares_panel_()], con las sumas de ingresos
#'   personales incorporadas por [expandir_hogares_panel()]. La estandarización
#'   por sí sola no prepara esos agregados.
#'
#' @returns `data.frame` o `tibble` con las columnas de entrada y las variables
#'   armonizadas, conservando las filas y su orden. Reemplaza factores PPA
#'   homónimos y variables armonizadas preexistentes. Conserva los conteos de
#'   perceptores incorporados previamente. No selecciona ni etiqueta columnas.
#'
#' @details
#' El catálogo de variables y sus unidades se presenta en
#' [expandir_hogares_panel()].
#'
#' ## Identificación, tamaño y urbanización
#'
#' La identificación y el tamaño del hogar se construyen desde los insumos
#' publicados. El tamaño procede de H, no del número de personas disponibles
#' en P. El ponderador es longitudinal, no transversal. La urbanización se
#' recodifica mediante una tabla; los códigos desconocidos quedan como `NA`.
#' Se conserva el detalle regional publicado, sin reconstruir supresiones.
#'
#' ## Construcción y conversión de ingresos
#'
#' Los componentes netos propios de H, publicados en euros anuales, se
#' convierten a moneda nacional por mes con `HX010 / 12`. Se combinan con las
#' sumas personales según la fuente de ingreso. Los ingresos personales ya
#' están en moneda nacional mensual y no se convierten nuevamente.
#'
#' Se conservan ingresos negativos y los faltantes se propagan en las sumas,
#' sin reemplazarlos por cero. No se reconstruyen componentes desde un total
#' oficial ni se sustituye el ingreso total construido por el ingreso
#' disponible publicado.
#'
#' ## Ingresos per cápita y PPA
#'
#' Las variantes per cápita dividen el ingreso por `hd01` sólo cuando el tamaño
#' es positivo y está disponible; en otro caso quedan como `NA`.
#'
#' Los factores PPA se incorporan por país y año de encuesta, reemplazando
#' factores homónimos preexistentes. La tabla interna cubre 2016–2025; sin
#' coincidencia, los factores quedan como `NA`, sin extrapolación. Para su
#' definición, ver [tabla_ppa].
#'
#' Las variantes PPA se calculan con `ingreso / ppa_factor * ppa_factor_us`,
#' tanto para los ingresos mensuales como para los per cápita. Se expresan en
#' dólares PPA por mes y por persona y mes, respectivamente. Si falta algún
#' factor, las variantes PPA quedan como `NA`, incluso para ingresos cero.
#'
#' @seealso [expandir_hogares_panel()], [estandarizar_hogares_panel_()]
calcular_hogares_panel_ <- function(.H) {
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
    "py00",
    "py10",
    "py11",
    "py12",
    "py20",
    "py21",
    "py22",
    "py23",
    "py24",
    "py25",
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
  
  return(.H)
}
