# ----------------------------------------------------------------------------
#' Construye variables del conjunto H longitudinal de la EU-SILC (interna)
#'
#' @param .H `data.frame` o `tibble`. Conjunto H longitudinal de la EU-SICL
#'   estandarizado por [estandarizar_hogares_panel_()].
#'
#' @returns Conjunto H con las variables originales y las armonizadas.
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