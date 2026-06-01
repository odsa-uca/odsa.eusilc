#' Construye variables en la base H de la EU-SILC.
#'
#' @param .H Conjunto H de la EU-SILC.
#'
#' @returns Conjunto H de la EU-SILC con variables adicionales.
#' 
#' @export
calcular_hogares <- function(.H) {
  # TODO: chequear argumentos
  
  calcular_hogares_(.H)
}

# ============================================================================
#' Construye variables en la base H de la EU-SILC.
#'
#' @param .H Conjunto H de la EU-SILC.
#'
#' @returns Conjunto H de la EU-SILC con variables adicionales.
calcular_hogares_ <- function(.H) {
  # Lookup -----------------------------------
  .H <- dplyr::left_join(
    x = .H,
    y = tabla_ppa,
    by = dplyr::join_by(HB010 == PB010, HB020 == PB020)
  )
  
  # Núcleo -----------------------------------
  .H <- .H |>
    dplyr::mutate(
      # Bloque I -----------------------
      hi01 = HB010,
      hi02 = HB020,
      hi04 = HB030,
      hi06 = DB090,
      # Bloque D -----------------------
      hd01 = HX040,
      hd02a = NA_integer_,
      hd02b = NA_integer_,
      # Bloque L -----------------------
      # Bloque Y -----------------------
      hy00 = py00 + (HY040N + HY050N + HY060N + HY070N + HY080N + HY090N + HY110N) / 12,
      hy20 = py20 + (HY040N + HY050N + HY060N + HY070N + HY080N + HY090N + HY110N) / 12,
      hy21 = (HY040N + HY080N + HY090N + HY110N) / 12,
      hy22 = (HY040N + HY090N) / 12,
      hy23 = (HY080N + HY110N) / 12,
      hy24 = py21 + py24 + py25 + (HY050N + HY060N + HY070N) / 12,
      hy25 = py24 + py25 + (HY050N + HY060N + HY070N) / 12,
      hy26 = (HY050N + HY060N + HY070N) / 12,
      dplyr::across(
        .cols = c(py00:py25, hy00:hy26),
        .fns = \(y) y / hd01, .names = "{.col}pc"
      ),
      dplyr::across(
        .cols = c(py00:py25, hy00:hy26),
        .fns = \(y) y / ppa,
        .names = "{.col}ppa"
      ),
      .keep = "all"
    )

  # ------------------------------------------
  return(.H)
}

# ============================================================================
#' Agrega variables de ingreso de la base P de la EU-SILC a nivel hogar.
#'
#' @param .personas Conjunto P de la EU-SILC expandido con [calcular_personas()].
#'
#' @returns Conjunto de datos con ingresos individuales agregados a nivel hogar.
agregar_personas <- function(.personas) {
  personas <- .personas |>
    dplyr::select(pi01, pi02, pi04, py00:py25) |>
    dplyr::mutate(
      dplyr::across(py00:py25, \(y) as.integer(y != 0), .names = "x{.col}")
    ) |>
    collapse::collap(
      by = ~ pi01 + pi02 + pi04, FUN = collapse::fsum, na.rm = FALSE
    )
  personas <- personas |>
    dplyr::rename_with(.cols = dplyr::starts_with("xpy"), .fn = \(n) sub("xpy", "hp", n))

  # ------------------------------------------
  return(personas)
}
