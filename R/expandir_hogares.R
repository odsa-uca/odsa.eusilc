#' Construir variables adicionales en los conjuntos de datos H de la EU-SILC
#'
#' @param .datos Conjunto de datos H de la EU-SILC.
#' @param .P Conjunto de datos P de la EU-SILC expandido por [expandir_personas()].
#' @param .D Conjunto de datos D de la EU-SILC.
#' @param .expandir Conservar las variables originales en el conjunto de datos final o eliminarlas.
#' @param ... ...
#'
#' @returns Conjunto de datos de la EU-SILC con variables adicionales de uso habitual
#' @export
expandir_hogares <- function(
    .datos,
    .P,
    .D = NULL,
    .expandir = FALSE,
    ...
) {
  # Chequeos args ------------------------------------------------------------
  if (!is.data.frame(.datos)) {
    rlang::abort("`.datos` debe ser un data.frame o tibble.")
  }
  if (!is.data.frame(.P)) {
    rlang::abort("`.P` debe ser un data.frame o tibble.")
  }
  if (is.null(attr(.P, "base"))) {
    rlang::abort("`.P` debe ser una base P expandida con `expandir_eusilc().`")
  }
  if (attr(.P, "base") != "P") {
    rlang::abort("`.P` debe ser una base P expandida con `expandir_eusilc().`")
  }
  if (!is.null(.D) & !is.data.frame(.D)) {
    rlang::abort("`.D` debe ser un data.frame o tibble.")
  }

  # Chequear bloques ---------------------------------------------------------
  bloques <- c(D = !is.null(.D), attr(.P, "bloques")["LMH"])

  if (bloques["D"]) {
    .datos <- dplyr::left_join(
      x = .datos,
      y = dplyr::select(.D, DB010, DB020, DB030, DB040, DB090),
      by = dplyr::join_by(HB010 == DB010, HB020 == DB020, HB030 == DB030)
    )
  } else {
    .datos <- dplyr::mutate(.datos, DB090 = NA)
    rlang::warn("No se proporciono el conjunto D. Se pierde: `hi06`.")
  }

  if (!bloques["LMH"]) {
    rlang::warn("No se encontro `PL130` o `PL230` en `.P`. Se pierden: `py01`, `py02`, `py03`.")
  }

  # Calcular vbles -----------------------------------------------------------
  P <- agregar_personas(.P)
  .datos <- dplyr::left_join(
    x = .datos, y = P,
    by = dplyr::join_by(HB010 == pi01, HB020 == pi02, HB030 == pi04)
  )
  .datos <- dplyr::left_join(
    x = .datos,
    y = tabla_ppa,
    by = dplyr::join_by(HB010 == PB010, HB020 == PB020)
  )
  .datos <- calcular_hogares(.datos)

  # Arreglos y devolver ------------------------------------------------------
  if (!.expandir) {
    .datos <- dplyr::select(.datos, dplyr::all_of(names(etq$H$variables)))
  } else {
    .datos <- dplyr::relocate(.datos, dplyr::all_of(names(etq$H$variables)))
  }

  attr(.datos, "base") <- "H"
  attr(.datos, "bloques") <- bloques
  attr(.datos, "expandida") <- .expandir

  return(.datos)
}

# ============================================================================
#' Agrega variables de ingreso de la base P de la EU-SILC a nivel hogar.
#'
#' @param .personas Conjunto P de la EU-SILC expandido con [calcular_personas()].
#'
#' @returns Conjunto de datos con ingresos individuales agregados a nivel hogar.
agregar_personas <- function(.personas) {
  # OPTIMIZAR, ES MUY LENTA
  personas <- .personas |>
    dplyr::mutate(
      dplyr::across(py00:py25, \(y) as.integer(y != 0), .names = "x{.col}")
    ) |>
    dplyr::summarise(
      dplyr::across(c(py00:py25, xpy00:xpy25), sum),
      .by = c(pi01, pi02, pi04)
    )
  personas <- personas |>
    dplyr::rename_with(.cols = dplyr::starts_with("xpy"), .fn = \(n) sub("xpy", "hp", n))

  # ------------------------------------------
  return(personas)
}

# ============================================================================
#' Agrega variables de ingreso de la base P de la EU-SILC a nivel hogar. (Optimizda)
#'
#' @param .personas Conjunto P de la EU-SILC expandido con [calcular_personas()].
#'
#' @returns Conjunto de datos con ingresos individuales agregados a nivel hogar.
f_agregar_personas <- function(.personas) {
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

# ============================================================================
#' Construye variables en la base H de la EU-SILC.
#'
#' @param .datos Conjunto H de la EU-SILC.
#' @param ... ...
#'
#' @returns Conjunto H de la EU-SILC con variables adicionales.
calcular_hogares <- function(
    .datos,
    ...
) {
  hogares <- .datos |>
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
  return(hogares)
}
