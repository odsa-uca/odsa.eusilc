#' Construye variables nuevas a partir de los conjuntos H y P de la EU-SILC
#' 
#' @description
#' Construye variables nuevas de nivel hogar a partir de los conjuntos H y P
#' (expandido) de la EU-SILC. Las variables se organizan en cinco bloques: I de
#' identificación, D de demográficos, L de laborales, Y de ingresos y P de
#' perceptores. Dependiendo del año y el país de la encuesta y si se
#' proporcionó en conjunto D, algunas de las variables pueden estar perdidas (`NA`).
#'
#' @param .H `data.frame`o `tibble`. Conjunto H de la EU-SILC
#' @param .P `data.frame`o `tibble`. Conjunto P de la EU-SILC expandido con [expandir_personas()]
#' @param .expandir `TRUE` o `FALSE` (por defecto). ¿Mantener las variables originales?
#'
#' @returns `tibble`. Conjunto H de la EU-SILC estandarizado con variables armonizadas
#' 
#' @details
#' Las variables construidas, según bloque, son las siguientes
#' 
#' ## (I) Identificación
#' 
#' - hi01. Año de la encuesta
#' - hi02. País
#' - hi03. Región
#' - hi04. Identificador del hogar
#' - hi06. Ponderador
#' 
#' ## (D) Demográficos
#' 
#' - hd01. Tamaño del hogar
#' - hd02a. Tipo de hogar desagregado
#' - hd02b. Tipo de hogar dicotómico
#' - hdxx. (a definir...)
#' 
#' ## (L) Laborales
#' 
#' - hlxx. (a definir...)
#' 
#' ## (Y) Ingresos
#' 
#' - py00. Ingreso total de los miembros
#' - py10. Ingreso total de los miembros por fuentes laborales
#' - py11. Ingreso de los miembros por trabajo asalariado
#' - py12. Ingreso de los miembros por trabajo no asalariado
#' - py13<sup>1</sup>. Ingreso de los miembros por trabajo en el sector público
#' - py14<sup>1</sup>. Ingreso de los miembros por trabajo en el sector privado formal
#' - py15<sup>1</sup>. Ingreso de los miembros por trabajo en el sector microinformal
#' - py20. Ingreso total de los miembros por fuentes no laborales
#' - py21. Ingreso total de los miembros por jubilaciones y pensiones privadas
#' - py22. Ingreso de los miembros por jubilación
#' - py23. Ingreso de los miembros por pensión privada
#' - py24. Ingreso de los miembros por desempleo
#' - py25. Ingreso de los miembros por otras ayudas
#' - hy00. Ingreso total del hogar
#' - hy20. Ingreso total del hogar por fuentes no laborales
#' - hy21. Ingreso total por inversiones y otras transferencias
#' - hy22. Ingreso del hogar por inversiones
#' - hy23. Ingreso del hogar por otras transferencias
#' - hy24. Ingreso total por política social
#' - hy25. Ingreso total por transferencias
#' - hy26. Ingreso del hogar por asistencia social
#' 
#' Nota 1: Dependen de las variables PL130 y PL230 del módulo LMH en la base P
#' 
#' ## (P) Perceptores
#' 
#' - hp00. Perceptores de ingreso
#' - hp10. Perceptores de ingreso por fuentes laborales
#' - hp11. Perceptores de ingreso por trabajo asalariado
#' - hp12. Perceptores de ingreso por trabajo no asalariado
#' - hp13. Perceptores de ingreso por trabajo en el sector público
#' - hp14. Perceptores de ingreso por trabajo en el sector privado formal
#' - hp15. Perceptores de ingreso por trabajo en el sector microinformal
#' - hp20. Perceptores de ingreso por fuentes no laborales
#' - hp21. Perceptores de ingreso por jubilaciones o pensiones privadas
#' - hp22. Perceptores de ingreso por jubilación
#' - hp23. Perceptores de ingreso por pensión privada
#' - hp24. Perceptores de ingreso por desempleo
#' - hp25. Perceptores de ingreso por otras ayudas
#' 
#' @export
calcular_hogares <- function(.H, .P, .expandir = FALSE) {
  # TODO: chequear argumentos
  if (!is.data.frame(.H)) {
    cli::cli_abort(
      c(".H debe ser un data.frame o tibble.",
        "x" = "Se paso un {class(.H)}"
      ),
      class = "no_data_frame"
    )
  }
  if (is.null(attr(.H, "estandar"))) {
    cli::cli_abort(
      ".H debe ser una base H estandarizada con estandarizar_hogares().",
      class = "no_estandar"
    )
  }
  if (attr(.H, "base") != "H") {
    cli::cli_abort(
      ".H debe ser una base H.",
      class = "no_p"
    )
  }
  
  anio <- unique(.H$HB010)
  pais <- unique(.H$HB020)
  
  if (!is.data.frame(.P)) {
    cli::cli_abort(
      c(".P debe ser un data.frame o tibble.",
        "x" = "Se paso un {class(.P)}"
      ),
      class = "no_data_frame"
    )
  } else if (is.null(attr(.P, "base"))) {
    cli::cli_abort(
      ".P debe ser una base P expandida con expandir_personas().",
      class = "no_expandida"
    )
  } else if (attr(.P, "base") != "P") {
    cli::cli_abort(
      ".P debe ser una base P.",
      class = "no_p"
    )
  }

  anio_p <- unique(.P$pi01)
  pais_p <- unique(.P$pi02)
  
  if (!(anio %in% anio_p)) {
    cli::cli_abort(
      c(".H y .P deben corresponder al mismo anio",
        "x" = ".H corresponde a {anio} y .P a {anio_p}"),
      class = "p_dif_anio"
    )
  }
  if (!(pais %in% pais_p)) {
    cli::cli_abort(
      c(".H y .P deben corresponder al mismo pais",
        "x" = ".H corresponde a {pais} y .P a {pais_p}"),
      class = "p_dif_pais"
    )
  }
  
  if (!is.logical(.expandir)) {
    cli::cli_abort(
      c(".expandir debe ser TRUE o FALSE.",
        "x" = "Se paso un {class(.expandir)}"
      ),
      class = "no_logical"
    )
  }
  
  .P <- agregar_personas(.P)
  .H <- dplyr::left_join(
    x = .H, y = .P,
    by = dplyr::join_by(HB010 == pi01, HB020 == pi02, HB030 == pi04)
  )
  .H <- calcular_hogares_(.H)
  
  return(.H)
}

# ============================================================================
#' Construye variables nuevas a partir de los conjuntos H y P de la EU-SILC (interna)
#' 
#' @description
#' ¡Esta función es interna! Construye variables nuevas de nivel hogar a partir
#' de los conjuntos H y P (expandido con [expandir_personas()]) de la EU-SILC.
#' Las variables se organizan en cinco bloques: I de identificación, D de
#' demográficos, L de laborales, Y de ingresos y P de perceptores. Dependiendo
#' del año y el país de la encuesta y si se proporcionó en conjunto D, algunas
#' de las variables pueden estar perdidas (`NA`).
#' 
#' @details
#' Esta función es el núcleo interno de [calcular_hogares()]. Para más detalles
#' consultar la documentación de esa función.
#'
#' @param .H `data.frame`o `tibble`. Conjunto H de la EU-SILC
#' 
#' @returns `tibble`. Conjunto H de la EU-SILC estandarizado con variables armonizadas
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
#' Agrega variables de ingreso de la base P de la EU-SILC a nivel hogar
#' 
#' @description
#' Agrega las variables de ingreso de la base P (expandida con [expandir_personas()])
#' por hogar y cuenta la cantidad de perceptores de cada variable de ingreso.
#'
#' @param .personas `data.frame` o `tibble`. Conjunto P de la EU-SILC expandido con [expandir_personas()].
#'
#' @returns `tibble`. Conjunto de datos con ingresos individuales agregados a nivel hogar y número de perceptores
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
