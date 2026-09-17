# ----------------------------------------------------------------------------
#' Obtiene el unico anio y pais validos de una base
#'
#' @param .datos Data frame de una base suministrada.
#' @param .columnas Nombres de las columnas de anio y pais, en ese orden.
#' @param .argumento Nombre del argumento para los mensajes.
#'
#' @returns Lista con anio y pais.
obtener_periodo_base <- function(
  .datos,
  .columnas,
  .argumento = rlang::caller_arg(.datos)
) {
  chequear_columnas(.datos, .columnas, .argumento)

  if (nrow(.datos) == 0L) {
    cli::cli_abort(
      "{.arg {(.argumento)}} no debe estar vacia.",
      class = "base_vacia"
    )
  }

  periodo <- list()
  for (indice in seq_along(.columnas)) {
    campo <- c("anio", "pais")[[indice]]
    columna <- .columnas[[indice]]
    valores <- unique(.datos[[columna]])

    if (anyNA(valores)) {
      cli::cli_abort(
        "{.arg {(.argumento)}} contiene valores faltantes en {.field {columna}} ({.field {campo}}).",
        class = "valores_faltantes"
      )
    }

    if (length(valores) != 1L) {
      cli::cli_abort(
        "{.arg {(.argumento)}} debe contener un unico valor en {.field {columna}} ({.field {campo}}); se encontraron {valores}.",
        class = c("varios_anios", "varios_paises")[[indice]]
      )
    }

    periodo[[campo]] <- valores
  }

  return(periodo)
}

# ----------------------------------------------------------------------------
#' Chequea la concordancia de una base auxiliar con la principal
#'
#' @param .referencia Lista con anio y pais validos de la base principal.
#' @param .auxiliar Lista con anio y pais validos de la auxiliar.
#' @param .principal Nombre del argumento de la base principal.
#' @param .argumento Nombre del argumento de la base auxiliar.
#' @param .prefijo Prefijo de las clases de discrepancia: p, d o r.
#'
#' @returns NULL, invisiblemente.
chequear_concordancia <- function(
  .referencia,
  .auxiliar,
  .principal,
  .argumento,
  .prefijo
) {
  for (campo in c("anio", "pais")) {
    if (.referencia[[campo]] != .auxiliar[[campo]]) {
      cli::cli_abort(
        c(
          "{.arg {(.principal)}} y {.arg {(.argumento)}} deben corresponder al mismo {campo}.",
          "x" = "{.arg {(.principal)}} corresponde a {(.referencia[[campo]])} y {.arg {(.argumento)}} a {(.auxiliar[[campo]])}."
        ),
        class = paste0(.prefijo, "_dif_", campo)
      )
    }
  }

  invisible(NULL)
}

# ----------------------------------------------------------------------------
#' Informa si el pais validado no fue probado con el paquete
#'
#' @param .periodo Lista con anio y pais validos.
#'
#' @returns NULL, invisiblemente.
informar_pais_no_probado <- function(.periodo) {
  if (!(.periodo$pais %in% paises_probados)) {
    cli::cli_h1("Ojo!")
    cli::cli_bullets(c(
      "!" = "{(.periodo$pais)} no ha sido testeado!",
      "i" = "Por ahora se han testeado {paises_probados}",
      "i" = "Revisa las SILC Disclosure Control Rules de {(.periodo$anio)} para ver las diferencias especificas de {(.periodo$pais)}"
    ))
  }

  invisible(NULL)
}
# ----------------------------------------------------------------------------
#' Chequea que los conjuntos P, D y R sean adecuados
#'
#' @param .P Argumento .P
#' @param .D Argumento .D
#' @param .R Argumento .R
#'
#' @returns NULL
chequear_bases_personas <- function(.P, .D, .R) {
  rlang::check_data_frame(.P, class = "no_data_frame")
  rlang::check_data_frame(.D, allow_null = TRUE, class = "no_data_frame")
  rlang::check_data_frame(.R, allow_null = TRUE, class = "no_data_frame")

  referencia <- obtener_periodo_base(.P, c("PB010", "PB020"))
  
  if (!is.null(.D)) {
    periodo_d <- obtener_periodo_base(.D, c("DB010", "DB020"))
    chequear_concordancia(referencia, periodo_d, ".P", ".D", "d")
  }
  if (!is.null(.R)) {
    periodo_r <- obtener_periodo_base(.R, c("RB010", "RB020"))
    chequear_concordancia(referencia, periodo_r, ".P", ".R", "r")
  }
  
  informar_pais_no_probado(referencia)

  invisible(NULL)
}

# ----------------------------------------------------------------------------
#' Chequea la presencia de columnas requeridas
#'
#' @param .datos Conjunto de datos.
#' @param .columnas Nombres de las columnas requeridas.
#' @param .argumento Nombre del argumento para el mensaje de error.
#'
#' @returns `NULL`, invisiblemente.
chequear_columnas <- function(
  .datos,
  .columnas,
  .argumento = rlang::caller_arg(.datos)
) {
  faltantes <- setdiff(.columnas, names(.datos))

  if (length(faltantes) > 0L) {
    cli::cli_abort(
      "En {.arg {(.argumento)}} faltan las columnas requeridas: {.field {faltantes}}.",
      class = "columnas_faltantes"
    )
  }

  invisible(NULL)
}

# ----------------------------------------------------------------------------
#' Chequea que los conjuntos H, P y D sean adecuados
#'
#' @param .H Argumento .H
#' @param .P Argumento .P
#' @param .D Argumento .D
#'
#' @returns NULL
chequear_bases_hogares <- function(.H, .P, .D) {
  rlang::check_data_frame(.H, class = "no_data_frame")
  rlang::check_data_frame(.P, allow_null = TRUE, class = "no_data_frame")
  rlang::check_data_frame(.D, allow_null = TRUE, class = "no_data_frame")

  referencia <- obtener_periodo_base(.H, c("HB010", "HB020"))

  if (!is.null(.P)) {
    periodo_p <- obtener_periodo_base(.P, c("pi01", "pi02"))
    
    if (is.null(attr(.P, "base", exact = TRUE))) {
      cli::cli_abort(
        ".P debe ser una base P expandida con expandir_personas().",
        class = "no_expandida"
      )
    } else if (!identical(attr(.P, "base", exact = TRUE), "P")) {
      cli::cli_abort(".P debe ser una base P.", class = "no_p")
    }
    
    chequear_concordancia(referencia, periodo_p, ".H", ".P", "p")
  }
  if (!is.null(.D)) {
    periodo_d <- obtener_periodo_base(.D, c("DB010", "DB020"))
    chequear_concordancia(referencia, periodo_d, ".H", ".D", "d")
  }
  
  informar_pais_no_probado(referencia)

  invisible(NULL)
}

# ============================================================================
#' Chequea si alguno de los insumos esta completamente perdido
#'
#' @param ... Vectores insumo
#'
#' @returns `TRUE` si alguno de los insumos esta completamente perdido
chequear_insumos_perdidos <- function(...) {
  insumos <- list(...)

  perdidos <- vapply(
    insumos,
    \(insumo) length(insumo) > 0L && all(is.na(insumo)),
    logical(1)
  )

  return(any(perdidos))
}

# ============================================================================
#' Chequea y avisa qué variables están completamente perdidas
#'
#' @param .datos `tibble`. Conjunto de datos
#' @param .base `character`. Qué tipo de base es, P o H
#'
#' @returns Nada
chequear_perdidas <- function(.datos, .base) {
  perdidas <- sapply(names(etiquetas_[[.base]]$variables), \(.v) {
    if (.v %in% names(.datos)) all(is.na(.datos[.v])) else FALSE
  })
  perdidas <- names(which(perdidas))

  if (length(perdidas) == 0) {
    cli::cli_alert_success("No hay variables perdidas!")
  } else {
    cli::cli_bullets(c(
      "!" = "Las siguientes variables estan perdidas:",
      " " = "{perdidas}",
      "i" = "Si alguna no esta mencionada en la estandarizacion, puede haber problemas!"
    ))
  }
}