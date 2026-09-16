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

  chequear_columnas(.P, c("PB010", "PB020"))
  
  if (!is.null(.D)) {
    chequear_columnas(.D, c("DB010", "DB020"))
  }
  if (!is.null(.R)) {
    chequear_columnas(.R, c("RB010", "RB020"))
  }

  anio <- unique(.P$PB010)
  pais <- unique(.P$PB020)

  if (length(anio) > 1) {
    cli::cli_abort(
      c(
        "Solo se aceptan bases P de un unico anio",
        "x" = "Se proporciono una base para {anio}."
      ),
      class = "varios_anios"
    )
  }
  if (length(pais) > 1) {
    cli::cli_abort(
      c(
        "Solo se aceptan bases P de un unico pais",
        "x" = "Se proporciono una base para {pais}."
      ),
      class = "varios_paises"
    )
  }

  if (!(pais %in% paises_probados)) {
    cli::cli_h1("Ojo!")
    cli::cli_bullets(c(
      "!" = "{pais} no ha sido testeado!",
      "i" = "Por ahora se han testeado {paises_probados}",
      "i" = "Revisa las SILC Disclosure Control Rules de {anio} para ver las diferencias especificas de {pais}"
    ))
  }

  if (!is.null(.D)) {
    anio_d <- unique(.D$DB010)
    pais_d <- unique(.D$DB020)

    if (!(anio %in% anio_d)) {
      cli::cli_abort(
        c(
          ".P y .D deben corresponder al mismo anio",
          "x" = ".P corresponde a {anio} y .D a {anio_d}"
        ),
        class = "d_dif_anio"
      )
    }
    if (!(pais %in% pais_d)) {
      cli::cli_abort(
        c(
          ".P y .D deben corresponder al mismo pais",
          "x" = ".P corresponde a {pais} y .D a {pais_d}"
        ),
        class = "d_dif_pais"
      )
    }
  }

  if (!is.null(.R)) {
    anio_r <- unique(.R$RB010)
    pais_r <- unique(.R$RB020)

    if (!(anio %in% anio_r)) {
      cli::cli_abort(
        c(
          ".P y .R deben corresponder al mismo anio",
          "x" = ".P corresponde a {anio} y .R a {anio_r}"
        ),
        class = "r_dif_anio"
      )
    }
    if (!(pais %in% pais_r)) {
      cli::cli_abort(
        c(
          ".P y .R deben corresponder al mismo pais",
          "x" = ".P corresponde a {pais} y .R a {pais_r}"
        ),
        class = "r_dif_pais"
      )
    }
  }
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
  if (!is.data.frame(.H)) {
    cli::cli_abort(
      c(".H debe ser un data.frame o tibble.", "x" = "Se paso un {class(.H)}"),
      class = "no_data_frame"
    )
  }

  anio <- unique(.H$HB010)
  pais <- unique(.H$HB020)

  if (length(anio) > 1) {
    cli::cli_abort(
      c(
        "Solo se aceptan bases H de un unico anio",
        "x" = "Se proporciono una base para {anio}."
      ),
      class = "varios_anios"
    )
  }
  if (length(pais) > 1) {
    cli::cli_abort(
      c(
        "Solo se aceptan bases H de un unico pais",
        "x" = "Se proporciono una base para {pais}."
      ),
      class = "varios_paises"
    )
  }

  if (!(pais %in% paises_probados)) {
    cli::cli_h1("Ojo!")
    cli::cli_bullets(c(
      "!" = "{pais} no ha sido testeado!",
      "i" = "Por ahora se han testeado {paises_probados}",
      "i" = "Revisa las SILC Disclosure Control Rules de {anio} para ver las diferencias especificas de {pais}"
    ))
  }

  if (!is.null(.P)) {
    if (!is.data.frame(.P)) {
      cli::cli_abort(
        c(
          ".P debe ser un data.frame o tibble.",
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
        c(
          ".H y .P deben corresponder al mismo anio",
          "x" = ".H corresponde a {anio} y .P a {anio_p}"
        ),
        class = "p_dif_anio"
      )
    }
    if (!(pais %in% pais_p)) {
      cli::cli_abort(
        c(
          ".H y .P deben corresponder al mismo pais",
          "x" = ".H corresponde a {pais} y .P a {pais_p}"
        ),
        class = "p_dif_pais"
      )
    }
  }

  if (!is.null(.D)) {
    if (!is.data.frame(.D)) {
      cli::cli_abort(
        c(
          ".D debe ser un data.frame o tibble.",
          "x" = "Se paso un {class(.D)}"
        ),
        class = "no_data_frame"
      )
    }

    anio_d <- unique(.D$DB010)
    pais_d <- unique(.D$DB020)

    if (!(anio %in% anio_d)) {
      cli::cli_abort(
        c(
          ".H y .D deben corresponder al mismo anio",
          "x" = ".H corresponde a {anio} y .D a {anio_d}"
        ),
        class = "d_dif_anio"
      )
    }
    if (!(pais %in% pais_d)) {
      cli::cli_abort(
        c(
          ".H y .D deben corresponder al mismo pais",
          "x" = ".H corresponde a {pais} y .D a {pais_d}"
        ),
        class = "d_dif_pais"
      )
    }
  }
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

# ============================================================================
obtener_contexto_advertencias <- function(.datos) {
  identificadores <- list(
    P = list(c("PB010", "PB020"), c("pi01", "pi02")),
    H = list(c("HB010", "HB020"), c("hi01", "hi02"))
  )

  contexto <- NULL
  for (base in names(identificadores)) {
    for (columnas in identificadores[[base]]) {
      if (all(columnas %in% names(.datos))) {
        contexto <- list(
          base = base,
          anio = unique(.datos[[columnas[1]]]),
          pais = unique(.datos[[columnas[2]]])
        )
        break
      }
    }
    if (!is.null(contexto)) break
  }

  if (is.null(contexto)) {
    rlang::abort(
      "No se pudo identificar si .datos es una base P o H.",
      class = "base_desconocida"
    )
  }
  if (length(contexto$anio) != 1 || is.na(contexto$anio)) {
    rlang::abort(
      ".datos debe corresponder a un unico anio.",
      class = "varios_anios"
    )
  }
  if (length(contexto$pais) != 1 || is.na(contexto$pais)) {
    rlang::abort(
      ".datos debe corresponder a un unico pais.",
      class = "varios_paises"
    )
  }

  contexto
}
