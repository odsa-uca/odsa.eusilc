#' Chequea que los conjuntos P, D y R sean adecuados
#'
#' @param .P Argumento .P
#' @param .D Argumento .D
#' @param .R Argumento .R
#'
#' @returns NULL
chequear_bases_personas <- function(.P, .D, .R) {
  if (!is.data.frame(.P)) {
    cli::cli_abort(
      c(".P debe ser un data.frame o tibble.",
        "x" = "Se paso un {class(.P)}"
      ),
      class = "no_data_frame"
    )
  }

  anio <- unique(.P$PB010)
  pais <- unique(.P$PB020)

  if (length(anio) > 1) {
    cli::cli_abort(
      c("Solo se aceptan bases P de un unico anio",
        "x" = "Se proporciono una base para {anio}."
      ),
      class = "varios_anios"
    )
  }
  if (length(pais) > 1) {
    cli::cli_abort(
      c("Solo se aceptan bases P de un unico pais",
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
    if (!is.data.frame(.D)) {
      cli::cli_abort(
        c(".D debe ser un data.frame o tibble.",
          "x" = "Se paso un {class(.D)}"
        ),
        class = "no_data_frame"
      )
    }

    anio_d <- unique(.D$DB010)
    pais_d <- unique(.D$DB020)

    if (!(anio %in% anio_d)) {
      cli::cli_abort(
        c(".P y .D deben corresponder al mismo anio",
          "x" = ".P corresponde a {anio} y .D a {anio_d}"),
        class = "d_dif_anio"
      )
    }
    if (!(pais %in% pais_d)) {
      cli::cli_abort(
        c(".P y .D deben corresponder al mismo pais",
          "x" = ".P corresponde a {pais} y .D a {pais_d}"),
        class = "d_dif_pais"
      )
    }
  }

  if (!is.null(.R)) {
    if (!is.data.frame(.R)) {
      cli::cli_abort(
        c(".R debe ser un data.frame o tibble.",
          "x" = "Se paso un {class(.R)}"
        ),
        class = "no_data_frame"
      )
    }

    anio_r <- unique(.R$RB010)
    pais_r <- unique(.R$RB020)

    if (!(anio %in% anio_r)) {
      cli::cli_abort(
        c(".P y .R deben corresponder al mismo anio",
          "x" = ".P corresponde a {anio} y .R a {anio_r}"),
        class = "r_dif_anio"
      )
    }
    if (!(pais %in% pais_r)) {
      cli::cli_abort(
        c(".P y .R deben corresponder al mismo pais",
          "x" = ".P corresponde a {pais} y .R a {pais_r}"),
        class = "r_dif_pais"
      )
    }
  }
}

# ============================================================================
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
      c(".H debe ser un data.frame o tibble.",
        "x" = "Se paso un {class(.H)}"
      ),
      class = "no_data_frame"
    )
  }
  
  anio <- unique(.H$HB010)
  pais <- unique(.H$HB020)

  if (length(anio) > 1) {
    cli::cli_abort(
      c("Solo se aceptan bases H de un unico anio",
        "x" = "Se proporciono una base para {anio}."
      ),
      class = "varios_anios"
    )
  }
  if (length(pais) > 1) {
    cli::cli_abort(
      c("Solo se aceptan bases H de un unico pais",
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
  }

  if (!is.null(.D)) {
    if (!is.data.frame(.D)) {
      cli::cli_abort(
        c(".D debe ser un data.frame o tibble.",
          "x" = "Se paso un {class(.D)}"
        ),
        class = "no_data_frame"
      )
    }

    anio_d <- unique(.D$DB010)
    pais_d <- unique(.D$DB020)

    if (!(anio %in% anio_d)) {
      cli::cli_abort(
        c(".H y .D deben corresponder al mismo anio",
          "x" = ".H corresponde a {anio} y .D a {anio_d}"),
        class = "d_dif_anio"
      )
    }
    if (!(pais %in% pais_d)) {
      cli::cli_abort(
        c(".H y .D deben corresponder al mismo pais",
          "x" = ".H corresponde a {pais} y .D a {pais_d}"),
        class = "d_dif_pais"
      )
    }
  }
}

# ============================================================================
#' Chequea y avisa qué variables están completamente perdidas
#'
#' @param .datos `tibble`. Conjunto de datos
#' @param .base `character`. Qué tipo de base es, P o H
#'
#' @returns Nada
chequear_perdidas <- function(.datos, .base) {
  perdidas <- sapply(names(etq[[.base]]$variables), \(.v) {
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