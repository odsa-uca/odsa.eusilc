#' Armoniza el conjunto de datos H de la EU-SILC
#' 
#' @description
#' Aplica una serie de transformaciones sobre los conjuntos de datos H y P de
#' la EU-SILC y devuelve un conjunto de datos de nivel hogar con variables
#' armonizadas. Las transformaciones tienen en cuenta el país y el año de la
#' encuesta y si se proporciona el conjunto de datos D.
#'
#' @param .H `data.frame`o `tibble`. Conjunto de datos H de la EU-SILC
#' @param .P `data.frame`o `tibble`. Conjunto de datos P de la EU-SILC expandido por [expandir_personas()]
#' @param .D `data.frame`o `tibble`. Conjunto de datos D de la EU-SILC
#' @param .expandir `TRUE` o `FALSE` (por defecto). ¿Conservar las variables originales en el conjunto de datos final?
#' @param .etiquetar `TRUE` (por defecto) o `FALSE`. ¿Aplicar etiquetas a las variables y sus valores?
#'
#' @returns `tibble`. Conjunto de datos de la EU-SILC con variables adicionales armonizadas
#' @export
expandir_hogares <- function(
    .H,
    .P,
    .D = NULL,
    .expandir = FALSE,
    .etiquetar = TRUE
) {
  # Chequeos args ------------------------------------------------------------
  chequear_bases_hogares(.H, .P, .D)

  if (!is.logical(.expandir)) {
    cli::cli_abort(
      c(".etiquetar debe ser TRUE o FALSE.",
        "x" = "Se paso un {class(.expandir)}"
      ),
      class = "no_logical"
    )
  }
  if (!is.logical(.etiquetar)) {
    cli::cli_abort(
      c(".etiquetar debe ser TRUE o FALSE.",
        "x" = "Se paso un {class(.etiquetar)}"
      ),
      class = "no_logical"
    )
  }

  # Estandarización ----------------------------------------------------------
  cli::cli_h1("Estandarizacion")
  .H <- estandarizar_hogares_(.H, .D, anio, pais)

  # Calcular vbles -----------------------------------------------------------
  cli::cli_h1("Calcular variables nuevas")
  P <- agregar_personas(.P)
  .H <- dplyr::left_join(
    x = .H, y = P,
    by = dplyr::join_by(HB010 == pi01, HB020 == pi02, HB030 == pi04)
  )
  .H <- calcular_hogares_(.H)

  # Arreglos y devolver ------------------------------------------------------
  if (!.expandir) {
    .H <- dplyr::select(.H, dplyr::any_of(names(etq$H$variables)))
  } else {
    .H <- dplyr::relocate(.H, dplyr::any_of(names(etq$H$variables)))
  }

  if (.etiquetar) {
    .H <- etiquetar_eusilc_(.H, .base = "H")
  }
  
  .H <- structure(
    .H,
    "base"      = "H",
    "vbles. D"  = !is.null(.D),
    "vbles. LMH"= attr(.P, "vble. PL230"),
    "expandida" = .expandir
  )

  return(.H)
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