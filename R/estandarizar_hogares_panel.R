# ----------------------------------------------------------------------------
#' Estandariza el conjunto H longitudinal de la EU-SILC (interna)
#'
#' @description
#' Prepara el conjunto H longitudinal para la armonización: completa insumos
#' ausentes e incorpora información desde D. Supone observaciones de 2021 en
#' adelante. Es una función interna.
#'
#' @param .H `data.frame` o `tibble`. Conjunto H longitudinal en formato largo,
#'   de un único país, con una fila por hogar y año.
#' @param .P `data.frame` o `tibble`. Conjunto P longitudinal armonizado por
#'   [expandir_personas_panel()], del mismo país y de los años correspondientes.
#'   Actualmente no interviene en la estandarización; su información se agrega
#'   e incorpora a H en [expandir_hogares_panel()].
#' @param .D `data.frame` o `tibble`, o `NULL` (por defecto). Conjunto D
#'   longitudinal del mismo país y de los años correspondientes.
#'
#' @returns `data.frame` o `tibble`. Conjunto H estandarizado, con las filas y
#'   el orden originales. Conserva las columnas de entrada y los insumos
#'   añadidos como `NA`. Reemplaza los insumos correspondientes cuando se
#'   proporciona D. No selecciona ni etiqueta columnas.
#'
#' @details
#' ## Disponibilidad de insumos
#'
#' Se completan con `NA` las columnas opcionales ausentes relacionadas con
#' ingresos netos del hogar, conversión de moneda y tamaño del hogar, así como
#' las requeridas desde D. Esto permite propagar los faltantes en los cálculos
#' posteriores. Los identificadores `HB010`, `HB020` y `HB030` deben estar
#' presentes en H. No se imputan valores ni se balancea el panel.
#'
#' ## Traspaso desde D
#'
#' D proporciona ponderación longitudinal, región, urbanización, grupo de
#' rotación (`DB075`) y número de encuesta dentro del panel (`DB076`). El cruce
#' utiliza año, país y hogar: `HB010/HB020/HB030` en H y `DB010/DB020/DB030`
#' en D. Cada clave coincidente debe identificar una única fila en D; las
#' coincidencias múltiples producen un error.
#'
#' Si se suministra D, sus valores reemplazan los insumos correspondientes de
#' H, incluso cuando faltan o no hay coincidencia; en esos casos quedan como
#' `NA`. Sin D, se conservan los insumos ya incorporados en H y se completan
#' los ausentes.
#'
#' ## Diferencias nacionales
#'
#' Se conserva la información publicada de región y urbanización, sin
#' reconstruir categorías agrupadas o suprimidas ni equiparar los niveles
#' regionales nacionales. Actualmente no se aplican transformaciones
#' específicas por país en esta etapa.
#'
#' @seealso [expandir_hogares_panel()], [calcular_hogares_panel_()]
estandarizar_hogares_panel_ <- function(.H, .P, .D = NULL) {
  insumos_h <- c(
    "HY040N",
    "HY050N",
    "HY060N",
    "HY070N",
    "HY080N",
    "HY090N",
    "HY110N",
    "HX010",
    "HX040"
  )
  for (variable in setdiff(insumos_h, names(.H))) {
    .H[[variable]] <- rep(NA_real_, nrow(.H))
  }
  insumos_d <- list(
    DB095 = NA_real_,
    DB040 = NA_character_,
    DB100 = NA_integer_,
    DB075 = NA_integer_,
    DB076 = NA_integer_
  )
  if (is.null(.D)) {
    for (variable in setdiff(names(insumos_d), names(.H))) {
      .H[[variable]] <- rep(insumos_d[[variable]], nrow(.H))
    }
  } else {
    for (variable in setdiff(names(insumos_d), names(.D))) {
      .D[[variable]] <- rep(insumos_d[[variable]], nrow(.D))
    }
  }

  # Traspaso de variables desde D --------------------------------------------
  if (!is.null(.D)) {
    .H <- dplyr::left_join(
      x = dplyr::select(.H, -dplyr::any_of(names(insumos_d))),
      y = dplyr::select(
        .D,
        DB010,
        DB020,
        DB030,
        dplyr::all_of(names(insumos_d))
      ),
      by = dplyr::join_by(HB010 == DB010, HB020 == DB020, HB030 == DB030),
      relationship = "many-to-one"
    )
  }

  return(.H)
}
