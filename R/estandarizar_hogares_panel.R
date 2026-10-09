# ----------------------------------------------------------------------------
#' Estandariza el conjunto H longitudinal (interna)
#'
#' @param .H `data.frame` o `tibble`. Conjunto H longitudinal en formato largo,
#'   de un único país, con una fila por hogar y año.
#' @param .P `data.frame` o `tibble`. Conjunto P longitudinal armonizado por
#'   [expandir_personas_panel()], del mismo país y de los años correspondientes.
#' @param .D `data.frame` o `tibble`, o `NULL` (por defecto). Conjunto D
#'   longitudinal del mismo país y de los años correspondientes.
#'
#' @returns `tibble`. Conjunto H estandarizado. Los insumos faltantes quedan
#'   como `NA`.
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
    DB100 = NA_integer_
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