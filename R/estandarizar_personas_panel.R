# ----------------------------------------------------------------------------
#' Estandariza el conjunto P longitudinal de la EU-SILC (interna)
#'
#' @description
#' Prepara el conjunto P longitudinal para la armonización: completa insumos
#' ausentes, incorpora variables desde R y D y aplica transformaciones
#' nacionales. Supone observaciones de 2021 en adelante. Es una función interna.
#'
#' @param .P `data.frame` o `tibble`. Conjunto P longitudinal en formato largo,
#'   con una fila por persona y año.
#' @param .D `data.frame` o `tibble`, o `NULL` (por defecto). Conjunto D
#'   longitudinal del mismo país y de los años correspondientes.
#' @param .R `data.frame` o `tibble`, o `NULL` (por defecto). Conjunto R
#'   longitudinal del mismo país y de los años correspondientes.
#'
#' @returns Conjunto P estandarizado, con las filas y el orden originales.
#'   Conserva los insumos añadidos como `NA` e incluye los auxiliares `maa` y
#'   `man`. No agrega atributos de armonización.
#'
#' @details
#' ## Disponibilidad de insumos
#'
#' Se completan con `NA` las columnas de insumo opcionales ausentes, de modo
#' que los cálculos posteriores puedan propagar los faltantes. Los insumos de
#' identificación `PB010`, `PB020`, `PB030`, `PX030` y `PB150` deben estar
#' presentes en P. La función no balancea el panel ni crea observaciones.
#'
#' ## Traspaso desde R y D
#'
#' Desde R se incorporan `RB062`–`RB066` (ponderadores longitudinales),
#' `RB080`, `RB081`, `RB082`, `RX010` y `RX020` (nacimiento y edades), y
#' `RB280` y `RB290` (país de nacimiento y ciudadanía).
#'
#' Desde D se incorporan `DB040` (región), `DB100` (urbanización),
#' `DB075` (grupo de rotación) y `DB076` (número de encuesta).
#'
#' El cruce con R utiliza año, país y persona: `PB010/PB020/PB030` en P y
#' `RB010/RB020/RB030` en R. El cruce con D utiliza año, país y hogar actual:
#' `PB010/PB020/PX030` en P y `DB010/DB020/DB030` en D. El hogar puede cambiar
#' entre olas. Cada clave debe identificar una única fila en el auxiliar;
#' claves duplicadas que coinciden con P producen un error.
#'
#' Si se suministra un auxiliar, sus valores reemplazan los insumos
#' correspondientes de P, incluso si están ausentes o no hay coincidencia.
#' En esos casos quedan como `NA`. Sin el auxiliar se conservan los insumos
#' ya incorporados en P y se completan los ausentes.
#'
#' ## Transformaciones nacionales
#'
#' En Italia, `PY120N` se establece en cero por observación cuando
#' `PY120N_F` vale -4, porque el importe se integra en otros componentes.
#' Es una regla contable independiente de la imputación. Si la flag no está
#' disponible o no indica esa situación, se conserva el valor publicado.
#' No se revierten agrupaciones ni perturbaciones.
#'
#' ## Auxiliares mensuales de actividad
#'
#' `maa` cuenta meses de actividad principal asalariada (códigos 1/2), y
#' `man`, no asalariada (3/4, incluidos trabajadores familiares), desde
#' `PL211A`–`PL211L`. No describen meses de cobro comprobado.
#'
#' Se requieren doce códigos entre 1 y 11. Un mes ausente, código desconocido
#' o flag mensual disponible negativa deja ambos conteos como `NA`. Las flags
#' positivas, incluidas imputaciones de Eurostat, se aceptan; las flags ausentes
#' o con `NA` no invalidan un código mensual válido. Los conteos se reconstruyen
#' desde el calendario, reemplazando auxiliares homónimos preexistentes.
#'
#' @seealso [expandir_personas_panel()], [calcular_personas_panel_()]
estandarizar_personas_panel_ <- function(.P, .D = NULL, .R = NULL) {
  # Estandarización de los conjuntos -----------------------------------------
  insumos_r <- list(
    RB062 = NA_real_,
    RB063 = NA_real_,
    RB064 = NA_real_,
    RB065 = NA_real_,
    RB066 = NA_real_,
    RB080 = NA_integer_,
    RB081 = NA_integer_,
    RB082 = NA_integer_,
    RX010 = NA_integer_,
    RX020 = NA_integer_,
    RB280 = NA_character_,
    RB290 = NA_character_
  )
  insumos_d <- list(
    DB075 = NA_integer_,
    DB076 = NA_integer_,
    DB040 = NA_character_,
    DB100 = NA_integer_
  )
  if (is.null(.R)) {
    for (variable in setdiff(names(insumos_r), names(.P))) {
      .P[[variable]] <- rep(insumos_r[[variable]], nrow(.P))
    }
  } else {
    for (variable in setdiff(names(insumos_r), names(.R))) {
      .R[[variable]] <- rep(insumos_r[[variable]], nrow(.R))
    }
  }
  if (is.null(.D)) {
    for (variable in setdiff(names(insumos_d), names(.P))) {
      .P[[variable]] <- rep(insumos_d[[variable]], nrow(.P))
    }
  } else {
    for (variable in setdiff(names(insumos_d), names(.D))) {
      .D[[variable]] <- rep(insumos_d[[variable]], nrow(.D))
    }
  }

  insumos <- c(
    "PY010N",
    "PY050N",
    "PY080N",
    "PY090N",
    "PY100N",
    "PY110N",
    "PY120N",
    "PY130N",
    "PY140N",
    "PX010",
    "PB140",
    "PX020",
    "PE041",
    "PL032",
    "PL040A",
    "PL040B",
    "PL051A",
    "PL051B",
    "PY030G",
    "PY035G",
    "PL141",
    "PL060",
    paste0("PL211", LETTERS[1:12])
  )
  for (variable in setdiff(insumos, names(.P))) {
    .P[[variable]] <- rep(NA_real_, nrow(.P))
  }

  if ("PY120N_F" %in% names(.P)) {
    .P <- dplyr::mutate(
      .P,
      PY120N = dplyr::if_else(
        PB020 == "IT" & PY120N_F == -4,
        0,
        PY120N,
        missing = PY120N
      )
    )
  }

  # Traspaso de variables desde conjuntos auxiliares -------------------------
  if (!is.null(.R)) {
    .P <- dplyr::left_join(
      x = dplyr::select(.P, -dplyr::any_of(names(insumos_r))),
      y = dplyr::select(
        .R,
        RB010,
        RB020,
        RB030,
        dplyr::all_of(names(insumos_r))
      ),
      by = dplyr::join_by(PB010 == RB010, PB020 == RB020, PB030 == RB030),
      relationship = "many-to-one"
    )
  }
  if (!is.null(.D)) {
    .P <- dplyr::left_join(
      x = dplyr::select(.P, -dplyr::any_of(names(insumos_d))),
      y = dplyr::select(
        .D,
        DB010,
        DB020,
        DB030,
        dplyr::all_of(names(insumos_d))
      ),
      by = dplyr::join_by(PB010 == DB010, PB020 == DB020, PX030 == DB030),
      relationship = "many-to-one"
    )
  }
  
  # Variables auxiliares para la imputación -----------------------------------
  .P <- dplyr::mutate(
    .P,
    maa = calcular_meses_actividad(.P, c(1, 2)),
    man = calcular_meses_actividad(.P, c(3, 4))
  )

  return(.P)
}
