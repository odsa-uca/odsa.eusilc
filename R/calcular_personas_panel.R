# ----------------------------------------------------------------------------
#' Construye variables del conjunto P longitudinal de la EU-SILC (interna)
#'
#' @description
#' Construye variables armonizadas de identificación, demográficas, laborales
#' e ingresos a partir del conjunto P longitudinal estandarizado. Incorpora
#' factores PPA y aplica las recodificaciones y conversiones de ingresos.
#' Supone observaciones de 2021 en adelante. Es una función interna.
#'
#' @param .P `data.frame` o `tibble`. Conjunto P longitudinal preparado
#'   por [estandarizar_personas_panel_()], incluidos los auxiliares `maa` y `man`.
#'
#' @returns Conjunto P con las columnas de entrada y las variables armonizadas,
#'   conservando las filas y su orden. Reemplaza factores PPA homónimos y
#'   variables armonizadas preexistentes. No selecciona ni etiqueta columnas.
#'
#' @details
#' El catálogo de variables y sus unidades se presenta en
#' [expandir_personas_panel()].
#'
#' ## Recodificaciones y reglas nacionales
#'
#' La urbanización, el nivel educativo, la condición de actividad y las
#' calificaciones ocupacionales se recodifican mediante tablas. Los códigos
#' desconocidos quedan como `NA` en las clasificaciones derivadas.
#'
#' El nivel educativo conserva la pérdida de detalle publicada: IT agrupa
#' niveles secundarios en el código 300 y PT niveles inferiores en 200.
#'
#' Para ES, IT, DE, PL y PT se interpretan códigos ocupacionales ISCO-08 de dos
#' dígitos. Los códigos militares 1, 2 y 3 representan 01, 02 y 03 y pertenecen
#' al grupo principal 0. No se infiere precisión por la magnitud del valor ni
#' se extiende a DE la agrupación a un dígito de entregas anteriores a 2021.
#'
#' En PT, el código 14 agrupa las ocupaciones 11–14: se conserva como ocupación,
#' su grupo principal es 1, la calificación queda como `NA` por ambigüedad y la
#' calificación profesional conserva la categoría común 2. La restricción se
#' aplica antes de construir las variantes C y el empleo de calidad.
#'
#' ## Edades y variantes laborales
#'
#' Para la edad de entrevista se prioriza `RB082` sobre `RX010`, sin
#' reconstrucción mediante fechas. Para la edad al final del período de
#' referencia de ingresos se priorizan `RB081`, `RX020` y `PX020`. Si faltan,
#' se usa el año de encuesta menos el nacimiento (primero `RB080`, luego
#' `PB140`) menos uno: en ES, IT y PL, y en PT sólo si el nacimiento es
#' posterior al año de encuesta menos 80. Esta alternativa no se aplica en
#' DE ni en otros países. Las restricciones no afectan las edades difundidas.
#'
#' Las variantes laborales C seleccionan A para ocupados y B para no ocupados
#' por observación. Si falta la condición de actividad, C queda como `NA`.
#' No se pierde un valor conocido porque la otra variante esté completamente
#' ausente. El nacimiento y la ciudadanía distinguen LOC (categoría 1) de
#' EU/OTH (categoría 2); otros códigos quedan como `NA`.
#'
#' ## Informalidad y calidad del empleo
#'
#' Se conservan las definiciones transversales basadas en categoría ocupacional
#' y contribuciones a la seguridad social y a pensiones privadas. Las
#' contribuciones faltantes no se reemplazan por cero.
#'
#' La calidad del empleo combina informalidad, calificación de la ocupación y
#' permanencia del trabajo principal. Si alguno de los insumos requeridos está
#' completamente perdido, la variable correspondiente queda como `NA` para
#' todo el conjunto. La pérdida de detalle ocupacional puede limitarla también.
#'
#' ## Conversión de ingresos
#'
#' Primero se incorporan los factores PPA por país y año de encuesta,
#' reemplazando los factores homónimos de entrada. No se usan factores de otro
#' año ni se extrapolan los faltantes; para su definición ver [tabla_ppa].
#'
#' Los ingresos se construyen desde importes netos anuales en euros. Se
#' conservan valores negativos y los faltantes se propagan en las sumas.
#' Las horas anuales se aproximan mediante meses de actividad principal por
#' horas semanales actuales por 4,2, para ocupados actuales con ingreso distinto
#' de cero, meses positivos y horas semanales positivas.
#'
#' Antes de mensualizar, los ingresos horarios se calculan como ingreso anual
#' por `PX010` dividido por las horas anuales aproximadas. Se conserva cero
#' cuando el ingreso es cero, aunque falten meses, horas o el factor de moneda.
#' Un ingreso anual faltante produce un horario `NA`.
#'
#' Los diez ingresos mensuales se convierten con `PX010 / 12`; los horarios
#' no se mensualizan ni se convierten nuevamente a moneda nacional. Finalmente,
#' las doce variantes PPA se construyen con
#' `ingreso / ppa_factor * ppa_factor_us`, en dólares PPA por mes o por hora.
#' Si falta algún factor, las variantes PPA quedan como `NA`, incluso para
#' ingresos iguales a cero.
#'
#' @seealso [expandir_personas_panel()], [estandarizar_personas_panel_()]
calcular_personas_panel_ <- function(.P) {
  # Factores de conversión a PPA ---------------------------------------------
  .P <- dplyr::left_join(
    x = dplyr::select(.P, -dplyr::any_of(c("ppa_factor", "ppa_factor_us"))),
    y = tabla_ppa_,
    by = dplyr::join_by(PB010, PB020),
    relationship = "many-to-one"
  )

  # Recodificación mediante tablas de lookup --------------------------------
  .P <- dplyr::mutate(
    .P,
    pi07 = dplyr::recode_values(
      DB100,
      from = tabla_pi07$DB100,
      to = tabla_pi07$pi07,
      default = NA_integer_
    ),
    pd03 = dplyr::recode_values(
      PE041,
      from = tabla_pd03$PE041,
      to = tabla_pd03$pd03,
      default = NA_integer_
    ),
    pl01 = dplyr::recode_values(
      PL032,
      from = tabla_pl01$PL032,
      to = tabla_pl01$pl01,
      default = NA_integer_
    ),
    pl12a = dplyr::recode_values(
      PL051A,
      from = tabla_isco$PL051,
      to = tabla_isco$pl12,
      default = NA_integer_
    ),
    pl12b = dplyr::recode_values(
      PL051B,
      from = tabla_isco$PL051,
      to = tabla_isco$pl12,
      default = NA_integer_
    ),
    pl13a = dplyr::recode_values(
      PL051A,
      from = tabla_isco$PL051,
      to = tabla_isco$pl13,
      default = NA_integer_
    ),
    pl13b = dplyr::recode_values(
      PL051B,
      from = tabla_isco$PL051,
      to = tabla_isco$pl13,
      default = NA_integer_
    )
  )

  # Construcción de nuevas variables y recodificación -------------------------
  .P <- dplyr::mutate(
    .P,
    # Bloque I --------------------------------------------------------------
    pi01 = PB010,
    pi02 = PB020,
    pi03 = DB040,
    pi04 = PX030,
    pi05 = PB030,
    pi06a = RB062,
    pi06b = RB063,
    pi06c = RB064,
    pi06d = RB065,
    pi06e = RB066,
    pi08a = DB075,
    pi08b = DB076,
    # Bloque D --------------------------------------------------------------
    pd01a = dplyr::coalesce(RB082, RX010),
    pd01b = dplyr::coalesce(
      RB081,
      RX020,
      PX020,
      dplyr::case_when(
        PB020 %in%
          c("ES", "IT", "PL") |
          (PB020 == "PT" & dplyr::coalesce(RB080, PB140) > PB010 - 80) ~
          PB010 - dplyr::coalesce(RB080, PB140) - 1,
        .default = NA_real_
      )
    ),
    pd02 = PB150,
    pd04 = dplyr::case_when(
      RB280 == "LOC" ~ 1L,
      RB280 %in% c("EU", "OTH") ~ 2L,
      .default = NA_integer_
    ),
    pd05 = dplyr::case_when(
      RB290 == "LOC" ~ 1L,
      RB290 %in% c("EU", "OTH") ~ 2L,
      .default = NA_integer_
    ),
    # Bloque L --------------------------------------------------------------
    pl02a = PL040A,
    pl02b = PL040B,
    pl02c = dplyr::case_when(
      PL032 == 1 ~ pl02a,
      PL032 != 1 ~ pl02b,
      .default = NA_real_
    ),
    pl10a = PL051A,
    pl10b = PL051B,
    pl10c = dplyr::case_when(
      PL032 == 1 ~ pl10a,
      PL032 != 1 ~ pl10b,
      .default = NA_real_
    ),
    pl11a = dplyr::if_else(
      PL051A %in% tabla_isco$PL051,
      PL051A %/% 10,
      NA_real_
    ),
    pl11b = dplyr::if_else(
      PL051B %in% tabla_isco$PL051,
      PL051B %/% 10,
      NA_real_
    ),
    pl11c = dplyr::case_when(
      PL032 == 1 ~ pl11a,
      PL032 != 1 ~ pl11b,
      .default = NA_real_
    ),
    pl12a = dplyr::if_else(PB020 == "PT" & PL051A == 14, NA_real_, pl12a),
    pl12b = dplyr::if_else(PB020 == "PT" & PL051B == 14, NA_real_, pl12b),
    pl12c = dplyr::case_when(
      PL032 == 1 ~ pl12a,
      PL032 != 1 ~ pl12b,
      .default = NA_real_
    ),
    pl13c = dplyr::case_when(
      PL032 == 1 ~ pl13a,
      PL032 != 1 ~ pl13b,
      .default = NA_real_
    ),
    pl40a = calc_informalidad(PL040A, PY030G, PY035G, "a"),
    pl40b = calc_informalidad(PL040A, PY030G, PY035G, "b"),
    toc = dplyr::case_when(
      PL141 %in% c(11, 21) ~ 1L,
      PL141 %in% c(12, 22) ~ 2L,
      .default = NA_integer_
    ),
    pomj = dplyr::case_when(
      PL141 %in% c(21, 22) ~ 1L,
      PL141 %in% c(11, 12) ~ 2L,
      .default = NA_integer_
    ),
    pl41 = calc_calidad(pl40a, pl12a, pomj),
    # Bloque Y --------------------------------------------------------------
    py00 = PY010N +
      PY050N +
      PY090N +
      PY110N +
      PY120N +
      PY130N +
      PY140N +
      PY100N +
      PY080N,
    py10 = PY010N + PY050N,
    py11 = PY010N,
    py12 = PY050N,
    py20 = PY090N + PY110N + PY120N + PY130N + PY140N + PY100N + PY080N,
    py21 = PY100N + PY080N,
    py22 = PY100N,
    py23 = PY080N,
    py24 = PY090N,
    py25 = PY110N + PY120N + PY130N + PY140N,
    haa = dplyr::if_else(
      pl01 == 1 & py11 != 0 & maa > 0 & PL060 > 0,
      maa * PL060 * 4.2,
      NA_real_
    ),
    han = dplyr::if_else(
      pl01 == 1 & py12 != 0 & man > 0 & PL060 > 0,
      man * PL060 * 4.2,
      NA_real_
    )
  )

  # Ingresos horarios, mensuales y PPA ---------------------------------------
  .P <- dplyr::mutate(
    .P,
    py11h = dplyr::if_else(py11 != 0, (py11 * PX010) / haa, 0),
    py12h = dplyr::if_else(py12 != 0, (py12 * PX010) / han, 0),
    dplyr::across(
      c(py00, py10, py11, py12, py20, py21, py22, py23, py24, py25),
      \(y) (y * PX010) / 12
    ),
    dplyr::across(
      c(
        py00,
        py10,
        py11,
        py12,
        py20,
        py21,
        py22,
        py23,
        py24,
        py25,
        py11h,
        py12h
      ),
      \(y) y / ppa_factor * ppa_factor_us,
      .names = "{.col}ppa"
    )
  )

  return(.P)
}
