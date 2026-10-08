# ----------------------------------------------------------------------------
#' Armoniza variables del conjunto P longitudinal de la EU-SILC
#'
#' @description
#' Construye variables de identificación, demográficas, laborales e ingresos del conjunto P
#' longitudinal de la EU-SILC, suponiendo observaciones de 2021 en adelante.
#' Incorpora ponderadores y demográficos desde R, y región, urbanización y
#' metadatos panel desde D cuando se suministran.
#' Las flags todavía no tienen efecto.
#'
#' @param .P `data.frame` o `tibble`. Conjunto P longitudinal en formato largo,
#'   de un único país y varios años, con una fila por persona y año.
#' @param .D `data.frame` o `tibble`, o `NULL` (por defecto). Conjunto D
#'   longitudinal del cual se incorporan `DB040`, `DB100`, `DB075` y `DB076`.
#' @param .R `data.frame` o `tibble`, o `NULL` (por defecto). Conjunto R
#'   longitudinal del cual se incorporan los ponderadores `RB062`–`RB066`,
#'   `RB080`, `RB081`, `RB082`, `RX010`, `RX020`, `RB280` y `RB290`.
#' @param .imputar `TRUE` o `FALSE` (por defecto). ¿Imputar valores faltantes o
#'   inconsistentes dentro de cada ola? Pendiente de implementación.
#' @param .expandir `TRUE` o `FALSE` (por defecto). ¿Conservar las columnas
#'   originales en el resultado? Pendiente de implementación.
#' @param .etiquetar `TRUE` (por defecto) o `FALSE`. ¿Aplicar etiquetas a las
#'   variables y sus valores? Pendiente de implementación.
#'
#' @returns `data.frame` o `tibble` con las variables originales y las variables
#'   armonizadas. Conserva las filas y su orden, incluidos paneles desbalanceados.
#'   Los ponderadores y metadatos panel se renombran y quedan como `NA` cuando
#'   sus insumos no están disponibles.
#'
#' @details
#' Se construyen `pi01`, `pi02`, `pi04`, `pi05` y `pd02` desde `PB010`, `PB020`,
#' `PX030`, `PB030` y `PB150`, respectivamente. Estos insumos deben estar presentes.
#'
#' `pi03` conserva la región publicada en `DB040`; `pi07` recodifica `DB100`
#' mediante `tabla_pi07`. `pd01a` prioriza `RB082` sobre `RX010` y no se
#' reconstruye con fechas de nacimiento y entrevista. `pd01b` prioriza `RB081`,
#' `RX020` y `PX020`; si faltan, usa el año de encuesta menos el nacimiento
#' (primero `RB080`, luego `PB140`) menos uno. Esta alternativa se aplica para
#' ES, IT y PL, y para PT sólo si el nacimiento es posterior al año de encuesta
#' menos 80. No se aplica para DE ni otros países fuera del alcance revisado.
#' Las restricciones afectan la reconstrucción, no las edades difundidas.
#'
#' `pd03` recodifica `PE041` mediante `tabla_pd03`, conservando las limitaciones
#' de los códigos publicados: IT agrupa niveles secundarios en 300 y PT niveles
#' inferiores en 200. `pd04` y `pd05` recodifican `RB280` y `RB290`: LOC como 1,
#' EU/OTH como 2 y códigos desconocidos como `NA`.
#'
#' `pl01` recodifica `PL032` mediante `tabla_pl01`; `pl02a/b` conservan
#' `PL040A/B`. `pl02c` toma A para ocupados y B para no ocupados según `PL032`,
#' y queda como `NA` si falta la actividad. `toc` distingue contrato escrito
#' (11/21) y verbal (12/22), y `pomj` permanente (21/22) y temporal (11/12),
#' desde `PL141`. Todas estas variables armonizadas se construyen aunque falten
#' sus insumos, completados con `NA` durante la estandarización.
#'
#' `pl10a/b` conservan la ocupación publicada en `PL051A/B`; `pl11a/b`
#' identifican el grupo principal ISCO y `pl12a/b` y `pl13a/b` recodifican
#' calificación y calificación profesional mediante `tabla_isco`. Las variantes
#' C seleccionan A para ocupados y B para no ocupados por observación, sin perder
#' valores conocidos porque la otra fuente esté completamente ausente.
#'
#' Para ES, IT, DE, PL y PT, las reglas longitudinales de las entregas L21–L25
#' se interpretan con códigos ISCO de dos dígitos: 1, 2 y 3 representan las
#' ocupaciones militares 01, 02 y 03 (grupo principal 0). No se infiere precisión
#' por la magnitud del valor ni se extiende a DE la agrupación a un dígito de
#' entregas anteriores a 2021. En PT, el código 14 agrupa las ocupaciones 11–14:
#' se conserva en `pl10*`, el grupo es 1, `pl12*` queda como `NA` por ambigüedad
#' y `pl13*` conserva la categoría común 2. No se recupera el detalle eliminado.
#' Los códigos desconocidos quedan como `NA` en las clasificaciones derivadas.
#'
#' `pl40a/b` reutilizan [calc_informalidad()] con `PL040A`, `PY030G` y `PY035G`;
#' `pl41` reutiliza [calc_calidad()] con `pl40a`, `pl12a` y `pomj`. Conservan las
#' reglas transversales, incluido devolver `NA` para todo el vector si algún
#' insumo requerido está completamente perdido. Las contribuciones faltantes no
#' se convierten en cero ni se trasladan automáticamente advertencias nacionales
#' transversales. La pérdida de detalle ocupacional puede limitar también `pl41`.
#'
#' Los ingresos `py00`, `py10`, `py11`, `py12`, `py20`, `py21`, `py22`, `py23`,
#' `py24` y `py25` siguen las definiciones de [calcular_personas()]. Se expresan
#' en moneda nacional por mes, convirtiendo los importes netos anuales en euros
#' mediante `PX010 / 12`. Se conservan los ingresos negativos y los faltantes se
#' propagan en las sumas. Si falta una columna de ingresos netos o `PX010`, las
#' variables que la requieren quedan como `NA`, sin impedir los otros cálculos.
#'
#' `maa` y `man` cuentan los meses con actividad principal asalariada (códigos
#' 1/2) y no asalariada (3/4, incluidos trabajadores familiares) desde
#' `PL211A`–`PL211L`, mediante [calcular_meses_actividad()]. No equivalen a meses
#' de cobro comprobado. Requieren doce códigos entre 1 y 11; un mes ausente,
#' código desconocido o flag mensual disponible negativa deja ambos conteos
#' como `NA`. Las flags positivas, incluidas imputaciones de Eurostat, son
#' aceptadas; flags ausentes o con `NA` no invalidan un código mensual válido.
#'
#' `haa` y `han` aproximan las horas anuales como meses de actividad por horas
#' semanales actuales (`PL060`) por 4,2. Se calculan sólo para ocupados actuales
#' con ingreso correspondiente distinto de cero, meses positivos y horas
#' semanales positivas. No reconstruyen las horas históricas de cada empleo.
#' `py11h` y `py12h` dividen el ingreso neto anual convertido a moneda nacional
#' por esas horas; conservan cero cuando el ingreso es cero. A diferencia del
#' cálculo transversal, horas semanales no positivas dejan las horas anuales
#' como `NA` y no generan un ingreso horario infinito por división por cero.
#' Los ingresos horarios no se mensualizan ni se convierten nuevamente a moneda
#' nacional.
#'
#' `ppa_factor` y `ppa_factor_us` se incorporan desde [tabla_ppa] por país y año
#' de encuesta, reemplazando factores homónimos preexistentes. Para los diez
#' ingresos mensuales y los dos horarios se crean variantes con sufijo `ppa`,
#' mediante `ingreso / ppa_factor * ppa_factor_us`, como en
#' [calcular_personas()]. Se expresan en dólares PPA mensuales o por hora,
#' respectivamente. La tabla interna cubre 2016–2025; sin coincidencia de país
#' y año, los factores y variantes PPA quedan como `NA`, sin extrapolación.
#'
#' En Italia, `PY120N` se establece en cero por observación cuando su flag
#' `PY120N_F` vale -4, indicando su integración en otros componentes. Esta regla
#' contable se aplica independientemente de `.imputar`. Sin la flag se conserva
#' el valor publicado. No se revierten agrupaciones ni perturbaciones publicadas.
#'
#' Los insumos `RB062`, `RB063`, `RB064`, `RB065` y `RB066`
#' se copian como `pi06a`, `pi06b`, `pi06c`, `pi06d` y `pi06e`, para paneles de
#' dos a seis años, respectivamente. `DB075` y `DB076` se copian como `pi08a`
#' (grupo de rotación) y `pi08b` (número de encuesta). No se elige un ponderador
#' automáticamente. Las siete variables armonizadas están siempre presentes;
#' los insumos ausentes se completan con `NA` durante la estandarización.
#'
#' Los cruces con `.R` incluyen país, año y persona; con `.D`, país, año y hogar
#' actual, que puede cambiar entre olas. Cada clave debe identificar una única
#' fila en el auxiliar. Sin coincidencia, las variables traspasadas quedan como
#' `NA`. Si se suministra un auxiliar, sus valores reemplazan los insumos
#' correspondientes de `.P`; si no se suministra, se conservan los insumos ya
#' incorporados en `.P`. No se balancea el panel ni se crean observaciones.
#'
#' Los insumos y ponderadores longitudinales requieren un tratamiento propio:
#' no debe suponerse la disponibilidad de las variables transversales ni elegirse
#' automáticamente un ponderador, cuya duración de seguimiento debe considerarse.
#' Las agrupaciones, supresiones y perturbaciones documentadas por Eurostat no
#' se tratarán automáticamente como inconsistencias imputables. La imputación
#' prevista utilizará únicamente información dentro de cada ola.
#'
#' Quedan pendientes el traspaso de otras variables desde `.D` y `.R`, la imputación,
#' la selección mediante `.expandir` y el etiquetado mediante `.etiquetar`.
#' Esta versión conserva todas las columnas originales, salvo
#' la transformación contable y el reemplazo de factores PPA indicados, y no agrega atributos
#' de armonización, imputación o etiquetado.
#' También queda pendiente `pd01c`.
#'
#' @seealso [expandir_hogares_panel()]
#' @export
expandir_personas_panel <- function(
  .P,
  .D = NULL,
  .R = NULL,
  .imputar = FALSE,
  .expandir = FALSE,
  .etiquetar = TRUE
) {
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
  insumos_ausentes <- setdiff(insumos, names(.P))
  for (variable in insumos_ausentes) {
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
  # Pendiente: incorporar otras variables auxiliares de D y R.
  # Variables auxiliares necesarias en imputación ------------------------------
  .P <- dplyr::mutate(
    .P,
    maa = calcular_meses_actividad(.P, c(1, 2)),
    man = calcular_meses_actividad(.P, c(3, 4))
  )

  # Imputación dentro de cada ola --------------------------------------------
  if (.imputar) {
    # Pendiente: imputar valores faltantes o inconsistentes dentro de cada ola.
  }

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
    pi07 = dplyr::recode_values(
      DB100,
      from = tabla_pi07$DB100,
      to = tabla_pi07$pi07,
      default = NA_integer_
    ),
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
    pd03 = dplyr::recode_values(
      PE041,
      from = tabla_pd03$PE041,
      to = tabla_pd03$pd03,
      default = NA_integer_
    ),
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
    pl01 = dplyr::recode_values(
      PL032,
      from = tabla_pl01$PL032,
      to = tabla_pl01$pl01,
      default = NA_integer_
    ),
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
    pl12a = dplyr::if_else(PB020 == "PT" & PL051A == 14, NA_real_, pl12a),
    pl12b = dplyr::if_else(PB020 == "PT" & PL051B == 14, NA_real_, pl12b),
    pl12c = dplyr::case_when(
      PL032 == 1 ~ pl12a,
      PL032 != 1 ~ pl12b,
      .default = NA_real_
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
    ),
    py11h = dplyr::if_else(py11 != 0, (py11 * PX010) / haa, 0),
    py12h = dplyr::if_else(py12 != 0, (py12 * PX010) / han, 0)
  )

  .P <- dplyr::left_join(
    x = dplyr::select(.P, -dplyr::any_of(c("ppa_factor", "ppa_factor_us"))),
    y = tabla_ppa_,
    by = dplyr::join_by(PB010, PB020),
    relationship = "many-to-one"
  )
  .P <- dplyr::mutate(
    .P,
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

  # Selección y ordenamiento de variables ------------------------------------
  .P <- dplyr::select(.P, -dplyr::any_of(insumos_ausentes))
  # Pendiente: conservar columnas originales según .expandir.

  # Etiquetado de variables y valores ----------------------------------------
  if (.etiquetar) {
    # Pendiente: aplicar las etiquetas correspondientes al panel.
  }

  # Devolución del conjunto panel --------------------------------------------
  return(.P)
}
