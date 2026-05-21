#' Imputa valores faltantes o inconsistentes en el conjunto P de la EU-SILC
#'
#' @description
#' Imputa valores faltantes o inconsistentes (según los criterios de armonización)
#' en el conjunto P de la EU-SILC estandarizado con [estandarizar_personas()].
#' Las variables imputadas tienen que ver con las horas trabajadas semanalmente,
#' los meses con percepción de ingresos laborales en el IRP y con características
#' de la ocupación y el lugar de trabajo.
#'
#' @param .datos `data.frame` o `tibble`. Conjunto de datos P de la EU-SILC
#'               estandarizado con [estandarizar_personas()].
#' @param .anio `numeric`. Año de la encuesta.
#'
#' @returns `tibble`. Conjunto P de la EU-SILC con valores imputados.
#'
#' @details
#'
#' ## Valores imputados
#'
#' La función imputa valores faltantes y valores considerados inconsistentes
#' según los criterios de armonización. En general estos se derivan de la
#' comparación de los montos de ingresos en el IRP, la cantidad de meses
#' trabajados en el IRP y la condición de actividad de la persona. A
#' continuación se detalla el criterio de imputación de las variables
#' consideradas.
#'
#' ### Meses con ingreso por trabajo asalariado y no asalariado en el IRP (maa y man)
#'
#' Las variables de la EU-SILC que registran esta información son las PL073 y
#' PL074 para el ingreso asalariado (full- y part-time respectivamente), y las
#' PL075 y PL076 para el ingreso no asalariado (full- y part-time). La suma de
#' meses con ingresos full o part-time está restringida a los valores 0, 1, ..., 12.
#' Como no era de interés la distinción entre full o part-time, para evitar los
#' problemas que introducía esta restricción se imputaron directamente las sumas:
#'
#' - maa = PL073 + PL074
#' - man = PL075 + PL076
#'
#' Los valores se imputaron si la persona percibió los ingresos correspondientes
#' en el IRP pero la cantidad de los meses está perdida o es cero.
#'
#' ### Horas semanales habitualmente trabajadas (PL060)
#'
#' Los valores se imputaron si estaban perdidos según lo reportado por EUROSTAT
#' (PL060_F = -1). No se imputaron los casos en los que EUROSTAT informa que las
#' horas son demasiado variables y no se pueden establecer (PL060_F = -2).
#'
#' ### Categoría ocupacional, ocupación y rama de actividad (A)
#'
#' Este grupo de variables corresponde a la ocupación actual de los respondentes
#' ocupados en el CRP (PL032 = 1). Se imputan los valores perdidos según lo
#' reportado por EUROSTAT. Como la condición de imputación es la misma para las
#' tres variables (PL032 = 1 y valor perdido), se imputan de forma simultánea.
#'
#' ### Categoría ocupacional, ocupación y rama de actividad (B)
#'
#' Este grupo de variables corresponde a la última ocupación de los respondentes
#' no ocupados en el CRP (PL032 != 1). Se imputan los valores perdidos según lo
#' reportado por EUROSTAT para aquellos que percibieron ingreso laboral en el
#' IRP y están desocupados en el CRP (PL032 != 1) o se desconoce su condición
#' de actividad (PL032 perdido). Al igual que con el bloque A, estas variables
#' se imputan simultáneamente.
#'
#' ### Tamaño del establecimiento
#'
#' Se imputan los valores perdidos según lo reportado por EUROSTAT (PL130_F = -1)
#' y aquellos que son *coarse* (PL130 = 14 o 15). Se imputan por separado los
#' que son propiamente faltantes, los que se sabe únicamente que son menores
#' que 10 y los que se sabe únicamente que son mayores que 10.
#'
#' ### Sector público o privado
#'
#' Se imputan los valores perdidos según lo reportado por EUROSTAT (PL230_F = -1)
#' y aquellos en los que el respondente no sabe (PL230 = 99) cuando son ocupados
#' y asalariados (PL032 = 1 y PL040A = 3).
#'
#' ## Modelos de imputación
#'
#' Por el momento, las imputaciones se hacen con bosques aleatorios implementados
#' con [missRanger::missRanger()]. Se optó por estos modelos por simplicidad y
#' porque las distribuciones de las variables suelen ser complejas.
#'
#' @export
imputar_personas <- function(
    .datos,
    .anio
) {
  # Flags ------------------------------------
  .datos <- calc_flags_imputacion(.datos, .anio)

  .datos <- dplyr::mutate(
    .datos,
    maa = dplyr::case_when(
      .f_maa == -1 ~ NA_integer_,
      .default = PL073 + PL074
    ),
    man = dplyr::case_when(
      .f_man == -1 ~ NA_integer_,
      .default = PL075 + PL076
    )
  )

  # Imputaciones -----------------------------
  .datos <-  imputar_meses(.datos)
  .datos <-  imputar_horas(.datos)
  .datos <-  imputar_laboral_a(.datos)
  .datos <-  imputar_laboral_b(.datos, .anio)

  if ("PL130" %in% names(.datos)) {
    .datos <- imputar_tamanio(.datos)
  }

  if ("PL230" %in% names(.datos)) {
    .datos <- imputar_sectorpp(.datos)
  }

  # Devolver -----------------------------------------------------------------
  return(.datos)
}

# ============================================================================
#' Title
#'
#' @param .datos .datos
#' @param .imputadas .imputadas
#' @param .predictoras .predictoras
#' @param .flags .flags
#' @param .factores .factores
#'
#' @returns conjunto de datos para imputar
armar_imputables <- function(
  .datos,
  .imputadas,
  .predictoras,
  .flags,
  .factores = NULL
) {
  datos_imp <- .datos[
    .datos[[.flags[1]]] %in% c(-1, 1),
    unique(c("PB010", "PB020", "PB030", .predictoras, .imputadas, .flags))
  ]

  for (.vble in .factores) {
    datos_imp[.vble] <- factor(datos_imp[[.vble]])
  }

  return(datos_imp)
}

# ============================================================================
#' Title
#'
#' @param .datos .datos
#' @param .anio .anio
#'
#' @returns .datos con flags de imputacion
calc_flags_imputacion <- function(.datos, .anio) {
  .datos <- dplyr::mutate(
    .datos,
    .f_maa = dplyr::case_when(
      PY010N != 0 & (is.na(PL073 + PL074) | PL073 + PL074 == 0) ~ -1,
      PY010N != 0 & !is.na(PL073 + PL074) & PL073 + PL074 != 0 ~ 1,
      .default = 0
    ),
    .f_man = dplyr::case_when(
      PY050N != 0 & (is.na(PL075 + PL076) | PL075 + PL076 == 0) ~ -1,
      PY050N != 0 & !is.na(PL075 + PL076) & PL075 + PL076 != 0 ~ 1,
      .default = 0
    ),
    .f_PL060 = dplyr::if_else(PL060_F %in% c(-1, 1), PL060_F, 0),
    .f_PL040A = dplyr::if_else(PL040A_F %in% c(-1, 1), PL040A_F, 0),
    .f_PL051A = dplyr::if_else(PL051A_F %in% c(-1, 1), PL051A_F, 0),
    .f_PL111A = dplyr::if_else(PL111A_F %in% c(-1, 1), PL111A_F, 0),
    .f_PL040B = dplyr::case_when(
      PY010N + PY050N != 0 & (PL032 != 1 | is.na(PL032)) & PL040B_F %in% c(-1, -2) ~ -1,
      PY010N + PY050N != 0 & PL040B_F == 1 ~ 1,
      .default = 0
    ),
    .f_PL051B = dplyr::case_when(
      PY010N + PY050N != 0 & (PL032 != 1 | is.na(PL032)) & PL051B_F %in% c(-1, -2) ~ -1,
      PY010N + PY050N != 0 & PL051B_F == 1 ~ 1,
      .default = 0
    )
  )

  if (.anio >= 2021) {
    .datos <- dplyr::mutate(
      .datos,
      .f_PL111B = dplyr::case_when(
        PY010N + PY050N != 0 & (PL032 != 1 | is.na(PL032)) & PL111B_F %in% c(-1, -2) ~ -1,
        PY010N + PY050N != 0 & PL111B_F == 1 ~ 1,
        .default = 0
      )
    )
  }

  if ("PL130" %in% names(.datos)) {
    .datos <- dplyr::mutate(
      .datos,
      .f_PL130 = dplyr::case_when(
        PL130 == 14 | PL130 == 15 | PL130_F == -1 ~ -1,
        .default = 0
      ),
      .fa_PL130 = dplyr::case_when(
        PL130 == 14 ~ -1,
        PL130 < 10 ~ 1,
        .default = 0
      ),
      .fb_PL130 = dplyr::case_when(
        PL130 == 15 ~ -1,
        PL130 %in% 10:13 ~ 1,
        .default = 0
      ),
      .fc_PL130 = dplyr::case_when(
        PL130_F == -1 ~ -1,
        PL130_F == 1 & PL130 != 14 & PL130 != 15 ~ 1,
        .default = 0
      )
    )
  }

  if ("PL230" %in% names(.datos)) {
    .datos <- dplyr::mutate(
      .datos,
      .f_PL230 = dplyr::case_when(
        PL032 == 1 & PL040A == 3 & PL230 == 99 ~ -1,
        PL230_F %in% c(-1, 1) ~ PL230_F,
        .default = 0
      )
    )
  }

  return(.datos)
}

# ============================================================================
#' Title
#'
#' @param .datos .datos
#' @param .imp .imp
#' @param .vble .vble
#' @param .flag .flag
#'
#' @returns Conjunto de datos con las imputaciones aplicadas
#' @export
aplicar_imputaciones <- function(.datos, .imp, .vble, .flag) {
  .imp <- .imp[.imp[[.flag]] == -1, c("PB010", "PB020", "PB030", .vble)]

  .datos <- dplyr::left_join(
    x = .datos,
    y = .imp,
    by = dplyr::join_by(PB010, PB020, PB030),
    suffix = c("", "_imp")
  )

  reemplazar <- .datos[[.flag]] == -1
  .datos[reemplazar, .vble] <- .datos[reemplazar, paste0(.vble, "_imp")]

  return(.datos)
}

# ============================================================================
chequear_faltantes <- function(.flag) {
  imputables <- length(.flag[.flag == -1])
  referencia <- length(.flag[.flag == 1])

  cli::cli_bullets(c(
    "i" = "Casos a imputar: {imputables}",
    "i" = "Casos de referencia: {referencia}"
  ))

  if (imputables == 0) {
    cli::cli_alert_success("Nada que imputar!")

    return("completo")
  } else if (imputables / referencia > 0.5) {
    cli::cli_bullets(c(
      "x" = "La razon entre casos a imputar y casos de referencia es mayor a 0.5",
      " " = "No se imputa la variable"
    ))

    return("no imputar")
  } else {
    return("imputar")
  }
}

# ============================================================================
#' Title
#'
#' @param .datos .datos
#'
#' @returns datos imputados
#' @export
imputar_meses <- function(.datos) {
  cli::cli_h2("Meses trabajados en el IRP (asalariados)")

  # Meses asalariados ------------------------
  imputar_maa <- chequear_faltantes(.datos$.f_maa)

  if (imputar_maa == "imputar") {
    imp_maa <- armar_imputables(
      .datos,
      .imputadas   = "maa",
      .predictoras = c("PY010N", "PY050N", "PB140", "PB150", "PE041"),
      .flag        = ".f_maa"
    )

    imp_maa <- missRanger::missRanger(
      data = imp_maa,
      formula = maa + PE041 ~ PY010N + PY050N + PB140 + PB150 + PE041,
      num.trees = 100,
      pmm.k = 10
    )

    .datos <- aplicar_imputaciones(.datos, imp_maa, "maa", ".f_maa")
  }


  # Meses no asalariados ---------------------
  imputar_maa <- chequear_faltantes(.datos$.f_man)

  if (imputar_maa == "completo" | imputar_maa == "no imputar") {
    return(.datos)
  } else {
    imp_man <- armar_imputables(
      .datos,
      .imputadas   = "man",
      .predictoras = c("PY010N", "PY050N", "PB140", "PB150", "PE041"),
      .flag        = ".f_man"
    )

    imp_man <- missRanger::missRanger(
      data = imp_man,
      formula = man + PE041 ~ PY010N + PY050N + PB140 + PB150 + PE041,
      num.trees = 100,
      pmm.k = 10
    )

    .datos <- aplicar_imputaciones(.datos, imp_man, "man", ".f_man")
  }

  return(.datos)
}

# ============================================================================
#' Title
#'
#' @param .datos .datos
#'
#' @returns datos imputados
#' @export
imputar_horas <- function(.datos) {
  cli::cli_h2("Horas semanales trabajadas habitualmente")

  imputar <- chequear_faltantes(.datos$.f_PL060)

  if (imputar == "imputar") {
    imp <- armar_imputables(
      .datos,
      .imputadas   = "PL060",
      .predictoras = c("PY010N", "PY050N", "PB140", "PB150", "PL073", "PL074", "PL075", "PL076"),
      .flag        = ".f_PL060"
    )

    imp <- missRanger::missRanger(
      data = imp,
      formula = PL060 ~ PY010N + PY050N + PB140 + PB150 + PL073 + PL074 + PL075 + PL076,
      num.trees = 100,
      pmm.k = 10
    )

    .datos <- aplicar_imputaciones(.datos, imp, "PL060", ".f_PL060")
  }

  return(.datos)
}

# ============================================================================
#' Title
#'
#' @param .datos .datos
#'
#' @returns datos imputados
#' @export
imputar_laboral_a <- function(.datos) {
  cli::cli_h2("Categoria ocupacional, ocupacion y rama de actividad (A)")
  cli::cli_alert_info(
    "Estas variables tienen la misma condicion de imputacion. Se imputan simultaneamente."
  )

  cli::cli_h3("Categoria ocupacional")
  imputar_PL040A <- chequear_faltantes(.datos$.f_PL040A)

  cli::cli_h3("Ocupacion")
  imputar_PL051A <- chequear_faltantes(.datos$.f_PL051A)

  cli::cli_h3("Rama de actividad")
  imputar_PL111A <- chequear_faltantes(.datos$.f_PL111A)

  imputar <- c(PL040A = imputar_PL040A, PL051A = imputar_PL051A, PL111A = imputar_PL111A)

  if (any(imputar == "imputar")) {
    imputadas <- names(imputar[imputar == "imputar"])
    completas <- names(imputar[imputar == "completo"])

    imp <- armar_imputables(
      .datos,
      .imputadas   = c("PL040A", "PL051A", "PL111A"),
      .predictoras = c("PY010N", "PY050N", "PB140", "PB150", "PE041"),
      .flags       = c(".f_PL040A", ".f_PL051A", ".f_PL111A"),
      .factores    = c("PL040A", "PL051A", "PE041")
    )

    formula <- paste0(paste(c(imputadas, "PE041"), collapse = " + "), " ~ ",
                      paste(c(imputadas, completas, "PY010N", "PY050N", "PB140", "PB150", "PE041"), collapse = " + "))
    imp <- missRanger::missRanger(
      data = imp,
      formula = formula(formula),
      num.trees = 100,
      pmm.k = 10
    )

    if ("PL040A" %in% imputadas) {
      imp$PL040A <- as.numeric(as.character(imp$PL040A))
    }
    if ("PL051A" %in% imputadas) {
      imp$PL051A <- as.numeric(as.character(imp$PL051A))
    }

    for (.vble in imputadas) {
      .datos <- aplicar_imputaciones(.datos, imp, .vble, paste0(".f_", .vble))
    }
  }

  return(.datos)
}

# ============================================================================
#' Title
#'
#' @param .datos .datos
#' @param .anio .anio
#'
#' @returns datos imputados
#' @export
imputar_laboral_b <- function(.datos, .anio) {
  cli::cli_h2("Categoria ocupacional, ocupacion y rama de actividad (B)")
  cli::cli_alert_info(
    "Estas variables tienen la misma condicion de imputacion. Se imputan simultaneamente."
  )

  cli::cli_h3("Categoria ocupacional")
  imputar_PL040B <- chequear_faltantes(.datos$.f_PL040B)

  cli::cli_h3("Ocupacion")
  imputar_PL051B <- chequear_faltantes(.datos$.f_PL051B)

  if (.anio >= 2021) {
    cli::cli_h3("Rama de actividad")
    imputar_PL111B <- chequear_faltantes(.datos$.f_PL111B)
  } else {
    cli::cli_h3("Rama de actividad")
    cli::cli_alert_warning("La variable no esta disponible antes de 2021")
    imputar_PL111B <- NULL
  }

  imputar <- c(PL040B = imputar_PL040B, PL051B = imputar_PL051B, PL111B = imputar_PL111B)

  if (any(imputar == "imputar")) {
    imputadas <- names(imputar[imputar == "imputar"])
    completas <- names(imputar[imputar == "completo"])

    imp <- armar_imputables(
      .datos,
      .imputadas   = imputadas,
      .predictoras = c("PY010N", "PY050N", "PB140", "PB150", "PE041"),
      .flags       = paste0(".f_", imputadas),
      .factores    = c(imputadas[imputadas != "PL111B"], "PE041")
    )

    formula <- paste0(paste(c(imputadas, "PE041"), collapse = " + "), " ~ ",
                      paste(c(imputadas, completas, "PY010N", "PY050N", "PB140", "PB150", "PE041"), collapse = " + "))
    imp <- missRanger::missRanger(
      data = imp,
      formula = formula(formula),
      num.trees = 100,
      pmm.k = 10
    )

    if ("PL040B" %in% imputadas) {
      imp$PL040B <- as.numeric(as.character(imp$PL040B))
    }
    if ("PL051B" %in% imputadas) {
      imp$PL051B <- as.numeric(as.character(imp$PL051B))
    }

    for (.vble in imputadas) {
      .datos <- aplicar_imputaciones(.datos, imp, .vble, paste0(".f_", .vble))
    }
  }

  return(.datos)
}

# ============================================================================
#' Title
#'
#' @param .datos .datos
#'
#' @returns datos imputados
#' @export
imputar_tamanio <- function(.datos) {
  cli::cli_h2("Tamanio del establecimiento")
  cli::cli_alert_info("Se imputa por partes segun el caso este perdido o truncado (codigos 14 y 15)")

  .datos <- dplyr::mutate(
    .datos,
    PL130_ = dplyr::if_else(PL130 %in% 14:15, NA_integer_, PL130)
  )

  # No sabe, menos de 10 ---------------------
  cli::cli_h3("No sabe, pero menos de 10 personas")
  imputar_PL130a <- chequear_faltantes(.datos$.fa_PL130)

  if (imputar_PL130a == "imputar") {
    imp_PL130a <- armar_imputables(
      .datos,
      .imputadas   = c("PL130_", "PE041", "PL111A"),
      .predictoras = c("PY010N", "PY050N", "PB140", "PB150", "PE041", "PL111A"),
      .flags       = c(".fa_PL130", ".f_PL130"),
      .factores    = "PE041"
    )

    imp_PL130a <- missRanger::missRanger(
      data = imp_PL130a,
      formula = PL130_ + PE041 + PL111A ~ PY010N + PY050N + PB140 + PB150 + PE041 + PL111A,
      num.trees = 100,
      pmm.k = 10
    )

    imp_PL130a <- dplyr::rename(imp_PL130a, PL130 = PL130_)
  } else {
    imp_PL130a <- NULL
  }

  # No sabe, mas de 10 -----------------------
  cli::cli_h3("No sabe, pero mas de 10 personas")
  imputar_PL130b <- chequear_faltantes(.datos$.fb_PL130)

  if (imputar_PL130b == "imputar") {
    imp_PL130b <- armar_imputables(
      .datos,
      .imputadas   = c("PL130_", "PE041", "PL111A"),
      .predictoras = c("PY010N", "PY050N", "PB140", "PB150", "PE041", "PL111A"),
      .flags       = c(".fb_PL130", ".f_PL130"),
      .factores    = c("PL130_", "PE041")
    )

    imp_PL130b <- missRanger::missRanger(
      data = imp_PL130b,
      formula = PL130_ + PE041 + PL111A ~ PY010N + PY050N + PB140 + PB150 + PE041 + PL111A,
      num.trees = 100,
      pmm.k = 10
    )

    imp_PL130b <- dplyr::mutate(imp_PL130b, PL130 = as.numeric(as.character(PL130_)))

    .datos <- aplicar_imputaciones(.datos, imp_PL130b, "PL130", ".fb_PL130")
  } else {
    imp_PL130b <- NULL
  }

  # Perdidos ---------------------------------
  cli::cli_h3("Perdido")
  imputar_PL130c <- chequear_faltantes(.datos$.fc_PL130)

  if (imputar_PL130c == "imputar") {
    imp_PL130c <- armar_imputables(
      .datos,
      .imputadas   = c("PL130", "PE041", "PL111A"),
      .predictoras = c("PY010N", "PY050N", "PB140", "PB150", "PE041", "PL111A"),
      .flags       = c(".fc_PL130", ".f_PL130"),
      .factores    = c("PL130", "PE041")
    )

    imp_PL130c <- missRanger::missRanger(
      data = imp_PL130c,
      formula = PL130 + PE041 + PL111A ~ PY010N + PY050N + PB140 + PB150 + PE041 + PL111A,
      num.trees = 100,
      pmm.k = 10
    )

    imp_PL130c <- dplyr::mutate(imp_PL130c, PL130 = as.numeric(as.character(PL130)))

    .datos <- aplicar_imputaciones(.datos, imp_PL130c, "PL130", ".fc_PL130")
  } else {
    imp_PL130c <- NULL
  }

  if (!any(c(imputar_PL130a, imputar_PL130b, imputar_PL130c) == "imputar")) {
    return(.datos)
  }

  imp <- dplyr::bind_rows(imp_PL130a, imp_PL130b, imp_PL130c)

  .datos <- aplicar_imputaciones(.datos, imp, "PL130", ".f_PL130")

  return(.datos)
}

# ============================================================================
#' Title
#'
#' @param .datos .datos
#'
#' @returns datos imputados
#' @export
imputar_sectorpp <- function(.datos) {
  cli::cli_h2("Sector publico o privado")

  imputar <- chequear_faltantes(.datos$.f_PL230)

  if (imputar == "imputar") {
    imp <- armar_imputables(
      .datos,
      .imputadas   = c("PL230", "PE041"),
      .predictoras = c("PY010N", "PY050N", "PB140", "PB150", "PE041"),
      .flag        = ".f_PL230",
      .factores    = c("PL230", "PE041")
    )

    imp <- missRanger::missRanger(
      data = imp,
      formula = PL230 + PE041 ~ PY010N + PY050N + PB140 + PB150 + PE041,
      num.trees = 100,
      pmm.k = 10
    )

    imp <- dplyr::mutate(imp, PL230 = as.numeric(as.character(PL230)))

    .datos <- aplicar_imputaciones(.datos, imp, "PL230", ".f_PL230")
  }

  return(.datos)
}
