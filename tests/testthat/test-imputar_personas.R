test_that("P es obligatorio y debe ser un data frame", {
  expect_error(imputar_personas(), "\\.P", class = "no_data_frame")

  for (valor in list(NULL, "", 1, list(), matrix(1))) {
    expect_error(imputar_personas(valor), "\\.P", class = "no_data_frame")
  }
})

test_that("P debe tener los atributos exactos de una base P estandarizada", {
  for (atributo in c("estandar", "base")) {
    datos <- structure(
      tibble::tibble(PB010 = 2023, PB020 = "DE"),
      estandar = TRUE,
      base = "P"
    )

    clase <- if (atributo == "estandar") "no_estandar" else "no_p"

    for (valor in list(NULL, FALSE, NA, 1, "H", logical(), c(TRUE, FALSE))) {
      attr(datos, atributo) <- valor
      expect_error(imputar_personas(datos), class = clase)
    }

    attr(datos, atributo) <- NULL
    attr(datos, paste0(atributo, "_extra")) <- if (atributo == "estandar") {
      TRUE
    } else {
      "P"
    }

    expect_error(imputar_personas(datos), class = clase)
  }
})

test_that("se requieren las columnas de anio y pais si P no fue imputada", {
  datos <- structure(
    tibble::tibble(PB010 = 2023, PB020 = "DE"),
    estandar = TRUE,
    base = "P",
    "flags imp." = TRUE
  )
  columnas <- names(datos)

  for (imputada in list(NULL, FALSE)) {
    attr(datos, "imputada") <- imputada

    for (faltantes in list(columnas[1], columnas[2], columnas)) {
      error <- expect_error(
        imputar_personas(datos[setdiff(columnas, faltantes)]),
        class = "columnas_faltantes"
      )
      expect_match(conditionMessage(error), ".P", fixed = TRUE)

      for (columna in faltantes) {
        expect_match(conditionMessage(error), columna, fixed = TRUE)
      }
    }
  }
})

test_that("P ya imputada se devuelve sin exigir columnas de anio y pais", {
  datos <- data.frame(PB010 = 2023, PB020 = "DE")
  columnas <- names(datos)

  for (entrada in list(datos, tibble::as_tibble(datos))) {
    for (faltantes in list(columnas[1], columnas[2], columnas)) {
      incompleta <- structure(
        entrada[setdiff(columnas, faltantes)],
        estandar = TRUE,
        base = "P",
        imputada = TRUE,
        "flags imp." = TRUE
      )
      expect_identical(imputar_personas(incompleta), incompleta)
    }
  }
})

test_that("los atributos de estado rechazan valores no booleanos", {
  for (atributo in c("imputada", "flags imp.")) {
    datos <- structure(
      tibble::tibble(PB010 = 2023, PB020 = "DE"),
      estandar = TRUE,
      base = "P",
      "flags imp." = TRUE
    )

    for (valor in list("", 0, 1, NA, logical(), c(TRUE, FALSE))) {
      attr(datos, atributo) <- valor

      error <- expect_error(imputar_personas(datos), class = "no_logical")
      expect_match(conditionMessage(error), atributo, fixed = TRUE)
    }
  }
})

test_that("el atributo flags imp. es obligatorio y su nombre debe ser exacto", {
  for (imputada in c(FALSE, TRUE)) {
    datos <- structure(
      tibble::tibble(PB010 = 2023, PB020 = "DE"),
      estandar = TRUE,
      base = "P",
      imputada = imputada
    )

    expect_error(imputar_personas(datos), "flags imp", class = "no_logical")
    attr(datos, "flags imp. extra") <- TRUE
    expect_error(imputar_personas(datos), "flags imp", class = "no_logical")
  }
})

test_that("solo imputada TRUE evita imputar y los flags controlan su calculo", {
  pasos <- character()
  simular_flags <- function(.datos, .anio, .pais) {
    expect_identical(.anio, 2023)
    expect_identical(.pais, "DE")
    pasos <<- c(pasos, "flags")
    .datos
  }
  simular_imputacion <- function(.datos) {
    pasos <<- c(pasos, "imputar")
    .datos
  }
  simular_laboral_b <- function(.datos, .anio) {
    expect_identical(.anio, 2023)
    simular_imputacion(.datos)
  }
  local_mocked_bindings(
    calc_flags_imputacion = simular_flags,
    imputar_meses = simular_imputacion,
    imputar_horas = simular_imputacion,
    imputar_laboral_a = simular_imputacion,
    imputar_laboral_b = simular_laboral_b,
    imputar_tamanio = simular_imputacion,
    imputar_sectorpp = simular_imputacion
  )

  datos <- data.frame(PB010 = 2023, PB020 = "DE", PL130 = 1, PL230 = 1)

  for (entrada in list(datos, tibble::as_tibble(datos))) {
    attr(entrada, "estandar") <- TRUE
    attr(entrada, "base") <- "P"
    attr(entrada, "imputada_extra") <- TRUE

    for (imputada in list(NULL, FALSE, TRUE)) {
      attr(entrada, "imputada") <- imputada

      for (flags in c(FALSE, TRUE)) {
        attr(entrada, "flags imp.") <- flags
        pasos <- character()
        resultado <- imputar_personas(entrada)

        if (identical(imputada, TRUE)) {
          expect_identical(resultado, entrada)
          expect_identical(pasos, character())
        } else {
          esperados <- c(if (!flags) "flags", rep("imputar", 6))

          expect_identical(pasos, esperados)
          attr(entrada, "imputada") <- TRUE
          expect_identical(resultado, entrada)
          attr(entrada, "imputada") <- imputada
        }
      }
    }
  }
})
