test_that("P es obligatorio y debe ser un data frame", {
  expect_error(calcular_personas(), "\\.P", class = "no_data_frame")
  for (valor in list(NULL, "", 1, list(), matrix(1))) {
    expect_error(calcular_personas(valor), "\\.P", class = "no_data_frame")
  }
})

test_that("P debe estar estandarizada", {
  P <- structure(tibble::tibble(PB010 = 2023, PB020 = "DE"), base = "P")
  
  for (valor in list(NULL, FALSE, NA, 1, "TRUE", logical(), c(TRUE, FALSE))) {
    attr(P, "estandar") <- valor
    expect_error(calcular_personas(P), class = "no_estandar")
  }
  
  attr(P, "estandar") <- NULL
  attr(P, "estandar_extra") <- TRUE
  expect_error(calcular_personas(P), class = "no_estandar")
})

test_that("P debe identificar exactamente la base de personas", {
  P <- structure(
    tibble::tibble(PB010 = 2023, PB020 = "DE"),
    estandar = TRUE
  )
  
  for (valor in list(NULL, "H", NA, character(), c("P", "H"))) {
    attr(P, "base") <- valor
    expect_error(calcular_personas(P), class = "no_p")
  }
  
  attr(P, "base") <- NULL
  attr(P, "base_extra") <- "P"
  expect_error(calcular_personas(P), class = "no_p")
})

test_that("se requieren las columnas de anio y pais", {
  P <- structure(
    tibble::tibble(PB010 = 2023, PB020 = "DE"),
    estandar = TRUE,
    base = "P"
  )
  columnas <- names(P)
  
  for (faltantes in list(columnas[1], columnas[2], columnas)) {
    datos <- P[setdiff(columnas, faltantes)]
    
    error <- expect_error(
      calcular_personas(datos),
      class = "columnas_faltantes"
    )
    expect_match(conditionMessage(error), ".P", fixed = TRUE)
    
    for (columna in faltantes) {
      expect_match(conditionMessage(error), columna, fixed = TRUE)
    }
  }
})

test_that("expandir requiere un unico TRUE o FALSE", {
  P <- structure(
    tibble::tibble(PB010 = 2023, PB020 = "DE"),
    estandar = TRUE,
    base = "P"
  )
  
  for (valor in list("", 0, 1, NA, NULL, logical(), c(TRUE, FALSE))) {
    expect_error(
      calcular_personas(P, .expandir = valor),
      "\\.expandir",
      class = "no_logical"
    )
  }
})

test_that("las entradas validas llegan al calculo con ambas opciones de expandir", {
  simular_calculo <- function(.P) {
    dplyr::mutate(.P, pi01 = PB010, pi02 = PB020)
  }
  omitir_informe <- function(.datos, .base) {
    invisible(NULL)
  }
  
  local_mocked_bindings(
    calcular_personas_ = simular_calculo,
    chequear_perdidas = omitir_informe
  )
  
  P <- data.frame(PB010 = 2023, PB020 = "DE")
  
  for (datos in list(P, tibble::as_tibble(P))) {
    attr(datos, "estandar") <- TRUE
    attr(datos, "base") <- "P"
    
    for (expandir in c(TRUE, FALSE)) {
      resultado <- calcular_personas(datos, .expandir = expandir)
      
      expect_identical(attr(resultado, "expandida"), expandir)
      expect_identical(resultado$pi01, datos$PB010)
      expect_identical(resultado$pi02, datos$PB020)
      expect_identical("PB010" %in% names(resultado), expandir)
    }
  }
})
