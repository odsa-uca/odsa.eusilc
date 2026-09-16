test_that("Error, P no es data.frame", {
  expect_error(estandarizar_personas(""), class = "no_data_frame")
})
test_that("Error, D no es data.frame", {
  P <- tibble::tibble(
    PB010 = 2023,
    PB020 = "DE"
  )
  expect_error(estandarizar_personas(P, ""), class = "no_data_frame")
})
test_that("Error, R no es data.frame", {
  P <- tibble::tibble(
    PB010 = 2023,
    PB020 = "DE"
  )
  expect_error(estandarizar_personas(P, NULL, ""), class = "no_data_frame")
})
test_that("Error, varios anios", {
  P <- tibble::tibble(
    PB010 = c(2023, 2022),
    PB020 = c("DE", "DE")
  )
  expect_error(estandarizar_personas(P), class = "varios_anios")
})
test_that("Error, varios paises", {
  P <- tibble::tibble(
    PB010 = c(2023, 2023),
    PB020 = c("DE", "ES")
  )
  expect_error(estandarizar_personas(P), class = "varios_paises")
})
test_that("Error, P y D distintos anios", {
  P <- tibble::tibble(
    PB010 = c(2023, 2023),
    PB020 = c("DE", "DE")
  )
  D <- tibble::tibble(
    DB010 = c(2022, 2022),
    DB020 = c("DE", "DE")
  )
  expect_error(estandarizar_personas(P, D), class = "d_dif_anio")
})
test_that("Error, P y D distintos paises", {
  P <- tibble::tibble(
    PB010 = c(2023, 2023),
    PB020 = c("DE", "DE")
  )
  D <- tibble::tibble(
    DB010 = c(2023, 2023),
    DB020 = c("ES", "ES")
  )
  expect_error(estandarizar_personas(P, D), class = "d_dif_pais")
})
test_that("Error, P y R distintos anios", {
  P <- tibble::tibble(
    PB010 = c(2023, 2023),
    PB020 = c("DE", "DE")
  )
  R <- tibble::tibble(
    RB010 = c(2022, 2022),
    RB020 = c("DE", "DE")
  )
  expect_error(estandarizar_personas(P, NULL, R), class = "r_dif_anio")
})
test_that("Error, P y R distintos paises", {
  P <- tibble::tibble(
    PB010 = c(2023, 2023),
    PB020 = c("DE", "DE")
  )
  R <- tibble::tibble(
    RB010 = c(2023, 2023),
    RB020 = c("ES", "ES")
  )
  expect_error(estandarizar_personas(P, NULL, R), class = "r_dif_pais")
})

test_that("flags requiere un unico TRUE o FALSE", {
  P <- tibble::tibble(PB010 = 2023, PB020 = "DE")
  
  for (valor in list("", 1, NA, NULL, logical(), c(TRUE, FALSE))) {
    expect_error(
      estandarizar_personas(P, .flags = valor),
      regexp = "\\.flags",
      class = "no_logical"
    )
  }
})

test_that("P es obligatorio y los tipos invalidos identifican el argumento", {
  P <- tibble::tibble(PB010 = 2023, PB020 = "DE")
  
  expect_error(estandarizar_personas(NULL), "\\.P", class = "no_data_frame")
  expect_error(estandarizar_personas(), "\\.P", class = "no_data_frame")
  expect_error(
    estandarizar_personas(P, .D = 1),
    "\\.D",
    class = "no_data_frame"
  )
  expect_error(
    estandarizar_personas(P, .R = list()),
    "\\.R",
    class = "no_data_frame"
  )
})

test_that("se requieren las columnas de identificacion de cada conjunto", {
  conjuntos <- list(
    .P = tibble::tibble(PB010 = 2023, PB020 = "DE"),
    .D = tibble::tibble(DB010 = 2023, DB020 = "DE"),
    .R = tibble::tibble(RB010 = 2023, RB020 = "DE")
  )
  
  for (argumento in names(conjuntos)) {
    columnas <- names(conjuntos[[argumento]])
    
    for (faltantes in list(columnas[1], columnas[2], columnas)) {
      argumentos <- conjuntos
      argumentos[[argumento]] <- conjuntos[[argumento]][setdiff(
        columnas,
        faltantes
      )]
      
      error <- expect_error(
        do.call(estandarizar_personas, argumentos),
        class = "columnas_faltantes"
      )
      expect_match(conditionMessage(error), argumento, fixed = TRUE)
      
      for (columna in faltantes) {
        expect_match(conditionMessage(error), columna, fixed = TRUE)
      }
    }
  }
})

test_that("el chequeo admite data frames, tibbles y auxiliares opcionales", {
  P <- data.frame(PB010 = 2023, PB020 = "DE")
  D <- data.frame(DB010 = 2023, DB020 = "DE")
  R <- data.frame(RB010 = 2023, RB020 = "DE")
  
  expect_no_error(chequear_bases_personas(P, NULL, NULL))
  expect_no_error(chequear_bases_personas(P, D, R))
  expect_no_error(chequear_bases_personas(
    tibble::as_tibble(P),
    tibble::as_tibble(D),
    tibble::as_tibble(R)
  ))
})
