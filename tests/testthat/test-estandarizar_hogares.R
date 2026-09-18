# No pasar data frames -------------------------------------------------------
test_that("Error, H no es data.frame", {
  expect_error(estandarizar_hogares("", ""), class = "no_data_frame")
})
test_that("Error, D no es data.frame", {
  H <- tibble::tibble(
    HB010 = 2023,
    HB020 = "DE"
  )
  expect_error(estandarizar_hogares(H, ""), class = "no_data_frame")
})

test_that("H es obligatorio y los errores de tipo identifican el argumento", {
  H <- tibble::tibble(HB010 = 2023, HB020 = "DE")
  
  expect_error(estandarizar_hogares(), "\\.H", class = "no_data_frame")
  expect_error(estandarizar_hogares(NULL), "\\.H", class = "no_data_frame")
  expect_error(
    estandarizar_hogares(H, .D = list()),
    "\\.D",
    class = "no_data_frame"
  )
})

test_that("se requieren las columnas de identificacion de H y D", {
  conjuntos <- list(
    .H = tibble::tibble(HB010 = 2023, HB020 = "DE"),
    .D = tibble::tibble(DB010 = 2023, DB020 = "DE")
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
        do.call(estandarizar_hogares, argumentos),
        class = "columnas_faltantes"
      )
      expect_match(conditionMessage(error), argumento, fixed = TRUE)
      
      for (columna in faltantes) {
        expect_match(conditionMessage(error), columna, fixed = TRUE)
      }
    }
  }
})

test_that("el chequeo de hogares admite data frames y auxiliares opcionales", {
  H <- data.frame(HB010 = 2023, HB020 = "DE")
  D <- data.frame(DB010 = 2023, DB020 = "DE")
  P <- structure(data.frame(pi01 = 2023, pi02 = "DE"), base = "P")
  
  expect_no_error(chequear_bases_hogares(H, NULL, NULL))
  expect_no_error(chequear_bases_hogares(H, NULL, D))
  expect_no_error(chequear_bases_hogares(H, P, D))
  expect_no_error(chequear_bases_hogares(
    tibble::as_tibble(H),
    NULL,
    tibble::as_tibble(D)
  ))
})
