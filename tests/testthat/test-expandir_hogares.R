# No pasar data frames -------------------------------------------------------
test_that("Error, H no es data.frame", {
  expect_error(expandir_hogares("", ""), class = "no_data_frame")
})
test_that("Error, P no es data.frame", {
  H <- tibble::tibble(
    HB010 = 2023,
    HB020 = "DE"
  )
  expect_error(expandir_hogares(H, ""), class = "no_data_frame")
})
test_that("Error, D no es data.frame", {
  H <- tibble::tibble(
    HB010 = 2023,
    HB020 = "DE"
  )
  P <- structure(
    tibble::tibble(
      pi01 = 2023,
      pi02 = "DE"
    ),
    base = "P"
  )
  expect_error(expandir_hogares(H, P, ""), class = "no_data_frame")
})

# No pasar logicals ----------------------------------------------------------
test_that("Error, expandir no es logical", {
  H <- tibble::tibble(
    HB010 = 2023,
    HB020 = "DE"
  )
  P <- structure(
    tibble::tibble(
      pi01 = 2023,
      pi02 = "DE"
    ),
    base = "P"
  )
  expect_error(expandir_hogares(H, P, NULL, ""), class = "no_logical")
})
test_that("Error, expandir no es logical", {
  H <- tibble::tibble(
    HB010 = 2023,
    HB020 = "DE"
  )
  P <- structure(
    tibble::tibble(
      pi01 = 2023,
      pi02 = "DE"
    ),
    base = "P"
  )
  expect_error(expandir_hogares(H, P, NULL, TRUE, ""), class = "no_logical")
})

# Pasar apiladas -------------------------------------------------------------
test_that("Error, varios anios", {
  H <- tibble::tibble(
    HB010 = c(2023, 2022),
    HB020 = "DE"
  )
  P <- structure(
    tibble::tibble(
      pi01 = 2023,
      pi02 = "DE"
    ),
    base = "P"
  )
  expect_error(expandir_hogares(H, P), class = "varios_anios")
})
test_that("Error, varios paises", {
  H <- tibble::tibble(
    HB010 = 2023,
    HB020 = c("DE", "ES")
  )
  P <- structure(
    tibble::tibble(
      pi01 = 2023,
      pi02 = "DE"
    ),
    base = "P"
  )
  expect_error(expandir_hogares(H, P), class = "varios_paises")
})

# Pasar distintos años y paises ----------------------------------------------
test_that("Error, H y P distintos anios", {
  H <- tibble::tibble(
    HB010 = 2023,
    HB020 = "DE"
  )
  P <- structure(
    tibble::tibble(
      pi01 = 2022,
      pi02 = "DE"
    ),
    base = "P"
  )
  expect_error(expandir_hogares(H, P), class = "p_dif_anio")
})
test_that("Error, H y P distintos paises", {
  H <- tibble::tibble(
    HB010 = 2023,
    HB020 = "DE"
  )
  P <- structure(
    tibble::tibble(
      pi01 = 2023,
      pi02 = "ES"
    ),
    base = "P"
  )
  expect_error(expandir_hogares(H, P), class = "p_dif_pais")
})
test_that("Error, H y D distintos anios", {
  H <- tibble::tibble(
    HB010 = 2023,
    HB020 = "DE"
  )
  P <- structure(
    tibble::tibble(
      pi01 = 2023,
      pi02 = "DE"
    ),
    base = "P"
  )
  D <- tibble::tibble(
    DB010 = 2022,
    DB020 = "DE"
  )
  expect_error(expandir_hogares(H, P, D), class = "d_dif_anio")
})
test_that("Error, H y D distintos paises", {
  H <- tibble::tibble(
    HB010 = 2023,
    HB020 = "DE"
  )
  P <- structure(
    tibble::tibble(
      pi01 = 2023,
      pi02 = "DE"
    ),
    base = "P"
  )
  D <- tibble::tibble(
    DB010 = 2023,
    DB020 = "ES"
  )
  expect_error(expandir_hogares(H, P, D), class = "d_dif_pais")
})

test_that("se validan las columnas de identificacion antes de expandir hogares", {
  conjuntos <- list(
    .H = tibble::tibble(HB010 = 2023, HB020 = "DE"),
    .D = tibble::tibble(DB010 = 2023, DB020 = "DE"),
    .P = structure(tibble::tibble(pi01 = 2023, pi02 = "DE"), base = "P")
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
        do.call(expandir_hogares, argumentos),
        class = "columnas_faltantes"
      )
      expect_match(conditionMessage(error), argumento, fixed = TRUE)
      
      for (columna in faltantes) {
        expect_match(conditionMessage(error), columna, fixed = TRUE)
      }
    }
  }
})
