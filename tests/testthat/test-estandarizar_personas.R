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
