test_that("P no es data frame", {
  expect_error(imputar_personas(""), class = "no_data_frame")
})
test_that("P no es estandar", {
  P <- tibble::tibble()
  expect_error(imputar_personas(P), class = "no_estandar")
})
test_that("P no es P", {
  P <- structure(
    tibble::tibble(),
    estandar = TRUE,
    base = "H"
  )
  expect_error(imputar_personas(P), class = "no_p")
})
test_that("P imputada", {
  P <- structure(
    tibble::tibble(),
    estandar = TRUE,
    base = "P",
    imputada = TRUE
  )
  expect_equal(imputar_personas(P), P)
})
