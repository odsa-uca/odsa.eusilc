# No pasar data frames -------------------------------------------------------
test_that("Error, H no es data.frame", {
  expect_error(estandarizar_hogares("", ""), class = "no_data_frame")
})
test_that("Error, P no es data.frame", {
  H <- tibble::tibble(
    HB010 = 2023,
    HB020 = "DE"
  )
  expect_error(estandarizar_hogares(H, ""), class = "no_data_frame")
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
  expect_error(estandarizar_hogares(H, P, ""), class = "no_data_frame")
})

