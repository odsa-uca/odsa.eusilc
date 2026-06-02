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
