test_that("datos es obligatorio y debe ser un data frame", {
  expect_error(ver_advertencias(), "\\.datos", class = "no_data_frame")
  
  for (valor in list(NULL, "", 1, TRUE, list(), matrix(1))) {
    expect_error(ver_advertencias(valor), "\\.datos", class = "no_data_frame")
  }
  
  objeto <- structure(list(), advertencias = tibble::tibble())
  expect_error(ver_advertencias(objeto), "\\.datos", class = "no_data_frame")
})

test_that("se conserva el error cuando no hay un atributo exacto advertencias", {
  for (datos in list(data.frame(), tibble::tibble())) {
    expect_error(ver_advertencias(datos), class = "advertencias_no_disponibles")
    attr(datos, "advertencias_extra") <- tibble::tibble()
    expect_error(ver_advertencias(datos), class = "advertencias_no_disponibles")
  }
})

test_that("el atributo advertencias debe ser un data frame", {
  datos <- tibble::tibble()
  
  for (valor in list("", 1, TRUE, NA, character(), list(), matrix(1))) {
    attr(datos, "advertencias") <- valor
    error <- expect_error(ver_advertencias(datos), class = "no_data_frame")
    expect_match(conditionMessage(error), ".datos", fixed = TRUE)
    expect_match(conditionMessage(error), "advertencias", fixed = TRUE)
  }
})

test_that("se devuelve el atributo intacto incluso cuando esta vacio", {
  tabla <- data.frame(
    variable = "ejemplo",
    advertencia = "Advertencia sintetica"
  )
  tablas <- list(
    tabla,
    tibble::as_tibble(tabla),
    tabla[0, ],
    tibble::as_tibble(tabla[0, ]),
    data.frame(),
    tibble::tibble()
  )
  
  for (datos in list(data.frame(), tibble::tibble())) {
    for (advertencias in tablas) {
      attr(advertencias, "origen") <- "sintetico"
      attr(datos, "advertencias") <- advertencias
      original <- datos
      
      expect_identical(ver_advertencias(datos), advertencias)
      expect_identical(datos, original)
    }
  }
})
