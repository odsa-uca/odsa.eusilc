test_that("datos es obligatorio y debe ser un data frame", {
  expect_error(etiquetar_eusilc(), "\\.datos", class = "no_data_frame")
  
  for (valor in list(NULL, "", 1, list(), matrix(1))) {
    expect_error(etiquetar_eusilc(valor), "\\.datos", class = "no_data_frame")
  }
})

test_that("expandida debe ser un atributo exacto booleano", {
  datos <- structure(tibble::tibble(pi07 = 1), base = "P")
  
  for (valor in list(NULL, "", 0, 1, NA, logical(), c(TRUE, FALSE))) {
    attr(datos, "expandida") <- valor
    expect_error(etiquetar_eusilc(datos), "expandida", class = "no_expandida")
  }
  
  attr(datos, "expandida") <- NULL
  attr(datos, "expandida_extra") <- TRUE
  expect_error(etiquetar_eusilc(datos), class = "no_expandida")
})

test_that("base debe ser un atributo exacto igual a P o H", {
  datos <- structure(tibble::tibble(pi07 = 1), expandida = FALSE)
  
  for (valor in list(
    NULL,
    "",
    "R",
    "p",
    NA,
    1,
    TRUE,
    character(),
    c("P", "H")
  )) {
    attr(datos, "base") <- valor
    error <- expect_error(etiquetar_eusilc(datos), class = "no_base")
    expect_match(conditionMessage(error), ".datos", fixed = TRUE)
    expect_match(conditionMessage(error), "base", fixed = TRUE)
  }
  
  attr(datos, "base") <- NULL
  attr(datos, "base_extra") <- "P"
  expect_error(etiquetar_eusilc(datos), class = "no_base")
})

test_that("se etiquetan personas y hogares con ambos estados de expansion", {
  for (base in c("P", "H")) {
    variable <- if (base == "P") "pi07" else "hi07"
    datos <- data.frame(codigo = c(1, 2), original = c(10, 20))
    names(datos)[1] <- variable
    
    for (entrada in list(datos, tibble::as_tibble(datos))) {
      attr(entrada, "base") <- base
      
      for (expandida in c(TRUE, FALSE)) {
        attr(entrada, "expandida") <- expandida
        resultado <- etiquetar_eusilc(entrada)
        
        expect_identical(
          attr(resultado[[variable]], "label"),
          etiquetas_[[base]]$variables[[variable]]
        )
        expect_identical(
          attr(resultado[[variable]], "labels"),
          etiquetas_[[base]]$valores[[variable]]
        )
        expect_identical(as.numeric(resultado[[variable]]), c(1, 2))
        expect_identical(resultado$original, entrada$original)
        expect_identical(names(resultado), names(entrada))
        expect_identical(class(resultado), class(entrada))
        expect_identical(attr(resultado, "base", exact = TRUE), base)
        expect_identical(attr(resultado, "expandida", exact = TRUE), expandida)
      }
    }
  }
})

test_that("no se exigen columnas que el etiquetado no utiliza", {
  for (base in c("P", "H")) {
    for (entrada in list(
      data.frame(),
      tibble::tibble(),
      data.frame(original = 1)
    )) {
      attr(entrada, "base") <- base
      attr(entrada, "expandida") <- FALSE
      expect_identical(etiquetar_eusilc(entrada), entrada)
    }
  }
})
