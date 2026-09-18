# No pasar data frames -------------------------------------------------------
test_that("Error, H no es data.frame", {
  expect_error(calcular_hogares("", ""), class = "no_data_frame")
})
test_that("Error, P no es data.frame", {
  H <- tibble::tibble(
    HB010 = 2023,
    HB020 = "DE"
  )
  expect_error(calcular_hogares(H, ""), class = "no_data_frame")
})

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
  expect_error(calcular_hogares(H, P), class = "varios_anios")
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
  expect_error(calcular_hogares(H, P), class = "varios_paises")
})

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
  expect_error(calcular_hogares(H, P), class = "p_dif_anio")
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
  expect_error(calcular_hogares(H, P), class = "p_dif_pais")
})

test_that("Error, H no es estandar", {
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
  expect_error(calcular_hogares(H, P), class = "no_estandar")
})
test_that("Error, H no es H", {
  H <- structure(
    tibble::tibble(
      HB010 = 2023,
      HB020 = "DE"
    ),
    estandar = TRUE,
    base = "P"
  )
  P <- structure(
    tibble::tibble(
      pi01 = 2023,
      pi02 = "DE"
    ),
    base = "P"
  )
  expect_error(calcular_hogares(H, P), class = "no_h")
})

test_that("se validan las columnas de identificacion antes de calcular hogares", {
  conjuntos <- list(
    .H = tibble::tibble(HB010 = 2023, HB020 = "DE"),
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
        do.call(calcular_hogares, argumentos),
        class = "columnas_faltantes"
      )
      expect_match(conditionMessage(error), argumento, fixed = TRUE)

      for (columna in faltantes) {
        expect_match(conditionMessage(error), columna, fixed = TRUE)
      }
    }
  }
})

test_that("H y P son obligatorios y deben ser data frames", {
  H <- tibble::tibble(HB010 = 2023, HB020 = "DE")
  P <- structure(tibble::tibble(pi01 = 2023, pi02 = "DE"), base = "P")
  
  expect_error(calcular_hogares(.P = P), class = "no_data_frame")
  expect_error(calcular_hogares(H), class = "no_data_frame")
  
  for (valor in list(NULL, "", 1, list(), matrix(1))) {
    expect_error(
      calcular_hogares(valor, P),
      "\\.H",
      class = "no_data_frame"
    )
    expect_error(
      calcular_hogares(H, valor),
      "\\.P",
      class = "no_data_frame"
    )
  }
})

test_that("H requiere atributos exactos de una base H estandarizada", {
  P <- structure(tibble::tibble(pi01 = 2023, pi02 = "DE"), base = "P")
  
  for (atributo in c("estandar", "base")) {
    H <- structure(
      tibble::tibble(HB010 = 2023, HB020 = "DE"),
      estandar = TRUE,
      base = "H"
    )
    clase <- if (atributo == "estandar") "no_estandar" else "no_h"
    
    for (valor in list(NULL, FALSE, NA, 1, "P", logical(), c(TRUE, FALSE))) {
      attr(H, atributo) <- valor
      expect_error(calcular_hogares(H, P), class = clase)
    }
    
    attr(H, atributo) <- NULL
    attr(H, paste0(atributo, "_extra")) <- if (atributo == "estandar") {
      TRUE
    } else {
      "H"
    }
    expect_error(calcular_hogares(H, P), class = clase)
  }
})

test_that("P requiere base exacta y un estado de expansion booleano", {
  H <- structure(
    tibble::tibble(HB010 = 2023, HB020 = "DE"),
    estandar = TRUE,
    base = "H"
  )
  P <- structure(tibble::tibble(pi01 = 2023, pi02 = "DE"), base = "P")
  
  for (valor in list(NULL, "", 0, 1, NA, logical(), c(TRUE, FALSE))) {
    attr(P, "expandida") <- valor
    expect_error(
      calcular_hogares(H, P),
      "expandida",
      class = "no_expandida"
    )
  }
  
  attr(P, "expandida") <- NULL
  attr(P, "expandida_extra") <- TRUE
  expect_error(calcular_hogares(H, P), class = "no_expandida")
  
  for (valor in list("H", NA, character(), c("P", "H"))) {
    attr(P, "base") <- valor
    expect_error(calcular_hogares(H, P), class = "no_p")
  }
  
  attr(P, "base") <- NULL
  expect_error(calcular_hogares(H, P), class = "no_expandida")
  attr(P, "base_extra") <- "P"
  expect_error(calcular_hogares(H, P), class = "no_expandida")
})

test_that("expandir requiere un unico TRUE o FALSE", {
  H <- structure(
    tibble::tibble(HB010 = 2023, HB020 = "DE"),
    estandar = TRUE,
    base = "H"
  )
  P <- structure(
    tibble::tibble(pi01 = 2023, pi02 = "DE"),
    base = "P",
    expandida = FALSE
  )
  
  for (valor in list(NULL, "", 0, 1, NA, logical(), c(TRUE, FALSE))) {
    expect_error(
      calcular_hogares(H, P, valor),
      "\\.expandir",
      class = "no_logical"
    )
  }
})

test_that("se requieren los identificadores para unir hogares y personas", {
  for (argumento in c(".H", ".P")) {
    argumentos <- list(
      .H = structure(
        tibble::tibble(HB010 = 2023, HB020 = "DE", HB030 = 1),
        estandar = TRUE,
        base = "H"
      ),
      .P = structure(
        tibble::tibble(pi01 = 2023, pi02 = "DE", pi04 = 1),
        base = "P",
        expandida = FALSE
      )
    )
    
    columna <- if (argumento == ".H") "HB030" else "pi04"
    argumentos[[argumento]][[columna]] <- NULL
    
    error <- expect_error(
      do.call(calcular_hogares, argumentos),
      class = "columnas_faltantes"
    )
    expect_match(conditionMessage(error), argumento, fixed = TRUE)
    expect_match(conditionMessage(error), columna, fixed = TRUE)
  }
})

test_that("entradas validas permiten ambos estados de expansion y conservan la union", {
  simular_agregacion <- function(.personas) {
    .personas
  }
  simular_calculo <- function(.H) {
    dplyr::mutate(.H, hi01 = HB010, hi02 = HB020, hi04 = HB030)
  }
  omitir_informe <- function(.datos, .base) {
    invisible(NULL)
  }
  local_mocked_bindings(
    agregar_personas = simular_agregacion,
    calcular_hogares_ = simular_calculo,
    chequear_perdidas = omitir_informe
  )
  
  H <- data.frame(HB010 = 2023, HB020 = "DE", HB030 = c(1, 2))
  P <- data.frame(
    pi01 = 2023,
    pi02 = "DE",
    pi04 = c(2, 1),
    py00 = c(20, 10)
  )
  
  for (datos_h in list(H, tibble::as_tibble(H))) {
    attr(datos_h, "estandar") <- TRUE
    attr(datos_h, "base") <- "H"
    
    for (datos_p in list(P, tibble::as_tibble(P))) {
      attr(datos_p, "base") <- "P"
      
      for (expandida in c(TRUE, FALSE)) {
        attr(datos_p, "expandida") <- expandida
        
        for (expandir in c(TRUE, FALSE)) {
          resultado <- calcular_hogares(datos_h, datos_p, .expandir = expandir)
          expect_identical(resultado$hi04, c(1, 2))
          expect_identical(resultado$py00, c(10, 20))
          expect_identical(attr(resultado, "expandida"), expandir)
          expect_identical("HB030" %in% names(resultado), expandir)
        }
      }
    }
  }
})
