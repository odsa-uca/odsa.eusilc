test_that("cada base suministrada exige filas, anio y pais unicos sin NA", {
  casos <- list(
    list(
      funciones = list(estandarizar_personas, expandir_personas),
      argumentos = list(
        .P = data.frame(PB010 = 2023, PB020 = "DE"),
        .D = data.frame(DB010 = 2023, DB020 = "DE"),
        .R = data.frame(RB010 = 2023, RB020 = "DE")
      )
    ),
    list(
      funciones = list(estandarizar_hogares),
      argumentos = list(
        .H = data.frame(HB010 = 2023, HB020 = "DE"),
        .D = data.frame(DB010 = 2023, DB020 = "DE")
      )
    ),
    list(
      funciones = list(expandir_hogares, calcular_hogares),
      argumentos = list(
        .H = structure(
          data.frame(HB010 = 2023, HB020 = "DE"),
          estandar = TRUE,
          base = "H"
        ),
        .P = structure(
          data.frame(pi01 = 2023, pi02 = "DE"),
          base = "P",
          expandida = FALSE
        ),
        .D = data.frame(DB010 = 2023, DB020 = "DE")
      )
    )
  )
  
  for (caso in casos) {
    for (funcion in caso$funciones) {
      originales <- caso$argumentos
      
      if (identical(funcion, calcular_hogares)) {
        originales$.D <- NULL
      }
      
      for (argumento in names(originales)) {
        argumentos <- originales
        argumentos[[argumento]] <- originales[[argumento]][0, ]
        
        error <- expect_error(
          do.call(funcion, argumentos),
          class = "base_vacia"
        )
        expect_match(conditionMessage(error), argumento, fixed = TRUE)
        
        for (indice in 1:2) {
          columna <- names(originales[[argumento]])[[indice]]
          valor <- originales[[argumento]][[columna]]
          variantes <- list(
            NA,
            c(valor, NA),
            c(NA, NA),
            c(valor, if (indice == 1) 2022 else "ES")
          )
          
          for (i in seq_along(variantes)) {
            argumentos <- originales
            datos <- originales[[argumento]][rep(1, length(variantes[[i]])), ]
            datos[[columna]] <- variantes[[i]]
            argumentos[[argumento]] <- datos
            clase <- if (i < 4) {
              "valores_faltantes"
            } else {
              c("varios_anios", "varios_paises")[[indice]]
            }
            
            error <- expect_error(do.call(funcion, argumentos), class = clase)
            expect_match(conditionMessage(error), argumento, fixed = TRUE)
            expect_match(conditionMessage(error), columna, fixed = TRUE)
          }
        }
      }
    }
  }
})

test_that("coincidencias separadas de anio y pais no bastan", {
  P <- data.frame(PB010 = 2023, PB020 = "DE")
  H <- data.frame(HB010 = 2023, HB020 = "DE")
  R <- data.frame(DB010 = c(2023, 2022), DB020 = c("ES", "DE"))
  
  expect_error(
    chequear_bases_personas(P, R, NULL),
    class = "varios_anios"
  )
  expect_error(
    chequear_bases_hogares(H, NULL, R),
    class = "varios_anios"
  )
})

test_that("se conservan las clases de discrepancia entre bases validas", {
  P <- data.frame(PB010 = 2023, PB020 = "DE")
  H <- data.frame(HB010 = 2023, HB020 = "DE")
  
  for (indice in 1:2) {
    registro <- data.frame(DB010 = 2023, DB020 = "DE")
    registro[[indice]] <- if (indice == 1) 2022 else "ES"
    campo <- c("anio", "pais")[[indice]]
    
    expect_error(
      chequear_bases_personas(P, registro, NULL),
      class = paste0("d_dif_", campo)
    )
    expect_error(
      chequear_bases_hogares(H, NULL, registro),
      class = paste0("d_dif_", campo)
    )
    
    names(registro) <- c("RB010", "RB020")
    expect_error(
      chequear_bases_personas(P, NULL, registro),
      class = paste0("r_dif_", campo)
    )
    
    names(registro) <- c("pi01", "pi02")
    attr(registro, "base") <- "P"
    expect_error(
      chequear_bases_hogares(H, registro, NULL),
      class = paste0("p_dif_", campo)
    )
  }
})

test_that("se admiten repeticiones, tipos numericos equivalentes y auxiliares NULL", {
  for (usar_tibble in c(FALSE, TRUE)) {
    P <- data.frame(PB010 = c(2023, 2023), PB020 = "DE")
    H <- data.frame(HB010 = c(2023, 2023), HB020 = "DE")
    D <- data.frame(DB010 = 2023L, DB020 = "DE")
    R <- data.frame(RB010 = 2023L, RB020 = "DE")
    P_exp <- data.frame(pi01 = 2023L, pi02 = "DE")
    
    if (usar_tibble) {
      P <- tibble::as_tibble(P)
      H <- tibble::as_tibble(H)
      D <- tibble::as_tibble(D)
      R <- tibble::as_tibble(R)
      P_exp <- tibble::as_tibble(P_exp)
    }
    
    attr(P_exp, "base") <- "P"
    for (auxiliar_d in list(NULL, D)) {
      for (auxiliar_r in list(NULL, R)) {
        expect_no_error(chequear_bases_personas(
          P,
          auxiliar_d,
          auxiliar_r
        ))
      }
      for (auxiliar_p in list(NULL, P_exp)) {
        expect_no_error(chequear_bases_hogares(H, auxiliar_p, auxiliar_d))
      }
    }
  }
})

test_that("el aviso de pais no probado se emite solo tras validar la concordancia", {
  P <- data.frame(PB010 = 2023, PB020 = "ZZ")
  D <- data.frame(DB010 = 2022, DB020 = "ZZ")
  
  mensajes <- character()
  error <- tryCatch(
    withCallingHandlers(
      chequear_bases_personas(P, D, NULL),
      message = function(.mensaje) {
        mensajes <<- c(mensajes, conditionMessage(.mensaje))
        invokeRestart("muffleMessage")
      }
    ),
    error = identity
  )
  expect_s3_class(error, "d_dif_anio")
  expect_length(mensajes, 0)
  expect_message(chequear_bases_personas(P, NULL, NULL), "Ojo")
})