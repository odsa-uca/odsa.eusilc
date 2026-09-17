# Script para armar las tablas auxiliares públicas e internas del paquete

library(dplyr)
library(readxl)
library(tidyr)

devtools::load_all()

etiquetas <- read_xlsx("data-raw/xlsx/tabla_etiquetas.xlsx")
tabla_isco <- read_xlsx("data-raw/xlsx/tabla_isco.xlsx")
tabla_pi07 <- read_xlsx("data-raw/xlsx/tabla_pi07.xlsx")
tabla_pd03 <- read_xlsx("data-raw/xlsx/tabla_pd03.xlsx")
tabla_pl01 <- read_xlsx("data-raw/xlsx/tabla_pl01.xlsx")
tabla_pl20 <- read_xlsx("data-raw/xlsx/tabla_pl20.xlsx")
tabla_pl21 <- read_xlsx("data-raw/xlsx/tabla_pl21.xlsx")
tabla_ppa <- read_xlsx("data-raw/xlsx/tabla_ppa.xlsx")
tabla_advertencias <- read_xlsx("data-raw/xlsx/tabla_advertencias.xlsx")
tabla_cobertura <- read_xlsx("data-raw/xlsx/tabla_cobertura.xlsx")

# Funciones ----------------------------------------------------------------
transformar_tabla_ppa <- function(.tabla_ppa) {
  tabla_ppa <-
    .tabla_ppa |>
    pivot_longer(
      cols = -PB020,
      names_to = "PB010",
      values_to = "ppa_factor",
      names_transform = list(PB010 = as.integer)
    ) |>
    relocate(PB020)

  ppa_us <-
    tabla_ppa |>
    filter(PB020 == "US") |>
    select(-PB020)

  tabla_ppa_ <-
    tabla_ppa |>
    left_join(ppa_us, by = "PB010", suffix = c("", "_us"))

  list(
    publica = tabla_ppa,
    interna = tabla_ppa_
  )
}

transformar_advertencias <- function(.advertencias) {
  .advertencias |>
    mutate(
      variable = strsplit(variable, ";")
    ) |>
    unnest(variable)
}

# Transformar tablas -------------------------------------------------------

etiquetas_ <- armar_etiquetas(etiquetas)

tablas_ppa <- transformar_tabla_ppa(tabla_ppa)
tabla_ppa <- tablas_ppa$publica
tabla_ppa_ <- tablas_ppa$interna

tabla_advertencias <- transformar_advertencias(tabla_advertencias)

paises_probados <- c("ES", "IT", "DE", "PL", "PT")

# Guardar ------------------------------------------------------------------

usethis::use_data(etiquetas, overwrite = TRUE)
usethis::use_data(tabla_ppa, overwrite = TRUE)
usethis::use_data(tabla_advertencias, overwrite = TRUE)
usethis::use_data(tabla_cobertura, overwrite = TRUE)
usethis::use_data(
  etiquetas_,
  tabla_ppa_,
  tabla_isco,
  tabla_pi07,
  tabla_pd03,
  tabla_pl01,
  tabla_pl20,
  tabla_pl21,
  paises_probados,
  internal = TRUE,
  overwrite = TRUE
)
