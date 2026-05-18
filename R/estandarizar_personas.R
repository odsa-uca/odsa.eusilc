#' Title
#'
#' @param .datos .datos
#' @param .anio .anio
#' @param .D .D
#' @param .R .R
#' @param .lmh lmh
#'
#' @returns conjunto de datos estandarizado para [calcular_personas()].
estandarizar_personas <- function(.datos, .anio, .D, .R, .lmh) {
  if (.anio <= 2021) {
    .datos <- dplyr::mutate(
      .datos,
      RB080 = PB140,
      RB081 = PB010 - PB140 - 1,
      RB082 = PB110 - PB140 - (PB130 > PB100),
      RB280 = PB210,
      RB290 = PB220A,
      PE041 = PE040,
      PL032 = dplyr::case_when(
        PL031 %in% 1:4 ~ 1,
        PL031 %in% 5 ~ 2,
        PL031 %in% 6:11 ~ 3,
        .default = NA_integer_
      ),
      PL040A = dplyr::if_else(PL032 == 1, PL040, NA),
      PL051A = dplyr::if_else(PL032 == 1, PL051, NA),
      PL111A = dplyr::if_else(PL032 == 1, PL111, NA),
      PL040B = dplyr::if_else(PL032 != 1 | is.na(PL032), PL040, NA),
      PL051B = dplyr::if_else(PL032 != 1 | is.na(PL032), PL051, NA),
      PL111B = NA_character_,
      # Los flags hacen falta si después se imputa
      PL040A_F = dplyr::if_else(PL032 == 1, PL040_F, -2),
      PL051A_F = dplyr::if_else(PL032 == 1, PL051_F, -2),
      PL111A_F = dplyr::if_else(PL032 == 1, PL111_F, -2),
      PL040B_F = dplyr::if_else(PL032 != 1 | is.na(PL032), PL040_F, -2),
      PL051B_F = dplyr::if_else(PL032 != 1 | is.na(PL032), PL051_F, -2),
      PL111B_F = -2,
    )

    cli::cli_bullets(c(
      "!" = "La base corresponde al {(.anio)}, anterior a 2021",
      "i" = "Se pierde PL111B"
    ))
  } else if (is.null(.R)) {
    .datos <- dplyr::mutate(
      .datos,
      RB080 = PB010 - PX020 - 1,
      RB081 = PX020,
      RB082 = NA_integer_,
      RB280 = NA_integer_,
      RB290 = NA_integer_
    )

    cli::cli_bullets(c(
      "!" = "No se proporciono el conjunto R",
      "i" = "Se pierden: `pd01a`, `pd04`, `pd05`"
    ))
  } else {
    .datos <- dplyr::left_join(
      x  = .datos,
      y  = dplyr::select(.R, RB010, RB020, RB030, RB080, RB081, RB082, RB280, RB290),
      by = dplyr::join_by(PB010 == RB010, PB020 == RB020, PB030 == RB030)
    )
  }

  if (is.null(.D)) {
    .datos <- dplyr::mutate(.datos, DB040 = NA_character_)

    cli::cli_bullets(c(
      "!" = "No se proporciono el conjunto D",
      "i" = "Se pierden: `pi03`"
    ))
  } else {
    .datos <- dplyr::left_join(
      x  = .datos,
      y  = dplyr::select(.D, DB010, DB020, DB030, DB040),
      by = dplyr::join_by(PB010 == DB010, PB020 == DB020, PX030 == DB030)
    )
  }

  if (.anio < 2021 & !.lmh) {
    .datos <- dplyr::mutate(.datos, PL230 = NA_integer_)

    cli::cli_bullets(c(
      "!" = "No se encontro `PL230`",
      "i" = "Se pierden: `pl07`, `pl09a`, `pl09b`, `py13`, `py14`, `py15`."
    ))
  } else if (!.lmh) {
    .datos <- dplyr::mutate(.datos, PL130 = NA_integer_, PL230 = NA_integer_)

    cli::cli_bullets(c(
      "!" = "No se encontro `PL130` o `PL230`",
      "i" = "Se pierden: `pl06a`, `pl06b`, `pl07`, `pl09a`, `pl09b`, `py13`, `py14`, `py15`."
    ))
  }

  return(.datos)
}
