# odsa.eusilc (development version)

- Se incorpora la creación de la variable `pl41` empleo de calidad.
- Se modifican algunas etiquetas de las variables `pl20a/b/c`.
- Se cambia la recodificación `PE041 -> pd03`; el nivel 4 ISCED pasa a
"secundario completo" y los niveles 5 a 8 de ISCED a "terciario incompleto o más".
- Se incorporan `tabla_advertencias` y `tabla_cobertura`.
- Se incorpora la función `ver_advertencias`, que permite revisar las advertencias
  correspondientes al país y año que se encontraron en `tabla_advertencias`.

# odsa.eusilc 0.1.1

- Se corrige un error en la estandarización de las bases. Se separaba el
procedimiento según la base fuese posterior a 2021, cuando debía ser posterior a 2020.

# odsa.eusilc 0.1.0

¡Primera versión!
