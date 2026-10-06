# odsa.eusilc 0.1.2

- Se incorpora la creación de la variable `pl41` empleo de calidad.
- Se modifican algunas etiquetas de las variables `pl20a/b/c`.
- Se cambia la recodificación `PE041 -> pd03`; el nivel 4 ISCED pasa a
"secundario completo" y los niveles 5 a 8 de ISCED a "terciario incompleto o más".
- Se incorporan `tabla_advertencias`, que indica advertencias por país, rango
  de años y variable, y `tabla_cobertura`, que especifica el estado de la
  revisión documental para cada país y rango de años.
- Se incorpora la función `ver_advertencias`, que permite revisar las
  advertencias correspondientes a un determinado conjunto de datos que se
  encontraron en `tabla_advertencias`.
- Se incorpora la creación de las variables `pi07` y `hi07` grado de
  urbanización, que distingue entre urbano y rural.
- Se extienden las tablas de conversión de ingresos a unidades de PPA para
  incluir a 39 países en los años de 2016 a 2025.

# odsa.eusilc 0.1.1

- Se corrige un error en la estandarización de las bases. Se separaba el
procedimiento según la base fuese posterior a 2021, cuando debía ser posterior a 2020.

# odsa.eusilc 0.1.0

¡Primera versión!
