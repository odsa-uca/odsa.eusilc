# Contenidos del repositorio

Este repositorio contiene el código de un paquete de R cuyo propósito es
transformar los conjuntos de datos de la _European Union Statistics on Income
and Living Conditions_. Los conjuntos de datos NO se incluyen ni se deben
incluir en el paquete; se espera que los usuarios accedan a los datos a través
de los medios apropiados.

El repositorio tiene los siguientes subdirectorios:

- `.agents/` contiene archivos de uso de Codex, junto con registros de planes y sugerencias.
- `.misc/` contiene archivos misceláneos que no son trackeados por git, como
  documentos, clasificadores, etc.
- `data/` contiene datos públicos para los usuarios del paquete.
- `data-raw/` contiene scripts de generación de datos públicos e internos del
  paquete, junto con los datos crudos que hacen de insumo
- `man` contiene la documentación de las funciones y los objetos del paquete.
- `R` contiene archivos de R con las definiciones de las funciones y datos internos del paquete.
- `tests` contiene tests para las funciones del paquete.

# Comandos

```
# Para correr código
Rscript -e "devtools::load_all(); codigo"

# Para correr todos los tests
Rscript -e "devtools::test()"

# Para documentar el paquete
Rscript -e "devtools::document()"

# Para chequear el paquete
Rscript -e "devtools::check()"

# Para crear nuevos archivos .r en \R 
Rscript -e "usethis::use_r('{nombre_del_archivo}')"

# Para crear tests asociados a un archivo
Rscript -e "usethis::use_test('{nombre_del_archivo}')"

# Para incluir datos en el paquete
Rscript -e "usethis::use_data({datos})"

# Si los datos son internos únicamente
Rscript -e "usethis::use_data({datos}, internal = TRUE)"

# Para incluir un paquete en las dependencias
Rscript -e "usethis::use_package('{paquete}')"
```

# Código

## Generación de código

- Prioriza la legibilidad por sobre la completitud; evita abstracciones y
  validaciones innecesarias.
- Prioriza las funciones del `tidyverse` que figuran como dependencias.
- Antes de agregar dependencias, pregúntamelo y explícame el motivo por el
  cual consideras que son necesarias.
- Si generas output a la línea de comandos, prioriza las funciones del paquete `cli`.

## Validación de argumentos

- Agrega validaciones de argumentos sólo cuando te lo pida.
- Prioriza las funciones del paquete `rlang`.
- Ubica las validaciones en los puntos de entrada de los objetos, de forma tal
  que las funciones internas puedan dar por supuesto que son adecuados.
- Ubica las validaciones al inicio de las funciones, dentro de lo posible.

## Estilo

- Usa predominantemente el español en el código, la documentación y los nombres
  de archivos nuevos. Conserva los nombres originales de variables, API y
  elementos externos cuando traducirlos reduzca la claridad o la compatibilidad.
- Usa el español para nombrar los objetos.
- Usa snake_case para nombrar los objetos.
- Usa verbos en infinitivo al inicio de los nombres de funciones (por ejemplo,
  `calcular_indicador()`).
- Siempre que definas una función, agrega un separador con guiones `-` hasta la
  columna 78 antes de la función y su documentación. En lo posible, evita
  comentar al interior de las funciones.
- Usa el pipe de R base `|>`, no el del paquete `magrittr`, `%>%`.
- Usa el operador de asignación `<-`, reserva `=` para argumentos de funciones.
- Usa un punto `.` antes de los nombres de los argumentos de las funciones para
  facilitar su identificación al interior de la misma.
- Formatea el código generado con `air`.

## Testeo

- Genera los tests necesarios cuando te lo pida.
- Recuerda que NO disponemos de los datos de la encuesta en este paquete, por
  lo cual no podemos crear tests que dependan de esos datos.
- Puedes utilizar datos sintéticos para validar el código.

# Documentación

- Documenta los objetos en formato `roxygen` en los scripts de la carpeta `R/`.
  Luego regenera la documentación de `man/` y el `NAMESPACE` mediante `devtools::document()`

# Control de cambios

- No crees nuevas branches ni cambies de branch sin consultarme.
- No hagas commits.
- No modifiques el archivo `.gitignore`.
- Conserva los cambios preexistentes.

# Planificación

En cuanto a la planificación, me gustaría que:

- Me pidas todas las aclaraciones que consideres necesarias.
- Me hagas notar posibles problemas o inconsistencias en lo que te pido.
- Sólo para planes extensos que desarrollemos en Plan Mode, guardes el prompt
  que te di y el plan final ejecutado en un archivo markdown en la carpeta
  `.agents/planes/`. El nombre del archivo debe tener un timestamp con formato
  "YYYY-MM-DD_hh-mm-ss" seguido de una breve descripción.
- Si te pido que guardes una sugerencia, hazlo en `.agents/sugerencias/` con el
  nombre `YYYY-MM-DD_hh-mm-ss_descripcion_breve.md`.
