# Mejora gradual de validaciones

## Prompt original

Me gustaría que me ayudes a mejorar la validación de argumentos de las
funciones del paquete. Para empezar, hay muchas validaciones que están hechas
"a mano" que podrían hacerse más sintéticamente con funciones del paquete
`rlang` como `rlang:::check_data_frame`. Además, hay validaciones complejas,
que tienen que ver con la concordancia entre años y países de los conjuntos de
datos proporcionados, que me parece que no están implementadas de manera
adecuada. Por último, hay cuestiones importantes que directamente no se
validan, como la presencia de las variables de año y país de los conjuntos. En
paralelo, estos cambios implicarían modificar la suite de tests que existen
hasta ahora. Me gustaría que encaremos este problema por etapas: (1) primero,
me gustaría que revises las funciones del paquete y evalúes las validaciones
que se hacen en ellas; (2) luego, me gustaría que avances con el reemplazo de
las validaciones "a mano" por su respectivas versiones del paquete `rlang`; (3)
después, me gustaría seguir con las validaciones complejas que chequean la
concordancia entre conjuntos de datos. En paralelo, me gustaría que, si hay
algo que es necesario validar, se incorpore la validación correspondiente en el
momento. También me gustaría que vayas modificando los tests correspondientes a
medida que realizamos cambios. Preferiría que vayamos por funciones así puedo
ir siguiendo los cambios que haces.

## Plan acordado

Una función por turno, empezando por estandarizar_personas(). Usar
rlang::check_data_frame() y rlang::check_bool(), declarar rlang >= 1.2.0 y
conservar las clases de error actuales. Los mensajes de tipo serán los
estándares de rlang. Validar columnas obligatorias al abordar cada función y
actualizar sus tests sintéticos. Las funciones internas de transformación
podrán asumir entradas válidas.

Primera entrega: reemplazar los controles de tipo de P, D y R en
chequear_bases_personas(), permitir NULL solo en D y R, comprobar PB010/PB020,
DB010/DB020 y RB010/RB020 antes de acceder a ellas, y validar .flags como un
único TRUE o FALSE. Agregar clase columnas_faltantes con argumento y columnas
en el mensaje. Actualizar también los tests de expandir_personas() por
compartir el validador. Reservar la lógica de concordancia para la tercera
etapa.

Orden posterior: expandir_personas(), estandarizar_hogares(),
expandir_hogares(), calcular_personas(), imputar_personas(),
calcular_hogares(), etiquetar_eusilc() y ver_advertencias(). Incorporar
controles necesarios de argumentos, columnas y atributos, distinguiendo estados
booleanos de expandida = FALSE, que es válido.

Tercera etapa: concordancia de personas y luego hogares. Cada conjunto
suministrado debe contener exclusivamente un año y país, iguales entre
conjuntos, sin NA ni bases vacías. Conservar las clases existentes de
discrepancia. No agregar restricciones de cobertura de individuos u hogares ni
validar exhaustivamente todas las variables de encuesta.

Verificar con datos sintéticos: tipos inválidos, booleanos inválidos, columnas
y atributos ausentes o incorrectos; posteriormente coincidencias, diferencias,
NA, conjuntos vacíos y auxiliares con valores adicionales. Ejecutar la suite
por entrega, formatear con air y regenerar documentación cuando corresponda.

## Primera entrega ejecutada

- DESCRIPTION declara rlang >= 1.2.0.
- chequear_bases_personas() usa check_data_frame() y comprueba columnas con el
  nuevo auxiliar documentado chequear_columnas(). Los controles estructurales
  se realizan antes de la concordancia.
- estandarizar_personas() usa check_bool() para .flags.
- Se conservan no_data_frame y no_logical; se agrega columnas_faltantes.
- Tests de ambas entradas cubren los nuevos controles; se comprueba aceptación
  de data frames, tibbles y auxiliares NULL en el validador.
- Código formateado con air y documentación regenerada con devtools::document().
- devtools::test(): 93 expectativas aprobadas, sin fallas, advertencias ni
  omisiones. R emite advertencias de configuración regional al iniciar, fuera
  de los tests.
- git diff --check sin errores de espacios.
- Pendiente: las entregas posteriores y la concordancia estricta. No se
  cambiaron ramas ni se hicieron commits.

## Segunda entrega ejecutada: expandir_personas()

- Se reemplazaron los controles manuales de .imputar, .expandir y .etiquetar
  por rlang::check_bool(), conservando la clase no_logical.
- Los tres argumentos exigen un único TRUE o FALSE. El mensaje de .expandir
  identifica ahora correctamente ese argumento.
- Los tests cubren texto, 0, 1, NA, NULL, logical() y vectores lógicos de
  longitud mayor que uno para cada argumento; verifican la clase y el nombre
  del argumento en el mensaje.
- Código formateado con air. No cambiaron las firmas ni el contenido de la
  documentación.
- devtools::test(): 132 expectativas aprobadas, sin fallas, advertencias ni
  omisiones. Persisten las advertencias de configuración regional al iniciar R,
  fuera de los tests.
- git diff --check sin errores.
- Próxima entrega: estandarizar_hogares(). La concordancia estricta continúa
  reservada para la tercera etapa.

## Tercera entrega ejecutada: estandarizar_hogares()

- Se reemplazaron los controles manuales de tipo en chequear_bases_hogares()
  por rlang::check_data_frame(), conservando no_data_frame. H es obligatorio;
  el auxiliar permite P y D nulos porque estandarizar_hogares() no necesita P y
  D es opcional.
- Se reutiliza chequear_columnas() para HB010/HB020, pi01/pi02 y DB010/DB020
  antes de acceder a ellas. Se conserva columnas_faltantes, indicando argumento
  y columnas.
- El cambio es efectivo en estandarizar_hogares(), expandir_hogares() y
  calcular_hogares(). Se ampliaron sus tests con ausencias individuales y
  conjuntas de columnas; se comprobaron H omitido o NULL, tipos invalidos y
  aceptación de data frames, tibbles y auxiliares opcionales en el validador.
- La concordancia y los controles de atributos no cambiaron. La obligatoriedad
  de P en las entradas que lo necesitan se abordará en sus entregas.
- Código formateado con air; no cambió el contenido de la documentación ni las firmas.
- devtools::test(): 209 expectativas aprobadas, sin fallas, advertencias ni
  omisiones. Persisten las advertencias regionales al iniciar R, fuera de los
  tests.
- git diff --check sin errores.
- Próxima entrega: expandir_hogares().

## Cuarta entrega ejecutada: expandir_hogares()

- .expandir y .etiquetar usan rlang::check_bool(), conservando no_logical e
  identificando correctamente el argumento.
- Se exige .P como data frame, rechazando NULL y el argumento omitido con
  no_data_frame.
- Se comprueba que el atributo exacto expandida de P sea un único TRUE o FALSE,
  con clase no_expandida. FALSE es válido: expresa que se descartaron variables
  originales.
- El chequeo compartido del atributo base usa coincidencia exacta del nombre e
  identical() con "P", evitando errores incidentales ante NA o vectores y
  conservando no_p y no_expandida.
- Se actualizaron los insumos sintéticos y los tests de argumentos lógicos, P
  obligatorio y atributos inválidos; se comprueba que ambos valores de
  expandida superan ese control y alcanzan el chequeo siguiente.
- Documentación de .P actualizada y regenerada con devtools::document(); código
  formateado con air.
- devtools::test(): 251 expectativas aprobadas, sin fallas, advertencias ni
  omisiones. R sigue emitiendo advertencias regionales al iniciar, fuera de los
  tests.
- git diff --check sin errores.
- Próxima entrega: calcular_personas(). La concordancia continúa pendiente para
  la etapa acordada.

## Quinta entrega ejecutada: calcular_personas()

- Se reemplazaron los controles manuales de tipo de .P y .expandir por
  check_data_frame() y check_bool(), conservando no_data_frame y no_logical.
- Se exige que los atributos exactos estandar y base sean TRUE y "P",
  respectivamente, conservando no_estandar y no_p. Se rechazan atributos
  ausentes, NA, vectores y coincidencias parciales de nombres.
- Se comprueba la presencia de PB010 y PB020 mediante chequear_columnas(). No
  se agregó todavía validación de sus valores ni concordancia.
- Nuevo archivo test-calcular_personas.R con 53 expectativas: tipos, atributos,
  columnas, booleanos y aceptación de data frames/tibbles con ambas opciones de
  expansión. Para las entradas válidas se simula el cálculo interno y el
  informe, sin utilizar datos de encuesta.
- Documentación actualizada y regenerada; código formateado con air. Se
  corrigió el BOM del nuevo archivo de tests para permitir su lectura por R y
  air.
- devtools::test(): 304 expectativas aprobadas, sin fallas, advertencias ni
  omisiones. Persisten las advertencias regionales al iniciar R, fuera de los
  tests.
- git diff --check sin errores.
- Próxima entrega: imputar_personas().

## Sexta entrega ejecutada: imputar_personas()

- Se reemplazó el control manual de .P por rlang::check_data_frame(),
  conservando no_data_frame. Se requieren los atributos exactos estandar = TRUE
  y base = "P", conservando no_estandar y no_p.
- Se comprueba la presencia de PB010 y PB020 con chequear_columnas() sólo
  después de descartar imputada = TRUE. Una base ya imputada se devuelve sin
  exigir esas columnas. La concordancia continúa pendiente.
- El atributo exacto imputada puede estar ausente o ser un único TRUE o FALSE.
  Sólo TRUE evita repetir la imputación; FALSE ya no provoca una salida errónea.
- El atributo exacto flags imp. es obligatorio y debe ser un único TRUE o FALSE.
  FALSE mantiene el cálculo de flags antes de imputar. Los estados inválidos
  se rechazan con check_bool(), clase no_logical y el atributo en el mensaje.
- Los tests de la función contienen 116 expectativas y cubren tipos, atributos,
  columnas y el flujo de entradas válidas como data frames y tibbles. Se simulan
  las funciones internas para comprobar el orden de las llamadas, el cálculo
  condicional de flags y la salida anticipada, sin utilizar datos de encuesta.
- Documentación actualizada y regenerada con devtools::document(); código
  formateado con air, incluidos ajustes de formato en los auxiliares del archivo.
- Se ajustaron los tests para exigir las columnas cuando imputada está ausente
  o es FALSE, y aceptar su ausencia individual o conjunta cuando es TRUE,
  tanto en data frames como en tibbles.
- devtools::test(): 416 expectativas aprobadas, sin fallas, advertencias ni
  omisiones. Persisten las advertencias regionales al iniciar R, fuera de los
  tests.
- git diff --check sin errores. Se conservaron los cambios previos del plan.
- Próxima entrega: calcular_hogares(), pendiente de confirmación.

## Séptima entrega ejecutada: calcular_hogares()

- Se exige .P como data frame con rlang::check_data_frame(), rechazando NULL
  y el argumento omitido con no_data_frame. Se conserva el validador compartido.
- Se requieren los atributos exactos estandar = TRUE y base = "H" en .H,
  conservando no_estandar y no_h.
- El atributo exacto expandida de .P debe ser un único TRUE o FALSE, con clase
  no_expandida. Ambos valores son válidos. .expandir usa check_bool(),
  conservando no_logical e identificando el argumento en el mensaje.
- Se comprueban HB030 y pi04 con chequear_columnas() antes de agregar personas
  y unir las bases. Los controles de año, país y base de .P siguen a cargo del
  validador compartido; no se modificó la concordancia.
- Los tests de la función contienen 147 expectativas. Cubren argumentos
  obligatorios, tipos, atributos exactos, booleanos e identificadores ausentes.
  Las entradas válidas combinan data frames y tibbles con ambos estados de
  expansión de .P y ambas opciones de .expandir. Se simulan la agregación, el
  cálculo y el informe, comprobando la unión real con datos sintéticos.
- Documentación de .H y .P actualizada y regenerada con devtools::document();
  código formateado con air, incluidos ajustes de formato en los auxiliares.
- devtools::test(): 535 expectativas aprobadas, sin fallas, advertencias ni
  omisiones. Persisten las advertencias regionales al iniciar R, fuera de los
  tests.
- git diff --check sin errores. No se cambiaron ramas ni se hicieron commits.
- Próxima entrega: etiquetar_eusilc(), pendiente de confirmación.

## Octava entrega ejecutada: etiquetar_eusilc()

- Se exige .datos como data frame con rlang::check_data_frame(), rechazando
  NULL y el argumento omitido con no_data_frame.
- El atributo exacto expandida debe ser un único TRUE o FALSE; ambos estados
  son válidos. Se usa check_bool() con clase no_expandida.
- Se exige el atributo exacto base igual a "P" o "H", con clase no_base y un
  mensaje que identifica el atributo y .datos. Antes no tenía un control propio.
- No se exigen columnas específicas: sólo se etiquetan las presentes. Una base
  sin columnas se devuelve sin cambios después de validar sus atributos,
  evitando un error incidental de labelled::set_value_labels().
- Nuevo archivo test-etiquetar_eusilc.R con 112 expectativas: tipos, atributos
  ausentes o inválidos, coincidencias parciales y etiquetado real de personas y
  hogares en data frames y tibbles, con ambos estados de expansión. Se comprueba
  la conservación de valores, columnas originales, clases y atributos, y se
  prueban conjuntos sin columnas o sin variables para etiquetar.
- Documentación actualizada y regenerada con devtools::document(); código
  formateado con air. No se modificó la función interna de etiquetado.
- devtools::test(): 647 expectativas aprobadas, sin fallas, advertencias ni
  omisiones. Persisten las advertencias regionales al iniciar R, fuera de los
  tests.
- git diff --check sin errores. No se cambiaron ramas ni se hicieron commits.
- Próxima entrega: ver_advertencias(), pendiente de confirmación.

## Novena entrega ejecutada: ver_advertencias()

- Se exige .datos como data frame con rlang::check_data_frame(), rechazando
  NULL y el argumento omitido con no_data_frame.
- Se conserva la lectura exacta del atributo advertencias y la clase
  advertencias_no_disponibles cuando falta. Si existe, debe ser un data frame
  o tibble; los tipos inválidos se rechazan con no_data_frame y un mensaje que
  identifica el atributo de .datos.
- Se aceptan tablas de advertencias vacías y se devuelve el atributo sin
  modificar. No se exigen columnas de encuesta ni otros atributos porque la
  función sólo consulta las advertencias almacenadas.
- Nuevo archivo test-ver_advertencias.R con 57 expectativas: tipos inválidos,
  atributo ausente o con nombre parcial, y devolución intacta de data frames y
  tibbles con contenido, sin filas o sin columnas, incluidos sus atributos.
- Documentación actualizada y regenerada con devtools::document(); código
  formateado con air. No se modificaron las funciones internas.
- devtools::test(): 704 expectativas aprobadas, sin fallas, advertencias ni
  omisiones. Persisten las advertencias regionales al iniciar R, fuera de los
  tests.
- git diff --check sin errores. No se cambiaron ramas ni se hicieron commits.
- Queda completada la segunda etapa para las funciones enumeradas en el plan.
  Próxima entrega: tercera etapa, concordancia de personas, pendiente de
  confirmación. No se avanzó en esa etapa.
