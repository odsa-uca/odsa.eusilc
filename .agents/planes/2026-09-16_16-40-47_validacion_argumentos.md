# Mejora gradual de validaciones

## Prompt original

Me gustaría que me ayudes a mejorar la validación de argumentos de las funciones del paquete. Para empezar, hay muchas validaciones que están hechas "a mano" que podrían hacerse más sintéticamente con funciones del paquete `rlang` como `rlang:::check_data_frame`. Además, hay validaciones complejas, que tienen que ver con la concordancia entre años y países de los conjuntos de datos proporcionados, que me parece que no están implementadas de manera adecuada. Por último, hay cuestiones importantes que directamente no se validan, como la presencia de las variables de año y país de los conjuntos. En paralelo, estos cambios implicarían modificar la suite de tests que existen hasta ahora. Me gustaría que encaremos este problema por etapas: (1) primero, me gustaría que revises las funciones del paquete y evalúes las validaciones que se hacen en ellas; (2) luego, me gustaría que avances con el reemplazo de las validaciones "a mano" por su respectivas versiones del paquete `rlang`; (3) después, me gustaría seguir con las validaciones complejas que chequean la concordancia entre conjuntos de datos. En paralelo, me gustaría que, si hay algo que es necesario validar, se incorpore la validación correspondiente en el momento. También me gustaría que vayas modificando los tests correspondientes a medida que realizamos cambios. Preferiría que vayamos por funciones así puedo ir siguiendo los cambios que haces.

## Plan acordado

Una función por turno, empezando por estandarizar_personas(). Usar rlang::check_data_frame() y rlang::check_bool(), declarar rlang >= 1.2.0 y conservar las clases de error actuales. Los mensajes de tipo serán los estándares de rlang. Validar columnas obligatorias al abordar cada función y actualizar sus tests sintéticos. Las funciones internas de transformación podrán asumir entradas válidas.

Primera entrega: reemplazar los controles de tipo de P, D y R en chequear_bases_personas(), permitir NULL solo en D y R, comprobar PB010/PB020, DB010/DB020 y RB010/RB020 antes de acceder a ellas, y validar .flags como un único TRUE o FALSE. Agregar clase columnas_faltantes con argumento y columnas en el mensaje. Actualizar también los tests de expandir_personas() por compartir el validador. Reservar la lógica de concordancia para la tercera etapa.

Orden posterior: expandir_personas(), estandarizar_hogares(), expandir_hogares(), calcular_personas(), imputar_personas(), calcular_hogares(), etiquetar_eusilc() y ver_advertencias(). Incorporar controles necesarios de argumentos, columnas y atributos, distinguiendo estados booleanos de expandida = FALSE, que es válido.

Tercera etapa: concordancia de personas y luego hogares. Cada conjunto suministrado debe contener exclusivamente un año y país, iguales entre conjuntos, sin NA ni bases vacías. Conservar las clases existentes de discrepancia. No agregar restricciones de cobertura de individuos u hogares ni validar exhaustivamente todas las variables de encuesta.

Verificar con datos sintéticos: tipos inválidos, booleanos inválidos, columnas y atributos ausentes o incorrectos; posteriormente coincidencias, diferencias, NA, conjuntos vacíos y auxiliares con valores adicionales. Ejecutar la suite por entrega, formatear con air y regenerar documentación cuando corresponda.

## Primera entrega ejecutada

- DESCRIPTION declara rlang >= 1.2.0.
- chequear_bases_personas() usa check_data_frame() y comprueba columnas con el nuevo auxiliar documentado chequear_columnas(). Los controles estructurales se realizan antes de la concordancia.
- estandarizar_personas() usa check_bool() para .flags.
- Se conservan no_data_frame y no_logical; se agrega columnas_faltantes.
- Tests de ambas entradas cubren los nuevos controles; se comprueba aceptación de data frames, tibbles y auxiliares NULL en el validador.
- Código formateado con air y documentación regenerada con devtools::document().
- devtools::test(): 93 expectativas aprobadas, sin fallas, advertencias ni omisiones. R emite advertencias de configuración regional al iniciar, fuera de los tests.
- git diff --check sin errores de espacios.
- Pendiente: las entregas posteriores y la concordancia estricta. No se cambiaron ramas ni se hicieron commits.

## Segunda entrega ejecutada: expandir_personas()

Solicitud: «Bien, pasemos a la próxima entrega.»

- Se reemplazaron los controles manuales de .imputar, .expandir y .etiquetar por rlang::check_bool(), conservando la clase no_logical.
- Los tres argumentos exigen un único TRUE o FALSE. El mensaje de .expandir identifica ahora correctamente ese argumento.
- Los tests cubren texto, 0, 1, NA, NULL, logical() y vectores lógicos de longitud mayor que uno para cada argumento; verifican la clase y el nombre del argumento en el mensaje.
- Código formateado con air. No cambiaron las firmas ni el contenido de la documentación.
- devtools::test(): 132 expectativas aprobadas, sin fallas, advertencias ni omisiones. Persisten las advertencias de configuración regional al iniciar R, fuera de los tests.
- git diff --check sin errores.
- Próxima entrega: estandarizar_hogares(). La concordancia estricta continúa reservada para la tercera etapa.
