# Normalización de los tipos de las variables

## Problema

Al importar datos, `readr` puede interpretar como lógicas las columnas cuyos
valores están completamente perdidos. Algunas operaciones posteriores requieren
tipos compatibles y pueden fallar: por ejemplo, la recodificación de `DB100`
para construir `pi07` y `hi07`.

El ajuste actual para Alemania antes de 2021 resuelve un caso particular, pero
el problema puede afectar otras variables, países y años.

## Propuesta

Mantener una tabla auxiliar con los tipos esperados de las variables utilizadas
por el paquete y aplicarla mediante una función interna, tentativamente
`normalizar_tipos()`, al comienzo de la estandarización.

La tabla tendría inicialmente los siguientes campos:

- `conjunto`: conjunto de origen, como D, R, P o H.
- `variable`: nombre original de la variable.
- `tipo`: tipo esperado, usando `integer`, `double` o `character`.

Ejemplos de registros:

- Conjunto D:
  - `DB040`: `character`.
  - `DB100`: `integer`.
  - `DB090`: `double`.

Agregar períodos de vigencia únicamente si se encuentran variables cuyo tipo
esperado cambia entre años.

Declarar explícitamente las variables utilizadas por el paquete, aunque la
mayoría sean numéricas. No asumir que las variables no registradas son
numéricas: esa regla podría ocultar omisiones y convertir códigos incorrectamente.
Las columnas adicionales que no figuren en la tabla quedarían intactas.

## Reglas de normalización

Distinguir entre asignar el tipo de una columna totalmente perdida y convertir
una columna con datos:

- Si la columna no está presente:
  - Dejarla ausente y conservar el tratamiento actual de disponibilidad.
- Si el tipo coincide con el esperado:
  - Conservarla.
- Si todos sus valores son `NA`:
  - Crear los valores perdidos del tipo esperado.
- Si el tipo difiere y la conversión conserva los valores:
  - Convertirla.
- Si la conversión pierde información o produce nuevos `NA`:
  - Detener el procedimiento e identificar la variable y el problema.

Por ejemplo, convertir `c(1, 2)` de real a entero es razonable, pero convertir
`c(1, 2.5)` a entero no debería truncar silenciosamente el segundo valor.

## Integración con el procedimiento

Normalizar los conjuntos originales antes de las uniones y las transformaciones,
incluidos D y R cuando se proporcionen. Esto permitiría resolver también
incompatibilidades entre columnas utilizadas como claves de unión.

Ubicar la llamada en el recorrido compartido por `estandarizar_*()` y
`expandir_*()`. Estas últimas llaman directamente a las funciones internas de
estandarización; agregar la normalización solamente a las funciones públicas
dejaría un camino sin cubrir.

## Aspectos a tener en cuenta

- Identificadores y códigos de texto:
  - Que contengan dígitos no implica que deban ser numéricos.
  - Una conversión puede eliminar ceros iniciales.
- Información perdida durante la lectura:
  - Si el lector ya convirtió `"001"` en `1`, la normalización posterior no
    puede recuperar el valor original.
  - A futuro, la misma tabla podría servir para configurar la importación.
- Factores y columnas etiquetadas:
  - Requieren un tratamiento explícito.
  - Convertir un factor directamente a entero devuelve sus códigos internos.
- Enteros frente a reales:
  - R puede representar códigos enteros como `double` sin que exista un problema.
  - Permitir la conversión a entero cuando sea exacta.
  - Reservar `double` para ponderadores, ingresos e identificadores que puedan
    exceder el rango de los enteros de R.
- Mantenimiento:
  - La tabla debe reflejar las variables originales utilizadas por el paquete.
  - Revisarla cuando se incorporen variables o cambien los formatos de origen.

## Implementación por etapas

1. Registrar los tipos esperados y corregir únicamente las columnas completamente
   perdidas.
2. Incorporar conversiones seguras para las columnas con valores, con una política
   explícita para conversiones que pierdan información.

La tabla sería la misma desde el inicio. Esta secuencia permite resolver primero
el problema actual y abordar después una política más amplia de conversión.

Esta es una propuesta pendiente de implementación.
