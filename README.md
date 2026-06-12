# odsa.eusilc

# Herramientas para compatibilizar los microdatos de la _European Union Statistics on Income and Living Conditions_ con los de la _Encuesta Permanente de Hogares_ argentina.

_¡Este paquete se encuentra en desarrollo activo! Está abierto a sugerencias,
correcciones y demás tipos de aporte._

## Descripción

`{odsa.eusilc}` permite compatibilizar variables de la [_European Union Statistics on Income and Living
Conditions_](https://ec.europa.eu/eurostat/web/microdata/collections-research/european-union-statistics-on-income-and-living-conditions)
con las de la [_Encuesta Permanente de Hogares_](https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos)
de la Argentina para facilitar los análisis comparativos. Este paquete **NO**
da acceso a los microdatos de la _EU-SILC_; los usuarios deben acceder a
ellos por los [medios oficiales de Eurostat](https://ec.europa.eu/eurostat/web/microdata/access).

El paquete ofrece funciones para transformar los cuatro conjuntos originales de
la EU-SILC (**D**, **H**, **R** y **P**) en dos bases armonizadas: una de nivel
persona y otra de nivel hogar. Estas bases incorporan variables construidas con
nombres, categorías, unidades y etiquetas organizadas en dimensiones habituales
de análisis de la EPH en el Observatorio de la Deuda Social Argentina:
características demográficas, inserción laboral, ingresos, perceptores y
atributos del hogar.

La compatibilización se realiza mediante una secuencia de operaciones
encadenadas integradas en las funciones principales del paquete:
estandarización de los datos originales para corregir diferencias de estructura
según país y año; imputación de valores faltantes o inconsistentes;
recodificación y construcción de variables comparables; y asigación de
etiquetas a variables y valores.

El paquete busca dar un primer paso en la construcción de un puente
metodológico entre las tradiciones estadísticas y de investigación europeas y
latinomericanas, facilitando las comparaciones entre regiones sin tener que
reconstruir desde cero los criterios de compatibilización entre fuentes de
información. 

# Instalación

Para instalar `odsa.eusilc` es necesario contar previamente con el
paquete `pak`.

```r
# 'pak' ya está en CRAN
install.packages("pak")
# Una vez instalado se carga
library(pak)
```
## Versión _release_ o estable

La versión estable se instala a partir del archivo `odsa.eusilc_0.1.0.tar.gz`,
descargable desde la sección de _releases_, con la función `pak::pkg_install`,
indicando la fuente `local`:

``` r
# La ruta es relativa al working directory, que se ve con getwd()
pak::pkg_install("local::/ruta/al/archivo/odsa.eusilc_0.1.0.tar.gz")
```

## Versión de desarrollo

La versión de desarrollo se instala desde GitHub con la misma función
`pak::pkg_install`:

``` r
pak::pkg_install("github::odsa-uca/odsa.eusilc")
```

Una vez instalado, el paquete queda en la biblioteca de R y se puede
cargar cuando se necesite.

``` r
# Se carga como cualquier otro paquete
library(odsa.eusilc)
```

El proceso de instalación solo debe repetirse cuando haya una nueva
versión.

# Funciones principales

## `expandir_personas`

Armoniza, imputa valores faltantes o inconsistentes, genera nuevas
variables y etiqueta el conjunto de datos **P**.

``` r
expandir_personas(
    .P,                # Conjunto de datos P de la EU-SILC
    .R = NULL,         # Conjunto de datos R de la EU-SILC (opcional)
    .D = NULL,         # Conjunto de datos D de la EU-SILC (opcional)
    .imputar = FALSE,  # ¿Se imputan valores faltantes? (opcional)
    .expandir = FALSE, # ¿Se retienen las variables originales? (opcional)
    .etiquetar = TRUE  # ¿Se etiquetan las variables y sus valores? (opcional)
  )
  
```

`expandir_personas` encadena cuatro operaciones principales sobre los
datos:

1.  **Estandarizar.** Según el año, el país y los conjuntos
    proporcionados (**R** y **D**), se reorganizan los datos para
    adoptar un formato estándar.
2.  **Imputar.** (Opcional) Se imputan valores faltantes o
    inconsistentes en las variables originales, de acuerdo con los
    criterios utilizados en los *scripts* de SPSS.
3.  **Construir variables.** Se construyen variables nuevas a partir de
    las originales.
4.  **Etiquetar.** (Opcional) Se asignan etiquetas a las variables
    nuevas y sus valores.


El resultado es un conjunto de datos de nivel persona con variables nuevas (y,
opcionalmente, variables originales) organizadas en bloques:

-   **I** *(Identificación)* Variables de ID, ponderadores, región, etc.
-   **D** *(Demográficos)* Edad, sexo, nivel educativo, jefatura del
    hogar, etc.
-   **L** *(Laborales)* Condición de actividad, ocupación,
    características del lugar de trabajo, etc.
-   **Y** *(Ingresos)* Ingresos totales y según fuente.
-   **(aux.)** *(Auxiliares)* Flags de imputación, principalmente.

## `expandir_hogares`

Armoniza, agrega información de nivel persona a nivel hogar, genera
nuevas variables y etiqueta el conjunto de datos **H**.

``` r
expandir_hogares(
    .H,                # Conjunto de datos H de la EU-SILC
    .P,                # Conjunto de datos resultante de expandir_personas(P)
    .D = NULL,         # Conjunto de datos D de la EU-SILC (opcional)
    .expandir = FALSE, # ¿Se retienen las variables originales?
    .etiquetar = TRUE  # ¿Se etiquetan variables y valores?
  )
  
```

`expandir_hogares` encadena operaciones similares:

1.  **Estandarizar.** De forma análoga a `expandir_personas`.
2.  **Agregar.** Agrega la información de ingresos de nivel persona a
    nivel hogar. La función requiere el conjunto **P** previamente
    procesado por `expandir_personas`.
3.  **Construir variables.** Se construyen variables nuevas,
    fundamentalmente de ingresos.
4.  **Etiquetar.** De forma análoga a `expandir_personas`.

El resultado es un conjunto de datos de nivel hogar con variables nuevas,
información agregada de ingresos personales organizadas en bloques:

-   **I** *(Identificación)* Variables de ID, ponderadores, etc.
-   **D** *(Demográficos)* Tamaño del hogar, tipo de hogar, etc.
-   **L** *(Laborales)* A definir.
-   **Y** *(Ingresos)* Ingresos totales y por fuente **de nivel
    individual y de nivel hogar**.
-   **P** *(Perceptores)* Perceptores de ingresos totales y por fuente.
-   **(aux.)** *(Auxiliares)* A definir.

# Otras funciones

Las operaciones que encadenan `expandir_personas` y `expandir_hogares`
se pueden aplicar de forma independiente.

``` r
# Personas
estandarizar_personas(.P, .R, .D)
imputar_personas(.P)
calcular_personas(.P)
etiquetar_personas(.P)

# Hogares
estandarizar_hogares(.H, .D)
agregar_personas(.P)
calcular_hogares(.H, .P)
etiquetar_hogares(.H)
```

Esto permite verificar los resultados de cada paso y aporta mayor
flexibilidad al proceso.

# Tablas auxiliares

El paquete también ofrece dos tablas auxiliares con información
potencialmente relevante:

-   `tabla_ppa`. Contiene los factores de conversión a PPA en dólares de
    Estados Unidos para un gran número de países europeos.
-   `etiquetas`. El diseño de registro de las bases de datos que
    construyen `expandir_personas` y `expandir_hogares`.


# Ejemplo de uso

Este paquete **NO** ofrece acceso a los microdatos de la _EU-SILC_;
cada usuario deberá obtener acceso por los medios oficiales de Eurostat. Las
herramientas que se ofrecen presuponen que el usuario tiene garantizado el
acceso a los conjuntos de datos.

Supongamos se quiere obtener los microdatos compatibilizados de Italia en
2023. El primer paso es cargar los conjuntos de datos en R:

``` r
# Cargamos los conjuntos necesarios y los guardamos
H <- read.csv("ruta/al/archivo/UDB_cIT23H.csv")
D <- read.csv("ruta/al/archivo/UDB_cIT23D.csv")
R <- read.csv("ruta/al/archivo/UDB_cIT23R.csv")
P <- read.csv("ruta/al/archivo/UDB_cIT23P.csv")
```

Luego se llama a `expandir_personas` con los conjuntos `P`, `R` y `D`, y
se guarda el resultado en `personas`.

``` r
personas <- odsa.eusilc::expandir_personas(P, D, R, .imputar = FALSE)
```

El conjunto `personas` conserva la misma cantidad de filas que `P` e
incorpora 73 variables nuevas, con variables y valores etiquetados.

Luego se crea la base de nivel hogar. El procedimiento es similar y el conjunto
resultante también queda etiquetado.

``` r
# ¡Ojo! el conjunto 'personas' es el que se obtuvo en el paso anterior
hogares <- odsa.eusilc::expandir_hogares(H, personas, D)
```

La documentación de cada función describe sus parámetros, explica en
detalle lo que hace y, en los casos relevantes, justifica las decisiones
metodológicas adoptadas.

``` r
# Por ejemplo, se puede acceder a la documentación de 'expandir_personas' mediante
?odsa.eusilc::expandir_personas
```

# Cita recomendada

`{odsa.eusilc}` no está en el repositorio oficial de R y se encuentra en
desarrollo activo. Por el momento, la cita recomendada es:

```
# Cita formateada
Piderit, F. (2026). {odsa.eusilc}: Funciones para trabajar con los datos de la encuesta EU-SILC (Versión 0.1.0). Observatorio de la Deuda Social Argentina. https://github.com/odsa-uca/odsa.eusilc.git

# En formato bibtex
@software{piderit_2026_odsaeusilc,
  title = {\{odsa.eusilc\}: Funciones Para Trabajar Con Los Datos de La Encuesta {{EU-SILC}}},
  shorttitle = {\{odsa.eusilc\}},
  author = {Piderit, Fernando},
  date = {2026-06-12},
  location = {Buenos Aires},
  url = {https://github.com/odsa-uca/odsa.eusilc.git},
  organization = {Observatorio de la Deuda Social Argentina},
  version = {0.1.0}
}
```
