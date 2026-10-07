# Revisión de insumos para armonizar los conjuntos panel de la EU-SILC

Fecha: 2026-10-06.

## Resultado principal

Hay un núcleo inicial viable de identificación, sexo, tamaño del hogar e
ingresos por fuentes. Los componentes de ingreso no sectoriales conservan sus
nombres a través del corte de 2021 y figuran en el contenido longitudinal. Su
disponibilidad nominal no garantiza valores utilizables en todas las filas:
importan los componentes netos efectivamente difundidos, las flags y la
cobertura de los miembros del hogar.

Actividad, educación, ocupación y contrato son ampliaciones posibles, pero los
paneles que atraviesan 2021 pueden tener huecos estructurales en sus primeras
olas. Región, urbanización y edades requieren auxiliares o sustituciones y
tienen excepciones por país. No existe un único ponderador personal
longitudinal que sustituya automáticamente `PB040`.

Los indicadores sectoriales y EGP quedan fuera del primer mínimo, por depender
de insumos excluidos o modulares. En cambio, los meses de actividad pueden
recuperarse del calendario mensual cuando esté publicado y completo, sin
imputación. No es correcto declarar toda la familia de ingresos horarios
imposible sólo porque falten `PL073`–`PL076`; sí debe explicarse la
aproximación que ya usa su fórmula.

## Método y límites de la evidencia

- Se leyeron las funciones [calcular_personas.R](../../R/calcular_personas.R),
  [calcular_hogares.R](../../R/calcular_hogares.R),
  [estandarizar_personas.R](../../R/estandarizar_personas.R),
  [estandarizar_hogares.R](../../R/estandarizar_hogares.R) y sus auxiliares. Se
  inspeccionaron las tablas internas de etiquetas, ISCO, recodificación y PPA.
- Se contrastaron las guías de las doce operaciones, las reglas longitudinales
  `L* DIFFERENCES` y los contenidos transversales relevantes para las fuentes
  potenciales. Las páginas citadas son páginas del PDF, contando desde 1.
- Las guías describen variables transmitidas a Eurostat. Para decidir su
  publicación se da prioridad a `L* DIFFERENCES`, sus listas de exclusión y sus
  anexos de contenido; un anexo nominal no invalida una supresión específica
  por país.
- Se distinguen cuatro estados: disponible nominalmente en panel; derivable de
  otros insumos panel; fuente transversal potencial condicionada; sin
  equivalente observado que permita recuperar la información exacta.
- La pérdida de nombres antiguos en paneles mixtos se toma como información
  aportada por el usuario. Las guías también describen cambios de nombre y
  flags de no aplicabilidad antes de 2021, pero no se supone una recodificación
  retrospectiva de las nuevas columnas.
- No se dispone de microdatos de encuesta. Esta es una revisión documental y
  del código: no verifica empíricamente cobertura de filas, correspondencias de
  identificadores ni completitud de componentes netos. `tabla_cobertura` se usó
  para seleccionar países; su cobertura transversal previa no acredita una
  revisión longitudinal.

## Entregas, años y fuentes auxiliares

### Cambios de disponibilidad

- Entregas 2014–2016: las listas generales no excluyen varios insumos laborales
  que luego desaparecen. Es razonable evaluarlos como disponibles nominalmente
  según las guías, sujeto a excepciones nacionales; no se garantiza su
  contenido efectivo. Fuentes: [L14], [L15] y [L16], sección general de
  exclusiones, página 2.
- Entrega 2017: se excluyen `PB210`, `PB220A/B`, `PB040`, `RB050`,
  `PL073`–`PL076`, `PL111`, `PL130`, `PL150` y el calendario `PL211A`–`PL211L`,
  entre otros. Fuente: [L17], páginas 2–3, variables eliminadas.
- Entregas 2018–2020: persisten las exclusiones de origen/ciudadanía de P,
  meses agregados, rama, tamaño y supervisión, pero el calendario vuelve a
  figurar en los anexos. Fuentes: [L18], páginas 2–3 y 28; [L19], páginas 2–3 y
  28; [L20], páginas 2–3 y 25.
- Entregas 2021–2025: no se difunden variables antiguas que dejaron de
  pertenecer al núcleo desde 2021. Se excluyen, además, `PL073`–`PL076`,
  `PL111A/B`, `PL150` y los ponderadores transversales personales. `PL130` y
  `PL230` no figuran en el contenido longitudinal examinado. Fuentes:
  [L21]–[L25], sección general, páginas 1–3; anexos de contenido.
- Deben distinguirse el año de la observación y el año de operación/entrega
  documentado por `L*`. Un registro de 2019 dentro de una entrega posterior no
  hereda automáticamente el contenido de la entrega longitudinal de 2019.
- Un período como 2019–2022 puede carecer de `PL031`, `PE040`, `PL040`, `PL051`
  o `PL140` para sus olas anteriores al corte. Sólo puede usarse un nombre
  nuevo si realmente tiene valores aplicables a esas olas; no basta su
  presencia como columna. No completar esos huecos usando valores de otras olas
  ni la situación del último mes del período de ingresos como si fuera la
  situación actual.

### Cruces longitudinales y transversales

- Cruce P–R longitudinal: país, año e identificador personal
  (`PB020/PB010/PB030` frente a `RB020/RB010/RB030`).
- Cruce P–D longitudinal: país, año y hogar actual (`PX030` frente a `DB030`).
  No extraer el hogar actual de una parte de `PB030`.
- Cruce H–D longitudinal: país, año e identificador del hogar
  (`HB020/HB010/HB030` frente a `DB020/DB010/DB030`).
- Agregación P–H longitudinal: país, año y hogar actual. Preservar filas y
  orden del conjunto principal; la existencia y unicidad de las claves deberá
  comprobarse cuando se implemente el traspaso.
- Una fuente transversal con el mismo país y año no acredita identidad de
  personas u hogares. Eurostat explica que los identificadores de los archivos
  científicos anonimizados impiden el enlace directo entre componentes; las
  correspondencias excepcionales requieren intervención de los propietarios de
  los datos. Fuente: [descripción de los microdatos EU-SILC, publicación de
  2024](https://ec.europa.eu/eurostat/documents/203647/20298610/EUSILC_DOI_2024_release_1.pdf/6bd8e55d-47eb-3fde-cbdb-0440d3cc9ac7?t=1730712751843),
  páginas 2 y 4.
- Por tanto, «P/R/D transversal» indica dónde buscar el insumo si se dispone de
  un enlace válido. No recomienda cruzar directamente identificadores ni
  reconstruir correspondencias por semejanza de características. Las
  diferencias de anonimización y población también deben revisarse antes de
  combinar valores.

## Inventario de variables de personas

### Identificación y ponderación

- `pi01`, `pi02`, `pi04`, `pi05`:
  - Cálculo actual: `PB010`, `PB020`, `PX030`, `PB030`.
  - Fuente: P longitudinal. No requieren un auxiliar para su construcción normal.
  - `PX030` identifica el hogar actual. R aporta `RB040` como alternativa
    identificada documentalmente, pero su utilización debe respetar el estado
    de pertenencia y la correspondencia por persona-año; no redefine la muestra
    principal.
  - Referencias: [L20], página 18 y sección de variables añadidas; [L25],
    páginas 1 y 20–23.
- `pi03` y `pi07`:
  - Insumos: `DB040` y `DB100`; `pi07` recodifica urbanización con `tabla_pi07`.
  - Traspaso ordinario: D longitudinal. D transversal sólo sería una fuente
    potencial si el campo longitudinal está suprimido y el cruce es viable; no
    garantiza la recuperación de detalle eliminado por anonimización.
  - Cobertura parcial: región no publicada en PT hasta la entrega 2017,
    restricciones alemanas y distintos niveles NUTS. Ver excepciones
    nacionales.
- `pi06`:
  - Fórmula transversal: `PB040`. Su ausencia longitudinal desde 2017 no se
    resuelve usando un peso base como `PB050` o `RB060`.
  - Traspaso correcto para disponer de ponderación longitudinal: `RB062`,
    `RB063`, `RB064` y, cuando corresponda, `RB065`, `RB066` desde R.
  - Cada peso se refiere a una duración distinta; no elegirlo por el número de
    filas individuales ni por el máximo número de años del archivo. Recomendar
    conservar los pesos con sus nombres originales y posponer una única `pi06`
    hasta definir su significado.
  - `PB040` de P transversal no sustituye un ponderador longitudinal, incluso
    si se pudiera enlazar.
  - Referencias: [G25], páginas 71 y 81–83, capítulo de ponderadores, y
    248–252, fichas de pesos personales; [L17], página 2; [L25], páginas 2 y 20.
- Metadatos de seguimiento:
  - `DB075` y `DB076` proceden de D longitudinal y pueden acompañar al conjunto
    armonizado sin inventar todavía nombres nuevos.
  - `DB076` se introduce en 2021; no debe exigirse como observación válida para
    años anteriores. No equivale necesariamente al número de registros
    publicados de cada persona en P.
  - Referencias: [G21] y [G25], páginas 107–108, fichas de grupo de rotación y ola.

### Demográficos

- `pd01a`, edad en la entrevista:
  - Fórmula actual: `RB082`; antes de 2021 se calcula con `PB110`, `PB140`,
    `PB130` y `PB100`.
  - R longitudinal ofrece `RX010` como edad difundida; desde 2021 también
    figura `RB082`. Usar `RX010` evita depender necesariamente de una edad no
    difundida o de meses agrupados.
  - La reconstrucción con meses convertidos en trimestres no proporciona una
    edad exacta. No tratarla como equivalente cuando los meses ya fueron
    anonimizados.
  - IT elimina `RB082` desde 2021, pero la lista italiana no elimina `RX010`;
    es una alternativa nominal, pendiente de comprobar sus valores. DE elimina
    las edades difundidas en las entregas 2023–2025.
  - Referencias: [L14]–[L25], variables añadidas, primera página; [L21], página
    9; [L25], páginas 5 y 9.
- `pd01b`, edad al final del período de ingresos:
  - Fórmula actual: `RB081`, con alternativa `PB010 - RB080 - 1`.
  - `PX020` está en P y `RX020` en R longitudinal. Son alternativas difundidas
    para el mismo concepto; comprobar excepciones antes de usarlas.
  - Nacimiento perturbado o agrupado permite sólo una aproximación. No
    denominar edad exacta a `PB010 - RB080 - 1` en esos casos.
  - Referencias: [L25], páginas 1, 3, 5 y 9; [G25], fichas de edades, páginas 253–256.
- `pd01c`, edad aproximada agrupada:
  - Insumo actual: `RB080`, junto con el año y `agrupar_nac()`; `PB140` ofrece
    nacimiento en P cuando esté disponible.
  - R longitudinal o nacimiento de P permiten una derivación aproximada. No
    recuperan el nacimiento original ni todos los grupos nacionales.
  - Las clases de anonimización pueden definirse por entrega o al ingreso al
    panel; aplicar una agrupación por año de observación no garantiza
    reproducirlas. Registrar el criterio de la variable nueva sin afirmar
    equivalencia exacta con DE.
- `pd02`, sexo:
  - Insumo: `PB150`, disponible nominalmente en P; R contiene `RB090` como otra
    fuente de sexo.
  - La recodificación alemana de parejas del mismo sexo afecta ambas variables;
    traspasar R no restaura el valor original. Fuente: [L25], página 5, reglas
    DE; [L16]–[L19], reglas DE.
- `pd03`, educación:
  - Insumo estandarizado: `PE041`; antes de 2021, `PE040`. Recodificación
    actual mediante `tabla_pd03`.
  - Fuente ordinaria: P longitudinal. En paneles mixtos, el nombre anterior
    puede faltar para las primeras olas; P transversal es fuente potencial del
    valor de ese año. No hay un equivalente general de educación en R/D que
    resuelva la ausencia.
  - Top coding y agrupaciones nacionales afectan la granularidad; IT puede
    agrupar códigos que `tabla_pd03` distingue, y PL no aplica el top coding
    general de educación.
  - Referencias: [G14], página 265; [G21], página 327; [G25], página 328; [L25], páginas 3, 9 y 13.
- `pd04`, origen migratorio, y `pd05`, ciudadanía:
  - Insumos estandarizados: `RB280`, `RB290`; antes de 2021 se toman `PB210`, `PB220A`.
  - Entregas 2014–2016: fuente nominal P longitudinal. Entregas 2017–2020: esos
    campos de P están excluidos y R anterior no incorpora las nuevas variables;
    buscar P transversal, condicionado al enlace.
  - Desde 2021: fuente ordinaria R longitudinal. Para IT se suprimen
    `RB280/RB290`; R transversal es fuente potencial si contiene valores
    utilizables y existe enlace. Para años anteriores al corte dentro de
    paneles mixtos, P transversal es la fuente antigua potencial.
  - Mantener la distinción entre origen/ciudadanía y su recodificación
    LOC/EU/OTH. No propagar ciudadanía entre olas: puede cambiar. No inferir
    origen desde otro familiar.
  - Referencias: [L17]–[L20], página 2; [G21], páginas 267 y 271; [L21]–[L25],
    reglas IT, página 9.
- `pd06`, jefatura del hogar:
  - Se asigna siempre `NA_integer_` en el código actual. No hay una fórmula
    cuya disponibilidad pueda acreditarse.
  - Identificar al respondente del cuestionario no establece automáticamente
    jefatura. Dejarla fuera hasta definir el concepto y el procedimiento.

### Actividad y características ocupacionales

- `pl01`:
  - Insumo actual: `PL032`; la estandarización convierte `PL031` anterior en
    ocupado, desempleado o inactivo.
  - Fuente: P longitudinal cuando está disponible para la ola. En paneles
    mixtos, P transversal puede aportar `PL031` de las primeras olas si hay
    enlace válido.
  - R contiene `RB211` desde 2021 y `RB210` previamente, pero su
    respondente/universo difiere y también hay cambio de nombre. Es una
    alternativa conceptual a evaluar, no una equivalencia automática de `PL032`
    ni un rescate garantizado de años antiguos.
  - No usar ingresos positivos o la actividad del diciembre del período de
    ingresos como sustitución exacta de la actividad actual.
  - Referencias: [G21], página 358; [G25], páginas 265 y 359; [L20], página 25;
    [L25], páginas 20–21.
- `pl02a`, `pl02b`, `pl02c`:
  - Insumos: `PL040A/B`, actividad actual y selección A/B para C. Antes de 2021
    se particiona `PL040` según `PL031`.
  - Fuente: P longitudinal. Si falta categoría o actividad en las olas antiguas
    de un panel mixto, la fuente potencial es P transversal; R/D no aportan una
    categoría ocupacional equivalente general.
  - Se requiere adaptar la selección de C para no perder valores A conocidos
    porque B esté enteramente ausente.
- `pl10a/b/c`, `pl11a/b/c`, `pl12a/b/c`, `pl13a/b/c`:
  - Insumos: `PL051A/B` (antes `PL051`), actividad para C y `tabla_isco` para
    calificación.
  - Fuente ordinaria: P longitudinal. P transversal es fuente potencial para
    olas antiguas sin `PL051`; el detalle suprimido por anonimización no se
    considera recuperado por defecto.
  - `pl10*` conserva la ocupación publicada; no debe etiquetarse como dos
    dígitos donde sólo hay uno. `pl11*` puede conservar directamente el grupo
    difundido en DE, en lugar de dividirlo por diez.
  - La magnitud numérica sola no distingue el grupo 1 de la ocupación 01. Usar
    la precisión declarada para país/entrega.
  - `pl12*` y `pl13*` no son siempre recuperables exactamente desde un grupo
    principal: el grupo 1 contiene distintas calificaciones y el 3 distintas
    calificaciones profesionales. Las agrupaciones de gestores de PT también
    eliminan detalle. Si se adoptara una clasificación más gruesa, explicitar
    la pérdida de equivalencia con la fórmula actual.
  - Referencias: [G21], páginas 361–368, categoría y ocupación; [L16], página
    5; [L17]–[L19], reglas DE; [L25], página 13, reglas PT.

### Rama, tamaño, sector e indicadores compuestos

- `pl20a/b/c`:
  - Insumos: `PL111A/B`; antes de 2021 se parte de `PL111` y actividad.
  - Las entregas 2014–2016 no excluyen nominalmente `PL111`; desde 2017 la rama
    se elimina del contenido longitudinal. No hay equivalente general en R/D.
  - Fuente potencial: P transversal. La variante B anterior a 2021 ya queda
    como `NA` en la estandarización actual; disponer de rama no obliga a
    inventar una rama de último empleo.
- `pl21a/b`:
  - Insumo: `PL130` y `tabla_pl21`. Disponible nominalmente según las guías
    previas, pero excluido longitudinalmente desde 2017 y ausente de los anexos
    posteriores a 2021.
  - Fuente potencial: P transversal, según disponibilidad del núcleo antiguo o
    del módulo. La propia cobertura del paquete registra ausencia manual para
    PL en 2020; no extender ese dato transversal como una regla longitudinal
    adicional.
- `pl22`:
  - Insumo: `PL230`; el cálculo excluye el código 99.
  - Fuente potencial: P transversal en los módulos pertinentes, por ejemplo el
    contenido C20 y C23 examinado. No aparece como insumo general en los anexos
    L correspondientes; tampoco hay equivalente general en R/D.
  - No inferir sector público/privado de ocupación o rama.
- `pl30` y `pl31`:
  - Dependen de `PL040A`, `PL032`, `pl20a`, `pl21b`, `pl22`, `pl13a`.
  - No forman parte del mínimo con panel: faltan rama/tamaño/sector en las
    entregas recientes. Sólo plantear construcción con todos los insumos
    aplicables y un rescate transversal válido; no asignar una categoría
    completa a partir de ramas parciales del clasificador.
- `pl50`, EGP:
  - Insumos: `PL051A`, `PL040A` y `PL150`.
  - `PL150` no se excluye nominalmente en 2014–2016, pero se elimina desde
    2017. Fuente potencial para rescate: P transversal.
  - Ocupación agregada puede impedir la clasificación exacta incluso si se
    recupera supervisión. Ciertas ramas del algoritmo no dependen de
    supervisión; estudiar resultados parciales sería otro cambio explícito, no
    ejecutar sin cambios la función actual.
- `pl40a/b`, informalidad:
  - Insumos: `PL040A`, `PY030G`, `PY035G`. Los dos componentes de
    contribuciones figuran en los anexos P longitudinales recientes.
  - Es una ampliación posible sólo con panel si los tres insumos están
    disponibles para la observación. El código depende de su presencia incluso
    para algunas ramas que usan menos información.
  - La ausencia nacional de contribuciones no equivale a cero. La advertencia
    existente del paquete sobre `PY030G` en DE procede de fuentes transversales
    y debe verificarse para la entrega longitudinal concreta antes de
    trasladarla.
  - Fuentes potenciales de un insumo faltante: P transversal; R/D no contienen
    un reemplazo general de contribuciones individuales.
- `pomj`, `toc`, `pl41`:
  - `pomj` usa `PL140` antes de 2021; desde 2021 separa permanencia desde
    `PL141` (21/22 permanente, 11/12 temporal). `toc` distingue contrato
    escrito (11/21) y verbal (12/22).
  - Fuente: P longitudinal para las olas aplicables. `toc` no tiene un insumo
    equivalente anterior a 2021 en el cálculo actual. P transversal puede
    rescatar `PL140` antiguo de paneles mixtos, condicionado al enlace.
  - `pl41` combina `pl40a`, `pl12a` y `pomj`; su cobertura depende de
    contribuciones, detalle ocupacional y contrato. Dejarlo como ampliación, no
    parte del núcleo.
  - Referencias para las exclusiones de este bloque: [L17]–[L20], páginas 2–3;
    [L21]–[L25], páginas 2–3; [L25], páginas 21–23. Contrato: [G14], página
    300; [G21] y [G25], página 392.

### Ingresos no sectoriales

Todos los siguientes componentes se obtienen de P longitudinal. Las fórmulas se describen primero en euros anuales; el código mensualiza y convierte a moneda nacional mediante `PX010 / 12`.

- `py11`: `PY010N`, trabajo asalariado.
- `py12`: `PY050N`, trabajo no asalariado; puede ser negativo.
- `py10`: `PY010N + PY050N`.
- `py22`: `PY100N`, jubilación.
- `py23`: `PY080N`, pensión privada.
- `py21`: `PY100N + PY080N`.
- `py24`: `PY090N`, desempleo.
- `py25`: `PY110N + PY120N + PY130N + PY140N`.
- `py20`: `PY090N + PY110N + PY120N + PY130N + PY140N + PY100N + PY080N`.
- `py00`: `PY010N + PY050N + PY090N + PY110N + PY120N + PY130N + PY140N + PY100N + PY080N`.

Clasificación: núcleo candidato con panel, sujeto a disponibilidad de los
componentes netos y conversión. Los nombres conservados evitan el problema
específico de renombrado de 2021, pero no la propagación de faltantes en las
sumas.

- No reemplazar un importe neto por el bruto: no son equivalentes. Si el neto
  no se difunde en la entrega concreta, P transversal es sólo una fuente
  potencial; sin él o un procedimiento fiscal explícito no se recupera el neto
  exacto.
- Las flags de componentes integrados en otros importes requieren
  interpretación documental. La regla actual que pone `PY120N` a cero en IT
  cuando toda la flag vale -4 no debe trasladarse a un panel como comprobación
  global de varios años. Antes de usarla, verificar la integración longitudinal
  por año/observación y distinguir una descomposición contable documentada de
  una imputación.
- Mantener top/bottom coding y perturbaciones ya publicados; no intentar
  invertirlos ni reemplazarlos por valores transversales con otra
  anonimización.
- Referencias: [L20], páginas 26–28; [L25], páginas 21–26 y regla monetaria de
  la primera página; [G25], capítulo de ingresos y flags, páginas 42–56 y
  fichas de componentes desde la página 424.

### Ingresos sectoriales, meses, horas y PPA

- `py13`, `py14`, `py15`:
  - Dependen de `py10` y `pl31`. Se clasifican como condicionados por los
    rescates sectoriales; no se incluyen en el mínimo panel.
- `maa`, `man`:
  - Fórmula actual: `maa = PL073 + PL074`, `man = PL075 + PL076`.
  - Alternativa panel: contar en `PL211A`–`PL211L` los códigos 1/2 para
    asalariados y 3/4 para no asalariados, incluyendo trabajadores familiares
    según las guías.
  - Los conteos describen meses de actividad, no prueban que se haya cobrado
    ingreso cada mes. Las guías admiten derivar los agregados desde el
    calendario.
  - Entrega 2017: calendario y agregados excluidos, por lo que la fuente
    potencial es P transversal. Desde 2018, el calendario figura en los anexos;
    2014–2016 permiten nominalmente los insumos anteriores.
  - Un calendario incompleto/no aplicable no equivale a meses con cero
    actividad. Para esta primera propuesta conservadora, no producir el total
    anual cuando faltan meses; no se imputa ni se extrapola.
  - Referencias: [G14], páginas 294 y 308; [G18], páginas 294 y 308; [G25],
    páginas 371–378 y 399–422; [L17], página 3; [L18], página 28; [L25], página
    21.
- `haa`, `han`, `py11h`, `py12h`:
  - Dependen de `pl01`, `PL060`, `maa/man`, ingresos correspondientes y `PX010`.
  - Fórmula de horas del código: meses por horas semanales actuales por 4,2;
    ingreso anual dividido por esas horas. No representa una observación
    directa de horas durante todos los empleos del año.
  - Son construibles de manera condicionada con panel, incluso con meses
    derivados, pero la aproximación debe quedar explícita. No rescatar horas
    históricas exactas a partir de las actuales. No incluirlas en el primer
    núcleo.
- `ppa_factor`, `ppa_factor_us` y todos los sufijos `ppa` de personas:
  - Fuente: tabla interna `tabla_ppa_`, unida por año y país; no provienen de
    R/D ni de transversales.
  - Conversión actual: importe en moneda nacional dividido por `ppa_factor` y
    multiplicado por `ppa_factor_us`.
  - La tabla inspeccionada cubre 2016–2025. Para 2014–2015 falta esa fuente
    interna: requiere ampliarla desde una fuente PPA apropiada, no rescatar una
    variable individual de la encuesta.
  - Las versiones PPA heredan la disponibilidad de sus ingresos base. No
    producir variantes sectoriales u horarias sólo porque haya factor PPA.
- Flags `.f_*` de imputación:
  - Fuera del alcance. No construirlas para aparentar que hubo imputación; las
    flags originales de publicación siguen siendo metadatos para interpretar
    valores.

## Inventario de variables de hogares

### Identificación y demográficos

- `hi01`, `hi02`, `hi04`: `HB010`, `HB020`, `HB030` de H longitudinal; núcleo
  candidato.
- `hi03`: región `DB040` desde D longitudinal. Está documentada pero no tiene
  asignación en `calcular_hogares_()` actual; no confundir ese defecto con
  ausencia de datos.
- `hi07`: `DB100` de D, recodificado con `tabla_pi07`; ampliación con las
  mismas restricciones que `pi07`.
- `hi06`: la fórmula transversal usa `DB090`; para panel, D publica `DB095`, de
  significado longitudinal diferente. Incorporar el peso longitudinal y
  documentar su significado; no reemplazarlo por `DB090` transversal.
- `hd01`: `HX040` de H; es tamaño total del hogar, no el número de filas en P.
  Sirve de denominador per cápita cuando sea válido.
- `hd02a`, `hd02b`: siempre `NA` en el código actual. `HB110` es un insumo
  candidato de tipología, pero se necesita definir categorías y equivalencia
  con esas variables. IT lo suprime desde 2021; no forma parte de este mínimo.
- Bloque laboral de hogares: el código no implementa variables `hl*`; no hay un
  cálculo actual que auditar.
- Referencias: [L25], páginas 20–21, contenidos D/H; [G25], páginas 112–113,
  `DB095`; reglas IT de [L21]–[L25], página 9.

### Agregados de personas y perceptores

- Los `py00`, `py10`, `py11`, `py12`, `py20`–`py25` del nivel hogar son sumas
  de los valores personales correspondientes por país-año-hogar actual; forman
  parte del núcleo candidato condicionado a su cobertura.
- `py13`–`py15` agregados heredan las limitaciones del sector individual;
  quedan fuera del núcleo.
- `hp00`, `hp10`, `hp11`, `hp12`, `hp20`–`hp25` cuentan las personas cuyo
  importe correspondiente es distinto de cero. `hp13`–`hp15` dependen de las
  variables sectoriales.
- El criterio actual `y != 0` también cuenta pérdidas de trabajo independiente.
  Mantenerlo identificado como criterio del código, sin cambiar silenciosamente
  la definición a ingresos positivos.
- `agregar_personas()` usa `na.rm = FALSE`: un faltante puede volver faltante
  el agregado o el conteo. No cambiarlo a cero como solución a falta de
  insumos.
- Se necesita P que cubra los miembros pertinentes de cada hogar-año, no
  solamente las personas con peso positivo para una duración o trayectorias
  completas. H/R pueden aportar controles de población, pero no inventar
  ingresos de miembros ausentes. El resultado debe describirse como suma de las
  personas cubiertas cuando no esté acreditada la cobertura total.
- La selección actual `py00:py25` presupone el orden de todas las columnas,
  incluidas las sectoriales. Para un mínimo reducido deberán seleccionarse
  explícitamente las fuentes construidas; no ejecutar sin cambios el agregador
  completo.

### Ingresos propios del hogar y combinados

Definir, únicamente como notación de esta revisión, los componentes mensuales
en moneda nacional `m040 = HY040N * HX010 / 12`, y análogamente `m050`, `m060`,
`m070`, `m080`, `m090`, `m110`. Proceden de H longitudinal.

- `hy00`: suma personal `py00` más `m040 + m050 + m060 + m070 + m080 + m090 + m110`.
- `hy20`: suma personal `py20` más los mismos componentes del hogar.
- `hy21`: `m040 + m080 + m090 + m110`.
- `hy22`: `m040 + m090`.
- `hy23`: `m080 + m110`.
- `hy24`: sumas personales `py21 + py24 + py25` más `m050 + m060 + m070`.
- `hy25`: sumas personales `py24 + py25` más `m050 + m060 + m070`.
- `hy26`: `m050 + m060 + m070`.

Clasificación: construibles con H/P longitudinales, según presencia de netos,
conversión y cobertura de personas. `hy21/22/23/26` no necesitan agregados P
para su cálculo matemático, aunque la interfaz prevista de hogares siga
requiriendo P.

Estas son las descomposiciones que usa el paquete. No sustituir automáticamente
`hy00` por `HY020`: el ingreso disponible oficial tiene una definición contable
que no coincide necesariamente con esa suma de fuentes. Si faltan componentes,
un total oficial puede ser otra variable útil, pero no recupera su
desagregación exacta. R/D longitudinales o transversales no son una fuente
equivalente de esos componentes H.

Referencias: código de `calcular_hogares_()`; [L20], contenido H; [L25],
páginas 21–25, contenido H y primera página, regla monetaria.

### Variantes per cápita y PPA

- Sufijo `pc` de cada ingreso agregado o propio del hogar: dividir el importe
  por `HX040`; hereda sus faltantes y la cobertura de su numerador.
- Sufijo `ppa`: aplicar los factores de la tabla interna al ingreso en moneda
  nacional; limitado actualmente a 2016–2025.
- Sufijo `pcppa`: combinar ambas operaciones, sin cambiar el numerador o
  denominador. Las etiquetas lo prevén, pero el cálculo actual no convierte los
  ingresos `pc` a PPA.
- No repetir el error monetario actual: P ya entrega moneda nacional, mientras
  `calcular_hogares_()` agrega `HY* / 12` sin `HX010`. En PL puede mezclar
  zlotys y euros. La estandarización/cálculo futuro debe homogeneizar unidades
  antes de sumar, evitando aplicar la conversión dos veces a los agregados P.

## Excepciones nacionales que modifican la propuesta

- España:
  - `DB040` se difunde a NUTS 2, frente a la regla general NUTS 1. Conservar el
    detalle publicado sin presentarlo como el mismo nivel regional de todos los
    países.
  - Fuentes: [L14], página 4; [L16], página 7; [L17]–[L19], página 8; [L20],
    página 6; [L21]–[L25], página 7.
- Italia:
  - Educación agrupada en las entregas anteriores y posteriores a 2021; algunas
    recodificaciones destruyen diferencias usadas por la tabla educativa
    actual.
  - Desde 2021 se eliminan `RB081`, `RB082`, `RB280`, `RB290`, `RB032`, `HB110`
    y otros campos. La lista no elimina por nombre los añadidos
    `RX010/RX020/PX020`; considerarlos alternativas nominales que deben
    comprobarse en la entrega, no garantizar cobertura.
  - La integración de `PY120N` en otras fuentes aparece en la lógica y
    advertencias del paquete; confirmar su aplicación longitudinal antes de
    reutilizar la transformación.
  - La publicación oficial de 2024 incluye IT entre los países con paneles de
    seis años. Esto contradice un máximo universal de cuatro años para todos
    los países de interés; no se modifica aquí la API ni se impone ese límite.
  - Fuentes: [L14], página 6; [L16], página 8; [L17]–[L19], página 10; [L20],
    página 7; [L21]–[L25], página 9; [descripción oficial de los microdatos,
    publicación de
    2024](https://ec.europa.eu/eurostat/documents/203647/20298610/EUSILC_DOI_2024_release_1.pdf/6bd8e55d-47eb-3fde-cbdb-0440d3cc9ac7?t=1730712751843),
    página 1.
- Alemania:
  - L14/L15 informan que no se publican datos DE. L20 describe una ruptura de
    muestra/metodología y ausencia de esa entrega; L21/L22 describen la
    reconstrucción progresiva. No confundir ausencia de un país en una entrega
    con un insumo faltante recuperable.
  - L16–L19 perturban/codifican nacimiento, recodifican sexo, agrupan ocupación
    a un dígito y alteran componentes de ingreso; región/urbanización tienen
    limitaciones específicas. L19 cambia el tratamiento de edades superiores
    respecto de entregas anteriores.
  - L21/L22 unifican urbanización 1 y 2. L23–L25 agrupan nacimiento y suprimen
    edades calculadas, incluso `PX020` y `RX010/RX020`; no se puede prometer un
    rescate desde R de una edad que R también omite. L23–L25 suprimen región en
    hogares de tamaño 6.
  - Se suprimen hogares grandes; L25 añade supresión de hogares unipersonales
    de menores de 16/17 años según la edad al final del período de ingresos. No
    reconstruir esas observaciones ni pretender recuperar su tamaño/ingreso por
    traspaso.
  - Fuentes: [L14], página 4; [L15], página 3; [L16], páginas 4–6; [L17]–[L19],
    reglas DE desde la página 5; [L20], página 5; [L21]–[L22], páginas 5–7;
    [L23]–[L24], páginas 5–7; [L25], páginas 4–7.
- Polonia:
  - Educación no top-coded en las reglas longitudinales desde L16; no reducir
    su precisión sin una decisión explícita de armonización.
  - País fuera del euro: `PX010/HX010` importan para todas las sumas monetarias
    mixtas P/H. Las etiquetas deben corresponder a la unidad realmente
    calculada.
  - El registro existente de ausencia de `PL130` transversal en 2020 no rescata
    su falta longitudinal ni acredita disponibilidad en otra entrega.
  - Fuentes: [L16], página 9; [L17], página 11; [L18]–[L19], página 13; [L20],
    página 11; [L21]–[L23], página 12; [L24], página 14; [L25], página 13 y
    regla monetaria de la página 1.
- Portugal:
  - Región no difundida en L14–L17; NUTS 2 desde L18.
  - Nacimiento tiene codificación inferior específica; gestores ISCO se
    agrupan, con diferencias de regla entre L14/L15 y entregas posteriores.
  - Es posible conservar ocupación agrupada o construir el grupo principal,
    pero no recuperar automáticamente calificaciones basadas en el detalle
    eliminado.
  - Fuentes: [L14]–[L15], páginas 7–8; [L16], página 10; [L17], página 12;
    [L18], página 13; [L19], página 14; [L20], página 11; [L21]–[L22], páginas
    12–13; [L23], página 13; [L24], página 14; [L25], páginas 13–14.

Las reglas presentes en un PDF no prueban por sí mismas que el país esté
efectivamente incluido en cada entrega recibida. La cobertura nacional concreta
debe comprobarse antes de aplicar esta propuesta.

## Adaptaciones necesarias antes de reutilizar las funciones transversales

- Estandarizar por año de observación y por reglas de la entrega; los `if
  (.anio < 2021)` y las decisiones nacionales actuales no admiten directamente
  un vector de años.
- Distinguir columnas ausentes de valores faltantes/no aplicables. No crear
  columnas `NA` y luego afirmar que un indicador fue armonizado con cobertura
  completa.
- Traspasar desde D/R sólo las variables necesarias y mantener el año en todas
  las claves. Conservar por separado los pesos de cada duración.
- Permitir cálculos independientes del mínimo, sin exigir ni ejecutar todas las
  variables sectoriales, EGP y auxiliares del monolito actual.
- Corregir la interpretación de ISCO publicado a un dígito antes de aplicar
  `%/% 10` o las tablas de dos dígitos. Las entradas 1/2/3 de la tabla actual
  también pueden representar ocupaciones militares 01/02/03; no utilizarlas
  como si fueran universalmente grupos principales.
- Revisar el bloqueo global de `calc_variante_c()`, `calc_informalidad()` y
  otros auxiliares: `chequear_insumos_perdidos()` devuelve verdadero si
  cualquier insumo está enteramente ausente. En C, eso descarta una rama A
  conocida aunque B esté toda en `NA`.
- Homogeneizar moneda y período de referencia antes de sumar ingresos P/H. No
  transformar los netos en brutos o viceversa.
- Evitar selección por rangos de columnas de ingresos cuando se construya un
  subconjunto. Mantener la propagación de faltantes de las sumas sin
  imputación.
- Resolver la diferencia entre cálculo y salida: `hi03` no se asigna;
  `hd01/hd02a/hd02b` se calculan o reservan, pero no están en las etiquetas H y
  pueden eliminarse al seleccionar la salida; `pcppa` tiene etiquetas pero no
  cálculo actual.
- La mera presencia de etiquetas no acredita que exista un cálculo. `pd06`,
  `hd02a/b` y los bloques `hl*` pendientes no son variables recuperadas por
  panel.

## Conjunto mínimo recomendado

### Primera construcción, sin transversales ni imputación

- Personas:
  - Identificación: `pi01`, `pi02`, `pi04`, `pi05`.
  - Demográficos: `pd02`.
  - Ingresos: `py00`, `py10`, `py11`, `py12`, `py20`, `py21`, `py22`, `py23`,
    `py24`, `py25`, construidos independientemente según sus componentes netos.
  - Metadatos separados, si se suministran los auxiliares: ponderadores
    originales `RB062`–`RB066` aplicables y `DB075/DB076` con su cobertura
    temporal. No inventar aún una única `pi06`.
- Hogares:
  - Identificación: `hi01`, `hi02`, `hi04`.
  - Tamaño: `hd01`, conservándolo explícitamente en la salida futura.
  - Peso longitudinal de D: `DB095`; definir/documentar antes si se denominará `hi06`.
  - Agregados personales y perceptores de las fuentes no sectoriales
    anteriores, cuando P cubra los miembros pertinentes.
  - `hy00`, `hy20`, `hy21`, `hy22`, `hy23`, `hy24`, `hy25`, `hy26`, según sus
    componentes y agregados disponibles, después de homogeneizar moneda.
  - Versiones `pc` de los ingresos construidos con un denominador válido.

«Núcleo» describe prioridad y estabilidad de insumos, no cobertura completa
garantizada de todos los países, entregas o filas. Una fuente disponible puede
permitir su variable simple y no un total con otros componentes ausentes.

### Ampliaciones construibles sólo con panel, según cobertura

- `pi03/hi03`, `pi07/hi07` desde D.
- `pd01a/b/c` mediante edades difundidas o nacimiento, distinguiendo
  aproximaciones.
- `pd03`, `pd04`, `pd05`, `pl01`, `pl02*`, `pl10*`, `pl11*`, `pl12*`, `pl13*`
  cuando estén los insumos de cada ola con precisión suficiente.
- `pl40a/b`, `pomj`, `toc` y eventualmente `pl41`, cuando exista toda la
  información aplicable.
- `maa/man` desde agregados o calendario; después `haa/han` e ingresos horarios
  con su aproximación explícita.
- Variantes PPA para los años cubiertos por la tabla y los ingresos
  efectivamente construidos.
- Rama/tamaño/EGP únicamente en entregas antiguas que conserven los insumos,
  con documentación de la cobertura; no incorporarlas a la primera construcción
  general.

### Excluir del mínimo o posponer

- Sector público/privado, heterogeneidad, sector de inserción e
  ingresos/perceptores sectoriales en entregas recientes sin los insumos.
- EGP sin supervisión o con detalle ISCO insuficiente.
- Jefatura y tipos de hogar pendientes de definición.
- Una sola ponderación personal elegida automáticamente.
- Valores históricos recuperados por propagación entre olas, cualquier
  imputación y cualquier cruce transversal sin correspondencia comprobada.

## Verificación realizada y cuestiones abiertas

- Se comprobó en memoria que `tabla_ppa_` cubre 2016–2025 y que los países
  seleccionados son DE/ES/IT/PL/PT.
- Se inspeccionó la estructura de `etiquetas_` y `tabla_isco`; las diferencias
  de salida y granularidad anteriores se basan en el código/tablas actuales.
- En la planificación se comprobó con ejemplos sintéticos que un ISCO de un
  dígito dividido por diez produce un grupo incorrecto y que
  `calc_variante_c()` puede devolver `NA` global cuando B está completamente
  perdida.
- No se agregaron tests ni se ejecutaron cálculos sobre datos reales. Las
  fuentes nominales, los calendarios completos, el detalle ISCO y la cobertura
  de miembros deberán verificarse en las entregas efectivamente suministradas.
- Quedan abiertas la correspondencia transversal-longitudinal, la duración de
  ponderación que se expondrá en la API, el contrato de metadatos de entrega y
  la eventual ampliación de la tabla PPA. Ninguna de ellas impide iniciar el
  núcleo propuesto con cobertura declarada.

## Fuentes documentales

Los identificadores G y L siguientes remiten a archivos locales. Las secciones y páginas específicas se indican en el cuerpo. Se examinó también el contenido de los archivos C pertinentes para identificar fuentes transversales; su existencia no acredita un enlace con L.

- Guías G14–G20:
  - [G14], `Guidelines_Doc65_2014.pdf`: ponderadores, página 111; educación, 265; actividad, 282; meses, 294; contrato, 300; calendario, 308.
  - [G15], `Guidelines_Doc65_2015.pdf`: educación, 265; actividad, 285; meses, 297; contrato, 303; calendario, 311. La extracción emite un aviso sobre una colección de caracteres; no se usa ese aviso como evidencia de disponibilidad.
  - [G16], `Guidelines_Doc65_2016.pdf`: peso hogar, 109; educación, 263; actividad, 283; meses, 294; contrato, 300; calendario, 308.
  - [G17], `Guidelines_Doc65_2017.pdf`: fichas principales en las mismas páginas anteriores; las reglas L17 restringen su publicación.
  - [G18], `DOCSILC065 operation 2018_V5.pdf`: peso hogar, 109; educación, 263; actividad, 283; meses, 294; contrato, 300; calendario, 308.
  - [G19], `DOCSILC065 operation 2019_V9.pdf`: peso hogar, 111; educación, 265; actividad, 285; meses, 296; contrato, 302; calendario, 310.
  - [G20], `DOCSILC065 operation 2020_V5.pdf`: peso hogar, 111; educación, 268; actividad, 288; meses, 299; contrato, 305; calendario, 313.
- Guías G21–G25:
  - [G21], `Methodological guidelines 2021 operation v8.pdf`: rotación/ola, 107–108; peso hogar, 112; educación, 327; actividad, 358; meses, 370; contrato, 392; calendario, 399.
  - [G22], `Methodological guidelines 2022 operation v7.pdf`: rotación/ola, 105–106; peso hogar, 110; educación, 318; actividad, 349; meses, 361; contrato, 382; calendario, 388.
  - [G23], `Methodological guidelines 2023 operation_v6-accessibility.pdf`: ola, 106; peso hogar, 110; educación, 316; actividad, 346; meses, 358; contrato, 379; calendario, 385.
  - [G24], `Methodological guidelines 2024 operation_v7.pdf`: ola, 108; peso hogar, 112; educación, 327; actividad, 358; meses, 370; contrato, 391; calendario, 398.
  - [G25], `Methodological guidelines 2025 operation_updated.pdf`: ponderación, 71 y 81–83; rotación/ola, 107–108; peso hogar, 112; educación, 328; actividad, 359; meses, 371–378; contrato, 392; calendario, 399–422.
- Reglas longitudinales L14–L20:
  - [L14], `L-2014 DIFFERENCES BETWEEN DATA COLLECTED AND UDB.pdf`: reglas generales, 1–3; excepciones nacionales desde 3.
  - [L15], `L15_DIFFERENCES_COLLECTION_VS_UDB.pdf`: reglas generales, 1–3; excepciones desde 3.
  - [L16], `L16_DIFFERENCES_COLLECTION_VS_UDB.pdf`: reglas generales, 1–3; excepciones desde 3.
  - [L17], `L17_DIFFERENCES_COLLECTION_VS_UDB_release_19-03.pdf`: exclusiones, 2–3; excepciones desde 5.
  - [L18], `L18_DIFFERENCES_COLLECTION_VS_UDB_release_20-03.pdf`: exclusiones, 2–3; excepciones desde 5; calendario en contenido, 28.
  - [L19], `L19_DIFFERENCES_COLLECTION_VS_UDB_release_21-03.pdf`: exclusiones, 2–3; excepciones desde 5; calendario en contenido, 28.
  - [L20], `L20_DIFFERENCES_COLLECTION_VS_UDB_release_22-03.pdf`: exclusiones, 2–3; excepciones desde 4; contenido desde 18, P/calendario en 24–28.
- Reglas longitudinales L21–L25:
  - [L21], `L21_DIFFERENCES_COLLECTION_VS_UDB_release_23-03.pdf`: exclusiones, 2–3; excepciones desde 4; contenido desde 20.
  - [L22], `L22_DIFFERENCES_COLLECTION_VS_UDB_release_24-09.pdf`: exclusiones, 2–3; excepciones desde 4; contenido desde 20.
  - [L23], `L23_DIFFERENCES_COLLECTION_VS_UDB_release_24-09.pdf`: exclusiones, 2–3; excepciones desde 4; contenido desde 20.
  - [L24], `L24_DIFFERENCES_COLLECTION_VS_UDB_release_25-12.pdf`: exclusiones, 2–3; excepciones desde 4; contenido desde 22.
  - [L25], `L25_DIFFERENCES_COLLECTION_VS_UDB_release_26-06.pdf`: reglas generales, 1–4; excepciones desde 4; contenido desde 20.
- Fuentes transversales relevantes:
  - [C18], `C18_DIFFERENCES_COLLECTION_VS_UDB_release_20-03.pdf`, contenido de P: confirma los insumos excluidos de L18; no se usó la copia `_old`.
  - [C20], `C20_DIFFERENCES_COLLECTION_VS_UDB_release_21-09.pdf`, contenido de P y módulo: fuente potencial de calendario, rama, tamaño, supervisión y `PL230`.
  - [C21], `C21_DIFFERENCES_COLLECTION_VS_UDB_release_22-09.pdf`, contenido P/R: fuente potencial de variables actuales y contribuciones; no recupera por sí mismo las olas antiguas.
  - [C23], `C23_DIFFERENCES_COLLECTION_VS_UDB_release_24-09.pdf`, contenido de P/módulo: incluye `PL130/PL230` frente a su ausencia en L23.
  - [C25], `C25_DIFFERENCES_COLLECTION_VS_UDB_release_26-06.pdf`, contenido P/R/D: permite localizar las fuentes transversales actuales, sujetas a sus propias exclusiones.

[G14]: <../../.misc/documentacion/2014/Guidelines_Doc65_2014.pdf>
[G15]: <../../.misc/documentacion/2015/Guidelines_Doc65_2015.pdf>
[G16]: <../../.misc/documentacion/2016/Guidelines_Doc65_2016.pdf>
[G17]: <../../.misc/documentacion/2017/Guidelines_Doc65_2017.pdf>
[G18]: <../../.misc/documentacion/2018/DOCSILC065 operation 2018_V5.pdf>
[G19]: <../../.misc/documentacion/2019/DOCSILC065 operation 2019_V9.pdf>
[G20]: <../../.misc/documentacion/2020/DOCSILC065 operation 2020_V5.pdf>
[G21]: <../../.misc/documentacion/2021/Methodological guidelines 2021 operation v8.pdf>
[G22]: <../../.misc/documentacion/2022/Methodological guidelines 2022 operation v7.pdf>
[G23]: <../../.misc/documentacion/2023/Methodological guidelines 2023 operation_v6-accessibility.pdf>
[G24]: <../../.misc/documentacion/2024/Methodological guidelines 2024 operation_v7.pdf>
[G25]: <../../.misc/documentacion/2025/Methodological guidelines 2025 operation_updated.pdf>
[L14]: <../../.misc/documentacion/2014/L-2014 DIFFERENCES BETWEEN DATA COLLECTED AND UDB.pdf>
[L15]: <../../.misc/documentacion/2015/L15_DIFFERENCES_COLLECTION_VS_UDB.pdf>
[L16]: <../../.misc/documentacion/2016/L16_DIFFERENCES_COLLECTION_VS_UDB.pdf>
[L17]: <../../.misc/documentacion/2017/L17_DIFFERENCES_COLLECTION_VS_UDB_release_19-03.pdf>
[L18]: <../../.misc/documentacion/2018/L18_DIFFERENCES_COLLECTION_VS_UDB_release_20-03.pdf>
[L19]: <../../.misc/documentacion/2019/L19_DIFFERENCES_COLLECTION_VS_UDB_release_21-03.pdf>
[L20]: <../../.misc/documentacion/2020/L20_DIFFERENCES_COLLECTION_VS_UDB_release_22-03.pdf>
[L21]: <../../.misc/documentacion/2021/L21_DIFFERENCES_COLLECTION_VS_UDB_release_23-03.pdf>
[L22]: <../../.misc/documentacion/2022/L22_DIFFERENCES_COLLECTION_VS_UDB_release_24-09.pdf>
[L23]: <../../.misc/documentacion/2023/L23_DIFFERENCES_COLLECTION_VS_UDB_release_24-09.pdf>
[L24]: <../../.misc/documentacion/2024/L24_DIFFERENCES_COLLECTION_VS_UDB_release_25-12.pdf>
[L25]: <../../.misc/documentacion/2025/L25_DIFFERENCES_COLLECTION_VS_UDB_release_26-06.pdf>
[C18]: <../../.misc/documentacion/2018/C18_DIFFERENCES_COLLECTION_VS_UDB_release_20-03.pdf>
[C20]: <../../.misc/documentacion/2020/C20_DIFFERENCES_COLLECTION_VS_UDB_release_21-09.pdf>
[C21]: <../../.misc/documentacion/2021/C21_DIFFERENCES_COLLECTION_VS_UDB_release_22-09.pdf>
[C23]: <../../.misc/documentacion/2023/C23_DIFFERENCES_COLLECTION_VS_UDB_release_24-09.pdf>
[C25]: <../../.misc/documentacion/2025/C25_DIFFERENCES_COLLECTION_VS_UDB_release_26-06.pdf>
