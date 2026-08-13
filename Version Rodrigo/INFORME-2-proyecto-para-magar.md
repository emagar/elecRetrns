# Informe 2 — Qué se hizo con el repositorio elecRetrns

**Para:** explicar el proyecto al Dr. Eric Magar
**Autor del trabajo:** Rodrigo Santibáñez Razo (con asistencia de Claude, Anthropic)
**Fecha:** 2026-07-16
**Resultado visible:** https://elecrazo.shinyapps.io/elecretrns/

---

## 1. Motivación

El repositorio `elecRetrns` contiene datos electorales de enorme valor, pero
hasta ahora la única forma de explotarlos era mediante R: descargar los CSV,
entender el formato ancho (`v01/l01...`) y programar la extracción. Eso deja
fuera a periodistas, estudiantes y a cualquier investigador sin R. El
objetivo del proyecto fue abrir el acceso sin tocar el repositorio original.

## 2. Qué se construyó

### 2.1 Versión *tidy* de los retornos municipales

Se creó un script reproducible (`tidy-mu-returns.r`) que convierte
`aymu1970-on.coalAgg.csv` y `aymu1970-on.coalSplit.csv` al formato largo
estándar: **una fila por candidatura-municipio-elección**, con columnas
fijas (`estado, municipio, inegi, año, partido, votos, share, rank`, más los
identificadores del codebook). Ventajas: filtrable en Excel/Sheets sin
programar, compatible con tablas dinámicas, y legible por cualquier
herramienta (Python, SQL, asistentes de IA).

Volumen: 153,120 filas (coalAgg) y 180,280 (coalSplit), 35,641 elecciones,
1970–2025.

### 2.2 Aplicación web pública (Shiny)

Una app en español, desplegada en shinyapps.io, con:

- **Mapa municipal interactivo** por estado-año: partido ganador (colores
  partidistas) o margen de victoria por rangos. Geometrías del marco
  CONABIO 2022, simplificadas. Tooltips con ganador, share y margen.
- **Ficha histórica**: evolución del share de los principales partidos en
  cualquier municipio, 1970–2025.
- **Motor de extracción**: tabla filtrable con exportación directa a
  CSV/Excel/portapapeles.
- **Cómo citar**: la cita APA 7 del repositorio (tal como la pide el README)
  con botón de copiado — para fomentar la citación correcta.
- Switch para alternar entre coaliciones agregadas (coalAgg) y votos
  divididos por partido (coalSplit).

### 2.3 Control de calidad

La conversión se validó de punta a punta:

- **Cero candidaturas perdidas** (el conteo de posiciones no vacías del
  formato ancho coincide exactamente con las filas tidy).
- **Suma de votos = `efec`** en 35,640 de 35,641 elecciones.
- El ganador derivado (`rank == 1`) **coincide al 100%** con la columna
  `win` de la fuente.
- Los **empates exactos en primer lugar** (21 casos, municipios chicos) se
  resuelven con la columna `win` de la fuente.
- Las **dobles elecciones del mismo año** (82 pares: anulada+extraordinaria,
  típicamente Chiapas 2018; primera vuelta+balotaje) se desambiguan con una
  regla explícita y documentada: ok > extraordinaria > primera vuelta,
  nunca cancelada/appointed si hay alternativa.
- El código de la app fue sometido a una **auditoría externa** (20 hallazgos;
  los de severidad alta, corregidos).

### 2.4 Hallazgo para reportar a Eric

**Posible error en la fuente:** en `aymu1970-on.coalAgg.csv` (y coalSplit),
la elección **jal-18.118 — Yahualica de González Gallo, Jalisco, 2021**: la
suma de los votos por candidato da 11,177 pero `efec` registra 11,375
(diferencia de 198). Es el único caso en 35,641 elecciones donde no cuadra.

Menor: en la copia local se detectó también que la versión de
`code/extract-state-yr-mu-returns.r` en el repo asume la ruta de
`xport-function.r` remota; Rodrigo desarrolló una copia local mejorada de la
función (validación de inputs, diagnósticos) que está en su fork
(`RazoR-28/elecRetrns`, `code/xport-function.r`) por si es de interés para
un pull request.

## 3. Qué NO se tocó

El repositorio original permanece intacto: todo el trabajo vive en el fork
de Rodrigo y en la carpeta `Version Rodrigo/` (no versionada). Los datos
mostrados en la app se derivan de los CSV publicados en el repo con reglas
100% reproducibles.

## 4. Ideas de siguiente etapa (discutibles con Eric)

- Incorporar la versión tidy y/o el script al repositorio oficial.
- Extender la app a gobernadores, diputados federales y presidencial.
- Capa para asistentes de IA: codebook legible por máquina + archivo
  `llms.txt` en el repo (costo casi nulo, abre los datos a consultas en
  lenguaje natural).
- Columnas derivadas de interés general: participación (tot/lisnom),
  alternancia, número efectivo de partidos.
- Corrección de Yahualica 2021 en la fuente.
