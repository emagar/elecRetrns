# Informe 1 — Archivos creados en este proyecto

**Ubicación base:** `elecRetrns/Version Rodrigo/` (dentro de tu copia local del repositorio)
**Fecha de cierre:** 2026-07-16

---

## Raíz de `Version Rodrigo/`

### `tidy-mu-returns.r`
Script de R que convierte los retornos municipales del formato ancho original
(`v01/l01, v02/l02...` — una columna por posición de candidatura) al formato
largo o *tidy* (una fila por candidatura-municipio-elección). Contiene la
función `tidy.mu(input, output)` en base R puro, sin paquetes externos.

Detalles relevantes:
- Conserva las variables identificadoras (emm, yr, edon, inegi, mun, efec, lisnom, etc.).
- Calcula `share` (proporción del voto efectivo) y `rank` (lugar de la candidatura).
- Regla de rank actualizada tras la validación: secuencial 1,2,3…; los ~21
  empates exactos en primer lugar los decide la columna `win` de la fuente.
- Incluye verificación de integridad automática al correr (candidaturas
  perdidas, sumas contra `efec`).
- Es el script "fuente de la verdad": si Eric actualiza los CSV originales,
  se corre esto y se regeneran los tidy.

### `aymu1970-on.tidy.csv`
Resultado de la conversión: la versión tidy de `data/aymu1970-on.coalAgg.csv`
(coaliciones agregadas por candidatura). 153,120 filas, 35,641 elecciones,
1970–2025. Validado: cero candidaturas perdidas, suma de votos = `efec` en
35,640/35,641 elecciones (la excepción es un error de la fuente, ver Informe 2).

### `INFORME-1/2/3-*.md`
Estos tres documentos.

### `shiny-app-RESPALDO-2026-07-16/`
Copia de seguridad íntegra de la app **antes** de aplicar las correcciones de
la auditoría externa (el `app.R` respaldado tiene md5 `303efbc5...`, el mismo
que auditó el otro Claude). Si algo saliera mal, aquí está la versión que ya
funcionaba en producción.

---

## Carpeta `shiny-app/` (la aplicación desplegada)

**URL pública:** https://elecrazo.shinyapps.io/elecretrns/
**Cuenta:** elecrazo en shinyapps.io (plan gratuito, 25 hrs/mes)

### `app.R`
La aplicación Shiny completa (~370 líneas, un solo archivo). Interfaz en
español con `bslib::page_navbar` (tema flatly). Cuatro pestañas:

1. **Explorador espacial** — mapa municipal interactivo (plotly
   `plot_mapbox`, fondo carto-positron, zoom/paneo/tooltips). Métricas:
   partido ganador (colores oficiales de partido) o margen de victoria
   (rangos 0–5/5–10/10–20/20–40/40+ pts).
2. **Ficha histórica** — serie de tiempo plotly del share de los 6
   principales partidos en cualquier municipio.
3. **Motor de extracción** — tabla DT con botones Copiar / CSV / Excel que
   exportan el filtro completo (estado + año).
4. **Cómo citar** — citas en APA 7 del dataset y de la nota metodológica,
   con botón de copiado al portapapeles.

Controles globales en la barra lateral: estado, año (reactivo al estado),
métrica del mapa, y switch coaliciones agregadas vs partidos individuales.

Incorpora todas las correcciones de la auditoría (race conditions con
`freezeReactiveValue`, desambiguación explícita de dobles elecciones,
`bindCache`, carga desde `.rds`, listas precomputadas, etc.).

### `aymu1970-on.tidy.csv` y `aymu1970-on.coalSplit.tidy.csv`
Los dos datasets que alimentan la app. El segundo es la versión tidy de
`coalSplit` (votos de coalición divididos por partido; 180,280 filas). Como
la fuente coalSplit no trae ganador ni margen, se calcularon desde los votos
y se validaron contra coalAgg (coincidencia 100%).

### `aymu1970-on.tidy.rds` y `aymu1970-on.coalSplit.tidy.rds` (si ya corriste make-rds.R)
Versiones binarias de los CSV anteriores; la app las prefiere porque cargan
10–30× más rápido. Se regeneran con `make-rds.R` cada vez que cambien los CSV.

### `descarga-geo.R`
Script de una sola corrida: descarga los GeoJSON municipales de los 32
estados (marco CONABIO 2022, vía el repo MIT `PhantomInsights/mexico-geojson`),
los simplifica con `sf::st_simplify` (tolerancia 200 m, ~95% menos peso),
extrae solo `inegi` (entero, cruza con los datos) y `NOMGEO` (nombre oficial
con acentos), y guarda `geo/edon-01.rds` … `geo/edon-32.rds`.

### `make-rds.R`
Convierte los CSV tidy a `.rds`. Correr tras cada actualización de datos.

### `geo/` (32 archivos `edon-XX.rds`)
Geometrías municipales simplificadas, una por estado, listas para `sf`.

### `rsconnect/`
Metadatos del despliegue a shinyapps.io (los genera rsconnect; no tocar).

---

## Fuera de `Version Rodrigo`

### `code/xport-function.r` (en tu fork de GitHub)
Tu versión mejorada de la función `xport()` de Eric (validación de inputs,
abreviaturas de estado, mensajes de diagnóstico cruzando
`resumen-haber-datos`, corrección de bugs). Committeada y pusheada a
`github.com/RazoR-28/elecRetrns` (rama master).

---

## Advertencia operativa

Los archivos de `Version Rodrigo/` **solo existen en tu OneDrive**; no están
en git. Recomendación pendiente: `git add "Version Rodrigo"` → commit → push
al fork para tener respaldo versionado. Además, recuerda que git y OneDrive
se llevan mal (locks, ruido de fines de línea) — la recomendación de fondo
sigue siendo mover el repo a una carpeta fuera de OneDrive.
