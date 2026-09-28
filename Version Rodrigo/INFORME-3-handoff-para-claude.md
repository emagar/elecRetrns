# Handoff — Continuación del proyecto elecRetrns

**Audiencia:** el próximo modelo Claude (Opus / Claude Code) que trabaje con Rodrigo.
**Fecha de corte:** 2026-07-16. Léelo completo antes de tocar nada.

---

## 1. Contexto en 60 segundos

Rodrigo Santibáñez trabaja con el repositorio de datos electorales mexicanos
de Eric Magar (ITAM): `github.com/emagar/elecRetrns` (retornos municipales,
gubernaturas, diputados, presidencial; ver su README, que es excelente).
Rodrigo aparece en los agradecimientos del repo y tiene fork propio:
`github.com/RazoR-28/elecRetrns` (remote `origin` de su copia local).

Objetivo del proyecto: hacer los datos usables sin R. Lo logrado: versión
tidy de los datos municipales + app Shiny pública en
**https://elecrazo.shinyapps.io/elecretrns/** (cuenta shinyapps: `elecrazo`).

## 2. Mapa de archivos

Copia local del repo (¡vive en OneDrive, ver §6!):
`C:\Users\rodri\OneDrive - INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO\Escritorio\elecRetrns`

Todo el trabajo nuevo está en `Version Rodrigo/`:

- `tidy-mu-returns.r` — conversión ancho→tidy (fuente de la verdad).
- `aymu1970-on.tidy.csv` — tidy coalAgg (153,120 filas).
- `shiny-app/` — la app: `app.R`, `descarga-geo.R`, `make-rds.R`, los dos
  CSV tidy (+ `.rds`), `geo/edon-01..32.rds`, `rsconnect/`.
- `shiny-app-RESPALDO-2026-07-16/` — respaldo pre-auditoría (no tocar).
- `Stress Test/`, `coalAgg_vs_coalSplit...` — pruebas propias de Rodrigo.
- `INFORME-1/2/3-*.md` — documentación de cierre.

## 3. Decisiones de datos que NO debes deshacer sin razón

1. **Formato tidy:** una fila por candidatura-elección; `share` = votos/efec
   (escala 0–1, verificado); `rank` secuencial 1..n por elección.
2. **Empates en 1er lugar** (21 casos): los decide la columna `win` de la
   fuente. Verificado: rank==1 coincide con `win` al 100% en ambos datasets.
3. **Dobles elecciones inegi-año** (82 pares): la app las desambigua con
   `prio.status()`: ok/new=1 > extra/new--extra=2 > ok--to-runoff=3 >
   resto=5 > cancelled/appointed=9. Razón: mostrar el resultado que valió
   (p.ej. Chiapas 2018: extraordinaria sobre anulada; balotaje sobre 1a vuelta).
4. **coalSplit no trae `win`/`mg` en la fuente**; se calcularon desde los
   votos al generar el tidy.
5. **Anomalía conocida de la fuente:** jal-18.118 (Yahualica, Jalisco 2021),
   suma de votos 11,177 ≠ efec 11,375. Único caso. Pendiente reportar a Eric.
6. **Colores de coalición** = color del primer partido de la etiqueta.
   Decisión deliberada y documentada en app.R; no es bug.

## 4. Arquitectura de la app (app.R, un solo archivo)

- `bslib::page_navbar` + sidebar global (estado, año reactivo, métrica,
  switch coalAgg/coalSplit). 4 nav_panels: mapa, serie, tabla DT, citas APA.
- **Mapa: `plotly::plot_mapbox` con sf nativo.** NO ES NEGOCIABLE A LA
  LIGERA: la app usaba leaflet y era mejor UX, pero leaflet 2.2.3 tiene a
  `raster` en Imports → arrastra `terra` → **terra no compila en
  shinyapps.io** (se intentó todo: actualizar, borrar, pinear versiones).
  Si algún día leaflet suelta a raster, reconsiderar.
- `Sys.setenv(MAPBOX_TOKEN = "pk.token-no-necesario")` al inicio: plotly
  exige token aunque carto-positron no lo usa. No quitar.
- **No hacer `library(jsonlite)`**: su `validate()` pisa a `shiny::validate`
  (bug ya sufrido). Por eso los `shiny::validate(shiny::need(...))`
  explícitos.
- Observers de año/municipio usan `freezeReactiveValue` (race condition
  F-01 de la auditoría; no quitar).
- `bindCache(input$estado, input$anio, input$metrica, input$coal)` en el
  mapa. Requiere htmlwidgets ≥1.5.3 (Rodrigo tiene 1.6.4).
- Datos: `lee.datos()` prefiere `.rds` y cae a CSV. Tras cambiar un CSV,
  SIEMPRE regenerar con `make-rds.R` (los rds viejos ganan silenciosamente).
- Geometrías: `lee.geo(edon)` con caché en env global, `geo/edon-XX.rds`
  creados por `descarga-geo.R` (CONABIO 2022 vía PhantomInsights/
  mexico-geojson, MIT; simplificadas st_simplify 200m; columnas `inegi`
  entero + `NOMGEO`). Cruce con datos por `inegi` = as.integer(CVEGEO).
- Tabla: `renderDT(server = FALSE)` deliberado — los botones Buttons
  exportan el filtro completo. Volumen acotado; no cambiar sin motivo.

Hubo una **auditoría externa** (archivo `diagnostico-app-elecretrns.md` que
Rodrigo puede reenviar): 20 hallazgos F-01..F-20. Aplicado: todos los P0,
F-04 con la regla de §3.3, F-08/09/10/11 (perf), F-12/16/17/19 y parte de
F-20. Abierto deliberadamente: F-13 (colores coalición, documentado),
F-14 (token, sin plan B), F-15 (solo aviso en UI), F-20a (height fijo).

## 5. Despliegue (ritual completo, en orden)

```r
## en la misma sesión, sin reiniciar entre medias:
options(rsconnect.packrat = TRUE)   # SIN esto, renv truena con "pre-flight validation"
rsconnect::deployApp(
  appDir  = "C:/Users/rodri/OneDrive - INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO/Escritorio/elecRetrns/Version Rodrigo/shiny-app",
  appName = "elecretrns",
  forceUpdate = TRUE)
```

Gotchas de despliegue ya sufridos (no repetir el viacrucis):
- rsconnect arma el manifiesto desde la librería LOCAL de Rodrigo: paquete
  no instalado localmente = no llega al servidor ("shiny not found").
- El error renv "Failed to snapshot dependencies" se evita con la opción
  packrat de arriba (dura solo la sesión).
- Si aparece "Error building terra": algo volvió a meter leaflet/raster a
  la cadena. Revisar `rsconnect::appDependencies(appDir)`.
- R de Rodrigo: 4.5.2. Paquetes de la app: shiny, bslib, sf, plotly, DT,
  RColorBrewer (transitivo de plotly, declarado igualmente).

## 6. Entorno de Rodrigo (importante para no romper nada)

- **Nivel técnico:** cómodo con R; git y terminal con acompañamiento paso a
  paso (darle comandos exactos y pedirle la salida). Responde bien a
  explicaciones "en cristiano". Prefiere que le preguntes antes de
  desarrollar respuestas largas (está en sus preferencias).
- **El repo vive en OneDrive** y eso causó: index.lock atorado, ruido de
  CRLF en 557 archivos, un rebase que dejó conflictos con marcadores
  `<<<<<<<` en `data/aymu1970-on.coalAgg.csv` local. Estado al cierre:
  se le dieron los comandos `git rebase --abort` + `git reset --hard
  origin/master`; **verificar con `git status` si los corrió**. Los datos
  limpios siempre pueden extraerse con `git show HEAD:data/archivo.csv`.
  Recomendación permanente: mover el repo fuera de OneDrive.
- Fork: `RazoR-28/elecRetrns` = remote `origin`. El repo de Magar NO está
  como remote (agregar `upstream` si se necesita sincronizar).
- Ya pusheado al fork: `code/xport-function.r` (su versión mejorada del
  xport de Eric). `Version Rodrigo/` NO está versionado — commit pendiente.

## 7. Hoja de ruta pendiente (en orden sugerido)

1. **Commit de `Version Rodrigo/` al fork** (respaldo versionado).
2. **Fase 3 — capa IA sobre el repo:** codebook legible por máquina
   (CSV: variable, descripción, tipo, archivos) + `llms.txt` en la raíz del
   fork con la convención de nombres (aymu/dfdf/goed/pred + coalAgg/
   coalSplit) y rutas raw. Costo bajo, alto valor. Opcional: Datasette
   (csvs-to-sqlite + datasette publish) para API JSON y MCP
   (`datasette-mcp`) — se discutió y quedó como complemento, no reemplazo.
3. **Mejoras a la app** (de una crítica de fondo que se hizo; por impacto):
   columnas calculadas de participación (tot/lisnom) y alternancia; perfil
   por municipio (línea de tiempo de ganadores + incumbents con nombre,
   usando `aymu1989-on.incumbents.csv`); gráfica estatal ponderada (suma de
   votos / suma de efec, no promedio simple de shares); glosario de siglas
   de partido; URLs compartibles (query params); más cargos (goed, dfdf, pred).
4. **Reportar a Eric:** Yahualica 2021 + ofrecer el tidy/script y el
   xport-function mejorado como PRs.
5. **Escalar tidy** a incumbents / gubernaturas / diputados si se extiende
   la app.

## 8. Verificación rápida del tidy (para re-correr tras cualquier cambio)

Con pandas o R, sobre ambos tidy: (a) filas == posiciones no vacías del
ancho; (b) por emm: |suma(votos) − efec| ≤ 1 salvo jal-18.118; (c) cada emm
tiene exactamente un rank==1 y coincide con `win`; (d) share y mg en 0–1;
(e) sin NAs en votos/share/efec. Al cierre, todo esto pasa.
