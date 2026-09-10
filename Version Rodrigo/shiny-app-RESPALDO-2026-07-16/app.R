########################################################################
## app.R - Explorador de resultados electorales municipales (Mexico) ##
## Datos: elecRetrns (Eric Magar, ITAM) - version tidy                ##
## Geometrias: CONABIO 2022 via PhantomInsights/mexico-geojson (MIT)  ##
##                                                                    ##
## ANTES DE CORRER POR PRIMERA VEZ:                                   ##
##   1. install.packages(c("shiny","bslib","ggplot2","sf",            ##
##                          "plotly","DT"))                           ##
##   2. source("descarga-geo.R")   # descarga los mapas una sola vez  ##
##                                                                    ##
## Correr:    shiny::runApp()                                         ##
## Publicar:  rsconnect::deployApp()                                  ##
##                                                                    ##
## Nota: el mapa usa plotly+sf (no leaflet) para evitar la cadena de  ##
## dependencias raster/terra que no compila en shinyapps.io           ##
########################################################################

library(shiny)
library(bslib)
library(sf)
library(plotly)
library(DT)

## plotly exige un token de Mapbox aunque el estilo carto-positron es
## libre y no lo usa; un token ficticio satisface la validacion
Sys.setenv(MAPBOX_TOKEN = "pk.token-no-necesario")

## ====================== 1. DATOS =====================================
## Dos versiones tidy: coaliciones agregadas (coalAgg) y votos
## divididos por partido individual (coalSplit)
d.coal  <- read.csv("aymu1970-on.tidy.csv",           stringsAsFactors = FALSE)
d.split <- read.csv("aymu1970-on.coalSplit.tidy.csv", stringsAsFactors = FALSE)

estados <- c("Aguascalientes","Baja California","Baja California Sur",
  "Campeche","Coahuila","Colima","Chiapas","Chihuahua","Ciudad de México",
  "Durango","Guanajuato","Guerrero","Hidalgo","Jalisco","México",
  "Michoacán","Morelos","Nayarit","Nuevo León","Oaxaca","Puebla",
  "Querétaro","Quintana Roo","San Luis Potosí","Sinaloa","Sonora",
  "Tabasco","Tamaulipas","Tlaxcala","Veracruz","Yucatán","Zacatecas")
d.coal$estado  <- estados[d.coal$edon]
d.split$estado <- estados[d.split$edon]

## ====================== 2. COLORES DE PARTIDO ========================
## Unica excepcion permitida de colores fijos: identidad de partidos.
## Las coaliciones toman el color del primer partido de la etiqueta.
pal.partido <- c(
  pan    = "#0059B3", pri    = "#E31B23", prd    = "#FFD400",
  morena = "#8B2231", pvem   = "#4CA33F", pt     = "#D52B1E",
  mc     = "#F58025", pna    = "#00B2A9", panal  = "#00B2A9",
  pes    = "#5C3A92", conve  = "#F58025", pfcrn  = "#B5121B",
  parm   = "#006341", pps    = "#C8102E", pdm    = "#5B2A86",
  psum   = "#C8102E", pst    = "#9E1B32", fxm    = "#E5006D",
  rsp    = "#4B4B4B", pla    = "#7BAFD4", indep  = "#808080")

color.partido <- function(etq) {
    ## primer partido de la etiqueta de coalicion (ej. "pri-pvem-pna" -> "pri")
    primero <- sapply(strsplit(tolower(etq), "-"), `[`, 1)
    primero[grepl("^indep", primero)] <- "indep"
    out <- pal.partido[primero]
    out[is.na(out)] <- "#B0B0B0"   # partidos chicos sin color asignado
    unname(out)
}

## ====================== 3. GEOMETRIAS ================================
## Cache en memoria: cada estado se lee de geo/edon-XX.rds una vez
.geo.cache <- new.env()
lee.geo <- function(edon) {
    key <- sprintf("e%02d", edon)
    if (!is.null(.geo.cache[[key]])) return(.geo.cache[[key]])
    f <- sprintf("geo/edon-%02d.rds", edon)
    if (!file.exists(f)) return(NULL)
    g <- readRDS(f)
    .geo.cache[[key]] <- g
    g
}

## ====================== 4. INTERFAZ ==================================
ui <- page_navbar(
  title = "Elecciones municipales de México, 1970–2025",
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  ## ---- barra lateral: controles globales ----
  sidebar = sidebar(
    width = 300,
    selectInput("estado", "Estado:", choices = estados,
                selected = "Aguascalientes"),
    selectInput("anio", "Año electoral:", choices = NULL),
    selectInput("metrica", "Métrica del mapa:",
                choices = c("Partido ganador" = "win",
                            "Margen de victoria" = "mg")),
    input_switch("coal", "Agrupar por coaliciones", value = TRUE),
    helpText("Con el switch apagado se muestran los votos divididos por ",
             "partido individual (coalSplit), donde la fuente lo permite."),
    hr(),
    p(class = "small text-muted",
      "Fuente: Eric Magar (2018), Recent Mexican election vote returns, ",
      a("github.com/emagar/elecRetrns",
        href = "https://github.com/emagar/elecRetrns", target = "_blank"))
  ),

  ## ---- pestaña 1: mapa ----
  nav_panel("Explorador espacial",
    card(full_screen = TRUE,
      card_header(textOutput("titulo_mapa")),
      plotlyOutput("mapa", height = 600))
  ),

  ## ---- pestaña 2: serie de tiempo ----
  nav_panel("Ficha histórica",
    card(full_screen = TRUE,
      card_header("Evolución del voto por municipio"),
      selectInput("municipio", "Municipio:", choices = NULL, width = "50%"),
      plotlyOutput("serie", height = 500))
  ),

  ## ---- pestaña 3: tabla ----
  nav_panel("Motor de extracción",
    card(full_screen = TRUE,
      card_header("Datos filtrados (estado y año seleccionados)"),
      DTOutput("tabla"))
  ),

  ## ---- pestaña 4: citas ----
  nav_panel("Cómo citar",
    card(
      card_header("Cita de los datos (APA 7)"),
      p("Si usas estos datos en un trabajo académico o periodístico, ",
        "cita la fuente original:"),
      tags$blockquote(id = "cita-datos", class = "border-start ps-3",
        "Magar, E. (2018). ", tags$em("Recent Mexican election vote returns
        repository"), " [Conjunto de datos]. GitHub.
        https://github.com/emagar/elecReturns"),
      tags$button(class = "btn btn-outline-primary btn-sm",
        onclick = "navigator.clipboard.writeText(
          document.getElementById('cita-datos').innerText.replace(/\\s+/g,' ').trim()
        ).then(() => { this.innerText = '¡Copiada!';
          setTimeout(() => this.innerText = 'Copiar cita', 1500); })",
        "Copiar cita")
    ),
    card(
      card_header("Nota metodológica de los datos municipales"),
      tags$blockquote(id = "cita-nota", class = "border-start ps-3",
        "Magar, E. (2025). ", tags$em("Deep description of the municipal
        election returns dataset"), " [Nota de investigación].
        https://github.com/emagar/elecRetrns/tree/master/papers/data-descriptives"),
      tags$button(class = "btn btn-outline-primary btn-sm",
        onclick = "navigator.clipboard.writeText(
          document.getElementById('cita-nota').innerText.replace(/\\s+/g,' ').trim()
        ).then(() => { this.innerText = '¡Copiada!';
          setTimeout(() => this.innerText = 'Copiar cita', 1500); })",
        "Copiar cita")
    ),
    card(
      card_header("Esta aplicación"),
      p("Explorador desarrollado por Rodrigo Santibáñez sobre los datos de
         elecRetrns. Los datos mostrados corresponden a la versión tidy de los
         retornos municipales (aymu1970-on), con coaliciones agregadas o
         divididas según el interruptor de la barra lateral.")
    )
  )
)

## ====================== 5. SERVIDOR ==================================
server <- function(input, output, session) {

  ## dataset activo segun el switch de coaliciones
  datos <- reactive(if (isTRUE(input$coal)) d.coal else d.split)

  ## --- años disponibles segun estado (dropdown reactivo) -------------
  observeEvent(list(input$estado, input$coal), {
    yrs <- sort(unique(datos()$yr[datos()$estado == input$estado]),
                decreasing = TRUE)
    sel <- if (!is.null(input$anio) && input$anio %in% yrs) input$anio else yrs[1]
    updateSelectInput(session, "anio", choices = yrs, selected = sel)
  })

  ## --- municipios del estado (para la ficha historica) ---------------
  observeEvent(input$estado, {
    d <- datos()[datos()$estado == input$estado, ]
    ## un municipio = un codigo inegi; etiqueta = nombre mas reciente
    d <- d[order(d$inegi, -d$yr), ]
    m <- d[!duplicated(d$inegi), c("inegi", "mun")]
    updateSelectInput(session, "municipio",
                      choices = setNames(m$inegi, m$mun))
  })

  ## --- datos del estado-año elegido, un renglon por eleccion ---------
  ganadores <- reactive({
    req(input$anio)
    d <- datos()
    d <- d[d$estado == input$estado & d$yr == as.integer(input$anio) &
           d$rank == 1, ]
    d[!duplicated(d$inegi), c("inegi", "mun", "partido", "share", "mg", "efec")]
  })

  output$titulo_mapa <- renderText({
    req(input$anio)
    sprintf("%s, %s — %s", input$estado, input$anio,
            ifelse(input$metrica == "win", "partido ganador",
                   "margen de victoria"))
  })

  ## --- pestaña 1: mapa plotly sobre mapbox (soporte sf nativo) --------
  output$mapa <- renderPlotly({
    g <- lee.geo(match(input$estado, estados))
    shiny::validate(shiny::need(!is.null(g),
      "Faltan los archivos de mapa. Corre una vez descarga-geo.R en la carpeta de la app."))
    w <- ganadores()
    g <- merge(g, w, by = "inegi", all.x = TRUE)   # conserva mpios sin datos
    g$etiq <- sprintf(
      "%s\nGanador: %s\nShare: %s | Margen: %s",
      g$NOMGEO,
      ifelse(is.na(g$partido), "sin datos", g$partido),
      ifelse(is.na(g$share), "—", sprintf("%.1f%%", 100 * g$share)),
      ifelse(is.na(g$mg),    "—", sprintf("%.1f pts", 100 * g$mg)))
    ##
    ## grupo a colorear: partido ganador o margen en rangos
    if (input$metrica == "win") {
        g$grupo <- ifelse(is.na(g$partido), "sin datos", g$partido)
        niveles <- setdiff(unique(g$grupo), "sin datos")
        pal <- setNames(color.partido(niveles), niveles)
    } else {
        g$grupo <- as.character(cut(100 * g$mg,
                     breaks = c(0, 5, 10, 20, 40, 100), include.lowest = TRUE,
                     labels = c("0–5 pts", "5–10 pts", "10–20 pts",
                                "20–40 pts", "40+ pts")))
        g$grupo[is.na(g$grupo)] <- "sin datos"
        pal <- setNames(RColorBrewer::brewer.pal(5, "YlOrRd"),
                        c("0–5 pts", "5–10 pts", "10–20 pts",
                          "20–40 pts", "40+ pts"))
    }
    pal["sin datos"] <- "#E8E8E8"
    ##
    ## centro y zoom para encuadrar el estado completo
    bb <- st_bbox(g)
    cx <- mean(bb[c("xmin", "xmax")]); cy <- mean(bb[c("ymin", "ymax")])
    dx <- max(bb["xmax"] - bb["xmin"], (bb["ymax"] - bb["ymin"]) * 1.4)
    zoom <- max(4.3, min(9, log2(360 / dx) - 0.4))
    ##
    plot_mapbox(g, split = ~grupo, color = ~grupo, colors = pal,
                text = ~etiq, hoverinfo = "text", hoveron = "fills",
                stroke = I("white"), span = I(1), alpha = 0.75) |>
      layout(mapbox = list(style = "carto-positron",
                           center = list(lon = cx, lat = cy), zoom = zoom),
             legend = list(orientation = "h", y = 0),
             margin = list(l = 0, r = 0, t = 0, b = 0)) |>
      config(scrollZoom = TRUE)
  })

  ## --- pestaña 2: serie de tiempo plotly ------------------------------
  output$serie <- renderPlotly({
    req(input$municipio)
    d <- datos()
    d <- d[d$inegi == as.integer(input$municipio), ]
    shiny::validate(shiny::need(nrow(d) > 0, "Sin datos para este municipio."))
    ## partidos relevantes: top 6 por votos acumulados en el municipio
    top <- names(sort(tapply(d$votos, d$partido, sum, na.rm = TRUE),
                      decreasing = TRUE))[1:min(6, length(unique(d$partido)))]
    d <- d[d$partido %in% top, ]
    cols <- setNames(color.partido(top), top)
    plot_ly(d, x = ~yr, y = ~share, color = ~partido, colors = cols,
            type = "scatter", mode = "lines+markers",
            hovertemplate = "%{y:.1%} en %{x}<extra>%{fullData.name}</extra>") |>
      layout(title = list(text = d$mun[1], x = 0.05),
             xaxis = list(title = "Año"),
             yaxis = list(title = "Share del voto efectivo",
                          tickformat = ".0%", rangemode = "tozero"),
             legend = list(orientation = "h", y = -0.2),
             hovermode = "x unified")
  })

  ## --- pestaña 3: tabla DT con botones de exportacion -----------------
  output$tabla <- renderDT({
    req(input$anio)
    d <- datos()
    d <- d[d$estado == input$estado & d$yr == as.integer(input$anio), ]
    cols <- c("mun", "partido", "votos", "share", "rank", "win", "mg",
              "efec", "lisnom", "status")
    cols <- cols[cols %in% colnames(d)]
    datatable(d[, cols],
      rownames = FALSE, extensions = "Buttons",
      colnames = c("Municipio", "Partido", "Votos", "Share", "Lugar",
                   "Ganador", "Margen", "Voto efec.", "Lista nom.",
                   "Estatus")[seq_along(cols)],
      options = list(
        dom = "Bfrtip",
        buttons = list(
          list(extend = "copy",  text = "Copiar"),
          list(extend = "csv",   text = "Descargar CSV",
               filename = paste0("elecRetrns-", input$estado, "-", input$anio)),
          list(extend = "excel", text = "Descargar Excel",
               filename = paste0("elecRetrns-", input$estado, "-", input$anio))),
        pageLength = 25, scrollX = TRUE,
        language = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/es-MX.json"))) |>
      formatPercentage(intersect(c("share", "mg"), cols), 1) |>
      formatRound(intersect(c("votos", "efec", "lisnom"), cols), 0, mark = ",")
  }, server = FALSE)   # server=FALSE: los botones exportan TODO lo filtrado
}

shinyApp(ui, server)
