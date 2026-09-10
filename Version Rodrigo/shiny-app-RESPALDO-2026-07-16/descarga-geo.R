########################################################################
## descarga-geo.R - CORRER UNA SOLA VEZ antes de usar/publicar la app ##
## Descarga los GeoJSON municipales (CONABIO 2022, via                ##
## github.com/PhantomInsights/mexico-geojson, licencia MIT),          ##
## simplifica los poligonos (menos peso, carga rapida) y los guarda   ##
## como .rds en la carpeta geo/ junto a app.R                         ##
##                                                                    ##
## Requiere: install.packages("sf")                                   ##
## Uso: setwd() a la carpeta shiny-app y source("descarga-geo.R")     ##
########################################################################

library(sf)

## nombres oficiales de los archivos en el repo fuente, en orden edon 1:32
geo.names <- c("Aguascalientes","Baja California","Baja California Sur",
  "Campeche","Coahuila de Zaragoza","Colima","Chiapas","Chihuahua",
  "Ciudad de México","Durango","Guanajuato","Guerrero","Hidalgo","Jalisco",
  "México","Michoacán de Ocampo","Morelos","Nayarit","Nuevo León","Oaxaca",
  "Puebla","Querétaro","Quintana Roo","San Luis Potosí","Sinaloa","Sonora",
  "Tabasco","Tamaulipas","Tlaxcala","Veracruz de Ignacio de la Llave",
  "Yucatán","Zacatecas")

base.url <- "https://raw.githubusercontent.com/PhantomInsights/mexico-geojson/main/2022/states/"
dir.create("geo", showWarnings = FALSE)

for (e in 1:32) {
    destino <- sprintf("geo/edon-%02d.rds", e)
    if (file.exists(destino)) { message("Ya existe: ", destino); next }
    message(sprintf("[%02d/32] %s ...", e, geo.names[e]))
    tmp <- tempfile(fileext = ".json")
    url <- paste0(base.url, utils::URLencode(paste0(geo.names[e], ".json")))
    tryCatch({
        download.file(url, tmp, mode = "wb", quiet = TRUE)
        g <- st_read(tmp, quiet = TRUE)
        g <- st_make_valid(g)
        ## simplificar: tolerancia ~200m; reduce el peso ~95%
        g <- st_simplify(g, dTolerance = 200, preserveTopology = TRUE)
        ## quedarnos solo con lo necesario
        g$inegi <- as.integer(g$CVEGEO)
        g <- g[, c("inegi", "NOMGEO")]
        saveRDS(g, destino)
        message("    OK: ", nrow(g), " municipios -> ", destino)
    }, error = function(err) {
        message("    FALLO en ", geo.names[e], ": ", conditionMessage(err))
    })
    unlink(tmp)
}

message("Listo. Archivos en geo/. Ya puedes correr la app.")
