########################################################################
## make-rds.R - CORRER UNA VEZ (y cada vez que se actualicen los CSV) ##
## Convierte los CSV tidy a .rds: la app arranca 10-30x mas rapido    ##
## porque readRDS no re-infiere tipos de columna.                     ##
## Uso: setwd() a la carpeta shiny-app y source("make-rds.R")         ##
########################################################################

for (base in c("aymu1970-on.tidy", "aymu1970-on.coalSplit.tidy")) {
    csv <- paste0(base, ".csv")
    rds <- paste0(base, ".rds")
    if (!file.exists(csv)) { message("No encontre ", csv, "; omitido."); next }
    message("Leyendo ", csv, " ...")
    d <- read.csv(csv, stringsAsFactors = FALSE)
    saveRDS(d, rds)
    message("  -> ", rds, " (", nrow(d), " filas)")
}

message("Listo. La app usara los .rds automaticamente.")
message("Sugerencia: al publicar, puedes mover los CSV fuera de la carpeta")
message("para que el paquete que sube rsconnect pese ~40 MB menos.")
