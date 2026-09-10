########################################################################
## tidy-mu-returns.r                                                  ##
## Convierte los retornos municipales de formato ancho (v01/l01/...)  ##
## a formato largo/tidy: una fila por candidatura-municipio-eleccion. ##
##                                                                    ##
## Entrada : data/aymu1970-on.coalAgg.csv (o coalSplit)               ##
## Salida  : CSV con columnas fijas:                                  ##
##   emm, yr, edon, inegi, ife, mun, date, status, cand_slot,         ##
##   partido, votos, share, rank, win, mg, efec, nr, nulos, tot,      ##
##   lisnom, ncand, ncoal, dextra, fuente                             ##
##                                                                    ##
## Uso:                                                               ##
##   source("tidy-mu-returns.r")                                      ##
##   tidy.mu(input  = "data/aymu1970-on.coalAgg.csv",                 ##
##           output = "data/tidy/aymu1970-on.tidy.csv")               ##
##                                                                    ##
## Autor: Rodrigo Santibanez, con base en elecRetrns de Eric Magar    ##
## Creado: 2-jul-2026                                                 ##
########################################################################

tidy.mu <- function(input, output = NA, verbose = TRUE) {
    if (!file.exists(input)) stop("No se encuentra el archivo de entrada: ", input)
    dat <- read.csv(input, stringsAsFactors = FALSE)
    ##
    ## columnas identificadoras que se conservan tal cual
    id.vars <- c("emm", "yr", "edon", "inegi", "ife", "mun", "date", "status",
                 "win", "mg", "efec", "nr", "nulos", "tot", "lisnom",
                 "ncand", "ncoal", "dextra", "fuente")
    id.vars <- id.vars[id.vars %in% colnames(dat)]  # tolera columnas ausentes
    ##
    ## pares de columnas voto/etiqueta (v01/l01 ... v23/l23)
    v.cols <- sort(grep("^v[0-9]{2}$", colnames(dat), value = TRUE))
    l.cols <- sort(grep("^l[0-9]{2}$", colnames(dat), value = TRUE))
    if (length(v.cols) != length(l.cols)) stop("Numero de columnas v y l no coincide")
    ##
    ## apila un data.frame por posicion de candidatura
    out <- vector("list", length(v.cols))
    for (k in seq_along(v.cols)) {
        label <- as.character(dat[[l.cols[k]]])
        votes <- suppressWarnings(as.numeric(dat[[v.cols[k]]]))
        ## posicion vacia = etiqueta "0", vacia o NA
        keep <- !is.na(label) & label != "0" & label != ""
        if (!any(keep)) next
        out[[k]] <- data.frame(dat[keep, id.vars, drop = FALSE],
                               cand_slot = k,
                               partido   = label[keep],
                               votos     = votes[keep],
                               stringsAsFactors = FALSE)
    }
    res <- do.call(rbind, out)
    ##
    ## share (proporcion del voto efectivo) y rank dentro de cada eleccion
    res$share <- ifelse(!is.na(res$efec) & res$efec > 0,
                        round(res$votos / res$efec, 4), NA)
    ## rank secuencial (1, 2, 3...): votos descendentes; los empates en
    ## primer lugar (existen ~21 casos) los decide la columna win de la
    ## fuente; ultimo desempate: orden de columna original (cand_slot)
    no.win <- as.integer(tolower(res$partido) != tolower(as.character(res$win)))
    res <- res[order(res$emm, -res$votos, no.win, res$cand_slot), ]
    res$rank <- ave(seq_along(res$emm), res$emm, FUN = seq_along)
    ##
    ## orden final: estado, municipio, año, votos descendentes
    res <- res[order(res$edon, res$inegi, res$yr, -res$votos), ]
    rownames(res) <- NULL
    ##
    ## ------------------- verificacion de integridad -------------------
    if (verbose) {
        ## 1. ninguna candidatura perdida: no-vacios en ancho == filas en largo
        n.wide <- sum(sapply(l.cols, function(cl) {
            x <- as.character(dat[[cl]]); sum(!is.na(x) & x != "0" & x != "")
        }))
        message("Candidaturas en formato ancho: ", n.wide,
                " | filas tidy: ", nrow(res),
                ifelse(n.wide == nrow(res), "  [OK]", "  [DISCREPANCIA!]"))
        ## 2. suma de votos por eleccion vs efec
        sums <- tapply(res$votos, res$emm, sum, na.rm = TRUE)
        efecs <- tapply(res$efec, res$emm, function(x) x[1])
        bad <- sum(abs(sums - efecs) > 0.5, na.rm = TRUE)
        message("Elecciones donde suma de votos != efec: ", bad, " de ",
                length(sums),
                " (discrepancias pequeñas pueden venir de la fuente)")
        message("Total filas: ", nrow(res),
                " | elecciones: ", length(unique(res$emm)),
                " | años: ", min(res$yr), "-", max(res$yr))
    }
    ##
    if (!is.na(output)) {
        dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
        write.csv(res, output, row.names = FALSE)
        if (verbose) message("Archivo escrito: ", output)
    }
    invisible(res)
}

## ---------------------------------------------------------------------
## Ejecucion directa (comentar si solo se quiere cargar la funcion):
## setwd("C:/Users/rodri/OneDrive - INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO/Escritorio/elecRetrns")
## tidy.mu(input  = "data/aymu1970-on.coalAgg.csv",
##         output = "Version Rodrigo/aymu1970-on.tidy.csv")
