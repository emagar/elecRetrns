rm(list=ls())
options(width = 120)

# read from clipboard
d <- read.table(pipe("xclip -selection clipboard -o", open="r"), sep = "\t", header = TRUE)
d[is.na(d)] <- 0
head(d)
d[1,]
colnames(d)

d2 <- d # duplicate
d <- d2

# clean
sel <- which(colnames(d) %in% c("SECCION", "ID_CASILLA", "TIPO_CASILLA", "EXT_CONTIGUA", "CASILLA", "ID_TIPO_CANDIDATURA", "NUM_ACTA_IMPRESO","CASILLA_INSTALADA","ESTATUS_PAQUETE", "ID_INCIDENTE", "NUMERO_VOTOS_VALIDOS", "TOTAL_VOTOS_ACTA", "TOTAL_VOTOS"))
d <- d[,-sel]

# turn characters into NAs
sel <- c(1)
tmp <- d[,-sel]
tmp <- lapply(tmp, as.numeric)
tmp <- as.data.frame(do.call(cbind,tmp))
tmp[is.na(tmp)] <- 0
head(tmp)
d[,-sel] <- tmp

d[1,]
colnames(d)

d <- within(d, expr = {
    pan <- ave(pan, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    pri <- ave(pri, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    prd <- ave(prd, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    pvem <- ave(pvem, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    pt <- ave(pt, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    mc <- ave(mc, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    pup <- ave(pup, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    pna <- ave(pna, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    morena <- ave(morena, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    pes <- ave(pes, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    psd <- ave(psd, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    pmr <- ave(pmr, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    pan.pri.mc <- ave(pan.pri.mc, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    pri.pvem.pna <- ave(pri.pvem.pna, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    pt.morena.pes <- ave(pt.morena.pes, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    prd.mc.pup <- ave(prd.mc.pup, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    pri.pvem <- ave(pri.pvem, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    pri.pna <- ave(pri.pna, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    pvem.pna <- ave(pvem.pna, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    prd.mc <- ave(prd.mc, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    ind.ALBERTO <- ave(ind.ALBERTO, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    ind.JUAN.DE.DIOS <- ave(ind.JUAN.DE.DIOS, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    ind.ELIAZAR <- ave(ind.ELIAZAR, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    ind.JOSE.D. <- ave(ind.JOSE.D., as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    ind.PEDRO <- ave(ind.PEDRO, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    ind.ARIEL.A. <- ave(ind.ARIEL.A., as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    ind.MANUEL <- ave(ind.MANUEL, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    ind.SANTIAGO <- ave(ind.SANTIAGO, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    ind.IVAN <- ave(ind.IVAN, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    ind.GERMAN <- ave(ind.GERMAN, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    ind.ROSA.M. <- ave(ind.ROSA.M., as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    ind.RUFFO.E. <- ave(ind.RUFFO.E., as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    ind.ARTEMIO <- ave(ind.ARTEMIO, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    ind.BARUCH <- ave(ind.BARUCH, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    ind.CARLOS.R. <- ave(ind.CARLOS.R., as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    ind.LUIS.E. <- ave(ind.LUIS.E., as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    ind.IVONNE <- ave(ind.IVONNE, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    ind.RAYMUNDO <- ave(ind.RAYMUNDO, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    ind.VICTOR <- ave(ind.VICTOR, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    ind.LUIS.E..1 <- ave(ind.LUIS.E..1, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    ind.ADOLFO.J. <- ave(ind.ADOLFO.J., as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    ind.RAUL <- ave(ind.RAUL, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    ind.JOEL.A. <- ave(ind.JOEL.A., as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    nr <- ave(nr, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    nul <- ave(nul, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    tot <- ave(tot, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
    lisnom <- ave(lisnom, as.factor(UBICACION), FUN=sum, na.rm=TRUE);
}
)


# drop redundant rows
d <- d[duplicated(as.factor(d$UBICACION))==FALSE,]
dim(d)
head(d)

# rename some cols
sel <- grep("LISTA", colnames(d))
colnames(d)[sel] <- "lisnom"
sel <- grep("NO.REG", colnames(d))
colnames(d)[sel] <- "nr"
sel <- grep("NULOS", colnames(d))
colnames(d)[sel] <- "nul"
sel <- grep("ID_ESTADO", colnames(d))
colnames(d)[sel] <- "edon"
sel <- grep("ID_DISTRITO", colnames(d))
colnames(d)[sel] <- "disn"
sel <- grep("NOMBRE_DIS", colnames(d))
colnames(d)[sel] <- "cabecera"

# add coals

# write to clipboard to export to excel
clipboard <- function(x, sep=",", row.names=FALSE, col.names=TRUE){
     con <- pipe("xclip -selection clipboard -i", open="w")
     write.table(x, con, sep=sep, row.names=row.names, col.names=col.names)
     close(con)
}

clipboard(d)




######################
## mich excel files ##
######################

library(xlsx)
source("~/Dropbox/data/useful-functions/notin.r")
setwd("/home/eric/Downloads/Desktop/MXelsCalendGovt/elecReturns/datosBrutos/not-in-git/resultCasillas/subnat/mic2021ayca")

all.f <- dir()


# get all colnames
all.n <- c()
for (i in 1:length(all.f)){
    #i <- 1
    message(sprintf("loop %s", i))
    d <- read.xlsx(all.f[i], sheetIndex = 1)
    #d[1,]
    all.n <- c(all.n, colnames(d))
}
all.n <- all.n[duplicated(all.n)==FALSE]
all.n <- all.n[order(all.n)]

# get all files, colSums into new object
new.o <- matrix(rep(0, length(all.n)), nrow = 1)
new.o  <- data.frame(new.o); colnames(new.o)  <- all.n; new.o$MUNICIPIO <- "drop this obs"
for (i in 1:length(all.f)){
    #i <- 1
    message(sprintf("loop %s", i))
    d <- read.xlsx(all.f[i], sheetIndex = 1)
    #d[1,]
    tmp.n <- which(all.n %notin% colnames(d)) # missing columns
    tmp.x <- matrix(0, nrow=nrow(d), ncol=length(tmp.n))
    tmp.x <- data.frame(tmp.x); colnames(tmp.x) <- all.n[tmp.n]
    d <- cbind(d, tmp.x)
    tmp <- d[1,which(colnames(d) %in% c("MUNICIPIO"))] # keep mun
    d <- colSums(d[,-which(colnames(d) %in% c("MUNICIPIO","TIPO.CASILLA","SECCION"))])
    d <- c(mun=tmp, d) # paste mun
    d <- d[order(names(d))]
    new.o <-
        rbind(new.o, d)
#    assign(d, paste0("f", sub("^([0-9]+)[,].+", "\\1", all.f[i]))) # rename object
}

clipboard(new.o)

new.o[1,]



#####################
## cua excel files ##
#####################
rm(list = ls())
#library(xlsx)
library(readxl)
source("~/Dropbox/data/useful-functions/notin.r")
f <- "/home/eric/Downloads/Desktop/MXelsCalendGovt/elecReturns/datosBrutos/not-in-git/resultCasillas/subnat/cua2021ay-casilla.xlsx"

# get all colnames
all.n <- c()
for (i in 1:67){
    #i <- 11
    message(sprintf("loop %s", i))
    #d <- read.xlsx(f, sheetIndex = i)
    #d <- read.xlsx(f, sheetIndex = i, header = FALSE, startRow = 1, endRow = 1)
    d <- read_excel(f, sheet = i)
    #d[1,]
    all.n <- c(all.n, colnames(d))
    rm(d)
}
all.n <- all.n[duplicated(all.n)==FALSE]
all.n <- all.n[order(all.n)]
all.n <- all.n[-which(all.n %in% c("Seccion","Casilla","Eleccion"))]

# get all files, colSums into new object
new.o <- matrix(rep(0, length(all.n)), nrow = 1)
new.o  <- data.frame(new.o); colnames(new.o)  <- all.n; new.o$Municipio <- "drop this obs"
#new.o <- new.o[,-which(colnames(new.o) %in% c("Seccion","Casilla","Eleccion"))]
for (i in 1:67){
    #i <- 1
    message(sprintf("loop %s", i))
    d <- read_excel(f, sheet = i)
    d <- as.data.frame(d) # chg tibble to data frame
    #d[1,]
    tmp.n <- which(all.n %notin% colnames(d)) # missing columns
    tmp.x <- matrix(0, nrow=nrow(d), ncol=length(tmp.n))
    tmp.x <- data.frame(tmp.x); colnames(tmp.x) <- all.n[tmp.n]
    d <- cbind(d, tmp.x)
    tmp <- d[1,which(colnames(d) %in% c("Municipio"))] # keep mun
    tmp2 <- d[1,which(colnames(d) %in% c("NoMpio"))] # keep ife
    d <- colSums(d[,-which(colnames(d) %in% c("Municipio","NoMpio","Seccion","Casilla","Eleccion"))])
    d <- c(Municipio=tmp, NoMpio=tmp2, d) # paste mun
    d <- d[order(names(d))]
    table(colnames(new.o)==names(d))
    new.o <- rbind(new.o, d)
#    assign(d, paste0("f", sub("^([0-9]+)[,].+", "\\1", all.f[i]))) # rename object
}

clipboard <- function(x, sep=",", row.names=FALSE, col.names=TRUE){
     con <- pipe("xclip -selection clipboard -i", open="w")
     write.table(x, con, sep=sep, row.names=row.names, col.names=col.names)
     close(con)
}
clipboard(new.o)

new.o[2,]

