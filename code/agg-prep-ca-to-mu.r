rm(list=ls())
options(width = 120)


# read from clipboard
d <- read.table(pipe("xclip -selection clipboard -o", open="r"), sep = "\t", header = TRUE)
d[is.na(d)] <- 0
str(d)
head(d)
d[1,]
colnames(d)

d2 <- d # duplicate
d <- d2

# clean
sel <- which(colnames(d) %in% c("ID_SECCION","CASILLA"))
d <- d[,-sel]

# turn characters into NAs
sel <- c(2,4,6,7,10,12)
tmp <- d[,-sel]
tmp <- lapply(tmp, as.numeric)
tmp <- as.data.frame(do.call(cbind,tmp))
tmp[is.na(tmp)] <- 0
head(tmp)
d[,-sel] <- tmp

d[2,]
colnames(d)



d <- within(d, expr = {
PAN <- ave(PAN, as.factor(paste(munn,demn,sep=".")), FUN=sum, na.rm=TRUE);
PRI <- ave(PRI, as.factor(paste(munn,demn,sep=".")), FUN=sum, na.rm=TRUE);
PRD <- ave(PRD, as.factor(paste(munn,demn,sep=".")), FUN=sum, na.rm=TRUE);
PT <- ave(PT, as.factor(paste(munn,demn,sep=".")), FUN=sum, na.rm=TRUE);
PVEM <- ave(PVEM, as.factor(paste(munn,demn,sep=".")), FUN=sum, na.rm=TRUE);
MC <- ave(MC, as.factor(paste(munn,demn,sep=".")), FUN=sum, na.rm=TRUE);
MORENA <- ave(MORENA, as.factor(paste(munn,demn,sep=".")), FUN=sum, na.rm=TRUE);
NAN <- ave(NAN, as.factor(paste(munn,demn,sep=".")), FUN=sum, na.rm=TRUE);
VIVA <- ave(VIVA, as.factor(paste(munn,demn,sep=".")), FUN=sum, na.rm=TRUE);
MLN <- ave(MLN, as.factor(paste(munn,demn,sep=".")), FUN=sum, na.rm=TRUE);
PES <- ave(PES, as.factor(paste(munn,demn,sep=".")), FUN=sum, na.rm=TRUE);
RSP <- ave(RSP, as.factor(paste(munn,demn,sep=".")), FUN=sum, na.rm=TRUE);
FXM <- ave(FXM, as.factor(paste(munn,demn,sep=".")), FUN=sum, na.rm=TRUE);
CI <- ave(CI, as.factor(paste(munn,demn,sep=".")), FUN=sum, na.rm=TRUE);
PAN_PRI_PRD <- ave(PAN_PRI_PRD, as.factor(paste(munn,demn,sep=".")), FUN=sum, na.rm=TRUE);
PAN_PRI <- ave(PAN_PRI, as.factor(paste(munn,demn,sep=".")), FUN=sum, na.rm=TRUE);
PAN_PRD <- ave(PAN_PRD, as.factor(paste(munn,demn,sep=".")), FUN=sum, na.rm=TRUE);
PRI_PRD <- ave(PRI_PRD, as.factor(paste(munn,demn,sep=".")), FUN=sum, na.rm=TRUE);
PT_PVEM_MORENA_NAN <- ave(PT_PVEM_MORENA_NAN, as.factor(paste(munn,demn,sep=".")), FUN=sum, na.rm=TRUE);
PT_PVEM_MORENA <- ave(PT_PVEM_MORENA, as.factor(paste(munn,demn,sep=".")), FUN=sum, na.rm=TRUE);
PT_PVEM_NAN <- ave(PT_PVEM_NAN, as.factor(paste(munn,demn,sep=".")), FUN=sum, na.rm=TRUE);
PT_MORENA_NAN <- ave(PT_MORENA_NAN, as.factor(paste(munn,demn,sep=".")), FUN=sum, na.rm=TRUE);
PVEM_MORENA_NAN <- ave(PVEM_MORENA_NAN, as.factor(paste(munn,demn,sep=".")), FUN=sum, na.rm=TRUE);
PT_PVEM <- ave(PT_PVEM, as.factor(paste(munn,demn,sep=".")), FUN=sum, na.rm=TRUE);
PT_MORENA <- ave(PT_MORENA, as.factor(paste(munn,demn,sep=".")), FUN=sum, na.rm=TRUE);
PT_NAN <- ave(PT_NAN, as.factor(paste(munn,demn,sep=".")), FUN=sum, na.rm=TRUE);
PVEM_MORENA <- ave(PVEM_MORENA, as.factor(paste(munn,demn,sep=".")), FUN=sum, na.rm=TRUE);
PVEM_NAN <- ave(PVEM_NAN, as.factor(paste(munn,demn,sep=".")), FUN=sum, na.rm=TRUE);
MORENA_NAN <- ave(MORENA_NAN, as.factor(paste(munn,demn,sep=".")), FUN=sum, na.rm=TRUE);
nr <- ave(nr, as.factor(paste(munn,demn,sep=".")), FUN=sum, na.rm=TRUE);
nul <- ave(nul, as.factor(paste(munn,demn,sep=".")), FUN=sum, na.rm=TRUE);
tot <- ave(tot, as.factor(paste(munn,demn,sep=".")), FUN=sum, na.rm=TRUE);
lisnom <- ave(lisnom, as.factor(paste(munn,demn,sep=".")), FUN=sum, na.rm=TRUE);
}
)


# drop redundant rows
d <- d[duplicated(as.factor(paste(d$munn,d$demn,sep=".")))==FALSE,]
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
all.n <- all.n[-which(all.n %in% c("Distrito","Cabecera.distrital","Sección","Casilla","Recuento"))]
all.n <- all.n[order(all.n)]

# get all files, colSums into new object
new.o <- matrix(rep(0, length(all.n)), nrow = 1)
new.o  <- data.frame(new.o); colnames(new.o)  <- all.n; new.o$Municipio <- "drop this obs"
for (i in 1:length(all.f)){
    #i <- 1
    message(sprintf("loop %s", i))
    d <- read.xlsx(all.f[i], sheetIndex = 1)
    #str(d)
    #d[1,]
    tmp.n <- which(all.n %notin% colnames(d)) # missing columns
    tmp.x <- matrix(0, nrow=nrow(d), ncol=length(tmp.n))
    tmp.x <- data.frame(tmp.x); colnames(tmp.x) <- all.n[tmp.n]
    d <- cbind(d, tmp.x)
    tmp <- d[1,which(colnames(d) %in% c("Municipio"))] # keep mun
    tmp2 <- d[1,which(colnames(d) %in% c("Cve..Municipio"))] # keep ife
    d <- colSums(d[,-which(colnames(d) %in% c("Municipio","Cve..Municipio","Distrito","Cabecera.distrital","Sección","Casilla","Recuento"))], na.rm=TRUE)
    d <- c(Municipio=tmp, Cve..Municipio=tmp2, d) # paste mun
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



#####################
## san excel files ##
#####################

library(xlsx)
source("~/Dropbox/data/useful-functions/notin.r")
setwd("/home/eric/Downloads/Desktop/MXelsCalendGovt/elecReturns/datosBrutos/not-in-git/resultCasillas/subnat/san2021ayca")

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
all.n <- all.n[-which(all.n %in% c("Municipio","Casilla","Dto..Local","No..Mpo"))]
all.n <- c(all.n, "mun", "ife")
all.n <- all.n[order(all.n)]

# get all files, colSums into new object
new.o <- matrix(rep(0, length(all.n)), nrow = 1)
new.o  <- data.frame(new.o); colnames(new.o)  <- all.n; new.o$mun <- "drop this obs"
for (i in 1:length(all.f)){
    #i <- 1
    message(sprintf("loop %s", i))
    d <- read.xlsx(all.f[i], sheetIndex = 1)
    #d[1,]
    #str(d)
    tmp.n <- which(all.n %notin% colnames(d)) # missing columns
    tmp.x <- matrix(0, nrow=nrow(d), ncol=length(tmp.n))
    tmp.x <- data.frame(tmp.x); colnames(tmp.x) <- all.n[tmp.n]
    d <- cbind(d, tmp.x)
    tmp <- d[1,which(colnames(d) %in% c("Municipio"))] # keep mun
    tmp2 <- as.numeric(d[1,which(colnames(d) %in% c("No..Mpo"))]) # keep ife
    d <- colSums(d[,-which(colnames(d) %in% c("Municipio","Casilla","Dto..Local","No..Mpo"))])
    d["mun"] <- tmp   # paste mun
    d["ife"] <- tmp2  # paste ife
    d <- d[order(names(d))]
    new.o <-
        rbind(new.o, d)
#    assign(d, paste0("f", sub("^([0-9]+)[,].+", "\\1", all.f[i]))) # rename object
}

clipboard(new.o)

new.o[1,]



###################
## oax csv files ##
###################

source("~/Dropbox/data/useful-functions/notin.r")
setwd("/home/eric/Downloads/Desktop/MXelsCalendGovt/elecReturns/datosBrutos/not-in-git/resultCasillas/subnat/oax2021ayca")
all.f <- dir()

# get all colnames
all.n <- c()
for (i in 1:length(all.f)){
    #i <- 1
    message(sprintf("loop %s", i))
    d <- read.csv(file = all.f[i], skip = 2, nrows = 1, header = TRUE)
    #d[1,]
    all.n <- c(all.n, colnames(d))
}
all.n <- all.n[duplicated(all.n)==FALSE]
all.n <- all.n[-which(all.n %in% c("ID_ESTADO","NOMBRE_ESTADO","ID_DISTRITO_LOCAL","CABECERA_DISTRITO_LOCAL","SECCION","ID_CASILLA","TIPO_CASILLA","EXT_CONTIGUA","ID_TIPO_CANDIDATURA","ESTATUS_ACTA","CASILLA_INSTALADA","ESTATUS_PAQUETE","ID_INCIDENTE","NUM_ACTA_IMPRESO"))]
#all.n <- c(all.n, "mun", "ife")
all.n <- all.n[order(all.n)]

# get all files, colSums into new object
new.o <- matrix(rep(0, length(all.n)), nrow = 1)
new.o  <- data.frame(new.o); colnames(new.o)  <- all.n; new.o$MUNICIPIO_LOCAL <- "drop this obs"
for (i in 1:length(all.f)){
    #i <- 1
    message(sprintf("loop %s", i))
    d <- read.csv(file = all.f[i], skip = 2, header = TRUE)
    sel <- which(colnames(d) %in% c("ID_ESTADO","NOMBRE_ESTADO","ID_DISTRITO_LOCAL","CABECERA_DISTRITO_LOCAL","SECCION","ID_CASILLA","TIPO_CASILLA","EXT_CONTIGUA","ID_TIPO_CANDIDATURA","ESTATUS_ACTA","CASILLA_INSTALADA","ESTATUS_PAQUETE","ID_INCIDENTE","NUM_ACTA_IMPRESO"))
    d <- d[,-sel]
    #d[1,]
    #str(d)
    tmp.n <- which(all.n %notin% colnames(d)) # missing columns
    tmp.x <- matrix(0, nrow=nrow(d), ncol=length(tmp.n))
    tmp.x <- data.frame(tmp.x); colnames(tmp.x) <- all.n[tmp.n]
    d <- cbind(d, tmp.x)
    tmp <- d[1,which(colnames(d) %in% c("MUNICIPIO_LOCAL"))] # keep mun
    tmp1 <- d[1,which(colnames(d) %in% c("ID_MUNICIPIO_LOCAL"))] # keep ife
    d <- colSums(d[,-which(colnames(d) %in% c("MUNICIPIO_LOCAL"))])
    d["MUNICIPIO_LOCAL"] <- tmp   # paste mun
    d["ID_MUNICIPIO_LOCAL"] <- tmp1   # paste ife
    d <- d[order(names(d))]
    new.o <-
        rbind(new.o, d)
#    assign(d, paste0("f", sub("^([0-9]+)[,].+", "\\1", all.f[i]))) # rename object
}

clipboard(new.o)

new.o[1,]

