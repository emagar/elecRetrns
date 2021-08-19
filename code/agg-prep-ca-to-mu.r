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
sel <- which(colnames(d) %in% c("cabecera", "disn", "dis", "tiposec", "casilla", "idcasilla", "tipocasilla","extcon","abreviatura"))
d <- d[,-sel]

# turn characters into NAs
sel <- c(1,3:33)
tmp <- d[,sel]
tmp <- lapply(tmp, as.numeric)
tmp <- as.data.frame(do.call(cbind,tmp))
tmp[is.na(tmp)] <- 0
head(tmp)
d[,sel] <- tmp

d[1,]
colnames(d)

d <- within(d, expr = {
    listanom <- ave(listanom, as.factor(ife), FUN=sum, na.rm=TRUE);
    pan <- ave(pan, as.factor(ife), FUN=sum, na.rm=TRUE);
    pri <- ave(pri, as.factor(ife), FUN=sum, na.rm=TRUE);
    prd <- ave(prd, as.factor(ife), FUN=sum, na.rm=TRUE);
    pvem <- ave(pvem, as.factor(ife), FUN=sum, na.rm=TRUE);
    pt <- ave(pt, as.factor(ife), FUN=sum, na.rm=TRUE);
    mc <- ave(mc, as.factor(ife), FUN=sum, na.rm=TRUE);
    morena <- ave(morena, as.factor(ife), FUN=sum, na.rm=TRUE);
    txver <- ave(txver, as.factor(ife), FUN=sum, na.rm=TRUE);
    podemos <- ave(podemos, as.factor(ife), FUN=sum, na.rm=TRUE);
    pc <- ave(pc, as.factor(ife), FUN=sum, na.rm=TRUE);
    uc <- ave(uc, as.factor(ife), FUN=sum, na.rm=TRUE);
    pes <- ave(pes, as.factor(ife), FUN=sum, na.rm=TRUE);
    rsp <- ave(rsp, as.factor(ife), FUN=sum, na.rm=TRUE);
    fxm <- ave(fxm, as.factor(ife), FUN=sum, na.rm=TRUE);
    pan_pri_prd <- ave(pan_pri_prd, as.factor(ife), FUN=sum, na.rm=TRUE);
    pan_pri <- ave(pan_pri, as.factor(ife), FUN=sum, na.rm=TRUE);
    pan_prd <- ave(pan_prd, as.factor(ife), FUN=sum, na.rm=TRUE);
    pri_prd <- ave(pri_prd, as.factor(ife), FUN=sum, na.rm=TRUE);
    pvem_pt_morena <- ave(pvem_pt_morena, as.factor(ife), FUN=sum, na.rm=TRUE);
    pvem_pt <- ave(pvem_pt, as.factor(ife), FUN=sum, na.rm=TRUE);
    pvem_morena <- ave(pvem_morena, as.factor(ife), FUN=sum, na.rm=TRUE);
    pt_morena <- ave(pt_morena, as.factor(ife), FUN=sum, na.rm=TRUE);
    ci1 <- ave(ci1, as.factor(ife), FUN=sum, na.rm=TRUE);
    ci2 <- ave(ci2, as.factor(ife), FUN=sum, na.rm=TRUE);
    nr <- ave(nr, as.factor(ife), FUN=sum, na.rm=TRUE);
    nul <- ave(nul, as.factor(ife), FUN=sum, na.rm=TRUE);
}
)


# drop redundant rows
d <- d[duplicated(as.factor(d$ife))==FALSE,]
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

