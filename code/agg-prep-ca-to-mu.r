rm(list=ls())

# read from clipboard
d <- read.table(pipe("xclip -selection clipboard -o", open="r"), sep = "\t", header = TRUE)
d[is.na(d)] <- 0
head(d)
d[1,]
colnames(d)

d2 <- d # duplicate
d <- d2

# clean
sel <- which(colnames(d) %in% c("SECCION", "ID_CASILLA", "TIPO_CASILLA","EXT_CONTIGUA","TIPO_ACTA"))
sel <- which(colnames(d) %in% c("DISTRITO","SECCIÓN","CASILLA"))
d <- d[,-sel]

# turn characters into NAs
sel <- c(1,3:35)
tmp <- d[,sel]
tmp <- lapply(tmp, as.numeric)
tmp <- as.data.frame(do.call(cbind,tmp))
tmp[is.na(tmp)] <- 0
head(tmp)
d[,sel] <- tmp

colnames(d)


d <- within(d, expr = {
    PAN <- ave(PAN, as.factor(MUNICIPIO), FUN=sum, na.rm=TRUE);
    PRI <- ave(PRI, as.factor(MUNICIPIO), FUN=sum, na.rm=TRUE);
    PRD <- ave(PRD, as.factor(MUNICIPIO), FUN=sum, na.rm=TRUE);
    PT <- ave(PT, as.factor(MUNICIPIO), FUN=sum, na.rm=TRUE);
    PVEM <- ave(PVEM, as.factor(MUNICIPIO), FUN=sum, na.rm=TRUE);
    MC <- ave(MC, as.factor(MUNICIPIO), FUN=sum, na.rm=TRUE);
    MORENA <- ave(MORENA, as.factor(MUNICIPIO), FUN=sum, na.rm=TRUE);
    NAN <- ave(NAN, as.factor(MUNICIPIO), FUN=sum, na.rm=TRUE);
    VIVA <- ave(VIVA, as.factor(MUNICIPIO), FUN=sum, na.rm=TRUE);
    MLN <- ave(MLN, as.factor(MUNICIPIO), FUN=sum, na.rm=TRUE);
    PES <- ave(PES, as.factor(MUNICIPIO), FUN=sum, na.rm=TRUE);
    RSP <- ave(RSP, as.factor(MUNICIPIO), FUN=sum, na.rm=TRUE);
    FXM <- ave(FXM, as.factor(MUNICIPIO), FUN=sum, na.rm=TRUE);
    CI <- ave(CI, as.factor(MUNICIPIO), FUN=sum, na.rm=TRUE);
    PAN_PRI_PRD <- ave(PAN_PRI_PRD, as.factor(MUNICIPIO), FUN=sum, na.rm=TRUE);
    PAN_PRI <- ave(PAN_PRI, as.factor(MUNICIPIO), FUN=sum, na.rm=TRUE);
    PAN_PRD <- ave(PAN_PRD, as.factor(MUNICIPIO), FUN=sum, na.rm=TRUE);
    PRI_PRD <- ave(PRI_PRD, as.factor(MUNICIPIO), FUN=sum, na.rm=TRUE);
    PT_PVEM_MORENA_NAN <- ave(PT_PVEM_MORENA_NAN, as.factor(MUNICIPIO), FUN=sum, na.rm=TRUE);
    PT_PVEM_MORENA <- ave(PT_PVEM_MORENA, as.factor(MUNICIPIO), FUN=sum, na.rm=TRUE);
    PT_PVEM_NAN <- ave(PT_PVEM_NAN, as.factor(MUNICIPIO), FUN=sum, na.rm=TRUE);
    PT_MORENA_NAN <- ave(PT_MORENA_NAN, as.factor(MUNICIPIO), FUN=sum, na.rm=TRUE);
    PVEM_MORENA_NAN <- ave(PVEM_MORENA_NAN, as.factor(MUNICIPIO), FUN=sum, na.rm=TRUE);
    PT_PVEM <- ave(PT_PVEM, as.factor(MUNICIPIO), FUN=sum, na.rm=TRUE);
    PT_MORENA <- ave(PT_MORENA, as.factor(MUNICIPIO), FUN=sum, na.rm=TRUE);
    PT_NAN <- ave(PT_NAN, as.factor(MUNICIPIO), FUN=sum, na.rm=TRUE);
    PVEM_MORENA <- ave(PVEM_MORENA, as.factor(MUNICIPIO), FUN=sum, na.rm=TRUE);
    PVEM_NAN <- ave(PVEM_NAN, as.factor(MUNICIPIO), FUN=sum, na.rm=TRUE);
    MORENA_NAN <- ave(MORENA_NAN, as.factor(MUNICIPIO), FUN=sum, na.rm=TRUE);
    NO_REGISTRADOS <- ave(NO_REGISTRADOS, as.factor(MUNICIPIO), FUN=sum, na.rm=TRUE);
    NULOS <- ave(NULOS, as.factor(MUNICIPIO), FUN=sum, na.rm=TRUE);
    TOTAL_VOTOS <- ave(TOTAL_VOTOS, as.factor(MUNICIPIO), FUN=sum, na.rm=TRUE);
    LISTA_NOMINAL <- ave(LISTA_NOMINAL, as.factor(MUNICIPIO), FUN=sum, na.rm=TRUE);
}
)

# drop redundant cols
d <- d[duplicated(as.factor(d$MUNICIPIO))==FALSE,]

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
