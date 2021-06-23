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
sel <- which(colnames(d) %in% c("CLAVE_CASILLA", "CLAVE_ACTA", "NOMBRE_ESTADO", "SECCION", "ID_CASILLA", "TIPO_CASILLA", "EXT_CONTIGUA", "CASILLA", "NUM_ACTA_IMPRESO", "OBSERVACIONES", "MECANISMOS_TRASLADO", "FECHA_HORA"))
d <- d[,-sel]

# turn characters into NAs
tmp <- d[,c(1:2,4:26)]
tmp <- lapply(tmp, as.numeric)
tmp <- as.data.frame(do.call(cbind,tmp))
tmp[is.na(tmp)] <- 0
head(tmp)
d[,c(1:2,4:26)] <- tmp

d <- within(d, expr = {
    PAN <-                         ave(PAN,                         as.factor(ID_ESTADO + ID_DISTRITO/100), FUN=sum, na.rm=TRUE);
    PRI <-                         ave(PRI,                         as.factor(ID_ESTADO + ID_DISTRITO/100), FUN=sum, na.rm=TRUE);
    PRD <-                         ave(PRD,                         as.factor(ID_ESTADO + ID_DISTRITO/100), FUN=sum, na.rm=TRUE);
    PVEM <-                        ave(PVEM,                        as.factor(ID_ESTADO + ID_DISTRITO/100), FUN=sum, na.rm=TRUE);
    PT <-                          ave(PT,                          as.factor(ID_ESTADO + ID_DISTRITO/100), FUN=sum, na.rm=TRUE);
    MC <-                          ave(MC,                          as.factor(ID_ESTADO + ID_DISTRITO/100), FUN=sum, na.rm=TRUE);
    MORENA <-                      ave(MORENA,                      as.factor(ID_ESTADO + ID_DISTRITO/100), FUN=sum, na.rm=TRUE);
    PES <-                         ave(PES,                         as.factor(ID_ESTADO + ID_DISTRITO/100), FUN=sum, na.rm=TRUE);
    RSP <-                         ave(RSP,                         as.factor(ID_ESTADO + ID_DISTRITO/100), FUN=sum, na.rm=TRUE);
    FXM <-                         ave(FXM,                         as.factor(ID_ESTADO + ID_DISTRITO/100), FUN=sum, na.rm=TRUE);
    CI <-                          ave(CI,                          as.factor(ID_ESTADO + ID_DISTRITO/100), FUN=sum, na.rm=TRUE);
    PAN.PRI.PRD <-                 ave(PAN.PRI.PRD,                 as.factor(ID_ESTADO + ID_DISTRITO/100), FUN=sum, na.rm=TRUE);
    PAN.PRI <-                     ave(PAN.PRI,                     as.factor(ID_ESTADO + ID_DISTRITO/100), FUN=sum, na.rm=TRUE);
    PAN.PRD <-                     ave(PAN.PRD,                     as.factor(ID_ESTADO + ID_DISTRITO/100), FUN=sum, na.rm=TRUE);
    PRI.PRD <-                     ave(PRI.PRD,                     as.factor(ID_ESTADO + ID_DISTRITO/100), FUN=sum, na.rm=TRUE);
    PVEM.PT.MORENA <-              ave(PVEM.PT.MORENA,              as.factor(ID_ESTADO + ID_DISTRITO/100), FUN=sum, na.rm=TRUE);
    PVEM.PT <-                     ave(PVEM.PT,                     as.factor(ID_ESTADO + ID_DISTRITO/100), FUN=sum, na.rm=TRUE);
    PVEM.MORENA <-                 ave(PVEM.MORENA,                 as.factor(ID_ESTADO + ID_DISTRITO/100), FUN=sum, na.rm=TRUE);
    PT.MORENA <-                   ave(PT.MORENA,                   as.factor(ID_ESTADO + ID_DISTRITO/100), FUN=sum, na.rm=TRUE);
    CANDIDATO.A.NO.REGISTRADO.A <- ave(CANDIDATO.A.NO.REGISTRADO.A, as.factor(ID_ESTADO + ID_DISTRITO/100), FUN=sum, na.rm=TRUE);
    VOTOS.NULOS <-                 ave(VOTOS.NULOS,                 as.factor(ID_ESTADO + ID_DISTRITO/100), FUN=sum, na.rm=TRUE);
    TOTAL_VOTOS_CALCULADOS <-      ave(TOTAL_VOTOS_CALCULADOS,      as.factor(ID_ESTADO + ID_DISTRITO/100), FUN=sum, na.rm=TRUE);
    LISTA_NOMINAL_CASILLA <-       ave(LISTA_NOMINAL_CASILLA,       as.factor(ID_ESTADO + ID_DISTRITO/100), FUN=sum, na.rm=TRUE);
    }
)

# drop redundant cols
d <- d[duplicated(as.factor(d$ID_ESTADO + d$ID_DISTRITO/100))==FALSE,]

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

