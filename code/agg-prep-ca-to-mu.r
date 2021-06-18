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
sel <- which(colnames(d) %in% c("CLAVE_CASILLA", "CLAVE_ACTA", "ID_ESTADO", "ESTADO", "SECCION", "ID_CASILLA", "TIPO_CASILLA", "EXT_CONTIGUA", "UBICACION_CASILLA", "TIPO_ACTA", "TOTAL_BOLETAS_SOBRANTES", "TOTAL_PERSONAS_VOTARON", "TOTAL_REP_PARTIDO_CI_VOTARON", "TOTAL_VOTOS_SACADOS"))
d <- d[,-sel]

# turn characters into NAs
tmp <- d[,3:39]
tmp <- lapply(tmp, as.numeric)
tmp <- as.data.frame(do.call(cbind,tmp))
tmp[is.na(tmp)] <- 0
head(tmp)
d[,3:39] <- tmp

d <- within(d, expr = {
    PAN                <- ave(PAN,             as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    PRI                <- ave(PRI,             as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    PRD                <- ave(PRD,             as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    PVEM               <- ave(PVEM,            as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    PT                 <- ave(PT,              as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    MC                 <- ave(MC,              as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    PUP                <- ave(PUP,             as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    MORENA             <- ave(MORENA,          as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    NAO                <- ave(NAO,             as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    PES                <- ave(PES,             as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    RSP                <- ave(RSP,             as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    FXM                <- ave(FXM,             as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CI1                <- ave(CI1,             as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CI2                <- ave(CI2,             as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CI3                <- ave(CI3,             as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CO_PAN_PRI_PRD     <- ave(CO_PAN_PRI_PRD,  as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CO_PAN_PRI         <- ave(CO_PAN_PRI,      as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CO_PAN_PRD         <- ave(CO_PAN_PRD,      as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CO_PRI_PRD         <- ave(CO_PRI_PRD,      as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_PRI_PRD_NAO <- ave(CC_PAN_PRI_PRD_NAO,  as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_PRI_PRD     <- ave(CC_PAN_PRI_PRD,  as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_PRI_NAO     <- ave(CC_PAN_PRI_NAO,  as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_PRD_NAO     <- ave(CC_PAN_PRD_NAO,  as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PRI_PRD_NAO     <- ave(CC_PRI_PRD_NAO,  as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_PRI         <- ave(CC_PAN_PRI,      as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_PRD         <- ave(CC_PAN_PRD,      as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_NAO         <- ave(CC_PAN_NAO,      as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PRI_PRD         <- ave(CC_PRI_PRD,      as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PRI_NAO         <- ave(CC_PRI_NAO,      as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PRD_NAO         <- ave(CC_PRD_NAO,      as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PVEM_PT         <- ave(CC_PVEM_PT,      as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PVEM_NAO        <- ave(CC_PVEM_NAO,     as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    LISTA_NOMINAL      <- ave(LISTA_NOMINAL,   as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    }
)

# drop redundant cols
d <- d[duplicated(d$ID_MUNICIPIO)==FALSE,]

# add coals

# write to clipboard to export to excel
clipboard <- function(x, sep=",", row.names=FALSE, col.names=TRUE){
     con <- pipe("xclip -selection clipboard -i", open="w")
     write.table(x, con, sep=sep, row.names=row.names, col.names=col.names)
     close(con)
}

clipboard(d)

