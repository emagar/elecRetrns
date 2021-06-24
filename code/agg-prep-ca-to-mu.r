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
sel <- which(colnames(d) %in% c("SECCION", "ID_CASILLA", "TIPO_CASILLA","EXT_CONTIGUA","UBICACION_CASILLA","TIPO_ACTA","TOTAL_BOLETAS_SOBRANTES", "TOTAL_PERSONAS_VOTARON","TOTAL_REP_PARTIDO_CI_VOTARON","TOTAL_VOTOS_SACADOS"))
sel <- which(colnames(d) %in% c("DISTRITO","SECCIÓN","CASILLA"))
d <- d[,-sel]

# turn characters into NAs
sel <- c(2:34)
tmp <- d[,sel]
tmp <- lapply(tmp, as.numeric)
tmp <- as.data.frame(do.call(cbind,tmp))
tmp[is.na(tmp)] <- 0
head(tmp)
d[,sel] <- tmp

colnames(d)

d <- within(d, expr = {
    PAN <-            ave(PAN            , as.factor(DEMARCACIÓN), FUN=sum, na.rm=TRUE);
    PRI <-            ave(PRI            , as.factor(DEMARCACIÓN), FUN=sum, na.rm=TRUE);                                
    PRD <-            ave(PRD            , as.factor(DEMARCACIÓN), FUN=sum, na.rm=TRUE);                                
    PVEM <-           ave(PVEM           , as.factor(DEMARCACIÓN), FUN=sum, na.rm=TRUE);                                
    PT <-             ave(PT             , as.factor(DEMARCACIÓN), FUN=sum, na.rm=TRUE);                                
    MC <-             ave(MC             , as.factor(DEMARCACIÓN), FUN=sum, na.rm=TRUE);                                
    MORENA <-         ave(MORENA         , as.factor(DEMARCACIÓN), FUN=sum, na.rm=TRUE);                                
    ELIGE <-          ave(ELIGE          , as.factor(DEMARCACIÓN), FUN=sum, na.rm=TRUE);                                
    PES <-            ave(PES            , as.factor(DEMARCACIÓN), FUN=sum, na.rm=TRUE);                                
    RSP <-            ave(RSP            , as.factor(DEMARCACIÓN), FUN=sum, na.rm=TRUE);                                
    FXM <-            ave(FXM            , as.factor(DEMARCACIÓN), FUN=sum, na.rm=TRUE);                                
    ROP <-            ave(ROP            , as.factor(DEMARCACIÓN), FUN=sum, na.rm=TRUE);                                
    HAAC <-           ave(HAAC           , as.factor(DEMARCACIÓN), FUN=sum, na.rm=TRUE);                                
    PRMC <-           ave(PRMC           , as.factor(DEMARCACIÓN), FUN=sum, na.rm=TRUE);                                
    ECR <-            ave(ECR            , as.factor(DEMARCACIÓN), FUN=sum, na.rm=TRUE);                                
    EVFA <-           ave(EVFA           , as.factor(DEMARCACIÓN), FUN=sum, na.rm=TRUE);                                
    AGP <-            ave(AGP            , as.factor(DEMARCACIÓN), FUN=sum, na.rm=TRUE);                                
    RGS <-            ave(RGS            , as.factor(DEMARCACIÓN), FUN=sum, na.rm=TRUE);                                
    PAN.PRI.PRD <-    ave(PAN.PRI.PRD    , as.factor(DEMARCACIÓN), FUN=sum, na.rm=TRUE);                                
    PAN.PRI <-        ave(PAN.PRI        , as.factor(DEMARCACIÓN), FUN=sum, na.rm=TRUE);                                
    PAN.PRD <-        ave(PAN.PRD        , as.factor(DEMARCACIÓN), FUN=sum, na.rm=TRUE);                                
    PRI.PRD <-        ave(PRI.PRD        , as.factor(DEMARCACIÓN), FUN=sum, na.rm=TRUE);                                
    PVEM.PT.MORENA <- ave(PVEM.PT.MORENA , as.factor(DEMARCACIÓN), FUN=sum, na.rm=TRUE);                                
    PVEM.PT <-        ave(PVEM.PT        , as.factor(DEMARCACIÓN), FUN=sum, na.rm=TRUE);                                
    PVEM.MORENA <-    ave(PVEM.MORENA    , as.factor(DEMARCACIÓN), FUN=sum, na.rm=TRUE);                                
    PT.MORENA <-      ave(PT.MORENA      , as.factor(DEMARCACIÓN), FUN=sum, na.rm=TRUE);                                
}
)

# drop redundant cols
d <- d[duplicated(as.factor(d$DEMARCACIÓN))==FALSE,]

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

