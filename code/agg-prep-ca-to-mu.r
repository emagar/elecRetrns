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
sel <- which(colnames(d) %in% c("SECCION", "TIPO_CASILLA"))
d <- d[,-sel]

# turn characters into NAs
tmp <- d[,c(1,3:44)]
tmp <- lapply(tmp, as.numeric)
tmp <- as.data.frame(do.call(cbind,tmp))
tmp[is.na(tmp)] <- 0
head(tmp)
d[,c(1,3:44)] <- tmp

d <- within(d, expr = {
    PAN <-                   ave(PAN                    , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    PRI <-                   ave(PRI                    , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    PRD <-                   ave(PRD                    , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    PVEM <-                  ave(PVEM                   , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    PT <-                    ave(PT                     , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    MC <-                    ave(MC                     , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    MORENA <-                ave(MORENA                 , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    MAS <-                   ave(MAS                    , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CQROO <-                 ave(CQROO                  , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    PES <-                   ave(PES                    , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    RSP <-                   ave(RSP                    , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    FMEX <-                  ave(FMEX                   , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CAND_IND_1 <-            ave(CAND_IND_1             , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CAND_IND_2 <-            ave(CAND_IND_2             , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CAND_IND_3 <-            ave(CAND_IND_3             , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CAND_IND_4 <-            ave(CAND_IND_4             , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    C_PAN_PRI_PRD_CQROO <-   ave(C_PAN_PRI_PRD_CQROO    , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    C_PAN_PRI_PRD <-         ave(C_PAN_PRI_PRD          , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    C_PAN_PRI_CQROO <-       ave(C_PAN_PRI_CQROO        , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    C_PAN_PRD_CQROO <-       ave(C_PAN_PRD_CQROO        , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    C_PRI_PRD_CQROO <-       ave(C_PRI_PRD_CQROO        , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    C_PAN_PRI <-             ave(C_PAN_PRI              , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    C_PAN_PRD <-             ave(C_PAN_PRD              , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    C_PAN_CQROO <-           ave(C_PAN_CQROO            , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    C_PRI_PRD <-             ave(C_PRI_PRD              , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    C_PRI_CQROO <-           ave(C_PRI_CQROO            , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    C_PRD_CQROO <-           ave(C_PRD_CQROO            , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    C_PVEM_PT_MORENA_MAS <-  ave(C_PVEM_PT_MORENA_MAS   , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    C_PVEM_PT_MORENA <-      ave(C_PVEM_PT_MORENA       , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    C_PVEM_PT_MAS <-         ave(C_PVEM_PT_MAS          , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    C_PVEM_MORENA_MAS <-     ave(C_PVEM_MORENA_MAS      , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    C_PT_MORENA_MAS <-       ave(C_PT_MORENA_MAS        , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    C_PVEM_PT <-             ave(C_PVEM_PT              , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    C_PVEM_MORENA <-         ave(C_PVEM_MORENA          , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    C_PVEM_MAS <-            ave(C_PVEM_MAS             , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    C_PT_MORENA <-           ave(C_PT_MORENA            , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    C_PT_MAS <-              ave(C_PT_MAS               , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    C_MORENA_MAS <-          ave(C_MORENA_MAS           , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    NO_REGISTRADOS <-        ave(NO_REGISTRADOS         , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    NULOS <-                 ave(NULOS                  , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    TOTAL_VOTOS_CALCULADO <- ave(TOTAL_VOTOS_CALCULADO  , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    LISTA_NOMINAL <-         ave(LISTA_NOMINAL          , as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
}
)

# drop redundant cols
d <- d[duplicated(as.factor(d$ID_MUNICIPIO))==FALSE,]

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

