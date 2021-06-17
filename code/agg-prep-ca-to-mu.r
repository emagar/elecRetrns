
# read from clipboard
d <- read.table(pipe("xclip -selection clipboard -o", open="r"), sep = "\t", header = TRUE)
d[is.na(d)] <- 0
head(d)
colnames(d)

d2 <- d # duplicate
d <- d2

# clean
sel <- which(colnames(d) %in% c("CLAVE_CASILLA", "CLAVE_ACTA", "ID_ESTADO", "ESTADO", "SECCION", "ID_CASILLA", "TIPO_CASILLA", "EXT_CONTIGUA", "UBICACION_CASILLA", "TIPO_ACTA", "TOTAL_BOLETAS_SOBRANTES", "TOTAL_PERSONAS_VOTARON", "TOTAL_REP_PARTIDO_CI_VOTARON", "TOTAL_VOTOS_SACADOS"))
d <- d[,-sel]

# turn characters into NAs
tmp <- d[,3:96]
tmp <- lapply(tmp, as.numeric)
tmp <- as.data.frame(do.call(cbind,tmp))
tmp[is.na(tmp)] <- 0
head(tmp)
d[,3:96] <- tmp

d <- within(d, expr = {
    PAN <- ave(PAN, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    PRI <- ave(PRI, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    PRD <- ave(PRD, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    PT <- ave(PT, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    PVEM <- ave(PVEM, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    MC <- ave(MC, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    PCPP <- ave(PCPP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    PSI <- ave(PSI, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    MORENA <- ave(MORENA, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    NAP <- ave(NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    PES <- ave(PES, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    RSP <- ave(RSP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    FXM <- ave(FXM, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CI1 <- ave(CI1, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CI2 <- ave(CI2, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CO_PT_MORENA <- ave(CO_PT_MORENA, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_PCPP <- ave(CC_PAN_PCPP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_PRD <- ave(CC_PAN_PRD, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_PSI <- ave(CC_PAN_PSI, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_PRD_PSI <- ave(CC_PAN_PRD_PSI, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PRD_PSI <- ave(CC_PRD_PSI, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_PRI <- ave(CC_PAN_PRI, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_PRI_PRD <- ave(CC_PAN_PRI_PRD, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PRI_PRD <- ave(CC_PRI_PRD, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_PRI_PRD_PCPP <- ave(CC_PAN_PRI_PRD_PCPP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_PRI_PCPP <- ave(CC_PAN_PRI_PCPP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_PRD_PCPP <- ave(CC_PAN_PRD_PCPP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PRI_PRD_PCPP <- ave(CC_PRI_PRD_PCPP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PRI_PCPP <- ave(CC_PRI_PCPP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PRD_PCPP <- ave(CC_PRD_PCPP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_PRI_PRD_PCPP_PSI <- ave(CC_PAN_PRI_PRD_PCPP_PSI, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_PRI_PRD_PSI <- ave(CC_PAN_PRI_PRD_PSI, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_PRI_PCPP_PSI <- ave(CC_PAN_PRI_PCPP_PSI, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_PRD_PCPP_PSI <- ave(CC_PAN_PRD_PCPP_PSI, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PRI_PRD_PCPP_PSI <- ave(CC_PRI_PRD_PCPP_PSI, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_PRI_PSI <- ave(CC_PAN_PRI_PSI, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_PCPP_PSI <- ave(CC_PAN_PCPP_PSI, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PRI_PRD_PSI <- ave(CC_PRI_PRD_PSI, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PRI_PCPP_PSI <- ave(CC_PRI_PCPP_PSI, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PRD_PCPP_PSI <- ave(CC_PRD_PCPP_PSI, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PRI_PSI <- ave(CC_PRI_PSI, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PCPP_PSI <- ave(CC_PCPP_PSI, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_PRI_PRD_PCPP_PSI_NAP <- ave(CC_PAN_PRI_PRD_PCPP_PSI_NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_PRI_PRD_PCPP_NAP <- ave(CC_PAN_PRI_PRD_PCPP_NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_PRI_PRD_PSI_NAP <- ave(CC_PAN_PRI_PRD_PSI_NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_PRI_PCPP_PSI_NAP <- ave(CC_PAN_PRI_PCPP_PSI_NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_PRD_PCPP_PSI_NAP <- ave(CC_PAN_PRD_PCPP_PSI_NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PRI_PRD_PCPP_PSI_NAP <- ave(CC_PRI_PRD_PCPP_PSI_NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_PRI_PRD_NAP <- ave(CC_PAN_PRI_PRD_NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_PRI_PCPP_NAP <- ave(CC_PAN_PRI_PCPP_NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_PRI_PSI_NAP <- ave(CC_PAN_PRI_PSI_NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_PRD_PCPP_NAP <- ave(CC_PAN_PRD_PCPP_NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_PRD_PSI_NAP <- ave(CC_PAN_PRD_PSI_NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_PCPP_PSI_NAP <- ave(CC_PAN_PCPP_PSI_NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PRI_PRD_PCPP_NAP <- ave(CC_PRI_PRD_PCPP_NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PRI_PRD_PSI_NAP <- ave(CC_PRI_PRD_PSI_NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PRI_PCPP_PSI_NAP <- ave(CC_PRI_PCPP_PSI_NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PRD_PCPP_PSI_NAP <- ave(CC_PRD_PCPP_PSI_NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_PRI_NAP <- ave(CC_PAN_PRI_NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_PRD_NAP <- ave(CC_PAN_PRD_NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_PCPP_NAP <- ave(CC_PAN_PCPP_NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_PSI_NAP <- ave(CC_PAN_PSI_NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PRI_PRD_NAP <- ave(CC_PRI_PRD_NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PRI_PCPP_NAP <- ave(CC_PRI_PCPP_NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PRI_PSI_NAP <- ave(CC_PRI_PSI_NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PRD_PCPP_NAP <- ave(CC_PRD_PCPP_NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PRD_PSI_NAP <- ave(CC_PRD_PSI_NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PCPP_PSI_NAP <- ave(CC_PCPP_PSI_NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PAN_NAP <- ave(CC_PAN_NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PRI_NAP <- ave(CC_PRI_NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PRD_NAP <- ave(CC_PRD_NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PCPP_NAP <- ave(CC_PCPP_NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PSI_NAP <- ave(CC_PSI_NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PT_MORENA_NAP <- ave(CC_PT_MORENA_NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PT_MORENA <- ave(CC_PT_MORENA, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PT_NAP <- ave(CC_PT_NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_MORENA_NAP <- ave(CC_MORENA_NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PT_PCPP <- ave(CC_PT_PCPP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PT_PCPP_MORENA <- ave(CC_PT_PCPP_MORENA, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PCPP_MORENA <- ave(CC_PCPP_MORENA, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PT_PCPP_NAP <- ave(CC_PT_PCPP_NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PT_PSI <- ave(CC_PT_PSI, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PT_PSI_MORENA <- ave(CC_PT_PSI_MORENA, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PSI_MORENA <- ave(CC_PSI_MORENA, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PT_PVEM <- ave(CC_PT_PVEM, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PVEM_PCPP <- ave(CC_PVEM_PCPP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PVEM_PSI <- ave(CC_PVEM_PSI, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_PVEM_NAP <- ave(CC_PVEM_NAP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CC_MC_PSI <- ave(CC_MC_PSI, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    LISTA_NOMINAL <- ave(LISTA_NOMINAL, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
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

