
# read from clipboard
d <- read.table(pipe("xclip -selection clipboard -o", open="r"), sep = "\t", header = TRUE)
d[is.na(d)] <- 0
head(d)

d <- within(d, expr = {
    PAN <- ave(PAN, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    PRI <- ave(PRI, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    PRD <- ave(PRD, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    PVEM <- ave(PVEM, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    PT <- ave(PT, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    MC <- ave(MC, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    MOR <- ave(MOR, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    PNA <- ave(PNA, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    PES <- ave(PES, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    RSP <- ave(RSP, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    FM <- ave(FM, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    CAND_IND_1 <- ave(CAND_IND_1, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    C_PAN_PRD_PNA <- ave(C_PAN_PRD_PNA, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    C_PAN_PRD <- ave(C_PAN_PRD, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    C_PAN_PNA <- ave(C_PAN_PNA, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    C_PRI_PRD_PNA <- ave(C_PRI_PRD_PNA, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    C_PRI_PRD <- ave(C_PRI_PRD, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    C_PRI_PNA <- ave(C_PRI_PNA, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    C_PRD_PNA <- ave(C_PRD_PNA, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    NO_REGISTRADOS <- ave(NO_REGISTRADOS, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    NULOS <- ave(NULOS, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    TOTAL_VOTOS_ASENTADO <- ave(TOTAL_VOTOS_ASENTADO, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    TOTAL_VOTOS_CALCULADO <- ave(TOTAL_VOTOS_CALCULADO, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE);
    LISTA_NOMINAL <- ave(LISTA_NOMINAL, as.factor(ID_MUNICIPIO), FUN=sum, na.rm=TRUE)
}
)

# drop redundant cols
d <- d[duplicated(d$ID_MUNICIPIO)==FALSE,]

# add coals

