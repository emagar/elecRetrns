rm(list=ls())

wd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/datosBrutos/resultCasillas/eum2018all/"

setwd(wd)

pr <- read.csv(file = "presidencia.csv", sep = "|", stringsAsFactors = FALSE)

colnames(pr)

pr <- pr[, c("ID_ESTADO", "ID_DISTRITO","NOMBRE_DISTRITO", "SECCION","ID_CASILLA", "TIPO_CASILLA","EXT_CONTIGUA", "CASILLA", "TOTAL_VOTOS_CALCULADOS","PAN","PRI", "PRD","PVEM", "PT","MOVIMIENTO.CIUDADANO", "NUEVA.ALIANZA","MORENA", "ENCUENTRO.SOCIAL","PAN_PRD_MC", "PAN_PRD","PAN_MC", "PRD_MC","PRI_PVEM_NA", "PRI_PVEM","PRI_NA", "PVEM_NA","PT_MORENA_PES", "PT_MORENA","PT_PES", "MORENA_PES","CAND_IND_01", "CAND_IND_02","CNR", "VN","LISTA_NOMINAL_CASILLA")]

sel <- which(colnames(pr)=="ID_ESTADO")
colnames(pr)[sel] <- "edon"
sel <- which(colnames(pr)=="ID_DISTRITO")
colnames(pr)[sel] <- "disn"
sel <- which(colnames(pr)=="NOMBRE_DISTRITO")
colnames(pr)[sel] <- "cab"
sel <- which(colnames(pr)=="SECCION")
colnames(pr)[sel] <- "seccion"
sel <- which(colnames(pr)=="ID_CASILLA")
colnames(pr)[sel] <- "casn"
sel <- which(colnames(pr)=="TIPO_CASILLA")
colnames(pr)[sel] <- "cast"
sel <- which(colnames(pr)=="EXT_CONTIGUA")
colnames(pr)[sel] <- "casc"
sel <- which(colnames(pr)=="LISTA_NOMINAL_CASILLA")
colnames(pr)[sel] <- "lisnom"
sel <- which(colnames(pr)=="CNR")
colnames(pr)[sel] <- "nr"
sel <- which(colnames(pr)=="VN")
colnames(pr)[sel] <- "nul"
sel <- which(colnames(pr)=="PAN")
colnames(pr)[sel] <- "pan"
sel <- which(colnames(pr)=="PRI")
colnames(pr)[sel] <- "pri"
sel <- which(colnames(pr)=="PRD")
colnames(pr)[sel] <- "prd"
sel <- which(colnames(pr)=="PVEM")
colnames(pr)[sel] <- "pvem"
sel <- which(colnames(pr)=="PT")
colnames(pr)[sel] <- "pt"
sel <- which(colnames(pr)=="MOVIMIENTO.CIUDADANO")
colnames(pr)[sel] <- "mc"
sel <- which(colnames(pr)=="NUEVA.ALIANZA")
colnames(pr)[sel] <- "panal"
sel <- which(colnames(pr)=="MORENA")
colnames(pr)[sel] <- "morena"
sel <- which(colnames(pr)=="ENCUENTRO.SOCIAL")
colnames(pr)[sel] <- "pes"
sel <- which(colnames(pr)=="PAN_PRD_MC")
colnames(pr)[sel] <- "pan.prd.mc"
sel <- which(colnames(pr)=="PAN_PRD")
colnames(pr)[sel] <- "pan.prd"
sel <- which(colnames(pr)=="PAN_MC")
colnames(pr)[sel] <- "pan.mc"
sel <- which(colnames(pr)=="PRD_MC")
colnames(pr)[sel] <- "prd.mc"
sel <- which(colnames(pr)=="PRI_PVEM_NA")
colnames(pr)[sel] <- "pri.pvem.panal"
sel <- which(colnames(pr)=="PRI_PVEM")
colnames(pr)[sel] <- "pri.pvem"
sel <- which(colnames(pr)=="PRI_NA")
colnames(pr)[sel] <- "pri.panal"
sel <- which(colnames(pr)=="PVEM_NA")
colnames(pr)[sel] <- "pvem.panal"
sel <- which(colnames(pr)=="PT_MORENA_PES")
colnames(pr)[sel] <- "morena.pt.pes"
sel <- which(colnames(pr)=="PT_MORENA")
colnames(pr)[sel] <- "morena.pt"
sel <- which(colnames(pr)=="PT_PES")
colnames(pr)[sel] <- "pt.pes"
sel <- which(colnames(pr)=="MORENA_PES")
colnames(pr)[sel] <- "morena.pes"
sel <- which(colnames(pr)=="CAND_IND_01")
colnames(pr)[sel] <- "indep1"
sel <- which(colnames(pr)=="CAND_IND_02")
colnames(pr)[sel] <- "indep2"

# consolida num casilla
pr$tmp <- pr$casn
sel <- which(pr$cast=="C")
pr$casn[sel] <- paste(pr$tmp[sel], pr$cast[sel], pr$casc[sel], sep = "")
pr$casn[-sel] <- paste(pr$tmp[-sel], pr$cast[-sel], sep = "")
pr$cast <- pr$casc <- pr$tmp <- NULL

pr[1,]

write.csv(pr, file = "eum2018prca.csv", row.names = FALSE)


dip <- read.csv(file = "eum2018dfcaPrep93porciento.csv", sep = "|", stringsAsFactors = FALSE)

dip <- dip[, c("ID_ESTADO", "ID_DISTRITO_FEDERAL","DISTRITO_FEDERAL", "SECCION","ID_CASILLA", "TIPO_CASILLA","EXT_CONTIGUA", "UBICACION_CASILLA","TIPO_ACTA","TOTAL_VOTOS_SACADOS","PAN","PRI", "PRD","PVEM", "PT","MC", "PANAL","MORENA", "PES","C_PAN_PRD_MC", "C_PAN_PRD","C_PAN_MC", "C_PRD_MC","C_PRI_PVEM_PANAL", "C_PRI_PVEM","C_PRI_PANAL", "C_PVEM_PANAL","C_PT_MORENA_PES", "C_PT_MORENA","C_PT_PES", "C_MORENA_PES","CAND_IND_01", "CAND_IND_02","NO_REGISTRADOS", "NULOS","LISTA_NOMINAL", "OBSERVACIONES")]

sel <- which(colnames(dip)=="ID_ESTADO")
colnames(dip)[sel] <- "edon"
sel <- which(colnames(dip)=="ID_DISTRITO_FEDERAL")
colnames(dip)[sel] <- "disn"
sel <- which(colnames(dip)=="DISTRITO_FEDERAL")
colnames(dip)[sel] <- "cab"
sel <- which(colnames(dip)=="SECCION")
colnames(dip)[sel] <- "seccion"
sel <- which(colnames(dip)=="ID_CASILLA")
colnames(dip)[sel] <- "casn"
sel <- which(colnames(dip)=="TIPO_CASILLA")
colnames(dip)[sel] <- "cast"
sel <- which(colnames(dip)=="EXT_CONTIGUA")
colnames(dip)[sel] <- "casc"
sel <- which(colnames(dip)=="LISTA_NOMINAL")
colnames(dip)[sel] <- "lisnom"
sel <- which(colnames(dip)=="OBSERVACIONES")
colnames(dip)[sel] <- "obs"
sel <- which(colnames(dip)=="NO_REGISTRADOS")
colnames(dip)[sel] <- "nr"
sel <- which(colnames(dip)=="NULOS")
colnames(dip)[sel] <- "nul"
sel <- which(colnames(dip)=="CAND_IND_01")
sel <- which(colnames(dip)=="PAN")
colnames(dip)[sel] <- "pan"
sel <- which(colnames(dip)=="DIPI")
colnames(dip)[sel] <- "dipi"
sel <- which(colnames(dip)=="DIPD")
colnames(dip)[sel] <- "dipd"
sel <- which(colnames(dip)=="PVEM")
colnames(dip)[sel] <- "pvem"
sel <- which(colnames(dip)=="PT")
colnames(dip)[sel] <- "pt"
sel <- which(colnames(dip)=="MC")
colnames(dip)[sel] <- "mc"
sel <- which(colnames(dip)=="PANAL")
colnames(dip)[sel] <- "panal"
sel <- which(colnames(dip)=="MORENA")
colnames(dip)[sel] <- "morena"
sel <- which(colnames(dip)=="PES")
colnames(dip)[sel] <- "pes"
sel <- which(colnames(dip)=="C_PAN_DIPD_MC")
colnames(dip)[sel] <- "pan-dipd-mc"
sel <- which(colnames(dip)=="C_PAN_DIPD")
colnames(dip)[sel] <- "pan-dipd"
sel <- which(colnames(dip)=="C_PAN_MC")
colnames(dip)[sel] <- "pan-mc"
sel <- which(colnames(dip)=="C_DIPD_MC")
colnames(dip)[sel] <- "dipd-mc"
sel <- which(colnames(dip)=="C_DIPI_PVEM_PANAL")
colnames(dip)[sel] <- "dipi-pvem-panal"
sel <- which(colnames(dip)=="C_DIPI_PVEM")
colnames(dip)[sel] <- "dipi-pvem"
sel <- which(colnames(dip)=="C_DIPI_PANAL")
colnames(dip)[sel] <- "dipi-panal"
sel <- which(colnames(dip)=="C_PVEM_PANAL")
colnames(dip)[sel] <- "pvem-panal"
sel <- which(colnames(dip)=="C_PT_MORENA_PES")
colnames(dip)[sel] <- "morena-pt-pes"
sel <- which(colnames(dip)=="C_PT_MORENA")
colnames(dip)[sel] <- "morena-pt"
sel <- which(colnames(dip)=="C_PT_PES")
colnames(dip)[sel] <- "pt-pes"
sel <- which(colnames(dip)=="C_MORENA_PES")
colnames(dip)[sel] <- "morena-pes"
sel <- which(colnames(dip)=="CAND_IND_01")
colnames(dip)[sel] <- "indep1"
sel <- which(colnames(dip)=="CAND_IND_02")
colnames(dip)[sel] <- "indep2"

# consolida num casilla
dip$tmp <- dip$casn
sel <- which(dip$cast=="C")
dip$casn[sel] <- paste(dip$tmp[sel], dip$cast[sel], dip$casc[sel], sep = "")
dip$casn[-sel] <- paste(dip$tmp[-sel], dip$cast[-sel], sep = "")
dip$cast <- dip$casc <- dip$tmp <- NULL

dip[1,]

write.csv(dip, file = "eum2018dipcaPrep93porciento.csv", row.names = FALSE)
