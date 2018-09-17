rm(list=ls())

wd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/datosBrutos/resultCasillas"

setwd(wd)

# secc-munn
sm <- read.csv(file = "../../../redistrict/ife.ine/equivSecc/tablaEquivalenciasSeccionalesDesde1994.csv", stringsAsFactors = FALSE)
sm <- sm[,c("edon","seccion","munn")]

########
# 2012 #
########
pr <- read.csv(file = "eum2012all/datos_computos_casillas_presidente.csv", sep = "|", stringsAsFactors = FALSE)

pr <- pr[, c("ID_ESTADO", "D_DISTRITO","SECCION","ID_CASILLA", "TIPO_CASILLA","EXT_CONTIGUA", "NUMERO_VOTOS_VALIDOS","PAN","PRI","PRD", "PVEM","PT","MC", "PANAL","PRI_PVEM","PRD_PT_MC", "PRD_PT","PRD_MC","PT_MC", "ID_MUNICIPIO","LISTA_NOMINAL")]

sel <- which(colnames(pr)=="ID_ESTADO")
colnames(pr)[sel] <- "edon"
sel <- which(colnames(pr)=="D_DISTRITO")
colnames(pr)[sel] <- "disn"
sel <- which(colnames(pr)=="SECCION")
colnames(pr)[sel] <- "seccion"
sel <- which(colnames(pr)=="ID_CASILLA")
colnames(pr)[sel] <- "casn"
sel <- which(colnames(pr)=="TIPO_CASILLA")
colnames(pr)[sel] <- "cast"
sel <- which(colnames(pr)=="EXT_CONTIGUA")
colnames(pr)[sel] <- "casc"
sel <- which(colnames(pr)=="ID_MUNICIPIO")
colnames(pr)[sel] <- "munn"
sel <- which(colnames(pr)=="LISTA_NOMINAL")
colnames(pr)[sel] <- "lisnom"
sel <- which(colnames(pr)=="NUMERO_VOTOS_VALIDOS")
colnames(pr)[sel] <- "efec"
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
sel <- which(colnames(pr)=="MC")
colnames(pr)[sel] <- "mc"
sel <- which(colnames(pr)=="PANAL")
colnames(pr)[sel] <- "panal"
sel <- which(colnames(pr)=="PRI_PVEM")
colnames(pr)[sel] <- "pri.pvem"
sel <- which(colnames(pr)=="PRD_PT_MC")
colnames(pr)[sel] <- "prd.pt.mc"
sel <- which(colnames(pr)=="PRD_PT")
colnames(pr)[sel] <- "prd.pt"
sel <- which(colnames(pr)=="PRD_MC")
colnames(pr)[sel] <- "prd.mc"
sel <- which(colnames(pr)=="PT_MC")
colnames(pr)[sel] <- "pt.mc"

# consolida num casilla
pr$tmp <- pr$casn
sel <- which(pr$cast=="C")
pr$casn[sel] <- paste(pr$tmp[sel], pr$cast[sel], pr$casc[sel], sep = "")
pr$casn[-sel] <- paste(pr$tmp[-sel], pr$cast[-sel], sep = "")
pr$cast <- pr$casc <- pr$tmp <- NULL

# consolida voto amlo
pr$amlo <- pr$prd + pr$pt + pr$mc + pr$prd.pt.mc + pr$prd.pt + pr$prd.mc + pr$pt.mc 
pr$prd <- pr$pt <- pr$mc <- pr$prd.pt.mc <- pr$prd.pt <- pr$prd.mc <- pr$pt.mc <- NULL
# consolida voto peña
pr$pena <- pr$pri + pr$pvem + pr$pri.pvem
pr$pri <- pr$pvem <- pr$pri.pvem <- NULL
# jvm
pr$jvm <- pr$pan
pr$pan <- NULL
# efec
pr$efec <- pr$panal + pr$amlo + pr$pena + pr$jvm
# clean
pr$casn <- pr$panal <- pr$lisnom <- pr$disn <- pr$munn <- NULL

# consolida secciones
pr$amlo <- ave(pr$amlo, as.factor(pr$edon*10000+pr$seccion), FUN=sum, na.rm=TRUE)
pr$pena <- ave(pr$pena, as.factor(pr$edon*10000+pr$seccion), FUN=sum, na.rm=TRUE)
pr$jvm <- ave(pr$jvm, as.factor(pr$edon*10000+pr$seccion), FUN=sum, na.rm=TRUE)
pr$efec <- ave(pr$efec, as.factor(pr$edon*10000+pr$seccion), FUN=sum, na.rm=TRUE)
pr <- pr[duplicated(pr$edon*10000+pr$seccion)==FALSE, ]

# merge munn
pr <- merge(x=sm, y=pr, by=c("edon","seccion"), all.x = FALSE, all.y = TRUE)
sel <- which(pr$seccion==0)
pr <- pr[-sel,] # drop votos extranjero
rm(sel)

# version secciones
pr.se <- pr
# shares
pr.se$amlo12 <- pr.se$amlo/pr.se$efec
pr.se$pena <- pr.se$pena/pr.se$efec
pr.se$jvm  <- pr.se$jvm/pr.se$efec
pr.se$amlo <- NULL

# consolida municipios
pr$amlo <- ave(pr$amlo, as.factor(pr$edon*1000+pr$munn), FUN=sum, na.rm=TRUE)
pr$pena <- ave(pr$pena, as.factor(pr$edon*1000+pr$munn), FUN=sum, na.rm=TRUE)
pr$jvm <- ave(pr$jvm, as.factor(pr$edon*1000+pr$munn), FUN=sum, na.rm=TRUE)
pr$efec <- ave(pr$efec, as.factor(pr$edon*1000+pr$munn), FUN=sum, na.rm=TRUE)
pr <- pr[duplicated(pr$edon*1000+pr$munn)==FALSE, ]
pr$seccion <- NULL

# shares
pr$amlo12 <- pr$amlo/pr$efec
pr$pena <- pr$pena/pr$efec
pr$jvm  <- pr$jvm/pr$efec
pr$amlo <- NULL

# version municipios
pr.mu <- pr
rm(pr)

####################################
# 2015 prd (Anaya's neg. baseline) #
####################################
df <- read.csv(file = "eum2015all/DiputadosMR2015.csv", sep = ",", stringsAsFactors = FALSE)

df <- df[, c("ID_ESTADO", "ID_DISTRITO","ID_MUNICIPIO","SECCION","CASILLA","TOTAL_VOTOS","PAN","PRI","PRD", "PVEM","PT","MC", "NVA_ALIANZA","MORENA","PH","ES","PAN_NVA_ALIANZA","PRI_PVEM","PRD_PT","CAND_IND1","CAND_IND2","LISTA_NOMINAL")]

sel <- which(colnames(df)=="ID_ESTADO")
colnames(df)[sel] <- "edon"
sel <- which(colnames(df)=="ID_DISTRITO")
colnames(df)[sel] <- "disn"
sel <- which(colnames(df)=="SECCION")
colnames(df)[sel] <- "seccion"
sel <- which(colnames(df)=="CASILLA")
colnames(df)[sel] <- "casn"
sel <- which(colnames(df)=="ID_MUNICIPIO")
colnames(df)[sel] <- "munn"
sel <- which(colnames(df)=="LISTA_NOMINAL")
colnames(df)[sel] <- "lisnom"
sel <- which(colnames(df)=="PAN")
colnames(df)[sel] <- "pan"
sel <- which(colnames(df)=="PRI")
colnames(df)[sel] <- "pri"
sel <- which(colnames(df)=="PRD")
colnames(df)[sel] <- "prd"
sel <- which(colnames(df)=="PVEM")
colnames(df)[sel] <- "pvem"
sel <- which(colnames(df)=="PT")
colnames(df)[sel] <- "pt"
sel <- which(colnames(df)=="MC")
colnames(df)[sel] <- "mc"
sel <- which(colnames(df)=="NVA_ALIANZA")
colnames(df)[sel] <- "panal"
sel <- which(colnames(df)=="MORENA")
colnames(df)[sel] <- "morena"
sel <- which(colnames(df)=="PH")
colnames(df)[sel] <- "ph"
sel <- which(colnames(df)=="ES")
colnames(df)[sel] <- "pes"
sel <- which(colnames(df)=="PAN_NVA_ALIANZA")
colnames(df)[sel] <- "pan.panal"
sel <- which(colnames(df)=="PRI_PVEM")
colnames(df)[sel] <- "pri.pvem"
sel <- which(colnames(df)=="PRD_PT")
colnames(df)[sel] <- "prd.pt"
sel <- which(colnames(df)=="CAND_IND1")
colnames(df)[sel] <- "indep1"
sel <- which(colnames(df)=="CAND_IND2")
colnames(df)[sel] <- "indep2"

# quita NAs
df[is.na(df)] <- 0

# efec
df$efec <- df$pan + df$pri + df$prd + df$pvem + df$pt + df$mc + df$panal + df$morena + df$ph + df$pes + df$pan.panal + df$pri.pvem + df$prd.pt + df$indep1 + df$indep2

# sólo prd (voto conjunto se lo atribuyo completo)
df$prd15 <- df$prd + df$prd.pt
df <- df[,c("edon","munn","seccion","prd15","efec")]

# consolida secciones
df$efec <- ave(df$efec, as.factor(df$edon*10000+df$seccion), FUN=sum, na.rm=TRUE)
df$prd15 <- ave(df$prd15, as.factor(df$edon*10000+df$seccion), FUN=sum, na.rm=TRUE)
df <- df[duplicated(df$edon*10000+df$seccion)==FALSE, ]

# merge secciones to pr
df2 <- df # duplicate
# shares
df2$prd15 <- df2$prd15/df2$efec
# keep prd only
df2 <- df2[,c("edon","seccion","prd15")]
pr.se <- merge(x=pr.se, y=df2, by=c("edon","seccion"), all.x = TRUE, all.y = FALSE)
rm(df2)

# consolida municipios
df$seccion <- NULL
df$efec <- ave(df$efec, as.factor(df$edon*1000+df$munn), FUN=sum, na.rm=TRUE)
df$prd15 <- ave(df$prd15, as.factor(df$edon*1000+df$munn), FUN=sum, na.rm=TRUE)
df <- df[duplicated(df$edon*1000+df$munn)==FALSE, ]
# shares
df$prd15 <- df$prd15/df$efec

# merge munn
df <- df[,c("edon","munn","prd15")]
pr.mu <- merge(x=pr.mu, y=df, by=c("edon","munn"), all.x = FALSE, all.y = TRUE)
rm(df)

########
# 2018 #
########
pr <- read.csv(file = "eum2018all/eum2018prca.csv", stringsAsFactors = FALSE)

# hace numéricos los votos
pr$edon <- as.numeric(pr$edon)
pr$disn <- as.numeric(pr$disn)
pr$seccion <- as.numeric(pr$seccion)
pr$TOTAL_VOTOS_CALCULADOS <- as.numeric(pr$TOTAL_VOTOS_CALCULADOS)
pr$pan <- as.numeric(pr$pan)
pr$pri <- as.numeric(pr$pri)
pr$prd <- as.numeric(pr$prd)
pr$pvem <- as.numeric(pr$pvem)
pr$pt <- as.numeric(pr$pt)
pr$mc <- as.numeric(pr$mc)
pr$panal <- as.numeric(pr$panal)
pr$morena <- as.numeric(pr$morena)
pr$pes <- as.numeric(pr$pes)
pr$pan.prd.mc <- as.numeric(pr$pan.prd.mc)
pr$pan.prd <- as.numeric(pr$pan.prd)
pr$pan.mc <- as.numeric(pr$pan.mc)
pr$prd.mc <- as.numeric(pr$prd.mc)
pr$pri.pvem.panal <- as.numeric(pr$pri.pvem.panal)
pr$pri.pvem <- as.numeric(pr$pri.pvem)
pr$pri.panal <- as.numeric(pr$pri.panal)
pr$pvem.panal <- as.numeric(pr$pvem.panal)
pr$morena.pt.pes <- as.numeric(pr$morena.pt.pes)
pr$morena.pt <- as.numeric(pr$morena.pt)
pr$pt.pes <- as.numeric(pr$pt.pes)
pr$morena.pes <- as.numeric(pr$morena.pes)
pr$indep1 <- as.numeric(pr$indep1)
pr$indep2 <- as.numeric(pr$indep2)
pr$nr <- as.numeric(pr$nr)
pr$nul <- as.numeric(pr$nul)
pr$lisnom <- as.numeric(pr$lisnom)
             
# cambia NAs por ceros
tmp <- pr[, c("edon", "disn", "seccion", "TOTAL_VOTOS_CALCULADOS", "pan", "pri", "prd", "pvem", "pt", "mc", "panal", "morena", "pes", "pan.prd.mc", "pan.prd", "pan.mc", "prd.mc", "pri.pvem.panal", "pri.pvem", "pri.panal", "pvem.panal", "morena.pt.pes", "morena.pt", "pt.pes", "morena.pes", "indep1", "indep2", "nr", "nul", "lisnom")]
tmp[is.na(tmp)==TRUE] <- 0
pr[, c("edon", "disn", "seccion", "TOTAL_VOTOS_CALCULADOS", "pan", "pri", "prd", "pvem", "pt", "mc", "panal", "morena", "pes", "pan.prd.mc", "pan.prd", "pan.mc", "prd.mc", "pri.pvem.panal", "pri.pvem", "pri.panal", "pvem.panal", "morena.pt.pes", "morena.pt", "pt.pes", "morena.pes", "indep1", "indep2", "nr", "nul", "lisnom")] <- tmp

# consolida amlo
pr$amlo <- pr$morena + pr$pt + pr$pes + pr$morena.pt.pes + pr$morena.pt + pr$morena.pes + pr$pt.pes
pr$morena <- pr$pt <- pr$pes <- pr$morena.pt.pes <- pr$morena.pt <- pr$morena.pes <- pr$pt.pes <- NULL
# consolida meade
pr$jam <- pr$pri + pr$pvem + pr$panal + pr$pri.pvem.panal + pr$pri.pvem + pr$pri.panal + pr$pvem.panal
pr$pri <- pr$pvem <- pr$panal <- pr$pri.pvem.panal <- pr$pri.pvem <- pr$pri.panal <- pr$pvem.panal <- NULL
# consolida anaya
pr$rac <- pr$pan + pr$prd + pr$mc + pr$pan.prd.mc + pr$pan.prd + pr$pan.mc + pr$prd.mc
pr$pan <- pr$prd <- pr$mc <- pr$pan.prd.mc <- pr$pan.prd <- pr$pan.mc <- pr$prd.mc <- NULL

# voto efec
pr$efec <- pr$TOTAL_VOTOS_CALCULADOS - pr$nr - pr$nul
# clean
pr$TOTAL_VOTOS_CALCULADOS <- pr$nr <- pr$nul <- NULL
pr$indep1 <- pr$indep2 <- NULL
#pr$lisnom <- NULL
#pr$cab <- NULL

# consolida distritos
sel <- which(pr$seccion==0)
tmp <- pr[-sel,]
tmp$pan.prd.mc <- tmp$pan.prd.mc + tmp$pan.prd + tmp$pan.mc + tmp$prd.mc
tmp$pan.prd + <- tmp$pan.mc + <- tmp$prd.mc <- NULL
tmp$pri.pvem.panal <- tmp$pri.pvem.panal + tmp$pri.pvem + tmp$pri.panal + tmp$pvem.panal
tmp$pri.pvem <- tmp$pri.panal <- tmp$pvem.panal <- NULL
tmp$morena.pt.pes <- tmp$morena.pt.pes + tmp$morena.pt + tmp$morena.pes + tmp$pt.pes
tmp$morena.pt <- tmp$morena.pes <- tmp$pt.pes <- NULL
#
tmp$pan <- ave(tmp$pan, as.factor(tmp$edon*100+tmp$disn), FUN=sum, na.rm=TRUE)
tmp$pri <- ave(tmp$pri, as.factor(tmp$edon*100+tmp$disn), FUN=sum, na.rm=TRUE)
tmp$prd <- ave(tmp$prd, as.factor(tmp$edon*100+tmp$disn), FUN=sum, na.rm=TRUE)
tmp$pvem <- ave(tmp$pvem, as.factor(tmp$edon*100+tmp$disn), FUN=sum, na.rm=TRUE)
tmp$pt <- ave(tmp$pt, as.factor(tmp$edon*100+tmp$disn), FUN=sum, na.rm=TRUE)
tmp$mc <- ave(tmp$mc, as.factor(tmp$edon*100+tmp$disn), FUN=sum, na.rm=TRUE)
tmp$panal <- ave(tmp$panal, as.factor(tmp$edon*100+tmp$disn), FUN=sum, na.rm=TRUE)
tmp$morena <- ave(tmp$morena, as.factor(tmp$edon*100+tmp$disn), FUN=sum, na.rm=TRUE)
tmp$pes <- ave(tmp$pes, as.factor(tmp$edon*100+tmp$disn), FUN=sum, na.rm=TRUE)
tmp$pan.prd.mc <- ave(tmp$pan.prd.mc, as.factor(tmp$edon*100+tmp$disn), FUN=sum, na.rm=TRUE)
tmp$pri.pvem.panal <- ave(tmp$pri.pvem.panal, as.factor(tmp$edon*100+tmp$disn), FUN=sum, na.rm=TRUE)
tmp$morena.pt.pes <- ave(tmp$morena.pt.pes, as.factor(tmp$edon*100+tmp$disn), FUN=sum, na.rm=TRUE)
tmp$efec <- ave(tmp$efec, as.factor(tmp$edon*100+tmp$disn), FUN=sum, na.rm=TRUE)
tmp$lisnom <- ave(tmp$lisnom, as.factor(tmp$edon*100+tmp$disn), FUN=sum, na.rm=TRUE)
#
tmp <- tmp[duplicated(tmp$edon*100+tmp$disn)==FALSE,]
tmp$seccion <- tmp$casn <- tmp$CASILLA <- NULL
#
write.csv(tmp, file = "eum2018all/eum2018prdf.csv", row.names = FALSE)
prdf <- tmp

# consolida secciones
pr$amlo <- ave(pr$amlo, as.factor(pr$edon*10000+pr$seccion), FUN=sum, na.rm=TRUE)
pr$jam <- ave(pr$jam, as.factor(pr$edon*10000+pr$seccion), FUN=sum, na.rm=TRUE)
pr$rac <- ave(pr$rac, as.factor(pr$edon*10000+pr$seccion), FUN=sum, na.rm=TRUE)
pr$efec <- ave(pr$efec, as.factor(pr$edon*10000+pr$seccion), FUN=sum, na.rm=TRUE)
pr <- pr[duplicated(pr$edon*10000+pr$seccion)==FALSE, ]

# merge munn
pr <- merge(x=sm, y=pr, by=c("edon","seccion"), all.x = FALSE, all.y = TRUE)
#sel <- which(pr$seccion==0)
#pr <- pr[-sel,] # drop votos extranjero
#rm(sel)

# version secciones
pr.duplicata <- pr # duplicate
# shares
pr$amlo18 <- pr$amlo/pr$efec
pr$jam <- pr$jam/pr$efec
pr$rac  <- pr$rac/pr$efec
pr$amlo <- NULL
# clean
pr$disn <- pr$casn <- pr$CASILLA <- NULL

# merge 2012 2018
pr.se$efec12 <- pr.se$efec; pr.se$efec <- NULL
pr$efec18 <- pr$efec; pr$efec <- NULL
pr.se <- merge(x = pr.se, y = pr, by = c("edon","seccion"), all = TRUE)

# consolida municipios
pr <- pr.duplicata
pr$amlo <- ave(pr$amlo, as.factor(pr$edon*1000+pr$munn), FUN=sum, na.rm=TRUE)
pr$jam <- ave(pr$jam, as.factor(pr$edon*1000+pr$munn), FUN=sum, na.rm=TRUE)
pr$rac <- ave(pr$rac, as.factor(pr$edon*1000+pr$munn), FUN=sum, na.rm=TRUE)
pr$efec <- ave(pr$efec, as.factor(pr$edon*1000+pr$munn), FUN=sum, na.rm=TRUE)
pr <- pr[duplicated(pr$edon*1000+pr$munn)==FALSE, ]
pr$seccion <- NULL
# clean
pr$disn <- pr$casn <- pr$CASILLA <- NULL

# shares
pr$amlo18 <- pr$amlo/pr$efec
pr$jam <- pr$jam/pr$efec
pr$rac  <- pr$rac/pr$efec
pr$amlo <- NULL

# merge 2012 2018
pr.mu$efec12 <- pr.mu$efec; pr.mu$efec <- NULL
pr$efec18 <- pr$efec; pr$efec <- NULL
pr.mu <- merge(x = pr.mu, y = pr, by = c("edon","munn"))
rm(pr, pr.duplicata, sm, tmp)

##############
# municipios #
##############
# 1st dif
pr.mu$camlo <- pr.mu$amlo18 - pr.mu$amlo12
pr.mu$cpri  <- pr.mu$jam - pr.mu$pena
pr.mu$cpan  <- pr.mu$rac - pr.mu$jvm 
pr.mu$dcamlopos <- as.numeric(pr.mu$camlo>0)
pr.mu$dcpripos <- as.numeric(pr.mu$cpri>0)
pr.mu$dcpanpos <- as.numeric(pr.mu$cpan>0)
pr.mu$camlopos <- pr.mu$camlo * pr.mu$dcamlopos
pr.mu$cpripos <- pr.mu$cpri * pr.mu$dcpripos
pr.mu$cpanpos <- pr.mu$cpan * pr.mu$dcpanpos
pr.mu$camloneg <- pr.mu$camlo * (1-pr.mu$dcamlopos)
pr.mu$cprineg <- pr.mu$cpri * (1-pr.mu$dcpripos)
pr.mu$cpanneg <- pr.mu$cpan * (1-pr.mu$dcpanpos)

summary(pr.mu$camlo)
summary(pr.mu$cpri)
summary(pr.mu$cpan)
table(pr.mu$dcamlopos)
table(pr.mu$dcpripos)
table(pr.mu$dcpanpos)
summary(pr.mu$camlopos)
summary(pr.mu$cpripos)
summary(pr.mu$cpanpos)
summary(pr.mu$camloneg)
summary(pr.mu$cprineg)
summary(pr.mu$cpanneg)

# winner 2012
tmp <- pr.mu[,c("pena","jvm","amlo12")] # ignores quadri's possible wins
tmp <- apply(tmp, 1, max)
pr.mu$dpenawon <- as.numeric(pr.mu$pena==tmp)
pr.mu$djvmwon <- as.numeric(pr.mu$jvm==tmp)
pr.mu$damlowon <- as.numeric(pr.mu$amlo12==tmp)
table(pr.mu$dpenawon)
table(pr.mu$djvmwon)
table(pr.mu$damlowon)

summary(pr.mu$cpri)
summary(pr.mu$cpri[pr.mu$dpenawon==1])
summary(pr.mu$cpan)
summary(pr.mu$cpan[pr.mu$djvmwon==1])
summary(pr.mu$camlo)
summary(pr.mu$camlo[pr.mu$damlowon==1])

table(pr.mu$jam[pr.mu$dpenawon==1]>pr.mu$amlo18[pr.mu$dpenawon==1] &
      pr.mu$jam[pr.mu$dpenawon==1]>pr.mu$rac[pr.mu$dpenawon==1])

tmp <- table(pr.mu$dpenawon==1); tmp; tmp/sum(tmp)
table(pr.mu$dpenawon==1)
table(pr.mu$dcpripos[pr.mu$dpenawon==1])
table(pr.mu$dcamlopos[pr.mu$dpenawon==1])
table(pr.mu$djvmwon)
table(pr.mu$dcpanpos[pr.mu$djvmwon==1])

x

###############
# Import maps #
###############

# script is adapted from federal district map prep in /data/elecs/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/fed/mapas/0code

# # OJO: when using spTranform in script, use line below for google earth, or next line for OSM/google maps
#x.map <- spTransform(x.map, CRS("+proj=longlat +datum=WGS84"))
#x.map <- spTransform(x.map, osm()) # project to osm native Mercator

## # to use osm backgrounds
library(rJava)
library(OpenStreetMap)
library(rgdal)

# working directory and data/map directories
mapdir <- "~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/fed/shp/disfed2018"                                    # main map directory (shapefile repo)
mapdir2 <- "fed/maps"                                             # will save maps here

###################
# geospatial data #
###################
library(spdep); library(maptools)
# used to determine what datum map is in 
library(rgdal)
#gpclibPermit()

# shave map's bb to exclude pacific island 
shave <- function(m = NA, p = .5, s = 0, eastwest = TRUE){ # m is map to be shaved, p the rightmost part (share) to keep, -1<s<1 a shift rightward, eastwest=FALSE makes shift northsouth
    m <- m; p <- p;
    dim <- ifelse(eastwest==TRUE, 1, 2) 
    b <- as.data.frame(m@bbox)
    b[dim,] <- b[dim,] - s*(b$max[dim] - b$min[dim])       # shift map rightward (bbox leftward)
    b$min[dim] <- b$max[dim] - p*(b$max[dim] - b$min[dim]) # keeps only 100*p% of horizontal length
    m@bbox <- as.matrix(b)
    #ed.map$col <- m
    return(m)
}

################
# national map #
################
ruta <- "/home/eric/Desktop/data/mapas/entidad"
nat.map <- readOGR(dsn = ruta, layer = 'ENTIDAD')
summary(nat.map)
# projects to a different datum with long and lat
nat.map <- spTransform(nat.map, osm()) # project to osm native Mercator

tmp.map <- readOGR(dsn = "/home/eric/Desktop/data/mapas/cartografia28feb2013rojano/cua", layer = 'DISTRITO')
summary(tmp.map@data)


# import 2000 p5li to plug into mu.map one by one
#  plug ife mun nums
tmp.p5 <- "/home/eric/Desktop/data/elecs/MXelsCalendGovt/elecReturns/ancillary/ife_to_inegi.csv"
tmp.p5 <- read.csv(file = tmp.p5, stringsAsFactors = FALSE)
tmp.p5$munn <- tmp.p5$ife - round(tmp.p5$ife/1000, 0)*1000
tmp.p5$inegi <- tmp.p5$ife <- tmp.p5$ord <- tmp.p5$edo <- NULL
#
tmp <- "/home/eric/Desktop/data/elecs/MXelsCalendGovt/censos/p5li/p5li00.csv"
tmp <- read.csv(file = tmp, stringsAsFactors = FALSE)
tmp$p5li <- round(tmp$p5li00 / tmp$p5, 4)
tmp <- tmp[,c("edon","munn","p5li")]
tmp.p5 <- merge(x = tmp.p5, y = tmp)

###################
# municipios maps #
###################
ruta <- file.path(mapdir, edo) # archivo con mapas ine
mu.map <- list()
i <- 1
#
tmp <- file.path(mapdir, "ags") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==i,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.li <- tmp.p5[tmp.p5$edon==i, c("munn","p5li")]
tmp.data <- merge(x = tmp.data, y = tmp.li, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$ags <- tmp
i <- i+1
#
tmp <- file.path(mapdir, "bc") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==i,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.li <- tmp.p5[tmp.p5$edon==i, c("munn","p5li")]
tmp.data <- merge(x = tmp.data, y = tmp.li, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$bc <- tmp
i <- i + 1
#
tmp <- file.path(mapdir, "bcs") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==i,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.li <- tmp.p5[tmp.p5$edon==i, c("munn","p5li")]
tmp.data <- merge(x = tmp.data, y = tmp.li, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$bcs <- tmp
i <- i + 1
#
tmp <- file.path(mapdir, "cam") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==i,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.li <- tmp.p5[tmp.p5$edon==i, c("munn","p5li")]
tmp.data <- merge(x = tmp.data, y = tmp.li, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$cam <- tmp
i <- i + 1
#
tmp <- file.path(mapdir, "coa") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==i,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.li <- tmp.p5[tmp.p5$edon==i, c("munn","p5li")]
tmp.data <- merge(x = tmp.data, y = tmp.li, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$coa <- tmp
i <- i + 1
#
tmp <- file.path(mapdir, "col") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==i,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.li <- tmp.p5[tmp.p5$edon==i, c("munn","p5li")]
tmp.data <- merge(x = tmp.data, y = tmp.li, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$col <- tmp
i <- i + 1
#
tmp <- file.path(mapdir, "cps") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==i,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.li <- tmp.p5[tmp.p5$edon==i, c("munn","p5li")]
tmp.data <- merge(x = tmp.data, y = tmp.li, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$cps <- tmp
i <- i + 1
#
tmp <- file.path(mapdir, "cua") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==i,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.li <- tmp.p5[tmp.p5$edon==i, c("munn","p5li")]
tmp.data <- merge(x = tmp.data, y = tmp.li, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$cua <- tmp
i <- i + 1
#
tmp <- file.path(mapdir, "df") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==i,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.li <- tmp.p5[tmp.p5$edon==i, c("munn","p5li")]
tmp.data <- merge(x = tmp.data, y = tmp.li, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$df <- tmp
i <- i + 1
#
tmp <- file.path(mapdir, "dgo") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==i,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.li <- tmp.p5[tmp.p5$edon==i, c("munn","p5li")]
tmp.data <- merge(x = tmp.data, y = tmp.li, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$dgo <- tmp
i <- i + 1
#
tmp <- file.path(mapdir, "gua") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==i,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.li <- tmp.p5[tmp.p5$edon==i, c("munn","p5li")]
tmp.data <- merge(x = tmp.data, y = tmp.li, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$gua <- tmp
i <- i + 1
#
tmp <- file.path(mapdir, "gue") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==i,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.li <- tmp.p5[tmp.p5$edon==i, c("munn","p5li")]
tmp.data <- merge(x = tmp.data, y = tmp.li, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$gue <- tmp
i <- i + 1
#
tmp <- file.path(mapdir, "hgo") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==i,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.li <- tmp.p5[tmp.p5$edon==i, c("munn","p5li")]
tmp.data <- merge(x = tmp.data, y = tmp.li, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$hgo <- tmp
i <- i + 1
#
tmp <- file.path(mapdir, "jal") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==i,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.li <- tmp.p5[tmp.p5$edon==i, c("munn","p5li")]
tmp.data <- merge(x = tmp.data, y = tmp.li, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$jal <- tmp
i <- i + 1
#
tmp <- file.path(mapdir, "mex") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==i,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.li <- tmp.p5[tmp.p5$edon==i, c("munn","p5li")]
tmp.data <- merge(x = tmp.data, y = tmp.li, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$mex <- tmp
i <- i + 1
#
tmp <- file.path(mapdir, "mic") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==i,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.li <- tmp.p5[tmp.p5$edon==i, c("munn","p5li")]
tmp.data <- merge(x = tmp.data, y = tmp.li, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$mic <- tmp
i <- i + 1
#
tmp <- file.path(mapdir, "mor") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==i,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.li <- tmp.p5[tmp.p5$edon==i, c("munn","p5li")]
tmp.data <- merge(x = tmp.data, y = tmp.li, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$mor <- tmp
i <- i + 1
#
tmp <- file.path(mapdir, "nay") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==i,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.li <- tmp.p5[tmp.p5$edon==i, c("munn","p5li")]
tmp.data <- merge(x = tmp.data, y = tmp.li, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$nay <- tmp
i <- i + 1
#
tmp <- file.path(mapdir, "nl") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==i,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.li <- tmp.p5[tmp.p5$edon==i, c("munn","p5li")]
tmp.data <- merge(x = tmp.data, y = tmp.li, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$nl <- tmp
i <- i + 1
#
tmp <- file.path(mapdir, "oax") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==i,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.li <- tmp.p5[tmp.p5$edon==i, c("munn","p5li")]
tmp.data <- merge(x = tmp.data, y = tmp.li, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$oax <- tmp
i <- i + 1
#
tmp <- file.path(mapdir, "pue") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==i,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.li <- tmp.p5[tmp.p5$edon==i, c("munn","p5li")]
tmp.data <- merge(x = tmp.data, y = tmp.li, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$pue <- tmp
i <- i + 1
#
tmp <- file.path(mapdir, "que") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==i,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.li <- tmp.p5[tmp.p5$edon==i, c("munn","p5li")]
tmp.data <- merge(x = tmp.data, y = tmp.li, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$que <- tmp
i <- i + 1
#
tmp <- file.path(mapdir, "qui") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==i,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.li <- tmp.p5[tmp.p5$edon==i, c("munn","p5li")]
tmp.data <- merge(x = tmp.data, y = tmp.li, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$qui <- tmp
i <- i + 1
#
tmp <- file.path(mapdir, "san") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==i,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.li <- tmp.p5[tmp.p5$edon==i, c("munn","p5li")]
tmp.data <- merge(x = tmp.data, y = tmp.li, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$san <- tmp
i <- i + 1
#
tmp <- file.path(mapdir, "sin") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==i,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.li <- tmp.p5[tmp.p5$edon==i, c("munn","p5li")]
tmp.data <- merge(x = tmp.data, y = tmp.li, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$sin <- tmp
i <- i + 1
#
tmp <- file.path(mapdir, "son") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==i,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.li <- tmp.p5[tmp.p5$edon==i, c("munn","p5li")]
tmp.data <- merge(x = tmp.data, y = tmp.li, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$son <- tmp
i <- i + 1
#
tmp <- file.path(mapdir, "tab") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==i,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.li <- tmp.p5[tmp.p5$edon==i, c("munn","p5li")]
tmp.data <- merge(x = tmp.data, y = tmp.li, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$tab <- tmp
i <- i + 1
#
tmp <- file.path(mapdir, "tam") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==i,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.li <- tmp.p5[tmp.p5$edon==i, c("munn","p5li")]
tmp.data <- merge(x = tmp.data, y = tmp.li, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$tam <- tmp
i <- i + 1
#
tmp <- file.path(mapdir, "tla") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==i,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.li <- tmp.p5[tmp.p5$edon==i, c("munn","p5li")]
tmp.data <- merge(x = tmp.data, y = tmp.li, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$tla <- tmp
i <- i + 1
#
tmp <- file.path(mapdir, "ver") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==i,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.li <- tmp.p5[tmp.p5$edon==i, c("munn","p5li")]
tmp.data <- merge(x = tmp.data, y = tmp.li, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$ver <- tmp
i <- i + 1
#
tmp <- file.path(mapdir, "yuc") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==i,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.li <- tmp.p5[tmp.p5$edon==i, c("munn","p5li")]
tmp.data <- merge(x = tmp.data, y = tmp.li, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$yuc <- tmp
i <- i + 1
#
tmp <- file.path(mapdir, "zac") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==i,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.li <- tmp.p5[tmp.p5$edon==i, c("munn","p5li")]
tmp.data <- merge(x = tmp.data, y = tmp.li, by = "munn", all.x = TRUE, all.y = FALSE)
#
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$zac <- tmp

# party colors
brown <- rgb(139,69,19, maxColorValue = 255) #"saddlebrown"
colpri <- "black"
colpan <- "blue"
colprd <- "gold"

# trial
plot(shave(nat.map, p = .95), lwd = .5, border = "gray")
axis(1); axis(2)



# amlo v pri
library(graphics)
#png("/home/eric/Desktop/MXelsCalendGovt/elecReturns/graph/nytPlusPri.png", width = 20, height = 20, units = "cm", res = 196)
#pdf("/home/eric/Desktop/MXelsCalendGovt/elecReturns/graph/nytPlusPri.pdf", width = 10, height = 7)
par(mar = c(0,0,0,0))
plot(shave(nat.map, p = .95), lwd = .5, border = "gray")
tmp.ranges <- par("usr") # keep calculated xy ranges to compute arrow length
for (i in 1:32){
    plot(mu.map[[i]], lwd = .25, border = "lightgray", add = TRUE)
}
plot(nat.map, lwd = .5,          border = "darkgray", add = TRUE)
plot(nat.map, lwd = .5, lty = 3, border = "white", add = TRUE)
# add arrows
# start-end of arrows
#cx <- 150000; cy <- cx/3
xlength <- (tmp.ranges[2] - tmp.ranges[1]) / 10
for (i in 1:32){
    # start-end of arrows
    #i <- 1 # debug
    cx <- xlength*mu.map[[i]]$camlopos; cy <- cx/3
    arrows(coordinates(mu.map[[i]])[,1],    coordinates(mu.map[[i]])[,2],
           coordinates(mu.map[[i]])[,1]-cx, coordinates(mu.map[[i]])[,2]+cy,
           length = .025, angle = 10,
           col = brown, lwd = (.1+cx/600000))
    cx <- xlength*mu.map[[i]]$cpripos; cy <- cx/3
    arrows(coordinates(mu.map[[i]])[,1],    coordinates(mu.map[[i]])[,2],
           coordinates(mu.map[[i]])[,1]+cx, coordinates(mu.map[[i]])[,2]+cy,
           length = .025, angle = 10,
           col = colpri, lwd = (.1+cx/600000))
}
# legend
xl <-  -10400000; yl <- 3000000
arrows(xl,yl,xl-150000,yl+50000,length = .025, angle = 10, col = brown, lwd = .75)
text(xl, yl, pos = 4, labels = "AMLO creció", cex = .75)
arrows(xl+310000,yl-150000,xl+310000+150000,yl-150000+50000,length = .025, angle = 10, col = colpri, lwd = .75)
text(xl+310000,yl-150000, pos = 2, labels = "Meade creció", cex = .75)
text(xl+180000, yl+280000, labels = "Cambio desde", font = 2)
text(xl+180000, yl+180000, labels = "2012 en municipios", font = 2)
text(-13000000, 1550000, labels = "Preparado por Eric Magar con datos del INE (@emagar)", col = "lightgray", pos = 4, cex = .65)
#dev.off()

# anaya v neg prd 2015
library(graphics)
#png("/home/eric/Desktop/MXelsCalendGovt/elecReturns/graph/nytPlusPan.png", width = 20, height = 20, units = "cm", res = 196)
#pdf("/home/eric/Desktop/MXelsCalendGovt/elecReturns/graph/nytPlusPan.pdf", width = 10, height = 7)
par(mar = c(0,0,0,0))
plot(shave(nat.map, p = .95), lwd = .5, border = "gray")
tmp.ranges <- par("usr") # keep calculated xy ranges to compute arrow length
for (i in 1:32){
    plot(mu.map[[i]], lwd = .25, border = "lightgray", add = TRUE)
}
plot(nat.map, lwd = .5,          border = "darkgray", add = TRUE)
plot(nat.map, lwd = .5, lty = 3, border = "white", add = TRUE)
# add arrows
# start-end of arrows
#cx <- 150000; cy <- cx/3
xlength <- (tmp.ranges[2] - tmp.ranges[1]) / 10
for (i in 1:32){
    # start-end of arrows
    #i <- 1 # debug
    cx <- xlength*mu.map[[i]]$prd15; cy <- cx/3
    arrows(coordinates(mu.map[[i]])[,1],    coordinates(mu.map[[i]])[,2],
           coordinates(mu.map[[i]])[,1]-cx, coordinates(mu.map[[i]])[,2]-cy,
           length = .025, angle = 10,
           col = colprd, lwd = (.1+cx/600000))
    cx <- xlength*mu.map[[i]]$cpanpos; cy <- cx/3
    arrows(coordinates(mu.map[[i]])[,1],    coordinates(mu.map[[i]])[,2],
           coordinates(mu.map[[i]])[,1]+cx, coordinates(mu.map[[i]])[,2]+cy,
           length = .025, angle = 10,
           col = colpan, lwd = (.1+cx/600000))
}
# legend
xl <-  -10400000; yl <- 3000000
arrows(xl,yl-150000,xl-150000,yl-150000-50000,length = .025, angle = 10, col = colprd, lwd = .75)
text(xl, yl-150000, pos = 4, labels = "el PRD en 2015", cex = .75)
arrows(xl+310000,yl,xl+310000+150000,yl+50000,length = .025, angle = 10, col = colpan, lwd = .75)
text(xl+310000,yl, pos = 2, labels = "Anaya superó a JVM", cex = .75)
#text(xl+180000, yl+280000, labels = "Cambio desde", font = 2)
text(xl+180000, yl+180000, labels = "Cambio en municipios", font = 2)
text(-13000000, 1550000, labels = "Preparado por Eric Magar con datos del INE (@emagar)", col = "lightgray", pos = 4, cex = .65)
#dev.off()

# meade donde peña ganó
library(graphics)
#png("/home/eric/Desktop/MXelsCalendGovt/elecReturns/graph/meade-pena-won.png", width = 20, height = 20, units = "cm", res = 196)
#pdf("/home/eric/Desktop/MXelsCalendGovt/elecReturns/graph/meade-pena-won.pdf", width = 10, height = 7)
par(mar = c(0,0,0,0))
plot(shave(nat.map, p = .95), lwd = .5, border = "gray")
tmp.ranges <- par("usr") # keep calculated xy ranges to compute arrow length
for (i in 1:32){
    plot(mu.map[[i]], lwd = .25, border = "lightgray", add = TRUE)
}
plot(nat.map, lwd = .5,          border = "darkgray", add = TRUE)
plot(nat.map, lwd = .5, lty = 3, border = "white", add = TRUE)
# add arrows
# start-end of arrows
#cx <- 150000; cy <- cx/3
xlength <- (tmp.ranges[2] - tmp.ranges[1]) / 10
for (i in 1:32){
    # start-end of arrows
    #i <- 19 # debug
    cy <- xlength*mu.map[[i]]$cprineg; cx <- cy*0
    cy[mu.map[[i]]$dpenawon==0] <- NA; cx[mu.map[[i]]$dpenawon==0] <- NA # keep only where Peña won
    arrows(coordinates(mu.map[[i]])[,1],    coordinates(mu.map[[i]])[,2],
           coordinates(mu.map[[i]])[,1]+cx, coordinates(mu.map[[i]])[,2]+cy,
           length = .025, angle = 10,
           col = "red", lwd = (.1+cx/5000))
    cy <- xlength*mu.map[[i]]$cpripos; cx <- cy*0
    cy[mu.map[[i]]$dpenawon==0] <- NA; cx[mu.map[[i]]$dpenawon==0] <- NA # keep only where Peña won
    arrows(coordinates(mu.map[[i]])[,1],    coordinates(mu.map[[i]])[,2],
           coordinates(mu.map[[i]])[,1]+cx, coordinates(mu.map[[i]])[,2]+cy,
           length = .025, angle = 10,
           col = "black", lwd = (.1+cx/5000))
}
# legend
xl <-  -10400000; yl <- 3000000
arrows(xl,yl,xl,yl+75000,length = .025, angle = 10, col = "black", lwd = .75)
text(xl, yl, pos = 4, labels = "Meade creció", cex = .75)
arrows(xl+310000,yl-150000,xl+310000,yl-150000-75000,length = .025, angle = 10, col = "red", lwd = .75)
text(xl+310000,yl-150000, pos = 2, labels = "Meade cayó", cex = .75)
text(xl+180000, yl+280000, labels = "Cambio en municipios", font = 2)
text(xl+180000, yl+180000, labels = "que ganó Peña", font = 2)
text(-13000000, 1550000, labels = "Preparado por Eric Magar con datos del INE (@emagar)", col = "lightgray", pos = 4, cex = .65)
#dev.off()

# amlo v meade-neg v anaya donde peña ganó
library(graphics)
#png("/home/eric/Desktop/MXelsCalendGovt/elecReturns/graph/nytAmloPlusAnayaPlusMeadeNegPenaWon.png", width = 20, height = 20, units = "cm", res = 196)
#pdf("/home/eric/Desktop/MXelsCalendGovt/elecReturns/graph/nytAmloPlusAnayaPlusMeadeNegPenaWon.pdf", width = 10, height = 7)
par(mar = c(0,0,0,0))
plot(shave(nat.map, p = .95), lwd = .5, border = "gray")
tmp.ranges <- par("usr") # keep calculated xy ranges to compute arrow length
for (i in 1:32){
    plot(mu.map[[i]], lwd = .25, border = "lightgray", add = TRUE)
}
plot(nat.map, lwd = .5,          border = "darkgray", add = TRUE)
plot(nat.map, lwd = .5, lty = 3, border = "white", add = TRUE)
# add arrows
# start-end of arrows
#cx <- 150000; cy <- cx/3
xlength <- (tmp.ranges[2] - tmp.ranges[1]) / 10
for (i in 1:32){
    points(coordinates(mu.map[[i]]), cex = .05, col = "darkgray", pch = 20) 
    # start-end of arrows
    #i <- 15 # debug
    cx <- xlength*mu.map[[i]]$camlopos; cy <- cx/3
    cx[mu.map[[i]]$dpenawon==0] <- NA; cy[mu.map[[i]]$dpenawon==0] <- NA # keep only where Peña won
    arrows(coordinates(mu.map[[i]])[,1],    coordinates(mu.map[[i]])[,2],
           coordinates(mu.map[[i]])[,1]-cx, coordinates(mu.map[[i]])[,2]+cy,
           length = .025, angle = 10,
           col = brown, lwd = (.1+cx/600000))
    cx <- -xlength*mu.map[[i]]$cprineg; cy <- cx; cx <- cx*0
    cx[mu.map[[i]]$dpenawon==0] <- NA; cy[mu.map[[i]]$dpenawon==0] <- NA # keep only where Peña won
    arrows(coordinates(mu.map[[i]])[,1],    coordinates(mu.map[[i]])[,2],
           coordinates(mu.map[[i]])[,1]+cx, coordinates(mu.map[[i]])[,2]-cy,
           length = .025, angle = 10,
           col = "red", lwd = (.1+cy/600000))
    cx <- xlength*mu.map[[i]]$cpanpos; cy <- cx/3
    cx[mu.map[[i]]$dpenawon==0] <- NA; cy[mu.map[[i]]$dpenawon==0] <- NA # keep only where Peña won
    arrows(coordinates(mu.map[[i]])[,1],    coordinates(mu.map[[i]])[,2],
           coordinates(mu.map[[i]])[,1]+cx, coordinates(mu.map[[i]])[,2]+cy,
           length = .025, angle = 10,
           col = colpan, lwd = (.1+cx/600000))
}
# legend
xl <-  -10200000; yl <- 3200000
points(xl, yl, cex = .5, col = "darkgray", pch = 20) 
#       
arrows(xl,yl,xl-150000,yl+50000,length = .025, angle = 10, col = brown, lwd = .75)
arrows(xl,yl,xl+150000,yl+50000,length = .025, angle = 10, col = colpan, lwd = .75)
arrows(xl,yl,xl,yl-150000,length = .025, angle = 10, col = "red", lwd = .75)
#
text(xl-150000,yl+50000, pos = 2, labels = "AMLO creció", cex = .75)
text(xl+150000,yl+50000, pos = 4, labels = "Anaya creció", cex = .75)
text(xl,yl-150000, pos = 1, labels = "Meade decreció", cex = .75)
#
text(xl, yl+380000, labels = "Cambio en municipios", font = 2)
text(xl, yl+280000, labels = "que ganó Peña", font = 2)
text(-13000000, 1550000, labels = "Preparado por Eric Magar con datos del INE (@emagar)", col = "lightgray", pos = 4, cex = .65)
#dev.off()


# anaya donde chepina ganó
library(graphics)
#png("/home/eric/Desktop/MXelsCalendGovt/elecReturns/graph/anaya-jvm-won.png", width = 20, height = 20, units = "cm", res = 196)
#pdf("/home/eric/Desktop/MXelsCalendGovt/elecReturns/graph/anaya-jvm-won.pdf", width = 10, height = 7)
par(mar = c(0,0,0,0))
plot(shave(nat.map, p = .95), lwd = .5, border = "gray")
tmp.ranges <- par("usr") # keep calculated xy ranges to compute arrow length
for (i in 1:32){
    plot(mu.map[[i]], lwd = .25, border = "lightgray", add = TRUE)
}
plot(nat.map, lwd = .5,          border = "darkgray", add = TRUE)
plot(nat.map, lwd = .5, lty = 3, border = "white", add = TRUE)
# add arrows
# start-end of arrows
#cx <- 150000; cy <- cx/3
xlength <- (tmp.ranges[2] - tmp.ranges[1]) / 7
for (i in 1:32){
    # start-end of arrows
    #i <- 19 # debug
    cy <- xlength*mu.map[[i]]$cpanneg; cx <- cy*0
    cy[mu.map[[i]]$djvmwon==0] <- NA; cx[mu.map[[i]]$djvmwon==0] <- NA # keep only where JVM won
    arrows(coordinates(mu.map[[i]])[,1],    coordinates(mu.map[[i]])[,2],
           coordinates(mu.map[[i]])[,1]+cx, coordinates(mu.map[[i]])[,2]+cy,
           length = .025, angle = 10,
           col = "red", lwd = (.1+cx/5000))
    cy <- xlength*mu.map[[i]]$cpanpos; cx <- cy*0
    cy[mu.map[[i]]$djvmwon==0] <- NA; cx[mu.map[[i]]$djvmwon==0] <- NA # keep only where JVM won
    arrows(coordinates(mu.map[[i]])[,1],    coordinates(mu.map[[i]])[,2],
           coordinates(mu.map[[i]])[,1]+cx, coordinates(mu.map[[i]])[,2]+cy,
           length = .025, angle = 10,
           col = "black", lwd = (.1+cx/5000))
}
# legend
xl <-  -10400000; yl <- 3000000
arrows(xl,yl,xl,yl+75000,length = .025, angle = 10, col = "black", lwd = .75)
text(xl, yl, pos = 4, labels = "Anaya creció", cex = .75)
arrows(xl+310000,yl-150000,xl+310000,yl-150000-75000,length = .025, angle = 10, col = "red", lwd = .75)
text(xl+310000,yl-150000, pos = 2, labels = "Anaya cayó", cex = .75)
text(xl+180000, yl+280000, labels = "Cambio en municipios", font = 2)
text(xl+180000, yl+180000, labels = "que Josefina ganó", font = 2)
text(-13000000, 1550000, labels = "Preparado por Eric Magar con datos del INE (@emagar)", col = "lightgray", pos = 4, cex = .65)
#dev.off()

# meade v amlo in indigenous municipios only
sel <- list()
for (i in 1:32){
    tmp <- which(mu.map[[i]]$p5li > .40)
    sel[[i]] <- tmp
}
#
mu.map2 <- mu.map # duplicate
for (i in 1:32){
    if (length(sel[[i]])==0) next
    mu.map2[[i]] <- mu.map[[i]][sel[[i]],] # subset municipios indigenas
}
#
library(graphics)
#png("/home/eric/Desktop/MXelsCalendGovt/elecReturns/graph/nytPlusPriIndig.png", width = 20, height = 20, units = "cm", res = 196)
#pdf("/home/eric/Desktop/MXelsCalendGovt/elecReturns/graph/nytPlusPriIndig.pdf", width = 10, height = 7)
par(mar = c(0,0,0,0))
plot(shave(nat.map, p = .95), lwd = .5, border = "gray")
tmp.ranges <- par("usr") # keep calculated xy ranges to compute arrow length
for (i in 1:32){
    plot(mu.map[[i]], lwd = .25, border = "lightgray", add = TRUE)
    if (length(sel[[i]])==0) next
    plot(mu.map2[[i]], lwd = .25, border = "lightgray", col = "gray", add = TRUE)
}
plot(nat.map, lwd = .5,          border = "darkgray", add = TRUE)
plot(nat.map, lwd = .5, lty = 3, border = "white", add = TRUE)
# add arrows
# start-end of arrows
#cx <- 150000; cy <- cx/3
xlength <- (tmp.ranges[2] - tmp.ranges[1]) / 10
for (i in 1:32){
    if (length(sel[[i]])==0) next
    # start-end of arrows
    #i <- 1 # debug
    cx <- xlength*mu.map2[[i]]$camlopos; cy <- cx/3
    arrows(coordinates(mu.map2[[i]])[,1],    coordinates(mu.map2[[i]])[,2],
           coordinates(mu.map2[[i]])[,1]-cx, coordinates(mu.map2[[i]])[,2]+cy,
           length = .025, angle = 10,
           col = brown, lwd = (.1+cx/600000))
    cx <- xlength*mu.map2[[i]]$cpripos; cy <- cx/3
    arrows(coordinates(mu.map2[[i]])[,1],    coordinates(mu.map2[[i]])[,2],
           coordinates(mu.map2[[i]])[,1]+cx, coordinates(mu.map2[[i]])[,2]+cy,
           length = .025, angle = 10,
           col = colpri, lwd = (.1+cx/600000))
}
# legend
xl <-  -10400000; yl <- 3000000
arrows(xl,yl,xl-150000,yl+50000,length = .025, angle = 10, col = brown, lwd = .75)
text(xl, yl, pos = 4, labels = "AMLO creció", cex = .75)
arrows(xl+310000,yl-150000,xl+310000+150000,yl-150000+50000,length = .025, angle = 10, col = colpri, lwd = .75)
text(xl+310000,yl-150000, pos = 2, labels = "Meade creció", cex = .75)
text(xl+180000, yl+280000, labels = "Cambio desde", font = 2)
text(xl+180000, yl+180000, labels = "2012 en municipios", font = 2)
text(-13000000, 1550000, labels = "Preparado por Eric Magar con datos del INE (@emagar)", col = "lightgray", pos = 4, cex = .65)
#dev.off()

# anaya v amlo in indigenous municipios only
sel <- list()
for (i in 1:32){
    tmp <- which(mu.map[[i]]$p5li > .40)
    sel[[i]] <- tmp
}
#
mu.map2 <- mu.map # duplicate
for (i in 1:32){
    if (length(sel[[i]])==0) next
    mu.map2[[i]] <- mu.map[[i]][sel[[i]],] # subset municipios indigenas
}
#
library(graphics)
#png("/home/eric/Desktop/MXelsCalendGovt/elecReturns/graph/nytPlusPanIndig.png", width = 20, height = 20, units = "cm", res = 196)
#pdf("/home/eric/Desktop/MXelsCalendGovt/elecReturns/graph/nytPlusPanIndig.pdf", width = 10, height = 7)
par(mar = c(0,0,0,0))
plot(shave(nat.map, p = .95), lwd = .5, border = "gray")
tmp.ranges <- par("usr") # keep calculated xy ranges to compute arrow length
for (i in 1:32){
    plot(mu.map[[i]], lwd = .25, border = "lightgray", add = TRUE)
    if (length(sel[[i]])==0) next
    plot(mu.map2[[i]], lwd = .25, border = "lightgray", col = "gray", add = TRUE)
}
plot(nat.map, lwd = .5,          border = "darkgray", add = TRUE)
plot(nat.map, lwd = .5, lty = 3, border = "white", add = TRUE)
# add arrows
# start-end of arrows
#cx <- 150000; cy <- cx/3
xlength <- (tmp.ranges[2] - tmp.ranges[1]) / 10
for (i in 1:32){
    if (length(sel[[i]])==0) next
    # start-end of arrows
    #i <- 1 # debug
    cx <- xlength*mu.map2[[i]]$camlopos; cy <- cx/3
    arrows(coordinates(mu.map2[[i]])[,1],    coordinates(mu.map2[[i]])[,2],
           coordinates(mu.map2[[i]])[,1]-cx, coordinates(mu.map2[[i]])[,2]+cy,
           length = .025, angle = 10,
           col = brown, lwd = (.1+cx/600000))
    cx <- xlength*mu.map2[[i]]$cpanpos; cy <- cx/3
    arrows(coordinates(mu.map2[[i]])[,1],    coordinates(mu.map2[[i]])[,2],
           coordinates(mu.map2[[i]])[,1]+cx, coordinates(mu.map2[[i]])[,2]+cy,
           length = .025, angle = 10,
           col = colpan, lwd = (.1+cx/600000))
}
# legend
xl <-  -10400000; yl <- 3000000
arrows(xl,yl,xl-150000,yl+50000,length = .025, angle = 10, col = brown, lwd = .75)
text(xl, yl, pos = 4, labels = "AMLO creció", cex = .75)
arrows(xl+310000,yl-150000,xl+310000+150000,yl-150000+50000,length = .025, angle = 10, col = colpan, lwd = .75)
text(xl+310000,yl-150000, pos = 2, labels = "Anaya creció", cex = .75)
text(xl+180000, yl+280000, labels = "Cambio desde", font = 2)
text(xl+180000, yl+180000, labels = "2012 en municipios", font = 2)
text(-13000000, 1550000, labels = "Preparado por Eric Magar con datos del INE (@emagar)", col = "lightgray", pos = 4, cex = .65)
#dev.off()




##################
##################
# SECCIONES MAPS #
##################
##################

# 1st dif
pr.se$camlo <- pr.se$amlo18 - pr.se$amlo12
pr.se$cpri  <- pr.se$jam - pr.se$pena
pr.se$cpan  <- pr.se$rac - pr.se$jvm 
pr.se$dcamlopos <- as.numeric(pr.se$camlo>0)
pr.se$dcpripos <- as.numeric(pr.se$cpri>0)
pr.se$dcpanpos <- as.numeric(pr.se$cpan>0)
pr.se$camlopos <- pr.se$camlo * pr.se$dcamlopos
pr.se$cpripos <- pr.se$cpri * pr.se$dcpripos
pr.se$cpanpos <- pr.se$cpan * pr.se$dcpanpos
pr.se$camloneg <- pr.se$camlo * (1-pr.se$dcamlopos)
pr.se$cprineg <- pr.se$cpri * (1-pr.se$dcpripos)
pr.se$cpanneg <- pr.se$cpan * (1-pr.se$dcpanpos)

summary(pr.se$camlo)
summary(pr.se$cpri)
summary(pr.se$cpan)
table(pr.se$dcamlopos)
table(pr.se$dcpripos)
table(pr.se$dcpanpos)
summary(pr.se$camlopos[pr.se$camlopos>0])
summary(pr.se$cpripos[pr.se$cpripos>0])
summary(pr.se$cpanpos[pr.se$cpanpos>0])
summary(pr.se$camloneg[pr.se$camloneg<0])
summary(pr.se$cprineg[pr.se$cprineg<0])
summary(pr.se$cpanneg[pr.se$cpanneg<0])

# choose state number
edon <- 31
edos <- c("ags","bc","bcs","cam","coa","col","cps","cua","df","dgo","gua","gue","hgo","jal","mex","mic","mor","nay","nl","oax","pue","que","qui","san","sin","son","tab","tam","tla","ver","yuc","zac")
edo <- edos[edon]
estado <- c("Aguascalientes","Baja California","Baja California Sur","Campeche","Coahuila","Colima","Chiapas","Chihuahua","Ciudad de México","Durango","Guanajuato","Guerrero","Hidalgo","Jalisco","México","Michoacán","Morelos","Nayarit","Nuevo León","Oaxaca","Puebla","Querétaro","Quintana Roo","San Luis Potosí","Sinaloa","Sonora","Tabasco","Tamaulipas","Tlacala","Veracruz","Yucatán","Zacatecas")[edon]

#################
# secciones map #
#################
ruta <- file.path(mapdir, edo) # archivo con mapas ine
se.map <- readOGR(dsn = ruta, layer = 'SECCION')
summary(se.map)
# projects to a different datum with long and lat
se.map <- spTransform(se.map, osm()) # project to osm native Mercator
# add pr data
tmp.data <- se.map@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$edon <- tmp.data$entidad; tmp.data$munn <- tmp.data$municipio
tmp.pr <- pr.se[pr.se$edon==edon,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "seccion", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
se.map@data <- tmp.data
rm(tmp.data, tmp.pr)

###################
# municipios maps #
###################
tmp <- file.path(mapdir, edo) # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
mu.map <- tmp

#################
# state borders #
#################
tmp <- file.path(mapdir, edo) # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
ed.map <- tmp

# trial
#plot(shave(mu.map, p = .95), lwd = .5, border = "gray")
plot(mu.map, lwd = .5, border = "gray", main = estado)
axis(1); axis(2)

# legend parameters
## xl <- .135 # general legend location as share of xrange
## yl <- .07  # general legend location as share of yrange
xl <- .25
yl <- .75
#    
## xn <- .9  # note location as share of xrange
## yn <- .95 # note location as share of xrange
xn <- .5 
yn <- .95

###################################
# amlo v pri secciones por estado #
###################################
library(graphics)
#png(paste("/home/eric/Desktop/MXelsCalendGovt/elecReturns/graph/nytPlusPri-", edo, ".png", sep = ""), width = 20, height = 20, units = "cm", res = 196)
#pdf(paste("/home/eric/Desktop/MXelsCalendGovt/elecReturns/graph/nytPlusPri-", edo, ".pdf", sep = ""), width = 10, height = 7)
par(mar = c(0,0,2,0))
#plot(shave(se.map, p = .16), lwd = .5, border = "lightgray", main = estado)
plot(se.map, lwd = .25, border = "lightgray", main = estado)
tmp.ranges <- par("usr") # keep calculated xy ranges to compute arrow length
plot(mu.map, lwd = .5,          border = "darkgray", add = TRUE)
plot(mu.map, lwd = .5, lty = 3, border = "white", add = TRUE)
#plot(ed.map, lwd = .5, lty = 3, border = "gray", add = TRUE)
# add arrows
# start-end of arrows
#cx <- 150000; cy <- cx/3
xlength <- (tmp.ranges[2] - tmp.ranges[1]) / 10
#
# start-end of arrows
cx <- xlength*se.map$camlopos; cy <- cx/3
arrows(coordinates(se.map)[,1],    coordinates(se.map)[,2],
       coordinates(se.map)[,1]-cx, coordinates(se.map)[,2]+cy,
       length = .025, angle = 10,
       col = brown, lwd = (.1+cx/(xlength*.4)))
cx <- xlength*se.map$cpripos; cy <- cx/3
arrows(coordinates(se.map)[,1],    coordinates(se.map)[,2],
       coordinates(se.map)[,1]+cx, coordinates(se.map)[,2]+cy,
       length = .025, angle = 10,
       col = colpri, lwd = (.1+cx/(xlength*.4)))
# legend
xll <- tmp.ranges[2] - (tmp.ranges[2] - tmp.ranges[1]) * (xl+.065)
yll <- tmp.ranges[4] - (tmp.ranges[4] - tmp.ranges[3]) * (yl+.08)
arrows(xll,yll,xll-xlength*.5,yll+xlength*.5/3,length = .025, angle = 10, col = brown, lwd = 2)
text(xll, yll, pos = 4, labels = "AMLO creció", cex = .75)
#
xll <- tmp.ranges[2] - (tmp.ranges[2] - tmp.ranges[1]) * (xl-.065)
yll <- tmp.ranges[4] - (tmp.ranges[4] - tmp.ranges[3]) * (yl+.11)
arrows(xll,yll,xll+xlength*.5,yll+xlength*.5/3,length = .025, angle = 10, col = colpri, lwd = 2)
text(xll,yll, pos = 2, labels = "Meade creció", cex = .75)
#
xll <- tmp.ranges[2] - (tmp.ranges[2] - tmp.ranges[1]) *  xl
yll <- tmp.ranges[4] - (tmp.ranges[4] - tmp.ranges[3]) *  yl
text(xll, yll, labels = "Cambio desde", font = 2)
xll <- tmp.ranges[2] - (tmp.ranges[2] - tmp.ranges[1]) *  xl
yll <- tmp.ranges[4] - (tmp.ranges[4] - tmp.ranges[3]) * (yl+.03)
text(xll, yll, labels = "2012 en secciones", font = 2)
#
## xll <- tmp.ranges[2] - (tmp.ranges[2] - tmp.ranges[1])*.9
## yll <- tmp.ranges[4] - (tmp.ranges[4] - tmp.ranges[3])*.95
xll <- tmp.ranges[2] - (tmp.ranges[2] - tmp.ranges[1])*xn
yll <- tmp.ranges[4] - (tmp.ranges[4] - tmp.ranges[3])*yn
text(xll, yll, labels = "Preparado por Eric Magar con datos del INE (@emagar)", col = "lightgray", cex = .65)
#dev.off()

###################################
# amlo v pan secciones por estado #
###################################
library(graphics)
#png(paste("/home/eric/Desktop/MXelsCalendGovt/elecReturns/graph/nytPlusPan-", edo, ".png", sep = ""), width = 20, height = 20, units = "cm", res = 196)
#pdf(paste("/home/eric/Desktop/MXelsCalendGovt/elecReturns/graph/nytPlusPan-", edo, ".pdf", sep = ""), width = 10, height = 7)
par(mar = c(0,0,2,0))
#plot(shave(se.map, p = .16), lwd = .5, border = "lightgray", main = estado)
plot(se.map, lwd = .25, border = "lightgray", main = estado)
tmp.ranges <- par("usr") # keep calculated xy ranges to compute arrow length
plot(mu.map, lwd = .5,          border = "darkgray", add = TRUE)
plot(mu.map, lwd = .5, lty = 3, border = "white", add = TRUE)
#plot(ed.map, lwd = .5, lty = 3, border = "gray", add = TRUE)
# add arrows
# start-end of arrows
#cx <- 150000; cy <- cx/3
xlength <- (tmp.ranges[2] - tmp.ranges[1]) / 10
#
# start-end of arrows
cx <- xlength*se.map$camlopos; cy <- cx/3
arrows(coordinates(se.map)[,1],    coordinates(se.map)[,2],
       coordinates(se.map)[,1]-cx, coordinates(se.map)[,2]+cy,
       length = .025, angle = 10,
       col = brown, lwd = (.1+cx/(xlength*.4)))
cx <- xlength*se.map$cpanpos; cy <- cx/3
arrows(coordinates(se.map)[,1],    coordinates(se.map)[,2],
       coordinates(se.map)[,1]+cx, coordinates(se.map)[,2]+cy,
       length = .025, angle = 10,
       col = colpan, lwd = (.1+cx/(xlength*.4)))
# legend
xll <- tmp.ranges[2] - (tmp.ranges[2] - tmp.ranges[1]) * (xl+.065)
yll <- tmp.ranges[4] - (tmp.ranges[4] - tmp.ranges[3]) * (yl+.08)
arrows(xll,yll,xll-xlength*.5,yll+xlength*.5/3,length = .025, angle = 10, col = brown, lwd = 2)
text(xll, yll, pos = 4, labels = "AMLO creció", cex = .75)
#
xll <- tmp.ranges[2] - (tmp.ranges[2] - tmp.ranges[1]) * (xl-.065)
yll <- tmp.ranges[4] - (tmp.ranges[4] - tmp.ranges[3]) * (yl+.11)
arrows(xll,yll,xll+xlength*.5,yll+xlength*.5/3,length = .025, angle = 10, col = colpan, lwd = 2)
text(xll,yll, pos = 2, labels = "Anaya creció", cex = .75)
#
xll <- tmp.ranges[2] - (tmp.ranges[2] - tmp.ranges[1]) *  xl
yll <- tmp.ranges[4] - (tmp.ranges[4] - tmp.ranges[3]) *  yl
text(xll, yll, labels = "Cambio desde", font = 2)
xll <- tmp.ranges[2] - (tmp.ranges[2] - tmp.ranges[1]) *  xl
yll <- tmp.ranges[4] - (tmp.ranges[4] - tmp.ranges[3]) * (yl+.03)
text(xll, yll, labels = "2012 en secciones", font = 2)
#
xll <- tmp.ranges[2] - (tmp.ranges[2] - tmp.ranges[1])*xn
yll <- tmp.ranges[4] - (tmp.ranges[4] - tmp.ranges[3])*yn
text(xll, yll, labels = "Preparado por Eric Magar con datos del INE (@emagar)", col = "lightgray", cex = .65)
#dev.off()


######################################
# pan v neg prd secciones por estado #
######################################
library(graphics)
#png(paste("/home/eric/Desktop/MXelsCalendGovt/elecReturns/graph/nytNegPrd-", edo, ".png", sep = ""), width = 20, height = 20, units = "cm", res = 196)
#pdf(paste("/home/eric/Desktop/MXelsCalendGovt/elecReturns/graph/nytNegPrd-", edo, ".pdf", sep = ""), width = 10, height = 7)
par(mar = c(0,0,2,0))
#plot(shave(se.map, p = .16), lwd = .5, border = "lightgray", main = estado)
plot(se.map, lwd = .25, border = "lightgray", main = estado)
tmp.ranges <- par("usr") # keep calculated xy ranges to compute arrow length
plot(mu.map, lwd = .5,          border = "darkgray", add = TRUE)
plot(mu.map, lwd = .5, lty = 3, border = "white", add = TRUE)
#plot(ed.map, lwd = .5, lty = 3, border = "gray", add = TRUE)
# add arrows
# start-end of arrows
#cx <- 150000; cy <- cx/3
xlength <- (tmp.ranges[2] - tmp.ranges[1]) / 10
#
# start-end of arrows
cx <- xlength*se.map$prd15; cy <- cx/3
arrows(coordinates(se.map)[,1],    coordinates(se.map)[,2],
       coordinates(se.map)[,1]-cx, coordinates(se.map)[,2]-cy,
       length = 0, angle = 10,
       col = colprd, lwd = (.1+cx/(xlength*.4)))
cx <- xlength*se.map$cpanpos; cy <- cx/3
arrows(coordinates(se.map)[,1],    coordinates(se.map)[,2],
       coordinates(se.map)[,1]+cx, coordinates(se.map)[,2]+cy,
       length = .025, angle = 10,
       col = colpan, lwd = (.1+cx/(xlength*.4)))
# legend
xll <- tmp.ranges[2] - (tmp.ranges[2] - tmp.ranges[1]) * (xl-.065)
yll <- tmp.ranges[4] - (tmp.ranges[4] - tmp.ranges[3]) * (yl+.08)
arrows(xll,yll,xll+xlength*.5,yll+xlength*.5/3,length = .025, angle = 10, col = colpan, lwd = 2)
text(xll, yll, pos = 2, labels = "Anaya creció", cex = .75)
#
xll <- tmp.ranges[2] - (tmp.ranges[2] - tmp.ranges[1]) * (xl+.065)
yll <- tmp.ranges[4] - (tmp.ranges[4] - tmp.ranges[3]) * (yl+.11)
arrows(xll,yll,xll-xlength*.5,yll-xlength*.5/3,length = 0, angle = 10, col = colprd, lwd = 2)
text(xll,yll, pos = 4, labels = "PRD en 2015", cex = .75)
#
xll <- tmp.ranges[2] - (tmp.ranges[2] - tmp.ranges[1]) *  xl
yll <- tmp.ranges[4] - (tmp.ranges[4] - tmp.ranges[3]) * (yl+.03)
text(xll, yll, labels = "Cambio en secciones", font = 2)
#
xll <- tmp.ranges[2] - (tmp.ranges[2] - tmp.ranges[1])*xn
yll <- tmp.ranges[4] - (tmp.ranges[4] - tmp.ranges[3])*yn
text(xll, yll, labels = "Preparado por Eric Magar con datos del INE (@emagar)", col = "lightgray", cex = .65)
#dev.off()


# Export to javascript



