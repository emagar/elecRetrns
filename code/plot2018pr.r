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
pr$indep1 <- pr$indep2 <- pr$lisnom <- NULL
pr$cab <- NULL

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
pr.mu$dcamlopos <- as.numeric(pr.mu$camlo>0)
pr.mu$dcpripos <- as.numeric(pr.mu$cpri>0)
pr.mu$camlopos <- pr.mu$camlo * pr.mu$dcamlopos
pr.mu$cpripos <- pr.mu$cpri * pr.mu$dcpripos
pr.mu$camloneg <- pr.mu$camlo * (1-pr.mu$dcamlopos)
pr.mu$cprineg <- pr.mu$cpri * (1-pr.mu$dcpripos)

summary(pr.mu$camlo)
summary(pr.mu$cpri)
table(pr.mu$dcamlopos)
table(pr.mu$dcpripos)
summary(pr.mu$camlopos)
summary(pr.mu$cpripos)
summary(pr.mu$camloneg)
summary(pr.mu$cprineg)


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

# choose state number
edon <- 9
edos <- c("ags","bc","bcs","cam","coa","col","cps","cua","df","dgo","gua","gue","hgo","jal","mex","mic","mor","nay","nl","oax","pue","que","qui","san","sin","son","tab","tam","tla","ver","yuc","zac")
edo <- edos[edon]

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


###################
# municipios maps #
###################
ruta <- file.path(mapdir, edo) # archivo con mapas ine
mu.map <- list()
#
tmp <- file.path(mapdir, "ags") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==1,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$ags <- tmp
#
tmp <- file.path(mapdir, "bc") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==2,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$bc <- tmp
#
tmp <- file.path(mapdir, "bcs") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==3,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$bcs <- tmp
#
tmp <- file.path(mapdir, "cam") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==4,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$cam <- tmp
#
tmp <- file.path(mapdir, "coa") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==5,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$coa <- tmp
#
tmp <- file.path(mapdir, "col") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==6,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$col <- tmp
#
tmp <- file.path(mapdir, "cps") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==7,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$cps <- tmp
#
tmp <- file.path(mapdir, "cua") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==8,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$cua <- tmp
#
tmp <- file.path(mapdir, "df") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==9,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$df <- tmp
#
tmp <- file.path(mapdir, "dgo") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==10,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$dgo <- tmp
#
tmp <- file.path(mapdir, "gua") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==11,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$gua <- tmp
#
tmp <- file.path(mapdir, "gue") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==12,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$gue <- tmp
#
tmp <- file.path(mapdir, "hgo") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==13,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$hgo <- tmp
#
tmp <- file.path(mapdir, "jal") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==14,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$jal <- tmp
#
tmp <- file.path(mapdir, "mex") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==15,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$mex <- tmp
#
tmp <- file.path(mapdir, "mic") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==16,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$mic <- tmp
#
tmp <- file.path(mapdir, "mor") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==17,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$mor <- tmp
#
tmp <- file.path(mapdir, "nay") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==18,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$nay <- tmp
#
tmp <- file.path(mapdir, "nl") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==19,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$nl <- tmp
#
tmp <- file.path(mapdir, "oax") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==20,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$oax <- tmp
#
tmp <- file.path(mapdir, "pue") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==21,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$pue <- tmp
#
tmp <- file.path(mapdir, "que") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==22,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$que <- tmp
#
tmp <- file.path(mapdir, "qui") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==23,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$qui <- tmp
#
tmp <- file.path(mapdir, "san") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==24,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$san <- tmp
#
tmp <- file.path(mapdir, "sin") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==25,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$sin <- tmp
#
tmp <- file.path(mapdir, "son") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==26,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$son <- tmp
#
tmp <- file.path(mapdir, "tab") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==27,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$tab <- tmp
#
tmp <- file.path(mapdir, "tam") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==28,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$tam <- tmp
#
tmp <- file.path(mapdir, "tla") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==29,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$tla <- tmp
#
tmp <- file.path(mapdir, "ver") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==30,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$ver <- tmp
#
tmp <- file.path(mapdir, "yuc") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==31,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$yuc <- tmp
#
tmp <- file.path(mapdir, "zac") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
# add pr data
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$municipio; tmp.data$mun <- tmp.data$nombre
tmp.pr <- pr.mu[pr.mu$edon==32,]
tmp.data <- merge(x = tmp.data, y = tmp.pr, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.pr)
mu.map$zac <- tmp
#
summary(mu.map)

# morena's color
brown <- rgb(139,69,19, maxColorValue = 255) #"saddlebrown"
colpri <- "black"

# trial
plot(nat.map, lwd = .5, border = "gray")
axis(1); axis(2)

# amlo v pri
library(graphics)
#png("/home/eric/Desktop/MXelsCalendGovt/elecReturns/graph/nytPlus.png", width = 20, height = 20, units = "cm", res = 196)
#pdf("/home/eric/Desktop/MXelsCalendGovt/elecReturns/graph/nytPlus.pdf", width = 10, height = 7)
par(mar = c(0,0,0,0))
plot(shave(nat.map, p = .95), lwd = .5, border = "gray")
for (i in 1:32){
    plot(mu.map[[i]], lwd = .25, border = "lightgray", add = TRUE)
}
plot(nat.map, lwd = .5,          border = "gray", add = TRUE)
plot(nat.map, lwd = .5, lty = 3, border = "white", add = TRUE)
# add arrows
# start-end of arrows
#cx <- 150000; cy <- cx/3
for (i in 1:32){
    # start-end of arrows
    #i <- 1 # debug
    cx <- 300000*mu.map[[i]]$camlopos; cy <- cx/3
    arrows(coordinates(mu.map[[i]])[,1],    coordinates(mu.map[[i]])[,2],
           coordinates(mu.map[[i]])[,1]-cx, coordinates(mu.map[[i]])[,2]+cy,
           length = .025, angle = 10,
           col = brown, lwd = (.1+cx/600000))
    cx <- 400000*mu.map[[i]]$cpripos; cy <- cx/3
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
text(xl+310000,yl-150000, pos = 2, labels = "PRI creció", cex = .75)
text(xl+180000, yl+280000, labels = "Cambio desde", font = 2)
text(xl+180000, yl+180000, labels = "2012 en municipios", font = 2)
text(-13000000, 1550000, labels = "Preparado por Eric Magar con datos del INE (ericmagar.com)", col = "lightgray", pos = 4, cex = .65)
#dev.off()



#################################################################
# read state borders 1-by-1: uncomment bordering states for map #
#################################################################
ed.map <- list()
## tmp <- file.path(mapdir, "ags") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$ags <- tmp
## #
## tmp <- file.path(mapdir, "bc") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$bc <- tmp
## #
## tmp <- file.path(mapdir, "bcs") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$bcs <- tmp
## #
## tmp <- file.path(mapdir, "cam") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$cam <- tmp
## #
## tmp <- file.path(mapdir, "coa") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$coa <- tmp
## #
## tmp <- file.path(mapdir, "col") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$col <- tmp
## #
## tmp <- file.path(mapdir, "cps") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$cps <- tmp
## #
## tmp <- file.path(mapdir, "cua") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$cua <- tmp
#
tmp <- file.path(mapdir, "df") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
ed.map$df <- tmp
#
## tmp <- file.path(mapdir, "dgo") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$dgo <- tmp
## #
## tmp <- file.path(mapdir, "gua") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$gua <- tmp
## #
## tmp <- file.path(mapdir, "gue") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$gue <- tmp
## #
tmp <- file.path(mapdir, "hgo") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
ed.map$hgo <- tmp
#
## tmp <- file.path(mapdir, "jal") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$jal <- tmp
#
tmp <- file.path(mapdir, "mex") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
ed.map$mex <- tmp
#
## tmp <- file.path(mapdir, "mic") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$mic <- tmp
## #
tmp <- file.path(mapdir, "mor") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
ed.map$mor <- tmp
#
## tmp <- file.path(mapdir, "nay") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$nay <- tmp
## #
## tmp <- file.path(mapdir, "nl") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$nl <- tmp
## #
## tmp <- file.path(mapdir, "oax") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$oax <- tmp
#
tmp <- file.path(mapdir, "pue") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
ed.map$pue <- tmp
#
tmp <- file.path(mapdir, "que") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
ed.map$que <- tmp
#
## tmp <- file.path(mapdir, "qui") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$qui <- tmp
## #
## tmp <- file.path(mapdir, "san") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$san <- tmp
## #
## tmp <- file.path(mapdir, "sin") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$sin <- tmp
## #
## tmp <- file.path(mapdir, "son") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$son <- tmp
## #
## tmp <- file.path(mapdir, "tab") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$tab <- tmp
## #
## tmp <- file.path(mapdir, "tam") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$tam <- tmp
## #
tmp <- file.path(mapdir, "tla") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
ed.map$tla <- tmp
#
## tmp <- file.path(mapdir, "ver") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$ver <- tmp
## #
## tmp <- file.path(mapdir, "yuc") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$yuc <- tmp
## #
## tmp <- file.path(mapdir, "zac") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$zac <- tmp

# read municipios
ruta <- file.path(mapdir, edo) # archivo con mapas ine
mu.map <- readOGR(dsn = ruta, layer = 'MUNICIPIO')
summary(mu.map)
mu.map$mun <- mu.map$nombre # mun names
mu.map$mun <- gsub(pattern = "[0-9]+", replacement = "", mu.map$mun) # some names have numbers, drop them
# may need to shorten mun names...
# projects to a different datum with long and lat
mu.map <- spTransform(mu.map, osm())

#######################################
# read shapefiles distritos federales #
#######################################
ruta <- file.path(mapdir, edo)
df.map <- readOGR(dsn = ruta, layer = 'DISTRITO')
# rename columns
head(df.map)
sel <- which(colnames(df.map@data)=="distrito")
colnames(df.map@data)[sel] <- "disfed"
sel <- which(colnames(df.map@data)=="entidad")
colnames(df.map@data)[sel] <- "edon"
# projects to a different ./datum with long and lat
df.map <- spTransform(df.map, osm()) # project to osm native Mercator
###################
# read disfed2006 #
###################
ruta <- file.path("fed/shp/disfed2006", edo) 
df2006.map <- readOGR(dsn = ruta, layer = 'DISTRITO')
# rename columns
head(df2006.map)
sel <- which(colnames(df2006.map@data)=="DISTRITO")
colnames(df2006.map@data)[sel] <- "disfed"
sel <- which(colnames(df2006.map@data)=="ENTIDAD")
colnames(df2006.map@data)[sel] <- "edon"
# projects to a different datum with long and lat
df2006.map <- spTransform(df2006.map, osm()) # project to osm native Mercator

## # add father/son info and dsi of mapLoc < ESTO NO LO TENGO PARA FEDERALES, HABRIA QUE PREPARARLO DESDE LA SECCIONES
## dsi <- "/home/eric/Desktop/data/elecs/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/simIndex/dist_mex.csv"
## dsi <- read.csv(file = dsi, stringsAsFactors = FALSE)
## #
## #df.map$ord <- 1:nrow(df.map@data)
## df.map@data <- merge(x = df.map@data, y = dsi, by.x = "disloc", by.y = "disloc2018", all.x = TRUE, all.y = FALSE)
## rm(dsi)

## # read comparative district maps
## # a. from seccion2dis map, in order to export into se.map for sub-setting
## #sec2dis <- read.csv("/home/eric/Dropbox/data/mapas/reseccionamiento/equivSecc/tablaEquivalenciasSeccionales1994-2010.2013.csv", stringsAsFactors = FALSE)
## #sec2dis <- sec2dis[sec2dis$edon == 18,]
## sec2dis <- read.csv("/home/eric/Dropbox/data/elecs/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/mexLoc.csv", stringsAsFactors = FALSE)
## # send to seccion map
## colnames(se.map)
## tmp <- data.frame(SECCION = se.map$SECCION)
## tmp$orden <- 1:nrow(tmp)
## tmp <- merge(x = tmp, y = sec2dis, by.x = "SECCION", by.y = "seccion", all.x = TRUE, all.y = FALSE)
## tmp <- tmp[order(tmp$orden), grep("^dis.+$", colnames(tmp))]
## #tmp <- tmp[order(tmp$orden), grep("SECCION|^dis.+$", colnames(tmp))]
## se.map@data <- cbind(se.map@data, tmp)
## rm(tmp)
## #

## # read cabeceras distritales (via vocal ejecutivo)
## tmp <- file.path(mapdir, edo, # archivo con mapas rojano
## cabDis <- readOGR(dsn = tmp, layer = 'VOCAL_EJECUTIVO_DISTRITAL')
## # projects to a different datum with long and lat
## cabDis <- spTransform(cabDis, osm())
## #
## cabDisNames <- read.csv(file.path(wd2, "cabeceras2006.csv"), stringsAsFactors = FALSE)

# read cabeceras distritales 2018
ruta <- "~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/fed/data/dfdfcabeceras2018.csv"
tmp <- read.csv(file = ruta, stringsAsFactors = FALSE)
tmp <- tmp[,c("edon","disn","mun")]
colnames(tmp) <- c("edon","disfed","cab")
tmp2 <- df.map@data
tmp2$ord <- 1:nrow(tmp2)
tmp2 <- merge(x = tmp2, y = tmp, by = c("edon","disfed"), all.x = TRUE, all.y = FALSE)
tmp2 <- tmp2[order(tmp2$ord),]
tmp2$ord <- NULL
df.map@data <- tmp2

#############################################################
# add distritos locales: change layer year to match state's #
#############################################################
ruta <- file.path("loc/shp", edo)
dl.map <- readOGR(dsn = ruta, layer = 'disloc2018')
# projects to a different datum with long and lat
dl.map <- spTransform(dl.map, osm()) # project to osm native Mercator

########################
# add casillas in 2012 #
########################
ruta <- file.path("fed/shp/disfed2006", edo) # archivo con mapas rojano
cas.map <- readOGR(dsn = ruta, layer = 'CASILLA')
# projects to a different datum with long and lat
cas.map <- spTransform(cas.map, osm()) # project to osm native Mercator

# add federal districts for subsetting
tmp <- cas.map@data; tmp$ord <- 1:nrow(tmp)
tmp <- merge(x = tmp, y = se.map[,c("seccion","distrito")], by.x = "SECCION", by.y = "seccion", all.x = TRUE, all.y = FALSE)
tmp <- tmp[order(tmp$ord),]
cas.map@data <- tmp

# drop casillas from missing secciones to avoid indeterminate subsetting
sel <- which(is.na(cas.map$distrito)==TRUE)
if (length(sel)>0) cas.map <- cas.map[-sel,] # drop missing cases
rm(sel)

# add ncasillas in 2012 to seccion map
tmp <- data.frame(seccion = se.map$seccion)
tmp$orden <- 1:nrow(tmp)
tmp <- merge(x = tmp, y = ncasillas[ncasillas$edon==edon, c("seccion","e12")], by = "seccion", all.x = TRUE, all.y = FALSE)
tmp <- tmp[order(tmp$orden), c("seccion","e12")]; 
se.map$ncasillas <- tmp$e12

# add nwin to seccion map
tmp <- data.frame(seccion = se.map$seccion)
tmp$orden <- 1:nrow(tmp)
tmp <- merge(x = tmp, y = nwin[nwin$edon==edon,], by = "seccion", all.x = TRUE, all.y = FALSE)
tmp <- tmp[order(tmp$orden), c("seccion","pan","pri","prd","e15")]
se.map$nwinpan <- tmp$pan
se.map$nwinpri <- tmp$pri
se.map$nwinprd <- tmp$prd
# add 2015 winner to seccion map
se.map$win15 <- tmp$e15
rm(tmp)
#
###########################################################
# make colors                                             #
# need to change left to exclude morena victories in 2015 # <- OJO
###########################################################
library(RColorBrewer)
nclr <- 7                                    #CATEGORÍAS DE COLOR (MIN=3 MAX=9)
blues <- brewer.pal(nclr,"Blues")            #GENERA CODIGOS DE COLOR QUE CRECEN CON GRADO
reds <- brewer.pal(nclr,"Reds")              
yellows <- brewer.pal(nclr,"YlOrBr")         
#
library(plyr)
se.map$pancol <- mapvalues ( se.map$nwinpan, from = 0:6, to = blues )
se.map$pricol <- mapvalues ( se.map$nwinpri, from = 0:6, to = reds )
se.map$prdcol <- mapvalues ( se.map$nwinprd, from = 0:6, to = yellows )
# bastiones = 4+ wins
se.map$bastion <- rgb(190,190,190, maxColorValue = 255) # "gray"
## se.map$bastion[se.map$nwinpan>=4] <- "blue"
## se.map$bastion[se.map$nwinpri>=4] <- "red"
## se.map$bastion[se.map$nwinprd>=4] <- "gold"
se.map$bastion[se.map$nwinpan==4] <- blues[3]
se.map$bastion[se.map$nwinpan==5] <- blues[5]
se.map$bastion[se.map$nwinpan==6] <- blues[7]
se.map$bastion[se.map$nwinpri==4] <- reds[3]
se.map$bastion[se.map$nwinpri==5] <- reds[5]
se.map$bastion[se.map$nwinpri==6] <- reds[7]
se.map$bastion[se.map$nwinprd==4] <- yellows[2]
se.map$bastion[se.map$nwinprd==5] <- yellows[4]
se.map$bastion[se.map$nwinprd==6] <- yellows[6]

# color 2015 winner
blue <- blues[7]
red <- reds[7]
yellow <- rgb(255,215,0, maxColorValue = 255) #"gold"
#brown <- rgb(160,82,45, maxColorValue = 255) #"siena"
brown <- rgb(139,69,19, maxColorValue = 255) #"saddlebrown"
gray <- rgb(190,190,190, maxColorValue = 255)
green <- rgb(0,100,0, maxColorValue = 255) #"darkgreen"
#
#table(win$e15)
tmp <- rep(gray, nrow(se.map@data))
tmp[se.map$win15=="pan"] <- blue
tmp[se.map$win15=="pri" | se.map$win15=="pric"] <- red
tmp[se.map$win15=="prd" | se.map$win15=="prdc"] <- yellow
tmp[se.map$win15=="pvem"] <- green
tmp[se.map$win15=="morena"] <- brown
#
se.map$win15 <- tmp


