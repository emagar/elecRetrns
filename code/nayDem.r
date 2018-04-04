setwd("/home/eric/Desktop/MXelsCalendGovt/elecReturns/")

########
# 2008 #
########
d <- read.csv("datosBrutos/nay2008aycasilla.regidDemarcacion.csv", stringsAsFactors = FALSE)

head(d)

d$v01 <- d$pan
d$l01 <- "pan"
d$v02 <- d$pt
d$l02 <- "pt"
d$v03 <- d$asd
d$l03 <- "asd"
d$v04 <- d$prd.pvem
d$l04 <- "prd-pvem"
d$v05 <- d$conve.prs
d$l05 <- "conve-prs"
d$v06 <- d$pri.panal
d$l06 <- "pri-panal"

# create mun-dem identifier
tmp <- d$demarcacion
tmp <- sub(pattern = "DEM (1[0-9])", replacement = "\\1", tmp)
tmp <- sub(pattern = "DEM ", replacement = "0", tmp)
tmp <- paste(d$munn, tmp, sep = ".")
tmp
d$demar <- tmp
rm(tmp)

d$v01 <- ave(d$v01, as.factor(d$demar), FUN=sum, na.rm=TRUE)
d$v02 <- ave(d$v02, as.factor(d$demar), FUN=sum, na.rm=TRUE)
d$v03 <- ave(d$v03, as.factor(d$demar), FUN=sum, na.rm=TRUE)
d$v04 <- ave(d$v04, as.factor(d$demar), FUN=sum, na.rm=TRUE)
d$v05 <- ave(d$v05, as.factor(d$demar), FUN=sum, na.rm=TRUE)
d$v06 <- ave(d$v06, as.factor(d$demar), FUN=sum, na.rm=TRUE)
d$nr <- ave(d$nr, as.factor(d$demar), FUN=sum, na.rm=TRUE)
d$nul <- ave(d$nul, as.factor(d$demar), FUN=sum, na.rm=TRUE)

head(d)

d$pan <- d$pt <- d$asd <- d$prd.pvem <- d$conve.prs <- d$pri.panal<- NULL
d$tipo <- d$secn <- d$casilla <- d$demarcacion <- NULL

d <- d[duplicated(d$demar)==FALSE,]

write.csv(d, file = "datosBrutos/nay2008ayde.regidDemarcacion.csv")

d08 <- d


########
# 2011 #
########
d <- read.csv("datosBrutos/nay2011aycasilla.regidDemarcacion.csv", stringsAsFactors = FALSE)

head(d)

d$v01 <- d$pan
d$l01 <- "pan"
d$v02 <- d$prd
d$l02 <- "prd"
d$v03 <- d$prs
d$l03 <- "prs"
d$v04 <- d$pri.pvem.panal
d$l04 <- "pri-pvem-panal"
d$v05 <- d$pt.conve
d$l05 <- "pt-conve"

# create mun-dem identifier
tmp <- d$demarcacion
tmp <- sub(pattern = "DEM (1[0-9])", replacement = "\\1", tmp)
tmp <- sub(pattern = "DEM ", replacement = "0", tmp)
tmp <- paste(d$munn, tmp, sep = ".")
d$demar <- tmp
rm(tmp)

d$v01 <- ave(d$v01, as.factor(d$demar), FUN=sum, na.rm=TRUE)
d$v02 <- ave(d$v02, as.factor(d$demar), FUN=sum, na.rm=TRUE)
d$v03 <- ave(d$v03, as.factor(d$demar), FUN=sum, na.rm=TRUE)
d$v04 <- ave(d$v04, as.factor(d$demar), FUN=sum, na.rm=TRUE)
d$v05 <- ave(d$v05, as.factor(d$demar), FUN=sum, na.rm=TRUE)
d$nr <- ave(d$nr, as.factor(d$demar), FUN=sum, na.rm=TRUE)
d$nul <- ave(d$nul, as.factor(d$demar), FUN=sum, na.rm=TRUE)

head(d)

d$pan <- d$prd <- d$prs <- d$pri.pvem.panal <- d$pt.conve <- NULL
d$secn <- d$casilla <- d$demarcacion <- NULL

d <- d[duplicated(d$demar)==FALSE,]

write.csv(d, file = "datosBrutos/nay2011ayde.regidDemarcacion.csv")

d11 <- d



########
# 2014 #
########
d <- read.csv("datosBrutos/nay2014aycasilla.regidDemarcacion.csv", stringsAsFactors = FALSE)

d$v01 <- d$pan
d$l01 <- "pan"
d$v02 <- d$pri.pvem.panal
d$l02 <- "pri-pvem-panal"
d$v03 <- d$prd
d$l03 <- "prd"
d$v04 <- d$pt
d$l04 <- "pt"
d$v05 <- d$prs
d$l05 <- "prs"
d$v06 <- d$mc
d$l06 <- "mc"
d$v07 <- d$indepAhoraSi # presentaron candidatos indendientes en casi todas las demarcaciones
d$l07 <- "indep" 
sel <- which(d$mun=="San Blas")
d$v07[sel] <- d$indepLayon[sel] # presentaron candidatos indendientes en todas las demarcaciones
sel <- which(d$mun=="Santa María del Oro")
d$v07[sel] <- d$indepMarcaDiferencia[sel] # se presentó 1 independiente en una demarcación
sel <- which(d$mun=="Xalisco")
d$v07[sel] <- d$indepL[sel] # se presentó 1 independiente en una demarcación

# drop NAs
d$v01[is.na(d$v01)==TRUE] <- 0
d$v02[is.na(d$v02)==TRUE] <- 0
d$v03[is.na(d$v03)==TRUE] <- 0
d$v04[is.na(d$v04)==TRUE] <- 0
d$v05[is.na(d$v05)==TRUE] <- 0
d$v06[is.na(d$v06)==TRUE] <- 0
d$v07[is.na(d$v07)==TRUE] <- 0

# create mun-dem identifier
d$demar <- d$munn+d$demn/100

d$v01 <- ave(d$v01, as.factor(d$demar), FUN=sum, na.rm=TRUE)
d$v02 <- ave(d$v02, as.factor(d$demar), FUN=sum, na.rm=TRUE)
d$v03 <- ave(d$v03, as.factor(d$demar), FUN=sum, na.rm=TRUE)
d$v04 <- ave(d$v04, as.factor(d$demar), FUN=sum, na.rm=TRUE)
d$v05 <- ave(d$v05, as.factor(d$demar), FUN=sum, na.rm=TRUE)
d$v06 <- ave(d$v06, as.factor(d$demar), FUN=sum, na.rm=TRUE)
d$v07 <- ave(d$v07, as.factor(d$demar), FUN=sum, na.rm=TRUE)
d$nr <- ave(d$nr, as.factor(d$demar), FUN=sum, na.rm=TRUE)
d$nul <- ave(d$nul, as.factor(d$demar), FUN=sum, na.rm=TRUE)

head(d)

d$pan <- d$pri.pvem.panal <- d$prd <- d$pt <- d$prs <- d$mc <- d$indepAhoraSi <- d$indepLayon <- d$indepMarcaDiferencia <- d$indepL <- NULL
d$secn <- d$casilla <- NULL

d <- d[duplicated(d$demar)==FALSE,]

write.csv(d, file = "datosBrutos/nay2014ayde.regidDemarcacion.csv")

d14 <- d

save.image(file="tmp.RData")

load(file = "tmp.RData")

##############################
# consolidate 2008-2011-2014 #
##############################
rm(d)
d14$yr <- 2014

colnames(d08)
colnames(d11)
colnames(d14)

d08$inegi <- d08$munn; d08$munn <- d08$munn-18000
d11$inegi <- d11$munn; d11$munn <- d11$munn-18000
d14$inegi <- d14$munn; d14$munn <- d14$munn-18000

d08$v07 <- d08$l07 <- 0
d11$v06 <- d11$l06 <- d11$v07 <- d11$l07 <- 0

d08 <- d08[c("yr","munn","inegi","mun","demar","v01","l01","v02","l02","v03","l03","v04","l04","v05","l05","v06","l06","v07","l07","nr","nul")]
d11 <- d11[c("yr","munn","inegi","mun","demar","v01","l01","v02","l02","v03","l03","v04","l04","v05","l05","v06","l06","v07","l07","nr","nul")]
d14 <- d14[c("yr","munn","inegi","mun","demar","v01","l01","v02","l02","v03","l03","v04","l04","v05","l05","v06","l06","v07","l07","nr","nul")]

d <- rbind(d08, d11)
d <- rbind(d, d14)
d <- d[order(d$yr, d$demar),]
d$ife <- d$dy <- d$ncand <- d$lisnom <- d$ord <- d$efec <- 0
d$status <- d$mo <- d$win <- d$fuente <- d$notas <- ""

d$v08 <- d$v09 <- d$v10 <- d$v11 <- d$v12 <- d$v13 <- d$v14 <- d$v15 <- d$l08 <- d$l09 <- d$l10 <- d$l11 <- d$l12 <- d$l13 <- d$l14 <- d$l15 <- 0
d <- d[c("ord","munn","inegi","ife","mun","demar","status","dy","mo","yr","ncand","win","v01","l01","v02","l02","v03","l03","v04","l04","v05","l05","v06","l06","v07","l07","v08","l08","v09","l09","v10","l10","v11","l11","v12","l12","v13","l13","v14","l14","v15","l15","efec","nr","nul","lisnom","fuente","notas")]
head(d)


########
# 2017 #
########
d17 <- read.csv("datosBrutos/nay2017ayde.regidores.csv", stringsAsFactors = FALSE)
# create mun-dem identifier
d17$demar <- d17$inegi+d17$demarc/100
d17$efec <- 0

d17 <- d17[c("ord","munn","inegi","ife","mun","demar","status","dy","mo","yr","ncand","win","v01","l01","v02","l02","v03","l03","v04","l04","v05","l05","v06","l06","v07","l07","v08","l08","v09","l09","v10","l10","v11","l11","v12","l12","v13","l13","v14","l14","v15","l15","efec","nr","nul","lisnom","fuente","notas")]

# consolidate all
d <- rbind(d, d17)

d$fuente <- "iee"
d$ord <- 1:nrow(d)
sel <- which(d$status=="")
d$status[sel] <- "ok"

sel <- grep("^v", colnames(d))
colnames(d)[sel]
v <- d[,sel] # subset votes
d$efec <- rowSums(v)

head(d)

# get info from old
dold <- read.csv(file="data/ayde2008-presentNayRegid.csv", stringsAsFactors = FALSE)
colnames(dold)
dold <- dold[,c("dy","mo","yr","mun","munn","inegi","ife")] # info to plug into new data

sel <- which(duplicated(dold$munn)==FALSE)
eq <- data.frame(mun=dold$mun[sel], munn=dold$munn[sel], inegi=dold$inegi[sel], ife=dold$ife[sel])
eq$mun <- as.character(eq$mun)

# fills better info
library(plyr)
d$mun <- mapvalues(d$munn, from = eq$munn, to = eq$mun)
d$inegi <- mapvalues(d$munn, from = eq$munn, to = eq$inegi)
d$ife <- mapvalues(d$munn, from = eq$munn, to = eq$ife)

unique(dold$mo[which(dold$yr==2008)])
unique(dold$dy[which(dold$yr==2008)])
d$mo[d$yr==2008] <- "jul"
d$dy[d$yr==2008] <- 6
unique(dold$mo[which(dold$yr==2011)])
unique(dold$dy[which(dold$yr==2011)])
d$mo[d$yr==2011] <- "jul"
d$dy[d$yr==2011] <- 3
unique(dold$mo[which(dold$yr==2014)])
unique(dold$dy[which(dold$yr==2014)])
d$mo[d$yr==2014] <- "jul"
d$dy[d$yr==2014] <- 6

head(d)

write.csv(d, file = "data/ayde2008-presentNayRegid.csv", row.names=FALSE)



