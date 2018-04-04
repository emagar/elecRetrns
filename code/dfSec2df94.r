rm(list=ls())

# este archivo tiene los nombres de las cabeceras y los municipios de 1994. abajo uso otro que es más versátil
workdir <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/datosBrutos/")
setwd(workdir)
#
d <- read.csv(file = "eum94dfCasilla.csv", stringsAsFactors=FALSE)
#
colnames(d)
library(plyr)
de <- c("AGUASCALIENTES", "BAJA CALIFORNIA", "BAJA CALIFORNIA SUR", "CAMPECHE", "CHIAPAS", "CHIHUAHUA", "COAHUILA", "COLIMA", "DISTRITO FEDERAL", "DURANGO", "GUANAJUATO", "GUERRERO", "HIDALGO", "JALISCO", "MEXICO", "MICHOACAN", "MORELOS", "NAYARIT", "NUEVO LEON", "OAXACA", "PUEBLA", "QUERETARO", "QUINTANA ROO", "SAN LUIS POTOSI", "SINALOA", "SONORA", "TABASCO", "TAMAULIPAS", "TLAXCALA", "VERACRUZ", "YUCATAN", "ZACATECAS")
a <- 1:32
d$edon <- mapvalues (d$ENTIDAD, from = de, to = a)
d$edon <- as.integer(as.character(d$edon))
rm(de, a)
colnames(d) <- c("edo", "cabecera", "disn", "mun", "secn", "casilla", "pan", "pri", "pps", "prd", "pfcrn", "parm", "uno.pdm", "pt", "pvem", "nr", "nul", "tot", "status", "edon")
#
# numbers d[is.na(d$pan)==TRUE,]
d$pan     <- as.numeric(d$pan)
d$pri     <- as.numeric(d$pri)
d$pps     <- as.numeric(d$pps)
d$prd     <- as.numeric(d$prd)
d$pfcrn   <- as.numeric(d$pfcrn)
d$parm    <- as.numeric(d$parm)
d$uno.pdm <- as.numeric(d$uno.pdm)
d$pt      <- as.numeric(d$pt)
d$pvem    <- as.numeric(d$pvem)
d$nr      <- as.numeric(d$nr)
d$nul     <- as.numeric(d$nul)
d$tot     <- as.numeric(d$tot)
#
# aggregate by federal district
d.df <- d
d.df$pan     <- ave(d.df$pan    , as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df$pri     <- ave(d.df$pri    , as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df$pps     <- ave(d.df$pps    , as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df$prd     <- ave(d.df$prd    , as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df$pfcrn   <- ave(d.df$pfcrn  , as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df$parm    <- ave(d.df$parm   , as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df$uno.pdm <- ave(d.df$uno.pdm, as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df$pt      <- ave(d.df$pt     , as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df$pvem    <- ave(d.df$pvem   , as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df$nr      <- ave(d.df$nr     , as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df$nul     <- ave(d.df$nul    , as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df$tot     <- ave(d.df$tot    , as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df <- d.df[duplicated(d.df$edon * 100 + d.df$disn)==FALSE,]
dim(d.df) # debug
d.df$mun <- d.df$secn <- d.df$casilla <- d.df$status <- NULL
#
head(d.df) # debug

write.csv(d.df, file = "eum94dfdf.csv")

workdir <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/datosBrutos/resultCasillas/casillaRes91-on/")
setwd(workdir)

# 1991
d <- read.csv(file = "dip1991.csv", stringsAsFactors=FALSE)
colnames(d) <- tolower(colnames(d))
head(d)
#
d.df <- d
d.df$pan     <- ave(d.df$pan    , as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df$parm    <- ave(d.df$parm   , as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df$pdm     <- ave(d.df$pdm, as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df$pfcrn   <- ave(d.df$pfcrn  , as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df$pps     <- ave(d.df$pps    , as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df$prd     <- ave(d.df$prd    , as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df$pri     <- ave(d.df$pri    , as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df$pt      <- ave(d.df$pt     , as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df$pem     <- ave(d.df$pem   , as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df$prt     <- ave(d.df$prt     , as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df$nr      <- ave(d.df$nr     , as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df$nul     <- ave(d.df$nul    , as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df$tot     <- ave(d.df$tot    , as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df <- d.df[duplicated(d.df$edon * 100 + d.df$disn)==FALSE,]
dim(d.df) # debug
head(d.df)
d.df$munn <- d.df$seccion <- d.df$casilla <- d.df$ID_ELEC <- d.df$STATUS <- NULL

# 1994
d <- read.csv(file = "dip1994.csv", stringsAsFactors=FALSE)
colnames(d)
#
d.df <- d
d.df$pan     <- ave(d.df$pan    , as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df$pri     <- ave(d.df$pri    , as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df$pps     <- ave(d.df$pps    , as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df$prd     <- ave(d.df$prd    , as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df$pfcrn   <- ave(d.df$pfcrn  , as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df$parm    <- ave(d.df$parm   , as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df$uno.pdm <- ave(d.df$uno.pdm, as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df$pt      <- ave(d.df$pt     , as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df$pvem    <- ave(d.df$pvem   , as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df$nr      <- ave(d.df$nr     , as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df$nul     <- ave(d.df$nul    , as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df$tot     <- ave(d.df$tot    , as.factor(d.df$edon * 100 + d.df$disn), FUN=sum, na.rm=TRUE)
d.df <- d.df[duplicated(d.df$edon * 100 + d.df$disn)==FALSE,]
dim(d.df) # debug
head(d.df)
d.df$munn <- d.df$seccion <- d.df$casilla <- d.df$ID_ELEC <- d.df$STATUS <- NULL
