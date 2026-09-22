#####################################################################
## Código que reconstituye las demarcaciones de mapas 2008 y 2023  ##
## a partir de las secciones electorales.                          ##
## Permite agregar la lista nominal, que falta en ayde, a partir   ##
## de las casillas en elecciones federales (con rezago de un año). ##
##                                                                 ##
## Autor: Eric Magar                                               ##
## Fecha: 15sep2026                                                ##
#####################################################################

rm(list = ls())
##
## read seccion maps
file <- "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/redistrict/ife.ine/equivSecc/tablaEquivalenciasSeccionalesDesde1994.csv"
sede <- read.csv(file)
##
## check
sede[1,]
##
## retiene sólo nayarit
sede <- sede[which(sede$edon==18),]
## retiene sólo las columnas relevantes
sede <- sede[, c("edon", "edo", "seccion", "inegi", "ife", "mun", "alta", "baja", "dem2008", "dem2023")]

## para elección 2024 no hay lag
file <- "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/dip2024-pre-trife.csv"
lis <- read.csv(file)
##
## check
lis[1,]
## retiene sólo nayarit
lis <- lis[which(lis$edo=="nay"),]
## retiene sólo las columnas relevantes
lis <- lis[, c("seccion", "lisnom")]
## agrega las casillas de cada sección
lis$lisnom2024 <- ave(lis$lisnom,
                  as.factor(lis$seccion),
                  FUN=function(x) sum(x, na.rm=TRUE)
                  )
lis$lisnom <- NULL
## elimina filas redundantes
lis <- lis[which(duplicated(lis$seccion)==FALSE), ]
## merge lisnom to sede
tmp <- merge(x = sede, y = lis, by = "seccion", all = TRUE)
tmp[1,]
## check dim, if all ok rename tmp to sede
dim(sede)
dim(lis)
dim(tmp)
sede <- tmp
rm(tmp)

## para elección 2021 no hay lag (solo falta lisnom de 1 munic con extraordinaria, permite verificar sumas)
file <- "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/dip2021.csv"
lis <- read.csv(file)
##
## check
lis[1,]
## retiene sólo nayarit
lis <- lis[which(lis$edon==18),]
## retiene sólo las columnas relevantes
lis <- lis[, c("seccion", "lisnom")]
## quita casillas especiales
lis <- lis[lis$seccion>0, ]
## agrega las casillas de cada sección
lis$lisnom2021 <- ave(lis$lisnom,
                  as.factor(lis$seccion),
                  FUN=function(x) sum(x, na.rm=TRUE)
                  )
lis$lisnom <- NULL
## elimina filas redundantes
lis <- lis[which(duplicated(lis$seccion)==FALSE), ]
## merge lisnom to sede
tmp <- merge(x = sede, y = lis, by = "seccion", all = TRUE)
tmp[1,]
## check dim, if all ok rename tmp to sede
dim(sede)
dim(lis)
dim(tmp)
sede <- tmp
rm(tmp)

## para elección 2017 hay lag de un año
file <- "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/dip2018.csv"
lis <- read.csv(file)
##
## check
lis[1,]
## retiene sólo nayarit
lis <- lis[which(lis$edon==18),]
## retiene sólo las columnas relevantes
lis <- lis[, c("seccion", "lisnom")]
## quita casillas especiales
lis <- lis[lis$seccion>0, ]
## agrega las casillas de cada sección
lis$lisnom2017 <- ave(lis$lisnom,
                  as.factor(lis$seccion),
                  FUN=function(x) sum(x, na.rm=TRUE)
                  )
lis$lisnom <- NULL
## elimina filas redundantes
lis <- lis[which(duplicated(lis$seccion)==FALSE), ]
## merge lisnom to sede
tmp <- merge(x = sede, y = lis, by = "seccion", all = TRUE)
tmp[1,]
## check dim, if all ok rename tmp to sede
dim(sede)
dim(lis)
dim(tmp)
sede <- tmp
rm(tmp)

## para elección 2014 hay lag de un año
file <- "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/dip2015.csv"
lis <- read.csv(file)
##
## check
lis[1,]
## retiene sólo nayarit
lis <- lis[which(lis$edon==18),]
## retiene sólo las columnas relevantes
lis <- lis[, c("seccion", "lisnom")]
## quita casillas especiales
lis <- lis[lis$seccion>0, ]
## agrega las casillas de cada sección
lis$lisnom2014 <- ave(lis$lisnom,
                  as.factor(lis$seccion),
                  FUN=function(x) sum(x, na.rm=TRUE)
                  )
lis$lisnom <- NULL
## elimina filas redundantes
lis <- lis[which(duplicated(lis$seccion)==FALSE), ]
## merge lisnom to sede
tmp <- merge(x = sede, y = lis, by = "seccion", all = TRUE)
tmp[1,]
## check dim, if all ok rename tmp to sede
dim(sede)
dim(lis)
dim(tmp)
sede <- tmp
rm(tmp)

## para elección 2011 hay lag de un año
file <- "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/dip2012.csv"
lis <- read.csv(file)
##
## check
lis[1,]
## retiene sólo nayarit
lis <- lis[which(lis$edon==18),]
## retiene sólo las columnas relevantes
lis <- lis[, c("seccion", "lisnom")]
## quita casillas especiales
lis <- lis[lis$seccion>0, ]
## agrega las casillas de cada sección
lis$lisnom2011 <- ave(lis$lisnom,
                  as.factor(lis$seccion),
                  FUN=function(x) sum(x, na.rm=TRUE)
                  )
lis$lisnom <- NULL
## elimina filas redundantes
lis <- lis[which(duplicated(lis$seccion)==FALSE), ]
## merge lisnom to sede
tmp <- merge(x = sede, y = lis, by = "seccion", all = TRUE)
tmp[1,]
## check dim, if all ok rename tmp to sede
dim(sede)
dim(lis)
dim(tmp)
sede <- tmp
rm(tmp)

## para elección 2008 hay lag de un año
file <- "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/dip2009.csv"
lis <- read.csv(file)
##
## check
lis[1,]
## retiene sólo nayarit
lis <- lis[which(lis$edon==18),]
## retiene sólo las columnas relevantes
lis <- lis[, c("seccion", "lisnom")]
## quita casillas especiales
lis <- lis[lis$seccion>0, ]
## agrega las casillas de cada sección
lis$lisnom2008 <- ave(lis$lisnom,
                  as.factor(lis$seccion),
                  FUN=function(x) sum(x, na.rm=TRUE)
                  )
lis$lisnom <- NULL
## elimina filas redundantes
lis <- lis[which(duplicated(lis$seccion)==FALSE), ]
## merge lisnom to sede
tmp <- merge(x = sede, y = lis, by = "seccion", all = TRUE)
tmp[1,]
## check dim, if all ok rename tmp to sede
dim(sede)
dim(lis)
dim(tmp)
sede <- tmp
rm(tmp)

## Agrega las listas nominales de 2008:2021 con mapa 2008
map08 <- sede
## elimina cols redundantes
map08$lisnom2024 <- NULL
map08$dem2023 <- NULL
## elimina filas de sessiones inexistentes antes de 2023
map08 <- map08[which(is.na(map08$dem2008)==FALSE), ]
## agrega
map08$lisnom2008 <- ave(map08$lisnom2008,
                        as.factor(map08$dem2008),
                        FUN=function(x) sum(x, na.rm=TRUE)
                        )
map08$lisnom2011 <- ave(map08$lisnom2011,
                        as.factor(map08$dem2008),
                        FUN=function(x) sum(x, na.rm=TRUE)
                        )
map08$lisnom2014 <- ave(map08$lisnom2014,
                        as.factor(map08$dem2008),
                        FUN=function(x) sum(x, na.rm=TRUE)
                        )
map08$lisnom2017 <- ave(map08$lisnom2017,
                        as.factor(map08$dem2008),
                        FUN=function(x) sum(x, na.rm=TRUE)
                        )
map08$lisnom2021 <- ave(map08$lisnom2021,
                        as.factor(map08$dem2008),
                        FUN=function(x) sum(x, na.rm=TRUE)
                        )
## elimina filas redundantes
map08 <- map08[which(duplicated(map08$dem2008)==FALSE), ]

## Agrega las listas nominales de 2024 con mapa 2023
map23 <- sede
## elimina cols redundantes
map23$lisnom2008 <- map23$lisnom2011 <- map23$lisnom2014 <- map23$lisnom2017 <- map23$lisnom2021 <- NULL
map23$dem2008 <- NULL
## elimina filas de sessiones inexistentes post 2023
map23 <- map23[which(is.na(map23$dem2023)==FALSE), ]
## agrega
map23$lisnom2024 <- ave(map23$lisnom2024,
                        as.factor(map23$dem2023),
                        FUN=function(x) sum(x, na.rm=TRUE)
                        )
## elimina filas redundantes
map23 <- map23[which(duplicated(map23$dem2023)==FALSE), ]

map08[1,]
map23[1,]
dim(map08)
dim(map23)

## renombra dem2008 y dem2023 a dem para merge
map08$demar <- map08$dem2008
map23$demar <- map23$dem2023

## merge ambos objetos
tmp <- merge(x=map08, y=map23[,c("demar","lisnom2024")], by = "demar", all=TRUE)
dim(tmp)
## exporta para inspeccionar
write.csv(tmp, file = "~/Downloads/tmp.csv")


<- "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/ay-nonfused/ayde2008-on-Nayarit-regid.csv"
