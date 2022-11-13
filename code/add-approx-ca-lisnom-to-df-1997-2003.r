######################################################################################
## Casilla-level dipfed returns lack lista nominal in source. But IFE distributed   ##
## sección-level lista nominales. Script splits lisnom evenly among its casillas.   ##
## When sección has no casillas especiales, this approximation should be on target. ##
## When there is a casilla especial, even split is slightly off target, as can be   ##
## seen in post-2003 returns.                                                       ##
######################################################################################

rm(list = ls())
options(width = 120)
#
# function renaming a column in a data frame without knowing its index number
pth <- ifelse (Sys.info()["user"] %in% c("eric", "magar"),
               "~/Dropbox/data/useful-functions",
              "https://raw.githubusercontent.com/emagar/useful-functions/master"
              )
source( paste(pth, "col.rename.r", sep = "/") )
#
# manipulated data path
md <- "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/"
#
# source data data
sd <- "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/datosBrutos/result-casillas-fed/lisnom-sec1991-2003/"
#
##########
## 1991 ##
##########
#
##########
## 1994 ##
##########
#
##########
## 1997 ##
##########
#
##########
## 2000 ##
##########
d <- read.csv(paste0(md, "dip2000.csv"))
l <- read.csv(paste0(sd, "lisnom2000ca.csv"))
# rename columns
l <- col.rename(l, "ESTADO",  "edon")
l <- col.rename(l, "SECCION", "seccion")
l <- col.rename(l, "CASILLA", "casilla")
l <- col.rename(l, "LISTA",   "lisnom")
# subset
l <- l[,c("edon","seccion","casilla","lisnom")]
#
# verify casillas named equally in both files
table(d$casilla)
table(l$casilla)
sel.d <- grep("^C", d$casilla)
sel.l <- grep("^C", l$casilla)
table(d$casilla[sel.d])
table(l$casilla[sel.l])
# C1 needs to turn into C
sel.l <- grep("^C1$", l$casilla)
l$casilla[sel.l] <- "C"
#
sel.d <- grep("^E", d$casilla)
sel.l <- grep("^E", l$casilla)

# EX 1 B to E etc
table(d$casilla[sel.d])
table(l$casilla[sel.l])
sel.l <- grep("^EX 1 B$", l$casilla)
l$casilla[sel.l] <- "E"

sel.d <- grep("^E.", d$casilla)
sel.l <- grep("^E.", l$casilla)
table(d$casilla[sel.d])
table(l$casilla[sel.l])

table(paste0(d$edon[sel.d],"-",d$seccion[sel.d]))
table(paste0(d$edon[sel.d],"-",d$seccion[sel.d]))
x

#
sel.d <- grep("^C", d$casilla)
sel.l <- grep("^C", l$casilla)
# all ok here
table(d$casilla[sel.d])
table(l$casilla[sel.l])
#
sel.d <- grep("^S", d$casilla)
sel.l <- grep("^S", l$casilla)
# no hay casillas S en lista nominal
table(d$casilla[sel.d])
table(l$casilla[sel.l])

# merge lisnom into d

##########
## 2003 ##
##########
d <- read.csv(paste0(md, "dip2003.csv"))
l <- read.csv(paste0(sd, "lisnom2003ca.csv"))
# rename columns
l <- col.rename(l, "ESTADO",  "edon")
l <- col.rename(l, "SECCION", "seccion")
l <- col.rename(l, "CASILLA", "casilla")
l <- col.rename(l, "LISTA",   "lisnom")
# subset
l <- l[,c("edon","seccion","casilla","lisnom")]
#
# verify casillas named equally in both files
table(d$casilla)
table(l$casilla)
sel.d <- grep("^B", d$casilla)
sel.l <- grep("^B", l$casilla)
table(d$casilla[sel.d])
table(l$casilla[sel.l])
# B1 needs to turn into B
d$casilla[sel.d] <- "B"
#
sel.d <- grep("^E", d$casilla)
sel.l <- grep("^E", l$casilla)
# all ok here
table(d$casilla[sel.d])
table(l$casilla[sel.l])
#
sel.d <- grep("^C", d$casilla)
sel.l <- grep("^C", l$casilla)
# all ok here
table(d$casilla[sel.d])
table(l$casilla[sel.l])
#
sel.d <- grep("^S", d$casilla)
sel.l <- grep("^S", l$casilla)
# no hay casillas S en lista nominal
table(d$casilla[sel.d])
table(l$casilla[sel.l])

# merge lisnom into d
d <- merge(x=d, y=l, by=c("edon","seccion","casilla"), all.x = TRUE, all.y = FALSE)

# these casillas do not match an observation in l and get lisnom=NA
sel.d <- which(is.na(d$lisnom))
d[sel.d, c("edon","seccion","casilla","tot","lisnom")]
table(d$casilla[sel.d])

# will save manupulated data here

head(d)
head(l)
dim(d)
dim(l)

