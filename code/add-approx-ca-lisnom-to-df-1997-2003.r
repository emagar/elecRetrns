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
#
# use casillaln to match casillas extraordinarias names towards merge (EX 1 B to E etc.)
sel.d <- grep("^E", d$casilla)
sel.l <- grep("^E", l$casilla)
table(d$casilla[sel.d])
table(l$casilla[sel.l])
table(d$casilla[sel.d], d$casillaln[sel.d])
#
## # was used once to extract casilla extra names (match was done using indexes in originally-sorted source files)
## table(paste0(d$edon[sel.d],"-",d$seccion[sel.d]))
## table(paste0(d$edon[sel.d],"-",d$seccion[sel.d]))

# C1 needs to turn into C
sel.d <- grep("^C", d$casilla)
sel.l <- grep("^C", l$casilla)
table(d$casilla[sel.d])
table(l$casilla[sel.l])
sel.l <- grep("^C1$", l$casilla)
l$casilla[sel.l] <- "C"
# fill casillaln for merge
sel.d <- grep("^C", d$casilla)
d$casillaln[sel.d] <- d$casilla[sel.d]
#
# B needs no manip
sel.d <- grep("^B", d$casilla)
sel.l <- grep("^B", l$casilla)
table(d$casilla[sel.d])
table(l$casilla[sel.l])
# fill casillaln for merge
d$casillaln[sel.d] <- d$casilla[sel.d]
#
# no hay casillas S en lista nominal
sel.d <- grep("^S", d$casilla)
sel.l <- grep("^S", l$casilla)
table(d$casilla[sel.d])
table(l$casilla[sel.l])
# fill casillaln for merge
d$casillaln[sel.d] <- d$casilla[sel.d]
#
table(d$casilla, d$casillaln)
table(is.na(d$casillaln))

# merge lisnom into d
d <- merge(x=d, y=l, by.x=c("edon","seccion","casillaln"), by.l=c("edon","seccion","casilla"), all.x = TRUE, all.y = FALSE)
head(d)

# these casillas do not match an observation in l and get lisnom=NA
sel.d <- which(is.na(d$lisnom))
table(d$casilla[sel.d])

# clean for saving
d[1,]
d$casillaln <- NULL # drop seccionln after merge is done
d <- d[order(d$ord),] # sort as unmanipulated

# save manipulated data
write.csv(d, paste0(md, "dip2000m.csv"), row.names = FALSE)

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

## # save manipulated data
## write.csv(d, paste0(md, "dip2003m.csv"))

head(d)
head(l)
dim(d)
dim(l)

