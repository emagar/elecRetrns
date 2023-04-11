######################################################################################
## Casilla-level dipfed returns lack lista nominal in source. But IFE distributed   ##
## sección-level lista nominales. Script splits lisnom evenly among its casillas.   ##
## When sección has no casillas especiales, this approximation should be on target. ##
## When there is a casilla especial, even split is slightly off target, as can be   ##
## seen in post-2003 returns.                                                       ##
## SCRIPT IS MEANT TO BE USED ONCE, THEN BECOMES USELESS                            ## 
######################################################################################

rm(list = ls())
options(width = 130)
#
# function renaming a column in a data frame without knowing its index number
pth <- ifelse (Sys.info()["user"] %in% c("eric", "magar"),
               "~/Dropbox/data/useful-functions",
              "https://raw.githubusercontent.com/emagar/useful-functions/master"
              )
source( paste(pth, "col.rename.r", sep = "/") )
# function to move columns in a data frame
source( paste(pth, "moveme.r", sep = "/") )
#
# function to split sección lisnom evenly among casillas
even.out <- function(x = NA){
    split.ln <- round(x / length(x), 0)
    return(split.ln)
}
#
# manipulated data path
md <- "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/"
#
# source data path
sd <- "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/datosBrutos/result-casillas-fed/lisnom-sec1991-2003/"
#
##########
## 1991 ##
##########
#
d <- read.csv(paste0(md, "dip1991.csv"))
l <- read.csv(paste0(sd, "lisnom1991se.csv"))
#
## 1991 reports use inegi instead of ife (???)
## import conversion function
source("../../ancillary/ife_to_inegi.r")
d$inegi <- inegi.to.ife(d$inegi)
d <- col.rename(d, "inegi", "ife")
d$edosecn <- d$ife*1000000 + d$disn*10000 + d$seccion
# rename columns
l <- col.rename(l, "ESTADO",    "edon")
l <- col.rename(l, "SECCION",   "seccion")
l <- col.rename(l, "LISTA",     "lisnom")
l <- col.rename(l, "DISTRITO",  "disn")
l <- col.rename(l, "MUNICIPIO", "ife")
l$ife <- l$edon*1000 + l$ife
l$edosecn <- l$ife*1000000 + l$disn*10000 + l$seccion
# no seccion duplicates
table(duplicated(l$edosecn))
#table(l$id[l$edon==29])
#
# subset
##l <- l[,c("edon","disn","ife","seccion","lisnom")]
l <- l[,c("edosecn","lisnom")]
l <- col.rename(l, "lisnom",  "lisnom.se")
#
# merge seccion-level lisnom into d
d <- merge(x=d, y=l, by="edosecn", all.x = TRUE, all.y = FALSE)
#
head(d)
head(l)
table(is.na(d$lisnom.se)) # these secciones do not appear in lista nominal
#
sel <- which(is.na(d$lisnom.se))
head(d[sel,c("edon","disn","ife","seccion","casilla","lisnom.se")])
#
d$lisnom.ca <- ave(d$lisnom.se, as.factor(d$edosecn), FUN=even.out, na.rm=TRUE)
#    
table(is.na(d$lisnom.ca))
#
# clean before saving
d$id <- NULL
d$lisnom.se <- NULL
d <- col.rename(d, "lisnom.ca", "lisnom")
d <- d[moveme(names(d), "lisnom before dextra; seccion after mun")]
# add note to first obs
d$note[1] <- "Casilla lisnom = seccion's evenly split"
#
# save manipulated data
write.csv(d, paste0(md, "dip1991m.csv"), row.names = FALSE)
#
##########
## 1994 ##
##########
d <- read.csv(paste0(md, "dip1994.csv"))
l <- read.csv(paste0(sd, "lisnom1994se.csv"))
# rename columns
l <- col.rename(l, "ESTADO",    "edon")
l <- col.rename(l, "SECCION",   "seccion")
l <- col.rename(l, "LISTA",     "lisnom")
l <- col.rename(l, "MUNICIPIO", "ife")
# 1994 has some repeated secciones: drop those with lisnom in 0-8. Rest are in Guerrero, separating secciones that split with creation of acatepec municipality for 1996 state race: regroup theose listas for present purposes
#l$ord <- 1:nrow(l)
l$edsec <- paste(l$edon, l$seccion, sep = "-")
#l <- l[order(l$lisnom, decreasing=TRUE),] # sort decreasing lisnom to spot cases with small lisnom
#l <- l[order(l$edsec),] # sort decreasing lisnom to spot cases with small lisnom
l$drep <- as.numeric(duplicated(paste(l$edon, l$seccion, sep = "-"))==TRUE)
table(l$drep)
sel <- which(l$edsec %in% l$edsec[l$drep==1]) # select cases with duplicates
# drop instances with small/null lisnom
sel.d <- which(l$lisnom[sel]<10)
#l$lisnom[sel[sel.d]]
l <- l[-sel[sel.d],]
# repeat: this gets guerrero's ife 73 and 76 split secciones; add them for 1994
l$drep <- as.numeric(duplicated(paste(l$edon, l$seccion, sep = "-"))==TRUE)
table(l$drep)
l$ife[l$drep==1] # will drop these after manipulation
sel <- which(l$edsec %in% l$edsec[l$drep==1]) # select 
#l[sel, c("edon", "seccion", "ife", "lisnom")]
# subset for manipulation: add seccion lisnom
tmp <- l[sel, c("edon", "seccion", "ife", "lisnom")]
tmp$lisnom <- ave(tmp$lisnom, as.factor(tmp$seccion), FUN=sum, na.rm=TRUE)
l[sel, c("edon", "seccion", "ife", "lisnom")] <- tmp #return to data
# drop reduntant secciones
sel <- which(l$drep==1)
l <- l[-sel,]
#
# subset
l <- l[,c("edon","seccion","lisnom")]
l <- col.rename(l, "lisnom",  "lisnom.se")
#
# merge seccion-level lisnom into d
d <- merge(x=d, y=l, by=c("edon","seccion"), all.x = TRUE, all.y = FALSE)
#
d$lisnom.ca <- ave(d$lisnom.se, as.factor(paste(d$edon, d$seccion, sep = "-")), FUN=even.out, na.rm=TRUE)
#    
table(is.na(d$lisnom.ca))
#
# clean before saving
d$lisnom.se <- NULL
d <- col.rename(d, "lisnom.ca", "lisnom")
d <- d[moveme(names(d), "lisnom before status; disn after edon; seccion after mun")]
d <- col.rename(d, "munn", "ife")
d$ife <- d$edon*1000 + d$ife
# add note to first obs
d$note[1] <- "Casilla lisnom = seccion's evenly split"
#
# save manipulated data
write.csv(d, paste0(md, "dip1994m.csv"), row.names = FALSE)
#
##########
## 1997 ##
##########
d <- read.csv(paste0(md, "dip1997.csv"))
l <- read.csv(paste0(sd, "lisnom1997se.csv"))
# rename columns
l <- col.rename(l, "ESTADO",  "edon")
l <- col.rename(l, "SECCION", "seccion")
l <- col.rename(l, "LISTA",   "lisnom")
# subset
l <- l[,c("edon","seccion","lisnom")]
l <- col.rename(l, "lisnom",  "lisnom.se")
#
# merge seccion-level lisnom into d
d <- merge(x=d, y=l, by=c("edon","seccion"), all.x = TRUE, all.y = FALSE)
#
d$lisnom.ca <- NA # add empty column
d$lisnom.ca <- ave(d$lisnom.se, as.factor(paste(d$edon, d$seccion, sep = "-")), FUN=even.out, na.rm=TRUE)
#    
head(d)
table(is.na(d$lisnom.ca))
#
# clean before saving
d$lisnom.se <- NULL
d <- col.rename(d, "lisnom.ca", "lisnom")
d <- d[moveme(names(d), "lisnom before status; disn after edon; seccion after mun")]
d <- col.rename(d, "munn", "ife")
d$ife <- d$edon*1000 + d$ife
# add note to first obs
d$note[1] <- "Casilla lisnom = seccion's evenly split"
#
# save manipulated data
write.csv(d, paste0(md, "dip1997m.csv"), row.names = FALSE)
#
##############################
## 2000 has ca-level lisnom ##
##############################
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
#
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
#
# merge lisnom into d
d <- merge(x=d, y=l, by.x=c("edon","seccion","casillaln"), by.l=c("edon","seccion","casilla"), all.x = TRUE, all.y = FALSE)
head(d)
#
# these casillas do not match an observation in l and get lisnom=NA
sel.d <- which(is.na(d$lisnom))
table(d$casilla[sel.d])
#
# clean for saving
d$casillaln <- NULL # drop seccionln after merge is done
d <- d[order(d$ord),] # sort as unmanipulated
d$ord <- NULL
d <- d[moveme(names(d), "lisnom before status; disn after edon; seccion after mun")]
d <- col.rename(d, "munn", "ife")
d$ife <- d$edon*1000 + d$ife
#
# save manipulated data
write.csv(d, paste0(md, "dip2000m.csv"), row.names = FALSE)
#
##############################
## 2003 has ca-level lisnom ##
##############################
d <- read.csv(paste0(md, "dip2003.csv"))
l <- read.csv(paste0(sd, "lisnom2003ca.csv"))
#l2 <- read.csv(paste0(sd, "lisnom2003ca-desde-html.csv"))
l2$casillao <- l2$casilla
#l2$casilla <- gsub(pattern = "([CES])0", replacement = "\\1", l2$casilla)
# rename columns
l <- col.rename(l, "ESTADO",  "edon")
l <- col.rename(l, "SECCION", "seccion")
l <- col.rename(l, "CASILLA", "casilla")
l <- col.rename(l, "LISTA",   "lisnom")
#l2 <- col.rename(l2, "edo_clave", "edon")
# subset
l <- l[,c("edon","seccion","casilla","lisnom")]
#l2 <- l2[,c("edon","seccion","casilla","casillao","lisnom")]
#l2 <- merge(x=l2, y=l, by=c("edon","seccion","casilla"))
#l2$dif <- l2$lisnom.x - l2$lisnom.y
#table(l2$dif)
#write.csv(l2, paste0(md, "tmp.csv"))
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
#
# merge lisnom into d
d <- merge(x=d, y=l, by=c("edon","seccion","casilla"), all.x = TRUE, all.y = FALSE)
#
# these casillas do not match an observation in l and get lisnom=NA
sel.d <- which(is.na(d$lisnom))
d[sel.d, c("edon","seccion","casilla","tot","lisnom")]
table(d$casilla[sel.d])
#
# clean for saving
d <- d[moveme(names(d), "lisnom before status; disn after edon; seccion after mun; casilla after seccion")]
d <- col.rename(d, "munn", "ife")
d$ife <- d$edon*1000 + d$ife
#
# save manipulated data
write.csv(d, paste0(md, "dip2003m.csv"), row.names = FALSE)


