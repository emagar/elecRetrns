#################################################################################
## Script matches casillas to theit lat/long coordinates                       ##
## and exports files that include them                                         ##
##                                                                             ##
## Prepared by Eric Magar with INE data retreived by Francisco Garfias at UCSD ##
## email: emagar at itam dot mx                                                ##
## last revision: 22feb2023                                                    ##
#################################################################################

rm(list=ls())
bd <- "/home/eric/Downloads/Desktop/MXelsCalendGovt/elecReturns/"
gd <- paste0(bd, "datosBrutos/not-in-git/casilla-locations-2006-2019-garfias/")
vd <- paste0(bd, "data/casillas/")

# not.in function
pth <- ifelse (Sys.info()["user"] %in% c("eric", "magar"),
    "~/Dropbox/data/useful-functions",
    "https://raw.githubusercontent.com/emagar/useful-functions/master"
    )
# Reads sortBy function
source( paste(pth, "notin.r", sep = "/") )
rm(pth)

###################
## 2006 lat long ##
###################
#
# make 2006 files have one casilla per row 
g <- read.csv(file=paste0(gd, "casillas_2006.csv"))
# drop redundant columns
g <- g[, c("edon","seccion","casilla","latitude","longitude")]
#
sel.r <- grep("[|]", g$casilla) # cases reporting >1 casillas need to be 'exploded'
#
while(length(sel.r)>0){
    print(paste(length(sel.r), "obs to be manipulated"))
    g.subset <- g[sel.r,] # subset needs manipulation
    g.dupli <- g.subset  # duplicate
    first <- sub(pattern = "(^[A-Z0-9]+)[|](.*$)", replacement = "\\1", g.subset$casilla) # get 1st casilla in bunch
    rest <- sub(pattern = "(^[A-Z0-9]+)[|](.*$)", replacement = "\\2", g.subset$casilla)  # remainder casillas in bunch
    #tail(first,50)
    #tail(rest,50)
    g.subset$casilla <- first # subset reports 1st casilla
    g.dupli$casilla <- rest   # dupli reports rest (for next loop)
    g[sel.r,] <- g.subset #  return manipulation
    g <- rbind(g, g.dupli) # append duplicate
    sel.r <- grep("[|]", g$casilla) # update target rows for next loop
}
#
# sort
g <- g[order(g$edon, g$seccion, g$casilla),]
#
# homogenize casilla nomenclature
c <- g$casilla
table(c)
c <- sub("C([1-9])$", paste0("C0","\\1"), c)
g$casilla <- c
#
########################
## 2006 dip fed votes ##
########################
v <- read.csv(file=paste0(vd, "dip2006.csv"))
c <- v$casilla
table(c)
c <- sub("E0", "E", c)
c <- sub("SMR0", "S", c)
v$casilla <- c
# verify same nomenclature
names(table(v$casilla))==names(table(g$casilla))
#
g2 <- g
v2 <- v
v2$id <- factor(paste(v2$edon*10000 + v2$seccion, v2$casilla, sep = "-"))
g2$id <- factor(paste(g2$edon*10000 + g2$seccion, g2$casilla, sep = "-"))
g2$edon <- g2$seccion <- g2$casilla <- NULL
v2 <- merge(x = v2, y = g2, by = "id", all.x = TRUE, all.y = FALSE)
#
head(g2)
head(v2)
#
v2 <- v2[order(v2$edon, v2$seccion, v2$casilla),]
table(is.na(v2$latitude)) # missings come from casillas_2006
#v2$id[is.na(v2$latitude)]
table(is.na(g2$latitude))
#
# replace with manipulated version
v2$id <- NULL
head(v2)
# write.csv(v2, file=paste0(vd, "dip2006.csv"), row.names = FALSE)

#############################
## 2006 presidential votes ##
#############################
v <- read.csv(file=paste0(vd, "pre2006.csv"))
c <- v$casilla
table(c)
c <- sub("E0", "E", c)
c <- sub("S0", "S", c)
v$casilla <- c
# verify same nomenclature
names(table(v$casilla))==names(table(g$casilla))
#
g2 <- g
v2 <- v
v2$id <- factor(paste(v2$edon*10000 + v2$seccion, v2$casilla, sep = "-"))
g2$id <- factor(paste(g2$edon*10000 + g2$seccion, g2$casilla, sep = "-"))
g2$edon <- g2$seccion <- g2$casilla <- NULL
v2 <- merge(x = v2, y = g2, by = "id", all.x = TRUE, all.y = FALSE)
#
head(g2)
head(v2)
#
v2 <- v2[order(v2$edon, v2$seccion, v2$casilla),]
table(is.na(v2$latitude)) # missings come from casillas_2006
#v2$id[is.na(v2$latitude)]
table(is.na(g2$latitude))
#
# replace with manipulated version
v2$id <- NULL
v2 <- v2[order(v2$ord),]
head(v2)
# write.csv(v2, file=paste0(vd, "pre2006.csv"), row.names = FALSE)


###################
## 2009 lat long ##
###################
#
# make 2009 files have one casilla per row 
g <- read.csv(file=paste0(gd, "casillas_2009.csv"))
# drop redundant columns
g <- g[, c("edon","seccion","casilla","latitude","longitude")]
#
sel.r <- grep("[|]", g$casilla) # cases reporting >1 casillas need to be 'exploded'
#
while(length(sel.r)>0){
    print(paste(length(sel.r), "obs to be manipulated"))
    g.subset <- g[sel.r,] # subset needs manipulation
    #g.subset$casilla
    g.dupli <- g.subset  # duplicate
    first <- sub(pattern = "(^[A-Z0-9]+)[|](.*$)", replacement = "\\1", g.subset$casilla) # get 1st casilla in bunch
    rest <-  sub(pattern = "(^[A-Z0-9]+)[|](.*$)", replacement = "\\2", g.subset$casilla) # remainder casillas in bunch
    #tail(first,50)
    #tail(rest,50)
    #first
    #rest
    g.subset$casilla <- first # subset reports 1st casilla
    g.dupli$casilla <- rest   # dupli reports rest (for next loop)
    g[sel.r,] <- g.subset #  return manipulation
    g <- rbind(g, g.dupli) # append duplicate
    sel.r <- grep("[|]", g$casilla) # update target rows for next loop
}
# sort
g <- g[order(g$edon, g$seccion, g$casilla),]
#
# homogenize casilla nomenclature
c <- g$casilla
table(c)
c <- sub("C([1-9])$", paste0("C0","\\1"), c)
c <- sub("E([1-9])$", paste0("E0","\\1"), c)
c <- sub("E([1-9])C", paste0("E0","\\1","C"), c)
g$casilla <- c
#
# 2009 dip fed votes
v <- read.csv(file=paste0(vd, "dip2009.csv"))
#
c <- v$casilla
table(c)
c <- sub("SMR0", "S", c)
v$casilla <- c
# verify same nomenclature
names(table(v$casilla))==names(table(g$casilla))
#
g2 <- g
v2 <- v
v2$id <- factor(paste(v2$edon*10000 + v2$seccion, v2$casilla, sep = "-"))
g2$id <- factor(paste(g2$edon*10000 + g2$seccion, g2$casilla, sep = "-"))
g2$edon <- g2$seccion <- g2$casilla <- NULL
v2 <- merge(x = v2, y = g2, by = "id", all.x = TRUE, all.y = FALSE)
#
head(g2)
head(v2)
#
v2 <- v2[order(v2$edon, v2$seccion, v2$casilla),]
table(is.na(v2$latitude)) # missing comes from casillas_2009
v2$id[is.na(v2$latitude)]
table(is.na(g2$latitude))
#
# replace with manipulated version
v2$id <- NULL
head(v2)
# write.csv(v2, file=paste0(vd, "dip2009.csv"), row.names = FALSE)

############################################################################
## 2012 lat long casilla names and votes casilla names have huge mismatch ##
## need to inquire with Registro to clarify, maybe another casillas_2012  ##
## file is needed                                                         ##
## WILL NOT EXPORT THESE COORDINATES TO VOTES FILES                       ##
############################################################################
#
# make 2012 files have one casilla per row 
g <- read.csv(file=paste0(gd, "casillas_2012.csv"))
# drop redundant columns
g <- g[, c("edon","seccion","casilla","latitude","longitude")]
#
sel.r <- grep("[|]", g$casilla) # cases reporting >1 casillas need to be 'exploded'
#
while(length(sel.r)>0){
    print(paste(length(sel.r), "obs to be manipulated"))
    g.subset <- g[sel.r,] # subset needs manipulation
    #g.subset$casilla
    g.dupli <- g.subset  # duplicate
    first <- sub(pattern = "(^[A-Z0-9]+)[|](.*$)", replacement = "\\1", g.subset$casilla) # get 1st casilla in bunch
    rest <-  sub(pattern = "(^[A-Z0-9]+)[|](.*$)", replacement = "\\2", g.subset$casilla) # remainder casillas in bunch
    #tail(first,50)
    #tail(rest,50)
    #first
    #rest
    g.subset$casilla <- first # subset reports 1st casilla
    g.dupli$casilla <- rest   # dupli reports rest (for next loop)
    g[sel.r,] <- g.subset #  return manipulation
    g <- rbind(g, g.dupli) # append duplicate
    sel.r <- grep("[|]", g$casilla) # update target rows for next loop
}
# sort
g <- g[order(g$edon, g$seccion, g$casilla),]
#
# homogenize casilla nomenclature
c <- g$casilla
table(c)
c <- sub("C([1-9])$", paste0("C0","\\1"), c)
c <- sub("E([1-9])$", paste0("E0","\\1"), c)
c <- sub("E([1-9])C", paste0("E0","\\1","C"), c)
g$casilla <- c
#
########################
## 2012 dip fed votes ##
########################
v <- read.csv(file=paste0(vd, "dip2012.csv"))
c <- v$casilla
table(c)
c <- sub("SMR0", "S", c)
v$casilla <- c
# casilla name mismatch between v and g:
print("These casilla names in v not in g:"); names(table(v$casilla))[which(names(table(v$casilla)) %notin% names(table(g$casilla)))]
print("These casilla names in g not in v:"); names(table(g$casilla))[which(names(table(g$casilla)) %notin% names(table(v$casilla)))]
#
g2 <- g
v2 <- v
v2$id <- factor(paste(v2$edon*10000 + v2$seccion, v2$casilla, sep = "-"))
g2$id <- factor(paste(g2$edon*10000 + g2$seccion, g2$casilla, sep = "-"))
g2$edon <- g2$seccion <- g2$casilla <- NULL
v2 <- merge(x = v2, y = g2, by = "id", all.x = TRUE, all.y = FALSE)
#
head(g2)
head(v2)
#
v2$id[which(v2$id %notin% g2$id)]
#
v2 <- v2[order(v2$edon, v2$seccion, v2$casilla),]
#
sel.m <- which(is.na(v2$latitude)) # missing comes from casillas_2012
sel.m <- sel.m[grep("^C", v2$casilla[sel.m])] # subset missing contiguas
#
sel.nm <- which(!is.na(v2$latitude)) # non-missing comes from casillas_2012
sel.nm <- sel.nm[grep("^B", v2$casilla[sel.nm])] # subset non-missing basicas
#
which(v2$seccion[sel.nm] %in% v2$seccion[sel.m])
#
tmp <- v2$id[is.na(v2$latitude)]
#
tmp[grep("C",tmp)]
#
table(is.na(g2$latitude))
#
# replace with manipulated version
v2$id <- NULL
head(v2)
# write.csv(v2, file=paste0(vd, "dip2012.csv"), row.names = FALSE)

########################
## 2012 presid votes  ##
########################
v <- read.csv(file=paste0(vd, "pre2012.csv"))
c <- v$casilla
table(c)
c <- sub("^M0([1-9]{1})$", "M00\\1", c)
c <- sub("^M([0-9]{2})$", "M0\\1", c)
v$casilla <- c
# casilla name mismatch between v and g:
print("These casilla names in v not in g:"); names(table(v$casilla))[which(names(table(v$casilla)) %notin% names(table(g$casilla)))]
print("These casilla names in g not in v:"); names(table(g$casilla))[which(names(table(g$casilla)) %notin% names(table(v$casilla)))]
#
g2 <- g
v2 <- v
v2$id <- factor(paste(v2$edon*10000 + v2$seccion, v2$casilla, sep = "-"))
g2$id <- factor(paste(g2$edon*10000 + g2$seccion, g2$casilla, sep = "-"))
g2$edon <- g2$seccion <- g2$casilla <- NULL
v2 <- merge(x = v2, y = g2, by = "id", all.x = TRUE, all.y = FALSE)
#
head(g2)
head(v2)
#
v2$id[which(v2$id %notin% g2$id)]
#
v2 <- v2[order(v2$edon, v2$seccion, v2$casilla),]
#
sel.m <- which(is.na(v2$latitude)) # missing comes from casillas_2012
sel.m <- sel.m[grep("^C", v2$casilla[sel.m])] # subset missing contiguas
#
sel.nm <- which(!is.na(v2$latitude)) # non-missing comes from casillas_2012
sel.nm <- sel.nm[grep("^B", v2$casilla[sel.nm])] # subset non-missing basicas
#
which(v2$seccion[sel.nm] %in% v2$seccion[sel.m])
#
tmp <- v2$id[is.na(v2$latitude)]
#
tmp[grep("C",tmp)]
#
table(is.na(g2$latitude))
#
# replace with manipulated version
v2$id <- NULL
head(v2)
# write.csv(v2, file=paste0(vd, "pre2012.csv"), row.names = FALSE)

###################
## 2015 lat long ##
###################
#
# 2015 file already has one casilla per row 
g <- read.csv(file=paste0(gd, "casillas_2015.csv"))
# drop redundant columns
g <- g[, c("edon","seccion","casilla","latitude","longitude")]
#
# sort
g <- g[order(g$edon, g$seccion, g$casilla),]
#
# 2015 votes
v <- read.csv(file=paste0(vd, "dip2015.csv"))
#
# homogenize casilla nomenclature
c <- g$casilla
table(c)
c <- sub("C([1-9])$", paste0("C0","\\1"), c)
c <- sub("E([1-9])$", paste0("E0","\\1"), c)
c <- sub("E([1-9])C", paste0("E0","\\1","C"), c)
g$casilla <- c
#
c <- v$casilla
table(c)
c <- sub("SMR0", "S", c)
v$casilla <- c
# verify same nomenclature
names(table(v$casilla))==names(table(g$casilla))
#
g2 <- g
v2 <- v
v2$id <- factor(paste(v2$edon*10000 + v2$seccion, v2$casilla, sep = "-"))
g2$id <- factor(paste(g2$edon*10000 + g2$seccion, g2$casilla, sep = "-"))
g2$edon <- g2$seccion <- g2$casilla <- NULL
v2 <- merge(x = v2, y = g2, by = "id", all.x = TRUE, all.y = FALSE)
#
head(g2)
head(v2)
#
v2 <- v2[order(v2$edon, v2$seccion, v2$casilla),]
table(is.na(v2$latitude)) # missings come from casillas_2015
v2$id[is.na(v2$latitude)]
table(is.na(g2$latitude))
#
# replace with manipulated version
v2$id <- NULL
head(v2)
# write.csv(v2, file=paste0(vd, "dip2015.csv"), row.names = FALSE)

###################
## 2018 lat long ##
###################
#
# make 2018 files have one casilla per row 
g <- read.csv(file=paste0(gd, "casillas_2018.csv"))
# drop redundant columns
g$latitude <- g$latitude_car; g$longitude <- g$longitude_car # keep cartografía's coords only
g <- g[, c("edon","seccion","casilla","latitude","longitude")]
#
sel.r <- grep("[|]", g$casilla) # cases reporting >1 casillas need to be 'exploded'
#
while(length(sel.r)>0){
    print(paste(length(sel.r), "obs to be manipulated"))
    g.subset <- g[sel.r,] # subset needs manipulation
    g.dupli <- g.subset  # duplicate
    first <- sub(pattern = "(^[A-Z0-9]+)[|](.*$)", replacement = "\\1", g.subset$casilla) # get 1st casilla in bunch
    rest <- sub(pattern = "(^[A-Z0-9]+)[|](.*$)", replacement = "\\2", g.subset$casilla)  # remainder casillas in bunch
    g.subset$casilla <- first # subset reports 1st casilla
    g.dupli$casilla <- rest   # dupli reports rest (for next loop)
    g[sel.r,] <- g.subset #  return manipulation
    g <- rbind(g, g.dupli) # append duplicate
    sel.r <- grep("[|]", g$casilla) # update target rows for next loop
}
# sort
g <- g[order(g$edon, g$seccion, g$casilla),]
#
# homogenize casilla nomenclature
c <- g$casilla
table(c)
c <- sub("C([1-9])$", paste0("C0","\\1"), c)
g$casilla <- c
#
########################
## 2018 dip fed votes ##
########################
v <- read.csv(file=paste0(vd, "dip2018.csv"))
table(v$casilla)
c <- v$casilla
c <- sub("^1B$", "B", c)
c <- sub("^([0-9]+)E", paste0("E","\\1"), c)
c <- sub("[.]", "C", c)
c <- sub("^([0-9]+)S", paste0("S","\\1"), c)
c <- sub("^([0-9]+)C$", paste0("C","\\1"), c)
c <- sub("C([1-9])$", paste0("C0","\\1"), c)
table(c)
v$casilla <- c
# verify same nomenclature
names(table(v$casilla))==names(table(g$casilla))
#
g2 <- g
v2 <- v
v2$id <- factor(paste(v2$edon*10000 + v2$seccion, v2$casilla, sep = "-"))
g2$id <- factor(paste(g2$edon*10000 + g2$seccion, g2$casilla, sep = "-"))
g2$edon <- g2$seccion <- g2$casilla <- NULL
v2 <- merge(x = v2, y = g2, by = "id", all.x = TRUE, all.y = FALSE)
#
head(g2)
head(v2)
#
v2 <- v2[order(v2$edon, v2$seccion, v2$casilla),]
table(is.na(v2$latitude))
v2[is.na(v2$latitude),] # all missing were unused precincts
table(is.na(g2$latitude))
#
# replace with manipulated version
v2$id <- NULL
head(v2)
# write.csv(v2, file=paste0(vd, "dip2018.csv"), row.names = FALSE)

#############################
## 2018 presidential votes ##
#############################
v <- read.csv(file=paste0(vd, "pre2018.csv"))
c <- v$casilla
table(c)
c <- sub("^1B$", "B", c)
c <- sub("^1M$", "M", c)
c <- sub("^([0-9]+)E", paste0("E","\\1"), c)
c <- sub("[.]", "C", c)
c <- sub("^([0-9]+)S", paste0("S","\\1"), c)
c <- sub("^([0-9]+)C$", paste0("C","\\1"), c)
c <- sub("C([1-9])$", paste0("C0","\\1"), c)
v$casilla <- c
# verify same nomenclature
names(table(v$casilla)) # pres has M casillas
names(table(g$casilla))
#
g2 <- g
v2 <- v
v2$id <- factor(paste(v2$edon*10000 + v2$seccion, v2$casilla, sep = "-"))
g2$id <- factor(paste(g2$edon*10000 + g2$seccion, g2$casilla, sep = "-"))
g2$edon <- g2$seccion <- g2$casilla <- NULL
v2 <- merge(x = v2, y = g2, by = "id", all.x = TRUE, all.y = FALSE)
#
head(g2)
head(v2)
#
v2 <- v2[order(v2$edon, v2$seccion, v2$casilla),]
table(is.na(v2$latitude))
v2[is.na(v2$latitude),] # all missing were unused precincts or M (expatriates)
table(is.na(g2$latitude))
#
# replace with manipulated version
v2$id <- NULL
head(v2)
# write.csv(v2, file=paste0(vd, "pre2018.csv"), row.names = FALSE)

