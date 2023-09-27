##########################################################################################
## Script invoked within code/elec-data-for-maps.r to manipulate changes in municipio   ##
## populations due to remunicipalización and minor adjustments to municipal borders.    ##
##                                                                                      ##
## Exports objects with municipio-level longitudinal populations:                       ##
##                                                                                      ##
##     - MXelsCalendGovt/censos/data/pob18/p18mu-for-municipal-elecs.csv,               ##
##       for municipal election analysis, has projections with all years since 1990,    ##
##       municipios adjusted to municipal election calendars                            ##
##                                                                                      ##
##     - MXelsCalendGovt/censos/data/pob18/p18mu-for-federal-elecs.csv,                 ##
##       for federal election analysis, has projections with fed elec yrs since 1991,   ##
##       municipios adjusted to federal election calendars                              ##
##                                                                                      ##
## Differences stem from new municipios often electing municipal governments many years ##
## before/after they are used as units in federal elections.                            ##
##                                                                                      ##
## Author: Eric Magar                                                                   ##
## emagar at itam dot mx                                                                ##
## Date: 1aug2023                                                                       ##
## Last modified: 26sep2023                                                             ##
##########################################################################################


####################################################################################
## Routine aggregates municipios and distrito censuses from secciones.            ##
## Used to be part of code/aggregates-mun-dis-from-sec, but brought here because  ##
## aggregates need manipulation to prevent double counting reseccionamiento cases ##
####################################################################################
#############################################
## census indicators: aggregate municipios ##
#############################################
sel.c <- c("p18_2005","p18_2010","p18_2020","ptot_2005","ptot_2010","ptot_2020")
# 1994 municipalities
d <- censo; d[is.na(d)] <- 0
d$ife <- d$ife1994                                       # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))                   # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]                       # drop from aggregation
stmp <- which(d$baja>0 & (d$alta>1994 | d$baja<=1994))   # bajas/altas after/before 1994
if (length(stmp)>0) d <- d[-stmp,]                       # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]             # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]             # drop counterfactual districts
##
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE)     # use aggregating function
d <- within(d, {
    seccion <- ord <- NULL                               # drop seccion ids and other useless columns
    alta <- baja <- OBSERVACIONES <- NULL
    action  <- orig.dest  <- when  <- NULL
    action2 <- orig.dest2 <- when2 <- NULL
    action3 <- orig.dest3 <- when3 <- NULL
    coment <- dmunchg <- NULL
})
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
d$inegi <- NA; d$inegi <- ife2inegi(d$ife)     # translate to inegi
d$mun <- ife2mun(d$ife)                        # translate to mun
censom94 <- d                                  # rename object
# 1997 municipalities
d <- censo; d[is.na(d)] <- 0
d$ife <- d$ife1997                                       # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
stmp <- which(d$baja>0 & (d$alta>1997 | d$baja<=1997))   # bajas/altas after/before 1997
if (length(stmp)>0) d <- d[-stmp,]                       # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]             # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]             # drop counterfactual districts
##
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE)     # use aggregating function
d <- within(d, {
    seccion <- ord <- NULL                               # drop seccion ids and other useless columns
    alta <- baja <- OBSERVACIONES <- NULL
    action  <- orig.dest  <- when  <- NULL
    action2 <- orig.dest2 <- when2 <- NULL
    action3 <- orig.dest3 <- when3 <- NULL
    coment <- dmunchg <- NULL
})
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
d$inegi <- NA; d$inegi <- ife2inegi(d$ife)     # translate to inegi
d$mun <- ife2mun(d$ife)                        # translate to mun
censom97 <- d                                  # rename object
# 2000 municipalities
d <- censo; d[is.na(d)] <- 0
d$ife <- d$ife2000                                       # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
stmp <- which(d$baja>0 & (d$alta>2000 | d$baja<=2000))   # bajas/altas after/before 2000
if (length(stmp)>0) d <- d[-stmp,]                       # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]             # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]             # drop counterfactual districts
##
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d <- within(d, {
    seccion <- ord <- NULL                       # drop seccion ids and other useless columns
    alta <- baja <- OBSERVACIONES <- NULL
    action  <- orig.dest  <- when  <- NULL
    action2 <- orig.dest2 <- when2 <- NULL
    action3 <- orig.dest3 <- when3 <- NULL
    coment <- dmunchg <- NULL
})
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
d$inegi <- NA; d$inegi <- ife2inegi(d$ife)     # translate to inegi
d$mun <- ife2mun(d$ife)                        # translate to mun
censom00 <- d                                  # rename object
# 2003 municipalities
d <- censo; d[is.na(d)] <- 0
d$ife <- d$ife2003                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
stmp <- which(d$baja>0 & (d$alta>2003 | d$baja<=2003))   # bajas/altas after/before 2003
if (length(stmp)>0) d <- d[-stmp,]                       # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
##
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d <- within(d, {
    seccion <- ord <- NULL                       # drop seccion ids and other useless columns
    alta <- baja <- OBSERVACIONES <- NULL
    action  <- orig.dest  <- when  <- NULL
    action2 <- orig.dest2 <- when2 <- NULL
    action3 <- orig.dest3 <- when3 <- NULL
    coment <- dmunchg <- NULL
})
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
d$inegi <- NA; d$inegi <- ife2inegi(d$ife)     # translate to inegi
d$mun <- ife2mun(d$ife)                        # translate to mun
censom03 <- d                                  # rename object
# 2006 municipalities
d <- censo; d[is.na(d)] <- 0
d$ife <- d$ife2006                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
stmp <- which(d$baja>0 & (d$alta>2006 | d$baja<=2006))   # bajas/altas after/before 2006
if (length(stmp)>0) d <- d[-stmp,]                       # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
##
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d <- within(d, {
    seccion <- ord <- NULL                       # drop seccion ids and other useless columns
    alta <- baja <- OBSERVACIONES <- NULL
    action  <- orig.dest  <- when  <- NULL
    action2 <- orig.dest2 <- when2 <- NULL
    action3 <- orig.dest3 <- when3 <- NULL
    coment <- dmunchg <- NULL
})
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
d$inegi <- NA; d$inegi <- ife2inegi(d$ife)     # translate to inegi
d$mun <- ife2mun(d$ife)                        # translate to mun
censom06 <- d                                  # rename object
# 2009 municipalities
d <- censo; d[is.na(d)] <- 0
d$ife <- d$ife2009                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
stmp <- which(d$baja>0 & (d$alta>2009 | d$baja<=2009))   # bajas/altas after/before 2009
if (length(stmp)>0) d <- d[-stmp,]                       # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
##
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d <- within(d, {
    seccion <- ord <- NULL                       # drop seccion ids and other useless columns
    alta <- baja <- OBSERVACIONES <- NULL
    action  <- orig.dest  <- when  <- NULL
    action2 <- orig.dest2 <- when2 <- NULL
    action3 <- orig.dest3 <- when3 <- NULL
    coment <- dmunchg <- NULL
})
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
d$inegi <- NA; d$inegi <- ife2inegi(d$ife)     # translate to inegi
d$mun <- ife2mun(d$ife)                        # translate to mun
censom09 <- d                                  # rename object
# 2012 municipalities
d <- censo; d[is.na(d)] <- 0
d$ife <- d$ife2012                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
stmp <- which(d$baja>0 & (d$alta>2012 | d$baja<=2012))   # bajas/altas after/before 2012
if (length(stmp)>0) d <- d[-stmp,]                       # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
##
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d <- within(d, {
    seccion <- ord <- NULL                       # drop seccion ids and other useless columns
    alta <- baja <- OBSERVACIONES <- NULL
    action  <- orig.dest  <- when  <- NULL
    action2 <- orig.dest2 <- when2 <- NULL
    action3 <- orig.dest3 <- when3 <- NULL
    coment <- dmunchg <- NULL
})
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
d$inegi <- NA; d$inegi <- ife2inegi(d$ife)     # translate to inegi
d$mun <- ife2mun(d$ife)                        # translate to mun
censom12 <- d                                  # rename object
# 2015 municipalities
d <- censo; d[is.na(d)] <- 0
d$ife <- d$ife2015                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
stmp <- which(d$baja>0 & (d$alta>2015 | d$baja<=2015))   # bajas/altas after/before 2015
if (length(stmp)>0) d <- d[-stmp,]                       # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
##
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d <- within(d, {
    seccion <- ord <- NULL                       # drop seccion ids and other useless columns
    alta <- baja <- OBSERVACIONES <- NULL
    action  <- orig.dest  <- when  <- NULL
    action2 <- orig.dest2 <- when2 <- NULL
    action3 <- orig.dest3 <- when3 <- NULL
    coment <- dmunchg <- NULL
})
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
d$inegi <- NA; d$inegi <- ife2inegi(d$ife)     # translate to inegi
d$mun <- ife2mun(d$ife)                        # translate to mun
censom15 <- d                                  # rename object
# 2018 municipalities
d <- censo; d[is.na(d)] <- 0
d$ife <- d$ife2018                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
stmp <- which(d$baja>0 & (d$alta>2018 | d$baja<=2018))   # bajas/altas after/before 2018
if (length(stmp)>0) d <- d[-stmp,]                       # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
##
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d <- within(d, {
    seccion <- ord <- NULL                       # drop seccion ids and other useless columns
    alta <- baja <- OBSERVACIONES <- NULL
    action  <- orig.dest  <- when  <- NULL
    action2 <- orig.dest2 <- when2 <- NULL
    action3 <- orig.dest3 <- when3 <- NULL
    coment <- dmunchg <- NULL
})
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
d$inegi <- NA; d$inegi <- ife2inegi(d$ife)     # translate to inegi
d$mun <- ife2mun(d$ife)                        # translate to mun
censom18 <- d                                  # rename object
# 2021 municipalities
d <- censo; d[is.na(d)] <- 0
d$ife <- d$ife2021                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
stmp <- which(d$baja>0 & (d$alta>2021 | d$baja<=2021))   # bajas/altas after/before 2021
if (length(stmp)>0) d <- d[-stmp,]                       # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
##
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d <- within(d, {
    seccion <- ord <- NULL                       # drop seccion ids and other useless columns
    alta <- baja <- OBSERVACIONES <- NULL
    action  <- orig.dest  <- when  <- NULL
    action2 <- orig.dest2 <- when2 <- NULL
    action3 <- orig.dest3 <- when3 <- NULL
    coment <- dmunchg <- NULL
})
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
d$inegi <- NA; d$inegi <- ife2inegi(d$ife)     # translate to inegi
d$mun <- ife2mun(d$ife)                        # translate to mun
censom21 <- d                                  # rename object
#
rm(d,sel.c,add.miss.mun,stmp)
##
############################################
## census indicators: aggregate distritos ##
############################################
sel.c <- c("p18_2005","p18_2010","p18_2020","ptot_2005","ptot_2010","ptot_2020")
## 1979 map
d <- censo; d[is.na(d)] <- 0
d$disn <- d$dis1979                            # district ids for the historic map
sel.drop <- which(d$disn==0)                   # drop secciones de baja
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones de baja
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
##
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- within(d, {
    seccion <- ord <- NULL                       # drop seccion ids and other useless columns
    ife <- inegi <- NULL
    alta <- baja <- OBSERVACIONES <- NULL
    action  <- orig.dest  <- when  <- NULL
    action2 <- orig.dest2 <- when2 <- NULL
    action3 <- orig.dest3 <- when3 <- NULL
    coment <- dmunchg <- NULL
})
d <- d[order(d$disn),]                         # sort
d <- d[moveme(names(d), "disn before edon")]   # order columns
censod79 <- d                                  # rename object
## 1997 map
d <- censo; d[is.na(d)] <- 0
d$disn <- d$dis1997                            # district ids for the historic map
sel.drop <- which(d$disn==0)                   # drop secciones de baja
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones de baja
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
##
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- within(d, {
    seccion <- ord <- NULL                       # drop seccion ids and other useless columns
    ife <- inegi <- NULL
    alta <- baja <- OBSERVACIONES <- NULL
    action  <- orig.dest  <- when  <- NULL
    action2 <- orig.dest2 <- when2 <- NULL
    action3 <- orig.dest3 <- when3 <- NULL
    coment <- dmunchg <- NULL
})
d <- d[order(d$disn),]                         # sort
d <- d[moveme(names(d), "disn before edon")]   # order columns
censod97 <- d                                  # rename object
## 2006 map
d <- censo; d[is.na(d)] <- 0
d$disn <- d$dis2006                            # district ids for the historic map
sel.drop <- which(d$disn==0)                   # drop secciones de baja
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones de baja
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
##
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- within(d, {
    seccion <- ord <- NULL                       # drop seccion ids and other useless columns
    ife <- inegi <- NULL
    alta <- baja <- OBSERVACIONES <- NULL
    action  <- orig.dest  <- when  <- NULL
    action2 <- orig.dest2 <- when2 <- NULL
    action3 <- orig.dest3 <- when3 <- NULL
    coment <- dmunchg <- NULL
})
d <- d[order(d$disn),]                         # sort
d <- d[moveme(names(d), "disn before edon")]   # order columns
censod06 <- d                                  # rename object
## 2018 map ##
d <- censo; d[is.na(d)] <- 0
d$disn <- d$dis2018                            # district ids for the historic map
sel.drop <- which(d$disn==0)                   # drop secciones de baja
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones de baja
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
##
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- within(d, {
    seccion <- ord <- NULL                       # drop seccion ids and other useless columns
    ife <- inegi <- NULL
    alta <- baja <- OBSERVACIONES <- NULL
    action  <- orig.dest  <- when  <- NULL
    action2 <- orig.dest2 <- when2 <- NULL
    action3 <- orig.dest3 <- when3 <- NULL
    coment <- dmunchg <- NULL
})
d <- d[order(d$disn),]                         # sort
d <- d[moveme(names(d), "disn before edon")]   # order columns
censod18 <- d                                  # rename object
#
rm(d,sel.c,sel.drop)





#########################################
## functions to gen elyr pops linearly ##
#########################################
prj9095 <- function(x=NA,yr=NA){
    chg <- (x$p18_1995 - x$p18_1990) / 5 # yearly pop change
    pop <- x$p18_1990 + chg * (yr - 1990)
    return(pop)
}
prj9500 <- function(x=NA,yr=NA){
    chg <- (x$p18_2000 - x$p18_1995) / 5 # yearly pop change
    pop <- x$p18_1995 + chg * (yr - 1995)
    return(pop)
}
prj0005 <- function(x=NA,yr=NA){
    chg <- (x$p18_2005 - x$p18_2000) / 5 # yearly pop change
    pop <- x$p18_2000 + chg * (yr - 2000)
    return(pop)
}
prj0510 <- function(x=NA,yr=NA){
    chg <- (x$p18_2010 - x$p18_2005) / 5 # yearly pop change
    pop <- x$p18_2005 + chg * (yr - 2005)
    return(pop)
}
prj1020 <- function(x=NA,yr=NA){
    chg <- (x$p18_2020 - x$p18_2010) / 10 # yearly pop change
    pop <- x$p18_2010 + chg * (yr - 2010)
    return(pop)
}
prj9000 <- function(x=NA,yr=NA){
    chg <- (x$p18_2000 - x$p18_1990) / 10 # yearly pop change
    pop <- x$p18_1990 + chg * (yr - 1990)
    return(pop)
}
prj9505 <- function(x=NA,yr=NA){
    chg <- (x$p18_2005 - x$p18_1995) / 10 # yearly pop change
    pop <- x$p18_1995 + chg * (yr - 1995)
    return(pop)
}



###################################################################################################
## Add municipio-level 1990:2000 pob18 (fills up for missing seccion-level data for those years) ##
## OJO: Actual municipios only, even for counterfactual maps                                     ##
###################################################################################################
censom <- within(censom, {
    p18_2000 <- as.numeric(pob18_2000);
    p18_1995 <- pob18_1995;
    p18_1990 <- pob18_1990;
    pob18_1990 <- pob18_1995 <- pob18_2000 <- NULL;
})
#########
## m00 ##
#########
ls()[grep("censo",ls())]
##censom00$ord <- 1:nrow(censom00) # to verify if order changes
##table(is.na(censom00$ife))
censom00 <- merge(x=censom00, y=censom[,c("ife","p18_1990","p18_1995","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE)
##table(censom00$ord - lag(ts(censom00$ord, start=1, end=nrow(censom00), frequency=1), k=1)) # order unchanged if all==0
##censom00$ord <- NULL
##table(is.na(censom00$ife))
##table(is.na(censom00$inegi))
censom00$inegi <- ife2inegi(censom00$ife) # fill missing inegi codes
## sort columns (drops ptot columns, add them again when needed for analysis)
censom00 <- censom00[,c("edon","ife","inegi","mun","p18_1990","p18_1995","p18_2000","p18_2005","p18_2010","p18_2020")]
#########
## m94 ##
#########
censom94 <- merge(x=censom94, y=censom[,c("ife","p18_1990","p18_1995","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE)
censom94$inegi <- ife2inegi(censom94$ife) # fill missing inegi codes
## sort columns (drops ptot columns, add them again if needed for analysis)
censom94 <- censom94[,c("edon","ife","inegi","mun","p18_1990","p18_1995","p18_2000","p18_2005","p18_2010","p18_2020")]
#########
## m97 ##
#########
censom97 <- merge(x=censom97, y=censom[,c("ife","p18_1990","p18_1995","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE)
censom97$inegi <- ife2inegi(censom97$ife) # fill missing inegi codes
## sort columns (drops ptot columns, add them again if needed for analysis)
censom97 <- censom97[,c("edon","ife","inegi","mun","p18_1990","p18_1995","p18_2000","p18_2005","p18_2010","p18_2020")]
#########
## m03 ##
#########
censom03 <- merge(x=censom03, y=censom[,c("ife","p18_1990","p18_1995","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE)
censom03$inegi <- ife2inegi(censom03$ife) # fill missing inegi codes
## sort columns (drops ptot columns, add them again if needed for analysis)
censom03 <- censom03[,c("edon","ife","inegi","mun","p18_1990","p18_1995","p18_2000","p18_2005","p18_2010","p18_2020")]
#########
## m06 ##
#########
censom06 <- merge(x=censom06, y=censom[,c("ife","p18_1990","p18_1995","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE)
censom06$inegi <- ife2inegi(censom06$ife) # fill missing inegi codes
## sort columns (drops ptot columns, add them again if needed for analysis)
censom06 <- censom06[,c("edon","ife","inegi","mun","p18_1990","p18_1995","p18_2000","p18_2005","p18_2010","p18_2020")]
#########
## m09 ##
#########
censom09 <- merge(x=censom09, y=censom[,c("ife","p18_1990","p18_1995","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE)
censom09$inegi <- ife2inegi(censom09$ife) # fill missing inegi codes
## sort columns (drops ptot columns, add them again if needed for analysis)
censom09 <- censom09[,c("edon","ife","inegi","mun","p18_1990","p18_1995","p18_2000","p18_2005","p18_2010","p18_2020")]
#########
## m12 ##
#########
censom12 <- merge(x=censom12, y=censom[,c("ife","p18_1990","p18_1995","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE)
censom12$inegi <- ife2inegi(censom12$ife) # fill missing inegi codes
## sort columns (drops ptot columns, add them again if needed for analysis)
censom12 <- censom12[,c("edon","ife","inegi","mun","p18_1990","p18_1995","p18_2000","p18_2005","p18_2010","p18_2020")]
#########
## m15 ##
#########
censom15 <- merge(x=censom15, y=censom[,c("ife","p18_1990","p18_1995","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE)
censom15$inegi <- ife2inegi(censom15$ife) # fill missing inegi codes
## sort columns (drops ptot columns, add them again if needed for analysis)
censom15 <- censom15[,c("edon","ife","inegi","mun","p18_1990","p18_1995","p18_2000","p18_2005","p18_2010","p18_2020")]
#########
## m18 ##
#########
censom18 <- merge(x=censom18, y=censom[,c("ife","p18_1990","p18_1995","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE)
censom18$inegi <- ife2inegi(censom18$ife) # fill missing inegi codes
## sort columns (drops ptot columns, add them again if needed for analysis)
censom18 <- censom18[,c("edon","ife","inegi","mun","p18_1990","p18_1995","p18_2000","p18_2005","p18_2010","p18_2020")]
#########
## m21 ##
#########
censom21 <- merge(x=censom21, y=censom[,c("ife","p18_1990","p18_1995","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE)
censom21$inegi <- ife2inegi(censom21$ife) # fill missing inegi codes
## sort columns (drops ptot columns, add them again if needed for analysis)
censom21 <- censom21[,c("edon","ife","inegi","mun","p18_1990","p18_1995","p18_2000","p18_2005","p18_2010","p18_2020")]
## clean
rm(censom)
##
## NAs to zero
censom94[is.na(censom94)] <- 0 # replace NAs with 0
censom97[is.na(censom97)] <- 0 # replace NAs with 0
censom00[is.na(censom00)] <- 0 # replace NAs with 0
censom03[is.na(censom03)] <- 0 # replace NAs with 0
censom06[is.na(censom06)] <- 0 # replace NAs with 0
censom09[is.na(censom09)] <- 0 # replace NAs with 0
censom12[is.na(censom12)] <- 0 # replace NAs with 0
censom15[is.na(censom15)] <- 0 # replace NAs with 0
censom18[is.na(censom18)] <- 0 # replace NAs with 0
censom21[is.na(censom21)] <- 0 # replace NAs with 0

## ## Ojo: there is a mistake in my prep of counterfactual objects. Below are state aggregates of municipal p18s. Despite inter-municipio changes, that m.. are made to capture, the state population should remain constant across maps. I must be duplicating municipios or secciones. Must check asap, problem seems to be in interpolate-census-data-se-by-se.r
##my_agg(censom94, sel.c="p18_2020", by="edon")[2,10]
##my_agg(censom21, sel.c="p18_2020", by="edon")[2,10]
## edon   object p18_2020     dif
##    2 censom94  1940964      --
##    2 censom97  1943928    2964  
##    2 censom00  1943928       0     
##    2 censom03  1943928       0     
##    2 censom06  2122227  178299
##    2 censom09  2122227       0     
##    2 censom12  2403311  281084
##    2 censom15  2550460  147149
##    2 censom18  2550460       0     
##    2 censom21  2707133  156673


##############################
## Fix parent/son municipio ##
##############################
## Rosarito (created 1998) has 1995 census data, elected mun gov in 1998, into fed els 1997
## must be added to Tijuana in pre-1997 maps
tmp <- censom94      # duplicate for manipulation
tmp[12:17,]
sel.parent <- which(tmp$inegi==2004)
sel.son <-    which(tmp$inegi==2005)
tmp[c(sel.parent, sel.son),]
tmp$p18_1995[sel.parent] <- tmp$p18_1995[sel.parent] + tmp$p18_1995[sel.son] # belongs in Tijuana in 1994 map
tmp$p18_1995[sel.son] <- 0
tmp$p18_2000[sel.parent] <- tmp$p18_2000[sel.parent] + tmp$p18_2000[sel.son] # belongs in Tijuana in 1994 map
tmp$p18_2000[sel.son] <- 0
tmp[c(sel.parent, sel.son),]
tmp -> censom94      # return after manipulation
## needs split 1990 from tj for 1997-on maps (use linear proj)
## wrap in function
mywrap <- function(x=NA){
    x[is.na(x)] <- 0 # replace NAs with 0
    sel.parent <- which(x$inegi==2004)
    sel.son <-    which(x$inegi==2005)
    ##x[c(sel.parent, sel.son),]
    x$p18_1990[sel.son]    <- prj9500(x[sel.son,], 1990) # project
    x$p18_1990[sel.parent] <- x$p18_1990[sel.parent] - x$p18_1990[sel.son] # subtract from parent
    ##x[c(sel.parent, sel.son),]
    return(x)
}
censom97 <- mywrap(censom97) # apply func
censom97[12:17,]
censom00 <- mywrap(censom00) # apply func
censom03 <- mywrap(censom03) # apply func
censom06 <- mywrap(censom06) # apply func
censom09 <- mywrap(censom09) # apply func
censom12 <- mywrap(censom12) # apply func
censom15 <- mywrap(censom15) # apply func
censom18 <- mywrap(censom18) # apply func
censom21 <- mywrap(censom21) # apply func
censom94[c(sel.parent, sel.son),]
censom97[c(sel.parent, sel.son),]
##
## Calakmul (created 1997)
## Elected 1st mu gov 1997 but incorporated to fed els til 2018
tmp <- censom21      # duplicate for manipulation
sel.parent <- which(tmp$inegi==4006)
sel.son <-    which(tmp$inegi==4010)
tmp[c(sel.parent, sel.son),]
tmp$p18_1995[sel.son] <- prj0005(tmp[sel.son,], yr=1995)
tmp$p18_1990[sel.son] <- prj0005(tmp[sel.son,], yr=1990)
tmp$p18_1995[sel.parent] <- tmp$p18_1995[sel.parent] - tmp$p18_1995[sel.son] # subtract from parent
tmp$p18_1990[sel.parent] <- tmp$p18_1990[sel.parent] - tmp$p18_1990[sel.son] # subtract from parent
tmp[c(sel.parent, sel.son),]
tmp -> censom21      # return
## Replicate 2018
censom18[c(sel.parent, sel.son), grep("^p18_", colnames(censom18))] <- censom21[c(sel.parent, sel.son), grep("^p18_", colnames(censom21))]
## Previous fed mu maps have no Calakmul (use 2018 map for mun els 1997:on)
tmp <- censom15      # duplicate for manipulation
tmp[c(sel.parent, sel.son),]
tmp$p18_2000[sel.parent] <- tmp$p18_2000[sel.parent] + tmp$p18_2000[sel.son] # sum offspring
tmp$p18_2005[sel.parent] <- tmp$p18_2005[sel.parent] + tmp$p18_2005[sel.son] # sum offspring
tmp$p18_2010[sel.parent] <- tmp$p18_2010[sel.parent] + tmp$p18_2010[sel.son] # sum offspring
tmp$p18_2020[sel.parent] <- tmp$p18_2020[sel.parent] + tmp$p18_2020[sel.son] # sum offspring
tmp$p18_2000[sel.son] <- 0
tmp$p18_2005[sel.son] <- 0
tmp$p18_2010[sel.son] <- 0
tmp$p18_2020[sel.son] <- 0
tmp[c(sel.parent, sel.son),]
tmp -> censom15      # return
## Replicate 1994:2012
censom12[c(sel.parent, sel.son), grep("^p18_", colnames(censom12))] <- censom15[c(sel.parent, sel.son), grep("^p18_", colnames(censom15))]
censom09[c(sel.parent, sel.son), grep("^p18_", colnames(censom09))] <- censom15[c(sel.parent, sel.son), grep("^p18_", colnames(censom15))]
censom06[c(sel.parent, sel.son), grep("^p18_", colnames(censom06))] <- censom15[c(sel.parent, sel.son), grep("^p18_", colnames(censom15))]
censom03[c(sel.parent, sel.son), grep("^p18_", colnames(censom03))] <- censom15[c(sel.parent, sel.son), grep("^p18_", colnames(censom15))]
censom00[c(sel.parent, sel.son), grep("^p18_", colnames(censom00))] <- censom15[c(sel.parent, sel.son), grep("^p18_", colnames(censom15))]
censom97[c(sel.parent, sel.son), grep("^p18_", colnames(censom97))] <- censom15[c(sel.parent, sel.son), grep("^p18_", colnames(censom15))]
censom94[c(sel.parent, sel.son), grep("^p18_", colnames(censom94))] <- censom15[c(sel.parent, sel.son), grep("^p18_", colnames(censom15))]
##
## Candelaria (created 2000) is easy
## Elected 1st mu gov 2000
## Has census data since 2000
tmp <- censom00      # duplicate for manipulation
sel.parent <- which(tmp$inegi==4003 | tmp$inegi==4009)
sel.son <-    which(tmp$inegi==4011)
tmp[c(sel.parent, sel.son),]
## Prior maps need sums
tmp <- censom97      # duplicate for manipulation
sel.parent <- which(tmp$inegi==4003 | tmp$inegi==4009)
sel.son <-    which(tmp$inegi==4011)
tmp[c(sel.parent, sel.son),]
tmp$p18_2000[tmp$inegi==4003] <- tmp$p18_2000[tmp$inegi==4003] + tmp$p18_2000[tmp$inegi==4011]*.85
tmp$p18_2000[tmp$inegi==4009] <- tmp$p18_2000[tmp$inegi==4009] + tmp$p18_2000[tmp$inegi==4011]*.15
tmp$p18_2000[tmp$inegi==4011] <- 0
tmp$p18_2020[tmp$inegi==4003] <- tmp$p18_2020[tmp$inegi==4003] + tmp$p18_2020[tmp$inegi==4011]*.75
tmp$p18_2020[tmp$inegi==4009] <- tmp$p18_2020[tmp$inegi==4009] + tmp$p18_2020[tmp$inegi==4011]*.25
tmp$p18_2020[tmp$inegi==4011] <- 0
tmp[c(sel.parent, sel.son),]
tmp -> censom97      # return
censom94     [c(sel.parent, sel.son), grep("^p18_", colnames(censom18))] <-
    censom97 [c(sel.parent, sel.son), grep("^p18_", colnames(censom21))]
##
## Seybaplaya (created 2019)
## Elected 1st mu gov 2021
## All looks ok
tmp <- censom21      # duplicate for manipulation
sel.parent <- which(tmp$inegi==4002 | tmp$inegi==4004)
sel.son <-    which(tmp$inegi==4012)
tmp[c(sel.parent, sel.son),]
##tmp -> censom21      # return after manipulation
##
## Dzitbalche (created 2019)
## Elected 1st mu gov 2021
## All looks ok
tmp <- censom21      # duplicate for manipulation
sel.parent <- which(tmp$inegi==4001)
sel.son <-    which(tmp$inegi==4013)
tmp[c(sel.parent, sel.son),]
##tmp -> censom21      # return after manipulation
##
## Aldama, Benemérito, Maravilla, Marqués, Montecristo, Sn Andrés, Santiago (created 2001)
## Elected 1st mu gov 2001 but not incorporated to fed els til 2018
## Use 2018 map for mun els since 2001
## Pre-2018 maps have 2000 that needs to go to parent
tmp <- censom94      # duplicate for manipulation
mywrap <- function(x=NA){
    x[is.na(x)] <- 0 # replace NAs with 0
    sel.parent <- which(x$inegi==7026)
    sel.son <-    which(x$inegi==7113)
    ##x[c(sel.parent, sel.son),]
    x$p18_2000[sel.parent] <- x$p18_2000[sel.parent] + x$p18_2000[sel.son]
    x$p18_2000[sel.son] <- 0
    ##x[c(sel.parent, sel.son),]
    return(x)
}
tmp <- mywrap(tmp)
tmp[c(sel.parent, sel.son),]
tmp -> censom94      # return after manipulation
censom97 <- mywrap(censom97)
censom00 <- mywrap(censom00)
censom03 <- mywrap(censom03)
censom06 <- mywrap(censom06)
censom09 <- mywrap(censom09)
censom12 <- mywrap(censom12)
censom15 <- mywrap(censom15)
##
mywrap <- function(x=NA){
    tmp <- x
        sel.parent <- which(tmp$inegi==7059)
    sel.son <-    which(tmp$inegi==7114)
    ##tmp[c(sel.parent, sel.son),]
    tmp$p18_2000[sel.parent] <- tmp$p18_2000[sel.parent] + tmp$p18_2000[sel.son]
    tmp$p18_2000[sel.son] <- 0
    ##tmp[c(sel.parent, sel.son),]
    return(tmp)
}
censom94 <- mywrap(censom94)
censom97 <- mywrap(censom97)
censom00 <- mywrap(censom00)
censom03 <- mywrap(censom03)
censom06 <- mywrap(censom06)
censom09 <- mywrap(censom09)
censom12 <- mywrap(censom12)
censom15 <- mywrap(censom15)
##
mywrap <- function(x=NA){
    tmp <- x
        sel.parent <- which(tmp$inegi==7052)
    sel.son <-    which(tmp$inegi==7115)
    tmp[c(sel.parent, sel.son),]
    tmp$p18_2000[sel.parent] <- tmp$p18_2000[sel.parent] + tmp$p18_2000[sel.son]
    tmp$p18_2000[sel.son] <- 0
    ##tmp[c(sel.parent, sel.son),]
    return(tmp)
}
censom94 <- mywrap(censom94)
censom97 <- mywrap(censom97)
censom00 <- mywrap(censom00)
censom03 <- mywrap(censom03)
censom06 <- mywrap(censom06)
censom09 <- mywrap(censom09)
censom12 <- mywrap(censom12)
censom15 <- mywrap(censom15)
##
mywrap <- function(x=NA){
    tmp <- x
        sel.parent <- which(tmp$inegi==7059)
    sel.son <-    which(tmp$inegi==7116)
    ##tmp[c(sel.parent, sel.son),]
    tmp$p18_2000[sel.parent] <- tmp$p18_2000[sel.parent] + tmp$p18_2000[sel.son]
    tmp$p18_2000[sel.son] <- 0
    ##tmp[c(sel.parent, sel.son),]
    return(tmp)
}
censom94 <- mywrap(censom94)
censom97 <- mywrap(censom97)
censom00 <- mywrap(censom00)
censom03 <- mywrap(censom03)
censom06 <- mywrap(censom06)
censom09 <- mywrap(censom09)
censom12 <- mywrap(censom12)
censom15 <- mywrap(censom15)
##
mywrap <- function(x=NA){
    tmp <- x
        sel.parent <- which(tmp$inegi==7008)
    sel.son <-    which(tmp$inegi==7117)
    ##tmp[c(sel.parent, sel.son),]
    tmp$p18_2000[sel.parent] <- tmp$p18_2000[sel.parent] + tmp$p18_2000[sel.son]
    tmp$p18_2000[sel.son] <- 0
    ##tmp[c(sel.parent, sel.son),]
    return(tmp)
}
censom94 <- mywrap(censom94)
censom97 <- mywrap(censom97)
censom00 <- mywrap(censom00)
censom03 <- mywrap(censom03)
censom06 <- mywrap(censom06)
censom09 <- mywrap(censom09)
censom12 <- mywrap(censom12)
censom15 <- mywrap(censom15)
##
mywrap <- function(x=NA){
    tmp <- x
        sel.parent <- which(tmp$inegi==7081)
    sel.son <-    which(tmp$inegi==7118)
    ##tmp[c(sel.parent, sel.son),]
    tmp$p18_2000[sel.parent] <- tmp$p18_2000[sel.parent] + tmp$p18_2000[sel.son]
    tmp$p18_2000[sel.son] <- 0
    ##tmp[c(sel.parent, sel.son),]
    return(tmp)
}
censom94 <- mywrap(censom94)
censom97 <- mywrap(censom97)
censom00 <- mywrap(censom00)
censom03 <- mywrap(censom03)
censom06 <- mywrap(censom06)
censom09 <- mywrap(censom09)
censom12 <- mywrap(censom12)
censom15 <- mywrap(censom15)
##
mywrap <- function(x=NA){
    tmp <- x
        sel.parent <- which(tmp$inegi==7049)
    sel.son <-    which(tmp$inegi==7119)
    ##tmp[c(sel.parent, sel.son),]
    tmp$p18_2000[sel.parent] <- tmp$p18_2000[sel.parent] + tmp$p18_2000[sel.son]
    tmp$p18_2000[sel.son] <- 0
    ##tmp[c(sel.parent, sel.son),]
    return(tmp)
}
censom94 <- mywrap(censom94)
censom97 <- mywrap(censom97)
censom00 <- mywrap(censom00)
censom03 <- mywrap(censom03)
censom06 <- mywrap(censom06)
censom09 <- mywrap(censom09)
censom12 <- mywrap(censom12)
censom15 <- mywrap(censom15)
##
## Capitán and Rincón (created 2018)
## Elected 1st mu gov 2018 but not incorporated to fed els til 2021
## Use 2021 map for mun els in 2018
## All looks ok
tmp <- censom21      # duplicate for manipulation
sel.parent <- which(tmp$inegi==7081)
sel.son <-    which(tmp$inegi==7120)
tmp[c(sel.parent, sel.son),]
##
sel.parent <- which(tmp$inegi==7072 | tmp$inegi==7047 | tmp$inegi==7091)
sel.son <-    which(tmp$inegi==7121)
tmp[c(sel.parent, sel.son),]
## for mun els, make m18 equal m21
censom18     [which(censom18$inegi==7047), grep("^p18_", colnames(censom18))] <-
    censom21 [which(censom21$inegi==7047), grep("^p18_", colnames(censom21))]
censom18     [which(censom18$inegi==7072), grep("^p18_", colnames(censom18))] <-
    censom21 [which(censom21$inegi==7072), grep("^p18_", colnames(censom21))]
censom18     [which(censom18$inegi==7081), grep("^p18_", colnames(censom18))] <-
    censom21 [which(censom21$inegi==7081), grep("^p18_", colnames(censom21))]
censom18     [which(censom18$inegi==7091), grep("^p18_", colnames(censom18))] <-
    censom21 [which(censom21$inegi==7091), grep("^p18_", colnames(censom21))]
censom18     [which(censom18$inegi==7120), grep("^p18_", colnames(censom18))] <-
    censom21 [which(censom21$inegi==7120), grep("^p18_", colnames(censom21))]
censom18     [which(censom18$inegi==7121), grep("^p18_", colnames(censom18))] <-
    censom21 [which(censom21$inegi==7121), grep("^p18_", colnames(censom21))]
##
## Parral, Zapata, Mezcalapa, (created 2012)
## Elected 1st mu gov 2012 but not incorporated to fed els til 2021
## Honduras (created 2020)
## Elected 1st mu gov 2021
## Use 2021 map for mun els since 2012
## All look ok
tmp <- censom21      # duplicate for manipulation
sel.parent <- which(tmp$inegi==7107 | tmp$inegi==7108)
sel.son <-    which(tmp$inegi==7122)
tmp[c(sel.parent, sel.son),]
##
tmp <- censom18      # duplicate for manipulation
sel.parent <- which(tmp$inegi==7002 | tmp$inegi==7027 | tmp$inegi==7106)
sel.son <-    which(tmp$inegi==7123)
tmp[c(sel.parent, sel.son),]
##
tmp <- censom18      # duplicate for manipulation
sel.parent <- which(tmp$inegi==7061 | tmp$inegi==7062 | tmp$inegi==7092)
sel.son <-    which(tmp$inegi==7124)
tmp[c(sel.parent, sel.son),]
##
tmp <- censom21      # duplicate for manipulation
sel.parent <- which(tmp$inegi==7081)
sel.son <-    which(tmp$inegi==7125)
tmp[c(sel.parent, sel.son),]
##
## Acatepec (created 1996)
## Elected 1st mu gov 1996
## Pre-97 maps to zero
tmp <- censom94      # duplicate for manipulation
sel.parent <- which(tmp$inegi==12072)
sel.son <-    which(tmp$inegi==12076)
tmp[c(sel.parent, sel.son),]
tmp$p18_1995[sel.parent] <- tmp$p18_1995[sel.parent] + tmp$p18_1995[sel.son]
tmp$p18_1995[sel.son] <- 0
tmp$p18_2000[sel.parent] <- tmp$p18_2000[sel.parent] + tmp$p18_2000[sel.son]
tmp$p18_2000[sel.son] <- 0
tmp -> censom94      # return after manipulation
##
## Marquelia (created 2005)
## Elected 1st mu gov 2005
## Looks ok
tmp <- censom09      # duplicate for manipulation
sel.parent <- which(tmp$inegi==12013)
sel.son <-    which(tmp$inegi==12077)
tmp[c(sel.parent, sel.son),]
##
## Cochoapa, JJ, Juchitán, Iliatenco (created 2008)
## Elected 1st mu gov 2008
## Look ok (some suffered reseccionamiento after 2009)
tmp <- censom09      # duplicate for manipulation
sel.parent <- which(tmp$inegi==12043)
sel.son <-    which(tmp$inegi==12078)
tmp[c(sel.parent, sel.son),]
sel.parent <- which(tmp$inegi==12028)
sel.son <-    which(tmp$inegi==12079)
tmp[c(sel.parent, sel.son),]
sel.parent <- which(tmp$inegi==12013)
sel.son <-    which(tmp$inegi==12080)
tmp[c(sel.parent, sel.son),]
sel.parent <- which(tmp$inegi==12041)
sel.son <-    which(tmp$inegi==12081)
tmp[c(sel.parent, sel.son),]
##
## San Ignacio (created 2006)
## Elected 1st mu gov 2006, into fed els 2012 (use 2012 map for 2006:2009 mun els)
## Looks ok
tmp <- censom12      # duplicate for manipulation
sel.parent <- which(tmp$inegi==14008)
sel.son <-    which(tmp$inegi==14125)
tmp[c(sel.parent, sel.son),]
##
## Luvianos, SJ (created 2003)
## Elected 1st mu gov 2003
## Looks ok
tmp <- censom03      # duplicate for manipulation
sel.parent <- which(tmp$inegi==15074)
sel.son <-    which(tmp$inegi==15123)
tmp[c(sel.parent, sel.son),]
sel.parent <- which(tmp$inegi==15044)
sel.son <-    which(tmp$inegi==15124)
tmp[c(sel.parent, sel.son),]
##
## Tonatitlw (created 2006)
## Elected 1st mu gov 2006
## Looks ok
tmp <- censom06      # duplicate for manipulation
sel.parent <- which(tmp$inegi==15044)
sel.son <-    which(tmp$inegi==15125)
tmp[c(sel.parent, sel.son),]
##
## Solidaridad (created 1996)
## Elected 1st mu gov 1996
## Pre-97 maps need sum
tmp <- censom94      # duplicate for manipulation
sel.parent <- which(tmp$inegi==23001)
sel.son <-    which(tmp$inegi==23008)
tmp[c(sel.parent, sel.son),]
tmp$p18_1995[sel.parent] <- tmp$p18_1995[sel.parent] + tmp$p18_1995[sel.son]
tmp$p18_1995[sel.son] <- 0
tmp$p18_2000[sel.parent] <- tmp$p18_2000[sel.parent] + tmp$p18_2000[sel.son]
tmp$p18_2000[sel.son] <- 0
tmp -> censom94      # return
##
## Tulum (created 2009)
## Elected 1st mu gov 2009, into fed els til 2018
## Use 2018 map for mun els 2008:17
## Use 2003 map for 1990:2007
tmp <- censom03      # duplicate for manipulation
tmp <- censom06      # duplicate for manipulation
sel.parent <- which(tmp$inegi==23008)
sel.son <-    which(tmp$inegi==23009)
tmp[c(sel.parent, sel.son),]
##
## Bacalar (created 2013)
## Elected 1st mu gov 2013, fed els in 2015
## Looks ok
tmp <- censom12      # duplicate for manipulation
sel.parent <- which(tmp$inegi==23004)
sel.son <-    which(tmp$inegi==23010)
tmp[c(sel.parent, sel.son),]
##
## Pto Mor (created 2016)
## Elected 1st mu gov 2016, into fed els til 2018
## Looks ok
tmp <- censom18      # duplicate for manipulation
sel.parent <- which(tmp$inegi==23005)
sel.son <-    which(tmp$inegi==23011)
tmp[c(sel.parent, sel.son),]
##
## Matlapa, Naranjo, Benito Juárez, San Ignacio (created 1997)
## Elected 1st mu gov 1997
## Pre-97 map needs sums
tmp <- censom94      # duplicate for manipulation
sel.parent <- which(tmp$inegi==24037)
sel.son <-    which(tmp$inegi==24057)
tmp[c(sel.parent, sel.son),]
tmp$p18_1995[sel.parent] <- tmp$p18_1995[sel.parent] + tmp$p18_1995[sel.son]
tmp$p18_1995[sel.son] <- 0
tmp$p18_2000[sel.parent] <- tmp$p18_2000[sel.parent] + tmp$p18_2000[sel.son]
tmp$p18_2000[sel.son] <- 0
tmp$p18_2005[sel.parent] <- tmp$p18_2005[sel.parent] + tmp$p18_2005[sel.son]
tmp$p18_2005[sel.son] <- 0
tmp$p18_2010[sel.parent] <- tmp$p18_2010[sel.parent] + tmp$p18_2010[sel.son]
tmp$p18_2010[sel.son] <- 0
tmp$p18_2020[sel.parent] <- tmp$p18_2020[sel.parent] + tmp$p18_2020[sel.son]
tmp$p18_2020[sel.son] <- 0
tmp[c(sel.parent, sel.son),]
sel.parent <- which(tmp$inegi==24010)
sel.son <-    which(tmp$inegi==24058)
tmp[c(sel.parent, sel.son),]
tmp$p18_1995[sel.parent] <- tmp$p18_1995[sel.parent] + tmp$p18_1995[sel.son]
tmp$p18_1995[sel.son] <- 0
tmp$p18_2000[sel.parent] <- tmp$p18_2000[sel.parent] + tmp$p18_2000[sel.son]
tmp$p18_2000[sel.son] <- 0
tmp$p18_2005[sel.parent] <- tmp$p18_2005[sel.parent] + tmp$p18_2005[sel.son]
tmp$p18_2005[sel.son] <- 0
tmp$p18_2010[sel.parent] <- tmp$p18_2010[sel.parent] + tmp$p18_2010[sel.son]
tmp$p18_2010[sel.son] <- 0
tmp$p18_2020[sel.parent] <- tmp$p18_2020[sel.parent] + tmp$p18_2020[sel.son]
tmp$p18_2020[sel.son] <- 0
tmp[c(sel.parent, sel.son),]
##
sel.parent <- which(tmp$inegi==26026)
sel.son <-    which(tmp$inegi==26071)
tmp[c(sel.parent, sel.son),]
tmp$p18_1995[sel.parent] <- tmp$p18_1995[sel.parent] + tmp$p18_1995[sel.son]
tmp$p18_1995[sel.son] <- 0
tmp$p18_2000[sel.parent] <- tmp$p18_2000[sel.parent] + tmp$p18_2000[sel.son]
tmp$p18_2000[sel.son] <- 0
tmp$p18_2005[sel.parent] <- tmp$p18_2005[sel.parent] + tmp$p18_2005[sel.son]
tmp$p18_2005[sel.son] <- 0
tmp$p18_2010[sel.parent] <- tmp$p18_2010[sel.parent] + tmp$p18_2010[sel.son]
tmp$p18_2010[sel.son] <- 0
tmp$p18_2020[sel.parent] <- tmp$p18_2020[sel.parent] + tmp$p18_2020[sel.son]
tmp$p18_2020[sel.son] <- 0
tmp[c(sel.parent, sel.son),]
##
sel.parent <- which(tmp$inegi==26029)
sel.son <-    which(tmp$inegi==26072)
tmp[c(sel.parent, sel.son),]
tmp$p18_1995[sel.parent] <- tmp$p18_1995[sel.parent] + tmp$p18_1995[sel.son]
tmp$p18_1995[sel.son] <- 0
tmp$p18_2000[sel.parent] <- tmp$p18_2000[sel.parent] + tmp$p18_2000[sel.son]
tmp$p18_2000[sel.son] <- 0
tmp$p18_2005[sel.parent] <- tmp$p18_2005[sel.parent] + tmp$p18_2005[sel.son]
tmp$p18_2005[sel.son] <- 0
tmp$p18_2010[sel.parent] <- tmp$p18_2010[sel.parent] + tmp$p18_2010[sel.son]
tmp$p18_2010[sel.son] <- 0
tmp$p18_2020[sel.parent] <- tmp$p18_2020[sel.parent] + tmp$p18_2020[sel.son]
tmp$p18_2020[sel.son] <- 0
tmp[c(sel.parent, sel.son),]
tmp -> censom94      # return
##
## tlaxcala bunch (created 1996)
## Elected 1st mu gov 1996
## 1994 map needs sums/zeroes
mywrap <- function(){
    tmp$p18_1995[sel.parent] <- tmp$p18_1995[sel.parent] + tmp$p18_1995[sel.son]
    tmp$p18_1995[sel.son] <- 0
    tmp$p18_2000[sel.parent] <- tmp$p18_2000[sel.parent] + tmp$p18_2000[sel.son]
    tmp$p18_2000[sel.son] <- 0
    tmp$p18_2005[sel.parent] <- tmp$p18_2005[sel.parent] + tmp$p18_2005[sel.son]
    tmp$p18_2005[sel.son] <- 0
    tmp$p18_2010[sel.parent] <- tmp$p18_2010[sel.parent] + tmp$p18_2010[sel.son]
    tmp$p18_2010[sel.son] <- 0
    tmp$p18_2020[sel.parent] <- tmp$p18_2020[sel.parent] + tmp$p18_2020[sel.son]
    tmp$p18_2020[sel.son] <- 0
    return(tmp)
}
tmp <- censom94      # duplicate for manipulation
sel.parent <- which(tmp$inegi==29020)
sel.son <-    which(tmp$inegi==29045)
tmp <- mywrap()
sel.parent <- which(tmp$inegi==29030)
sel.son <-    which(tmp$inegi==29046)
tmp <- mywrap()
sel.parent <- which(tmp$inegi==29030)
sel.son <-    which(tmp$inegi==29047)
tmp <- mywrap()
sel.parent <- which(tmp$inegi==29010)
sel.son <-    which(tmp$inegi==29048)
tmp <- mywrap()
sel.parent <- which(tmp$inegi==29032)
sel.son <-    which(tmp$inegi==29049)
tmp <- mywrap()
sel.parent <- which(tmp$inegi==29010)
sel.son <-    which(tmp$inegi==29050)
tmp <- mywrap()
sel.parent <- which(tmp$inegi==29032)
sel.son <-    which(tmp$inegi==29051)
tmp <- mywrap()
sel.parent <- which(tmp$inegi==29038)
sel.son <-    which(tmp$inegi==29052)
tmp <- mywrap()
sel.parent <- which(tmp$inegi==29029)
sel.son <-    which(tmp$inegi==29053)
tmp <- mywrap()
sel.parent <- which(tmp$inegi==29044)
sel.son <-    which(tmp$inegi==29054)
tmp <- mywrap()
sel.parent <- which(tmp$inegi==29040)
sel.son <-    which(tmp$inegi==29055)
tmp <- mywrap()
sel.parent <- which(tmp$inegi==29015)
sel.son <-    which(tmp$inegi==29056)
tmp <- mywrap()
sel.parent <- which(tmp$inegi==29023)
sel.son <-    which(tmp$inegi==29057)
tmp <- mywrap()
sel.parent <- which(tmp$inegi==29044)
sel.son <-    which(tmp$inegi==29058)
tmp <- mywrap()
sel.parent <- which(tmp$inegi==29022)
sel.son <-    which(tmp$inegi==29059)
tmp <- mywrap()
sel.parent <- which(tmp$inegi==29029)
sel.son <-    which(tmp$inegi==29060)
tmp <- mywrap()
tmp -> censom94      # return
##
## Carrillo, tata, Uxpa (created 1997)
## Elected 1st mu gov 1997
## Pre-97 map needs sums
tmp <- censom94      # duplicate for manipulation
sel.parent <- which(tmp$inegi==30045)
sel.son <-    which(tmp$inegi==30208)
tmp <- mywrap()
sel.parent <- which(tmp$inegi==30104)
sel.son <-    which(tmp$inegi==30209)
tmp <- mywrap()
sel.parent <- which(tmp$inegi==30070)
sel.son <-    which(tmp$inegi==30210)
tmp <- mywrap()
tmp -> censom94      # return
##
## San Rafael, santiago (created 2004)
## Elected 1st mu gov 2004
## All ok
tmp <- censom03      # duplicate for manipulation
sel.parent <- which(tmp$inegi==30102)
sel.son <-    which(tmp$inegi==30211)
tmp[c(sel.parent, sel.son),]
sel.parent <- which(tmp$inegi==30130)
sel.son <-    which(tmp$inegi==30212)
tmp[c(sel.parent, sel.son),]
##
## Trancoso (created 2001)
## Elected 1st mu gov 2004
## pre-2003 map needs sum/zeroes
tmp <- censom00      # duplicate for manipulation
sel.parent <- which(tmp$inegi==32017)
sel.son <-    which(tmp$inegi==32057)
tmp[c(sel.parent, sel.son),]
tmp$p18_2000[sel.parent] <- tmp$p18_2000[sel.parent] + tmp$p18_2000[sel.son]
tmp$p18_2000[sel.son] <- 0
tmp[c(sel.parent, sel.son),]
tmp -> censom00      # return
## Make prior maps same as m00
censom94    [c(sel.parent, sel.son), grep("p18_", colnames(censom94))] <-
    censom97[c(sel.parent, sel.son), grep("p18_", colnames(censom97))] <-
    censom00[c(sel.parent, sel.son), grep("p18_", colnames(censom00))]
##
## Sta María (created 2007)
## Elected 1st mu gov 2007, into fed els 2006
## all ok
tmp <- censom03      # duplicate for manipulation
sel.parent <- which(tmp$inegi==32047)
sel.son <-    which(tmp$inegi==32058)
tmp[c(sel.parent, sel.son),]


############################################
## Check mu census for zeroes before proj ##
############################################
sel.r <- which(censom21$p18_2005==0 | censom21$p18_2010==0 | censom21$p18_2020==0)
censom21[sel.r,]
sel.r <- which(censom21$p18_2000==0)
censom21[sel.r,]
sel.r <- which(censom21$inegi==7058)
censom21$p18_2000[sel.r] <- prj9505(x=censom21[sel.r,], yr=2000)
sel.r <- which(censom21$p18_2000==0)
censom21$p18_2000[sel.r] <- prj0510(x=censom21[sel.r,], yr=2000)
censom21$p18_1995[sel.r] <- prj0510(x=censom21[sel.r,], yr=1995) # has negs that will be innocuous
censom21$p18_1990[sel.r] <- prj0510(x=censom21[sel.r,], yr=1990) # has negs that will be innocuous
##
sel.r <- which(censom21$p18_1995==0 & censom21$p18_1990>0)
censom21[sel.r,]
censom21$p18_1995[sel.r] <- prj9000(x=censom21[sel.r,], yr=1995) # has negs that will be innocuous
##
sel.r <- which(censom21$p18_1995==0)
censom21[sel.r,]
censom21$p18_1995[sel.r] <- prj0005(x=censom21[sel.r,], yr=1995) # has negs that will be innocuous
censom21$p18_1990[sel.r] <- prj0005(x=censom21[sel.r,], yr=1990) # has negs that will be innocuous
##
#########
## m18 ##
#########
dok <- rep(0, nrow(censom18))
sel.r <- which(censom18$p18_2005==0 | censom18$p18_2010==0 | censom18$p18_2020==0)
censom18[sel.r,]
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom18$p18_2000==0 & censom18$p18_1995>0 & dok==0)
censom18[sel.r,]
censom18$p18_2000[sel.r] <- (censom18$p18_1995[sel.r] + censom18$p18_2005[sel.r]) / 2
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom18$p18_2000==0 & dok==0)
censom18[sel.r,]
censom18$p18_2000[sel.r] <- prj0510(censom18[sel.r,], 2000)
censom18$p18_1995[sel.r] <- prj0510(censom18[sel.r,], 1995)
censom18$p18_1990[sel.r] <- prj0510(censom18[sel.r,], 1990)
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom18$p18_1995==0 & censom18$p18_1990>0 & dok==0)
censom18[sel.r,]
censom18$p18_1995[sel.r] <- (censom18$p18_1990[sel.r] + censom18$p18_2000[sel.r]) / 2
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom18$p18_1995==0 & dok==0)
censom18$p18_1995[sel.r] <- prj0005(censom18[sel.r,], 1995)
censom18$p18_1990[sel.r] <- prj0005(censom18[sel.r,], 1990)
censom18[sel.r,]
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom18$p18_1990==0 & dok==0)
censom18[sel.r,]
censom18$p18_1990[sel.r] <- prj9500(censom18[sel.r,], 1990)
censom18[sel.r,]
dok[sel.r] <- 1 ## indicate cases ok
##
rm(dok)
##
#########
## m15 ##
#########
dok <- rep(0, nrow(censom15))
sel.r <- which(censom15$p18_2005==0 | censom15$p18_2010==0 | censom15$p18_2020==0)
censom15[sel.r,]
##
## ## use tulum m18 for mun els 2008:2016
## sel.r <- which(censom15$inegi==23009)
## censom06[sel.r,] <- censom09[sel.r,] <- censom12[sel.r,] <- censom15[sel.r,] <- censom18[sel.r,]
## dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom15$p18_2005==0 | censom15$p18_2010==0 | censom15$p18_2020==0 & dok==0)
censom15[sel.r,]
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom15$p18_2000==0 & censom15$p18_1995>0 & dok==0)
censom15[sel.r,]
censom15$p18_2000[sel.r] <- (censom15$p18_1995[sel.r] + censom15$p18_2005[sel.r]) / 2
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom15$p18_2000==0 & dok==0)
censom15[sel.r,]
censom15$p18_2000[sel.r] <- prj0510(censom15[sel.r,], 2000)
censom15$p18_1995[sel.r] <- prj0510(censom15[sel.r,], 1995)
censom15$p18_1990[sel.r] <- prj0510(censom15[sel.r,], 1990)
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom15$p18_1995==0 & censom15$p18_1990>0 & dok==0)
censom15[sel.r,]
censom15$p18_1995[sel.r] <- (censom15$p18_1990[sel.r] + censom15$p18_2000[sel.r]) / 2
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom15$p18_1995==0 & dok==0)
censom15$p18_1995[sel.r] <- prj0005(censom15[sel.r,], 1995)
censom15$p18_1990[sel.r] <- prj0005(censom15[sel.r,], 1990)
censom15[sel.r,]
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom15$p18_1990==0 & dok==0)
censom15[sel.r,]
censom15$p18_1990[sel.r] <- prj9500(censom15[sel.r,], 1990)
censom15[sel.r,]
dok[sel.r] <- 1 ## indicate cases ok
##
#########
## m12 ##
#########
dok <- rep(0, nrow(censom12))
sel.r <- which(censom12$p18_2005==0 | censom12$p18_2010==0 | censom12$p18_2020==0)
censom12[sel.r,]
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom12$p18_2000==0 & censom12$p18_1995>0 & dok==0)
censom12[sel.r,]
censom12$p18_2000[sel.r] <- (censom12$p18_1995[sel.r] + censom12$p18_2005[sel.r]) / 2
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom12$p18_2000==0 & dok==0)
censom12[sel.r,]
censom12$p18_2000[sel.r] <- prj0510(censom12[sel.r,], 2000)
censom12$p18_1995[sel.r] <- prj0510(censom12[sel.r,], 1995)
censom12$p18_1990[sel.r] <- prj0510(censom12[sel.r,], 1990)
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom12$p18_1995==0 & censom12$p18_1990>0 & dok==0)
censom12[sel.r,]
censom12$p18_1995[sel.r] <- (censom12$p18_1990[sel.r] + censom12$p18_2000[sel.r]) / 2
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom12$p18_1995==0 & dok==0)
censom12$p18_1995[sel.r] <- prj0005(censom12[sel.r,], 1995)
censom12$p18_1990[sel.r] <- prj0005(censom12[sel.r,], 1990)
censom12[sel.r,]
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom12$p18_1990==0 & dok==0)
censom12[sel.r,]
censom12$p18_1990[sel.r] <- prj9500(censom12[sel.r,], 1990)
censom12[sel.r,]
dok[sel.r] <- 1 ## indicate cases ok
##
#########
## m09 ##
#########
dok <- rep(0, nrow(censom09))
sel.r <- which(censom09$p18_2005==0 & censom09$p18_2010>0 & censom09$p18_2020>0)
censom09[sel.r,]
censom09$p18_2005[sel.r] <- prj1020(censom09[sel.r,], 2005)
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom09$p18_2005==0 | censom09$p18_2010==0 | censom09$p18_2020==0)
censom09[sel.r,]
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom09$p18_2000==0 & censom09$p18_1995>0 & dok==0)
censom09[sel.r,]
censom09$p18_2000[sel.r] <- (censom09$p18_1995[sel.r] + censom09$p18_2005[sel.r]) / 2
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom09$p18_2000==0 & dok==0)
censom09[sel.r,] ## Ojo: Marquelia is ok but will need fine-tuning: 2006 and 2009 had minuscule pop, then in 2012 won territ
censom09$p18_2000[sel.r] <- prj0510(censom09[sel.r,], 2000)
censom09$p18_1995[sel.r] <- prj0510(censom09[sel.r,], 1995)
censom09$p18_1990[sel.r] <- prj0510(censom09[sel.r,], 1990)
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom09$p18_1995==0 & censom09$p18_1990>0 & dok==0)
censom09[sel.r,]
censom09$p18_1995[sel.r] <- (censom09$p18_1990[sel.r] + censom09$p18_2000[sel.r]) / 2
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom09$p18_1995==0 & dok==0)
censom09$p18_1995[sel.r] <- prj0005(censom09[sel.r,], 1995)
censom09$p18_1990[sel.r] <- prj0005(censom09[sel.r,], 1990)
censom09[sel.r,]
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom09$p18_1990==0 & dok==0)
censom09[sel.r,]
censom09$p18_1990[sel.r] <- prj9500(censom09[sel.r,], 1990)
censom09[sel.r,]
dok[sel.r] <- 1 ## indicate cases ok
##
#########
## m06 ##
#########
dok <- rep(0, nrow(censom06))
sel.r <- which(censom06$p18_2005==0 | censom06$p18_2010==0 | censom06$p18_2020==0)
censom06[sel.r,]
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom06$p18_2000==0 & censom06$p18_1995>0 & dok==0)
censom06[sel.r,]
censom06$p18_2000[sel.r] <- (censom06$p18_1995[sel.r] + censom06$p18_2005[sel.r]) / 2
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom06$p18_2000==0 & dok==0)
censom06[sel.r,] ## Ojo: Marquelia is ok but will need fine-tuning: 2006 and 2009 had minuscule pop, then in 2012 won territ
censom06$p18_2000[sel.r] <- prj0510(censom06[sel.r,], 2000)
censom06$p18_1995[sel.r] <- prj0510(censom06[sel.r,], 1995)
censom06$p18_1990[sel.r] <- prj0510(censom06[sel.r,], 1990)
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom06$p18_1995==0 & censom06$p18_1990>0 & dok==0)
censom06[sel.r,]
censom06$p18_1995[sel.r] <- (censom06$p18_1990[sel.r] + censom06$p18_2000[sel.r]) / 2
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom06$p18_1995==0 & dok==0)
censom06$p18_1995[sel.r] <- prj0005(censom06[sel.r,], 1995)
censom06$p18_1990[sel.r] <- prj0005(censom06[sel.r,], 1990)
censom06[sel.r,]
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom06$p18_1990==0 & dok==0)
censom06[sel.r,]
censom06$p18_1990[sel.r] <- prj9500(censom06[sel.r,], 1990)
censom06[sel.r,]
dok[sel.r] <- 1 ## indicate cases ok
##
#########
## m03 ##
#########
dok <- rep(0, nrow(censom03))
sel.r <- which(censom03$p18_2005==0 | censom03$p18_2010==0 | censom03$p18_2020==0)
censom03[sel.r,]
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom03$p18_2000==0 & censom03$p18_1995>0 & dok==0)
censom03[sel.r,]
censom03$p18_2000[sel.r] <- (censom03$p18_1995[sel.r] + censom03$p18_2005[sel.r]) / 2
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom03$p18_2000==0 & dok==0)
censom03[sel.r,]
censom03$p18_2000[sel.r] <- prj0510(censom03[sel.r,], 2000)
censom03$p18_1995[sel.r] <- prj0510(censom03[sel.r,], 1995)
censom03$p18_1990[sel.r] <- prj0510(censom03[sel.r,], 1990)
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom03$p18_1995==0 & censom03$p18_1990>0 & dok==0)
censom03[sel.r,]
censom03$p18_1995[sel.r] <- (censom03$p18_1990[sel.r] + censom03$p18_2000[sel.r]) / 2
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom03$p18_1995==0 & dok==0)
censom03$p18_1995[sel.r] <- prj0005(censom03[sel.r,], 1995)
censom03$p18_1990[sel.r] <- prj0005(censom03[sel.r,], 1990)
censom03[sel.r,]
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom03$p18_1990==0 & dok==0)
censom03[sel.r,]
censom03$p18_1990[sel.r] <- prj9500(censom03[sel.r,], 1990)
censom03[sel.r,]
dok[sel.r] <- 1 ## indicate cases ok
##
#########
## m00 ##
#########
dok <- rep(0, nrow(censom00))
sel.r <- which(censom00$p18_2005==0 | censom00$p18_2010==0 | censom00$p18_2020==0)
censom00[sel.r,]
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom00$p18_2000==0 & censom00$p18_1995>0 & dok==0)
censom00[sel.r,]
censom00$p18_2000[sel.r] <- (censom00$p18_1995[sel.r] + censom00$p18_2005[sel.r]) / 2
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom00$p18_2000==0 & dok==0)
censom00[sel.r,]
censom00$p18_2000[sel.r] <- prj0510(censom00[sel.r,], 2000)
censom00$p18_1995[sel.r] <- prj0510(censom00[sel.r,], 1995)
censom00$p18_1990[sel.r] <- prj0510(censom00[sel.r,], 1990)
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom00$p18_1995==0 & censom00$p18_1990>0 & dok==0)
censom00[sel.r,]
censom00$p18_1995[sel.r] <- (censom00$p18_1990[sel.r] + censom00$p18_2000[sel.r]) / 2
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom00$p18_1995==0 & dok==0)
censom00$p18_1995[sel.r] <- prj0005(censom00[sel.r,], 1995)
censom00$p18_1990[sel.r] <- prj0005(censom00[sel.r,], 1990)
censom00[sel.r,]
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom00$p18_1990==0 & dok==0)
censom00[sel.r,]
censom00$p18_1990[sel.r] <- prj9500(censom00[sel.r,], 1990)
censom00[sel.r,]
dok[sel.r] <- 1 ## indicate cases ok
##
#########
## m97 ##
#########
dok <- rep(0, nrow(censom97))
sel.r <- which(censom97$p18_2005==0 | censom97$p18_2010==0 | censom97$p18_2020==0)
censom97[sel.r,]
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom97$p18_2000==0 & censom97$p18_1995>0 & dok==0)
censom97[sel.r,]
censom97$p18_2000[sel.r] <- (censom97$p18_1995[sel.r] + censom97$p18_2005[sel.r]) / 2
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom97$p18_2000==0 & dok==0)
censom97[sel.r,]
censom97$p18_2000[sel.r] <- prj0510(censom97[sel.r,], 2000)
censom97$p18_1995[sel.r] <- prj0510(censom97[sel.r,], 1995)
censom97$p18_1990[sel.r] <- prj0510(censom97[sel.r,], 1990)
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom97$p18_1995==0 & censom97$p18_1990>0 & dok==0)
censom97[sel.r,]
censom97$p18_1995[sel.r] <- (censom97$p18_1990[sel.r] + censom97$p18_2000[sel.r]) / 2
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom97$p18_1995==0 & dok==0)
censom97$p18_1995[sel.r] <- prj0005(censom97[sel.r,], 1995)
censom97$p18_1990[sel.r] <- prj0005(censom97[sel.r,], 1990)
censom97[sel.r,]
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom97$p18_1990==0 & dok==0)
censom97[sel.r,]
censom97$p18_1990[sel.r] <- prj9500(censom97[sel.r,], 1990)
censom97[sel.r,]
dok[sel.r] <- 1 ## indicate cases ok
##
#########
## m94 ##
#########
dok <- rep(0, nrow(censom94))
sel.r <- which(censom94$p18_2005==0 | censom94$p18_2010==0 | censom94$p18_2020==0)
censom94[sel.r,]
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom94$p18_2000==0 & censom94$p18_1995>0 & dok==0)
censom94[sel.r,]
censom94$p18_2000[sel.r] <- (censom94$p18_1995[sel.r] + censom94$p18_2005[sel.r]) / 2
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom94$p18_2000==0 & dok==0)
censom94[sel.r,]
censom94$p18_2000[sel.r] <- prj0510(censom94[sel.r,], 2000)
censom94$p18_1995[sel.r] <- prj0510(censom94[sel.r,], 1995)
censom94$p18_1990[sel.r] <- prj0510(censom94[sel.r,], 1990)
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom94$p18_1995==0 & censom94$p18_1990>0 & dok==0)
censom94[sel.r,]
censom94$p18_1995[sel.r] <- (censom94$p18_1990[sel.r] + censom94$p18_2000[sel.r]) / 2
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom94$p18_1995==0 & dok==0)
censom94$p18_1995[sel.r] <- prj0005(censom94[sel.r,], 1995)
censom94$p18_1990[sel.r] <- prj0005(censom94[sel.r,], 1990)
censom94[sel.r,]
dok[sel.r] <- 1 ## indicate cases ok
##
sel.r <- which(censom94$p18_1990==0 & dok==0)
censom94[sel.r,]
censom94$p18_1990[sel.r] <- prj9500(censom94[sel.r,], 1990)
censom94[sel.r,]
dok[sel.r] <- 1 ## indicate cases ok
##
rm(dok, sel.son, sel.parent, sel.ignore, sel.r, tmp) ## clean


###############################
## CONSOLIDAR MAPAS CENSALES ##
###############################
tmp <- censom94 # duplicate for manipulation
mywrap <- function(){
    tmp$p18_1991 <- prj9095(x=tmp, yr=1991)
    tmp$p18_1992 <- prj9095(x=tmp, yr=1992)
    tmp$p18_1993 <- prj9095(x=tmp, yr=1993)
    tmp$p18_1994 <- prj9095(x=tmp, yr=1994)
    ##
    tmp$p18_1996 <- prj9500(x=tmp, yr=1996)
    tmp$p18_1997 <- prj9500(x=tmp, yr=1997)
    tmp$p18_1998 <- prj9500(x=tmp, yr=1998)
    tmp$p18_1999 <- prj9500(x=tmp, yr=1999)
    ##
    tmp$p18_2001 <- prj0005(x=tmp, yr=2001)
    tmp$p18_2002 <- prj0005(x=tmp, yr=2002)
    tmp$p18_2003 <- prj0005(x=tmp, yr=2003)
    tmp$p18_2004 <- prj0005(x=tmp, yr=2004)
    ##
    tmp$p18_2006 <- prj0510(x=tmp, yr=2006)
    tmp$p18_2007 <- prj0510(x=tmp, yr=2007)
    tmp$p18_2008 <- prj0510(x=tmp, yr=2008)
    tmp$p18_2009 <- prj0510(x=tmp, yr=2009)
    ##
    tmp$p18_2011 <- prj1020(x=tmp, yr=2011)
    tmp$p18_2012 <- prj1020(x=tmp, yr=2012)
    tmp$p18_2013 <- prj1020(x=tmp, yr=2013)
    tmp$p18_2014 <- prj1020(x=tmp, yr=2014)
    tmp$p18_2015 <- prj1020(x=tmp, yr=2015)
    tmp$p18_2016 <- prj1020(x=tmp, yr=2016)
    tmp$p18_2017 <- prj1020(x=tmp, yr=2017)
    tmp$p18_2018 <- prj1020(x=tmp, yr=2018)
    tmp$p18_2019 <- prj1020(x=tmp, yr=2019)
    tmp$p18_2021 <- prj1020(x=tmp, yr=2021)
    ##tmp$p18_2022 <- prj1020(x=tmp, yr=2022)
    ##tmp$p18_2023 <- prj1020(x=tmp, yr=2023)
    tmp <- tmp[, order(colnames(tmp))]
    return(tmp)
}
tmp <- mywrap()
tmp[1,]
tmp -> censom94 # return
##
tmp <- censom97 # duplicate for manipulation
tmp <- mywrap()
tmp -> censom97 # return
##
tmp <- censom00 # duplicate for manipulation
tmp <- mywrap()
tmp -> censom00 # return
##
tmp <- censom03 # duplicate for manipulation
tmp <- mywrap()
tmp -> censom03 # return
##
tmp <- censom06 # duplicate for manipulation
tmp <- mywrap()
tmp -> censom06 # return
##
tmp <- censom09 # duplicate for manipulation
tmp <- mywrap()
tmp -> censom09 # return
##
tmp <- censom12 # duplicate for manipulation
tmp <- mywrap()
tmp -> censom12 # return
##
tmp <- censom15 # duplicate for manipulation
tmp <- mywrap()
tmp -> censom15 # return
##
tmp <- censom18 # duplicate for manipulation
tmp <- mywrap()
tmp -> censom18 # return
##
tmp <- censom21 # duplicate for manipulation
tmp <- mywrap()
tmp -> censom21 # return
rm(tmp)
#

#######################################################################################################################################
## Esta rutina proyecta poblaciones municipales a partir de los mapas seccionales de municipios 2005 2010 2020 para años electorales ##
## federales. Esto es, prescinde de los mu-level 1990 1995 2000. Los agrupa en objeto tmpfs, y es el objeto apropiado para proyectar ##
## poblaciones seccionales usando compositional variables.                                                                           ##
#######################################################################################################################################
tmpfs <- censom94[, c("edon", "ife", "inegi", "mun")] # initialize object
## duplicate censom.. to keep only 2005 2010 2020
tmpm94 <- censom94[, c("edon", "ife", "inegi", "mun", "p18_2005", "p18_2010", "p18_2020")]
tmpm97 <- censom97[, c("edon", "ife", "inegi", "mun", "p18_2005", "p18_2010", "p18_2020")]
tmpm00 <- censom00[, c("edon", "ife", "inegi", "mun", "p18_2005", "p18_2010", "p18_2020")]
tmpm03 <- censom03[, c("edon", "ife", "inegi", "mun", "p18_2005", "p18_2010", "p18_2020")]
tmpm06 <- censom06[, c("edon", "ife", "inegi", "mun", "p18_2005", "p18_2010", "p18_2020")]
tmpm09 <- censom09[, c("edon", "ife", "inegi", "mun", "p18_2005", "p18_2010", "p18_2020")]
tmpm12 <- censom12[, c("edon", "ife", "inegi", "mun", "p18_2005", "p18_2010", "p18_2020")]
tmpm15 <- censom15[, c("edon", "ife", "inegi", "mun", "p18_2005", "p18_2010", "p18_2020")]
tmpm18 <- censom18[, c("edon", "ife", "inegi", "mun", "p18_2005", "p18_2010", "p18_2020")]
tmpm21 <- censom21[, c("edon", "ife", "inegi", "mun", "p18_2005", "p18_2010", "p18_2020")]
##
## get census projection functions
source("../../code/v-hat-etc/interpolate-census-functions.r")
tmp.regs <- interlog(what="p18", yr=1994, unit="m", frm="log(dv)~iv", census.data=tmpm94, digits=1)
tmpfs$p18_1994 <- tmp.regs$interp
tmp.regs <- interlog(what="p18", yr=1997, unit="m", frm="log(dv)~iv", census.data=tmpm97, digits=1)
tmpfs$p18_1997 <- tmp.regs$interp
tmp.regs <- interlog(what="p18", yr=2000, unit="m", frm="log(dv)~iv", census.data=tmpm00, digits=1)
tmpfs$p18_2000 <- tmp.regs$interp
tmp.regs <- interlog(what="p18", yr=2003, unit="m", frm="log(dv)~iv", census.data=tmpm03, digits=1)
tmpfs$p18_2003 <- tmp.regs$interp
tmp.regs <- interlog(what="p18", yr=2006, unit="m", frm="log(dv)~iv", census.data=tmpm06, digits=1)
tmpfs$p18_2006 <- tmp.regs$interp
## tmp.lins <- interpol(what="p18", yr=2006, unit="m", census.data=tmpm06, digits=1)
## tmpfs$p18_2006 <- tmp.lins
tmp.regs <- interlog(what="p18", yr=2009, unit="m", frm="log(dv)~iv", census.data=tmpm09, digits=1)
tmpfs$p18_2009 <- tmp.regs$interp
## tmp.lins <- interpol(what="p18", yr=2009, unit="m", census.data=tmpm09, digits=1)
## tmpfs$p18_2009 <- tmp.lins
tmp.regs <- interlog(what="p18", yr=2012, unit="m", frm="log(dv)~iv", census.data=tmpm12, digits=1)
tmpfs$p18_2012 <- tmp.regs$interp
## tmp.lins <- interpol(what="p18", yr=2012, unit="m", census.data=tmpm12, digits=1)
## tmpfs$p18_2012 <- tmp.lins
tmp.regs <- interlog(what="p18", yr=2015, unit="m", frm="log(dv)~iv", census.data=tmpm15, digits=1)
tmpfs$p18_2015 <- tmp.regs$interp
## tmp.lins <- interpol(what="p18", yr=2015, unit="m", census.data=tmpm15, digits=1)
## tmpfs$p18_2015 <- tmp.lins
tmp.regs <- interlog(what="p18", yr=2018, unit="m", frm="log(dv)~iv", census.data=tmpm18, digits=1)
tmpfs$p18_2018 <- tmp.regs$interp
## tmp.lins <- interpol(what="p18", yr=2018, unit="m", census.data=tmpm18, digits=1)
## tmpfs$p18_2018 <- tmp.lins
tmp.regs <- interlog(what="p18", yr=2021, unit="m", frm="log(dv)~iv", census.data=tmpm21, digits=1)
tmpfs$p18_2021 <- tmp.regs$interp
##
#############
## Export  ##
#############
## This has projections with fed elec yrs since 1994, municipios adjusted to federal election calendars from seccion maps
write.csv(x=tmpfs, file="/home/eric/Dropbox/data/elecs/MXelsCalendGovt/censos/data/pob18/p18mu-for-compos-var-seccion-censos.csv", row.names=FALSE)
##
rm(tmpm94, tmpm97, tmpm00, tmpm03, tmpm06, tmpm09, tmpm12, tmpm15, tmpm18, tmpm21, tmp.regs)#, tmp.lins)



## Duplicate, one for actual populations for federal calendar, one for municipal calendar
tmpf <- censom21
tmpm <- censom21
##
## Manipulate remunicipalizacion cases
## 2005	1998	PLAYAS DE ROSARITO 5
sel.parent <- which(tmpm$inegi==2004)
sel.son <-    which(tmpm$inegi==2005)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
##
## Calakmul (created 1997)
## Elected 1st mu gov 1997 but incorporated to fed els til 2018
sel.parent <- which(tmpm$inegi==4006)
sel.son <-    which(tmpm$inegi==4010)
tmpm[c(sel.parent, sel.son),]
censom15[c(sel.parent, sel.son), sel.c]
censom18[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2018|2019|2020|2021)", colnames(tmpm))
tmpf[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
tmpf[c(sel.parent, sel.son),]
##
## Candelaria (created 2000) is easy
## Elected 1st mu gov 2000
sel.parent <- which(tmpm$inegi==4003 | tmpm$inegi==4009)
sel.son <-    which(tmpm$inegi==4011)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom00[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom00[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
##
## Seybaplaya (created 2019)
## Elected 1st mu gov 2021
## All looks ok
sel.parent <- which(tmpm$inegi==4002 | tmpm$inegi==4004)
sel.son <-    which(tmpm$inegi==4012)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom21[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom21[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
##
## Dzitbalche (created 2019)
## Elected 1st mu gov 2021
## All looks ok
sel.parent <- which(tmpm$inegi==4001)
sel.son <-    which(tmpm$inegi==4013)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom21[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom21[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
##
## Aldama, Benemérito, Maravilla, Marqués, Montecristo, Sn Andrés, Santiago (created 2001)
## Elected 1st mu gov 2001 but not incorporated to fed els til 2018
## Use 2018 map for mun els since 2001
## Pre-2018 maps have 2000 that needs to go to parent
sel.parent <- which(tmpm$inegi==7026)
sel.son <-    which(tmpm$inegi==7113)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017)", colnames(tmpm))
tmpf[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2018|2019|2020|2021)", colnames(tmpm))
tmpf[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
tmpf[c(sel.parent, sel.son),]
##
sel.parent <- which(tmpm$inegi==7059)
sel.son <-    which(tmpm$inegi==7114)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017)", colnames(tmpm))
tmpf[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2018|2019|2020|2021)", colnames(tmpm))
tmpf[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
tmpf[c(sel.parent, sel.son),]
##
sel.parent <- which(tmpm$inegi==7052)
sel.son <-    which(tmpm$inegi==7115)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017)", colnames(tmpm))
tmpf[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2018|2019|2020|2021)", colnames(tmpm))
tmpf[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
tmpf[c(sel.parent, sel.son),]
##
sel.parent <- which(tmpm$inegi==7059)
sel.son <-    which(tmpm$inegi==7116)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017)", colnames(tmpm))
tmpf[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2018|2019|2020|2021)", colnames(tmpm))
tmpf[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
tmpf[c(sel.parent, sel.son),]
##
sel.parent <- which(tmpm$inegi==7008)
sel.son <-    which(tmpm$inegi==7117)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017)", colnames(tmpm))
tmpf[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2018|2019|2020|2021)", colnames(tmpm))
tmpf[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
tmpf[c(sel.parent, sel.son),]
##
sel.parent <- which(tmpm$inegi==7081)
sel.son <-    which(tmpm$inegi==7118)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017)", colnames(tmpm))
tmpf[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2018|2019|2020|2021)", colnames(tmpm))
tmpf[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
tmpf[c(sel.parent, sel.son),]
##
sel.parent <- which(tmpm$inegi==7049)
sel.son <-    which(tmpm$inegi==7119)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017)", colnames(tmpm))
tmpf[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2018|2019|2020|2021)", colnames(tmpm))
tmpf[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
tmpf[c(sel.parent, sel.son),]
##
## Capitán and Rincón (created 2018)
## Elected 1st mu gov 2018 but not incorporated to fed els til 2021
## Use 2021 map for mun els in 2018
sel.parent <- which(tmpm$inegi==7081)
sel.son <-    which(tmpm$inegi==7120)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020)", colnames(tmpm))
tmpf[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2021)", colnames(tmpm))
tmpf[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
##
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son),]
tmpm[c(sel.parent, sel.son),]
##
sel.parent <- which(tmpm$inegi==7072 | tmpm$inegi==7047 | tmpm$inegi==7091)
sel.son <-    which(tmpm$inegi==7121)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020)", colnames(tmpm))
tmpf[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2021)", colnames(tmpm))
tmpf[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
##
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son),]
tmpm[c(sel.parent, sel.son),]
##
## Parral, Zapata, Mezcalapa, (created 2012)
## Elected 1st mu gov 2012 but not incorporated to fed els til 2021
## Use 2021 map for mun els since 2012
sel.parent <- which(tmpm$inegi==7107 | tmpm$inegi==7108)
sel.son <-    which(tmpm$inegi==7122)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020)", colnames(tmpm))
tmpf[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2021)", colnames(tmpm))
tmpf[c(sel.parent, sel.son), sel.c] <- censom21[c(sel.parent, sel.son), sel.c]
##
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom21[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son),]
tmpm[c(sel.parent, sel.son),]
##
sel.parent <- which(tmpm$inegi==7002 | tmpm$inegi==7027 | tmpm$inegi==7106)
sel.son <-    which(tmpm$inegi==7123)
censom18[c(sel.parent, sel.son),]
censom21[c(sel.parent, sel.son),]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020)", colnames(tmpm))
tmpf[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2021)", colnames(tmpm))
tmpf[c(sel.parent, sel.son), sel.c] <- censom21[c(sel.parent, sel.son), sel.c]
##
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom21[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son),]
tmpm[c(sel.parent, sel.son),]
##
sel.parent <- which(tmpm$inegi==7061 | tmpm$inegi==7062 | tmpm$inegi==7092)
sel.son <-    which(tmpm$inegi==7124)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020)", colnames(tmpm))
tmpf[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2021)", colnames(tmpm))
tmpf[c(sel.parent, sel.son), sel.c] <- censom21[c(sel.parent, sel.son), sel.c]
##
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom21[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son),]
tmpm[c(sel.parent, sel.son),]
##
## Honduras (created 2020)
## Elected 1st mu gov 2021
sel.parent <- which(tmpm$inegi==7081)
sel.son <-    which(tmpm$inegi==7125)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom21[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom21[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
##
## 1 sec from chihuahua to aldama in 2015
sel.parent <- which(tmpm$inegi==8019)
sel.son <-    which(tmpm$inegi==8002)
censom12[c(sel.parent, sel.son),]
censom15[c(sel.parent, sel.son),]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom12[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom12[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
##
## 4 secs from xochimilco to tlahuac in 2013
sel.parent <- which(tmpm$inegi==9013)
sel.son <-    which(tmpm$inegi==9011)
censom12[c(sel.parent, sel.son),]
censom15[c(sel.parent, sel.son),]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom12[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom12[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
##
## ## 7 secs from ao to cuajimalpa in 2020 CANT BE FIXED WITH 2020 CENSUS ONLY
## sel.parent <- which(tmpm$inegi==9010)
## sel.son <-    which(tmpm$inegi==9011)
## censom18[c(sel.parent, sel.son),]
## censom21[c(sel.parent, sel.son),]
## sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020)", colnames(tmpm))
## tmpm[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
## tmpf[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
## sel.c <- grep("p18_(2021)", colnames(tmpm))
## tmpm[c(sel.parent, sel.son), sel.c] <- censom21[c(sel.parent, sel.son), sel.c]
## tmpf[c(sel.parent, sel.son), sel.c] <- censom21[c(sel.parent, sel.son), sel.c]
## tmpm[c(sel.parent, sel.son),]
##
## PARTICULAR CASE
## Acatepec (created 1996)
## Elected 1st mu gov 1996
sel.parent <- which(tmpm$inegi==12072)
sel.son <-    which(tmpm$inegi==12076)
censom94[c(sel.parent, sel.son),]
censom97[c(sel.parent, sel.son),]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
## 3 secs from zapot to acatepec in 2005
sel.parent <- which(tmpm$inegi==12072)
sel.son <-    which(tmpm$inegi==12076)
censom03[c(sel.parent, sel.son),]
censom06[c(sel.parent, sel.son),]
sel.c <- grep("p18_(1997|1998|1999|2000|2001|2002|2003|2004|2005)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom03[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom03[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom06[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom06[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
##
## 1 sec from acatepec to ayutla in 2011
sel.parent <- which(tmpm$inegi==12076)
sel.son <-    which(tmpm$inegi==12012)
censom09[c(sel.parent, sel.son),]
censom12[c(sel.parent, sel.son),]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011)", colnames(tmpm))
tmpm[  sel.parent          , sel.c] <- censom09[  sel.parent          , sel.c] # chg parent only due to fixes above
tmpf[  sel.parent          , sel.c] <- censom09[  sel.parent          , sel.c]
sel.c <- grep("p18_(2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom12[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom12[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
##
## 1 sec from tecpan to petatlan in 2011
sel.parent <- which(tmpm$inegi==12057)
sel.son <-    which(tmpm$inegi==12048)
censom06[c(sel.parent, sel.son),]
censom12[c(sel.parent, sel.son),]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom06[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom06[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom12[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom12[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
##
## Marquelia (created 2005)
## Elected 1st mu gov 2005
sel.parent <- which(tmpm$inegi==12013)
sel.son <-    which(tmpm$inegi==12077)
censom03[c(sel.parent, sel.son),]
censom06[c(sel.parent, sel.son),]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom03[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom06[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005)", colnames(tmpm))
tmpf[c(sel.parent, sel.son), sel.c] <- censom03[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpf[c(sel.parent, sel.son), sel.c] <- censom06[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son),]
tmpm[c(sel.parent, sel.son),]
##
## Cochoapa, JJ, Juchitán, Iliatenco (created 2008)
## Elected 1st mu gov 2008
sel.parent <- which(tmpm$inegi==12043)
sel.son <-    which(tmpm$inegi==12078)
censom06[c(sel.parent, sel.son),]
censom09[c(sel.parent, sel.son),]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom06[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom06[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom09[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom09[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
##
sel.parent <- which(tmpm$inegi==12028)
sel.son <-    which(tmpm$inegi==12079)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom06[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom06[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom09[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom09[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
##
sel.parent <- which(tmpm$inegi==12013)
sel.son <-    which(tmpm$inegi==12080)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom06[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom06[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom09[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom09[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
##
sel.parent <- which(tmpm$inegi==12041)
sel.son <-    which(tmpm$inegi==12081)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom06[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom06[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom09[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom09[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
##
## San Ignacio (created 2006)
## Elected 1st mu gov 2006, into fed els 2012 (use 2012 map for 2006:2012 mun els)
sel.parent <- which(tmpm$inegi==14008)
sel.son <-    which(tmpm$inegi==14125)
censom09[c(sel.parent, sel.son),]
censom12[c(sel.parent, sel.son),]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom09[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011)", colnames(tmpm))
tmpf[c(sel.parent, sel.son), sel.c] <- censom09[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom12[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpf[c(sel.parent, sel.son), sel.c] <- censom12[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
##
## 4 secs from pachuca to mineral ref in 2007
sel.parent <- which(tmpm$inegi==13048)
sel.son <-    which(tmpm$inegi==13051)
censom06[c(sel.parent, sel.son),]
censom09[c(sel.parent, sel.son),]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom06[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom06[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom09[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom09[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
##
## 13 secs from chicolapan to la paz in 2013
sel.parent <- which(tmpm$inegi==15029)
sel.son <-    which(tmpm$inegi==15070)
censom09[c(sel.parent, sel.son),]
censom15[c(sel.parent, sel.son),]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom09[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom09[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
##
## 2 secs from sn mateo to lerma in 2016
sel.parent <- which(tmpm$inegi==15076)
sel.son <-    which(tmpm$inegi==15051)
censom15[c(sel.parent, sel.son),]
censom18[c(sel.parent, sel.son),]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
##
## 3 secs from toluca to otzolotepec in 2016
## 3 secs from toluca to otzolotepec in 2017
sel.parent <- which(tmpm$inegi==15106)
sel.son <-    which(tmpm$inegi==15067)
censom15[c(sel.parent, sel.son),]
censom18[c(sel.parent, sel.son),]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
##
## 1 sec from almoloya to villa vic in 2016
sel.parent <- which(tmpm$inegi==15005)
sel.son <-    which(tmpm$inegi==15114)
censom15[c(sel.parent, sel.son),]
censom18[c(sel.parent, sel.son),]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
##
##
## Luvianos, SJ (created 2003)
## Elected 1st mu gov 2003
## Looks ok
sel.parent <- which(tmpm$inegi==15074)
sel.son <-    which(tmpm$inegi==15123)
censom00[c(sel.parent, sel.son),]
censom03[c(sel.parent, sel.son),]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom00[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom00[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom03[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom03[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
##
sel.parent <- which(tmpm$inegi==15044)
sel.son <-    which(tmpm$inegi==15124)
censom00[c(sel.parent, sel.son),]
censom03[c(sel.parent, sel.son),]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom00[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom00[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom03[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom03[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
##
## Tonatitla (created 2006)
## Elected 1st mu gov 2006
## Looks ok
sel.parent <- which(tmpm$inegi==15044)
sel.son <-    which(tmpm$inegi==15125)
censom03[c(sel.parent, sel.son),]
censom06[c(sel.parent, sel.son),]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom03[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom03[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom06[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom06[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
##
## 1 sec from tarimbaro to morelia in 2014
sel.parent <- which(tmpm$inegi==16088)
sel.son <-    which(tmpm$inegi==16053)
censom12[c(sel.parent, sel.son),]
censom15[c(sel.parent, sel.son),]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom12[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom12[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
##
## 1 sec from apodaca to gpe in 2017
sel.parent <- which(tmpm$inegi==19006)
sel.son <-    which(tmpm$inegi==19026)
censom15[c(sel.parent, sel.son),]
censom18[c(sel.parent, sel.son),]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
##
## ## 4 secs from apodaca to gpe in 2021 CANT BE FIXED WITH 2020 CENSO ONLY
## sel.parent <- which(tmpm$inegi==19006)
## sel.son <-    which(tmpm$inegi==16026)
## censom21[c(sel.parent, sel.son),]
## censom24[c(sel.parent, sel.son),]
## sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
## tmpm[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
## tmpf[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
## sel.c <- grep("p18_(2022)", colnames(tmpm))
## tmpm[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
## tmpf[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
## tmpm[c(sel.parent, sel.son),]
##
## 1 sec from sn marcos to huajua in 2015
## 1 sec from sn jer to huajua in 2015
sel.parent <- which(tmpm$inegi==20160 | tmpm$inegi==20237)
sel.son <-    which(tmpm$inegi==20039)
censom15[c(sel.parent, sel.son),]
censom18[c(sel.parent, sel.son),]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
##
## 1 sec from cholula to puebla in 2014
sel.parent <- which(tmpm$inegi==21119)
sel.son <-    which(tmpm$inegi==21114)
censom12[c(sel.parent, sel.son),]
censom15[c(sel.parent, sel.son),]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom12[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom12[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
##
## 1 sec from quere to el marques in 2008
sel.parent <- which(tmpm$inegi==22014)
sel.son <-    which(tmpm$inegi==22011)
censom06[c(sel.parent, sel.son),]
censom09[c(sel.parent, sel.son),]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
##
##
## SPECIAL CASE, SOLIDARIDAD AND TULUM NEED JOINT MANIPULATION
## Solidaridad (created 1996)
## Elected 1st mu gov 1996
## Tulum (created 2009)
## Elected 1st mu gov 2009, into fed els til 2018
sel.r <- which(tmpm$inegi==23001 | tmpm$inegi==23008 | tmpm$inegi==23009)
censom94[sel.r,]
censom97[sel.r,]
censom06[sel.r,]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995)", colnames(tmpm))
tmpm[sel.r, sel.c] <- censom94[sel.r, sel.c]
sel.c <- grep("p18_(1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008)", colnames(tmpm))
tmpm[sel.r, sel.c] <- censom97[sel.r, sel.c]
sel.c <- grep("p18_(2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[sel.r, sel.c] <- censom06[sel.r, sel.c]
tmpm[sel.r,]
##
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996)", colnames(tmpm))
tmpf[sel.r, sel.c] <- censom94[sel.r, sel.c]
sel.c <- grep("p18_(1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017)", colnames(tmpm))
tmpf[sel.r, sel.c] <- censom97[sel.r, sel.c]
sel.c <- grep("p18_(2018|2019|2020|2021)", colnames(tmpm))
tmpf[sel.r, sel.c] <- censom06[sel.r, sel.c]
tmpf[sel.r,]
##
## ## 6 secs from solidaridad to pto mor in 2020 CANT BE FIXED WITH 2020 CENSO ONLY
## sel.parent <- which(tmpm$inegi==23008)
## sel.son <-    which(tmpm$inegi==23011)
## censom18[c(sel.parent, sel.son),]
## censom21[c(sel.parent, sel.son),]
## sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020)", colnames(tmpm))
## tmpm[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
## tmpf[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
## sel.c <- grep("p18_(2021)", colnames(tmpm))
## tmpm[c(sel.parent, sel.son), sel.c] <- censom21[c(sel.parent, sel.son), sel.c]
## tmpf[c(sel.parent, sel.son), sel.c] <- censom21[c(sel.parent, sel.son), sel.c]
## tmpm[c(sel.parent, sel.son),]
##
## Bacalar (created 2013)
## Elected 1st mu gov 2013, fed els in 2015
sel.parent <- which(tmpm$inegi==23004)
sel.son <-    which(tmpm$inegi==23010)
censom12[c(sel.parent, sel.son),]
censom15[c(sel.parent, sel.son),]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom12[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom12[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
##
## Pto Mor (created 2016)
## Elected 1st mu gov 2016, into fed els til 2018
## Looks ok
sel.parent <- which(tmpm$inegi==23005)
sel.son <-    which(tmpm$inegi==23011)
censom15[c(sel.parent, sel.son),]
censom18[c(sel.parent, sel.son),]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
##
## 12 secs from slp to soledad in 2014
sel.parent <- which(tmpm$inegi==24028)
sel.son <-    which(tmpm$inegi==24035)
censom12[c(sel.parent, sel.son),]
censom15[c(sel.parent, sel.son),]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
##
## ## 1 sec from magdalena to chiau in 2020 CANT BE FIXED WITH 2020 CENSO ONLY
## sel.parent <- which(tmpm$inegi==29048)
## sel.son <-    which(tmpm$inegi==29010)
## censom18[c(sel.parent, sel.son),]
## censom21[c(sel.parent, sel.son),]
## sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020)", colnames(tmpm))
## tmpm[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
## tmpf[c(sel.parent, sel.son), sel.c] <- censom15[c(sel.parent, sel.son), sel.c]
## sel.c <- grep("p18_(2021)", colnames(tmpm))
## tmpm[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
## tmpf[c(sel.parent, sel.son), sel.c] <- censom18[c(sel.parent, sel.son), sel.c]
## tmpm[c(sel.parent, sel.son),]
##
## Matlapa, Naranjo, Benito Juárez, San Ignacio (created 1997)
## Elected 1st mu gov 1997
## Pre-97 map needs sums
sel.parent <- which(tmpm$inegi==24037)
sel.son <-    which(tmpm$inegi==24057)
censom94[c(sel.parent, sel.son),]
censom97[c(sel.parent, sel.son),]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
##
sel.parent <- which(tmpm$inegi==24010)
sel.son <-    which(tmpm$inegi==24058)
censom94[c(sel.parent, sel.son),]
censom97[c(sel.parent, sel.son),]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
##
sel.parent <- which(tmpm$inegi==26026)
sel.son <-    which(tmpm$inegi==26071)
censom94[c(sel.parent, sel.son),]
censom97[c(sel.parent, sel.son),]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
##
sel.parent <- which(tmpm$inegi==26029)
sel.son <-    which(tmpm$inegi==26072)
censom94[c(sel.parent, sel.son),]
censom97[c(sel.parent, sel.son),]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
tmpm[c(sel.parent, sel.son),]
##
## tlaxcala bunch (created 1996)
## Elected 1st mu gov 1996
## 1994 map needs sums/zeroes
sel.parent <- which(tmpm$inegi==29020)
sel.son <-    which(tmpm$inegi==29045)
censom94[c(sel.parent, sel.son),]
censom97[c(sel.parent, sel.son),]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
##
sel.parent <- which(tmpm$inegi==29030)
sel.son <-    which(tmpm$inegi==29046)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
##
sel.parent <- which(tmpm$inegi==29030)
sel.son <-    which(tmpm$inegi==29047)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
##
sel.parent <- which(tmpm$inegi==29010)
sel.son <-    which(tmpm$inegi==29048)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
##
sel.parent <- which(tmpm$inegi==29032)
sel.son <-    which(tmpm$inegi==29049)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
##
sel.parent <- which(tmpm$inegi==29010)
sel.son <-    which(tmpm$inegi==29050)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
##
sel.parent <- which(tmpm$inegi==29032)
sel.son <-    which(tmpm$inegi==29051)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
##
sel.parent <- which(tmpm$inegi==29038)
sel.son <-    which(tmpm$inegi==29052)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
##
sel.parent <- which(tmpm$inegi==29029)
sel.son <-    which(tmpm$inegi==29053)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
##
sel.parent <- which(tmpm$inegi==29044)
sel.son <-    which(tmpm$inegi==29054)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
##
sel.parent <- which(tmpm$inegi==29040)
sel.son <-    which(tmpm$inegi==29055)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
##
sel.parent <- which(tmpm$inegi==29015)
sel.son <-    which(tmpm$inegi==29056)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
##
sel.parent <- which(tmpm$inegi==29023)
sel.son <-    which(tmpm$inegi==29057)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
##
sel.parent <- which(tmpm$inegi==29044)
sel.son <-    which(tmpm$inegi==29058)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
##
sel.parent <- which(tmpm$inegi==29022)
sel.son <-    which(tmpm$inegi==29059)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
##
sel.parent <- which(tmpm$inegi==29029)
sel.son <-    which(tmpm$inegi==29060)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
##
## Carrillo, tata, Uxpa (created 1997)
## Elected 1st mu gov 1997
## Pre-97 map needs sums
sel.parent <- which(tmpm$inegi==30045)
sel.son <-    which(tmpm$inegi==30208)
censom94[c(sel.parent, sel.son),]
censom97[c(sel.parent, sel.son),]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
##
sel.parent <- which(tmpm$inegi==30104)
sel.son <-    which(tmpm$inegi==30209)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
##
sel.parent <- which(tmpm$inegi==30070)
sel.son <-    which(tmpm$inegi==30210)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom94[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(1997|1998|1999|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom97[c(sel.parent, sel.son), sel.c]
##
## San Rafael, santiago (created 2004)
## Elected 1st mu gov 2004
sel.parent <- which(tmpm$inegi==30102)
sel.son <-    which(tmpm$inegi==30211)
censom03[c(sel.parent, sel.son),]
censom06[c(sel.parent, sel.son),]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom03[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom03[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom06[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom06[c(sel.parent, sel.son), sel.c]
##
sel.parent <- which(tmpm$inegi==30130)
sel.son <-    which(tmpm$inegi==30212)
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom03[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom03[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom06[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son), sel.c] <- censom06[c(sel.parent, sel.son), sel.c]
##
## Trancoso (created 2001)
## Elected 1st mu gov 2004
sel.parent <- which(tmpm$inegi==32017)
sel.son <-    which(tmpm$inegi==32057)
censom00[c(sel.parent, sel.son),]
censom03[c(sel.parent, sel.son),]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom00[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom03[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002)", colnames(tmpm))
tmpf[c(sel.parent, sel.son), sel.c] <- censom00[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpf[c(sel.parent, sel.son), sel.c] <- censom03[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son),]
tmpm[c(sel.parent, sel.son),]
##
## Sta María (created 2007)
## Elected 1st mu gov 2007, into fed els 2006
## all ok
sel.parent <- which(tmpm$inegi==32047)
sel.son <-    which(tmpm$inegi==32058)
censom03[c(sel.parent, sel.son),]
censom06[c(sel.parent, sel.son),]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom03[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpm[c(sel.parent, sel.son), sel.c] <- censom06[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005)", colnames(tmpm))
tmpf[c(sel.parent, sel.son), sel.c] <- censom03[c(sel.parent, sel.son), sel.c]
sel.c <- grep("p18_(2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021)", colnames(tmpm))
tmpf[c(sel.parent, sel.son), sel.c] <- censom06[c(sel.parent, sel.son), sel.c]
tmpf[c(sel.parent, sel.son),]
tmpm[c(sel.parent, sel.son),]
##
## Keep fed el yrs only
tmpf <- tmpf[, c("edon", "ife", "inegi", "mun",
##                 "p18_1990", "p18_1995", "p18_2000", "p18_2005", "p18_2010", "p18_2020", 
                 "p18_1991", "p18_1994", "p18_1997", "p18_2000", "p18_2003", "p18_2006", "p18_2009", "p18_2012", "p18_2015", "p18_2018", "p18_2021")]
## Check
tmpf[2,]
tmpm[2,]

#############
## Export  ##
#############
## This has projections with all years since 1990, municipios adjusted to municipal election calendars
write.csv(x=tmpm, file="/home/eric/Dropbox/data/elecs/MXelsCalendGovt/censos/data/pob18/p18mu-for-municipal-elecs.csv", row.names=FALSE)
##
## This has projections with fed elec yrs since 1991, municipios adjusted to federal election calendars
write.csv(x=tmpf, file="/home/eric/Dropbox/data/elecs/MXelsCalendGovt/censos/data/pob18/p18mu-for-federal-elecs.csv", row.names=FALSE)

###########
## Clean ##
###########
rm(mywrap, prj0005, prj0510, prj1020, prj9095, prj9500, sel.c, sel.parent, sel.son, sel.r)
