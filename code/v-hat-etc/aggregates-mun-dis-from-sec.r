#########################################################################
## Script to aggregate municipio and distrito diputado federal returns ##
## from seccion-level INE reports                                      ##
## invoked from code/elec-data-for-maps.r                              ##
##                                                                     ##
## Author: Eric Magar                                                  ##
## emagar at itam dot mx                                               ##
## Date: 6may2023                                                      ##
## Last modified: 7jun2023                                             ##
#########################################################################


#######################################
## ################################# ##
## ## aggregate municipio returns ## ##
## ################################# ##
#######################################

##########################################################################
## Function to square d by adding municipios absent from that election  ##
##########################################################################
add.miss.mun <- function(d){
    ## get vector of all municipios in eq 
    all.ife <- eq[,grep("^ife", colnames(eq))]
    all.ife <- as.vector(unlist(all.ife))
    all.ife <- unique(all.ife)
    all.ife <- all.ife[order(all.ife)]
    all.ife <- all.ife[-which(all.ife==0)] ## drop ife==0 of secciones dadas de baja from vector
    ##tail(all.ife)
    ## any municipios missing?
    mis.ife <- all.ife[which(all.ife %notin% d$ife)]
    if (length(mis.ife)>0){
        tmp <- d[1:length(mis.ife),]
        tmp[] <- NA
        tmp$ife <- mis.ife
        tmp$edon <- as.integer(tmp$ife/1000)
        ## if so, add empty rows to d
        d <- rbind(d, tmp)
    }
    return(d)
}

##########
## 1991 ## OJO: 1991 seccion identifiers are wrong, can aggregate with ife, but no info for counterfactuals
##########
sel.c <- c("pan","pri","pps","prd","pfcrn","parm","pdm","prt","pem","pt","efec","lisnom","dextra")
# actual municipalities
d <- v91s; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
#d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
#d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
#d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
#d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- d[moveme(names(d), "efec before lisnom; ife after edon; mun after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v91m <- d                                      # rename object  

##########
## 1994 ##
##########
sel.c <- c("pan","pri","pps","prd","pfcrn","parm","uno.pdm","pt","pvem","efec","lisnom","dextra")
# actual 1994 municipalities
d <- v94s; d[is.na(d)] <- 0
d$ife <- d$ife1994                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v94m <- d                                      # rename object  
# 1997 municipalities
d <- v94s; d[is.na(d)] <- 0
d$ife <- d$ife1997                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v94m97 <- d                                    # rename object  
# 2000 municipalities
d <- v94s; d[is.na(d)] <- 0
d$ife <- d$ife2000                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v94m00 <- d                                    # rename object  
# 2003 municipalities
d <- v94s; d[is.na(d)] <- 0
d$ife <- d$ife2003                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v94m03 <- d                                    # rename object  
# 2006 municipalities
d <- v94s; d[is.na(d)] <- 0
d$ife <- d$ife2006                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v94m06 <- d                                    # rename object  
# 2009 municipalities
d <- v94s; d[is.na(d)] <- 0
d$ife <- d$ife2009                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v94m09 <- d                                    # rename object  
# 2012 municipalities
d <- v94s; d[is.na(d)] <- 0
d$ife <- d$ife2012                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v94m12 <- d                                    # rename object  
# 2015 municipalities
d <- v94s; d[is.na(d)] <- 0
d$ife <- d$ife2015                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v94m15 <- d                                    # rename object  
# 2018 municipalities
d <- v94s; d[is.na(d)] <- 0
d$ife <- d$ife2018                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v94m18 <- d                                    # rename object  
# 2021 municipalities
d <- v94s; d[is.na(d)] <- 0
d$ife <- d$ife2021                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v94m21 <- d                                    # rename object  

##########
## 1997 ##
##########
sel.c <- c("pan","pri","prd","pc","pt","pvem","pps","pdm","efec","lisnom","dextra")
# 1994 municipalities
d <- v97s; d[is.na(d)] <- 0
#d$ife <- d$ife1994                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v97m94 <- d                                    # rename object  
# actual 1997 municipalities
d <- v97s; d[is.na(d)] <- 0
#d$ife <- d$ife1997                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v97m <- d                                      # rename object  
# 2000 municipalities
d <- v97s; d[is.na(d)] <- 0
d$ife <- d$ife2000                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v97m00 <- d                                    # rename object  
# 2003 municipalities
d <- v97s; d[is.na(d)] <- 0
d$ife <- d$ife2003                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v97m03 <- d                                    # rename object  
# 2006 municipalities
d <- v97s; d[is.na(d)] <- 0
d$ife <- d$ife2006                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v97m06 <- d                                    # rename object  
# 2009 municipalities
d <- v97s; d[is.na(d)] <- 0
d$ife <- d$ife2009                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v97m09 <- d                                    # rename object  
# 2012 municipalities
d <- v97s; d[is.na(d)] <- 0
d$ife <- d$ife2012                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v97m12 <- d                                    # rename object  
# 2015 municipalities
d <- v97s; d[is.na(d)] <- 0
d$ife <- d$ife2015                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v97m15 <- d                                    # rename object  
# 2018 municipalities
d <- v97s; d[is.na(d)] <- 0
d$ife <- d$ife2018                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v97m18 <- d                                    # rename object  
# 2021 municipalities
d <- v97s; d[is.na(d)] <- 0
d$ife <- d$ife2021                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v97m21 <- d                                    # rename object  

##########
## 2000 ##
##########
sel.c <- c("panc","pri","prdc","pcd","parm","dsppn","efec","lisnom","dpanc","dprdc","dextra")
# 1994 municipalities
d <- v00s; d[is.na(d)] <- 0
d$ife <- d$ife1994                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v00m94 <- d                                    # rename object  
# 1997 municipalities
d <- v00s; d[is.na(d)] <- 0
d$ife <- d$ife1997                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v00m97 <- d                                    # rename object  
# actual 2000 municipalities
d <- v00s; d[is.na(d)] <- 0
#d$ife <- d$ife2000                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v00m <- d                                      # rename object  
# 2003 municipalities
d <- v00s; d[is.na(d)] <- 0
d$ife <- d$ife2003                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v00m03 <- d                                    # rename object  
# 2006 municipalities
d <- v00s; d[is.na(d)] <- 0
d$ife <- d$ife2006                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v00m06 <- d                                    # rename object  
# 2009 municipalities
d <- v00s; d[is.na(d)] <- 0
d$ife <- d$ife2009                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v00m09 <- d                                    # rename object  
# 2012 municipalities
d <- v00s; d[is.na(d)] <- 0
d$ife <- d$ife2012                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v00m12 <- d                                    # rename object  
# 2015 municipalities
d <- v00s; d[is.na(d)] <- 0
d$ife <- d$ife2015                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v00m15 <- d                                    # rename object  
# 2018 municipalities
d <- v00s; d[is.na(d)] <- 0
d$ife <- d$ife2018                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v00m18 <- d                                    # rename object  
# 2021 municipalities
d <- v00s; d[is.na(d)] <- 0
d$ife <- d$ife2021                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v00m21 <- d                                    # rename object  

##########
## 2003 ##
##########
sel.c <- c("pan","pri","pric","prd","pt","pvem","conve","psn","pas","mp","plm","fc","efec","lisnom","dpric","dextra")
# 1994 municipalities
d <- v03s; d[is.na(d)] <- 0
d$ife <- d$ife1994                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v03m94 <- d                                    # rename object  
# 1997 municipalities
d <- v03s; d[is.na(d)] <- 0
d$ife <- d$ife1997                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v03m97 <- d                                    # rename object  
# 2000 municipalities
d <- v03s; d[is.na(d)] <- 0
d$ife <- d$ife2000                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v03m00 <- d                                    # rename object  
# 2003 actual municipalities
d <- v03s; d[is.na(d)] <- 0
#d$ife <- d$ife2003                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v03m <- d                                      # rename object  
# 2006 municipalities
d <- v03s; d[is.na(d)] <- 0
d$ife <- d$ife2006                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v03m06 <- d                                    # rename object  
# 2009 municipalities
d <- v03s; d[is.na(d)] <- 0
d$ife <- d$ife2009                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v03m09 <- d                                    # rename object  
# 2012 municipalities
d <- v03s; d[is.na(d)] <- 0
d$ife <- d$ife2012                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v03m12 <- d                                    # rename object  
# 2015 municipalities
d <- v03s; d[is.na(d)] <- 0
d$ife <- d$ife2015                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v03m15 <- d                                    # rename object  
# 2018 municipalities
d <- v03s; d[is.na(d)] <- 0
d$ife <- d$ife2018                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v03m18 <- d                                    # rename object  
# 2021 municipalities
d <- v03s; d[is.na(d)] <- 0
d$ife <- d$ife2021                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v03m21 <- d                                    # rename object  

##########
## 2006 ##
##########
sel.c <- c("pan","pric","prdc","pna","asdc","efec","lisnom","dpric","dprdc","dextra")
# 1994 municipalities
d <- v06s; d[is.na(d)] <- 0
d$ife <- d$ife1994                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v06m94 <- d                                    # rename object  
# 1997 municipalities
d <- v06s; d[is.na(d)] <- 0
d$ife <- d$ife1997                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v06m97 <- d                                    # rename object  
# 2000 municipalities
d <- v06s; d[is.na(d)] <- 0
d$ife <- d$ife2000                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v06m00 <- d                                    # rename object  
# 2003 municipalities
d <- v06s; d[is.na(d)] <- 0
d$ife <- d$ife2003                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v06m03 <- d                                    # rename object  
# 2006 actual municipalities
d <- v06s; d[is.na(d)] <- 0
#d$ife <- d$ife2006                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v06m <- d                                      # rename object  
# 2009 municipalities
d <- v06s; d[is.na(d)] <- 0
d$ife <- d$ife2009                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v06m09 <- d                                    # rename object  
# 2012 municipalities
d <- v06s; d[is.na(d)] <- 0
d$ife <- d$ife2012                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v06m12 <- d                                    # rename object  
# 2015 municipalities
d <- v06s; d[is.na(d)] <- 0
d$ife <- d$ife2015                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v06m15 <- d                                    # rename object  
# 2018 municipalities
d <- v06s; d[is.na(d)] <- 0
d$ife <- d$ife2018                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v06m18 <- d                                    # rename object  
# 2021 municipalities
d <- v06s; d[is.na(d)] <- 0
d$ife <- d$ife2021                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v06m21 <- d                                    # rename object  

##########
## 2009 ##
##########
sel.c <- c("pan","pri","pric","prd","pvem","pt","ptc","conve","pna","psd","efec","lisnom","dpric","dptc","dextra")
# 1994 municipalities
d <- v09s; d[is.na(d)] <- 0
d$ife <- d$ife1994                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dptc  <- as.numeric(d$dptc>0 )               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v09m94 <- d                                    # rename object  
# 1997 municipalities
d <- v09s; d[is.na(d)] <- 0
d$ife <- d$ife1997                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dptc  <- as.numeric(d$dptc>0 )               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v09m97 <- d                                    # rename object  
# 2000 municipalities
d <- v09s; d[is.na(d)] <- 0
d$ife <- d$ife2000                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dptc  <- as.numeric(d$dptc>0 )               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v09m00 <- d                                    # rename object  
# 2003 municipalities
d <- v09s; d[is.na(d)] <- 0
d$ife <- d$ife2003                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dptc  <- as.numeric(d$dptc>0 )               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v09m03 <- d                                    # rename object  
# 2006 municipalities
d <- v09s; d[is.na(d)] <- 0
d$ife <- d$ife2006                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dptc  <- as.numeric(d$dptc>0 )               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v09m06 <- d                                    # rename object  
# 2009 actual municipalities
d <- v09s; d[is.na(d)] <- 0
#d$ife <- d$ife2009                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dptc  <- as.numeric(d$dptc>0 )               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v09m <- d                                      # rename object  
# 2012 municipalities
d <- v09s; d[is.na(d)] <- 0
d$ife <- d$ife2012                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dptc  <- as.numeric(d$dptc>0 )               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v09m12 <- d                                    # rename object  
# 2015 municipalities
d <- v09s; d[is.na(d)] <- 0
d$ife <- d$ife2015                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dptc  <- as.numeric(d$dptc>0 )               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v09m15 <- d                                    # rename object  
# 2018 municipalities
d <- v09s; d[is.na(d)] <- 0
d$ife <- d$ife2018                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dptc  <- as.numeric(d$dptc>0 )               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v09m18 <- d                                    # rename object  
# 2021 municipalities
d <- v09s; d[is.na(d)] <- 0
d$ife <- d$ife2021                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dptc  <- as.numeric(d$dptc>0 )               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v09m21 <- d                                    # rename object  

##########
## 2012 ##
##########
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","pric","prdc","efec","lisnom","dpric","dprdc","dextra")
# 1994 municipalities
d <- v12s; d[is.na(d)] <- 0
d$ife <- d$ife1994                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v12m94 <- d                                    # rename object  
# 1997 municipalities
d <- v12s; d[is.na(d)] <- 0
d$ife <- d$ife1997                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v12m97 <- d                                    # rename object  
# 2000 municipalities
d <- v12s; d[is.na(d)] <- 0
d$ife <- d$ife2000                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v12m00 <- d                                    # rename object  
# 2003 municipalities
d <- v12s; d[is.na(d)] <- 0
d$ife <- d$ife2003                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v12m03 <- d                                    # rename object  
# 2006 municipalities
d <- v12s; d[is.na(d)] <- 0
d$ife <- d$ife2006                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v12m06 <- d                                    # rename object  
# 2009 municipalities
d <- v12s; d[is.na(d)] <- 0
d$ife <- d$ife2009                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v12m09 <- d                                    # rename object  
# 2012 municipalities
d <- v12s; d[is.na(d)] <- 0
#d$ife <- d$ife2000                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v12m <- d                                      # rename object  
# 2015 municipalities
d <- v12s; d[is.na(d)] <- 0
d$ife <- d$ife2015                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v12m15 <- d                                    # rename object  
# 2018 municipalities
d <- v12s; d[is.na(d)] <- 0
d$ife <- d$ife2018                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v12m18 <- d                                    # rename object  
# 2021 municipalities
d <- v12s; d[is.na(d)] <- 0
d$ife <- d$ife2021                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v12m21 <- d                                    # rename object  

##########
## 2015 ##
##########
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","ph","pes","pric","prdc","indep1","indep2","efec","lisnom","dpric","dprdc","dextra")
# 1994 municipalities
d <- v15s; d[is.na(d)] <- 0
d$ife <- d$ife1994                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v15m94 <- d                                    # rename object  
# 1997 municipalities
d <- v15s; d[is.na(d)] <- 0
d$ife <- d$ife1997                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v15m97 <- d                                    # rename object  
# 2000 municipalities
d <- v15s; d[is.na(d)] <- 0
d$ife <- d$ife2000                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v15m00 <- d                                    # rename object  
# 2003 municipalities
d <- v15s; d[is.na(d)] <- 0
d$ife <- d$ife2003                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v15m03 <- d                                    # rename object  
# 2006 municipalities
d <- v15s; d[is.na(d)] <- 0
d$ife <- d$ife2006                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v15m06 <- d                                    # rename object  
# 2009 municipalities
d <- v15s; d[is.na(d)] <- 0
d$ife <- d$ife2009                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v15m09 <- d                                    # rename object  
# 2012 municipalities
d <- v15s; d[is.na(d)] <- 0
d$ife <- d$ife2012                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v15m12 <- d                                    # rename object  
# 2015 actual municipalities
d <- v15s; d[is.na(d)] <- 0
#d$ife <- d$ife2015                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v15m <- d                                      # rename object  
# 2018 municipalities
d <- v15s; d[is.na(d)] <- 0
d$ife <- d$ife2018                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v15m18 <- d                                    # rename object  
# 2021 municipalities
d <- v15s; d[is.na(d)] <- 0
d$ife <- d$ife2021                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v15m21 <- d                                    # rename object  

##########
## 2018 ##
##########
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","pes","panc","pric","morenac","indep1","indep2","efec","lisnom","dpanc","dpric","dmorenac","dextra")
# 1994 municipalities
d <- v18s; d[is.na(d)] <- 0
d$ife <- d$ife1994                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
#d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v18m94 <- d                                    # rename object  
# 1997 municipalities
d <- v18s; d[is.na(d)] <- 0
d$ife <- d$ife1997                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
#d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v18m97 <- d                                    # rename object  
# 2000 municipalities
d <- v18s; d[is.na(d)] <- 0
d$ife <- d$ife2000                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
#d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v18m00 <- d                                    # rename object  
# 2003 municipalities
d <- v18s; d[is.na(d)] <- 0
d$ife <- d$ife2003                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
#d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v18m03 <- d                                    # rename object  
# 2006 municipalities
d <- v18s; d[is.na(d)] <- 0
d$ife <- d$ife2006                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
#d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v18m06 <- d                                    # rename object  
# 2009 municipalities
d <- v18s; d[is.na(d)] <- 0
d$ife <- d$ife2009                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
#d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v18m09 <- d                                    # rename object  
# 2012 municipalities
d <- v18s; d[is.na(d)] <- 0
d$ife <- d$ife2012                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
#d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v18m12 <- d                                    # rename object  
# 2015 municipalities
d <- v18s; d[is.na(d)] <- 0
d$ife <- d$ife2015                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
#d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v18m15 <- d                                    # rename object  
# 2018 municipalities
d <- v18s; d[is.na(d)] <- 0
#d$ife <- d$ife2018                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
#d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v18m <- d                                      # rename object  
# 2021 municipalities
d <- v18s; d[is.na(d)] <- 0
d$ife <- d$ife2021                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
#d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v18m21 <- d                                    # rename object  

##########
## 2021 ##
##########
sel.c <- c("pan","pri","prd","pvem","pt","mc","morena","pes","rsp","fxm","indep","panc","pric","morenac","efec","lisnom","dpanc","dpric","dmorenac","dextra")
# 1994 municipalities
d <- v21s; d[is.na(d)] <- 0
d$ife <- d$ife1994                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v21m94 <- d                                    # rename object
# 1997 municipalities
d <- v21s; d[is.na(d)] <- 0
d$ife <- d$ife1997                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v21m97 <- d                                    # rename object  
# 2000 municipalities
d <- v21s; d[is.na(d)] <- 0
d$ife <- d$ife2000                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v21m00 <- d                                    # rename object  
# 2003 municipalities
d <- v21s; d[is.na(d)] <- 0
d$ife <- d$ife2003                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v21m03 <- d                                    # rename object  
# 2006 municipalities
d <- v21s; d[is.na(d)] <- 0
d$ife <- d$ife2006                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v21m06 <- d                                    # rename object  
# 2009 municipalities
d <- v21s; d[is.na(d)] <- 0
d$ife <- d$ife2009                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v21m09 <- d                                    # rename object  
# 2012 municipalities
d <- v21s; d[is.na(d)] <- 0
d$ife <- d$ife2012                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v21m12 <- d                                    # rename object  
# 2015 municipalities
d <- v21s; d[is.na(d)] <- 0
d$ife <- d$ife2015                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v21m15 <- d                                    # rename object  
# 2018 municipalities
d <- v21s; d[is.na(d)] <- 0
d$ife <- d$ife2018                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v21m18 <- d                                    # rename object  
# 2021 actual municipalities
d <- v21s; d[is.na(d)] <- 0
#d$ife <- d$ife2021                             # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v21m <- d                                      # rename object

#####################
## Fix inegi codes ##
#####################
##
v94m  $inegi <- ife2inegi(v94m  $ife); v94m  $mun <- ife2mun(v94m  $ife)
v94m97$inegi <- ife2inegi(v94m97$ife); v94m97$mun <- ife2mun(v94m97$ife)
v94m00$inegi <- ife2inegi(v94m00$ife); v94m00$mun <- ife2mun(v94m00$ife)
v94m03$inegi <- ife2inegi(v94m03$ife); v94m03$mun <- ife2mun(v94m03$ife)
v94m06$inegi <- ife2inegi(v94m06$ife); v94m06$mun <- ife2mun(v94m06$ife)
v94m09$inegi <- ife2inegi(v94m09$ife); v94m09$mun <- ife2mun(v94m09$ife)
v94m12$inegi <- ife2inegi(v94m12$ife); v94m12$mun <- ife2mun(v94m12$ife)
v94m15$inegi <- ife2inegi(v94m15$ife); v94m15$mun <- ife2mun(v94m15$ife)
v94m18$inegi <- ife2inegi(v94m18$ife); v94m18$mun <- ife2mun(v94m18$ife)
v94m21$inegi <- ife2inegi(v94m21$ife); v94m21$mun <- ife2mun(v94m21$ife)
##
v97m94$inegi <- ife2inegi(v97m94$ife); v97m94$mun <- ife2mun(v97m94$ife)
v97m  $inegi <- ife2inegi(v97m  $ife); v97m  $mun <- ife2mun(v97m  $ife)
v97m00$inegi <- ife2inegi(v97m00$ife); v97m00$mun <- ife2mun(v97m00$ife)
v97m03$inegi <- ife2inegi(v97m03$ife); v97m03$mun <- ife2mun(v97m03$ife)
v97m06$inegi <- ife2inegi(v97m06$ife); v97m06$mun <- ife2mun(v97m06$ife)
v97m09$inegi <- ife2inegi(v97m09$ife); v97m09$mun <- ife2mun(v97m09$ife)
v97m12$inegi <- ife2inegi(v97m12$ife); v97m12$mun <- ife2mun(v97m12$ife)
v97m15$inegi <- ife2inegi(v97m15$ife); v97m15$mun <- ife2mun(v97m15$ife)
v97m18$inegi <- ife2inegi(v97m18$ife); v97m18$mun <- ife2mun(v97m18$ife)
v97m21$inegi <- ife2inegi(v97m21$ife); v97m21$mun <- ife2mun(v97m21$ife)
##
v00m94$inegi <- ife2inegi(v00m94$ife); v00m94$mun <- ife2mun(v00m94$ife)
v00m97$inegi <- ife2inegi(v00m97$ife); v00m97$mun <- ife2mun(v00m97$ife)
v00m  $inegi <- ife2inegi(v00m  $ife); v00m  $mun <- ife2mun(v00m  $ife)
v00m03$inegi <- ife2inegi(v00m03$ife); v00m03$mun <- ife2mun(v00m03$ife)
v00m06$inegi <- ife2inegi(v00m06$ife); v00m06$mun <- ife2mun(v00m06$ife)
v00m09$inegi <- ife2inegi(v00m09$ife); v00m09$mun <- ife2mun(v00m09$ife)
v00m12$inegi <- ife2inegi(v00m12$ife); v00m12$mun <- ife2mun(v00m12$ife)
v00m15$inegi <- ife2inegi(v00m15$ife); v00m15$mun <- ife2mun(v00m15$ife)
v00m18$inegi <- ife2inegi(v00m18$ife); v00m18$mun <- ife2mun(v00m18$ife)
v00m21$inegi <- ife2inegi(v00m21$ife); v00m21$mun <- ife2mun(v00m21$ife)
##                                   ##                                 
v03m94$inegi <- ife2inegi(v03m94$ife); v03m94$mun <- ife2mun(v03m94$ife)
v03m97$inegi <- ife2inegi(v03m97$ife); v03m97$mun <- ife2mun(v03m97$ife)
v03m00$inegi <- ife2inegi(v03m00$ife); v03m00$mun <- ife2mun(v03m00$ife)
v03m  $inegi <- ife2inegi(v03m  $ife); v03m  $mun <- ife2mun(v03m  $ife)
v03m06$inegi <- ife2inegi(v03m06$ife); v03m06$mun <- ife2mun(v03m06$ife)
v03m09$inegi <- ife2inegi(v03m09$ife); v03m09$mun <- ife2mun(v03m09$ife)
v03m12$inegi <- ife2inegi(v03m12$ife); v03m12$mun <- ife2mun(v03m12$ife)
v03m15$inegi <- ife2inegi(v03m15$ife); v03m15$mun <- ife2mun(v03m15$ife)
v03m18$inegi <- ife2inegi(v03m18$ife); v03m18$mun <- ife2mun(v03m18$ife)
v03m21$inegi <- ife2inegi(v03m21$ife); v03m21$mun <- ife2mun(v03m21$ife)
##                                   ##                                 
v06m94$inegi <- ife2inegi(v06m94$ife); v06m94$mun <- ife2mun(v06m94$ife)
v06m97$inegi <- ife2inegi(v06m97$ife); v06m97$mun <- ife2mun(v06m97$ife)
v06m00$inegi <- ife2inegi(v06m00$ife); v06m00$mun <- ife2mun(v06m00$ife)
v06m03$inegi <- ife2inegi(v06m03$ife); v06m03$mun <- ife2mun(v06m03$ife)
v06m  $inegi <- ife2inegi(v06m  $ife); v06m  $mun <- ife2mun(v06m  $ife)
v06m09$inegi <- ife2inegi(v06m09$ife); v06m09$mun <- ife2mun(v06m09$ife)
v06m12$inegi <- ife2inegi(v06m12$ife); v06m12$mun <- ife2mun(v06m12$ife)
v06m15$inegi <- ife2inegi(v06m15$ife); v06m15$mun <- ife2mun(v06m15$ife)
v06m18$inegi <- ife2inegi(v06m18$ife); v06m18$mun <- ife2mun(v06m18$ife)
v06m21$inegi <- ife2inegi(v06m21$ife); v06m21$mun <- ife2mun(v06m21$ife)
##                                   ##                                 
v09m94$inegi <- ife2inegi(v09m94$ife); v09m94$mun <- ife2mun(v09m94$ife)
v09m97$inegi <- ife2inegi(v09m97$ife); v09m97$mun <- ife2mun(v09m97$ife)
v09m00$inegi <- ife2inegi(v09m00$ife); v09m00$mun <- ife2mun(v09m00$ife)
v09m03$inegi <- ife2inegi(v09m03$ife); v09m03$mun <- ife2mun(v09m03$ife)
v09m06$inegi <- ife2inegi(v09m06$ife); v09m06$mun <- ife2mun(v09m06$ife)
v09m  $inegi <- ife2inegi(v09m  $ife); v09m  $mun <- ife2mun(v09m  $ife)
v09m12$inegi <- ife2inegi(v09m12$ife); v09m12$mun <- ife2mun(v09m12$ife)
v09m15$inegi <- ife2inegi(v09m15$ife); v09m15$mun <- ife2mun(v09m15$ife)
v09m18$inegi <- ife2inegi(v09m18$ife); v09m18$mun <- ife2mun(v09m18$ife)
v09m21$inegi <- ife2inegi(v09m21$ife); v09m21$mun <- ife2mun(v09m21$ife)
##                                   ##                                 
v12m94$inegi <- ife2inegi(v12m94$ife); v12m94$mun <- ife2mun(v12m94$ife)
v12m97$inegi <- ife2inegi(v12m97$ife); v12m97$mun <- ife2mun(v12m97$ife)
v12m00$inegi <- ife2inegi(v12m00$ife); v12m00$mun <- ife2mun(v12m00$ife)
v12m03$inegi <- ife2inegi(v12m03$ife); v12m03$mun <- ife2mun(v12m03$ife)
v12m06$inegi <- ife2inegi(v12m06$ife); v12m06$mun <- ife2mun(v12m06$ife)
v12m09$inegi <- ife2inegi(v12m09$ife); v12m09$mun <- ife2mun(v12m09$ife)
v12m  $inegi <- ife2inegi(v12m  $ife); v12m  $mun <- ife2mun(v12m  $ife)
v12m15$inegi <- ife2inegi(v12m15$ife); v12m15$mun <- ife2mun(v12m15$ife)
v12m18$inegi <- ife2inegi(v12m18$ife); v12m18$mun <- ife2mun(v12m18$ife)
v12m21$inegi <- ife2inegi(v12m21$ife); v12m21$mun <- ife2mun(v12m21$ife)
##                                   ##                                 
v15m94$inegi <- ife2inegi(v15m94$ife); v15m94$mun <- ife2mun(v15m94$ife)
v15m97$inegi <- ife2inegi(v15m97$ife); v15m97$mun <- ife2mun(v15m97$ife)
v15m00$inegi <- ife2inegi(v15m00$ife); v15m00$mun <- ife2mun(v15m00$ife)
v15m03$inegi <- ife2inegi(v15m03$ife); v15m03$mun <- ife2mun(v15m03$ife)
v15m06$inegi <- ife2inegi(v15m06$ife); v15m06$mun <- ife2mun(v15m06$ife)
v15m09$inegi <- ife2inegi(v15m09$ife); v15m09$mun <- ife2mun(v15m09$ife)
v15m12$inegi <- ife2inegi(v15m12$ife); v15m12$mun <- ife2mun(v15m12$ife)
v15m  $inegi <- ife2inegi(v15m  $ife); v15m  $mun <- ife2mun(v15m  $ife)
v15m18$inegi <- ife2inegi(v15m18$ife); v15m18$mun <- ife2mun(v15m18$ife)
v15m21$inegi <- ife2inegi(v15m21$ife); v15m21$mun <- ife2mun(v15m21$ife)
##                                   ##                                 
v18m94$inegi <- ife2inegi(v18m94$ife); v18m94$mun <- ife2mun(v18m94$ife)
v18m97$inegi <- ife2inegi(v18m97$ife); v18m97$mun <- ife2mun(v18m97$ife)
v18m00$inegi <- ife2inegi(v18m00$ife); v18m00$mun <- ife2mun(v18m00$ife)
v18m03$inegi <- ife2inegi(v18m03$ife); v18m03$mun <- ife2mun(v18m03$ife)
v18m06$inegi <- ife2inegi(v18m06$ife); v18m06$mun <- ife2mun(v18m06$ife)
v18m09$inegi <- ife2inegi(v18m09$ife); v18m09$mun <- ife2mun(v18m09$ife)
v18m12$inegi <- ife2inegi(v18m12$ife); v18m12$mun <- ife2mun(v18m12$ife)
v18m15$inegi <- ife2inegi(v18m15$ife); v18m15$mun <- ife2mun(v18m15$ife)
v18m  $inegi <- ife2inegi(v18m  $ife); v18m  $mun <- ife2mun(v18m  $ife)
v18m21$inegi <- ife2inegi(v18m21$ife); v18m21$mun <- ife2mun(v18m21$ife)
##                                   ##                                 
v21m94$inegi <- ife2inegi(v21m94$ife); v21m94$mun <- ife2mun(v21m94$ife)
v21m97$inegi <- ife2inegi(v21m97$ife); v21m97$mun <- ife2mun(v21m97$ife)
v21m00$inegi <- ife2inegi(v21m00$ife); v21m00$mun <- ife2mun(v21m00$ife)
v21m03$inegi <- ife2inegi(v21m03$ife); v21m03$mun <- ife2mun(v21m03$ife)
v21m06$inegi <- ife2inegi(v21m06$ife); v21m06$mun <- ife2mun(v21m06$ife)
v21m09$inegi <- ife2inegi(v21m09$ife); v21m09$mun <- ife2mun(v21m09$ife)
v21m12$inegi <- ife2inegi(v21m12$ife); v21m12$mun <- ife2mun(v21m12$ife)
v21m15$inegi <- ife2inegi(v21m15$ife); v21m15$mun <- ife2mun(v21m15$ife)
v21m18$inegi <- ife2inegi(v21m18$ife); v21m18$mun <- ife2mun(v21m18$ife)
v21m  $inegi <- ife2inegi(v21m  $ife); v21m  $mun <- ife2mun(v21m  $ife)


#######################
## census indicators ##
#######################
sel.c <- c("p18_2005","p18_2010","p18_2020","ptot_2005","ptot_2010","ptot_2020")
# 1994 municipalities
d <- censo; d[is.na(d)] <- 0
d$ife <- d$ife1994                                       # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))  # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]      # drop from aggregation
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

# verify nrow==2469
table(c(
    nrow(v91m), nrow(v94m),
    nrow(v97m), nrow(v00m), nrow(v03m),
    nrow(v06m), nrow(v09m), nrow(v12m), nrow(v15m),
    nrow(v18m), nrow(v21m),
##
##
    nrow(v97m94), nrow(v00m94), nrow(v03m94),
    nrow(v06m94), nrow(v09m94), nrow(v12m94), nrow(v15m94),
    nrow(v18m94), nrow(v21m94),
##
    nrow(v94m97),
                  nrow(v00m97), nrow(v03m97),
    nrow(v06m97), nrow(v09m97), nrow(v12m97), nrow(v15m97),
    nrow(v18m97), nrow(v21m97),
##
    nrow(v94m00),
    nrow(v97m00),               nrow(v03m00),
    nrow(v06m00), nrow(v09m00), nrow(v12m00), nrow(v15m00),
    nrow(v18m00), nrow(v21m00),
##
    nrow(v94m03),
    nrow(v97m03), nrow(v00m03), 
    nrow(v06m03), nrow(v09m03), nrow(v12m03), nrow(v15m03),
    nrow(v18m03), nrow(v21m03),
##
    nrow(v94m06),
    nrow(v97m06), nrow(v00m06), nrow(v03m06),
                  nrow(v09m06), nrow(v12m06), nrow(v15m06),
    nrow(v18m06), nrow(v21m06),
##
    nrow(v94m09),
    nrow(v97m09), nrow(v00m09), nrow(v03m09),
    nrow(v06m09),               nrow(v12m09), nrow(v15m09),
    nrow(v18m09), nrow(v21m09),
##
    nrow(v94m12),
    nrow(v97m12), nrow(v00m12), nrow(v03m12),
    nrow(v06m12), nrow(v09m12),               nrow(v15m12),
    nrow(v18m12), nrow(v21m12),
##
    nrow(v94m15),
    nrow(v97m15), nrow(v00m15), nrow(v03m15),
    nrow(v06m15), nrow(v09m15), nrow(v12m15), 
    nrow(v18m15), nrow(v21m15),
##
    nrow(v94m18),
    nrow(v97m18), nrow(v00m18), nrow(v03m18),
    nrow(v06m18), nrow(v09m18), nrow(v12m18), nrow(v15m18),
                  nrow(v21m18),
    ##
    nrow(v94m21),
    nrow(v97m21), nrow(v00m21), nrow(v03m21),
    nrow(v06m21), nrow(v09m21), nrow(v12m21), nrow(v15m21),
    nrow(v18m21),
    ##
    nrow(censom94), nrow(censom97), nrow(censom00), nrow(censom03), nrow(censom06),
    nrow(censom09), nrow(censom12), nrow(censom15), nrow(censom18), nrow(censom21)
))



######################################
## ################################ ##
## ## aggregate district returns ## ##
## ################################ ##
######################################

##########
## 1991 ## OJO: 1991 seccion identifiers are wrong, can aggregate with disn/ife, but no info for counterfactuals
##########
sel.c <- c("pan","pri","pps","prd","pfcrn","parm","pdm","prt","pem","pt","efec","lisnom","dextra")
# actual districts
d <- v91s; d[is.na(d)] <- 0
sel.drop <- which(d$disn==0)                   # drop secciones added to keep v.. objects square
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones added to keep v.. objects square
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=TRUE) # use aggregating function
#d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
#d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
#d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v91d <- d                                      # rename object  

##########
## 1994 ##
##########
sel.c <- c("pan","pri","pps","prd","pfcrn","parm","uno.pdm","pt","pvem","efec","lisnom","dextra")
# actual districts
d <- v94s; d[is.na(d)] <- 0
sel.drop <- which(d$disn==0)                   # drop secciones added to keep v.. objects square
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones added to keep v.. objects square
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v94d <- d                                      # rename object  
# 1997 counterfactual districts
d <- v94s; d[is.na(d)] <- 0
d$disn <- d$dis1997                            # counterfactual map
sel.drop <- which(d$disn==0)                   # drop secciones de baja
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones de baja
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v94d97 <- d                                    # rename object  
# 2006 counterfactual districts
d <- v94s; d[is.na(d)] <- 0
d$disn <- d$dis2006                            # counterfactual map
sel.drop <- which(d$disn==0)                   # drop secciones de baja
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones de baja
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v94d06 <- d                                    # rename object  
# 2018 counterfactual districts
d <- v94s; d[is.na(d)] <- 0
d$disn <- d$dis2018                            # counterfactual map
sel.drop <- which(d$disn==0)                   # drop secciones de baja
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones de baja
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v94d18 <- d                                    # rename object  

##########
## 1997 ##
##########
sel.c <- c("pan","pri","prd","pc","pt","pvem","pps","pdm","efec","lisnom","dextra")
# actual districts
d <- v97s; d[is.na(d)] <- 0
sel.drop <- which(d$disn==0)                   # drop secciones added to keep v.. objects square
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones added to keep v.. objects square
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v97d <- d                                      # rename object  
# 1979 counterfactual districts
d <- v97s; d[is.na(d)] <- 0
d$disn <- d$dis1979                            # counterfactual map
sel.drop <- which(d$disn==0)                   # drop secciones de baja
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones de baja
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v97d79 <- d                                    # rename object  
# 2006 counterfactual districts
d <- v97s; d[is.na(d)] <- 0
d$disn <- d$dis2006                            # counterfactual map
sel.drop <- which(d$disn==0)                   # drop secciones de baja
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones de baja
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v97d06 <- d                                    # rename object  
# 2018 counterfactual districts
d <- v97s; d[is.na(d)] <- 0
d$disn <- d$dis2018                            # counterfactual map
sel.drop <- which(d$disn==0)                   # drop secciones de baja
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones de baja
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v97d18 <- d                                    # rename object  

##########
## 2000 ##
##########
sel.c <- c("panc","pri","prdc","pcd","parm","dsppn","efec","lisnom","dpanc","dprdc","dextra")
# actual districts
d <- v00s; d[is.na(d)] <- 0
sel.drop <- which(d$disn==0)                   # drop secciones added to keep v.. objects square
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones added to keep v.. objects square
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v00d <- d                                      # rename object  
# 1979 counterfactual districts
d <- v00s; d[is.na(d)] <- 0
d$disn <- d$dis1979                            # counterfactual map
sel.drop <- which(d$disn==0)                   # drop secciones de baja
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones de baja
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v00d79 <- d                                    # rename object  
# 2006 counterfactual districts
d <- v00s; d[is.na(d)] <- 0
d$disn <- d$dis2006                            # counterfactual map
sel.drop <- which(d$disn==0)                   # drop secciones de baja
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones de baja
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v00d06 <- d                                    # rename object  
# 2018 counterfactual districts
d <- v00s; d[is.na(d)] <- 0
d$disn <- d$dis2018                            # counterfactual map
sel.drop <- which(d$disn==0)                   # drop secciones de baja
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones de baja
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v00d18 <- d                                    # rename object  

##########
## 2003 ##
##########
sel.c <- c("pan","pri","pric","prd","pt","pvem","conve","psn","pas","mp","plm","fc","efec","lisnom","dpric","dextra")
# actual districts
d <- v03s; d[is.na(d)] <- 0
sel.drop <- which(d$disn==0)                   # drop secciones added to keep v.. objects square
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones added to keep v.. objects square
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v03d <- d                                      # rename object  
# 1979 counterfactual districts
d <- v03s; d[is.na(d)] <- 0
d$disn <- d$dis1979                            # counterfactual map
sel.drop <- which(d$disn==0)                   # drop secciones de baja
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones de baja
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v03d79 <- d                                    # rename object  
# 2006 counterfactual districts
d <- v03s; d[is.na(d)] <- 0
d$disn <- d$dis2006                            # counterfactual map
sel.drop <- which(d$disn==0)                   # drop secciones de baja
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones de baja
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v03d06 <- d                                    # rename object  
# 2018 counterfactual districts
d <- v03s; d[is.na(d)] <- 0
d$disn <- d$dis2018                            # counterfactual map
sel.drop <- which(d$disn==0)                   # drop secciones de baja
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones de baja
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v03d18 <- d                                    # rename object  

##########
## 2006 ##
##########
sel.c <- c("pan","pric","prdc","pna","asdc","efec","lisnom","dpric","dprdc","dextra")
# actual districts
d <- v06s; d[is.na(d)] <- 0
sel.drop <- which(d$disn==0)                   # drop secciones added to keep v.. objects square
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones added to keep v.. objects square
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v06d <- d                                      # rename object  
# 1979 counterfactual districts
d <- v06s; d[is.na(d)] <- 0
d$disn <- d$dis1979                            # counterfactual map
sel.drop <- which(d$disn==0)                   # drop secciones de baja
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones de baja
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v06d79 <- d                                    # rename object  
# 1997 counterfactual districts
d <- v06s; d[is.na(d)] <- 0
d$disn <- d$dis1997                            # counterfactual map
sel.drop <- which(d$disn==0)                   # drop secciones de baja
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones de baja
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v06d97 <- d                                    # rename object  
# 2018 counterfactual districts
d <- v06s; d[is.na(d)] <- 0
d$disn <- d$dis2018                            # counterfactual map
sel.drop <- which(d$disn==0)                   # drop secciones de baja
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones de baja
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v06d18 <- d                                    # rename object  

##########
## 2009 ##
##########
sel.c <- c("pan","pri","pric","prd","pvem","pt","ptc","conve","pna","psd","efec","lisnom","dpric","dptc","dextra")
# actual districts
d <- v09s; d[is.na(d)] <- 0
sel.drop <- which(d$disn==0)                   # drop secciones added to keep v.. objects square
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones added to keep v.. objects square
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dptc  <- as.numeric(d$dptc>0 )               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v09d <- d                                      # rename object  
# 1979 counterfactual districts
d <- v09s; d[is.na(d)] <- 0
d$disn <- d$dis1979                            # counterfactual map
sel.drop <- which(d$disn==0)                   # drop secciones de baja
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones de baja
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dptc  <- as.numeric(d$dptc>0 )               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v09d79 <- d                                    # rename object  
# 1997 counterfactual districts
d <- v09s; d[is.na(d)] <- 0
d$disn <- d$dis1997                            # counterfactual map
sel.drop <- which(d$disn==0)                   # drop secciones de baja
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones de baja
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dptc  <- as.numeric(d$dptc>0 )               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v09d97 <- d                                    # rename object  
# 2018 counterfactual districts
d <- v09s; d[is.na(d)] <- 0
d$disn <- d$dis2018                            # counterfactual map
sel.drop <- which(d$disn==0)                   # drop secciones de baja
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones de baja
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dptc  <- as.numeric(d$dptc>0 )               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v09d18 <- d                                    # rename object  

##########
## 2012 ##
##########
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","pric","prdc","efec","lisnom","dpric","dprdc","dextra")
# actual districts
d <- v12s; d[is.na(d)] <- 0
sel.drop <- which(d$disn==0)                   # drop secciones added to keep v.. objects square
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones added to keep v.. objects square
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v12d <- d                                      # rename object  
# 1979 counterfactual districts
d <- v12s; d[is.na(d)] <- 0
d$disn <- d$dis1979                            # counterfactual map
sel.drop <- which(d$disn==0)                   # drop secciones de baja
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones de baja
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v12d79 <- d                                    # rename object  
# 1997 counterfactual districts
d <- v12s; d[is.na(d)] <- 0
d$disn <- d$dis1997                            # counterfactual map
sel.drop <- which(d$disn==0)                   # drop secciones de baja
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones de baja
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v12d97 <- d                                    # rename object  
# 2018 counterfactual districts
d <- v12s; d[is.na(d)] <- 0
d$disn <- d$dis2018                            # counterfactual map
sel.drop <- which(d$disn==0)                   # drop secciones de baja
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones de baja
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v12d18 <- d                                    # rename object  

##########
## 2015 ##
##########
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","ph","pes","pric","prdc","indep1","indep2","efec","lisnom","dpric","dprdc","dextra")
# actual districts
d <- v15s; d[is.na(d)] <- 0
sel.drop <- which(d$disn==0)                   # drop secciones added to keep v.. objects square
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones added to keep v.. objects square
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v15d <- d                                      # rename object  
# 1979 counterfactual districts
d <- v15s; d[is.na(d)] <- 0
d$disn <- d$dis1979                            # counterfactual map
sel.drop <- which(d$disn==0)                   # drop secciones de baja
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones de baja
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v15d79 <- d                                    # rename object  
# 1997 counterfactual districts
d <- v15s; d[is.na(d)] <- 0
d$disn <- d$dis1997                            # counterfactual map
sel.drop <- which(d$disn==0)                   # drop secciones de baja
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones de baja
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v15d97 <- d                                    # rename object  
# 2018 counterfactual districts
d <- v15s; d[is.na(d)] <- 0
d$disn <- d$dis2018                            # counterfactual map
sel.drop <- which(d$disn==0)                   # drop secciones de baja
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones de baja
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v15d18 <- d                                    # rename object  

##########
## 2018 ##
##########
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","pes","panc","pric","morenac","indep1","indep2","efec","lisnom","dpanc","dpric","dmorenac","dextra")
# actual districts
d <- v18s; d[is.na(d)] <- 0
sel.drop <- which(d$disn==0)                   # drop secciones added to keep v.. objects square
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones added to keep v.. objects square
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v18d <- d                                      # rename object  
# 1979 counterfactual districts
d <- v18s; d[is.na(d)] <- 0
d$disn <- d$dis1979                            # counterfactual map
sel.drop <- which(d$disn==0)                   # drop secciones de baja
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones de baja
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v18d79 <- d                                    # rename object  
# 1997 counterfactual districts
d <- v18s; d[is.na(d)] <- 0
d$disn <- d$dis1997                            # counterfactual map
sel.drop <- which(d$disn==0)                   # drop secciones de baja
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones de baja
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v18d97 <- d                                    # rename object  
# 2006 counterfactual districts
d <- v18s; d[is.na(d)] <- 0
d$disn <- d$dis2006                            # counterfactual map
sel.drop <- which(d$disn==0)                   # drop secciones de baja
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones de baja
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v18d06 <- d                                    # rename object  

##########
## 2021 ##
##########
sel.c <- c("pan","pri","prd","pvem","pt","mc","morena","pes","rsp","fxm","indep","panc","pric","morenac","efec","lisnom","dpanc","dpric","dmorenac","dextra")
# actual districts
d <- v21s; d[is.na(d)] <- 0
sel.drop <- which(d$disn==0)                   # drop secciones added to keep v.. objects square
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones added to keep v.. objects square
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v21d <- d                                      # rename object  
# 1979 counterfactual districts
d <- v21s; d[is.na(d)] <- 0
d$disn <- d$dis1979                            # counterfactual map
sel.drop <- which(d$disn==0)                   # drop secciones de baja
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones de baja
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v21d79 <- d                                    # rename object  
# 1997 counterfactual districts
d <- v21s; d[is.na(d)] <- 0
d$disn <- d$dis1997                            # counterfactual map
sel.drop <- which(d$disn==0)                   # drop secciones de baja
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones de baja
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v21d97 <- d                                    # rename object  
# 2006 counterfactual districts
d <- v21s; d[is.na(d)] <- 0
d$disn <- d$dis2006                            # counterfactual map
sel.drop <- which(d$disn==0)                   # drop secciones de baja
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones de baja
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v21d06 <- d                                    # rename object

#######################
## census indicators ##
#######################
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

# verify nrow==300
table(c(
    nrow(v91d), nrow(v94d),
    nrow(v97d), nrow(v00d), nrow(v03d),
    nrow(v06d), nrow(v09d), nrow(v12d), nrow(v15d),
    nrow(v18d), nrow(v21d), 
    nrow(v97d79), nrow(v00d79), nrow(v03d79),
    nrow(v06d79), nrow(v09d79), nrow(v12d79), nrow(v15d79),
    nrow(v18d79), nrow(v21d79), 
    nrow(v94d97),
    nrow(v06d97), nrow(v09d97), nrow(v12d97), nrow(v15d97),
    nrow(v18d97), nrow(v21d97), 
    nrow(v94d06),
    nrow(v97d06), nrow(v00d06), nrow(v03d06),
    nrow(v18d06), nrow(v21d06), 
    nrow(v94d18),
    nrow(v97d18), nrow(v00d18), nrow(v03d18), nrow(v06d18),
    nrow(v09d18), nrow(v12d18), nrow(v15d18),
    ##
    nrow(censod79), nrow(censod97), nrow(censod06), nrow(censod18)
))

