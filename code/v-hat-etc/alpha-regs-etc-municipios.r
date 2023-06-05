####################################################################
## Script for autoregressive vote estimates and alpha regressions ##
## invoked from code/elec-data-for-maps.r                         ##
##                                                                ##
## Author: Eric Magar                                             ##
## emagar at itam dot mx                                          ##
## Date: 17apr2023                                                ##
## Last modified: 30may2023                                       ##
####################################################################

###################################################################
## Note: Search function estim_mun below, that wraps estimation. ##
## Script above that preps time-series data.                     ##
## Script below that saves output and cleans.                    ##
###################################################################

#########################################################################################
## Municipio 5-yr estimates that can be computed before 2024                           ##
## |      | map  |      |      |      |      |       |       |       |       |       | ##
## | vhat | 1994 | 1997 | 2000 | 2003 | 2006 |  2009 |  2012 |  2015 |  2018 | 2021  | ##
## |------+------+------+------+------+------+-------+-------+-------+-------+-------| ##
## | 1991 | back |      |      |      |      |       |       |       |       |       | ##
## | 1994 | back |      |      |      |      |       |       |       |       |       | ##
## | 1997 |      | back |      |      |      |       |       |       |       |       | ##
## | 2000 |      |      | back |      |      |       |       |       |       |       | ##
## | 2003 |      |      |      | back |      |       |       |       |       |       | ##
## | 2006 |      |      |      |      |back* |       |       |       |       |       | ##
## | 2009 |      |      |      |      |      | *fwd* |       |       |       |       | ##
## | 2012 |      |      |      |      |      |       | *fwd* |       |       |       | ##
## | 2015 |      |      |      |      |      |       |       | *fwd* |       |       | ##
## | 2018 |      |      |      |      |      |       |       |       | *fwd* |       | ##
## | 2021 |      |      |      |      |      |       |       |       |       | *fwd* | ##
## | 2024 |      |      |      |      |      |       |       |       |       | *fwd* | ##
#########################################################################################

#################################
## aggregate municipio returns ##
#################################
##########################################################################
## Function to square d by adding municipios absent frim that election  ##
##########################################################################
add.miss.mun <- function(d){
    ## get vector of all municipios in eq 
    all.ife <- eq[,grep("^ife", colnames(eq))]
    all.ife <- as.vector(unlist(all.ife))
    all.ife <- unique(all.ife)
    all.ife <- all.ife[order(all.ife)]
    tail(all.ife)
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
# for estimation, will force 1991 into 1994 municipalities 
v91m94 <- v91m


##########
## 1994 ##
##########
sel.c <- c("pan","pri","pps","prd","pfcrn","parm","uno.pdm","pt","pvem","efec","lisnom","dextra")
# actual 1994 municipalities
d <- v94s; d[is.na(d)] <- 0
#d$ife <- d$ife1994                             # municipio ids from the historic map
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
d$ife <- d$ife1994                             # municipio ids from the historic map
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
#
rm(sel.c,add.miss.mun)

# verify nrow==2469
table(c(
    nrow(v91m), nrow(v94m),
    nrow(v97m), nrow(v00m), nrow(v03m),
    nrow(v06m), nrow(v09m), nrow(v12m), nrow(v15m),
    nrow(v18m), nrow(v21m),
##
##
    nrow(v91m94), 
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
    nrow(v18m21) 
))
##
nmun <- nrow(v21m)  ## n municipalities in square data

## empty 91 counterfactual maps to fill in below
v91mXX <- v91m
v91mXX[, c("pan","pri","pps","prd","pfcrn","parm","pdm","prt","pem","pt","efec","lisnom","dextra")] <- NA


###############################################################################
## Prepare manipulated party objects for time-series and alpha regressions   ##
## After 2024 election, uncheck/add lines                                    ##
##                *** One object per municipio map ***                       ##
## *** Map changes almost every year, so one per federal election  ***       ##
###############################################################################
#
# version 1: extend partial coalitions across the board
# shares
panm21 <- data.frame(
    v91 =  with(v91mXX, ifelse(efec==0, NA,  pan               / efec)), 
    v94 =  with(v94m21, ifelse(efec==0, NA,  pan               / efec)),
    v97 =  with(v97m21, ifelse(efec==0, NA,  pan               / efec)),
    v00 =  with(v00m21, ifelse(efec==0, NA,  panc              / efec)),
    v03 =  with(v03m21, ifelse(efec==0, NA,  pan               / efec)),
    v06 =  with(v06m21, ifelse(efec==0, NA,  pan               / efec)),
    v09 =  with(v09m21, ifelse(efec==0, NA,  pan               / efec)),
    v12 =  with(v12m21, ifelse(efec==0, NA,  pan               / efec)),
    v15 =  with(v15m21, ifelse(efec==0, NA,  pan               / efec)),
    v18 =  with(v18m21, ifelse(efec==0, NA, (pan + panc + prd) / efec)),  # dropped mc
    v21 =  with(v21m,   ifelse(efec==0, NA, (pan + panc + prd) / efec))   # drop prd?
)
panm18 <- data.frame(
    v91 =  with(v91mXX, ifelse(efec==0, NA,  pan               / efec)), 
    v94 =  with(v94m18, ifelse(efec==0, NA,  pan               / efec)),
    v97 =  with(v97m18, ifelse(efec==0, NA,  pan               / efec)),
    v00 =  with(v00m18, ifelse(efec==0, NA,  panc              / efec)),
    v03 =  with(v03m18, ifelse(efec==0, NA,  pan               / efec)),
    v06 =  with(v06m18, ifelse(efec==0, NA,  pan               / efec)),
    v09 =  with(v09m18, ifelse(efec==0, NA,  pan               / efec)),
    v12 =  with(v12m18, ifelse(efec==0, NA,  pan               / efec)),
    v15 =  with(v15m18, ifelse(efec==0, NA,  pan               / efec)),
    v18 =  with(v18m,   ifelse(efec==0, NA, (pan + panc + prd) / efec)),  # dropped mc
    v21 =  with(v21m18, ifelse(efec==0, NA, (pan + panc + prd) / efec))   # drop prd?
)
panm15 <- data.frame(
    v91 =  with(v91mXX, ifelse(efec==0, NA,  pan               / efec)), 
    v94 =  with(v94m15, ifelse(efec==0, NA,  pan               / efec)),
    v97 =  with(v97m15, ifelse(efec==0, NA,  pan               / efec)),
    v00 =  with(v00m15, ifelse(efec==0, NA,  panc              / efec)),
    v03 =  with(v03m15, ifelse(efec==0, NA,  pan               / efec)),
    v06 =  with(v06m15, ifelse(efec==0, NA,  pan               / efec)),
    v09 =  with(v09m15, ifelse(efec==0, NA,  pan               / efec)),
    v12 =  with(v12m15, ifelse(efec==0, NA,  pan               / efec)),
    v15 =  with(v15m,   ifelse(efec==0, NA,  pan               / efec)),
    v18 =  with(v18m15, ifelse(efec==0, NA, (pan + panc + prd) / efec)),  # dropped mc
    v21 =  with(v21m15, ifelse(efec==0, NA, (pan + panc + prd) / efec))   # drop prd?
)
panm12 <- data.frame(
    v91 =  with(v91mXX, ifelse(efec==0, NA,  pan               / efec)), 
    v94 =  with(v94m12, ifelse(efec==0, NA,  pan               / efec)),
    v97 =  with(v97m12, ifelse(efec==0, NA,  pan               / efec)),
    v00 =  with(v00m12, ifelse(efec==0, NA,  panc              / efec)),
    v03 =  with(v03m12, ifelse(efec==0, NA,  pan               / efec)),
    v06 =  with(v06m12, ifelse(efec==0, NA,  pan               / efec)),
    v09 =  with(v09m12, ifelse(efec==0, NA,  pan               / efec)),
    v12 =  with(v12m,   ifelse(efec==0, NA,  pan               / efec)),
    v15 =  with(v15m12, ifelse(efec==0, NA,  pan               / efec)),
    v18 =  with(v18m12, ifelse(efec==0, NA, (pan + panc + prd) / efec)),  # dropped mc
    v21 =  with(v21m12, ifelse(efec==0, NA, (pan + panc + prd) / efec))   # drop prd?
)
panm09 <- data.frame(
    v91 =  with(v91mXX, ifelse(efec==0, NA,  pan               / efec)), 
    v94 =  with(v94m09, ifelse(efec==0, NA,  pan               / efec)),
    v97 =  with(v97m09, ifelse(efec==0, NA,  pan               / efec)),
    v00 =  with(v00m09, ifelse(efec==0, NA,  panc              / efec)),
    v03 =  with(v03m09, ifelse(efec==0, NA,  pan               / efec)),
    v06 =  with(v06m09, ifelse(efec==0, NA,  pan               / efec)),
    v09 =  with(v09m,   ifelse(efec==0, NA,  pan               / efec)),
    v12 =  with(v12m09, ifelse(efec==0, NA,  pan               / efec)),
    v15 =  with(v15m09, ifelse(efec==0, NA,  pan               / efec)),
    v18 =  with(v18m09, ifelse(efec==0, NA, (pan + panc + prd) / efec)),  # dropped mc
    v21 =  with(v21m09, ifelse(efec==0, NA, (pan + panc + prd) / efec))   # drop prd?
)
panm06 <- data.frame(
    v91 =  with(v91mXX, ifelse(efec==0, NA,  pan               / efec)), 
    v94 =  with(v94m06, ifelse(efec==0, NA,  pan               / efec)),
    v97 =  with(v97m06, ifelse(efec==0, NA,  pan               / efec)),
    v00 =  with(v00m06, ifelse(efec==0, NA,  panc              / efec)),
    v03 =  with(v03m06, ifelse(efec==0, NA,  pan               / efec)),
    v06 =  with(v06m,   ifelse(efec==0, NA,  pan               / efec)),
    v09 =  with(v09m06, ifelse(efec==0, NA,  pan               / efec)),
    v12 =  with(v12m06, ifelse(efec==0, NA,  pan               / efec)),
    v15 =  with(v15m06, ifelse(efec==0, NA,  pan               / efec)),
    v18 =  with(v18m06, ifelse(efec==0, NA, (pan + panc + prd) / efec)),  # dropped mc
    v21 =  with(v21m06, ifelse(efec==0, NA, (pan + panc + prd) / efec))   # drop prd?
)
panm03 <- data.frame(
    v91 =  with(v91mXX, ifelse(efec==0, NA,  pan               / efec)), 
    v94 =  with(v94m03, ifelse(efec==0, NA,  pan               / efec)),
    v97 =  with(v97m03, ifelse(efec==0, NA,  pan               / efec)),
    v00 =  with(v00m03, ifelse(efec==0, NA,  panc              / efec)),
    v03 =  with(v03m,   ifelse(efec==0, NA,  pan               / efec)),
    v06 =  with(v06m03, ifelse(efec==0, NA,  pan               / efec)),
    v09 =  with(v09m03, ifelse(efec==0, NA,  pan               / efec)),
    v12 =  with(v12m03, ifelse(efec==0, NA,  pan               / efec)),
    v15 =  with(v15m03, ifelse(efec==0, NA,  pan               / efec)),
    v18 =  with(v18m03, ifelse(efec==0, NA, (pan + panc + prd) / efec)),  # dropped mc
    v21 =  with(v21m03, ifelse(efec==0, NA, (pan + panc + prd) / efec))   # drop prd?
)
panm00 <- data.frame(
    v91 =  with(v91mXX, ifelse(efec==0, NA,  pan               / efec)), 
    v94 =  with(v94m00, ifelse(efec==0, NA,  pan               / efec)),
    v97 =  with(v97m00, ifelse(efec==0, NA,  pan               / efec)),
    v00 =  with(v00m,   ifelse(efec==0, NA,  panc              / efec)),
    v03 =  with(v03m00, ifelse(efec==0, NA,  pan               / efec)),
    v06 =  with(v06m00, ifelse(efec==0, NA,  pan               / efec)),
    v09 =  with(v09m00, ifelse(efec==0, NA,  pan               / efec)),
    v12 =  with(v12m00, ifelse(efec==0, NA,  pan               / efec)),
    v15 =  with(v15m00, ifelse(efec==0, NA,  pan               / efec)),
    v18 =  with(v18m00, ifelse(efec==0, NA, (pan + panc + prd) / efec)),  # dropped mc
    v21 =  with(v21m00, ifelse(efec==0, NA, (pan + panc + prd) / efec))   # drop prd?
)
panm97 <- data.frame(
    v91 =  with(v91mXX, ifelse(efec==0, NA,  pan               / efec)), 
    v94 =  with(v94m97, ifelse(efec==0, NA,  pan               / efec)),
    v97 =  with(v97m,   ifelse(efec==0, NA,  pan               / efec)),
    v00 =  with(v00m97, ifelse(efec==0, NA,  panc              / efec)),
    v03 =  with(v03m97, ifelse(efec==0, NA,  pan               / efec)),
    v06 =  with(v06m97, ifelse(efec==0, NA,  pan               / efec)),
    v09 =  with(v09m97, ifelse(efec==0, NA,  pan               / efec)),
    v12 =  with(v12m97, ifelse(efec==0, NA,  pan               / efec)),
    v15 =  with(v15m97, ifelse(efec==0, NA,  pan               / efec)),
    v18 =  with(v18m97, ifelse(efec==0, NA, (pan + panc + prd) / efec)),  # dropped mc
    v21 =  with(v21m97, ifelse(efec==0, NA, (pan + panc + prd) / efec))   # drop prd?
)
panm94 <- data.frame(
    v91 =  with(v91m94, ifelse(efec==0, NA,  pan               / efec)), 
    v94 =  with(v94m,   ifelse(efec==0, NA,  pan               / efec)),
    v97 =  with(v97m94, ifelse(efec==0, NA,  pan               / efec)),
    v00 =  with(v00m94, ifelse(efec==0, NA,  panc              / efec)),
    v03 =  with(v03m94, ifelse(efec==0, NA,  pan               / efec)),
    v06 =  with(v06m94, ifelse(efec==0, NA,  pan               / efec)),
    v09 =  with(v09m94, ifelse(efec==0, NA,  pan               / efec)),
    v12 =  with(v12m94, ifelse(efec==0, NA,  pan               / efec)),
    v15 =  with(v15m94, ifelse(efec==0, NA,  pan               / efec)),
    v18 =  with(v18m94, ifelse(efec==0, NA, (pan + panc + prd) / efec)),  # dropped mc
    v21 =  with(v21m94, ifelse(efec==0, NA, (pan + panc + prd) / efec))   # drop prd?
)
## panm91 <- data.frame(
##     v91 =  with(v91m,   ifelse(efec==0, NA,  pan               / efec)), 
##     v94 =  with(v94m91, ifelse(efec==0, NA,  pan               / efec)),
##     v97 =  with(v97m91, ifelse(efec==0, NA,  pan               / efec)),
##     v00 =  with(v00m91, ifelse(efec==0, NA,  panc              / efec)),
##     v03 =  with(v03m91, ifelse(efec==0, NA,  pan               / efec)),
##     v06 =  with(v06m91, ifelse(efec==0, NA,  pan               / efec)),
##     v09 =  with(v09m91, ifelse(efec==0, NA,  pan               / efec)),
##     v12 =  with(v12m91, ifelse(efec==0, NA,  pan               / efec)),
##     v15 =  with(v15m91, ifelse(efec==0, NA,  pan               / efec)),
##     v18 =  with(v18m91, ifelse(efec==0, NA, (pan + panc + prd) / efec)),  # dropped mc
##     v21 =  with(v21m91, ifelse(efec==0, NA, (pan + panc + prd) / efec))   # drop prd?
## )
#
panm21 <- round(panm21, 3)
panm18 <- round(panm18, 3)
panm15 <- round(panm15, 3)
panm12 <- round(panm12, 3)
panm09 <- round(panm09, 3)
panm06 <- round(panm06, 3)
panm03 <- round(panm03, 3)
panm00 <- round(panm00, 3)
panm97 <- round(panm97, 3)
panm94 <- round(panm94, 3)
## panm91 <- round(panm91, 3)
#
prim21 <- data.frame(
    v91 =  with(v91mXX, ifelse(efec==0, NA,  pri                      / efec)),
    v94 =  with(v94m21, ifelse(efec==0, NA,  pri                      / efec)),
    v97 =  with(v97m21, ifelse(efec==0, NA,  pri                      / efec)),
    v00 =  with(v00m21, ifelse(efec==0, NA,  pri                      / efec)),
    v03 =  with(v03m21, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v06 =  with(v06m21, ifelse(efec==0, NA,  pric                     / efec)),
    v09 =  with(v09m21, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v12 =  with(v12m21, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v15 =  with(v15m21, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v18 =  with(v18m21, ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)), # drop pvem + pna?
    v21 =  with(v21m,   ifelse(efec==0, NA,  pri                      / efec))  # coal vote to pan+prd ok?
)
prim18 <- data.frame(
    v91 =  with(v91mXX, ifelse(efec==0, NA,  pri                      / efec)),
    v94 =  with(v94m18, ifelse(efec==0, NA,  pri                      / efec)),
    v97 =  with(v97m18, ifelse(efec==0, NA,  pri                      / efec)),
    v00 =  with(v00m18, ifelse(efec==0, NA,  pri                      / efec)),
    v03 =  with(v03m18, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v06 =  with(v06m18, ifelse(efec==0, NA,  pric                     / efec)),
    v09 =  with(v09m18, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v12 =  with(v12m18, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v15 =  with(v15m18, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v18 =  with(v18m,   ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)), # drop pvem + pna?
    v21 =  with(v21m18, ifelse(efec==0, NA,  pri                      / efec))  # coal vote to pan+prd ok?
)
prim15 <- data.frame(
    v91 =  with(v91mXX, ifelse(efec==0, NA,  pri                      / efec)),
    v94 =  with(v94m15, ifelse(efec==0, NA,  pri                      / efec)),
    v97 =  with(v97m15, ifelse(efec==0, NA,  pri                      / efec)),
    v00 =  with(v00m15, ifelse(efec==0, NA,  pri                      / efec)),
    v03 =  with(v03m15, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v06 =  with(v06m15, ifelse(efec==0, NA,  pric                     / efec)),
    v09 =  with(v09m15, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v12 =  with(v12m15, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v15 =  with(v15m,   ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v18 =  with(v18m15, ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)), # drop pvem + pna?
    v21 =  with(v21m15, ifelse(efec==0, NA,  pri                      / efec))  # coal vote to pan+prd ok?
)
prim12 <- data.frame(
    v91 =  with(v91mXX, ifelse(efec==0, NA,  pri                      / efec)),
    v94 =  with(v94m12, ifelse(efec==0, NA,  pri                      / efec)),
    v97 =  with(v97m12, ifelse(efec==0, NA,  pri                      / efec)),
    v00 =  with(v00m12, ifelse(efec==0, NA,  pri                      / efec)),
    v03 =  with(v03m12, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v06 =  with(v06m12, ifelse(efec==0, NA,  pric                     / efec)),
    v09 =  with(v09m12, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v12 =  with(v12m,   ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v15 =  with(v15m12, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v18 =  with(v18m12, ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)), # drop pvem + pna?
    v21 =  with(v21m12, ifelse(efec==0, NA,  pri                      / efec))  # coal vote to pan+prd ok?
)
prim09 <- data.frame(
    v91 =  with(v91mXX, ifelse(efec==0, NA,  pri                      / efec)),
    v94 =  with(v94m09, ifelse(efec==0, NA,  pri                      / efec)),
    v97 =  with(v97m09, ifelse(efec==0, NA,  pri                      / efec)),
    v00 =  with(v00m09, ifelse(efec==0, NA,  pri                      / efec)),
    v03 =  with(v03m09, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v06 =  with(v06m09, ifelse(efec==0, NA,  pric                     / efec)),
    v09 =  with(v09m,   ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v12 =  with(v12m09, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v15 =  with(v15m09, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v18 =  with(v18m09, ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)), # drop pvem + pna?
    v21 =  with(v21m09, ifelse(efec==0, NA,  pri                      / efec))  # coal vote to pan+prd ok?
)
prim06 <- data.frame(
    v91 =  with(v91mXX, ifelse(efec==0, NA,  pri                      / efec)),
    v94 =  with(v94m06, ifelse(efec==0, NA,  pri                      / efec)),
    v97 =  with(v97m06, ifelse(efec==0, NA,  pri                      / efec)),
    v00 =  with(v00m06, ifelse(efec==0, NA,  pri                      / efec)),
    v03 =  with(v03m06, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v06 =  with(v06m,   ifelse(efec==0, NA,  pric                     / efec)),
    v09 =  with(v09m06, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v12 =  with(v12m06, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v15 =  with(v15m06, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v18 =  with(v18m06, ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)), # drop pvem + pna?
    v21 =  with(v21m06, ifelse(efec==0, NA,  pri                      / efec))  # coal vote to pan+prd ok?
)
prim03 <- data.frame(
    v91 =  with(v91mXX, ifelse(efec==0, NA,  pri                      / efec)),
    v94 =  with(v94m03, ifelse(efec==0, NA,  pri                      / efec)),
    v97 =  with(v97m03, ifelse(efec==0, NA,  pri                      / efec)),
    v00 =  with(v00m03, ifelse(efec==0, NA,  pri                      / efec)),
    v03 =  with(v03m,   ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v06 =  with(v06m03, ifelse(efec==0, NA,  pric                     / efec)),
    v09 =  with(v09m03, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v12 =  with(v12m03, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v15 =  with(v15m03, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v18 =  with(v18m03, ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)), # drop pvem + pna?
    v21 =  with(v21m03, ifelse(efec==0, NA,  pri                      / efec))  # coal vote to pan+prd ok?
)
prim00 <- data.frame(
    v91 =  with(v91mXX, ifelse(efec==0, NA,  pri                      / efec)),
    v94 =  with(v94m00, ifelse(efec==0, NA,  pri                      / efec)),
    v97 =  with(v97m00, ifelse(efec==0, NA,  pri                      / efec)),
    v00 =  with(v00m,   ifelse(efec==0, NA,  pri                      / efec)),
    v03 =  with(v03m00, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v06 =  with(v06m00, ifelse(efec==0, NA,  pric                     / efec)),
    v09 =  with(v09m00, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v12 =  with(v12m00, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v15 =  with(v15m00, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v18 =  with(v18m00, ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)), # drop pvem + pna?
    v21 =  with(v21m00, ifelse(efec==0, NA,  pri                      / efec))  # coal vote to pan+prd ok?
)
prim97 <- data.frame(
    v91 =  with(v91mXX, ifelse(efec==0, NA,  pri                      / efec)),
    v94 =  with(v94m97, ifelse(efec==0, NA,  pri                      / efec)),
    v97 =  with(v97m,   ifelse(efec==0, NA,  pri                      / efec)),
    v00 =  with(v00m97, ifelse(efec==0, NA,  pri                      / efec)),
    v03 =  with(v03m97, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v06 =  with(v06m97, ifelse(efec==0, NA,  pric                     / efec)),
    v09 =  with(v09m97, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v12 =  with(v12m97, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v15 =  with(v15m97, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v18 =  with(v18m97, ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)), # drop pvem + pna?
    v21 =  with(v21m97, ifelse(efec==0, NA,  pri                      / efec))  # coal vote to pan+prd ok?
)
prim94 <- data.frame(
    v91 =  with(v91m94, ifelse(efec==0, NA,  pri                      / efec)),
    v94 =  with(v94m,   ifelse(efec==0, NA,  pri                      / efec)),
    v97 =  with(v97m94, ifelse(efec==0, NA,  pri                      / efec)),
    v00 =  with(v00m94, ifelse(efec==0, NA,  pri                      / efec)),
    v03 =  with(v03m94, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v06 =  with(v06m94, ifelse(efec==0, NA,  pric                     / efec)),
    v09 =  with(v09m94, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v12 =  with(v12m94, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v15 =  with(v15m94, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v18 =  with(v18m94, ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)), # drop pvem + pna?
    v21 =  with(v21m94, ifelse(efec==0, NA,  pri                      / efec))  # coal vote to pan+prd ok?
)
## prim91 <- data.frame(
##     v91 =  with(v91m,   ifelse(efec==0, NA,  pri                      / efec)),
##     v94 =  with(v94m91, ifelse(efec==0, NA,  pri                      / efec)),
##     v97 =  with(v97m91, ifelse(efec==0, NA,  pri                      / efec)),
##     v00 =  with(v00m91, ifelse(efec==0, NA,  pri                      / efec)),
##     v03 =  with(v03m91, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
##     v06 =  with(v06m91, ifelse(efec==0, NA,  pric                     / efec)),
##     v09 =  with(v09m91, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
##     v12 =  with(v12m91, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
##     v15 =  with(v15m91, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
##     v18 =  with(v18m91, ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)), # drop pvem + pna?
##     v21 =  with(v21m91, ifelse(efec==0, NA,  pri                      / efec))  # coal vote to pan+prd ok?
## )
#
prim21 <- round(prim21, 3)
prim18 <- round(prim18, 3)
prim15 <- round(prim15, 3)
prim12 <- round(prim12, 3)
prim09 <- round(prim09, 3)
prim06 <- round(prim06, 3)
prim03 <- round(prim03, 3)
prim00 <- round(prim00, 3)
prim97 <- round(prim97, 3)
prim94 <- round(prim94, 3)
## prim91 <- round(prim91, 3)
#
leftm21 <- data.frame(
    v91 = with(v91mXX, ifelse(efec==0, NA,  prd                             / efec)),
    v94 = with(v94m21, ifelse(efec==0, NA,  prd                             / efec)),
    v97 = with(v97m21, ifelse(efec==0, NA,  prd                             / efec)),
    v00 = with(v00m21, ifelse(efec==0, NA,  prdc                            / efec)),
    v03 = with(v03m21, ifelse(efec==0, NA, (prd + pt + conve)               / efec)),
    v06 = with(v06m21, ifelse(efec==0, NA,  prdc                            / efec)),
    v09 = with(v09m21, ifelse(efec==0, NA, (prd + pt + ptc + conve)         / efec)),
    v12 = with(v12m21, ifelse(efec==0, NA, (prd + prdc + pt + mc)           / efec)),
    v15 = with(v15m21, ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
    v18 = with(v18m21, ifelse(efec==0, NA, (morena + morenac + pt + pes)    / efec)),
    v21 = with(v21m,   ifelse(efec==0, NA, (morena + morenac + pt + pvem)   / efec))  # drop pt + pvem?
)
leftm18 <- data.frame(
    v91 = with(v91mXX, ifelse(efec==0, NA,  prd                             / efec)),
    v94 = with(v94m18, ifelse(efec==0, NA,  prd                             / efec)),
    v97 = with(v97m18, ifelse(efec==0, NA,  prd                             / efec)),
    v00 = with(v00m18, ifelse(efec==0, NA,  prdc                            / efec)),
    v03 = with(v03m18, ifelse(efec==0, NA, (prd + pt + conve)               / efec)),
    v06 = with(v06m18, ifelse(efec==0, NA,  prdc                            / efec)),
    v09 = with(v09m18, ifelse(efec==0, NA, (prd + pt + ptc + conve)         / efec)),
    v12 = with(v12m18, ifelse(efec==0, NA, (prd + prdc + pt + mc)           / efec)),
    v15 = with(v15m18, ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
    v18 = with(v18m,   ifelse(efec==0, NA, (morena + morenac + pt + pes)    / efec)),
    v21 = with(v21m18, ifelse(efec==0, NA, (morena + morenac + pt + pvem)   / efec))  # drop pt + pvem?
)
leftm15 <- data.frame(
    v91 = with(v91mXX, ifelse(efec==0, NA,  prd                             / efec)),
    v94 = with(v94m15, ifelse(efec==0, NA,  prd                             / efec)),
    v97 = with(v97m15, ifelse(efec==0, NA,  prd                             / efec)),
    v00 = with(v00m15, ifelse(efec==0, NA,  prdc                            / efec)),
    v03 = with(v03m15, ifelse(efec==0, NA, (prd + pt + conve)               / efec)),
    v06 = with(v06m15, ifelse(efec==0, NA,  prdc                            / efec)),
    v09 = with(v09m15, ifelse(efec==0, NA, (prd + pt + ptc + conve)         / efec)),
    v12 = with(v12m15, ifelse(efec==0, NA, (prd + prdc + pt + mc)           / efec)),
    v15 = with(v15m,   ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
    v18 = with(v18m15, ifelse(efec==0, NA, (morena + morenac + pt + pes)    / efec)),
    v21 = with(v21m15, ifelse(efec==0, NA, (morena + morenac + pt + pvem)   / efec))  # drop pt + pvem?
)
leftm12 <- data.frame(
    v91 = with(v91mXX, ifelse(efec==0, NA,  prd                             / efec)),
    v94 = with(v94m12, ifelse(efec==0, NA,  prd                             / efec)),
    v97 = with(v97m12, ifelse(efec==0, NA,  prd                             / efec)),
    v00 = with(v00m12, ifelse(efec==0, NA,  prdc                            / efec)),
    v03 = with(v03m12, ifelse(efec==0, NA, (prd + pt + conve)               / efec)),
    v06 = with(v06m12, ifelse(efec==0, NA,  prdc                            / efec)),
    v09 = with(v09m12, ifelse(efec==0, NA, (prd + pt + ptc + conve)         / efec)),
    v12 = with(v12m,   ifelse(efec==0, NA, (prd + prdc + pt + mc)           / efec)),
    v15 = with(v15m12, ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
    v18 = with(v18m12, ifelse(efec==0, NA, (morena + morenac + pt + pes)    / efec)),
    v21 = with(v21m12, ifelse(efec==0, NA, (morena + morenac + pt + pvem)   / efec))  # drop pt + pvem?
)
leftm09 <- data.frame(
    v91 = with(v91mXX, ifelse(efec==0, NA,  prd                             / efec)),
    v94 = with(v94m09, ifelse(efec==0, NA,  prd                             / efec)),
    v97 = with(v97m09, ifelse(efec==0, NA,  prd                             / efec)),
    v00 = with(v00m09, ifelse(efec==0, NA,  prdc                            / efec)),
    v03 = with(v03m09, ifelse(efec==0, NA, (prd + pt + conve)               / efec)),
    v06 = with(v06m09, ifelse(efec==0, NA,  prdc                            / efec)),
    v09 = with(v09m,   ifelse(efec==0, NA, (prd + pt + ptc + conve)         / efec)),
    v12 = with(v12m09, ifelse(efec==0, NA, (prd + prdc + pt + mc)           / efec)),
    v15 = with(v15m09, ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
    v18 = with(v18m09, ifelse(efec==0, NA, (morena + morenac + pt + pes)    / efec)),
    v21 = with(v21m09, ifelse(efec==0, NA, (morena + morenac + pt + pvem)   / efec))  # drop pt + pvem?
)
leftm06 <- data.frame(
    v91 = with(v91mXX, ifelse(efec==0, NA,  prd                             / efec)),
    v94 = with(v94m06, ifelse(efec==0, NA,  prd                             / efec)),
    v97 = with(v97m06, ifelse(efec==0, NA,  prd                             / efec)),
    v00 = with(v00m06, ifelse(efec==0, NA,  prdc                            / efec)),
    v03 = with(v03m06, ifelse(efec==0, NA, (prd + pt + conve)               / efec)),
    v06 = with(v06m,   ifelse(efec==0, NA,  prdc                            / efec)),
    v09 = with(v09m06, ifelse(efec==0, NA, (prd + pt + ptc + conve)         / efec)),
    v12 = with(v12m06, ifelse(efec==0, NA, (prd + prdc + pt + mc)           / efec)),
    v15 = with(v15m06, ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
    v18 = with(v18m06, ifelse(efec==0, NA, (morena + morenac + pt + pes)    / efec)),
    v21 = with(v21m06, ifelse(efec==0, NA, (morena + morenac + pt + pvem)   / efec))  # drop pt + pvem?
)
leftm03 <- data.frame(
    v91 = with(v91mXX, ifelse(efec==0, NA,  prd                             / efec)),
    v94 = with(v94m03, ifelse(efec==0, NA,  prd                             / efec)),
    v97 = with(v97m03, ifelse(efec==0, NA,  prd                             / efec)),
    v00 = with(v00m03, ifelse(efec==0, NA,  prdc                            / efec)),
    v03 = with(v03m,   ifelse(efec==0, NA, (prd + pt + conve)               / efec)),
    v06 = with(v06m03, ifelse(efec==0, NA,  prdc                            / efec)),
    v09 = with(v09m03, ifelse(efec==0, NA, (prd + pt + ptc + conve)         / efec)),
    v12 = with(v12m03, ifelse(efec==0, NA, (prd + prdc + pt + mc)           / efec)),
    v15 = with(v15m03, ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
    v18 = with(v18m03, ifelse(efec==0, NA, (morena + morenac + pt + pes)    / efec)),
    v21 = with(v21m03, ifelse(efec==0, NA, (morena + morenac + pt + pvem)   / efec))  # drop pt + pvem?
)
leftm00 <- data.frame(
    v91 = with(v91mXX, ifelse(efec==0, NA,  prd                             / efec)),
    v94 = with(v94m00, ifelse(efec==0, NA,  prd                             / efec)),
    v97 = with(v97m00, ifelse(efec==0, NA,  prd                             / efec)),
    v00 = with(v00m,   ifelse(efec==0, NA,  prdc                            / efec)),
    v03 = with(v03m00, ifelse(efec==0, NA, (prd + pt + conve)               / efec)),
    v06 = with(v06m00, ifelse(efec==0, NA,  prdc                            / efec)),
    v09 = with(v09m00, ifelse(efec==0, NA, (prd + pt + ptc + conve)         / efec)),
    v12 = with(v12m00, ifelse(efec==0, NA, (prd + prdc + pt + mc)           / efec)),
    v15 = with(v15m00, ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
    v18 = with(v18m00, ifelse(efec==0, NA, (morena + morenac + pt + pes)    / efec)),
    v21 = with(v21m00, ifelse(efec==0, NA, (morena + morenac + pt + pvem)   / efec))  # drop pt + pvem?
)
leftm97 <- data.frame(
    v91 = with(v91mXX, ifelse(efec==0, NA,  prd                             / efec)),
    v94 = with(v94m97, ifelse(efec==0, NA,  prd                             / efec)),
    v97 = with(v97m,   ifelse(efec==0, NA,  prd                             / efec)),
    v00 = with(v00m97, ifelse(efec==0, NA,  prdc                            / efec)),
    v03 = with(v03m97, ifelse(efec==0, NA, (prd + pt + conve)               / efec)),
    v06 = with(v06m97, ifelse(efec==0, NA,  prdc                            / efec)),
    v09 = with(v09m97, ifelse(efec==0, NA, (prd + pt + ptc + conve)         / efec)),
    v12 = with(v12m97, ifelse(efec==0, NA, (prd + prdc + pt + mc)           / efec)),
    v15 = with(v15m97, ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
    v18 = with(v18m97, ifelse(efec==0, NA, (morena + morenac + pt + pes)    / efec)),
    v21 = with(v21m97, ifelse(efec==0, NA, (morena + morenac + pt + pvem)   / efec))  # drop pt + pvem?
)
leftm94 <- data.frame(
    v91 = with(v91m94, ifelse(efec==0, NA,  prd                             / efec)),
    v94 = with(v94m,   ifelse(efec==0, NA,  prd                             / efec)),
    v97 = with(v97m94, ifelse(efec==0, NA,  prd                             / efec)),
    v00 = with(v00m94, ifelse(efec==0, NA,  prdc                            / efec)),
    v03 = with(v03m94, ifelse(efec==0, NA, (prd + pt + conve)               / efec)),
    v06 = with(v06m94, ifelse(efec==0, NA,  prdc                            / efec)),
    v09 = with(v09m94, ifelse(efec==0, NA, (prd + pt + ptc + conve)         / efec)),
    v12 = with(v12m94, ifelse(efec==0, NA, (prd + prdc + pt + mc)           / efec)),
    v15 = with(v15m94, ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
    v18 = with(v18m94, ifelse(efec==0, NA, (morena + morenac + pt + pes)    / efec)),
    v21 = with(v21m94, ifelse(efec==0, NA, (morena + morenac + pt + pvem)   / efec))  # drop pt + pvem?
)
## leftm91 <- data.frame(
##     v91 = with(v91m,   ifelse(efec==0, NA,  prd                             / efec)),
##     v94 = with(v94m91, ifelse(efec==0, NA,  prd                             / efec)),
##     v97 = with(v97m91, ifelse(efec==0, NA,  prd                             / efec)),
##     v00 = with(v00m91, ifelse(efec==0, NA,  prdc                            / efec)),
##     v03 = with(v03m91, ifelse(efec==0, NA, (prd + pt + conve)               / efec)),
##     v06 = with(v06m91, ifelse(efec==0, NA,  prdc                            / efec)),
##     v09 = with(v09m91, ifelse(efec==0, NA, (prd + pt + ptc + conve)         / efec)),
##     v12 = with(v12m91, ifelse(efec==0, NA, (prd + prdc + pt + mc)           / efec)),
##     v15 = with(v15m91, ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
##     v18 = with(v18m91, ifelse(efec==0, NA, (morena + morenac + pt + pes)    / efec)),
##     v21 = with(v21m91, ifelse(efec==0, NA, (morena + morenac + pt + pvem)   / efec))  # drop pt + pvem?
## )
#
leftm21 <- round(leftm21, 3)
leftm18 <- round(leftm18, 3)
leftm15 <- round(leftm15, 3)
leftm12 <- round(leftm12, 3)
leftm09 <- round(leftm09, 3)
leftm06 <- round(leftm06, 3)
leftm03 <- round(leftm03, 3)
leftm00 <- round(leftm00, 3)
leftm97 <- round(leftm97, 3)
leftm94 <- round(leftm94, 3)
## leftm91 <- round(leftm91, 3)
#
othm21 <- data.frame(
    v91 =  with(v91mXX, ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt)     / efec)),
    v94 =  with(v94m21, ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
    v97 =  with(v97m21, ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm)               / efec)),
    v00 =  with(v00m21, ifelse(efec==0, NA, (pcd + parm + dsppn)                       / efec)),
    v03 =  with(v03m21, ifelse(efec==0, NA, (psn + pas + mp + plm + fc)                / efec)),
    v06 =  with(v06m21, ifelse(efec==0, NA, (pna + asdc)                               / efec)),
    v09 =  with(v09m21, ifelse(efec==0, NA, (pna + psd)                                / efec)),
    v12 =  with(v12m21, ifelse(efec==0, NA,  pna                                       / efec)),
    v15 =  with(v15m21, ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2)          / efec)),
    v18 =  with(v18m21, ifelse(efec==0, NA, (mc + indep1 + indep2)                     / efec)),
    v21 =  with(v21m,   ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep)             / efec))
)
othm18 <- data.frame(
    v91 =  with(v91mXX, ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt)     / efec)),
    v94 =  with(v94m18, ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
    v97 =  with(v97m18, ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm)               / efec)),
    v00 =  with(v00m18, ifelse(efec==0, NA, (pcd + parm + dsppn)                       / efec)),
    v03 =  with(v03m18, ifelse(efec==0, NA, (psn + pas + mp + plm + fc)                / efec)),
    v06 =  with(v06m18, ifelse(efec==0, NA, (pna + asdc)                               / efec)),
    v09 =  with(v09m18, ifelse(efec==0, NA, (pna + psd)                                / efec)),
    v12 =  with(v12m18, ifelse(efec==0, NA,  pna                                       / efec)),
    v15 =  with(v15m18, ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2)          / efec)),
    v18 =  with(v18m,   ifelse(efec==0, NA, (mc + indep1 + indep2)                     / efec)),
    v21 =  with(v21m18, ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep)             / efec))
)
othm15 <- data.frame(
    v91 =  with(v91mXX, ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt)     / efec)),
    v94 =  with(v94m15, ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
    v97 =  with(v97m15, ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm)               / efec)),
    v00 =  with(v00m15, ifelse(efec==0, NA, (pcd + parm + dsppn)                       / efec)),
    v03 =  with(v03m15, ifelse(efec==0, NA, (psn + pas + mp + plm + fc)                / efec)),
    v06 =  with(v06m15, ifelse(efec==0, NA, (pna + asdc)                               / efec)),
    v09 =  with(v09m15, ifelse(efec==0, NA, (pna + psd)                                / efec)),
    v12 =  with(v12m15, ifelse(efec==0, NA,  pna                                       / efec)),
    v15 =  with(v15m,   ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2)          / efec)),
    v18 =  with(v18m15, ifelse(efec==0, NA, (mc + indep1 + indep2)                     / efec)),
    v21 =  with(v21m15, ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep)             / efec))
)
othm12 <- data.frame(
    v91 =  with(v91mXX, ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt)     / efec)),
    v94 =  with(v94m12, ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
    v97 =  with(v97m12, ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm)               / efec)),
    v00 =  with(v00m12, ifelse(efec==0, NA, (pcd + parm + dsppn)                       / efec)),
    v03 =  with(v03m12, ifelse(efec==0, NA, (psn + pas + mp + plm + fc)                / efec)),
    v06 =  with(v06m12, ifelse(efec==0, NA, (pna + asdc)                               / efec)),
    v09 =  with(v09m12, ifelse(efec==0, NA, (pna + psd)                                / efec)),
    v12 =  with(v12m,   ifelse(efec==0, NA,  pna                                       / efec)),
    v15 =  with(v15m12, ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2)          / efec)),
    v18 =  with(v18m12, ifelse(efec==0, NA, (mc + indep1 + indep2)                     / efec)),
    v21 =  with(v21m12, ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep)             / efec))
)
othm09 <- data.frame(
    v91 =  with(v91mXX, ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt)     / efec)),
    v94 =  with(v94m09, ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
    v97 =  with(v97m09, ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm)               / efec)),
    v00 =  with(v00m09, ifelse(efec==0, NA, (pcd + parm + dsppn)                       / efec)),
    v03 =  with(v03m09, ifelse(efec==0, NA, (psn + pas + mp + plm + fc)                / efec)),
    v06 =  with(v06m09, ifelse(efec==0, NA, (pna + asdc)                               / efec)),
    v09 =  with(v09m,   ifelse(efec==0, NA, (pna + psd)                                / efec)),
    v12 =  with(v12m09, ifelse(efec==0, NA,  pna                                       / efec)),
    v15 =  with(v15m09, ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2)          / efec)),
    v18 =  with(v18m09, ifelse(efec==0, NA, (mc + indep1 + indep2)                     / efec)),
    v21 =  with(v21m09, ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep)             / efec))
)
othm06 <- data.frame(
    v91 =  with(v91mXX, ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt)     / efec)),
    v94 =  with(v94m06, ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
    v97 =  with(v97m06, ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm)               / efec)),
    v00 =  with(v00m06, ifelse(efec==0, NA, (pcd + parm + dsppn)                       / efec)),
    v03 =  with(v03m06, ifelse(efec==0, NA, (psn + pas + mp + plm + fc)                / efec)),
    v06 =  with(v06m,   ifelse(efec==0, NA, (pna + asdc)                               / efec)),
    v09 =  with(v09m06, ifelse(efec==0, NA, (pna + psd)                                / efec)),
    v12 =  with(v12m06, ifelse(efec==0, NA,  pna                                       / efec)),
    v15 =  with(v15m06, ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2)          / efec)),
    v18 =  with(v18m06, ifelse(efec==0, NA, (mc + indep1 + indep2)                     / efec)),
    v21 =  with(v21m06, ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep)             / efec))
)
othm03 <- data.frame(
    v91 =  with(v91mXX, ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt)     / efec)),
    v94 =  with(v94m03, ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
    v97 =  with(v97m03, ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm)               / efec)),
    v00 =  with(v00m03, ifelse(efec==0, NA, (pcd + parm + dsppn)                       / efec)),
    v03 =  with(v03m,   ifelse(efec==0, NA, (psn + pas + mp + plm + fc)                / efec)),
    v06 =  with(v06m03, ifelse(efec==0, NA, (pna + asdc)                               / efec)),
    v09 =  with(v09m03, ifelse(efec==0, NA, (pna + psd)                                / efec)),
    v12 =  with(v12m03, ifelse(efec==0, NA,  pna                                       / efec)),
    v15 =  with(v15m03, ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2)          / efec)),
    v18 =  with(v18m03, ifelse(efec==0, NA, (mc + indep1 + indep2)                     / efec)),
    v21 =  with(v21m03, ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep)             / efec))
)
othm00 <- data.frame(
    v91 =  with(v91mXX, ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt)     / efec)),
    v94 =  with(v94m00, ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
    v97 =  with(v97m00, ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm)               / efec)),
    v00 =  with(v00m,   ifelse(efec==0, NA, (pcd + parm + dsppn)                       / efec)),
    v03 =  with(v03m00, ifelse(efec==0, NA, (psn + pas + mp + plm + fc)                / efec)),
    v06 =  with(v06m00, ifelse(efec==0, NA, (pna + asdc)                               / efec)),
    v09 =  with(v09m00, ifelse(efec==0, NA, (pna + psd)                                / efec)),
    v12 =  with(v12m00, ifelse(efec==0, NA,  pna                                       / efec)),
    v15 =  with(v15m00, ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2)          / efec)),
    v18 =  with(v18m00, ifelse(efec==0, NA, (mc + indep1 + indep2)                     / efec)),
    v21 =  with(v21m00, ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep)             / efec))
)
othm97 <- data.frame(
    v91 =  with(v91mXX, ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt)     / efec)),
    v94 =  with(v94m97, ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
    v97 =  with(v97m,   ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm)               / efec)),
    v00 =  with(v00m97, ifelse(efec==0, NA, (pcd + parm + dsppn)                       / efec)),
    v03 =  with(v03m97, ifelse(efec==0, NA, (psn + pas + mp + plm + fc)                / efec)),
    v06 =  with(v06m97, ifelse(efec==0, NA, (pna + asdc)                               / efec)),
    v09 =  with(v09m97, ifelse(efec==0, NA, (pna + psd)                                / efec)),
    v12 =  with(v12m97, ifelse(efec==0, NA,  pna                                       / efec)),
    v15 =  with(v15m97, ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2)          / efec)),
    v18 =  with(v18m97, ifelse(efec==0, NA, (mc + indep1 + indep2)                     / efec)),
    v21 =  with(v21m97, ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep)             / efec))
)
othm94 <- data.frame(
    v91 =  with(v91m94, ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt)     / efec)),
    v94 =  with(v94m,   ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
    v97 =  with(v97m94, ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm)               / efec)),
    v00 =  with(v00m94, ifelse(efec==0, NA, (pcd + parm + dsppn)                       / efec)),
    v03 =  with(v03m94, ifelse(efec==0, NA, (psn + pas + mp + plm + fc)                / efec)),
    v06 =  with(v06m94, ifelse(efec==0, NA, (pna + asdc)                               / efec)),
    v09 =  with(v09m94, ifelse(efec==0, NA, (pna + psd)                                / efec)),
    v12 =  with(v12m94, ifelse(efec==0, NA,  pna                                       / efec)),
    v15 =  with(v15m94, ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2)          / efec)),
    v18 =  with(v18m94, ifelse(efec==0, NA, (mc + indep1 + indep2)                     / efec)),
    v21 =  with(v21m94, ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep)             / efec))
)
## othm91 <- data.frame(
##     v91 =  with(v91m,   ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt)     / efec)),
##     v94 =  with(v94m91, ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
##     v97 =  with(v97m91, ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm)               / efec)),
##     v00 =  with(v00m91, ifelse(efec==0, NA, (pcd + parm + dsppn)                       / efec)),
##     v03 =  with(v03m91, ifelse(efec==0, NA, (psn + pas + mp + plm + fc)                / efec)),
##     v06 =  with(v06m91, ifelse(efec==0, NA, (pna + asdc)                               / efec)),
##     v09 =  with(v09m91, ifelse(efec==0, NA, (pna + psd)                                / efec)),
##     v12 =  with(v12m91, ifelse(efec==0, NA,  pna                                       / efec)),
##     v15 =  with(v15m91, ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2)          / efec)),
##     v18 =  with(v18m91, ifelse(efec==0, NA, (mc + indep1 + indep2)                     / efec)),
##     v21 =  with(v21m91, ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep)             / efec))
## )
#
othm21 <- round(othm21, 3)
othm18 <- round(othm18, 3)
othm15 <- round(othm15, 3)
othm12 <- round(othm12, 3)
othm09 <- round(othm09, 3)
othm06 <- round(othm06, 3)
othm03 <- round(othm03, 3)
othm00 <- round(othm00, 3)
othm97 <- round(othm97, 3)
othm94 <- round(othm94, 3)
## othm91 <- round(othm91, 3)
#
efecm21 <- data.frame(
    v91 = v91mXX$efec,
    v94 = v94m21$efec,
    v97 = v97m21$efec,
    v00 = v00m21$efec,
    v03 = v03m21$efec,
    v06 = v06m21$efec,
    v09 = v09m21$efec,
    v12 = v12m21$efec,
    v15 = v15m21$efec,
    v18 = v18m21$efec,
    v21 = v21m  $efec
)
efecm18 <- data.frame(
    v91 = v91mXX$efec,
    v94 = v94m18$efec,
    v97 = v97m18$efec,
    v00 = v00m18$efec,
    v03 = v03m18$efec,
    v06 = v06m18$efec,
    v09 = v09m18$efec,
    v12 = v12m18$efec,
    v15 = v15m18$efec,
    v18 = v18m  $efec,
    v21 = v21m18$efec
)
efecm15 <- data.frame(
    v91 = v91mXX$efec,
    v94 = v94m15$efec,
    v97 = v97m15$efec,
    v00 = v00m15$efec,
    v03 = v03m15$efec,
    v06 = v06m15$efec,
    v09 = v09m15$efec,
    v12 = v12m15$efec,
    v15 = v15m  $efec,
    v18 = v18m15$efec,
    v21 = v21m15$efec
)
efecm12 <- data.frame(
    v91 = v91mXX$efec,
    v94 = v94m12$efec,
    v97 = v97m12$efec,
    v00 = v00m12$efec,
    v03 = v03m12$efec,
    v06 = v06m12$efec,
    v09 = v09m12$efec,
    v12 = v12m  $efec,
    v15 = v15m12$efec,
    v18 = v18m12$efec,
    v21 = v21m12$efec
)
efecm09 <- data.frame(
    v91 = v91mXX$efec,
    v94 = v94m09$efec,
    v97 = v97m09$efec,
    v00 = v00m09$efec,
    v03 = v03m09$efec,
    v06 = v06m09$efec,
    v09 = v09m  $efec,
    v12 = v12m09$efec,
    v15 = v15m09$efec,
    v18 = v18m09$efec,
    v21 = v21m09$efec
)
efecm06 <- data.frame(
    v91 = v91mXX$efec,
    v94 = v94m06$efec,
    v97 = v97m06$efec,
    v00 = v00m06$efec,
    v03 = v03m06$efec,
    v06 = v06m  $efec,
    v09 = v09m06$efec,
    v12 = v12m06$efec,
    v15 = v15m06$efec,
    v18 = v18m06$efec,
    v21 = v21m06$efec
)
efecm03 <- data.frame(
    v91 = v91mXX$efec,
    v94 = v94m03$efec,
    v97 = v97m03$efec,
    v00 = v00m03$efec,
    v03 = v03m  $efec,
    v06 = v06m03$efec,
    v09 = v09m03$efec,
    v12 = v12m03$efec,
    v15 = v15m03$efec,
    v18 = v18m03$efec,
    v21 = v21m03$efec
)
efecm00 <- data.frame(
    v91 = v91mXX$efec,
    v94 = v94m00$efec,
    v97 = v97m00$efec,
    v00 = v00m  $efec,
    v03 = v03m00$efec,
    v06 = v06m00$efec,
    v09 = v09m00$efec,
    v12 = v12m00$efec,
    v15 = v15m00$efec,
    v18 = v18m00$efec,
    v21 = v21m00$efec
)
efecm97 <- data.frame(
    v91 = v91mXX$efec,
    v94 = v94m97$efec,
    v97 = v97m  $efec,
    v00 = v00m97$efec,
    v03 = v03m97$efec,
    v06 = v06m97$efec,
    v09 = v09m97$efec,
    v12 = v12m97$efec,
    v15 = v15m97$efec,
    v18 = v18m97$efec,
    v21 = v21m97$efec
)
efecm94 <- data.frame(
    v91 = v91m94$efec,
    v94 = v94m  $efec,
    v97 = v97m94$efec,
    v00 = v00m94$efec,
    v03 = v03m94$efec,
    v06 = v06m94$efec,
    v09 = v09m94$efec,
    v12 = v12m94$efec,
    v15 = v15m94$efec,
    v18 = v18m94$efec,
    v21 = v21m94$efec
)
## efecm91 <- data.frame(
##     v91 = v91m  $efec,
##     v94 = v94m91$efec,
##     v97 = v97m91$efec,
##     v00 = v00m91$efec,
##     v03 = v03m91$efec,
##     v06 = v06m91$efec,
##     v09 = v09m91$efec,
##     v12 = v12m91$efec,
##     v15 = v15m91$efec,
##     v18 = v18m91$efec,
##     v21 = v21m91$efec
## )
#
lisnomm21 <- data.frame(
    v91 = v91mXX$lisnom,
    v94 = v94m21$lisnom,
    v97 = v97m21$lisnom,
    v00 = v00m21$lisnom,
    v03 = v03m21$lisnom,
    v06 = v06m21$lisnom,
    v09 = v09m21$lisnom,
    v12 = v12m21$lisnom,
    v15 = v15m21$lisnom,
    v18 = v18m21$lisnom,
    v21 = v21m  $lisnom
)
lisnomm18 <- data.frame(
    v91 = v91mXX$lisnom,
    v94 = v94m18$lisnom,
    v97 = v97m18$lisnom,
    v00 = v00m18$lisnom,
    v03 = v03m18$lisnom,
    v06 = v06m18$lisnom,
    v09 = v09m18$lisnom,
    v12 = v12m18$lisnom,
    v15 = v15m18$lisnom,
    v18 = v18m  $lisnom,
    v21 = v21m18$lisnom
)
lisnomm15 <- data.frame(
    v91 = v91mXX$lisnom,
    v94 = v94m15$lisnom,
    v97 = v97m15$lisnom,
    v00 = v00m15$lisnom,
    v03 = v03m15$lisnom,
    v06 = v06m15$lisnom,
    v09 = v09m15$lisnom,
    v12 = v12m15$lisnom,
    v15 = v15m  $lisnom,
    v18 = v18m15$lisnom,
    v21 = v21m15$lisnom
)
lisnomm12 <- data.frame(
    v91 = v91mXX$lisnom,
    v94 = v94m12$lisnom,
    v97 = v97m12$lisnom,
    v00 = v00m12$lisnom,
    v03 = v03m12$lisnom,
    v06 = v06m12$lisnom,
    v09 = v09m12$lisnom,
    v12 = v12m  $lisnom,
    v15 = v15m12$lisnom,
    v18 = v18m12$lisnom,
    v21 = v21m12$lisnom
)
lisnomm09 <- data.frame(
    v91 = v91mXX$lisnom,
    v94 = v94m09$lisnom,
    v97 = v97m09$lisnom,
    v00 = v00m09$lisnom,
    v03 = v03m09$lisnom,
    v06 = v06m09$lisnom,
    v09 = v09m  $lisnom,
    v12 = v12m09$lisnom,
    v15 = v15m09$lisnom,
    v18 = v18m09$lisnom,
    v21 = v21m09$lisnom
)
lisnomm06 <- data.frame(
    v91 = v91mXX$lisnom,
    v94 = v94m06$lisnom,
    v97 = v97m06$lisnom,
    v00 = v00m06$lisnom,
    v03 = v03m06$lisnom,
    v06 = v06m  $lisnom,
    v09 = v09m06$lisnom,
    v12 = v12m06$lisnom,
    v15 = v15m06$lisnom,
    v18 = v18m06$lisnom,
    v21 = v21m06$lisnom
)
lisnomm03 <- data.frame(
    v91 = v91mXX$lisnom,
    v94 = v94m03$lisnom,
    v97 = v97m03$lisnom,
    v00 = v00m03$lisnom,
    v03 = v03m  $lisnom,
    v06 = v06m03$lisnom,
    v09 = v09m03$lisnom,
    v12 = v12m03$lisnom,
    v15 = v15m03$lisnom,
    v18 = v18m03$lisnom,
    v21 = v21m03$lisnom
)
lisnomm00 <- data.frame(
    v91 = v91mXX$lisnom,
    v94 = v94m00$lisnom,
    v97 = v97m00$lisnom,
    v00 = v00m  $lisnom,
    v03 = v03m00$lisnom,
    v06 = v06m00$lisnom,
    v09 = v09m00$lisnom,
    v12 = v12m00$lisnom,
    v15 = v15m00$lisnom,
    v18 = v18m00$lisnom,
    v21 = v21m00$lisnom
)
lisnomm97 <- data.frame(
    v91 = v91mXX$lisnom,
    v94 = v94m97$lisnom,
    v97 = v97m  $lisnom,
    v00 = v00m97$lisnom,
    v03 = v03m97$lisnom,
    v06 = v06m97$lisnom,
    v09 = v09m97$lisnom,
    v12 = v12m97$lisnom,
    v15 = v15m97$lisnom,
    v18 = v18m97$lisnom,
    v21 = v21m97$lisnom
)
lisnomm94 <- data.frame(
    v91 = v91m94$lisnom,
    v94 = v94m  $lisnom,
    v97 = v97m94$lisnom,
    v00 = v00m94$lisnom,
    v03 = v03m94$lisnom,
    v06 = v06m94$lisnom,
    v09 = v09m94$lisnom,
    v12 = v12m94$lisnom,
    v15 = v15m94$lisnom,
    v18 = v18m94$lisnom,
    v21 = v21m94$lisnom
)
## lisnomm91 <- data.frame(
##     v91 = v91m  $lisnom,
##     v94 = v94m91$lisnom,
##     v97 = v97m91$lisnom,
##     v00 = v00m91$lisnom,
##     v03 = v03m91$lisnom,
##     v06 = v06m91$lisnom,
##     v09 = v09m91$lisnom,
##     v12 = v12m91$lisnom,
##     v15 = v15m91$lisnom,
##     v18 = v18m91$lisnom,
##     v21 = v21m91$lisnom
## )
#
# transpose to plug columns (units) into new data.frames
panm21    <- t(panm21)
panm18    <- t(panm18)
panm15    <- t(panm15)
panm12    <- t(panm12)
panm09    <- t(panm09)
panm06    <- t(panm06)
panm03    <- t(panm03)
panm00    <- t(panm00)
panm97    <- t(panm97)
panm94    <- t(panm94)
## panm91    <- t(panm91)
#
prim21    <- t(prim21)
prim18    <- t(prim18)
prim15    <- t(prim15)
prim12    <- t(prim12)
prim09    <- t(prim09)
prim06    <- t(prim06)
prim03    <- t(prim03)
prim00    <- t(prim00)
prim97    <- t(prim97)
prim94    <- t(prim94)
## prim91    <- t(prim91)
#
leftm21    <- t(leftm21)
leftm18    <- t(leftm18)
leftm15    <- t(leftm15)
leftm12    <- t(leftm12)
leftm09    <- t(leftm09)
leftm06    <- t(leftm06)
leftm03    <- t(leftm03)
leftm00    <- t(leftm00)
leftm97    <- t(leftm97)
leftm94    <- t(leftm94)
## leftm91    <- t(leftm91)
#
othm21    <- t(othm21)
othm18    <- t(othm18)
othm15    <- t(othm15)
othm12    <- t(othm12)
othm09    <- t(othm09)
othm06    <- t(othm06)
othm03    <- t(othm03)
othm00    <- t(othm00)
othm97    <- t(othm97)
othm94    <- t(othm94)
## othm91    <- t(othm91)
#
efecm21    <- t(efecm21)
efecm18    <- t(efecm18)
efecm15    <- t(efecm15)
efecm12    <- t(efecm12)
efecm09    <- t(efecm09)
efecm06    <- t(efecm06)
efecm03    <- t(efecm03)
efecm00    <- t(efecm00)
efecm97    <- t(efecm97)
efecm94    <- t(efecm94)
## efecm91    <- t(efecm91)
#
lisnomm21    <- t(lisnomm21)
lisnomm18    <- t(lisnomm18)
lisnomm15    <- t(lisnomm15)
lisnomm12    <- t(lisnomm12)
lisnomm09    <- t(lisnomm09)
lisnomm06    <- t(lisnomm06)
lisnomm03    <- t(lisnomm03)
lisnomm00    <- t(lisnomm00)
lisnomm97    <- t(lisnomm97)
lisnomm94    <- t(lisnomm94)
## lisnomm91    <- t(lisnomm91)
#
rm(v91mXX)

#################################################################
## extendCoal.. will receive data for regressions, one per map ##
#################################################################
tmp <-     as.list(rep(NA, nmun)) # empty list will receive one data.frame per unit
names(tmp) <- v00m$ife
#
extendCoalm21 <- extendCoalm18 <- 
extendCoalm15 <- extendCoalm12 <- extendCoalm09 <- extendCoalm06 <- 
extendCoalm03 <- extendCoalm00 <- extendCoalm97 <- 
extendCoalm94 <-
##extendCoalm91 <- 
    tmp
rm(tmp)


## loop over municipios
for (i in 1:nmun){
    ##i <- 81 # debug
    message(sprintf("loop %s of %s", i, nmun))
    ## ######################
    ## votes with 2021 map ##
    ## ######################
    tmp <- data.frame(yr     = seq(from=1991, to=2021, by=3),
                      pan    = panm21   [,i] ,
                      pri    = prim21   [,i] ,
                      left   = leftm21  [,i] ,
                      oth    = othm21   [,i] ,
                      efec   = efecm21  [,i] ,
                      lisnom = lisnomm21[,i] )
    ## replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan   [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri   [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left  [is.na(tmp$left)] <- per.means["left"];
        tmp$oth   [is.na(tmp$oth)]  <- per.means["oth"];
        tmp$efec  [is.na(tmp$efec)   | tmp$efec==0]   <- 1
        tmp$lisnom[is.na(tmp$lisnom) | tmp$lisnom==0] <- 2
    }
    ## add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    ## re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    ## add id
    tmp$ife    <- v00m$ife[i]
    ## fill info to new list
    extendCoalm21[[i]] <- tmp
    ## name list object
    names(extendCoalm21)[i] <- tmp$ife[1]
    ##
    ## ######################
    ## votes with 2018 map ##
    ## ######################
    tmp <- data.frame(yr     = seq(from=1991, to=2021, by=3),
                      pan    = panm18[,i],
                      pri    = prim18[,i],
                      left   = leftm18[,i],
                      oth    = othm18[,i],
                      efec   = efecm18[,i],
                      lisnom = lisnomm18[,i])
    ## replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan   [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri   [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left  [is.na(tmp$left)] <- per.means["left"];
        tmp$oth   [is.na(tmp$oth)]  <- per.means["oth"];
        tmp$efec  [is.na(tmp$efec)   | tmp$efec==0]   <- 1
        tmp$lisnom[is.na(tmp$lisnom) | tmp$lisnom==0] <- 2
    }
    ## add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    ## re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    ## add id
    tmp$ife    <- v00m$ife[i]
    ## fill info to new list
    extendCoalm18[[i]] <- tmp
    ## name list object
    names(extendCoalm18)[i] <- tmp$ife[1]
    ##
    ## ######################
    ## votes with 2015 map ##
    ## ######################
    tmp <- data.frame(yr     = seq(from=1991, to=2021, by=3),
                      pan    = panm15[,i],
                      pri    = prim15[,i],
                      left   = leftm15[,i],
                      oth    = othm15[,i],
                      efec   = efecm15[,i],
                      lisnom = lisnomm15[,i])
    ## replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan   [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri   [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left  [is.na(tmp$left)] <- per.means["left"];
        tmp$oth   [is.na(tmp$oth)]  <- per.means["oth"];
        tmp$efec  [is.na(tmp$efec)   | tmp$efec==0]   <- 1
        tmp$lisnom[is.na(tmp$lisnom) | tmp$lisnom==0] <- 2
    }
    ## add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    ## re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    ## add id
    tmp$ife    <- v00m$ife[i]
    ## fill info to new list
    extendCoalm15[[i]] <- tmp
    ## name list object
    names(extendCoalm15)[i] <- tmp$ife[1]
    ##
    ## ######################
    ## votes with 2012 map ##
    ## ######################
    tmp <- data.frame(yr     = seq(from=1991, to=2021, by=3),
                      pan    = panm12[,i],
                      pri    = prim12[,i],
                      left   = leftm12[,i],
                      oth    = othm12[,i],
                      efec   = efecm12[,i],
                      lisnom = lisnomm12[,i])
    ## replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan   [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri   [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left  [is.na(tmp$left)] <- per.means["left"];
        tmp$oth   [is.na(tmp$oth)]  <- per.means["oth"];
        tmp$efec  [is.na(tmp$efec)   | tmp$efec==0]   <- 1
        tmp$lisnom[is.na(tmp$lisnom) | tmp$lisnom==0] <- 2
    }
    ## add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    ## re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    ## add id
    tmp$ife    <- v00m$ife[i]
    ## fill info to new list
    extendCoalm12[[i]] <- tmp
    ## name list object
    names(extendCoalm12)[i] <- tmp$ife[1]
    ##
    ## ######################
    ## votes with 2009 map ##
    ## ######################
    tmp <- data.frame(yr     = seq(from=1991, to=2021, by=3),
                      pan    = panm09[,i],
                      pri    = prim09[,i],
                      left   = leftm09[,i],
                      oth    = othm09[,i],
                      efec   = efecm09[,i],
                      lisnom = lisnomm09[,i])
    ## replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan   [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri   [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left  [is.na(tmp$left)] <- per.means["left"];
        tmp$oth   [is.na(tmp$oth)]  <- per.means["oth"];
        tmp$efec  [is.na(tmp$efec)   | tmp$efec==0]   <- 1
        tmp$lisnom[is.na(tmp$lisnom) | tmp$lisnom==0] <- 2
    }
    ## add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    ## re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    ## add id
    tmp$ife    <- v00m$ife[i]
    ## fill info to new list
    extendCoalm09[[i]] <- tmp
    ## name list object
    names(extendCoalm09)[i] <- tmp$ife[1]
    ##
    ## ######################
    ## votes with 2006 map ##
    ## ######################
    tmp <- data.frame(yr     = seq(from=1991, to=2021, by=3),
                      pan    = panm06[,i],
                      pri    = prim06[,i],
                      left   = leftm06[,i],
                      oth    = othm06[,i],
                      efec   = efecm06[,i],
                      lisnom = lisnomm06[,i])
    ## replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan   [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri   [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left  [is.na(tmp$left)] <- per.means["left"];
        tmp$oth   [is.na(tmp$oth)]  <- per.means["oth"];
        tmp$efec  [is.na(tmp$efec)   | tmp$efec==0]   <- 1
        tmp$lisnom[is.na(tmp$lisnom) | tmp$lisnom==0] <- 2
    }
    ## add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    ## re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    ## add id
    tmp$ife    <- v00m$ife[i]
    ## fill info to new list
    extendCoalm06[[i]] <- tmp
    ## name list object
    names(extendCoalm06)[i] <- tmp$ife[1]
    ##
    ## ######################
    ## votes with 2003 map ##
    ## ######################
    tmp <- data.frame(yr     = seq(from=1991, to=2021, by=3),
                      pan    = panm03[,i],
                      pri    = prim03[,i],
                      left   = leftm03[,i],
                      oth    = othm03[,i],
                      efec   = efecm03[,i],
                      lisnom = lisnomm03[,i])
    ## replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan   [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri   [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left  [is.na(tmp$left)] <- per.means["left"];
        tmp$oth   [is.na(tmp$oth)]  <- per.means["oth"];
        tmp$efec  [is.na(tmp$efec)   | tmp$efec==0]   <- 1
        tmp$lisnom[is.na(tmp$lisnom) | tmp$lisnom==0] <- 2
    }
    ## add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    ## re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    ## add id
    tmp$ife    <- v00m$ife[i]
    ## fill info to new list
    extendCoalm03[[i]] <- tmp
    ## name list object
    names(extendCoalm03)[i] <- tmp$ife[1]
    ##
    ## ######################
    ## votes with 2000 map ##
    ## ######################
    tmp <- data.frame(yr     = seq(from=1991, to=2021, by=3),
                      pan    = panm00[,i],
                      pri    = prim00[,i],
                      left   = leftm00[,i],
                      oth    = othm00[,i],
                      efec   = efecm00[,i],
                      lisnom = lisnomm00[,i])
    ## replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan   [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri   [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left  [is.na(tmp$left)] <- per.means["left"];
        tmp$oth   [is.na(tmp$oth)]  <- per.means["oth"];
        tmp$efec  [is.na(tmp$efec)   | tmp$efec==0]   <- 1
        tmp$lisnom[is.na(tmp$lisnom) | tmp$lisnom==0] <- 2
    }
    ## add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    ## re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    ## add id
    tmp$ife    <- v00m$ife[i]
    ## fill info to new list
    extendCoalm00[[i]] <- tmp
    ## name list object
    names(extendCoalm00)[i] <- tmp$ife[1]
    ##
    ## ######################
    ## votes with 1997 map ##
    ## ######################
    tmp <- data.frame(yr     = seq(from=1991, to=2021, by=3),
                      pan    = panm97[,i],
                      pri    = prim97[,i],
                      left   = leftm97[,i],
                      oth    = othm97[,i],
                      efec   = efecm97[,i],
                      lisnom = lisnomm97[,i])
    ## replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan   [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri   [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left  [is.na(tmp$left)] <- per.means["left"];
        tmp$oth   [is.na(tmp$oth)]  <- per.means["oth"];
        tmp$efec  [is.na(tmp$efec)   | tmp$efec==0]   <- 1
        tmp$lisnom[is.na(tmp$lisnom) | tmp$lisnom==0] <- 2
    }
    ## add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    ## re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    ## add id
    tmp$ife    <- v00m$ife[i]
    ## fill info to new list
    extendCoalm97[[i]] <- tmp
    ## name list object
    names(extendCoalm97)[i] <- tmp$ife[1]
    ##
    ## ######################
    ## votes with 1994 map ##
    ## ######################
    ##tmp <- data.frame(yr     = seq(from=1991, to=2021, by=3),
    tmp <- data.frame(yr     = seq(from=1991, to=2021, by=3),
                      pan    = panm94[,i],
                      pri    = prim94[,i],
                      left   = leftm94[,i],
                      oth    = othm94[,i],
                      efec   = efecm94[,i],
                      lisnom = lisnomm94[,i])
    ## replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan   [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri   [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left  [is.na(tmp$left)] <- per.means["left"];
        tmp$oth   [is.na(tmp$oth)]  <- per.means["oth"];
        tmp$efec  [is.na(tmp$efec)   | tmp$efec==0]   <- 1
        tmp$lisnom[is.na(tmp$lisnom) | tmp$lisnom==0] <- 2
    }
    ## add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    ## re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    ## add id
    tmp$ife    <- v00m$ife[i]
    ## fill info to new list
    extendCoalm94[[i]] <- tmp
    ## name list object
    names(extendCoalm94)[i] <- tmp$ife[1]
    ##
    ## #########################
    ## ## votes with map 1991 ##
    ## #########################
    ## tmp <- data.frame(yr     = seq(from=1991, to=2021, by=3),
    ##                   pan    = panm91[,i],
    ##                   pri    = prim91[,i],
    ##                   left   = leftm91[,i],
    ##                   oth    = othm91[,i],
    ##                   efec   = efecm91[,i],
    ##                   lisnom = lisnomm91[,i])
    ## # replace NAs with period's mean
    ## if (length(tmp[is.na(tmp)])>0){
    ##     per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
    ##     tmp$pan   [is.na(tmp$pan)]  <- per.means["pan"];
    ##     tmp$pri   [is.na(tmp$pri)]  <- per.means["pri"];
    ##     tmp$left  [is.na(tmp$left)] <- per.means["left"];
    ##     tmp$oth   [is.na(tmp$oth)]  <- per.means["oth"];
    ##     tmp$efec  [is.na(tmp$efec)   | tmp$efec==0]   <- 1
    ##     tmp$lisnom[is.na(tmp$lisnom) | tmp$lisnom==0] <- 2
    ## }
    ## # add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    ## if (length(tmp[tmp==0])>0){
    ##     tmp[tmp==0] <- 0.001;
    ## }
    ## # re-compute shares to add to 1
    ## tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    ## # add id
    ## tmp$ife    <- v00m$ife[i]
    ## # fill info to new list
    ## extendCoalm91[[i]] <- tmp
    ## # name list object
    ## names(extendCoalm91)[i] <- tmp$ife[1]
}
## clean
##rm(panm91,prim91,leftm91,othm91,efecm91,lisnomm91)
rm(panm94,prim94,leftm94,othm94,efecm94,lisnomm94)
rm(panm97,prim97,leftm97,othm97,efecm97,lisnomm97)
rm(panm00,prim00,leftm00,othm00,efecm00,lisnomm00)
rm(panm03,prim03,leftm03,othm03,efecm03,lisnomm03)
rm(panm06,prim06,leftm06,othm06,efecm06,lisnomm06)
rm(panm09,prim09,leftm09,othm09,efecm09,lisnomm09)
rm(panm12,prim12,leftm12,othm12,efecm12,lisnomm12)
rm(panm15,prim15,leftm15,othm15,efecm15,lisnomm15)
rm(panm18,prim18,leftm18,othm18,efecm18,lisnomm18)
rm(panm21,prim21,leftm21,othm21,efecm21,lisnomm21)
    
##################################
## datos para regresión de alfa ##
##################################
##
#############################################################################################################
## Nota 16jul2021: al añadir datos 2021 cambiarán todas las alfas y betas! Una solución fácil (que usaré)  ##
## y otra que requiere más coco. La fácil es reestimar con 2021 e identificar el commit que reportaba la   ##
## versión hasta 2018. La otra: definir una ventana temporal (como cinco elecciones) para producir alfas   ##
## y betas cada elección: alfa.2006, alfa.2009, etc. Debería poder investigar cómo usan esto en el Capital ##
## Asset Pricing Model...                                                                                  ##
#############################################################################################################
yr.means <- data.frame(yr = seq(1991,2021,3), # 11 election-years
                       pan    = rep(NA,11),
                       pri    = rep(NA,11),
                       left   = rep(NA,11),
                       oth    = rep(NA,11))
## function to sum numeric columns
cs <- function(x){
    sel.nums <- unlist(lapply(x, is.numeric), use.names = FALSE) # selects only numeric columns in data frame
    res <- colSums(x[,sel.nums], na.rm=TRUE)
    return(res)
}
#
## compute national mean vote
yr.means$pan [1] <-  cs(v91s)["pan"]  /  cs(v91s)["efec"]
yr.means$pri [1] <-  cs(v91s)["pri"]  /  cs(v91s)["efec"]
yr.means$left[1] <-  cs(v91s)["prd"]  /  cs(v91s)["efec"]
yr.means$oth [1] <-  with(yr.means[1,], 1 - pan - pri - left)
##
yr.means$pan [2] <-  cs(v94s)["pan"]  / cs(v94s)["efec"]
yr.means$pri [2] <-  cs(v94s)["pri"]  / cs(v94s)["efec"]
yr.means$left[2] <-  cs(v94s)["prd"]  / cs(v94s)["efec"]
yr.means$oth [2] <-  with(yr.means[2,], 1 - pan - pri - left)
##                
yr.means$pan [3] <-  cs(v97s)["pan"]  / cs(v97s)["efec"]
yr.means$pri [3] <-  cs(v97s)["pri"]  / cs(v97s)["efec"]
yr.means$left[3] <-  cs(v97s)["prd"]  / cs(v97s)["efec"]
yr.means$oth [3] <-  with(yr.means[3,], 1 - pan - pri - left)
##                
yr.means$pan [4] <-  cs(v00s)["panc"]  / cs(v00s)["efec"]
yr.means$pri [4] <-  cs(v00s)["pri"]   / cs(v00s)["efec"]
yr.means$left[4] <-  cs(v00s)["prdc"]  / cs(v00s)["efec"]
yr.means$oth [4] <-  with(yr.means[4,], 1 - pan - pri - left)
##                
yr.means$pan [5] <-  cs(v03s)["pan"]                                         / cs(v03s)["efec"]
yr.means$pri [5] <- (cs(v03s)["pri"] + cs(v03s)["pric"] + cs(v03s)["pvem"])  / cs(v03s)["efec"]
yr.means$left[5] <- (cs(v03s)["prd"] + cs(v03s)["pt"]   + cs(v03s)["conve"]) / cs(v03s)["efec"]
yr.means$oth [5] <-  with(yr.means[5,], 1 - pan - pri - left)
##                
yr.means$pan [6] <-  cs(v06s)["pan"]   / cs(v06s)["efec"]
yr.means$pri [6] <-  cs(v06s)["pric"]  / cs(v06s)["efec"]
yr.means$left[6] <-  cs(v06s)["prdc"]  / cs(v06s)["efec"]
yr.means$oth [6] <-  with(yr.means[6,], 1 - pan - pri - left)
##                
yr.means$pan [7] <-  cs(v09s)["pan"]                                                           / cs(v09s)["efec"]
yr.means$pri [7] <- (cs(v09s)["pri"] + cs(v09s)["pric"] + cs(v09s)["pvem"])                    / cs(v09s)["efec"]
yr.means$left[7] <- (cs(v09s)["prd"] + cs(v09s)["pt"]   + cs(v09s)["ptc"] + cs(v09s)["conve"]) / cs(v09s)["efec"]
yr.means$oth [7] <-  with(yr.means[7,], 1 - pan - pri - left)
##                
yr.means$pan [8] <-  cs(v12s)["pan"]                                                       / cs(v12s)["efec"]
yr.means$pri [8] <- (cs(v12s)["pri"] + cs(v12s)["pric"] + cs(v12s)["pvem"])                / cs(v12s)["efec"]
yr.means$left[8] <- (cs(v12s)["prd"] + cs(v12s)["prdc"] + cs(v12s)["pt"] + cs(v12s)["mc"]) / cs(v12s)["efec"]
yr.means$oth [8] <-  with(yr.means[8,], 1 - pan - pri - left)
##                
yr.means$pan [9] <-  cs(v15s)["pan"]                                                            / cs(v15s)["efec"]
yr.means$pri [9] <- (cs(v15s)["pri"] + cs(v15s)["pric"] + cs(v15s)["pvem"])                     / cs(v15s)["efec"]
yr.means$left[9] <- (cs(v15s)["prd"] + cs(v15s)["prdc"] + cs(v15s)["pt"] + cs(v15s)["morena"] ) / cs(v15s)["efec"] # dropped cs(v15s)["pes"]
yr.means$oth [9] <-  with(yr.means[9,], 1 - pan - pri - left)
##
yr.means$pan [10] <- (cs(v18s)["pan"]    + cs(v18s)["panc"]    + cs(v18s)["prd"]  + cs(v18s)["mc"])  / cs(v18s)["efec"]
yr.means$pri [10] <- (cs(v18s)["pri"]    + cs(v18s)["pric"]    + cs(v18s)["pvem"] + cs(v18s)["pna"]) / cs(v18s)["efec"]
yr.means$left[10] <- (cs(v18s)["morena"] + cs(v18s)["morenac"] + cs(v18s)["pt"]   + cs(v18s)["pes"]) / cs(v18s)["efec"]
yr.means$oth [10] <-  with(yr.means[10,], 1 - pan - pri - left)
##
yr.means$pan [11] <- (cs(v21s)["pan"]    + cs(v21s)["panc"]    + cs(v21s)["prd"])                    / cs(v21s)["efec"]
yr.means$pri [11] <-  cs(v21s)["pri"]                                                                / cs(v21s)["efec"] # dropped cs(v21s)["pric"]
yr.means$left[11] <- (cs(v21s)["morena"] + cs(v21s)["morenac"] + cs(v21s)["pt"]  + cs(v21s)["pvem"]) / cs(v21s)["efec"]
yr.means$oth [11] <-  with(yr.means[11,], 1 - pan - pri - left)
##
#############################
## votes relative to pri's ##
#############################
yr.means <- within(yr.means, mean.rpan    <- pan  / pri)
yr.means <- within(yr.means, mean.rleft   <- left / pri)
yr.means <- within(yr.means, mean.roth    <- oth  / pri)
##
yr.means[,2:8] <- round(yr.means[,2:8], 3)
##
## ## Drop 1991, not used now ##
## sel.r <- which(yr.means==1991)
## if (length(sel.r>0)) yr.means <- yr.means[-sel.r,]
##
## plug into data
for (i in 1:nmun){
    #i <- 2 # debug
    extendCoalm21[[i]] <- cbind(extendCoalm21[[i]], yr.means[,6:8])
    extendCoalm18[[i]] <- cbind(extendCoalm18[[i]], yr.means[,6:8])
    extendCoalm15[[i]] <- cbind(extendCoalm15[[i]], yr.means[,6:8])
    extendCoalm12[[i]] <- cbind(extendCoalm12[[i]], yr.means[,6:8])
    extendCoalm09[[i]] <- cbind(extendCoalm09[[i]], yr.means[,6:8])
    extendCoalm06[[i]] <- cbind(extendCoalm06[[i]], yr.means[,6:8])
    extendCoalm03[[i]] <- cbind(extendCoalm03[[i]], yr.means[,6:8])
    extendCoalm00[[i]] <- cbind(extendCoalm00[[i]], yr.means[,6:8])
    extendCoalm97[[i]] <- cbind(extendCoalm97[[i]], yr.means[,6:8])
    extendCoalm94[[i]] <- cbind(extendCoalm94[[i]], yr.means[,6:8])
}

#################################################################################################
## - should also try jags estimation to get post-sample of vhats and alphas                    ##
## - report mg effect of unit change in bar(v) at year's level instead of betahat (cf. Linzer) ##
#################################################################################################

###############################
## código de las regresiones ##
###############################
vhat.2024 <-                 # <--- prospective, with up-to 2021 returns
vhat.2021 <- vhat.2018 <- vhat.2015 <- vhat.2012 <- vhat.2009 <-
vhat.2006 <- vhat.2003 <- vhat.2000 <- vhat.1997 <- vhat.1994 <- vhat.1991 <- vhat.1988 <- 
    data.frame(pan  = rep(NA, nmun),
               pri  = rep(NA, nmun),
               left = rep(NA, nmun)) # will receive vote estimates
##
alphahat <- data.frame(pan    = rep(NA, nmun),
                       pri    = rep(NA, nmun),
                       left   = rep(NA, nmun)) # will receive municipio's alphas
betahat <- data.frame(pan    = rep(NA, nmun),
                      left   = rep(NA, nmun),
                      oth    = rep(NA, nmun)) # will receive municipio's betas (none for pri)
##
tmp <- as.list(rep(NA, nmun)) ## empty list will receive one time-series
                              ## regression per unit, each used to
                              ## predict votes in 2006:2021
## add names to m and s (to d must be done yearly basis due to redistricting)
names(tmp) <- v00m$ife
## if (agg=="s") names(tmp) <- v00d$edon*10000 + v00d$seccion # untested
##
regs.2024 <- regs.2021 <- regs.2018 <- regs.2015 <- regs.2012 <- regs.2009 <-
regs.2006 <- regs.2003 <- regs.2000 <- regs.1997 <- regs.1994 <- regs.1991 <- regs.1988 <-         
    list(pan    = tmp,
         left   = tmp,
         oth    = tmp,
         readme = "No pri regs because DVs are pri-ratios")
## one mean.reg per map
mean.regs.m21 <- mean.regs.m18 <-
mean.regs.m15 <- mean.regs.m12 <- mean.regs.m09 <- mean.regs.m06 <-
mean.regs.m03 <- mean.regs.m00 <- mean.regs.m97 <- mean.regs.m94 <-
##mean.regs.m91 <- mean.regs.m88 <-
        list(pan    = tmp,
             left   = tmp,
             oth    = tmp,
             readme = "No pri regs bec DVs are pri-ratios")
##
## drop list elements that still have NAs from loop
## (happens with new municipios before birth)
non.nas <- function(sel.map){
    if (sel.map %notin% seq(1994,2024,by=3)) stop("wrong sel.map in non.nas()")
    if (sel.map==1994) nn <- lapply(extendCoalm94, sum)
    if (sel.map==1997) nn <- lapply(extendCoalm97, sum)
    if (sel.map==2000) nn <- lapply(extendCoalm00, sum)
    if (sel.map==2003) nn <- lapply(extendCoalm03, sum)
    if (sel.map==2006) nn <- lapply(extendCoalm06, sum)
    if (sel.map==2009) nn <- lapply(extendCoalm09, sum)
    if (sel.map==2012) nn <- lapply(extendCoalm12, sum)
    if (sel.map==2015) nn <- lapply(extendCoalm15, sum)
    if (sel.map==2018) nn <- lapply(extendCoalm18, sum)
    if (sel.map==2021) nn <- lapply(extendCoalm21, sum)
    nn <- unlist(nn)
    ##table(is.na(nn))
    ##nn                     # debug
    ##extendCoalm18[[206]]           # debug: 20jul2021 NA due to unreported sole sección in cps municipio
    ##which(is.na(nn)==TRUE) # debug
    nn <- which(is.na(nn)==FALSE)
    ##length(nn)
    return(nn)
}
non.nas(2000)[1:18] # debug
setdiff(1:nmun, non.nas(2000)) # one year's complement
##    
#########################################################################################
## Municipio 5-yr estimates that can be computed before 2024                           ##
## |      | map  |      |      |      |      |       |       |       |       |       | ##
## | vhat | 1994 | 1997 | 2000 | 2003 | 2006 |  2009 |  2012 |  2015 |  2018 | 2021  | ##
## |------+------+------+------+------+------+-------+-------+-------+-------+-------| ##
## | 1991 | back |      |      |      |      |       |       |       |       |       | ##
## | 1994 | back |      |      |      |      |       |       |       |       |       | ##
## | 1997 |      | back |      |      |      |       |       |       |       |       | ##
## | 2000 |      |      | back |      |      |       |       |       |       |       | ##
## | 2003 |      |      |      | back |      |       |       |       |       |       | ##
## | 2006 |      |      |      |      |back* |       |       |       |       |       | ##
## | 2009 |      |      |      |      |      | *fwd* |       |       |       |       | ##
## | 2012 |      |      |      |      |      |       | *fwd* |       |       |       | ##
## | 2015 |      |      |      |      |      |       |       | *fwd* |       |       | ##
## | 2018 |      |      |      |      |      |       |       |       | *fwd* |       | ##
## | 2021 |      |      |      |      |      |       |       |       |       | *fwd* | ##
## | 2024 |      |      |      |      |      |       |       |       |       | *fwd* | ##
#########################################################################################


##############################################
## wrap municipio estimations in a function ##
##############################################
## ##              1    2    3    4    5    6    7    8    9    0    1 
## sel.map <- c(1994,1997,2000,2003,2006,2009,2012,2015,2018,2021,2024,by=3)[5]
## ##
pb <- txtProgressBar(min = 0, max = nmun, initial = 0) # for progress bar
estim_mun <- function(sel.map){
    message("Estimating ", sel.map)
    #
    ## if map is 1994 will estimate 1991 and 1988 backwards, add row in data frames
    if (sel.map==1994){
        add1988 <- function(x){
            x <- rbind(v88=x[1,], x) # repeat 1st row
            x$yr[1] <- 1988
            x[1,c(2:7,9:11)] <- NA # all NAs except ife
            return(x)
        }
        ## OJO: should replace NAs above with 1988 mun returns, if I have them
        tmp <- extendCoalm94 # duplicate for manipulation
        tmp <- lapply(extendCoalm94, add1988) # add row for 1991 to each data frame in list
        extendCoalm94 <- tmp
        add1991 <- function(x){
            rbind(v91=x[1,], x) # repeat 1st row
            x$yr[1] <- 1991
            x[1,c(2:7,9:11)] <- NA # all NAs except ife
            return(x)
        }
        ## ## OJO: should replace NAs above with 1991 mun returns in file
        ## tmp <- extendCoalm94 # duplicate for manipulation
        ## tmp <- lapply(extendCoalm94, add1991) # add row for 1991 to each data frame in list
        ## extendCoalm94 <- tmp
        rm(add1988,add1991,tmp)
    }
    #
    if (sel.map==2021){
        # will estimate 2024 in 2021 municipal map, so add row in data frames
        # this block will change to 2027 after 24's election
        add2024 <- function(x) rbind(x, v24=c(2024,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
        tmp <- extendCoalm21 # duplicate for manipulation
        tmp <- lapply(extendCoalm21, add2024) # add row for 2024 to each data frame in list
        extendCoalm21 <- tmp
        rm(add2024,tmp)
    }
    ##
    ## ################################################################ ##
    ## Add missing columns to unmanipulated list items (for squareness) ##
    ## ################################################################ ##
    sel.r <- setdiff(1:nmun, non.nas(sel.map))
    ## subset data to single unit
    if (sel.map==1994) data.tmp <- extendCoalm94[sel.r]
    if (sel.map==1997) data.tmp <- extendCoalm97[sel.r]
    if (sel.map==2000) data.tmp <- extendCoalm00[sel.r]
    if (sel.map==2003) data.tmp <- extendCoalm03[sel.r]
    if (sel.map==2006) data.tmp <- extendCoalm06[sel.r]
    if (sel.map==2009) data.tmp <- extendCoalm09[sel.r]
    if (sel.map==2012) data.tmp <- extendCoalm12[sel.r]
    if (sel.map==2015) data.tmp <- extendCoalm15[sel.r]
    if (sel.map==2018) data.tmp <- extendCoalm18[sel.r]
    if (sel.map==2021) data.tmp <- extendCoalm21[sel.r]
    ##
    data.tmp <- lapply(data.tmp, FUN = function(X){
        X <- within(X, {
            oth <- NULL;
            mean.rpan <- mean.rleft <- mean.roth <- NULL;
            ## betahat.oth   <- NA;
            betahat.left  <- NA;
            betahat.pan   <- NA;
            alphahat.left <- NA;
            alphahat.pri  <- NA;
            alphahat.pan  <- NA;
            betahat.left  <- NA;
            alphahat.left <- NA;
            dbackward     <- NA;
            bhat.left     <- NA;
            bhat.pan      <- NA;
            vhat.left     <- NA;
            vhat.pri      <- NA;
            vhat.pan      <- NA;
            d.left        <- NA;
            d.pri         <- NA;
            d.pan         <- NA;
        })
    })
    ## return estimates to data object
    if (sel.map==1994) extendCoalm94[sel.r] <- data.tmp
    if (sel.map==1997) extendCoalm97[sel.r] <- data.tmp
    if (sel.map==2000) extendCoalm00[sel.r] <- data.tmp
    if (sel.map==2003) extendCoalm03[sel.r] <- data.tmp
    if (sel.map==2006) extendCoalm06[sel.r] <- data.tmp
    if (sel.map==2009) extendCoalm09[sel.r] <- data.tmp
    if (sel.map==2012) extendCoalm12[sel.r] <- data.tmp
    if (sel.map==2015) extendCoalm15[sel.r] <- data.tmp
    if (sel.map==2018) extendCoalm18[sel.r] <- data.tmp
    if (sel.map==2021) extendCoalm21[sel.r] <- data.tmp
    ## 
    ## ######### ##
    ## MAIN LOOP ##
    ## ######### ##
    for (i in non.nas(sel.map)){
        #i <- 81 # debug
        #i <- 44508 # debug
        ## message(sprintf("loop %s of %s", i, max(non.nas(sel.map))))
        setTxtProgressBar(pb,i)
        # subset data to single unit
        if (sel.map==1994) data.tmp <- extendCoalm94[[i]]
        if (sel.map==1997) data.tmp <- extendCoalm97[[i]]
        if (sel.map==2000) data.tmp <- extendCoalm00[[i]]
        if (sel.map==2003) data.tmp <- extendCoalm03[[i]]
        if (sel.map==2006) data.tmp <- extendCoalm06[[i]]
        if (sel.map==2009) data.tmp <- extendCoalm09[[i]]
        if (sel.map==2012) data.tmp <- extendCoalm12[[i]]
        if (sel.map==2015) data.tmp <- extendCoalm15[[i]]
        if (sel.map==2018) data.tmp <- extendCoalm18[[i]]
        if (sel.map==2021) data.tmp <- extendCoalm21[[i]]
        ##
        ## add first-differences
        tmp.ln <- nrow(data.tmp)
        data.tmp$d.pan    <- data.tmp$pan    - c(NA, data.tmp$pan   [-tmp.ln])
        data.tmp$d.pri    <- data.tmp$pri    - c(NA, data.tmp$pri   [-tmp.ln])
        data.tmp$d.left   <- data.tmp$left   - c(NA, data.tmp$left  [-tmp.ln])
        rm(tmp.ln)
        ##
        ############################################
        ## backwards-predict 1988 with next 5 els ##
        ############################################
        if (sel.map==1994){
            tmp.back <- 1 # will indicate backwards prediction
            year <- 1988  # redundant for secciones/municipios, retained to keep code similar to distritos
            reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            #
            new.d <- data.frame(yr = year)
            rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
            rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
            rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
            vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
            bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
            #
            ## plug into results objects ##
            vhat.1988[i,] <- c(vhat.pan, vhat.pri, vhat.left)
            regs.1988$pan [[i]]   <- reg.pan
            regs.1988$left[[i]]   <- reg.left
            regs.1988$oth [[i]]   <- reg.oth
            #
            # add slot for projections/estimates if absent
            if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA  
            if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA  
            if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA 
            if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA  
            if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA 
            if ("dbackward" %notin% colnames(data.tmp)) data.tmp$dbackward <- NA
            #
            data.tmp$vhat.pan [data.tmp$yr==year] <- vhat.pan   # input vote estimates
            data.tmp$vhat.pri [data.tmp$yr==year] <- vhat.pri
            data.tmp$vhat.left[data.tmp$yr==year] <- vhat.left
            data.tmp$bhat.pan [data.tmp$yr==year] <- bhat.pan   # input slope estimates
            data.tmp$bhat.left[data.tmp$yr==year] <- bhat.left
            data.tmp$dbackward[data.tmp$yr==year] <- tmp.back   # input fwd/back
        }
        #
        ############################################
        ## backwards-predict 1991 with next 5 els ##
        ############################################
        if (sel.map==1994){
            tmp.back <- 1 # will indicate backwards prediction
            year <- 1991  # redundant for secciones/municipios, retained to keep code similar to distritos
            reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            #
            new.d <- data.frame(yr = year)
            rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
            rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
            rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
            vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
            bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
            #
            ## plug into results objects ##
            vhat.1991[i,] <- c(vhat.pan, vhat.pri, vhat.left)
            regs.1991$pan [[i]]   <- reg.pan
            regs.1991$left[[i]]   <- reg.left
            regs.1991$oth [[i]]   <- reg.oth
            #
            # add slot for projections/estimates if absent
            if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA  
            if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA  
            if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA 
            if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA  
            if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA 
            if ("dbackward" %notin% colnames(data.tmp)) data.tmp$dbackward <- NA
            #
            data.tmp$vhat.pan [data.tmp$yr==year] <- vhat.pan   # input vote estimates
            data.tmp$vhat.pri [data.tmp$yr==year] <- vhat.pri
            data.tmp$vhat.left[data.tmp$yr==year] <- vhat.left
            data.tmp$bhat.pan [data.tmp$yr==year] <- bhat.pan   # input slope estimates
            data.tmp$bhat.left[data.tmp$yr==year] <- bhat.left
            data.tmp$dbackward[data.tmp$yr==year] <- tmp.back   # input fwd/back
        }
        #
        ############################################
        ## backwards-predict 1994 with next 5 els ##
        ############################################
        if (sel.map==1994){
            tmp.back <- 1 # will indicate backwards prediction
            year <- 1994  # redundant for secciones/municipios, retained to keep code similar to distritos
            reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            #
            new.d <- data.frame(yr = year)
            rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
            rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
            rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
            vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
            bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
            #
            ## plug into results objects ##
            vhat.1994[i,] <- c(vhat.pan, vhat.pri, vhat.left)
            regs.1994$pan [[i]]   <- reg.pan
            regs.1994$left[[i]]   <- reg.left
            regs.1994$oth [[i]]   <- reg.oth
            #
            # add slot for projections/estimates if absent
            if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA  
            if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA  
            if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA 
            if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA  
            if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA 
            if ("dbackward" %notin% colnames(data.tmp)) data.tmp$dbackward <- NA
            #
            data.tmp$vhat.pan [data.tmp$yr==year] <- vhat.pan   # input vote estimates
            data.tmp$vhat.pri [data.tmp$yr==year] <- vhat.pri
            data.tmp$vhat.left[data.tmp$yr==year] <- vhat.left
            data.tmp$bhat.pan [data.tmp$yr==year] <- bhat.pan   # input slope estimates
            data.tmp$bhat.left[data.tmp$yr==year] <- bhat.left
            data.tmp$dbackward[data.tmp$yr==year] <- tmp.back   # input fwd/back
        }
        #
        ############################################
        ## backwards-predict 1997 with next 5 els ##
        ############################################
        if (sel.map==1997){
            tmp.back <- 1 # will indicate backwards prediction
            year <- 1997  # redundant for secciones/municipios, retained to keep code similar to distritos
            reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            #
            new.d <- data.frame(yr = year)
            rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
            rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
            rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
            vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
            bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
            #
            ## plug into results objects ##
            vhat.1997[i,] <- c(vhat.pan, vhat.pri, vhat.left)
            regs.1997$pan [[i]]   <- reg.pan
            regs.1997$left[[i]]   <- reg.left
            regs.1997$oth [[i]]   <- reg.oth
            #
            # add slot for projections/estimates if absent
            if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA  
            if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA  
            if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA 
            if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA  
            if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA 
            if ("dbackward" %notin% colnames(data.tmp)) data.tmp$dbackward <- NA
            #
            data.tmp$vhat.pan [data.tmp$yr==year] <- vhat.pan   # input vote estimates
            data.tmp$vhat.pri [data.tmp$yr==year] <- vhat.pri
            data.tmp$vhat.left[data.tmp$yr==year] <- vhat.left
            data.tmp$bhat.pan [data.tmp$yr==year] <- bhat.pan   # input slope estimates
            data.tmp$bhat.left[data.tmp$yr==year] <- bhat.left
            data.tmp$dbackward[data.tmp$yr==year] <- tmp.back   # input fwd/back
        }
        #
        ############################################
        ## backwards-predict 2000 with next 5 els ##
        ############################################
        if (sel.map==2000){
            tmp.back <- 1 # will indicate backwards prediction
            year <- 2000  # redundant for secciones/municipios, retained to keep code similar to distritos
            reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            #
            new.d <- data.frame(yr = year)
            rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
            rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
            rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
            vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
            bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
            #
            ## plug into results objects ##
            vhat.2000[i,] <- c(vhat.pan, vhat.pri, vhat.left)
            regs.2000$pan [[i]]   <- reg.pan
            regs.2000$left[[i]]   <- reg.left
            regs.2000$oth [[i]]   <- reg.oth
            #
            # add slot for projections/estimates if absent
            if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA  
            if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA  
            if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA 
            if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA  
            if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA 
            if ("dbackward" %notin% colnames(data.tmp)) data.tmp$dbackward <- NA
            #
            data.tmp$vhat.pan [data.tmp$yr==year] <- vhat.pan   # input vote estimates
            data.tmp$vhat.pri [data.tmp$yr==year] <- vhat.pri
            data.tmp$vhat.left[data.tmp$yr==year] <- vhat.left
            data.tmp$bhat.pan [data.tmp$yr==year] <- bhat.pan   # input slope estimates
            data.tmp$bhat.left[data.tmp$yr==year] <- bhat.left
            data.tmp$dbackward[data.tmp$yr==year] <- tmp.back   # input fwd/back
        }
        #
        ############################################
        ## backwards-predict 2003 with next 5 els ##
        ############################################
        if (sel.map==2003){
            tmp.back <- 1 # will indicate backwards prediction
            year <- 2003  # redundant for secciones/municipios, retained to keep code similar to distritos
            reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            #
            new.d <- data.frame(yr = year)
            rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
            rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
            rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
            vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
            bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
            #
            ## plug into results objects ##
            vhat.2003[i,] <- c(vhat.pan, vhat.pri, vhat.left)
            regs.2003$pan [[i]]   <- reg.pan
            regs.2003$left[[i]]   <- reg.left
            regs.2003$oth [[i]]   <- reg.oth
            #
            # add slot for projections/estimates if absent
            if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA  
            if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA  
            if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA 
            if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA  
            if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA 
            if ("dbackward" %notin% colnames(data.tmp)) data.tmp$dbackward <- NA
            #
            data.tmp$vhat.pan [data.tmp$yr==year] <- vhat.pan   # input vote estimates
            data.tmp$vhat.pri [data.tmp$yr==year] <- vhat.pri
            data.tmp$vhat.left[data.tmp$yr==year] <- vhat.left
            data.tmp$bhat.pan [data.tmp$yr==year] <- bhat.pan   # input slope estimates
            data.tmp$bhat.left[data.tmp$yr==year] <- bhat.left
            data.tmp$dbackward[data.tmp$yr==year] <- tmp.back   # input fwd/back
        }
        #
        ############################################
        ## backwards-predict 2006 with next 5 els ##
        ############################################
        if (sel.map==2006){
            tmp.back <- 1 # will indicate backwards prediction
            year <- 2006  # redundant for secciones/municipios, retained to keep code similar to distritos
            reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            #
            new.d <- data.frame(yr = year)
            rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
            rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
            rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
            vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
            bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
            #
            ## plug into results objects ##
            vhat.2006[i,] <- c(vhat.pan, vhat.pri, vhat.left)
            regs.2006$pan [[i]]   <- reg.pan
            regs.2006$left[[i]]   <- reg.left
            regs.2006$oth [[i]]   <- reg.oth
            #
            # add slot for projections/estimates if absent
            if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA  
            if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA  
            if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA 
            if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA  
            if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA 
            if ("dbackward" %notin% colnames(data.tmp)) data.tmp$dbackward <- NA
            #
            data.tmp$vhat.pan [data.tmp$yr==year] <- vhat.pan   # input vote estimates
            data.tmp$vhat.pri [data.tmp$yr==year] <- vhat.pri
            data.tmp$vhat.left[data.tmp$yr==year] <- vhat.left
            data.tmp$bhat.pan [data.tmp$yr==year] <- bhat.pan   # input slope estimates
            data.tmp$bhat.left[data.tmp$yr==year] <- bhat.left
            data.tmp$dbackward[data.tmp$yr==year] <- tmp.back   # input fwd/back
        }
        #
        ##################################
        ## predict 2009 with last 5 els ##
        ##################################
        if (sel.map==2009){
            tmp.back <- 0 # will indicate backwards prediction
            year <- 2009  # redundant for secciones/municipios, retained to keep code similar to distritos
            reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
            reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
            reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
            #
            new.d <- data.frame(yr = year)
            rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
            rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
            rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
            vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
            bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
            #
            ## plug into results objects ##
            vhat.2009[i,] <- c(vhat.pan, vhat.pri, vhat.left)
            regs.2009$pan [[i]]   <- reg.pan
            regs.2009$left[[i]]   <- reg.left
            regs.2009$oth [[i]]   <- reg.oth
            #
            # add slot for projections/estimates if absent
            if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA  
            if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA  
            if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA 
            if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA  
            if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA 
            if ("dbackward" %notin% colnames(data.tmp)) data.tmp$dbackward <- NA
            #
            data.tmp$vhat.pan [data.tmp$yr==year] <- vhat.pan   # input vote estimates
            data.tmp$vhat.pri [data.tmp$yr==year] <- vhat.pri
            data.tmp$vhat.left[data.tmp$yr==year] <- vhat.left
            data.tmp$bhat.pan [data.tmp$yr==year] <- bhat.pan   # input slope estimates
            data.tmp$bhat.left[data.tmp$yr==year] <- bhat.left
            data.tmp$dbackward[data.tmp$yr==year] <- tmp.back   # input fwd/back
        }
        ##
        ## ###############################
        ## predict 2012 with last 5 els ##
        ## ###############################
        if (sel.map==2012){
            tmp.back <- 0 # will indicate backwards prediction
            year <- 2012  # redundant for secciones/municipios, retained to keep code similar to distritos
            reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
            reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
            reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
            #
            new.d <- data.frame(yr = year)
            rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
            rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
            rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
            vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
            bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
            #
            ## plug into results objects ##
            vhat.2012[i,] <- c(vhat.pan, vhat.pri, vhat.left)
            regs.2012$pan [[i]]   <- reg.pan
            regs.2012$left[[i]]   <- reg.left
            regs.2012$oth [[i]]   <- reg.oth
            #
            # add slot for projections/estimates if absent
            if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA  
            if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA  
            if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA 
            if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA  
            if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA 
            if ("dbackward" %notin% colnames(data.tmp)) data.tmp$dbackward <- NA
            #
            data.tmp$vhat.pan [data.tmp$yr==year] <- vhat.pan   # input vote estimates
            data.tmp$vhat.pri [data.tmp$yr==year] <- vhat.pri
            data.tmp$vhat.left[data.tmp$yr==year] <- vhat.left
            data.tmp$bhat.pan [data.tmp$yr==year] <- bhat.pan   # input slope estimates
            data.tmp$bhat.left[data.tmp$yr==year] <- bhat.left
            data.tmp$dbackward[data.tmp$yr==year] <- tmp.back   # input fwd/back
        }
        ##
        ## ###############################
        ## predict 2015 with last 5 els ##
        ## ###############################
        if (sel.map==2015){
            tmp.back <- 0 # will indicate backwards prediction
            year <- 2015  # redundant for secciones/municipios, retained to keep code similar to distritos
            reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
            reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
            reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
            #
            new.d <- data.frame(yr = year)
            rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
            rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
            rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
            vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
            bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
            #
            ## plug into results objects ##
            vhat.2015[i,] <- c(vhat.pan, vhat.pri, vhat.left)
            regs.2015$pan [[i]]   <- reg.pan
            regs.2015$left[[i]]   <- reg.left
            regs.2015$oth [[i]]   <- reg.oth
            #
            # add slot for projections/estimates if absent
            if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA  
            if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA  
            if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA 
            if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA  
            if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA 
            if ("dbackward" %notin% colnames(data.tmp)) data.tmp$dbackward <- NA
            #
            data.tmp$vhat.pan [data.tmp$yr==year] <- vhat.pan   # input vote estimates
            data.tmp$vhat.pri [data.tmp$yr==year] <- vhat.pri
            data.tmp$vhat.left[data.tmp$yr==year] <- vhat.left
            data.tmp$bhat.pan [data.tmp$yr==year] <- bhat.pan   # input slope estimates
            data.tmp$bhat.left[data.tmp$yr==year] <- bhat.left
            data.tmp$dbackward[data.tmp$yr==year] <- tmp.back   # input fwd/back
        }
        ##
        ## ###############################
        ## predict 2018 with last 5 els ##
        ## ###############################
        if (sel.map==2018){
            tmp.back <- 0 # will indicate backwards prediction
            year <- 2018  # redundant for secciones/municipios, retained to keep code similar to distritos
            reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
            reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
            reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
            ##
            new.d <- data.frame(yr = year)
            rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
            rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
            rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
            vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
            bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
            ##
            ## plug into results objects ##
            vhat.2018[i,] <- c(vhat.pan, vhat.pri, vhat.left)
            regs.2018$pan [[i]]   <- reg.pan
            regs.2018$left[[i]]   <- reg.left
            regs.2018$oth [[i]]   <- reg.oth
            #
            # add slot for projections/estimates if absent
            if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA  
            if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA  
            if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA 
            if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA  
            if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA 
            if ("dbackward" %notin% colnames(data.tmp)) data.tmp$dbackward <- NA
            #
            data.tmp$vhat.pan [data.tmp$yr==year] <- vhat.pan   # input vote estimates
            data.tmp$vhat.pri [data.tmp$yr==year] <- vhat.pri
            data.tmp$vhat.left[data.tmp$yr==year] <- vhat.left
            data.tmp$bhat.pan [data.tmp$yr==year] <- bhat.pan   # input slope estimates
            data.tmp$bhat.left[data.tmp$yr==year] <- bhat.left
            data.tmp$dbackward[data.tmp$yr==year] <- tmp.back   # input fwd/back
        }
        ##
        ## ###############################
        ## predict 2021 with last 5 els ##
        ## ###############################
        if (sel.map==2021){
            tmp.back <- 0 # will indicate backwards prediction
            year <- 2021  # redundant for secciones/municipios, retained to keep code similar to distritos
            reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
            reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
            reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
            #
            new.d <- data.frame(yr = year)
            rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
            rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
            rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
            vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
            bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
            #
            ## plug into results objects ##
            vhat.2021[i,] <- c(vhat.pan, vhat.pri, vhat.left)
            regs.2021$pan [[i]]   <- reg.pan
            regs.2021$left[[i]]   <- reg.left
            regs.2021$oth [[i]]   <- reg.oth
            #
            # add slot for projections/estimates if absent
            if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA  
            if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA  
            if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA 
            if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA  
            if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA 
            if ("dbackward" %notin% colnames(data.tmp)) data.tmp$dbackward <- NA
            #
            data.tmp$vhat.pan [data.tmp$yr==year] <- vhat.pan   # input vote estimates
            data.tmp$vhat.pri [data.tmp$yr==year] <- vhat.pri
            data.tmp$vhat.left[data.tmp$yr==year] <- vhat.left
            data.tmp$bhat.pan [data.tmp$yr==year] <- bhat.pan   # input slope estimates
            data.tmp$bhat.left[data.tmp$yr==year] <- bhat.left
            data.tmp$dbackward[data.tmp$yr==year] <- tmp.back   # input fwd/back
        }
        ##
        ## ###############################
        ## predict 2024 with last 5 els ## OJO: IN 2024 CHANGE sel.map==2024
        ## ###############################
        if (sel.map==2021){
            tmp.back <- 0 # will indicate backwards prediction
            year <- 2024  # redundant for secciones/municipios, retained to keep code similar to distritos
            reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
            reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
            reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
            #
            new.d <- data.frame(yr = year)
            rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
            rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
            rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
            vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
            bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
            ##
            ## plug into results objects ##
            vhat.2024[i,] <- c(vhat.pan, vhat.pri, vhat.left)
            regs.2024$pan [[i]]   <- reg.pan
            regs.2024$left[[i]]   <- reg.left
            regs.2024$oth [[i]]   <- reg.oth
            ##
            # add slot for projections/estimates if absent
            if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA  
            if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA  
            if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA 
            if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA  
            if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA 
            #
            data.tmp$vhat.pan [data.tmp$yr==year] <- vhat.pan   # input vote estimates
            data.tmp$vhat.pri [data.tmp$yr==year] <- vhat.pri
            data.tmp$vhat.left[data.tmp$yr==year] <- vhat.left
            data.tmp$bhat.pan [data.tmp$yr==year] <- bhat.pan   # input slope estimates
            data.tmp$bhat.left[data.tmp$yr==year] <- bhat.left
            data.tmp$dbackward[data.tmp$yr==year] <- tmp.back   # input fwd/back
        }
        ##
        ## ######################################################################
        ## alpha regressions (cf. Díaz Cayeros, Estévez, Magaloni 2016, p. 90) ##
        ## ######################################################################
        reg.pan   <-  lm(formula = log(pan /pri)  ~  mean.rpan,  data = data.tmp)
        reg.left  <-  lm(formula = log(left/pri)  ~  mean.rleft, data = data.tmp)
        reg.oth   <-  lm(formula = log(oth /pri)  ~  mean.roth,  data = data.tmp)
        ##
        ## point prediction alpha with mean at zero 
        new.d <- data.frame(mean.rpan = 0)
        rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
        new.d <- data.frame(mean.rleft = 0)
        rhat.left   <- exp(predict.lm(reg.left  , newdata = new.d))#, interval = "confidence")
        new.d <- data.frame(mean.roth = 0)
        rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
        vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left + rhat.oth), 3)
        vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left + rhat.oth), 3)
        vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left + rhat.oth), 3)
        #
        #c(vhat.pan, vhat.pri, vhat.left, 1-vhat.pan-vhat.pri-vhat.left)
        alphahat[i,] <- c(vhat.pan, vhat.pri, vhat.left  )
        betahat[i,1] <- coef(reg.pan)   [2]
        betahat[i,2] <- coef(reg.left  )[2]
        betahat[i,3] <- coef(reg.oth)   [2]
        #
        if (sel.map==1994){
            mean.regs.m94$pan   [[i]] <- reg.pan
            mean.regs.m94$left  [[i]] <- reg.left  
            mean.regs.m94$oth   [[i]] <- reg.oth
        }
        if (sel.map==1997){
            mean.regs.m97$pan   [[i]] <- reg.pan
            mean.regs.m97$left  [[i]] <- reg.left  
            mean.regs.m97$oth   [[i]] <- reg.oth
        }
        if (sel.map==2000){
            mean.regs.m00$pan   [[i]] <- reg.pan
            mean.regs.m00$left  [[i]] <- reg.left  
            mean.regs.m00$oth   [[i]] <- reg.oth
        }
        if (sel.map==2003){
            mean.regs.m03$pan   [[i]] <- reg.pan
            mean.regs.m03$left  [[i]] <- reg.left  
            mean.regs.m03$oth   [[i]] <- reg.oth
        }
        if (sel.map==2006){
            mean.regs.m06$pan   [[i]] <- reg.pan
            mean.regs.m06$left  [[i]] <- reg.left  
            mean.regs.m06$oth   [[i]] <- reg.oth
        }
        if (sel.map==2009){
            mean.regs.m09$pan   [[i]] <- reg.pan
            mean.regs.m09$left  [[i]] <- reg.left  
            mean.regs.m09$oth   [[i]] <- reg.oth
        }
        if (sel.map==2012){
            mean.regs.m12$pan   [[i]] <- reg.pan
            mean.regs.m12$left  [[i]] <- reg.left  
            mean.regs.m12$oth   [[i]] <- reg.oth
        }
        if (sel.map==2015){
            mean.regs.m15$pan   [[i]] <- reg.pan
            mean.regs.m15$left  [[i]] <- reg.left  
            mean.regs.m15$oth   [[i]] <- reg.oth
        }
        if (sel.map==2018){
            mean.regs.m18$pan   [[i]] <- reg.pan
            mean.regs.m18$left  [[i]] <- reg.left  
            mean.regs.m18$oth   [[i]] <- reg.oth
        }
        if (sel.map==2021){
            mean.regs.m21$pan   [[i]] <- reg.pan
            mean.regs.m21$left  [[i]] <- reg.left  
            mean.regs.m21$oth   [[i]] <- reg.oth
        }
        ##
        ## add alphas and betas for whole period
        data.tmp$alphahat.left  <- data.tmp$alphahat.pri <- data.tmp$alphahat.pan <- NA # open slots for alphas
        data.tmp$betahat.left   <- data.tmp$betahat.pan <- NA                           # open slots for betas
        data.tmp$alphahat.pan   <- alphahat$pan [i]
        data.tmp$alphahat.pri   <- alphahat$pri [i]
        data.tmp$alphahat.left  <- alphahat$left[i]
        data.tmp$betahat.pan    <- betahat$pan  [i]
        data.tmp$betahat.left   <- betahat$left [i]
        data.tmp$betahat.oth    <- betahat$oth  [i]
        data.tmp <- round(data.tmp,3)
        #
        ########################################################
        ## optional: plug vhats alphas betas back into data   ##
        ########################################################
        data.tmp <- within(data.tmp, {
            mean.rpan <- mean.rleft   <- mean.roth <- NULL; # drop mean ratios
            oth <- NULL; # drop compositional vote complement
            betahat.oth <- NULL; # drop this beta
            #betahat.pan <- betahat.left   <- betahat.oth <- NULL; # drop betas
        })
        # return estimates to data object
        if (sel.map==1994) extendCoalm94[[i]] <- data.tmp
        if (sel.map==1997) extendCoalm97[[i]] <- data.tmp
        if (sel.map==2000) extendCoalm00[[i]] <- data.tmp
        if (sel.map==2003) extendCoalm03[[i]] <- data.tmp
        if (sel.map==2006) extendCoalm06[[i]] <- data.tmp
        if (sel.map==2009) extendCoalm09[[i]] <- data.tmp
        if (sel.map==2012) extendCoalm12[[i]] <- data.tmp
        if (sel.map==2015) extendCoalm15[[i]] <- data.tmp
        if (sel.map==2018) extendCoalm18[[i]] <- data.tmp
        if (sel.map==2021) extendCoalm21[[i]] <- data.tmp
        close(pb)
    }
    ###################################################
    ## warnings correspond to units with no variance ##
    ## (eg. period mean in new municipio in 2017)    ##
    ###################################################
    ## 
    ################################
    ## function will return this: ##
    ################################
    if (sel.map==1994){
        res.tmp <-
            list(ec = extendCoalm94,
                 rgs94 = regs.1994,
                 rgs91 = regs.1991,
                 rgs88 = regs.1988,
                 m.rgs = mean.regs.m94
                 )
        return(res.tmp)
    }
    if (sel.map==1997){
        res.tmp <-
            list(ec = extendCoalm97,
                 rgs = regs.1997,
                 m.rgs = mean.regs.m97
                 )
        return(res.tmp)
    }
    if (sel.map==2000){
        res.tmp <-
            list(ec = extendCoalm00,
                 rgs = regs.2000,
                 m.rgs = mean.regs.m00
                 )
        return(res.tmp)
    }
    if (sel.map==2003){
        res.tmp <-
            list(ec = extendCoalm03,
                 rgs = regs.2003,
                 m.rgs = mean.regs.m03
                 )
        return(res.tmp)
    }
    if (sel.map==2006){
        res.tmp <-
            list(ec = extendCoalm06,
                 rgs = regs.2006,
                 m.rgs = mean.regs.m06
                 )
        return(res.tmp)
    }
    if (sel.map==2009){
        res.tmp <-
            list(ec = extendCoalm09,
                 rgs = regs.2009,
                 m.rgs = mean.regs.m09
                 )
        return(res.tmp)
    }
    if (sel.map==2012){
        res.tmp <-
            list(ec = extendCoalm12,
                 rgs = regs.2012,
                 m.rgs = mean.regs.m12
                 )
        return(res.tmp)
    }
    if (sel.map==2015){
        res.tmp <-
            list(ec = extendCoalm15,
                 rgs = regs.2015,
                 m.rgs = mean.regs.m15
                 )
        return(res.tmp)
    }
    if (sel.map==2018){
        res.tmp <-
            list(ec = extendCoalm18,
                 rgs = regs.2018,
                 m.rgs = mean.regs.m18
                 )
        return(res.tmp)
    }
    if (sel.map==2021){
        res.tmp <-
            list(ec = extendCoalm21,
                 rgs21 = regs.2021,
                 rgs24 = regs.2024,
                 m.rgs = mean.regs.m21
                 )
        return(res.tmp)
    }
}


#########################################################################################
## REGRESSIONS ESTIMATION ROUTINE HERE                                                 ##
## municipal estimates are contingent on chosen map, run each once for full estimates  ##
#########################################################################################
##              1    2    3    4    5    6    7    8    9    0 
sel.map <- c(1994,1997,2000,2003,2006,2009,2012,2015,2018,2021,by=3)[1] ## EITHER CHOOSE SINGLE YEAR HERE
for (j in 2:10){                                                        ## OR RUN WHOLE LOOP
    sel.map <- c(1994,1997,2000,2003,2006,2009,2012,2015,2018,2021,by=3)[j]
    res.tmp <- estim_mun(sel.map)
    ###################################################
    ## warnings correspond to units with no variance ##
    ## (eg. period mean in new municipio in 2017)    ##
    ###################################################
    ##
    ## UNPACK OUTPUT
    if (sel.map==1994){
        extendCoalm94 <- res.tmp$ec
        regs.1994     <- res.tmp$rgs94
        regs.1991     <- res.tmp$rgs91
        regs.1988     <- res.tmp$rgs88
        mean.regs.m94 <- res.tmp$m.rgs
    }
    if (sel.map==1997){
        extendCoalm97 <- res.tmp$ec
        regs.1997     <- res.tmp$rgs
        mean.regs.m97 <- res.tmp$m.rgs
    }
    if (sel.map==2000){
        extendCoalm00 <- res.tmp$ec
        regs.2000     <- res.tmp$rgs
        mean.regs.m00 <- res.tmp$m.rgs
    }
    if (sel.map==2003){
        extendCoalm03 <- res.tmp$ec
        regs.2003     <- res.tmp$rgs
        mean.regs.m03 <- res.tmp$m.rgs
    }
    if (sel.map==2006){
        extendCoalm06 <- res.tmp$ec
        regs.2006     <- res.tmp$rgs
        mean.regs.m06 <- res.tmp$m.rgs
    }
    if (sel.map==2009){
        extendCoalm09 <- res.tmp$ec
        regs.2009     <- res.tmp$rgs
        mean.regs.m09 <- res.tmp$m.rgs
    }
    if (sel.map==2012){
        extendCoalm12 <- res.tmp$ec
        regs.2012     <- res.tmp$rgs
        mean.regs.m12 <- res.tmp$m.rgs
    }
    if (sel.map==2015){
        extendCoalm15 <- res.tmp$ec
        regs.2015     <- res.tmp$rgs
        mean.regs.m15 <- res.tmp$m.rgs
    }
    if (sel.map==2018){
        extendCoalm18 <- res.tmp$ec
        regs.2018     <- res.tmp$rgs
        mean.regs.m18 <- res.tmp$m.rgs
    }
    if (sel.map==2021){
        extendCoalm21 <- res.tmp$ec
        regs.2021     <- res.tmp$rgs21
        regs.2024     <- res.tmp$rgs24
        mean.regs.m21 <- res.tmp$m.rgs
    }
}

rm(res.tmp)

## ## debug
## save.image("../../datosBrutos/not-in-git/tmp2-restore.RData")

## ## load image
## rm(list=ls())
## options(width = 110)
## dd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/")
## setwd(dd)
## load(file="../../datosBrutos/not-in-git/tmp2-restore.RData")

## clean (all this is saved in extendCoal, mean.regs, regs.1988 ... regs.2024)
##ls()
rm(alphahat, betahat, cs, estim_mun, per.means, yr.means,
vhat.1988, vhat.1991, vhat.1994,
vhat.1997, vhat.2000, vhat.2003,
vhat.2006,
vhat.2009, vhat.2012, vhat.2015,
vhat.2018, vhat.2021, vhat.2024
)
rm(     v94m97, v94m00, v94m03, v94m06, v94m09, v94m12, v94m15, v94m18, v94m21,
v97m94,         v97m00, v97m03, v97m06, v97m09, v97m12, v97m15, v97m18, v97m21,
v00m94, v00m97,         v00m03, v00m06, v00m09, v00m12, v00m15, v00m18, v00m21,
v03m94, v03m97, v03m00,         v03m06, v03m09, v03m12, v03m15, v03m18, v03m21,
v06m94, v06m97, v06m00, v06m03,         v06m09, v06m12, v06m15, v06m18, v06m21,
v09m94, v09m97, v09m00, v09m03, v09m06,         v09m12, v09m15, v09m18, v09m21,
v12m94, v12m97, v12m00, v12m03, v12m06, v12m09,         v12m15, v12m18, v12m21,
v15m94, v15m97, v15m00, v15m03, v15m06, v15m09, v15m12,         v15m18, v15m21,
v18m94, v18m97, v18m00, v18m03, v18m06, v18m09, v18m12, v18m15,         v18m21,
v21m94, v21m97, v21m00, v21m03, v21m06, v21m09, v21m12, v21m15, v21m18
)
rm(sel.map, tmp, i, j, d, non.nas)

##########################################################################
## generate data frame with one year's predictions/estimates for export ##
##########################################################################
for.export <- function(year) {
    #year <- 2006         # debug
    #X <- extendCoalm94[[1]] # debug
    ## select relevant results object
    if (year %notin% seq(1988,2024,by=3)) stop("Year unavailable")
    if (year==2024) tmp.dat <- extendCoalm21
    if (year==2021) tmp.dat <- extendCoalm21
    if (year==2018) tmp.dat <- extendCoalm18
    if (year==2015) tmp.dat <- extendCoalm15
    if (year==2012) tmp.dat <- extendCoalm12
    if (year==2009) tmp.dat <- extendCoalm09
    if (year==2006) tmp.dat <- extendCoalm06
    if (year==2003) tmp.dat <- extendCoalm03
    if (year==2000) tmp.dat <- extendCoalm00
    if (year==1997) tmp.dat <- extendCoalm97
    if (year==1994) tmp.dat <- extendCoalm94
    if (year==1991) tmp.dat <- extendCoalm94
    if (year==1988) tmp.dat <- extendCoalm94
    sel.row <- which(tmp.dat[[1]]$yr==year) # which row reports year sought (symmetric across objects in list)
    # generate list with selected row only in every district
    tmp.out <- lapply(tmp.dat, FUN = function(X) {
        prune <- X[sel.row,]
        return(prune)
    })
    ## # spot NAs in list
    ## tmp.sel <- setdiff(1:length(extendCoal), non.nas(sel.map))
    ## # fill with same-dim NA data.frame
    ## tmp.manip <- tmp.out[[non.nas(sel.map)[1]]]
    ## tmp.manip[,-1] <- NA # all but 1st col (yr) to NA
    ## if (length(tmp.sel)>0) tmp.out[tmp.sel] <- lapply(tmp.out[tmp.sel], function(x) tmp.manip)
    # turn into one dataframe
    # table(summary(tmp)) # debug
    tmp.out <- do.call("rbind", tmp.out)
    rownames(tmp.out) <- NULL
    return(tmp.out)
}

out.y1988 <- for.export(year=1988)
out.y1991 <- for.export(year=1991)
out.y1994 <- for.export(year=1994)
out.y1997 <- for.export(year=1997)
out.y2000 <- for.export(year=2000)
out.y2003 <- for.export(year=2003)
out.y2006 <- for.export(year=2006)
out.y2009 <- for.export(year=2009)
out.y2012 <- for.export(year=2012)
out.y2015 <- for.export(year=2015)
out.y2018 <- for.export(year=2018)
out.y2021 <- for.export(year=2021)
out.y2024 <- for.export(year=2024)

## place id in 2nd col
out.y1988 <- out.y1988[moveme(names(out.y1988), "ife after yr")]
out.y1991 <- out.y1991[moveme(names(out.y1991), "ife after yr")]
out.y1994 <- out.y1994[moveme(names(out.y1994), "ife after yr")]
out.y1997 <- out.y1997[moveme(names(out.y1997), "ife after yr")]
out.y2000 <- out.y2000[moveme(names(out.y2000), "ife after yr")]
out.y2003 <- out.y2003[moveme(names(out.y2003), "ife after yr")]
out.y2006 <- out.y2006[moveme(names(out.y2006), "ife after yr")]
out.y2009 <- out.y2009[moveme(names(out.y2009), "ife after yr")]
out.y2012 <- out.y2012[moveme(names(out.y2012), "ife after yr")]
out.y2015 <- out.y2015[moveme(names(out.y2015), "ife after yr")]
out.y2018 <- out.y2018[moveme(names(out.y2018), "ife after yr")]
out.y2021 <- out.y2021[moveme(names(out.y2021), "ife after yr")]
out.y2024 <- out.y2024[moveme(names(out.y2024), "ife after yr")]

##################
## save to disk ##
##################
write.csv(out.y1988,
          file = paste(sd, "mun/dipfed-municipio-vhat-1988.csv", sep = ""), row.names = FALSE)
##
write.csv(out.y1991,
          file = paste(sd, "mun/dipfed-municipio-vhat-1991.csv", sep = ""), row.names = FALSE)
##
write.csv(out.y1994,
          file = paste(sd, "mun/dipfed-municipio-vhat-1994.csv", sep = ""), row.names = FALSE)
##
write.csv(out.y1997,
          file = paste(sd, "mun/dipfed-municipio-vhat-1997.csv", sep = ""), row.names = FALSE)
##
write.csv(out.y2000,
          file = paste(sd, "mun/dipfed-municipio-vhat-2000.csv", sep = ""), row.names = FALSE)
##
write.csv(out.y2003,
          file = paste(sd, "mun/dipfed-municipio-vhat-2003.csv", sep = ""), row.names = FALSE)
##
write.csv(out.y2006,
          file = paste(sd, "mun/dipfed-municipio-vhat-2006.csv", sep = ""), row.names = FALSE)
##
write.csv(out.y2009,
          file = paste(sd, "mun/dipfed-municipio-vhat-2009.csv", sep = ""), row.names = FALSE)
##
write.csv(out.y2012,
          file = paste(sd, "mun/dipfed-municipio-vhat-2012.csv", sep = ""), row.names = FALSE)
##
write.csv(out.y2015,
          file = paste(sd, "mun/dipfed-municipio-vhat-2015.csv", sep = ""), row.names = FALSE)
##
write.csv(out.y2018,
          file = paste(sd, "mun/dipfed-municipio-vhat-2018.csv", sep = ""), row.names = FALSE)
##
write.csv(out.y2021,
          file = paste(sd, "mun/dipfed-municipio-vhat-2021.csv", sep = ""), row.names = FALSE)
##
write.csv(out.y2024,
          file = paste(sd, "mun/dipfed-municipio-vhat-2024.csv", sep = ""), row.names = FALSE)
##
#############################################################
## save district regression objects (one mean.reg per map) ##
#############################################################
##save(mean.regs.m91, file = paste(sd, "mun/dipfed-municipio-mean-regs-1991.RData", sep = ""), compress = c("gzip", "bzip2", "xz")[3])
save(mean.regs.m94, file = paste(sd, "mun/dipfed-municipio-mean-regs-1994.RData", sep = ""), compress = c("gzip", "bzip2", "xz")[3])
save(mean.regs.m97, file = paste(sd, "mun/dipfed-municipio-mean-regs-1997.RData", sep = ""), compress = c("gzip", "bzip2", "xz")[3])
save(mean.regs.m00, file = paste(sd, "mun/dipfed-municipio-mean-regs-2000.RData", sep = ""), compress = c("gzip", "bzip2", "xz")[3])
save(mean.regs.m03, file = paste(sd, "mun/dipfed-municipio-mean-regs-2003.RData", sep = ""), compress = c("gzip", "bzip2", "xz")[3])
save(mean.regs.m06, file = paste(sd, "mun/dipfed-municipio-mean-regs-2006.RData", sep = ""), compress = c("gzip", "bzip2", "xz")[3])
save(mean.regs.m09, file = paste(sd, "mun/dipfed-municipio-mean-regs-2009.RData", sep = ""), compress = c("gzip", "bzip2", "xz")[3])
save(mean.regs.m12, file = paste(sd, "mun/dipfed-municipio-mean-regs-2012.RData", sep = ""), compress = c("gzip", "bzip2", "xz")[3])
save(mean.regs.m15, file = paste(sd, "mun/dipfed-municipio-mean-regs-2015.RData", sep = ""), compress = c("gzip", "bzip2", "xz")[3])
save(mean.regs.m18, file = paste(sd, "mun/dipfed-municipio-mean-regs-2018.RData", sep = ""), compress = c("gzip", "bzip2", "xz")[3])
save(mean.regs.m21, file = paste(sd, "mun/dipfed-municipio-mean-regs-2021.RData", sep = ""), compress = c("gzip", "bzip2", "xz")[3])
## save(mean.regs.m24, file = paste(sd, "mun/dipfed-municipio-mean-regs-2024.RData", sep = ""), compress = c("gzip", "bzip2", "xz")[3])
##
save(regs.1988, file = paste(sd, "too-big-4-github/dipfed-municipio-regs-1988.RData", sep = ""), compress = "gzip")
save(regs.1991, file = paste(sd, "too-big-4-github/dipfed-municipio-regs-1991.RData", sep = ""), compress = "gzip")
save(regs.1994, file = paste(sd, "too-big-4-github/dipfed-municipio-regs-1994.RData", sep = ""), compress = "gzip")
save(regs.1997, file = paste(sd, "too-big-4-github/dipfed-municipio-regs-1997.RData", sep = ""), compress = "gzip")
save(regs.2000, file = paste(sd, "too-big-4-github/dipfed-municipio-regs-2000.RData", sep = ""), compress = "gzip")
save(regs.2003, file = paste(sd, "too-big-4-github/dipfed-municipio-regs-2003.RData", sep = ""), compress = "gzip")
save(regs.2006, file = paste(sd, "too-big-4-github/dipfed-municipio-regs-2006.RData", sep = ""), compress = "gzip")
save(regs.2009, file = paste(sd, "too-big-4-github/dipfed-municipio-regs-2009.RData", sep = ""), compress = "gzip")
save(regs.2012, file = paste(sd, "too-big-4-github/dipfed-municipio-regs-2012.RData", sep = ""), compress = "gzip")
save(regs.2015, file = paste(sd, "too-big-4-github/dipfed-municipio-regs-2015.RData", sep = ""), compress = "gzip")
save(regs.2018, file = paste(sd, "too-big-4-github/dipfed-municipio-regs-2018.RData", sep = ""), compress = "gzip")
save(regs.2021, file = paste(sd, "too-big-4-github/dipfed-municipio-regs-2021.RData", sep = ""), compress = "gzip")
save(regs.2024, file = paste(sd, "too-big-4-github/dipfed-municipio-regs-2024.RData", sep = ""), compress = "gzip")
## save(regs.2027, file = paste(sd, "too-big-4-github/dipfed-municipio-regs-2027.RData", sep = ""), compress = "gzip")

###########
## clean ##
###########
rm(
    out.y1988,
    out.y1991,
    out.y1994,
    out.y1997,
    out.y2000,
    out.y2003,
    out.y2006,
    out.y2009,
    out.y2012,
    out.y2015,
    out.y2018,
    out.y2021,
    out.y2024
)
rm(
    regs.1988,
    regs.1991,
    regs.1994,
    regs.1997,
    regs.2000,
    regs.2003,
    regs.2006,
    regs.2009,
    regs.2012,
    regs.2015,
    regs.2018,
    regs.2021,
    regs.2024
)
rm(
    mean.regs.m94,
    mean.regs.m97,
    mean.regs.m00,
    mean.regs.m03,
    mean.regs.m06,
    mean.regs.m09,
    mean.regs.m12,
    mean.regs.m15,
    mean.regs.m18,
    mean.regs.m21
    )
rm(
    extendCoalm94,
    extendCoalm97,
    extendCoalm00,
    extendCoalm03,
    extendCoalm06,
    extendCoalm09,
    extendCoalm12,
    extendCoalm15,
    extendCoalm18,
    extendCoalm21
)
##ls()

        
## ###########################
## ## inspect saved objects ##
## ###########################
## # load regression object
## load(file = paste(wd, "data/dipfed-municipio-regs-2006.RData", sep = ""))
## ls()
## summary(regs.2006$pan[[1]])
## summary.lm(regs.2009$pan[[1]])$coef[2,1]
## x


# version 2: coalitions only in districts where they happened
## leftRealm <- data.frame(v00 = v00m$prdc / v00m$efec,
##                         v03 = v03m$prd / v03m$efec,
##                         v06 = v06m$prdc / v06m$efec,
##                         v09 = v09m$prd / v09m$efec,
##                         v12 = v12m$prdc / v12m$efec,
##                         v15 = (v15m$prd * (1 - v15m$dprdc) + v15m$prdc * v15m$dprdc + v15m$morena) / v15m$efec,
##                         v18 = (v18m$morena * (1 - v18m$dmorenac) + v18m$morenac * v18m$dmorenac) / v18m$efec)
## esto no jala, parece que morena!=0 cuando dmorenac==1
## morenaRealm[1,]
# version 3: party's own vote plus proportional part of coal
## morenaBrkm

