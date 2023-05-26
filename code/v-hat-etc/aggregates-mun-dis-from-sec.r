#########################################################################
## Script to aggregate municipio and distrito diputado federal returns ##
## from seccion-level INE reports                                      ##
## invoked from code/elec-data-for-maps.r                              ##
##                                                                     ##
## Author: Eric Magar                                                  ##
## emagar at itam dot mx                                               ##
## Date: 6may2023                                                      ##
## Last modified: 6may2023                                             ##
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
d <- my_agg(d=d, sel.c=sel.c, by="dis1997", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis1997                            # district ids for the historic map
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v94d97 <- d                                    # rename object  
# 2006 counterfactual districts
d <- v94s; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="dis2006", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis2006                            # district ids for the historic map
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v94d06 <- d                                    # rename object  
# 2018 counterfactual districts
d <- v94s; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="dis2018", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis2018                            # district ids for the historic map
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
d <- my_agg(d=d, sel.c=sel.c, by="dis1979", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis1979                            # district ids for the historic map
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v97d79 <- d                                    # rename object  
# 2006 counterfactual districts
d <- v97s; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="dis2006", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis2006                            # district ids for the historic map
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d <- d[moveme(names(d), "efec before lisnom")] # order columns
d <- d[order(d$disn),]                         # sort districts
v97d06 <- d                                    # rename object  
# 2018 counterfactual districts
d <- v97s; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="dis2018", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis2018                            # district ids for the historic map
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
d <- my_agg(d=d, sel.c=sel.c, by="dis1979", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis1979                            # district ids for the historic map
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
d <- my_agg(d=d, sel.c=sel.c, by="dis2006", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis2006                            # district ids for the historic map
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
d <- my_agg(d=d, sel.c=sel.c, by="dis2018", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis2018                            # district ids for the historic map
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
d <- my_agg(d=d, sel.c=sel.c, by="dis1979", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis1979                            # district ids for the historic map
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
d <- my_agg(d=d, sel.c=sel.c, by="dis2006", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis2006                            # district ids for the historic map
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
d <- my_agg(d=d, sel.c=sel.c, by="dis2018", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis2018                            # district ids for the historic map
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
d <- my_agg(d=d, sel.c=sel.c, by="dis1979", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis1979                            # district ids for the historic map
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
d <- my_agg(d=d, sel.c=sel.c, by="dis1997", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis1997                            # district ids for the historic map
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
d <- my_agg(d=d, sel.c=sel.c, by="dis2018", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis2018                            # district ids for the historic map
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
d <- my_agg(d=d, sel.c=sel.c, by="dis1979", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis1979                            # district ids for the historic map
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
d <- my_agg(d=d, sel.c=sel.c, by="dis1997", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis1997                            # district ids for the historic map
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
d <- my_agg(d=d, sel.c=sel.c, by="dis2018", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis2018                            # district ids for the historic map
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
d <- my_agg(d=d, sel.c=sel.c, by="dis1979", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis1997                            # district ids for the historic map
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
d <- my_agg(d=d, sel.c=sel.c, by="dis1997", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis1997                            # district ids for the historic map
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
d <- my_agg(d=d, sel.c=sel.c, by="dis2018", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis2018                            # district ids for the historic map
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
d <- my_agg(d=d, sel.c=sel.c, by="dis1979", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis1979                            # district ids for the historic map
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
d <- my_agg(d=d, sel.c=sel.c, by="dis1997", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis1997                            # district ids for the historic map
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
d <- my_agg(d=d, sel.c=sel.c, by="dis2018", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis2018                            # district ids for the historic map
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
d <- my_agg(d=d, sel.c=sel.c, by="dis1979", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis1979                            # district ids for the historic map
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
d <- my_agg(d=d, sel.c=sel.c, by="dis1997", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis1997                            # district ids for the historic map
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
d <- my_agg(d=d, sel.c=sel.c, by="dis2006", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis2006                            # district ids for the historic map
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
d <- my_agg(d=d, sel.c=sel.c, by="dis1979", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis1979                            # district ids for the historic map
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
d <- my_agg(d=d, sel.c=sel.c, by="dis1997", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis1997                            # district ids for the historic map
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
d <- my_agg(d=d, sel.c=sel.c, by="dis2006", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis2006                            # district ids for the historic map
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
#
rm(sel.drop,sel.c)

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
    nrow(v09d18), nrow(v12d18), nrow(v15d18)
))

