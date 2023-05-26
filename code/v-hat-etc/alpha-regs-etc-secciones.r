####################################################################
## Script for autoregressive vote estimates and alpha regressions ##
## invoked from code/elec-data-for-maps.r                         ##
##                                                                ##
## Author: Eric Magar                                             ##
## emagar at itam dot mx                                          ##
## Date: 17may2023                                                ##
## Last modified: 18may2023                                       ##
####################################################################

#################################################
## Note: Search 'Performs seccion estimations' ##
##       to locate estimation block            ##
#################################################

#############################################################
## Sección 5-yr estimates that can be computed before 2024 ##
## Reseccionamiento routine allows to work with single map ##
## |      | map     |                                      ##
## | vhat | unique  |                                      ##
## |------+---------|                                      ##
## | 1991 | back    |  not useful due to sección mismatch  ##
## | 1994 | back    |                                      ##
## | 1997 | back    |                                      ##
## | 2000 | back    |                                      ##
## | 2003 | back    |                                      ##
## | 2006 | back    |                                      ##
## | 2009 | fwd     |                                      ##
## | 2012 | fwd     |                                      ##
## | 2015 | fwd     |                                      ##
## | 2018 | fwd     |                                      ##
## | 2021 | fwd     |                                      ##
## | 2024 | fwd     |                                      ##
#############################################################


###########################
## clean seccion returns ##
###########################
##
## ##########
## ## 1991 ## OJO: 1991 seccion identifiers are wrong
## ##########
## sel.c <- c("pan","pri","pps","prd","pfcrn","parm","pdm","prt","pem","pt","efec","lisnom","dextra")
## # actual secciones
## d <- v91s; d[is.na(d)] <- 0
## v91s <- d                                      # rename object  
##
##########
## 1994 ##
##########
sel.c <- c("pan","pri","pps","prd","pfcrn","parm","uno.pdm","pt","pvem","efec","lisnom","dextra")
# actual secciones (reseccionaiento routine recovers most missing units)
d <- v94s; d[is.na(d)] <- 0
d$edosecn <- NULL                              # drop seccion ids
##d <- my_agg(d=d, sel.c=sel.c, by="seccion", y1991=FALSE) # use aggregating function
d <- d[moveme(names(d), "seccion first; efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- d[order(d$seccion),]                      # sort
v94s <- d                                      # rename object  
##
##########
## 1997 ##
##########
sel.c <- c("pan","pri","prd","pc","pt","pvem","pps","pdm","efec","lisnom","dextra")
# actual secciones (reseccionaiento routine recovers most missing units)
d <- v97s; d[is.na(d)] <- 0
d$edosecn <- NULL                              # drop seccion ids
##d <- my_agg(d=d, sel.c=sel.c, by="seccion", y1991=FALSE) # use aggregating function
d <- d[moveme(names(d), "seccion first; efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- d[order(d$seccion),]                      # sort
v97s <- d                                      # rename object  
##
##########
## 2000 ##
##########
sel.c <- c("panc","pri","prdc","pcd","parm","dsppn","efec","lisnom","dpanc","dprdc","dextra")
# actual secciones (reseccionaiento routine recovers most missing units)
d <- v00s; d[is.na(d)] <- 0
d$edosecn <- NULL                              # drop seccion ids
##d <- my_agg(d=d, sel.c=sel.c, by="seccion", y1991=FALSE) # use aggregating function
d <- d[moveme(names(d), "seccion first; efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- d[order(d$seccion),]                      # sort
v00s <- d                                      # rename object  
##
##########
## 2003 ##
##########
sel.c <- c("pan","pri","pric","prd","pt","pvem","conve","psn","pas","mp","plm","fc","efec","lisnom","dpric","dextra")
# actual secciones (reseccionaiento routine recovers most missing units)
d <- v03s; d[is.na(d)] <- 0
d$edosecn <- NULL                              # drop seccion ids
##d <- my_agg(d=d, sel.c=sel.c, by="seccion", y1991=FALSE) # use aggregating function
d <- d[moveme(names(d), "seccion first; efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- d[order(d$seccion),]                      # sort
v03s <- d                                      # rename object  
##
##########
## 2006 ##
##########
sel.c <- c("pan","pric","prdc","pna","asdc","efec","lisnom","dpric","dprdc","dextra")
# actual secciones (reseccionaiento routine recovers most missing units)
d <- v06s; d[is.na(d)] <- 0
d$edosecn <- NULL                              # drop seccion ids
##d <- my_agg(d=d, sel.c=sel.c, by="seccion", y1991=FALSE) # use aggregating function
d <- d[moveme(names(d), "seccion first; efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- d[order(d$seccion),]                      # sort
v06s <- d                                      # rename object  
##
##########
## 2009 ##
##########
sel.c <- c("pan","pri","pric","prd","pvem","pt","ptc","conve","pna","psd","efec","lisnom","dpric","dptc","dextra")
# actual secciones (reseccionaiento routine recovers most missing units)
d <- v09s; d[is.na(d)] <- 0
d$edosecn <- NULL                              # drop seccion ids
##d <- my_agg(d=d, sel.c=sel.c, by="seccion", y1991=FALSE) # use aggregating function
d <- d[moveme(names(d), "seccion first; efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- d[order(d$seccion),]                      # sort
v09s <- d                                      # rename object  
##
##########
## 2012 ##
##########
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","pric","prdc","efec","lisnom","dpric","dprdc","dextra")
# actual secciones (reseccionaiento routine recovers most missing units)
d <- v12s; d[is.na(d)] <- 0
d$edosecn <- NULL                              # drop seccion ids
##d <- my_agg(d=d, sel.c=sel.c, by="seccion", y1991=FALSE) # use aggregating function
d <- d[moveme(names(d), "seccion first; efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- d[order(d$seccion),]                      # sort
v12s <- d                                      # rename object  
##
##########
## 2015 ##
##########
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","ph","pes","pric","prdc","indep1","indep2","efec","lisnom","dpric","dprdc","dextra")
# actual secciones (reseccionaiento routine recovers most missing units)
d <- v15s; d[is.na(d)] <- 0
d$edosecn <- NULL                              # drop seccion ids
##d <- my_agg(d=d, sel.c=sel.c, by="seccion", y1991=FALSE) # use aggregating function
d <- d[moveme(names(d), "seccion first; efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- d[order(d$seccion),]                      # sort
v15s <- d                                      # rename object  
##
##########
## 2018 ##
##########
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","pes","panc","pric","morenac","indep1","indep2","efec","lisnom","dpanc","dpric","dmorenac","dextra")
# actual secciones (reseccionaiento routine recovers most missing units)
d <- v18s; d[is.na(d)] <- 0
d$edosecn <- NULL                              # drop seccion ids
##d <- my_agg(d=d, sel.c=sel.c, by="seccion", y1991=FALSE) # use aggregating function
d <- d[moveme(names(d), "seccion first; efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- d[order(d$seccion),]                      # sort
v18s <- d                                      # rename object  
##
##########
## 2021 ##
##########
sel.c <- c("pan","pri","prd","pvem","pt","mc","morena","pes","rsp","fxm","indep","panc","pric","morenac","efec","lisnom","dpanc","dpric","dmorenac","dextra")
# actual secciones (reseccionaiento routine recovers most missing units)
d <- v21s; d[is.na(d)] <- 0
d$edosecn <- NULL                              # drop seccion ids
##d <- my_agg(d=d, sel.c=sel.c, by="seccion", y1991=FALSE) # use aggregating function
##d <- d[moveme(names(d), "seccion first; efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- d[moveme(names(d), "seccion first; efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- d[order(d$seccion),]                      # sort
v21s <- d                                      # rename object  
##
rm(d)
##
# verify nrow==same (70018)
table(c(
    ## nrow(v91s),
    nrow(v94s),
    nrow(v97s), nrow(v00s), nrow(v03s),
    nrow(v06s), nrow(v09s), nrow(v12s), nrow(v15s),
    nrow(v18s), nrow(v21s)))
##
nsec <- nrow(v21s)  ## n secciones in square data



###############################################################################
## Prepare manipulated party objects for time-series and alpha regressions   ##
## After 2024 election, uncheck/add lines                                    ##
##                *** One object per municipio map ***                       ##
## *** Map changes almost every year, so one per federal election  ***       ##
###############################################################################
#
# version 1: extend partial coalitions across the board
# shares
pans <- data.frame(
    ## v91 =  with(v91s, ifelse(efec==0, NA,  pan               / efec)), 
    v94 =  with(v94s, ifelse(efec==0, NA,  pan               / efec)),
    v97 =  with(v97s, ifelse(efec==0, NA,  pan               / efec)),
    v00 =  with(v00s, ifelse(efec==0, NA,  panc              / efec)),
    v03 =  with(v03s, ifelse(efec==0, NA,  pan               / efec)),
    v06 =  with(v06s, ifelse(efec==0, NA,  pan               / efec)),
    v09 =  with(v09s, ifelse(efec==0, NA,  pan               / efec)),
    v12 =  with(v12s, ifelse(efec==0, NA,  pan               / efec)),
    v15 =  with(v15s, ifelse(efec==0, NA,  pan               / efec)),
    v18 =  with(v18s, ifelse(efec==0, NA, (pan + panc + prd) / efec)),  # dropped mc
    v21 =  with(v21s, ifelse(efec==0, NA, (pan + panc + prd) / efec))   # drop prd?
)
#
## pans[487:492,] ## see reseccionamiento manipulation in action: secciones 489- created before 2006 inherit parent's vote
## v94s[487:492,]
pans <- round(pans, 3)
#
pris <- data.frame(
    ## v91 =  with(v91s, ifelse(efec==0, NA,  pri                      / efec)),
    v94 =  with(v94s, ifelse(efec==0, NA,  pri                      / efec)),
    v97 =  with(v97s, ifelse(efec==0, NA,  pri                      / efec)),
    v00 =  with(v00s, ifelse(efec==0, NA,  pri                      / efec)),
    v03 =  with(v03s, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v06 =  with(v06s, ifelse(efec==0, NA,  pric                     / efec)),
    v09 =  with(v09s, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v12 =  with(v12s, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v15 =  with(v15s, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v18 =  with(v18s, ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)), # drop pvem + pna?
    v21 =  with(v21s, ifelse(efec==0, NA,  pri                      / efec))  # coal vote to pan+prd ok?
)
#
pris <- round(pris, 3)
#
lefts <- data.frame(
    ## v91 = with(v91s, ifelse(efec==0, NA,  prd                             / efec)),
    v94 = with(v94s, ifelse(efec==0, NA,  prd                             / efec)),
    v97 = with(v97s, ifelse(efec==0, NA,  prd                             / efec)),
    v00 = with(v00s, ifelse(efec==0, NA,  prdc                            / efec)),
    v03 = with(v03s, ifelse(efec==0, NA, (prd + pt + conve)               / efec)),
    v06 = with(v06s, ifelse(efec==0, NA,  prdc                            / efec)),
    v09 = with(v09s, ifelse(efec==0, NA, (prd + pt + ptc + conve)         / efec)),
    v12 = with(v12s, ifelse(efec==0, NA, (prd + prdc + pt + mc)           / efec)),
    v15 = with(v15s, ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
    v18 = with(v18s, ifelse(efec==0, NA, (morena + morenac + pt + pes)    / efec)),
    v21 = with(v21s, ifelse(efec==0, NA, (morena + morenac + pt + pvem)   / efec))  # drop pt + pvem?
)
#
lefts <- round(lefts, 3)
#
oths <- data.frame(
    ## v91 =  with(v91s, ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt)     / efec)),
    v94 =  with(v94s, ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
    v97 =  with(v97s, ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm)               / efec)),
    v00 =  with(v00s, ifelse(efec==0, NA, (pcd + parm + dsppn)                       / efec)),
    v03 =  with(v03s, ifelse(efec==0, NA, (psn + pas + mp + plm + fc)                / efec)),
    v06 =  with(v06s, ifelse(efec==0, NA, (pna + asdc)                               / efec)),
    v09 =  with(v09s, ifelse(efec==0, NA, (pna + psd)                                / efec)),
    v12 =  with(v12s, ifelse(efec==0, NA,  pna                                       / efec)),
    v15 =  with(v15s, ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2)          / efec)),
    v18 =  with(v18s, ifelse(efec==0, NA, (mc + indep1 + indep2)                     / efec)),
    v21 =  with(v21s, ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep)             / efec))
)
#
oths <- round(oths, 3)
#
efecs <- data.frame(
    ## v91 = v91s$efec,
    v94 = v94s$efec,
    v97 = v97s$efec,
    v00 = v00s$efec,
    v03 = v03s$efec,
    v06 = v06s$efec,
    v09 = v09s$efec,
    v12 = v12s$efec,
    v15 = v15s$efec,
    v18 = v18s$efec,
    v21 = v21s$efec
)
#
lisnoms <- data.frame(
    ## v91 = v91s$lisnom,
    v94 = v94s$lisnom,
    v97 = v97s$lisnom,
    v00 = v00s$lisnom,
    v03 = v03s$lisnom,
    v06 = v06s$lisnom,
    v09 = v09s$lisnom,
    v12 = v12s$lisnom,
    v15 = v15s$lisnom,
    v18 = v18s$lisnom,
    v21 = v21s$lisnom
)
#
# transpose to plug columns (units) into new data.frames
pans    <- t(pans)
pris    <- t(pris)
lefts   <- t(lefts)
oths    <- t(oths)
efecs   <- t(efecs)
lisnoms <- t(lisnoms)

#################################################################
## extendCoal.. will receive data for regressions, one per map ##
#################################################################
tmp <-     as.list(rep(NA, nsec)) # empty list will receive one data.frame per unit
names(tmp) <- v00s$seccion
#
extendCoals <- tmp
rm(tmp)
##
# loop over secciones
pb <- txtProgressBar(min = 0, max = nsec, initial = 0) # for progress bar
for (i in 1:nsec){
    #i <- 81 # debug
    setTxtProgressBar(pb,i)
    ###########################
    ## votes with unique map ##
    ###########################
    tmp <- data.frame(yr     = seq(from=1994, to=2021, by=3),
                      pan    = pans   [,i] ,
                      pri    = pris   [,i] ,
                      left   = lefts  [,i] ,
                      oth    = oths   [,i] ,
                      efec   = efecs  [,i] ,
                      lisnom = lisnoms[,i] )
    # replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan   [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri   [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left  [is.na(tmp$left)] <- per.means["left"];
        tmp$oth   [is.na(tmp$oth)]  <- per.means["oth"];
        tmp$efec  [is.na(tmp$efec)   | tmp$efec==0]   <- 1
        tmp$lisnom[is.na(tmp$lisnom) | tmp$lisnom==0] <- 2
    }
    # add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    # re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    # add id
    tmp$seccion <- v00s$seccion[i]
    # fill info to new list
    extendCoals[[i]] <- tmp
    ## # name list object
    ## names(extendCoals)[i] <- tmp$seccion[1]
    #
    close(pb)
}
## clean
rm(pans,pris,lefts,oths,efecs,lisnoms)
    
##################################
## datos para regresión de alfa ##
##################################
#
#############################################################################################################
## Nota 16jul2021: al añadir datos 2021 cambiarán todas las alfas y betas! Una solución fácil (que usaré)  ##
## y otra que requiere más coco. La fácil es reestimar con 2021 e identificar el commit que reportaba la   ##
## versión hasta 2018. La otra: definir una ventana temporal (como cinco elecciones) para producir alfas   ##
## y betas cada elección: alfa.2006, alfa.2009, etc. Debería poder investigar cómo usan esto en el Capital ##
## Asset Pricing Model...                                                                                  ##
#############################################################################################################
yr.means <- data.frame(yr = seq(1991,2021,3), # 11 election-years (drop 1991 later if unused in analysis)
                       pan    = rep(NA,11),
                       pri    = rep(NA,11),
                       left   = rep(NA,11),
                       oth    = rep(NA,11))
# function to sum numeric columns
cs <- function(x){
    sel.nums <- unlist(lapply(x, is.numeric), use.names = FALSE) # selects only numeric columns in data frame
    res <- colSums(x[x$dunbaja==0,sel.nums], na.rm=TRUE) # drops secciones manipulated upon splitting (would duplicate votes)
    return(res)
}
#
# compute national mean vote
yr.means$pan [1] <-  cs(v91s)["pan"]  /  cs(v91s)["efec"]
yr.means$pri [1] <-  cs(v91s)["pri"]  /  cs(v91s)["efec"]
yr.means$left[1] <-  cs(v91s)["prd"]  /  cs(v91s)["efec"]
yr.means$oth [1] <-  with(yr.means[1,], 1 - pan - pri - left)
#
yr.means$pan [2] <-  cs(v94s)["pan"]  / cs(v94s)["efec"]
yr.means$pri [2] <-  cs(v94s)["pri"]  / cs(v94s)["efec"]
yr.means$left[2] <-  cs(v94s)["prd"]  / cs(v94s)["efec"]
yr.means$oth [2] <-  with(yr.means[2,], 1 - pan - pri - left)
#                
yr.means$pan [3] <-  cs(v97s)["pan"]  / cs(v97s)["efec"]
yr.means$pri [3] <-  cs(v97s)["pri"]  / cs(v97s)["efec"]
yr.means$left[3] <-  cs(v97s)["prd"]  / cs(v97s)["efec"]
yr.means$oth [3] <-  with(yr.means[3,], 1 - pan - pri - left)
#                
yr.means$pan [4] <-  cs(v00s)["panc"]  / cs(v00s)["efec"]
yr.means$pri [4] <-  cs(v00s)["pri"]   / cs(v00s)["efec"]
yr.means$left[4] <-  cs(v00s)["prdc"]  / cs(v00s)["efec"]
yr.means$oth [4] <-  with(yr.means[4,], 1 - pan - pri - left)
#                
yr.means$pan [5] <-  cs(v03s)["pan"]                                         / cs(v03s)["efec"]
yr.means$pri [5] <- (cs(v03s)["pri"] + cs(v03s)["pric"] + cs(v03s)["pvem"])  / cs(v03s)["efec"]
yr.means$left[5] <- (cs(v03s)["prd"] + cs(v03s)["pt"]   + cs(v03s)["conve"]) / cs(v03s)["efec"]
yr.means$oth [5] <-  with(yr.means[5,], 1 - pan - pri - left)
#                
yr.means$pan [6] <-  cs(v06s)["pan"]   / cs(v06s)["efec"]
yr.means$pri [6] <-  cs(v06s)["pric"]  / cs(v06s)["efec"]
yr.means$left[6] <-  cs(v06s)["prdc"]  / cs(v06s)["efec"]
yr.means$oth [6] <-  with(yr.means[6,], 1 - pan - pri - left)
#                
yr.means$pan [7] <-  cs(v09s)["pan"]                                                           / cs(v09s)["efec"]
yr.means$pri [7] <- (cs(v09s)["pri"] + cs(v09s)["pric"] + cs(v09s)["pvem"])                    / cs(v09s)["efec"]
yr.means$left[7] <- (cs(v09s)["prd"] + cs(v09s)["pt"]   + cs(v09s)["ptc"] + cs(v09s)["conve"]) / cs(v09s)["efec"]
yr.means$oth [7] <-  with(yr.means[7,], 1 - pan - pri - left)
#                
yr.means$pan [8] <-  cs(v12s)["pan"]                                                       / cs(v12s)["efec"]
yr.means$pri [8] <- (cs(v12s)["pri"] + cs(v12s)["pric"] + cs(v12s)["pvem"])                / cs(v12s)["efec"]
yr.means$left[8] <- (cs(v12s)["prd"] + cs(v12s)["prdc"] + cs(v12s)["pt"] + cs(v12s)["mc"]) / cs(v12s)["efec"]
yr.means$oth [8] <-  with(yr.means[8,], 1 - pan - pri - left)
#                
yr.means$pan [9] <-  cs(v15s)["pan"]                                                            / cs(v15s)["efec"]
yr.means$pri [9] <- (cs(v15s)["pri"] + cs(v15s)["pric"] + cs(v15s)["pvem"])                     / cs(v15s)["efec"]
yr.means$left[9] <- (cs(v15s)["prd"] + cs(v15s)["prdc"] + cs(v15s)["pt"] + cs(v15s)["morena"] ) / cs(v15s)["efec"] # dropped cs(v15s)["pes"]
yr.means$oth [9] <-  with(yr.means[9,], 1 - pan - pri - left)
#
yr.means$pan [10] <- (cs(v18s)["pan"]    + cs(v18s)["panc"]    + cs(v18s)["prd"]  + cs(v18s)["mc"])  / cs(v18s)["efec"]
yr.means$pri [10] <- (cs(v18s)["pri"]    + cs(v18s)["pric"]    + cs(v18s)["pvem"] + cs(v18s)["pna"]) / cs(v18s)["efec"]
yr.means$left[10] <- (cs(v18s)["morena"] + cs(v18s)["morenac"] + cs(v18s)["pt"]   + cs(v18s)["pes"]) / cs(v18s)["efec"]
yr.means$oth [10] <-  with(yr.means[10,], 1 - pan - pri - left)
#
yr.means$pan [11] <- (cs(v21s)["pan"]    + cs(v21s)["panc"]    + cs(v21s)["prd"])                    / cs(v21s)["efec"]
yr.means$pri [11] <-  cs(v21s)["pri"]                                                                / cs(v21s)["efec"] # dropped cs(v21s)["pric"]
yr.means$left[11] <- (cs(v21s)["morena"] + cs(v21s)["morenac"] + cs(v21s)["pt"]  + cs(v21s)["pvem"]) / cs(v21s)["efec"]
yr.means$oth [11] <-  with(yr.means[11,], 1 - pan - pri - left)
##
## Until seccion ids are fixed/located, 1991 will be an out-of-sample prediction in units that can't be compared to 1991 votes.
## This drops 1991 missings from yr.means
yr.means
yr.means <- yr.means[-1,]
rownames(yr.means) <- NULL # reset row names to avoid confusion

#############################
## votes relative to pri's ##
#############################
yr.means <- within(yr.means, mean.rpan    <- pan  / pri)
yr.means <- within(yr.means, mean.rleft   <- left / pri)
yr.means <- within(yr.means, mean.roth    <- oth  / pri)
#
yr.means[,2:8] <- round(yr.means[,2:8], 3)
#
# plug into data
extendCoals <- lapply(extendCoals, function(x) x <- cbind(x, yr.means[,6:8]))


#################################################################################################
## - should also try jags estimation to get post-sample of vhats and alphas                    ##
## - report mg effect of unit change in bar(v) at year's level instead of betahat (cf. Linzer) ##
#################################################################################################

###############################
## código de las regresiones ##
###############################
vhat.2024 <-                 # <--- prospective, with up-to 2021 returns
vhat.2021 <- vhat.2018 <- vhat.2015 <- vhat.2012 <- vhat.2009 <-
vhat.2006 <- vhat.2003 <- vhat.2000 <- vhat.1997 <- vhat.1994 <- vhat.1991 <-
## vhat.1988 <-
    data.frame(pan  = rep(NA, nsec),
               pri  = rep(NA, nsec),
               left = rep(NA, nsec)) # will receive vote estimates
##
alphahat <- data.frame(pan    = rep(NA, nsec),
                       pri    = rep(NA, nsec),
                       left   = rep(NA, nsec)) # will receive municipio's alphas
betahat <- data.frame(pan    = rep(NA, nsec),
                      left   = rep(NA, nsec),
                      oth    = rep(NA, nsec)) # will receive municipio's betas (none for pri)
##
tmp <- as.list(rep(NA, nsec)) # empty list will receive one time-series
                                   # regression per unit, each used to
                                   # predict votes in 2006:2021
## add names to m and s (to d must be done yearly basis due to redistricting)
names(tmp) <- v00s$seccion
##
regs.2024 <- regs.2021 <- regs.2018 <- regs.2015 <- regs.2012 <- regs.2009 <-
regs.2006 <- regs.2003 <- regs.2000 <- regs.1997 <- regs.1994 <- regs.1991 <-
## regs.1988 <-
    list(pan    = tmp,
         left   = tmp,
         oth    = tmp,
         readme = "No pri regs because DVs are pri-ratios")
## one mean.reg per map
mean.regs.s <- 
    list(pan    = tmp,
         left   = tmp,
         oth    = tmp,
         readme = "No pri regs bec DVs are pri-ratios")
##
## drop list elements that still have NAs from loop
non.nas <- function(x=extendCoals){
    nn <- lapply(x, sum)
    ##nn <- lapply(extendCoals, sum)
    nn <- unlist(nn)
    ##table(is.na(nn))
    ##nn                     # debug
    ##extendCoals[[206]]     # debug: 20jul2021 NA due to unreported sole sección in cps municipio
    ##which(is.na(nn)==TRUE) # debug
    nn <- which(is.na(nn)==FALSE)
    ##length(nn)
    return(nn)
}
non.nas <- non.nas() 
non.nas[1:199] # debug
setdiff(1:nsec, non.nas) # one year's complement
##extendCoals[[60108]] # appears to be a seccion unused
##
#############################################################
## Sección 5-yr estimates that can be computed before 2024 ##
## Reseccionamiento routine allows to work with single map ##
## |      | map     |                                      ##
## | vhat | unique  |                                      ##
## |------+---------|                                      ##
## | 1991 | back    |  not useful due to sección mismatch  ##
## | 1994 | back    |                                      ##
## | 1997 | back    |                                      ##
## | 2000 | back    |                                      ##
## | 2003 | back    |                                      ##
## | 2006 | back    |                                      ##
## | 2009 | fwd     |                                      ##
## | 2012 | fwd     |                                      ##
## | 2015 | fwd     |                                      ##
## | 2018 | fwd     |                                      ##
## | 2021 | fwd     |                                      ##
## | 2024 | fwd     |                                      ##
#############################################################


##################################
## Performs seccion estimations ##
##################################
##
## Will estimate fwd out-of-period vhats and reg, need to add row in data frames.
## This block will change to 2027 after 24's election, and so forth after each new election.
add1991 <- function(x){
    rbind(v91=c(1991,NA,NA,NA,NA,NA,NA,x$seccion[1],NA,NA,NA), x );
}
add2024 <- function(x){
    rbind(x, v24=c(2024,NA,NA,NA,NA,NA,NA,x$seccion[1],NA,NA,NA) );
}
tmp <- extendCoals # duplicate for manipulation
tmp <- lapply(extendCoals, add2024) # add row for 2024 to each data frame in list
tmp <- lapply(tmp, add1991)         # add row for 1991 to each data frame in list
extendCoals <- tmp
extendCoals[[1]]
rm(add1991,add2024,tmp)

## ################################################################ ##
## Add missing columns to unmanipulated list items (for squareness) ##
## ################################################################ ##
sel.r <- setdiff(1:nsec, non.nas)
if (length(sel.r)>0){
    ## subset data to single unit
    data.tmp <- extendCoals[sel.r]
    data.tmp <- lapply(data.tmp, FUN = function(X){
        X <- within(X, {
            ## oth <- NULL;
            mean.rpan <- mean.rleft <- mean.roth <- NULL;
            dbackward     <- NA;
            bhat.left     <- NA;
            bhat.pan      <- NA;
            vhat.left     <- NA;
            vhat.pri      <- NA;
            vhat.pan      <- NA;
            betahat.left  <- NA;
            betahat.pan   <- NA;
            alphahat.left <- NA;
            alphahat.pri  <- NA;
            alphahat.pan  <- NA;
            d.left        <- NA;
            d.pri         <- NA;
            d.pan         <- NA;
        })
    })
}
## return estimates to data object
extendCoals[sel.r] <- data.tmp

## debug
save.image("../../datosBrutos/not-in-git/tmp2-restore.RData")

## load image
rm(list=ls())
options(width = 110)
dd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/")
setwd(dd)
load(file="../../datosBrutos/not-in-git/tmp2-restore.RData")


## ########## ##
## MAIN LOOPS ##
## ########## ##

## add first-differences
for (i in non.nas){
    ##message(sprintf("loop %s of %s", i, max(non.nas(sel.map))))
    ##
    ## subset data to single unit
    data.tmp <- extendCoals[[i]]
    ##
    tmp.ln <- nrow(data.tmp)
    data.tmp$d.pan    <- data.tmp$pan    - c(NA, data.tmp$pan   [-tmp.ln])
    data.tmp$d.pri    <- data.tmp$pri    - c(NA, data.tmp$pri   [-tmp.ln])
    data.tmp$d.left   <- data.tmp$left   - c(NA, data.tmp$left  [-tmp.ln])
    rm(tmp.ln)
    ##
    ## return estimates to data object
    extendCoals[[i]] <- data.tmp
}


## ######################################################################
## alpha regressions (cf. Díaz Cayeros, Estévez, Magaloni 2016, p. 90) ##
## Performs them first to include output across extendCoals. Then will ##
## estimate yearly regs, saving output, to free mem before next year   ##
## ######################################################################
for (i in non.nas){
    ##i <- 44508 # debug
    message(sprintf("alpha: loop %s of %s", i, max(non.nas)))
    ##
    ## subset data to single unit
    data.tmp <- extendCoals[[i]]
    ##
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
    ##
    ##c(vhat.pan, vhat.pri, vhat.left, 1-vhat.pan-vhat.pri-vhat.left)
    alphahat[i,] <- c(vhat.pan, vhat.pri, vhat.left  )
    betahat[i,1] <- coef(reg.pan)   [2]
    betahat[i,2] <- coef(reg.left  )[2]
    betahat[i,3] <- coef(reg.oth)   [2]
    ##
    mean.regs.s$pan   [[i]] <- reg.pan
    mean.regs.s$left  [[i]] <- reg.left  
    mean.regs.s$oth   [[i]] <- reg.oth
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
    ##
    ## #####################################################
    ## optional: plug vhats alphas betas back into data   ##
    ## #####################################################
    data.tmp <- within(data.tmp, {
        mean.rpan <- mean.rleft   <- mean.roth <- NULL; # drop mean ratios
        ## oth <- NULL; # drop compositional vote complement
        betahat.oth <- NULL; # drop this beta
        ##betahat.pan <- betahat.left   <- betahat.oth <- NULL; # drop betas
    })
    ## return estimates to data object
    extendCoals[[i]] <- data.tmp
}
##################################
## save mean regression objects ##
##################################
save(mean.regs.s, file = paste(wd, "data/too-big-4-github/dipfed-seccion-mean-regs.RData", sep = ""), compress = c("gzip", "bzip2", "xz")[3])
## clean mem
rm(mean.regs.s)


####################################################################################
## function generates data frame with one year's predictions/estimates for export ##
####################################################################################
for.export <- function(year) {
    #year <- 1991         # debug
    #X <- extendCoals[[10000]] # debug
    ## select relevant results object
    if (year %notin% seq(1991,2024,by=3)) stop("Year unavailable")
    sel.row <- which(extendCoals[[1]]$yr==year) # which row reports year sought (symmetric across objects in list)
    # generate list with selected row only in every district
    tmp.out <- lapply(extendCoals, FUN = function(X) {
        prune <- X[sel.row,]
        return(prune)
    })
    ## ## spot NAs in list
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


## #########################################
## backwards-predict 1991 with next 5 els ##
## #########################################
for (i in non.nas){
    ##i <- 44508 # debug
    message(sprintf("1991: loop %s of %s", i, max(non.nas)))
    ##
    ## subset data to single unit
    data.tmp <- extendCoals[[i]]
    ##
    tmp.back <- 1 # will indicate backwards prediction
    year <- 1991  # redundant for secciones/municipios, retained to keep code similar to distritos
    reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
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
    vhat.1991[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.1991$pan [[i]]   <- reg.pan
    regs.1991$left[[i]]   <- reg.left
    regs.1991$oth [[i]]   <- reg.oth
    ##
    ## add slot for projections/estimates if absent
    if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA  
    if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA  
    if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA 
    if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA  
    if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA 
    if ("dbackward" %notin% colnames(data.tmp)) data.tmp$dbackward <- NA
    ##
    data.tmp$vhat.pan [data.tmp$yr==year] <- vhat.pan   # input vote estimates
    data.tmp$vhat.pri [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left[data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan [data.tmp$yr==year] <- bhat.pan   # input slope estimates
    data.tmp$bhat.left[data.tmp$yr==year] <- bhat.left
    data.tmp$dbackward[data.tmp$yr==year] <- tmp.back   # input fwd/back
    ##
    ## return estimates to data object
    extendCoals[[i]] <- data.tmp
}
##################
## save to disk ##
##################
save(regs.1991, file = paste(wd, "data/too-big-4-github/dipfed-seccion-regs-1991.RData", sep = ""), compress = "gzip")
## clean memory
rm(regs.1991)


## #########################################
## backwards-predict 1994 with next 5 els ##
## #########################################
for (i in non.nas){
    ##i <- 44508 # debug
    message(sprintf("1994: loop %s of %s", i, max(non.nas)))
    ##
    ## subset data to single unit
    data.tmp <- extendCoals[[i]]
    ##
    tmp.back <- 1 # will indicate backwards prediction
    year <- 1994  # redundant for secciones/municipios, retained to keep code similar to distritos
    reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
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
    vhat.1994[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.1994$pan [[i]]   <- reg.pan
    regs.1994$left[[i]]   <- reg.left
    regs.1994$oth [[i]]   <- reg.oth
    ##
    ## add slot for projections/estimates if absent
    if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA  
    if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA  
    if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA 
    if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA  
    if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA 
    if ("dbackward" %notin% colnames(data.tmp)) data.tmp$dbackward <- NA
    ##
    data.tmp$vhat.pan [data.tmp$yr==year] <- vhat.pan   # input vote estimates
    data.tmp$vhat.pri [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left[data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan [data.tmp$yr==year] <- bhat.pan   # input slope estimates
    data.tmp$bhat.left[data.tmp$yr==year] <- bhat.left
    data.tmp$dbackward[data.tmp$yr==year] <- tmp.back   # input fwd/back
    ##
    ## return estimates to data object
    extendCoals[[i]] <- data.tmp
}
##################
## save to disk ##
##################
save(regs.1994, file = paste(wd, "data/too-big-4-github/dipfed-seccion-regs-1994.RData", sep = ""), compress = "gzip")
## clean memory
rm(regs.1994)


## #########################################
## backwards-predict 1997 with next 5 els ##
## #########################################
for (i in non.nas){
    ##i <- 44508 # debug
    message(sprintf("1997: loop %s of %s", i, max(non.nas)))
    ##
    ## subset data to single unit
    data.tmp <- extendCoals[[i]]
    ##
    tmp.back <- 1 ## will indicate backwards prediction
    year <- 1997  ## redundant for secciones/municipios, retained to keep code similar to distritos
    reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
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
    vhat.1997[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.1997$pan [[i]]   <- reg.pan
    regs.1997$left[[i]]   <- reg.left
    regs.1997$oth [[i]]   <- reg.oth
    ##
    ## add slot for projections/estimates if absent
    if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA  
    if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA  
    if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA 
    if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA  
    if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA 
    if ("dbackward" %notin% colnames(data.tmp)) data.tmp$dbackward <- NA
    ##
    data.tmp$vhat.pan [data.tmp$yr==year] <- vhat.pan   ## input vote estimates
    data.tmp$vhat.pri [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left[data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan [data.tmp$yr==year] <- bhat.pan   ## input slope estimates
    data.tmp$bhat.left[data.tmp$yr==year] <- bhat.left
    data.tmp$dbackward[data.tmp$yr==year] <- tmp.back   ## input fwd/back
    ##
    ## return estimates to data object
    extendCoals[[i]] <- data.tmp
}
##################
## save to disk ##
##################
save(regs.1997, file = paste(wd, "data/too-big-4-github/dipfed-seccion-regs-1997.RData", sep = ""), compress = "gzip")
## clean memory
rm(regs.1997)


## #########################################
## backwards-predict 2000 with next 5 els ##
## #########################################
for (i in non.nas){
    ##i <- 44508 # debug
    message(sprintf("2000: loop %s of %s", i, max(non.nas)))
    ##
    ## subset data to single unit
    data.tmp <- extendCoals[[i]]
    ##
    tmp.back <- 1 ## will indicate backwards prediction
    year <- 2000  ## redundant for secciones/municipios, retained to keep code similar to distritos
    reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
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
    vhat.2000[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.2000$pan [[i]]   <- reg.pan
    regs.2000$left[[i]]   <- reg.left
    regs.2000$oth [[i]]   <- reg.oth
    ##
    ## add slot for projections/estimates if absent
    if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA  
    if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA  
    if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA 
    if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA  
    if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA 
    if ("dbackward" %notin% colnames(data.tmp)) data.tmp$dbackward <- NA
    ##
    data.tmp$vhat.pan [data.tmp$yr==year] <- vhat.pan   ## input vote estimates
    data.tmp$vhat.pri [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left[data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan [data.tmp$yr==year] <- bhat.pan   ## input slope estimates
    data.tmp$bhat.left[data.tmp$yr==year] <- bhat.left
    data.tmp$dbackward[data.tmp$yr==year] <- tmp.back   ## input fwd/back
    ##
    ## return estimates to data object
    extendCoals[[i]] <- data.tmp
}
##################
## save to disk ##
##################
save(regs.2000, file = paste(wd, "data/too-big-4-github/dipfed-seccion-regs-2000.RData", sep = ""), compress = "gzip")
## clean memory
rm(regs.2000)


## #########################################
## backwards-predict 2003 with next 5 els ##
## #########################################
for (i in non.nas){
    ##i <- 44508 # debug
    message(sprintf("2003: loop %s of %s", i, max(non.nas)))
    ##
    ## subset data to single unit
    data.tmp <- extendCoals[[i]]
    ##
    tmp.back <- 1 ## will indicate backwards prediction
    year <- 2003  ## redundant for secciones/municipios, retained to keep code similar to distritos
    reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
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
    vhat.2003[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.2003$pan [[i]]   <- reg.pan
    regs.2003$left[[i]]   <- reg.left
    regs.2003$oth [[i]]   <- reg.oth
    ##
    ## add slot for projections/estimates if absent
    if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA  
    if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA  
    if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA 
    if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA  
    if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA 
    if ("dbackward" %notin% colnames(data.tmp)) data.tmp$dbackward <- NA
    ##
    data.tmp$vhat.pan [data.tmp$yr==year] <- vhat.pan   ## input vote estimates
    data.tmp$vhat.pri [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left[data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan [data.tmp$yr==year] <- bhat.pan   ## input slope estimates
    data.tmp$bhat.left[data.tmp$yr==year] <- bhat.left
    data.tmp$dbackward[data.tmp$yr==year] <- tmp.back   ## input fwd/back
    ##
    ## return estimates to data object
    extendCoals[[i]] <- data.tmp
}
##################
## save to disk ##
##################
save(regs.2003, file = paste(wd, "data/too-big-4-github/dipfed-seccion-regs-2003.RData", sep = ""), compress = "gzip")
## clean memory
rm(regs.2003)


## #########################################
## backwards-predict 2006 with next 5 els ##
## #########################################
for (i in non.nas){
    ##i <- 44508 # debug
    message(sprintf("2006: loop %s of %s", i, max(non.nas)))
    ##
    ## subset data to single unit
    data.tmp <- extendCoals[[i]]
    ##
    tmp.back <- 1 ## will indicate backwards prediction
    year <- 2006  ## redundant for secciones/municipios, retained to keep code similar to distritos
    reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
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
    vhat.2006[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.2006$pan [[i]]   <- reg.pan
    regs.2006$left[[i]]   <- reg.left
    regs.2006$oth [[i]]   <- reg.oth
    ##
    ## add slot for projections/estimates if absent
    if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA  
    if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA  
    if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA 
    if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA  
    if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA 
    if ("dbackward" %notin% colnames(data.tmp)) data.tmp$dbackward <- NA
    ##
    data.tmp$vhat.pan [data.tmp$yr==year] <- vhat.pan   ## input vote estimates
    data.tmp$vhat.pri [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left[data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan [data.tmp$yr==year] <- bhat.pan   ## input slope estimates
    data.tmp$bhat.left[data.tmp$yr==year] <- bhat.left
    data.tmp$dbackward[data.tmp$yr==year] <- tmp.back   ## input fwd/back
    ##
    ## return estimates to data object
    extendCoals[[i]] <- data.tmp
}
##################
## save to disk ##
##################
save(regs.2006, file = paste(wd, "data/too-big-4-github/dipfed-seccion-regs-2006.RData", sep = ""), compress = "gzip")
## clean memory
rm(regs.2006)


## ###############################
## predict 2009 with last 5 els ##
## ###############################
for (i in non.nas){
    ##i <- 44508 # debug
    message(sprintf("2009: loop %s of %s", i, max(non.nas)))
    ##
    ## subset data to single unit
    data.tmp <- extendCoals[[i]]
    ##
    tmp.back <- 0 ## will indicate backwards prediction
    year <- 2009  ## redundant for secciones/municipios, retained to keep code similar to distritos
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
    vhat.2009[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.2009$pan [[i]]   <- reg.pan
    regs.2009$left[[i]]   <- reg.left
    regs.2009$oth [[i]]   <- reg.oth
    ##
    ## add slot for projections/estimates if absent
    if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA  
    if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA  
    if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA 
    if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA  
    if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA 
    if ("dbackward" %notin% colnames(data.tmp)) data.tmp$dbackward <- NA
    ##
    data.tmp$vhat.pan [data.tmp$yr==year] <- vhat.pan   ## input vote estimates
    data.tmp$vhat.pri [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left[data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan [data.tmp$yr==year] <- bhat.pan   ## input slope estimates
    data.tmp$bhat.left[data.tmp$yr==year] <- bhat.left
    data.tmp$dbackward[data.tmp$yr==year] <- tmp.back   ## input fwd/back
    ##
    ## return estimates to data object
    extendCoals[[i]] <- data.tmp
}
##################
## save to disk ##
##################
save(regs.2009, file = paste(wd, "data/too-big-4-github/dipfed-seccion-regs-2009.RData", sep = ""), compress = "gzip")
## clean memory
rm(regs.2009)


## ###############################
## predict 2012 with last 5 els ##
## ###############################
for (i in non.nas){
    ##i <- 44508 # debug
    message(sprintf("2012: loop %s of %s", i, max(non.nas)))
    ##
    ## subset data to single unit
    data.tmp <- extendCoals[[i]]
    ##
    tmp.back <- 0 ## will indicate backwards prediction
    year <- 2012  ## redundant for secciones/municipios, retained to keep code similar to distritos
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
    vhat.2012[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.2012$pan [[i]]   <- reg.pan
    regs.2012$left[[i]]   <- reg.left
    regs.2012$oth [[i]]   <- reg.oth
    ##
    ## add slot for projections/estimates if absent
    if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA  
    if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA  
    if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA 
    if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA  
    if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA 
    if ("dbackward" %notin% colnames(data.tmp)) data.tmp$dbackward <- NA
    ##
    data.tmp$vhat.pan [data.tmp$yr==year] <- vhat.pan   ## input vote estimates
    data.tmp$vhat.pri [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left[data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan [data.tmp$yr==year] <- bhat.pan   ## input slope estimates
    data.tmp$bhat.left[data.tmp$yr==year] <- bhat.left
    data.tmp$dbackward[data.tmp$yr==year] <- tmp.back   ## input fwd/back
    ##
    ## return estimates to data object
    extendCoals[[i]] <- data.tmp
}
##################
## save to disk ##
##################
save(regs.2012, file = paste(wd, "data/too-big-4-github/dipfed-seccion-regs-2012.RData", sep = ""), compress = "gzip")
## clean memory
rm(regs.2012)


## ###############################
## predict 2015 with last 5 els ##
## ###############################
for (i in non.nas){
    ##i <- 44508 # debug
    message(sprintf("2015: loop %s of %s", i, max(non.nas)))
    ##
    ## subset data to single unit
    data.tmp <- extendCoals[[i]]
    ##
    tmp.back <- 0 ## will indicate backwards prediction
    year <- 2015  ## redundant for secciones/municipios, retained to keep code similar to distritos
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
    vhat.2015[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.2015$pan [[i]]   <- reg.pan
    regs.2015$left[[i]]   <- reg.left
    regs.2015$oth [[i]]   <- reg.oth
    ##
    ## add slot for projections/estimates if absent
    if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA  
    if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA  
    if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA 
    if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA  
    if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA 
    if ("dbackward" %notin% colnames(data.tmp)) data.tmp$dbackward <- NA
    ##
    data.tmp$vhat.pan [data.tmp$yr==year] <- vhat.pan   ## input vote estimates
    data.tmp$vhat.pri [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left[data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan [data.tmp$yr==year] <- bhat.pan   ## input slope estimates
    data.tmp$bhat.left[data.tmp$yr==year] <- bhat.left
    data.tmp$dbackward[data.tmp$yr==year] <- tmp.back   ## input fwd/back
    ##
    ## return estimates to data object
    extendCoals[[i]] <- data.tmp
}
##################
## save to disk ##
##################
save(regs.2015, file = paste(wd, "data/too-big-4-github/dipfed-seccion-regs-2015.RData", sep = ""), compress = "gzip")
## clean memory
rm(regs.2015)


## ###############################
## predict 2018 with last 5 els ##
## ###############################
for (i in non.nas){
    ##i <- 44508 # debug
    message(sprintf("2018: loop %s of %s", i, max(non.nas)))
    ##
    ## subset data to single unit
    data.tmp <- extendCoals[[i]]
    ##
    tmp.back <- 0 ## will indicate backwards prediction
    year <- 2018  ## redundant for secciones/municipios, retained to keep code similar to distritos
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
    ##
    ## add slot for projections/estimates if absent
    if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA  
    if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA  
    if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA 
    if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA  
    if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA 
    if ("dbackward" %notin% colnames(data.tmp)) data.tmp$dbackward <- NA
    ##
    data.tmp$vhat.pan [data.tmp$yr==year] <- vhat.pan   ## input vote estimates
    data.tmp$vhat.pri [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left[data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan [data.tmp$yr==year] <- bhat.pan   ## input slope estimates
    data.tmp$bhat.left[data.tmp$yr==year] <- bhat.left
    data.tmp$dbackward[data.tmp$yr==year] <- tmp.back   ## input fwd/back
    ##
    ## return estimates to data object
    extendCoals[[i]] <- data.tmp
}
##################
## save to disk ##
##################
save(regs.2018, file = paste(wd, "data/too-big-4-github/dipfed-seccion-regs-2018.RData", sep = ""), compress = "gzip")
## clean memory
rm(regs.2018)


## ###############################
## predict 2021 with last 5 els ##
## ###############################
for (i in non.nas){
    ##i <- 44508 # debug
    message(sprintf("2021: loop %s of %s", i, max(non.nas)))
    ##
    ## subset data to single unit
    data.tmp <- extendCoals[[i]]
    ##
    tmp.back <- 0 ## will indicate backwards prediction
    year <- 2021  ## redundant for secciones/municipios, retained to keep code similar to distritos
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
    vhat.2021[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.2021$pan [[i]]   <- reg.pan
    regs.2021$left[[i]]   <- reg.left
    regs.2021$oth [[i]]   <- reg.oth
    ##
    ## add slot for projections/estimates if absent
    if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA  
    if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA  
    if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA 
    if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA  
    if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA 
    if ("dbackward" %notin% colnames(data.tmp)) data.tmp$dbackward <- NA
    ##
    data.tmp$vhat.pan [data.tmp$yr==year] <- vhat.pan   ## input vote estimates
    data.tmp$vhat.pri [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left[data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan [data.tmp$yr==year] <- bhat.pan   ## input slope estimates
    data.tmp$bhat.left[data.tmp$yr==year] <- bhat.left
    data.tmp$dbackward[data.tmp$yr==year] <- tmp.back   ## input fwd/back
    ##
    ## return estimates to data object
    extendCoals[[i]] <- data.tmp
}
##################
## save to disk ##
##################
save(regs.2021, file = paste(wd, "data/too-big-4-github/dipfed-seccion-regs-2021.RData", sep = ""), compress = "gzip")
## clean memory
rm(regs.2021)


## ###############################
## predict 2024 with last 5 els ##
## ###############################
for (i in non.nas){
    ##i <- 44508 # debug
    message(sprintf("2024: loop %s of %s", i, max(non.nas)))
    ##
    ## subset data to single unit
    data.tmp <- extendCoals[[i]]
    ##
    tmp.back <- 0 ## will indicate backwards prediction
    year <- 2024  ## redundant for secciones/municipios, retained to keep code similar to distritos
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
    vhat.2024[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.2024$pan [[i]]   <- reg.pan
    regs.2024$left[[i]]   <- reg.left
    regs.2024$oth [[i]]   <- reg.oth
    ##
    ## add slot for projections/estimates if absent
    if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA  
    if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA  
    if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA 
    if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA  
    if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA 
    ##
    data.tmp$vhat.pan [data.tmp$yr==year] <- vhat.pan   ## input vote estimates
    data.tmp$vhat.pri [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left[data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan [data.tmp$yr==year] <- bhat.pan   ## input slope estimates
    data.tmp$bhat.left[data.tmp$yr==year] <- bhat.left
    data.tmp$dbackward[data.tmp$yr==year] <- tmp.back   ## input fwd/back
    ##
    ## return estimates to data object
    extendCoals[[i]] <- data.tmp
}
##################
## save to disk ##
##################
save(regs.2024, file = paste(wd, "data/too-big-4-github/dipfed-seccion-regs-2024.RData", sep = ""), compress = "gzip")
## clean memory
rm(regs.2024)


## #####################################################
## warnings correspond to units with no variance      ##
## (eg. period mean in new municipio created in 2017) ##
## #####################################################
## 


## clean (all this is saved in extendCoal, mean.regs, regs.1988 ... regs.2024)
ls()
rm(alphahat, betahat, cs, per.means, yr.means,
vhat.1991, vhat.1994,
vhat.1997, vhat.2000, vhat.2003, vhat.2006,
vhat.2009, vhat.2012, vhat.2015,
vhat.2018, vhat.2021, vhat.2024
)
rm(
bhat.pan, bhat.left,
reg.pan, reg.left, reg.oth,
rhat.pan, rhat.left, rhat.oth,
vhat.pan, vhat.pri, vhat.left,
new.d, year
)
rm(i, d2, non.nas, pb, nsec, sel.c, sel.r)


####################################################
## sort/clean seccion standard columns for export ##
####################################################
tmp <- lapply(extendCoals, function(x) {
    x$oth <- NULL;
    x <- x[, c("yr", "pan", "pri", "left", "efec", "lisnom", "seccion", "d.pan", "d.pri", "d.left", "vhat.pan", "vhat.pri", "vhat.left", "bhat.pan", "bhat.left", "alphahat.pan", "alphahat.pri", "alphahat.left", "betahat.pan", "betahat.left", "dbackward")] # order
    }
)
extendCoals <- tmp

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

##################
## save to disk ##
##################
write.csv(out.y1991,
          file = paste(wd, "data/sec/dipfed-seccion-vhat-1991.csv", sep = ""), row.names = FALSE)
##
write.csv(out.y1994,
          file = paste(wd, "data/sec/dipfed-seccion-vhat-1994.csv", sep = ""), row.names = FALSE)
##
write.csv(out.y1997,
          file = paste(wd, "data/sec/dipfed-seccion-vhat-1997.csv", sep = ""), row.names = FALSE)
##
write.csv(out.y2000,
          file = paste(wd, "data/sec/dipfed-seccion-vhat-2000.csv", sep = ""), row.names = FALSE)
##
write.csv(out.y2003,
          file = paste(wd, "data/sec/dipfed-seccion-vhat-2003.csv", sep = ""), row.names = FALSE)
##
write.csv(out.y2006,
          file = paste(wd, "data/sec/dipfed-seccion-vhat-2006.csv", sep = ""), row.names = FALSE)
##
write.csv(out.y2009,
          file = paste(wd, "data/sec/dipfed-seccion-vhat-2009.csv", sep = ""), row.names = FALSE)
##
write.csv(out.y2012,
          file = paste(wd, "data/sec/dipfed-seccion-vhat-2012.csv", sep = ""), row.names = FALSE)
##
write.csv(out.y2015,
          file = paste(wd, "data/sec/dipfed-seccion-vhat-2015.csv", sep = ""), row.names = FALSE)
##
write.csv(out.y2018,
          file = paste(wd, "data/sec/dipfed-seccion-vhat-2018.csv", sep = ""), row.names = FALSE)
##
write.csv(out.y2021,
          file = paste(wd, "data/sec/dipfed-seccion-vhat-2021.csv", sep = ""), row.names = FALSE)
##
write.csv(out.y2024,
          file = paste(wd, "data/sec/dipfed-seccion-vhat-2024.csv", sep = ""), row.names = FALSE)

###########
## clean ##
###########
ls()
rm(
##    out.y1988,
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
    out.y2024,
    for.export
)
rm(
    tmp, tmp.back
)
rm(
    extendCoals
)
##ls()

        
## ###########################
## ## inspect saved objects ##
## ###########################
## # load regression object
## load(file = paste(wd, "data/dipfed-seccion-regs-2006.RData", sep = ""))
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

