## Code is a replica, updated, of /home/eric/Downloads/Desktop/MXelsCalendGovt/atlasDis/code/mapPrep.r
## Prepares various seccion-level measures of party performance in federal diputado elections
## retrieves electoral info from data in another repository: github.com/emagar/elecRturns

## esto prepara los datos electorales para mapear los distritos de cada estado.
## el contenido queda guardado en un archivo de datos.
## sólo tiene que correrse en caso de cambios.
rm(list=ls())
options(width = 110)
#
dd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/")   # data
md <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/ife.ine/")          # maps
sd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/v-hats-etc/") # save
setwd(dd)

## ## cuando use mapas deberé cambiar por cartografía 2017 en
## ## /home/eric/Downloads/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/fed/shp/disfed2018
## ## o cartografía 2020   <---  OJO
## md <- c("/home/eric/Dropbox/data/mapas/cartografia28feb2013rojano/")

##############################
## Define several functions ##
##############################
##
##################################################################
## function to aggregate casilla-level votes into higher levels ##
##################################################################
my_agg <- function(d=d, sel.c=sel.c, by=NA, y1991=FALSE, drop.dupli=TRUE){
    d <- d              # assign mem
    sel.c <- sel.c      # assign mem
    if (is.na(by)==TRUE){ # default is aggregating secciones when by==NA
        if (y1991==FALSE){
            by <- as.factor(d$edon*10000+d$seccion) # seccion indicator post 1991
        } else {
            by <- as.factor(d$ife*1000000+d$disn*10000+d$seccion) # seccion indicator 1991
        }
    } else { # to aggregate by something else (e.g. dis1979 or disn or ife)
        by <- d[,which(colnames(d) %in% by)] # duplicate column that will serve as aggregator
        by <- as.factor(by)
    }
    sel.c <- which(colnames(d) %in% sel.c); # extract indices of selected columns
    for (i in sel.c){
        #i <- sel.c[1] #debug
        d[,i] <- ave(d[,i], by, FUN=function(x) sum(x, na.rm=TRUE)) # sum up
    }
    if (drop.dupli==TRUE){
        sel.r <- which(duplicated(by)==TRUE)         # drop duplicate obs
        d <- d[-sel.r,]
    }
    return(d)
}
##########################################################################
## function to replace character/factor cells with numeric value or NA; ##
## then chg NAs w 0s                                                    ##
##########################################################################
to.num <- function(d = d, sel.c = sel.c){
    sel.c <- which(colnames(d) %in% sel.c); # extract indices
    tmp.d <- d[,sel.c];                     # subset data for manipulation
    # filter non-mumeric w chars then numeric: if not 0-9 will become NAs 
    tmp.d <- sapply(tmp.d, function(x){     
        if (is.numeric(x)) x else suppressWarnings(as.numeric(as.character(x)))
    });
    tmp.d[is.na(tmp.d)] <- 0; # change resulting NAs with zero
    d[,sel.c] <- tmp.d;       # return manipulated data
    return(d)
}
##################################################################################
## function splitting coalition vote in proportion to votes won by each in unit ##
##################################################################################
apportion_v <- function(dat=NA, members=NA, joint=NA){
    if (length(members)==0 | length(joint)) stop
    dummy <- paste0("d",joint) # dummy indicating coalition in unit
    sel.r <- which(dat[,dummy]==1 & dat[,joint]>0) # rows with a coalition and non-zero joint vote
    sel.c <- which(colnames(dat) %in% members)   # target columns
    tmp_v <- dat[sel.r,sel.c]              # subset individual contribs for manipulation
    tmp_j <- dat[sel.r,joint]              # subset joint votes for manipulation
    tmp_w <- tmp_v / rowSums(tmp_v)      # weights
    tmp_w[is.na(tmp_w)] <- 1/ncol(tmp_w) # indeterminates to even split (eg. pt=0 mc=0 pt.mc=10)
    tmp_m <- round(tmp_v + tmp_j * tmp_w, 1) # manipulated version
    #
    dat[sel.r,sel.c] <- tmp_m # return manipulation to member's columns
    dat[sel.r,joint] <- 0     # joint vote to zero
    return(dat)
}
###############################################
## function to fill sel.c column NAs to zero ##
###############################################
na2zero <- function(dat=NA, sel.c){
    tmp <- dat[,sel.c] # subset for manipulation
    tmp[is.na(tmp)] <- 0
    dat[,sel.c] <- tmp # return manipulation to data
    return(dat)
}
##############################################################################
## function to sort one data frame by order of another, matching data frame ##
##############################################################################
source("/home/eric/Dropbox/data/useful-functions/sortBy.r")
#######################
## 'not in' function ##
#######################
source("/home/eric/Dropbox/data/useful-functions/notin.r")
##########################
## 'inegi2ife' function ##
##########################
source("/home/eric/Dropbox/data/useful-functions/inegi2ife.r")
#################################
## function to sort df columns ##
#################################
source("/home/eric/Dropbox/data/useful-functions/moveme.r")



###################################
## ############################# ##
## ## read casilla-level data ## ##
## ############################# ##
###################################
##
##########
## 1991 ## OJO: 1991 seccion ids incomparable to later ones, yet possible to aggregate disn/ife (no counterfactuals, though)
##########
d <- read.csv("dip1991.csv", header=TRUE, stringsAsFactors=FALSE)
d <- d[order(d$edon, d$seccion),]
# vote columns selector
sel.c <-            c("pan","pri","pps","prd","pfcrn","parm","pdm","prt","pem","pt","efec","lisnom")
# clean data
#table(d$note)
#table(d$status)
d <- to.num(d,sel.c)                                                               # numericize
d <- na2zero(dat=d)                                                                # NAs to zero in sel.c
d$dextra <- 0; d$dextra[grep("extraordinaria", d$note)] <- 1                       # special elections
sel.r <- which(d$status=="Anulada"); d[sel.r,sel.c] <- 0                           # voided casillas to zero
d <- within(d, efec <- pan + pri + pps + prd + pfcrn + parm + pdm + prt + pem + pt)# valid vote
d <- within(d, tot <- nul <- nr <- note <- status <- casilla <- NULL)              # economize columns
## aggregate seccion-level votes ##
d <- my_agg(d=d, sel.c=sel.c, by=NA, y1991=TRUE) #ag.sec(d, sel.c)
## ## coalition dummies
## d <- within(d, dpanc <- dpric <- dprdc <- 0)
##############################################
## rename vote returns objects for analysis ##
##############################################
v91_agg <- v91_split <- d
##
##########
## 1994 ##
##########
d <- read.csv("dip1994.csv", header=TRUE, stringsAsFactors=FALSE)
## OJO: 1994 has inegi instead of ife municipal codes
d$ife <- inegi2ife(d$inegi)
d <- d[moveme(names(d), "ife before inegi")]
d$inegi <- NULL
d <- d[order(d$edon, d$seccion),] # sort
# vote columns selector
sel.c <-            c("pan","pri","pps","prd","pfcrn","parm","uno.pdm","pt","pvem","efec","lisnom")
# clean data
#table(d$status)
#table(d$nota)
d <- to.num(d,sel.c)                                                               # numericize
d <- na2zero(dat=d)                                                                # NAs to zero in sel.c
d$dextra <- 0; d$dextra[grep("extraordinaria", d$nota)] <- 1                       # special elections
sel.r <- which(d$status=="Anulada"); d[sel.r,sel.c] <- 0                           # voided casillas to zero
d <- within(d, efec <- pan + pri + pps + prd + pfcrn + parm + uno.pdm + pt + pvem) # valid vote
d <- within(d, tot <- nul <- nr <- nota <- status <- casilla <- NULL)              # economize columns
## aggregate seccion-level votes ##
d <- my_agg(d=d, sel.c=sel.c, by=NA, y1991=FALSE) #ag.sec(d, sel.c)
## ## coalition dummies
## d <- within(d, dpanc <- dpric <- dprdc <- 0)
##############################################
## rename vote returns objects for analysis ##
##############################################
v94_agg <- v94_split <- d
##
##########
## 1997 ##
##########
d <- read.csv("dip1997.csv", header=TRUE, stringsAsFactors=FALSE)
d <- d[order(d$edon, d$seccion),] # sort
# vote columns selector
sel.c <-            c("pan","pri","prd","pc","pt","pvem","pps","pdm","efec","lisnom")
# clean
#table(d$status)
#table(d$note)
d <- to.num(d,sel.c)                                                               # numericize
d <- na2zero(dat=d)                                                                # NAs to zero in sel.c
d$dextra <- 0                                                                      # no special elections this year
d <- within(d, efec <- pan + pri + prd + pc + pt + pvem + pps + pdm)               # valid vote
sel.r <- which(d$status=="Anulada"); d[sel.r,sel.c] <- 0                           # voided casillas to zero
d <- within(d, tot <- nul <- nr <- status <- note <- casilla <- NULL)              # economize cols
## aggregate seccion-level votes ##
d <- my_agg(d=d, sel.c=sel.c, by=NA, y1991=FALSE) #ag.sec(d, sel.c)
## ## coalition dummies
## d <- within(d, dpanc <- dpric <- dprdc <- 0)
##############################################
## rename vote returns objects for analysis ##
##############################################
v97_agg <- v97_split <- d
##
##########
## 2000 ##
##########
d <- read.csv("dip2000.csv", header=TRUE, stringsAsFactors=FALSE)
d <- d[order(d$edon, d$seccion),] # sort
# vote columns selector
colnames(d)[which(colnames(d)=="pan.pvem")]             <- "panc"
colnames(d)[which(colnames(d)=="prd.pt.conve.pas.psn")] <- "prdc"
sel.c <-            c("panc","pri","prdc","pcd","parm","dsppn","efec","lisnom")
# clean data
#table(d$status)
d <- to.num(d,sel.c)                                                               # numericize
d <- na2zero(dat=d)                                                                # NAs to zero in sel.c
d$dextra <- 0                                                                      # no special elections this year
sel.r <- which(d$status=="Anulada"); d[sel.r,sel.c] <- 0                           # voided casillas to zero
d <- within(d, efec <- panc + pri + prdc + pcd + parm + dsppn)                     # valid vote
d <- within(d, tot <- nul <- nr <- status <- nota <- casilla <- NULL)              # economize columns
## aggregate seccion-level votes ##
d <- my_agg(d=d, sel.c=sel.c, by=NA, y1991=FALSE) #ag.sec(d, sel.c)
# district coalition dummies
d$dpanc <- ave(d$panc, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dpanc <- as.numeric(d$dpanc>0)
## d$dpric <- 0
d$dprdc <- ave(d$prdc, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dprdc <- as.numeric(d$dprdc>0)
table(d$dpanc, useNA = "always")
table(d$dprdc, useNA = "always")
##############################################
## rename vote returns objects for analysis ##
##############################################
v00_agg <- d
##
##########
## 2003 ##
##########
d <- read.csv( "dip2003.csv", header=TRUE, , stringsAsFactors=FALSE)
d <- d[order(d$edon, d$seccion),] # sort
# vote columns selector
colnames(d)[which(colnames(d)=="pri.pvem")] <- "pric" # "pri-pvem"
sel.c <-            c("pan","pri","prd","pt","pvem","conve","psn","pas","mp","plm","fc","pric","efec","lisnom")
# clean
#table(d$status)
d <- to.num(d,sel.c)                                                               # numericize
d <- na2zero(dat=d)                                                                # NAs to zero in sel.c
d$dextra <- as.numeric(d$nota=="eleccion extraordinaria 2004")                     # special elections
sel.r <- grep("Anulada|instalada|entregado", d$status); d[sel.r,sel.c] <- 0        # voided casillas to zero
d <- within(d, efec <- pan + pri + prd + pt + pvem + conve + psn + pas + mp + plm + fc + pric) # valid vote
d <- within(d, tot <- nul <- nr <- nota <- status <- casilla <- NULL)              # economize columns
## aggregate seccion-level votes ##
d <- my_agg(d=d, sel.c=sel.c, by=NA, y1991=FALSE) #ag.sec(d, sel.c)
# district coalition dummies
## d$dpanc <- 0
d$dpric <- ave(d$pric, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dpric <- as.numeric(d$dpric>0)
## d$dprdc <- 0
table(d$dpric, useNA = "always")
##############################################
## rename vote returns objects for analysis ##
##############################################
v03_agg <- d
##
##########
## 2006 ##
##########
d <- read.csv( "dip2006.csv", header=TRUE, , stringsAsFactors=FALSE)
d <- d[order(d$edon, d$seccion),] # sort
# vote columns selector
colnames(d)[which(colnames(d)=="pri.pvem")]     <- "pric"
colnames(d)[which(colnames(d)=="prd.pt.conve")] <- "prdc"
sel.c <-            c("pan","pric","prdc","pna","asdc","efec","lisnom")
#
# clean
#table(d$status)
d <- to.num(d,sel.c)                                                               # numericize
d <- na2zero(dat=d)                                                                # NAs to zero in sel.c
d$dextra <- 0                                                                      # no special elections this year
sel.r <- grep("Anulada|instalada", d$status); d[sel.r,sel.c] <- 0                  # voided casillas to zero
d <- within(d, efec <- pan + pric + prdc + pna + asdc)                             # valid vote
d <- within(d, tot <- nul <- nr <- status <- casilla <- NULL)                      # economize columns
## aggregate seccion-level votes ##
d <- my_agg(d=d, sel.c=sel.c, by=NA, y1991=FALSE) #ag.sec(d, sel.c)
## coalition dummies
d <- within(d, {
##    dpanc <- 0;
    dpric <- dprdc <- 1
})
##############################################
## rename vote returns objects for analysis ##
##############################################
v06_agg <- d
##
##########
## 2009 ##
##########
d <- read.csv( "dip2009.csv", header=TRUE, , stringsAsFactors=FALSE)
d <- d[order(d$edon, d$seccion),] # sort
# vote columns selector
colnames(d)[which(colnames(d)=="pri.pvem")] <- "pric"
colnames(d)[which(colnames(d)=="pt.conve")] <- "ptc"
sel.c <-            c("pan","pri","prd","pvem","pt","conve","pna","psd","pric","ptc","efec","lisnom")
# clean data
#table(d$status)
#table(d$tepjf)
d <- to.num(d,sel.c)                                                               # numericize
d$dextra <- 0                                                                      # no special elections this year
sel.r <- grep("instalada|entregado|sin acta", d$status, ignore.case = TRUE)        # voided casillas to zero
sel.r <- c(sel.r, grep("Anulada",             d$tepjf, ignore.case = TRUE))        # voided casillas to zero
d[sel.r,sel.c] <- 0                                                                # voided casillas to zero
d <- within(d, efec <- pan + pri + prd + pvem + pt + conve + pna + psd + pric + ptc) # valid vote
d <- within(d, tot <- nul <- nr <- tepjf <- status <- casilla <- NULL)             # economize columns
## aggregate seccion-level votes ##
d <- my_agg(d=d, sel.c=sel.c, by=NA, y1991=FALSE) #ag.sec(d, sel.c)
# district coalition dummies
## d$dpanc <- 0
d$dpric <- ave(d$pric, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dpric <- as.numeric(d$dpric>0)
## d$dprdc <- 0
d$dptc <- ave(d$ptc, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE)   # if >0 will infer district coalition
d$dptc <- as.numeric(d$dptc>0)
table(d$dpric, useNA = "always")
table(d$dptc, useNA = "always")
#####################################################################
## aggregate coalitions where present for correct winner assesment ##
#####################################################################
d_agg <- d # duplicate for manipulation
sel.r <- which(d_agg$dpric==1)
d_agg[sel.r,] <- within(d_agg[sel.r,], {
    pric <- pri + pric + pvem;
    pri <- pvem <- 0;
})
sel.r <- which(d_agg$dptc==1)
d_agg[sel.r,] <- within(d_agg[sel.r,], {
    ptc <- pt + conve + ptc;
    pt <- conve <- 0;
})
# ptc coal across the board
d_agg$pt <- d_agg$conve <- NULL
#############################################################
## split coalition vote according to contributions in unit ##
#############################################################
d_split <- d # duplicate for manipulation
d_split <- apportion_v(dat=d_split, members=c("pri","pvem"), joint="pric")
d_split <- apportion_v(dat=d_split, members=c("pt","conve"), joint="ptc")
##############################################
## rename vote returns objects for analysis ##
##############################################
v09_agg <- d_agg
v09_split <- d_split
##
##########
## 2012 ##
##########
d <- read.csv( "dip2012.csv", header=TRUE, , stringsAsFactors=FALSE)
d <- d[order(d$edon, d$seccion),] # sort
# vote columns selector
colnames(d)[which(colnames(d)=="pri.pvem")]  <- "pric"
colnames(d)[which(colnames(d)=="prd.pt.mc")] <- "prdc"
sel.c <-            c("pan","pri","prd","pvem","pt","mc","pna","pric","prdc","efec","lisnom")
# clean data
#table(d$status)
#table(d$tepjf)
#table(d$nota)
d <- to.num(d,sel.c)                                                               # numericize
d <- na2zero(dat=d)                                                                # NAs to zero in sel.c
d$dextra <- 0                                                                      # no special elections this year
sel.r <- grep("Anulada", d$tepjf, ignore.case = TRUE)                              # voided casillas to zero
sel.r <- c(sel.r, grep("instalada|entregado", d$status, ignore.case = TRUE))       # voided casillas to zero
d[sel.r,sel.c] <- 0                                                                # voided casillas to zero
d <- within(d, efec <- pan + pri + prd + pvem + pt + mc + pna + pric + prdc)       # valid vote
d <- within(d, tot <- nul <- nr <- status <- tepjf <- casilla <- nota <- NULL)     # economize columns
## aggregate seccion-level votes ##
d <- my_agg(d=d, sel.c=sel.c, by=NA, y1991=FALSE) #ag.sec(d, sel.c)
# coalition dummies
## d$dpanc <- 0
d$dpric <- ave(d$pric, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dpric <- as.numeric(d$dpric>0)
d$dprdc <- ave(d$prdc, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE)   # if >0 will infer district coalition
d$dprdc <- as.numeric(d$dprdc>0)
table(d$dpric, useNA = "always")
table(d$dprdc, useNA = "always")
#####################################################################
## aggregate coalitions where present for correct winner assesment ##
#####################################################################
d_agg <- d # duplicate for manipulation
sel.r <- which(d_agg$dpric==1)
d_agg[sel.r,] <- within(d_agg[sel.r,], {
    pric <- pri + pric + pvem;
    pri <- pvem <- 0;
})
sel.r <- which(d_agg$dprdc==1)
d_agg[sel.r,] <- within(d_agg[sel.r,], {
    prdc <- prd + pt + mc + prdc;
    prd <- pt <- mc <- 0;
})
# prdc across the board
d_agg$prd <- d_agg$pt <- d_agg$mc <- NULL
#############################################################
## split coalition vote according to contributions in unit ##
#############################################################
d_split <- d # duplicate for manipulation
d_split <- apportion_v(dat=d_split, members=c("pri","pvem"), joint="pric")
d_split <- apportion_v(dat=d_split, members=c("prd", "pt","mc"), joint="prdc")
##############################################
## rename vote returns objects for analysis ##
##############################################
v12_agg <- d_agg
v12_split <- d_split
##
##########
## 2015 ##
##########
d <- read.csv( "dip2015.csv", header=TRUE, , stringsAsFactors=FALSE)
d <- d[order(d$edon, d$seccion),] # sort
# vote columns selector
colnames(d)[which(colnames(d)=="pan.pna")]  <- "panc"
colnames(d)[which(colnames(d)=="pri.pvem")] <- "pric"
colnames(d)[which(colnames(d)=="prd.pt")]   <- "prdc"
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","ph","pes","panc","pric","prdc","indep1","indep2","efec","lisnom")
# clean data
#table(d$status)
#table(d$tepjf)
#table(d$nota)
d <- to.num(d,sel.c)                                                               # numericize
d <- na2zero(dat=d)                                                                # NAs to zero in sel.c
d$dextra <- 0; d$dextra[grep("extraordinaria", d$nota)] <- 1                       # special elections
sel.r <- grep("suspensión|instalada|entregado", d$status, ignore.case = TRUE)      # voided casillas to zero
sel.r <- c(sel.r, grep("Anulada", d$tepjf, ignore.case = TRUE))                    # voided casillas to zero
d[sel.r,sel.c] <- 0                                                                # voided casillas to zero
d <- within(d, efec <- pan + pri + prd + pvem + pt + mc + pna + morena + ph + pes + panc + pric + prdc + indep1 + indep2) # valid votes
d <- within(d, tot <- nul <- nr <- status <- tepjf <- nota <- casilla <- NULL)     # economize columns
# district coalition dummies
d$dpanc <- ave(d$panc, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE)
d$dpanc <- as.numeric(d$dpanc>0)
d$dpric <- ave(d$pric, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dpric <- as.numeric(d$dpric>0)
d$dprdc <- ave(d$prdc, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dprdc <- as.numeric(d$dprdc>0)
## d$dmorenac <- 0
table(d$dpanc, useNA = "always")
table(d$dpric, useNA = "always")
table(d$dprdc, useNA = "always")
## aggregate seccion-level votes ##
d <- my_agg(d=d, sel.c=sel.c, by=NA, y1991=FALSE) #ag.sec(d, sel.c)
#####################################################################
## aggregate coalitions where present for correct winner assesment ##
#####################################################################
d_agg <- d # duplicate for manipulation
sel.r <- which(d_agg$dpanc==1)
d_agg[sel.r,] <- within(d_agg[sel.r,], {
    panc <- pan + panc + pna;
    pan <- pna <- 0;
})
sel.r <- which(d_agg$dpric==1)
d_agg[sel.r,] <- within(d_agg[sel.r,], {
    pric <- pri + pric + pvem;
    pri <- pvem <- 0;
})
sel.r <- which(d_agg$dprdc==1)
d_agg[sel.r,] <- within(d_agg[sel.r,], {
    prdc <- prd + pt + prdc;
    prd <- pt <- 0;
})
##############################################################
## split coalition votes according to contributions in unit ##
##############################################################
d_split <- d # duplicate for manipulation
d_split <- apportion_v(dat=d_split, members=c("pan","pna"), joint="panc")
d_split <- apportion_v(dat=d_split, members=c("pri","pvem"), joint="pric")
d_split <- apportion_v(dat=d_split, members=c("prd", "pt"), joint="prdc")
##############################################
## rename vote returns objects for analysis ##
##############################################
v15_agg <- d_agg
v15_split <- d_split
##
##########
## 2018 ##
##########
d <- read.csv( "dip2018.csv", header=TRUE, , stringsAsFactors=FALSE)
d <- d[order(d$edon, d$seccion),] # sort
# vote columns selector
colnames(d)[which(colnames(d)=="pan.prd.mc")]    <- "panc"
colnames(d)[which(colnames(d)=="pri.pvem.pna")]  <- "pric"
colnames(d)[which(colnames(d)=="pt.morena.pes")] <- "morenac"
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","pes","panc","pric","morenac","indep1","indep2","efec","lisnom")
# clean data
table(d$nota)
d <- to.num(d,sel.c)                                                               # numericize
d <- na2zero(dat=d)                                                                # NAs to zero in sel.c
d$dextra <- 0                                                                      # no special elections this year
sel.r <- grep("suspensi.n|no instalada|no entregado", d$nota, ignore.case = TRUE)  # voided casillas to zero
#table(d$nota[sel.r])
d[sel.r,sel.c] <- 0                                                                # voided casillas to zero
d <- within(d, efec <- pan + pri + prd + pvem + pt + mc + pna + morena + pes + panc + pric + morenac + indep1 + indep2) # valid vote
d <- within(d, nr <- nul <- tot <- nota <- casilla  <- NULL)                       # economize columns
# district coalition dummies
d$dpanc <- ave(d$panc, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dpanc <- as.numeric(d$dpanc>0)
d$dpric <- ave(d$pric, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dpric <- as.numeric(d$dpric>0)
#d$dprdc <- d$dpanc
d$dmorenac <- ave(d$morenac, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dmorenac <- as.numeric(d$dmorenac>0)
table(d$dpanc, useNA = "always")
table(d$dpric, useNA = "always")
#table(d$dprdc, useNA = "always")
table(d$dmorenac, useNA = "always")
## aggregate seccion-level votes ##
d <- my_agg(d=d, sel.c=sel.c, by=NA, y1991=FALSE) #ag.sec(d, sel.c)
#####################################################################
## aggregate coalitions where present for correct winner assesment ##
#####################################################################
d_agg <- d # duplicate for manipulation
sel.r <- which(d_agg$dpanc==1)
d_agg[sel.r,] <- within(d_agg[sel.r,], {
    panc <- pan + prd + mc + panc;        # pan-prd-etc vote to panc
    pan <- prd <- mc <- 0;
})
sel.r <- which(d_agg$dpric==1)
d_agg[sel.r,] <- within(d_agg[sel.r,], {
    pric <- pri + pric + pvem + pna;
    pri <- pvem <- pna <- 0;
})
sel.r <- which(d_agg$dmorenac==1)
d_agg[sel.r,] <- within(d_agg[sel.r,], {
    morenac <- morena + pt + pes + morenac;
    morena <- pt <- pes <- 0;
})
#############################################################
## split coalition vote according to contributions in unit ##
#############################################################
d_split <- d # duplicate for manipulation
d_split <- apportion_v(dat=d_split, members=c("pan","prd","mc"), joint="panc")
d_split <- apportion_v(dat=d_split, members=c("pri","pvem","pna"), joint="pric")
d_split <- apportion_v(dat=d_split, members=c("morena", "pt","pes"), joint="morenac")
##############################################
## rename vote returns objects for analysis ##
##############################################
v18_agg <- d_agg
v18_split <- d_split
##
##########
## 2021 ##
##########
d <- read.csv( "dip2021.csv", header=TRUE, , stringsAsFactors=FALSE)
d <- d[order(d$edon, d$seccion),] # sort
# drop voto extranjero
table(d$edon[which(d$seccion==0)])
d <- d[-which(d$seccion==0),]
# vote columns selector
colnames(d)[which(colnames(d)=="pan.pri.prd")]    <- "panc"
colnames(d)[which(colnames(d)=="pvem.pt.morena")] <- "morenac"
sel.c <- c("pan","pri","prd","pvem","pt","mc","morena","pes","rsp","fxm","indep","panc","morenac","efec","lisnom")
# clean data
#table(d$observaciones)
#table(d$tepjf)
d <- to.num(d,sel.c)                                                               # numericize
d <- na2zero(dat=d)                                                                # NAs to zero in sel.c
d$dextra <- 0                                                                      # no special elections this year
sel.r <- grep("anulada", d$tepjf, ignore.case = TRUE)                              # voided casillas to zero
sel.r <- c(sel.r, grep("sin boletas|no recibido|suspensi.n|no instalada|no entregado", d$observaciones, ignore.case = TRUE)) # voided casillas to zero
d[sel.r,sel.c] <- 0                                                                # voided casillas to zero
d <- within(d, efec <- pan + pri + prd + pvem + pt + mc + morena + pes + rsp + fxm + indep + panc + morenac) # valid vote
d <- within(d, nr <- nul <- total <- observaciones <- tepjf <- casilla <- NULL)    # economize columns
# district coalition dummies
d$dpanc <- ave(d$panc, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dpanc <- as.numeric(d$dpanc>0)
d$dmorenac <- ave(d$morenac, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dmorenac <- as.numeric(d$dmorenac>0)
d$dpric <- d$dpanc
#d$dprdc <- d$dpanc
table(d$dpanc, useNA = "always")
table(d$dmorenac, useNA = "always")
## aggregate seccion-level votes ##
d <- my_agg(d=d, sel.c=sel.c, by=NA, y1991=FALSE) #ag.sec(d, sel.c)
#####################################################################
## aggregate coalitions where present for correct winner assesment ##
#####################################################################
d_agg <- d # duplicate for manipulation
sel.r <- which(d_agg$dpanc==1)
d_agg[sel.r,] <- within(d_agg[sel.r,], {
    panc <- pan + pri + prd + panc;       # pan-pri-prd vote to panc
    pan <- pri <- prd <- 0;
})
sel.r <- which(d_agg$dmorenac==1)
d_agg[sel.r,] <- within(d_agg[sel.r,], {
    morenac <- morena + pvem + pt + morenac;
    morena <- pvem <- pt <- 0;
})
#############################################################
## split coalition vote according to contributions in unit ##
#############################################################
d_split <- d # duplicate for manipulation
d_split <- apportion_v(dat=d_split, members=c("pan","pri", "prd"), joint="panc")
d_split <- apportion_v(dat=d_split, members=c("morena", "pvem", "pt"), joint="morenac")
##############################################
## rename vote returns objects for analysis ##
##############################################
v21_agg <- d_agg
v21_split <- d_split
##
##################################
## ADD 2024 HERE WHEN AVAILABLE ##
##################################
##
## clean
rm(d,d_agg,d_split,sel.c,sel.r,to.num,apportion_v,na2zero)

## commented 26mar2023, needs work in 2021 to allocate pan.pri.prd win to largest
## ################################
## ## district winners 1994-2021 ##
## ################################
## #v91d <- v91_agg;
## v94d <- v94_agg; v97d <- v97_agg; v00d <- v00_agg; v03d <- v03_agg; v06d <- v06_agg; v09d <- v09_agg; v12d <- v12_agg; v15d <- v15_agg; v18d <- v18_agg; v21d <- v21_agg
## v94d <- within(v94d, {
##     pan <-   ave(pan,     as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pri <-   ave(pri,     as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pps <-   ave(pps,     as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     prd <-   ave(prd,     as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pfcrn <- ave(pfcrn,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     parm <-  ave(parm,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     uno.pdm<-ave(uno.pdm, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pt <-    ave(pt,      as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pvem <-  ave(pvem,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     efec  <- ave(efec,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
## })
## v94d <- v94d[duplicated(v94d$edon*100 + v94d$disn)==FALSE,]
## v94d <- v94d[order(v94d$edon*100, v94d$disn),]
## #
## v97d <- within(v97d, {
##     pan <-   ave(pan,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pri <-   ave(pri,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     prd <-   ave(prd,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pc <-    ave(pc,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pt <-    ave(pt,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pvem <-  ave(pvem,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pps <-   ave(pps,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pdm <-   ave(pdm,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     efec  <- ave(efec,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
## })
## v97d <- v97d[duplicated(v97d$edon*100 + v97d$disn)==FALSE,]
## v97d <- v97d[order(v97d$edon*100, v97d$disn),]
## #
## v00d <- within(v00d, {
##     panc <-  ave(panc,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pri <-   ave(pri,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     prdc <-  ave(prdc,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pcd <-   ave(pcd,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     parm <-  ave(parm,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     dsppn <- ave(dsppn, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     efec  <- ave(efec,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
## })
## v00d <- v00d[duplicated(v00d$edon*100 + v00d$disn)==FALSE,]
## v00d <- v00d[order(v00d$edon*100, v00d$disn),]
## #
## v03d <- within(v03d, {
##     pan <-   ave(pan,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pri <-   ave(pri,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     prd <-   ave(prd,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pt <-    ave(pt,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pvem <-  ave(pvem,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     conve <- ave(conve, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     psn <-   ave(psn,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pas <-   ave(pas,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     mp <-    ave(mp,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     plm <-   ave(plm,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     fc <-    ave(fc,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pric <-  ave(pric,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     efec  <- ave(efec,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
## })
## v03d <- v03d[duplicated(v03d$edon*100 + v03d$disn)==FALSE,]
## v03d <- v03d[order(v03d$edon*100, v03d$disn),]
## #
## v06d <- within(v06d, {
##     pan   <- ave(pan,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pric  <- ave(pric, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     prdc  <- ave(prdc, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pna   <- ave(pna,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     asdc  <- ave(asdc, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     efec  <- ave(efec, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
## })
## v06d <- v06d[duplicated(v06d$edon*100 + v06d$disn)==FALSE,]
## v06d <- v06d[order(v06d$edon*100, v06d$disn),]
## #
## v09d <- within(v09d, {
##     pan  <- ave(pan,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pri  <- ave(pri,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     prd  <- ave(prd,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pvem <- ave(pvem, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pna  <- ave(pna,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     psd  <- ave(psd,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pric <- ave(pric, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     ptc  <- ave(ptc,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     efec  <- ave(efec, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
## })
## v09d <- v09d[duplicated(v09d$edon*100 + v09d$disn)==FALSE,]
## v09d <- v09d[order(v09d$edon*100, v09d$disn),]
## #
## v12d <- within(v12d, {
##     pan <-  ave(pan,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pri <-  ave(pri,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pvem <- ave(pvem, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pna <-  ave(pna,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pric <- ave(pric, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     prdc <- ave(prdc, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     efec  <- ave(efec, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
## })
## v12d <- v12d[duplicated(v12d$edon*100 + v12d$disn)==FALSE,]
## v12d <- v12d[order(v12d$edon*100, v12d$disn),]
## #
## v15d <- within(v15d, {
##     pan <-    ave(pan,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pri <-    ave(pri,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     prd <-    ave(prd,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pvem <-   ave(pvem,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pt <-     ave(pt,     as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     mc <-     ave(mc,     as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pna <-    ave(pna,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     morena <- ave(morena, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     ph <-     ave(ph,     as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pes <-    ave(pes,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     panc <-   ave(panc,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pric <-   ave(pric,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     prdc <-   ave(prdc,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     indep1 <- ave(indep1, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     indep2 <- ave(indep2, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     efec  <- ave(efec, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
## })
## v15d <- v15d[duplicated(v15d$edon*100 + v15d$disn)==FALSE,]
## v15d <- v15d[order(v15d$edon*100, v15d$disn),]
## #
## v18d <- within(v18d, {
##     pan <-    ave(pan,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pri <-    ave(pri,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     prd <-    ave(prd,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pvem <-   ave(pvem,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pt <-     ave(pt,     as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     mc <-     ave(mc,     as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pna <-    ave(pna,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     morena <- ave(morena, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pes <-    ave(pes,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     panc <-   ave(panc,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pric <-   ave(pric,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     morenac<- ave(morenac,as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     indep1 <- ave(indep1, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     indep2 <- ave(indep2, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     efec  <- ave(efec, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
## })
## v18d <- v18d[duplicated(v18d$edon*100 + v18d$disn)==FALSE,]
## v18d <- v18d[order(v18d$edon*100, v18d$disn),]
## #
## v21d <- within(v21d, {
##     pan <-    ave(pan,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pri <-    ave(pri,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     prd <-    ave(prd,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pvem <-   ave(pvem,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pt <-     ave(pt,     as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     mc <-     ave(mc,     as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     morena <- ave(morena, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pes <-    ave(pes,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     rsp <-    ave(rsp,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     fxm <-    ave(fxm,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     indep <- ave(indep, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     panc <-   ave(panc,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
## #    pric <-   ave(pric,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     morenac<- ave(morenac,as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     efec  <- ave(efec, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
## })
## v21d <- v21d[duplicated(v21d$edon*100 + v21d$disn)==FALSE,]
## v21d <- v21d[order(v21d$edon*100, v21d$disn),]
## #
## windis <- v15d[,c("edon","disn")] # will receive data
## #
## ## # example
## ## v <- data.frame(c1=c(30,15,3), c2=c(10,25,2), c3=c(20,35,4))
## ## w <- data.frame(c1=c("thirty","fifteen","three"), c2=c("ten","twenty-five","two"), c3=c("twenty","thirty-five","four"))
## ## v.sorted <- t(apply(v, 1, function(x) sort(x, decreasing = TRUE))) # sort each row of df -- http://stackoverflow.com/questions/6063881/sorting-rows-alphabetically
## ## w.sorted <- sortBy(target = w, By = v)
## ## sortBy(target = v, By = v)
## #
## # 1994
## vot <- v94d[,c("pan","pri","pps","prd","pfcrn","parm","uno.pdm","pt","pvem")]
## # crea objeto de etiquetas
## etiq <- data.frame(matrix(rep(colnames(vot), nrow(vot)), nrow=nrow(vot), byrow = TRUE), stringsAsFactors = FALSE)
## colnames(etiq) <- paste("l", 1:ncol(vot), sep = "")
## #
## etiq <- sortBy(target = etiq, By = vot)
## vot <- t(apply(vot, 1, function(x) sort(x, decreasing = TRUE)))
## #
## windis$e94 <- etiq[,1] 
## #
## # 1997
## vot <- v97d[,c("pan","pri","prd","pc","pt","pvem","pps","pdm")]
## vot[is.na(vot)==TRUE] <- 0 # drop NAs
## # crea objeto de etiquetas
## etiq <- data.frame(matrix(rep(colnames(vot), nrow(vot)), nrow=nrow(vot), byrow = TRUE), stringsAsFactors = FALSE)
## colnames(etiq) <- paste("l", 1:ncol(vot), sep = "")
## #
## etiq <- sortBy(target = etiq, By = vot)
## vot <- t(apply(vot, 1, function(x) sort(x, decreasing = TRUE)))
## #
## windis$e97 <- etiq[,1] 
## #
## # 2000
## vot <- v00d[,c("panc","pri","prdc","pcd","parm","dsppn")]
## vot[is.na(vot)==TRUE] <- 0 # drop NAs
## # crea objeto de etiquetas
## etiq <- data.frame(matrix(rep(colnames(vot), nrow(vot)), nrow=nrow(vot), byrow = TRUE), stringsAsFactors = FALSE)
## colnames(etiq) <- paste("l", 1:ncol(vot), sep = "")
## #
## etiq <- sortBy(target = etiq, By = vot)
## vot <- t(apply(vot, 1, function(x) sort(x, decreasing = TRUE)))
## #
## windis$e00 <- etiq[,1] 
## #
## # 2003
## vot <- v03d[,c("pan","pri","prd","pt","pvem","conve","psn","pas","mp","plm","fc","pric")]
## vot[is.na(vot)==TRUE] <- 0 # drop NAs
## # crea objeto de etiquetas
## etiq <- data.frame(matrix(rep(colnames(vot), nrow(vot)), nrow=nrow(vot), byrow = TRUE), stringsAsFactors = FALSE)
## colnames(etiq) <- paste("l", 1:ncol(vot), sep = "")
## #
## etiq <- sortBy(target = etiq, By = vot)
## vot <- t(apply(vot, 1, function(x) sort(x, decreasing = TRUE)))
## #
## windis$e03 <- etiq[,1] 
## #
## # 2006
## vot <- v06d[,c("pan","pric","prdc","pna","asdc")]
## vot[is.na(vot)==TRUE] <- 0 # drop NAs
## # crea objeto de etiquetas
## etiq <- data.frame(matrix(rep(colnames(vot), nrow(vot)), nrow=nrow(vot), byrow = TRUE), stringsAsFactors = FALSE)
## colnames(etiq) <- paste("l", 1:ncol(vot), sep = "")
## #
## etiq <- sortBy(target = etiq, By = vot)
## vot <- t(apply(vot, 1, function(x) sort(x, decreasing = TRUE)))
## #
## windis$e06 <- etiq[,1] 
## ## runnerup$e06 <- etiq[,2]
## ## mg$e06 <- (vot[,1] - vot[,2]) / rowSums(vot)
## ## enp$e06 <- 1/rowSums((vot/rowSums(vot))^2)
## #
## # 2009
## vot <- v09d[,c("pan","pri","prd","pvem","pna","psd","pric","ptc")]
## vot[is.na(vot)==TRUE] <- 0 # drop NAs
## # crea objeto de etiquetas
## etiq <- data.frame(matrix(rep(colnames(vot), nrow(vot)), nrow=nrow(vot), byrow = TRUE), stringsAsFactors = FALSE)
## colnames(etiq) <- paste("l", 1:ncol(vot), sep = "")
## #
## etiq <- sortBy(target = etiq, By = vot)
## vot <- t(apply(vot, 1, function(x) sort(x, decreasing = TRUE)))
## #
## windis$e09 <- etiq[,1] 
## ## runnerup$e09 <- etiq[,2]
## ## mg$e09 <- (vot[,1] - vot[,2]) / rowSums(vot)
## ## enp$e09 <- 1/rowSums((vot/rowSums(vot))^2)
## #
## # 2012
## vot <- v12d[,c("pan","pri","pvem","pna","pric","prdc")]
## vot[is.na(vot)==TRUE] <- 0 # drop NAs
## # crea objeto de etiquetas
## etiq <- data.frame(matrix(rep(colnames(vot), nrow(vot)), nrow=nrow(vot), byrow = TRUE), stringsAsFactors = FALSE)
## colnames(etiq) <- paste("l", 1:ncol(vot), sep = "")
## #
## etiq <- sortBy(target = etiq, By = vot)
## vot <- t(apply(vot, 1, function(x) sort(x, decreasing = TRUE)))
## #
## windis$e12 <- etiq[,1] 
## ## runnerup$e12 <- etiq[,2]
## ## mg$e12 <- (vot[,1] - vot[,2]) / rowSums(vot)
## ## enp$e12 <- 1/rowSums((vot/rowSums(vot))^2)
## #
## # 2015
## vot <- v15d[,c("pan","pri","prd","pvem","pt","mc","pna","morena","ph","pes","panc","pric","prdc","indep1","indep2")]
## vot[is.na(vot)==TRUE] <- 0 # drop NAs
## # crea objeto de etiquetas
## etiq <- data.frame(matrix(rep(colnames(vot), nrow(vot)), nrow=nrow(vot), byrow = TRUE), stringsAsFactors = FALSE)
## colnames(etiq) <- paste("l", 1:ncol(vot), sep = "")
## #
## etiq <- sortBy(target = etiq, By = vot)
## vot <- t(apply(vot, 1, function(x) sort(x, decreasing = TRUE)))
## #
## windis$e15 <- etiq[,1]
## ## runnerup$e15 <- etiq[,2]
## ## mg$e15 <- (vot[,1] - vot[,2]) / rowSums(vot)
## ## enp$e15 <- 1/rowSums((vot/rowSums(vot))^2)
## #
## # 2018
## vot <- v18d[,c("pan","pri","prd","pvem","pt","mc","pna","morena","pes","panc","pric","morenac","indep1","indep2")]
## vot[is.na(vot)==TRUE] <- 0 # drop NAs
## # crea objeto de etiquetas
## etiq <- data.frame(matrix(rep(colnames(vot), nrow(vot)), nrow=nrow(vot), byrow = TRUE), stringsAsFactors = FALSE)
## colnames(etiq) <- paste("l", 1:ncol(vot), sep = "")
## #
## etiq <- sortBy(target = etiq, By = vot)
## vot <- t(apply(vot, 1, function(x) sort(x, decreasing = TRUE)))
## #
## windis$e18 <- etiq[,1]
## ## runnerup$e15 <- etiq[,2]
## ## mg$e15 <- (vot[,1] - vot[,2]) / rowSums(vot)
## ## enp$e15 <- 1/rowSums((vot/rowSums(vot))^2)
## #
## # 2021 -- uses v21dw instead of v21d
## vot <- v21d[,c("pan","pri","prd","pvem","pt","mc","morena","pes","rsp","fxm","indep","panc","morenac")]
## vot[is.na(vot)==TRUE] <- 0 # drop NAs
## # crea objeto de etiquetas
## etiq <- data.frame(matrix(rep(colnames(vot), nrow(vot)), nrow=nrow(vot), byrow = TRUE), stringsAsFactors = FALSE)
## colnames(etiq) <- paste("l", 1:ncol(vot), sep = "")
## #
## etiq <- sortBy(target = etiq, By = vot)
## vot <- t(apply(vot, 1, function(x) sort(x, decreasing = TRUE)))
## #
## windis$e21 <- etiq[,1]
## #
## # assign coal win to bigger of pan or pri in district---PENDING, USE v21_split instead of w
## lab <- apply(tmp.w, 1, max)
## lab <- ifelse(tmp.w$pan==lab, "panc", "pric")
## windis$e21[which(windis$e21=="panc")] <- lab[which(windis$e21=="panc")]
## # assign coal win to bigger of morena or pvem in district
## lab <- apply(tmp.w2, 1, max)
## lab <- ifelse(tmp.w2$morena==lab, "morenac", "pvemc")
## windis$e21[which(windis$e21=="morenac")] <- lab[which(windis$e21=="morenac")]
## ## runnerup$e21 <- etiq[,2]
## ## mg$e21 <- (vot[,1] - vot[,2]) / rowSums(vot)
## ## enp$e21 <- 1/rowSums((vot/rowSums(vot))^2)
## #
## rm(vot,etiq)
## rm(v21dw,tmp.w,tmp.w2) # drop to avoid confusion
## #
## write.csv(windis, file = paste(dd, "dfdf2006-on-winners.csv", sep = ""))


###############################################################################
## keep split vote objects only for analysis                                 ##
## OJO: 2000, 2003, and 2006 have joint coalition vote only                  ##
## OJO: assumes pan=panc in 00, pri=pric in 03 and 06, prd=prdc in 00 and 06 ##
###############################################################################
v91 <- v91_split
v94 <- v94_split
v97 <- v97_split
v00 <- v00_agg
v03 <- v03_agg
v06 <- v06_agg
v09 <- v09_split
v12 <- v12_split
v15 <- v15_split
v18 <- v18_split
v21 <- v21_split
# free memory
rm(
    v91_split, v91_agg,
    v94_split, v94_agg,
    v97_split, v97_agg,
               v00_agg,
               v03_agg,
               v06_agg,
    v09_split, v09_agg,
    v12_split, v12_agg,
    v15_split, v15_agg,
    v18_split, v18_agg,
    v21_split, v21_agg
)

##############################################################################
## OJO: Script code/manip-dis1979-dis2018-to-assign-dropped-secciones.r     ##
## ran independently 12apr2023 to infer historic federal districts when     ##
## reseccionamiento occurred. Output was incorportated into excel sheet     ##
## redistrict/ife.ine/equivSecc/tablaEquivalenciasSeccionalesDesde1994.csv. ##
## Run script again if changes to original file occurred                    ##
######  |  ###################################################################
     #  |  #
######  V  ########################
## get equivalencias seccionales ##
###################################
tmp <- paste(md, "equivSecc/tablaEquivalenciasSeccionalesDesde1994.csv", sep = "")
eq <- read.csv(tmp, stringsAsFactors = FALSE)
# drop secciones whose numbers have never been used
sel <- which(eq$baja==1992); eq <- eq[-sel,]; rm(sel)
# check: any secciones missing from eq?
library(crayon)
miss_secc <- function(dat=v21_split){ # defaults to 2021
    tmp <- deparse(substitute(dat)) # extract object's name
    sel <- which(as.factor(dat$edon+dat$seccion/10000) %notin% as.factor(eq$edon+eq$seccion/10000))
    if (length(sel)>0){
        cat(magenta("Some secciones not in eq object\n"))
        dat$edon[sel]+dat$seccion[sel]/10000
#        print(c(tmp, ": ", dat$edon[sel]+dat$seccion[sel]/10000))
    } else {
        print(paste0(tmp, ": All v-secciones in eq object"))
    }
}
miss_secc(dat=v94)
miss_secc(dat=v97)
miss_secc(dat=v00)
miss_secc(dat=v03)
miss_secc(dat=v06)
miss_secc(dat=v09)
miss_secc(dat=v12)
miss_secc(dat=v15)
miss_secc(dat=v18)
miss_secc(dat=v21) # some 2021 secciones not in eq, all break sequence so must be temporary
#                  # OJO: will drop them to keep things square (still no clue about them)
sel.r <-          which(v21$edon==17 & v21$seccion>=5000)
sel.r <- c(sel.r, which(v21$edon==19 & v21$seccion>=9000))
v21 <- v21[-sel.r,]
rm(miss_secc,sel.r,tmp) # clean

######################################################################
## extract historic municipio and district info to merge into votes ##
######################################################################
mundis <- eq[, c("edon", "seccion", "ife", "inegi",
                 "ife1994", "ife1997", "ife2000", "ife2003", "ife2006", "ife2009",
                   "ife2012", "ife2015", "ife2018", "ife2021", 
                 "dis1979", "dis1997", "dis2006", "dis2013", "dis2018")]
##
###########################################
## match yearly observations (secciones) ##
## add edon to seccion ids               ##
###########################################
#dim(v06); dim(v09); dim(v12); dim(v15)
## v91 <- within(v91, {                                  # 1991 cannot be matched
##     seccion <- ife*1000000 + disn*10000 + seccion
##     d91     <- 1;
## })
v94 <- within(v94, {
    seccion <- edon*10000 + seccion;
    d94     <- 1;
})
v97 <- within(v97, {
    seccion <- edon*10000 + seccion;
    d97     <- 1;
})
v00 <- within(v00, {
    seccion <- edon*10000 + seccion;
    d00     <- 1;
})
v03 <- within(v03, {
    seccion <- edon*10000 + seccion;
    d03     <- 1;
})
v06 <- within(v06, {
    seccion <- edon*10000 + seccion;
    d06     <- 1;
})
v09 <- within(v09, {
    seccion <- edon*10000 + seccion;
    d09     <- 1;
})
v12 <- within(v12, {
    seccion <- edon*10000 + seccion;
    d12     <- 1;
})
v15 <- within(v15, {
    seccion <- edon*10000 + seccion;
    d15     <- 1;
})
v18 <- within(v18, {
    seccion <- edon*10000 + seccion;
    d18    <- 1;
})
v21 <- within(v21, {
    seccion <- edon*10000 + seccion;
    d21    <- 1;
})
#
## Indicate secciones existing each year with dummies d91 d94 ... 
##tmp <- merge(x=v91[,c("edosecn","d91")], y=v94[,c("edosecn","d94")], by = "edosecn", all = TRUE)
##tmp <- merge(x=tmp,                      y=v97[,c("edosecn","d97")], by = "edosecn", all = TRUE)
tmp.all.sec <- merge(x=v94[,c("seccion","d94")], y=v97[,c("seccion","d97")], by = "seccion", all = TRUE)
tmp.all.sec <- merge(x=tmp.all.sec,              y=v00[,c("seccion","d00")], by = "seccion", all = TRUE)
tmp.all.sec <- merge(x=tmp.all.sec,              y=v03[,c("seccion","d03")], by = "seccion", all = TRUE)
tmp.all.sec <- merge(x=tmp.all.sec,              y=v06[,c("seccion","d06")], by = "seccion", all = TRUE)
tmp.all.sec <- merge(x=tmp.all.sec,              y=v09[,c("seccion","d09")], by = "seccion", all = TRUE)
tmp.all.sec <- merge(x=tmp.all.sec,              y=v12[,c("seccion","d12")], by = "seccion", all = TRUE)
tmp.all.sec <- merge(x=tmp.all.sec,              y=v15[,c("seccion","d15")], by = "seccion", all = TRUE)
tmp.all.sec <- merge(x=tmp.all.sec,              y=v18[,c("seccion","d18")], by = "seccion", all = TRUE)
tmp.all.sec <- merge(x=tmp.all.sec,              y=v21[,c("seccion","d21")], by = "seccion", all = TRUE)
## fill 0s in year dummies
sel.c <- grep("^d[0-9]{2}$", colnames(tmp.all.sec))
tmp <- tmp.all.sec[,sel.c]
tmp[is.na(tmp)] <- 0
tmp.all.sec[,sel.c] <- tmp
## check ok
which(tmp.all.sec$seccion %notin% (eq$edon*10000+eq$seccion)) # all used secciones (tmp.all.sec) are in eq
which((eq$edon*10000+eq$seccion) %notin% tmp.all.sec$seccion) # many eq secciones not in used ones (tmp.all.sec)
#
## clean
##v91$d91 <-
v94$d94 <- v97$d97 <- v00$d00 <- v03$d03 <- v06$d06 <- v09$d09 <- v12$d12 <- v15$d15 <- v18$d18 <- v21$d21 <- NULL
#
# adds any missing used secciones to each object
#v91 <- merge(x=tmp.all.sec, y=v91, by = "seccion", all = TRUE)
v94  <- merge(x=tmp.all.sec, y=v94, by = "seccion", all = TRUE)
v97  <- merge(x=tmp.all.sec, y=v97, by = "seccion", all = TRUE)
v00  <- merge(x=tmp.all.sec, y=v00, by = "seccion", all = TRUE)
v03  <- merge(x=tmp.all.sec, y=v03, by = "seccion", all = TRUE)
v06  <- merge(x=tmp.all.sec, y=v06, by = "seccion", all = TRUE)
v09  <- merge(x=tmp.all.sec, y=v09, by = "seccion", all = TRUE)
v12  <- merge(x=tmp.all.sec, y=v12, by = "seccion", all = TRUE)
v15  <- merge(x=tmp.all.sec, y=v15, by = "seccion", all = TRUE)
v18  <- merge(x=tmp.all.sec, y=v18, by = "seccion", all = TRUE)
v21  <- merge(x=tmp.all.sec, y=v21, by = "seccion", all = TRUE)
###########################
## verify dimensionality ##
###########################
dim(v21);
nrow(v94)==nrow(v97)
nrow(v97)==nrow(v00)
nrow(v00)==nrow(v03)
nrow(v03)==nrow(v06)
nrow(v06)==nrow(v06)
nrow(v06)==nrow(v12)
nrow(v12)==nrow(v15)
nrow(v15)==nrow(v18)
nrow(v18)==nrow(v21)
# re-computing edon from seccion ids will fill in any missing edon for added observations
add.edon.secn <- function(x) {
    within(x, edon <- as.integer(seccion/10000))
}
#v91 <- add.edon.secn(v91)
v94 <- add.edon.secn(v94)
v97 <- add.edon.secn(v97)
v00 <- add.edon.secn(v00)
v03 <- add.edon.secn(v03)
v06 <- add.edon.secn(v06)
v09 <- add.edon.secn(v09)
v12 <- add.edon.secn(v12)
v15 <- add.edon.secn(v15)
v18 <- add.edon.secn(v18)
v21 <- add.edon.secn(v21)


#########################
## READ/PREP pop>18yrs ##
#########################
##
## create a fresh tmp.mun.sec
tmp.all.sec <- eq
##tmp.all.sec <- eq[,c("edon","seccion","inegi","ife","mun","alta","baja")]
##
########################
## 2005 seccion-level ##
########################
edos <- c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac")
tmp18 <- data.frame()
for (i in 1:9){
    tmp2005 <- read.csv( paste0("/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/secciones/eceg_2005/", edos[i], "/0", i, "_", edos[i], "_pob.csv"), stringsAsFactors = FALSE)
    tmp18 <- rbind(tmp18, tmp2005[, grep("ENTIDAD|SECCION|POB_TOT|EDQUI0[1-4]", colnames(tmp2005))])
}
for (i in 10:32){
    tmp2005 <- read.csv( paste0("/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/secciones/eceg_2005/", edos[i], "/", i, "_", edos[i], "_pob.csv"), stringsAsFactors = FALSE)
    tmp18 <- rbind(tmp18, tmp2005[, grep("ENTIDAD|SECCION|POB_TOT|EDQUI0[1-4]", colnames(tmp2005))])
}

## drop seccion=0
sel.r <- which(tmp18$SECCION==0)
if (length(sel.r)>0) tmp18 <- tmp18[-sel.r,]
rm(sel.r)
## EDQUI04 covers ages 15:19, drop two-fifths (assumes yearly cohorts equal size)
tmp18$EDQUI04 <- as.integer(tmp18$EDQUI04 * .6)
## p18 ptot
tmp18$p18_2005 <- tmp18$POB_TOT - tmp18$EDQUI01 - tmp18$EDQUI02 - tmp18$EDQUI03 - tmp18$EDQUI04
tmp18$ptot_2005 <- tmp18$POB_TOT
##
tmp18$seccion <- tmp18$ENTIDAD*10000 + tmp18$SECCION
tmp18 <- tmp18[, c("seccion", "p18_2005", "ptot_2005")]
## add missing secciones (tmp adds up all secciones reported in yearly federal returns)
tmp.all.sec$seccion <- tmp.all.sec$edon * 10000 + tmp.all.sec$seccion
sel <- which(tmp18$seccion %notin% tmp.all.sec$seccion)
tmp18$seccion[sel]
c(nrow(tmp.all.sec), nrow(tmp18))
tmp18  <- merge(x=tmp.all.sec, y=tmp18,  by = "seccion", all = TRUE)
c(nrow(tmp.all.sec), nrow(tmp18))
## add to pop object
pob18 <- tmp18
rm(tmp18)
##
########################
## 2010 seccion-level ##
########################
tmp18 <- data.frame()
tmp.all.sec$tmp.drop <- NA;                          # redundant column to preserve data frame form
tmp.all.sec <- tmp.all.sec[,c("seccion","tmp.drop")] # trim to avoid duplicated cols in merge
for (i in 1:9){
    tmp2010 <- read.csv( paste0("/home/eric/Desktop/MXelsCalendGovt/censos/secciones/eceg_2010/", edos[i], "/secciones_0", i, ".csv"), stringsAsFactors = FALSE)
    tmp18 <- rbind(tmp18, tmp2010[, grep("ENTIDAD|CLAVEGEO|P_18YMAS$|POBTOT", colnames(tmp2010))])
}
for (i in 10:32){
    tmp2010 <- read.csv( paste0("/home/eric/Desktop/MXelsCalendGovt/censos/secciones/eceg_2010/", edos[i], "/secciones_", i, ".csv"), stringsAsFactors = FALSE)
    tmp18 <- rbind(tmp18, tmp2010[, grep("ENTIDAD|CLAVEGEO|P_18YMAS$|POBTOT", colnames(tmp2010))])
}
rm(tmp2010)
## get seccion
lastn <- function(x, n) substr(x, nchar(x)-n+1, nchar(x))
tmp18$seccion <- as.numeric(lastn(tmp18$CLAVEGEO, 4))
##
tmp18$seccion <- tmp18$ENTIDAD*10000 + tmp18$seccion
tmp18$p18_2010 <- tmp18$P_18YMAS
tmp18$ptot_2010 <- tmp18$POBTOT
tmp18 <- tmp18[, c("seccion", "p18_2010", "ptot_2010")]
## add missing secciones
tmp18  <- merge(x=tmp.all.sec, y=tmp18,  by = "seccion", all = TRUE)
tmp18$tmp.drop <- NULL # clean
## add to pop object
pob18 <- merge(pob18, tmp18, by = "seccion", all = TRUE)
head(pob18)
##
########################
## 2020 seccion-level ##
########################
tmp18 <- read.csv("/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/secciones/eceg_2020/conjunto_de_datos/INE_SECCION_2020.csv", stringsAsFactors = FALSE)[, c(2,5,7,13)]
tmp18$p18_2020 <- tmp18$POBTOT - tmp18$P_0A17
tmp18$ptot_2020 <- tmp18$POBTOT
tmp18$seccion <- tmp18$ENTIDAD*10000 + tmp18$SECCION
tmp18 <- tmp18[, c("seccion", "p18_2020", "ptot_2020")]
## add missing secciones
tmp18  <- merge(x=tmp.all.sec, y=tmp18,  by = "seccion", all = TRUE)
tmp18$tmp.drop <- NULL # clean
## add to pop object
pob18 <- merge(pob18, tmp18, by = "seccion", all = TRUE)
head(pob18)
##
## will need cleaning with reseccionamiento functions later
table(y2005na=is.na(pob18$p18_2005), y2010na=is.na(pob18$p18_2010))
table(y2005na=is.na(pob18$p18_2005), y2020na=is.na(pob18$p18_2020))
table(y2010na=is.na(pob18$p18_2010), y2020na=is.na(pob18$p18_2020))
## clean
rm(add.edon.secn, edos, tmp.all.sec, tmp, lastn, tmp18, tmp2005, sel, sel.c, i) # clean
##
## rename object
censo <- pob18 # rename object
rm(pob18)
head(censo)


#############################
## 2000 munic-level census ##
#############################
c00 <- read.csv("/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/2000censo/cgpv2000_iter_00.csv")
## drop localidades etc, keep municipios only
sel.r <- grep("TOTAL MUNICIPAL|TOTAL DE LA DELEGACION", c00$nom_loc)
c00 <- c00[sel.r,c("entidad","mun","nom_mun","pob18_")]
c00 <- within(c00, {
    edon <- entidad;
    inegi <- edon*1000+mun;
    mun <- nom_mun;
    pob18 <- pob18_;
    entidad <- nom_mun <- pob18_ <- NULL
    }
)
##c00 <- merge(x=c00, y=censom00[,c("ife","inegi")], by="inegi", all.y=TRUE)
c00[c00$inegi==9002,]

#############################
## 1995 munic-level conteo ##
#############################
tmpf <- c("/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/1995conteo/Cont95Enum_Ags_Poblacion.csv", "/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/1995conteo/Cont95Enum_BC_Poblacion.csv", "/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/1995conteo/Cont95Enum_BCS_Poblacion.csv", "/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/1995conteo/Cont95Enum_Camp_Poblacion.csv", "/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/1995conteo/Cont95Enum_Chih_Poblacion.csv", "/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/1995conteo/Cont95Enum_Chis_Poblacion.csv", "/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/1995conteo/Cont95Enum_Coah_Poblacion.csv", "/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/1995conteo/Cont95Enum_Col_Poblacion.csv", "/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/1995conteo/Cont95Enum_DF_Poblacion.csv", "/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/1995conteo/Cont95Enum_Dgo_Poblacion.csv", "/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/1995conteo/Cont95Enum_Gro_Poblacion.csv", "/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/1995conteo/Cont95Enum_Gto_Poblacion.csv", "/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/1995conteo/Cont95Enum_Hgo_Poblacion.csv", "/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/1995conteo/Cont95Enum_Jal_Poblacion.csv", "/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/1995conteo/Cont95Enum_Mex_Poblacion.csv", "/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/1995conteo/Cont95Enum_Mich_Poblacion.csv", "/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/1995conteo/Cont95Enum_Mor_Poblacion.csv", "/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/1995conteo/Cont95Enum_Nay_Poblacion.csv", "/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/1995conteo/Cont95Enum_NL_Poblacion.csv", "/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/1995conteo/Cont95Enum_Oax_Poblacion.csv", "/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/1995conteo/Cont95Enum_Pue_Poblacion.csv", "/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/1995conteo/Cont95Enum_QRoo_Poblacion.csv", "/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/1995conteo/Cont95Enum_Qro_Poblacion.csv", "/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/1995conteo/Cont95Enum_Sin_Poblacion.csv", "/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/1995conteo/Cont95Enum_SLP_Poblacion.csv", "/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/1995conteo/Cont95Enum_Son_Poblacion.csv", "/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/1995conteo/Cont95Enum_Tab_Poblacion.csv", "/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/1995conteo/Cont95Enum_Tamps_Poblacion.csv", "/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/1995conteo/Cont95Enum_Tlax_Poblacion.csv", "/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/1995conteo/Cont95Enum_Ver_Poblacion.csv", "/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/1995conteo/Cont95Enum_Yuc_Poblacion.csv", "/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/1995conteo/Cont95Enum_Zac_Poblacion.csv")
##
c95f <- function(i=NA){
    #i <- 6
    tmp <- read.csv(tmpf[i])
    tmpt <- tmp[grep("^Total$", tmp$Edad.desplegada),] # keep lines with mun totals only
    tmpt <- tmpt[-1,] # drop 1st obs = state total
    tmpt <- within(tmpt,{
        edon <- as.numeric(sub("^([0-9]+) .*$", "\\1", Entidad.federativa));
        mun  <-                         sub("^([0-9]+)+[ ]+([0-9a-zñáéíóúü,. -]+)", "\\2", Municipio, ignore.case = TRUE);
        inegi <- edon*1000 + as.numeric(sub("^([0-9]+)+[ ]+([0-9a-zñáéíóúü,. -]+)", "\\1", Municipio, ignore.case = TRUE));
        ptot <- Población.total..1;
    })
    ##
    sel.r <- grep("^[0-9] año.?$|^1[0-7] año.?$", tmp$Edad.desplegada) # find 0 to 17 yrs
    tmpm <- tmp[sel.r,]
    tmpm <- tmpm[-1:-18,] # drop obs 1-18 = state totals
    tmpm <- within(tmpm,{
        edon <- as.numeric(sub("^([0-9]+) .*$", "\\1", Entidad.federativa));
        inegi <- edon*1000 + as.numeric(sub("^([0-9]+)+[ ]+([0-9a-zñáéíóúü,. -]+)", "\\1", Municipio, ignore.case = TRUE));
        ptot <- Población.total..1;
        Entidad.federativa <- NULL;
        pob17 <- ave(Población.total..1, as.factor(inegi), FUN=sum, na.rm=TRUE); # sum sub-18 pop
    })
    tmpm <- tmpm[duplicated(tmpm$inegi)==FALSE,] # drop redundant rows
    ##
    tmpt <- cbind(tmpt, pob17=tmpm$pob17)
    tmpt <- within(tmpt, pob18 <- ptot - pob17)
    ##
    return(tmpt[,c("inegi","mun","ptot","pob18")])
}
c95 <- data.frame() # initialize empty data frame
for (i in 1:32){
    c95 <- rbind(c95, c95f(i=i))
}
## add municipios zapatistas, absent in conteo
## inegi	ife	mun
## 7004	       7004	ALTAMIRANO
## 7013	       7013	BOCHIL
## 7014	       7014	BOSQUE--EL
## 7024	       7024	CHANAL
## 7026	       7026	CHENALHO
## 7038	       7039	HUIXTAN
## 7039	       7038	HUITIUPAN
## 7041	       7041	INDEPENDENCIA--LA
## 7052	       7052	MARGARITAS--LAS
## 7059	       7059	OCOSINGO
## 7064	       7064	OXCHUC
## 7076	       7075	SABANILLA
## 7081	       7082	SIMOJOVEL
## 7100	       7100	TUMBALA
## 7112	       7079	SAN JUAN CANCUC
tmpz <- c(7004, 7013, 7014, 7024, 7026, 7038, 7039, 7041, 7052, 7059, 7064, 7076, 7081, 7100, 7112)
c95 <- merge(x=c95, y=data.frame(inegi=tmpz), all=TRUE)
## split missing mun aggregate (relying on 2000 rel pops)
sel.r <- which(c95$inegi==7999)
c95zap <- c95[ sel.r,]
c95    <- c95[-sel.r,]
tmprel <- as.numeric(c00$pob18[c00$inegi %in% tmpz]) / sum(as.numeric(c00$pob18[c00$inegi %in% tmpz]))
c95$pob18[c95$inegi %in% tmpz] <- round(c95zap$pob18 * tmprel)
c95$ptot [c95$inegi %in% tmpz] <- round(c95zap$ptot  * tmprel)
rm(c95zap, tmpz, tmprel, sel.r)
##
## rename
c95$pob18_1995 <- c95$pob18; c95$pob18 <- NULL
c00$pob18_2000 <- c00$pob18; c00$pob18 <- NULL
censom <- merge(x=c00, y=c95[,c("inegi","pob18_1995")], by="inegi", all=TRUE)
## add ife
censom$ife <- inegi2ife(censom$inegi)
censom <- censom[, c("edon","ife","inegi","mun","pob18_1995","pob18_2000")]
## clean
rm(c00,c95,c95f,i,tmpf)


################################
## censo 1990 municipio level ##
################################
c90 <- read.csv("/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/raw/munic/1990/ptot1990mu.csv")
c90$pob18_1990 <- c90$p18
dim(c90)
dim(censom)
censom <- merge(x=censom, y=c90[,c("inegi","pob18_1990")], by = "inegi", all=TRUE)
rm(c90)


#############################################################
## drop casilla longitude/latitude from files that have it ##
#############################################################
d <- v06 # duplicate for manipulation
sel.c <- grep("longitude|latitude", colnames(d), ignore.case=TRUE)
if (length(d)>0) d <- d[,-sel.c]
d -> v06 # return to data
#
d <- v09 # duplicate for manipulation
sel.c <- grep("longitude|latitude", colnames(d), ignore.case=TRUE)
if (length(d)>0) d <- d[,-sel.c]
d -> v09 # return to data
#
## v12 has no long/lat
#
d <- v15 # duplicate for manipulation
sel.c <- grep("longitude|latitude", colnames(d), ignore.case=TRUE)
if (length(d)>0) d <- d[,-sel.c]
d -> v15 # return to data
#
d <- v18 # duplicate for manipulation
sel.c <- grep("longitude|latitude", colnames(d), ignore.case=TRUE)
if (length(d)>0) d <- d[,-sel.c]
d -> v18 # return to data

#############################################################
##   consolidate districts & municipios before secciones   ##
##   are manipulated to deal with reseccionamiento         ##
#############################################################
## eg. 1979 map federal election counterfactuals        ##
## to postdict 1988 vote (cf. cantú, but longitudinal)  ##
##########################################################
#
# add edon to seccion ids
mundis$seccion <- mundis$edon*10000 + mundis$seccion
# add counterfactual district and municipios to votes
sel.ignore <- which(colnames(mundis) %in% c("edon","ife")) # do not merge these repeated columns
# v91 <- merge(x = v91, y = mundis[,-sel.ignore], by = "seccion", all.x = TRUE, all.y = FALSE); v91$munn <- NULL
v94 <- merge(x = v94,  y = mundis[,-sel.ignore], by = "seccion", all.x = TRUE, all.y = FALSE); v94$munn <- NULL
v97 <- merge(x = v97,  y = mundis[,-sel.ignore], by = "seccion", all.x = TRUE, all.y = FALSE); v97$munn <- NULL
v00 <- merge(x = v00,  y = mundis[,-sel.ignore], by = "seccion", all.x = TRUE, all.y = FALSE); v00$munn <- NULL
v03 <- merge(x = v03,  y = mundis[,-sel.ignore], by = "seccion", all.x = TRUE, all.y = FALSE); v03$munn <- NULL
v06 <- merge(x = v06,  y = mundis[,-sel.ignore], by = "seccion", all.x = TRUE, all.y = FALSE); v06$munn <- NULL
v09 <- merge(x = v09,  y = mundis[,-sel.ignore], by = "seccion", all.x = TRUE, all.y = FALSE); v09$munn <- NULL
v12 <- merge(x = v12,  y = mundis[,-sel.ignore], by = "seccion", all.x = TRUE, all.y = FALSE); v12$munn <- NULL
v15 <- merge(x = v15,  y = mundis[,-sel.ignore], by = "seccion", all.x = TRUE, all.y = FALSE); v15$munn <- NULL
v18 <- merge(x = v18,  y = mundis[,-sel.ignore], by = "seccion", all.x = TRUE, all.y = FALSE); v18$munn <- NULL
v21 <- merge(x = v21,  y = mundis[,-sel.ignore], by = "seccion", all.x = TRUE, all.y = FALSE); v21$munn <- NULL
#
rm(mundis)

####################################################
## fill ife codes in secciones that changed munic ##
####################################################
v94$ife <- v94$ife1994
v94$inegi <- inegi2ife(v94$ife)
##
v97$ife <- v97$ife1997
v97$inegi <- inegi2ife(v97$ife)
##
v00$ife <- v00$ife2000
v00$inegi <- inegi2ife(v00$ife)
##
v03$ife <- v03$ife2003
v03$inegi <- inegi2ife(v03$ife)
##
v06$ife <- v06$ife2006
v06$inegi <- inegi2ife(v06$ife)
##
v09$ife <- v09$ife2009
v09$inegi <- inegi2ife(v09$ife)
##
v12$ife <- v12$ife2012
v12$inegi <- inegi2ife(v12$ife)
##
v15$ife <- v15$ife2015
v15$inegi <- inegi2ife(v15$ife)
##
v18$ife <- v18$ife2018
v18$inegi <- inegi2ife(v18$ife)
##
v21$ife <- v21$ife2021
v21$inegi <- inegi2ife(v21$ife)

# rename seccion vote objects
v91s <- v91; v94s <- v94; v97s <- v97; v00s <- v00; v03s <- v03; v06s <- v06; v09s <- v09; v12s <- v12; v15s <- v15; v18s <- v18; v21s <- v21;
rm(v91,v94, v97, v00, v03, v06, v09, v12, v15, v18, v21)

###############################################################
## Script code/aggregates-mun-dis-from-sec.r produces        ##
## municipio- and district-level vote and census aggregates  ##
###############################################################
source("../../code/v-hat-etc/aggregates-mun-dis-from-sec.r")

# save all to restore after manipulating district/munic aggregates
save.image("../../datosBrutos/not-in-git/tmp-restore.RData")

# load image
rm(list=ls())
options(width = 110)
dd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/")
setwd(dd)
load(file="../../datosBrutos/not-in-git/tmp-restore.RData")


##################################################################################
## Next script preps and exports municipio-level populations with manipulations ##
## to deal with new municipalities and minor adjustments to municipal borders.  ##
## No need to re-run it unless there have been changes to eq or to censo data   ##
## (eg. if another census indicator were desired.) Else, just import directly   ##
## the files that the script generates.                                         ##
##################################################################################
## source("../../code/v-hat-etc/export-mu-pops.r")
## censom <- tmpf
## 
## ## or read saved version:
censom <- read.csv("/home/eric/Dropbox/data/elecs/MXelsCalendGovt/censos/data/pob18/p18mu-for-compos-var-seccion-censos.csv")
##censom <- read.csv("/home/eric/Dropbox/data/elecs/MXelsCalendGovt/censos/data/pob18/p18mu-for-federal-elecs.csv")
##censom <- read.csv("/home/eric/Dropbox/data/elecs/MXelsCalendGovt/censos/data/pob18/p18mu-for-municipal-elecs.csv")


####################################################################
## Script to add population projections to municipal v..m.. maps. ##
## Drops censom.. objects.                                        ##
## Very slow, preferable to open/run script by hand...            ##
####################################################################
source("../../code/v-hat-etc/interpolate-census-data-se-by-se.r")

v18m[1,]
x

###################################################
## Finished adding census to vote objects, clean ##
###################################################
ls()[grep("censo", ls())]
## ls()[grep("v..m", ls())]
## drop most census maps, no longer needed
rm(list = ls()[grep("^censod|^censom[0-9{2}]", ls())])
rm(list = ls()[grep("tmp", ls())])
rm(non.nas, preds, projyr, myf, interpol, interlog)


## save all to restore after manipulating district/munic aggregates
save.image("../../datosBrutos/not-in-git/tmp-restore.RData")

# load image
rm(list=ls())
options(width = 110)
dd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/")
setwd(dd)
load(file="../../datosBrutos/not-in-git/tmp-restore.RData")






################################################################################
## Run script code/alpha-regs-etc-districts to estimate DISTRICT regressions. ##
## Preferable to open/run script by hand...                                   ##
################################################################################
source("../../code/v-hat-etc/alpha-regs-etc-districts.r")

#################################################################################
## Run script code/alpha-regs-etc-districts to estimate MUNICIPIO regressions. ##
## Preferable to open/run script by hand...                                    ##
#################################################################################
source("../../code/v-hat-etc/alpha-regs-etc-municipios.r")




#########################################################
## drop unnecessary columns from seccion-level objects ##
#########################################################
##v09s[1,]
v94s <- within(v94s, d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- d21 <- NULL)
v94s <- within(v94s, ife1994 <- ife1997 <- ife2000 <- ife2003 <- ife2006 <- ife2009 <- ife2012 <- ife2015 <- ife2018 <- ife2021 <- NULL)
v94s <- within(v94s, dis1979 <- dis1997 <- dis2006 <- dis2013 <- dis2018 <- NULL)
v97s <- within(v97s, d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- d21 <- NULL)
v97s <- within(v97s, ife1994 <- ife1997 <- ife2000 <- ife2003 <- ife2006 <- ife2009 <- ife2012 <- ife2015 <- ife2018 <- ife2021 <- NULL)
v97s <- within(v97s, dis1979 <- dis1997 <- dis2006 <- dis2013 <- dis2018 <- NULL)
v00s <- within(v00s, d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- d21 <- NULL)
v00s <- within(v00s, ife1994 <- ife1997 <- ife2000 <- ife2003 <- ife2006 <- ife2009 <- ife2012 <- ife2015 <- ife2018 <- ife2021 <- NULL)
v00s <- within(v00s, dis1979 <- dis1997 <- dis2006 <- dis2013 <- dis2018 <- NULL)
v03s <- within(v03s, d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- d21 <- NULL)
v03s <- within(v03s, ife1994 <- ife1997 <- ife2000 <- ife2003 <- ife2006 <- ife2009 <- ife2012 <- ife2015 <- ife2018 <- ife2021 <- NULL)
v03s <- within(v03s, dis1979 <- dis1997 <- dis2006 <- dis2013 <- dis2018 <- NULL)
v06s <- within(v06s, d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- d21 <- NULL)
v06s <- within(v06s, ife1994 <- ife1997 <- ife2000 <- ife2003 <- ife2006 <- ife2009 <- ife2012 <- ife2015 <- ife2018 <- ife2021 <- NULL)
v06s <- within(v06s, dis1979 <- dis1997 <- dis2006 <- dis2013 <- dis2018 <- NULL)
v09s <- within(v09s, d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- d21 <- NULL)
v09s <- within(v09s, ife1994 <- ife1997 <- ife2000 <- ife2003 <- ife2006 <- ife2009 <- ife2012 <- ife2015 <- ife2018 <- ife2021 <- NULL)
v09s <- within(v09s, dis1979 <- dis1997 <- dis2006 <- dis2013 <- dis2018 <- NULL)
v12s <- within(v12s, d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- d21 <- NULL)
v12s <- within(v12s, ife1994 <- ife1997 <- ife2000 <- ife2003 <- ife2006 <- ife2009 <- ife2012 <- ife2015 <- ife2018 <- ife2021 <- NULL)
v12s <- within(v12s, dis1979 <- dis1997 <- dis2006 <- dis2013 <- dis2018 <- NULL)
v15s <- within(v15s, d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- d21 <- NULL)
v15s <- within(v15s, ife1994 <- ife1997 <- ife2000 <- ife2003 <- ife2006 <- ife2009 <- ife2012 <- ife2015 <- ife2018 <- ife2021 <- NULL)
v15s <- within(v15s, dis1979 <- dis1997 <- dis2006 <- dis2013 <- dis2018 <- NULL)
v18s <- within(v18s, d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- d21 <- NULL)
v18s <- within(v18s, ife1994 <- ife1997 <- ife2000 <- ife2003 <- ife2006 <- ife2009 <- ife2012 <- ife2015 <- ife2018 <- ife2021 <- NULL)
v18s <- within(v18s, dis1979 <- dis1997 <- dis2006 <- dis2013 <- dis2018 <- NULL)
v21s <- within(v21s, d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- d21 <- NULL)
v21s <- within(v21s, ife1994 <- ife1997 <- ife2000 <- ife2003 <- ife2006 <- ife2009 <- ife2012 <- ife2015 <- ife2018 <- ife2021 <- NULL)
v21s <- within(v21s, dis1979 <- dis1997 <- dis2006 <- dis2013 <- dis2018 <- NULL)


####################################################
## Save a copy to restore all after manipulating  ##
## reseccionamiento for seccion-level regressions ##
####################################################
save.image("../../datosBrutos/not-in-git/tmp3-restore.RData")


## load image
rm(list=ls())
options(width = 110)
dd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/")
setwd(dd)
load(file="../../datosBrutos/not-in-git/tmp3-restore.RData")




##############################################################################
## script performs manipulation of secciones that suffered reseccionamiento ##
## Preferable to open/run script by hand...                                 ##
##############################################################################
source("../../code/v-hat-etc/resecc-deal-with-splits-and-merge-to.r")



# save for debug
save.image("../../datosBrutos/not-in-git/tmp-debug.RData")

# load image
rm(list=ls())
options(width = 110)
dd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/")
setwd(dd)
load(file="../../datosBrutos/not-in-git/tmp-debug.RData")



###############################################################################
## Run script code/alpha-regs-etc-districts to estimate SECCION regressions. ##
## Must open and run script by hand...                                       ##
###############################################################################
source("../../code/v-hat-etc/alpha-regs-etc-secciones.r")





# drop columns before saving raw vote seccion files
#v91s <- within(v91, munn <- NULL)
#write.csv(v91s, file = paste(sd, "data/dipfed-seccion-vraw-1991.csv", sep = ""), row.names = FALSE)
tmp <- within(v94s, edosecn <- NULL)
write.csv(tmp, file = paste(sd, "sec/dipfed-seccion-vraw-1994.csv", sep = ""), row.names = FALSE)
tmp <- within(v97s, edosecn <- NULL)
write.csv(tmp, file = paste(sd, "sec/dipfed-seccion-vraw-1997.csv", sep = ""), row.names = FALSE)
tmp <- within(v00s, edosecn <- NULL)
write.csv(tmp, file = paste(sd, "sec/dipfed-seccion-vraw-2000.csv", sep = ""), row.names = FALSE)
tmp <- within(v03s, edosecn <- NULL)
write.csv(tmp, file = paste(sd, "sec/dipfed-seccion-vraw-2003.csv", sep = ""), row.names = FALSE)
tmp <- within(v06s, edosecn <- NULL)
write.csv(tmp, file = paste(sd, "sec/dipfed-seccion-vraw-2006.csv", sep = ""), row.names = FALSE)
tmp <- within(v09s, edosecn <- NULL)
write.csv(tmp, file = paste(sd, "sec/dipfed-seccion-vraw-2009.csv", sep = ""), row.names = FALSE)
tmp <- within(v12s, edosecn <- NULL)
write.csv(tmp, file = paste(sd, "sec/dipfed-seccion-vraw-2012.csv", sep = ""), row.names = FALSE)
tmp <- within(v15s, edosecn <- NULL)
write.csv(tmp, file = paste(sd, "sec/dipfed-seccion-vraw-2015.csv", sep = ""), row.names = FALSE)
tmp <- within(v18s, edosecn <- NULL)
write.csv(tmp, file = paste(sd, "sec/dipfed-seccion-vraw-2018.csv", sep = ""), row.names = FALSE)
tmp <- within(v21s, edosecn <- NULL)
write.csv(tmp, file = paste(sd, "sec/dipfed-seccion-vraw-2021.csv", sep = ""), row.names = FALSE)
rm(tmp)


## ##############################################################################################################
## ## 31may2023: script below needs debugging. Should do this with coalAgg objects, before dropping them above ##
## ##############################################################################################################
## # save municipal winners with correct manipulated data for new municipalities
## # v.. needed to work with winner script
## v91 <- v91m;
## v94 <- v94m; v97 <- v97m; v00 <- v00m; v03 <- v03m; v06 <- v06m; v09 <- v09m; v12 <- v12m; v15 <- v15m; v18 <- v18m; v21 <- v21m; v21w <- v21mw
## # get unit winners and margins: will output object winner for chosen agg
## agg <- "m"
## source("../../code/v-hat-etc/get-winners.r")
## tail(winner) # NAs before new mun creation
## # save first part of output
## write.csv(winner,
##           file = paste(sd, "mun/dipfed-municipio-win.csv", sep = ""), row.names = FALSE)
## #
## rm(tmp,tmp.w,tmp.w2) # drop to avoid confusion


# saves fixed mun raw aggregates
write.csv(v91m,  file = paste(sd, "mun/dipfed-municipio-vraw-1991.csv", sep = ""), row.names = FALSE)
write.csv(v94m,  file = paste(sd, "mun/dipfed-municipio-vraw-1994.csv", sep = ""), row.names = FALSE)
write.csv(v97m,  file = paste(sd, "mun/dipfed-municipio-vraw-1997.csv", sep = ""), row.names = FALSE)
write.csv(v00m,  file = paste(sd, "mun/dipfed-municipio-vraw-2000.csv", sep = ""), row.names = FALSE)
write.csv(v03m,  file = paste(sd, "mun/dipfed-municipio-vraw-2003.csv", sep = ""), row.names = FALSE)
write.csv(v06m,  file = paste(sd, "mun/dipfed-municipio-vraw-2006.csv", sep = ""), row.names = FALSE)
write.csv(v09m,  file = paste(sd, "mun/dipfed-municipio-vraw-2009.csv", sep = ""), row.names = FALSE)
write.csv(v12m,  file = paste(sd, "mun/dipfed-municipio-vraw-2012.csv", sep = ""), row.names = FALSE)
write.csv(v15m,  file = paste(sd, "mun/dipfed-municipio-vraw-2015.csv", sep = ""), row.names = FALSE)
write.csv(v18m,  file = paste(sd, "mun/dipfed-municipio-vraw-2018.csv", sep = ""), row.names = FALSE)
write.csv(v21m,  file = paste(sd, "mun/dipfed-municipio-vraw-2021.csv", sep = ""), row.names = FALSE)

# saves fixed district raw aggregates
write.csv(v91d,  file = paste(sd, "dis/dipfed-distrito-vraw-1991.csv", sep = ""), row.names = FALSE)
write.csv(v94d,  file = paste(sd, "dis/dipfed-distrito-vraw-1994.csv", sep = ""), row.names = FALSE)
write.csv(v97d,  file = paste(sd, "dis/dipfed-distrito-vraw-1997.csv", sep = ""), row.names = FALSE)
write.csv(v00d,  file = paste(sd, "dis/dipfed-distrito-vraw-2000.csv", sep = ""), row.names = FALSE)
write.csv(v03d,  file = paste(sd, "dis/dipfed-distrito-vraw-2003.csv", sep = ""), row.names = FALSE)
write.csv(v06d,  file = paste(sd, "dis/dipfed-distrito-vraw-2006.csv", sep = ""), row.names = FALSE)
write.csv(v09d,  file = paste(sd, "dis/dipfed-distrito-vraw-2009.csv", sep = ""), row.names = FALSE)
write.csv(v12d,  file = paste(sd, "dis/dipfed-distrito-vraw-2012.csv", sep = ""), row.names = FALSE)
write.csv(v15d,  file = paste(sd, "dis/dipfed-distrito-vraw-2015.csv", sep = ""), row.names = FALSE)
write.csv(v18d,  file = paste(sd, "dis/dipfed-distrito-vraw-2018.csv", sep = ""), row.names = FALSE)
write.csv(v21d,  file = paste(sd, "dis/dipfed-distrito-vraw-2021.csv", sep = ""), row.names = FALSE)



ToDo jul2021:
1)  [x] push reload and seccion data manip for later
2)  [x] v5 for factual and counterfactual v..ms
3)  [x] alpha regs seem to need little manipulation: generate year swings with each v..m, then regress as before
4)  [x] beta regs need to rely on appropriate counterfactuals instead of factual v..ms
5)  [x] Fix winners 2021: prepare temp coal agg object to use with pri in it to determine correct unit winners
6)  [x] Why do vhats have NA 2467 line? 
7)  [ ] Why do vhats have NA in ife 7124?
8)  [ ] Seybaplaya+Dzitbalche have 0s d.pan, bhats, and betahats... usa means en vez de cf? Quizás reseccionamiento post 2018 juega papel
9)  [x] Al agregar votos 2021, no suman bien el voto PRI en vhat (ver foto en ife.ine/data)
10) [x] Separar 2021 pvem de morenac
11) [ ] Separar tambien prd y pt de sus coaliciones? otros años también?
12) [x] Ya puedo generar vhat.2024 (con municipios 21)
Cuando haya codificado historia de AMGE:
13) [X] Fix seccion action and to.from






## this may be needed in case coal dummies cause troubles below
v00m  <-  within(v00m, dpanc <-          dprdc                  <- NULL)
v03m  <-  within(v03m,          dpric                           <- NULL)
v06m  <-  within(v06m,          dpric <- dprdc                  <- NULL)
v09m  <-  within(v09m,          dpric <-          dptc          <- NULL)
v12m  <-  within(v12m,          dpric <- dprdc                  <- NULL)
v15m  <-  within(v15m, dpanc <- dpric <- dprdc                  <- NULL)
v18m  <-  within(v18m, dpanc <- dpric <-               dmorenac <- NULL)
v21m  <-  within(v21m, dpanc <- dpric <-               dmorenac <- NULL)







