######################################################################
## Script to generate inter-census populations.                     ##
## Adds p18 to district, municipio, and seccion vote objects.       ##
## Projects seccion-level censuses 05 10 20, then uses that to      ##
## aggregate municipio- and district-level populations.             ##
## Invoked from elec-data-for-maps.r,.                              ##
## Has routine (commented out) used to evaluate projection methods, ##
## discussed in blog entry.                                         ##
##                                                                  ##
## Author: Eric Magar                                               ##
## emagar at itam dot mx                                            ##
## Date: 22aug2023                                                  ##
## Last modified: 26sep2023                                         ##
######################################################################

## 1. Prep: sum.split <- function(d=censo, year.var=2020, rnd=1)
## 2. Prep: function interlog --- for 3pt log(dv=r)~iv
## 3. Prep: function interpol --- for 2pt segments

## Function shows rows above/below numbered rows
show <- function(x, rows, after = 0, before = 0) {
  ## From https://stackoverflow.com/questions/13155609/returning-above-and-below-rows-of-specific-rows-in-r-dataframe
  ##match.idx  <- which(rownames(x) %in% rows) ## if using names rows
  match.idx  <- rows                           ## if using numbered rows
  span       <- seq(from = -before, to = after)
  extend.idx <- c(outer(match.idx, span, `+`))
  extend.idx <- Filter(function(i) i > 0 & i <= nrow(x), extend.idx)
  extend.idx <- sort(unique(extend.idx))
  return(x[extend.idx, , drop = FALSE])
}
##
##############################################################
## manipulate secciones that suffered some reseccionamiento ##
##############################################################
sum.split <- function( d=censo, year.var=2020, rnd=1, dfull=censo, year.to.split=c(NA,2005,2010,2020)[1], restore=FALSE ) {
    ## Notes: (1) d can be a subset of dfull's rows (dfull needed to find target secciones).
    ##        (2) year.to.split!=NA applies year.var's relative weigths to split year.to.split's agg pop *instead of* summing year.var.
    ##        (3) restore=TRUE manipulates target secciones's drestore to 1 *instead of* splitting (2).
    ##
    if (year.var %notin% c(2005,2010,2020)){
        print("Warning: year.var is not a sección-level census year")
        stop
    }
    ##year.var      <- 2020                      # debug
    ##year.to.split <- 2010                      # debug
    ##d <-     nm[which(nm$action=="split.to"),] # debug
    ##dfull <- nm                                # debug
    d     <- d
    dfull <- dfull
    ## exclude non-numeric columns that needn't sum-up
    sel.col <- setdiff(colnames(d),
                       c("ord", "edon", "edo", "seccion", "ife", "inegi", "mun", "ddone", "dskip", "OBSERVACIONES", "coment"
                       , "alta", "baja", "dmunchg"
                       , "action", "orig.dest", "when", "action2", "orig.dest2", "when2", "action3", "orig.dest3", "when3"
                       , "ife1991", "ife1994", "ife1997", "ife2000", "ife2003", "ife2006"
                       , "ife2009", "ife2012", "ife2015", "ife2018", "ife2021", "ife2024"
                       , "dis1979", "dis1997", "dis2006", "dis2013", "dis2018"
                         ))
    sel.col.split <- sel.col[grep(year.to.split, sel.col)] # crop sel.col to split only year.to.split
    sel.col <-       sel.col[grep(year.var,      sel.col)] # crop sel.col to sum only year.var
    ##
    for (i in 1:nrow(d)){
        ##i <- 100 # debug
        if (rnd==1){
            year <- d$when [i]                              # reseccionamiento year (modify data in subsequent years only)
            sel.to <- eval(str2expression(d$orig.dest [i])) # turn string w vector def into vector of target secciones to sum-up
        }
        if (rnd==2){
            year <- d$when2[i]
            sel.to <- eval(str2expression(d$orig.dest2[i]))
        }
        if (rnd==3){
            year <- d$when3[i]
            sel.to <- eval(str2expression(d$orig.dest3[i]))
        }
        ## standardize seccion ids
        sel.to <- d$edon[i] * 10000 + sel.to
        ## indices to be summed-up from censo
        sel.agg <-  which(dfull$seccion %in% sel.to)
        ## sum population
        totals <- colSums(dfull[sel.agg, sel.col], na.rm = TRUE)
        ## relative populations, for use in breaking up past aggregates
        relpops <-        dfull[sel.agg, sel.col] ## duplicate
        relpops <- sweep(x=relpops, MARGIN=2, STATS=totals, FUN="/") # sweep "/" divides whole columns by corresponding totals
        ## paste into sección under manipulation
        if (is.na(year.to.split)==TRUE)  {
            totals -> d[i,sel.col]
        }
        if (is.na(year.to.split)==FALSE  & restore==FALSE) {
            relpops <- round( sweep(x=relpops, MARGIN=2, STATS=as.matrix(d[i, sel.col.split]), FUN="*") ,1)
            ## return disaggregated matrix
            relpops -> dfull[sel.agg, sel.col.split]
        }
        if (is.na(year.to.split)==FALSE  & restore==TRUE) {
            drestore$p18_2005[sel.agg] <- drestore$p18_2010[sel.agg] <- drestore$p18_2020[sel.agg] <- 1
        }
    }
    ## return manipulated data
    if (is.na(year.to.split)==TRUE)                   return(d)
    if (is.na(year.to.split)==FALSE & restore==FALSE) return(dfull)
    if (is.na(year.to.split)==FALSE & restore==TRUE)  return(drestore)
}
##
## Linear projection functions
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
    chg <- (x$p18_2010 - x$p18_2005) /5 # yearly pop change
    pop <- x$p18_2005 + chg * (yr - 2005)
    return(pop)
}
prj1020 <- function(x=NA,yr=NA){
    chg <- (x$p18_2020 - x$p18_2010) / 10 # yearly pop change
    pop <- x$p18_2010 + chg * (yr - 2010)
    return(pop)
}
prj0520 <- function(x=NA,yr=NA){
    chg <- (x$p18_2020 - x$p18_2005) / 15 # yearly pop change
    pop <- x$p18_2005 + chg * (yr - 2005)
    return(pop)
}
##
## get census projection functions
source("../../code/v-hat-etc/interpolate-census-functions.r")

## MOVE TO EXPORT...
##
## 4. Duplicate nm=censo, adding cols 1994:2021, m:2005-2010-2020, ddone=0, etc
##    nm = nominal quantities
nm       <- censo
nm.saved <- censo[, grep("seccion|^p18_(2005|2010|2020)", colnames(nm))] # backup, to restore
nm.saved[is.na(nm.saved)] <- 0 # NAs to zero in backup
drestore <- nm.saved; drestore[,-1] <- 0 # indicate cases that need to be restore here
##
## #####################################
## ## Make inegi codes equal 2021 map ##
## #####################################
## nm$inegi <- ife2inegi(nm$ife2021)
##
#####################################################################
## change orig.dest w non-adjacent seccion nums to vector notation ##
#####################################################################
sel <- grep("[|]", nm$orig.dest)
##nm$orig.dest[sel]
if (length(sel)>0){
    tmp <- gsub("[|]", ",", nm$orig.dest [sel], perl = TRUE)
    tmp <- paste0("c(", tmp, ")")
    nm$orig.dest [sel] <- tmp
}
sel <- grep("[|]", nm$orig.dest2)
##nm$orig.dest2[sel]
if (length(sel)>0){
    tmp <- gsub("[|]", ",", nm$orig.dest2[sel], perl = TRUE)
    tmp <- paste0("c(", tmp, ")")
    nm$orig.dest2[sel] <- tmp
}
sel <- grep("[|]", nm$orig.dest3)
##nm$orig.dest3[sel]
if (length(sel)>0){
    tmp <- gsub("[|]", ",", nm$orig.dest3[sel], perl = TRUE)
    tmp <- paste0("c(", tmp, ")")
    nm$orig.dest3[sel] <- tmp
}
##
#############################################################
## Add cols that will receive p18 projections for elec yrs ##
#############################################################
nm[1,]
nm <- within(nm, {
    dfirst <- dsingle <- dskip <- dready2est <- 0;
#    nmanip <- ddone <- dneedsum <- 0;
#    dneedsum05 <- dneedsum10 <- dneedsum20 <- dneedsum <- 0;
#    when3b <- when3; orig.dest3b <- orig.dest3; action3b <- action3; #  backup action/orig.dest/when so that these
#    when2b <- when2; orig.dest2b <- orig.dest2; action2b <- action2; #  can be erased once seccion gets manipulated
#    whenb  <- when;  orig.destb  <- orig.dest;  actionb  <- action;  #  ---helps debug block immediately below
    p18e_20 <- NA; # yy so grep excludes
    p18e_10 <- NA; # yy so grep excludes
    p18e_05 <- NA; # yy so grep excludes
    #p18_1994 <- p18_1997 <- p18_2000 <- p18_2003 <- p18_2006 <- p18_2009 <- p18_2012 <- p18_2015 <- p18_2018 <- p18_2021 <- NA;
})
##nm <- nm[order(nm$inegi, nm$seccion),] # sort mun
##


## MOVE TO EXPORT...
##
##############################################################
## Cleaning block starts here                               ##
## OJO: Need to re-evaluate each sub-block of the block     ##
##      below when new seccion-level censo is available.    ##
##      Block below takes care of manipulating and setting  ##
##      aside (with dready2est and dskip) secciones that    ##
##      can/cannot be handled with log(dv)~iv approach to   ##
##      project seccion-level p18.                          ##
##      Run with a fresh nm <- censo when debugging.        ##
##############################################################
##
## These are innocuous, drop label
##sel.tmp <- which(nm$action3=="mun.chg")
##nm[sel.tmp,] <- within(nm[sel.tmp,], {
##    action3 <- "";      orig.dest3 <- "";         when3 <- NA
##})
sel.tmp <- which(nm$action2=="mun.chg")
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
sel.tmp <- which(nm$action=="mun.chg")
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    action  <- action2; orig.dest  <- orig.dest2; when  <- when2
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
## these have no census info, skip
sel.tmp <- which(nm$action=="state.chg")
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
nm[sel.tmp, c("alta", "baja", "action", "p18_2005", "p18_2010", "p18_2020")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2005 <- p18_2010 <- p18_2020 <- 0;
    dskip <- 1; 
})
##
## Cases that have zeroes across the board
sel.tmp <- which(
    nm$    dready2est==0 &
    nm$    dskip     ==0 &
    (is.na(nm$p18_2005)==TRUE | nm$p18_2005==0) &
    (is.na(nm$p18_2010)==TRUE | nm$p18_2010==0) &
    (is.na(nm$p18_2020)==TRUE | nm$p18_2020==0) &
    nm$action=="")
nm.w <- nm [sel.tmp,]
## onle action3 can be salvaged, ignore previous
nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
nm.w$dskip <- 1 # indicate skip
nm.w -> nm [sel.tmp,] # return to data
##
## Cases that need no manipulation
sel.tmp <- which(
    nm$      dready2est==0 &
    nm$      dskip     ==0 &
    is.na(nm$p18_2005)==FALSE &
    is.na(nm$p18_2010)==FALSE &
    is.na(nm$p18_2020)==FALSE &
    nm$action=="")
nm$dready2est[sel.tmp] <- 1
##
## Split that can't be figured out, then one that can
sel.tmp <- which(nm$seccion %in% c(190439))
nm.w       <- nm       [sel.tmp,]
drestore.w <- drestore [sel.tmp,]
## only action3 can be salvaged, ignore previous
nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
nm.w[, c("seccion","alta","baja","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
## flat since 2005 
nm.w <- sum.split(d=nm.w, year.var=2005, rnd=2, dfull=nm) # add-up pre-split population
nm.w$p18_2010 <- nm.w$p18_2005
nm.w$p18_2020 <- nm.w$p18_2005
nm.w$dready2est[nm.w$p18_2005!=0] <- 1 # indicate readiness if 2005 non null
nm.w$dskip     [nm.w$p18_2005==0] <- 1 # indicate skip if 2020 null
drestore.w$p18_2005 <- drestore.w$p18_2010 <- drestore.w$p18_2020 <- 1 # indicate restore
nm.w       -> nm       [sel.tmp,] # return to data
drestore.w -> drestore [sel.tmp,]
##
## Subset Triple missing
sel.trip <- which( nm$      dready2est==0 &
                   nm$      dskip     ==0 &
                   is.na(nm$p18_2005) ==TRUE &
                   is.na(nm$p18_2010) ==TRUE &
                   is.na(nm$p18_2020) ==TRUE )
table( nm$action3 [sel.trip], useNA = "always" )
table( nm$action2 [sel.trip], useNA = "always" )
table( nm$action  [sel.trip], useNA = "always" )
rm(sel.trip)
## Subset Triple missing action3==split.to, can be fixed with sum.split
sel.trip <- which( nm$      dready2est==0 &
                   nm$      dskip     ==0 &
                   nm$      action3   =="split.to" &
                   is.na(nm$p18_2005) ==TRUE &
                   is.na(nm$p18_2010) ==TRUE &
                   is.na(nm$p18_2020) ==TRUE )
nm.w       <- nm       [sel.trip,]
drestore.w <- drestore [sel.trip,]
head(nm.w[, c("seccion","alta","baja","action3","orig.dest3","when3","p18_2005","p18_2010","p18_2020","dskip","dready2est")] )
head(nm.w[, c("seccion","alta","baja","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dskip","dready2est")] )
head(nm.w[, c("seccion","alta","baja","action" ,"orig.dest" ,"when", "p18_2005","p18_2010","p18_2020","dskip","dready2est")] )
## make flat 2020 across the board
nm.w <- sum.split(d=nm.w, year.var=2020, rnd=3, dfull=nm) # add-up pre-split population
nm.w$p18_2005 <- nm.w$p18_2020
nm.w$p18_2010 <- nm.w$p18_2020
nm.w$dready2est[nm.w$p18_2020!=0] <- 1 # indicate readiness if 2020 non null
nm.w$dskip     [nm.w$p18_2020==0] <- 1 # indicate skip if 2020 null
drestore.w$p18_2005 <- drestore.w$p18_2010 <- drestore.w$p18_2020 <- 1 # indicate restore
nm.w       -> nm       [sel.trip,] # return to data
drestore.w -> drestore [sel.trip,]
##
## Subset Triple missing action2==split.to, can be fixed with sum.split
sel.trip <- which( nm$      dready2est==0 &
                   nm$      dskip     ==0 &
                   nm$      action2   =="merged.to" &
                   is.na(nm$p18_2005) ==TRUE &
                   is.na(nm$p18_2010) ==TRUE &
                   is.na(nm$p18_2020) ==TRUE )
nm.w       <- nm       [sel.trip,]
drestore.w <- drestore [sel.trip,]
head(nm.w[, c("seccion","alta","baja","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dskip","dready2est")] )
## make flat 2020 across the board
nm.w <- sum.split(d=nm.w, year.var=2020, rnd=2, dfull=nm) # add-up pre-split population
nm.w$p18_2005 <- nm.w$p18_2020
nm.w$p18_2010 <- nm.w$p18_2020
nm.w$dready2est[nm.w$p18_2020!=0] <- 1 # indicate readiness if 2020 non null
nm.w$dskip     [nm.w$p18_2020==0] <- 1 # indicate skip if 2020 null
drestore.w$p18_2005 <- drestore.w$p18_2010 <- drestore.w$p18_2020 <- 1 # indicate restore
nm.w       -> nm       [sel.trip,] # return to data
drestore.w -> drestore [sel.trip,]
## Subset Triple missing action==split.to, can be fixed with sum.split
sel.trip <- which( nm$      dready2est==0 &
                   nm$      dskip     ==0 &
                   nm$      action    =="split.to" &
                   is.na(nm$p18_2005) ==TRUE &
                   is.na(nm$p18_2010) ==TRUE &
                   is.na(nm$p18_2020) ==TRUE )
nm.w       <- nm       [sel.trip,]
drestore.w <- drestore [sel.trip,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")] 
## all pre 2005
table(nm.w$when)
## sum
nm.w <- sum.split(d=nm.w, year.var=2020, rnd=1, dfull=nm) # add-up pre-split population
nm.w <- sum.split(d=nm.w, year.var=2010, rnd=1, dfull=nm) # add-up pre-split population
nm.w <- sum.split(d=nm.w, year.var=2005, rnd=1, dfull=nm) # add-up pre-split population
nm.w$dready2est <- 1 # indicate readiness
drestore.w$p18_2005 <- drestore.w$p18_2010 <- drestore.w$p18_2020 <- 1 # indicate restore
nm.w       -> nm       [sel.trip,] # return to data
drestore.w -> drestore [sel.trip,]
##
## Subset Triple missing action==merged.to, can't be fixed
sel.trip <- which( nm$      dready2est==0 &
                   nm$      dskip     ==0 &
                   nm$      action    =="merged.to" &
                   is.na(nm$p18_2005) ==TRUE &
                   is.na(nm$p18_2010) ==TRUE &
                   is.na(nm$p18_2020) ==TRUE )
nm.w <- nm [sel.trip,]
## none have any census nfo
nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")] 
## indicate
nm.w$p18_2005 <- nm.w$p18_2010 <- nm.w$p18_2020 <- 0
nm.w$dskip <- 1
nm.w -> nm [sel.trip,] # return to data
##
## Subset Triple missing action==split.from, can't be fixed
sel.trip <- which( nm$      dready2est==0 &
                   nm$      dskip     ==0 &
                   nm$      action    =="split.from" &
                   is.na(nm$p18_2005) ==TRUE &
                   is.na(nm$p18_2010) ==TRUE &
                   is.na(nm$p18_2020) ==TRUE )
nm.w <- nm [sel.trip,]
head(nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")] )
## all altas in 2022, skip
table(baja=nm.w$alta, when=nm.w$when, useNA = "always")
nm.w$p18_2005 <- nm.w$p18_2010 <- nm.w$p18_2020 <- 0
nm.w$dskip <- 1
nm.w -> nm [sel.trip,] # return to data
##
## No unmanipulated Triple missing remaining
sel.trip <- which( nm$      dready2est==0 &
                   nm$      dskip     ==0 &
                   is.na(nm$p18_2005) ==TRUE &
                   is.na(nm$p18_2010) ==TRUE &
                   is.na(nm$p18_2020) ==TRUE )
table( nm$action3 [sel.trip], useNA = "always" )
table( nm$action2 [sel.trip], useNA = "always" )
table( nm$action  [sel.trip], useNA = "always" )
rm(sel.trip)
##
## Subset 10-20 Double missing
sel.doub <- which( nm$      dready2est==0 &
                   nm$      dskip     ==0 &
                   is.na(nm$p18_2010) ==TRUE &
                   is.na(nm$p18_2020) ==TRUE )
table( nm$action3 [sel.doub], useNA = "always" )
table( nm$action2 [sel.doub], useNA = "always" )
table( nm$action  [sel.doub], useNA = "always" )
rm(sel.doub)
##
## Subset 10-20 Double missing action2==split.to, salvageable with sum.split
sel.doub <- which( nm$      dready2est==0 &
                   nm$      dskip     ==0 &
                   nm$      action2   =="split.to" &
                   is.na(nm$p18_2010) ==TRUE &
                   is.na(nm$p18_2020) ==TRUE )
nm.w       <- nm       [sel.doub,]
drestore.w <- drestore [sel.doub,]
nm.w[, c("seccion","alta","baja","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
nm.w[, c("seccion","alta","baja","action" ,"orig.dest" ,"when" ,"p18_2005","p18_2010","p18_2020","dskip","dready2est")] # ignore this, pre-2005
## make flat 2010 after 2010
nm.w <- sum.split(d=nm.w, year.var=2010, rnd=2, dfull=nm) # add-up pre-split population
nm.w$p18_2020 <- nm.w$p18_2010
nm.w$dready2est[nm.w$p18_2010!=0] <- 1 # indicate readiness if 2010 non null
nm.w$dskip     [nm.w$p18_2010==0] <- 1 # indicate skip if 2010 null
drestore.w$p18_2005 <- drestore.w$p18_2010 <- drestore.w$p18_2020 <- 1 # indicate restore
nm.w       -> nm       [sel.doub,] # return to data
drestore.w -> drestore [sel.doub,]
##
## Subset 10-20 Double missing action2==merged.to, unsalvageable
sel.doub <- which( nm$      dready2est==0 &
                   nm$      dskip     ==0 &
                   nm$      action2   =="merged.to" &
                   is.na(nm$p18_2010) ==TRUE &
                   is.na(nm$p18_2020) ==TRUE )
nm.w       <- nm       [sel.doub,]
drestore.w <- drestore [sel.doub,]
nm.w[, c("seccion","alta","baja","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
## make flat 2005 afterwards
nm.w$p18_2020 <- nm.w$p18_2010 <- nm.w$p18_2005
nm.w$dready2est[nm.w$p18_2005!=0] <- 1 # indicate readiness if 2005 non null
nm.w$dskip     [nm.w$p18_2005==0] <- 1 # indicate skip if 2005 null
drestore.w$p18_2005 <- drestore.w$p18_2010 <- drestore.w$p18_2020 <- 1 # indicate restore
nm.w       -> nm       [sel.doub,] # return to data
drestore.w -> drestore [sel.doub,]
##
## Subset 10-20 Double missing action==split.to, salvageable w sum.split
sel.doub <- which( nm$      dready2est==0 &
                   nm$      dskip     ==0 &
                   nm$      action   =="split.to" &
                   is.na(nm$p18_2010) ==TRUE &
                   is.na(nm$p18_2020) ==TRUE )
nm.w       <- nm       [sel.doub,]
drestore.w <- drestore [sel.doub,]
nm.w[, c("seccion","alta","baja","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
## all between 2007 and 2010
table(nm.w$when)
## sum.plit than flat 2010 afterwards
nm.w <- sum.split(d=nm.w, year.var=2020, rnd=1, dfull=nm) # add-up pre-split population
nm.w <- sum.split(d=nm.w, year.var=2010, rnd=1, dfull=nm) # add-up pre-split population
nm.w$dready2est <- 1 # indicate readiness
drestore.w$p18_2005 <- drestore.w$p18_2010 <- drestore.w$p18_2020 <- 1 # indicate restore
nm.w       -> nm       [sel.doub,] # return to data
drestore.w -> drestore [sel.doub,]
##
## Subset 10-20 Double missing action==merged.to, flat 2005 across the board
sel.doub <- which( nm$      dready2est==0 &
                   nm$      dskip     ==0 &
                   nm$      action   =="merged.to" &
                   is.na(nm$p18_2010) ==TRUE &
                   is.na(nm$p18_2020) ==TRUE )
nm.w       <- nm       [sel.doub,]
drestore.w <- drestore [sel.doub,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
## all between 2008 and 2011
table(nm.w$when)
## make flat 2005 afterwards
nm.w$p18_2020 <- nm.w$p18_2010 <- nm.w$p18_2005
nm.w$dready2est[nm.w$p18_2005!=0] <- 1 # indicate readiness if 2005 non null
nm.w$dskip     [nm.w$p18_2005==0] <- 1 # indicate skip if 2005 null
drestore.w$p18_2005 <- drestore.w$p18_2010 <- drestore.w$p18_2020 <- 1 # indicate restore
nm.w       -> nm       [sel.doub,] # return to data
drestore.w -> drestore [sel.doub,]
##
## No unmanipulated 10-20 Double missing remaining
sel.doub <- which( nm$      dready2est==0 &
                   nm$      dskip     ==0 &
                   is.na(nm$p18_2010) ==TRUE &
                   is.na(nm$p18_2020) ==TRUE )
table( nm$action3 [sel.doub], useNA = "always" )
table( nm$action2 [sel.doub], useNA = "always" )
table( nm$action  [sel.doub], useNA = "always" )
rm(sel.doub)
##
## Subset 05-10 Double missing
sel.doub <- which( nm$      dready2est==0 &
                   nm$      dskip     ==0 &
                   is.na(nm$p18_2005) ==TRUE &
                   is.na(nm$p18_2010) ==TRUE )
table( nm$action3 [sel.doub], useNA = "always" )
table( nm$action2 [sel.doub], useNA = "always" )
table( nm$action  [sel.doub], useNA = "always" )
rm(sel.doub)
##
## Subset 05-10 Double missing action2==split.to, baja 2022, no manip needed
sel.doub <- which( nm$      dready2est==0 &
                   nm$      dskip     ==0 &
                   nm$      action2   =="split.to" &
                   nm$      baja      ==2022 &
                   is.na(nm$p18_2005) ==TRUE &
                   is.na(nm$p18_2010) ==TRUE )
nm.w       <- nm       [sel.doub,]
drestore.w <- drestore [sel.doub,]
nm.w[, c("seccion","alta","baja","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
## use pre-sum.split 2020 for 2010
nm.w$p18_2005 <- nm.w$p18_2010 <- nm.w$p18_2020
nm.w$dready2est[nm.w$p18_2020!=0] <- 1 # indicate readiness if 2020 non null
nm.w$dskip     [nm.w$p18_2020==0] <- 1 # indicate skip if 2020 null
drestore.w$p18_2005 <- drestore.w$p18_2010 <- drestore.w$p18_2020 <- 1 # indicate restore
nm.w       -> nm       [sel.doub,] # return to data
drestore.w -> drestore [sel.doub,]
##
## Subset 05-10 Double missing action2==split.to, alta
sel.doub <- which( nm$      dready2est==0 &
                   nm$      dskip     ==0 &
                   nm$      action2   =="split.to" &
                   is.na(nm$baja)     ==TRUE &
                   is.na(nm$p18_2005) ==TRUE &
                   is.na(nm$p18_2010) ==TRUE )
nm.w       <- nm       [sel.doub,]
drestore.w <- drestore [sel.doub,]
nm.w[, c("seccion","alta","baja","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
## use pre-sum.split 2020 for 2010
nm.w$p18_2005 <- nm.w$p18_2010 <- nm.w$p18_2020
nm.w$dready2est[nm.w$p18_2020!=0] <- 1 # indicate readiness if 2020 non null
nm.w$dskip     [nm.w$p18_2020==0] <- 1 # indicate skip if 2020 null
drestore.w$p18_2005 <- drestore.w$p18_2010 <- drestore.w$p18_2020 <- 1 # indicate restore
nm.w       -> nm       [sel.doub,] # return to data
drestore.w -> drestore [sel.doub,]
##
## Subset 05-10 Double missing action==split.from, flat 2020
sel.doub <- which( nm$      dready2est==0 &
                   nm$      dskip     ==0 &
                   nm$      action    =="split.from" &
                   is.na(nm$baja)     ==TRUE &
                   is.na(nm$p18_2005) ==TRUE &
                   is.na(nm$p18_2010) ==TRUE )
nm.w       <- nm       [sel.doub,]
drestore.w <- drestore [sel.doub,]
table(nm.w$when)
nm.w[, c("seccion","alta","baja","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
nm.w$p18_2005 <- nm.w$p18_2010 <- nm.w$p18_2020
nm.w$dready2est[nm.w$p18_2020!=0] <- 1 # indicate readiness if 2020 non null
nm.w$dskip     [nm.w$p18_2020==0] <- 1 # indicate skip if 2020 null
drestore.w$p18_2005 <- drestore.w$p18_2010 <- drestore.w$p18_2020 <- 1 # indicate restore
nm.w       -> nm       [sel.doub,] # return to data
drestore.w -> drestore [sel.doub,]
##
## no unmanipulated 05-10 Double missing left
sel.doub <- which( nm$      dready2est==0 &
                   nm$      dskip     ==0 &
                   is.na(nm$p18_2005) ==TRUE &
                   is.na(nm$p18_2010) ==TRUE )
table( nm$action3 [sel.doub], useNA = "always" )
table( nm$action2 [sel.doub], useNA = "always" )
table( nm$action  [sel.doub], useNA = "always" )
rm(sel.doub)
##
## Subset 05-20 Double missing
sel.doub <- which( nm$      dready2est==0 &
                   nm$      dskip     ==0 &
                   is.na(nm$p18_2005) ==TRUE &
                   is.na(nm$p18_2020) ==TRUE )
table( act3=nm$action3 [sel.doub], useNA = "always" )
table( act2=nm$action2 [sel.doub], useNA = "always" )
table( act =nm$action  [sel.doub], useNA = "always" )
rm(sel.doub)
##
## Subset 05-20 Double missing action2==split.to, salvageable
sel.doub <- which( nm$      dready2est==0 &
                   nm$      dskip     ==0 &
                   nm$      action2   =="split.to" &
                   is.na(nm$p18_2005) ==TRUE &
                   is.na(nm$p18_2020) ==TRUE )
nm.w       <- nm       [sel.doub,]
drestore.w <- drestore [sel.doub,]
nm.w[, c("seccion","alta","baja","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
nm.w[, c("seccion","alta","baja","action", "orig.dest", "when", "p18_2005","p18_2010","p18_2020","dskip","dready2est")]
## make flat 2010 in 2005
nm.w <- sum.split(d=nm.w, year.var=2020, rnd=2, dfull=nm) # add-up pre-split population
nm.w$p18_2005 <- nm.w$p18_2010
## fill pre-split counterfactuals for estimation (operates in nm, so might change some pre-manipulated secciones, improving upon flat pop).
nm <- sum.split(d=nm.w, year.var=2020, rnd=2, dfull=nm, year.to.split=2010) # split pre-split.to pop
nm <- sum.split(d=nm.w, year.var=2020, rnd=2, dfull=nm, year.to.split=2005) # split pre-split.to pop
drestore <- sum.split(d=nm.w, year.var=2020, rnd=2, dfull=nm, year.to.split=2010, restore=TRUE ) # indicate restore
##tmp <- which(nm$seccion %in% 156573:156579)
##nm[tmp, c("p18_2005","p18_2010","p18_2020")]
##drestore[tmp,]
##prj1020(tmp, yr=2005) # This would be an alternative to flat 05
nm.w$dready2est[nm.w$p18_2010!=0] <- 1 # indicate readiness if 2010 non null
nm.w$dskip     [nm.w$p18_2010==0] <- 1 # indicate skip if 2010 null
drestore.w$p18_2005 <- drestore.w$p18_2010 <- drestore.w$p18_2020 <- 1 # indicate restore
nm.w       -> nm       [sel.doub,] # return to data
drestore.w -> drestore [sel.doub,]
##
## Subset 05-20 Double missing action==merged.to, unsalvageable 
sel.doub <- which( nm$      dready2est==0 &
                   nm$      dskip     ==0 &
                   nm$      action    =="merged.to" &
                   is.na(nm$p18_2005) ==TRUE &
                   is.na(nm$p18_2020) ==TRUE )
nm.w       <- nm       [sel.doub,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
## make flat 2010 in 2005
nm.w$p18_2005 <- nm.w$p18_2010 <- nm.w$p18_2020 <- 0
nm.w$dskip <- 1
nm.w -> nm [sel.doub,] # return to data
##
## Twice split that can't be figured out, then split that can be summed
sel.doub <- which(nm$seccion %in% c(161976))
nm.w       <- nm       [sel.doub,]
drestore.w <- drestore [sel.doub,]
nm.w[, c("seccion","alta","baja","action", "orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
nm.w[, c("seccion","alta","baja","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
nm.w[, c("seccion","alta","baja","action3","orig.dest3","when3","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
## 
nm.w <- sum.split(d=nm.w, year.var=2020, rnd=3, dfull=nm) # add-up pre-split population
## fill pre-split counterfactuals for estimation (works in nm, so might change some pre-manipulated secciones, improving upon flat pop).
nm <- sum.split(d=nm.w, year.var=2020, rnd=3, dfull=nm, year.to.split=2010) # split pre-split.to pop
nm <- sum.split(d=nm.w, year.var=2020, rnd=3, dfull=nm, year.to.split=2005) # split pre-split.to pop
drestore <- sum.split(d=nm.w, year.var=2020, rnd=3, dfull=nm, year.to.split=2010, restore=TRUE ) # indicate restore
##tmp <- which(nm$seccion %in% 162678:162699)
##nm[tmp, c("p18_2005","p18_2010","p18_2020")]
nm.w$dready2est <- 1 # indicate readiness
drestore.w$p18_2005 <- drestore.w$p18_2010 <- drestore.w$p18_2020 <- 1 # indicate restore
nm.w       -> nm       [sel.doub,] # return to data
drestore.w -> drestore [sel.doub,]
##
## Twice split that can't be figured out, then split that can be summed
sel.doub <- which(nm$dskip==0 & nm$dready2est==0 &
                 nm$action3=="split.to")
nm.w       <- nm       [sel.doub,]
drestore.w <- drestore [sel.doub,]
## only action3 can be salvaged, ignore previous
nm.w[, c("seccion","alta","baja","action" ,"orig.dest" ,"when" ,"p18_2005","p18_2010","p18_2020","dskip","dready2est")]
nm.w[, c("seccion","alta","baja","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
nm.w[, c("seccion","alta","baja","action3","orig.dest3","when3","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
## 
nm.w <- sum.split(d=nm.w, year.var=2020, rnd=3, dfull=nm) # add-up pre-split population
## fill pre-split counterfactuals for estimation (works in nm, so might change some pre-manipulated secciones, improving upon flat pop).
nm <-   sum.split(d=nm.w, year.var=2020, rnd=3, dfull=nm, year.to.split=2010) # split pre-split.to pop
nm <-   sum.split(d=nm.w, year.var=2020, rnd=3, dfull=nm, year.to.split=2005) # split pre-split.to pop
drestore <- sum.split(d=nm.w, year.var=2020, rnd=3, dfull=nm, year.to.split=2010, restore=TRUE ) # indicate restore
##tmp <- which(nm$seccion %in% 192809:192822)
##nm[tmp, c("p18_2005","p18_2010","p18_2020")]
nm.w$dready2est <- 1 # indicate readiness
drestore.w$p18_2005 <- drestore.w$p18_2010 <- drestore.w$p18_2020 <- 1 # indicate restore
nm.w       -> nm       [sel.doub,] # return to data
drestore.w -> drestore [sel.doub,]
rm(sel.doub)
##
############################################################################################
## These are hard cases where sección was split w/o baja --- ie. lost some territory to   ##
## another seccion, sometimes new but not always. Will ignore these splits w/o attempting ##
## to sum.split() population...                                                           ##
############################################################################################
## Twice split wo baja
sel.tmp <- which(nm$seccion %in% c(190647))
nm$dready2est[sel.tmp] <- 1
## Split wo baja in action2, flat 2010 for 2005 if 2005 is NA
sel.tmp <- which(nm$seccion %in% c(192197, 192260, 192261, 192273, 192333, 161976, 122710))
nm.w       <- nm       [sel.tmp,]
drestore.w <- drestore [sel.tmp,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
nm.w[, c("seccion","alta","baja","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
nm.w[, c("seccion","alta","baja","action3","orig.dest3","when3","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
nm.w$p18_2005[is.na(nm.w$p18_2005)] <- nm.w$p18_2010[is.na(nm.w$p18_2005)]
nm.w$dready2est <- 1 # indicate readiness
drestore.w$p18_2005 <- drestore.w$p18_2010 <- drestore.w$p18_2020 <- 1 # indicate restore
nm.w       -> nm       [sel.tmp,] # return to data
drestore.w -> drestore [sel.tmp,]
rm(sel.tmp)
## Split wo baja in action, no further change
sel.tmp <- which(nm$seccion %in%  c(10451, 20170, 20709, 20710, 20725, 20733, 21297, 21298, 21368, 40339, 40485, 50171, 50279, 50285, 50449, 50717, 51089, 70014, 70015, 70018, 70019, 71044, 71047, 71419, 71435, 71436, 71836, 71837, 80034, 80058, 80059, 90774, 93951, 93976, 94151, 94153, 94154, 94156, 94157, 94163, 94164, 94237, 100055, 101234, 120742, 120760, 120761, 120762, 121174, 121200, 121709, 121712, 121713, 121714, 121744, 121754, 121759, 121762, 122051, 122527, 122706, 122716, 130742, 130900, 130928, 130936, 130938, 130953, 140137, 142563, 142719, 143084, 143088, 150054, 150633, 150634, 150638, 150670, 150938, 151276, 152025, 152027, 152028, 152459, 152460, 152462, 153800, 153866, 153922, 153941, 154004, 154028, 154032, 154068, 154079, 154084, 154086, 154100, 154206, 154257, 154277, 154305, 154308, 154342, 154413, 154601, 154602, 154603, 154614, 154839, 155442, 155461, 155465, 155480, 155681, 155787, 155826, 155829, 155867, 160001, 160002, 160003, 160005, 160324, 160759, 160760, 161256, 161914, 161971, 161991, 162153, 162238, 162346, 162531, 162533, 170619, 180002, 190066, 190107, 190145, 190146, 190150, 190567, 190568, 190588, 190590, 190620, 190804, 190852, 191705, 191706, 191770, 191772, 200784, 201199, 201532, 201726, 201728, 201749, 201997, 202086, 211079, 211128, 211416, 220481, 220502, 220511, 220513, 220542, 220550, 220554, 230092, 230171, 230172, 230175, 230177, 230179, 230180, 230261, 230290, 240176, 261154, 290047, 290293, 290400, 290401, 290402, 290578, 300480, 301097, 301144, 301303, 301523, 301525, 301591, 302381, 302383, 302519, 302521, 302659, 302661, 302979, 303102, 303105, 303106, 303366, 303367, 303385, 303388, 303706, 303712, 303730, 303792, 304158, 304507, 304616, 304631, 310223, 310652, 310991) & nm$dskip==0 & nm$dready2est==0 & nm$action2=="")
#
nm.w <- nm[sel.tmp,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
nm.w$dready2est <- 1 # indicate readiness
nm.w -> nm [sel.tmp,] # return to data
rm(sel.tmp)
##
#####################
## Merged.to cases ##
#####################
sel.tmp <- which(nm$dskip==0 & nm$dready2est==0 & nm$action3=="merged.to")
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
## merged.to cases in action 2
sel.tmp <- which(nm$dskip==0 & nm$dready2est==0 & nm$action2=="merged.to")
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
nm.w       <- nm       [sel.tmp,]
drestore.w <- drestore [sel.tmp,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dready2est")]
nm.w$p18_2020 <- nm.w$p18_2010 # flat post 2010
nm.w$dready2est[nm.w$p18_2010!=0] <- 1 # indicate readiness if 2010 non null
nm.w$dskip     [nm.w$p18_2010==0] <- 1 # indicate skip if 2010 null
drestore.w$p18_2005 <- drestore.w$p18_2010 <- drestore.w$p18_2020 <- 1 # indicate restore
nm.w       -> nm       [sel.tmp,] # return to data
drestore.w -> drestore [sel.tmp,]
rm(sel.tmp)
##
## merged.to in action
sel.tmp <- which(nm$dskip==0 & nm$dready2est==0 & nm$action=="merged.to")
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "always")
nm.w       <- nm       [sel.tmp,]
drestore.w <- drestore [sel.tmp,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020")]
nm.w$p18_2020 <- nm.w$p18_2010 # flat post 2010
sel.tmp2 <- which(nm.w$p18_2010==0)
nm.w$p18_2010[sel.tmp2] <- nm.w$p18_2005[sel.tmp2] # unless 2010 null, flat 2005
nm.w$p18_2020[sel.tmp2] <- nm.w$p18_2010[sel.tmp2] # unless 2010 null, flat 2005
rm(sel.tmp2)
nm.w$dready2est[nm.w$p18_2010!=0] <- 1 # indicate readiness if 2010 non null
nm.w$dskip     [nm.w$p18_2010==0] <- 1 # indicate skip if 2010 null
drestore.w$p18_2005 <- drestore.w$p18_2010 <- drestore.w$p18_2020 <- 1 # indicate restore
nm.w       -> nm       [sel.tmp,] # return to data
drestore.w -> drestore [sel.tmp,]
rm(sel.tmp)
##
####################
## Split.to cases ##
####################
sel.tmp <- which(nm$dskip==0 & nm$dready2est==0)
table(nm$action3[sel.tmp])
table(nm$action2[sel.tmp])
table(nm$action [sel.tmp])
## split.to in action 2
sel.tmp <- which(nm$dskip==0 & nm$dready2est==0 & nm$action2=="split.to")
nm.w <- nm[sel.tmp,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020")]
## two special cases: ignore split wo baja
sel.tmp <- which(nm$seccion %in% c(21368, 191706))
nm.w       <- nm       [sel.tmp,]
drestore.w <- drestore [sel.tmp,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dready2est")]
nm.w <- sum.split(d=nm.w, year.var=2020, rnd=2, dfull=nm) # add-up pre-split population
## fill pre-split counterfactuals for estimation (works in nm, so might change some pre-manipulated secciones, improving upon flat pop).
nm <-   sum.split(d=nm.w, year.var=2020, rnd=2, dfull=nm, year.to.split=2010) # split pre-split.to pop
nm <-   sum.split(d=nm.w, year.var=2020, rnd=2, dfull=nm, year.to.split=2005) # split pre-split.to pop
drestore <- sum.split(d=nm.w, year.var=2020, rnd=2, dfull=nm, year.to.split=2010, restore=TRUE ) # indicate restore
#tmp <- which(nm$seccion %in% 21868:21942)
#nm[tmp, c("p18_2005","p18_2010","p18_2020")]
nm.w$dready2est <- 1 # indicate readiness if 2010 non null
drestore.w$p18_2005 <- drestore.w$p18_2010 <- drestore.w$p18_2020 <- 1 # indicate restore
nm.w       -> nm       [sel.tmp,] # return to data
drestore.w -> drestore [sel.tmp,]
rm(sel.tmp)
## split.to in 2022
sel.tmp <- which(nm$dskip==0 & nm$dready2est==0 & nm$action2=="split.to" & nm$when2==2022)
nm.w       <- nm       [sel.tmp,]
drestore.w <- drestore [sel.tmp,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dready2est")]
nm.w$p18_2005[is.na(nm.w$p18_2005)] <- nm.w$p18_2010[is.na(nm.w$p18_2005)] # flat pre 2010 if NA
nm.w$dready2est <- 1 # indicate readiness
drestore.w$p18_2005 <- drestore.w$p18_2010 <- drestore.w$p18_2020 <- 1 # indicate restore
nm.w       -> nm       [sel.tmp,] # return to data
drestore.w -> drestore [sel.tmp,]
rm(sel.tmp)
## remainder split.tos in 2019 or 2020
sel.tmp <- which(nm$dskip==0 & nm$dready2est==0 & nm$action2=="split.to")
nm.w       <- nm       [sel.tmp,]
drestore.w <- drestore [sel.tmp,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dready2est")]
nm.w <- sum.split(d=nm.w, year.var=2020, rnd=2, dfull=nm) # add-up pre-split population
## fill pre-split counterfactuals for estimation (works in nm, so might change some pre-manipulated secciones, improving upon flat pop).
nm <-   sum.split(d=nm.w, year.var=2020, rnd=2, dfull=nm, year.to.split=2010) # split pre-split.to pop
nm <-   sum.split(d=nm.w, year.var=2020, rnd=2, dfull=nm, year.to.split=2005) # split pre-split.to pop
drestore <- sum.split(d=nm.w, year.var=2020, rnd=2, dfull=nm, year.to.split=2010, restore=TRUE ) # indicate restore
##tmp <- which(nm$seccion %in% 22017:22033)
##nm[tmp, c("p18_2005","p18_2010","p18_2020")]
nm.w$dready2est <- 1 # indicate readiness
drestore.w$p18_2005 <- drestore.w$p18_2010 <- drestore.w$p18_2020 <- 1 # indicate restore
nm.w       -> nm       [sel.tmp,] # return to data
drestore.w -> drestore [sel.tmp,]
rm(sel.tmp)
##
## split.to in action
sel.tmp <- which(nm$dskip==0 & nm$dready2est==0 & nm$action=="split.to")
nm.w <- nm[sel.tmp,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dready2est")]
## split.to in 2022 no manip needed
sel.tmp <- which(nm$dskip==0 & nm$dready2est==0 & nm$action=="split.to" & nm$when==2022)
nm.w <- nm[sel.tmp,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dready2est")]
nm.w$dready2est <- 1 # indicate readiness
nm.w -> nm [sel.tmp,] # return to data
rm(sel.tmp)
## split.to in action
sel.tmp <- which(nm$dskip==0 & nm$dready2est==0 & nm$action=="split.to" & nm$when>2010 & !is.na(nm$baja))
nm.w       <- nm       [sel.tmp,]
drestore.w <- drestore [sel.tmp,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dready2est")]
nm.w <- sum.split(d=nm.w, year.var=2020, rnd=1, dfull=nm) # add-up pre-split population
## fill pre-split counterfactuals for estimation (works in nm, so might change some pre-manipulated secciones, improving upon flat pop).
nm <-   sum.split(d=nm.w, year.var=2020, rnd=1, dfull=nm, year.to.split=2010) # split pre-split.to pop
nm <-   sum.split(d=nm.w, year.var=2020, rnd=1, dfull=nm, year.to.split=2005) # split pre-split.to pop
drestore <- sum.split(d=nm.w, year.var=2020, rnd=1, dfull=nm, year.to.split=2010, restore=TRUE ) # indicate restore
##tmp <- which(nm$seccion %in% 311107:311129)
##nm[tmp, c("p18_2005","p18_2010","p18_2020")]
nm.w$dready2est <- 1 # indicate readiness
drestore.w$p18_2005 <- drestore.w$p18_2010 <- drestore.w$p18_2020 <- 1 # indicate restore
nm.w       -> nm       [sel.tmp,] # return to data
drestore.w -> drestore [sel.tmp,]
rm(sel.tmp)
## rest are split.tos wo baja, no manip
sel.tmp <- which(nm$dskip==0 & nm$dready2est==0 & nm$action=="split.to")
nm.w <- nm[sel.tmp,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dready2est")]
nm.w$dready2est <- 1 # indicate readiness
nm.w -> nm [sel.tmp,] # return to data
rm(sel.tmp)
##
#####################################
## Split.from cases: unsalvageable ##
#####################################
sel.tmp <- which(nm$dskip==0 & nm$dready2est==0 & nm$action=="split.from")
nm.w <- nm[sel.tmp,]
table(nm.w$when)
nm.w$seccion
## pre 2005 no manip
sel.tmp <- which(nm$dskip==0 & nm$dready2est==0 & nm$action=="split.from" & nm$when<2005)
nm.w <- nm[sel.tmp,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dready2est")]
nm.w$dready2est <- 1 # indicate readiness
nm.w -> nm [sel.tmp,] # return to data
rm(sel.tmp)
## 2005-2010
sel.tmp <- which(nm$dskip==0 & nm$dready2est==0 & nm$action=="split.from" & nm$when<2010)
nm.w       <- nm       [sel.tmp,]
drestore.w <- drestore [sel.tmp,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dready2est")]
nm.w$p18_2005[is.na(nm.w$p18_2005)] <- nm.w$p18_2010[is.na(nm.w$p18_2005)] # flat pre 2010 if NA
nm.w$dready2est <- 1 # indicate readiness
drestore.w$p18_2005 <- drestore.w$p18_2010 <- drestore.w$p18_2020 <- 1 # indicate restore
nm.w       -> nm       [sel.tmp,] # return to data
drestore.w -> drestore [sel.tmp,]
rm(sel.tmp)
## rest are 2011
sel.tmp <- which(nm$dskip==0 & nm$dready2est==0 & nm$action=="split.from")
nm.w       <- nm       [sel.tmp,]
drestore.w <- drestore [sel.tmp,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dready2est")]
nm.w$p18_2005[is.na(nm.w$p18_2005)] <- nm.w$p18_2010[is.na(nm.w$p18_2005)] # flat pre 2010 if NA
## if 2010 zero, flat 2020 across the board
sel.tmp2 <- which(nm.w$p18_2010==0)
nm.w$p18_2005[sel.tmp2] <- nm.w$p18_2010[sel.tmp2] <- nm.w$p18_2020[sel.tmp2] # flat pre 2020 if 0
rm(sel.tmp2)
## if 2020 zero too, skip
sel.tmp2 <- which(nm.w$p18_2020==0)
nm.w$dready2est[-sel.tmp2] <- 1 # indicate readiness
nm.w$dskip     [ sel.tmp2] <- 1 # indicate skip
rm(sel.tmp2)
drestore.w$p18_2005 <- drestore.w$p18_2010 <- drestore.w$p18_2020 <- 1 # indicate restore
nm.w       -> nm       [sel.tmp,] # return to data
drestore.w -> drestore [sel.tmp,]
rm(sel.tmp)
##
## four cases that have no 2005 info: linear (no restore)
sel.tmp <- which(nm$dskip==0 & nm$dready2est==0)
nm.w <- nm[sel.tmp,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dready2est")]
nm.w <- within(nm.w, {
    p18_2005 <- prj1020(nm.w, 2005)
    dready2est <- 1
})
nm.w -> nm [sel.tmp,] # return to data
rm(sel.tmp)
##
## no cases left...
which(nm$dskip==0 & nm$dready2est==0)
## ... yet some special cases xsneed further manipulation
## Case has zero pop 05 and 20 but not 10, fix arbitrarily (no restore)
sel.tmp <- which(nm$seccion==200457)
show(nm[,c("seccion","inegi","p18_2005","p18_2010","p18_2020")], rows=sel.tmp, after=3, before=3)
nm$p18_2005[sel.tmp] <- 500
nm$p18_2020[sel.tmp] <- 800 
##
sel.tmp <- which(nm$seccion==200802)
show(nm[,c("seccion","inegi","p18_2005","p18_2010","p18_2020")], rows=sel.tmp, after=2, before=2)
nm$p18_2020[sel.tmp] <- 100
##
sel.tmp <- which(nm$seccion==201305)
show(nm[,c("seccion","baja","inegi","p18_2005","p18_2010","p18_2020")], rows=sel.tmp, after=2, before=2)
nm$p18_2020[sel.tmp] <- 500
##
## These have zero pop 05-10-20
sel.tmp <- which(nm$seccion %in% c(71529, 122374, 151965, 152035, 152710, 152712, 152713, 152714, 152715, 152718, 152719, 152720, 152722, 230450, 290526))
nm.w <- nm[sel.tmp,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dskip")]
nm.w$p18_2005[is.na(nm.w$p18_2005)] <- nm.w$p18_2010[is.na(nm.w$p18_2005)] # flat pre 2010 if NA
nm.w$dready2est <- 0 # indicate readiness
nm.w$dskip <- 1      # indicate skip
nm.w -> nm [sel.tmp,] # return to data
rm(sel.tmp)
##
## These secciones have odd zeroes in one census, arbitrarily add given other years' pops
sel.tmp <- which(nm$seccion==50153); nm$p18_2020[sel.tmp] <- 10
sel.tmp <- which(nm$seccion==51468); nm$p18_2005[sel.tmp] <- 700
sel.tmp <- which(nm$seccion==70385); nm$p18_2005[sel.tmp] <- 200
sel.tmp <- which(nm$seccion==70386); nm$p18_2005[sel.tmp] <- 400
sel.tmp <- which(nm$seccion==71753); nm$p18_2010[sel.tmp] <- 200
sel.tmp <- which(nm$seccion==81942); nm$p18_2010[sel.tmp] <- 1500
sel.tmp <- which(nm$seccion==90479); nm$p18_2010[sel.tmp] <- 100
sel.tmp <- which(nm$seccion==90479); nm$p18_2020[sel.tmp] <- 100
sel.tmp <- which(nm$seccion==91040); nm$p18_2010[sel.tmp] <- 200
sel.tmp <- which(nm$seccion==92054); nm$p18_2010[sel.tmp] <- 10
sel.tmp <- which(nm$seccion==92054); nm$p18_2020[sel.tmp] <- 10
sel.tmp <- which(nm$seccion==92085); nm$p18_2010[sel.tmp] <- 10
sel.tmp <- which(nm$seccion==92085); nm$p18_2020[sel.tmp] <- 10
sel.tmp <- which(nm$seccion==92428); nm$p18_2020[sel.tmp] <- 600
sel.tmp <- which(nm$seccion==93107); nm$p18_2010[sel.tmp] <- 10
sel.tmp <- which(nm$seccion==93376); nm$p18_2010[sel.tmp] <- 10
sel.tmp <- which(nm$seccion==93376); nm$p18_2020[sel.tmp] <- 10
sel.tmp <- which(nm$seccion==93531); nm$p18_2010[sel.tmp] <- 10
sel.tmp <- which(nm$seccion==93531); nm$p18_2020[sel.tmp] <- 10
sel.tmp <- which(nm$seccion==93886); nm$p18_2010[sel.tmp] <- 15
sel.tmp <- which(nm$seccion==93887); nm$p18_2020[sel.tmp] <- 200
sel.tmp <- which(nm$seccion==94988); nm$p18_2010[sel.tmp] <- 5
sel.tmp <- which(nm$seccion==94988); nm$p18_2020[sel.tmp] <- 5
sel.tmp <- which(nm$seccion==94989); nm$p18_2020[sel.tmp] <- 5
sel.tmp <- which(nm$seccion==94991); nm$p18_2010[sel.tmp] <- 5
sel.tmp <- which(nm$seccion==94991); nm$p18_2020[sel.tmp] <- 5
sel.tmp <- which(nm$seccion==95501); nm$p18_2010[sel.tmp] <- 5
sel.tmp <- which(nm$seccion==95501); nm$p18_2020[sel.tmp] <- 5
sel.tmp <- which(nm$seccion==100370); nm$p18_2005[sel.tmp] <- 3
sel.tmp <- which(nm$seccion==112266); nm$p18_2010[sel.tmp] <- 100
sel.tmp <- which(nm$seccion==120473); nm$p18_2005[sel.tmp] <- 400
sel.tmp <- which(nm$seccion==120681); nm$p18_2020[sel.tmp] <- 300
sel.tmp <- which(nm$seccion==121180); nm$p18_2005[sel.tmp] <- 100
sel.tmp <- which(nm$seccion==121381); nm$p18_2020[sel.tmp] <- 60
sel.tmp <- which(nm$seccion==121385); nm$p18_2010[sel.tmp] <- 1
sel.tmp <- which(nm$seccion==121385); nm$p18_2020[sel.tmp] <- 1
sel.tmp <- which(nm$seccion==122356); nm$p18_2005[sel.tmp] <- 50
sel.tmp <- which(nm$seccion==122356); nm$p18_2020[sel.tmp] <- 50
sel.tmp <- which(nm$seccion==122363); nm$p18_2010[sel.tmp] <- 100
sel.tmp <- which(nm$seccion==122768); nm$p18_2005[sel.tmp] <- 400
sel.tmp <- which(nm$seccion==142167); nm$p18_2005[sel.tmp] <- 10
sel.tmp <- which(nm$seccion==151294); nm$p18_2020[sel.tmp] <- 10
sel.tmp <- which(nm$seccion==152311); nm$p18_2005[sel.tmp] <- 100
sel.tmp <- which(nm$seccion==152313); nm$p18_2005[sel.tmp] <- 100
sel.tmp <- which(nm$seccion==152717); nm$p18_2005[sel.tmp] <- 100
sel.tmp <- which(nm$seccion==152721); nm$p18_2005[sel.tmp] <- 500
sel.tmp <- which(nm$seccion==153206); nm$p18_2010[sel.tmp] <- 10
sel.tmp <- which(nm$seccion==153206); nm$p18_2020[sel.tmp] <- 10
sel.tmp <- which(nm$seccion==154433); nm$p18_2010[sel.tmp] <- 10
sel.tmp <- which(nm$seccion==154433); nm$p18_2020[sel.tmp] <- 10
sel.tmp <- which(nm$seccion==154540); nm$p18_2005[sel.tmp] <- 1000
sel.tmp <- which(nm$seccion==154642); nm$p18_2020[sel.tmp] <- 1250
sel.tmp <- which(nm$seccion==155918); nm$p18_2005[sel.tmp] <- 200
sel.tmp <- which(nm$seccion==155980); nm$p18_2020[sel.tmp] <- 2
sel.tmp <- which(nm$seccion==162621); nm$p18_2005[sel.tmp] <- 500
sel.tmp <- which(nm$seccion==170235); nm$p18_2010[sel.tmp] <- 10
sel.tmp <- which(nm$seccion==170235); nm$p18_2020[sel.tmp] <- 10
sel.tmp <- which(nm$seccion==180294); nm$p18_2005[sel.tmp] <- 500
sel.tmp <- which(nm$seccion==180432); nm$p18_2005[sel.tmp] <- 100
sel.tmp <- which(nm$seccion==200175); nm$p18_2005[sel.tmp] <- 100
sel.tmp <- which(nm$seccion==200222); nm$p18_2010[sel.tmp] <- 500
sel.tmp <- which(nm$seccion==200463); nm$p18_2005[sel.tmp] <- 700
sel.tmp <- which(nm$seccion==200904); nm$p18_2020[sel.tmp] <- 40
sel.tmp <- which(nm$seccion==201694); nm$p18_2005[sel.tmp] <- 100
sel.tmp <- which(nm$seccion==201694); nm$p18_2010[sel.tmp] <- 100
sel.tmp <- which(nm$seccion==202285); nm$p18_2020[sel.tmp] <- 200
sel.tmp <- which(nm$seccion==202453); nm$p18_2010[sel.tmp] <- 50
sel.tmp <- which(nm$seccion==210142); nm$p18_2020[sel.tmp] <- 50
sel.tmp <- which(nm$seccion==211492); nm$p18_2010[sel.tmp] <- 500
sel.tmp <- which(nm$seccion==211493); nm$p18_2010[sel.tmp] <- 10
sel.tmp <- which(nm$seccion==211493); nm$p18_2020[sel.tmp] <- 10
sel.tmp <- which(nm$seccion==220742); nm$p18_2010[sel.tmp] <- 10
sel.tmp <- which(nm$seccion==220742); nm$p18_2020[sel.tmp] <- 10
sel.tmp <- which(nm$seccion==250422); nm$p18_2005[sel.tmp] <- 10
sel.tmp <- which(nm$seccion==250492); nm$p18_2005[sel.tmp] <- 50
sel.tmp <- which(nm$seccion==250493); nm$p18_2005[sel.tmp] <- 50
sel.tmp <- which(nm$seccion==250624); nm$p18_2005[sel.tmp] <- 50
sel.tmp <- which(nm$seccion==250647); nm$p18_2020[sel.tmp] <- 150
sel.tmp <- which(nm$seccion==251331); nm$p18_2005[sel.tmp] <- 100
sel.tmp <- which(nm$seccion==251832); nm$p18_2005[sel.tmp] <- 50
sel.tmp <- which(nm$seccion==251902); nm$p18_2005[sel.tmp] <- 5
sel.tmp <- which(nm$seccion==251993); nm$p18_2020[sel.tmp] <- 2
sel.tmp <- which(nm$seccion==251994); nm$p18_2020[sel.tmp] <- 350
sel.tmp <- which(nm$seccion==252053); nm$p18_2005[sel.tmp] <- 50
sel.tmp <- which(nm$seccion==252239); nm$p18_2005[sel.tmp] <- 2
sel.tmp <- which(nm$seccion==252239); nm$p18_2010[sel.tmp] <- 2
sel.tmp <- which(nm$seccion==252304); nm$p18_2005[sel.tmp] <- 200
sel.tmp <- which(nm$seccion==252316); nm$p18_2005[sel.tmp] <- 400
sel.tmp <- which(nm$seccion==252415); nm$p18_2005[sel.tmp] <- 50
sel.tmp <- which(nm$seccion==252466); nm$p18_2005[sel.tmp] <- 100
sel.tmp <- which(nm$seccion==253040); nm$p18_2005[sel.tmp] <- 15
sel.tmp <- which(nm$seccion==253174); nm$p18_2005[sel.tmp] <- 3
sel.tmp <- which(nm$seccion==253174); nm$p18_2010[sel.tmp] <- 3
sel.tmp <- which(nm$seccion==253386); nm$p18_2020[sel.tmp] <- 100
sel.tmp <- which(nm$seccion==253388); nm$p18_2010[sel.tmp] <- 1
sel.tmp <- which(nm$seccion==253388); nm$p18_2020[sel.tmp] <- 1
sel.tmp <- which(nm$seccion==253545); nm$p18_2005[sel.tmp] <- 4
sel.tmp <- which(nm$seccion==253590); nm$p18_2005[sel.tmp] <- 100
sel.tmp <- which(nm$seccion==253590); nm$p18_2020[sel.tmp] <- 500
sel.tmp <- which(nm$seccion==253772); nm$p18_2020[sel.tmp] <- 900
sel.tmp <- which(nm$seccion==270459); nm$p18_2010[sel.tmp] <- 10
sel.tmp <- which(nm$seccion==270459); nm$p18_2020[sel.tmp] <- 10
sel.tmp <- which(nm$seccion==270511); nm$p18_2010[sel.tmp] <- 10
sel.tmp <- which(nm$seccion==270511); nm$p18_2020[sel.tmp] <- 5
sel.tmp <- which(nm$seccion==270750); nm$p18_2010[sel.tmp] <- 500
sel.tmp <- which(nm$seccion==270797); nm$p18_2010[sel.tmp] <- 50
sel.tmp <- which(nm$seccion==271087); nm$p18_2010[sel.tmp] <- 300
sel.tmp <- which(nm$seccion==271133); nm$p18_2010[sel.tmp] <- 200
sel.tmp <- which(nm$seccion==280700); nm$p18_2005[sel.tmp] <- 8
sel.tmp <- which(nm$seccion==280785); nm$p18_2010[sel.tmp] <- 100
sel.tmp <- which(nm$seccion==281067); nm$p18_2010[sel.tmp] <- 700
sel.tmp <- which(nm$seccion==281093); nm$p18_2010[sel.tmp] <- 10
sel.tmp <- which(nm$seccion==281093); nm$p18_2020[sel.tmp] <- 10
sel.tmp <- which(nm$seccion==281578); nm$p18_2010[sel.tmp] <- 10
sel.tmp <- which(nm$seccion==281578); nm$p18_2020[sel.tmp] <- 10
sel.tmp <- which(nm$seccion==290339); nm$p18_2005[sel.tmp] <- 50
sel.tmp <- which(nm$seccion==290508); nm$p18_2005[sel.tmp] <- 100
sel.tmp <- which(nm$seccion==301225); nm$p18_2010[sel.tmp] <- 5
sel.tmp <- which(nm$seccion==301225); nm$p18_2020[sel.tmp] <- 5
sel.tmp <- which(nm$seccion==303762); nm$p18_2020[sel.tmp] <- 500
sel.tmp <- which(nm$seccion==320113); nm$p18_2020[sel.tmp] <- 250
sel.tmp <- which(nm$seccion==320348); nm$p18_2005[sel.tmp] <- 60
sel.tmp <- which(nm$seccion==320813); nm$p18_2005[sel.tmp] <- 200
rm(sel.tmp)
##
## Make 2010 census linear with 2005 and 2020 because too low in these cases (no restore)
sel.tmp <- which(nm$seccion %in% c(160829, 303230, 50718, 71879, 200221, 230616, 71640, 71800, 260645, 281040, 281061, 281063, 51437, 71739, 71773, 71832, 71834, 281041, 51049, 71638, 71742, 71744, 71771, 71841, 71850, 71856, 71916, 91734, 94990, 101214, 130585, 153849, 154024, 154393, 156019, 201813, 210025, 250362, 250416, 260306, 260307, 260643, 281042, 281062, 281068, 290490, 300090, 300697, 300715, 302274, 302456, 302625, 302839, 303187, 20554, 21386, 30352, 50442, 50717, 51105, 51110, 51271, 51276, 51305, 51309, 51316, 51396, 71443, 71602, 71829, 71831, 71843, 71848, 71878, 82253, 82842, 91279, 112059, 120444, 121658, 122149, 122777, 130022, 130218, 130339, 130395, 130800, 131083, 131446, 131559, 142273, 142729, 143164, 150080, 150101, 150129, 150147, 150485, 150901, 152000, 152349, 152394, 152770, 152787, 154025, 154195, 154416, 154544, 155752, 155757, 155926, 156014, 156143, 160077, 160149, 160827, 160866, 170353, 170377, 170596, 180295, 180418, 180741, 190193, 190194, 191192, 191236, 200842, 201172, 211564, 211618, 211647, 211648, 211913, 211915, 212222, 220554, 241447, 241650, 250632, 250995, 251293, 251603, 251767, 251844, 251845, 251852, 252644, 252960, 253007, 253041, 253074, 260081, 260082, 260160, 260305, 260760, 261110, 261325, 270580, 270660, 270665, 280785, 281038, 281057, 281064, 300409, 301223, 301524, 301777, 302445, 302477, 302626, 302816, 302863, 303232, 303233, 303297, 303451, 310992, 321035))
nm[sel.tmp, c("seccion","alta","baja","p18_2005","p18_2010","p18_2020")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2010 <- prj0520(nm[sel.tmp,], 2010)
})
##
sel.tmp <- which(nm$dskip==0 & nm$dready2est==0)
nm.w <- nm[sel.tmp,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dready2est")]
##
## Manipulation left new cases that have zeroes across the board 
sel.tmp <- which(
    nm$ dready2est==0 &
    nm$ dskip     ==0 &
    nm$ p18_2005==0 &
    nm$ p18_2010==0 &
    nm$ p18_2020==0)
nm.w <- nm [sel.tmp,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
## nm.w$dskip <- 1 # indicate skip
## nm.w -> nm [sel.tmp,] # return to data
##
## skip with some info?
sel.tmp <- which(
    nm$ dskip   ==1 &
    (nm$ p18_2005>0 |
    nm$ p18_2010>0 |
    nm$ p18_2020>0))
nm.w <- nm [sel.tmp,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
table(nm[nm$dskip==1, grep("p18_", colnames(nm))])
##
########################
## Verify census pops ##
########################
table(is.na(nm$p18_2005[nm$dskip==0 | nm$dready2est==1]))
table(is.na(nm$p18_2010[nm$dskip==0 | nm$dready2est==1]))
table(is.na(nm$p18_2020[nm$dskip==0 | nm$dready2est==1]))
table(      nm$p18_2005[nm$dskip==0 | nm$dready2est==1]==0)
table(      nm$p18_2010[nm$dskip==0 | nm$dready2est==1]==0)
table(      nm$p18_2020[nm$dskip==0 | nm$dready2est==1]==0)
## sel.tmp <- which(nm$dskip==0 & nm$dready2est==1 & (nm$p18_2005==0 | nm$p18_2010==0 | nm$p18_2020==0))
## nm$seccion[sel.tmp]
## nm[sel.tmp, c("seccion","alta","baja","action","orig.dest","when","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020")]
## ##
## table(nm[nm$dready2est==1, c("p18_2005","p18_2010","p18_2020")] <= 0)
## nm[which(nm$dready2est==1 & nm$p18_2005 <= 0), c("p18_2005","p18_2010","p18_2020")]
## nm[which(nm$dready2est==1 & nm$p18_2010 <= 0), c("p18_2005","p18_2010","p18_2020")]
## nm[which(nm$dready2est==1 & nm$p18_2020 <= 0), c("p18_2005","p18_2010","p18_2020")]
## ## set arbitrarilly to 50% of 2010 pop
## nm$p18_2005 [which(nm$dready2est==1 & nm$p18_2005 <= 0)] <- nm$p18_2010 [which(nm$dready2est==1 & nm$p18_2005 <= 0)] / 2
## #nm$p18_2010 [which(nm$dready2est==1 & nm$p18_2010 <= 0)] <- 3
## nm$p18_2020 [which(nm$dready2est==1 & nm$p18_2020 <= 0)] <- nm$p18_2010 [which(nm$dready2est==1 & nm$p18_2020 <= 0)] / 2
##
## Check that dummies are mutually exclusive and exhaustive. if so, any single dummy selects cases ok
table(skip=nm$dskip,    ready=nm$dready2est)                      ## mutually exclusive
if (length(sel.tmp <- which(nm$dskip==0 & nm$dready2est==0) )==0) print("they're exhaustive") ## exhaustive (length==0)
#####################
## Block ends here ##
#####################

########################################
## Indicate 1st seccion in each state ##
########################################
nm$dfirst <- 0
nm <- nm[order(nm$ord),] ## sort
nm$dfirst[duplicated(nm$edon)==FALSE] <- 1
table(nm$dfirst==1) # --> there should be 32 TRUE if all ok


## MOVE TO EXPORT...
##
##################################################################################
## Apply my_agg to generate state aggregates (nm[sel.r,]$m:2005-2010-2020),     ##
## will be used in projeection routine below.                                   ##
## Will recompute them after restoring unmanipulated censos.                    ##
## (EMM Meaning of Last sentence unclear...                                     ##
##################################################################################
## ##
#############################################################################################################
## (Discovered when using municipio aggregates in prior incarnation of the routine.)                       ##
## Ojo: In some cases, these aggs don't equal saved municipal-level census pops due to remunicipalización. ##
## E.g., Bacalar appears in 2005 and 2010 censuses, but its legal birth (1st municipal election) is 2013,  ##
## and therefore gets zero pop in saved (remunicipalización-corrected) 2005 and 2010 census data.          ##
## Will not fix this here, despite mismatches in checks below, as 2005 and 2010 aid in using interlog to   ##
## generate sección-level projections. Will fix after aggregation.                                         ##
#############################################################################################################
## fill in p18s for aggregation
nm <- within(nm, {
    p18e_05 <- p18_2005;
    p18e_10 <- p18_2010;
    p18e_20 <- p18_2020;
})

## MOVE TO EXPORT...
##
## Rules to code altas and bajas in eq and its source (tablaEquivalenciasSeccionalesDesde1994.csv):
## - if actualización cartografía in jan-mar 2001 then baja/alta <- 2001
## - if actualización cartografía in ago-dic 2002 then baja/alta <- 2003
## - if actualización cartografía in abr-jul 2002 then check if no election that year for baja/alta <- 2003, else 2002
## Pre-alta and post-baja pops to zero to avoid double counting in aggregation
tmp <- nm # duplicate for manipulation
sel.r <- which(tmp$baja<=2005)
tmp[sel.r,] <- within(tmp[sel.r,], {
    p18e_05 <- 0;
    p18e_10 <- 0;
    p18e_20 <- 0;
})
sel.r <- which(tmp$baja>2005 & tmp$baja<=2010)
tmp[sel.r,] <- within(tmp[sel.r,], {
    p18e_10 <- 0;
    p18e_20 <- 0;
})
sel.r <- which(tmp$baja>2010 & tmp$baja<=2020)
tmp[sel.r,] <- within(tmp[sel.r,], {
    p18e_20 <- 0;
})
## altas
sel.r <- which(tmp$alta>2020)
tmp[sel.r,] <- within(tmp[sel.r,], {
    p18e_05 <- 0;
    p18e_10 <- 0;
    p18e_20 <- 0;
})
sel.r <- which(tmp$alta>2010 & tmp$alta<=2020)
tmp[sel.r,] <- within(tmp[sel.r,], {
    p18e_05 <- 0;
    p18e_10 <- 0;
})
sel.r <- which(tmp$alta>2005 & tmp$alta<=2010)
tmp[sel.r,] <- within(tmp[sel.r,], {
    p18e_05 <- 0;
})
##
table((tmp$p18e_05 - nm$p18e_05)==0) # FALSE are how many changed?
table(nm$seccion==tmp$seccion)
## fill manip
nm$p18e_05 <- tmp$p18e_05
nm$p18e_10 <- tmp$p18e_10
nm$p18e_20 <- tmp$p18e_20
rm(tmp)

sel.c <- c("p18e_05", "p18e_10", "p18e_20")                  ## select state pop columns
##nm <- my_agg(d=nm, sel.c=sel.c, by="inegi", drop.dupli=FALSE) ## and sum-up municipio secciones
nm <- my_agg(d=nm, sel.c=sel.c, by="edon", drop.dupli=FALSE) ## and sum-up state secciones


## ################################################################################################################
## ## Used to verify that summing manipulated se-level censos returns nums much teh same as summing them here... ##
## ## Bring mun pops from saved censos to convert sh.hats back into nm.hats                                      ##
## ################################################################################################################
## ## censoms have mu-level pops manipulated for remunicipalización
## tmp94 <- censom94
## tmp97 <- censom97
## tmp00 <- censom00
## tmp03 <- censom03
## tmp06 <- censom06
## tmp09 <- censom09
## tmp12 <- censom12
## tmp15 <- censom15
## tmp18 <- censom18
## tmp21 <- censom21
## ## check all sorted
## table(tmp94$inegi==tmp97$inegi)
## table(tmp94$inegi==tmp00$inegi)
## table(tmp94$inegi==tmp03$inegi)
## table(tmp94$inegi==tmp06$inegi)
## table(tmp94$inegi==tmp09$inegi)
## table(tmp94$inegi==tmp12$inegi)
## table(tmp94$inegi==tmp15$inegi)
## table(tmp94$inegi==tmp18$inegi)
## table(tmp94$inegi==tmp21$inegi)
## ## drop san quintín
## sel.r <- which(tmp03$inegi==2006) # san quintín
## tmp94 <- tmp94[-sel.r,] 
## tmp97 <- tmp97[-sel.r,] 
## tmp00 <- tmp00[-sel.r,] 
## tmp03 <- tmp03[-sel.r,] 
## tmp06 <- tmp06[-sel.r,]
## tmp09 <- tmp09[-sel.r,]
## tmp12 <- tmp12[-sel.r,]
## tmp15 <- tmp15[-sel.r,]
## tmp18 <- tmp18[-sel.r,]
## tmp21 <- tmp21[-sel.r,]
##
## tmp.cen <- tmp21
## tmp.cen[1,]
## tmp.cen$p18_2005 <- tmp06$p18_2005
## tmp.cen$p18_2010 <- tmp12$p18_2010
## tmp.cen$p18_2020 <- tmp21$p18_2020
## ## add cols
## tmp.sum <- nm[duplicated(nm$inegi)==FALSE, c("inegi","p18e_05","p18e_10","p18e_20")]
## tmp.cen <- tmp.cen[order(tmp.cen$inegi),]; tmp.sum <- tmp.sum[order(tmp.sum$inegi),]
## table(tmp.cen$inegi==tmp.sum$inegi)
## tmp.sum <- cbind(tmp.sum, tmp.cen[, c("p18_2005","p18_2010","p18_2020")])
## colnames(tmp.sum) <- c("inegi","sec.sum05","sec.sum10","sec.sum20","censo05","censo10","censo20")
## tmp.sum <- within(tmp.sum, {
##                   dif20 <- round((sec.sum20 - censo20) / sec.sum20, 2);
##                   dif10 <- round((sec.sum10 - censo10) / sec.sum10, 2);
##                   dif05 <- round((sec.sum05 - censo05) / sec.sum05, 2);
##                   })
## tmp.sum[12:16,]
## summary(tmp.sum$dif05)
## summary(tmp.sum$dif10)
## summary(tmp.sum$dif20)
## ## around 1% of all municipios have pop discrepancies greater than 5 percent
## which(abs(tmp.sum$dif05) > .05)
## which(abs(tmp.sum$dif10) > .05)
## which(abs(tmp.sum$dif20) > .05)


##################################################################################
## Paste election-year saved censuses adapted for compositional var projections ##
##################################################################################
tmp <- censom                                                                      ## duplicate for manipulation
colnames(tmp) <- gsub("(p18)(_)([0-9]{2})([0-9]{2})", "\\1e\\2\\4", colnames(tmp)) ## rename columns for totals
sel.c <- colnames(tmp)[grep("p18", colnames(tmp))]                                 ## column selector
tmp <- my_agg(d=tmp, sel.c=sel.c, by="edon", drop.dupli=TRUE)                      ## generate state aggregates 
tmp$ife <- tmp$inegi <- tmp$mun <- NULL                                            ## drop redundant info
head(tmp)
## Add slots for state pop aggregates in nm
nm.w <- within(nm, {
    p18e_21 <- NA;
    p18e_18 <- NA;
    p18e_15 <- NA;
    p18e_12 <- NA;
    p18e_09 <- NA;
    p18e_06 <- NA;
    p18e_03 <- NA;
    p18e_00 <- NA;
    p18e_97 <- NA;
    p18e_94 <- NA;
})
## fill 1st seccion in each state with state pops
nm.w[nm.w$dfirst==1, grep("p18e_(94|97|00|03|06|09|12|15|18|21)", colnames(nm.w))] <- tmp[,sel.c]
nm.w[1:2,]
tmp[1:2,]
sel.c <- grep("p18e_(94|97|00|03|06|09|12|15|18|21)", colnames(nm.w)) ## re-define column selector for nm.w
nm.w <- split(x=nm.w, f=nm.w$edon) # split into list of data frames, one per state
tmp.f <- function(x=NA){
    ##x <- nm.w[[2]] # debug
    repl <- do.call(rbind, replicate(n=nrow(x), x[1, sel.c], simplify = FALSE)) # replicate 1st seccion times all sec in state
    x[, sel.c] <- repl
    return(x)
}
nm.w <- lapply(nm.w, function(x) tmp.f(x))  # apply function
nm.w <- do.call(rbind, nm.w) # return to data frame form
##
## return manip to data
nm <- nm.w
rm(nm.w)

## DROP
## sel.r <- which(nm$seccion %in% c(20733, 20709, 20710, 20721, 20723, 20725, 21970, 21971, 21972, 21973, 22044:22047))
## nm[sel.r, grep("seccion|p18_20[05|10|20]", colnames(nm))]
## sel.c <- paste0("p18_", seq(1991,2021,3))
## tmp <- my_agg(d=tmp, sel.c=sel.c, by="edon", drop.dupli=TRUE) ## and sum-up state pops for secciones
## dim(tmp)
## colnames(tmp) <- sub("p18_", "p18e_", colnames(tmp)) ## rename mun pop vars
## ##
## ## ## This commented block would rely on counterfactual municipio map populations instead of censom 
## ## #############################################################################
## ## ## Project mun pops from saved censos to convert sh.hats back into nm.hats ##
## ## #############################################################################
## ## tmp <- censom21 # use latest municipio map
## ## tmp[1,]
## ## prj <- function(x=NA,yr=NA){
## ##     chg <- (x$p18_1995 - x$p18_1990) /5 # yearly pop change
## ##     pop <- x$p18_1990 + chg * (yr - 1990)
## ##     return(pop)
## ## }
## ## tmp <- within(tmp, {
## ##     p18_1994 <- prj(tmp, 1994)
## ## })
## ## prj <- function(x=NA,yr=NA){
## ##     chg <- (x$p18_2000 - x$p18_1995) /5 # yearly pop change
## ##     pop <- x$p18_1995 + chg * (yr - 1995)
## ##     return(pop)
## ## }
## ## tmp <- within(tmp, {
## ##     p18_1997 <- prj(tmp, 1997)
## ## })
## ## prj <- function(x=NA,yr=NA){
## ##     chg <- (x$p18_2005 - x$p18_2000) /5 # yearly pop change
## ##     pop <- x$p18_2000 + chg * (yr - 2000)
## ##     return(pop)
## ## }
## ## tmp <- within(tmp, {
## ##     p18_2003 <- prj(tmp, 2003)
## ## })
## ## prj <- function(x=NA,yr=NA){
## ##     chg <- (x$p18_2010 - x$p18_2005) /5 # yearly pop change
## ##     pop <- x$p18_2005 + chg * (yr - 2005)
## ##     return(pop)
## ## }
## ## tmp <- within(tmp, {
## ##     p18_2006 <- prj(tmp, 2006)
## ##     p18_2009 <- prj(tmp, 2009)
## ## })
## ## prj <- function(x=NA,yr=NA){
## ##     chg <- (x$p18_2020 - x$p18_2010) /10 # yearly pop change
## ##     pop <- x$p18_2010 + chg * (yr - 2010)
## ##     return(pop)
## ## }
## ## tmp <- within(tmp, {
## ##     p18_2012 <- prj(tmp, 2012)
## ##     p18_2015 <- prj(tmp, 2015)
## ##     p18_2018 <- prj(tmp, 2018)
## ##     p18_2021 <- prj(tmp, 2021)
## ## })
## ## tmp <- tmp[,order(colnames(tmp))]
## ## tmp$p18_1990 <- tmp$p18_1995 <- tmp$edon <- tmp$mun <- tmp$ife <- NULL
## ## #tmp$p18_2005 <- tmp$p18_2010 <- tmp$p18_2020 <- NULL
## ## #colnames(tmp) <- sub("^p18_[12][90]", "p18e_", colnames(tmp))
## ## colnames(tmp) <- sub("^p18_", "p18e_", colnames(tmp))
## ## dim(tmp)
## ## tmp[1,]
## ##
## ## pick municipios needed for all secciones and merge saved pops
## tmp2 <- data.frame(ord=1:nrow(nm), edon=nm$edon, seccion=nm$seccion)
## tmp2 <- split(x=tmp2, f=tmp2$edon) # split into list of data frames, one per state
## for (i in 1:32){
##     #i <- 1
##     tmp3 <- tmp2[[i]] # subset state i's secciones
##     ##
##     tmp4 <- data.frame(tmp[i, grep("p18e_", colnames(tmp))]) ## take state i's yealry pops
##     tmp4 <- tmp4[rep(1, nrow(tmp3)),]                        ## repeat as many times as there are secciones
##     ##
##     tmp3 <- cbind(tmp3, tmp4)  ## bind pops to state's secciones
##     ##
##     tmp3 -> tmp2[[i]] # return to data
## }
## tmp2 <- do.call(rbind, tmp2) # return to data frame form
## tmp2 <- tmp2[order(tmp2$ord),]; tmp2$ord <- NULL                  # sort in case order was not preserved
## tmp <- tmp2; rm(tmp2)
## tmp[1:3,]
## ## plug el.yr mun pops to nm
## tmp2 <- nm
## tmp2[1,]
## tmp[1,]
## table(tmp$seccion==tmp2$seccion) # verify same order and dimensionality
## tmp2 <- cbind(tmp2, tmp[,c(-1,-2)])
## tmp2[1,]
## nm[1,]
## table(nm$seccion==tmp2$seccion) # verify same order and dimensionality
## nm <- tmp2 ## fill data



####################################################
## Add columns for el.yr se-level pop projections ##
####################################################
nm <- within(nm, {
    p18_1994 <- p18_1997 <- p18_2000 <- p18_2003 <- p18_2006 <- p18_2009 <- p18_2012 <- p18_2015 <- p18_2018 <- p18_2021 <- NA;
})

################
## Compute sh ##
################
sh <- nm
sh$p18_2005 <- nm$p18_2005 / nm$p18e_05
sh$p18_2010 <- nm$p18_2010 / nm$p18e_10
sh$p18_2020 <- nm$p18_2020 / nm$p18e_20
##
summary(sh$p18_2005)
summary(sh$p18_2010)
summary(sh$p18_2020)
sh[which(sh$p18_2020>.17),]
##
## Any sh=0 first seccion?
sel.tmp <- which(sh$p18_2005==0 & sh$dfirst==1)
sel.tmp
sel.tmp <- which(sh$p18_2010==0 & sh$dfirst==1)
sel.tmp
sel.tmp <- which(sh$p18_2020==0 & sh$dfirst==1)
sel.tmp
##nm[sel.tmp, c("seccion","inegi","p18_2005","p18_2010","p18_2020","p18e_05","p18e_10","p18e_20")]
##
#####################################################
## Prep object to receive all possible regressions ##
#####################################################
regs <- vector(mode='list', length(nrow(nm))) ## empty list
regs[nm$dskip==1] <- "No regression, skipped due to lack of census data"
#regs[nm$ddone==1] <- "No regression, < 3 censuses data points, linear/flat estimates used"



######################################################
## Check no sección #1 in each municipio has pop>0, ##
## else change index to avoid dividing by zero      ##
######################################################
which(nm$dfirst==1 & nm$p18_2020==0)
## show(nm[,grep("ord|seccion|inegi|alta|baja|p18.*_(2005|2010|2020)|ddone|dfirst|dskip", colnames(nm))], 16435,  5, 0)
## show(nm[,grep("ord|seccion|inegi|alta|baja|p18.*_(2005|2010|2020)|ddone|dfirst|dskip", colnames(nm))], 25302,  5, 0)
## show(nm[,grep("ord|seccion|inegi|alta|baja|p18.*_(2005|2010|2020)|ddone|dfirst|dskip", colnames(nm))], 25926,  5, 0)
## show(nm[,grep("ord|seccion|inegi|alta|baja|p18.*_(2005|2010|2020)|ddone|dfirst|dskip", colnames(nm))], 32095,  5, 0)
## show(nm[,grep("ord|seccion|inegi|alta|baja|p18.*_(2005|2010|2020)|ddone|dfirst|dskip", colnames(nm))], 42419,  5, 0)
## show(nm[,grep("ord|seccion|inegi|alta|baja|p18.*_(2005|2010|2020)|ddone|dfirst|dskip", colnames(nm))], 43224,  5, 0)
## show(nm[,grep("ord|seccion|inegi|alta|baja|p18.*_(2005|2010|2020)|ddone|dfirst|dskip", colnames(nm))], 50927,  5, 0)
## show(nm[,grep("ord|seccion|inegi|alta|baja|p18.*_(2005|2010|2020)|ddone|dfirst|dskip", colnames(nm))], 57629,  5, 0)
##
## Are there zero pop 1st secciones? If so, flip indices with another seccion
nm <- nm[order(nm$ord),] # ord-sort
sel.c <- grep("seccion|^p18_", colnames(nm))
tmp <- nm[nm$dfirst==1, sel.c]
tmp$dzero <- rowSums(tmp[,-1]==0, na.rm = TRUE)
table(tmp$dzero)
nm[nm$dfirst==1,][which(tmp$dzero>0),]
##
if (sum(tmp$dzero) > 0) { # if >0, manipulate indices
    tmp[which(tmp$dzero>0),]
    ## switch indices
    nm <- nm[order(nm$ord),] # make sure ord-sorted
    tmp <- split(x=nm, f=nm$inegi) # split into list of data frames, one per municipio (2006 map)
    ## for cases with zero pop in 1st seccion, this swaps indices of secciones 1 and 2 
    wrap.f <- function(x=NA){
        ##x <- tmp[[1028]]   # debug
        ##x$p18_2005[1] <- 0 # debug
        x <- x
        if (nrow(x)>2){                           # proceed only if multirow>2 data frame (checked: all cases of interest >2)
            y  <- sum(x[1, sel.c]==0, na.rm=TRUE) # how many zeroes, excluding NAs, in sección 1's sel.c columns
            yy <- sum(x[2, sel.c]==0, na.rm=TRUE) # how many zeroes, excluding NAs, in sección 2's sel.c columns
            if (y>0){ # if sección 1 has zeroes
                if (yy>0){ # if sección 2 has zeroes, flip indices with sección 3
                    x$ord   [1:3] <- x$ord   [3:1]
                    x$dfirst[1:3] <- x$dfirst[3:1]
                } else {   # flip indices with sección 2
                    x$ord   [1:2] <- x$ord   [2:1]
                    x$dfirst[1:2] <- x$dfirst[2:1]
                }
            }
        }
        return(x)
    }
    tmp <- lapply(tmp, wrap.f) # apply function
    tmp <- do.call(rbind, tmp) # return to data frame form
    tmp <- tmp[order(tmp$ord),] # re-sort
    rownames(tmp) <- NULL
    tmp -> nm                   # replace nm with manipulation (some secciones)
    ##
    ## check again: zeroes?
    tmp <- nm[nm$dfirst==1, sel.c]
    tmp$dzero <- rowSums(tmp[,-1]==0, na.rm = TRUE)
    sum(tmp$dzero) # if >0, manipulate indices
}


###############
## Compute r ##
###############
## OJO: To compute r, all secciones in multisección municipios must be considered.
## Then, estimation on (dready2est==1 & dsingle==0 & dfirst==0) only.
## Translating r.hats into sh.hats again needs all secciones, in order to compute 1+sum(r) for denominator.
r <- split(x=sh, f=sh$edon) # split into list of data frames, one per state, to compute r=p18_i / p18_1
sel.c <- c("p18_2005", "p18_2010", "p18_2020")
tmp.f <- function(x=NA){
    ##x <- cenr[[1005]] # debug
    tmp.denom <- do.call(rbind, replicate(n=nrow(x), x[1, sel.c], simplify = FALSE)) # replicate 1st seccion times all sec in state
    x[, sel.c] <- x[, sel.c] / tmp.denom                                             # divide each row by 1st row
    return(x)
}
r <- lapply(r, tmp.f)  # apply function
r <- do.call(rbind, r) # return to data frame form
## Any NAs?
with(r[r$dready2est==1,], table(is.na(p18_2005)))
with(r[r$dready2est==1,], table(is.na(p18_2010)))
with(r[r$dready2est==1,], table(is.na(p18_2020)))
## sel.tmp <- with(r[r$dready2est==1,], which(is.na(p18_2010)))  # dready indices with 2010 NA
## sel.tmp <- which(r$seccion %in% sh[r$dready2est==1,][sel.tmp,]$seccion) # translated to r indices
## show(nm[,c("seccion","inegi","p18_2005","p18_2010","p18_2020","p18e_05","p18e_10","p18e_20","ddone")], sel.tmp[1], 0, 30)

## Inspect/manipulate and drop unseless indicators
with(nm, table(dskip, dready2est))
table(nm[nm$dskip==1, c("p18_2005","p18_2010","p18_2020")])
nm[nm$dskip==1, grep("p18_", colnames(nm))] <- 0 ## Zeroes across the board for these secciones
with(nm[nm$dskip==0,], table(dready2est, dfirst))




##############################################################
## Apply interlog to r[dready2est==1] to estimate and       ##
## predict 1994:2003 and 2021; apply interpol for 2006:2018 ##
##############################################################
## Subset data, excluding first secciones (flat r=1)
sel.c <- grep("seccion|inegi|^p18_(2005|2010|2020)$", colnames(nm))
sel.r <- which(r$dready2est==1 & r$dfirst==0 & r$dsingle==0)
##
##########################
r.w  <-  r[sel.r, sel.c] #
##########################
##
dim(r)
dim(r.w)
##
#### DEBUG: test with small dataset
##r.w <- r.w[1:1262,]
##
## Log-linear projection of 1994, retaining regressions to use for 1997-on
tmp.regs <- vector (mode='list', length(nrow(r.w))) ## empty list
tmp.regs <- interlog (what="p18", yr=1994, unit="s", frm="log(dv)~iv", census.data=r.w, digits=6)
## tmp.e will receive all log-linear predictions
tmp.e <- data.frame(seccion  = r.w$seccion,
                    p18_1994 = tmp.regs[[1]])
##non.nas <- apply(tmp.e, 1, sum); non.nas <- which(!is.na(non.nas)) # determine in which cases to skip prediction
new.d <- data.frame(iv=seq(1997,2021,3))     ## prep predictions 1997-on
preds <- vector(mode='list', nrow(tmp.e))    ## empty list
## predict
preds <- lapply(tmp.regs[[3]], function(x) data.frame(dv.hat=predict.lm(x, newdata = new.d)))
preds <- lapply(preds, function(x) x <- t(x))       # transpose to have yrs in cols
preds <- do.call(rbind, preds)                      # to data frame
colnames(preds) <- paste0("p18_", seq(1997,2021,3)) # add names
preds <- exp(preds)                                 # exponentiate log-linear predictions
tmp.e <- cbind(tmp.e, preds)                        # consolidate predictions
##tmp.e <- cbind(tmp.e, r.w[, paste0("p18_", c(2005,2010,2020))]) # add census yrs
##tmp.e <- tmp.e[, order(colnames(tmp.e))]            # sort columns except 1st (seccion)
rownames(tmp.e) <- NULL
head(tmp.e)
tmp.e$p18_1994 # all looks ok?
summary(tmp.e$p18_1994)
## linear
tmp.l <- data.frame(seccion  =r.w$seccion,
                    p18_1994=interpol(what="p18", yr=1994, unit="s", census.data = r.w, digits=4),
                    p18_1997=interpol(what="p18", yr=1997, unit="s", census.data = r.w, digits=4),
                    p18_2000=interpol(what="p18", yr=2000, unit="s", census.data = r.w, digits=4),
                    p18_2003=interpol(what="p18", yr=2003, unit="s", census.data = r.w, digits=4),
                    p18_2006=interpol(what="p18", yr=2006, unit="s", census.data = r.w, digits=4),
                    p18_2009=interpol(what="p18", yr=2009, unit="s", census.data = r.w, digits=4),
                    p18_2012=interpol(what="p18", yr=2012, unit="s", census.data = r.w, digits=4),
                    p18_2015=interpol(what="p18", yr=2015, unit="s", census.data = r.w, digits=4),
                    p18_2018=interpol(what="p18", yr=2018, unit="s", census.data = r.w, digits=4),
                    p18_2021=interpol(what="p18", yr=2021, unit="s", census.data = r.w, digits=4)
                    )
summary(tmp.l$p18_2006)
## subset again with all cols to fill-in tmp.e and tmp.l predictions
####################
r.w  <-  r[sel.r,] #
####################
r.w <- within(r.w, {
    p18_1994 <- tmp.e$p18_1994
    p18_1997 <- tmp.e$p18_1997
    p18_2000 <- tmp.e$p18_2000
    p18_2003 <- tmp.e$p18_2003
    p18_2006 <- tmp.l$p18_2006
    p18_2009 <- tmp.l$p18_2009
    p18_2012 <- tmp.l$p18_2012
    p18_2015 <- tmp.l$p18_2015
    p18_2018 <- tmp.l$p18_2018
    p18_2021 <- tmp.e$p18_2021
    ##dready2est <- 0
    ##ddone <- 1
})
r.w[1,]
##
## Return r.hats to data
####################
r.w  ->  r[sel.r,] #
####################
r[1:2,]
rm(r.w, sel.tmp, tmp, tmp.e, tmp.l, tmp.f, tmp.regs)

################################
## Add 1s to first seccion rs ##
################################
sel.c <- grep("^p18_", colnames(r))
r[r$dfirst==1, sel.c] <- 1


####################################################################
## convert r.hat to sh.hat. All obs needed to compute denominator ##
####################################################################
denom <- r[, grep("seccion|edon|inegi|^p18_", colnames(r))]
r[r$inegi==1004,grep("p18", colnames(r))]
denom[denom$inegi==1004,]
denom <- split(x=denom, f=denom$edon)
#denom <- denom[[4]] # debug
fun.tmp <- function(x){
    tmp  <- do.call(rbind, replicate(nrow(x), colSums(x[,-c(1,2,3)], na.rm = TRUE), simplify = FALSE))
    x[,-c(1,2,3)] <- tmp
    return(x)
}
#fun.tmp(denom)
tmp <- lapply(denom, function(x) fun.tmp(x))
denom <- do.call(rbind, tmp) 
rownames(denom) <- NULL
denom[denom$inegi==1004,]

## sort all (sh and denom not in same order, use seccion to avoid adding ord to denom)
table(r$seccion==   sh$seccion)
table(r$seccion==   nm$seccion)
table(r$seccion==denom$seccion)
r     <- r    [order(r    $seccion),]
sh    <- sh   [order(sh   $seccion),]
nm    <- nm   [order(nm   $seccion),]
denom <- denom[order(denom$seccion),]


## convert, only estimated secciones needed
## Subset data, excluding single secciones (ddone=1)
##sel.c <- grep("seccion|inegi|^p18_(2005|2010|2020)$", colnames(nm))
sel.r <- which(r$dready2est==1 & r$dsingle==0)
##
#############################
r.w      <-      r[sel.r, ] #
sh.w     <-     sh[sel.r, ] #
nm.w     <-     nm[sel.r, ] #
denom.w  <-  denom[sel.r, ] #
#############################
## compute sh.hat and empty in sh
tmp <-  r.w[, grep("^p18_(1994|1997|2000|2003|2006|2009|2012|2015|2018|2021)", colnames(r.w))] /
    denom.w[, grep("^p18_(1994|1997|2000|2003|2006|2009|2012|2015|2018|2021)", colnames(denom.w))]
tmp[r.w$inegi==1004,]
head(tmp)
tmp -> sh.w[, grep("^p18_(1994|1997|2000|2003|2006|2009|2012|2015|2018|2021)", colnames(sh.w))]
##
####################################
## compute nm.hat and empty in nm ##
####################################
tmp.nm <- sh.w[, grep( "^p18_(1994|1997|2000|2003|2006|2009|2012|2015|2018|2021)", colnames(sh.w))] *
          sh.w[, grep("^p18e_(94|97|00|03|06|09|12|15|18|21)", colnames(sh.w))]
tmp.nm <- round(tmp.nm, 1)
tmp.nm[r.w$inegi==1004,]
tmp.nm -> nm.w[, grep( "^p18_(1994|1997|2000|2003|2006|2009|2012|2015|2018|2021)", colnames(nm.w))]
##
## check
sh.w[sh.w$inegi==1004, grep("^p18_", colnames(sh.w))]
nm.w[sh.w$inegi==1004, grep("^p18_", colnames(nm.w))]
head(nm.w[nm.w$inegi==1001, grep("^p18_", colnames(nm.w))])
colSums(sh.w[,              grep("^p18_(1994|1997|2000|2003|2006|2009|2012|2015|2018|2021)", colnames(sh.w))])
##
summary(nm.w$p18_1994)
summary(nm.w$p18_1997)
summary(nm.w$p18_2000)
summary(nm.w$p18_2003)
summary(nm.w$p18_2006)
summary(nm.w$p18_2009)
summary(nm.w$p18_2012)
summary(nm.w$p18_2015)
summary(nm.w$p18_2018)
summary(nm.w$p18_2021)
## While these should be flat across the period, changes in ratio to section 1 and in state pops imprint some temporal variation. Looks ok
sel.tmp <- which(nm.w$p18_2005==nm.w$p18_2010 & nm.w$p18_2010==nm.w$p18_2020)
nm.w[sel.tmp[1:10], grep("^p18_", colnames(nm.w))]
sh.w[sel.tmp[1:10], grep("^p18_", colnames(sh.w))]
r.w [sel.tmp[1:10], grep("^p18_", colnames(r.w))]
##
##################
## To full data ##
##################
r.w      ->      r[sel.r, ]
sh.w     ->     sh[sel.r, ]
nm.w     ->     nm[sel.r, ]

rm(fun.tmp, sel.tmp, tmp, tmp3, tmp4, tmp.cen, tmp.ine, tmp.nm) # clean


#####################################################################
## Generate projection objects aggregated at municipal map levels. ##
## For comparidon with saved municipio projections.                ##
## Drops pre-alta and post-baja secciones from mu sums.            ##
#####################################################################
##
## 1994 municipal map
cen.w <- nm     # duplicate for manipulation
## altas/bajas to zero before/after 
table(cen.w$alta, useNA = "ifany")
sel.r <- which(is.na(cen.w$baja)==FALSE)
table(cen.w$baja[sel.r])
## wrap routine in function CHANGE THIS: USE drestore
pre.alta.post.baja.to.zero <- function(){
    sel.r <- which(      cen.w$alta > 1994)
    ##cen.w$p18_1991[sel.r] <- 0
    cen.w$p18_1994[sel.r] <- 0
    sel.r <- which(      cen.w$alta > 1997)
    cen.w$p18_1997[sel.r] <- 0
    sel.r <- which(      cen.w$alta > 2000)
    cen.w$p18_2000[sel.r] <- 0
    sel.r <- which(      cen.w$alta > 2003)
    cen.w$p18_2003[sel.r] <- 0
    sel.r <- which(      cen.w$alta > 2006)
    cen.w$p18_2005[sel.r] <- 0
    cen.w$p18_2006[sel.r] <- 0
    sel.r <- which(      cen.w$alta > 2009)
    cen.w$p18_2009[sel.r] <- 0
    sel.r <- which(      cen.w$alta > 2010)
    cen.w$p18_2010[sel.r] <- 0
    sel.r <- which(      cen.w$alta > 2012)
    cen.w$p18_2012[sel.r] <- 0
    sel.r <- which(      cen.w$alta > 2015)
    cen.w$p18_2015[sel.r] <- 0
    sel.r <- which(      cen.w$alta > 2018)
    cen.w$p18_2018[sel.r] <- 0
    sel.r <- which(      cen.w$alta > 2020)
    cen.w$p18_2020[sel.r] <- 0
    sel.r <- which(      cen.w$alta > 2021)
    cen.w$p18_2021[sel.r] <- 0
    ##
    sel.r <- which(      cen.w$baja <= 2021)
    cen.w$p18_2021[sel.r] <- 0
    cen.w$p18_2020[sel.r] <- 0
    sel.r <- which(      cen.w$baja <= 2018)
    cen.w$p18_2018[sel.r] <- 0
    sel.r <- which(      cen.w$baja <= 2015)
    cen.w$p18_2015[sel.r] <- 0
    sel.r <- which(      cen.w$baja <= 2012)
    cen.w$p18_2012[sel.r] <- 0
    cen.w$p18_2010[sel.r] <- 0
    sel.r <- which(      cen.w$baja <= 2009)
    cen.w$p18_2009[sel.r] <- 0
    sel.r <- which(      cen.w$baja <= 2006)
    cen.w$p18_2006[sel.r] <- 0
    cen.w$p18_2005[sel.r] <- 0
    sel.r <- which(      cen.w$baja <= 2003)
    cen.w$p18_2003[sel.r] <- 0
    sel.r <- which(      cen.w$baja <= 2000)
    cen.w$p18_2000[sel.r] <- 0
    sel.r <- which(      cen.w$baja <= 1997)
    cen.w$p18_1997[sel.r] <- 0
    return(cen.w)
}
##
cen.w <- pre.alta.post.baja.to.zero()
##
sel.c <- colnames(nm) [grep("^p18_", colnames(nm))]                         # subset censo cols
cen.w <- within(cen.w, {ife <- ife1994})                                    # this is the actual municipal code
cen.w <- my_agg(d=cen.w, sel.c=sel.c, by="ife", drop.dupli = TRUE)          # perform aggregation
rm(sel.c)
##sel.c <- grep(pattern="edon|ife$|p18_(1994|1997|2000|2003|2006|2009|2012|2015|2018|2021)", x=colnames(cen.w))
##cen.w <- cen.w[, sel.c]  # keep id and censo cols only
##
## clean
        sel.c <- grep("ife[0-9]{4}", colnames(cen.w))
cen.w[, sel.c] <- NULL  ## drop ife maps
rm(sel.c)
##
projm94 <- cen.w
rm(sel.r)
##
##
## 1997 municipal map
cen.w <- nm     # duplicate for manipulation
## altas/bajas to zero before/after 
table(cen.w$alta, useNA = "ifany")
sel.r <- which(is.na(cen.w$baja)==FALSE)
table(cen.w$baja[sel.r])
##
cen.w <- pre.alta.post.baja.to.zero()
##
sel.c <- colnames(nm) [grep("^p18_", colnames(nm))]                         # subset censo cols
cen.w <- within(cen.w, {ife <- ife1997})                                    # this is the actual municipal code
cen.w <- my_agg(d=cen.w, sel.c=sel.c, by="ife", drop.dupli = TRUE)          # perform aggregation
rm(sel.c)
##sel.c <- grep(pattern="edon|ife$|p18_(1994|1997|2000|2003|2006|2009|2012|2015|2018|2021)", x=colnames(cen.w))
##cen.w <- cen.w[, sel.c]  # keep id and censo cols only
##
## clean
        sel.c <- grep("ife[0-9]{4}", colnames(cen.w))
cen.w[, sel.c] <- NULL  ## drop ife maps
rm(sel.c)
##
projm97 <- cen.w
rm(sel.r)
##
##
## 2000 municipal map
cen.w <- nm     # duplicate for manipulation
## altas/bajas to zero before/after 
table(cen.w$alta, useNA = "ifany")
sel.r <- which(is.na(cen.w$baja)==FALSE)
table(cen.w$baja[sel.r])
##
cen.w <- pre.alta.post.baja.to.zero()
##
sel.c <- colnames(nm) [grep("^p18_", colnames(nm))]                         # subset censo cols
cen.w <- within(cen.w, {ife <- ife2000})                                    # this is the actual municipal code
cen.w <- my_agg(d=cen.w, sel.c=sel.c, by="ife", drop.dupli = TRUE)          # perform aggregation
rm(sel.c)
##sel.c <- grep(pattern="edon|ife$|p18_(1994|1997|2000|2003|2006|2009|2012|2015|2018|2021)", x=colnames(cen.w))
##cen.w <- cen.w[, sel.c]  # keep id and censo cols only
##
## clean
        sel.c <- grep("ife[0-9]{4}", colnames(cen.w))
cen.w[, sel.c] <- NULL  ## drop ife maps
rm(sel.c)
##
projm00 <- cen.w
rm(sel.r)
##
##
## 2003 municipal map
cen.w <- nm     # duplicate for manipulation
## altas/bajas to zero before/after 
table(cen.w$alta, useNA = "ifany")
sel.r <- which(is.na(cen.w$baja)==FALSE)
table(cen.w$baja[sel.r])
##
cen.w <- pre.alta.post.baja.to.zero()
##
sel.c <- colnames(nm) [grep("^p18_", colnames(nm))]                         # subset censo cols
cen.w <- within(cen.w, {ife <- ife2003})                                    # this is the actual municipal code
cen.w <- my_agg(d=cen.w, sel.c=sel.c, by="ife", drop.dupli = TRUE)          # perform aggregation
rm(sel.c)
##sel.c <- grep(pattern="edon|ife$|p18_(1994|1997|2000|2003|2006|2009|2012|2015|2018|2021)", x=colnames(cen.w))
##cen.w <- cen.w[, sel.c]  # keep id and censo cols only
##
## clean
        sel.c <- grep("ife[0-9]{4}", colnames(cen.w))
cen.w[, sel.c] <- NULL  ## drop ife maps
rm(sel.c)
##
projm03 <- cen.w
rm(sel.r)
##
##
## 2006 municipal map
cen.w <- nm     # duplicate for manipulation
## altas/bajas to zero before/after 
table(cen.w$alta, useNA = "ifany")
sel.r <- which(is.na(cen.w$baja)==FALSE)
table(cen.w$baja[sel.r])
##
cen.w <- pre.alta.post.baja.to.zero()
##
sel.c <- colnames(nm) [grep("^p18_", colnames(nm))]                         # subset censo cols
cen.w <- within(cen.w, {ife <- ife2006})                                    # this is the actual municipal code
cen.w <- my_agg(d=cen.w, sel.c=sel.c, by="ife", drop.dupli = TRUE)          # perform aggregation
rm(sel.c)
##sel.c <- grep(pattern="edon|ife$|p18_(1994|1997|2000|2003|2006|2009|2012|2015|2018|2021)", x=colnames(cen.w))
##cen.w <- cen.w[, sel.c]  # keep id and censo cols only
##
## clean
        sel.c <- grep("ife[0-9]{4}", colnames(cen.w))
cen.w[, sel.c] <- NULL  ## drop ife maps
rm(sel.c)
##
projm06 <- cen.w
rm(sel.r)
##
##
## 2009 municipal map
cen.w <- nm     # duplicate for manipulation
## altas/bajas to zero before/after 
table(cen.w$alta, useNA = "ifany")
sel.r <- which(is.na(cen.w$baja)==FALSE)
table(cen.w$baja[sel.r])
##
cen.w <- pre.alta.post.baja.to.zero()
##
sel.c <- colnames(nm) [grep("^p18_", colnames(nm))]                         # subset censo cols
cen.w <- within(cen.w, {ife <- ife2009})                                    # this is the actual municipal code
cen.w <- my_agg(d=cen.w, sel.c=sel.c, by="ife", drop.dupli = TRUE)          # perform aggregation
rm(sel.c)
##sel.c <- grep(pattern="edon|ife$|p18_(1994|1997|2000|2003|2006|2009|2012|2015|2018|2021)", x=colnames(cen.w))
##cen.w <- cen.w[, sel.c]  # keep id and censo cols only
##
## clean
        sel.c <- grep("ife[0-9]{4}", colnames(cen.w))
cen.w[, sel.c] <- NULL  ## drop ife maps
rm(sel.c)
##
projm09 <- cen.w
rm(sel.r)
##
##
## 2012 municipal map
cen.w <- nm     # duplicate for manipulation
## altas/bajas to zero before/after 
table(cen.w$alta, useNA = "ifany")
sel.r <- which(is.na(cen.w$baja)==FALSE)
table(cen.w$baja[sel.r])
##
cen.w <- pre.alta.post.baja.to.zero()
##
sel.c <- colnames(nm) [grep("^p18_", colnames(nm))]                         # subset censo cols
cen.w <- within(cen.w, {ife <- ife2012})                                    # this is the actual municipal code
cen.w <- my_agg(d=cen.w, sel.c=sel.c, by="ife", drop.dupli = TRUE)          # perform aggregation
rm(sel.c)
##sel.c <- grep(pattern="edon|ife$|p18_(1994|1997|2000|2003|2006|2009|2012|2015|2018|2021)", x=colnames(cen.w))
##cen.w <- cen.w[, sel.c]  # keep id and censo cols only
##
## clean
        sel.c <- grep("ife[0-9]{4}", colnames(cen.w))
cen.w[, sel.c] <- NULL  ## drop ife maps
rm(sel.c)
##
projm12 <- cen.w
rm(sel.r)
##
##
## 2015 municipal map
cen.w <- nm     # duplicate for manipulation
## altas/bajas to zero before/after 
table(cen.w$alta, useNA = "ifany")
sel.r <- which(is.na(cen.w$baja)==FALSE)
table(cen.w$baja[sel.r])
##
cen.w <- pre.alta.post.baja.to.zero()
##
sel.c <- colnames(nm) [grep("^p18_", colnames(nm))]                         # subset censo cols
cen.w <- within(cen.w, {ife <- ife2015})                                    # this is the actual municipal code
cen.w <- my_agg(d=cen.w, sel.c=sel.c, by="ife", drop.dupli = TRUE)          # perform aggregation
rm(sel.c)
##sel.c <- grep(pattern="edon|ife$|p18_(1994|1997|2000|2003|2006|2009|2012|2015|2018|2021)", x=colnames(cen.w))
##cen.w <- cen.w[, sel.c]  # keep id and censo cols only
##
## clean
        sel.c <- grep("ife[0-9]{4}", colnames(cen.w))
cen.w[, sel.c] <- NULL  ## drop ife maps
rm(sel.c)
##
projm15 <- cen.w
rm(sel.r)
##
##
## 2018 municipal map
cen.w <- nm     # duplicate for manipulation
## altas/bajas to zero before/after 
table(cen.w$alta, useNA = "ifany")
sel.r <- which(is.na(cen.w$baja)==FALSE)
table(cen.w$baja[sel.r])
##
cen.w <- pre.alta.post.baja.to.zero()
##
sel.c <- colnames(nm) [grep("^p18_", colnames(nm))]                         # subset censo cols
cen.w <- within(cen.w, {ife <- ife2018})                                    # this is the actual municipal code
cen.w <- my_agg(d=cen.w, sel.c=sel.c, by="ife", drop.dupli = TRUE)          # perform aggregation
rm(sel.c)
##sel.c <- grep(pattern="edon|ife$|p18_(1994|1997|2000|2003|2006|2009|2012|2015|2018|2021)", x=colnames(cen.w))
##cen.w <- cen.w[, sel.c]  # keep id and censo cols only
##
## clean
        sel.c <- grep("ife[0-9]{4}", colnames(cen.w))
cen.w[, sel.c] <- NULL  ## drop ife maps
rm(sel.c)
##
projm18 <- cen.w
rm(sel.r)
##
##
## 2021 municipal map
cen.w <- nm     # duplicate for manipulation
## altas/bajas to zero before/after 
table(cen.w$alta, useNA = "ifany")
sel.r <- which(is.na(cen.w$baja)==FALSE)
table(cen.w$baja[sel.r])
##
cen.w <- pre.alta.post.baja.to.zero()
##
sel.c <- colnames(nm) [grep("^p18_", colnames(nm))]                         # subset censo cols
cen.w <- within(cen.w, {ife <- ife2021})                                    # this is the actual municipal code
cen.w <- my_agg(d=cen.w, sel.c=sel.c, by="ife", drop.dupli = TRUE)          # perform aggregation
rm(sel.c)
##sel.c <- grep(pattern="edon|ife$|p18_(1994|1997|2000|2003|2006|2009|2012|2015|2018|2021)", x=colnames(cen.w))
##cen.w <- cen.w[, sel.c]  # keep id and censo cols only
##
## clean
        sel.c <- grep("ife[0-9]{4}", colnames(cen.w))
cen.w[, sel.c] <- NULL  ## drop ife maps
rm(sel.c)
##
projm21 <- cen.w
rm(sel.r)
##

## check projm.. here
projm94[15,]
projm97[15,]
projm00[15,]
projm03[15,]
projm06[15,]
projm09[15,]
projm12[15,]
projm15[15,]
projm18[15,]
projm21[15,]
projm15[15,c("p18_2005","p18_2010","p18_2020")]
projm18[15,c("p18_2005","p18_2010","p18_2020")]
projm21[15,c("p18_2005","p18_2010","p18_2020")]

table(projm94[15,]==projm97[15,])
table(projm94[15,]==projm00[15,])
table(projm94[15,]==projm03[15,])
table(projm94[15,]==projm06[15,])
table(projm94[15,]==projm09[15,])
table(projm94[15,]==projm12[15,])
table(projm94[15,]==projm15[15,])
table(projm94[15,]==projm18[15,])
table(projm94[15,]==projm21[15,])

# save all to restore after manipulating district/munic aggregates
save.image("../../datosBrutos/not-in-git/tmp-restore-post-interp.RData")

# load image
rm(list=ls())
options(width = 110)
dd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/")
setwd(dd)
load(file="../../datosBrutos/not-in-git/tmp-restore-post-interp.RData")

x

## now paste to v..m objects
v94m   <- merge(x = v94m  , y = projm94[, c("ife","p18_1994")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v94m)  [grep("p18_", colnames(v94m))]   <- "p18"
v97m94 <- merge(x = v97m94, y = projm94[, c("ife","p18_1997")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v97m94)[grep("p18_", colnames(v97m94))] <- "p18"
v00m94 <- merge(x = v00m94, y = projm94[, c("ife","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v00m94)[grep("p18_", colnames(v00m94))] <- "p18"
v03m94 <- merge(x = v03m94, y = projm94[, c("ife","p18_2003")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v03m94)[grep("p18_", colnames(v03m94))] <- "p18"
v06m94 <- merge(x = v06m94, y = projm94[, c("ife","p18_2006")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v06m94)[grep("p18_", colnames(v06m94))] <- "p18"
v09m94 <- merge(x = v09m94, y = projm94[, c("ife","p18_2009")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v09m94)[grep("p18_", colnames(v09m94))] <- "p18"
v12m94 <- merge(x = v12m94, y = projm94[, c("ife","p18_2012")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v12m94)[grep("p18_", colnames(v12m94))] <- "p18"
v15m94 <- merge(x = v15m94, y = projm94[, c("ife","p18_2015")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v15m94)[grep("p18_", colnames(v15m94))] <- "p18"
v18m94 <- merge(x = v18m94, y = projm94[, c("ife","p18_2018")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v18m94)[grep("p18_", colnames(v18m94))] <- "p18"
v21m94 <- merge(x = v21m94, y = projm94[, c("ife","p18_2021")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v21m94)[grep("p18_", colnames(v21m94))] <- "p18"
##
## 1997 municipal map
v94m97 <- merge(x = v94m97, y = projm97[, c("ife","p18_1994")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v94m97)[grep("p18_", colnames(v94m97))] <- "p18"
v97m   <- merge(x = v97m  , y = projm97[, c("ife","p18_1997")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v97m)  [grep("p18_", colnames(v97m))]   <- "p18"
v00m97 <- merge(x = v00m97, y = projm97[, c("ife","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v00m97)[grep("p18_", colnames(v00m97))] <- "p18"
v03m97 <- merge(x = v03m97, y = projm97[, c("ife","p18_2003")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v03m97)[grep("p18_", colnames(v03m97))] <- "p18"
v06m97 <- merge(x = v06m97, y = projm97[, c("ife","p18_2006")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v06m97)[grep("p18_", colnames(v06m97))] <- "p18"
v09m97 <- merge(x = v09m97, y = projm97[, c("ife","p18_2009")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v09m97)[grep("p18_", colnames(v09m97))] <- "p18"
v12m97 <- merge(x = v12m97, y = projm97[, c("ife","p18_2012")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v12m97)[grep("p18_", colnames(v12m97))] <- "p18"
v15m97 <- merge(x = v15m97, y = projm97[, c("ife","p18_2015")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v15m97)[grep("p18_", colnames(v15m97))] <- "p18"
v18m97 <- merge(x = v18m97, y = projm97[, c("ife","p18_2018")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v18m97)[grep("p18_", colnames(v18m97))] <- "p18"
v21m97 <- merge(x = v21m97, y = projm97[, c("ife","p18_2021")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v21m97)[grep("p18_", colnames(v21m97))] <- "p18"
##
## 2000 municipal map
v94m00 <- merge(x = v94m00, y = projm00[, c("ife","p18_1994")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v94m00)[grep("p18_", colnames(v94m00))] <- "p18"
v97m00 <- merge(x = v97m00, y = projm00[, c("ife","p18_1997")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v97m00)[grep("p18_", colnames(v97m00))] <- "p18"
v00m   <- merge(x = v00m  , y = projm00[, c("ife","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v00m)  [grep("p18_", colnames(v00m))]   <- "p18"
v03m00 <- merge(x = v03m00, y = projm00[, c("ife","p18_2003")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v03m00)[grep("p18_", colnames(v03m00))] <- "p18"
v06m00 <- merge(x = v06m00, y = projm00[, c("ife","p18_2006")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v06m00)[grep("p18_", colnames(v06m00))] <- "p18"
v09m00 <- merge(x = v09m00, y = projm00[, c("ife","p18_2009")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v09m00)[grep("p18_", colnames(v09m00))] <- "p18"
v12m00 <- merge(x = v12m00, y = projm00[, c("ife","p18_2012")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v12m00)[grep("p18_", colnames(v12m00))] <- "p18"
v15m00 <- merge(x = v15m00, y = projm00[, c("ife","p18_2015")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v15m00)[grep("p18_", colnames(v15m00))] <- "p18"
v18m00 <- merge(x = v18m00, y = projm00[, c("ife","p18_2018")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v18m00)[grep("p18_", colnames(v18m00))] <- "p18"
v21m00 <- merge(x = v21m00, y = projm00[, c("ife","p18_2021")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v21m00)[grep("p18_", colnames(v21m00))] <- "p18"
##
## 2003 municipal map
v94m03 <- merge(x = v94m03, y = projm03[, c("ife","p18_1994")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v94m03)[grep("p18_", colnames(v94m03))] <- "p18"
v97m03 <- merge(x = v97m03, y = projm03[, c("ife","p18_1997")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v97m03)[grep("p18_", colnames(v97m03))] <- "p18"
v00m03 <- merge(x = v00m03, y = projm03[, c("ife","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v00m03)[grep("p18_", colnames(v00m03))] <- "p18"
v03m   <- merge(x = v03m  , y = projm03[, c("ife","p18_2003")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v03m)  [grep("p18_", colnames(v03m))]   <- "p18"
v06m03 <- merge(x = v06m03, y = projm03[, c("ife","p18_2006")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v06m03)[grep("p18_", colnames(v06m03))] <- "p18"
v09m03 <- merge(x = v09m03, y = projm03[, c("ife","p18_2009")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v09m03)[grep("p18_", colnames(v09m03))] <- "p18"
v12m03 <- merge(x = v12m03, y = projm03[, c("ife","p18_2012")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v12m03)[grep("p18_", colnames(v12m03))] <- "p18"
v15m03 <- merge(x = v15m03, y = projm03[, c("ife","p18_2015")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v15m03)[grep("p18_", colnames(v15m03))] <- "p18"
v18m03 <- merge(x = v18m03, y = projm03[, c("ife","p18_2018")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v18m03)[grep("p18_", colnames(v18m03))] <- "p18"
v21m03 <- merge(x = v21m03, y = projm03[, c("ife","p18_2021")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v21m03)[grep("p18_", colnames(v21m03))] <- "p18"
##
## 2006 municipal map
v94m06 <- merge(x = v94m06, y = projm06[, c("ife","p18_1994")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v94m06)[grep("p18_", colnames(v94m06))] <- "p18"
v97m06 <- merge(x = v97m06, y = projm06[, c("ife","p18_1997")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v97m06)[grep("p18_", colnames(v97m06))] <- "p18"
v00m06 <- merge(x = v00m06, y = projm06[, c("ife","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v00m06)[grep("p18_", colnames(v00m06))] <- "p18"
v03m06 <- merge(x = v03m06, y = projm06[, c("ife","p18_2003")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v03m06)[grep("p18_", colnames(v03m06))] <- "p18"
v06m   <- merge(x = v06m  , y = projm06[, c("ife","p18_2006")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v06m)  [grep("p18_", colnames(v06m))]   <- "p18"
v09m06 <- merge(x = v09m06, y = projm06[, c("ife","p18_2009")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v09m06)[grep("p18_", colnames(v09m06))] <- "p18"
v12m06 <- merge(x = v12m06, y = projm06[, c("ife","p18_2012")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v12m06)[grep("p18_", colnames(v12m06))] <- "p18"
v15m06 <- merge(x = v15m06, y = projm06[, c("ife","p18_2015")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v15m06)[grep("p18_", colnames(v15m06))] <- "p18"
v18m06 <- merge(x = v18m06, y = projm06[, c("ife","p18_2018")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v18m06)[grep("p18_", colnames(v18m06))] <- "p18"
v21m06 <- merge(x = v21m06, y = projm06[, c("ife","p18_2021")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v21m06)[grep("p18_", colnames(v21m06))] <- "p18"
rm(cen.w)                                                         # rename
##
## 2009 municipal map
v94m09 <- merge(x = v94m09, y = projm09[, c("ife","p18_1994")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v94m09)[grep("p18_", colnames(v94m09))] <- "p18"
v97m09 <- merge(x = v97m09, y = projm09[, c("ife","p18_1997")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v97m09)[grep("p18_", colnames(v97m09))] <- "p18"
v00m09 <- merge(x = v00m09, y = projm09[, c("ife","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v00m09)[grep("p18_", colnames(v00m09))] <- "p18"
v03m09 <- merge(x = v03m09, y = projm09[, c("ife","p18_2003")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v03m09)[grep("p18_", colnames(v03m09))] <- "p18"
v06m09 <- merge(x = v06m09, y = projm09[, c("ife","p18_2006")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v06m09)[grep("p18_", colnames(v06m09))] <- "p18"
v09m   <- merge(x = v09m  , y = projm09[, c("ife","p18_2009")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v09m)  [grep("p18_", colnames(v09m))]   <- "p18"
v12m09 <- merge(x = v12m09, y = projm09[, c("ife","p18_2012")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v12m09)[grep("p18_", colnames(v12m09))] <- "p18"
v15m09 <- merge(x = v15m09, y = projm09[, c("ife","p18_2015")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v15m09)[grep("p18_", colnames(v15m09))] <- "p18"
v18m09 <- merge(x = v18m09, y = projm09[, c("ife","p18_2018")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v18m09)[grep("p18_", colnames(v18m09))] <- "p18"
v21m09 <- merge(x = v21m09, y = projm09[, c("ife","p18_2021")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v21m09)[grep("p18_", colnames(v21m09))] <- "p18"
##
## 2012 municipal map
v94m12 <- merge(x = v94m12, y = projm12[, c("ife","p18_1994")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v94m12)[grep("p18_", colnames(v94m12))] <- "p18"
v97m12 <- merge(x = v97m12, y = projm12[, c("ife","p18_1997")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v97m12)[grep("p18_", colnames(v97m12))] <- "p18"
v00m12 <- merge(x = v00m12, y = projm12[, c("ife","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v00m12)[grep("p18_", colnames(v00m12))] <- "p18"
v03m12 <- merge(x = v03m12, y = projm12[, c("ife","p18_2003")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v03m12)[grep("p18_", colnames(v03m12))] <- "p18"
v06m12 <- merge(x = v06m12, y = projm12[, c("ife","p18_2006")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v06m12)[grep("p18_", colnames(v06m12))] <- "p18"
v09m12 <- merge(x = v09m12, y = projm12[, c("ife","p18_2009")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v09m12)[grep("p18_", colnames(v09m12))] <- "p18"
v12m   <- merge(x = v12m  , y = projm12[, c("ife","p18_2012")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v12m)  [grep("p18_", colnames(v12m))]   <- "p18"
v15m12 <- merge(x = v15m12, y = projm12[, c("ife","p18_2015")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v15m12)[grep("p18_", colnames(v15m12))] <- "p18"
v18m12 <- merge(x = v18m12, y = projm12[, c("ife","p18_2018")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v18m12)[grep("p18_", colnames(v18m12))] <- "p18"
v21m12 <- merge(x = v21m12, y = projm12[, c("ife","p18_2021")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v21m12)[grep("p18_", colnames(v21m12))] <- "p18"
##
## 2015 municipal map
v94m15 <- merge(x = v94m15, y = projm15[, c("ife","p18_1994")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v94m15)[grep("p18_", colnames(v94m15))] <- "p18"
v97m15 <- merge(x = v97m15, y = projm15[, c("ife","p18_1997")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v97m15)[grep("p18_", colnames(v97m15))] <- "p18"
v00m15 <- merge(x = v00m15, y = projm15[, c("ife","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v00m15)[grep("p18_", colnames(v00m15))] <- "p18"
v03m15 <- merge(x = v03m15, y = projm15[, c("ife","p18_2003")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v03m15)[grep("p18_", colnames(v03m15))] <- "p18"
v06m15 <- merge(x = v06m15, y = projm15[, c("ife","p18_2006")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v06m15)[grep("p18_", colnames(v06m15))] <- "p18"
v09m15 <- merge(x = v09m15, y = projm15[, c("ife","p18_2009")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v09m15)[grep("p18_", colnames(v09m15))] <- "p18"
v12m15 <- merge(x = v12m15, y = projm15[, c("ife","p18_2012")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v12m15)[grep("p18_", colnames(v12m15))] <- "p18"
v15m   <- merge(x = v15m  , y = projm15[, c("ife","p18_2015")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v15m)  [grep("p18_", colnames(v15m))]   <- "p18"
v18m15 <- merge(x = v18m15, y = projm15[, c("ife","p18_2018")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v18m15)[grep("p18_", colnames(v18m15))] <- "p18"
v21m15 <- merge(x = v21m15, y = projm15[, c("ife","p18_2021")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v21m15)[grep("p18_", colnames(v21m15))] <- "p18"
##
## 2018 municipal map
v94m18 <- merge(x = v94m18, y = projm18[, c("ife","p18_1994")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v94m18)[grep("p18_", colnames(v94m18))] <- "p18"
v97m18 <- merge(x = v97m18, y = projm18[, c("ife","p18_1997")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v97m18)[grep("p18_", colnames(v97m18))] <- "p18"
v00m18 <- merge(x = v00m18, y = projm18[, c("ife","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v00m18)[grep("p18_", colnames(v00m18))] <- "p18"
v03m18 <- merge(x = v03m18, y = projm18[, c("ife","p18_2003")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v03m18)[grep("p18_", colnames(v03m18))] <- "p18"
v06m18 <- merge(x = v06m18, y = projm18[, c("ife","p18_2006")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v06m18)[grep("p18_", colnames(v06m18))] <- "p18"
v09m18 <- merge(x = v09m18, y = projm18[, c("ife","p18_2009")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v09m18)[grep("p18_", colnames(v09m18))] <- "p18"
v12m18 <- merge(x = v12m18, y = projm18[, c("ife","p18_2012")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v12m18)[grep("p18_", colnames(v12m18))] <- "p18"
v15m18 <- merge(x = v15m18, y = projm18[, c("ife","p18_2015")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v15m18)[grep("p18_", colnames(v15m18))] <- "p18"
v18m   <- merge(x = v18m,   y = projm18[, c("ife","p18_2018")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v18m)  [grep("p18_", colnames(v18m))]   <- "p18"
v21m18 <- merge(x = v21m18, y = projm18[, c("ife","p18_2021")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v21m18)[grep("p18_", colnames(v21m18))] <- "p18"
##
## 2021 municipal map
v94m21 <- merge(x = v94m21, y = projm21[, c("ife","p18_1994")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v94m21)[grep("p18_", colnames(v94m21))] <- "p18"
v97m21 <- merge(x = v97m21, y = projm21[, c("ife","p18_1997")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v97m21)[grep("p18_", colnames(v97m21))] <- "p18"
v00m21 <- merge(x = v00m21, y = projm21[, c("ife","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v00m21)[grep("p18_", colnames(v00m21))] <- "p18"
v03m21 <- merge(x = v03m21, y = projm21[, c("ife","p18_2003")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v03m21)[grep("p18_", colnames(v03m21))] <- "p18"
v06m21 <- merge(x = v06m21, y = projm21[, c("ife","p18_2006")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v06m21)[grep("p18_", colnames(v06m21))] <- "p18"
v09m21 <- merge(x = v09m21, y = projm21[, c("ife","p18_2009")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v09m21)[grep("p18_", colnames(v09m21))] <- "p18"
v12m21 <- merge(x = v12m21, y = projm21[, c("ife","p18_2012")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v12m21)[grep("p18_", colnames(v12m21))] <- "p18"
v15m21 <- merge(x = v15m21, y = projm21[, c("ife","p18_2015")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v15m21)[grep("p18_", colnames(v15m21))] <- "p18"
v18m21 <- merge(x = v18m21, y = projm21[, c("ife","p18_2018")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v18m21)[grep("p18_", colnames(v18m21))] <- "p18"
v21m   <- merge(x = v21m,   y = projm21[, c("ife","p18_2021")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v21m)  [grep("p18_", colnames(v21m))]   <- "p18"
##



# compare to mu-censuses
tmp.ine <- read.csv("/home/eric/Dropbox/data/elecs/MXelsCalendGovt/censos/data/pob18/p18mu-for-municipal-elecs.csv")
nextp <- function(){
    tmp <- c(
        v94m21$p18[i],
        v97m21$p18[i],
        v00m21$p18[i],
        v03m21$p18[i],
        v06m21$p18[i],
        v09m21$p18[i],
        v12m21$p18[i],
        v15m21$p18[i],
        v18m21$p18[i],
        v21m  $p18[i])
    ## tmp2 <- c(
    ##     v94m21$p18x[i],
    ##     v97m21$p18x[i],
    ##     v00m21$p18x[i],
    ##     v03m21$p18x[i],
    ##     v06m21$p18x[i],
    ##     v09m21$p18x[i],
    ##     v12m21$p18x[i],
    ##     v15m21$p18x[i],
    ##     v18m21$p18x[i],
    ##     v21m  $p18x[i])
    tmp3 <- c(
        tmp.ine$p18_1990[i],
        tmp.ine$p18_1995[i],
        tmp.ine$p18_2000[i],
        tmp.ine$p18_2005[i],
        tmp.ine$p18_2010[i],
        tmp.ine$p18_2020[i])
    plot(x = seq(1994, 2021, 3),
         y = tmp, ylim = c(0, max(c(tmp,tmp3))), xlim = c(1990, 2021), main = paste(i, v21m$mun[i]), pch = 20)
                                        #points(x = seq(1994, 2021, 3), y = tmp2, col = "red", pch = 20)
    points(x = c(1990, 1995, 2000, 2005, 2010, 2020), y = tmp3, col = "blue", pch = 19)
    abline(v = c(2005, 2010, 2020), lty = 2, col = "gray")
    legend(x = "bottomleft", legend = c("se-by-se compos", "mu censos"), pch = 20, col = c("black","blue"))
}

i <- 220
i <- i + 1; nextp()
x

## Possible explanation for near-systematic deficit of my mu-aggregates of sección-level p18 projections:
## 1. Missing secciones in municipal maps ife1994 ife1997 ...?
## 2. Sequence:
##    - se-level nm
##    - sh <- nm / agg(mu)           (excluding dfirst==1 & dskip==1 all zeroes in my data)  MAYBE I DIDNT SUBSET HERE?
##    -  r <- sh / sh1               (   "         "       "    "        "  "  )
##    - project r.hat for el yrs     (   "         "       "    "        "  "  )
##    - derive sh.hat for el yrs     (   "         "       "    "        "  "  )
##    - nm.hat <- sh.hat * agg(mu)   (   "         "       "    "        "  "  )


rm(denom.w, nm.w, sh.w, r.w, denom)
   prj, preds, sec.tmp, sel, sel.c, sel.ignore, sel.r, tmp.nm, wrap.f) ## clean


## 14. Apply interpol to r[sel.r,], predict 2005:2020
## 15. Translate r[sel.r,] -> sh[sel.r,] -> nm[sel.r,]
## 16. Restore 2020 values
## 17. Flat for 2021 (alt would use agg's slope, complicated)
## 18. Indicate ddone[sel.r]=1
## 19. Perhaps: erase round's action/orig.dest/when
## 20. Return nm[sel.r,] to nm (and sh and r)
## 21. Back to 6) for rounds 2, then 3
## 
## 21. sel.r <- seccion split in 2019 cases, round 1
## 22. Subset nm[sel.r,] sh[sel.r,] r[sel.r,] 
## 23. Save 2020 values to restore
## 24. Apply sum.split(d=nm[sel.r,], year.var=2020, rnd=1)
## 25. Apply my_sum(nm[sel.r,]$m:2005-2010-2020) 
## 26. Compute sh[sel.r,], compute r[sel.r,]
## 27. Apply interlog to r[sel.r,], predict 1994:2004 and and 2021
## 28. Apply interpol to r[sel.r,], predict 2005:2020
## 29. Translate r[sel.r,] -> sh[sel.r,] -> nm[sel.r,]
## 30. Restore 2020 values
## 31. Flat for 2019:2021 (alt would use agg's slope, complicated)
## 32. Indicate ddone[sel.r]=1
## 33. Perhaps: erase round's action/orig.dest/when
## 34. Return nm[sel.r,] to nm (and sh and r)
## 35. Back to 21) for rounds 2, then 3
## 
## 36. sel.r <- seccion split in 2018 cases, round 1
## 37. Subset nm[sel.r,] sh[sel.r,] r[sel.r,] 
## 38. Save 2020 values to restore
## 39. Apply sum.split(d=nm[sel.r,], year.var=2020, rnd=1)
## 40. Apply my_sum(nm[sel.r,]$m:2005-2010-2020) 
## 41. Compute sh[sel.r,], compute r[sel.r,]
## 42. Apply interlog to r[sel.r,], predict 1994:2004 and and 2021
## 43. Apply interpol to r[sel.r,], predict 2005:2020
## 44. Translate r[sel.r,] -> sh[sel.r,] -> nm[sel.r,]
## 45. Restore 2020 values
## 46. Flat for 2018:2021 (alt would use agg's slope, complicated)
## 47. Indicate ddone[sel.r]=1
## 48. Perhaps: erase round's action/orig.dest/when
## 49. Return nm[sel.r,] to nm (and sh and r)
## 50. Back to 36) for rounds 2, then 3
## 
## 51. Same for 2011:2019
## 
## 52. sel.r <- seccion split in 2010 cases, round 1
## 53. Subset nm[sel.r,] sh[sel.r,] r[sel.r,] 
## 54. Save 2010 and 2020 values to restore
## 55. Apply sum.split(d=nm[sel.r,], year.var=2010, rnd=1)
## 56. Apply sum.split(d=nm[sel.r,], year.var=2020, rnd=1)
## 57. Apply my_sum(nm[sel.r,]$m:2005-2010-2020) 
## 58. Compute sh[sel.r,], compute r[sel.r,]
## 59. Apply interlog to r[sel.r,], predict 1994:2004
## 60. Apply interpol to r[sel.r,], predict 2005:2009
## 61. Translate r[sel.r,] -> sh[sel.r,] -> nm[sel.r,]
## 62. Restore 2010 and 2020 values
## 63. Apply interpol to r[sel.r,], predict 2010:2021
## 64. Indicate ddone[sel.r]=1
## 65. Perhaps: erase round's action/orig.dest/when
## 66. Return nm[sel.r,] to nm (and sh and r)
## 67. Back to 52) for rounds 2, then 3
## 
## 68. sel.r <- seccion split in 2009 cases, round 1
## 69. Subset nm[sel.r,] sh[sel.r,] r[sel.r,] 
## 70. Save 2010 and 2020 values to restore
## 71. Apply sum.split(d=nm[sel.r,], year.var=2010, rnd=1)
## 72. Apply sum.split(d=nm[sel.r,], year.var=2020, rnd=1)
## 73. Apply my_sum(nm[sel.r,]$m:2005-2010-2020) 
## 74. Compute sh[sel.r,], compute r[sel.r,]
## 75. Apply interlog to r[sel.r,], predict 1994:2004
## 76. Apply interpol to r[sel.r,], predict 2005:2008
## 77. Translate r[sel.r,] -> sh[sel.r,] -> nm[sel.r,]
## 78. Restore 2010 and 2020 values
## 79. Apply interpol to r[sel.r,], predict 2009:2021
## 80. Indicate ddone[sel.r]=1
## 81. Perhaps: erase round's action/orig.dest/when
## 82. Return nm[sel.r,] to nm (and sh and r)
## 83. Back to 68) for rounds 2, then 3
## 
## 84. Same for 2006:2009
## 
## 85. sel.r <- seccion split in 2005 cases, round 1
## 86. Subset nm[sel.r,] sh[sel.r,] r[sel.r,] 
## 87. Save 2005 2010 and 2020 values to restore
## 88. Apply sum.split(d=nm[sel.r,], year.var=2005, rnd=1)
## 89. Apply sum.split(d=nm[sel.r,], year.var=2010, rnd=1)
## 90. Apply sum.split(d=nm[sel.r,], year.var=2020, rnd=1)
## 91. Apply my_sum(nm[sel.r,]$m:2005-2010-2020) 
## 92. Compute sh[sel.r,], compute r[sel.r,]
## 93. Apply interlog to r[sel.r,], predict 1994:2004
## 94. Translate r[sel.r,] -> sh[sel.r,] -> nm[sel.r,]
## 95. Restore 2005 2010 and 2020 values
## 96. Apply interlog to r[sel.r,], predict 2021
## 97. Apply interpol to r[sel.r,], predict 2005:2020
## 98. Indicate ddone[sel.r]=1
## 99. Perhaps: erase round's action/orig.dest/when
## 100. Return nm[sel.r,] to nm (and sh and r)
## 101. Back to 85) for rounds 2, then 3
## 
## 102. sel.r <- seccion split in 2004 cases, round 1
## 103. Subset nm[sel.r,] sh[sel.r,] r[sel.r,] 
## 104. Flat at 0 up to 2004
## 105. Apply my_sum(nm[sel.r,]$m:2005-2010-2020) 
## 106. Compute sh[sel.r,], compute r[sel.r,]
## 107. Apply interpol to r[sel.r,], predict 2005:2020
## 108. Apply interlog to r[sel.r,], predict 2021 
## 109. Translate r[sel.r,] -> sh[sel.r,] -> nm[sel.r,]
## 110. Indicate ddone[sel.r]=1
## 111. Perhaps: erase round's action/orig.dest/when
## 112. Return nm[sel.r,] to nm (and sh and r)
## 113. Back to 102) for rounds 2, then 3
## 
## 114. Same for 1994:2004

## TODO
## - drop ddone
