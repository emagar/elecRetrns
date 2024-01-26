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
## Last modified: 16jan2024                                                             ##
##########################################################################################


###################################################
## Function shows rows above/below numbered rows ##
###################################################
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
sum.split <- function( d=censo, year.var=2020, rnd=1, dfull=censo, year.to.split=NA, restore=FALSE, ready2=FALSE, skip=FALSE ) {
    ## Notes: (1) d can be a subset of dfull's rows (dfull needed to find target secciones).
    ##        (2) year.to.split!=NA applies year.var's relative weigths to split year.to.split's agg pop *instead of* summing year.var.
    ##        (3) restore/ready2/skip=TRUE manipulate target secciones's drestore/dready2est/dskip to 1 *instead of* splitting (2).
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
        if (is.na(year.to.split)==FALSE  & restore==FALSE & ready2==FALSE & skip==FALSE) {
            relpops <- round( sweep(x=relpops, MARGIN=2, STATS=as.matrix(d[i, sel.col.split]), FUN="*") ,1)
            ## return disaggregated matrix
            relpops -> dfull[sel.agg, sel.col.split]
        }
        if (is.na(year.to.split)==FALSE  & restore==TRUE & ready2==FALSE & skip==FALSE) {
            drestore$p18_2005[sel.agg] <- drestore$p18_2010[sel.agg] <- drestore$p18_2020[sel.agg] <- 1
        }
        if (is.na(year.to.split)==FALSE  & restore==FALSE & ready2==TRUE & skip==FALSE) {
            dfull$dready2est[sel.agg] <- 1
        }
        if (is.na(year.to.split)==FALSE  & restore==FALSE & ready2==FALSE & skip==TRUE) {
            dfull$dskip[sel.agg] <- 1
        }
    }
    ## return manipulated data
    if (is.na(year.to.split)==TRUE)                                                 return(d)
    if (is.na(year.to.split)==FALSE & restore==FALSE & ready2==FALSE & skip==FALSE) return(dfull)
    if (is.na(year.to.split)==FALSE & restore==TRUE  & ready2==FALSE & skip==FALSE) return(drestore)
    if (is.na(year.to.split)==FALSE & restore==FALSE & (ready2==TRUE | skip==TRUE)) return(dfull)
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
prj0520 <- function(x=NA,yr=NA){
    chg <- (x$p18_2020 - x$p18_2005) / 15 # yearly pop change
    pop <- x$p18_2005 + chg * (yr - 2005)
    return(pop)
}



## 4. Duplicate nm=censo, adding cols 1994:2021, m:2005-2010-2020, ddone=0, etc
##    `nm' stands for nominal quantities
nm       <- censo
nm.saved <- censo[, grep("seccion|^p18_(2005|2010|2020)", colnames(nm))] # backup, to restore
nm.saved[is.na(nm.saved)] <- 0 # NAs to zero in backup
#drestore <- nm.saved; drestore[,-1] <- 0 # indicate cases that need to be restore here
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
## #    nmanip <- ddone <- dneedsum <- 0;
## #    dneedsum05 <- dneedsum10 <- dneedsum20 <- dneedsum <- 0;
## #    when3b <- when3; orig.dest3b <- orig.dest3; action3b <- action3; #  backup action/orig.dest/when so that these
## #    when2b <- when2; orig.dest2b <- orig.dest2; action2b <- action2; #  can be erased once seccion gets manipulated
## #    whenb  <- when;  orig.destb  <- orig.dest;  actionb  <- action;  #  ---helps debug block immediately below
##     p18m_20 <- NA; # yy so grep excludes
##     p18m_10 <- NA; # yy so grep excludes
##     p18m_05 <- NA; # yy so grep excludes
##     p18e_20 <- NA; # yy so grep excludes
##     p18e_10 <- NA; # yy so grep excludes
##     p18e_05 <- NA; # yy so grep excludes
##     #p18_1994 <- p18_1997 <- p18_2000 <- p18_2003 <- p18_2006 <- p18_2009 <- p18_2012 <- p18_2015 <- p18_2018 <- p18_2021 <- NA;
})
nm <- nm[order(nm$inegi, nm$seccion),] # sort mun
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
nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
nm.w$dskip <- 1 # indicate skip
nm.w -> nm [sel.tmp,] # return to data
## 
## Cases with NAs that are, in fact, zeroes
sel.tmp <- which(nm$seccion %in% 230970) ## seccion created to adjust municipal limits in 2018 but had no voters, then merged 
nm[sel.tmp, c("p18_2005","p18_2010","p18_2020")] <- 0
nm$dskip[sel.tmp] <- 1
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
## Subset single-action=split.to with baja, using sum.split to reconstruct populations for counterfactual maps
sel.sing <- which( nm$      dready2est==0 &
                   nm$      dskip     ==0 &
                   nm$      action   =="split.to" &
                   nm$      action2  =="" &
                   nm$      action3  =="" &
                  is.na(nm$baja)     ==FALSE )
table( nm$when  [sel.sing], useNA = "always" )
nm[sel.sing, c("seccion","p18_2005","p18_2010","p18_2020")]
## 2002 cases
sel.sing <- which( nm$      dready2est==0 &
                   nm$      dskip     ==0 &
                   nm$      action   =="split.to" &
                   nm$      action2  =="" &
                   nm$      action3  =="" &
                   nm$      baja %in% 2002 &
                   nm$      when %in% 2002 )
nm.w       <- nm       [sel.sing,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
## sum.split
nm.w <- sum.split(d=nm.w, year.var=2020, rnd=1, dfull=nm) # add-up pre-split population
nm.w <- sum.split(d=nm.w, year.var=2010, rnd=1, dfull=nm) # add-up pre-split population
nm.w <- sum.split(d=nm.w, year.var=2005, rnd=1, dfull=nm) # add-up pre-split population
nm.w$dready2est <- 1 # indicate readiness
nm.w       -> nm       [sel.sing,] # return to data
## 2006 cases
sel.sing <- which( nm$      dready2est==0 &
                   nm$      dskip     ==0 &
                   nm$      action   =="split.to" &
                   nm$      action2  =="" &
                   nm$      action3  =="" &
                   nm$      baja==2006 &
                   nm$      when==2006 )
nm.w       <- nm       [sel.sing,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
## sum.split
nm.w <- sum.split(d=nm.w, year.var=2020, rnd=1, dfull=nm) # add-up pre-split population
nm.w <- sum.split(d=nm.w, year.var=2010, rnd=1, dfull=nm) # add-up pre-split population
nm.w <- sum.split(d=nm.w, year.var=2005, rnd=1, dfull=nm) # add-up pre-split population
nm.w$dready2est <- 1 # indicate readiness
nm.w       -> nm       [sel.sing,] # return to data
##
## 2009 cases and 2011:2012, which lack disagg 2010 pop
sel.sing <- which( nm$      dready2est==0 &
                   nm$      dskip     ==0 &
                   nm$      action   =="split.to" &
                   nm$      action2  =="" &
                   nm$      action3  =="" &
                   nm$      baja %in% 2009:2012 &
                   nm$      when %in% 2009:2012 )
nm.w       <- nm       [sel.sing,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
## sum.split
nm.w <- sum.split(d=nm.w, year.var=2020, rnd=1, dfull=nm) # add-up pre-split population
nm.w <- sum.split(d=nm.w, year.var=2010, rnd=1, dfull=nm) # add-up pre-split population
nm.w$dready2est <- 1 # indicate readiness
## fill pre-split counterfactuals for estimation (works in nm, so might change some pre-manipulated secciones, improving upon flat pop).
nm <-       sum.split(d=nm.w, year.var=2010, rnd=1, dfull=nm, year.to.split=2005) # split pre-split.to pop
#nm <-       sum.split(d=nm.w, year.var=2010, rnd=1, dfull=nm, year.to.split=2005, ready2=TRUE ) # indicate offspring readiness
tmp <- which(nm$seccion %in% 304746:304757)
nm[tmp, c("p18_2005","p18_2010","p18_2020")]
## return to data
nm.w       -> nm       [sel.sing,]
##
## 2013:2021 cases
sel.sing <- which( nm$      dready2est==0 &
                   nm$      dskip     ==0 &
                   nm$      action   =="split.to" &
                   nm$      action2  =="" &
                   nm$      action3  =="" &
                   nm$      baja %in% 2013:2021 &
                   nm$      when %in% 2013:2021 )
nm.w       <- nm       [sel.sing,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
## sum.split
nm.w <- sum.split(d=nm.w, year.var=2020, rnd=1, dfull=nm) # add-up pre-split population
nm.w$dready2est <- 1 # indicate readiness
## fill pre-split counterfactuals for estimation (works in nm, so might change some pre-manipulated secciones, improving upon flat pop).
nm <-       sum.split(d=nm.w, year.var=2020, rnd=1, dfull=nm, year.to.split=2005) # split pre-split.to pop
nm <-       sum.split(d=nm.w, year.var=2020, rnd=1, dfull=nm, year.to.split=2010) # split pre-split.to pop
#nm <-       sum.split(d=nm.w, year.var=2010, rnd=1, dfull=nm, year.to.split=2005, ready2=TRUE ) # indicate offspring readiness
tmp <- which(nm$seccion %in% 230874:230922)
head(nm[tmp, c("p18_2005","p18_2010","p18_2020")])
## return to data
nm.w       -> nm       [sel.sing,]
##
## 2022 cases: will need 2025 conteo or 2030 census to split pops
sel.sing <- which( nm$      dready2est==0 &
                   nm$      dskip     ==0 &
                   nm$      action   =="split.to" &
                   nm$      action2  =="" &
                   nm$      action3  =="" &
                   nm$      baja %in% 2022 &
                   nm$      when %in% 2022 )
nm.w       <- nm       [sel.sing,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
## sum.split
#nm.w <- sum.split(d=nm.w, year.var=2020, rnd=1, dfull=nm) # add-up pre-split population
nm.w$dready2est <- 1 # indicate readiness
## until 2025 conteo or 2030 census, can't figure out weights, skip
nm <-       sum.split(d=nm.w, year.var=2010, rnd=1, dfull=nm, year.to.split=2005, skip=TRUE )
## fill pre-split counterfactuals for estimation (works in nm, so might change some pre-manipulated secciones, improving upon flat pop).
#nm <-       sum.split(d=nm.w, year.var=2025, rnd=1, dfull=nm, year.to.split=2005) # split pre-split.to pop
#nm <-       sum.split(d=nm.w, year.var=2025, rnd=1, dfull=nm, year.to.split=2010) # split pre-split.to pop
#nm <-       sum.split(d=nm.w, year.var=2025, rnd=1, dfull=nm, year.to.split=2020) # split pre-split.to pop
#nm <-       sum.split(d=nm.w, year.var=2010, rnd=1, dfull=nm, year.to.split=2005, ready2=TRUE ) # indicate offspring readiness
tmp <- which(nm$seccion %in% 321920:321926)
nm[tmp, c("p18_2005","p18_2010","p18_2020")]
nm$dskip[tmp]
## return to data
nm.w       -> nm       [sel.sing,]
rm(sel.sing)
##
## Subset split.tos in action 3 with baja (special cases)
sel.tmp <- which(nm$seccion %in% c(161976, 192135, 192136))
nm.w       <- nm       [sel.tmp,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
nm.w[, c("seccion","alta","baja","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
nm.w[, c("seccion","alta","baja","action3","orig.dest3","when3","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
## sum.split
nm.w <- sum.split(d=nm.w, year.var=2020, rnd=3, dfull=nm) # add-up pre-split population
nm.w$dready2est <- 1 # indicate readiness
## fill pre-split counterfactuals for estimation (works in nm, so might change some pre-manipulated secciones, improving upon flat pop).
nm <-       sum.split(d=nm.w, year.var=2020, rnd=3, dfull=nm, year.to.split=2005) # split pre-split.to pop
nm <-       sum.split(d=nm.w, year.var=2020, rnd=3, dfull=nm, year.to.split=2010) # split pre-split.to pop
nm <-       sum.split(d=nm.w, year.var=2020, rnd=3, dfull=nm, year.to.split=2005, ready2=TRUE ) # indicate offspring readiness
## action 2 ignored, indicate readiness
nm$dready2est[nm$seccion==192437] <- 1
tmp <- which(nm$seccion %in% 162678:162699)
nm[tmp, c("p18_2005","p18_2010","p18_2020","dskip","dready2est")]
## return to data
nm.w       -> nm       [sel.tmp,]
rm(sel.tmp)
##
## Subset split.tos in action 3 with baja (special cases), proceed by steps
sel.trip <- which(nm$seccion==230875)
nm.w       <- nm       [sel.trip,]
nm.w[, c("seccion","alta","baja","action3","orig.dest3","when3","p18_2005","p18_2010","p18_2020","dskip","dready2est")] 
nm.w[, c("seccion","alta","baja","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dskip","dready2est")] 
nm.w[, c("seccion","alta","baja","action" ,"orig.dest" ,"when", "p18_2005","p18_2010","p18_2020","dskip","dready2est")] 
## step 1: add up 2020 and return to data
nm.w <- sum.split(d=nm.w, year.var=2020, rnd=3, dfull=nm) # add-up pre-split population
nm.w       -> nm       [sel.trip,] # return to data
## step 2: re-manipulate parent (offspring 875 had NAs) (works in nm)
sel.parent <- which(nm$seccion==230205)
nm.w <- nm[sel.parent,]
nm.w[, c("seccion","alta","baja","action" ,"orig.dest" ,"when", "p18_2005","p18_2010","p18_2020","dskip","dready2est")] 
nm <-       sum.split(d=nm.w, year.var=2020, rnd=1, dfull=nm, year.to.split=2005) # split pre-split.to pop
nm <-       sum.split(d=nm.w, year.var=2020, rnd=1, dfull=nm, year.to.split=2010) # split pre-split.to pop
nm <-       sum.split(d=nm.w, year.var=2020, rnd=1, dfull=nm, year.to.split=2005, ready2=TRUE ) # indicate offspring readiness
sel.tmp <- which(nm$seccion %in% 230874:230922)
head( nm[sel.tmp, c("seccion","alta","baja","action" ,"orig.dest" ,"when", "p18_2005","p18_2010","p18_2020","dskip","dready2est")] )
## step 3: back to original--ready
nm.w       <- nm       [sel.trip,]
nm.w[, c("seccion","alta","baja","action3","orig.dest3","when3","p18_2005","p18_2010","p18_2020","dskip","dready2est")] 
rm(list=ls()[grep("sel", ls())]) # clean all sels
##
## special case requiring manip before next block
sel.tmp <- which(nm$seccion==82828)
nm.w       <- nm       [sel.tmp,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
nm.w[, c("seccion","alta","baja","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
## sum.split
nm.w <- sum.split(d=nm.w, year.var=2010, rnd=2, dfull=nm) # add-up pre-split population
nm.w <- sum.split(d=nm.w, year.var=2020, rnd=2, dfull=nm) # add-up pre-split population
nm.w$dready2est <- 1 # indicate readiness
## fill pre-split counterfactuals for estimation (in nm).
nm <-       sum.split(d=nm.w, year.var=2010, rnd=2, dfull=nm, year.to.split=2005) # split pre-split.to pop
nm <-       sum.split(d=nm.w, year.var=2010, rnd=2, dfull=nm, year.to.split=2005, ready2=TRUE ) # indicate offspring readiness
tmp <- which(nm$seccion %in% 82937:83027)
nm[tmp, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
## return to data
nm.w       -> nm       [sel.tmp,]
rm(sel.tmp)
##
## Subset split.tos in action 2 with baja post 2010
sel.doub <- which( nm$      dready2est==0 &
                   nm$      dskip     ==0 &
                   nm$      action2  =="split.to" &
                   nm$      action3  =="" &
                   nm$      when2  %in% 2010:2021 &
                  is.na(nm$baja)     ==FALSE )
table( nm$when2  [sel.doub], useNA = "always" )
nm.w       <- nm       [sel.doub,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020")]
## sum.split
nm.w <- sum.split(d=nm.w, year.var=2020, rnd=2, dfull=nm) # add-up pre-split population
nm.w$dready2est <- 1 # indicate readiness
## fill pre-split counterfactuals for estimation (works in nm, so might change some pre-manipulated secciones, improving upon flat pop).
nm <-       sum.split(d=nm.w, year.var=2020, rnd=2, dfull=nm, year.to.split=2005) # split pre-split.to pop
nm <-       sum.split(d=nm.w, year.var=2020, rnd=2, dfull=nm, year.to.split=2010) # split pre-split.to pop
nm <-       sum.split(d=nm.w, year.var=2020, rnd=2, dfull=nm, year.to.split=2005, ready2=TRUE ) # indicate offspring readiness
tmp <- which(nm$seccion %in% 231022:231068)
nm[tmp, c("p18_2005","p18_2010","p18_2020","dskip","dready2est")]
## return to data
nm.w       -> nm       [sel.doub,]
rm(sel.doub)
##
## Subset split.tos in action 2 with baja post 2005
sel.doub <- which( nm$      dready2est==0 &
                   nm$      dskip     ==0 &
                   nm$      action2  =="split.to" &
                   nm$      action3  =="" &
                   nm$      when2  %in% 2005:2009 &
                  is.na(nm$baja)     ==FALSE )
table( nm$when2  [sel.doub], useNA = "always" )
nm.w       <- nm       [sel.doub,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020")]
## sum.split
nm.w <- sum.split(d=nm.w, year.var=2005, rnd=2, dfull=nm) # add-up pre-split population
nm.w <- sum.split(d=nm.w, year.var=2010, rnd=2, dfull=nm) # add-up pre-split population
nm.w <- sum.split(d=nm.w, year.var=2020, rnd=2, dfull=nm) # add-up pre-split population
nm.w$dready2est <- 1 # indicate readiness
## fill pre-split counterfactuals for estimation (works in nm, so might change some pre-manipulated secciones, improving upon flat pop).
nm <-       sum.split(d=nm.w, year.var=2020, rnd=2, dfull=nm, year.to.split=2005) # split pre-split.to pop
nm <-       sum.split(d=nm.w, year.var=2020, rnd=2, dfull=nm, year.to.split=2010) # split pre-split.to pop
nm <-       sum.split(d=nm.w, year.var=2020, rnd=2, dfull=nm, year.to.split=2005, ready2=TRUE ) # indicate offspring readiness
tmp <- which(nm$seccion %in% 192136:192165)
head(nm[tmp, c("seccion","p18_2005","p18_2010","p18_2020","dskip","dready2est")])
## fix action2 manually
nm[which(nm$seccion %in% c(192136,192437)),]
sh <- nm$p18_2010[nm$seccion==192437] / (nm$p18_2010[nm$seccion==192136] + nm$p18_2010[nm$seccion==192437])
nm$p18_2005[nm$seccion==192437] <- round(nm$p18_2005[nm$seccion==192136] * sh, 1)
## return to data
nm.w       -> nm       [sel.doub,]
rm(sel.doub)
##
## Now select action==split.froms that have no more subsequent actions: they have been manipulated above and are ready
sel.sf <- which(      nm$ dready2est==0 &
                      nm$ dskip     ==0 &
                      nm$ action    =="split.from" &
                      nm$ action2   =="" &
                      nm$ action3   =="" &
                is.na(nm$ p18_2005) ==FALSE & 
                is.na(nm$ p18_2010) ==FALSE & 
                is.na(nm$ p18_2020) ==FALSE & 
                      nm$ when      < 2022 )
nm.w       <- nm       [sel.sf,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020")]
nm.w$dready2est[nm.w$p18_2005!=0 | nm.w$p18_2010!=0 | nm.w$p18_2020!=0] <- 1 # indicate readiness
nm.w$dskip     [nm.w$p18_2005==0 & nm.w$p18_2010==0 & nm.w$p18_2020==0] <- 1 # indicate skip
## return to data
nm.w       -> nm       [sel.sf,]
rm(sel.sf)
##
## handle special cases arbitrarily before next block
sel.tmp <- which(nm$seccion==21369)
nm.w       <- nm       [sel.tmp,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
## almost flat
nm.w$p18_2010 <- 35
nm.w$p18_2020 <- 40
nm.w$dready2est <- 1
## return to data
nm.w       -> nm       [sel.tmp,]
##
sel.tmp <- which(nm$seccion==155931)
nm.w       <- nm       [sel.tmp,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
## almost flat
nm.w$p18_2020 <- 85
nm.w$dready2est <- 1
## return to data
nm.w       -> nm       [sel.tmp,]
rm(sel.tmp)
##
## Subset 1995:2006 single-action=split.to without baja, can't fix so ignore this action as if a population shock only
sel.sing <- which( nm$      dready2est==0 &
                   nm$      dskip     ==0 &
                   nm$      action    =="split.to" &
                   nm$      action2   =="" &
                   nm$      action3   =="" &
                   nm$      when    %in% 1995:2006 &
                   is.na(nm$ baja)     ==TRUE )
table( nm$when  [sel.sing], useNA = "always" )
nm.w       <- nm       [sel.sing,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
nm.w$dready2est <- 1 ## indicate readiness
## indicate offspring ready
nm <-       sum.split(d=nm.w, year.var=2010, rnd=1, dfull=nm, year.to.split=2020, ready2=TRUE ) # indicate offspring readiness
## check offspring
sel.tmp <- nm.w$orig.dest
sel.tmp <- lapply(sel.tmp, function(x) eval(str2expression(x)))
sel.tmp <- mapply(function(x,y) y*10000 + x, x=sel.tmp, y=lapply(nm.w$edon, function(x) x))
sel.tmp <- do.call(c, sel.tmp)
sel.tmp <- which(nm$seccion %in% sel.tmp)
nm[sel.tmp, c("seccion","alta","baja","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")] 
## return to data
nm.w       -> nm       [sel.sing,]
rm(sel.sing)
##
## Subset 2007:2008 single-action=split.to without baja, can't fix so ignore this action as if a population shock only...
sel.sing <- which( nm$      dready2est==0 &
                   nm$      dskip     ==0 &
                   nm$      action    =="split.to" &
                   nm$      action2   =="" &
                   nm$      action3   =="" &
                   nm$      when    %in% 2007:2008 &
                   is.na(nm$ baja)     ==TRUE )
## step 1: indicate readiness in parents and offspring
nm.w       <- nm       [sel.sing,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
nm.w$dready2est <- 1 ## indicate readiness
## indicate offspring readiness
nm <-       sum.split(d=nm.w, year.var=2010, rnd=1, dfull=nm, year.to.split=2005, ready2=TRUE )
## step 2: manipulate only offspring with NA in 2005
sel.nas <- which(nm$seccion %in% c(130742, 130900, 130928, 130936, 130938, 130953, 143084, 220550, 220554))
nm.w2      <- nm       [sel.nas,]
## check missingness (will manipulate parents too, since they appear in orig.dest, but by returning nm.w below restores unmanip parents) 
sel.tmp <- nm.w2$orig.dest
sel.tmp <- lapply(sel.tmp, function(x) eval(str2expression(x)))
sel.tmp <- mapply(function(x,y) y*10000 + x, x=sel.tmp, y=lapply(nm.w2$edon, function(x) x))
sel.tmp <- do.call(c, sel.tmp)
sel.tmp <- which(nm$seccion %in% sel.tmp)
nm[sel.tmp, c("seccion","alta","baja","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")] 
nm <-       sum.split(d=nm.w2, year.var=2010, rnd=1, dfull=nm, year.to.split=2005) # split pre-split.to pop
## step 3: return nm.w to data
nm.w       -> nm       [sel.sing,]
rm(nm.w, nm.w2, sel.sing, sel.nas)
## ##
## ## Split that can't be figured out, then one that can
## sel.tmp <- which(nm$seccion %in% c(190439))
## nm.w       <- nm       [sel.tmp,]
## ## only action2 can be manipulated, ignore previous
## nm.w[, c("seccion","alta","baja","action3","orig.dest3","when3","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
## nm.w[, c("seccion","alta","baja","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
## nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
## ## sum.split 
## nm.w <- sum.split(d=nm.w, year.var=2005, rnd=2, dfull=nm) # add-up pre-split population
## nm.w <- sum.split(d=nm.w, year.var=2010, rnd=2, dfull=nm) # add-up pre-split population
## nm.w <- sum.split(d=nm.w, year.var=2020, rnd=2, dfull=nm) # add-up pre-split population
## nm.w$dready2est <- 1 # indicate readiness
## nm.w       -> nm       [sel.tmp,] # return to data
##
## Subset Triple missing
sel.trip <- which( nm$      dready2est==0 &
                   nm$      dskip     ==0 &
                   is.na(nm$p18_2005) ==TRUE &
                   is.na(nm$p18_2010) ==TRUE &
                   is.na(nm$p18_2020) ==TRUE )
table( three=nm$action3 [sel.trip], useNA = "always" )
table( two=  nm$action2 [sel.trip], useNA = "always" )
table( one=  nm$action  [sel.trip], useNA = "always" )
rm(sel.trip)
##
## Subset Triple missing action==merged.to, can't be fixed
sel.trip <- which( nm$      dready2est==0 &
                   nm$      dskip     ==0 &
                   nm$      action    =="merged.to" &
                   is.na(nm$p18_2005) ==TRUE &
                   is.na(nm$p18_2010) ==TRUE &
                   is.na(nm$p18_2020) ==TRUE )
nm.w <- nm [sel.trip,]
## none have any census info
nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")] 
## indicate
nm.w$p18_2005 <- nm.w$p18_2010 <- nm.w$p18_2020 <- 0
nm.w$dskip <- 1
nm.w -> nm [sel.trip,] # return to data
##
## Subset split.froms then merged.to with no info --> skip
sel.trip <- which( nm$      dready2est==0 &
                   nm$      dskip     ==0 &
                   nm$      action    =="split.from" &
                   nm$      action2   =="merged.to" &
                   is.na(nm$p18_2005) ==TRUE &
                   is.na(nm$p18_2010) ==TRUE &
                   is.na(nm$p18_2020) ==TRUE )
table(one= nm$when  [sel.trip], two=nm$when2  [sel.trip], useNA = "always" )
nm.w <- nm [sel.trip,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020")] 
nm.w[, c("seccion","alta","baja","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020")] 
nm.w$p18_2005 <- nm.w$p18_2010 <- nm.w$p18_2020 <- 0
nm.w$dskip <- 1
nm.w -> nm [sel.trip,] # return to data
##
## Subset split.froms in 2022 --> skip
sel.trip <- which( nm$      dready2est==0 &
                   nm$      dskip     ==0 &
                   nm$      action    =="split.from" &
                   is.na(nm$p18_2005) ==TRUE &
                   is.na(nm$p18_2010) ==TRUE &
                   is.na(nm$p18_2020) ==TRUE )
table(one= nm$when  [sel.trip], two=nm$when2  [sel.trip], useNA = "always" )
nm.w <- nm [sel.trip,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020")] 
nm.w[, c("seccion","alta","baja","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020")] 
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
## Subset 10-20 Double missing action==split.to, salvageable with sum.split
sel.doub <- which( nm$      dready2est==0 &
                   nm$      dskip     ==0 &
                   nm$      action   =="split.to" &
                   is.na(nm$p18_2010) ==TRUE &
                   is.na(nm$p18_2020) ==TRUE )
nm.w       <- nm       [sel.doub,]
nm.w[, c("seccion","alta","baja","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
nm.w[, c("seccion","alta","baja","action" ,"orig.dest" ,"when" ,"p18_2005","p18_2010","p18_2020","dskip","dready2est")] # ignore this, pre-2005
## small pops: fill arbitrarily
nm.w$p18_2010 <- round(nm.w$p18_2005*1.05, 1)
nm.w$p18_2020 <- round(nm.w$p18_2010*1.10, 1)
nm.w$dready2est <- 1 ## indicate readiness
nm.w       -> nm       [sel.doub,] # return to data
##
## Subset action2 split.tos in 2022 --> ready (til new census)
sel.tmp <- which( nm$      dready2est==0 &
                  nm$      dskip     ==0 &
                  nm$      action    =="split.from" &
                  nm$      action2   =="split.to" &
                  nm$      when2     ==2022 )
nm.w       <- nm       [sel.tmp,]
nm.w[, c("seccion","alta","baja","action" ,"orig.dest" ,"when" ,"p18_2005","p18_2010","p18_2020","dskip","dready2est")] # ignore this, pre-2005
nm.w[, c("seccion","alta","baja","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
nm.w$dready2est <- 1        ## indicate readiness
nm.w       -> nm [sel.tmp,] ## return to data
##
## Subset action2 split.tos previously split.from
sel.tmp <- which( nm$      dready2est==0 &
                  nm$      dskip     ==0 &
                  nm$      action    =="split.from" &
                  nm$      action2   =="split.to" )
nm.w       <- nm       [sel.tmp,]
## all wo baja, look ok
nm.w[, c("seccion","alta","baja","action" ,"orig.dest","when","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020")]
nm.w$dready2est <- 1        ## indicate readiness
nm.w       -> nm [sel.tmp,] ## return to data
## check offspring, pick unready for manip
sel.tmp <- which(nm$seccion %in% c(192711:192716, 192719, 192717, 192727:192731))
nm[sel.tmp, c("seccion","alta","baja","action" ,"orig.dest" ,"when" ,"p18_2005","p18_2010","p18_2020","dskip","dready2est") ]
#sel.tmp <- which(nm$seccion %in% c(192197, 192220, 192221, 190852, 192333, 192614))
sel.tmp <- which(nm$seccion %in% c(192197, 192333, 192614))
nm.w       <- nm       [sel.tmp,]
nm.w[, c("seccion","alta","baja","action" ,"orig.dest","when","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020")]
## fill pre-split counterfactuals for estimation (works in nm, so might change some pre-manipulated secciones, improving upon flat pop).
nm <-       sum.split(d=nm.w, year.var=2020, rnd=2, dfull=nm, year.to.split=2005) # split pre-split.to pop
nm <-       sum.split(d=nm.w, year.var=2020, rnd=2, dfull=nm, year.to.split=2010) # split pre-split.to pop
nm <-       sum.split(d=nm.w, year.var=2020, rnd=2, dfull=nm, year.to.split=2005, ready2=TRUE ) # indicate offspring readiness
## this case skip
nm$dready2est[which(nm$seccion==192716)] <- 0
##
## indicate this seccion as ready
nm$dready2est[nm$seccion==122710] <- 1
##
## Subset remaining action2 split.tos
sel.tmp <- which( nm$      dready2est==0 &
                  nm$      dskip     ==0 &
                  nm$      action2   =="split.to" )
nm.w       <- nm       [sel.tmp,]
## wo baja, looks ok
nm.w[, c("seccion","alta","baja","action" ,"orig.dest","when","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
## inspect 2017's child
nm[nm$seccion==192732, c("seccion","alta","baja","action" ,"orig.dest","when","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
## fill pre-split counterfactuals for estimation (works in nm, so might change some pre-manipulated secciones, improving upon flat pop).
nm <-       sum.split(d=nm.w, year.var=2020, rnd=1, dfull=nm, year.to.split=2005) # split pre-split.to pop
nm <-       sum.split(d=nm.w, year.var=2020, rnd=1, dfull=nm, year.to.split=2010) # split pre-split.to pop
nm <-       sum.split(d=nm.w, year.var=2020, rnd=1, dfull=nm, year.to.split=2005, ready2=TRUE ) # indicate offspring readiness
## indicate readiness and return to data
nm.w$dready2est <- 1 ## indicate readiness
nm.w       -> nm       [sel.tmp,] # return to data
##
## Special cases need manip before next block
nm$p18_2020[which(nm$seccion==80058)] <- round( 265*128/(128+258) , 1)
nm$p18_2005[which(nm$seccion==95547)] <- nm$p18_2010[which(nm$seccion==95547)] <- 0
nm$dskip   [which(nm$seccion==95547)] <- 1
nm$dready2est[which(nm$seccion==93951)] <- 1
nm$p18_2020  [which(nm$seccion==122051)] <- 10
nm$dready2est[which(nm$seccion==122051)] <- 1
##                  
## Subset action==split.to, salvageable w sum.split
sel.tmp <- which( nm$      dready2est==0 &
                  nm$      dskip     ==0 &
                  nm$      action   =="split.to")
table(nm$when[sel.tmp])
## 2022 cases: indicate ready (til next censo)
sel.tmp <- which( nm$      dready2est==0 &
                  nm$      dskip     ==0 &
                  nm$      action    =="split.to" &
                  nm$      when      ==2022 )
nm.w       <- nm       [sel.tmp,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
nm.w$dready2est <- 1
nm.w       -> nm       [sel.tmp,] # return to data
##
## 2013:21 cases
sel.tmp <- which( nm$      dready2est==0 &
                  nm$      dskip     ==0 &
                  nm$      action    =="split.to" &
                  nm$      when      %in% 2013:2021 )
nm.w       <- nm       [sel.tmp,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
## indicate readiness
nm.w$dready2est <- 1
## fill pre-split counterfactuals for estimation (works in nm, so might change some pre-manipulated secciones, improving upon flat pop).
nm <-       sum.split(d=nm.w, year.var=2020, rnd=1, dfull=nm, year.to.split=2005) # split pre-split.to pop
nm <-       sum.split(d=nm.w, year.var=2020, rnd=1, dfull=nm, year.to.split=2010) # split pre-split.to pop
nm <-       sum.split(d=nm.w, year.var=2020, rnd=1, dfull=nm, year.to.split=2005, ready2=TRUE ) # indicate offspring readiness
tmp <- which(nm$seccion %in% 95547)
nm[tmp, c("seccion","alta","baja","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
nm[tmp, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
## return to data
nm.w       -> nm       [sel.tmp,]
## these have all zeroes and had dskip=1, drop readiness
nm[which(nm$dskip==1 & nm$dready2est==1), c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
nm$dready2est[which(nm$dskip==1 & nm$dready2est==1)] <- 0
## 2009:2011 cases
sel.tmp <- which( nm$      dready2est==0 &
                  nm$      dskip     ==0 &
                  nm$      action    =="split.to" &
                  nm$      when      %in% 2009:2011 )
nm.w       <- nm       [sel.tmp,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
## indicate readiness
nm.w$dready2est <- 1
## fill pre-split counterfactuals for estimation (works in nm, so might change some pre-manipulated secciones, improving upon flat pop).
nm <-       sum.split(d=nm.w, year.var=2020, rnd=1, dfull=nm, year.to.split=2005) # split pre-split.to pop
nm <-       sum.split(d=nm.w, year.var=2020, rnd=1, dfull=nm, year.to.split=2005, ready2=TRUE ) # indicate offspring readiness
tmp <- which(nm$seccion %in% c(143357))
nm[tmp, c("seccion","alta","baja","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
nm[tmp, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
## return to data
nm.w       -> nm       [sel.tmp,]
##
## manipulate before next block
nm$p18_2020      [nm$seccion==122714] <- round(
    nm$p18_2020  [nm$seccion==122700] *
    (nm$p18_2010 [nm$seccion==122714] /
     (nm$p18_2010[nm$seccion==122714] +
      nm$p18_2010[nm$seccion==122700]))
  , 1)
nm$dready2est    [nm$seccion==122714] <- 1
##
## remaining split.to odd, negligible, ignore w manip to one offspring
nm$p18_2020[nm$seccion==10459] <- round(nm$p18_2010[nm$seccion==10459]*1.09, 1)
nm$p18_2010[nm$seccion==10487] <- round(nm$p18_2005[nm$seccion==10487]*1.05, 1)
nm$p18_2020[nm$seccion==10487] <- round(nm$p18_2010[nm$seccion==10487]*1.09, 1)
sel.tmp <- which( nm$      dready2est==0 &
                  nm$      dskip     ==0 &
                  nm$action          =="split.to")
nm[           nm$seccion %in% c(10366,10408,10459,10378,10487),
     c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
nm$dready2est[nm$seccion %in% c(10366,10408,10459,10378,10487)] <- 1 ## indicate readiness
##
## early split.ons merged later
sel.tmp <- which( nm$      dready2est==0 &
                  nm$      dskip     ==0 &
                  nm$      action    =="split.from" &
                  nm$      when     %in% c(1996,2004) )
nm.w       <- nm       [sel.tmp,]
nm.w[, c("seccion","alta","baja","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
## do by hand
sh <- nm$p18_2005[nm$seccion==122779] / (nm$p18_2005[nm$seccion==122776] + nm$p18_2005[nm$seccion==122779])
nm$p18_2010      [nm$seccion==122779] <- round(nm$p18_2010[nm$seccion==122776] * sh, 1)
nm$p18_2020      [nm$seccion==122779] <- round(nm$p18_2020[nm$seccion==122776] * sh, 1)
##
sm <- (nm$p18_2005[nm$seccion==122714] + nm$p18_2005[nm$seccion==122780] + nm$p18_2005[nm$seccion==122781] + nm$p18_2005[nm$seccion==122782])
sh <- nm$p18_2005[nm$seccion==122780] / sm
nm$p18_2010      [nm$seccion==122780] <- round(nm$p18_2010[nm$seccion==122714] * sh, 1)
nm$p18_2020      [nm$seccion==122780] <- round(nm$p18_2020[nm$seccion==122714] * sh, 1)
##
sh <- nm$p18_2005[nm$seccion==122781] / sm
nm$p18_2010      [nm$seccion==122781] <- round(nm$p18_2010[nm$seccion==122714] * sh, 1)
nm$p18_2020      [nm$seccion==122781] <- round(nm$p18_2020[nm$seccion==122714] * sh, 1)
##
sh <- nm$p18_2005[nm$seccion==122782] / sm
nm$p18_2010      [nm$seccion==122782] <- round(nm$p18_2010[nm$seccion==122714] * sh, 1)
nm$p18_2020      [nm$seccion==122782] <- round(nm$p18_2020[nm$seccion==122714] * sh, 1)
##
nm$dready2est[nm$seccion %in% 122779:122782] <- 1 ## indicate readiness
## 
## action3 split.to whose offspring remain unamipulated
sel.tmp <- which( nm$      dready2est==0 &
                  nm$      dskip     ==0 &
                  nm$      action    =="split.from")
nm.w       <- nm       [sel.tmp,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
sel.tmp <- which( nm$seccion==230875) ## parent
nm.w       <- nm       [sel.tmp,]
nm.w[, c("seccion","alta","baja","action3","orig.dest3","when3","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
## fill pre-split counterfactuals for estimation (works in nm, so might change some pre-manipulated secciones, improving upon flat pop).
nm <-       sum.split(d=nm.w, year.var=2020, rnd=3, dfull=nm, year.to.split=2005) # split pre-split.to pop
nm <-       sum.split(d=nm.w, year.var=2020, rnd=3, dfull=nm, year.to.split=2010) # split pre-split.to pop
nm <-       sum.split(d=nm.w, year.var=2020, rnd=3, dfull=nm, year.to.split=2005, ready2=TRUE ) # indicate offspring readiness
tmp <- which(nm$seccion %in% 230975:231003)
nm[tmp, c("p18_2005","p18_2010","p18_2020","dready2est")]
##
## manip these manually
sel.tmp <- which( nm$      dready2est==0 &
                  nm$      dskip     ==0 &
                  nm$      action    =="split.from")
nm.w       <- nm       [sel.tmp,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
##
sh <- nm$p18_2020[nm$seccion==230974] / (nm$p18_2020[nm$seccion==230875] + nm$p18_2020[nm$seccion==230974])
nm$p18_2010      [nm$seccion==230974] <- round(nm$p18_2010[nm$seccion==230875] * sh, 1)
nm$p18_2005      [nm$seccion==230974] <- round(nm$p18_2005[nm$seccion==230875] * sh, 1)
nm$dready2est    [nm$seccion==230974] <- 1
##
sh <- nm$p18_2020[nm$seccion==311133] / (nm$p18_2020[nm$seccion==311002] + nm$p18_2020[nm$seccion==311003] + nm$p18_2020[nm$seccion==311004] + nm$p18_2020[nm$seccion==311133])
nm$p18_2010      [nm$seccion==311133] <- round(nm$p18_2010[nm$seccion==310514] * sh, 1)
nm$p18_2005      [nm$seccion==311133] <- round(nm$p18_2005[nm$seccion==310514] * sh, 1)
nm$dready2est    [nm$seccion==311133] <- 1
##
sh <- nm$p18_2020[nm$seccion==180982] / (nm$p18_2020[nm$seccion==180804] + nm$p18_2020[nm$seccion==180967] + nm$p18_2020[nm$seccion==180982])
nm$p18_2010      [nm$seccion==180982] <- round((nm$p18_2010[nm$seccion==180804] + nm$p18_2010[nm$seccion==180967]) * sh, 1)
nm$p18_2005      [nm$seccion==180982] <- round((nm$p18_2005[nm$seccion==180804] + nm$p18_2005[nm$seccion==180967]) * sh, 1)
nm$dready2est    [nm$seccion==180982] <- 1
##
sh <- nm$p18_2020[nm$seccion==180983] / (nm$p18_2020[nm$seccion==180967] + nm$p18_2020[nm$seccion==180970] + nm$p18_2020[nm$seccion==180971] + nm$p18_2020[nm$seccion==180983])
nm$p18_2010      [nm$seccion==180983] <- round((nm$p18_2010[nm$seccion==180967] + nm$p18_2010[nm$seccion==180970] + nm$p18_2010[nm$seccion==180970]) * sh, 1)
nm$p18_2005      [nm$seccion==180983] <- round((nm$p18_2005[nm$seccion==180967] + nm$p18_2005[nm$seccion==180970] + nm$p18_2005[nm$seccion==180970]) * sh, 1)
nm$dready2est    [nm$seccion==180983] <- 1
##
sh <- nm$p18_2020[nm$seccion==180984] / (nm$p18_2020[nm$seccion==180753] + nm$p18_2020[nm$seccion==180754] + nm$p18_2020[nm$seccion==180766] + nm$p18_2020[nm$seccion==180984])
nm$p18_2010      [nm$seccion==180984] <- round((nm$p18_2010[nm$seccion==180753] + nm$p18_2010[nm$seccion==180754] + nm$p18_2010[nm$seccion==180766]) * sh, 1)
nm$p18_2005      [nm$seccion==180984] <- round((nm$p18_2005[nm$seccion==180753] + nm$p18_2005[nm$seccion==180754] + nm$p18_2005[nm$seccion==180766]) * sh, 1)
nm$dready2est    [nm$seccion==180984] <- 1
##
## unmerge function
unmerge.to <- function( d=censo, year.var=2020, rnd=1, dfull=censo, year.to.unmerge=NA ) {
    ## Notes: (1) d can be a subset of dfull's rows (dfull needed to find target secciones).
    ##        (2) year.to.unmerge!=NA applies year.var's relative weigths to split year.to.unmerge's agg pop *instead of* summing year.var.
    ##        (3) restore/ready2/skip=TRUE manipulate target secciones's drestore/dready2est/dskip to 1 *instead of* splitting (2).
    ##
    if (year.var %notin% c(2005,2010,2020)){
        print("Warning: year.var is not a sección-level census year")
        stop
    }
    ##year.var      <- 2010                      # debug
    ##year.to.unmerge <- 2020                      # debug
    ##d <-     nm.w  ##nm[which(nm$action=="split.to"),] # debug
    ##dfull <- nm                                # debug
    ##rnd <- 1                                # debug
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
    sel.col.unmerge <- sel.col[grep(year.to.unmerge, sel.col)] # crop sel.col to split only year.to.unmerge
    sel.col <-       sel.col[grep(year.var,      sel.col)]   # crop sel.col to sum only year.var
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
        totals <- totals + d[i,sel.col]
        ## relative populations, for use in breaking up past aggregates
        relpops <-        dfull[sel.agg, sel.col] ## duplicate
        relpops <- sweep(x=relpops, MARGIN=2, STATS=totals, FUN="/") # sweep "/" divides whole columns by corresponding totals
        ## paste into sección under manipulation
        result <- round( dfull[sel.agg, sel.col] * (1-relpops), 1 )
        result -> d[i,sel.col.unmerge]
    }
    ## return manipulated data
    return(d)
}
##
## remainder are merged.to cases
sel.tmp <- which( nm$      dready2est==0 &
                  nm$      dskip     ==0 )
nm.w  <- nm [sel.tmp,]
table(nm.w$action, nm.w$when)
##
## merged.to cases: 2013:2021
sel.tmp <- which( nm$      dready2est==0 &
                  nm$      dskip     ==0 &
                  nm$      when %in% 2013:2021 )
nm.w  <- nm [sel.tmp,]
head(
    nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
)
## indicate skips
tmp <- which(nm.w$p18_2010==0 & nm.w$p18_2005==0)
nm.w$p18_2020[tmp] <- 0; nm.w$dskip[tmp] <- 1
tmp <- which(nm.w$seccion==95097 | nm.w$seccion==95095)
nm.w$p18_2005[tmp] <- 0; nm.w$p18_2020[tmp] <- 0; nm.w$dskip[tmp] <- 1
## apply function
nm.w <-  unmerge.to(d=nm.w, year.var=2010, rnd=1, dfull=nm, year.to.unmerge=2020) # split pre-split.to pop
nm.w$dready2est[nm.w$dskip==0] <- 1 ## indicate readiness
nm.w  -> nm [sel.tmp,] ## return to data
##
## remained are merged.to cases: 2009:2012
sel.tmp <- which( nm$      dready2est==0 &
                  nm$      dskip     ==0 &
                  nm$      when %in% 2009:2012 )
nm.w  <- nm [sel.tmp,]
table(nm.w$when)
head(
    nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
)
## indicate skips
tmp <- which(nm.w$p18_2005==0)
nm.w$p18_2020[tmp] <- 0; nm.w$p18_2010[tmp] <- 0; nm.w$dskip[tmp] <- 1
## apply function
nm.w <-  unmerge.to(d=nm.w, year.var=2005, rnd=1, dfull=nm, year.to.unmerge=2020) # split pre-split.to pop
nm.w <-  unmerge.to(d=nm.w, year.var=2005, rnd=1, dfull=nm, year.to.unmerge=2010) # split pre-split.to pop
nm.w$dready2est[nm.w$dskip==0] <- 1 ## indicate readiness
nm.w  -> nm [sel.tmp,] ## return to data
##
## cases missing 2005, make linear
sel.tmp <- which(nm$seccion %in% c(100632,100633,101013,101014))
nm.w <- nm[sel.tmp,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","action2","orig.dest2","when2","p18_2005","p18_2010","p18_2020","dready2est")]
nm.w <- within(nm.w, {
    p18_2005 <- prj1020(nm.w, 2005)
    dready2est <- 1
})
nm.w -> nm [sel.tmp,] # return to data
rm(sel.tmp)
##
## Progress
table(ready=nm$dready2est, skip=nm$dskip)
## manips remaining
sel.tmp <- which( nm$      dready2est==0 &
                  nm$      dskip     ==0 )
##table( nm$action3 [sel.tmp], useNA = "always" )
##table( nm$action2 [sel.tmp], useNA = "always" )
##table( nm$action  [sel.tmp], useNA = "always" )
##
## These secciones have odd zeroes in one census: arbitrarily add given other years' pops
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
rm(list=ls()[grep("sel|tmp",ls())]) # clean
##
## Make 2010 census linear with 2005 and 2020 because too low in these cases
sel.tmp <- which(nm$seccion %in% c(160829, 303230, 50718, 71879, 200221, 230616, 71640, 71800, 260645, 281040, 281061, 281063, 51437, 71739, 71773, 71832, 71834, 281041, 51049, 71638, 71742, 71744, 71771, 71841, 71850, 71856, 71916, 91734, 94990, 101214, 130585, 153849, 154024, 154393, 156019, 201813, 210025, 250362, 250416, 260306, 260307, 260643, 281042, 281062, 281068, 290490, 300090, 300697, 300715, 302274, 302456, 302625, 302839, 303187, 20554, 21386, 30352, 50442, 50717, 51105, 51110, 51271, 51276, 51305, 51309, 51316, 51396, 71443, 71602, 71829, 71831, 71843, 71848, 71878, 82253, 82842, 91279, 112059, 120444, 121658, 122149, 122777, 130022, 130218, 130339, 130395, 130800, 131083, 131446, 131559, 142273, 142729, 143164, 150080, 150101, 150129, 150147, 150485, 150901, 152000, 152349, 152394, 152770, 152787, 154025, 154195, 154416, 154544, 155752, 155757, 155926, 156014, 156143, 160077, 160149, 160827, 160866, 170353, 170377, 170596, 180295, 180418, 180741, 190193, 190194, 191192, 191236, 200842, 201172, 211564, 211618, 211647, 211648, 211913, 211915, 212222, 220554, 241447, 241650, 250632, 250995, 251293, 251603, 251767, 251844, 251845, 251852, 252644, 252960, 253007, 253041, 253074, 260081, 260082, 260160, 260305, 260760, 261110, 261325, 270580, 270660, 270665, 280785, 281038, 281057, 281064, 300409, 301223, 301524, 301777, 302445, 302477, 302626, 302816, 302863, 303232, 303233, 303297, 303451, 310992, 321035))
nm[sel.tmp, c("seccion","alta","baja","p18_2005","p18_2010","p18_2020")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2010 <- prj0520(nm[sel.tmp,], 2010)
})
##
## manual fixes
sh <- nm$p18_2005[nm$seccion==122778] / (nm$p18_2005[nm$seccion==122778] + nm$p18_2005[nm$seccion==122701])
nm$p18_2010[nm$seccion==122778] <- round(sh * nm$p18_2010[nm$seccion==122701], 1)
nm$p18_2020[nm$seccion==122778] <- round(sh * nm$p18_2020[nm$seccion==122701], 1)
##
sh <- nm$p18_2020[nm$seccion==21744] / (nm$p18_2020[nm$seccion==21744] + nm$p18_2020[nm$seccion==21294])
nm$p18_2005[nm$seccion==21744] <- round(sh * nm$p18_2005[nm$seccion==21294], 1)
nm$p18_2010[nm$seccion==21744] <- round(sh * nm$p18_2010[nm$seccion==21294], 1)
##
sh <- nm$p18_2020[nm$seccion==51575] / (nm$p18_2020[nm$seccion==51575] + nm$p18_2020[nm$seccion==51411])
nm$p18_2005[nm$seccion==51575] <- round(sh * nm$p18_2005[nm$seccion==51411], 1)
nm$p18_2010[nm$seccion==51575] <- round(sh * nm$p18_2010[nm$seccion==51411], 1)
##
sh <- nm$p18_2020[nm$seccion==71943] / (nm$p18_2020[nm$seccion==71943] + nm$p18_2020[nm$seccion==71950] + nm$p18_2020[nm$seccion==71963] + nm$p18_2020[nm$seccion==71606])
nm$p18_2005[nm$seccion==71943] <- round(sh * nm$p18_2005[nm$seccion==71606], 1)
nm$p18_2010[nm$seccion==71943] <- round(sh * nm$p18_2010[nm$seccion==71606], 1)
##
sh <- nm$p18_2020[nm$seccion==71950] / (nm$p18_2020[nm$seccion==71943] + nm$p18_2020[nm$seccion==71950] + nm$p18_2020[nm$seccion==71963] + nm$p18_2020[nm$seccion==71606])
nm$p18_2005[nm$seccion==71950] <- round(sh * nm$p18_2005[nm$seccion==71606], 1)
nm$p18_2010[nm$seccion==71950] <- round(sh * nm$p18_2010[nm$seccion==71606], 1)
##
sh <- nm$p18_2020[nm$seccion==71963] / (nm$p18_2020[nm$seccion==71943] + nm$p18_2020[nm$seccion==71963] + nm$p18_2020[nm$seccion==71963] + nm$p18_2020[nm$seccion==71606])
nm$p18_2005[nm$seccion==71963] <- round(sh * nm$p18_2005[nm$seccion==71606], 1)
nm$p18_2010[nm$seccion==71963] <- round(sh * nm$p18_2010[nm$seccion==71606], 1)
## arbitrary
nm$p18_2005[nm$seccion==200457] <- 550
nm$p18_2020[nm$seccion==200457] <- 800
##
## arbitrary
nm$p18_2020[nm$seccion==200802] <- 100
##
## arbitrary
nm$p18_2020[nm$seccion==201305] <- 700
##
sh <- nm$p18_2005[nm$seccion==251330] / (nm$p18_2005[nm$seccion==251330] + nm$p18_2005[nm$seccion==251331])
nm$p18_2010[nm$seccion==251330] <- round(sh * nm$p18_2010[nm$seccion==251331], 1)
nm$p18_2020[nm$seccion==251330] <- round(sh * nm$p18_2020[nm$seccion==251331], 1)
##
sh <- nm$p18_2005[nm$seccion==200457] / (nm$p18_2005[nm$seccion==200457] + nm$p18_2005[nm$seccion==251331])
nm$p18_2010[nm$seccion==200457] <- round(sh * nm$p18_2010[nm$seccion==251331], 1)
nm$p18_2020[nm$seccion==200457] <- round(sh * nm$p18_2020[nm$seccion==251331], 1)
rm(sh)
##
## Did manipulation leave cases that have zeroes across the board?
sel.tmp <- which(
    nm$ dready2est==1 &
    nm$ p18_2005==0 &
    nm$ p18_2010==0 &
    nm$ p18_2020==0)
nm.w <- nm [sel.tmp,]
nm.w[, c("seccion","alta","baja","action","orig.dest","when","p18_2005","p18_2010","p18_2020","dskip","dready2est")]
nm.w$dskip <- 1 # indicate skip
nm.w$dready2est <- 0 # unindicate readiness
nm.w -> nm [sel.tmp,] # return to data
##
## skip with some info?
sel.tmp <- which(
    nm$ dskip   ==1 &
   (nm$ p18_2005>0 | nm$ p18_2010>0 | nm$ p18_2020>0))
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
rm(nm.w,sel.tmp)
#####################
## Block ends here ##
#####################


## intento de usar función de interpolación-compositional en objeto nm
nm <- nm[order(nm$ord),]
manip <- nm[, c("ord","inegi","p18_2005","p18_2010","p18_2020")]
## get census projection functions
source("../../code/v-hat-etc/interpolate-census-functions.r")

tmp <- interlog(what="p18", yr=1997, unit="s", census.data=nm, frm="log(dv)~iv", digits=1)

tmp.est <- data.frame(
    "y1995"=NA,
    "y1996"=NA,
    "y1997"=tmp$interp,
    "y1998"=NA,
    "y1999"=NA,
    "y2000"=NA,
    "y2001"=NA,
    "y2002"=NA,
    "y2003"=NA,
    "y2004"=NA,
    "y2005"=NA,
    "y2006"=NA,
    "y2007"=NA,
    "y2008"=NA,
    "y2009"=NA,
    "y2010"=NA,
    "y2011"=NA,
    "y2012"=NA,
    "y2013"=NA,
    "y2014"=NA,
    "y2015"=NA,
    "y2016"=NA,
    "y2017"=NA,
    "y2018"=NA,
    "y2019"=NA,
    "y2020"=NA,
    "y2021"=NA,
    "y2022"=NA,
    "y2023"=NA)
##
for (yr in c(1994:1996,1998:2023)){
    ##yr <- 1994
    print(yr)
    tmp2 <- predict.interlog(data=tmp, yr=yr)
    tmp.est[, grep(yr, colnames(tmp.est))] <- tmp2
}

setwd(sd)
write.csv(tmp.est, file="censo/p18se-proj-w-interlog.csv", )

## explore
i <- sel.r[1]
i <- i+1; plot(x=c(1995,2023), y=c(min(tmp.est[i,], nm[i, grep("p18", colnames(nm))]), max(tmp.est[i,], nm[i, grep("p18", colnames(nm))])), type="n", main=i); lines(x=1995:2023, y=tmp.est[i,]); points(x=2000+c(5,10,20), y=nm[i, grep("p18", colnames(nm))]); 

# save for debug
save.image("too-big-4-github/tmp-debug.RData")

# load image
rm(list=ls())
options(width = 110)
dd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/v-hats-etc")
setwd(dd)
load(file="too-big-4-github/tmp-debug.RData")

tmp.est2 <- tmp.est ## duplicate before manip

## Turn NAs back to zero to avoid double-counting
done <- rep(0, nrow(tmp.est)) ## indicate progress here
done[nm$dskip==1] <- 1
table(done)
##
## no reseccionamiento
sel.r <- which(nm$dskip==0 & is.na(nm$when) & done==0)
done[sel.r] <- 1

## cases with single reseccionamiento
sel.r <- which(nm.saved$p18_2020==0 & nm$dskip==0 & is.na(nm$when2))
table(nm$baja[sel.r])
table(nm$alta[sel.r])
## alta in 2006
sel.r <- which(nm.saved$p18_2020==0 & nm$dskip==0 & is.na(nm$when2) & nm$alta==2006)
tmp.est[sel.r, grep("199|200[0-5]", colnames(tmp.est))] <- 0 ## pre alta to zero
##
## baja in 2021
sel.r <- which(nm.saved$p18_2020==0 & nm$dskip==0 & is.na(nm$when2) & nm$baja==2021)
tmp.est[sel.r, grep("202[1-3]", colnames(tmp.est))] <- 0 ## post baja to zero
sel2 <- which(nm.saved$p18_2010[sel.r]==0)               ## which had zero pop 2010
nm.saved[sel.r[sel2],] ## give when2 not NA, no opther reseccionamiento, so leave untouched
sel2 <- which(nm.saved$p18_2005[sel.r]==0)               ## which had zero pop 2005
nm.saved[sel.r[sel2],] ## give when2 not NA, no opther reseccionamiento, so leave untouched
##
## mark as done
done[sel.r] <- 1
##
## baja in 2020
sel.r <- which(nm.saved$p18_2020==0 & nm$dskip==0 & is.na(nm$when2) & nm$baja==2020)
tmp.est[sel.r, grep("202[0-3]", colnames(tmp.est))] <- 0 ## post baja to zero
sel2 <- which(nm.saved$p18_2010[sel.r]==0)               ## which had zero pop 2010
nm.saved[sel.r[sel2],] ## give when2 not NA, no opther reseccionamiento, so leave untouched
sel2 <- which(nm.saved$p18_2005[sel.r]==0)               ## which had zero pop 2005
nm.saved[sel.r[sel2],] ## give when2 not NA, no opther reseccionamiento, so leave untouched
## mark as done
done[sel.r] <- 1
##
## baja in 2015
sel.r <- which(nm.saved$p18_2020==0 & nm$dskip==0 & is.na(nm$when2) & nm$baja==2015)
tmp.est[sel.r, grep("201[5-9]|202[0-3]", colnames(tmp.est))] <- 0 ## post baja to zero
sel2 <- which(nm.saved$p18_2010[sel.r]==0)               ## which had zero pop 2010
nm.saved[sel.r[sel2],] ## give when2 not NA, no opther reseccionamiento, so leave untouched
sel2 <- which(nm.saved$p18_2005[sel.r]==0)               ## which had zero pop 2005
nm.saved[sel.r[sel2],] ## give when2 not NA, no opther reseccionamiento, so leave untouched
## mark as done
done[sel.r] <- 1
##
## baja in 2013
sel.r <- which(nm.saved$p18_2020==0 & nm$dskip==0 & is.na(nm$when2) & nm$baja==2013)
tmp.est[sel.r, grep("201[3-9]|202[0-3]", colnames(tmp.est))] <- 0 ## post baja to zero
sel2 <- which(nm.saved$p18_2010[sel.r]==0)               ## which had zero pop 2010
nm.saved[sel.r[sel2],] ## give when2 not NA, no opther reseccionamiento, so leave untouched
sel2 <- which(nm.saved$p18_2005[sel.r]==0)               ## which had zero pop 2005
nm.saved[sel.r[sel2],] ## give when2 not NA, no opther reseccionamiento, so leave untouched
## mark as done
done[sel.r] <- 1
##
## baja in 2012
sel.r <- which(nm.saved$p18_2020==0 & nm$dskip==0 & is.na(nm$when2) & nm$baja==2012)
tmp.est[sel.r, grep("201[2-9]|202[0-3]", colnames(tmp.est))] <- 0 ## post baja to zero
sel2 <- which(nm.saved$p18_2010[sel.r]==0)               ## which had zero pop 2010
nm.saved[sel.r[sel2],] ## give when2 not NA, no opther reseccionamiento, so leave untouched
sel2 <- which(nm.saved$p18_2005[sel.r]==0)               ## which had zero pop 2005
nm.saved[sel.r[sel2],] ## give when2 not NA, no opther reseccionamiento, so leave untouched
## mark as done
done[sel.r] <- 1
##
## baja in 2011
sel.r <- which(nm.saved$p18_2020==0 & nm$dskip==0 & is.na(nm$when2) & nm$baja==2011)
tmp.est[sel.r, grep("201[1-9]|202[0-3]", colnames(tmp.est))] <- 0 ## post baja to zero
sel2 <- which(nm.saved$p18_2010[sel.r]==0)               ## which had zero pop 2010
nm.saved[sel.r[sel2],] ## give when2 not NA, no opther reseccionamiento, so leave untouched
sel2 <- which(nm.saved$p18_2005[sel.r]==0)               ## which had zero pop 2005
nm.saved[sel.r[sel2],] ## give when2 not NA, no opther reseccionamiento, so leave untouched
## mark as done
done[sel.r] <- 1
##
## baja in 2009
sel.r <- which(nm.saved$p18_2020==0 & nm$dskip==0 & is.na(nm$when2) & nm$baja==2009)
tmp.est[sel.r, grep("2009|201|202[0-3]", colnames(tmp.est))] <- 0 ## post baja to zero
sel2 <- which(nm.saved$p18_2010[sel.r]==0)               ## which had zero pop 2010
nm.saved[sel.r[sel2],] ## give when2 not NA, no opther reseccionamiento, so leave untouched
sel2 <- which(nm.saved$p18_2005[sel.r]==0)               ## which had zero pop 2005
nm.saved[sel.r[sel2],] ## give when2 not NA, no opther reseccionamiento, so leave untouched
## mark as done
done[sel.r] <- 1
##
## baja in 2006
sel.r <- which(nm.saved$p18_2020==0 & nm$dskip==0 & is.na(nm$when2) & nm$baja==2006)
tmp.est[sel.r, grep("200[6-9]|201|202[0-3]", colnames(tmp.est))] <- 0 ## post baja to zero
sel2 <- which(nm.saved$p18_2010[sel.r]==0)               ## which had zero pop 2010
nm.saved[sel.r[sel2],] ## give when2 not NA, no opther reseccionamiento, so leave untouched
sel2 <- which(nm.saved$p18_2005[sel.r]==0)               ## which had zero pop 2005
nm.saved[sel.r[sel2],] ## give when2 not NA, no opther reseccionamiento, so leave untouched
## mark as done
done[sel.r] <- 1
##
## baja in 2002
sel.r <- which(nm.saved$p18_2020==0 & nm$dskip==0 & is.na(nm$when2) & nm$baja==2002)
tmp.est[sel.r, grep("200[2-9]|201|202[0-3]", colnames(tmp.est))] <- 0 ## post baja to zero
sel2 <- which(nm.saved$p18_2010[sel.r]==0)               ## which had zero pop 2010
nm.saved[sel.r[sel2],] ## give when2 not NA, no opther reseccionamiento, so leave untouched
sel2 <- which(nm.saved$p18_2005[sel.r]==0)               ## which had zero pop 2005
nm.saved[sel.r[sel2],] ## give when2 not NA, no opther reseccionamiento, so leave untouched
## mark as done
done[sel.r] <- 1

table(done)
x

i <- 1
data.frame(nm=nm[sel.r[i], grep("secc|p18", colnames(nm))], sav=nm.saved[sel.r[i],])
data.frame(nm=tmp.est[i,], sav=nm.saved[sel.r[i],])

tmp.est[i, grep("2005|2010|2020", colnames(tmp.est))]

sel.r[i]
x

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



############################################################################################
## Apply my_agg to generate municipal and state aggregates (nm[sel.r,]$m:2005-2010-2020), ##
## will be used in projection routine below.                                              ##
############################################################################################
## ##
## fill in p18s for aggregation
nm <- within(nm, {
    p18m_05 <- p18_2005;
    p18m_10 <- p18_2010;
    p18m_20 <- p18_2020;
    p18e_05 <- p18_2005;
    p18e_10 <- p18_2010;
    p18e_20 <- p18_2020;
})


###################################################################################################
## Ojo: Reseccionamiento duplicates some pop counts. This manipulations prevent double-counting. ##
#############################################################################################################
## Rules to code altas and bajas in eq and its source (tablaEquivalenciasSeccionalesDesde1994.csv):        ##
## if actualización cartografía                                                                            ##
## - in ene-mar 2001 then baja/alta <- 2001                                                                ##
## - in ago-dic 2002 then baja/alta <- 2003                                                                ##
## - in abr-jul 2002 then check if no election that year for baja/alta <- 2003, else 2002                  ##
## Pre-alta and post-baja pops to zero to avoid double counting in aggregation.                            ##
#############################################################################################################
tmp <- nm # duplicate for manipulation
sel.r <- which(tmp$baja<=2005)
tmp[sel.r,] <- within(tmp[sel.r,], {
    p18m_05 <- 0;
    p18m_10 <- 0;
    p18m_20 <- 0;
    p18e_05 <- 0;
    p18e_10 <- 0;
    p18e_20 <- 0;
})
sel.r <- which(tmp$baja>2005 & tmp$baja<=2010)
tmp[sel.r,] <- within(tmp[sel.r,], {
    p18m_10 <- 0;
    p18m_20 <- 0;
    p18e_10 <- 0;
    p18e_20 <- 0;
})
sel.r <- which(tmp$baja>2010 & tmp$baja<=2020)
tmp[sel.r,] <- within(tmp[sel.r,], {
    p18m_20 <- 0;
    p18e_20 <- 0;
})
## altas
sel.r <- which(tmp$alta>2020)
tmp[sel.r,] <- within(tmp[sel.r,], {
    p18m_05 <- 0;
    p18m_10 <- 0;
    p18m_20 <- 0;
    p18e_05 <- 0;
    p18e_10 <- 0;
    p18e_20 <- 0;
})
sel.r <- which(tmp$alta>2010 & tmp$alta<=2020)
tmp[sel.r,] <- within(tmp[sel.r,], {
    p18m_05 <- 0;
    p18m_10 <- 0;
    p18e_05 <- 0;
    p18e_10 <- 0;
})
sel.r <- which(tmp$alta>2005 & tmp$alta<=2010)
tmp[sel.r,] <- within(tmp[sel.r,], {
    p18m_05 <- 0;
    p18e_05 <- 0;
})
##
table((tmp$p18e_05 - nm$p18e_05)==0) # FALSE are how many changed?
table(nm$seccion==tmp$seccion)
## fill manip
nm$p18m_05 <- tmp$p18m_05
nm$p18m_10 <- tmp$p18m_10
nm$p18m_20 <- tmp$p18m_20
nm$p18e_05 <- tmp$p18e_05
nm$p18e_10 <- tmp$p18e_10
nm$p18e_20 <- tmp$p18e_20
rm(tmp)
##
## aggregate state pops
sel.c <- c("p18e_05", "p18e_10", "p18e_20") ## select state pop columns
nm[1:2,]
nm <- my_agg(d=nm, sel.c=sel.c, by="edon", drop.dupli=FALSE)                 ## and sum-up state secciones
##
##
####################################################################################
## Subroutine aggregates municipios and distrito censuses from secciones.         ##
## Used to be part of code/aggregates-mun-dis-from-sec, but brought here because  ##
## aggregates need manipulation to prevent double counting reseccionamiento cases ##
####################################################################################
#############################################
## census indicators: aggregate municipios ##
#############################################
sel.c <- c("p18m_05", "p18m_10", "p18m_20") ## select mun pop columns
# 1994 municipalities
d <- nm; d[is.na(d)] <- 0
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
d <- add.miss.mun(d)                                     # add missing municipios, if any
d <- d[order(d$ife),]                                    # sort
d$inegi <- NA; d$inegi <- ife2inegi(d$ife)               # translate to inegi
d$mun <- ife2mun(d$ife)                                  # translate to mun
d <- within(d, {
    p18_2005 <- p18m_05;                                 # rename pop cols
    p18_2010 <- p18m_10;
    p18_2020 <- p18m_20;
})
## clean
d$p18m_05 <- d$p18m_10 <- d$p18m_20 <- d$p18e_05 <- d$p18e_10 <- d$p18e_20 <- NULL
censom94 <- d                                            # rename object
# 1997 municipalities
d <- nm; d[is.na(d)] <- 0
d$ife <- d$ife1997                                       # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))                   # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]                       # drop from aggregation
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
d <- add.miss.mun(d)                                     # add missing municipios, if any
d <- d[order(d$ife),]                                    # sort
d$inegi <- NA; d$inegi <- ife2inegi(d$ife)               # translate to inegi
d$mun <- ife2mun(d$ife)                                  # translate to mun
d <- within(d, {
    p18_2005 <- p18m_05;                                 # rename pop cols
    p18_2010 <- p18m_10;
    p18_2020 <- p18m_20;
})
## clean
d$p18m_05 <- d$p18m_10 <- d$p18m_20 <- d$p18e_05 <- d$p18e_10 <- d$p18e_20 <- NULL
censom97 <- d                                            # rename object
# 2000 municipalities
d <- nm; d[is.na(d)] <- 0
d$ife <- d$ife2000                                       # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))                   # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]                       # drop from aggregation
stmp <- which(d$baja>0 & (d$alta>2000 | d$baja<=2000))   # bajas/altas after/before 2000
if (length(stmp)>0) d <- d[-stmp,]                       # drop from aggregation
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]             # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]             # drop counterfactual districts
##
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE)     # use aggregating function
d <- within(d, {
    seccion <- ord <- NULL                               # drop seccion ids and other useless columns
    alta <- baja <- OBSERVACIONES  <- NULL
    action  <- orig.dest  <- when  <- NULL
    action2 <- orig.dest2 <- when2 <- NULL
    action3 <- orig.dest3 <- when3 <- NULL
    coment <- dmunchg <- NULL
})
d <- add.miss.mun(d)                                     # add missing municipios, if any
d <- d[order(d$ife),]                                    # sort
d$inegi <- NA; d$inegi <- ife2inegi(d$ife)               # translate to inegi
d$mun <- ife2mun(d$ife)                                  # translate to mun
d <- within(d, {
    p18_2005 <- p18m_05;                                 # rename pop cols
    p18_2010 <- p18m_10;
    p18_2020 <- p18m_20;
})
## clean
d$p18m_05 <- d$p18m_10 <- d$p18m_20 <- d$p18e_05 <- d$p18e_10 <- d$p18e_20 <- NULL
censom00 <- d                                            # rename object
# 2003 municipalities
d <- nm; d[is.na(d)] <- 0
d$ife <- d$ife2003                                       # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))                   # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]                       # drop from aggregation
stmp <- which(d$baja>0 & (d$alta>2003 | d$baja<=2003))   # bajas/altas after/before 2003
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
d <- add.miss.mun(d)                                     # add missing municipios, if any
d <- d[order(d$ife),]                                    # sort
d$inegi <- NA; d$inegi <- ife2inegi(d$ife)               # translate to inegi
d$mun <- ife2mun(d$ife)                                  # translate to mun
d <- within(d, {
    p18_2005 <- p18m_05;                                 # rename pop cols
    p18_2010 <- p18m_10;
    p18_2020 <- p18m_20;
})
## clean
d$p18m_05 <- d$p18m_10 <- d$p18m_20 <- d$p18e_05 <- d$p18e_10 <- d$p18e_20 <- NULL
censom03 <- d                                            # rename object
# 2006 municipalities
d <- nm; d[is.na(d)] <- 0
d$ife <- d$ife2006                                       # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))                   # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]                       # drop from aggregation
stmp <- which(d$baja>0 & (d$alta>2006 | d$baja<=2006))   # bajas/altas after/before 2006
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
d <- add.miss.mun(d)                                     # add missing municipios, if any
d <- d[order(d$ife),]                                    # sort
d$inegi <- NA; d$inegi <- ife2inegi(d$ife)               # translate to inegi
d$mun <- ife2mun(d$ife)                                  # translate to mun
d <- within(d, {
    p18_2005 <- p18m_05;                                 # rename pop cols
    p18_2010 <- p18m_10;
    p18_2020 <- p18m_20;
})
## clean
d$p18m_05 <- d$p18m_10 <- d$p18m_20 <- d$p18e_05 <- d$p18e_10 <- d$p18e_20 <- NULL
censom06 <- d                                            # rename object
# 2009 municipalities
d <- nm; d[is.na(d)] <- 0
d$ife <- d$ife2009                                       # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))                   # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]                       # drop from aggregation
stmp <- which(d$baja>0 & (d$alta>2009 | d$baja<=2009))   # bajas/altas after/before 2009
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
d <- add.miss.mun(d)                                     # add missing municipios, if any
d <- d[order(d$ife),]                                    # sort
d$inegi <- NA; d$inegi <- ife2inegi(d$ife)               # translate to inegi
d$mun <- ife2mun(d$ife)                                  # translate to mun
d <- within(d, {
    p18_2005 <- p18m_05;                                 # rename pop cols
    p18_2010 <- p18m_10;
    p18_2020 <- p18m_20;
})
## clean
d$p18m_05 <- d$p18m_10 <- d$p18m_20 <- d$p18e_05 <- d$p18e_10 <- d$p18e_20 <- NULL
censom09 <- d                                            # rename object
# 2012 municipalities
d <- nm; d[is.na(d)] <- 0
d$ife <- d$ife2012                                       # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))                   # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]                       # drop from aggregation
stmp <- which(d$baja>0 & (d$alta>2012 | d$baja<=2012))   # bajas/altas after/before 2012
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
d <- add.miss.mun(d)                                     # add missing municipios, if any
d <- d[order(d$ife),]                                    # sort
d$inegi <- NA; d$inegi <- ife2inegi(d$ife)               # translate to inegi
d$mun <- ife2mun(d$ife)                                  # translate to mun
d <- within(d, {
    p18_2005 <- p18m_05;                                 # rename pop cols
    p18_2010 <- p18m_10;
    p18_2020 <- p18m_20;
})
## clean
d$p18m_05 <- d$p18m_10 <- d$p18m_20 <- d$p18e_05 <- d$p18e_10 <- d$p18e_20 <- NULL
censom12 <- d                                            # rename object
# 2015 municipalities
d <- nm; d[is.na(d)] <- 0
d$ife <- d$ife2015                                       # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))                   # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]                       # drop from aggregation
stmp <- which(d$baja>0 & (d$alta>2015 | d$baja<=2015))   # bajas/altas after/before 2015
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
d <- add.miss.mun(d)                                     # add missing municipios, if any
d <- d[order(d$ife),]                                    # sort
d$inegi <- NA; d$inegi <- ife2inegi(d$ife)               # translate to inegi
d$mun <- ife2mun(d$ife)                                  # translate to mun
d <- within(d, {
    p18_2005 <- p18m_05;                                 # rename pop cols
    p18_2010 <- p18m_10;
    p18_2020 <- p18m_20;
})
## clean
d$p18m_05 <- d$p18m_10 <- d$p18m_20 <- d$p18e_05 <- d$p18e_10 <- d$p18e_20 <- NULL
censom15 <- d                                            # rename object
# 2018 municipalities
d <- nm; d[is.na(d)] <- 0
d$ife <- d$ife2018                                       # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))                   # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]                       # drop from aggregation
stmp <- which(d$baja>0 & (d$alta>2018 | d$baja<=2018))   # bajas/altas after/before 2018
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
d <- add.miss.mun(d)                                     # add missing municipios, if any
d <- d[order(d$ife),]                                    # sort
d$inegi <- NA; d$inegi <- ife2inegi(d$ife)               # translate to inegi
d$mun <- ife2mun(d$ife)                                  # translate to mun
d <- within(d, {
    p18_2005 <- p18m_05;                                 # rename pop cols
    p18_2010 <- p18m_10;
    p18_2020 <- p18m_20;
})
## clean
d$p18m_05 <- d$p18m_10 <- d$p18m_20 <- d$p18e_05 <- d$p18e_10 <- d$p18e_20 <- NULL
censom18 <- d                                            # rename object
# 2021 municipalities
d <- nm; d[is.na(d)] <- 0
d$ife <- d$ife2021                                       # municipio ids from the historic map
stmp <- which(d$ife==0 | is.na(d$ife))                   # missing ids (secciones dadas de baja)
if (length(stmp)>0) d <- d[-stmp,]                       # drop from aggregation
stmp <- which(d$baja>0 & (d$alta>2021 | d$baja<=2021))   # bajas/altas after/before 2021
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
d <- add.miss.mun(d)                                     # add missing municipios, if any
d <- d[order(d$ife),]                                    # sort
d$inegi <- NA; d$inegi <- ife2inegi(d$ife)               # translate to inegi
d$mun <- ife2mun(d$ife)                                  # translate to mun
d <- within(d, {
    p18_2005 <- p18m_05;                                 # rename pop cols
    p18_2010 <- p18m_10;
    p18_2020 <- p18m_20;
})
## clean
d$p18m_05 <- d$p18m_10 <- d$p18m_20 <- d$p18e_05 <- d$p18e_10 <- d$p18e_20 <- NULL
censom21 <- d                                            # rename object
#
rm(d,sel.c,add.miss.mun,stmp)
##
##
############################################
## census indicators: aggregate distritos ##
############################################
sel.c <- c("p18m_05", "p18m_10", "p18m_20") ## to avoid opening new slots, uses m columns for district manip
## 1979 map
d <- nm; d[is.na(d)] <- 0
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
d <- within(d, {
    p18_2005 <- p18m_05;                                 # rename pop cols
    p18_2010 <- p18m_10;
    p18_2020 <- p18m_20;
})
## clean
d$p18m_05 <- d$p18m_10 <- d$p18m_20 <- d$p18e_05 <- d$p18e_10 <- d$p18e_20 <- NULL
censod79 <- d                                  # rename object
## 1997 map
d <- nm; d[is.na(d)] <- 0
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
d <- within(d, {
    p18_2005 <- p18m_05;                                 # rename pop cols
    p18_2010 <- p18m_10;
    p18_2020 <- p18m_20;
})
## clean
d$p18m_05 <- d$p18m_10 <- d$p18m_20 <- d$p18e_05 <- d$p18e_10 <- d$p18e_20 <- NULL
censod97 <- d                                  # rename object
## 2006 map
d <- nm; d[is.na(d)] <- 0
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
d <- within(d, {
    p18_2005 <- p18m_05;                                 # rename pop cols
    p18_2010 <- p18m_10;
    p18_2020 <- p18m_20;
})
## clean
d$p18m_05 <- d$p18m_10 <- d$p18m_20 <- d$p18e_05 <- d$p18e_10 <- d$p18e_20 <- NULL
censod06 <- d                                  # rename object
## 2018 map ##
d <- nm; d[is.na(d)] <- 0
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
d <- within(d, {
    p18_2005 <- p18m_05;                                 # rename pop cols
    p18_2010 <- p18m_10;
    p18_2020 <- p18m_20;
})
## clean
d$p18m_05 <- d$p18m_10 <- d$p18m_20 <- d$p18e_05 <- d$p18e_10 <- d$p18e_20 <- NULL
censod18 <- d                                  # rename object
#
rm(d,sel.c,sel.drop)


AQUI CHECK
###################################################################################################
## Add municipio-level 1990:2000 pob18 (fills up for missing seccion-level data for those years) ##
## OJO: Actual municipios only, even for counterfactual maps                                     ##
###################################################################################################
ls()[grep("censo",ls())]
censom <- within(censom, {
    p18_2000 <- as.numeric(pob18_2000);
    p18_1995 <- pob18_1995;
    p18_1990 <- pob18_1990;
    pob18_1990 <- pob18_1995 <- pob18_2000 <- NULL;
})
#########
## m00 ##
#########
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
my_agg(censom94, sel.c="p18_2020", by="edon")[2,10]
my_agg(censom97, sel.c="p18_2020", by="edon")[2,10]
my_agg(censom00, sel.c="p18_2020", by="edon")[2,10]
my_agg(censom03, sel.c="p18_2020", by="edon")[2,10]
my_agg(censom06, sel.c="p18_2020", by="edon")[2,10]
my_agg(censom09, sel.c="p18_2020", by="edon")[2,10]
my_agg(censom12, sel.c="p18_2020", by="edon")[2,10]
my_agg(censom15, sel.c="p18_2020", by="edon")[2,10]
my_agg(censom18, sel.c="p18_2020", by="edon")[2,10]
my_agg(censom21, sel.c="p18_2020", by="edon")[2,10]
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
## Tonatitla (created 2006)
## Elected 1st mu gov 2006
## Looks ok
tmp <- censom06      # duplicate for manipulation
sel.parent <- which(tmp$inegi==15044)
sel.son <-    which(tmp$inegi==15125)
tmp[c(sel.parent, sel.son),]
##

## Coatetelco, Xoxocotla, Huayapan (created 2019)
## Into fed els 2021, became usos y costumbres 2021
## Look ok
tmp <- censom06      # duplicate for manipulation
sel.parent <- which(tmp$inegi==17015)
sel.son <-    which(tmp$inegi==17034)
tmp[c(sel.parent, sel.son),]
##
tmp <- censom06      # duplicate for manipulation
sel.parent <- which(tmp$inegi==17017)
sel.son <-    which(tmp$inegi==17035)
tmp[c(sel.parent, sel.son),]
##
tmp <- censom06      # duplicate for manipulation
sel.parent <- which(tmp$inegi==17022)
sel.son <-    which(tmp$inegi==17036)
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
rm(dok, sel.son, sel.parent, sel.r, tmp) ## clean


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
