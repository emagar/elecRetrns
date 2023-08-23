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
## Last modified: 22aug2023                                         ##
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
sum.split <- function(d=censo, year.var=2020, rnd=1, dfull=censo) {
    ## Note: d can be a subset of dfull (dfull needed to find target secciones)
    ##
    if (year.var %notin% c(2005,2010,2020)){
        print("Warning: year.var is not a sección-level census year")
        stop
    }
    ##year.var <- 2020 # debug
    ##d <-     sh.w # debug
    ##dfull <- sh # debug
    d <- d
    ## exclude non-numeric columns that needn't sum-up
    sel.col <- setdiff(colnames(d),
                       c("ord", "edon", "edo", "seccion", "ife", "inegi", "mun", "ddone", "dskip", "OBSERVACIONES", "coment"
                       , "alta", "baja", "dmunchg"
                       , "action", "orig.dest", "when", "action2", "orig.dest2", "when2", "action3", "orig.dest3", "when3"
                       , "ife1991", "ife1994", "ife1997", "ife2000", "ife2003", "ife2006"
                       , "ife2009", "ife2012", "ife2015", "ife2018", "ife2021", "ife2024"
                       , "dis1979", "dis1997", "dis2006", "dis2013", "dis2018"
                         ))
    sel.col <- sel.col[grep(year.var, sel.col)] # crop sel.col to sum only year.var
    ##
    for (i in 1:nrow(d)){
        ##i <- 1 # debug
        if (rnd==1){
            year <- d$when[i]  # reseccionamiento year (modify data in subsequent years only)
            sel.to <- eval(str2expression(d$orig.dest [i])) # turn string w vector def into vector of target secciones to sum-up
        }
        if (rnd==2){
            year <- d$when2[i]  # reseccionamiento year (modify data in subsequent years only)
            sel.to <- eval(str2expression(d$orig.dest2[i])) # turn string w vector def into vector of target secciones to sum-up
        }
        if (rnd==3){
            year <- d$when3[i]  # reseccionamiento year (modify data in subsequent years only)
            sel.to <- eval(str2expression(d$orig.dest3[i])) # turn string w vector def into vector of target secciones to sum-up
        }
        sel.to <- d$edon[i] * 10000 + sel.to
        ##
        ##sel.agg <- which(d$seccion %in% sel.to) ## sel.to must be defined before invoking this function below
        ##sel.agg <- which(censo$seccion %in% sel.to) ## indices to be summed-up from censo
        sel.agg <- which(dfull$seccion %in% sel.to) ## indices to be summed-up from censo
        ## sum population
        totals <- colSums(dfull[sel.agg, sel.col], na.rm = TRUE)
        ## paste them into manipulated sección
        d[i,sel.col] <- totals;
        ##d$dmanip[i] <- 1;  # indicates manipulation
    }
    ##
    ## return manipulated data
    return(d)
}

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

## get census projection functions
source("../../code/v-hat-etc/interpolate-census-functions.r")

## 4. Duplicate nm=censo, adding cols 1994:2021, m:2005-2010-2020, ddone=0, etc
##    nm = nominal quantities
nm <- censo
#nm <- censo[censo$inegi==1001,] # debug
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
    nmanip <- dfirst <- dsingle <- dskip <- dready2est <- ddone <- dneedsum <- 0;
    when3b <- when3; orig.dest3b <- orig.dest3; action3b <- action3; #  backup action/orig.dest/when so that these
    when2b <- when2; orig.dest2b <- orig.dest2; action2b <- action2; #  can be erased once seccion gets manipulated
    whenb  <- when;  orig.destb  <- orig.dest;  actionb  <- action;  #  ---helps debug block immediately below
    p18e_20 <- NA; # yy so grep excludes
    p18e_10 <- NA; # yy so grep excludes
    p18e_05 <- NA; # yy so grep excludes
    #p18_1994 <- p18_1997 <- p18_2000 <- p18_2003 <- p18_2006 <- p18_2009 <- p18_2012 <- p18_2015 <- p18_2018 <- p18_2021 <- NA;
})
##nm <- nm[order(nm$inegi, nm$seccion),] # sort mun
##


##############################################################
## Block starts here                                        ##
## OJO: Need to re-evaluate each sub-block of the block     ##
##      below when new seccion-level censo is available.    ##
##      Block below takes care of manipulating and setting  ##
##      aside (with dready2est and ddone) secciones that    ##
##      can/cannot be handled with log(dv)~iv approach to   ##
##      project seccion-level p18.                          ##
##      Run with a fresh nm <- censo when debugging         ##
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
## these have no census info, drop action
sel.tmp <- which(nm$action=="state.chg")
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
nm[sel.tmp, c("alta", "baja", "action", "p18_2005", "p18_2010", "p18_2020")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    dskip <- 1; 
    action  <- action2; orig.dest  <- orig.dest2; when  <- when2
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
############################################################################################
## These are hard cases where sección was split w/o baja --- ie. lost some territory to   ##
## another seccion, sometimes new but not always. Will ignore these splits w/o attempting ##
## to sum.split() population...                                                           ##
############################################################################################
## Case where the split is in action3
sel.tmp <- which(nm$seccion %in% c(122710))
nm[sel.tmp, c("edon","seccion","orig.destb","orig.dest2b","orig.dest3b")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
## Cases where the split is in action2
sel.tmp <- which(nm$seccion %in% c(143645, 161976, 162692, 190647, 192136, 192197, 192260, 192261, 192273, 192333, 192614, 230874, 230875))
nm[sel.tmp, c("edon","seccion","orig.destb","orig.dest2b","orig.dest3b")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
## Cases where the split is in action
sel.tmp <- which(nm$seccion %in%  c(10451, 20170, 20709, 20710, 20725, 20733, 21297, 21298, 21368, 40339, 40485, 50171, 50279, 50285, 50449, 50717, 51089, 70014, 70015, 70018, 70019, 71044, 71047, 71419, 71435, 71436, 71836, 71837, 80034, 80058, 80059, 90774, 93951, 93976, 94151, 94153, 94154, 94156, 94157, 94163, 94164, 94237, 100055, 101234, 120742, 120760, 120761, 120762, 121174, 121200, 121709, 121712, 121713, 121714, 121744, 121754, 121759, 121762, 122051, 122527, 122704, 122706, 122710, 122716, 122722, 130742, 130900, 130928, 130936, 130938, 130953, 140137, 142563, 142719, 143084, 143088, 150054, 150633, 150634, 150638, 150670, 150938, 151276, 152025, 152027, 152028, 152459, 152460, 152462, 153800, 153866, 153922, 153941, 154004, 154028, 154032, 154068, 154079, 154084, 154086, 154100, 154206, 154257, 154277, 154305, 154308, 154342, 154413, 154601, 154602, 154603, 154614, 154839, 155442, 155461, 155465, 155480, 155681, 155787, 155826, 155829, 155867, 160001, 160002, 160003, 160005, 160324, 160759, 160760, 161256, 161914, 161971, 161976, 161991, 162153, 162238, 162346, 162531, 162533, 170619, 180002, 190066, 190107, 190145, 190146, 190150, 190439, 190567, 190568, 190588, 190590, 190620, 190647, 190804, 190852, 191705, 191706, 191770, 191772, 200784, 201199, 201532, 201726, 201728, 201749, 201997, 202086, 211079, 211128, 211416, 220481, 220502, 220511, 220513, 220542, 220550, 220554, 230092, 230171, 230172, 230175, 230177, 230179, 230180, 230261, 230290, 240176, 261154, 290047, 290293, 290400, 290401, 290402, 290578, 300480, 301097, 301144, 301303, 301523, 301525, 301591, 302381, 302383, 302519, 302521, 302659, 302661, 302979, 303102, 303105, 303106, 303366, 303367, 303385, 303388, 303706, 303712, 303730, 303792, 304158, 304507, 304616, 304631, 310223, 310652, 310991))
nm[sel.tmp, c("edon","seccion","orig.destb","orig.dest2b","orig.dest3b")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    action  <- action2; orig.dest  <- orig.dest2; when  <- when2
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
## Merged.to secciones in 2020 get flat 2010 pop
sel.tmp <- which(nm$action=="merged.to" & (nm$when==2012|nm$when==2014|nm$when==2020) & !is.na(nm$p18_2005))
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2020 <- p18_2010
    dready2est <- 1
    nmanip <- nmanip + 1
    action  <- action2; orig.dest  <- orig.dest2; when  <- when2
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
## Except these, missing 2005
sel.tmp <- which(nm$action=="merged.to" & (nm$when==2008|nm$when==2010|nm$when==2012|nm$when==2014|nm$when==2020) & (is.na(nm$p18_2005)|nm$p18_2005==0))
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","dready2est","dskip")]
nm$dskip[sel.tmp] <- 1
## same as above for action2
sel.tmp <- which(nm$alta==1993 & nm$action2=="merged.to" & nm$when2==2020)
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
## nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
## nm[sel.tmp,] <- within(nm[sel.tmp,], {
##     p18_2020 <- p18_2010
##     dready2est <- 1
##     nmanip <- nmanip + 1
##     action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
##     action3 <- "";      orig.dest3 <- "";         when3 <- NA
## })
##
sel.tmp <- which(nm$action3=="merged.to")
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
## merged.to cases in action 2
sel.tmp <- which(nm$action2=="merged.to")
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
##
sel.tmp <- which(nm$action2=="merged.to" & (nm$alta>=1993 & nm$alta<=1996) & (nm$baja==2008|nm$baja==2011) & nm$dskip==0)
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    ##p18_1994 <- p18_1997 <- p18_2000 <- p18_2003 <- p18_2006 <- p18_2009 <- p18_2010 <- p18_2005
    p18_2020 <- p18_2010 <- p18_2005
    dready2est <- 1
    nmanip <- nmanip + 1
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
## this one gets linear 2005-2010 from alta to baja
sel.tmp <- which(nm$action2=="merged.to" & nm$alta==2002 & nm$baja==2014)
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2020 <- prj0510(nm[sel.tmp,], 2020)
    dready2est <- 1
    nmanip <- nmanip + 1
    action  <- "";      orig.dest  <- "";         when  <- NA
    action2 <- "";      orig.dest2 <- "";         when2 <- NA
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
## these have no info
sel.tmp <- which(nm$action2=="merged.to" & nm$alta>=2013 & nm$alta<=2017 & (nm$baja==2019|nm$baja==2020))
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    dskip <- 1
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
## flat 2005 from alta to baja
sel.tmp <- which(nm$action2=="merged.to" & nm$alta==2004 & nm$baja==2008)
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2020 <- p18_2010 <- p18_2005
    #p18_2006 <- p18_2005
    dready2est <- 1
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
## no more action2 merged.to
sel.tmp <- which(nm$action2=="merged.to")
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
##
################################
## remainder action merged.to ##
################################
sel.tmp <- which(nm$action =="merged.to" & nm$dskip==0)
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
table(action=nm$action[sel.tmp], action2=nm$action2[sel.tmp], useNA = "ifany")
## These two can be ignored, no pop 05 10 then merged to in 20
sel.tmp <- which(nm$action=="merged.to" & nm$alta==1993 & nm$baja==2020 & is.na(nm$p18_2020) & nm$dskip==0)
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
## nm[sel.tmp,] <- within(nm[sel.tmp,], {
##     dskip <- 1
##     action  <- action2; orig.dest  <- orig.dest2; when  <- when2
##     action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
##     action3 <- "";      orig.dest3 <- "";         when3 <- NA
## })
## These have no info
sel.tmp <- which(nm$action=="merged.to" & nm$alta==1993 & nm$baja==2008 & is.na(nm$p18_2005) & nm$dskip==0)
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
## nm[sel.tmp,] <- within(nm[sel.tmp,], {
##     dskip <- 1
##     action  <- action2; orig.dest  <- orig.dest2; when  <- when2
##     action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
##     action3 <- "";      orig.dest3 <- "";         when3 <- NA
## })
## Fully flat up to baja
sel.tmp <- which(nm$action=="merged.to" & nm$alta==1993 & nm$ddone==0 & (nm$baja>=2008 & nm$baja<=2011) & nm$dskip==0)
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2020 <- p18_2010 <- p18_2005
    #p18_2010 <- p18_1994 <- p18_1997 <- p18_2000 <- p18_2003 <- p18_2006 <- p18_2009 <- p18_2005
    dready2est <- 1;
    nmanip <- nmanip + 1
    action  <- action2; orig.dest  <- orig.dest2; when  <- when2
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
## No more merged-to remaining
nm[which(nm$action=="merged.to" & nm$dskip==0),]
table(nm$action , skip=nm$dskip)
table(nm$action2, skip=nm$dskip)
table(nm$action3, skip=nm$dskip)
##
## No way to know population here
sel.tmp <- which(nm$action=="split.from" & nm$action2=="" & (nm$when==2021|nm$when==2022))
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    dskip <- 1; 
    action  <- action2; orig.dest  <- orig.dest2; when  <- when2
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
## Split.from 2009-2010, no censo 2010 -- flat
sel.tmp <- which(nm$action=="split.from" & nm$action2=="" & (nm$when==2009|nm$when==2010)  & nm$p18_2010==0 & nm$p18_2020>0 & nm$dskip==0)
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2005 <- p18_2010 <- p18_2020
    #p18_2010 <- p18_1994 <- p18_1997 <- p18_2000 <- p18_2003 <- p18_2006 <- p18_2009 <- p18_2005
    dready2est <- 1;
    nmanip <- nmanip + 1
    action  <- action2; orig.dest  <- orig.dest2; when  <- when2
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
## Split.from in 2010-2020: no census 2020
sel.tmp <- which(nm$action=="split.from" & nm$action2=="" & (nm$when==2009|nm$when==2012|nm$when==2017|nm$when==2019|nm$when==2020) & (is.na(nm$p18_2020) | nm$p18_2020==0))
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    dskip <- 1; 
    action  <- action2; orig.dest  <- orig.dest2; when  <- when2
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
## Split.from in 2019-2020: fully flat
sel.tmp <- which(nm$action=="split.from" & nm$action2=="" & (nm$when==2019|nm$when==2020))
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
nm[sel.tmp[150:155], c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
table(is.na(nm$p18_2020[sel.tmp]))
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2005 <- p18_2010 <- p18_2020
    #p18_2021 <- p18_2020
    dready2est <- 1
    nmanip <- nmanip + 1
    action  <- action2; orig.dest  <- orig.dest2; when  <- when2
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
## Special handling for these short-lived secciones
## sel.tmp <- which(nm$seccion==10487)
## nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
##
## nm[sel.tmp,] <- within(nm[sel.tmp,], {
##     p18_2006 <- p18_2005
##     ddone <- 1
##     nmanip <- nmanip + 1
##     action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
##     action3 <- "";      orig.dest3 <- "";         when3 <- NA
## })
## sel.tmp <-            which(nm$seccion==21369)
## nm[sel.tmp,]
## nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
## nm[sel.tmp,] <- within(nm[sel.tmp,], {
##     p18_1997 <- p18_2000 <- p18_2003 <- p18_2006 <- p18_2009 <- p18_2010 <- p18_2005
##     ddone <- 1
##     nmanip <- nmanip + 1
##     action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
##     action3 <- "";      orig.dest3 <- "";         when3 <- NA
## })
## ## gets linear 2005-2010 projection across the board
## sel.tmp <- which(nm$seccion==155931)
## nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
## nm[sel.tmp,] <- within(nm[sel.tmp,], {
##     p18_2003 <- prj0510(nm[sel.tmp,], 2003)
##     p18_2006 <- prj0510(nm[sel.tmp,], 2006)
##     p18_2009 <- prj0510(nm[sel.tmp,], 2009)
##     p18_2012 <- prj0510(nm[sel.tmp,], 2012)
##     ddone <- 1
##     nmanip <- nmanip + 1
##     action  <- "";      orig.dest  <- "";         when  <- NA
##     action2 <- "";      orig.dest2 <- "";         when2 <- NA
##     action3 <- "";      orig.dest3 <- "";         when3 <- NA
## })
##
## These have 2020 NAs, need sum.split()
sel.tmp <- which(nm$action=="split.from" & nm$action2=="split.to" & nm$dready2est==0 & (nm$alta==2007|nm$alta==2012|nm$alta==2009) & (nm$baja==2019|nm$baja==2020) & is.na(nm$p18_2020))
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    dneedsum <- 1
    action  <- action2; orig.dest  <- orig.dest2; when  <- when2
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
## Ready given reseccionamiento before/after censuses
sel.tmp <- which(nm$dskip==0 & nm$ddone==0 & nm$dready2est==0 & nm$dneedsum==0 &
                 nm$action=="split.from" & nm$action2=="split.to" & (nm$when2==2021|nm$when2==2022) & nm$alta==2002)
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    dready2est <- 1
    action  <- "";      orig.dest  <- "";         when  <- NA
    action2 <- "";      orig.dest2 <- "";         when2 <- NA
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
## gets linear 2010-2020 projection across the board
sel.tmp <-            which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 & 
                            nm$action=="split.from" & nm$action2=="split.to" & nm$alta==2007 & nm$baja==2021)
sel.tmp <- c(sel.tmp, which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 & 
                            nm$action=="split.from" & nm$action2=="split.to" & nm$dready2est==0 & nm$alta==2009 & nm$baja==2022))
sel.tmp <- c(sel.tmp, which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 & 
                            nm$action=="split.from" & nm$action2=="split.to" & nm$dready2est==0 & nm$alta==2009 & is.na(nm$baja)))
sel.tmp <- c(sel.tmp, which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 & 
                            nm$action=="split.from" & (nm$alta>=2007 & nm$alta<=2011) & nm$dready2est==0 & nm$ddone==0))
table(nm$action3[sel.tmp])
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
nm[sel.tmp[1:10], c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
table(is.na(nm$p18_2010[sel.tmp]))
table(is.na(nm$p18_2020[sel.tmp]))
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2005 <- prj1020(nm[sel.tmp,], 2005)
    ## p18_2009 <- prj1020(nm[sel.tmp,], 2009)
    ## p18_2012 <- prj1020(nm[sel.tmp,], 2012)
    ## p18_2015 <- prj1020(nm[sel.tmp,], 2015)
    ## p18_2018 <- prj1020(nm[sel.tmp,], 2018)
    ## p18_2021 <- prj1020(nm[sel.tmp,], 2021)
    dready2est <- 1
    nmanip <- nmanip + 1
    action  <- "";      orig.dest  <- "";         when  <- NA
    action2 <- "";      orig.dest2 <- "";         when2 <- NA
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
## Needs sum.split()
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 & 
                 nm$action=="split.from" & nm$action2=="split.to" & nm$alta==2002 & nm$baja==2010)
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    dneedsum <- 1
})
## Flat 2020 from alta to baja
sel.tmp <- which(nm$action=="split.from" & nm$action2=="split.to" & nm$dready2est==0 & nm$ddone ==0 & nm$alta==2014)
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2005 <- p18_2010 <- p18_2020
    #p18_2015 <- p18_2018 <- p18_2021 <- p18_2020
    dready2est <- 1
    nmanip <- nmanip + 1
    action  <- action2; orig.dest  <- orig.dest2; when  <- when2
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
## ## Flat 2020 from alta on
## sel.tmp <- which(nm$seccion==143645)
## nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
## nm[sel.tmp,] <- within(nm[sel.tmp,], {
##     p18_2021 <- p18_2020
##     ddone <- 1
##     nmanip <- nmanip + 1
##     action  <- action2; orig.dest  <- orig.dest2; when  <- when2
##     action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
##     action3 <- "";      orig.dest3 <- "";         when3 <- NA
## })
## Flat 2020 from alta on
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 & 
                 nm$action=="split.from" & (nm$alta==2016|nm$alta==2017))
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2005 <- p18_2010 <- p18_2020
    #p18_2018 <- p18_2021 <- p18_2020
    dready2est <- 1
    nmanip <- nmanip + 1
    action  <- action2; orig.dest  <- orig.dest2; when  <- when2
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
## Flat 2020 from alta on
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 & 
                 nm$action=="split.from" & (nm$alta==2013|nm$alta==2014|nm$alta==2015) & nm$dready2est==0 & nm$ddone ==0)
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2005 <- p18_2010 <- p18_2020
    #p18_2015 <- p18_2018 <- p18_2021 <- p18_2020
    dready2est <- 1
    nmanip <- nmanip + 1
    action  <- action2; orig.dest  <- orig.dest2; when  <- when2
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
## Flat 2020 from alta on
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 & 
                 nm$action=="split.from" & nm$alta==2012 & nm$dready2est==0 & nm$ddone ==0)
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2005 <- p18_2010 <- p18_2020
    #p18_2012 <- p18_2015 <- p18_2018 <- p18_2021 <- p18_2020
    dready2est <- 1
    nmanip <- nmanip + 1
    action  <- action2; orig.dest  <- orig.dest2; when  <- when2
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
##
## what is left to manipulate?
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0)
table(action =nm$action [sel.tmp], action2=nm$action2[sel.tmp])
table(action2=nm$action2[sel.tmp], action3=nm$action3[sel.tmp])
## Remainder split.froms
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 &
                 nm$action=="split.from")
table(nm$when[sel.tmp], useNA = "ifany")
## Nothing to learn from split.from, drop label
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 &
                 nm$action=="split.from" & nm$when<=2005)
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    action  <- action2; orig.dest  <- orig.dest2; when  <- when2
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
## Verify only split.to cases remain
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0)
table(action =nm$action [sel.tmp], action2=nm$action2[sel.tmp])
table(action2=nm$action2[sel.tmp], action3=nm$action3[sel.tmp])
##
## action3 case needs sum.split()
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 &
                 nm$action3=="split.to")
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
##nm$dneedsum[sel.tmp] <- 1
## action 2 cases
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 &
                 nm$action2=="split.to")
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
## no info
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 &
                 nm$action2=="split.to" & nm$alta==1993 & nm$baja==2005)
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
##nm$dskip[sel.tmp] <- 1
## gets linear 2005-2010 for 2003:2012, but flat before 2003
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 &
                 nm$action2=="split.to" & nm$alta==1993 & nm$baja==2012)
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
## nm[sel.tmp,] <- within(nm[sel.tmp,], {
##     p18_2003 <- prj0510(nm[sel.tmp,], 2003)
##     p18_2006 <- prj0510(nm[sel.tmp,], 2006)
##     p18_2009 <- prj0510(nm[sel.tmp,], 2009)
##     p18_2012 <- prj0510(nm[sel.tmp,], 2012)
## })
## nm[sel.tmp,] <- within(nm[sel.tmp,], {
##     p18_1994 <- p18_1997 <- p18_2000 <- p18_2003 
##     ddone <- 1
##     nmanip <- nmanip + 1
##     action  <- "";      orig.dest  <- "";         when  <- NA
##     action2 <- "";      orig.dest2 <- "";         when2 <- NA
##     action3 <- "";      orig.dest3 <- "";         when3 <- NA
## })
## gets linear 2005-2010 for 2003:2020, but flat before 2003
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 &
                 nm$action2=="split.to" & nm$alta==1993 & nm$baja==2020)
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
## nm[sel.tmp,] <- within(nm[sel.tmp,], {
##     p18_2003 <- prj0510(nm[sel.tmp,], 2003)
##     p18_2006 <- prj0510(nm[sel.tmp,], 2006)
##     p18_2009 <- prj0510(nm[sel.tmp,], 2009)
##     p18_2012 <- prj0510(nm[sel.tmp,], 2012)
##     p18_2015 <- prj0510(nm[sel.tmp,], 2015)
##     p18_2018 <- prj0510(nm[sel.tmp,], 2018)
## })
## nm[sel.tmp,] <- within(nm[sel.tmp,], {
##     p18_1994 <- p18_1997 <- p18_2000 <- p18_2003 
##     ddone <- 1
##     nmanip <- nmanip + 1
##     action  <- "";      orig.dest  <- "";         when  <- NA
##     action2 <- "";      orig.dest2 <- "";         when2 <- NA
##     action3 <- "";      orig.dest3 <- "";         when3 <- NA
## })
## Need sum.split()
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 &
                 nm$action2=="split.to" & nm$alta==1993 & is.na(nm$baja) & nm$when2==2021)
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
## nm$dneedsum[sel.tmp] <- 1
## resecc took place before 2005, ready for estimation
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 &
                 nm$action2=="split.to" & nm$alta==1993 & is.na(nm$baja) & nm$when2==2005)
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
## nm[sel.tmp,] <- within(nm[sel.tmp,], {
##     dready2est[sel.tmp] <- 1
##     action  <- "";      orig.dest  <- "";         when  <- NA
##     action2 <- "";      orig.dest2 <- "";         when2 <- NA
##     action3 <- "";      orig.dest3 <- "";         when3 <- NA
## })
## what is left to manipulate?
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0)
table(action =nm$action [sel.tmp], action2=nm$action2[sel.tmp])
table(action2=nm$action2[sel.tmp], action3=nm$action3[sel.tmp])
## needs 2020 sum
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 &
                 nm$action2=="split.to")
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
##nm$dneedsum[sel.tmp] <- 1
## these are ready for estimation 
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 &
                 nm$when<=2005 & is.na(nm$baja))
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm$dready2est[sel.tmp] <- 1
## no census info
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 &
                 nm$when<=2005 & !is.na(nm$baja))
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm$dskip[sel.tmp] <- 1
## need sum
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 &
                 nm$when>=2007 & nm$when<=2010 & is.na(nm$baja))
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
##nm$dneedsum[sel.tmp] <- 1
## flat up to baja
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 &
                 nm$when>=2007 & nm$when<=2010 & !is.na(nm$baja))
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2020 <- p18_2010 <- p18_2005
    #p18_1994 <- p18_1997 <- p18_2000 <- p18_2003 <- p18_2006 <- p18_2009 <- p18_2005
    dready2est <- 1
    action  <- action2; orig.dest  <- orig.dest2; when  <- when2
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
## need sum
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 &
                 nm$when>=2011 & nm$when<=2019 & is.na(nm$baja))
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm$dneedsum[sel.tmp] <- 1
## linear 2005-2010 2005:on, flat 2005 before
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 &
                 nm$when>=2011 & nm$when<=2019 & !is.na(nm$baja))
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2020 <- prj0510(nm[sel.tmp,], 2020)
    ## p18_2006 <- prj0510(nm[sel.tmp,], 2006)
    ## p18_2009 <- prj0510(nm[sel.tmp,], 2009)
    ## p18_2012 <- prj0510(nm[sel.tmp,], 2012)
    dready2est <- 1
    nmanip <- nmanip + 1
    action  <- "";      orig.dest  <- "";         when  <- NA
    action2 <- "";      orig.dest2 <- "";         when2 <- NA
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
## nm[sel.tmp,] <- within(nm[sel.tmp,], {
##     p18_1994 <- p18_1997 <- p18_2000 <- p18_2003  <- p18_2005
## })
## need sum
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 &
                 nm$when==2020)
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm$dneedsum[sel.tmp] <- 1
## These split after 2020 census will get log proj 
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 &
                 nm$when>2020)
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm$dready2est[sel.tmp] <- 1
## These have missing 2005, use linear
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 &
                 nm$action=="" & is.na(nm$p18_2005))
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2005 <- prj1020(nm[sel.tmp,], 2005)
    ## p18_1994 <- prj1020(nm[sel.tmp,], 1994)
    ## p18_1997 <- prj1020(nm[sel.tmp,], 1997)
    ## p18_2000 <- prj1020(nm[sel.tmp,], 2000)
    ## p18_2003 <- prj1020(nm[sel.tmp,], 2003)
    ## p18_2006 <- prj1020(nm[sel.tmp,], 2006)
    ## p18_2009 <- prj1020(nm[sel.tmp,], 2009)
    ## p18_2012 <- prj1020(nm[sel.tmp,], 2012)
    ## p18_2015 <- prj1020(nm[sel.tmp,], 2015)
    ## p18_2018 <- prj1020(nm[sel.tmp,], 2018)
    ## p18_2021 <- prj1020(nm[sel.tmp,], 2021)
    dready2est <- 1
    nmanip <- nmanip + 1
    action  <- "";      orig.dest  <- "";         when  <- NA
    action2 <- "";      orig.dest2 <- "";         when2 <- NA
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
## Case has zero pop 05 and 20 but not 10, fix arbitrarily
sel.tmp <- which(nm$seccion==200457)
show(nm[,c("seccion","inegi","p18_2005","p18_2010","p18_2020")], rows=sel.tmp, after=3, before=3)
nm$p18_2005[sel.tmp] <- 500
nm$p18_2020[sel.tmp] <- 800 
nm$nmanip[sel.tmp] <- nm$nmanip[sel.tmp] + 1
##
sel.tmp <- which(nm$seccion==200802)
show(nm[,c("seccion","inegi","p18_2005","p18_2010","p18_2020")], rows=sel.tmp, after=2, before=2)
nm$p18_2020[sel.tmp] <- 100
nm$nmanip[sel.tmp] <- nm$nmanip[sel.tmp] + 1
##
sel.tmp <- which(nm$seccion==201305)
show(nm[,c("seccion","inegi","p18_2005","p18_2010","p18_2020")], rows=sel.tmp, after=2, before=2)
nm$p18_2020[sel.tmp] <- 500
nm$nmanip[sel.tmp] <- nm$nmanip[sel.tmp] + 1
##
## These have zero pop 05-10-20
sel.tmp <- which(nm$seccion %in% c(71529, 122374, 151965, 152035, 152710, 152712, 152713, 152714, 152715, 152718, 152719, 152720, 152722, 230450, 290526))
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    dskip <- 1
    dready2est <- 0
})
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
## Nothing to manipulate left?
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0)
table(is.na(nm$p18_2005[sel.tmp]))
table(is.na(nm$p18_2010[sel.tmp]))
table(is.na(nm$p18_2020[sel.tmp]))
table(      nm$p18_2005[sel.tmp]==0)
table(      nm$p18_2010[sel.tmp]==0)
table(      nm$p18_2020[sel.tmp]==0)
##table(action =nm$when [sel.tmp])
## indicate cases wo reseccionamiento and full census
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 &
                 nm$action=="" & !is.na(nm$p18_2005) & !is.na(nm$p18_2010) & !is.na(nm$p18_2020))
nm$dready2est[sel.tmp] <- 1
##
## This needs 2005 manip (will get 2020 split.sum)
sel.r <- which(nm$seccion==192305)
nm[sel.r, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip","dneedsum")]
nm$p18_2005[sel.r] <- nm$p18_2010[sel.r]
nm$nmanip[sel.r] <- nm$nmanip[sel.r] + 1
##
## Make 2010 census linear with 2005 and 2020
sel.tmp <- which(nm$seccion %in% c(160829, 303230, 50718, 71879, 200221, 230616, 71640, 71800, 260645, 281040, 281061, 281063, 51437, 71739, 71773, 71832, 71834, 281041, 51049, 71638, 71742, 71744, 71771, 71841, 71850, 71856, 71916, 91734, 94990, 101214, 130585, 153849, 154024, 154393, 156019, 201813, 210025, 250362, 250416, 260306, 260307, 260643, 281042, 281062, 281068, 290490, 300090, 300697, 300715, 302274, 302456, 302625, 302839, 303187, 20554, 21386, 30352, 50442, 50717, 51105, 51110, 51271, 51276, 51305, 51309, 51316, 51396, 71443, 71602, 71829, 71831, 71843, 71848, 71878, 82253, 82842, 91279, 112059, 120444, 121658, 122149, 122777, 130022, 130218, 130339, 130395, 130800, 131083, 131446, 131559, 142273, 142729, 143164, 150080, 150101, 150129, 150147, 150485, 150901, 152000, 152349, 152394, 152770, 152787, 154025, 154195, 154416, 154544, 155752, 155757, 155926, 156014, 156143, 160077, 160149, 160827, 160866, 170353, 170377, 170596, 180295, 180418, 180741, 190193, 190194, 191192, 191236, 200842, 201172, 211564, 211618, 211647, 211648, 211913, 211915, 212222, 220554, 241447, 241650, 250632, 250995, 251293, 251603, 251767, 251844, 251845, 251852, 252644, 252960, 253007, 253041, 253074, 260081, 260082, 260160, 260305, 260760, 261110, 261325, 270580, 270660, 270665, 280785, 281038, 281057, 281064, 300409, 301223, 301524, 301777, 302445, 302477, 302626, 302816, 302863, 303232, 303233, 303297, 303451, 310992, 321035))
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2010 <- prj0510(nm[sel.tmp,], 2006)
    nmanip <- nmanip + 1
})
#####################
## Block ends here ##
#####################

## Check that dummies are mutually exclusive and exhaustive. if so, any single dummy selects ok
table(skip=nm$dskip,    done= nm$ddone)
table(skip=nm$dskip,    sum=  nm$dneedsum)
table(skip=nm$dskip,    ready=nm$dready2est)
table(done=nm$ddone,    sum=  nm$dneedsum)
table(done=nm$ddone,    ready=nm$dready2est)
table(sum= nm$dneedsum, ready=nm$dready2est)
sel.tmp <- which(nm$dskip==0 & nm$ddone==0 & nm$dneedsum==0 & nm$dready2est==0); length(sel.tmp) ## exhaustive


########################################
## Indicate 1st seccion in each state ##
########################################
nm$dfirst <- 0
nm <- nm[order(nm$ord),] ## sort
nm$dfirst[duplicated(nm$edon)==FALSE] <- 1
table(nm$dfirst) # --> there should be 32 ones if all ok


#############################################################
## Manipulate split.to secciones with sum.split() function ##
#############################################################
## Duplicate nm to restore factual census quantities after sum.splits()
sel.c <- grep("seccion|^p18_(2005|2010|2020)", colnames(nm))
nm.saved <- nm[, sel.c]
drestore <- nm.saved; drestore[,-1] <- 0 # indicate cases that need to be restore here
##
## Target cases
sel.tmp <- which(nm$dneedsum==1)
with(nm[sel.tmp,], table(alta=alta, baja =baja,  useNA = "ifany"))
with(nm[sel.tmp,], table(alta=alta, when3=when3, useNA = "ifany"))
with(nm[sel.tmp,], table(alta=alta, when2=when2, useNA = "ifany"))
with(nm[sel.tmp,], table(alta=alta, when =when , useNA = "ifany"))
##
################
## 2020 cases ##
################
sel.tmp <-  which(nm$dneedsum==1 &                                (nm$baja==2020|is.na(nm$baja)))
sel.tmp2 <- which(nm$dneedsum==1 & (nm$alta==2007|nm$alta==2009) & nm$baja==2020) # subset of above needs linear
nm[sel.tmp,  c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm[sel.tmp2, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
##
nm.w <- nm[sel.tmp,]                              # subset cases to manipulate
##nm.w[, grep("p18_(2005|2010|2020)", colnames(nm))] # debug
nm.w <- sum.split(d=nm.w, year.var=2020, rnd=1, dfull=nm) # add-up pre-split population
nm.w[,  c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
drestore$p18_2020[sel.tmp] <- 1                 # indicate cases where counterfactual pop pasted
##
## Cases that need census linear projections
## Flat 2020
sel.tmp3 <- which(nm.w$alta>=2010)
nm.w[sel.tmp3,  c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm.w$p18_2005[sel.tmp3] <- nm.w$p18_2010[sel.tmp3] <- nm.w$p18_2020[sel.tmp3]
## Missing censo05, lineproj
sel.tmp3 <- which(is.na(nm.w$p18_2005)==TRUE)
nm.w[sel.tmp3,  c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm.w$p18_2005[sel.tmp3] <- prj1020(nm[sel.tmp3,], 2005)
rm(sel.tmp3)
##
nm.w$dready2est <- 1; nm.w$dneedsum <- 0        # indicate manipulation
nm.w$nmanip <- nm.w$nmanip + 1
nm.w -> nm[sel.tmp,]                            # Return manipulated obs to nm
##
## needs sum.split(2010) for linear proj
sel.tmp <- which(nm$dneedsum==1 & nm$alta==2002 & nm$baja==2010)
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm.w <- nm[sel.tmp,]                              # subset cases to manipulate
##nm.w[, grep("p18_(2003|2005|2010|2020)", colnames(nm))] # debug
nm.w <- sum.split(d=nm.w, year.var=2010, rnd=2, dfull=nm) # add-up pre-split population
nm.w <- within(nm.w, {
    p18_2020 <- prj0510(nm.w, 2020)
    dneedsum <- 0
    dready2est <- 1
    nmanip <- nmanip + 1
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
drestore$p18_2010[sel.tmp] <- 1                 # indicate cases where counterfactual pop pasted
nm.w -> nm[sel.tmp,]                            # Return manipulated obs to nm
## needs sum.split(2020) for linear proj
sel.tmp <- which(nm$dneedsum==1 & nm$alta==2009 & nm$baja==2019)
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm.w <- nm[sel.tmp,]                              # subset cases to manipulate
##nm.w[, grep("p18_(2003|2005|2010|2020)", colnames(nm))] # debug
nm.w <- sum.split(d=nm.w, year.var=2020, rnd=1, dfull=nm) # add-up pre-split population
nm.w <- within(nm.w, {
    p18_2020 <- prj1020(nm.w, 2020)
    dneedsum <- 0
    dready2est <- 1
    nmanip <- nmanip + 1
    action  <- action2; orig.dest  <- orig.dest2; when  <- when2
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
drestore$p18_2020[sel.tmp] <- 1                 # indicate cases where counterfactual pop pasted
nm.w -> nm[sel.tmp,]                            # Return manipulated obs to nm

## Check 2005-2010-2020 complete
sel.tmp <- which(nm$dready2est==1)
table(c05na=is.na(nm$p18_2005), ready=nm$dready2est)
table(c10na=is.na(nm$p18_2010), ready=nm$dready2est)
table(c20na=is.na(nm$p18_2020), ready=nm$dready2est)

#####################################
## Change NAs to 0s in census data ##
#####################################
sel.tmp <- which(nm$dskip==1)
table(c05=is.na(nm$p18_2005[sel.tmp]))
table(c10=is.na(nm$p18_2010[sel.tmp]))
table(c20=is.na(nm$p18_2020[sel.tmp]))
tmp <- nm[sel.tmp, c("p18_2005","p18_2010","p18_2020")]
tmp[is.na(tmp)] <- 0
tmp -> nm[sel.tmp, c("p18_2005","p18_2010","p18_2020")]


########################
## Verify census pops ##
########################
sel.tmp <- which(nm$dready2est==1 & nm$p18_2005==0 & nm$p18_2010==0 & nm$p18_2020==0)
nm$seccion[sel.tmp]
##
table(nm[nm$dready2est==1, c("p18_2005","p18_2010","p18_2020")] <= 0)
nm[which(nm$dready2est==1 & nm$p18_2005 <= 0), c("p18_2005","p18_2010","p18_2020")]
nm[which(nm$dready2est==1 & nm$p18_2010 <= 0), c("p18_2005","p18_2010","p18_2020")]
nm[which(nm$dready2est==1 & nm$p18_2020 <= 0), c("p18_2005","p18_2010","p18_2020")]
## set arbitrarilly to 50% of 2010 pop
nm$p18_2005 [which(nm$dready2est==1 & nm$p18_2005 <= 0)] <- nm$p18_2010 [which(nm$dready2est==1 & nm$p18_2005 <= 0)] / 2
#nm$p18_2010 [which(nm$dready2est==1 & nm$p18_2010 <= 0)] <- 3
nm$p18_2020 [which(nm$dready2est==1 & nm$p18_2020 <= 0)] <- nm$p18_2010 [which(nm$dready2est==1 & nm$p18_2020 <= 0)] / 2


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
sel.c <- c("p18e_05", "p18e_10", "p18e_20")                   ## select state pop columns
nm <- my_agg(d=nm, sel.c=sel.c, by="edon", drop.dupli=FALSE) ## and sum-up its secciones
nm[1:3,]
## nm <- my_agg(d=nm, sel.c=sel.c, by="inegi", drop.dupli=FALSE) ## and sum-up its secciones


###########################################################################
## Bring mun pops from saved censos to convert sh.hats back into nm.hats ##
###########################################################################
tmp <- censom ## censom has mu-level pops manipulated for remunicipalización
sel.c <- paste0("p18_", seq(1991,2021,3))
tmp <- my_agg(d=tmp, sel.c=sel.c, by="edon", drop.dupli=TRUE) ## and sum-up state pops for secciones
dim(tmp)
colnames(tmp) <- sub("p18_", "p18e_", colnames(tmp)) ## rename mun pop vars
##
## ## This commented block would rely on counterfactual municipio map populations instead of censom 
## #############################################################################
## ## Project mun pops from saved censos to convert sh.hats back into nm.hats ##
## #############################################################################
## tmp <- censom21 # use latest municipio map
## tmp[1,]
## prj <- function(x=NA,yr=NA){
##     chg <- (x$p18_1995 - x$p18_1990) /5 # yearly pop change
##     pop <- x$p18_1990 + chg * (yr - 1990)
##     return(pop)
## }
## tmp <- within(tmp, {
##     p18_1994 <- prj(tmp, 1994)
## })
## prj <- function(x=NA,yr=NA){
##     chg <- (x$p18_2000 - x$p18_1995) /5 # yearly pop change
##     pop <- x$p18_1995 + chg * (yr - 1995)
##     return(pop)
## }
## tmp <- within(tmp, {
##     p18_1997 <- prj(tmp, 1997)
## })
## prj <- function(x=NA,yr=NA){
##     chg <- (x$p18_2005 - x$p18_2000) /5 # yearly pop change
##     pop <- x$p18_2000 + chg * (yr - 2000)
##     return(pop)
## }
## tmp <- within(tmp, {
##     p18_2003 <- prj(tmp, 2003)
## })
## prj <- function(x=NA,yr=NA){
##     chg <- (x$p18_2010 - x$p18_2005) /5 # yearly pop change
##     pop <- x$p18_2005 + chg * (yr - 2005)
##     return(pop)
## }
## tmp <- within(tmp, {
##     p18_2006 <- prj(tmp, 2006)
##     p18_2009 <- prj(tmp, 2009)
## })
## prj <- function(x=NA,yr=NA){
##     chg <- (x$p18_2020 - x$p18_2010) /10 # yearly pop change
##     pop <- x$p18_2010 + chg * (yr - 2010)
##     return(pop)
## }
## tmp <- within(tmp, {
##     p18_2012 <- prj(tmp, 2012)
##     p18_2015 <- prj(tmp, 2015)
##     p18_2018 <- prj(tmp, 2018)
##     p18_2021 <- prj(tmp, 2021)
## })
## tmp <- tmp[,order(colnames(tmp))]
## tmp$p18_1990 <- tmp$p18_1995 <- tmp$edon <- tmp$mun <- tmp$ife <- NULL
## #tmp$p18_2005 <- tmp$p18_2010 <- tmp$p18_2020 <- NULL
## #colnames(tmp) <- sub("^p18_[12][90]", "p18e_", colnames(tmp))
## colnames(tmp) <- sub("^p18_", "p18e_", colnames(tmp))
## dim(tmp)
## tmp[1,]

## pick municipios needed for all secciones and merge saved pops
tmp2 <- data.frame(ord=1:nrow(nm), edon=nm$edon, seccion=nm$seccion)
tmp2 <- split(x=tmp2, f=tmp2$edon) # split into list of data frames, one per state
for (i in 1:32){
    #i <- 1
    tmp3 <- tmp2[[i]] # subset state i's secciones
    ##
    tmp4 <- data.frame(tmp[i, grep("p18e_", colnames(tmp))]) ## take state i's yealry pops
    tmp4 <- tmp4[rep(1, nrow(tmp3)),]                        ## repeat as many times as there are secciones
    ##
    tmp3 <- cbind(tmp3, tmp4)  ## bind pops to state's secciones
    ##
    tmp3 -> tmp2[[i]] # return to data
}
tmp2 <- do.call(rbind, tmp2) # return to data frame form
tmp2 <- tmp2[order(tmp2$ord),]; tmp2$ord <- NULL                  # sort in case order was not preserved
tmp <- tmp2; rm(tmp2)
tmp[1:3,]
## plug el.yr mun pops to nm
tmp2 <- nm
tmp2[1,]
tmp[1,]
table(tmp$seccion==tmp2$seccion) # verify same order and dimensionality
tmp2 <- cbind(tmp2, tmp[,c(-1,-2)])
tmp2[1,]
nm[1,]
table(nm$seccion==tmp2$seccion) # verify same order and dimensionality
nm <- tmp2 ## fill data


eric  x
##################################################################################
## OJO: hay un déficit sistemático en los datos seccionales vis-à-vis el censo: ##
##################################################################################
tmp.ine <- read.csv("/home/eric/Dropbox/data/elecs/MXelsCalendGovt/censos/data/pob18/p18mu-for-municipal-elecs.csv")
tmp.ine <- tmp.ine[,c("edon","inegi","p18_2005","p18_2010","p18_2020")]
tmp.ine <- my_agg(d=tmp.ine, sel.c=c("p18_2005","p18_2010","p18_2020"), by="edon", drop.dupli=TRUE)
##
tmp.cen <- nm[,c("edon","p18_2005","p18_2010","p18_2020")]
tmp.cen <- my_agg(d=tmp.cen, sel.c=c("p18_2005","p18_2010","p18_2020"), by="edon", drop.dupli=TRUE)
ajuste.censal <- data.frame(edon=tmp.cen$edon,
                            d_2005=tmp.cen$p18_2005 - tmp.ine$p18_2005,
                            d_2010=tmp.cen$p18_2010 - tmp.ine$p18_2010,
                            d_2020=tmp.cen$p18_2020 - tmp.ine$p18_2020)
## pct mismatch
round(
    (ajuste.censal[,-1] / tmp.ine[,-c(1,2)]) * 100
)
                             
## ##
## ##############################################################################################################
## ## ¿Explicación? Por ser tan relativamente plano, podría ser gente que no vive en el estado...              ##
## ## Sea lo que sea, por ser plano es fácil ajustar los totales censales con base en las diferencias 05 10 20 ##
## ##############################################################################################################
## tmp.regs <- interlog(what="d", yr=1994, unit="e", frm="log(dv)~iv", census.data=ajuste.censal, digits=1)
## ## ajuste.censal.proj will receive all log-linear predictions
## ajuste.censal.proj <- data.frame(edon  = 1:32, 
##                     d_1994 = tmp.regs[[1]])
## ##non.nas <- apply(ajuste.censal.proj, 1, sum); non.nas <- which(!is.na(non.nas)) # determine in which cases to skip prediction
## new.d <- data.frame(iv=seq(1997,2021,3))     ## prep predictions 1997-on
## preds <- vector(mode='list', nrow(ajuste.censal.proj))    ## empty list
## ## predict
## preds <- lapply(tmp.regs[[3]], function(x) data.frame(dv.hat=predict.lm(x, newdata = new.d)))
## preds <- lapply(preds, function(x) x <- t(x))       # transpose to have yrs in cols
## preds <- do.call(rbind, preds)                      # to data frame
## colnames(preds) <- paste0("d_", seq(1997,2021,3)) # add names
## preds <- exp(preds)                                 # exponentiate log-linear predictions
## ajuste.censal.proj <- cbind(ajuste.censal.proj, preds)                        # consolidate predictions
## ##ajuste.censal.proj <- cbind(ajuste.censal.proj, r.w[, paste0("p18_", c(2005,2010,2020))]) # add census yrs
## ##ajuste.censal.proj <- ajuste.censal.proj[, order(colnames(ajuste.censal.proj))]            # sort columns except 1st (seccion)
## rownames(ajuste.censal.proj) <- NULL
## head(ajuste.censal.proj)
## ##plot(seq(1994, 2021, 3), ajuste.censal.proj[9,-1], ylim = c(0, 40000)) # all looks ok?
## ##
## ## Ajustar datos censales
## tmp2 <- data.frame(ord=1:nrow(nm), edon=nm$edon, seccion=nm$seccion)
## tmp2 <- split(x=tmp2, f=tmp2$edon) # split into list of data frames, one per state
## for (i in 1:32){
##     #i <- 1
##     tmp3 <- tmp2[[i]] # subset state i's secciones
##     ##
##     tmp4 <- data.frame(ajuste.censal.proj[i, grep("d_", colnames(ajuste.censal.proj))]) ## take state i's yealry difs
##     tmp4 <- tmp4[rep(1, nrow(tmp3)),]                        ## repeat as many times as there are secciones
##     ##
##     tmp3 <- cbind(tmp3, tmp4)  ## bind difs to state's secciones
##     ##
##     tmp3 -> tmp2[[i]] # return to data
## }
## tmp2 <- do.call(rbind, tmp2) # return to data frame form
## tmp2 <- tmp2[order(tmp2$ord),]; tmp2$ord <- NULL                  # sort in case order was not preserved
## ajuste.censal.proj.se <- tmp2; rm(tmp2)
## ajuste.censal.proj.se[1:3,]
## ##
## table(ajuste.censal.proj.se$seccion==nm$seccion)
## ##
## nm$p18e_1991 <- NULL  ### Drop 1991 state totals, unneeded until 1991 data available
## ##
## ############################################################################################
## ## Add adjustment to el.yr state census totals                                            ##
## ## Ojo: Need to formalize, I was expecting to subtract ajuste, not sum it.                ##
## ## But sum would increase, not lower, gaps in se-level population gaps with census.ine... ##
## ## WHY?                                                                                   ##
## ############################################################################################
## nm[, grep("p18e_(1994|1997|2000|2003|2006|2009|2012|2015|2018|2021)", colnames(nm))] <-
##     nm                   [, grep("p18e_(1994|1997|2000|2003|2006|2009|2012|2015|2018|2021)", colnames(nm))] +
##     ajuste.censal.proj.se[, grep(   "d_(1994|1997|2000|2003|2006|2009|2012|2015|2018|2021)", colnames(ajuste.censal.proj.se))]



################################
## Add columns for el.yr pops ##
################################
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
##
## Any sh=0 first seccion?
sel.tmp <- which(sh$p18_2005==0 & sh$dfirst==1)
sel.tmp
sel.tmp <- which(sh$p18_2010==0 & sh$dfirst==1)
sel.tmp
sel.tmp <- which(sh$p18_2020==0 & sh$dfirst==1)
sel.tmp
nm[sel.tmp, c("seccion","inegi","p18_2005","p18_2010","p18_2020","p18e_05","p18e_10","p18e_20")]
##


#####################################################
## Prep object to receive all possible regressions ##
#####################################################
regs <- vector(mode='list', length(nrow(nm))) ## empty list
regs[nm$dskip==1] <- "No regression, skipped due to lack of census data"
#regs[nm$ddone==1] <- "No regression, < 3 censuses data points, linear/flat estimates used"
regs[[70977]]


##################################
## Estimate dready2est==1 cases ##
##################################
## Check working cases
table(skip=nm$dskip,    ready=nm$dready2est)
table(done=nm$ddone,    ready=nm$dready2est)
table(sum= nm$dneedsum, ready=nm$dready2est)
sel.tmp <- which(nm$dskip==0 & nm$ddone==0 & nm$dneedsum==0 & nm$dready2est==0); length(sel.tmp) ## exhaustive when equal 0



######################################################
## Check no sección #1 in each municipio has pop>0, ##
## else change index to avoid dividing by zero      ##
######################################################
which(nm$dfirst==1 & nm$p18_2020==0)
show(nm[,grep("ord|seccion|inegi|alta|baja|p18.*_(2005|2010|2020)|ddone|dfirst|dskip", colnames(nm))], 16435,  5, 0)
show(nm[,grep("ord|seccion|inegi|alta|baja|p18.*_(2005|2010|2020)|ddone|dfirst|dskip", colnames(nm))], 25302,  5, 0)
show(nm[,grep("ord|seccion|inegi|alta|baja|p18.*_(2005|2010|2020)|ddone|dfirst|dskip", colnames(nm))], 25926,  5, 0)
show(nm[,grep("ord|seccion|inegi|alta|baja|p18.*_(2005|2010|2020)|ddone|dfirst|dskip", colnames(nm))], 32095,  5, 0)
show(nm[,grep("ord|seccion|inegi|alta|baja|p18.*_(2005|2010|2020)|ddone|dfirst|dskip", colnames(nm))], 42419,  5, 0)
show(nm[,grep("ord|seccion|inegi|alta|baja|p18.*_(2005|2010|2020)|ddone|dfirst|dskip", colnames(nm))], 43224,  5, 0)
show(nm[,grep("ord|seccion|inegi|alta|baja|p18.*_(2005|2010|2020)|ddone|dfirst|dskip", colnames(nm))], 50927,  5, 0)
show(nm[,grep("ord|seccion|inegi|alta|baja|p18.*_(2005|2010|2020)|ddone|dfirst|dskip", colnames(nm))], 57629,  5, 0)
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
}
##
## check again: zeroes?
tmp <- nm[nm$dfirst==1, sel.c]
tmp$dzero <- rowSums(tmp[,-1]==0, na.rm = TRUE)
sum(tmp$dzero) # if >0, manipulate indices


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
with(nm, table(ddone, dneedsum))
nm$dneedsum <- NULL ## No longer needed
with(nm, table(ddone, dskip))
nm[nm$dskip==1, c("p18_2005","p18_2010","p18_2020")]
nm[nm$dskip==1, grep("p18_", colnames(nm))] <- 0 ## Zeroes across the board for these secciones
with(nm, table(dready2est, dfirst))
with(nm, table(ddone, dfirst))




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
tmp.regs <- vector(mode='list', length(nrow(r.w))) ## empty list
tmp.regs <- interlog(what="p18", yr=1994, unit="s", frm="log(dv)~iv", census.data=r.w, digits=6)
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
          sh.w[, grep("^p18e_(1994|1997|2000|2003|2006|2009|2012|2015|2018|2021)", colnames(sh.w))]
tmp.nm <- round(tmp.nm, 1)
tmp.nm[r.w$inegi==1004,]
tmp.nm -> nm.w[, grep( "^p18_(1994|1997|2000|2003|2006|2009|2012|2015|2018|2021)", colnames(nm.w))]
##
## check
sh.w[sh.w$inegi==1004, grep("^p18_", colnames(sh.w))]
nm.w[sh.w$inegi==1004, grep("^p18_", colnames(nm.w))]
head(nm.w[nm.w$inegi==1001, grep("^p18_", colnames(nm.w))])
colSums(sh.w[,               grep("^p18_(1994|1997|2000|2003|2006|2009|2012|2015|2018|2021)", colnames(sh.w))])
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

rm(fun.tmp, sel.tmp, sel.tmp2, tmp, tmp3, tmp4, tmp.cen, tmp.ine, tmp.nm) # clean

## Aggregate municipios from seccion projections
##
## 1994 municipal map
ls()[grep("censo", ls())]
cen.w <- nm     # duplicate for manipulation
sel.c <- colnames(nm) [grep("^p18_", colnames(nm))]                                  # subset censo cols
cen.w <- my_agg(d=cen.w, sel.c=sel.c, by="ife1994", drop.dupli = TRUE)               # perform aggregation
cen.w <- within(cen.w, {ife <- ife1994})                                               # this is the actual municipal code
#sel.c <- grep(pattern="ord|edon|ife$|p18_(1994|1997|2000|2003|2006|2009|2012|2015|2018|2021)", x=colnames(cen.w))
#cen.w <- cen.w[, sel.c]  # keep id and censo cols only
# now paste to v..m objects
v94m   <- merge(x = v94m  , y = cen.w[, c("ife","p18_1994")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v94m)  [grep("p18_", colnames(v94m))]   <- "p18"
v97m94 <- merge(x = v97m94, y = cen.w[, c("ife","p18_1997")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v97m94)[grep("p18_", colnames(v97m94))] <- "p18"
v00m94 <- merge(x = v00m94, y = cen.w[, c("ife","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v00m94)[grep("p18_", colnames(v00m94))] <- "p18"
v03m94 <- merge(x = v03m94, y = cen.w[, c("ife","p18_2003")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v03m94)[grep("p18_", colnames(v03m94))] <- "p18"
v06m94 <- merge(x = v06m94, y = cen.w[, c("ife","p18_2006")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v06m94)[grep("p18_", colnames(v06m94))] <- "p18"
v09m94 <- merge(x = v09m94, y = cen.w[, c("ife","p18_2009")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v09m94)[grep("p18_", colnames(v09m94))] <- "p18"
v12m94 <- merge(x = v12m94, y = cen.w[, c("ife","p18_2012")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v12m94)[grep("p18_", colnames(v12m94))] <- "p18"
v15m94 <- merge(x = v15m94, y = cen.w[, c("ife","p18_2015")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v15m94)[grep("p18_", colnames(v15m94))] <- "p18"
v18m94 <- merge(x = v18m94, y = cen.w[, c("ife","p18_2018")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v18m94)[grep("p18_", colnames(v18m94))] <- "p18"
v21m94 <- merge(x = v21m94, y = cen.w[, c("ife","p18_2021")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v21m94)[grep("p18_", colnames(v21m94))] <- "p18"
rm(cen.w)                                                         # rename
##
## 1997 municipal map
ls()[grep("censo", ls())]
cen.w <- nm     # duplicate for manipulation
sel.c <- colnames(nm) [grep("^p18_", colnames(nm))]                     # subset censo cols
cen.w <- my_agg(d=cen.w, sel.c=sel.c, by="ife1997", drop.dupli = TRUE)  # perform aggregation
cen.w <- within(cen.w, {ife <- ife1997})                                # this is the actual municipal code
# now paste to v..m objects
v94m97 <- merge(x = v94m97, y = cen.w[, c("ife","p18_1994")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v94m97)[grep("p18_", colnames(v94m97))] <- "p18"
v97m   <- merge(x = v97m  , y = cen.w[, c("ife","p18_1997")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v97m)  [grep("p18_", colnames(v97m))]   <- "p18"
v00m97 <- merge(x = v00m97, y = cen.w[, c("ife","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v00m97)[grep("p18_", colnames(v00m97))] <- "p18"
v03m97 <- merge(x = v03m97, y = cen.w[, c("ife","p18_2003")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v03m97)[grep("p18_", colnames(v03m97))] <- "p18"
v06m97 <- merge(x = v06m97, y = cen.w[, c("ife","p18_2006")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v06m97)[grep("p18_", colnames(v06m97))] <- "p18"
v09m97 <- merge(x = v09m97, y = cen.w[, c("ife","p18_2009")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v09m97)[grep("p18_", colnames(v09m97))] <- "p18"
v12m97 <- merge(x = v12m97, y = cen.w[, c("ife","p18_2012")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v12m97)[grep("p18_", colnames(v12m97))] <- "p18"
v15m97 <- merge(x = v15m97, y = cen.w[, c("ife","p18_2015")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v15m97)[grep("p18_", colnames(v15m97))] <- "p18"
v18m97 <- merge(x = v18m97, y = cen.w[, c("ife","p18_2018")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v18m97)[grep("p18_", colnames(v18m97))] <- "p18"
v21m97 <- merge(x = v21m97, y = cen.w[, c("ife","p18_2021")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v21m97)[grep("p18_", colnames(v21m97))] <- "p18"
rm(cen.w)                                                         # rename
##
## 2000 municipal map
ls()[grep("censo", ls())]
cen.w <- nm     # duplicate for manipulation
sel.c <- colnames(nm) [grep("^p18_", colnames(nm))]                     # subset censo cols
cen.w <- my_agg(d=cen.w, sel.c=sel.c, by="ife2000", drop.dupli = TRUE)  # perform aggregation
cen.w <- within(cen.w, {ife <- ife2000})                                # this is the actual municipal code
# now paste to v..m objects
v94m00 <- merge(x = v94m00, y = cen.w[, c("ife","p18_1994")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v94m00)[grep("p18_", colnames(v94m00))] <- "p18"
v97m00 <- merge(x = v97m00, y = cen.w[, c("ife","p18_1997")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v97m00)[grep("p18_", colnames(v97m00))] <- "p18"
v00m   <- merge(x = v00m  , y = cen.w[, c("ife","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v00m)  [grep("p18_", colnames(v00m))]   <- "p18"
v03m00 <- merge(x = v03m00, y = cen.w[, c("ife","p18_2003")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v03m00)[grep("p18_", colnames(v03m00))] <- "p18"
v06m00 <- merge(x = v06m00, y = cen.w[, c("ife","p18_2006")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v06m00)[grep("p18_", colnames(v06m00))] <- "p18"
v09m00 <- merge(x = v09m00, y = cen.w[, c("ife","p18_2009")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v09m00)[grep("p18_", colnames(v09m00))] <- "p18"
v12m00 <- merge(x = v12m00, y = cen.w[, c("ife","p18_2012")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v12m00)[grep("p18_", colnames(v12m00))] <- "p18"
v15m00 <- merge(x = v15m00, y = cen.w[, c("ife","p18_2015")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v15m00)[grep("p18_", colnames(v15m00))] <- "p18"
v18m00 <- merge(x = v18m00, y = cen.w[, c("ife","p18_2018")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v18m00)[grep("p18_", colnames(v18m00))] <- "p18"
v21m00 <- merge(x = v21m00, y = cen.w[, c("ife","p18_2021")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v21m00)[grep("p18_", colnames(v21m00))] <- "p18"
rm(cen.w)                                                         # rename
##
## 2003 municipal map
ls()[grep("censo", ls())]
cen.w <- nm     # duplicate for manipulation
sel.c <- colnames(nm) [grep("^p18_", colnames(nm))]                     # subset censo cols
cen.w <- my_agg(d=cen.w, sel.c=sel.c, by="ife2003", drop.dupli = TRUE)  # perform aggregation
cen.w <- within(cen.w, {ife <- ife2003})                                # this is the actual municipal code
# now paste to v..m objects
v94m03 <- merge(x = v94m03, y = cen.w[, c("ife","p18_1994")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v94m03)[grep("p18_", colnames(v94m03))] <- "p18"
v97m03 <- merge(x = v97m03, y = cen.w[, c("ife","p18_1997")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v97m03)[grep("p18_", colnames(v97m03))] <- "p18"
v00m03 <- merge(x = v00m03, y = cen.w[, c("ife","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v00m03)[grep("p18_", colnames(v00m03))] <- "p18"
v03m   <- merge(x = v03m  , y = cen.w[, c("ife","p18_2003")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v03m)  [grep("p18_", colnames(v03m))]   <- "p18"
v06m03 <- merge(x = v06m03, y = cen.w[, c("ife","p18_2006")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v06m03)[grep("p18_", colnames(v06m03))] <- "p18"
v09m03 <- merge(x = v09m03, y = cen.w[, c("ife","p18_2009")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v09m03)[grep("p18_", colnames(v09m03))] <- "p18"
v12m03 <- merge(x = v12m03, y = cen.w[, c("ife","p18_2012")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v12m03)[grep("p18_", colnames(v12m03))] <- "p18"
v15m03 <- merge(x = v15m03, y = cen.w[, c("ife","p18_2015")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v15m03)[grep("p18_", colnames(v15m03))] <- "p18"
v18m03 <- merge(x = v18m03, y = cen.w[, c("ife","p18_2018")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v18m03)[grep("p18_", colnames(v18m03))] <- "p18"
v21m03 <- merge(x = v21m03, y = cen.w[, c("ife","p18_2021")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v21m03)[grep("p18_", colnames(v21m03))] <- "p18"
rm(cen.w)                                                         # rename
##
## 2006 municipal map
ls()[grep("censo", ls())]
cen.w <- nm     # duplicate for manipulation
sel.c <- colnames(nm) [grep("^p18_", colnames(nm))]                     # subset censo cols
cen.w <- my_agg(d=cen.w, sel.c=sel.c, by="ife2006", drop.dupli = TRUE)  # perform aggregation
cen.w <- within(cen.w, {ife <- ife2006})                                # this is the actual municipal code
# now paste to v..m objects
v94m06 <- merge(x = v94m06, y = cen.w[, c("ife","p18_1994")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v94m06)[grep("p18_", colnames(v94m06))] <- "p18"
v97m06 <- merge(x = v97m06, y = cen.w[, c("ife","p18_1997")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v97m06)[grep("p18_", colnames(v97m06))] <- "p18"
v00m06 <- merge(x = v00m06, y = cen.w[, c("ife","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v00m06)[grep("p18_", colnames(v00m06))] <- "p18"
v03m06 <- merge(x = v03m06, y = cen.w[, c("ife","p18_2003")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v03m06)[grep("p18_", colnames(v03m06))] <- "p18"
v06m   <- merge(x = v06m  , y = cen.w[, c("ife","p18_2006")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v06m)  [grep("p18_", colnames(v06m))]   <- "p18"
v09m06 <- merge(x = v09m06, y = cen.w[, c("ife","p18_2009")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v09m06)[grep("p18_", colnames(v09m06))] <- "p18"
v12m06 <- merge(x = v12m06, y = cen.w[, c("ife","p18_2012")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v12m06)[grep("p18_", colnames(v12m06))] <- "p18"
v15m06 <- merge(x = v15m06, y = cen.w[, c("ife","p18_2015")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v15m06)[grep("p18_", colnames(v15m06))] <- "p18"
v18m06 <- merge(x = v18m06, y = cen.w[, c("ife","p18_2018")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v18m06)[grep("p18_", colnames(v18m06))] <- "p18"
v21m06 <- merge(x = v21m06, y = cen.w[, c("ife","p18_2021")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v21m06)[grep("p18_", colnames(v21m06))] <- "p18"
rm(cen.w)                                                         # rename
##
## 2009 municipal map
ls()[grep("censo", ls())]
cen.w <- nm     # duplicate for manipulation
sel.c <- colnames(nm) [grep("^p18_", colnames(nm))]                     # subset censo cols
cen.w <- my_agg(d=cen.w, sel.c=sel.c, by="ife2009", drop.dupli = TRUE)  # perform aggregation
cen.w <- within(cen.w, {ife <- ife2009})                                # this is the actual municipal code
# now paste to v..m objects
v94m09 <- merge(x = v94m09, y = cen.w[, c("ife","p18_1994")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v94m09)[grep("p18_", colnames(v94m09))] <- "p18"
v97m09 <- merge(x = v97m09, y = cen.w[, c("ife","p18_1997")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v97m09)[grep("p18_", colnames(v97m09))] <- "p18"
v00m09 <- merge(x = v00m09, y = cen.w[, c("ife","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v00m09)[grep("p18_", colnames(v00m09))] <- "p18"
v03m09 <- merge(x = v03m09, y = cen.w[, c("ife","p18_2003")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v03m09)[grep("p18_", colnames(v03m09))] <- "p18"
v06m09 <- merge(x = v06m09, y = cen.w[, c("ife","p18_2006")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v06m09)[grep("p18_", colnames(v06m09))] <- "p18"
v09m   <- merge(x = v09m  , y = cen.w[, c("ife","p18_2009")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v09m)  [grep("p18_", colnames(v09m))]   <- "p18"
v12m09 <- merge(x = v12m09, y = cen.w[, c("ife","p18_2012")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v12m09)[grep("p18_", colnames(v12m09))] <- "p18"
v15m09 <- merge(x = v15m09, y = cen.w[, c("ife","p18_2015")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v15m09)[grep("p18_", colnames(v15m09))] <- "p18"
v18m09 <- merge(x = v18m09, y = cen.w[, c("ife","p18_2018")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v18m09)[grep("p18_", colnames(v18m09))] <- "p18"
v21m09 <- merge(x = v21m09, y = cen.w[, c("ife","p18_2021")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v21m09)[grep("p18_", colnames(v21m09))] <- "p18"
rm(cen.w)                                                         # rename
##
## 2012 municipal map
ls()[grep("censo", ls())]
cen.w <- nm     # duplicate for manipulation
sel.c <- colnames(nm) [grep("^p18_", colnames(nm))]                     # subset censo cols
cen.w <- my_agg(d=cen.w, sel.c=sel.c, by="ife2012", drop.dupli = TRUE)  # perform aggregation
cen.w <- within(cen.w, {ife <- ife2012})                                # this is the actual municipal code
# now paste to v..m objects
v94m12 <- merge(x = v94m12, y = cen.w[, c("ife","p18_1994")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v94m12)[grep("p18_", colnames(v94m12))] <- "p18"
v97m12 <- merge(x = v97m12, y = cen.w[, c("ife","p18_1997")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v97m12)[grep("p18_", colnames(v97m12))] <- "p18"
v00m12 <- merge(x = v00m12, y = cen.w[, c("ife","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v00m12)[grep("p18_", colnames(v00m12))] <- "p18"
v03m12 <- merge(x = v03m12, y = cen.w[, c("ife","p18_2003")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v03m12)[grep("p18_", colnames(v03m12))] <- "p18"
v06m12 <- merge(x = v06m12, y = cen.w[, c("ife","p18_2006")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v06m12)[grep("p18_", colnames(v06m12))] <- "p18"
v09m12 <- merge(x = v09m12, y = cen.w[, c("ife","p18_2009")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v09m12)[grep("p18_", colnames(v09m12))] <- "p18"
v12m   <- merge(x = v12m  , y = cen.w[, c("ife","p18_2012")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v12m)  [grep("p18_", colnames(v12m))]   <- "p18"
v15m12 <- merge(x = v15m12, y = cen.w[, c("ife","p18_2015")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v15m12)[grep("p18_", colnames(v15m12))] <- "p18"
v18m12 <- merge(x = v18m12, y = cen.w[, c("ife","p18_2018")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v18m12)[grep("p18_", colnames(v18m12))] <- "p18"
v21m12 <- merge(x = v21m12, y = cen.w[, c("ife","p18_2021")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v21m12)[grep("p18_", colnames(v21m12))] <- "p18"
rm(cen.w)                                                         # rename
##
## 2015 municipal map
ls()[grep("censo", ls())]
cen.w <- nm     # duplicate for manipulation
sel.c <- colnames(nm) [grep("^p18_", colnames(nm))]                     # subset censo cols
cen.w <- my_agg(d=cen.w, sel.c=sel.c, by="ife2015", drop.dupli = TRUE)  # perform aggregation
cen.w <- within(cen.w, {ife <- ife2015})                                # this is the actual municipal code
# now paste to v..m objects
v94m15 <- merge(x = v94m15, y = cen.w[, c("ife","p18_1994")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v94m15)[grep("p18_", colnames(v94m15))] <- "p18"
v97m15 <- merge(x = v97m15, y = cen.w[, c("ife","p18_1997")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v97m15)[grep("p18_", colnames(v97m15))] <- "p18"
v00m15 <- merge(x = v00m15, y = cen.w[, c("ife","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v00m15)[grep("p18_", colnames(v00m15))] <- "p18"
v03m15 <- merge(x = v03m15, y = cen.w[, c("ife","p18_2003")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v03m15)[grep("p18_", colnames(v03m15))] <- "p18"
v06m15 <- merge(x = v06m15, y = cen.w[, c("ife","p18_2006")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v06m15)[grep("p18_", colnames(v06m15))] <- "p18"
v09m15 <- merge(x = v09m15, y = cen.w[, c("ife","p18_2009")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v09m15)[grep("p18_", colnames(v09m15))] <- "p18"
v12m15 <- merge(x = v12m15, y = cen.w[, c("ife","p18_2012")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v12m15)[grep("p18_", colnames(v12m15))] <- "p18"
v15m   <- merge(x = v15m  , y = cen.w[, c("ife","p18_2015")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v15m)  [grep("p18_", colnames(v15m))]   <- "p18"
v18m15 <- merge(x = v18m15, y = cen.w[, c("ife","p18_2018")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v18m15)[grep("p18_", colnames(v18m15))] <- "p18"
v21m15 <- merge(x = v21m15, y = cen.w[, c("ife","p18_2021")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v21m15)[grep("p18_", colnames(v21m15))] <- "p18"
rm(cen.w)                                                         # rename
##
## 2018 municipal map
ls()[grep("censo", ls())]
cen.w <- nm     # duplicate for manipulation
sel.c <- colnames(nm) [grep("^p18_", colnames(nm))]                     # subset censo cols
cen.w <- my_agg(d=cen.w, sel.c=sel.c, by="ife2018", drop.dupli = TRUE)  # perform aggregation
cen.w <- within(cen.w, {ife <- ife2018})                                # this is the actual municipal code
# now paste to v..m objects
v94m18 <- merge(x = v94m18, y = cen.w[, c("ife","p18_1994")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v94m18)[grep("p18_", colnames(v94m18))] <- "p18"
v97m18 <- merge(x = v97m18, y = cen.w[, c("ife","p18_1997")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v97m18)[grep("p18_", colnames(v97m18))] <- "p18"
v00m18 <- merge(x = v00m18, y = cen.w[, c("ife","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v00m18)[grep("p18_", colnames(v00m18))] <- "p18"
v03m18 <- merge(x = v03m18, y = cen.w[, c("ife","p18_2003")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v03m18)[grep("p18_", colnames(v03m18))] <- "p18"
v06m18 <- merge(x = v06m18, y = cen.w[, c("ife","p18_2006")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v06m18)[grep("p18_", colnames(v06m18))] <- "p18"
v09m18 <- merge(x = v09m18, y = cen.w[, c("ife","p18_2009")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v09m18)[grep("p18_", colnames(v09m18))] <- "p18"
v12m18 <- merge(x = v12m18, y = cen.w[, c("ife","p18_2012")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v12m18)[grep("p18_", colnames(v12m18))] <- "p18"
v15m18 <- merge(x = v15m18, y = cen.w[, c("ife","p18_2015")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v15m18)[grep("p18_", colnames(v15m18))] <- "p18"
v18m   <- merge(x = v18m,   y = cen.w[, c("ife","p18_2018")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v18m)  [grep("p18_", colnames(v18m))]   <- "p18"
v21m18 <- merge(x = v21m18, y = cen.w[, c("ife","p18_2021")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v21m18)[grep("p18_", colnames(v21m18))] <- "p18"
rm(cen.w)                                                         # rename
##
## 2021 municipal map
ls()[grep("censo", ls())]
cen.w <- nm     # duplicate for manipulation
sel.c <- colnames(nm) [grep("^p18_", colnames(nm))]                     # subset censo cols
cen.w <- my_agg(d=cen.w, sel.c=sel.c, by="ife2021", drop.dupli = TRUE)  # perform aggregation
cen.w <- within(cen.w, {ife <- ife2021})                                # this is the actual municipal code
# now paste to v..m objects
v94m21 <- merge(x = v94m21, y = cen.w[, c("ife","p18_1994")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v94m21)[grep("p18_", colnames(v94m21))] <- "p18"
v97m21 <- merge(x = v97m21, y = cen.w[, c("ife","p18_1997")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v97m21)[grep("p18_", colnames(v97m21))] <- "p18"
v00m21 <- merge(x = v00m21, y = cen.w[, c("ife","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v00m21)[grep("p18_", colnames(v00m21))] <- "p18"
v03m21 <- merge(x = v03m21, y = cen.w[, c("ife","p18_2003")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v03m21)[grep("p18_", colnames(v03m21))] <- "p18"
v06m21 <- merge(x = v06m21, y = cen.w[, c("ife","p18_2006")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v06m21)[grep("p18_", colnames(v06m21))] <- "p18"
v09m21 <- merge(x = v09m21, y = cen.w[, c("ife","p18_2009")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v09m21)[grep("p18_", colnames(v09m21))] <- "p18"
v12m21 <- merge(x = v12m21, y = cen.w[, c("ife","p18_2012")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v12m21)[grep("p18_", colnames(v12m21))] <- "p18"
v15m21 <- merge(x = v15m21, y = cen.w[, c("ife","p18_2015")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v15m21)[grep("p18_", colnames(v15m21))] <- "p18"
v18m21 <- merge(x = v18m21, y = cen.w[, c("ife","p18_2018")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v18m21)[grep("p18_", colnames(v18m21))] <- "p18"
v21m   <- merge(x = v21m,   y = cen.w[, c("ife","p18_2021")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v21m)  [grep("p18_", colnames(v21m))]   <- "p18"
rm(cen.w)                                                         # rename
##


# compare to mu-censuses
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
        censom21$p18_2005[i],
        censom21$p18_2010[i],
        censom21$p18_2020[i])
    plot(x = seq(1994, 2021, 3),
         y = tmp, ylim = c(0, max(c(tmp,tmp3))), main = v21m$mun[i], pch = 20)
                                        #points(x = seq(1994, 2021, 3), y = tmp2, col = "red", pch = 20)
    points(x = c(2005, 2010, 2020), y = tmp3, col = "blue", pch = 19)
    legend(x = "bottomleft", legend = c("se-by-se compos", "mu-by-mu", "mu censos"), pch = 20, col = c("black","red","blue"))
}

i <- i + 1; nextp()
x

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
