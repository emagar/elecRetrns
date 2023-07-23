## 1. Prep: sum.split <- function(d=censo, year.var=2020, rnd=1)
## 2. Prep: function interlog --- for 3pt log(dv=r)~iv
## 3. Prep: function interpol --- for 2pt segments

##############################################################
## manipulate secciones that suffered some reseccionamiento ##
##############################################################
sum.split <- function(d=censo, year.var=2020, rnd=1) {
    ##
    if (year.var %notin% c(2005,2010,2020)){
        print("Warning: year.var is not a sección-level census year")
        stop
    }
    ##year.var <- 2020 # debug
    ##d <- nm.w # debug
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
        sel.agg <- which(censo$seccion %in% sel.to) ## indices to be summed-up from censo
        ## sum population
        totals <- colSums(censo[sel.agg, sel.col], na.rm = TRUE)
        ## paste them into manipulated sección
        d[i,sel.col] <- totals;
        ##d$dmanip[i] <- 1;  # indicates manipulation
    }
    ##
    ## return manipulated data
    return(d)
}

## get census projection functions
source("../../code/v-hat-etc/interpolate-census-functions.r")

## 4. Duplicate nm=censo, adding cols 1994:2021, m:2005-2010-2020, ddone=0, etc
##    nm = nominal quantities
nm <- censo
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
# add cols that will receive p18 projections for elec yrs
nm <- within(nm, {
    nmanip <- dfirst <- dsingle <- dskip <- dready2est <- ddone <- dneedsum <- 0;
    when3b <- when3; orig.dest3b <- orig.dest3; action3b <- action3; #  backup action/orig.dest/when so that these
    when2b <- when2; orig.dest2b <- orig.dest2; action2b <- action2; #  can be erased once seccion gets manipulated
    whenb  <- when;  orig.destb  <- orig.dest;  actionb  <- action;  #  ---helps debug block immediately below
    p18m_20 <- NA; # yy so grep excludes
    p18m_10 <- NA; # yy so grep excludes
    p18m_05 <- NA; # yy so grep excludes
    p18_1994 <- p18_1997 <- p18_2000 <- p18_2003 <- p18_2006 <- p18_2009 <- p18_2012 <- p18_2015 <- p18_2018 <- p18_2021 <- NA;
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
sel.tmp <- which(nm$action=="merged.to" & (nm$when==2012|nm$when==2014|nm$when==2020) & is.na(nm$p18_2005))
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
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
sel.tmp <- which(nm$action2=="merged.to" & (nm$alta>=1993 & nm$alta<=1996) & (nm$baja==2008|nm$baja==2011) & nm$ddone==0)
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_1994 <- p18_1997 <- p18_2000 <- p18_2003 <- p18_2006 <- p18_2009 <- p18_2010 <- p18_2005
    ddone <- 1
    nmanip <- nmanip + 1
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
## this one gets linear 2005-2010 from alta to baja
sel.tmp <- which(nm$action2=="merged.to" & nm$alta==2002 & nm$baja==2014)
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
prj <- function(x=NA,yr=NA){
    chg <- (x$p18_2010 - x$p18_2005) /5 # yearly pop change
    pop <- x$p18_2005 + chg * (yr - 2005)
    return(pop)
}
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2003 <- prj(nm[sel.tmp,], 2003)
    p18_2006 <- prj(nm[sel.tmp,], 2006)
    p18_2009 <- prj(nm[sel.tmp,], 2009)
    p18_2012 <- prj(nm[sel.tmp,], 2012)
    ddone <- 1
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
    p18_2006 <- p18_2005
    ddone <- 1
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
sel.tmp <- which(nm$action =="merged.to")
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
table(action=nm$action[sel.tmp], action2=nm$action2[sel.tmp], useNA = "ifany")
## These have no info
sel.tmp <- which(nm$action=="merged.to" & nm$alta==1993 & nm$baja==2008 & is.na(nm$p18_2005))
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    dskip <- 1
    action  <- action2; orig.dest  <- orig.dest2; when  <- when2
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
## Fully flat up to baja
sel.tmp <- which(nm$action=="merged.to" & nm$alta==1993 & nm$ddone==0 & (nm$baja>=2008 & nm$baja<=2011))
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2010 <- p18_1994 <- p18_1997 <- p18_2000 <- p18_2003 <- p18_2006 <- p18_2009 <- p18_2005
    ddone <- 1;
    nmanip <- nmanip + 1
    action  <- action2; orig.dest  <- orig.dest2; when  <- when2
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
## No more merged-to remaining
table(nm$action)
table(nm$action2)
table(nm$action3)
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
## Split.from in 2019-2020: fully flat
sel.tmp <- which(nm$action=="split.from" & nm$action2=="" & (nm$when==2019|nm$when==2020))
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
table(is.na(nm$p18_2020[sel.tmp]))
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2021 <- p18_2020
    ddone <- 1
    nmanip <- nmanip + 1
    action  <- action2; orig.dest  <- orig.dest2; when  <- when2
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
## Special handling for these short-lived secciones
sel.tmp <- which(nm$seccion==10487)
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2006 <- p18_2005
    ddone <- 1
    nmanip <- nmanip + 1
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
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
## gets linear 2005-2010 projection across the board
sel.tmp <- which(nm$seccion==155931)
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
prj <- function(x=NA,yr=NA){
    chg <- (x$p18_2010 - x$p18_2005) /5 # yearly pop change
    pop <- x$p18_2005 + chg * (yr - 2005)
    return(pop)
}
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2003 <- prj(nm[sel.tmp,], 2003)
    p18_2006 <- prj(nm[sel.tmp,], 2006)
    p18_2009 <- prj(nm[sel.tmp,], 2009)
    p18_2012 <- prj(nm[sel.tmp,], 2012)
    ddone <- 1
    nmanip <- nmanip + 1
    action  <- "";      orig.dest  <- "";         when  <- NA
    action2 <- "";      orig.dest2 <- "";         when2 <- NA
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
##
## These have 2020 NAs, need sum.split()
sel.tmp <- which(nm$action=="split.from" & nm$action2=="split.to" & nm$dready2est==0 & (nm$alta==2007|nm$alta==2009) & (nm$baja==2019|nm$baja==2020) & is.na(nm$p18_2020))
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
prj <- function(x=NA,yr=NA){
    chg <- (x$p18_2020 - x$p18_2010) / 10 # yearly pop change
    pop <- x$p18_2010 + chg * (yr - 2010)
    return(pop)
}
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2009 <- prj(nm[sel.tmp,], 2009)
    p18_2012 <- prj(nm[sel.tmp,], 2012)
    p18_2015 <- prj(nm[sel.tmp,], 2015)
    p18_2018 <- prj(nm[sel.tmp,], 2018)
    p18_2021 <- prj(nm[sel.tmp,], 2021)
    ddone <- 1
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
    p18_2015 <- p18_2018 <- p18_2021 <- p18_2020
    ddone <- 1
    nmanip <- nmanip + 1
    action  <- action2; orig.dest  <- orig.dest2; when  <- when2
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
## Flat 2020 from alta on
sel.tmp <- which(nm$seccion==143645)
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2021 <- p18_2020
    ddone <- 1
    nmanip <- nmanip + 1
    action  <- action2; orig.dest  <- orig.dest2; when  <- when2
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
## Flat 2020 from alta on
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 & 
                 nm$action=="split.from" & (nm$alta==2016|nm$alta==2017))
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2018 <- p18_2021 <- p18_2020
    ddone <- 1
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
    p18_2015 <- p18_2018 <- p18_2021 <- p18_2020
    ddone <- 1
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
    p18_2012 <- p18_2015 <- p18_2018 <- p18_2021 <- p18_2020
    ddone <- 1
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
nm$dneedsum[sel.tmp] <- 1
## action 2 cases
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 &
                 nm$action2=="split.to")
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
## no info
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 &
                 nm$action2=="split.to" & nm$alta==1993 & nm$baja==2005)
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm$dskip[sel.tmp] <- 1
## gets linear 2005-2010 for 2003:2012, but flat before 2003
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 &
                 nm$action2=="split.to" & nm$alta==1993 & nm$baja==2012)
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
prj <- function(x=NA,yr=NA){
    chg <- (x$p18_2010 - x$p18_2005) /5 # yearly pop change
    pop <- x$p18_2005 + chg * (yr - 2005)
    return(pop)
}
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2003 <- prj(nm[sel.tmp,], 2003)
    p18_2006 <- prj(nm[sel.tmp,], 2006)
    p18_2009 <- prj(nm[sel.tmp,], 2009)
    p18_2012 <- prj(nm[sel.tmp,], 2012)
})
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_1994 <- p18_1997 <- p18_2000 <- p18_2003 
    ddone <- 1
    nmanip <- nmanip + 1
    action  <- "";      orig.dest  <- "";         when  <- NA
    action2 <- "";      orig.dest2 <- "";         when2 <- NA
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
## gets linear 2005-2010 for 2003:2020, but flat before 2003
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 &
                 nm$action2=="split.to" & nm$alta==1993 & nm$baja==2020)
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2003 <- prj(nm[sel.tmp,], 2003)
    p18_2006 <- prj(nm[sel.tmp,], 2006)
    p18_2009 <- prj(nm[sel.tmp,], 2009)
    p18_2012 <- prj(nm[sel.tmp,], 2012)
    p18_2015 <- prj(nm[sel.tmp,], 2015)
    p18_2018 <- prj(nm[sel.tmp,], 2018)
})
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_1994 <- p18_1997 <- p18_2000 <- p18_2003 
    ddone <- 1
    nmanip <- nmanip + 1
    action  <- "";      orig.dest  <- "";         when  <- NA
    action2 <- "";      orig.dest2 <- "";         when2 <- NA
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
## Need sum.split()
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 &
                 nm$action2=="split.to" & nm$alta==1993 & is.na(nm$baja) & nm$when2==2021)
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm$dneedsum[sel.tmp] <- 1
## resecc took place before 2005, ready for estimation
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 &
                 nm$action2=="split.to" & nm$alta==1993 & is.na(nm$baja) & nm$when2==2005)
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    dready2est[sel.tmp] <- 1
    action  <- "";      orig.dest  <- "";         when  <- NA
    action2 <- "";      orig.dest2 <- "";         when2 <- NA
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
## what is left to manipulate?
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0)
table(action =nm$action [sel.tmp], action2=nm$action2[sel.tmp])
table(action2=nm$action2[sel.tmp], action3=nm$action3[sel.tmp])
## needs 2020 sum
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 &
                 nm$action2=="split.to")
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm$dneedsum[sel.tmp] <- 1
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
nm$dneedsum[sel.tmp] <- 1
## flat up to baja
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 &
                 nm$when>=2007 & nm$when<=2010 & !is.na(nm$baja))
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_1994 <- p18_1997 <- p18_2000 <- p18_2003 <- p18_2006 <- p18_2009 <- p18_2005
    ddone <- 1
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
prj <- function(x=NA,yr=NA){
    chg <- (x$p18_2010 - x$p18_2005) /5 # yearly pop change
    pop <- x$p18_2005 + chg * (yr - 2005)
    return(pop)
}
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2006 <- prj(nm[sel.tmp,], 2006)
    p18_2009 <- prj(nm[sel.tmp,], 2009)
    p18_2012 <- prj(nm[sel.tmp,], 2012)
})
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_1994 <- p18_1997 <- p18_2000 <- p18_2003  <- p18_2005
    ddone <- 1
    nmanip <- nmanip + 1
    action  <- "";      orig.dest  <- "";         when  <- NA
    action2 <- "";      orig.dest2 <- "";         when2 <- NA
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
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
prj <- function(x=NA,yr=NA){
    chg <- (x$p18_2020 - x$p18_2010) /10 # yearly pop change
    pop <- x$p18_2010 + chg * (yr - 2010)
    return(pop)
}
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_1994 <- prj(nm[sel.tmp,], 1994)
    p18_1997 <- prj(nm[sel.tmp,], 1997)
    p18_2000 <- prj(nm[sel.tmp,], 2000)
    p18_2003 <- prj(nm[sel.tmp,], 2003)
    p18_2006 <- prj(nm[sel.tmp,], 2006)
    p18_2009 <- prj(nm[sel.tmp,], 2009)
    p18_2012 <- prj(nm[sel.tmp,], 2012)
    p18_2015 <- prj(nm[sel.tmp,], 2015)
    p18_2018 <- prj(nm[sel.tmp,], 2018)
    p18_2021 <- prj(nm[sel.tmp,], 2021)
    ddone <- 1
    nmanip <- nmanip + 1
    action  <- "";      orig.dest  <- "";         when  <- NA
    action2 <- "";      orig.dest2 <- "";         when2 <- NA
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
## Case has zero pop 05 and 20 but not 10, fix arbitrarily
## Function also shows rows above/below numbered rows
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
sel.tmp <- which(nm$seccion==200457)
show(nm[,c("seccion","inegi","p18_2005","p18_2010","p18_2020")], rows=sel.tmp, after=3, before=3)
nm$p18_2005[sel.tmp] <- 500
nm$p18_2020[sel.tmp] <- 800 
nm$nmanip <- nm$nmanip + 1
##
sel.tmp <- which(nm$seccion==200802)
show(nm[,c("seccion","inegi","p18_2005","p18_2010","p18_2020")], rows=sel.tmp, after=2, before=2)
nm$p18_2020[sel.tmp] <- 100
nm$nmanip <- nm$nmanip + 1
##
sel.tmp <- which(nm$seccion==201305)
show(nm[,c("seccion","inegi","p18_2005","p18_2010","p18_2020")], rows=sel.tmp, after=2, before=2)
nm$p18_2020[sel.tmp] <- 500
nm$nmanip <- nm$nmanip + 1
##
## These have zero pop 05-10-20
sel.tmp <- which(nm$seccion %in% c(71529, 122374, 151965, 152035, 152710, 152712, 152713, 152714, 152715, 152718, 152719, 152720, 152722, 230450, 290526))
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
table(nm$p18_2005[sel.tmp]==0)
table(nm$p18_2010[sel.tmp]==0)
table(nm$p18_2020[sel.tmp]==0)
##table(action =nm$when [sel.tmp])
## indicate cases wo reseccionamiento and full census
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0 &
                 nm$action=="" & !is.na(nm$p18_2005) & !is.na(nm$p18_2010) & !is.na(nm$p18_2020))
nm$dready2est[sel.tmp] <- 1
#####################
## Block ends here ##
#####################
##
## Check that dummies are mutually exclusive and exhaustive. if so, single dummy can be used to select 
table(skip=nm$dskip,    done= nm$ddone)
table(skip=nm$dskip,    sum=  nm$dneedsum)
table(skip=nm$dskip,    ready=nm$dready2est)
table(done=nm$ddone,    sum=  nm$dneedsum)
table(done=nm$ddone,    ready=nm$dready2est)
table(sum= nm$dneedsum, ready=nm$dready2est)
sel.tmp <- which(nm$dskip==0 & nm$ddone==0 & nm$dneedsum==0 & nm$dready2est==0); length(sel.tmp) ## exhaustive

####################################################################################
## Apply my_agg to generate municipal aggregates (nm[sel.r,]$m:2005-2010-2020)  ##
####################################################################################
## fill in p18s for aggregation
nm <- within(nm, {
    p18m_05 <- p18_2005;
    p18m_10 <- p18_2010;
    p18m_20 <- p18_2020;
})
sel.c <- c("p18m_05", "p18m_10", "p18m_20")
nm <- my_agg(d=nm, sel.c=sel.c, by="inegi", drop.dupli=FALSE)
##
########################################
## Indicate single-sección municipios ##
########################################
tmp <- split(x=nm, f=nm$inegi)
tmp <- lapply(tmp, function(x){
    n <- nrow(x)
    n <- data.frame(inegi=NA, n=n)
    return(n)
    })
tmp <- do.call(rbind, tmp)
tmp$inegi <- as.numeric(rownames(tmp))
nm$dsingle[which(nm$inegi  %in% tmp$inegi[tmp$n==1])] <- 1
##
############################################
## Indicate 1st seccion in each municipio ##
############################################
nm <- nm[order(nm$ord),] # make sure ord-sorted
tmp <- split(x=nm, f=nm$inegi) # split into list of data frames, one per municipio OJO: uses inegi
tmp.f <- function(x=NA){
    ##x <- tmp[[1005]] # debug
    x$dfirst[1] <- 1            # indicate 1st seccion in municipio
    return(x)
}
tmp <- lapply(tmp, tmp.f) # apply function
tmp <- do.call(rbind, tmp) # return to data frame form
rownames(tmp) <- NULL
nm <- tmp # return manipulated data
##
## sort
table(1:nrow(nm) - nm$ord)
nm <- nm[order(nm$ord),]

#############################################################
## Manipulate split.to secciones with sum.split() function ##
#############################################################
## Duplicate nm to restore factual census quantities after sum.splits()
sel.c <- grep("^p18_(2005|2010|2020)", colnames(nm))
nm.saved <- nm[, sel.c]
drestore <- nm.saved; drestore[] <- 0 # indicate cases that need to be restore here
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
sel.tmp <-  which(nm$dneedsum==1 & (nm$baja==2020|is.na(nm$baja)))
sel.tmp2 <- which(nm$dneedsum==1 & (nm$alta==2007|nm$alta==2009) & nm$baja==2020) # subset of above needs linear
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
##
nm.w <- nm[sel.tmp,]                              # subset cases to manipulate
##nm.w[, grep("p18_(2005|2010|2020)", colnames(nm))] # debug
nm.w <- sum.split(d=nm.w, year.var=2020, rnd=1) # add-up pre-split population
drestore$p18_2020[sel.tmp] <- 1                   # indicate cases where counterfactual pop pasted
nm.w$dready2est <- 1; nm.w$dneedsum <- 0        # indicate manipulation
nm[sel.tmp,] <- nm.w                              # Return manipulated obs to nm
## These use sum.split(2020) for linear 2010-2020
nm[sel.tmp2, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
prj <- function(x=NA,yr=NA){
    chg <- (x$p18_2020 - x$p18_2010) /10 # yearly pop change
    pop <- x$p18_2010 + chg * (yr - 2010)
    return(pop)
}
nm[sel.tmp2,] <- within(nm[sel.tmp2,], {
    p18_2009 <- prj(nm[sel.tmp2,], 2009)
    p18_2012 <- prj(nm[sel.tmp2,], 2012)
    p18_2015 <- prj(nm[sel.tmp2,], 2015)
    p18_2018 <- prj(nm[sel.tmp2,], 2018)
    ddone <- 1
    dready2est <- 0
    nmanip <- nmanip + 1
    action  <- action2; orig.dest  <- orig.dest2; when  <- when2
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
rm(sel.tmp2)
## needs sum.split(2010) for linear proj
sel.tmp <- which(nm$dneedsum==1 & nm$alta==2002 & nm$baja==2010)
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm.w <- nm[sel.tmp,]                              # subset cases to manipulate
##nm.w[, grep("p18_(2003|2005|2010|2020)", colnames(nm))] # debug
nm.w <- sum.split(d=nm.w, year.var=2010, rnd=2) # add-up pre-split population
prj <- function(x=NA,yr=NA){
    chg <- (x$p18_2010 - x$p18_2005) /5 # yearly pop change
    pop <- x$p18_2005 + chg * (yr - 2005)
    return(pop)
}
nm.w <- within(nm.w, {
    p18_2003 <- p18_2005 # flat
    p18_2006 <- prj(nm.w, 2006)
    p18_2009 <- prj(nm.w, 2009)
    ddone <- 1
    dneedsum <- 0
    nmanip <- nmanip + 1
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
drestore$p18_2010[sel.tmp] <- 1                   # indicate cases where counterfactual pop pasted
nm[sel.tmp,] <- nm.w                              # Return manipulated obs to nm
## needs sum.split(2020) for linear proj
sel.tmp <- which(nm$dneedsum==1 & nm$alta==2009 & nm$baja==2019)
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm.w <- nm[sel.tmp,]                              # subset cases to manipulate
##nm.w[, grep("p18_(2003|2005|2010|2020)", colnames(nm))] # debug
nm.w <- sum.split(d=nm.w, year.var=2020, rnd=1) # add-up pre-split population
prj <- function(x=NA,yr=NA){
    chg <- (x$p18_2020 - x$p18_2010) /10 # yearly pop change
    pop <- x$p18_2010 + chg * (yr - 2010)
    return(pop)
}
nm.w <- within(nm.w, {
    p18_2009 <- prj(nm.w, 2009)
    p18_2012 <- prj(nm.w, 2012)
    p18_2015 <- prj(nm.w, 2015)
    p18_2018 <- prj(nm.w, 2018)
    ddone <- 1
    dneedsum <- 0
    nmanip <- nmanip + 1
    action  <- action2; orig.dest  <- orig.dest2; when  <- when2
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
drestore$p18_2020[sel.tmp] <- 1                   # indicate cases where counterfactual pop pasted
nm[sel.tmp,] <- nm.w                              # Return manipulated obs to nm
##
## Check 2005-2010-2020 complete
sel.tmp <- which(nm$dready2est==1)
table(is.na(nm$p18_2005[sel.tmp]))
table(is.na(nm$p18_2010[sel.tmp]))
table(is.na(nm$p18_2020[sel.tmp]))
table(nm$p18_2005[sel.tmp]==0)
table(nm$p18_2010[sel.tmp]==0)
table(nm$p18_2020[sel.tmp]==0)

#####################################################
## Prep object to receive all possible regressions ##
#####################################################
regs <- vector(mode='list', length(nrow(nm))) ## empty list
regs[nm$dskip==1] <- "No regression, skipped due to lack of census data"
regs[nm$ddone==1] <- "No regression, < 3 censuses data points, linear/flat estimates used"
regs[[70977]]
##
#####################################################################
## Apply interlog directly to nm for single-sección municipio to   ##
## predict 1994:2003 and 2021; apply interpol to predict 2006:2018 ##
#####################################################################
## Check single-seccion municipios
table(single=nm$dsingle, ready=nm$dready2est)
table(single=nm$dsingle, done=nm$ddone)
## Subset data
sel.c <- grep("seccion|^p18_(2005|2010|2020)$", colnames(nm))
nm.w <- nm[nm$dsingle==1, sel.c]
## Log-linear projection of 2003, retaining regressions to use for 1994-on
tmp.regs <- vector(mode='list', length(nrow(nm.w))) ## empty list
tmp.regs <- interlog(what="p18", yr=1994, unit="s", frm="log(dv)~iv", census.data = nm.w)
## tmp.e will receive all log-linear predictions
tmp.e <- data.frame(seccion  =nm.w$seccion,
                    p18_1994=tmp.regs[[1]])
##non.nas <- apply(tmp.e, 1, sum); non.nas <- which(!is.na(non.nas)) # determine in which cases to skip prediction
new.d <- data.frame(iv=seq(1997,2021,3))     ## prep predictions 1997-on
preds <- vector(mode='list', nrow(tmp.e))    ## empty list
tmp <- data.frame(dv=rep(NA, nrow(new.d)))   ## empty df for non non.nas
preds <- lapply(preds, function(x) x <- tmp) ## empty df for non non.nas
## predict
preds <- lapply(tmp.regs[[3]], function(x) data.frame(dv.hat=predict.lm(x, newdata = new.d)))
preds <- lapply(preds, function(x) x <- t(x))       # transpose to have yrs in cols
preds <- do.call(rbind, preds)                      # to data frame
colnames(preds) <- paste0("p18_", seq(1997,2021,3)) # add names
preds <- exp(preds)                                 # exponentiate log-linear predictions
preds <- round(preds,1)                             # round to 1 digit
preds[1:10,]
tmp.e <- cbind(tmp.e, preds)                        # consolidate predictions
##tmp.e <- cbind(tmp.e, nm.w[, paste0("p18_", c(2005,2010,2020))]) # add census yrs
##tmp.e <- tmp.e[, order(colnames(tmp.e))]            # sort columns except 1st (seccion)
rownames(tmp.e) <- NULL
head(tmp.e)
tmp.e$p18_1994 # all looks ok?
## linear
tmp.l <- data.frame(seccion  =nm.w$seccion,
                    p18_1994=interpol(what="p18", yr=1994, unit="s", census.data = nm.w),
                    p18_1997=interpol(what="p18", yr=1997, unit="s", census.data = nm.w),
                    p18_2000=interpol(what="p18", yr=2000, unit="s", census.data = nm.w),
                    p18_2003=interpol(what="p18", yr=2003, unit="s", census.data = nm.w),
                    p18_2006=interpol(what="p18", yr=2006, unit="s", census.data = nm.w),
                    p18_2009=interpol(what="p18", yr=2009, unit="s", census.data = nm.w),
                    p18_2012=interpol(what="p18", yr=2012, unit="s", census.data = nm.w),
                    p18_2015=interpol(what="p18", yr=2015, unit="s", census.data = nm.w),
                    p18_2018=interpol(what="p18", yr=2018, unit="s", census.data = nm.w),
                    p18_2021=interpol(what="p18", yr=2021, unit="s", census.data = nm.w)
                    )
tmp.l$p18_2006
## subset again with all cols to fill-in estimates
nm.w <- nm[nm$dsingle==1,]
nm.w <- within(nm.w, {
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
    dready2est <- 0
    ddone <- 1
})
nm.w[1,]
## return estimates and regressions to data
nm[nm$dsingle==1,] <- nm.w
regs[nm$dsingle==1] <- tmp.regs[3]
rm(nm.w, tmp.regs)


##################################
## Estimate dready2est==1 cases ##
##################################
## Check working cases
table(skip=nm$dskip,    ready=nm$dready2est)
table(done=nm$ddone,    ready=nm$dready2est)
table(sum= nm$dneedsum, ready=nm$dready2est)
sel.tmp <- which(nm$dskip==0 & nm$ddone==0 & nm$dneedsum==0 & nm$dready2est==0); length(sel.tmp) ## exhaustive
##
## Verify muns with zero pop
## All recently created
nm$inegi[which(nm$p18m_05==0)]
nm$inegi[which(nm$p18m_10==0)]
nm$inegi[which(nm$p18m_20==0)]
nm[
   which(nm$inegi==4011),
   c(grep("alta|inegi|p18.*_(2005|2010|2020)", colnames(nm)), grep("p18m_(05|10|20)", colnames(nm)))
]



#####################################
## Change NAs to 0s in census data ##
#####################################
tmp <- nm[,c("p18_2005","p18_2010","p18_2020")]
tmp[1,]
tmp[is.na(tmp)] <- 0
##tmp[tmp==0] <- 1
tmp -> nm[,c("p18_2005","p18_2010","p18_2020")]

######################################################
## Check no sección #1 in each municipio has pop>0, ##
## else change index to avoid dividing by zero      ##
######################################################
which(nm$dfirst==1 & nm$p18_2020==0)
show(nm[,grep("ord|seccion|inegi|alta|baja|p18.*_(2005|2010|2020)|ddone|dfirst|dskip", colnames(nm))], 16498,  5, 0)
show(nm[,grep("ord|seccion|inegi|alta|baja|p18.*_(2005|2010|2020)|ddone|dfirst|dskip", colnames(nm))], 25598,  5, 0)
show(nm[,grep("ord|seccion|inegi|alta|baja|p18.*_(2005|2010|2020)|ddone|dfirst|dskip", colnames(nm))], 26001,  5, 0)
show(nm[,grep("ord|seccion|inegi|alta|baja|p18.*_(2005|2010|2020)|ddone|dfirst|dskip", colnames(nm))], 31982,  5, 0)
show(nm[,grep("ord|seccion|inegi|alta|baja|p18.*_(2005|2010|2020)|ddone|dfirst|dskip", colnames(nm))], 43159,  5, 0)
show(nm[,grep("ord|seccion|inegi|alta|baja|p18.*_(2005|2010|2020)|ddone|dfirst|dskip", colnames(nm))], 44010,  5, 0)
show(nm[,grep("ord|seccion|inegi|alta|baja|p18.*_(2005|2010|2020)|ddone|dfirst|dskip", colnames(nm))], 51543,  5, 0)
show(nm[,grep("ord|seccion|inegi|alta|baja|p18.*_(2005|2010|2020)|ddone|dfirst|dskip", colnames(nm))], 57659,  5, 0)
##
## Are there zero pop 1st secciones? If so, flip indices with another seccion
nm <- nm[order(nm$ord),] # ord-sort
sel.c <- grep("seccion|^p18_", colnames(nm))
tmp <- nm[nm$dfirst==1, sel.c]
tmp$dzero <- rowSums(tmp[,-1]==0, na.rm = TRUE)
table(tmp$dzero)
nm[nm$dfirst==1,][which(tmp$dzero>0),]
sum(tmp$dzero) # if >0, manipulate indices
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




######################
## Compute sh and r ##
######################
## OJO: To compute r, all secciones in multisección municipios must be considered.
## Then, estimation on (dready2est==1 & dsingle==0 & dfirst==0) only.
## Translating r.hats into sh.hats again needs all secciones, in order to compute 1+sum(r) for denominator.
sh <- within(nm, {
    p18_2005 <- p18_2005 / p18m_05;
    p18_2010 <- p18_2010 / p18m_10;
    p18_2020 <- p18_2020 / p18m_20;
})
summary(sh$p18_2005)
summary(sh$p18_2010)
summary(sh$p18_2020)
## Any sh=1 that are not singles?
sel.tmp <- which(sh$p18_2005==1 & sh$dsingle==0)
sel.tmp
sel.tmp <- which(sh$p18_2010==1 & sh$dsingle==0)
sel.tmp
sel.tmp <- which(sh$p18_2020==1 & sh$dsingle==0)
sel.tmp
show(nm[,c("seccion","inegi","p18_2005","p18_2010","p18_2020","p18m_05","p18m_10","p18m_20")], rows=sel.tmp, after=2, before=2)
##
## Any sh=0 first seccion?
sel.tmp <- which(sh$p18_2005==0 & sh$dfirst==1)
sel.tmp
sel.tmp <- which(sh$p18_2010==0 & sh$dfirst==1)
sel.tmp
sel.tmp <- which(sh$p18_2020==0 & sh$dfirst==1)
sel.tmp
nm[sel.tmp, c("seccion","inegi","p18_2005","p18_2010","p18_2020","p18m_05","p18m_10","p18m_20")]
##
r <- split(x=sh, f=sh$inegi) # split into list of data frames, one per municipio, to compute r=p18_i / p18_1
sel.c <- c("p18_2005", "p18_2010", "p18_2020")
tmp.f <- function(x=NA){
    ##x <- cenr[[1005]] # debug
    tmp.denom <- do.call(rbind, replicate(n=nrow(x), x[1, sel.c], simplify = FALSE)) # replicate 1st seccion times all sec in mun
    x[, sel.c] <- x[, sel.c] / tmp.denom                                             # divide each row by 1st row
    ##x$dfirst[1] <- 1                                                                  # indicate 1st seccion in municipio
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
## show(nm[,c("seccion","inegi","p18_2005","p18_2010","p18_2020","p18m_05","p18m_10","p18m_20","ddone")], sel.tmp[1], 0, 30)

## Inspect/manipulate and drop unseless indicators
with(nm, table(ddone, dneedsum))
nm$dneedsum <- NULL ## No longer needed
with(nm, table(ddone, dskip))
nm[nm$dskip==1, c("p18_2005","p18_2010","p18_2020")]
nm[nm$dskip==1, grep("p18_", colnames(nm))] <- 0 ## Zeroes across the board
with(nm, table(dready2est, dfirst))
with(nm, table(ddone, dfirst))


##############################################################################
## Apply interlog to r[dready2est==1] to estimate and                       ##
## predict 1994:2003 and 2021; apply interpol for 2006:2018                 ##
##############################################################################
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
tmp.regs <- interlog(what="p18", yr=1994, unit="s", frm="log(dv)~iv", census.data=r.w, digits=4)
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
rm(r.w, fun.tmp, sel.tmp, tmp, tmp.e, tmp.l, tmp.f, tmp.regs)



################################
## Add 1s to first seccion rs ##
################################
sel.c <- grep("^p18_", colnames(r))
r[r$dfirst==1, sel.c] <- 1


## convert r.hat to sh.hat. All obs needed to compute denominator
denom <- r[, grep("seccion|inegi|^p18_", colnames(r))]
r[r$inegi==1004,grep("p18", colnames(r))]
denom[denom$inegi==1004,]
denom <- split(x=denom, f=denom$inegi)
#denom <- denom[[4]] # debug
fun.tmp <- function(x){
    tmp  <- do.call(rbind, replicate(nrow(x), colSums(x[,-c(1,2)], na.rm = TRUE), simplify = FALSE))
    x[,-c(1,2)] <- tmp
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
tmp -> sh.w[, grep("^p18_(1994|1997|2000|2003|2006|2009|2012|2015|2018|2021)", colnames(sh.w))]

#################################################################
## Get projected mun pops to convert sh.hats back into nm.hats ##
#################################################################
tmp <- censom21
prj <- function(x=NA,yr=NA){
    chg <- (x$p18_1995 - x$p18_1990) /5 # yearly pop change
    pop <- x$p18_1990 + chg * (yr - 1990)
    return(pop)
}
tmp <- within(tmp, {
    p18_1994 <- prj(tmp, 1994)
})
prj <- function(x=NA,yr=NA){
    chg <- (x$p18_2000 - x$p18_1995) /5 # yearly pop change
    pop <- x$p18_1995 + chg * (yr - 1995)
    return(pop)
}
tmp <- within(tmp, {
    p18_1997 <- prj(tmp, 1997)
})
prj <- function(x=NA,yr=NA){
    chg <- (x$p18_2005 - x$p18_2000) /5 # yearly pop change
    pop <- x$p18_2000 + chg * (yr - 2000)
    return(pop)
}
tmp <- within(tmp, {
    p18_2003 <- prj(tmp, 2003)
})
prj <- function(x=NA,yr=NA){
    chg <- (x$p18_2010 - x$p18_2005) /5 # yearly pop change
    pop <- x$p18_2005 + chg * (yr - 2005)
    return(pop)
}
tmp <- within(tmp, {
    p18_2006 <- prj(tmp, 2006)
    p18_2009 <- prj(tmp, 2009)
})
prj <- function(x=NA,yr=NA){
    chg <- (x$p18_2020 - x$p18_2010) /10 # yearly pop change
    pop <- x$p18_2010 + chg * (yr - 2010)
    return(pop)
}
tmp <- within(tmp, {
    p18_2012 <- prj(tmp, 2012)
    p18_2015 <- prj(tmp, 2015)
    p18_2018 <- prj(tmp, 2018)
    p18_2021 <- prj(tmp, 2021)
})
tmp <- tmp[,order(colnames(tmp))]
tmp$p18_1990 <- tmp$p18_1995 <- tmp$p18_2005 <- tmp$p18_2010 <- tmp$p18_2020 <- tmp$edon <- tmp$mun <- tmp$ife <- NULL
dim(tmp)
tmp[1,]
## pick municipios needed
tmp2 <- data.frame(ord=1:nrow(sh.w), inegi=sh.w$inegi)
tmp2 <- merge(x=tmp2, y=tmp, by="inegi", all.x=TRUE, all.y=FALSE)
tmp2 <- tmp2[order(tmp2$ord),]; tmp2$ord <- NULL
tmp <- tmp2; rm(tmp2)
tmp[1:3,]
##
## compute nm.hat and empty in nm
tmp.nm <- sh.w[, grep("^p18_(1994|1997|2000|2003|2006|2009|2012|2015|2018|2021)", colnames(r.w))] *
          tmp [, grep("^p18_(1994|1997|2000|2003|2006|2009|2012|2015|2018|2021)", colnames(tmp))]
## check
tmp[tmp$inegi==1001, grep("^p18_", colnames(tmp))]
tmp.nm[tmp$inegi==1001, grep("^p18_", colnames(tmp.nm))]
nm[nm$inegi==1001, grep("^p18_", colnames(nm))]
## check
head(tmp)
head(tmp.nm)
head(nm)
## all should be 1 !!!!
tmp <- sh[, grep("^p18_", colnames(sh))]
tmp <- split(tmp, sh$inegi)
tmp <- lapply(tmp, colSums)
tmp <- do.call(rbind, tmp)
summary(tmp)

rm(fun.tmp, denom.w, nm.w, sh.w, r.w, prj, preds, sec.tmp, sel, sel.c, sel.ignore, sel.r, sel.tmp, tmp, tmp.nm, wrap.f) ## clean


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

