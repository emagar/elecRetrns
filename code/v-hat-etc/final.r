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

## 4. Duplicate nm=censo, adding cols 1994:2021, m06:2005-2010-2020, ddone=0, etc
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
    p18m06_20 <- p18_2020; # yy so grep excludes
    p18m06_10 <- p18_2010; # yy so grep excludes
    p18m06_05 <- p18_2005; # yy so grep excludes
    p18_1994 <- p18_1997 <- p18_2000 <- p18_2003 <- p18_2006 <- p18_2009 <- p18_2012 <- p18_2015 <- p18_2018 <- p18_2021 <- NA;
})
##nm <- nm[order(nm$ife2006, nm$seccion),] # sort mun
##
## 5. Apply my_agg to generate municipal aggregates (nm[sel.r,]$m06:2005-2010-2020) 
sel.c <- c("p18m06_05", "p18m06_10", "p18m06_20")
nm <- my_agg(d=nm, sel.c=sel.c, by="ife2006", drop.dupli=FALSE)
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
## Merged.to secciones in 2020 get same 2010 pop
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
nm[sel.tmp, c("seccion","alta","baja","action","when","action2","when2","action3","when3","p18_2005","p18_2010","p18_2020","nmanip")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2020 <- p18_2010
    dready2est <- 1
    nmanip <- nmanip + 1
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
    action3 <- "";      orig.dest3 <- "";         when3 <- NA
})
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
## Nothing to manipulate left?
sel.tmp <- which(nm$ddone==0 & nm$dskip==0 & nm$dneedsum==0 & nm$dready2est==0)
table(is.na(nm$p18_2005[sel.tmp]))
table(is.na(nm$p18_2010[sel.tmp]))
table(is.na(nm$p18_2020[sel.tmp]))
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


########################################
## Indicate single-sección municipios ##
########################################
tmp <- split(x=nm, f=nm$ife2006)
tmp <- lapply(tmp, function(x){
    n <- nrow(x)
    n <- data.frame(ife2006=NA, n=n)
    return(n)
    })
tmp <- do.call(rbind, tmp)
tmp$ife2006 <- as.numeric(rownames(tmp))
nm$dsingle[which(nm$ife2006  %in% tmp$ife2006[tmp$n==1])] <- 1
##
############################################
## Indicate 1st seccion in each municipio ##
############################################
nm <- nm[order(nm$ord),] # make sure ord-sorted
tmp <- split(x=nm, f=nm$ife2006) # split into list of data frames, one per municipio, to compute r
tmp.f <- function(x=NA){
    ##x <- tmp[[1005]] # debug
    x$dfirst[1] <- 1            # indicate 1st seccion in municipio
    return(x)
}
tmp <- lapply(tmp, tmp.f) # apply function
tmp <- do.call(rbind, tmp) # return to data frame form
rownames(tmp) <- NULL
nm <- tmp # return manipulated data



## OJO MOVE THIS BLOCK: SHOULD CHECK ONCE ALL p18s ARE PROJECTED
######################################################
## Check no sección #1 in each municipio has pop>0, ##
## else change index to avoid dividing by zero      ##
######################################################
## Are there zero pop 1st secciones?
sel.c <- grep("seccion|^p18_", colnames(nm))
tmp <- nm[nm$dfirst==1, sel.c]
tmp$dzero <- rowSums(tmp[,-1]==0, na.rm = TRUE)
sum(tmp$dzero)
tmp[which(tmp$dzero==1),]
## switch indices
nm <- nm[order(nm$ord),] # make sure ord-sorted
tmp <- split(x=nm, f=nm$ife2006) # split into list of data frames, one per municipio (2006 map)
## for cases with zero pop in 1st seccion, this swaps indices of secciones 1 and 2 
wrap.f <- function(x=NA){
    ##x <- tmp[[1028]]   # debug
    ##x$p18_2005[1] <- 0 # debug
    x <- x
    if (nrow(x)>1){                           # proceed only if multirow data frame
        y  <- sum(x[1, sel.c]==0, na.rm=TRUE) # how many zeroes, excluding NAs, in sección 1's sel.c columns
        ##yy <- sum(x[2, sel.c]==0, na.rm=TRUE) # how many zeroes, excluding NAs, in sección 2's sel.c columns
        if (y>0){ # if sección 1 has zeroes, flip indices with sección 2
            x$ord   [1:2] <- x$ord   [2:1]
            x$dfirst[1:2] <- x$dfirst[2:1]
        }
    }
    return(x)
}
tmp <- lapply(tmp, wrap.f) # apply function
tmp <- do.call(rbind, tmp) # return to data frame form
tmp <- tmp[order(tmp$ord),] # re-sort
rownames(tmp) <- NULL
nm <- tmp                   # replace nm with manipulation (some secciones)
##
## check again: zeroes?
tmp <- nm[nm$dfirst==1, sel.c]
tmp$dzero <- rowSums(tmp[,-1]==0, na.rm = TRUE)
sum(tmp$dzero)






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
    #action  <- action2; orig.dest  <- orig.dest2; when  <- when2
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

## NOW GET ln(dv)~iv
## Check 2005-2010-2020 complete
sel.tmp <- which(nm$dready2est==1)
table(is.na(nm$p18_2005[sel.tmp]))
table(is.na(nm$p18_2010[sel.tmp]))
table(is.na(nm$p18_2020[sel.tmp]))



## 5.1 Verify no mun with zero pop
print("All are municipios created recently before 2006")
nm$ife2006[which(nm$p18m06_05==0)]
nm$ife2006[which(nm$p18m06_10==0)]
nm$ife2006[which(nm$p18m06_20==0)]
nm[
    which(nm$ife2006==4011),
    c(grep("ife2006|p18.*_(2005|2010|2020)", colnames(nm)), grep("p18m06_(05|10|20)", colnames(nm)))
]

## 11. Compute prelim sh and prelim r
sh <- within(nm, {
    p18_2005 <- p18_2005 / p18m06_05;
    p18_2010 <- p18_2010 / p18m06_10;
    p18_2020 <- p18_2020 / p18m06_20;
})
##
r <- split(x=sh, f=sh$ife2006) # split into list of data frames, one per municipio, to compute r
sel.c <- c("p18_2005", "p18_2010", "p18_2020")
tmp.f <- function(x=NA){
    ##x <- cenr[[1005]] # debug
    tmp.denom <- do.call(rbind, replicate(n=nrow(x), x[1, sel.c], simplify = FALSE)) # replicate 1st seccion times all sec in mun
    x[, sel.c] <- x[, sel.c] / tmp.denom     # divide each row by 1st row
    x$dskip[1] <- 1            # indicate 1st seccion in municipio
    return(x)
}
r <- lapply(r, tmp.f) # apply function
r <- do.call(rbind, r) # return to data frame form

## verify no seccion#1 has zero pop
table(r$p18_2005[r$dskip==1], useNA = "ifany")
r$ife2006[r$dskip==1][which(is.na(r$p18_2005[r$dskip==1]))]
table(r$p18_2010[r$dskip==1], useNA = "ifany")
r$ife2006[r$dskip==1][which(is.na(r$p18_2010[r$dskip==1]))]
table(r$p18_2020[r$dskip==1], useNA = "ifany")
r$ife2006[r$dskip==1][which(is.na(r$p18_2020[r$dskip==1]))]

nm[nm$ife2006==4010, c("alta", "baja", "action", "action2", "action3", "p18_2005", "p18_2010", "p18_2020")]
nm[nm$ife2006==4011, c("alta", "baja", "action", "action2", "action3", "p18_2005", "p18_2010", "p18_2020")]
nm[nm$ife2006==7001, c("alta", "baja", "action", "action2", "action3", "p18_2005", "p18_2010", "p18_2020")]
nm[nm$ife2006==7035, c("alta", "baja", "action", "action2", "action3", "p18_2005", "p18_2010", "p18_2020")]
nm[nm$ife2006==7051, c("alta", "baja", "action", "action2", "action3", "p18_2005", "p18_2010", "p18_2020")]
nm[nm$ife2006==7062, c("alta", "baja", "action", "action2", "action3", "p18_2005", "p18_2010", "p18_2020")]
nm[nm$ife2006==9016, c("alta", "baja", "action", "action2", "action3", "p18_2005", "p18_2010", "p18_2020")]
nm[nm$ife2006==13040, c("alta", "baja", "action", "action2", "action3", "p18_2005", "p18_2010", "p18_2020")]
nm[nm$ife2006==13068, c("alta", "baja", "action", "action2", "action3", "p18_2005", "p18_2010", "p18_2020")]
nm[nm$ife2006==14075, c("alta", "baja", "action", "action2", "action3", "p18_2005", "p18_2010", "p18_2020")]
nm[nm$ife2006==15012, c("alta", "baja", "action", "action2", "action3", "p18_2005", "p18_2010", "p18_2020")]
nm[nm$ife2006==15036, c("alta", "baja", "action", "action2", "action3", "p18_2005", "p18_2010", "p18_2020")]
nm[nm$ife2006==17035, c("alta", "baja", "action", "action2", "action3", "p18_2005", "p18_2010", "p18_2020")]
nm[nm$ife2006==18004, c("alta", "baja", "action", "action2", "action3", "p18_2005", "p18_2010", "p18_2020")]
nm[nm$ife2006==19038, c("alta", "baja", "action", "action2", "action3", "p18_2005", "p18_2010", "p18_2020")]
nm[nm$ife2006==19045, c("alta", "baja", "action", "action2", "action3", "p18_2005", "p18_2010", "p18_2020")]
nm[nm$ife2006==20066, c("alta", "baja", "action", "action2", "action3", "p18_2005", "p18_2010", "p18_2020")]
nm[nm$ife2006==20076, c("alta", "baja", "action", "action2", "action3", "p18_2005", "p18_2010", "p18_2020")]
nm[nm$ife2006==21039, c("alta", "baja", "action", "action2", "action3", "p18_2005", "p18_2010", "p18_2020")]
nm[nm$ife2006==23008, c("alta", "baja", "action", "action2", "action3", "p18_2005", "p18_2010", "p18_2020")]
nm[nm$ife2006==23009, c("alta", "baja", "action", "action2", "action3", "p18_2005", "p18_2010", "p18_2020")]
nm[nm$ife2006==23010, c("alta", "baja", "action", "action2", "action3", "p18_2005", "p18_2010", "p18_2020")]
nm[nm$ife2006==25001, c("alta", "baja", "action", "action2", "action3", "p18_2005", "p18_2010", "p18_2020")]
nm[nm$ife2006==26003, c("alta", "baja", "action", "action2", "action3", "p18_2005", "p18_2010", "p18_2020")]
nm[nm$ife2006==26061, c("alta", "baja", "action", "action2", "action3", "p18_2005", "p18_2010", "p18_2020")]
nm[nm$ife2006==29038, c("alta", "baja", "action", "action2", "action3", "p18_2005", "p18_2010", "p18_2020")]
nm[nm$ife2006==30034, c("alta", "baja", "action", "action2", "action3", "p18_2005", "p18_2010", "p18_2020")]
nm[nm$ife2006==30088, c("alta", "baja", "action", "action2", "action3", "p18_2005", "p18_2010", "p18_2020")]
nm[nm$ife2006==30110, c("alta", "baja", "action", "action2", "action3", "p18_2005", "p18_2010", "p18_2020")]




## 5.x Drop cases with many zeroes or NAs
###############################
## inspect/manipulate zeroes ##
###############################
## all zeroes get 10 fixed
sel.tmp <- which(nm$p18_2005==0 & nm$p18_2010==0 & nm$p18_2020==0) ## all zero (but have votes, eg. campo militar 1)
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_1994 <- p18_1997 <- p18_2003 <- p18_2006 <- p18_2009 <- p18_2012 <- p18_2015 <- p18_2018 <- p18_2021 <- 10 #10 instead of 0
})
## Take these out of projection routine to exclude log predictions
nm.done <- nm[ sel.tmp,]
nm <-      nm[-sel.tmp,]
## move these to all 10 bunch, NA after baja
sel.tmp <- which(nm$p18_2005==0 & nm$baja==2008)
nm[sel.tmp,c("p18_2005","p18_2010","p18_2020")]
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
table(act=nm$action[sel.tmp], act2=nm$action2[sel.tmp], useNA = "ifany")
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_1994 <- p18_1997 <- p18_2003 <- p18_2005 <- p18_2006 <- 10
})
nm.done <- rbind(nm.done, nm[ sel.tmp,])
nm <-                     nm[-sel.tmp,]
## move these to all 10 bunch, NA before alta
sel.tmp <- which(is.na(nm$p18_2005) & nm$p18_2010==0 & nm$p18_2020==0 & nm$alta==2009) ## two zero
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2009 <- p18_2010 <- p18_2012 <- p18_2015 <- p18_2018 <- p18_2020 <- p18_2021 <- 10
})
nm.done <- rbind(nm.done, nm[ sel.tmp,])
nm <-                     nm[-sel.tmp,]
## move these to all 10 bunch, NA after baja
sel.tmp <- which(is.na(nm$p18_2005) & nm$p18_2010==0 & is.na(nm$p18_2020)) ## two zero
nm[sel.tmp,c("p18_2005","p18_2010","p18_2020")]
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
table(act=nm$action[sel.tmp], act2=nm$action2[sel.tmp], useNA = "ifany")
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_1994 <- p18_1997 <- p18_2003 <- p18_2005 <- p18_2006 <- p18_2009 <- p18_2010 <- p18_2012 <- p18_2015 <- p18_2018 <- 10
})
nm.done <- rbind(nm.done, nm[ sel.tmp,])
nm <-                     nm[-sel.tmp,]
## move these to all 10 bunch
sel.tmp <- which(nm$p18_2005==0 & nm$p18_2010==0 & is.na(nm$p18_2020)) ## two zero
nm[sel.tmp,c("p18_2005","p18_2010","p18_2020")]
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
table(act=nm$action[sel.tmp], act2=nm$action2[sel.tmp], useNA = "ifany")
##
sel.tmp <- which(nm$p18_2005==0 & nm$p18_2010==0 & is.na(nm$p18_2020) & (nm$baja==2012|nm$baja==2014)) ## two zero
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_1994 <- p18_1997 <- p18_2003 <- p18_2005 <- p18_2006 <- p18_2009 <- p18_2010 <- 10
})
nm.done <- rbind(nm.done, nm[ sel.tmp,])
nm <-                     nm[-sel.tmp,]
##
sel.tmp <- which(nm$p18_2005==0 & nm$p18_2010==0 & is.na(nm$p18_2020) & nm$baja==2020) ## two zero
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_1994 <- p18_1997 <- p18_2003 <- p18_2005 <- p18_2006 <- p18_2009 <- p18_2010 <- p18_2012 <- p18_2015 <- p18_2018 <- 10
})
nm.done <- rbind(nm.done, nm[ sel.tmp,])
nm <-                     nm[-sel.tmp,]
## move these to all NA bunch
sel.tmp <- which(is.na(nm$p18_2005) & is.na(nm$p18_2010) & is.na(nm$p18_2020) & nm$action=="state.chg")
nm[sel.tmp,c("p18_2005","p18_2010","p18_2020")]
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
nm.done <- rbind(nm.done, nm[ sel.tmp,])
nm <-                     nm[-sel.tmp,]
##
sel.tmp <- which(nm$baja==2022)#p18_2005) & is.na(nm$p18_2010) & nm$baja==2002)
with(nm[sel.tmp,], table(action=action , when=when , useNA = "ifany"))
with(nm[sel.tmp,], table(action=action2, when=when2, useNA = "ifany"))
with(nm[sel.tmp,], table(action=action3, when=when3, useNA = "ifany"))
##
table(nm$baja)
##
## these will get flat 10 up to baja
sel.tmp <- which(is.na(nm$p18_2005) & is.na(nm$p18_2010) & nm$baja==2002)
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_1994 <- p18_1997 <- p18_2000 <- 10
})
nm.done <- rbind(nm.done, nm[ sel.tmp,])
nm <-                     nm[-sel.tmp,]
##
sel.tmp <- which(is.na(nm$p18_2005) & is.na(nm$p18_2010) & nm$baja==2005)
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_1994 <- p18_1997 <- p18_2000 <- p18_2003 <- 10
})
nm.done <- rbind(nm.done, nm[ sel.tmp,])
nm <-                     nm[-sel.tmp,]
##
sel.tmp <- which(is.na(nm$p18_2005) & is.na(nm$p18_2010) & nm$baja==2008)
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_1994 <- p18_1997 <- p18_2000 <- p18_2003 <- p18_2005 <- p18_2006 <- 10
})
nm.done <- rbind(nm.done, nm[ sel.tmp,])
nm <-                     nm[-sel.tmp,]
##
sel.tmp <- which(is.na(nm$p18_2005) & is.na(nm$p18_2010))
nm[sel.tmp,c("p18_2005","p18_2010","p18_2020")]
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
table(alta=nm$alta[sel.tmp], baja=nm$action[sel.tmp], useNA = "ifany")
table(act=nm$action[sel.tmp], act2=nm$action2[sel.tmp], useNA = "ifany")
##
##
##
sel.tmp <- which(is.na(nm$p18_2005) & is.na(nm$p18_2010) & (nm$alta==2012 | nm$alta==2013 | nm$alta==2014))
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2012 <- p18_2015 <- p18_2018 <- p18_2021 <- p18_2020
})
nm.done <- rbind(nm.done, nm[ sel.tmp,])
nm <-                     nm[-sel.tmp,]
##
sel.tmp <- which(is.na(nm$p18_2005) & is.na(nm$p18_2010) & (nm$alta==2015 | nm$alta==2016 | nm$alta==2017))
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2015 <- p18_2018 <- p18_2021 <- p18_2020
})
nm.done <- rbind(nm.done, nm[ sel.tmp,])
nm <-                     nm[-sel.tmp,]
##
sel.tmp <- which(is.na(nm$p18_2005) & is.na(nm$p18_2010) & (nm$alta==2018 | nm$alta==2019 | nm$alta==2020))
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2018 <- p18_2021 <- p18_2020
})
nm.done <- rbind(nm.done, nm[ sel.tmp,])
nm <-                     nm[-sel.tmp,]
##
##
##
table(nm$p18_2005==0)
table(nm$p18_2010==0)
table(nm$p18_2020==0)
table(is.na(nm$p18_2005))
table(is.na(nm$p18_2010))
table(is.na(nm$p18_2020))
##
with(nm[nm$action=="split.to",], table(when=when, baja=baja))
dim(nm)
dim(nm.done)
##
##
sel.tmp <- which(is.na(nm$p18_2005) & is.na(nm$p18_2010) & is.na(nm$p18_2020) & nm$baja<=2005)
table(act=nm$action[sel.tmp], act2=nm$action2[sel.tmp], useNA = "ifany")
##
nm.done <- rbind(nm.done, nm[ sel.tmp,])
nm <-                     nm[-sel.tmp,]
##
##
sel.tmp <- which(is.na(nm$p18_2005) & is.na(nm$p18_2010) & is.na(nm$p18_2020))
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
table(act=nm$action[sel.tmp], act2=nm$action2[sel.tmp], useNA = "ifany")
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_1994 <- p18_1997 <- p18_2003 <- p18_2005 <- p18_2006 <- p18_2009 <- p18_2010 <- p18_2012 <- p18_2015 <- p18_2018 <- p18_2020 <- p18_2021 <- 10
})
nm.done <- rbind(nm.done, nm[ sel.tmp,])
nm <-                     nm[-sel.tmp,]
##
## will get flat 2005 pop until
sel.tmp <- which(is.na(nm$p18_2010) & is.na(nm$p18_2020))
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
table(act=nm$action[sel.tmp], act2=nm$action2[sel.tmp], useNA = "ifany")
nm[sel.tmp,c("p18_2005","p18_2010","p18_2020")]
##
dim(nm)
dim(nm.done)



## are there still zeroes?
tmp <- r[,c("p18_2005", "p18_2010", "p18_2020")]
tmp <- apply(tmp, 1, function(x) sum(as.numeric(x==0), na.rm=TRUE))
names(tmp) <- NULL
tmp <- which(tmp==1)
nm[tmp[1],c("alta", "baja", "action", "p18_2005", "p18_2010", "p18_2020")]
x

## 12. Re-subset
r.w <- r[sel.r,]
r.w[1,c("p18_2005", "p18_2010", "p18_2020")]
x
## 13. Apply interlog to r[sel.r,], predict 1994:2004 and and 2021
## Log-linear projection of 2003, retaining regressions to use for 1994-on
tmp.regs <- vector(mode='list', length(nrow(cenr))) ## empty list
sel <- which(cenr$dskip==0)
tmp.regs[sel] <- interlog(what="p18", yr=2003, unit="s", frm="log(dv)~iv", census.data = cenr[sel,])
## tmp.e will receive all log-linear predictions
tmp.e <- data.frame(seccion=censo$seccion,
                     p18_1991= tmp.regs[[1]])
non.nas <- apply(tmp.e, 1, sum); non.nas <- which(!is.na(non.nas)) # determine in which cases to skip prediction
new.d <- data.frame(iv=seq(1994,2021,3))     ## prep predictions 1994-on
preds <- vector(mode='list', nrow(tmp.e))    ## empty list
tmp <- data.frame(dv=rep(NA, nrow(new.d)))   ## empty df for non non.nas
preds <- lapply(preds, function(x) x <- tmp) ## empty df for non non.nas
## predict
preds[non.nas] <- lapply(tmp.regs[[3]][non.nas], function(x) data.frame(dv.hat=predict.lm(x, newdata = new.d)))
preds <- lapply(preds, function(x) x <- t(x))       # transpose to have yrs in cols
preds <- do.call(rbind, preds)                      # to data frame
colnames(preds) <- paste0("p18_", seq(1994,2021,3)) # add names
preds <- exp(preds)                                 # exponentiate log-linear predictions
preds <- round(preds,1)                             # round to 1 digit
preds[1:10,]
tmp.e <- cbind(tmp.e, preds)                        # consolidate predictions
tmp.e <- cbind(tmp.e, censo[, paste0("p18_", c(2005,2010,2020))]) # add census yrs
tmp.e <- tmp.e[, order(colnames(tmp.e))]            # sort columns except 1st (seccion)
rownames(tmp.e) <- NULL
head(tmp.e)

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
## 25. Apply my_sum(nm[sel.r,]$m06:2005-2010-2020) 
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
## 40. Apply my_sum(nm[sel.r,]$m06:2005-2010-2020) 
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
## 57. Apply my_sum(nm[sel.r,]$m06:2005-2010-2020) 
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
## 73. Apply my_sum(nm[sel.r,]$m06:2005-2010-2020) 
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
## 91. Apply my_sum(nm[sel.r,]$m06:2005-2010-2020) 
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
## 105. Apply my_sum(nm[sel.r,]$m06:2005-2010-2020) 
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

