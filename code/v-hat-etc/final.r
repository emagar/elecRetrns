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

## 4. Duplicate nm=censo, adding 1994:2021, m06:2005-2010-2020, and ddone=0

# duplicate censo for manipulation (nmnal quantities)
nm <- censo

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
    dmanip <- dsingle <- dskip <- ddone <- 0;
    actionb  <- action;  whenb  <- when;  orig.destb  <- orig.dest;  # backup
    action2b <- action2; when2b <- when2; orig.dest2b <- orig.dest2; # backup
    action3b <- action3; when3b <- when3; orig.dest3b <- orig.dest3; # backup
    p18m06_20 <- p18_2020; # yy so grep excludes
    p18m06_10 <- p18_2010; # yy so grep excludes
    p18m06_05 <- p18_2005; # yy so grep excludes
    p18_1994 <- p18_1997 <- p18_2003 <- p18_2006 <- p18_2009 <- p18_2012 <- p18_2015 <- p18_2018 <- p18_2021 <- NA;
})
##nm <- nm[order(nm$ife2006, nm$seccion),] # sort mun
##
## 5. Apply my_sum to generate municipal aggregates (nm[sel.r,]$m06:2005-2010-2020) 
sel.c <- c("p18m06_05", "p18m06_10", "p18m06_20")
nm <- my_agg(d=nm, sel.c=sel.c, by="ife2006", drop.dupli=FALSE)

## Cherry-pick secciones that need manipulation by hand
## these are innocuous, drop label
sel.tmp <- which(nm$action2=="mun.chg")
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
})
sel.tmp <- which(nm$action=="mun.chg")
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    action <- action2; orig.dest <- orig.dest2; when <- when2
})
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
})
## these have no census info, drop label
sel.tmp <- which(nm$action=="state.chg")
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
nm[sel.tmp, c("alta", "baja", "action", "p18_2005", "p18_2010", "p18_2020")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    action <- action2; orig.dest <- orig.dest2; when <- when2
})
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
})
## Merged.to secciones in 2020 get flat 2010 pop
sel.tmp <- which(nm$action=="merged.to" & (nm$when==2012|nm$when==2014|nm$when==2020))
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
nm[sel.tmp, c("alta", "baja", "action", "p18_2005", "p18_2010", "p18_2020")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2020 <- p18_2010
    dmanip <- 1
    action <- action2; orig.dest <- orig.dest2; when <- when2
})
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
})
##
sel.tmp <- which(nm$alta==1993 & nm$action2=="merged.to" & nm$when2==2020)
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
nm[sel.tmp, c("alta", "baja", "action2", "p18_2005", "p18_2010", "p18_2020")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2020 <- p18_2010
    dmanip <- 1
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
})
## merged.to: flat up to baja
sel.tmp <- which(nm$action=="merged.to" & nm$action2=="" & (nm$when==2008|nm$when==2009|nm$when==2010|nm$when==2011))
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
nm[sel.tmp, c("alta", "baja", "action", "p18_2005", "p18_2010", "p18_2020")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2020 <- p18_2010 <- p18_2005
    dmanip <- 1
    action <- action2; orig.dest <- orig.dest2; when <- when2
})
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
})
## No way to know population here
sel.tmp <- which(nm$action=="split.from" & nm$action2=="" & (nm$when==2021|nm$when==2022))
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
nm[sel.tmp, c("alta", "baja", "action", "p18_2005", "p18_2010", "p18_2020")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    action <- action2; orig.dest <- orig.dest2; when <- when2
})
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
})
## Split.from in 2019-2020: flat
sel.tmp <- which(nm$action=="split.from" & nm$action2=="" & (nm$when==2019|nm$when==2020))
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
nm[sel.tmp, c("alta", "baja", "action", "p18_2005", "p18_2010", "p18_2020")]
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_2005 <- p18_2010 <- p18_2020
    dmanip <- 1
    action <- action2; orig.dest <- orig.dest2; when <- when2
})
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    action2 <- action3; orig.dest2 <- orig.dest3; when2 <- when3
})

## Split.from in 2019: 
sel.tmp <- which(nm$action=="split.from" & nm$action2=="" & nm$when==2017)
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
nm[sel.tmp, c("seccion", "alta", "baja", "action", "p18_2005", "p18_2010", "p18_2020")]


table(one=nm$action , two  =nm$action2, useNA = "ifany")
table(two=nm$action2, three=nm$action3, useNA = "ifany")
table(nm$action)
table(nm$action2)
table(nm$action3)

sel.tmp <- which(nm$action2=="merged.to" & nm$action3=="merged.to")
nm[sel.tmp, c("seccion", "alta", "baja", "action", "action2", "when", "when2", "p18_2005", "p18_2010", "p18_2020")]
x

## 5.1 Verify no mun with zero pop
nm$ife2006[which(nm$p18m06_05==0)]
nm$ife2006[which(nm$p18m06_10==0)]
nm$ife2006[which(nm$p18m06_20==0)]
print("All are municipios created recently before 2006")
nm[which(nm$ife2006==4011), c(32, grep("p18.*_(2005|2010|2020)", colnames(nm)), grep("p18m06_(05|10|20)", colnames(nm)))]

## 5.2 Indicate single-sección municipios
tmp <- split(x=nm, f=nm$ife2006)
tmp <- lapply(tmp, function(x){
    n <- nrow(x)
    n <- data.frame(ife2006=NA, n=n)
    return(n)
    })
tmp <- do.call(rbind, tmp)
tmp$ife2006 <- as.numeric(rownames(tmp))
nm$dsingle[which(nm$ife2006  %in% tmp$ife2006[tmp$n==1])] <- 1

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

sel.tmp <- which(nm$baja==2022)#p18_2005) & is.na(nm$p18_2010) & nm$baja==2002)
with(nm[sel.tmp,], table(action=action , when=when , useNA = "ifany"))
with(nm[sel.tmp,], table(action=action2, when=when2, useNA = "ifany"))
with(nm[sel.tmp,], table(action=action3, when=when3, useNA = "ifany"))

table(nm$baja)

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

sel.tmp <- which(is.na(nm$p18_2005) & is.na(nm$p18_2010))
nm[sel.tmp,c("p18_2005","p18_2010","p18_2020")]
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
table(alta=nm$alta[sel.tmp], baja=nm$action[sel.tmp], useNA = "ifany")
table(act=nm$action[sel.tmp], act2=nm$action2[sel.tmp], useNA = "ifany")
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



table(nm$p18_2005==0)
table(nm$p18_2010==0)
table(nm$p18_2020==0)
table(is.na(nm$p18_2005))
table(is.na(nm$p18_2010))
table(is.na(nm$p18_2020))

with(nm[nm$action=="split.to",], table(when=when, baja=baja))
dim(nm)
dim(nm.done)


sel.tmp <- which(is.na(nm$p18_2005) & is.na(nm$p18_2010) & is.na(nm$p18_2020) & nm$baja<=2005)
table(act=nm$action[sel.tmp], act2=nm$action2[sel.tmp], useNA = "ifany")

nm.done <- rbind(nm.done, nm[ sel.tmp,])
nm <-                     nm[-sel.tmp,]


sel.tmp <- which(is.na(nm$p18_2005) & is.na(nm$p18_2010) & is.na(nm$p18_2020))
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
table(act=nm$action[sel.tmp], act2=nm$action2[sel.tmp], useNA = "ifany")
nm[sel.tmp,] <- within(nm[sel.tmp,], {
    p18_1994 <- p18_1997 <- p18_2003 <- p18_2005 <- p18_2006 <- p18_2009 <- p18_2010 <- p18_2012 <- p18_2015 <- p18_2018 <- p18_2020 <- p18_2021 <- 10
})
nm.done <- rbind(nm.done, nm[ sel.tmp,])
nm <-                     nm[-sel.tmp,]

## will get flat 2005 pop until
sel.tmp <- which(is.na(nm$p18_2010) & is.na(nm$p18_2020))
table(alta=nm$alta[sel.tmp], baja=nm$baja[sel.tmp], useNA = "ifany")
table(act=nm$action[sel.tmp], act2=nm$action2[sel.tmp], useNA = "ifany")
nm[sel.tmp,c("p18_2005","p18_2010","p18_2020")]

dim(nm)
dim(nm.done)

## 6. sel.r <- seccion split in 2020 cases, round 1
sel.r <- which(nm$action=="split.to")
table(nm$when[sel.r])
##
sel.r <- which(nm$action=="split.to" & nm$when==2020)

## 7. Subset nm[sel.r,] sh[sel.r,] r[sel.r,] 
nm.w <- nm[sel.r,]  ## subset

## 8. Save 2020 values to restore later
nm.saved <- nm.w[, colnames(nm)[grep("_2020", colnames(nm))]]

## 9. Apply sum.split to add-up pre-split population
nm.w[1:3, grep("p18_(2005|2010|2020)", colnames(nm))]
nm.w <- sum.split(d=nm.w, year.var=2020, rnd=1)

## 10. Return obs to nm in order to compute r with pre-split pop (1st seccion in each mun is the reference)
nm[sel.r,] <- nm.w


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

