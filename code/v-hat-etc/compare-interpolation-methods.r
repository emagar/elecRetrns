source("../../code/v-hat-etc/interpolate-census.r")

## function to interpolate and stats to compare
comp <- function(what="p18", yr=NA, unit="m", census.data = NA){
    tmp2 <- censom00[, c("edon","inegi","ife")]
    tmp2 <- within(tmp2,{
        pob18l <- lss <- lr2 <- NA                                                         # slots for regression predictions/stats
        pob18e <- ess <- er2 <- NA                                                         # slots for regression predictions/stats
        pob18t <- interpol(what=what, yr=yr, unit=unit, census.data=census.data)           # segments predictions
    })
    tmp3 <- interlog(what=what, yr=yr, unit=unit, census.data=census.data, frm="log(dv)~iv")  # log models
    tmp2$pob18e <- tmp3[[1]]                                                                   # log predictions
    non.nas <- which(!is.na(tmp3[[1]]))                                                        # skip NAs
    tmp2$er2[non.nas] <- unlist(lapply(tmp3[[2]][non.nas], function(x) x$r2[1]))               # regression r2s
    tmp2$ess <- unlist(lapply(tmp3[[2]], function(x) round(sum(x$abs.resid / mean(x$dv)), 3))) # sum of abs resid / mean pop
    ##
    tmp3 <- interlog(what=what, yr=yr, unit=unit, census.data=census.data, frm="dv~iv")       # linear models
    tmp2$pob18l <- tmp3[[1]]                                                                   # liner predictions
    tmp2$lr2[non.nas] <- unlist(lapply(tmp3[[2]][non.nas], function(x) x$r2[1]))               # regression r2s
    tmp2$lss <- unlist(lapply(tmp3[[2]], function(x) round(sum(x$abs.resid / mean(x$dv)), 3))) # sum of abs resid / mean pop
    return(tmp2)
}

#######################################
## Add p18 to municipal vote objects ##
#######################################
## 1991
tmp2 <- comp(what="p18", yr=1991, unit="m", census.data = censom00)
##
v91m$pob18t   <- tmp2$pob18t # paste interpolation
v91m$pob18e   <- tmp2$pob18e # paste interpolation
v91m$pob18ess <- tmp2$pob18ess                    
v91m$pob18er2 <- tmp2$pob18er2                    
v91m$pob18l   <- tmp2$pob18l # paste interpolation
v91m$pob18lss <- tmp2$pob18lss                    
v91m$pob18lr2 <- tmp2$pob18lr2                    
## ## 1994
## tmp2 <- censom94[, c("edon","inegi","ife")]
## tmp2$pob18t <- interpol(what="p18", yr=1994, unit="m")
## tmp2$pob18e <- interlog(what="p18", yr=1994, unit="m", frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=1994, unit="m", frm="dv~iv")
## v94m$pob18t <- tmp2$pob18t # paste interpolation
## v94m$pob18e <- tmp2$pob18e # paste interpolation
## v94m$pob18l <- tmp2$pob18l # paste interpolation
## ## 1994m97
## tmp2 <- censom97[, c("edon","inegi","ife")]
## tmp2$pob18t <- interpol(what="p18", yr=1994, unit="m", census.data = censom97)
## tmp2$pob18e <- interlog(what="p18", yr=1994, unit="m", census.data = censom97, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=1994, unit="m", census.data = censom97, frm="dv~iv")
## v94m97$pob18t <- tmp2$pob18t # paste interpolation
## v94m97$pob18e <- tmp2$pob18e # paste interpolation
## v94m97$pob18l <- tmp2$pob18l # paste interpolation

## 1994m00
tmp2 <- comp(what="p18", yr=1994, unit="m", census.data = censom00)
##
v94m00$pob18t   <- tmp2$pob18t # paste interpolation
v94m00$pob18e   <- tmp2$pob18e # paste interpolation
v94m00$pob18ess <- tmp2$pob18ess
v94m00$pob18er2 <- tmp2$pob18er2
v94m00$pob18l   <- tmp2$pob18l # paste interpolation
v94m00$pob18lss <- tmp2$pob18lss
v94m00$pob18lr2 <- tmp2$pob18lr2                    
## ## 1994m03
## ## 1994m06
## ## 1994m09
## ## 1994m12
## ## 1994m15
## ## 1994m18
## ## 1994m21

## ## 1997
## ## 1997m94
## 1997m00
tmp2 <- comp(what="p18", yr=1997, unit="m", census.data = censom00)
##
v97m00$pob18t   <- tmp2$pob18t # paste interpolation
v97m00$pob18e   <- tmp2$pob18e # paste interpolation
v97m00$pob18ess <- tmp2$pob18ess                    
v97m00$pob18er2 <- tmp2$pob18er2                    
v97m00$pob18l   <- tmp2$pob18l # paste interpolation
v97m00$pob18lss <- tmp2$pob18lss                    
v97m00$pob18lr2 <- tmp2$pob18lr2                    
## ## 1997m03
## ## 1997m06
## ## 1997m09
## ## 1997m12
## ## 1997m15
## ## 1997m18
## ## 1997m21

## 2000
tmp2 <- comp(what="p18", yr=2000, unit="m", census.data = censom00)
##
v00m$pob18t   <- tmp2$pob18t # paste interpolation
v00m$pob18e   <- tmp2$pob18e # paste interpolation
v00m$pob18ess <- tmp2$pob18ess                    
v00m$pob18er2 <- tmp2$pob18er2                    
v00m$pob18l   <- tmp2$pob18l # paste interpolation
v00m$pob18lss <- tmp2$pob18lss                    
v00m$pob18lr2 <- tmp2$pob18lr2                    
## ## 2000m94
## ## 2000m97
## ## 2000m03
## ## 2000m06
## ## 2000m09
## ## 2000m12
## ## 2000m15
## ## 2000m18
## ## 2000m21

## ## 2003
## ## 2003m94
## ## 2003m97
## 2003m00
tmp2 <- comp(what="p18", yr=2003, unit="m", census.data = censom00)
##
v03m00$pob18t   <- tmp2$pob18t # paste interpolation
v03m00$pob18e   <- tmp2$pob18e # paste interpolation
v03m00$pob18ess <- tmp2$pob18ess                    
v03m00$pob18er2 <- tmp2$pob18er2                    
v03m00$pob18l   <- tmp2$pob18l # paste interpolation
v03m00$pob18lss <- tmp2$pob18lss                    
v03m00$pob18lr2 <- tmp2$pob18lr2                    
## ## 2003m06
## ## 2003m09
## ## 2003m12
## ## 2003m15
## ## 2003m18
## ## 2003m21

## ## 2006
## ## 2006m94
## ## 2006m97
## 2006m00
tmp2 <- comp(what="p18", yr=2006, unit="m", census.data = censom00)
##
v06m00$pob18t   <- tmp2$pob18t # paste interpolation
v06m00$pob18e   <- tmp2$pob18e # paste interpolation
v06m00$pob18ess <- tmp2$pob18ess                    
v06m00$pob18er2 <- tmp2$pob18er2                    
v06m00$pob18l   <- tmp2$pob18l # paste interpolation
v06m00$pob18lss <- tmp2$pob18lss                    
v06m00$pob18lr2 <- tmp2$pob18lr2                    
## ## 2006m03
## ## 2006m09
## ## 2006m12
## ## 2006m15
## ## 2006m18
## ## 2006m21

## ## 2009
## ## 2009m94
## ## 2009m97
## 2009m00
tmp2 <- comp(what="p18", yr=2009, unit="m", census.data = censom00)
##
v09m00$pob18t   <- tmp2$pob18t # paste interpolation
v09m00$pob18e   <- tmp2$pob18e # paste interpolation
v09m00$pob18ess <- tmp2$pob18ess                    
v09m00$pob18er2 <- tmp2$pob18er2                    
v09m00$pob18l   <- tmp2$pob18l # paste interpolation
v09m00$pob18lss <- tmp2$pob18lss                    
v09m00$pob18lr2 <- tmp2$pob18lr2                    
## ## 2009m03
## ## 2009m06
## ## 2009m12
## ## 2009m15
## ## 2009m18
## ## 2009m21

## ## 2012
## ## 2012m94
## ## 2012m97
## 2012m00
tmp2 <- comp(what="p18", yr=2012, unit="m", census.data = censom00)
##
v12m00$pob18t   <- tmp2$pob18t # paste interpolation
v12m00$pob18e   <- tmp2$pob18e # paste interpolation
v12m00$pob18ess <- tmp2$pob18ess                    
v12m00$pob18er2 <- tmp2$pob18er2                    
v12m00$pob18l   <- tmp2$pob18l # paste interpolation
v12m00$pob18lss <- tmp2$pob18lss                    
v12m00$pob18lr2 <- tmp2$pob18lr2                    
## ## 2012m03
## ## 2012m06
## ## 2012m09
## ## 2012m15
## ## 2012m18
## ## 2012m21

## ## 2015
## ## 2015m94
## ## 2015m97
## 2015m00
tmp2 <- comp(what="p18", yr=2015, unit="m", census.data = censom00)
##
v15m00$pob18t   <- tmp2$pob18t # paste interpolation
v15m00$pob18e   <- tmp2$pob18e # paste interpolation
v15m00$pob18ess <- tmp2$pob18ess                    
v15m00$pob18er2 <- tmp2$pob18er2                    
v15m00$pob18l   <- tmp2$pob18l # paste interpolation
v15m00$pob18lss <- tmp2$pob18lss                    
v15m00$pob18lr2 <- tmp2$pob18lr2                    
## ## 2015m03
## ## 2015m06
## ## 2015m09
## ## 2015m12
## ## 2015m18
## ## 2015m21

## ## 2018
## ## 2018m94
## ## 2018m97
## 2018m00
tmp2 <- comp(what="p18", yr=2018, unit="m", census.data = censom00)
##
v18m00$pob18t   <- tmp2$pob18t # paste interpolation
v18m00$pob18e   <- tmp2$pob18e # paste interpolation
v18m00$pob18ess <- tmp2$pob18ess                    
v18m00$pob18er2 <- tmp2$pob18er2                    
v18m00$pob18l   <- tmp2$pob18l # paste interpolation
v18m00$pob18lss <- tmp2$pob18lss                    
v18m00$pob18lr2 <- tmp2$pob18lr2                    
## ## 2018m03
## ## 2018m06
## ## 2018m09
## ## 2018m12
## ## 2018m15
## ## 2018m21

## ## 2021
## ## 2021m94
## ## 2021m97
## 2021m00
tmp2 <- comp(what="p18", yr=2021, unit="m", census.data = censom00)
##
v21m00$pob18t   <- tmp2$pob18t # paste interpolation
v21m00$pob18e   <- tmp2$pob18e # paste interpolation
v21m00$pob18ess <- tmp2$pob18ess                    
v21m00$pob18er2 <- tmp2$pob18er2                    
v21m00$pob18l   <- tmp2$pob18l # paste interpolation
v21m00$pob18lss <- tmp2$pob18lss                    
v21m00$pob18lr2 <- tmp2$pob18lr2                    
## ## 2021m03
## ## 2021m06
## ## 2021m09
## ## 2021m12
## ## 2021m15
## ## 2021m18



## all same sorted?
v94m00[1,]
censom00[1,]
table(v94m00$ife==censom00$ife)


## ## compare linear and log projections in census years
## tmp <- data.frame(
##     edon = censom00$edon,
##     inegi= censom00$inegi,
##     ife  = censom00$ife
## )
## tmp$c95  <- censom00$p18_1995
## tmp$c00  <- censom00$p18_2000
## tmp$c05  <- censom00$p18_2005
## tmp$c10  <- censom00$p18_2010
## tmp$c20  <- censom00$p18_2020
## tmp$li95 <- interlog(what="p18", yr=1995, unit="m", census.data = censom00, frm="dv~iv")
## tmp$li00 <- interlog(what="p18", yr=2000, unit="m", census.data = censom00, frm="dv~iv")
## tmp$li05 <- interlog(what="p18", yr=2005, unit="m", census.data = censom00, frm="dv~iv")
## tmp$li10 <- interlog(what="p18", yr=2010, unit="m", census.data = censom00, frm="dv~iv")
## tmp$li20 <- interlog(what="p18", yr=2020, unit="m", census.data = censom00, frm="dv~iv")
## tmp$lg95 <- interlog(what="p18", yr=1995, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp$lg00 <- interlog(what="p18", yr=2000, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp$lg05 <- interlog(what="p18", yr=2005, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp$lg10 <- interlog(what="p18", yr=2010, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp$lg20 <- interlog(what="p18", yr=2020, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp$tw95 <- interpol(what="p18", yr=1995, unit="m", census.data = censom00)
## tmp$tw00 <- interpol(what="p18", yr=2000, unit="m", census.data = censom00)
## tmp$tw05 <- interpol(what="p18", yr=2005, unit="m", census.data = censom00)
## tmp$tw10 <- interpol(what="p18", yr=2010, unit="m", census.data = censom00)
## tmp$tw20 <- interpol(what="p18", yr=2020, unit="m", census.data = censom00)

## tmp <- tmp[moveme(names(tmp), "li95 after lg95")]
## tmp <- tmp[moveme(names(tmp), "tw95 after li95")]
## tmp <- tmp[moveme(names(tmp), "li00 after lg00")]
## tmp <- tmp[moveme(names(tmp), "tw00 after li00")]
## tmp <- tmp[moveme(names(tmp), "li05 after lg05")]
## tmp <- tmp[moveme(names(tmp), "tw05 after li05")]
## tmp <- tmp[moveme(names(tmp), "li10 after lg10")]
## tmp <- tmp[moveme(names(tmp), "tw10 after li10")]
## tmp <- tmp[moveme(names(tmp), "li20 after lg20")]
## tmp <- tmp[moveme(names(tmp), "tw20 after li20")]

## str(tmp)
## tmp[1:5,]

## ## stats to compare li and lg census projections
## tmp <- within(tmp, {
##     tmp1 <- ifelse(lg95 - c95==0, 1, lg95 - c95) ## if 0 add 1
##     tmp2 <- ifelse(li95 - c95==0, 1, li95 - c95)
##     tmp3 <- ifelse(tw95 - c95==0, 1, tw95 - c95)
##     dlg95 <- tmp1 / c95
##     dli95 <- tmp2 / c95
##     dtw95 <- tmp3 / c95
##     dif95 <-   c95/abs(tmp1) - c95/abs(tmp2)
##     off95 <- ((   abs(tmp1) +   abs(tmp2) ) / 2) / c95
##     tmp1 <-   ifelse(lg00 - c00==0, 1, lg00 - c00)
##     tmp2 <-   ifelse(li00 - c00==0, 1, li00 - c00)
##     tmp3 <-   ifelse(tw00 - c00==0, 1, tw00 - c00)
##     dlg00 <- tmp1 / c00
##     dli00 <- tmp2 / c00
##     dtw00 <- tmp3 / c00
##     dif00 <-   c00/abs(tmp1) - c00/abs(tmp2)
##     off00 <- ((   abs(tmp1) +   abs(tmp2) ) / 2) / c00
##     tmp1 <-   ifelse(lg05 - c05==0, 1, lg05 - c05)
##     tmp2 <-   ifelse(li05 - c05==0, 1, li05 - c05)
##     tmp2 <-   ifelse(tw05 - c05==0, 1, tw05 - c05)
##     dlg05 <- tmp1 / c05
##     dli05 <- tmp2 / c05
##     dtw05 <- tmp3 / c05
##     dif05 <-   c05/abs(tmp1) - c05/abs(tmp2)
##     off05 <- ((   abs(tmp1) +   abs(tmp2) ) / 2) / c05
##     tmp1 <-   ifelse(lg10 - c10==0, 1, lg10 - c10)
##     tmp2 <-   ifelse(li10 - c10==0, 1, li10 - c10)
##     tmp3 <-   ifelse(tw10 - c10==0, 1, tw10 - c10)
##     dlg10 <- tmp1 / c10
##     dli10 <- tmp2 / c10
##     dtw10 <- tmp3 / c10
##     dif10 <-   c10/abs(tmp1) - c10/abs(tmp2)
##     off10 <- ((   abs(tmp1) +   abs(tmp2) ) / 2) / c10
##     tmp1 <-   ifelse(lg20 - c20==0, 1, lg20 - c20)
##     tmp2 <-   ifelse(li20 - c20==0, 1, li20 - c20)
##     tmp3 <-   ifelse(tw20 - c20==0, 1, tw20 - c20)
##     dlg20 <- tmp1 / c20
##     dli20 <- tmp2 / c20
##     dtw20 <- tmp3 / c20
##     dif20 <-   c20/abs(tmp1) - c20/abs(tmp2)
##     off20 <- ((   abs(tmp1) +   abs(tmp2) ) / 2) / c20
## })
## tmp$tmp1 <- tmp$tmp2 <- tmp$tmp3 <- NULL
## tmp[,grep("^dif|^off|^dlg|^dli|^dtw", colnames(tmp))] <- round(tmp[,grep("^dif|^off|^dlg|^dli|^dtw", colnames(tmp))], 2)
## tmp <- tmp[moveme(names(tmp), "dlg00 after dlg95")]
## tmp <- tmp[moveme(names(tmp), "dlg05 after dlg00")]
## tmp <- tmp[moveme(names(tmp), "dlg10 after dlg05")]
## tmp <- tmp[moveme(names(tmp), "dlg20 after dlg10")]
## tmp <- tmp[moveme(names(tmp), "dli95 after dlg95")]
## tmp <- tmp[moveme(names(tmp), "dli00 after dlg00")]
## tmp <- tmp[moveme(names(tmp), "dli05 after dlg05")]
## tmp <- tmp[moveme(names(tmp), "dli10 after dlg10")]
## tmp <- tmp[moveme(names(tmp), "dli20 after dlg20")]
## tmp <- tmp[moveme(names(tmp), "dtw95 after dli95")]
## tmp <- tmp[moveme(names(tmp), "dtw00 after dli00")]
## tmp <- tmp[moveme(names(tmp), "dtw05 after dli05")]
## tmp <- tmp[moveme(names(tmp), "dtw10 after dli10")]
## tmp <- tmp[moveme(names(tmp), "dtw20 after dli20")]
## tmp <- tmp[moveme(names(tmp), "dif95 after dtw95")]
## tmp <- tmp[moveme(names(tmp), "dif00 after dtw00")]
## tmp <- tmp[moveme(names(tmp), "dif05 after dtw05")]
## tmp <- tmp[moveme(names(tmp), "dif10 after dtw10")]
## tmp <- tmp[moveme(names(tmp), "dif20 after dtw20")]
## tmp <- tmp[moveme(names(tmp), "off95 after dif95")]
## tmp <- tmp[moveme(names(tmp), "off00 after dif00")]
## tmp <- tmp[moveme(names(tmp), "off05 after dif05")]
## tmp <- tmp[moveme(names(tmp), "off10 after dif10")]
## tmp <- tmp[moveme(names(tmp), "off20 after dif20")]

## tmp[1,]

## ## Interpretation:
## ## dif=0 -- log and linear perform identical 
## ## dif>0 -- log   outperforms linear
## ## dif<0 -- log underperforms linear
## ## off is mean absolute dev from census
## tmp[1:10,grep("inegi|^dlg|^dli|^dif|^off", colnames(tmp))]
## tmp[1:10,grep("inegi|^dlg95|^dli95|^dif95|^off95", colnames(tmp))]
## tmp[1:10,grep("inegi|95", colnames(tmp))]
## round(quantile(tmp$dif95, probs = seq(0,1,.025), na.rm=TRUE),1)
## round(quantile(tmp$dlg95, probs = seq(0,1,.025), na.rm=TRUE),1)
## round(quantile(tmp$dli95, probs = seq(0,1,.025), na.rm=TRUE),1)
## round(quantile(tmp$dif00, probs = seq(0,1,.025), na.rm=TRUE),1)
## round(quantile(tmp$dif05, probs = seq(0,1,.025), na.rm=TRUE),1)
## summary(tmp$off95)
## round(apply(tmp[,grep("^dif|^off", colnames(tmp))], 2, summary), 3)

## summary(tmp$dli95 - tmp$dtw95)



## ## yearly estimates prep for use in plots
## tmp2 <- data.frame(
##     li94=interlog(what="p18", yr=1994, unit="m", census.data = censom00, frm="dv~iv")
##     )
## tmp2$li95 <- interlog(what="p18", yr=1995, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li96 <- interlog(what="p18", yr=1996, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li97 <- interlog(what="p18", yr=1997, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li98 <- interlog(what="p18", yr=1998, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li99 <- interlog(what="p18", yr=1999, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li00 <- interlog(what="p18", yr=2000, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li01 <- interlog(what="p18", yr=2001, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li02 <- interlog(what="p18", yr=2002, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li03 <- interlog(what="p18", yr=2003, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li04 <- interlog(what="p18", yr=2004, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li05 <- interlog(what="p18", yr=2005, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li06 <- interlog(what="p18", yr=2006, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li07 <- interlog(what="p18", yr=2007, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li08 <- interlog(what="p18", yr=2008, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li09 <- interlog(what="p18", yr=2009, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li10 <- interlog(what="p18", yr=2010, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li11 <- interlog(what="p18", yr=2011, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li12 <- interlog(what="p18", yr=2012, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li13 <- interlog(what="p18", yr=2013, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li14 <- interlog(what="p18", yr=2014, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li15 <- interlog(what="p18", yr=2015, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li16 <- interlog(what="p18", yr=2016, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li17 <- interlog(what="p18", yr=2017, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li18 <- interlog(what="p18", yr=2018, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li19 <- interlog(what="p18", yr=2019, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li20 <- interlog(what="p18", yr=2020, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li21 <- interlog(what="p18", yr=2021, unit="m", census.data = censom00, frm="dv~iv")
## ##
## tmp2$lg94 <- interlog(what="p18", yr=1994, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg95 <- interlog(what="p18", yr=1995, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg96 <- interlog(what="p18", yr=1996, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg97 <- interlog(what="p18", yr=1997, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg98 <- interlog(what="p18", yr=1998, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg99 <- interlog(what="p18", yr=1999, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg00 <- interlog(what="p18", yr=2000, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg01 <- interlog(what="p18", yr=2001, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg02 <- interlog(what="p18", yr=2002, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg03 <- interlog(what="p18", yr=2003, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg04 <- interlog(what="p18", yr=2004, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg05 <- interlog(what="p18", yr=2005, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg06 <- interlog(what="p18", yr=2006, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg07 <- interlog(what="p18", yr=2007, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg08 <- interlog(what="p18", yr=2008, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg09 <- interlog(what="p18", yr=2009, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg10 <- interlog(what="p18", yr=2010, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg11 <- interlog(what="p18", yr=2011, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg12 <- interlog(what="p18", yr=2012, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg13 <- interlog(what="p18", yr=2013, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg14 <- interlog(what="p18", yr=2014, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg15 <- interlog(what="p18", yr=2015, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg16 <- interlog(what="p18", yr=2016, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg17 <- interlog(what="p18", yr=2017, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg18 <- interlog(what="p18", yr=2018, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg19 <- interlog(what="p18", yr=2019, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg20 <- interlog(what="p18", yr=2020, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg21 <- interlog(what="p18", yr=2021, unit="m", census.data = censom00, frm="log(dv)~iv")

 

## Inspect census figs
myplot <- function(i=NA, print=FALSE){
    if (print==TRUE) png(paste0("/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/plots/", "mu", censom00$inegi[i], "p18-interlog.png"))
    plot(  seq(1994,2021,3), li,
         xlim = c(1988,2027),
         ylim = c(0,1.3*max(li,lg,ls   )), col="blue", xlab="", ylab="voting age population", main = paste(censom00$inegi[i], censom00$mun[i], "Coahuila"), sub="blue=linear model, red=log-linear model, solid=census")
    ##plot(  seq(1994,2021,3), li, ylim = c(min(li,lg,ls,ef),max(li,lg,ls,ef)), main = paste(i, "bck=line, red=log, blue=lis, grn=efec"))
    points(seq(1994,2021,3), lg, col = "red")
    ##points(seq(1994,2021,3), ls, col = "blue")
    ##points(seq(1994,2021,3), ef, col = "green")
    points(1995, c1, pch = 19, cex = .75)
    points(  2000,            c2, pch = 19, cex = .75)
    points(c(2005,2010,2020), censom00[i,c("p18_2005","p18_2010","p18_2020")], pch=19, cex=.75)
    legend("bottomright",
           legend = c(paste0("R^2 = ", v15m00$pob18er2[i]), paste0("R^2 = ", v15m00$pob18lr2[i])),
           pch = c(1,1), col = c("red","blue"))
    ##abline(v=2000, lty=2)
##
## points(1988, interlog(what="p18", yr=1988, unit="m", census.data = censom00, frm="log(dv)~iv")[i], col = "red")
## points(1991, interlog(what="p18", yr=1991, unit="m", census.data = censom00, frm="log(dv)~iv")[i], col = "red")
## points(2024, interlog(what="p18", yr=2024, unit="m", census.data = censom00, frm="log(dv)~iv")[i], col = "red")
## points(2027, interlog(what="p18", yr=2027, unit="m", census.data = censom00, frm="log(dv)~iv")[i], col = "red")
## ##
## points(1988, interlog(what="p18", yr=1988, unit="m", census.data = censom00, frm="dv~iv")[i], col = "blue")
## points(1991, interlog(what="p18", yr=1991, unit="m", census.data = censom00, frm="dv~iv")[i], col = "blue")
## points(2024, interlog(what="p18", yr=2024, unit="m", census.data = censom00, frm="dv~iv")[i], col = "blue")
## points(2027, interlog(what="p18", yr=2027, unit="m", census.data = censom00, frm="dv~iv")[i], col = "blue")
##
    if (print==TRUE) dev.off()
}

11
60

i <- i+1
ls <- c(
    v94m00$lisnom[i],
    v97m00$lisnom[i],
    v00m  $lisnom[i],
    v03m00$lisnom[i],
    v06m00$lisnom[i],
    v09m00$lisnom[i],
    v12m00$lisnom[i],
    v15m00$lisnom[i],
    v18m00$lisnom[i],
    v21m00$lisnom[i]
)
ef <- c(
    v94m00$efec[i],
    v97m00$efec[i],
    v00m  $efec[i],
    v03m00$efec[i],
    v06m00$efec[i],
    v09m00$efec[i],
    v12m00$efec[i],
    v15m00$efec[i],
    v18m00$efec[i],
    v21m00$efec[i]
)
li <- c(
    v94m00$pob18l[i],
    v97m00$pob18l[i],
    v00m  $pob18l[i],
    v03m00$pob18l[i],
    v06m00$pob18l[i],
    v09m00$pob18l[i],
    v12m00$pob18l[i],
    v15m00$pob18l[i],
    v18m00$pob18l[i],
    v21m00$pob18l[i]
)
lg <- c(
    v94m00$pob18e[i],
    v97m00$pob18e[i],
    v00m  $pob18e[i],
    v03m00$pob18e[i],
    v06m00$pob18e[i],
    v09m00$pob18e[i],
    v12m00$pob18e[i],
    v15m00$pob18e[i],
    v18m00$pob18e[i],
    v21m00$pob18e[i]
)
lgr2 <- c(
    v94m00$pob18er2[i],
    v97m00$pob18er2[i],
    v00m  $pob18er2[i],
    v03m00$pob18er2[i],
    v06m00$pob18er2[i],
    v09m00$pob18er2[i],
    v12m00$pob18er2[i],
    v15m00$pob18er2[i],
    v18m00$pob18er2[i],
    v21m00$pob18er2[i]
)
lir2 <- c(
    v94m00$pob18lr2[i],
    v97m00$pob18lr2[i],
    v00m  $pob18lr2[i],
    v03m00$pob18lr2[i],
    v06m00$pob18lr2[i],
    v09m00$pob18lr2[i],
    v12m00$pob18lr2[i],
    v15m00$pob18lr2[i],
    v18m00$pob18lr2[i],
    v21m00$pob18lr2[i]
)
tw <- c(
    v94m00$pob18t[i],
    v97m00$pob18t[i],
    v00m  $pob18t[i],
    v03m00$pob18t[i],
    v06m00$pob18t[i],
    v09m00$pob18t[i],
    v12m00$pob18t[i],
    v15m00$pob18t[i],
    v18m00$pob18t[i],
    v21m00$pob18t[i]
)
c1 <- as.numeric(censom00$p18_1995[i])
c2 <- as.numeric(censom00$p18_2000[i])
##
myplot(i, print=FALSE)


## RR report r2s


mysum <- function(x){
    quantile(x, probs = c(0,.01,.025,.05,.1,.25,.5,.75,.9,.95,.975,.99,1), na.rm = TRUE)
}



round(mysum(v21m00$pob18er2), 3)
round(mysum(v21m00$pob18lr2), 3)

round(quantile(v21m00$pob18er2, seq(0,1,.05), na.rm=TRUE),3)




