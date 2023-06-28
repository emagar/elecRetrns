source("../../code/v-hat-etc/interpolate-census.r")

## function to interpolate and stats to compare
comp <- function(what="p18", yr=NA, unit="m", census.data = NA){
    tmp2 <- censom00[, c("edon","inegi","ife")]
    tmp2 <- within(tmp2,{
        pob18l <- lss <- lr2 <- NA                                                         # slots for regression predictions/stats
        pob18e <- ess <- er2 <- NA                                                         # slots for regression predictions/stats
        pob18t <- interpol(what=what, yr=yr, unit=unit, census.data=census.data)           # segments predictions
    })
    tmp3 <- interlog(what=what, yr=yr, unit=unit, census.data=census.data, frm="log(dv)~iv")          # log models
    tmp2$pob18e <- tmp3[[1]]                                                                          # log predictions
    non.nas <- which(!is.na(tmp3[[1]]))                                                               # skip NAs
    tmp2$er2[non.nas] <- unlist(lapply(tmp3[[2]][non.nas], function(x) x$r2[1]))                      # regression r2s
    tmp2$ess <- unlist(lapply(tmp3[[2]], function(x) round(mean(sqrt(x$sq.resid)) / mean(x$dv) , 3))) # quadratic mean resid / mean pop
    ##
    tmp3 <- interlog(what=what, yr=yr, unit=unit, census.data=census.data, frm="dv~iv")               # linear models
    tmp2$pob18l <- tmp3[[1]]                                                                          # liner predictions
    tmp2$lr2[non.nas] <- unlist(lapply(tmp3[[2]][non.nas], function(x) x$r2[1]))                      # regression r2s
    tmp2$lss <- unlist(lapply(tmp3[[2]], function(x) round(mean(sqrt(x$sq.resid)) / mean(x$dv) , 3))) # quadratic mean resid / mean pop
    return(tmp2)
}

#######################################
## Add p18 to municipal vote objects ##
#######################################
## 1991
tmp2 <- comp(what="p18", yr=1991, unit="m", census.data = censom00)
##
v91m$p18t   <- tmp2$pob18t # paste interpolation
v91m$p18e   <- tmp2$pob18e # paste interpolation
v91m$ess    <- tmp2$ess                    
v91m$er2    <- tmp2$er2                    
v91m$p18l   <- tmp2$pob18l # paste interpolation
v91m$lss    <- tmp2$lss                    
v91m$lr2    <- tmp2$lr2                    
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
v94m00$p18t   <- tmp2$pob18t # paste interpolation
v94m00$p18e   <- tmp2$pob18e # paste interpolation
v94m00$ess    <- tmp2$ess
v94m00$er2    <- tmp2$er2
v94m00$p18l   <- tmp2$pob18l # paste interpolation
v94m00$lss    <- tmp2$lss
v94m00$lr2    <- tmp2$lr2                    
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
v97m00$p18t   <- tmp2$pob18t # paste interpolation
v97m00$p18e   <- tmp2$pob18e # paste interpolation
v97m00$ess    <- tmp2$ess                    
v97m00$er2    <- tmp2$er2                    
v97m00$p18l   <- tmp2$pob18l # paste interpolation
v97m00$lss    <- tmp2$lss                    
v97m00$lr2    <- tmp2$lr2                    
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
v00m$p18t   <- tmp2$pob18t # paste interpolation
v00m$p18e   <- tmp2$pob18e # paste interpolation
v00m$ess    <- tmp2$ess                    
v00m$er2    <- tmp2$er2                    
v00m$p18l   <- tmp2$pob18l # paste interpolation
v00m$lss    <- tmp2$lss                    
v00m$lr2    <- tmp2$lr2                    
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
v03m00$p18t   <- tmp2$pob18t # paste interpolation
v03m00$p18e   <- tmp2$pob18e # paste interpolation
v03m00$ess    <- tmp2$ess                    
v03m00$er2    <- tmp2$er2                    
v03m00$p18l   <- tmp2$pob18l # paste interpolation
v03m00$lss    <- tmp2$lss                    
v03m00$lr2    <- tmp2$lr2                    
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
v06m00$p18t   <- tmp2$pob18t # paste interpolation
v06m00$p18e   <- tmp2$pob18e # paste interpolation
v06m00$ess    <- tmp2$ess                    
v06m00$er2    <- tmp2$er2                    
v06m00$p18l   <- tmp2$pob18l # paste interpolation
v06m00$lss    <- tmp2$lss                    
v06m00$lr2    <- tmp2$lr2                    
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
v09m00$p18t   <- tmp2$pob18t # paste interpolation
v09m00$p18e   <- tmp2$pob18e # paste interpolation
v09m00$ess    <- tmp2$ess                    
v09m00$er2    <- tmp2$er2                    
v09m00$p18l   <- tmp2$pob18l # paste interpolation
v09m00$lss    <- tmp2$lss                    
v09m00$lr2    <- tmp2$lr2                    
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
v12m00$p18t   <- tmp2$pob18t # paste interpolation
v12m00$p18e   <- tmp2$pob18e # paste interpolation
v12m00$ess    <- tmp2$ess                    
v12m00$er2    <- tmp2$er2                    
v12m00$p18l   <- tmp2$pob18l # paste interpolation
v12m00$lss    <- tmp2$lss                    
v12m00$lr2    <- tmp2$lr2                    
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
v15m00$p18t   <- tmp2$pob18t # paste interpolation
v15m00$p18e   <- tmp2$pob18e # paste interpolation
v15m00$ess    <- tmp2$ess                    
v15m00$er2    <- tmp2$er2                    
v15m00$p18l   <- tmp2$pob18l # paste interpolation
v15m00$lss    <- tmp2$lss                    
v15m00$lr2    <- tmp2$lr2                    
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
v18m00$p18t   <- tmp2$pob18t # paste interpolation
v18m00$p18e   <- tmp2$pob18e # paste interpolation
v18m00$ess    <- tmp2$ess                    
v18m00$er2    <- tmp2$er2                    
v18m00$p18l   <- tmp2$pob18l # paste interpolation
v18m00$lss    <- tmp2$lss                    
v18m00$lr2    <- tmp2$lr2                    
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
v21m00$p18t   <- tmp2$pob18t # paste interpolation
v21m00$p18e   <- tmp2$pob18e # paste interpolation
v21m00$ess    <- tmp2$ess                    
v21m00$er2    <- tmp2$er2                    
v21m00$p18l   <- tmp2$pob18l # paste interpolation
v21m00$lss    <- tmp2$lss                    
v21m00$lr2    <- tmp2$lr2                    
## ## 2021m03
## ## 2021m06
## ## 2021m09
## ## 2021m12
## ## 2021m15
## ## 2021m18

mysum <- function(x){
    prbs <- c(0,.01,.025,.05,.25,.5,.75,.95,.975,.99,1)
    y <- round(quantile(x, probs=prbs, na.rm=TRUE), digits=3)
    return(y)
}
## Any year has the map's statistics
mysum(v00m$ess)
mysum(v00m$lss)
mysum(v00m$er2)
mysum(v00m$lr2)

quantile(v00m$ess, probs=seq(0,1,.0001), na.rm = TRUE)
quantile(v00m$lss, probs=seq(0,1,.01), na.rm = TRUE)

## all same sorted?
v94m00[1,]
censom00[1,]
table(v94m00$ife==censom00$ife)

## ## compare linear and log projections in census years
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
           legend = c(paste0("RQMR = ", v00m$ess[i]), paste0("RQMR = ", v00m$lss[i])),
#           legend = c(paste0("R^2 = ", v15m00$pob18er2[i]), paste0("R^2 = ", v15m00$pob18lr2[i])),
           pch = c(1,1), col = c("red","blue"))
    ##abline(v=2000, lty=2)
##
## points(1988, interlog(what="p18", yr=1988, unit="m", census.data = censom00, frm="log(dv)~iv")[[1]][i], col = "red")
## points(1991, interlog(what="p18", yr=1991, unit="m", census.data = censom00, frm="log(dv)~iv")[[1]][i], col = "red")
## points(2024, interlog(what="p18", yr=2024, unit="m", census.data = censom00, frm="log(dv)~iv")[[1]][i], col = "red")
## points(2027, interlog(what="p18", yr=2027, unit="m", census.data = censom00, frm="log(dv)~iv")[[1]][i], col = "red")
## ##
## points(1988, interlog(what="p18", yr=1988, unit="m", census.data = censom00, frm="dv~iv")[[1]][i], col = "blue")
## points(1991, interlog(what="p18", yr=1991, unit="m", census.data = censom00, frm="dv~iv")[[1]][i], col = "blue")
## points(2024, interlog(what="p18", yr=2024, unit="m", census.data = censom00, frm="dv~iv")[[1]][i], col = "blue")
## points(2027, interlog(what="p18", yr=2027, unit="m", census.data = censom00, frm="dv~iv")[[1]][i], col = "blue")
##
    if (print==TRUE) dev.off()
}

i <- 60
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
    v94m00$p18l[i],
    v97m00$p18l[i],
    v00m  $p18l[i],
    v03m00$p18l[i],
    v06m00$p18l[i],
    v09m00$p18l[i],
    v12m00$p18l[i],
    v15m00$p18l[i],
    v18m00$p18l[i],
    v21m00$p18l[i]
)
lg <- c(
    v94m00$p18e[i],
    v97m00$p18e[i],
    v00m  $p18e[i],
    v03m00$p18e[i],
    v06m00$p18e[i],
    v09m00$p18e[i],
    v12m00$p18e[i],
    v15m00$p18e[i],
    v18m00$p18e[i],
    v21m00$p18e[i]
)
tw <- c(
    v94m00$p18t[i],
    v97m00$p18t[i],
    v00m  $p18t[i],
    v03m00$p18t[i],
    v06m00$p18t[i],
    v09m00$p18t[i],
    v12m00$p18t[i],
    v15m00$p18t[i],
    v18m00$p18t[i],
    v21m00$p18t[i]
)
c1 <- as.numeric(censom00$p18_1995[i])
c2 <- as.numeric(censom00$p18_2000[i])
##
myplot(i, print=FALSE)


#####################################################################################################################
## Analyze three-censuses projection in municipalities (as in secciones) while comparing to 1995 and 2000 censuses ##
#####################################################################################################################

## duplicate censo mun to drop two censuses
censom00.3pt <- censom00
## drop 1995 and 2000
censom00.3pt$p18_1995 <- censom00.3pt$p18_2000 <- NULL
censom00.3pt[1,]

## re-project
tmp2 <- comp(what="p18", yr=1991, unit="m", census.data = censom00.3pt)
##
v91m$p18t   <- tmp2$pob18t # paste interpolation
v91m$p18e   <- tmp2$pob18e # paste interpolation
v91m$ess    <- tmp2$ess                    
v91m$er2    <- tmp2$er2                    
v91m$p18l   <- tmp2$pob18l # paste interpolation
v91m$lss    <- tmp2$lss                    
v91m$lr2    <- tmp2$lr2                    
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
tmp2 <- comp(what="p18", yr=1994, unit="m", census.data = censom00.3pt)
##
v94m00$p18t   <- tmp2$pob18t # paste interpolation
v94m00$p18e   <- tmp2$pob18e # paste interpolation
v94m00$ess    <- tmp2$ess
v94m00$er2    <- tmp2$er2
v94m00$p18l   <- tmp2$pob18l # paste interpolation
v94m00$lss    <- tmp2$lss
v94m00$lr2    <- tmp2$lr2                    
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
tmp2 <- comp(what="p18", yr=1997, unit="m", census.data = censom00.3pt)
##
v97m00$p18t   <- tmp2$pob18t # paste interpolation
v97m00$p18e   <- tmp2$pob18e # paste interpolation
v97m00$ess    <- tmp2$ess                    
v97m00$er2    <- tmp2$er2                    
v97m00$p18l   <- tmp2$pob18l # paste interpolation
v97m00$lss    <- tmp2$lss                    
v97m00$lr2    <- tmp2$lr2                    
## ## 1997m03
## ## 1997m06
## ## 1997m09
## ## 1997m12
## ## 1997m15
## ## 1997m18
## ## 1997m21

## 2000
tmp2 <- comp(what="p18", yr=2000, unit="m", census.data = censom00.3pt)
##
v00m$p18t   <- tmp2$pob18t # paste interpolation
v00m$p18e   <- tmp2$pob18e # paste interpolation
v00m$ess    <- tmp2$ess                    
v00m$er2    <- tmp2$er2                    
v00m$p18l   <- tmp2$pob18l # paste interpolation
v00m$lss    <- tmp2$lss                    
v00m$lr2    <- tmp2$lr2                    
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
tmp2 <- comp(what="p18", yr=2003, unit="m", census.data = censom00.3pt)
##
v03m00$p18t   <- tmp2$pob18t # paste interpolation
v03m00$p18e   <- tmp2$pob18e # paste interpolation
v03m00$ess    <- tmp2$ess                    
v03m00$er2    <- tmp2$er2                    
v03m00$p18l   <- tmp2$pob18l # paste interpolation
v03m00$lss    <- tmp2$lss                    
v03m00$lr2    <- tmp2$lr2                    
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
tmp2 <- comp(what="p18", yr=2006, unit="m", census.data = censom00.3pt)
##
v06m00$p18t   <- tmp2$pob18t # paste interpolation
v06m00$p18e   <- tmp2$pob18e # paste interpolation
v06m00$ess    <- tmp2$ess                    
v06m00$er2    <- tmp2$er2                    
v06m00$p18l   <- tmp2$pob18l # paste interpolation
v06m00$lss    <- tmp2$lss                    
v06m00$lr2    <- tmp2$lr2                    
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
tmp2 <- comp(what="p18", yr=2009, unit="m", census.data = censom00.3pt)
##
v09m00$p18t   <- tmp2$pob18t # paste interpolation
v09m00$p18e   <- tmp2$pob18e # paste interpolation
v09m00$ess    <- tmp2$ess                    
v09m00$er2    <- tmp2$er2                    
v09m00$p18l   <- tmp2$pob18l # paste interpolation
v09m00$lss    <- tmp2$lss                    
v09m00$lr2    <- tmp2$lr2                    
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
tmp2 <- comp(what="p18", yr=2012, unit="m", census.data = censom00.3pt)
##
v12m00$p18t   <- tmp2$pob18t # paste interpolation
v12m00$p18e   <- tmp2$pob18e # paste interpolation
v12m00$ess    <- tmp2$ess                    
v12m00$er2    <- tmp2$er2                    
v12m00$p18l   <- tmp2$pob18l # paste interpolation
v12m00$lss    <- tmp2$lss                    
v12m00$lr2    <- tmp2$lr2                    
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
tmp2 <- comp(what="p18", yr=2015, unit="m", census.data = censom00.3pt)
##
v15m00$p18t   <- tmp2$pob18t # paste interpolation
v15m00$p18e   <- tmp2$pob18e # paste interpolation
v15m00$ess    <- tmp2$ess                    
v15m00$er2    <- tmp2$er2                    
v15m00$p18l   <- tmp2$pob18l # paste interpolation
v15m00$lss    <- tmp2$lss                    
v15m00$lr2    <- tmp2$lr2                    
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
tmp2 <- comp(what="p18", yr=2018, unit="m", census.data = censom00.3pt)
##
v18m00$p18t   <- tmp2$pob18t # paste interpolation
v18m00$p18e   <- tmp2$pob18e # paste interpolation
v18m00$ess    <- tmp2$ess                    
v18m00$er2    <- tmp2$er2                    
v18m00$p18l   <- tmp2$pob18l # paste interpolation
v18m00$lss    <- tmp2$lss                    
v18m00$lr2    <- tmp2$lr2                    
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
tmp2 <- comp(what="p18", yr=2021, unit="m", census.data = censom00.3pt)
##
v21m00$p18t   <- tmp2$pob18t # paste interpolation
v21m00$p18e   <- tmp2$pob18e # paste interpolation
v21m00$ess    <- tmp2$ess                    
v21m00$er2    <- tmp2$er2                    
v21m00$p18l   <- tmp2$pob18l # paste interpolation
v21m00$lss    <- tmp2$lss                    
v21m00$lr2    <- tmp2$lr2                    
## ## 2021m03
## ## 2021m06
## ## 2021m09
## ## 2021m12
## ## 2021m15
## ## 2021m18

## compare 3-census projections against 1995 and 2000 censuses
p18_1995.e <- interlog(what="p18", yr=1995, unit="m", census.data = censom00.3pt, frm="log(dv)~iv")[[1]]
p18_2000.e <- interlog(what="p18", yr=2000, unit="m", census.data = censom00.3pt, frm="log(dv)~iv")[[1]]
p18_1995.l <- interlog(what="p18", yr=1995, unit="m", census.data = censom00.3pt, frm="dv~iv")[[1]]
p18_2000.l <- interlog(what="p18", yr=2000, unit="m", census.data = censom00.3pt, frm="dv~iv")[[1]]
off95e <- abs(p18_1995.e - censom00$p18_1995) / censom00$p18_1995
off00e <- abs(p18_2000.e - censom00$p18_2000) / censom00$p18_2000
off95l <- abs(p18_1995.l - censom00$p18_1995) / censom00$p18_1995
off00l <- abs(p18_2000.l - censom00$p18_2000) / censom00$p18_2000

off9500e <- sqrt((((p18_1995.e - censom00$p18_1995) / censom00$p18_1995)^2 + ((p18_2000.e - censom00$p18_2000) / censom00$p18_2000)^2)/2)
off9500l <- sqrt((((p18_1995.l - censom00$p18_1995) / censom00$p18_1995)^2 + ((p18_2000.l - censom00$p18_2000) / censom00$p18_2000)^2)/2)

off9500e[i]
off9500l[i]

## summarize
mysum(off95e)
mysum(off00e)
mysum(off95l)
mysum(off00l)

iv <- v00m$ess + .0001 # avoid indeterminate logs
dv <- off95e   + .0001
summary(iv)
summary(dv)
plot(dv ~ iv, log="xy", axes = TRUE)
abline(lm(dv ~ iv), untf = TRUE)
axis(1, at = c(.0001,.001,.01,1))
summary(lm(dv ~ iv))

off9500l[i]



## ## compare linear and log projections in census years
## Inspect census figs
myplot <- function(i=NA, print=FALSE){
    #i <- 63 # debug
    if (print==TRUE) png(paste0("/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/plots/", "mu", censom00$inegi[i], "p18-interlog-3pt.png"))
    plot(  seq(1994,2021,3), li,
         xlim = c(1988,2027),
         ylim = c(0,1.3*max(li,lg,ls   )), col="blue", xlab="", ylab="voting age population", main = paste(censom00$inegi[i], censom00$mun[i], "Coahuila"), sub="blue=linear model, red=log-linear model, solid=census")
    ##plot(  seq(1994,2021,3), li, ylim = c(min(li,lg,ls,ef),max(li,lg,ls,ef)), main = paste(i, "bck=line, red=log, blue=lis, grn=efec"))
    points(seq(1994,2021,3), lg, col = "red")
    ##points(seq(1994,2021,3), ls, col = "blue")
    ##points(seq(1994,2021,3), ef, col = "green")
    points(1995, c1, pch = 19, cex = .75)
    points(1995, c1, pch = "X", cex = .75)
    points(  2000,            c2, pch = 19, cex = .75)
    points(  2000,            c2, pch = "X", cex = .75)
    points(c(2005,2010,2020), censom00[i,c("p18_2005","p18_2010","p18_2020")], pch=19, cex=.75)
    legend("bottomright",
           legend = c("RQMR (05-20) = 0.015", "RQMR (05-20) = 0.047", "RQMR (95-00) = 0.098", "RQMR (95-00) = 0.731"),
           pch = c(1,1,1,1), col = c("red","blue","red","blue"))
##     legend("bottomright",
##            legend = c(paste0("RQMR (3-point) = ", v00m$ess[i]), paste0("RQMR (3-point) = ", v00m$lss[i])),
## #           legend = c(paste0("R^2 = ", v15m00$pob18er2[i]), paste0("R^2 = ", v15m00$pob18lr2[i])),
##            pch = c(1,1), col = c("red","blue"))
    ##abline(v=2000, lty=2)
##
## points(1988, interlog(what="p18", yr=1988, unit="m", census.data = censom00.3pt, frm="log(dv)~iv")[[1]][i], col = "red")
## points(1991, interlog(what="p18", yr=1991, unit="m", census.data = censom00.3pt, frm="log(dv)~iv")[[1]][i], col = "red")
## points(2024, interlog(what="p18", yr=2024, unit="m", census.data = censom00.3pt, frm="log(dv)~iv")[[1]][i], col = "red")
## points(2027, interlog(what="p18", yr=2027, unit="m", census.data = censom00.3pt, frm="log(dv)~iv")[[1]][i], col = "red")
## ##
## points(1988, interlog(what="p18", yr=1988, unit="m", census.data = censom00.3pt, frm="dv~iv")[[1]][i], col = "blue")
## points(1991, interlog(what="p18", yr=1991, unit="m", census.data = censom00.3pt, frm="dv~iv")[[1]][i], col = "blue")
## points(2024, interlog(what="p18", yr=2024, unit="m", census.data = censom00.3pt, frm="dv~iv")[[1]][i], col = "blue")
## points(2027, interlog(what="p18", yr=2027, unit="m", census.data = censom00.3pt, frm="dv~iv")[[1]][i], col = "blue")
##
    if (print==TRUE) dev.off()
}

i <- 55
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
    v94m00$p18l[i],
    v97m00$p18l[i],
    v00m  $p18l[i],
    v03m00$p18l[i],
    v06m00$p18l[i],
    v09m00$p18l[i],
    v12m00$p18l[i],
    v15m00$p18l[i],
    v18m00$p18l[i],
    v21m00$p18l[i]
)
lg <- c(
    v94m00$p18e[i],
    v97m00$p18e[i],
    v00m  $p18e[i],
    v03m00$p18e[i],
    v06m00$p18e[i],
    v09m00$p18e[i],
    v12m00$p18e[i],
    v15m00$p18e[i],
    v18m00$p18e[i],
    v21m00$p18e[i]
)
tw <- c(
    v94m00$p18t[i],
    v97m00$p18t[i],
    v00m  $p18t[i],
    v03m00$p18t[i],
    v06m00$p18t[i],
    v09m00$p18t[i],
    v12m00$p18t[i],
    v15m00$p18t[i],
    v18m00$p18t[i],
    v21m00$p18t[i]
)
c1 <- as.numeric(censom00$p18_1995[i])
c2 <- as.numeric(censom00$p18_2000[i])
##
myplot(i, print=FALSE)

## Any year has the map's statistics
mysum(v00m$ess)
mysum(v00m$lss)



