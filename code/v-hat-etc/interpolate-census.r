ls()[grep("censo", ls())]
censo[1,]

interpol <- function(what="p18", yr=NA, force.map=NA){  # force.map allow usage of counterfactual maps, not used now
    what <- "p18"; yr <- 2006; force.map <- NA # debug
    ##
    if (!is.na(force.map)) {
        cs <- force.map
    } else {
        if (yr< 1997)           cs <- censod79
        if (yr>=1997 & yr<2006) cs <- censod97
        if (yr>=2006 & yr<2018) cs <- censod06
        if (yr>=2018 & yr<2024) cs <- censod18
    }
    ##
    sel.c <- grep(pattern=paste0(what, "_[0-9]{4}"), colnames(cs))  # columns with census values for indicator what
    cs <- cs[, sel.c]                                                # subset target columns
    ##
    ys <- as.numeric(sub(pattern = ".+_([0-9]{4})", replacement = "\\1", colnames(cs)))
    ##
    ## deltas
    d.ys <- ys [-1] - ys [-ncol(cs)]
    d.cs <- cs[,-1] - cs[,-ncol(cs)]
    ## slopes
    b <- d.cs / d.ys
    ##b[1:4,] # debug
    ## constants
    c1 <- cs[, -ncol(cs)]
    y1 <- ys  [-ncol(cs)]
    a <- y1 - b * c1
    ##a[1:4,] # debug
    ## name cols
    colnames(a) <- colnames(b) <- paste(y1, ys[-1], sep = "-")
    ##
    if (           yr<=ys[1]) interp <- a[,1] + b[,1] * yr # use 1st constant/slope for yr before 1st census
    if (yr>ys[1] & yr<=ys[2]) interp <- a[,1] + b[,1] * yr # use 1st constant/slope for yr between 1st and 2nd census
    if (yr>ys[2] & yr<=ys[3]) interp <- a[,2] + b[,2] * yr # use 2nd constant/slope for yr between 2nd and 3rd census
    if (yr>ys[3]            ) interp <- a[,2] + b[,2] * yr # use 2nd constant/slope for yr after 3rd census
    ##
    return(interp)
    ##rm(c, sel.c, what, yr, ys, y1, c1, a, b, force.map, interp) # clean debug
}

interpol(what="p18", yr=2005)
    
##########################################################################################
## generate yearly linear projections of pob18 (routine takes care of reseccionamiento) ##
##########################################################################################
# fix seccion with p18=ptot
sel <- which(pob18$seccion==120348)
pob18$p18_2005[sel] <- 100 # set to 45% ptot, as in 2010
# fix seccion 152717 --- inside campo militar 1, casilla prob moved to contiguous seccion
## 2006 lisnom=318
## 2009 lisnom=312
## 2012 lisnom=156
## 2015-on vanished
sel <- which(pob18$seccion==152717)
pob18$p18_2020[sel] <- 1
pobtot$ptot_2020[sel] <- 1
# fix 5 secciones in censo 2020 with ptot<p18 (all tiny)
sel <- which(pob18$seccion %in% c(51480, 143253, 250329, 250640, 252415))
pobtot$ptot_2020[sel] <- pob18$p18_2020[sel]
#
# start by making a generic object for manipulation
generic <- pob18
head(generic)
colnames(generic) <- c("seccion","cen_2005","cen_2010","cen_2020")
#
source(paste0(wd, "code/code-to-manip-census-in-split-secciones.r"))
# 
# output is an object named eq2, rename it
eq2[1,]
table(eq2$times.manip, eq2$action, useNA = "always")
eq2$dmanip <- as.numeric(eq2$times.manip>1)
#sel <- c("seccion", paste0("y", 1997:2022), "dmanip") # keep select columns only
pob18y <- eq2#[,sel] # rename year-by-year projections
rm(eq2, eq3) # clean

##########################################################################################
## generate yearly linear projections of pobtot (routine takes care of reseccionamiento) ##
##########################################################################################
# start by making a generic object for manipulation
generic <- pobtot
head(generic)
colnames(generic) <- c("seccion","cen_2005","cen_2010","cen_2020")
#
source(paste0(wd, "code/code-to-manip-census-in-split-secciones.r"))
# 
# output is an object named eq2, rename it
eq2[1,]
eq2$dmanip <- as.numeric(eq2$times.manip>1)
#sel <- c("seccion", paste0("y", 1997:2022), "dmanip") # keep select columns only
pobtoty <- eq2#[,sel] # rename year-by-year projections
rm(eq2, eq3) # clean

########################################################
## there are negative projections, make those equal 1 ##
########################################################
# pob18
sel.c <- paste0("y", 1997:2022) # yearly projection cols
sel <- which(apply(X=pob18y[,sel.c], 1, min) < 0) # row w neg values
tmp <- pob18y[,sel.c]
tmp[tmp<=0] <- 1
pob18y[,sel.c] <- tmp
# pobtot
sel <- which(apply(X=pobtoty[,sel.c], 1, min) < 0) # row w neg values
tmp <- pobtoty[,sel.c]
tmp[tmp<=0] <- 1
pobtoty[,sel.c] <- tmp
tmp[2,]
# fix 152716 v=83 in 1994, v=88 in 2000, v=0 in 2003 pop=0 in 2005 (merged to 2717 in 2010)---make p18=170=ptot constant since 1997
sel <- which(pobtoty$seccion==152716)
selc <- grep("y199[7-9]|y200[0-2]", colnames(pob18y))
pobtoty[sel,selc] <- pob18y[sel,selc] <- 170
selc <- grep("y200[3-9]|y201[0-9]|y202[0-9]", colnames(pob18y))
pobtoty[sel,selc] <- 1
pob18y[sel,selc] <- 1
# fix seccion 190439
sel <- which(pobtoty$seccion==190439)
selc <- grep("y200[5-9]|y201[0-9]|y202[0-9]", colnames(pob18y))
pobtoty[sel,selc] <- 1
pob18y[sel,selc] <- 1
selc <- grep("y2003", colnames(pob18y))
pobtoty[sel,selc] <- 37000
pob18y[sel,selc] <- 20000
selc <- grep("y2004", colnames(pob18y))
pobtoty[sel,selc] <- 50000
pob18y[sel,selc] <- 30000

## ######################################
## ## compare lisnom to p18 projection ##
## ######################################
## share <- cbind(seccion=pob18y$seccion,
##                pob18y[,sel.c] / pobtoty[,sel.c])
## summary(share)
## share[1,]
## # there are units where p18=ptot---they're either small or special (eg 152717 is inside campo militar 1)
## summary(share[,"y2005"]>.999)
## summary(share[,"y2010"]>.999)
## summary(share[,"y2020"]>.999)
## sel <- which(share[,"y2005"]>.999)
## tmp <- data.frame(seccion=pob18y$seccion[sel],
##                   p18=pob18y[sel,"y2005"],
##                   ptot=pobtoty[sel,"y2005"],
##                   dif=pob18y[sel,"y2005"]-pobtoty[sel,"y2005"])
## table(tmp$dif)
## summary(tmp$p18)
## sel <- which(share[,"y2010"]>.999)
## tmp <- data.frame(seccion=pob18y$seccion[sel],
##                   p18=pob18y[sel,"y2010"],
##                   ptot=pobtoty[sel,"y2010"],
##                   dif=pob18y[sel,"y2010"]-pobtoty[sel,"y2010"])
## table(tmp$dif)
## summary(tmp$p18)
## sel1 <- which(tmp$p18>20)
## tmp[sel1,]
## #
## sel <- which(share[,"y2020"]>.999)
## tmp <- data.frame(seccion=pob18y$seccion[sel],
##                   p18=pob18y[sel,"y2020"],
##                   ptot=pobtoty[sel,"y2020"],
##                   dif=pob18y[sel,"y2020"]-pobtoty[sel,"y2020"])
## table(tmp$dif)
## summary(tmp$p18)
## sel1 <- which(tmp$p18>20)
## tmp[sel1,]

# good number of projections below 45degree line
sel <- which(as.integer(pobtoty$seccion/10000)==24) # pick one edon
lo <- 0; hi <- 12000
plot(x=c(lo, hi), y=c(lo, hi), type = "n", xlab = "p18", ylab = "ptot")
for (i in sel){
    points(x=pob18y[i,sel.c], y=pobtoty[i,sel.c], pch=19, cex = .75, col = rgb(1,0,0, alpha = .15))
}
abline(a=0,b=1)

tmp <- pobtoty[sel,sel.c] - pob18y[sel,sel.c]
tmp.f <- function(x) min(x, na.rm = TRUE)
sel1 <- which(apply(X=tmp, 1, FUN=tmp.f) < 0)

pobtoty[sel[sel1[1]],sel.c]
pob18y[sel[sel1[1]],sel.c]
pobtoty[sel[sel1[1]],sel.c] - pob18y[sel[sel1[1]],sel.c]

# manipulate out-of-range projections
tmp.mean <- mean(as.matrix(share[,sel.c]), na.rm = TRUE) # will force out-of-range projections to mean
sel <- which(pobtoty[,sel.c[1]]== 1   &   pob18y[,sel.c[1]]==1) # rows equal 1 in both
x

tmp <-  pob18y [, c("seccion", "y2021")]
tmp2 <- pobtoty[, c("seccion", "y2021")]
colnames(tmp) <- c("seccion","p18")
colnames(tmp2) <- c("seccion","ptot")
share <- data.frame(seccion = tmp[,1],
                    sh = tmp[,-1] / tmp2[,-1])
head(share)

tmp <- merge(tmp, tmp2, by = "seccion")
tmp <- within(tmp, sh <-  p18 / ptot)
summary(tmp$sh)
head(tmp)


tmp <- tmp[order(tmp$sh),]
head(tmp)

head(v21s)
tmp <- pob18y[, c("seccion", "y2021")]
tmp$p18 <- tmp[,2]
head(tmp)
tmp2 <-  v21s[, c("seccion","lisnom")]
tmp $v1 <- 1
tmp2$v2 <- 2
tmp <- merge(x = tmp, y = tmp2, by = "seccion", all = TRUE)
tmp$orig <- "neither"
tmp$orig[tmp$v1==1] <- "p18y"
tmp$orig[tmp$v2==2] <- "v09s"
tmp$orig[tmp$v1==1 & tmp$v2==2] <- "both"
table(tmp$orig)
tmp <- within(tmp, dif <- abs(lisnom - p18) / lisnom)
summary(tmp$dif)

tmp <- tmp[order(-tmp$dif),]
tmp[70721:70730,]
tail(tmp)
x



