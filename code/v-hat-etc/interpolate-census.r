####################################################################
## Function to interpolate multi-census data linearly             ##
## invoked from code/elec-data-for-maps.r                         ##
## but easily generalizable                                       ##
##                                                                ##
## Author: Eric Magar                                             ##
## emagar at itam dot mx                                          ##
## Date: 9jun2023                                                 ##
## Last modified: 9jun2023                                        ##
####################################################################

## Usage with default data: interpol(what="p18", yr=2003, unit="m")

## ## Example with tmp as census data:
## tmp <- data.frame(p18_2005=c( 5, 8, 12, 1, 4),
##                   p18_2010=c( 7, 8, 20, 2, 4),
##                   p18_2020=c(10, 9, 21, 3, 4))
## interpol(what="p18", yr=2005, census.data=tmp)
## interpol(what="p18", yr=2010, census.data=tmp)
## interpol(what="p18", yr=2020, census.data=tmp)

## tmp2 <- data.frame(
##     c02=interpol(what="p18", yr=2002, census.data=tmp)
##   , c03=interpol(what="p18", yr=2003, census.data=tmp)
##   , c04=interpol(what="p18", yr=2004, census.data=tmp)
##   , c05=interpol(what="p18", yr=2005, census.data=tmp)
##   , c06=interpol(what="p18", yr=2006, census.data=tmp)
##   , c07=interpol(what="p18", yr=2007, census.data=tmp)
##   , c08=interpol(what="p18", yr=2008, census.data=tmp)
##   , c09=interpol(what="p18", yr=2009, census.data=tmp)
##   , c10=interpol(what="p18", yr=2010, census.data=tmp)
##   , c11=interpol(what="p18", yr=2011, census.data=tmp)
##   , c12=interpol(what="p18", yr=2012, census.data=tmp)
##   , c13=interpol(what="p18", yr=2013, census.data=tmp)
##   , c14=interpol(what="p18", yr=2014, census.data=tmp)
##   , c15=interpol(what="p18", yr=2015, census.data=tmp)
##   , c16=interpol(what="p18", yr=2016, census.data=tmp)
##   , c17=interpol(what="p18", yr=2017, census.data=tmp)
##   , c18=interpol(what="p18", yr=2018, census.data=tmp)
##   , c19=interpol(what="p18", yr=2019, census.data=tmp)
##   , c20=interpol(what="p18", yr=2020, census.data=tmp)
##   , c21=interpol(what="p18", yr=2021, census.data=tmp)
## )
## tmp2
## plot(2002:2021, tmp2[3,])
## points(c(2005,2010,2020), tmp[3,], pch = 20)

interpol <- function(what="p18", yr=NA, unit=c("d","m","s")[2], census.data=NA, add.plot=FALSE, plot.n=NA){  # census.data allow usage of counterfactual maps, not used now
    ## Census.data is a data.frame reporting X>1 censuses, with the following characteristics:
    ## - it can report more than one indicator, 'what' selects which one to manipulate in call;
    ## - it has groups of X columns reporting yearly indicators, eg. ptot_1990 ptot_2000 ptot_2010 ptot_2020;
    ## - colnames are: indicator_year
    ## - rows are units.
    ## Option 'unit' is redundant whenever a census.data is provided in call.
    ##
    ##what <- "p18"; yr <- 1997; census.data <- data.frame(p18_2005=c( 5, 8, 12, 1, 4), p18_2010=c( 7, 8, 20, 2, 4), p18_2020=c(10, 9, 21, 3, 4)); plot.n = 4;
# debug
    ##
    if (!is.null(dim(census.data))) {  # if data provided in call, use it
        cs <- census.data
    } else {                         # else the function is customized for code/elec-data-for-maps.r
        if (unit=="d"){
            if (           yr<1997) cs <- censod79
            if (yr>=1997 & yr<2006) cs <- censod97
            if (yr>=2006 & yr<2018) cs <- censod06
            if (yr>=2018 & yr<2024) cs <- censod18
        }
        if (unit=="m"){
            if (           yr<1997) cs <- censom94
            if (yr>=1997 & yr<2000) cs <- censom97
            if (yr>=2000 & yr<2003) cs <- censom00
            if (yr>=2003 & yr<2006) cs <- censom03
            if (yr>=2006 & yr<2009) cs <- censom06
            if (yr>=2009 & yr<2012) cs <- censom09
            if (yr>=2012 & yr<2015) cs <- censom12
            if (yr>=2015 & yr<2018) cs <- censom15
            if (yr>=2018 & yr<2021) cs <- censom18
            if (yr>=2021          ) cs <- censom21
        }
    }
    ##
    sel.c <- grep(pattern=paste0(what, "_[0-9]{4}"), colnames(cs))  # columns with census values for indicator 'what'
    cs <- cs[, sel.c]                                               # subset target columns
    ##
    ys <- as.numeric(sub(pattern = ".+_([0-9]{4})", replacement = "\\1", colnames(cs)))
    ys <- mapply(rep, ys, nrow(cs)) # repeat ys for each row in cs for easier operations
    ##
    ## for deltas
    y2 <- ys[,-1]
    y1 <- ys[,-ncol(cs)]
    c2 <- cs[,-1]
    c1 <- cs[,-ncol(cs)]
    ## slopes
    b <- (c2 - c1) / (y2 - y1)
    ##b[1:4,] # debug
    ## constants
    a <- c2 - b * y2
    ##a <- (y2*c1 - y1*c2) / (y2 - y1)
    ##a[1:4,] # debug
    ## name cols
    ##colnames(b) <- paste(y1, y2, sep = "-")
    ##
    if (           yr<=ys[1,1]) interp <- c1[,1] + b[,1] * (yr - 2005) # use 1st level/slope for yr before 1st census
    if (yr>ys[1,1] & yr<=ys[1,2]) interp <- c1[,1] + b[,1] * (yr - 2005) # use 1st level/slope for yr between 1st and 2nd census
    if (yr>ys[1,2] & yr<=ys[1,3]) interp <- c1[,2] + b[,2] * (yr - 2010) # use 2nd level/slope for yr between 2nd and 3rd census
    if (yr>ys[1,3]            ) interp <- c1[,2] + b[,2] * (yr - 2010) # use 2nd level/slope for yr after 3rd census
    ##
    ## plot option
    if (add.plot==TRUE){
        if (!is.na(plot.n)){ ## if plot.n specified, subset data for plotting
            cs0 <- cs[plot.n,]
            ys0 <- ys[plot.n,]
            interp0 <- interp[plot.n]
            a0 <- a[plot.n,]
            b0 <- b[plot.n,]
        } else {             ## else just duplicate for plotting
            cs0 <- cs
            ys0 <- ys
            interp0 <- interp
            a0 <- a
            b0 <- b
        }
        plot(
            x=c((min(ys0)-15),(max(ys0)+10)),
            y=c(min(cs0,interp0),max(cs0,interp0)),
            xlab="", ylab="",
            type="n"
        )
        abline(h=0, col="red")
        points(x=unlist(as.vector(ys0)),
               y=unlist(as.vector(cs0)), cex=.75, col = "gray")
        for (i in 1:length(unlist(as.vector(a0)))){
            abline(a=unlist(as.vector(a0))[i],
                   b=unlist(as.vector(b0))[i],
                   col = "gray")
        }
        points(x=rep(yr, length(interp0)), y=interp0, cex=.75)
    }
    ##
    return(round(interp, 1))
    ##rm(c, sel.c, what, yr, ys, y1, c1, a, b, census.data, interp) # clean debug
}

## performs projection with a log lm
interlog <- function(what="p18", yr=NA, unit=c("d","m","s")[2], census.data=NA, add.plot=FALSE, plot.n=NA){  # census.data allow usage of counterfactual maps, not used now
    ## Census.data is a data.frame reporting X>1 censuses, with the following characteristics:
    ## - it can report more than one indicator, 'what' selects which one to manipulate in call;
    ## - it has groups of X columns reporting yearly indicators, eg. ptot_1990 ptot_2000 ptot_2010 ptot_2020;
    ## - colnames are: indicator_year
    ## - rows are units.
    ## Option 'unit' is redundant whenever a census.data is provided in call.
    ##
    ##what <- "p18"; yr <- 1997; census.data <- data.frame(p18_2005=c( 5, 8, 12, 1, 4), p18_2010=c( 7, 8, 20, 2, 4), p18_2020=c(10, 9, 21, 3, 4)); plot.n = 4;
# debug
    ##
    if (!is.null(dim(census.data))) {  # if data provided in call, use it
        cs <- census.data
    } else {                         # else the function is customized for code/elec-data-for-maps.r
        if (unit=="d"){
            if (           yr<1997) cs <- censod79
            if (yr>=1997 & yr<2006) cs <- censod97
            if (yr>=2006 & yr<2018) cs <- censod06
            if (yr>=2018 & yr<2024) cs <- censod18
        }
        if (unit=="m"){
            if (           yr<1997) cs <- censom94
            if (yr>=1997 & yr<2000) cs <- censom97
            if (yr>=2000 & yr<2003) cs <- censom00
            if (yr>=2003 & yr<2006) cs <- censom03
            if (yr>=2006 & yr<2009) cs <- censom06
            if (yr>=2009 & yr<2012) cs <- censom09
            if (yr>=2012 & yr<2015) cs <- censom12
            if (yr>=2015 & yr<2018) cs <- censom15
            if (yr>=2018 & yr<2021) cs <- censom18
            if (yr>=2021          ) cs <- censom21
        }
    }
    ##
    sel.c <- grep(pattern=paste0(what, "_[0-9]{4}"), colnames(cs))  # columns with census values for indicator 'what'
    cs <- cs[, sel.c]                                               # subset target columns
    ##
    ys <- as.numeric(sub(pattern = ".+_([0-9]{4})", replacement = "\\1", colnames(cs)))
    ys <- mapply(rep, ys, nrow(cs)) # repeat ys for each row in cs for easier operations
    ##
    ####################
    ## define formula ##
    ####################
    dv <- cs; iv <- ys # so dv and iv can be subsetted when plotting
    frm   <- as.formula("    dv  ~     iv") ; frm0 <- as.formula("    unlist(as.vector(dv))  ~     unlist(as.vector(iv))")   # linear
    ##frm <- as.formula("log(dv) ~     iv") ; frm0 <- as.formula("log(unlist(as.vector(dv))) ~     unlist(as.vector(iv))")   # log dv
    ##frm <- as.formula("    dv  ~ log(iv)"); frm0 <- as.formula("    unlist(as.vector(dv))  ~ log(unlist(as.vector(iv)))")  # log iv
    ##
    ## regress one line and predict
    regs <- mapply(rbind, split(iv, seq(nrow(iv))), split(dv, seq(nrow(dv))), SIMPLIFY = FALSE) ## split observations into list of dfs
    regs <- lapply(regs, function(x) t(x))                                                      ## transpose
    regs <- lapply(regs, function(x) x <- data.frame(iv=x[,1], dv=x[,2]))                       ## name cols
    ##                                                                                          #############
    regs <- lapply(regs, function(x) lm(formula=frm, data=x))                                   ## regress ##
    ##                                                                                          #############
    new.d <- vector(mode = "list", length = length(regs) )                                      ## prep predictions
    new.d <- lapply(new.d, function(x) x <- yr)                                                 ## prep predictions
    interp <- lapply(regs, function(x) predict.lm(x, newdata = new.d))                     ## predict
    interp <- unlist(as.data.frame(interp))                                                ## turn into vector

    ## exp(predict.lm(reg.pan,    newdata = new.d))
    ##
    ## plot option
    if (add.plot==TRUE){
        if (!is.na(plot.n)){ ## if plot.n specified, subset data for plotting
            dv <- cs[plot.n,]
            iv <- ys[plot.n,]
            interp0 <- interp[plot.n]
            regs0 <- regs[plot.n]
        } else {             ## else just duplicate for plotting
            cs0 <- cs
            ys0 <- ys
            interp0 <- interp
            regs0 <- regs
        }
        ## define plot x and y ranges
        yrng <- c(min(cs0,interp0),max(cs0,interp0))
        xrng <- c((min(ys0)-15),(max(ys0)+10))
        ## plot log-trasformed model, not in log scale
        plot(
##          log(yrng) ~     xrng,
                yrng  ~     xrng,
##              yrng  ~ log(xrng),
            xlab="", ##ylab="",
            type="n"
        )
        abline(h=0, col="red")
        points(frm, cex=.75, col = "gray")
        lapply(regs0, function(x) abline(reg=x, untf=FALSE, col = "gray"))

        points(log(interp0)~rep(yr, length(interp0)), cex=.75)

        # log scale
        plot(
            y=c(min(cs0,interp0),max(cs0,interp0)), x=c((min(ys0)-15),(max(ys0)+10)), log = "y"
          , xlab="", ylab=""
          , type="n"
        )
        abline(h=0, col="red")
        points(unlist(as.vector(cs0)) ~ unlist(as.vector(ys0)),
               cex=.75, col = "gray")
        lapply(regs0, function(x) abline(reg=x, untf=TRUE, col = "gray"))
    }
    ##
    return(round(interp, 1))
    ##rm(c, sel.c, what, yr, ys, y1, c1, a, b, census.data, interp) # clean debug
}


     
## ##########################################################################################
## ## generate yearly linear projections of pob18 (routine takes care of reseccionamiento) ##
## ##########################################################################################
## # fix seccion with p18=ptot
## sel <- which(pob18$seccion==120348)
## pob18$p18_2005[sel] <- 100 # set to 45% ptot, as in 2010
## # fix seccion 152717 --- inside campo militar 1, casilla prob moved to contiguous seccion
## ## 2006 lisnom=318
## ## 2009 lisnom=312
## ## 2012 lisnom=156
## ## 2015-on vanished
## sel <- which(pob18$seccion==152717)
## pob18$p18_2020[sel] <- 1
## pobtot$ptot_2020[sel] <- 1
## # fix 5 secciones in censo 2020 with ptot<p18 (all tiny)
## sel <- which(pob18$seccion %in% c(51480, 143253, 250329, 250640, 252415))
## pobtot$ptot_2020[sel] <- pob18$p18_2020[sel]
## #
## # start by making a generic object for manipulation
## generic <- pob18
## head(generic)
## colnames(generic) <- c("seccion","cen_2005","cen_2010","cen_2020")
## #
## source(paste0(wd, "code/code-to-manip-census-in-split-secciones.r"))
## # 
## # output is an object named eq2, rename it
## eq2[1,]
## table(eq2$times.manip, eq2$action, useNA = "always")
## eq2$dmanip <- as.numeric(eq2$times.manip>1)
## #sel <- c("seccion", paste0("y", 1997:2022), "dmanip") # keep select columns only
## pob18y <- eq2#[,sel] # rename year-by-year projections
## rm(eq2, eq3) # clean
##
## ##########################################################################################
## ## generate yearly linear projections of pobtot (routine takes care of reseccionamiento) ##
## ##########################################################################################
## # start by making a generic object for manipulation
## generic <- pobtot
## head(generic)
## colnames(generic) <- c("seccion","cen_2005","cen_2010","cen_2020")
## #
## source(paste0(wd, "code/code-to-manip-census-in-split-secciones.r"))
## # 
## # output is an object named eq2, rename it
## eq2[1,]
## eq2$dmanip <- as.numeric(eq2$times.manip>1)
## #sel <- c("seccion", paste0("y", 1997:2022), "dmanip") # keep select columns only
## pobtoty <- eq2#[,sel] # rename year-by-year projections
## rm(eq2, eq3) # clean
##
## ########################################################
## ## there are negative projections, make those equal 1 ##
## ########################################################
## # pob18
## sel.c <- paste0("y", 1997:2022) # yearly projection cols
## sel <- which(apply(X=pob18y[,sel.c], 1, min) < 0) # row w neg values
## tmp <- pob18y[,sel.c]
## tmp[tmp<=0] <- 1
## pob18y[,sel.c] <- tmp
## # pobtot
## sel <- which(apply(X=pobtoty[,sel.c], 1, min) < 0) # row w neg values
## tmp <- pobtoty[,sel.c]
## tmp[tmp<=0] <- 1
## pobtoty[,sel.c] <- tmp
## tmp[2,]
## # fix 152716 v=83 in 1994, v=88 in 2000, v=0 in 2003 pop=0 in 2005 (merged to 2717 in 2010)---make p18=170=ptot constant since 1997
## sel <- which(pobtoty$seccion==152716)
## selc <- grep("y199[7-9]|y200[0-2]", colnames(pob18y))
## pobtoty[sel,selc] <- pob18y[sel,selc] <- 170
## selc <- grep("y200[3-9]|y201[0-9]|y202[0-9]", colnames(pob18y))
## pobtoty[sel,selc] <- 1
## pob18y[sel,selc] <- 1
## # fix seccion 190439
## sel <- which(pobtoty$seccion==190439)
## selc <- grep("y200[5-9]|y201[0-9]|y202[0-9]", colnames(pob18y))
## pobtoty[sel,selc] <- 1
## pob18y[sel,selc] <- 1
## selc <- grep("y2003", colnames(pob18y))
## pobtoty[sel,selc] <- 37000
## pob18y[sel,selc] <- 20000
## selc <- grep("y2004", colnames(pob18y))
## pobtoty[sel,selc] <- 50000
## pob18y[sel,selc] <- 30000
##
## ## ######################################
## ## ## compare lisnom to p18 projection ##
## ## ######################################
## ## share <- cbind(seccion=pob18y$seccion,
## ##                pob18y[,sel.c] / pobtoty[,sel.c])
## ## summary(share)
## ## share[1,]
## ## # there are units where p18=ptot---they're either small or special (eg 152717 is inside campo militar 1)
## ## summary(share[,"y2005"]>.999)
## ## summary(share[,"y2010"]>.999)
## ## summary(share[,"y2020"]>.999)
## ## sel <- which(share[,"y2005"]>.999)
## ## tmp <- data.frame(seccion=pob18y$seccion[sel],
## ##                   p18=pob18y[sel,"y2005"],
## ##                   ptot=pobtoty[sel,"y2005"],
## ##                   dif=pob18y[sel,"y2005"]-pobtoty[sel,"y2005"])
## ## table(tmp$dif)
## ## summary(tmp$p18)
## ## sel <- which(share[,"y2010"]>.999)
## ## tmp <- data.frame(seccion=pob18y$seccion[sel],
## ##                   p18=pob18y[sel,"y2010"],
## ##                   ptot=pobtoty[sel,"y2010"],
## ##                   dif=pob18y[sel,"y2010"]-pobtoty[sel,"y2010"])
## ## table(tmp$dif)
## ## summary(tmp$p18)
## ## sel1 <- which(tmp$p18>20)
## ## tmp[sel1,]
## ## #
## ## sel <- which(share[,"y2020"]>.999)
## ## tmp <- data.frame(seccion=pob18y$seccion[sel],
## ##                   p18=pob18y[sel,"y2020"],
## ##                   ptot=pobtoty[sel,"y2020"],
## ##                   dif=pob18y[sel,"y2020"]-pobtoty[sel,"y2020"])
## ## table(tmp$dif)
## ## summary(tmp$p18)
## ## sel1 <- which(tmp$p18>20)
## ## tmp[sel1,]
##
## # good number of projections below 45degree line
## sel <- which(as.integer(pobtoty$seccion/10000)==24) # pick one edon
## lo <- 0; hi <- 12000
## plot(x=c(lo, hi), y=c(lo, hi), type = "n", xlab = "p18", ylab = "ptot")
## for (i in sel){
##     points(x=pob18y[i,sel.c], y=pobtoty[i,sel.c], pch=19, cex = .75, col = rgb(1,0,0, alpha = .15))
## }
## abline(a=0,b=1)
##
## tmp <- pobtoty[sel,sel.c] - pob18y[sel,sel.c]
## tmp.f <- function(x) min(x, na.rm = TRUE)
## sel1 <- which(apply(X=tmp, 1, FUN=tmp.f) < 0)
##
## pobtoty[sel[sel1[1]],sel.c]
## pob18y[sel[sel1[1]],sel.c]
## pobtoty[sel[sel1[1]],sel.c] - pob18y[sel[sel1[1]],sel.c]
##
## # manipulate out-of-range projections
## tmp.mean <- mean(as.matrix(share[,sel.c]), na.rm = TRUE) # will force out-of-range projections to mean
## sel <- which(pobtoty[,sel.c[1]]== 1   &   pob18y[,sel.c[1]]==1) # rows equal 1 in both
## x
##
## tmp <-  pob18y [, c("seccion", "y2021")]
## tmp2 <- pobtoty[, c("seccion", "y2021")]
## colnames(tmp) <- c("seccion","p18")
## colnames(tmp2) <- c("seccion","ptot")
## share <- data.frame(seccion = tmp[,1],
##                     sh = tmp[,-1] / tmp2[,-1])
## head(share)
##
## tmp <- merge(tmp, tmp2, by = "seccion")
## tmp <- within(tmp, sh <-  p18 / ptot)
## summary(tmp$sh)
## head(tmp)
##
##
## tmp <- tmp[order(tmp$sh),]
## head(tmp)
##
## head(v21s)
## tmp <- pob18y[, c("seccion", "y2021")]
## tmp$p18 <- tmp[,2]
## head(tmp)
## tmp2 <-  v21s[, c("seccion","lisnom")]
## tmp $v1 <- 1
## tmp2$v2 <- 2
## tmp <- merge(x = tmp, y = tmp2, by = "seccion", all = TRUE)
## tmp$orig <- "neither"
## tmp$orig[tmp$v1==1] <- "p18y"
## tmp$orig[tmp$v2==2] <- "v09s"
## tmp$orig[tmp$v1==1 & tmp$v2==2] <- "both"
## table(tmp$orig)
## tmp <- within(tmp, dif <- abs(lisnom - p18) / lisnom)
## summary(tmp$dif)
##
## tmp <- tmp[order(-tmp$dif),]
## tmp[70721:70730,]
## tail(tmp)
## x



