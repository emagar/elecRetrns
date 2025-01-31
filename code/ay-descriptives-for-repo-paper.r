rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
setwd(dd)

#####################
## Handy functions ##
#####################
pth <- ifelse (Sys.info()["user"] %in% c("eric", "magar"),
    "~/Dropbox/data/useful-functions",
    "https://raw.githubusercontent.com/emagar/useful-functions/master"
    )
source( paste(pth, "edo2edon.r", sep = "/") )  ## translates state codes
source( paste(pth, "inegi2ife.r", sep = "/") ) ## translates municipio codes
rm(pth)

#####################
## Read data files ##
#####################
## choose one
d <- read.csv(file = "aymu1970-on.csv"          , stringsAsFactors = FALSE) # raw
d <- read.csv(file = "aymu1970-on.coalAgg.csv"  , stringsAsFactors = FALSE) # coalitions aggregated
d <- read.csv(file = "aymu1970-on.coalSplit.csv", stringsAsFactors = FALSE) # split coalitions

##################################################################################
## Election cycle from emm (consecutive but not necessarilly aligned w federal) ##
##################################################################################
d$cycle <- sub(pattern="^[a-z]+.([0-9]+).*$", replacement = "\\1", d$emm, perl=TRUE)
d$cycle <- as.numeric(d$cycle)
table(d$cycle)
##
###########################################################################
## Use yr to make cycle aligned w federal (some extras may fall in next) ##
###########################################################################
d$cyclef <- cut(d$yr, breaks = seq(1970,2027, by=3), right = FALSE)
table(d$cyclef)
## ## This other, more precise route aborted, too many exceptions
## ## make coa cycle 13 cycle 14 and so forth
## sel <- which(d$edon==5 & d$cycle>=13)
## d$cycle[sel] <- d$cycle[sel] + 1
## ## make hgo cycle 15 cycle 16, cycle 16 cycle 18, cycle 17 cycle 18
## sel <- which(d$edon==13 & d$cycle==17)
## d$cycle[sel] <- d$cycle[sel] + 2
## sel <- which(d$edon==13 & d$cycle==16)
## d$cycle[sel] <- d$cycle[sel] + 2
## sel <- which(d$edon==13 & d$cycle==15)
## d$cycle[sel] <- d$cycle[sel] + 1
## ## make mex cycle 10 cycle 11 and so forth
## sel <- which(d$edon==15 & d$cycle>=10)
## d$cycle[sel] <- d$cycle[sel] + 1
## ## make ver cycle 17 cycle 18 and so forth
## sel <- which(d$edon==30 & d$cycle>=17)
## d$cycle[sel] <- d$cycle[sel] + 1
## ## create cycle2 that actually follows fed three-year terms
## d$cycle2 <- (d$cycle-1)*3+1970
## d$cycle2 <- paste(d$cycle2,d$cycle, sep="-")
## table(d$yr[d$edon==30],d$cycle2[d$edon==30])
## table(d$yr,d$cycle2)
## which(d$cycle2==1994 & d$yr==1997)


##############
## Coverage ##
##############
##
table(d$status)
## drop cancelled races 
sel.r <- grep("cancelled", d$status); if (length(sel.r)>0) ds <- d[-sel.r,] # subset wo cancelled races keeps actual number
## drop runoffs (comment if data is v7, ay-vote.r dropped 1st round keeping runoff only with regular emm code and note in status)
sel.r <- grep("to-runoff", d$status); if (length(sel.r)>0) ds <- d[-sel.r,] # subset wo runoff races keeps actual number
##
i <- 0
i <- i+1; edon2edo(i); table(ds$cycle[ds$edon==i]); table(ds$yr[ds$edon==i])
## Overall
table(ds$cycle)
table(ds$yr)
##
d2 <- read.csv(file = "../ancillary/mun.yrs.csv"          , stringsAsFactors = FALSE) # raw
d2[1,]
table(d2$yr01, d2$edo, useNA = "ifany")
table(d2$yr01,         useNA = "ifany")
table(is.na(d2$yr01))
table(is.na(d2$yr02))
table(is.na(d2$yr03))
table(is.na(d2$yr04))
table(is.na(d2$yr05))
table(is.na(d2$yr06))
table(is.na(d2$yr07))
table(is.na(d2$yr08))
table(is.na(d2$yr09))
table(is.na(d2$yr10))
table(is.na(d2$yr11))
table(is.na(d2$yr12))
table(is.na(d2$yr13))
table(is.na(d2$yr14))
table(is.na(d2$yr15))
table(is.na(d2$yr16))
table(is.na(d2$yr17))
table(is.na(d2$yr18))
table(is.na(d2$yr19))

#####################################
## drop un-analyzable obs for good ##
#####################################
sel.r <- grep("cancelled|to-runoff", d$status); d <- d[-sel.r,] # subset wo cancelled races keeps actual number
sel.r <- grep("missing|appointed|litigio", d$status); d <- d[-sel.r,]
sel.r <- grep("proj", d$status); d <- d[-sel.r,] ## drop missing cases projected from df for lags
table(d$status)


######################################
## plot party shares by year/cyclef ##
######################################
#
###################################################################################################################
## imports object with split-coal ay data since 1988 including vote shares for pan pri prd morena mc pvem pt oth ##
## To update, run code in reelec/code/ay-vote.r from start to where this file is saved to disk                   ##
###################################################################################################################
library(lubridate)

getwd()
d <- read.csv(file = "../../reelec/data/aymu1988-on-v7-coalSplit.csv")
d <- d[d$yr>1978,] ## drop pre 1979
d$morena[d$yr<2015] <- NA ## drop zeroes from pre-morena
d$pvem[d$yr<1993] <- NA ## drop zeroes from pre-pvem
d$mc[d$yr<2000] <- NA ## drop zeroes from pre-conve
table(d$status)
d[1,]
##################
## format dates ##
##################
date <- d$date
sel.r <- which(date<19790000) # cases where dy missing
table(date[sel.r]) ## check only dy missing
date[sel.r] <- date[sel.r] * 100 + 15 ## arbitrary put at mid month
date <- ymd(date)
table(date)
date -> d$date ## return manipulation

## add +/- 4 weeks as jitter for plot
x.jitter <- round(runif(n=nrow(d),-28,28),0)
x.jitter <- days(x.jitter)
date <- date + x.jitter

## add pt to other category for plotting
d$oth <- d$oth + d$pt 

## yearly means quartiles etc
## mn <- data.frame(
##                  pan   =aggregate(pan    ~ yr, data = d, FUN = mean),
##                  pri   =aggregate(pri    ~ yr, data = d, FUN = mean),
##                  prd   =aggregate(prd    ~ yr, data = d, FUN = mean),
##                  morena=aggregate(morena ~ yr, data = d, FUN = mean)
## )
## mn$yr <- ymd(mn$pan.yr*10000+701)
## mn$pan <- mn$pan.pan; 
## mn$pri <- mn$pri.pri; 
## mn$prd <- mn$prd.prd; 
## mn$morena <- mn$morena.morena; 
##

q9 <- merge(x=aggregate(x=pan    ~ yr, data = d, FUN = 'quantile', probs=(.75)),
            y=aggregate(x=pri    ~ yr, data = d, FUN = 'quantile', probs=(.75)), by = "yr", all = TRUE)
q9 <- merge(x=q9,
            y=aggregate(x=prd    ~ yr, data = d, FUN = 'quantile', probs=(.75)), by = "yr", all = TRUE)
q9 <- merge(x=q9,
            y=aggregate(x=mc     ~ yr, data = d, FUN = 'quantile', probs=(.75)), by = "yr", all = TRUE)
q9 <- merge(x=q9,
            y=aggregate(x=pvem   ~ yr, data = d, FUN = 'quantile', probs=(.75)), by = "yr", all = TRUE)
q9 <- merge(x=q9,
            y=aggregate(x=morena ~ yr, data = d, FUN = 'quantile', probs=(.75)), by = "yr", all = TRUE)
q9 <- merge(x=q9,
            y=aggregate(x=oth    ~ yr, data = d, FUN = 'quantile', probs=(.75)), by = "yr", all = TRUE)
q9$yr <- ymd(q9$yr*10000+701)
##
q1 <- merge(x=aggregate(x=pan    ~ yr, data = d, FUN = 'quantile', probs=(.25)),
            y=aggregate(x=pri    ~ yr, data = d, FUN = 'quantile', probs=(.25)), by = "yr", all = TRUE)
q1 <- merge(x=q1,
            y=aggregate(x=prd    ~ yr, data = d, FUN = 'quantile', probs=(.25)), by = "yr", all = TRUE)
q1 <- merge(x=q1,
            y=aggregate(x=mc     ~ yr, data = d, FUN = 'quantile', probs=(.25)), by = "yr", all = TRUE)
q1 <- merge(x=q1,
            y=aggregate(x=pvem   ~ yr, data = d, FUN = 'quantile', probs=(.25)), by = "yr", all = TRUE)
q1 <- merge(x=q1,
            y=aggregate(x=morena ~ yr, data = d, FUN = 'quantile', probs=(.25)), by = "yr", all = TRUE)
q1 <- merge(x=q1,
            y=aggregate(x=oth    ~ yr, data = d, FUN = 'quantile', probs=(.25)), by = "yr", all = TRUE)
q1$yr <- ymd(q1$yr*10000+701)
##
q5 <- merge(x=aggregate(x=pan    ~ yr, data = d, FUN = 'quantile', probs=(.5)),
            y=aggregate(x=pri    ~ yr, data = d, FUN = 'quantile', probs=(.5)), by = "yr", all = TRUE)
q5 <- merge(x=q5,
            y=aggregate(x=prd    ~ yr, data = d, FUN = 'quantile', probs=(.5)), by = "yr", all = TRUE)
q5 <- merge(x=q5,
            y=aggregate(x=mc     ~ yr, data = d, FUN = 'quantile', probs=(.5)), by = "yr", all = TRUE)
q5 <- merge(x=q5,
            y=aggregate(x=pvem   ~ yr, data = d, FUN = 'quantile', probs=(.5)), by = "yr", all = TRUE)
q5 <- merge(x=q5,
            y=aggregate(x=morena ~ yr, data = d, FUN = 'quantile', probs=(.5)), by = "yr", all = TRUE)
q5 <- merge(x=q5,
            y=aggregate(x=oth    ~ yr, data = d, FUN = 'quantile', probs=(.5)), by = "yr", all = TRUE)
q5$yr <- ymd(q5$yr*10000+701)

## define party colors for plots
col.pan    <- function(alpha=1) return(rgb(0,0,1                  , alpha=alpha))
col.pri    <- function(alpha=1) return(rgb(1,0,0                  , alpha=alpha))
col.prd    <- function(alpha=1) return(rgb(245/255,189/255,2/255 , alpha=alpha))
col.morena <- function(alpha=1) return(rgb(114/255,0/255,18/255   , alpha=alpha))
col.pvem   <- function(alpha=1) return(rgb(0,128/255,0            , alpha=alpha))
col.mc     <- function(alpha=1) return(rgb(255/255,95/255,31/255  , alpha=alpha))
col.oth    <- function(alpha=1) return(rgb(113/255,113/255,113/255, alpha=alpha))


## function to draw interquartile range spline
gen.ci <- function(pty=c("pan","pri","prd","morena","oth")[1]){
    pty <- paste0("^", pty, "$")
    sel <- grep(pty, colnames(q9))
    tmp <- smooth.spline(x = q9$yr, y = q9[,sel]   *100, df = 10)
    tmp.hilo <- data.frame(xhi = tmp$x, yhi = tmp$y)
    tmp <- smooth.spline(x = q1$yr, y = q1[,sel]   *100, df = 10)
    tmp.hilo <- cbind(tmp.hilo, data.frame(xlo = rev(tmp$x), ylo = rev(tmp$y)))
    tmp.hilo$yhi[tmp.hilo$yhi<0] <- 0; tmp.hilo$yhi[tmp.hilo$yhi>100] <- 100 ## deal w out-of-range
    tmp.hilo$ylo[tmp.hilo$ylo<0] <- 0; tmp.hilo$ylo[tmp.hilo$ylo>100] <- 100 ## deal w out-of-range
    tmp.hilo$yhi[tmp.hilo$yhi<0] <- 0; tmp.hilo$yhi[tmp.hilo$yhi>100] <- 100 ## deal w out-of-range
    sel <- which(tmp.hilo$yhi < tmp.hilo$ylo) ## which lo are above hi? invert them
    tmp.hilo[sel, c("yhi", "ylo")] <- tmp.hilo[sel, c("ylo", "yhi")] ## swap lo and hi
    return(tmp.hilo)
}

getwd()

pdf(file = "../plots/vpct1979-2024.pdf", width=10, height=7)
par(mar=c(3,4,2,2)+0.1) # drop title space and xlab space
plot(x=c(ymd(19790601),ymd(20241231)), y=c(0,100), type="n", axes = FALSE, xlab = "", ylab = "% vote", main = "")
axis(1, at = seq(from=ymd(19790101), to=ymd(20240101), by = 'years'), labels = FALSE)
tmp <- c(1979, paste0("'", seq(82,97,3)), 2000, paste0("'", formatC(seq(3,21,3), width = 2, format = "d", flag = "0")), 2024) # xlabels
axis(1, at = seq(from=ymd(19790101), to=ymd(20240101), by = '3 year'), labels = tmp)
axis(2, at = seq(0,100,5), labels = FALSE)
axis(2, at = seq(0,100,25))
abline(h=seq(0,100,5), col="gray", lwd=.5)
abline(h=seq(0,100,10), col="gray")
abline(h=50, col="gray", lwd=2)
##
## ## quartile splines ... no jala
## tmp.hilo <- gen.ci("pan")    
## polygon(x = c(tmp.hilo$xhi, tmp.hilo$xlo), y = c(tmp.hilo$yhi, tmp.hilo$ylo), border = NA, col = col.pan(alpha=.4) )
## tmp.hilo <- gen.ci("pri")    
## polygon(x = c(tmp.hilo$xhi, tmp.hilo$xlo), y = c(tmp.hilo$yhi, tmp.hilo$ylo), border = NA, col = col.pri(alpha=.4) )
## tmp.hilo <- gen.ci("prd")    
## polygon(x = c(tmp.hilo$xhi, tmp.hilo$xlo), y = c(tmp.hilo$yhi, tmp.hilo$ylo), border = NA, col = col.prd(alpha=.4) )
## tmp.hilo <- gen.ci("morena")    
## polygon(x = c(tmp.hilo$xhi, tmp.hilo$xlo), y = c(tmp.hilo$yhi, tmp.hilo$ylo), border = NA, col = col.morena(alpha=.4) )
##
## ## grayish traces
## lines( smooth.spline(x = date[is.na(d$pan)   ==FALSE], y = d$pan   [is.na(d$pan)   ==FALSE]*100, df = 10), lty = 1, lwd = .75, col = col.pan   (alpha=.2) )
## lines( smooth.spline(x = date[is.na(d$pri)   ==FALSE], y = d$pri   [is.na(d$pri)   ==FALSE]*100, df = 10), lty = 1, lwd = .75, col = col.pri   (alpha=.2) )
## lines( smooth.spline(x = date[is.na(d$prd)   ==FALSE], y = d$prd   [is.na(d$prd)   ==FALSE]*100, df = 10), lty = 1, lwd = .75, col = col.prd   (alpha=.2) )
## lines( smooth.spline(x = date[is.na(d$morena)==FALSE], y = d$morena[is.na(d$morena)==FALSE]*100, df =  4), lty = 1, lwd = .75, col = col.morena(alpha=.2) )
##
## points(date, d$pan*100, col=col.pan(alpha=.02), pch = 1, cex = .5)
## lines( smooth.spline(x = date[is.na(d$pan)==FALSE], y = d$pan[is.na(d$pan)==FALSE]*100, df = 10), lty = 1, lwd = 2, col = col.pan(alpha=1) )
## ##
## points(date, d$pri*100, col=col.pri(alpha=.02), pch = 1, cex = .5)
## lines( smooth.spline(x = date[is.na(d$pri)==FALSE], y = d$pri[is.na(d$pri)==FALSE]*100, df = 10), lty = 1, lwd = 2, col = col.pri(alpha=1) )
## ##
## points(date, d$prd*100, col=col.prd(alpha=.02), pch = 1, cex = .5)
## lines( smooth.spline(x = date[is.na(d$prd)==FALSE], y = d$prd[is.na(d$prd)==FALSE]*100, df = 10), lty = 1, lwd = 2, col = col.prd(alpha=1) )
## ##
## points(date, d$morena*100, col=col.morena(alpha=.02), pch = 1, cex = .5)
## lines( smooth.spline(x = date[is.na(d$morena)==FALSE], y = d$morena[is.na(d$morena)==FALSE]*100, df = 4), lty = 1, lwd = 2, col = col.morena(alpha=1) )
## ##
## points(date, d$mc*100, col=col.mc(alpha=.02), pch = 1, cex = .5)
## lines( smooth.spline(x = date[is.na(d$mc)==FALSE], y = d$mc[is.na(d$mc)==FALSE]*100, df = 8), lty = 1, lwd = 2, col = col.mc(alpha=1) )
## ##
## points(date, d$pvem*100, col=col.pvem(alpha=.02), pch = 1, cex = .5)
## lines( smooth.spline(x = date[is.na(d$pvem)==FALSE], y = d$pvem[is.na(d$pvem)==FALSE]*100, df = 8), lty = 1, lwd = 2, col = col.pvem(alpha=1) )
##
## put all party points in a sorted object so they are not plotted party-by-party (too much emphasis on last party added)
allpts <- rbind(
    data.frame(date=date, point=d$pan,    clr=col.pan   (alpha=.05)),
    data.frame(date=date, point=d$pri   , clr=col.pri   (alpha=.05)),
    data.frame(date=date, point=d$prd   , clr=col.prd   (alpha=.05)),
    data.frame(date=date, point=d$pvem  , clr=col.pvem  (alpha=.05)),
    data.frame(date=date, point=d$mc    , clr=col.mc    (alpha=.05)),
    data.frame(date=date, point=d$morena, clr=col.morena(alpha=.05))
)

legend(x=15000, y=102, legend = c(c("Center-right (PAN)","PRI","Left (PRD)","Morena","Mov. Ciud.","Green (PVEM)","Others")), col = c(col.pan(),col.pri(),col.prd(),col.morena(),col.mc(),col.pvem(),col.oth()), lty = 1, lwd=1.5, bg = "white")
##
## add legend before points so the few in region are visible
allpts <- allpts[order(allpts$point),] # sort so that points are not plottet party by party
points(allpts$date, allpts$point*100, col=allpts$clr, pch = 1, cex = .5)
##
## ## smooth mean
## lines( smooth.spline(x = date[is.na(d$pan)==FALSE],    y = d$pan[is.na(d$pan)==FALSE]*100,       df = 10), lty = 1, lwd = 2, col = col.pan() )
## lines( smooth.spline(x = date[is.na(d$pri)==FALSE],    y = d$pri[is.na(d$pri)==FALSE]*100,       df = 10), lty = 1, lwd = 2, col = col.pri() )
## lines( smooth.spline(x = date[is.na(d$prd)==FALSE],    y = d$prd[is.na(d$prd)==FALSE]*100,       df = 10), lty = 1, lwd = 2, col = col.prd() )
## lines( smooth.spline(x = date[is.na(d$morena)==FALSE], y = d$morena[is.na(d$morena)==FALSE]*100, df = 4),  lty = 1, lwd = 2, col = col.morena() )
## lines( smooth.spline(x = date[is.na(d$mc)==FALSE],     y = d$mc[is.na(d$mc)==FALSE]*100,         df = 10), lty = 1, lwd = 2, col = col.mc() )
## lines( smooth.spline(x = date[is.na(d$pvem)==FALSE],   y = d$pvem[is.na(d$pvem)==FALSE]*100,     df = 10), lty = 1, lwd = 2, col = col.pvem() )
##
## smooth median
tmp <- smooth.spline(x = q5$yr[!is.na(q5$oth)],    y = q5$oth[!is.na(q5$oth)]*100,       df = 7)
tmp$y[tmp$y<0] <- 0; tmp$y[tmp$y>100] <- 100
lines(tmp , lty = 1, lwd = 2, col = col.oth() )
##
tmp <- smooth.spline(x = q5$yr[!is.na(q5$prd)],    y = q5$prd[!is.na(q5$prd)]*100,       df = 7)
tmp$y[tmp$y<0] <- 0; tmp$y[tmp$y>100] <- 100
lines(tmp , lty = 1, lwd = 2, col = col.prd() )
##
tmp <- smooth.spline(x = q5$yr[!is.na(q5$pan)],    y = q5$pan[!is.na(q5$pan)]*100,       df = 7)
tmp$y[tmp$y<0] <- 0; tmp$y[tmp$y>100] <- 100
lines(tmp , lty = 1, lwd = 2, col = col.pan() )
##
tmp <- smooth.spline(x = q5$yr[!is.na(q5$pri)],    y = q5$pri[!is.na(q5$pri)]*100,       df = 7)
tmp$y[tmp$y<0] <- 0; tmp$y[tmp$y>100] <- 100
lines(tmp , lty = 1, lwd = 2, col = col.pri() )
##
tmp <- smooth.spline(x = q5$yr[!is.na(q5$morena)], y = q5$morena[!is.na(q5$morena)]*100, df = 4)
tmp$y[tmp$y<0] <- 0; tmp$y[tmp$y>100] <- 100
lines(tmp , lty = 1, lwd = 2, col = col.morena() )
##
tmp <- smooth.spline(x = q5$yr[!is.na(q5$mc)],     y = q5$mc[!is.na(q5$mc)]*100,         df = 7)
tmp$y[tmp$y<0] <- 0; tmp$y[tmp$y>100] <- 100
lines(tmp , lty = 1, lwd = 2, col = col.mc() )
##
tmp <- smooth.spline(x = q5$yr[!is.na(q5$pvem)],   y = q5$pvem[!is.na(q5$pvem)]*100,     df = 7)
tmp$y[tmp$y<0] <- 0; tmp$y[tmp$y>100] <- 100
lines(tmp , lty = 1, lwd = 2, col = col.pvem() )
##
dev.off()

 

