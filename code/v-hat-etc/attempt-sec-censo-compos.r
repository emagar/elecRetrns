
# duplicate censo for manipulation
censh <- censo
##
# add cols that will receive p18 projections for elec yrs
censh <- within(censh, {
    p18_1994 <- p18_1997 <- p18_2003 <- p18_2006 <- p18_2009 <- p18_2012 <- p18_2015 <- p18_2018 <- p18_2021 <- NA
})
censh <- censh[order(censh$ife2006, censh$seccion),] # sort mun

###############################
## inspect/manipulate zeroes ##
###############################
## all zeroes remain so
sel.r <- which(censh$p18_2005==0 & censh$p18_2010==0 & censh$p18_2020==0) ## all zero (but have votes, eg. campo militar 1)
table(censh$alta[sel.r], useNA = "ifany")
table(censh$baja[sel.r], useNA = "ifany")
censh[sel.r,] <- within(censh[sel.r,], {
    p18_1994 <- p18_1997 <- p18_2003 <- p18_2006 <- p18_2009 <- p18_2012 <- p18_2015 <- p18_2018 <- p18_2021 <- 0
})
## Take these out of projection routine to exclude log predictions
censh.out <- censh[ sel.r,]
censh <-     censh[-sel.r,]
## move these to all zero bunch
sel.r <- which(censh$p18_2005==0 & censh$baja==2008)
censh[sel.r,c("p18_2005","p18_2010","p18_2020")]
table(censh$baja[sel.r], useNA = "ifany")
censh[sel.r,] <- within(censh[sel.r,], {
    p18_1994 <- p18_1997 <- p18_2003 <- p18_2006 <- p18_2009 <- p18_2010 <- p18_2012 <- p18_2015 <- p18_2018 <- p18_2020 <- p18_2021 <- 0
})
censh.out <- rbind(censh.out, censh[ sel.r,])
censh <-                      censh[-sel.r,]
## move these to all zero bunch
sel.r <-              which(censh$p18_2005==0 & censh$p18_2010==0 & is.na(censh$p18_2020)) ## two zero
sel.r <- union(sel.r, which(is.na(censh$p18_2005) & censh$p18_2010==0 & censh$p18_2020==0)) ## two zero
sel.r <- union(sel.r, which(is.na(censh$p18_2005) & censh$p18_2010==0 & is.na(censh$p18_2020))) ## two zero
censh[sel.r,c("p18_2005","p18_2010","p18_2020")]
table(censh$alta[sel.r], useNA = "ifany")
table(censh$baja[sel.r], useNA = "ifany")
censh[sel.r,] <- within(censh[sel.r,], {
    p18_1994 <- p18_1997 <- p18_2003 <- p18_2005 <- p18_2006 <- p18_2009 <- p18_2010 <- p18_2012 <- p18_2015 <- p18_2018 <- p18_2020 <- p18_2021 <- 0
})
censh.out <- rbind(censh.out, censh[ sel.r,])
censh <-                      censh[-sel.r,]
## move these to all zero bunch
sel.r <- which(is.na(censh$p18_2005) & is.na(censh$p18_2010) & is.na(censh$p18_2020))
table(censh$alta[sel.r], useNA = "ifany")
table(censh$baja[sel.r], useNA = "ifany")
censh[sel.r,] <- within(censh[sel.r,], {
    p18_1994 <- p18_1997 <- p18_2003 <- p18_2005 <- p18_2006 <- p18_2009 <- p18_2010 <- p18_2012 <- p18_2015 <- p18_2018 <- p18_2020 <- p18_2021 <- 0
})
censh.out <- rbind(censh.out, censh[ sel.r,])
censh <-                      censh[-sel.r,]
## will give flat demography post alta (given single census)
sel.r <- which(is.na(censh$p18_2005) & is.na(censh$p18_2010))
table(censh$alta[sel.r], useNA = "ifany")
##
sel.r <-              which(is.na(censh$p18_2005) & is.na(censh$p18_2010) & censh$alta==2012)
sel.r <- union(sel.r, which(is.na(censh$p18_2005) & is.na(censh$p18_2010) & censh$alta==1994)) # these changed to alta 2012 in eqSecc.xls 
censh[sel.r,] <- within(censh[sel.r,], {
    p18_1994 <- p18_1997 <- p18_2003 <- p18_2005 <- p18_2006 <- p18_2009 <- p18_2010 <- 0;
    p18_2012 <- p18_2015 <- p18_2018 <- p18_2021 <- p18_2020
})
censh.out <- rbind(censh.out, censh[ sel.r,])
censh <-                      censh[-sel.r,]
##
sel.r <- which(is.na(censh$p18_2005) & is.na(censh$p18_2010) & (censh$alta==2013|censh$alta==2014|censh$alta==2015))
censh[sel.r,] <- within(censh[sel.r,], {
    p18_1994 <- p18_1997 <- p18_2003 <- p18_2005 <- p18_2006 <- p18_2009 <- p18_2010 <- p18_2012 <- 0;
    p18_2015 <- p18_2018 <- p18_2021 <- p18_2020
})
censh.out <- rbind(censh.out, censh[ sel.r,])
censh <-                      censh[-sel.r,]
##
sel.r <- which(is.na(censh$p18_2005) & is.na(censh$p18_2010) & (censh$alta==2016|censh$alta==2017))
censh[sel.r,] <- within(censh[sel.r,], {
    p18_1994 <- p18_1997 <- p18_2003 <- p18_2005 <- p18_2006 <- p18_2009 <- p18_2010 <- p18_2012 <- p18_2015 <- 0;
    p18_2018 <- p18_2021 <- p18_2020
})
censh.out <- rbind(censh.out, censh[ sel.r,])
censh <-                      censh[-sel.r,]
##
sel.r <- which(is.na(censh$p18_2005) & is.na(censh$p18_2010) & (censh$alta==2019|censh$alta==2020))
censh[sel.r,] <- within(censh[sel.r,], {
    p18_1994 <- p18_1997 <- p18_2003 <- p18_2005 <- p18_2006 <- p18_2009 <- p18_2010 <- p18_2012 <- p18_2015 <- p18_2018 <- 0;
    p18_2021 <- p18_2020
})
censh.out <- rbind(censh.out, censh[ sel.r,])
censh <-                      censh[-sel.r,]
## assign these flat demography between alta and baja
sel.r <- which(is.na(censh$p18_2005) & is.na(censh$p18_2020))
censh[sel.r, c("seccion","p18_2005","p18_2010","p18_2020")]
table(alta=censh$alta[sel.r], baja=censh$baja[sel.r], useNA = "ifany")
##
sel.r <- which(is.na(censh$p18_2005) & is.na(censh$p18_2020) & censh$baja==2019)
censh[sel.r,] <- within(censh[sel.r,], {
    p18_1994 <- p18_1997 <- p18_2003 <- p18_2005 <- p18_2006 <- p18_2020 <- p18_2021 <- 0;
    p18_2009 <- p18_2012 <- p18_2015 <- p18_2018 <- p18_2010
})
censh.out <- rbind(censh.out, censh[ sel.r,])
censh <-                      censh[-sel.r,]
##
sel.r <- which(is.na(censh$p18_2005) & is.na(censh$p18_2020) & censh$baja==2020)
censh[sel.r,] <- within(censh[sel.r,], {
    p18_1994 <- p18_1997 <- p18_2003 <- p18_2005 <- p18_2006 <- p18_2021 <- 0;
    p18_2009 <- p18_2012 <- p18_2015 <- p18_2018 <- p18_2020 <- p18_2010
})
censh.out <- rbind(censh.out, censh[ sel.r,])
censh <-                      censh[-sel.r,]
##
## these get flat demography up to baja
sel.r <- which(is.na(censh$p18_2010) & is.na(censh$p18_2020))
table(alta=censh$alta[sel.r], baja=censh$baja[sel.r], useNA = "ifany")
censh$p18_2005[sel.r]
##
sel.r <- which(is.na(censh$p18_2010) & is.na(censh$p18_2020) & (censh$baja==2007|censh$baja==2008|censh$baja==2009))
censh[sel.r,] <- within(censh[sel.r,], {
    p18_1994 <- p18_1997 <- p18_2003 <- p18_2006 <- p18_2005;
    p18_2009 <- p18_2012 <- p18_2015 <- p18_2018 <- p18_2020 <- p18_2021 <- 0
})
censh.out <- rbind(censh.out, censh[ sel.r,])
censh <-                      censh[-sel.r,]
##
sel.r <- which(is.na(censh$p18_2010) & is.na(censh$p18_2020) & (censh$baja==2010|censh$baja==2011))
censh[sel.r,] <- within(censh[sel.r,], {
    p18_1994 <- p18_1997 <- p18_2003 <- p18_2006 <- p18_2009 <- p18_2005;
    p18_2012 <- p18_2015 <- p18_2018 <- p18_2020 <- p18_2021 <- 0
})
censh.out <- rbind(censh.out, censh[ sel.r,])
censh <-                      censh[-sel.r,]

## aqui me quedé
sel.r <- which(is.na(censh$p18_2005))
censh[sel.r[1], c("seccion","p18_2005","p18_2010","p18_2020")]
table(alta=censh$alta[sel.r], baja=censh$baja[sel.r], useNA = "ifany")

sel.r <- which(is.na(censh$p18_2010==0))
sel.r <- which(is.na(censh$p18_2020==0))
censh[sel.r[1], c("seccion","p18_2005","p18_2010","p18_2020")]
dim(censh.out)


## will make these have flat demog 2005-2010
sel.r <- which(censh$p18_2005==0)
censh$p18_2005[sel.r] <- censh$p18_2010[sel.r]

sel.r <- which(censh$p18_2010==0)
sel.r <- which(censh$p18_2020==0)
sel.r <- which(is.na(censh$p18_2010==0))
sel.r <- which(is.na(censh$p18_2020==0)
censh[sel.r[6], c("seccion","alta","baja","p18_2005","p18_2010","p18_2020")]
censh[sel.r[1],]

sel.r <- which(censh$p18_2005==0)
censh$baja[sel.r]
censh[sel.r[1],]
censh[which(censh$p18_2005<=0),c("p18_2005","p18_2010","p18_2020")]
censh$p18_2005[which(censh$p18_2005<=0)]
)


tmp.drop <- grep("^edo$|edon|mun|^ife$|inegi|dis|OBSER|action|orig|when|comen|ptot", colnames(censh)) # drop cols
tmp.drop <- grep("^edo$|edon|mun|^ife$|inegi|dis|OBSER|action|orig|when|comen|alta|baja|ptot", colnames(censh)) # drop cols
censh <- censh[,-tmp.drop]
censh[1,]

## columns will receive mun aggragates
censh$p18_2005m06 <- censh$p18_2005
censh$p18_2010m06 <- censh$p18_2010 # duplicate column for manipulation
censh$p18_2020m06 <- censh$p18_2020 # duplicate column for manipulation
sel.c <- c("p18_2005m06", "p18_2010m06", "p18_2020m06")
censh <- my_agg(d=censh, sel.c=sel.c, by="ife2006", drop.dupli=FALSE)
##censh <- my_agg(d=censh, sel.c=sel.c, by="ife2009", drop.dupli=FALSE) ## maybe this one needed for 2010 censo 
##censh <- my_agg(d=censh, sel.c=sel.c, by="ife2021", drop.dupli=FALSE) ## maybe this one needed for 2020 censo 

# seccion p18 as share of municipio p18
censh <- within(censh, {
    p18_2005 <- p18_2005 / p18_2005m06
    p18_2010 <- p18_2010 / p18_2010m06
    p18_2020 <- p18_2020 / p18_2020m06
    })

# inspect zeroes
censh[tmp[1],]
tmp <- which(censh$p18_2005<=0)
censh[which(censh$p18_2005<=0),c("p18_2005","p18_2010","p18_2020")]
censh$p18_2005[which(censh$p18_2005<=0)]
)

## within municipio, generate p18 ratios of 1st seccion
cenr <- split(x=censh, f=censh$ife2006) # split into list of data frames, one per municipio, to compute r
sel.c <- c("p18_2005", "p18_2010", "p18_2020")
tmp.f <- function(x=NA){
    ##x <- cenr[[1005]] # debug
    tmp.denom <- do.call(rbind, replicate(n=nrow(x), x[1, sel.c], simplify = FALSE)) # replicate 1st seccion times secciones in mun
    x[, sel.c] <- x[, sel.c] / tmp.denom     # divide each row by 1st row
    x$dskip <- 0; x$dskip[1] <- 1            # add dummy for 1st obs
    return(x)
}
cenr <- lapply(cenr, tmp.f) # apply function
cenr <- do.call(rbind, cenr) # return to data frame form
cenr <- cenr[,grep("seccion|ord|dskip|p18_[0-9]{4}$", colnames(cenr))] # keep ids, rs, dskip only 
cenr[cenr$dskip==0,]

## get census projection functions
source("../../code/v-hat-etc/interpolate-census-functions.r")

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

## inspect log-linear predictions
tmp.inspect <- tmp.e
tmp.inspect <- apply(tmp.e, c(1,2), function(x) as.numeric(x<0))
tmp.inspect[is.na(tmp.inspect)] <- 0
apply(tmp.inspect, 2, sum) ## no negative predictions!!!



censom03[1,]
2005
censom06[1,] <--
censom09[1,] <--
2010
censom12[1,]
censom15[1,]
censom18[1,]
2020
censom21[1,] <--

