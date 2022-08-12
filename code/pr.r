rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
setwd(dd)

# read 2006 by district
dat <- read.csv(file = "../datosBrutos/eum2006prdf.csv", stringsAsFactors = FALSE)
dat <- within(dat, {
    disn <- NULL;
    cab <- NULL;
    })
colnames(dat)
# aggregate by states
dat <- within(dat, {
    pan          <- ave(pan         , as.factor(edon), FUN=sum, na.rm=TRUE)
    pri.pvem     <- ave(pri.pvem    , as.factor(edon), FUN=sum, na.rm=TRUE)
    prd.pt.conve <- ave(prd.pt.conve, as.factor(edon), FUN=sum, na.rm=TRUE)
    pna          <- ave(pna         , as.factor(edon), FUN=sum, na.rm=TRUE)
    asdc         <- ave(asdc        , as.factor(edon), FUN=sum, na.rm=TRUE)
    nr           <- ave(nr          , as.factor(edon), FUN=sum, na.rm=TRUE)
    nul          <- ave(nul         , as.factor(edon), FUN=sum, na.rm=TRUE)
    tot          <- ave(tot         , as.factor(edon), FUN=sum, na.rm=TRUE)
    lisnom       <- ave(lisnom      , as.factor(edon), FUN=sum, na.rm=TRUE)
})
dat <- dat[duplicated(dat$edon)==FALSE,]
write.csv(dat, file="../datosBrutos/eum2006pred.csv")

# read 2012 by district
dat <- read.csv(file = "../datosBrutos/eum2012prdf-coal-split.csv", stringsAsFactors = FALSE)
dat <- within(dat, {
    disn <- NULL;
    cab <- NULL;
    note <- NULL;
})
dat$pri.pvem <- NA; dat$prd.pt.mc <- NA;
head(dat)
# aggregate by states
dat <- within(dat, {
    pan    <- ave(pan   , as.factor(edon), FUN=sum, na.rm=TRUE)
    pri    <- ave(pri   , as.factor(edon), FUN=sum, na.rm=TRUE)
    prd    <- ave(prd   , as.factor(edon), FUN=sum, na.rm=TRUE)
    pvem   <- ave(pvem  , as.factor(edon), FUN=sum, na.rm=TRUE)
    pt     <- ave(pt    , as.factor(edon), FUN=sum, na.rm=TRUE)
    mc     <- ave(mc    , as.factor(edon), FUN=sum, na.rm=TRUE)
    pna    <- ave(pna   , as.factor(edon), FUN=sum, na.rm=TRUE)
    nr     <- ave(nr    , as.factor(edon), FUN=sum, na.rm=TRUE)
    nul    <- ave(nul   , as.factor(edon), FUN=sum, na.rm=TRUE)
    tot    <- ave(tot   , as.factor(edon), FUN=sum, na.rm=TRUE)
})
dat <- dat[duplicated(dat$edon)==FALSE,]
# remove 1 vote from coalition partner and add it to coalition sum to indicate its presence
dat <- within(dat, {
    pri.pvem  <- 2
    prd.pt.mc <- 3
    pri  <- pri  - 1
    pvem <- pvem - 1
    prd  <- prd  - 1
    pt   <- pt   - 1
    mc   <- mc   - 1
})
# compute efec
dat <- within(dat, efec <- pan + pri + prd + pvem + pt + mc + pna + pri.pvem + prd.pt.mc)
# sort columns
dat <- dat[, c("edon", "estado", "pan", "pri", "prd", "pvem", "pt", "mc", "pna", "pri.pvem", "prd.pt.mc", "efec", "nr", "nul", "tot", "fuente")]
head(dat)
write.csv(dat, file="../datosBrutos/eum2012pred.csv")


# read 2018 by district
dat <- read.csv(file = "../datosBrutos/eum2018prdf.csv", stringsAsFactors = FALSE)
## # aggregate coalitions
## head(dat)
## dat <- within(dat, {
##     pan.prd.mc <- pan + prd + mc + pan.prd.mc;
##     pri.pvem.pna <- pri + pvem + pna + pri.pvem.pna;
##     morena.pt.pes <- morena + pt + pes + morena.pt.pes;
##     })
dat <- within(dat, {
    ## pan    <- NULL;
    ## pri    <- NULL;
    ## prd    <- NULL;
    ## mc     <- NULL;
    ## pvem   <- NULL;
    ## pna    <- NULL;
    ## pt     <- NULL;
    ## morena <- NULL;
    ## pes    <- NULL;
    disn <- NULL;
    cab <- NULL;
    })
colnames(dat)
# aggregate by states
dat <- within(dat, {
    pan           <- ave(pan          , as.factor(edon), FUN=sum, na.rm=TRUE)
    pri           <- ave(pri          , as.factor(edon), FUN=sum, na.rm=TRUE)
    prd           <- ave(prd          , as.factor(edon), FUN=sum, na.rm=TRUE)
    mc            <- ave(mc           , as.factor(edon), FUN=sum, na.rm=TRUE)
    pvem          <- ave(pvem         , as.factor(edon), FUN=sum, na.rm=TRUE)
    pna           <- ave(pna          , as.factor(edon), FUN=sum, na.rm=TRUE)
    pt            <- ave(pt           , as.factor(edon), FUN=sum, na.rm=TRUE)
    morena        <- ave(morena       , as.factor(edon), FUN=sum, na.rm=TRUE)
    pes           <- ave(pes          , as.factor(edon), FUN=sum, na.rm=TRUE)
    pan.prd.mc    <- ave(pan.prd.mc   , as.factor(edon), FUN=sum, na.rm=TRUE)
    pri.pvem.pna  <- ave(pri.pvem.pna , as.factor(edon), FUN=sum, na.rm=TRUE)
    morena.pt.pes <- ave(morena.pt.pes, as.factor(edon), FUN=sum, na.rm=TRUE)
    efec          <- ave(efec         , as.factor(edon), FUN=sum, na.rm=TRUE)
    lisnom        <- ave(lisnom       , as.factor(edon), FUN=sum, na.rm=TRUE)
})
dat <- dat[duplicated(dat$edon)==FALSE,]
write.csv(dat, file="../datosBrutos/eum2018pred.csv")

head(dat)
colnames(dat)


