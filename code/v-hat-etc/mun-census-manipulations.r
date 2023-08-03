Module runs from elec-data-for-maps.
Takes counterfactual census maps at municipio level, done with seccion-level data.

#############################################################################
## Project mun pops from saved censos to convert sh.hats back into nm.hats ##
#############################################################################
tmp <- censom94 # use latest municipio map
tmp[12:17,]
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
tmp$p18_1990 <- tmp$p18_1995 <- tmp$edon <- tmp$mun <- tmp$ife <- NULL
#tmp$p18_2005 <- tmp$p18_2010 <- tmp$p18_2020 <- NULL
#colnames(tmp) <- sub("^p18_[12][90]", "p18m_", colnames(tmp))
colnames(tmp) <- sub("^p18_", "p18m_", colnames(tmp))
dim(tmp)
tmp[1,]
## pick municipios needed
tmp2 <- data.frame(ord=1:nrow(nm), inegi=nm$inegi)
tmp2 <- merge(x=tmp2, y=tmp, by="inegi", all.x=TRUE, all.y=FALSE)
tmp2 <- tmp2[order(tmp2$ord),]; tmp2$ord <- NULL
tmp <- tmp2; rm(tmp2)
tmp[1:3,]
## plug el.yr mun pops to nm
tmp2 <- nm
table(tmp$inegi==tmp2$inegi)
tmp2 <- cbind(tmp2, tmp[,-1])
table(tmp2$p18m_05 - tmp2$p18m_2005) #check
table(tmp2$p18m_10 - tmp2$p18m_2010) #check
table(tmp2$p18m_20 - tmp2$p18m_2020) #check
#tmp2 <- nm
