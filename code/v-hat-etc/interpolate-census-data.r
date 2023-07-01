######################################################################
## Script to generate inter-census populations.                     ##
## Adds p18 to district, municipio, and seccion vote objects.       ##
## Invoked from elec-data-for-maps.r,.                              ##
## Has routine (commented out) used to evaluate projection methods, ##
## discussed in blog entry.                                         ##
##                                                                  ##
## Author: Eric Magar                                               ##
## emagar at itam dot mx                                            ##
## Date: 30jun2023                                                  ##
## Last modified: 1jul2023                                          ##
######################################################################

## get census projection functions
source("../../code/v-hat-etc/interpolate-census-functions.r")

#############################################
## ADD PROJECTIONS TO DISTRICT CENSUS MAPS ##
#############################################

## Function to generate one year's municipal projections.
projyr <- function(what="p18", yr=NA, census.data = NA){
    ##what <- "p18"; yr <- 1991; census.data <- censod06 # debug
    if (yr<1997)            proj <- censod79[, c("edon","disn")]                 # df with all districts 
    if (yr>=1997 & yr<2006) proj <- censod97[, c("edon","disn")]                 # df with all districts 
    if (yr>=2006 & yr<2018) proj <- censod06[, c("edon","disn")]                 # df with all districts 
    if (yr>=2018)           proj <- censod18[, c("edon","disn")]                 # df with all districts 
    proj <- within(proj,{
        ##pob18s <- rqmrs <- NA                                                  # slots for linear regression predictions/stats
        pob18e <- rqmre <- NA                                                    # slots for log-linear regression predictions/stats
        pob18s <- interpol(what=what, yr=yr, unit="d", census.data=census.data)  # segments predictions
        yr <- yr;
    })
    tmp <- interlog(what=what, yr=yr, unit="d", census.data=census.data, frm="log(dv)~iv")             # log models
    proj$pob18e <- tmp[[1]]                                                                            # log predictions
    proj$rqmre <- unlist(lapply(tmp[[2]], function(x) round(mean(sqrt(x$sq.resid)) / mean(x$dv) , 3))) # quadratic mean resid / mean pop
    ##
    ## tmp <- interlog(what=what, yr=yr, unit="m", census.data=census.data, frm="dv~iv")                  # linear models
    ## proj$pob18s <- tmp[[1]]                                                                            # linear predictions
    ## proj$rqmrs <- unlist(lapply(tmp[[2]], function(x) round(mean(sqrt(x$sq.resid)) / mean(x$dv) , 3))) # quadratic mean resid / mean pop
    ##
    return(proj) # returns a data frame with ids yr p18 and relative quad mean of residuals to gauge model fit (done for census years)
}

## wrapper
myf <- function(what=what, yr=yr, census.data=census.data){ # function to paste cols
    ##what <- "p18"; yr <- 1991; census.data <- censod06 # debug
    tmp <- projyr(what=what, yr=yr, census.data = census.data)  # yr's projection
    if (yr>=1990 & yr<=2020){ 
        tmp2 <- tmp$pob18s           # btw censuses use segments
    } else {
        tmp2 <- tmp$pob18e           # else log-linear
    } 
    return(tmp2) # returns vector with yr's projection
    ## tmp3 <- census.data
    ## tmp3[, paste0("p18_", yr)] <- tmp2 # add appropriately named column
    ## ##assign(paste0("censom", as.integer((yr/100 - as.integer(yr/100))*100), "$p18_", yr)) <- tmp2
    ## tmp3[1,]
    ## return(tmp3) # returns manipulated data frame w id yr proj res.stat
}

## 1979 district map
tmp2 <- censod79 # duplicate for manipulation
tmp2$p18_1988 <- myf(what="p18", yr=1988, census.data = censod79)
tmp2$p18_1991 <- myf(what="p18", yr=1991, census.data = censod79)
tmp2$p18_1994 <- myf(what="p18", yr=1994, census.data = censod79)
tmp2$p18_1997 <- myf(what="p18", yr=1997, census.data = censod79)
tmp2$p18_2000 <- myf(what="p18", yr=2000, census.data = censod79)
tmp2$p18_2003 <- myf(what="p18", yr=2003, census.data = censod79)
tmp2$p18_2006 <- myf(what="p18", yr=2006, census.data = censod79)
tmp2$p18_2009 <- myf(what="p18", yr=2009, census.data = censod79)
tmp2$p18_2012 <- myf(what="p18", yr=2012, census.data = censod79)
tmp2$p18_2015 <- myf(what="p18", yr=2015, census.data = censod79)
tmp2$p18_2018 <- myf(what="p18", yr=2018, census.data = censod79)
tmp2$p18_2021 <- myf(what="p18", yr=2021, census.data = censod79)
# sort cols keeping only elec yrs
tmp2 <- tmp2[, order(colnames(tmp2))]
censod79 <- tmp2 # replace original object w manipulation

## 1997 district map
tmp2 <- censod97 # duplicate for manipulation
tmp2$p18_1988 <- myf(what="p18", yr=1988, census.data = censod97)
tmp2$p18_1991 <- myf(what="p18", yr=1991, census.data = censod97)
tmp2$p18_1994 <- myf(what="p18", yr=1994, census.data = censod97)
tmp2$p18_1997 <- myf(what="p18", yr=1997, census.data = censod97)
tmp2$p18_2000 <- myf(what="p18", yr=2000, census.data = censod97)
tmp2$p18_2003 <- myf(what="p18", yr=2003, census.data = censod97)
tmp2$p18_2006 <- myf(what="p18", yr=2006, census.data = censod97)
tmp2$p18_2009 <- myf(what="p18", yr=2009, census.data = censod97)
tmp2$p18_2012 <- myf(what="p18", yr=2012, census.data = censod97)
tmp2$p18_2015 <- myf(what="p18", yr=2015, census.data = censod97)
tmp2$p18_2018 <- myf(what="p18", yr=2018, census.data = censod97)
tmp2$p18_2021 <- myf(what="p18", yr=2021, census.data = censod97)
# sort cols keeping only elec yrs
tmp2 <- tmp2[, order(colnames(tmp2))]
censod97 <- tmp2 # replace original object w manipulation

## 2006 district map
tmp2 <- censod06 # duplicate for manipulation
tmp2$p18_1988 <- myf(what="p18", yr=1988, census.data = censod06)
tmp2$p18_1991 <- myf(what="p18", yr=1991, census.data = censod06)
tmp2$p18_1994 <- myf(what="p18", yr=1994, census.data = censod06)
tmp2$p18_1997 <- myf(what="p18", yr=1997, census.data = censod06)
tmp2$p18_2000 <- myf(what="p18", yr=2000, census.data = censod06)
tmp2$p18_2003 <- myf(what="p18", yr=2003, census.data = censod06)
tmp2$p18_2006 <- myf(what="p18", yr=2006, census.data = censod06)
tmp2$p18_2009 <- myf(what="p18", yr=2009, census.data = censod06)
tmp2$p18_2012 <- myf(what="p18", yr=2012, census.data = censod06)
tmp2$p18_2015 <- myf(what="p18", yr=2015, census.data = censod06)
tmp2$p18_2018 <- myf(what="p18", yr=2018, census.data = censod06)
tmp2$p18_2021 <- myf(what="p18", yr=2021, census.data = censod06)
# sort cols keeping only elec yrs
tmp2 <- tmp2[, order(colnames(tmp2))]
censod06 <- tmp2 # replace original object w manipulation

## 2018 district map
tmp2 <- censod18 # duplicate for manipulation
tmp2$p18_1988 <- myf(what="p18", yr=1988, census.data = censod18)
tmp2$p18_1991 <- myf(what="p18", yr=1991, census.data = censod18)
tmp2$p18_1994 <- myf(what="p18", yr=1994, census.data = censod18)
tmp2$p18_1997 <- myf(what="p18", yr=1997, census.data = censod18)
tmp2$p18_2000 <- myf(what="p18", yr=2000, census.data = censod18)
tmp2$p18_2003 <- myf(what="p18", yr=2003, census.data = censod18)
tmp2$p18_2006 <- myf(what="p18", yr=2006, census.data = censod18)
tmp2$p18_2009 <- myf(what="p18", yr=2009, census.data = censod18)
tmp2$p18_2012 <- myf(what="p18", yr=2012, census.data = censod18)
tmp2$p18_2015 <- myf(what="p18", yr=2015, census.data = censod18)
tmp2$p18_2018 <- myf(what="p18", yr=2018, census.data = censod18)
tmp2$p18_2021 <- myf(what="p18", yr=2021, census.data = censod18)
# sort cols keeping only elec yrs
tmp2 <- tmp2[, order(colnames(tmp2))]
censod18 <- tmp2 # replace original object w manipulation

###############################
## ADD P18 TO v..d.. objects ##
###############################
v94d   <- merge(x = v94d,   y = censod79[, c("disn","p18_1994")], by = "disn", all.x = TRUE, all.y = FALSE)
v94d97 <- merge(x = v94d97, y = censod97[, c("disn","p18_1994")], by = "disn", all.x = TRUE, all.y = FALSE)
v94d06 <- merge(x = v94d06, y = censod06[, c("disn","p18_1994")], by = "disn", all.x = TRUE, all.y = FALSE)
v94d18 <- merge(x = v94d18, y = censod18[, c("disn","p18_1994")], by = "disn", all.x = TRUE, all.y = FALSE)
colnames(v94d)  [grep("p18_", colnames(v94d))]   <- "p18" 
colnames(v94d97)[grep("p18_", colnames(v94d97))] <- "p18"
colnames(v94d06)[grep("p18_", colnames(v94d06))] <- "p18"
colnames(v94d18)[grep("p18_", colnames(v94d18))] <- "p18"

v97d79 <- merge(x = v97d79, y = censod79[, c("disn","p18_1997")], by = "disn", all.x = TRUE, all.y = FALSE)
v97d   <- merge(x = v97d  , y = censod97[, c("disn","p18_1997")], by = "disn", all.x = TRUE, all.y = FALSE)
v97d06 <- merge(x = v97d06, y = censod06[, c("disn","p18_1997")], by = "disn", all.x = TRUE, all.y = FALSE)
v97d18 <- merge(x = v97d18, y = censod18[, c("disn","p18_1997")], by = "disn", all.x = TRUE, all.y = FALSE)
colnames(v97d79)[grep("p18_", colnames(v97d79))] <- "p18" 
colnames(v97d)  [grep("p18_", colnames(v97d))]   <- "p18"
colnames(v97d06)[grep("p18_", colnames(v97d06))] <- "p18"
colnames(v97d18)[grep("p18_", colnames(v97d18))] <- "p18"

v00d79 <- merge(x = v00d79, y = censod79[, c("disn","p18_2000")], by = "disn", all.x = TRUE, all.y = FALSE)
v00d   <- merge(x = v00d  , y = censod97[, c("disn","p18_2000")], by = "disn", all.x = TRUE, all.y = FALSE)
v00d06 <- merge(x = v00d06, y = censod06[, c("disn","p18_2000")], by = "disn", all.x = TRUE, all.y = FALSE)
v00d18 <- merge(x = v00d18, y = censod18[, c("disn","p18_2000")], by = "disn", all.x = TRUE, all.y = FALSE)
colnames(v00d79)[grep("p18_", colnames(v00d79))] <- "p18" 
colnames(v00d)  [grep("p18_", colnames(v00d))]   <- "p18"
colnames(v00d06)[grep("p18_", colnames(v00d06))] <- "p18"
colnames(v00d18)[grep("p18_", colnames(v00d18))] <- "p18"

v03d79 <- merge(x = v03d79, y = censod79[, c("disn","p18_2003")], by = "disn", all.x = TRUE, all.y = FALSE)
v03d   <- merge(x = v03d  , y = censod97[, c("disn","p18_2003")], by = "disn", all.x = TRUE, all.y = FALSE)
v03d06 <- merge(x = v03d06, y = censod06[, c("disn","p18_2003")], by = "disn", all.x = TRUE, all.y = FALSE)
v03d18 <- merge(x = v03d18, y = censod18[, c("disn","p18_2003")], by = "disn", all.x = TRUE, all.y = FALSE)
colnames(v03d79)[grep("p18_", colnames(v03d79))] <- "p18" 
colnames(v03d)  [grep("p18_", colnames(v03d))]   <- "p18"
colnames(v03d06)[grep("p18_", colnames(v03d06))] <- "p18"
colnames(v03d18)[grep("p18_", colnames(v03d18))] <- "p18"

v06d79 <- merge(x = v06d79, y = censod79[, c("disn","p18_2006")], by = "disn", all.x = TRUE, all.y = FALSE)
v06d97 <- merge(x = v06d97, y = censod97[, c("disn","p18_2006")], by = "disn", all.x = TRUE, all.y = FALSE)
v06d   <- merge(x = v06d  , y = censod06[, c("disn","p18_2006")], by = "disn", all.x = TRUE, all.y = FALSE)
v06d18 <- merge(x = v06d18, y = censod18[, c("disn","p18_2006")], by = "disn", all.x = TRUE, all.y = FALSE)
colnames(v06d79)[grep("p18_", colnames(v06d79))] <- "p18" 
colnames(v06d97)[grep("p18_", colnames(v06d97))] <- "p18"
colnames(v06d)  [grep("p18_", colnames(v06d))]   <- "p18"
colnames(v06d18)[grep("p18_", colnames(v06d18))] <- "p18"

v09d79 <- merge(x = v09d79, y = censod79[, c("disn","p18_2009")], by = "disn", all.x = TRUE, all.y = FALSE)
v09d97 <- merge(x = v09d97, y = censod97[, c("disn","p18_2009")], by = "disn", all.x = TRUE, all.y = FALSE)
v09d   <- merge(x = v09d  , y = censod06[, c("disn","p18_2009")], by = "disn", all.x = TRUE, all.y = FALSE)
v09d18 <- merge(x = v09d18, y = censod18[, c("disn","p18_2009")], by = "disn", all.x = TRUE, all.y = FALSE)
colnames(v09d79)[grep("p18_", colnames(v09d79))] <- "p18" 
colnames(v09d97)[grep("p18_", colnames(v09d97))] <- "p18"
colnames(v09d)  [grep("p18_", colnames(v09d))]   <- "p18"
colnames(v09d18)[grep("p18_", colnames(v09d18))] <- "p18"

v12d79 <- merge(x = v12d79, y = censod79[, c("disn","p18_2012")], by = "disn", all.x = TRUE, all.y = FALSE)
v12d97 <- merge(x = v12d97, y = censod97[, c("disn","p18_2012")], by = "disn", all.x = TRUE, all.y = FALSE)
v12d   <- merge(x = v12d  , y = censod06[, c("disn","p18_2012")], by = "disn", all.x = TRUE, all.y = FALSE)
v12d18 <- merge(x = v12d18, y = censod18[, c("disn","p18_2012")], by = "disn", all.x = TRUE, all.y = FALSE)
colnames(v12d79)[grep("p18_", colnames(v12d79))] <- "p18" 
colnames(v12d97)[grep("p18_", colnames(v12d97))] <- "p18"
colnames(v12d)  [grep("p18_", colnames(v12d))]   <- "p18"
colnames(v12d18)[grep("p18_", colnames(v12d18))] <- "p18"

v15d79 <- merge(x = v15d79, y = censod79[, c("disn","p18_2015")], by = "disn", all.x = TRUE, all.y = FALSE)
v15d97 <- merge(x = v15d97, y = censod97[, c("disn","p18_2015")], by = "disn", all.x = TRUE, all.y = FALSE)
v15d   <- merge(x = v15d  , y = censod06[, c("disn","p18_2015")], by = "disn", all.x = TRUE, all.y = FALSE)
v15d18 <- merge(x = v15d18, y = censod18[, c("disn","p18_2015")], by = "disn", all.x = TRUE, all.y = FALSE)
colnames(v15d79)[grep("p18_", colnames(v15d79))] <- "p18" 
colnames(v15d97)[grep("p18_", colnames(v15d97))] <- "p18"
colnames(v15d)  [grep("p18_", colnames(v15d))]   <- "p18"
colnames(v15d18)[grep("p18_", colnames(v15d18))] <- "p18"

v18d79 <- merge(x = v18d79, y = censod79[, c("disn","p18_2018")], by = "disn", all.x = TRUE, all.y = FALSE)
v18d97 <- merge(x = v18d97, y = censod97[, c("disn","p18_2018")], by = "disn", all.x = TRUE, all.y = FALSE)
v18d06 <- merge(x = v18d06, y = censod06[, c("disn","p18_2018")], by = "disn", all.x = TRUE, all.y = FALSE)
v18d   <- merge(x = v18d  , y = censod18[, c("disn","p18_2018")], by = "disn", all.x = TRUE, all.y = FALSE)
colnames(v18d79)[grep("p18_", colnames(v18d79))] <- "p18" 
colnames(v18d97)[grep("p18_", colnames(v18d97))] <- "p18"
colnames(v18d06)[grep("p18_", colnames(v18d06))] <- "p18"
colnames(v18d)  [grep("p18_", colnames(v18d))]   <- "p18"

v21d79 <- merge(x = v21d79, y = censod79[, c("disn","p18_2021")], by = "disn", all.x = TRUE, all.y = FALSE)
v21d97 <- merge(x = v21d97, y = censod97[, c("disn","p18_2021")], by = "disn", all.x = TRUE, all.y = FALSE)
v21d06 <- merge(x = v21d06, y = censod06[, c("disn","p18_2021")], by = "disn", all.x = TRUE, all.y = FALSE)
v21d   <- merge(x = v21d  , y = censod18[, c("disn","p18_2021")], by = "disn", all.x = TRUE, all.y = FALSE)
colnames(v21d79)[grep("p18_", colnames(v21d79))] <- "p18" 
colnames(v21d97)[grep("p18_", colnames(v21d97))] <- "p18"
colnames(v21d06)[grep("p18_", colnames(v21d06))] <- "p18"
colnames(v21d)  [grep("p18_", colnames(v21d))]   <- "p18"




##############################################
## ADD PROJECTIONS TO MUNICIPAL CENSUS MAPS ##
##############################################

## Function to generate one year's municipal projections.
projyr <- function(what="p18", yr=NA, census.data = NA){
    ##what <- "p18"; yr <- 1991; census.data <- censom94 # debug
    proj <- censom00[, c("edon","inegi","ife")]                                  # generic df with all municipios
    proj <- within(proj,{
        ##pob18s <- rqmrs <- NA                                                  # slots for linear regression predictions/stats
        pob18e <- rqmre <- NA                                                    # slots for log-linear regression predictions/stats
        pob18s <- interpol(what=what, yr=yr, unit="m", census.data=census.data)  # segments predictions
        yr <- yr;
    })
    tmp <- interlog(what=what, yr=yr, unit="m", census.data=census.data, frm="log(dv)~iv")             # log models
    proj$pob18e <- tmp[[1]]                                                                            # log predictions
    proj$rqmre <- unlist(lapply(tmp[[2]], function(x) round(mean(sqrt(x$sq.resid)) / mean(x$dv) , 3))) # quadratic mean resid / mean pop
    ##
    ## tmp <- interlog(what=what, yr=yr, unit="m", census.data=census.data, frm="dv~iv")                  # linear models
    ## proj$pob18s <- tmp[[1]]                                                                            # linear predictions
    ## proj$rqmrs <- unlist(lapply(tmp[[2]], function(x) round(mean(sqrt(x$sq.resid)) / mean(x$dv) , 3))) # quadratic mean resid / mean pop
    ##
    return(proj) # returns a data frame with ids yr p18 and relative quad mean of residuals to gauge model fit (done for census years)
}

## wrapper
myf <- function(what=what, yr=yr, census.data=census.data){ # function to paste cols
    ##what <- "p18"; yr <- 1991; census.data <- censom94 # debug
    tmp <- projyr(what=what, yr=yr, census.data = census.data)  # yr's projection
    if (yr>=1990 & yr<=2020){ 
        tmp2 <- tmp$pob18s           # btw censuses use segments
    } else {
        tmp2 <- tmp$pob18e           # else log-linear
    } 
    return(tmp2) # returns vector with yr's projection
    ## tmp3 <- census.data
    ## tmp3[, paste0("p18_", yr)] <- tmp2 # add appropriately named column
    ## ##assign(paste0("censom", as.integer((yr/100 - as.integer(yr/100))*100), "$p18_", yr)) <- tmp2
    ## tmp3[1,]
    ## return(tmp3) # returns manipulated data frame w id yr proj res.stat
}

## 1994 municipal map
tmp2 <- censom94 # duplicate for manipulation
tmp2$p18_1988 <- myf(what="p18", yr=1988, census.data = censom94)
tmp2$p18_1991 <- myf(what="p18", yr=1991, census.data = censom94)
tmp2$p18_1994 <- myf(what="p18", yr=1994, census.data = censom94)
tmp2$p18_1997 <- myf(what="p18", yr=1997, census.data = censom94)
tmp2$p18_2000 <- myf(what="p18", yr=2000, census.data = censom94)
tmp2$p18_2003 <- myf(what="p18", yr=2003, census.data = censom94)
tmp2$p18_2006 <- myf(what="p18", yr=2006, census.data = censom94)
tmp2$p18_2009 <- myf(what="p18", yr=2009, census.data = censom94)
tmp2$p18_2012 <- myf(what="p18", yr=2012, census.data = censom94)
tmp2$p18_2015 <- myf(what="p18", yr=2015, census.data = censom94)
tmp2$p18_2018 <- myf(what="p18", yr=2018, census.data = censom94)
tmp2$p18_2021 <- myf(what="p18", yr=2021, census.data = censom94)
# sort cols keeping only elec yrs
tmp2 <- tmp2[, order(colnames(tmp2))]
censom94 <- tmp2 # replace original object w manipulation

## 1997 municipal map
tmp2 <- censom97 # duplicate for manipulation
tmp2$p18_1988 <- myf(what="p18", yr=1988, census.data = censom97)
tmp2$p18_1991 <- myf(what="p18", yr=1991, census.data = censom97)
tmp2$p18_1994 <- myf(what="p18", yr=1994, census.data = censom97)
tmp2$p18_1997 <- myf(what="p18", yr=1997, census.data = censom97)
tmp2$p18_2000 <- myf(what="p18", yr=2000, census.data = censom97)
tmp2$p18_2003 <- myf(what="p18", yr=2003, census.data = censom97)
tmp2$p18_2006 <- myf(what="p18", yr=2006, census.data = censom97)
tmp2$p18_2009 <- myf(what="p18", yr=2009, census.data = censom97)
tmp2$p18_2012 <- myf(what="p18", yr=2012, census.data = censom97)
tmp2$p18_2015 <- myf(what="p18", yr=2015, census.data = censom97)
tmp2$p18_2018 <- myf(what="p18", yr=2018, census.data = censom97)
tmp2$p18_2021 <- myf(what="p18", yr=2021, census.data = censom97)
# sort cols keeping only elec yrs
tmp2 <- tmp2[, order(colnames(tmp2))]
censom97 <- tmp2 # replace original object w manipulation

## 2000 municipal map
tmp2 <- censom00 # duplicate for manipulation
tmp2$p18_1988 <- myf(what="p18", yr=1988, census.data = censom00)
tmp2$p18_1991 <- myf(what="p18", yr=1991, census.data = censom00)
tmp2$p18_1994 <- myf(what="p18", yr=1994, census.data = censom00)
tmp2$p18_1997 <- myf(what="p18", yr=1997, census.data = censom00)
tmp2$p18_2000 <- myf(what="p18", yr=2000, census.data = censom00)
tmp2$p18_2003 <- myf(what="p18", yr=2003, census.data = censom00)
tmp2$p18_2006 <- myf(what="p18", yr=2006, census.data = censom00)
tmp2$p18_2009 <- myf(what="p18", yr=2009, census.data = censom00)
tmp2$p18_2012 <- myf(what="p18", yr=2012, census.data = censom00)
tmp2$p18_2015 <- myf(what="p18", yr=2015, census.data = censom00)
tmp2$p18_2018 <- myf(what="p18", yr=2018, census.data = censom00)
tmp2$p18_2021 <- myf(what="p18", yr=2021, census.data = censom00)
# sort cols keeping only elec yrs
tmp2 <- tmp2[, order(colnames(tmp2))]
censom00 <- tmp2 # replace original object w manipulation

## 2003 municipal map
tmp2 <- censom03 # duplicate for manipulation
tmp2$p18_1988 <- myf(what="p18", yr=1988, census.data = censom03)
tmp2$p18_1991 <- myf(what="p18", yr=1991, census.data = censom03)
tmp2$p18_1994 <- myf(what="p18", yr=1994, census.data = censom03)
tmp2$p18_1997 <- myf(what="p18", yr=1997, census.data = censom03)
tmp2$p18_2000 <- myf(what="p18", yr=2000, census.data = censom03)
tmp2$p18_2003 <- myf(what="p18", yr=2003, census.data = censom03)
tmp2$p18_2006 <- myf(what="p18", yr=2006, census.data = censom03)
tmp2$p18_2009 <- myf(what="p18", yr=2009, census.data = censom03)
tmp2$p18_2012 <- myf(what="p18", yr=2012, census.data = censom03)
tmp2$p18_2015 <- myf(what="p18", yr=2015, census.data = censom03)
tmp2$p18_2018 <- myf(what="p18", yr=2018, census.data = censom03)
tmp2$p18_2021 <- myf(what="p18", yr=2021, census.data = censom03)
# sort cols keeping only elec yrs
tmp2 <- tmp2[, order(colnames(tmp2))]
censom03 <- tmp2 # replace original object w manipulation

## 2006 municipal map
tmp2 <- censom06 # duplicate for manipulation
tmp2$p18_1988 <- myf(what="p18", yr=1988, census.data = censom06)
tmp2$p18_1991 <- myf(what="p18", yr=1991, census.data = censom06)
tmp2$p18_1994 <- myf(what="p18", yr=1994, census.data = censom06)
tmp2$p18_1997 <- myf(what="p18", yr=1997, census.data = censom06)
tmp2$p18_2000 <- myf(what="p18", yr=2000, census.data = censom06)
tmp2$p18_2003 <- myf(what="p18", yr=2003, census.data = censom06)
tmp2$p18_2006 <- myf(what="p18", yr=2006, census.data = censom06)
tmp2$p18_2009 <- myf(what="p18", yr=2009, census.data = censom06)
tmp2$p18_2012 <- myf(what="p18", yr=2012, census.data = censom06)
tmp2$p18_2015 <- myf(what="p18", yr=2015, census.data = censom06)
tmp2$p18_2018 <- myf(what="p18", yr=2018, census.data = censom06)
tmp2$p18_2021 <- myf(what="p18", yr=2021, census.data = censom06)
# sort cols keeping only elec yrs
tmp2 <- tmp2[, order(colnames(tmp2))]
censom06 <- tmp2 # replace original object w manipulation

## 2009 municipal map
tmp2 <- censom09 # duplicate for manipulation
tmp2$p18_1988 <- myf(what="p18", yr=1988, census.data = censom09)
tmp2$p18_1991 <- myf(what="p18", yr=1991, census.data = censom09)
tmp2$p18_1994 <- myf(what="p18", yr=1994, census.data = censom09)
tmp2$p18_1997 <- myf(what="p18", yr=1997, census.data = censom09)
tmp2$p18_2000 <- myf(what="p18", yr=2000, census.data = censom09)
tmp2$p18_2003 <- myf(what="p18", yr=2003, census.data = censom09)
tmp2$p18_2006 <- myf(what="p18", yr=2006, census.data = censom09)
tmp2$p18_2009 <- myf(what="p18", yr=2009, census.data = censom09)
tmp2$p18_2012 <- myf(what="p18", yr=2012, census.data = censom09)
tmp2$p18_2015 <- myf(what="p18", yr=2015, census.data = censom09)
tmp2$p18_2018 <- myf(what="p18", yr=2018, census.data = censom09)
tmp2$p18_2021 <- myf(what="p18", yr=2021, census.data = censom09)
# sort cols keeping only elec yrs
tmp2 <- tmp2[, order(colnames(tmp2))]
censom09 <- tmp2 # replace original object w manipulation

## 2012 municipal map
tmp2 <- censom12 # duplicate for manipulation
tmp2$p18_1988 <- myf(what="p18", yr=1988, census.data = censom12)
tmp2$p18_1991 <- myf(what="p18", yr=1991, census.data = censom12)
tmp2$p18_1994 <- myf(what="p18", yr=1994, census.data = censom12)
tmp2$p18_1997 <- myf(what="p18", yr=1997, census.data = censom12)
tmp2$p18_2000 <- myf(what="p18", yr=2000, census.data = censom12)
tmp2$p18_2003 <- myf(what="p18", yr=2003, census.data = censom12)
tmp2$p18_2006 <- myf(what="p18", yr=2006, census.data = censom12)
tmp2$p18_2009 <- myf(what="p18", yr=2009, census.data = censom12)
tmp2$p18_2012 <- myf(what="p18", yr=2012, census.data = censom12)
tmp2$p18_2015 <- myf(what="p18", yr=2015, census.data = censom12)
tmp2$p18_2018 <- myf(what="p18", yr=2018, census.data = censom12)
tmp2$p18_2021 <- myf(what="p18", yr=2021, census.data = censom12)
# sort cols keeping only elec yrs
tmp2 <- tmp2[, order(colnames(tmp2))]
censom12 <- tmp2 # replace original object w manipulation

## 2015 municipal map
tmp2 <- censom15 # duplicate for manipulation
tmp2$p18_1988 <- myf(what="p18", yr=1988, census.data = censom15)
tmp2$p18_1991 <- myf(what="p18", yr=1991, census.data = censom15)
tmp2$p18_1994 <- myf(what="p18", yr=1994, census.data = censom15)
tmp2$p18_1997 <- myf(what="p18", yr=1997, census.data = censom15)
tmp2$p18_2000 <- myf(what="p18", yr=2000, census.data = censom15)
tmp2$p18_2003 <- myf(what="p18", yr=2003, census.data = censom15)
tmp2$p18_2006 <- myf(what="p18", yr=2006, census.data = censom15)
tmp2$p18_2009 <- myf(what="p18", yr=2009, census.data = censom15)
tmp2$p18_2012 <- myf(what="p18", yr=2012, census.data = censom15)
tmp2$p18_2015 <- myf(what="p18", yr=2015, census.data = censom15)
tmp2$p18_2018 <- myf(what="p18", yr=2018, census.data = censom15)
tmp2$p18_2021 <- myf(what="p18", yr=2021, census.data = censom15)
# sort cols keeping only elec yrs
tmp2 <- tmp2[, order(colnames(tmp2))]
censom15 <- tmp2 # replace original object w manipulation

## 2018 municipal map
tmp2 <- censom18 # duplicate for manipulation
tmp2$p18_1988 <- myf(what="p18", yr=1988, census.data = censom18)
tmp2$p18_1991 <- myf(what="p18", yr=1991, census.data = censom18)
tmp2$p18_1994 <- myf(what="p18", yr=1994, census.data = censom18)
tmp2$p18_1997 <- myf(what="p18", yr=1997, census.data = censom18)
tmp2$p18_2000 <- myf(what="p18", yr=2000, census.data = censom18)
tmp2$p18_2003 <- myf(what="p18", yr=2003, census.data = censom18)
tmp2$p18_2006 <- myf(what="p18", yr=2006, census.data = censom18)
tmp2$p18_2009 <- myf(what="p18", yr=2009, census.data = censom18)
tmp2$p18_2012 <- myf(what="p18", yr=2012, census.data = censom18)
tmp2$p18_2015 <- myf(what="p18", yr=2015, census.data = censom18)
tmp2$p18_2018 <- myf(what="p18", yr=2018, census.data = censom18)
tmp2$p18_2021 <- myf(what="p18", yr=2021, census.data = censom18)
# sort cols keeping only elec yrs
tmp2 <- tmp2[, order(colnames(tmp2))]
censom18 <- tmp2 # replace original object w manipulation

## 2021 municipal map
tmp2 <- censom21 # duplicate for manipulation
tmp2$p18_1988 <- myf(what="p18", yr=1988, census.data = censom21)
tmp2$p18_1991 <- myf(what="p18", yr=1991, census.data = censom21)
tmp2$p18_1994 <- myf(what="p18", yr=1994, census.data = censom21)
tmp2$p18_1997 <- myf(what="p18", yr=1997, census.data = censom21)
tmp2$p18_2000 <- myf(what="p18", yr=2000, census.data = censom21)
tmp2$p18_2003 <- myf(what="p18", yr=2003, census.data = censom21)
tmp2$p18_2006 <- myf(what="p18", yr=2006, census.data = censom21)
tmp2$p18_2009 <- myf(what="p18", yr=2009, census.data = censom21)
tmp2$p18_2012 <- myf(what="p18", yr=2012, census.data = censom21)
tmp2$p18_2015 <- myf(what="p18", yr=2015, census.data = censom21)
tmp2$p18_2018 <- myf(what="p18", yr=2018, census.data = censom21)
tmp2$p18_2021 <- myf(what="p18", yr=2021, census.data = censom21)
# sort cols keeping only elec yrs
tmp2 <- tmp2[, order(colnames(tmp2))]
censom21 <- tmp2 # replace original object w manipulation

rm(tmp2)

###################################
## NOW ADD P18 TO v..m.. objects ##
###################################
v91m   <- merge(x = v91m,   y = censom94[, c("ife","p18_1991")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v91m)  [grep("p18_", colnames(v91m))]   <- "p18" # absent m91 map, use m94

v94m   <- merge(x = v94m  , y = censom94[, c("ife","p18_1994")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v94m)  [grep("p18_", colnames(v94m))]   <- "p18"
v94m97 <- merge(x = v94m97, y = censom97[, c("ife","p18_1994")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v94m97)[grep("p18_", colnames(v94m97))] <- "p18"
v94m00 <- merge(x = v94m00, y = censom00[, c("ife","p18_1994")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v94m00)[grep("p18_", colnames(v94m00))] <- "p18"
v94m03 <- merge(x = v94m03, y = censom03[, c("ife","p18_1994")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v94m03)[grep("p18_", colnames(v94m03))] <- "p18"
v94m06 <- merge(x = v94m06, y = censom06[, c("ife","p18_1994")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v94m06)[grep("p18_", colnames(v94m06))] <- "p18"
v94m09 <- merge(x = v94m09, y = censom09[, c("ife","p18_1994")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v94m09)[grep("p18_", colnames(v94m09))] <- "p18"
v94m12 <- merge(x = v94m12, y = censom12[, c("ife","p18_1994")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v94m12)[grep("p18_", colnames(v94m12))] <- "p18"
v94m15 <- merge(x = v94m15, y = censom15[, c("ife","p18_1994")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v94m15)[grep("p18_", colnames(v94m15))] <- "p18"
v94m18 <- merge(x = v94m18, y = censom18[, c("ife","p18_1994")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v94m18)[grep("p18_", colnames(v94m18))] <- "p18"
v94m21 <- merge(x = v94m21, y = censom21[, c("ife","p18_1994")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v94m21)[grep("p18_", colnames(v94m21))] <- "p18"
                                                              
v97m94 <- merge(x = v97m94, y = censom94[, c("ife","p18_1997")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v97m94)[grep("p18_", colnames(v97m94))] <- "p18"
v97m   <- merge(x = v97m,   y = censom97[, c("ife","p18_1997")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v97m)  [grep("p18_", colnames(v97m))]   <- "p18"
v97m00 <- merge(x = v97m00, y = censom00[, c("ife","p18_1997")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v97m00)[grep("p18_", colnames(v97m00))] <- "p18"
v97m03 <- merge(x = v97m03, y = censom03[, c("ife","p18_1997")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v97m03)[grep("p18_", colnames(v97m03))] <- "p18"
v97m06 <- merge(x = v97m06, y = censom06[, c("ife","p18_1997")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v97m06)[grep("p18_", colnames(v97m06))] <- "p18"
v97m09 <- merge(x = v97m09, y = censom09[, c("ife","p18_1997")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v97m09)[grep("p18_", colnames(v97m09))] <- "p18"
v97m12 <- merge(x = v97m12, y = censom12[, c("ife","p18_1997")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v97m12)[grep("p18_", colnames(v97m12))] <- "p18"
v97m15 <- merge(x = v97m15, y = censom15[, c("ife","p18_1997")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v97m15)[grep("p18_", colnames(v97m15))] <- "p18"
v97m18 <- merge(x = v97m18, y = censom18[, c("ife","p18_1997")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v97m18)[grep("p18_", colnames(v97m18))] <- "p18"
v97m21 <- merge(x = v97m21, y = censom21[, c("ife","p18_1997")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v97m21)[grep("p18_", colnames(v97m21))] <- "p18"
                                                              
v00m94 <- merge(x = v00m94, y = censom94[, c("ife","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v00m94)[grep("p18_", colnames(v00m94))] <- "p18"
v00m97 <- merge(x = v00m97, y = censom97[, c("ife","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v00m97)[grep("p18_", colnames(v00m97))] <- "p18"
v00m   <- merge(x = v00m,   y = censom00[, c("ife","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v00m)  [grep("p18_", colnames(v00m))]   <- "p18"
v00m03 <- merge(x = v00m03, y = censom03[, c("ife","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v00m03)[grep("p18_", colnames(v00m03))] <- "p18"
v00m06 <- merge(x = v00m06, y = censom06[, c("ife","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v00m06)[grep("p18_", colnames(v00m06))] <- "p18"
v00m09 <- merge(x = v00m09, y = censom09[, c("ife","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v00m09)[grep("p18_", colnames(v00m09))] <- "p18"
v00m12 <- merge(x = v00m12, y = censom12[, c("ife","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v00m12)[grep("p18_", colnames(v00m12))] <- "p18"
v00m15 <- merge(x = v00m15, y = censom15[, c("ife","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v00m15)[grep("p18_", colnames(v00m15))] <- "p18"
v00m18 <- merge(x = v00m18, y = censom18[, c("ife","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v00m18)[grep("p18_", colnames(v00m18))] <- "p18"
v00m21 <- merge(x = v00m21, y = censom21[, c("ife","p18_2000")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v00m21)[grep("p18_", colnames(v00m21))] <- "p18"
                                                              
v03m94 <- merge(x = v03m94, y = censom94[, c("ife","p18_2003")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v03m94)[grep("p18_", colnames(v03m94))] <- "p18"
v03m97 <- merge(x = v03m97, y = censom97[, c("ife","p18_2003")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v03m97)[grep("p18_", colnames(v03m97))] <- "p18"
v03m00 <- merge(x = v03m00, y = censom00[, c("ife","p18_2003")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v03m00)[grep("p18_", colnames(v03m00))] <- "p18"
v03m   <- merge(x = v03m,   y = censom03[, c("ife","p18_2003")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v03m)  [grep("p18_", colnames(v03m))]   <- "p18"
v03m06 <- merge(x = v03m06, y = censom06[, c("ife","p18_2003")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v03m06)[grep("p18_", colnames(v03m06))] <- "p18"
v03m09 <- merge(x = v03m09, y = censom09[, c("ife","p18_2003")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v03m09)[grep("p18_", colnames(v03m09))] <- "p18"
v03m12 <- merge(x = v03m12, y = censom12[, c("ife","p18_2003")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v03m12)[grep("p18_", colnames(v03m12))] <- "p18"
v03m15 <- merge(x = v03m15, y = censom15[, c("ife","p18_2003")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v03m15)[grep("p18_", colnames(v03m15))] <- "p18"
v03m18 <- merge(x = v03m18, y = censom18[, c("ife","p18_2003")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v03m18)[grep("p18_", colnames(v03m18))] <- "p18"
v03m21 <- merge(x = v03m21, y = censom21[, c("ife","p18_2003")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v03m21)[grep("p18_", colnames(v03m21))] <- "p18"
                                                              
v06m94 <- merge(x = v06m94, y = censom94[, c("ife","p18_2006")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v06m94)[grep("p18_", colnames(v06m94))] <- "p18"
v06m97 <- merge(x = v06m97, y = censom97[, c("ife","p18_2006")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v06m97)[grep("p18_", colnames(v06m97))] <- "p18"
v06m00 <- merge(x = v06m00, y = censom00[, c("ife","p18_2006")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v06m00)[grep("p18_", colnames(v06m00))] <- "p18"
v06m03 <- merge(x = v06m03, y = censom03[, c("ife","p18_2006")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v06m03)[grep("p18_", colnames(v06m03))] <- "p18"
v06m   <- merge(x = v06m,   y = censom06[, c("ife","p18_2006")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v06m)  [grep("p18_", colnames(v06m))]   <- "p18"
v06m09 <- merge(x = v06m09, y = censom09[, c("ife","p18_2006")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v06m09)[grep("p18_", colnames(v06m09))] <- "p18"
v06m12 <- merge(x = v06m12, y = censom12[, c("ife","p18_2006")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v06m12)[grep("p18_", colnames(v06m12))] <- "p18"
v06m15 <- merge(x = v06m15, y = censom15[, c("ife","p18_2006")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v06m15)[grep("p18_", colnames(v06m15))] <- "p18"
v06m18 <- merge(x = v06m18, y = censom18[, c("ife","p18_2006")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v06m18)[grep("p18_", colnames(v06m18))] <- "p18"
v06m21 <- merge(x = v06m21, y = censom21[, c("ife","p18_2006")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v06m21)[grep("p18_", colnames(v06m21))] <- "p18"
                                                              
v09m94 <- merge(x = v09m94, y = censom94[, c("ife","p18_2009")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v09m94)[grep("p18_", colnames(v09m94))] <- "p18"
v09m97 <- merge(x = v09m97, y = censom97[, c("ife","p18_2009")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v09m97)[grep("p18_", colnames(v09m97))] <- "p18"
v09m00 <- merge(x = v09m00, y = censom00[, c("ife","p18_2009")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v09m00)[grep("p18_", colnames(v09m00))] <- "p18"
v09m03 <- merge(x = v09m03, y = censom03[, c("ife","p18_2009")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v09m03)[grep("p18_", colnames(v09m03))] <- "p18"
v09m06 <- merge(x = v09m06, y = censom06[, c("ife","p18_2009")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v09m06)[grep("p18_", colnames(v09m06))] <- "p18"
v09m   <- merge(x = v09m,   y = censom09[, c("ife","p18_2009")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v09m)  [grep("p18_", colnames(v09m))]   <- "p18"
v09m12 <- merge(x = v09m12, y = censom12[, c("ife","p18_2009")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v09m12)[grep("p18_", colnames(v09m12))] <- "p18"
v09m15 <- merge(x = v09m15, y = censom15[, c("ife","p18_2009")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v09m15)[grep("p18_", colnames(v09m15))] <- "p18"
v09m18 <- merge(x = v09m18, y = censom18[, c("ife","p18_2009")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v09m18)[grep("p18_", colnames(v09m18))] <- "p18"
v09m21 <- merge(x = v09m21, y = censom21[, c("ife","p18_2009")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v09m21)[grep("p18_", colnames(v09m21))] <- "p18"
                                                              
v12m94 <- merge(x = v12m94, y = censom94[, c("ife","p18_2012")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v12m94)[grep("p18_", colnames(v12m94))] <- "p18"
v12m97 <- merge(x = v12m97, y = censom97[, c("ife","p18_2012")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v12m97)[grep("p18_", colnames(v12m97))] <- "p18"
v12m00 <- merge(x = v12m00, y = censom00[, c("ife","p18_2012")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v12m00)[grep("p18_", colnames(v12m00))] <- "p18"
v12m03 <- merge(x = v12m03, y = censom03[, c("ife","p18_2012")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v12m03)[grep("p18_", colnames(v12m03))] <- "p18"
v12m06 <- merge(x = v12m06, y = censom06[, c("ife","p18_2012")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v12m06)[grep("p18_", colnames(v12m06))] <- "p18"
v12m09 <- merge(x = v12m09, y = censom09[, c("ife","p18_2012")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v12m09)[grep("p18_", colnames(v12m09))] <- "p18"
v12m   <- merge(x = v12m,   y = censom12[, c("ife","p18_2012")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v12m)  [grep("p18_", colnames(v12m))]   <- "p18"
v12m15 <- merge(x = v12m15, y = censom15[, c("ife","p18_2012")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v12m15)[grep("p18_", colnames(v12m15))] <- "p18"
v12m18 <- merge(x = v12m18, y = censom18[, c("ife","p18_2012")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v12m18)[grep("p18_", colnames(v12m18))] <- "p18"
v12m21 <- merge(x = v12m21, y = censom21[, c("ife","p18_2012")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v12m21)[grep("p18_", colnames(v12m21))] <- "p18"
                                                              
v15m94 <- merge(x = v15m94, y = censom94[, c("ife","p18_2015")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v15m94)[grep("p18_", colnames(v15m94))] <- "p18"
v15m97 <- merge(x = v15m97, y = censom97[, c("ife","p18_2015")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v15m97)[grep("p18_", colnames(v15m97))] <- "p18"
v15m00 <- merge(x = v15m00, y = censom00[, c("ife","p18_2015")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v15m00)[grep("p18_", colnames(v15m00))] <- "p18"
v15m03 <- merge(x = v15m03, y = censom03[, c("ife","p18_2015")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v15m03)[grep("p18_", colnames(v15m03))] <- "p18"
v15m06 <- merge(x = v15m06, y = censom06[, c("ife","p18_2015")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v15m06)[grep("p18_", colnames(v15m06))] <- "p18"
v15m09 <- merge(x = v15m09, y = censom09[, c("ife","p18_2015")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v15m09)[grep("p18_", colnames(v15m09))] <- "p18"
v15m12 <- merge(x = v15m12, y = censom12[, c("ife","p18_2015")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v15m12)[grep("p18_", colnames(v15m12))] <- "p18"
v15m   <- merge(x = v15m,   y = censom15[, c("ife","p18_2015")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v15m)  [grep("p18_", colnames(v15m))]   <- "p18"
v15m18 <- merge(x = v15m18, y = censom18[, c("ife","p18_2015")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v15m18)[grep("p18_", colnames(v15m18))] <- "p18"
v15m21 <- merge(x = v15m21, y = censom21[, c("ife","p18_2015")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v15m21)[grep("p18_", colnames(v15m21))] <- "p18"
                                                              
v18m94 <- merge(x = v18m94, y = censom94[, c("ife","p18_2018")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v18m94)[grep("p18_", colnames(v18m94))] <- "p18"
v18m97 <- merge(x = v18m97, y = censom97[, c("ife","p18_2018")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v18m97)[grep("p18_", colnames(v18m97))] <- "p18"
v18m00 <- merge(x = v18m00, y = censom00[, c("ife","p18_2018")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v18m00)[grep("p18_", colnames(v18m00))] <- "p18"
v18m03 <- merge(x = v18m03, y = censom03[, c("ife","p18_2018")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v18m03)[grep("p18_", colnames(v18m03))] <- "p18"
v18m06 <- merge(x = v18m06, y = censom06[, c("ife","p18_2018")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v18m06)[grep("p18_", colnames(v18m06))] <- "p18"
v18m09 <- merge(x = v18m09, y = censom09[, c("ife","p18_2018")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v18m09)[grep("p18_", colnames(v18m09))] <- "p18"
v18m12 <- merge(x = v18m12, y = censom12[, c("ife","p18_2018")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v18m12)[grep("p18_", colnames(v18m12))] <- "p18"
v18m15 <- merge(x = v18m15, y = censom15[, c("ife","p18_2018")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v18m15)[grep("p18_", colnames(v18m15))] <- "p18"
v18m   <- merge(x = v18m,   y = censom18[, c("ife","p18_2018")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v18m)  [grep("p18_", colnames(v18m))]   <- "p18"
v18m21 <- merge(x = v18m21, y = censom21[, c("ife","p18_2018")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v18m21)[grep("p18_", colnames(v18m21))] <- "p18"
                                                              
v21m94 <- merge(x = v21m94, y = censom94[, c("ife","p18_2021")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v21m94)[grep("p18_", colnames(v21m94))] <- "p18"
v21m97 <- merge(x = v21m97, y = censom97[, c("ife","p18_2021")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v21m97)[grep("p18_", colnames(v21m97))] <- "p18"
v21m00 <- merge(x = v21m00, y = censom00[, c("ife","p18_2021")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v21m00)[grep("p18_", colnames(v21m00))] <- "p18"
v21m03 <- merge(x = v21m03, y = censom03[, c("ife","p18_2021")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v21m03)[grep("p18_", colnames(v21m03))] <- "p18"
v21m06 <- merge(x = v21m06, y = censom06[, c("ife","p18_2021")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v21m06)[grep("p18_", colnames(v21m06))] <- "p18"
v21m09 <- merge(x = v21m09, y = censom09[, c("ife","p18_2021")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v21m09)[grep("p18_", colnames(v21m09))] <- "p18"
v21m12 <- merge(x = v21m12, y = censom12[, c("ife","p18_2021")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v21m12)[grep("p18_", colnames(v21m12))] <- "p18"
v21m15 <- merge(x = v21m15, y = censom15[, c("ife","p18_2021")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v21m15)[grep("p18_", colnames(v21m15))] <- "p18"
v21m18 <- merge(x = v21m18, y = censom18[, c("ife","p18_2021")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v21m18)[grep("p18_", colnames(v21m18))] <- "p18"
v21m   <- merge(x = v21m,   y = censom21[, c("ife","p18_2021")], by = "ife", all.x = TRUE, all.y = FALSE); colnames(v21m)  [grep("p18_", colnames(v21m))]   <- "p18"



########################################
## ################################## ##
## ## Project p18 at seccion-level ## ##
## ################################## ##
########################################

## Segments projections
tmp.s <- data.frame(seccion=censo$seccion,
                    p18_1991=interpol(what="p18", yr=1991, unit="s", census.data = censo),
                    p18_1994=interpol(what="p18", yr=1994, unit="s", census.data = censo),
                    p18_1997=interpol(what="p18", yr=1997, unit="s", census.data = censo),
                    p18_2000=interpol(what="p18", yr=2000, unit="s", census.data = censo),
                    p18_2003=interpol(what="p18", yr=2003, unit="s", census.data = censo),
                    p18_2006=interpol(what="p18", yr=2006, unit="s", census.data = censo),
                    p18_2009=interpol(what="p18", yr=2009, unit="s", census.data = censo),
                    p18_2012=interpol(what="p18", yr=2012, unit="s", census.data = censo),
                    p18_2015=interpol(what="p18", yr=2015, unit="s", census.data = censo),
                    p18_2018=interpol(what="p18", yr=2018, unit="s", census.data = censo),
                    p18_2021=interpol(what="p18", yr=2011, unit="s", census.data = censo))

## Log-linear projection of 1991, retaining regressions to use for 1994-on
tmp.regs <- interlog(what="p18", yr=1991, unit="s", frm="log(dv)~iv", census.data = censo)
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

## paste p18 prediction to v..s objects
v94s <- merge(x=v94s, y=tmp.e[,c("seccion","p18_1994")], by = "seccion", all.x = TRUE, all.y = FALSE) # log-linear prediction prior to 2005
v97s <- merge(x=v97s, y=tmp.e[,c("seccion","p18_1997")], by = "seccion", all.x = TRUE, all.y = FALSE) # log-linear prediction prior to 2005
v00s <- merge(x=v00s, y=tmp.e[,c("seccion","p18_2000")], by = "seccion", all.x = TRUE, all.y = FALSE) # log-linear prediction prior to 2005
v03s <- merge(x=v03s, y=tmp.e[,c("seccion","p18_2003")], by = "seccion", all.x = TRUE, all.y = FALSE) # log-linear prediction prior to 2005
v06s <- merge(x=v06s, y=tmp.s[,c("seccion","p18_2006")], by = "seccion", all.x = TRUE, all.y = FALSE) # segments between 2005 and 2020     
v09s <- merge(x=v09s, y=tmp.s[,c("seccion","p18_2009")], by = "seccion", all.x = TRUE, all.y = FALSE) # segments between 2005 and 2020     
v12s <- merge(x=v12s, y=tmp.s[,c("seccion","p18_2012")], by = "seccion", all.x = TRUE, all.y = FALSE) # segments between 2005 and 2020     
v15s <- merge(x=v15s, y=tmp.s[,c("seccion","p18_2015")], by = "seccion", all.x = TRUE, all.y = FALSE) # segments between 2005 and 2020     
v18s <- merge(x=v18s, y=tmp.s[,c("seccion","p18_2018")], by = "seccion", all.x = TRUE, all.y = FALSE) # segments between 2005 and 2020     
v21s <- merge(x=v21s, y=tmp.e[,c("seccion","p18_2021")], by = "seccion", all.x = TRUE, all.y = FALSE) # log-linear prediction after 2020   
## rename columns                                                                                     
colnames(v94s)[grep("p18_", colnames(v94s))] <- "p18"
colnames(v97s)[grep("p18_", colnames(v97s))] <- "p18"
colnames(v00s)[grep("p18_", colnames(v00s))] <- "p18"
colnames(v03s)[grep("p18_", colnames(v03s))] <- "p18"
colnames(v06s)[grep("p18_", colnames(v06s))] <- "p18"
colnames(v09s)[grep("p18_", colnames(v09s))] <- "p18"
colnames(v12s)[grep("p18_", colnames(v12s))] <- "p18"
colnames(v15s)[grep("p18_", colnames(v15s))] <- "p18"
colnames(v18s)[grep("p18_", colnames(v18s))] <- "p18"
colnames(v21s)[grep("p18_", colnames(v21s))] <- "p18"


## ########################### 
## ## COMPARE PERFORMANCE #### 
## ########################### 
## ## 
## ## function to interpolate and generate comparative stats## ## 
## comp <- function(what="p18", yr=NA, unit="m", census.data = NA){## ## 
##     tmp2 <- censom00[, c("edon","inegi","ife")]## ## 
##     tmp2 <- within(tmp2,{## ## 
##         pob18l <- lss <- lr2 <- NA                                                         # slots for regression predictions/stats## ## 
##         pob18e <- ess <- er2 <- NA                                                         # slots for regression predictions/stats## ## 
##         pob18t <- interpol(what=what, yr=yr, unit=unit, census.data=census.data)           # segments predictions## ## 
##     })## ## 
##     tmp3 <- interlog(what=what, yr=yr, unit=unit, census.data=census.data, frm="log(dv)~iv")          # log models## ## 
##     tmp2$pob18e <- tmp3[[1]]                                                                          # log predictions## ## 
##     non.nas <- which(!is.na(tmp3[[1]]))                                                               # skip NAs## ## 
##     tmp2$er2[non.nas] <- unlist(lapply(tmp3[[2]][non.nas], function(x) x$r2[1]))                      # regression r2s## ## 
##     tmp2$ess <- unlist(lapply(tmp3[[2]], function(x) round(mean(sqrt(x$sq.resid)) / mean(x$dv) , 3))) # quadratic mean resid / mean pop## ## 
##     #### ## 
##     tmp3 <- interlog(what=what, yr=yr, unit=unit, census.data=census.data, frm="dv~iv")               # linear models## ## 
##     tmp2$pob18l <- tmp3[[1]]                                                                          # liner predictions## ## 
##     tmp2$lr2[non.nas] <- unlist(lapply(tmp3[[2]][non.nas], function(x) x$r2[1]))                      # regression r2s## ## 
##     tmp2$lss <- unlist(lapply(tmp3[[2]], function(x) round(mean(sqrt(x$sq.resid)) / mean(x$dv) , 3))) # quadratic mean resid / mean pop## ## 
##     return(tmp2)## ## 
## }## 
## ## 
## ######################################### ## 
## ## Add p18 to municipal vote objects #### ## 
## ######################################### ## 
## ## 1991## ## 
## tmp2 <- comp(what="p18", yr=1991, unit="m", census.data = censom00)## ## 
## #### ## 
## v91m$p18t   <- tmp2$pob18t # paste interpolation## ## 
## v91m$p18e   <- tmp2$pob18e # paste interpolation## ## 
## v91m$ess    <- tmp2$ess                    ## ## 
## v91m$er2    <- tmp2$er2                    ## ## 
## v91m$p18l   <- tmp2$pob18l # paste interpolation## ## 
## v91m$lss    <- tmp2$lss                    ## ## 
## v91m$lr2    <- tmp2$lr2                    ## ## 
## ## ## 1994## ## 
## ## tmp2 <- censom94[, c("edon","inegi","ife")]## ## 
## ## tmp2$pob18t <- interpol(what="p18", yr=1994, unit="m")## ## 
## ## tmp2$pob18e <- interlog(what="p18", yr=1994, unit="m", frm="log(dv)~iv")## ## 
## ## tmp2$pob18l <- interlog(what="p18", yr=1994, unit="m", frm="dv~iv")## ## 
## ## v94m$pob18t <- tmp2$pob18t # paste interpolation## ## 
## ## v94m$pob18e <- tmp2$pob18e # paste interpolation## ## 
## ## v94m$pob18l <- tmp2$pob18l # paste interpolation## ## 
## ## ## 1994m97## ## 
## ## tmp2 <- censom97[, c("edon","inegi","ife")]## ## 
## ## tmp2$pob18t <- interpol(what="p18", yr=1994, unit="m", census.data = censom97)## ## 
## ## tmp2$pob18e <- interlog(what="p18", yr=1994, unit="m", census.data = censom97, frm="log(dv)~iv")## ## 
## ## tmp2$pob18l <- interlog(what="p18", yr=1994, unit="m", census.data = censom97, frm="dv~iv")## ## 
## ## v94m97$pob18t <- tmp2$pob18t # paste interpolation## ## 
## ## v94m97$pob18e <- tmp2$pob18e # paste interpolation## ## 
## ## v94m97$pob18l <- tmp2$pob18l # paste interpolation## 
## ## 
## ## 1994m00## ## 
## tmp2 <- comp(what="p18", yr=1994, unit="m", census.data = censom00)## ## 
## #### ## 
## v94m00$p18t   <- tmp2$pob18t # paste interpolation## ## 
## v94m00$p18e   <- tmp2$pob18e # paste interpolation## ## 
## v94m00$ess    <- tmp2$ess## ## 
## v94m00$er2    <- tmp2$er2## ## 
## v94m00$p18l   <- tmp2$pob18l # paste interpolation## ## 
## v94m00$lss    <- tmp2$lss## ## 
## v94m00$lr2    <- tmp2$lr2                    ## ## 
## ## ## 1994m03## ## 
## ## ## 1994m06## ## 
## ## ## 1994m09## ## 
## ## ## 1994m12## ## 
## ## ## 1994m15## ## 
## ## ## 1994m18## ## 
## ## ## 1994m21## 
## ## 
## ## ## 1997## ## 
## ## ## 1997m94## ## 
## ## 1997m00## ## 
## tmp2 <- comp(what="p18", yr=1997, unit="m", census.data = censom00)## ## 
## #### ## 
## v97m00$p18t   <- tmp2$pob18t # paste interpolation## ## 
## v97m00$p18e   <- tmp2$pob18e # paste interpolation## ## 
## v97m00$ess    <- tmp2$ess                    ## ## 
## v97m00$er2    <- tmp2$er2                    ## ## 
## v97m00$p18l   <- tmp2$pob18l # paste interpolation## ## 
## v97m00$lss    <- tmp2$lss                    ## ## 
## v97m00$lr2    <- tmp2$lr2                    ## ## 
## ## ## 1997m03## ## 
## ## ## 1997m06## ## 
## ## ## 1997m09## ## 
## ## ## 1997m12## ## 
## ## ## 1997m15## ## 
## ## ## 1997m18## ## 
## ## ## 1997m21## 
## ## 
## ## 2000## ## 
## tmp2 <- comp(what="p18", yr=2000, unit="m", census.data = censom00)## ## 
## #### ## 
## v00m$p18t   <- tmp2$pob18t # paste interpolation## ## 
## v00m$p18e   <- tmp2$pob18e # paste interpolation## ## 
## v00m$ess    <- tmp2$ess                    ## ## 
## v00m$er2    <- tmp2$er2                    ## ## 
## v00m$p18l   <- tmp2$pob18l # paste interpolation## ## 
## v00m$lss    <- tmp2$lss                    ## ## 
## v00m$lr2    <- tmp2$lr2                    ## ## 
## ## ## 2000m94## ## 
## ## ## 2000m97## ## 
## ## ## 2000m03## ## 
## ## ## 2000m06## ## 
## ## ## 2000m09## ## 
## ## ## 2000m12## ## 
## ## ## 2000m15## ## 
## ## ## 2000m18## ## 
## ## ## 2000m21## 
## ## 
## ## ## 2003## ## 
## ## ## 2003m94## ## 
## ## ## 2003m97## ## 
## ## 2003m00## ## 
## tmp2 <- comp(what="p18", yr=2003, unit="m", census.data = censom00)## ## 
## #### ## 
## v03m00$p18t   <- tmp2$pob18t # paste interpolation## ## 
## v03m00$p18e   <- tmp2$pob18e # paste interpolation## ## 
## v03m00$ess    <- tmp2$ess                    ## ## 
## v03m00$er2    <- tmp2$er2                    ## ## 
## v03m00$p18l   <- tmp2$pob18l # paste interpolation## ## 
## v03m00$lss    <- tmp2$lss                    ## ## 
## v03m00$lr2    <- tmp2$lr2                    ## ## 
## ## ## 2003m06## ## 
## ## ## 2003m09## ## 
## ## ## 2003m12## ## 
## ## ## 2003m15## ## 
## ## ## 2003m18## ## 
## ## ## 2003m21## 
## ## 
## ## ## 2006## ## 
## ## ## 2006m94## ## 
## ## ## 2006m97## ## 
## ## 2006m00## ## 
## tmp2 <- comp(what="p18", yr=2006, unit="m", census.data = censom00)## ## 
## #### ## 
## v06m00$p18t   <- tmp2$pob18t # paste interpolation## ## 
## v06m00$p18e   <- tmp2$pob18e # paste interpolation## ## 
## v06m00$ess    <- tmp2$ess                    ## ## 
## v06m00$er2    <- tmp2$er2                    ## ## 
## v06m00$p18l   <- tmp2$pob18l # paste interpolation## ## 
## v06m00$lss    <- tmp2$lss                    ## ## 
## v06m00$lr2    <- tmp2$lr2                    ## ## 
## ## ## 2006m03## ## 
## ## ## 2006m09## ## 
## ## ## 2006m12## ## 
## ## ## 2006m15## ## 
## ## ## 2006m18## ## 
## ## ## 2006m21## 
## ## 
## ## ## 2009## ## 
## ## ## 2009m94## ## 
## ## ## 2009m97## ## 
## ## 2009m00## ## 
## tmp2 <- comp(what="p18", yr=2009, unit="m", census.data = censom00)## ## 
## #### ## 
## v09m00$p18t   <- tmp2$pob18t # paste interpolation## ## 
## v09m00$p18e   <- tmp2$pob18e # paste interpolation## ## 
## v09m00$ess    <- tmp2$ess                    ## ## 
## v09m00$er2    <- tmp2$er2                    ## ## 
## v09m00$p18l   <- tmp2$pob18l # paste interpolation## ## 
## v09m00$lss    <- tmp2$lss                    ## ## 
## v09m00$lr2    <- tmp2$lr2                    ## ## 
## ## ## 2009m03## ## 
## ## ## 2009m06## ## 
## ## ## 2009m12## ## 
## ## ## 2009m15## ## 
## ## ## 2009m18## ## 
## ## ## 2009m21## 
## ## 
## ## ## 2012## ## 
## ## ## 2012m94## ## 
## ## ## 2012m97## ## 
## ## 2012m00## ## 
## tmp2 <- comp(what="p18", yr=2012, unit="m", census.data = censom00)## ## 
## #### ## 
## v12m00$p18t   <- tmp2$pob18t # paste interpolation## ## 
## v12m00$p18e   <- tmp2$pob18e # paste interpolation## ## 
## v12m00$ess    <- tmp2$ess                    ## ## 
## v12m00$er2    <- tmp2$er2                    ## ## 
## v12m00$p18l   <- tmp2$pob18l # paste interpolation## ## 
## v12m00$lss    <- tmp2$lss                    ## ## 
## v12m00$lr2    <- tmp2$lr2                    ## ## 
## ## ## 2012m03## ## 
## ## ## 2012m06## ## 
## ## ## 2012m09## ## 
## ## ## 2012m15## ## 
## ## ## 2012m18## ## 
## ## ## 2012m21## 
## ## 
## ## ## 2015## ## 
## ## ## 2015m94## ## 
## ## ## 2015m97## ## 
## ## 2015m00## ## 
## tmp2 <- comp(what="p18", yr=2015, unit="m", census.data = censom00)## ## 
## #### ## 
## v15m00$p18t   <- tmp2$pob18t # paste interpolation## ## 
## v15m00$p18e   <- tmp2$pob18e # paste interpolation## ## 
## v15m00$ess    <- tmp2$ess                    ## ## 
## v15m00$er2    <- tmp2$er2                    ## ## 
## v15m00$p18l   <- tmp2$pob18l # paste interpolation## ## 
## v15m00$lss    <- tmp2$lss                    ## ## 
## v15m00$lr2    <- tmp2$lr2                    ## ## 
## ## ## 2015m03## ## 
## ## ## 2015m06## ## 
## ## ## 2015m09## ## 
## ## ## 2015m12## ## 
## ## ## 2015m18## ## 
## ## ## 2015m21## 
## ## 
## ## ## 2018## ## 
## ## ## 2018m94## ## 
## ## ## 2018m97## ## 
## ## 2018m00## ## 
## tmp2 <- comp(what="p18", yr=2018, unit="m", census.data = censom00)## ## 
## #### ## 
## v18m00$p18t   <- tmp2$pob18t # paste interpolation## ## 
## v18m00$p18e   <- tmp2$pob18e # paste interpolation## ## 
## v18m00$ess    <- tmp2$ess                    ## ## 
## v18m00$er2    <- tmp2$er2                    ## ## 
## v18m00$p18l   <- tmp2$pob18l # paste interpolation## ## 
## v18m00$lss    <- tmp2$lss                    ## ## 
## v18m00$lr2    <- tmp2$lr2                    ## ## 
## ## ## 2018m03## ## 
## ## ## 2018m06## ## 
## ## ## 2018m09## ## 
## ## ## 2018m12## ## 
## ## ## 2018m15## ## 
## ## ## 2018m21## 
## ## 
## ## ## 2021## ## 
## ## ## 2021m94## ## 
## ## ## 2021m97## ## 
## ## 2021m00## ## 
## tmp2 <- comp(what="p18", yr=2021, unit="m", census.data = censom00)## ## 
## #### ## 
## v21m00$p18t   <- tmp2$pob18t # paste interpolation## ## 
## v21m00$p18e   <- tmp2$pob18e # paste interpolation## ## 
## v21m00$ess    <- tmp2$ess                    ## ## 
## v21m00$er2    <- tmp2$er2                    ## ## 
## v21m00$p18l   <- tmp2$pob18l # paste interpolation## ## 
## v21m00$lss    <- tmp2$lss                    ## ## 
## v21m00$lr2    <- tmp2$lr2                    ## ## 
## ## ## 2021m03## ## 
## ## ## 2021m06## ## 
## ## ## 2021m09## ## 
## ## ## 2021m12## ## 
## ## ## 2021m15## ## 
## ## ## 2021m18## 
## ## 
## mysum <- function(x){## ## 
##     prbs <- c(0,.01,.025,.05,.25,.5,.75,.95,.975,.99,1)## ## 
##     y <- round(quantile(x, probs=prbs, na.rm=TRUE), digits=3)## ## 
##     return(y)## ## 
## }## ## 
## ## Any year has the map's statistics## ## 
## mysum(v00m$ess)## ## 
## mysum(v00m$lss)## ## 
## mysum(v00m$er2)## ## 
## mysum(v00m$lr2)## 
## ## 
## quantile(v00m$ess, probs=seq(0,1,.01), na.rm = TRUE)## ## 
## quantile(v00m$lss, probs=seq(0,1,.01), na.rm = TRUE)## 
## ## 
## ## all same sorted?## ## 
## v94m00[1,]## ## 
## censom00[1,]## ## 
## table(v94m00$ife==censom00$ife)## 
## ## 
## ## ## compare linear and log projections in census years## ## 
## ## Inspect census figs## ## 
## myplot <- function(i=NA, print=FALSE){## ## 
##     if (print==TRUE) png(paste0("/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/plots/", "mu", censom00$inegi[i], "p18-interlog.png"))## ## 
##     plot(  seq(1994,2021,3), li,## ## 
##          xlim = c(1988,2027),## ## 
##          ylim = c(0,1.3*max(li,lg,ls   )), col="blue", xlab="", ylab="voting age population", main = paste(censom00$inegi[i], censom00$mun[i], edo[censom00$edon[i]]), sub="blue=linear model, red=log-linear model, solid=census")## ## 
##     ##plot(  seq(1994,2021,3), li, ylim = c(min(li,lg,ls,ef),max(li,lg,ls,ef)), main = paste(i, "bck=line, red=log, blue=lis, grn=efec"))## ## 
##     points(seq(1994,2021,3), lg, col = "red")## ## 
##     ##points(seq(1994,2021,3), ls, col = "blue")## ## 
##     ##points(seq(1994,2021,3), ef, col = "green")## ## 
##     points(1990, c0, pch = 19, cex = .75)## ## 
##     points(1995, c1, pch = 19, cex = .75)## ## 
##     points(2000, c2, pch = 19, cex = .75)## ## 
##     points(c(2005,2010,2020), censom00[i,c("p18_2005","p18_2010","p18_2020")], pch=19, cex=.75)## ## 
##     legend("bottomright",## ## 
##            legend = c(paste0("RQMR = ", v00m$ess[i]), paste0("RQMR = ", v00m$lss[i])),## ## 
## #           legend = c(paste0("R^2 = ", v15m00$pob18er2[i]), paste0("R^2 = ", v15m00$pob18lr2[i])),## ## 
##            pch = c(1,1), col = c("red","blue"))## ## 
##     ##abline(v=2000, lty=2)## ## 
## #### ## 
## ## points(1988, interlog(what="p18", yr=1988, unit="m", census.data = censom00, frm="log(dv)~iv")[[1]][i], col = "red")## ## 
## ## points(1991, interlog(what="p18", yr=1991, unit="m", census.data = censom00, frm="log(dv)~iv")[[1]][i], col = "red")## ## 
## ## points(2024, interlog(what="p18", yr=2024, unit="m", census.data = censom00, frm="log(dv)~iv")[[1]][i], col = "red")## ## 
## ## points(2027, interlog(what="p18", yr=2027, unit="m", census.data = censom00, frm="log(dv)~iv")[[1]][i], col = "red")## ## 
## ## #### ## 
## ## points(1988, interlog(what="p18", yr=1988, unit="m", census.data = censom00, frm="dv~iv")[[1]][i], col = "blue")## ## 
## ## points(1991, interlog(what="p18", yr=1991, unit="m", census.data = censom00, frm="dv~iv")[[1]][i], col = "blue")## ## 
## ## points(2024, interlog(what="p18", yr=2024, unit="m", census.data = censom00, frm="dv~iv")[[1]][i], col = "blue")## ## 
## ## points(2027, interlog(what="p18", yr=2027, unit="m", census.data = censom00, frm="dv~iv")[[1]][i], col = "blue")## ## 
## #### ## 
##     if (print==TRUE) dev.off()## ## 
## }## 
## ## 
## edo <- c("Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua", "Distrito Federal", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "México", "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas")
## 
## ## 
## i <- 1000## ## 
## i <- i+1## ## 
## ls <- c(## ## 
##     v94m00$lisnom[i],## ## 
##     v97m00$lisnom[i],## ## 
##     v00m  $lisnom[i],## ## 
##     v03m00$lisnom[i],## ## 
##     v06m00$lisnom[i],## ## 
##     v09m00$lisnom[i],## ## 
##     v12m00$lisnom[i],## ## 
##     v15m00$lisnom[i],## ## 
##     v18m00$lisnom[i],## ## 
##     v21m00$lisnom[i]## ## 
## )## ## 
## ef <- c(## ## 
##     v94m00$efec[i],## ## 
##     v97m00$efec[i],## ## 
##     v00m  $efec[i],## ## 
##     v03m00$efec[i],## ## 
##     v06m00$efec[i],## ## 
##     v09m00$efec[i],## ## 
##     v12m00$efec[i],## ## 
##     v15m00$efec[i],## ## 
##     v18m00$efec[i],## ## 
##     v21m00$efec[i]## ## 
## )## ## 
## li <- c(## ## 
##     v94m00$p18l[i],## ## 
##     v97m00$p18l[i],## ## 
##     v00m  $p18l[i],## ## 
##     v03m00$p18l[i],## ## 
##     v06m00$p18l[i],## ## 
##     v09m00$p18l[i],## ## 
##     v12m00$p18l[i],## ## 
##     v15m00$p18l[i],## ## 
##     v18m00$p18l[i],## ## 
##     v21m00$p18l[i]## ## 
## )## ## 
## lg <- c(## ## 
##     v94m00$p18e[i],## ## 
##     v97m00$p18e[i],## ## 
##     v00m  $p18e[i],## ## 
##     v03m00$p18e[i],## ## 
##     v06m00$p18e[i],## ## 
##     v09m00$p18e[i],## ## 
##     v12m00$p18e[i],## ## 
##     v15m00$p18e[i],## ## 
##     v18m00$p18e[i],## ## 
##     v21m00$p18e[i]## ## 
## )## ## 
## tw <- c(## ## 
##     v94m00$p18t[i],## ## 
##     v97m00$p18t[i],## ## 
##     v00m  $p18t[i],## ## 
##     v03m00$p18t[i],## ## 
##     v06m00$p18t[i],## ## 
##     v09m00$p18t[i],## ## 
##     v12m00$p18t[i],## ## 
##     v15m00$p18t[i],## ## 
##     v18m00$p18t[i],## ## 
##     v21m00$p18t[i]## ## 
## )## ## 
## c0 <- as.numeric(censom00$p18_1990[i])## ## 
## c1 <- as.numeric(censom00$p18_1995[i])## ## 
## c2 <- as.numeric(censom00$p18_2000[i])## ## 
## #### ## 
## myplot(i, print=FALSE)
## 
## ## 
## ####################################################################################################################### ## 
## ## Analyze three-censuses projection in municipalities (as in secciones) while comparing to 1995 and 2000 censuses #### ## 
## ####################################################################################################################### 
## ## 
## ## duplicate censo mun to drop two censuses## ## 
## censom00.3pt <- censom00## ## 
## ## drop 1995 and 2000## ## 
## censom00.3pt$p18_1995 <- censom00.3pt$p18_2000 <- NULL## ## 
## censom00.3pt[1,]## 
## ## 
## ## re-project## ## 
## tmp2 <- comp(what="p18", yr=1991, unit="m", census.data = censom00.3pt)## ## 
## #### ## 
## v91m$p18t   <- tmp2$pob18t # paste interpolation## ## 
## v91m$p18e   <- tmp2$pob18e # paste interpolation## ## 
## v91m$ess    <- tmp2$ess                    ## ## 
## v91m$er2    <- tmp2$er2                    ## ## 
## v91m$p18l   <- tmp2$pob18l # paste interpolation## ## 
## v91m$lss    <- tmp2$lss                    ## ## 
## v91m$lr2    <- tmp2$lr2                    ## ## 
## ## ## 1994## ## 
## ## tmp2 <- censom94[, c("edon","inegi","ife")]## ## 
## ## tmp2$pob18t <- interpol(what="p18", yr=1994, unit="m")## ## 
## ## tmp2$pob18e <- interlog(what="p18", yr=1994, unit="m", frm="log(dv)~iv")## ## 
## ## tmp2$pob18l <- interlog(what="p18", yr=1994, unit="m", frm="dv~iv")## ## 
## ## v94m$pob18t <- tmp2$pob18t # paste interpolation## ## 
## ## v94m$pob18e <- tmp2$pob18e # paste interpolation## ## 
## ## v94m$pob18l <- tmp2$pob18l # paste interpolation## ## 
## ## ## 1994m97## ## 
## ## tmp2 <- censom97[, c("edon","inegi","ife")]## ## 
## ## tmp2$pob18t <- interpol(what="p18", yr=1994, unit="m", census.data = censom97)## ## 
## ## tmp2$pob18e <- interlog(what="p18", yr=1994, unit="m", census.data = censom97, frm="log(dv)~iv")## ## 
## ## tmp2$pob18l <- interlog(what="p18", yr=1994, unit="m", census.data = censom97, frm="dv~iv")## ## 
## ## v94m97$pob18t <- tmp2$pob18t # paste interpolation## ## 
## ## v94m97$pob18e <- tmp2$pob18e # paste interpolation## ## 
## ## v94m97$pob18l <- tmp2$pob18l # paste interpolation## 
## ## 
## ## 1994m00## ## 
## tmp2 <- comp(what="p18", yr=1994, unit="m", census.data = censom00.3pt)## ## 
## #### ## 
## v94m00$p18t   <- tmp2$pob18t # paste interpolation## ## 
## v94m00$p18e   <- tmp2$pob18e # paste interpolation## ## 
## v94m00$ess    <- tmp2$ess## ## 
## v94m00$er2    <- tmp2$er2## ## 
## v94m00$p18l   <- tmp2$pob18l # paste interpolation## ## 
## v94m00$lss    <- tmp2$lss## ## 
## v94m00$lr2    <- tmp2$lr2                    ## ## 
## ## ## 1994m03## ## 
## ## ## 1994m06## ## 
## ## ## 1994m09## ## 
## ## ## 1994m12## ## 
## ## ## 1994m15## ## 
## ## ## 1994m18## ## 
## ## ## 1994m21## 
## ## 
## ## ## 1997## ## 
## ## ## 1997m94## ## 
## ## 1997m00## ## 
## tmp2 <- comp(what="p18", yr=1997, unit="m", census.data = censom00.3pt)## ## 
## #### ## 
## v97m00$p18t   <- tmp2$pob18t # paste interpolation## ## 
## v97m00$p18e   <- tmp2$pob18e # paste interpolation## ## 
## v97m00$ess    <- tmp2$ess                    ## ## 
## v97m00$er2    <- tmp2$er2                    ## ## 
## v97m00$p18l   <- tmp2$pob18l # paste interpolation## ## 
## v97m00$lss    <- tmp2$lss                    ## ## 
## v97m00$lr2    <- tmp2$lr2                    ## ## 
## ## ## 1997m03## ## 
## ## ## 1997m06## ## 
## ## ## 1997m09## ## 
## ## ## 1997m12## ## 
## ## ## 1997m15## ## 
## ## ## 1997m18## ## 
## ## ## 1997m21## 
## ## 
## ## 2000## ## 
## tmp2 <- comp(what="p18", yr=2000, unit="m", census.data = censom00.3pt)## ## 
## #### ## 
## v00m$p18t   <- tmp2$pob18t # paste interpolation## ## 
## v00m$p18e   <- tmp2$pob18e # paste interpolation## ## 
## v00m$ess    <- tmp2$ess                    ## ## 
## v00m$er2    <- tmp2$er2                    ## ## 
## v00m$p18l   <- tmp2$pob18l # paste interpolation## ## 
## v00m$lss    <- tmp2$lss                    ## ## 
## v00m$lr2    <- tmp2$lr2                    ## ## 
## ## ## 2000m94## ## 
## ## ## 2000m97## ## 
## ## ## 2000m03## ## 
## ## ## 2000m06## ## 
## ## ## 2000m09## ## 
## ## ## 2000m12## ## 
## ## ## 2000m15## ## 
## ## ## 2000m18## ## 
## ## ## 2000m21## 
## ## 
## ## ## 2003## ## 
## ## ## 2003m94## ## 
## ## ## 2003m97## ## 
## ## 2003m00## ## 
## tmp2 <- comp(what="p18", yr=2003, unit="m", census.data = censom00.3pt)## ## 
## #### ## 
## v03m00$p18t   <- tmp2$pob18t # paste interpolation## ## 
## v03m00$p18e   <- tmp2$pob18e # paste interpolation## ## 
## v03m00$ess    <- tmp2$ess                    ## ## 
## v03m00$er2    <- tmp2$er2                    ## ## 
## v03m00$p18l   <- tmp2$pob18l # paste interpolation## ## 
## v03m00$lss    <- tmp2$lss                    ## ## 
## v03m00$lr2    <- tmp2$lr2                    ## ## 
## ## ## 2003m06## ## 
## ## ## 2003m09## ## 
## ## ## 2003m12## ## 
## ## ## 2003m15## ## 
## ## ## 2003m18## ## 
## ## ## 2003m21## 
## ## 
## ## ## 2006## ## 
## ## ## 2006m94## ## 
## ## ## 2006m97## ## 
## ## 2006m00## ## 
## tmp2 <- comp(what="p18", yr=2006, unit="m", census.data = censom00.3pt)## ## 
## #### ## 
## v06m00$p18t   <- tmp2$pob18t # paste interpolation## ## 
## v06m00$p18e   <- tmp2$pob18e # paste interpolation## ## 
## v06m00$ess    <- tmp2$ess                    ## ## 
## v06m00$er2    <- tmp2$er2                    ## ## 
## v06m00$p18l   <- tmp2$pob18l # paste interpolation## ## 
## v06m00$lss    <- tmp2$lss                    ## ## 
## v06m00$lr2    <- tmp2$lr2                    ## ## 
## ## ## 2006m03## ## 
## ## ## 2006m09## ## 
## ## ## 2006m12## ## 
## ## ## 2006m15## ## 
## ## ## 2006m18## ## 
## ## ## 2006m21## 
## ## 
## ## ## 2009## ## 
## ## ## 2009m94## ## 
## ## ## 2009m97## ## 
## ## 2009m00## ## 
## tmp2 <- comp(what="p18", yr=2009, unit="m", census.data = censom00.3pt)## ## 
## #### ## 
## v09m00$p18t   <- tmp2$pob18t # paste interpolation## ## 
## v09m00$p18e   <- tmp2$pob18e # paste interpolation## ## 
## v09m00$ess    <- tmp2$ess                    ## ## 
## v09m00$er2    <- tmp2$er2                    ## ## 
## v09m00$p18l   <- tmp2$pob18l # paste interpolation## ## 
## v09m00$lss    <- tmp2$lss                    ## ## 
## v09m00$lr2    <- tmp2$lr2                    ## ## 
## ## ## 2009m03## ## 
## ## ## 2009m06## ## 
## ## ## 2009m12## ## 
## ## ## 2009m15## ## 
## ## ## 2009m18## ## 
## ## ## 2009m21## 
## ## 
## ## ## 2012## ## 
## ## ## 2012m94## ## 
## ## ## 2012m97## ## 
## ## 2012m00## ## 
## tmp2 <- comp(what="p18", yr=2012, unit="m", census.data = censom00.3pt)## ## 
## #### ## 
## v12m00$p18t   <- tmp2$pob18t # paste interpolation## ## 
## v12m00$p18e   <- tmp2$pob18e # paste interpolation## ## 
## v12m00$ess    <- tmp2$ess                    ## ## 
## v12m00$er2    <- tmp2$er2                    ## ## 
## v12m00$p18l   <- tmp2$pob18l # paste interpolation## ## 
## v12m00$lss    <- tmp2$lss                    ## ## 
## v12m00$lr2    <- tmp2$lr2                    ## ## 
## ## ## 2012m03## ## 
## ## ## 2012m06## ## 
## ## ## 2012m09## ## 
## ## ## 2012m15## ## 
## ## ## 2012m18## ## 
## ## ## 2012m21## 
## ## 
## ## ## 2015## ## 
## ## ## 2015m94## ## 
## ## ## 2015m97## ## 
## ## 2015m00## ## 
## tmp2 <- comp(what="p18", yr=2015, unit="m", census.data = censom00.3pt)## ## 
## #### ## 
## v15m00$p18t   <- tmp2$pob18t # paste interpolation## ## 
## v15m00$p18e   <- tmp2$pob18e # paste interpolation## ## 
## v15m00$ess    <- tmp2$ess                    ## ## 
## v15m00$er2    <- tmp2$er2                    ## ## 
## v15m00$p18l   <- tmp2$pob18l # paste interpolation## ## 
## v15m00$lss    <- tmp2$lss                    ## ## 
## v15m00$lr2    <- tmp2$lr2                    ## ## 
## ## ## 2015m03## ## 
## ## ## 2015m06## ## 
## ## ## 2015m09## ## 
## ## ## 2015m12## ## 
## ## ## 2015m18## ## 
## ## ## 2015m21## 
## ## 
## ## ## 2018## ## 
## ## ## 2018m94## ## 
## ## ## 2018m97## ## 
## ## 2018m00## ## 
## tmp2 <- comp(what="p18", yr=2018, unit="m", census.data = censom00.3pt)## ## 
## #### ## 
## v18m00$p18t   <- tmp2$pob18t # paste interpolation## ## 
## v18m00$p18e   <- tmp2$pob18e # paste interpolation## ## 
## v18m00$ess    <- tmp2$ess                    ## ## 
## v18m00$er2    <- tmp2$er2                    ## ## 
## v18m00$p18l   <- tmp2$pob18l # paste interpolation## ## 
## v18m00$lss    <- tmp2$lss                    ## ## 
## v18m00$lr2    <- tmp2$lr2                    ## ## 
## ## ## 2018m03## ## 
## ## ## 2018m06## ## 
## ## ## 2018m09## ## 
## ## ## 2018m12## ## 
## ## ## 2018m15## ## 
## ## ## 2018m21## 
## ## 
## ## ## 2021## ## 
## ## ## 2021m94## ## 
## ## ## 2021m97## ## 
## ## 2021m00## ## 
## tmp2 <- comp(what="p18", yr=2021, unit="m", census.data = censom00.3pt)## ## 
## #### ## 
## v21m00$p18t   <- tmp2$pob18t # paste interpolation## ## 
## v21m00$p18e   <- tmp2$pob18e # paste interpolation## ## 
## v21m00$ess    <- tmp2$ess                    ## ## 
## v21m00$er2    <- tmp2$er2                    ## ## 
## v21m00$p18l   <- tmp2$pob18l # paste interpolation## ## 
## v21m00$lss    <- tmp2$lss                    ## ## 
## v21m00$lr2    <- tmp2$lr2                    ## ## 
## ## ## 2021m03## ## 
## ## ## 2021m06## ## 
## ## ## 2021m09## ## 
## ## ## 2021m12## ## 
## ## ## 2021m15## ## 
## ## ## 2021m18## 
## ## 
## ## compare 3-census projections against 1995 and 2000 censuses## ## 
## p18_1995.e <- interlog(what="p18", yr=1995, unit="m", census.data = censom00.3pt, frm="log(dv)~iv")[[1]]## ## 
## p18_2000.e <- interlog(what="p18", yr=2000, unit="m", census.data = censom00.3pt, frm="log(dv)~iv")[[1]]## ## 
## p18_1995.l <- interlog(what="p18", yr=1995, unit="m", census.data = censom00.3pt, frm="dv~iv")[[1]]## ## 
## p18_2000.l <- interlog(what="p18", yr=2000, unit="m", census.data = censom00.3pt, frm="dv~iv")[[1]]## ## 
## off95e <- abs(p18_1995.e - censom00$p18_1995) / censom00$p18_1995## ## 
## off00e <- abs(p18_2000.e - censom00$p18_2000) / censom00$p18_2000## ## 
## off95l <- abs(p18_1995.l - censom00$p18_1995) / censom00$p18_1995## ## 
## off00l <- abs(p18_2000.l - censom00$p18_2000) / censom00$p18_2000## 
## ## 
## off9500e <- sqrt((((p18_1995.e - censom00$p18_1995) / censom00$p18_1995)^2 + ((p18_2000.e - censom00$p18_2000) / censom00$p18_2000)^2)/2)## ## 
## off9500l <- sqrt((((p18_1995.l - censom00$p18_1995) / censom00$p18_1995)^2 + ((p18_2000.l - censom00$p18_2000) / censom00$p18_2000)^2)/2)## 
## ## 
## off9500e[i]## ## 
## off9500l[i]## 
## ## 
## ## summarize## ## 
## mysum(off95e)## ## 
## mysum(off00e)## ## 
## mysum(off95l)## ## 
## mysum(off00l)## 
## ## 
## iv <- v00m$ess + .0001 # avoid indeterminate logs## ## 
## dv <- off95e   + .0001## ## 
## summary(iv)## ## 
## summary(dv)## ## 
## plot(dv ~ iv, log="xy", axes = TRUE)## ## 
## abline(lm(dv ~ iv), untf = TRUE)## ## 
## axis(1, at = c(.0001,.001,.01,1))## ## 
## summary(lm(dv ~ iv))## 
## ## 
## off9500l[i]
## 
## ## 
## ## ## compare linear and log projections in census years## ## 
## ## Inspect census figs## ## 
## myplot <- function(i=NA, print=FALSE){## ## 
##     #i <- 63 # debug## ## 
##     if (print==TRUE) png(paste0("/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/plots/", "mu", censom00$inegi[i], "p18-interlog-3pt.png"))## ## 
##     plot(  seq(1994,2021,3), li,## ## 
##          xlim = c(1988,2027),## ## 
##          ylim = c(0,1.3*max(li,lg,ls   )), col="blue", xlab="", ylab="voting age population", main = paste(censom00$inegi[i], censom00$mun[i], "Coahuila"), sub="blue=linear model, red=log-linear model, solid=census")## ## 
##     ##plot(  seq(1994,2021,3), li, ylim = c(min(li,lg,ls,ef),max(li,lg,ls,ef)), main = paste(i, "bck=line, red=log, blue=lis, grn=efec"))## ## 
##     points(seq(1994,2021,3), lg, col = "red")## ## 
##     ##points(seq(1994,2021,3), ls, col = "blue")## ## 
##     ##points(seq(1994,2021,3), ef, col = "green")## ## 
##     points(1995, c1, pch = 19, cex = .75)## ## 
##     points(1995, c1, pch = "X", cex = .75)## ## 
##     points(  2000,            c2, pch = 19, cex = .75)## ## 
##     points(  2000,            c2, pch = "X", cex = .75)## ## 
##     points(c(2005,2010,2020), censom00[i,c("p18_2005","p18_2010","p18_2020")], pch=19, cex=.75)## ## 
##     legend("bottomright",## ## 
##            legend = c("RQMR (05-20) = 0.015", "RQMR (05-20) = 0.047", "RQMR (95-00) = 0.098", "RQMR (95-00) = 0.731"),## ## 
##            pch = c(1,1,1,1), col = c("red","blue","red","blue"))## ## 
## ##     legend("bottomright",## ## 
## ##            legend = c(paste0("RQMR (3-point) = ", v00m$ess[i]), paste0("RQMR (3-point) = ", v00m$lss[i])),## ## 
## ## #           legend = c(paste0("R^2 = ", v15m00$pob18er2[i]), paste0("R^2 = ", v15m00$pob18lr2[i])),## ## 
## ##            pch = c(1,1), col = c("red","blue"))## ## 
##     ##abline(v=2000, lty=2)## ## 
## #### ## 
## ## points(1988, interlog(what="p18", yr=1988, unit="m", census.data = censom00.3pt, frm="log(dv)~iv")[[1]][i], col = "red")## ## 
## ## points(1991, interlog(what="p18", yr=1991, unit="m", census.data = censom00.3pt, frm="log(dv)~iv")[[1]][i], col = "red")## ## 
## ## points(2024, interlog(what="p18", yr=2024, unit="m", census.data = censom00.3pt, frm="log(dv)~iv")[[1]][i], col = "red")## ## 
## ## points(2027, interlog(what="p18", yr=2027, unit="m", census.data = censom00.3pt, frm="log(dv)~iv")[[1]][i], col = "red")## ## 
## ## #### ## 
## ## points(1988, interlog(what="p18", yr=1988, unit="m", census.data = censom00.3pt, frm="dv~iv")[[1]][i], col = "blue")## ## 
## ## points(1991, interlog(what="p18", yr=1991, unit="m", census.data = censom00.3pt, frm="dv~iv")[[1]][i], col = "blue")## ## 
## ## points(2024, interlog(what="p18", yr=2024, unit="m", census.data = censom00.3pt, frm="dv~iv")[[1]][i], col = "blue")## ## 
## ## points(2027, interlog(what="p18", yr=2027, unit="m", census.data = censom00.3pt, frm="dv~iv")[[1]][i], col = "blue")## ## 
## #### ## 
##     if (print==TRUE) dev.off()## ## 
## }## 
## ## 
## i <- 55## ## 
## i <- i+1## ## 
## ls <- c(## ## 
##     v94m00$lisnom[i],## ## 
##     v97m00$lisnom[i],## ## 
##     v00m  $lisnom[i],## ## 
##     v03m00$lisnom[i],## ## 
##     v06m00$lisnom[i],## ## 
##     v09m00$lisnom[i],## ## 
##     v12m00$lisnom[i],## ## 
##     v15m00$lisnom[i],## ## 
##     v18m00$lisnom[i],## ## 
##     v21m00$lisnom[i]## ## 
## )## ## 
## ef <- c(## ## 
##     v94m00$efec[i],## ## 
##     v97m00$efec[i],## ## 
##     v00m  $efec[i],## ## 
##     v03m00$efec[i],## ## 
##     v06m00$efec[i],## ## 
##     v09m00$efec[i],## ## 
##     v12m00$efec[i],## ## 
##     v15m00$efec[i],## ## 
##     v18m00$efec[i],## ## 
##     v21m00$efec[i]## ## 
## )## ## 
## li <- c(## ## 
##     v94m00$p18l[i],## ## 
##     v97m00$p18l[i],## ## 
##     v00m  $p18l[i],## ## 
##     v03m00$p18l[i],## ## 
##     v06m00$p18l[i],## ## 
##     v09m00$p18l[i],## ## 
##     v12m00$p18l[i],## ## 
##     v15m00$p18l[i],## ## 
##     v18m00$p18l[i],## ## 
##     v21m00$p18l[i]## ## 
## )## ## 
## lg <- c(## ## 
##     v94m00$p18e[i],## ## 
##     v97m00$p18e[i],## ## 
##     v00m  $p18e[i],## ## 
##     v03m00$p18e[i],## ## 
##     v06m00$p18e[i],## ## 
##     v09m00$p18e[i],## ## 
##     v12m00$p18e[i],## ## 
##     v15m00$p18e[i],## ## 
##     v18m00$p18e[i],## ## 
##     v21m00$p18e[i]## ## 
## )## ## 
## tw <- c(## ## 
##     v94m00$p18t[i],## ## 
##     v97m00$p18t[i],## ## 
##     v00m  $p18t[i],## ## 
##     v03m00$p18t[i],## ## 
##     v06m00$p18t[i],## ## 
##     v09m00$p18t[i],## ## 
##     v12m00$p18t[i],## ## 
##     v15m00$p18t[i],## ## 
##     v18m00$p18t[i],## ## 
##     v21m00$p18t[i]## ## 
## )## ## 
## c1 <- as.numeric(censom00$p18_1995[i])## ## 
## c2 <- as.numeric(censom00$p18_2000[i])## ## 
## #### ## 
## myplot(i, print=FALSE)## 
## ## 
## ## Any year has the map's statistics## ## 
## mysum(v00m$ess)## ## 
## mysum(v00m$lss)



