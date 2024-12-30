rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
setwd(dd)

#####################
## Read data files ##
#####################
d  <- read.csv(file = "aymu1970-on.coalAgg.csv"  , stringsAsFactors = FALSE)
ds <- read.csv(file = "aymu1970-on.coalSplit.csv", stringsAsFactors = FALSE)


