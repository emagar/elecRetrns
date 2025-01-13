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

####################
## Election cycle ##
####################
d$cycle <- sub(pattern="^[a-z]+.([0-9]+).*$", replacement = "\\1", d$emm, perl=TRUE)
d$cycle <- as.numeric(d$cycle)
table(d$cycle)

##############
## Coverage ##
##############
table(d$status)
sel.r <- grep("cancelled|to-runoff", d$status); ds <- ds <- d[-sel.r,] # subset wo cancelled races keeps actual number
i <- 0
i <- i+1; edon2edo(i); table(ds$cycle[ds$edon==i]); table(ds$yr[ds$edon==i])
## Overall
table(ds$cycle)
table(ds$yr)

getwd()
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
x


edon2estado(1:32)
edon2edo(1:32)
