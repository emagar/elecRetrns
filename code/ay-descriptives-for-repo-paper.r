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
sel.r <- grep("cancelled", d$status); ds <- ds <- d[-sel.r,] # subset wo cancelled races keeps actual number
i <- i+1; edon2edo(i); table(ds$yr[ds$edon==i])
i <- 0
i <- i+1; edon2edo(i); sel.r <- which(d$edon==i & d$status!="cancelled"); ds <- d[sel.r,]; table(ds$yr)
x


edon2estado(1:32)
edon2edo(1:32)
