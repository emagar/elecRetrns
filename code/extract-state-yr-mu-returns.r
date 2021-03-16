##############################################################################################
## Script to export single state-year municipal election returns with coalition aggregates. ##
## Output saved in ./xport/ directory with one column for each party.                       ##
##############################################################################################

# Edit your path to aymu1989-present.coalAgg.csv
setwd("/home/eric/Desktop/MXelsCalendGovt/elecReturns/data")

# if my machine use scripts in disk
pth <- ifelse (Sys.info()["user"] %in% c("eric", "magar"),
    "~/Dropbox/data/useful-functions",
    "https://raw.githubusercontent.com/emagar/useful-functions/master"
    )
#pth <- "https://raw.githubusercontent.com/emagar/useful-functions/master" # debug


# Reads xport function
pth <- paste(pth, "xport-function.r", sep = "/")
source(pth)

## # if no internet connection, source from disk version
## source("~/dropbox/data/useful-functions/xport-function.r")

# Usage
xport(e=2, y=2019) # where e is edon or edo

# or be prompted for a year
xport(e=2)
