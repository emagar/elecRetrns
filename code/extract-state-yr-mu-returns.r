########################################################################################################################
## Script reads a function exporting a single state-year municipal election returns with simplified column structure. ##
## Output is returned (or saved in ./xport/ if write.to.file set to TRUE) with one column for each party.             ##
## Choose a state number (eg. e=2 for Baja California) and a known electoral year (eg. y=2019) in order               ##
## to output a dataframe with municipalities reported in each row. Function re-arranges the data frame so             ##
## that vote returns appear in columns named after the corresponding party/coalition.                                 ##
##                                                                                                                    ##
## Usage:                                                                                                             ##
## xport(e=2, y=2019) # where e is edon, y a known electoral year                                                     ##
##                                                                                                                    ##
## Author: Eric Magar emagar at itam dot mx                                                                           ##
## Created:      13-mar-2021                                                                                          ##
## Last revised:  4-apr-2025                                                                                           ##
########################################################################################################################

## Edit your path to where aymu1970-present.coalAgg.csv and aymu1970-present.coalSplit.csv are stored 
setwd("/home/eric/Desktop/MXelsCalendGovt/elecReturns/data")

## if my machine use scripts in disk, else get from web
pth <- ifelse (Sys.info()["user"] %in% c("eric", "magar"),
    "~/Dropbox/data/useful-functions",
    "https://raw.githubusercontent.com/emagar/useful-functions/master"
    )
## Reads xport function
pth <- paste(pth, "xport-function.r", sep = "/")
source(pth)
rm(pth) ## clean

## # Usage
## xport(e=2, y=2019) # where e is edon
