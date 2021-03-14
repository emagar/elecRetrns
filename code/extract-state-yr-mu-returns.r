##############################################################################################
## Script to export single state-year municipal election returns with coalition aggregates. ##
## Output saved in ./xport/ directory with one column for each party.                       ##
##############################################################################################


# Reads xport function
setwd("/home/eric/Downloads/Desktop/MXelsCalendGovt/elecReturns/code")
source("xport-function.r")

# Usage
xport(e=2, y=2019)

