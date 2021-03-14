##############################################################################################
## Script to export single state-year municipal election returns with coalition aggregates. ##
## Output saved in ./xport/ directory with one column for each party.                       ##
##############################################################################################

# Edit your path to code/
setwd("/home/eric/Desktop/MXelsCalendGovt/elecReturns/code")

# Reads xport function
source("xport-function.r")

# Usage
xport(e=2, y=2019)

# or be prompted for e and y
xport()
