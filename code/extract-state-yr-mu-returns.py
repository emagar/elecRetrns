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
## Translated to Python with chatGPT from original R code on: 6-jun-2026                                              ##
## Last revised:  6-jun-2026                                                                                          ##
########################################################################################################################

import os
import getpass
from pathlib import Path
import requests

# Edit to the directory where
# aymu1970-present.coalAgg.csv and aymu1970-present.coalSplit.csv are stored
os.chdir("/home/eric/Desktop/MXelsCalendGovt/elecReturns/data")

# If on my machine, use local scripts; otherwise get from the web
user = getpass.getuser()

if user in ("eric", "magar"):
    pth = os.path.expanduser("~/Dropbox/data/useful-functions")
else:
    pth = "https://raw.githubusercontent.com/emagar/useful-functions/master"

# Read xport function
xport_path = f"{pth}/xport-function.py"

if xport_path.startswith("http"):
    response = requests.get(xport_path)
    response.raise_for_status()

    # Execute downloaded script in current namespace
    exec(response.text, globals())
else:
    with open(xport_path, "r") as f:
        exec(f.read(), globals())

del pth
del xport_path

# Usage:
# xport(e=2, y=2019)  # where e is edon
