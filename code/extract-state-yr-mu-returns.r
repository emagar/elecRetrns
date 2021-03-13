rm(list = ls())

# set workind directory here
dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
setwd(dd)

# read data with coalition aggregates file
dat <- read.csv(file = "aymu1989-present.coalAgg.csv", stringsAsFactors = FALSE)
dim(dat)

# state abbreviations
edos <- c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac")
estados <- c("aguascalientes", "baja california", "baja california sur", "campeche", "coahuila", "colima", "chiapas", "chihuahua", "distrito federal", "durango", "guanajuato", "guerrero", "hidalgo", "jalisco", "mexico", "michoacan", "morelos", "nayarit", "nuevo leon", "oaxaca", "puebla", "queretaro", "quintana roo", "san luis potosi", "sinaloa", "sonora", "tabasco", "tamaulipas", "tlaxcala", "veracruz", "yucatan", "zacatecas")

# prompt
e <- 9
edo <- edos[e]

list("Choose one of the following municipal election years:", as.numeric(names(table(dat$yr[dat$edon==e], useNA = "ifany"))))
y <- 2015




