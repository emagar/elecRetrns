rm(list=ls())
wd <- "/home/eric/Downloads/Desktop/MXelsCalendGovt/elecReturns/datosBrutos/irene-camila/"
setwd(wd)

library(haven) ## reads stata files

v18 <- read_dta("Elecciones2018.dta")
head(v18)
write.csv(v18, "e2018.csv")

v21 <- read_dta("Elecciones2021.dta")
head(v21)
write.csv(v21, "e2021.csv")

v24 <- read_dta("Elecciones2024.dta")
head(v24)
write.csv(v24, "e2024.csv")
