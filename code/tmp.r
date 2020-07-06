rm(list = ls())
wd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
setwd(wd)

# alcaldes
inc <- read.csv(file = "aymu1989-present.incumbents.csv", stringsAsFactors = FALSE)
colnames(inc)

# votos
vot <- read.csv(file = "aymu1989-present.coalAgg.csv", stringsAsFactors = FALSE)
vot$mg <- (vot$v01 - vot$v02)/vot$efec
vot <- vot[,c("emm","l01","l02","mg")]

# merge
inc <- merge(x = inc, y = vot, by = "emm", all.x = TRUE, all.y = FALSE)
colnames(inc)

write.csv(inc, file = "tmp.csv")

inc[1,]
vot[1,]
