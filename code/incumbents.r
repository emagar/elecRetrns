wd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
setwd(wd)

# alcaldes
inc <- read.csv(file = "aymu1985-present.incumbents.csv", stringsAsFactors = FALSE)

head(inc)


inc$race.prior <- NA

# drop observations
sel <- grep("drop-obs", inc$race.after, ignore.case = TRUE)
inc <- inc[-sel,]

# lag race.after to generate race.prior
inc <- inc[order(inc$emm),] # sort munn-chrono
for (e in 1:32){
    #e <- 8 #debug
    sel.e <- which(inc$edon==e)
    inc.e <- inc[sel.e,]
    mm <- unique(inc.e$munn)
    for (m in mm){
        #m <- 5 #debug
        sel.m <- which(inc.e$munn==m)
        inc.m <- inc.e[sel.m,]
        M <- nrow(inc.m)
        if (M==1) next
        tmp <- inc.m$race.after
        tmp <- c(NA, tmp[-M])
        inc.m$race.prior <- tmp
        inc.e[sel.m,] <- inc.m
    }
    inc[sel.e,] <- inc.e
}

table(inc$race.prior)
sel <- grep("out", inc$race.prior, ignore.case = TRUE) 
inc$race.prior[sel] <- "Frosh-open-seat"
sel <- grep("higher-office", inc$race.prior, ignore.case = TRUE) 
inc$race.prior[sel] <- "Frosh-open-seat"
sel <- which(inc$race.prior=="Term-limited")
inc$race.prior[sel] <- "Frosh-open-seat"
sel <- which(inc$race.prior=="Renom-killed")
inc$race.prior[sel] <- "Frosh-open-seat"
sel <- which(inc$race.prior=="Beaten")
inc$race.prior[sel] <- "Frosh-who-ousted"

sel <- which(inc$race.prior=="" & inc$incumbent!="") #cases still unknown
#inc$incumbent[sel]
inc$race.prior[sel] <- "Frosh-unknown"

table(inc$edon[inc$yr==2018], inc$race.prior[inc$yr==2018])
      table(inc$race.prior[inc$yr==2018])
round(table(inc$race.prior[inc$yr==2018]) / sum(as.numeric(inc$yr==2018)),2)

head(inc)

