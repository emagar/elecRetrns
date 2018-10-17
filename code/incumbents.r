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

# simplify parties
inc$win2 <- inc$win
sel <- grep("ci_|^ci$|c-i-|ind_|eduardo|luis|oscar|indep", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "indep"
sel <- grep("pan-", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "pan-coal"
sel <- grep("pri-", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "pri-coal"
sel <- grep("prd-", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "prd-coal"
sel <- grep("morena-|-morena", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "morena-coal"
sel <- grep("pvem-", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "pvem-coal"
sel <- grep("mas|paz|pcp|phm|pmr|ppg|psd1|psi|pudc|pup|via_", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "local"
sel <- grep("pt1", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "pt"
#
sel <- which(inc$yr==2018)
table(inc$win2[sel])


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
sel <- which(inc$race.prior=="") #cases still unknown
inc$race.prior[sel] <- "Yet-to-know"


sel <- which(inc$yr==2018 & inc$edon!=9 & inc$edon!=21)
table(inc$edon[sel], inc$race.prior[sel])
      table(inc$race.prior[sel])
round(table(inc$race.prior[sel]) / length(sel),2)





head(inc)

I still need to collect data for Chiapas, Michoacán, and Sinaloa. 

