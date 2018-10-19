wd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
setwd(wd)

# alcaldes
inc <- read.csv(file = "aymu1985-present.incumbents.csv", stringsAsFactors = FALSE)

head(inc)

inc$race.prior <- NA
inc$pty.prior <- NA

# drop observations
sel <- grep("drop-obs", inc$race.after, ignore.case = TRUE)
inc <- inc[-sel,]

# lag race.after & win to generate race.prior & pty.prior
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
        tmp <- inc.m$win
        tmp <- c(NA, tmp[-M])
        inc.m$pty.prior <- tmp
        inc.e[sel.m,] <- inc.m
    }
    inc[sel.e,] <- inc.e
}

# simplify parties
inc$win2 <- inc$win
sel <- grep("ci_|^ci$|c-i-|ind_|eduardo|luis|oscar|indep", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "indep"
sel <- grep("pan-", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "pan"
sel <- grep("pri-", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "pri"
sel <- grep("prd-", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "prd"
sel <- grep("morena-|-morena", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "morena"
sel <- grep("pvem-", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "pvem"
sel <- grep("mas|paz|pcp|phm|pmr|ppg|psd1|psi|pudc|pup|via_", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "local"
sel <- grep("pt1", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "pt"
#
inc$pty.prior2 <- inc$pty.prior
sel <- grep("ci_|^ci$|c-i-|ind_|eduardo|luis|oscar|indep", inc$pty.prior2, ignore.case = TRUE)
inc$pty.prior2[sel] <- "indep"
sel <- grep("pan", inc$pty.prior2, ignore.case = TRUE)
inc$pty.prior2[sel] <- "pan"
sel <- grep("pri", inc$pty.prior2, ignore.case = TRUE)
inc$pty.prior2[sel] <- "pri"
sel <- grep("prd", inc$pty.prior2, ignore.case = TRUE)
inc$pty.prior2[sel] <- "prd"
sel <- grep("morena", inc$pty.prior2, ignore.case = TRUE)
inc$pty.prior2[sel] <- "morena"
sel <- grep("pvem", inc$pty.prior2, ignore.case = TRUE)
inc$pty.prior2[sel] <- "pvem"
sel <- grep("mc", inc$pty.prior2, ignore.case = TRUE)
inc$pty.prior2[sel] <- "mc"
sel <- grep("mas|paz|pcp|phm|pmr|ppg|psd1|psi|pudc|pup|via_|pasd|pcu|ph|pmac|prs", inc$pty.prior2, ignore.case = TRUE)
inc$pty.prior2[sel] <- "local"
sel <- grep("pt1", inc$pty.prior2, ignore.case = TRUE)
inc$pty.prior2[sel] <- "pt"
#
sel <- which(inc$yr==2018)
table(inc$pty.prior2[sel])

table(inc$race.prior)
sel <- which(inc$race.prior=="Reelected")
inc$race.prior[sel] <- "Incumb-stayed"
sel <- which(inc$race.prior=="Beaten")
inc$race.prior[sel] <- "Incumb-ousted"
sel <- grep("p-won", inc$race.prior, ignore.case = TRUE) 
inc$race.prior[sel] <- "Open-same-pty"
sel <- grep("p-lost", inc$race.prior, ignore.case = TRUE) 
inc$race.prior[sel] <- "Open-dif-pty"
sel <- grep("pending", inc$race.prior, ignore.case = TRUE) 
inc$race.prior[sel] <- "pending"


sel <- which(inc$race.prior=="" & inc$incumbent!="") #cases still unknown
#inc$incumbent[sel]
inc$race.prior[sel] <- "Frosh-unknown"
sel <- which(inc$race.prior=="") #cases still unknown
inc$race.prior[sel] <- "Yet-to-know"

# subset
sel <- which(inc$yr==2018 & inc$edon!=9 & inc$edon!=21)
sel <- which(inc$yr==2018 & inc$edon!=9 & inc$edon!=21 & inc$edon!=7 & inc$edon!=16)
inc.sub <- inc[sel,]

table(inc.sub$edon, inc.sub$race.prior)
      table(inc.sub$race.prior)
round(table(inc.sub$race.prior) / length(sel),2)

table(inc.sub$win2, inc.sub$race.prior)
tab <- table(inc.sub$pty.prior2, inc.sub$race.prior)
rowSums(tab)
round(table(inc.sub$pty.prior2, inc.sub$race.prior) *100 / rowSums(tab), 0)

                     party reelected
incumbent on ballot  yes          no
yes
no

out-pwon  = open-seat-same-pty
out-plost = open-seat-dif-party
reelected = incumbent-reelected
beaten    = incumbent-ousted

head(inc)


