wd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
setwd(wd)

# alcaldes
inc <- read.csv(file = "aymu1985-present.incumbents.csv", stringsAsFactors = FALSE)

head(inc)


inc$prev.race <- NA


# lag next.race
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
        tmp <- inc.m$next.race
        tmp <- c(NA, tmp[-M])
        inc.m$prev.race <- tmp
        inc.e[sel.m,] <- inc.m
    }
    inc[sel.e,] <- inc.e
}

table(inc$prev.race)
sel <- grep("out", inc$prev.race, ignore.case = TRUE) 
inc$prev.race[sel] <- "Frosh-open seat"
sel <- which(inc$prev.race=="Higher-office")
inc$prev.race[sel] <- "Frosh-open seat"
sel <- which(inc$prev.race=="Term-limited")
inc$prev.race[sel] <- "Frosh-open seat"
sel <- which(inc$prev.race=="Beaten")
inc$prev.race[sel] <- "Frosh-beat incumbent"

sel <- which(inc$prev.race=="" & inc$incumbent!="") #cases still unknown
#inc$incumbent[sel]
inc$prev.race[sel] <- "Frosh-unknown"

table(inc$prev.race[inc$yr==2018])

head(inc)

