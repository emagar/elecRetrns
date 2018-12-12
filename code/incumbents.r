rm(list = ls())
wd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
setwd(wd)

# alcaldes
inc <- read.csv(file = "aymu1997-present.incumbents.csv", stringsAsFactors = FALSE)
colnames(inc)

## # merge a new coalAgg into incumbents
## cagg <- read.csv(file = "aymu1997-present.coalAgg.csv", stringsAsFactors = FALSE)
## colnames(cagg)
## cagg <- cagg[, c("emm","win")]
## inc <- merge(x = inc, y = cagg, by = "emm", all = TRUE)
## sel <- grep("pt1", inc$win.y, ignore.case = TRUE)  # change labels
## inc$win.y[sel] <- gsub("pt1", "pt", inc$win.y[sel])# change labels
## sel <- grep("pmc$|pmc-", inc$win.y, ignore.case = TRUE) # change labels
## inc$win.y[sel] <- gsub("pmc", "mc", inc$win.y[sel])                             # change labels
## sel <- grep("panal", inc$win.y, ignore.case = TRUE)# change labels
## inc$win.y[sel] <- gsub("panal", "pna", inc$win.y[sel])                           # change labels
## sel <- grep("[a-z]+[-][0-9]{2}[ab].*", inc$emm) # find anuladas/ballotage
## inc <- inc[-sel,] # drop them
## write.csv(inc, file = "tmp.csv", row.names = FALSE) # verify what tmp.csv looks like


inc$race.prior <- NA
inc$win.prior <- NA

# drop observations that went to usos y costumbres
sel <- grep("drop-obs", inc$race.after, ignore.case = TRUE)
if (length(sel)>0) inc <- inc[-sel,]

# lag race.after & win to generate race.prior & win.prior
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
        inc.m$win.prior <- tmp
        inc.e[sel.m,] <- inc.m
    }
    inc[sel.e,] <- inc.e
}

# simplify parties
inc$win2 <- inc$win
sel <- grep("conve", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "mc"
sel <- grep("ci_|^ci$|c-i-|ind_|eduardo|luis|oscar|indep", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "indep"
sel <- grep("pan-", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "pan"
sel <- grep("pri-", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "pri"
sel <- grep("prd-|-prd", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "prd"
sel <- grep("morena-|-morena", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "morena"
sel <- grep("pvem-|-pvem", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "pvem"
sel <- grep("mc-|-mc", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "mc"
sel <- grep("-pes", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "pes"
sel <- grep("mas|paz|pcp|phm|pmr|ppg|psd1|psi|pudc|pup|via_|pchu|pmch|pver|prs|prv|ps1|poc|pjs|pd1|pec|pasd|pac1|npp|pcu|pcdt|pmac|pcm2|pdm|pps|ppt|ph|pmp|fc1|pcd1|psn|ave", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "loc/oth"
#
inc$win.prior2 <- inc$win.prior
sel <- grep("ci_|^ci$|c-i-|ind_|eduardo|luis|oscar|indep", inc$win.prior2, ignore.case = TRUE)
inc$win.prior2[sel] <- "indep"
sel <- grep("pan", inc$win.prior2, ignore.case = TRUE)
inc$win.prior2[sel] <- "pan"
sel <- grep("pri", inc$win.prior2, ignore.case = TRUE)
inc$win.prior2[sel] <- "pri"
sel <- grep("prd", inc$win.prior2, ignore.case = TRUE)
inc$win.prior2[sel] <- "prd"
sel <- grep("morena", inc$win.prior2, ignore.case = TRUE)
inc$win.prior2[sel] <- "morena"
sel <- grep("pvem", inc$win.prior2, ignore.case = TRUE)
inc$win.prior2[sel] <- "pvem"
sel <- grep("mc", inc$win.prior2, ignore.case = TRUE)
inc$win.prior2[sel] <- "mc"
sel <- grep("mas|paz|pcp|phm|pmr|ppg|psd1|psi|pudc|pup|via_|pasd|pcu|ph|pmac|prs", inc$win.prior2, ignore.case = TRUE)
inc$win.prior2[sel] <- "local"
sel <- grep("pt1", inc$win.prior2, ignore.case = TRUE)
inc$win.prior2[sel] <- "pt"
#
sel <- which(inc$yr==2018)
table(inc$win.prior2[sel])

table(inc$race.prior)
sel <- which(inc$race.prior=="Reelected")
inc$race.prior[sel] <- "Incumb-stayed"
sel <- which(inc$race.prior=="Beaten")
inc$race.prior[sel] <- "Incumb-ousted"
sel <- grep("p-won", inc$race.prior, ignore.case = TRUE) 
inc$race.prior[sel] <- "Open-same-pty"
sel <- grep("p-lost", inc$race.prior, ignore.case = TRUE) 
inc$race.prior[sel] <- "Open-dif-pty"
sel <- grep("pending|out-p-[?]", inc$race.prior, ignore.case = TRUE) 
inc$race.prior[sel] <- "pending"
table(inc$race.prior)

#############################################
# subset: cases allowing reelection in 2018 #
# esto lo reporté en el blog de Nexos       #
#############################################
sel <- which(inc$yr==2018 & inc$edon!=9 & inc$edon!=21)
inc.sub <- inc[sel,]

sel <- which(inc.sub$race.prior=="pending"|inc.sub$race.prior=="")
inc.sub$emm[sel]
inc.sub <- inc.sub[-sel,] # drop cases with pending election

table(inc.sub$edon, inc.sub$race.prior)
      table(inc.sub$race.prior)
nrow(inc.sub)
round(table(inc.sub$race.prior) / nrow(inc.sub),2)

table(inc.sub$win2, inc.sub$race.prior)
tab <- table(inc.sub$win.prior2, inc.sub$race.prior)
rowSums(tab)
sum(rowSums(tab))
round(table(inc.sub$win.prior2, inc.sub$race.prior) *100 / rowSums(tab), 1)
round(colSums(tab) *100 / sum(rowSums(tab)), 1)


# subset: cases NOT allowing reelection in 2018
sel <- which(inc$yr==2018 & inc$edon!=9 & inc$edon!=21)
sel <- which(inc$yr==2018 & (inc$edon==9 | inc$edon==21))
inc.sub <- inc[sel,]

table(inc.sub$edon, inc.sub$race.prior)
      table(inc.sub$race.prior)
round(table(inc.sub$race.prior) / length(sel),2)

table(inc.sub$win2, inc.sub$race.prior)
tab <- table(inc.sub$win.prior2, inc.sub$race.prior)
rowSums(tab)
round(table(inc.sub$win.prior2, inc.sub$race.prior) *100 / rowSums(tab), 0)

sel <- which(inc$yr==2018 & inc$edon!=16)
inc.sub <- inc[sel,]

###############################################################
# classify term-limited cases according to party returned/not #
###############################################################
duplic.win <- inc$win; duplic.win.prior <- inc$win.prior # duplicate
inc$win <- gsub("conve", "mc", inc$win)
inc$win.prior <- gsub("conve", "mc", inc$win.prior)
#
inc$manipule <- inc$race.prior # manipulate a copy
# 
sel.tl <- which(inc$manipule=="Term-limited" & inc$win!="" & inc$win.prior!="")
inc.sub <- inc[sel.tl,] # subset
# n coalition members
num <- gsub(pattern = "[^-]*", replacement = "", inc.sub$win, perl = TRUE) # keep hyphens only
num <- sapply(num, function(x) nchar(x)+1); names(num) <- NULL#n hyphens
sel <- which(num==7) # subset coals with seven members
for (i in sel){
    tmp7 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\7", inc.sub$win[i])
    tmp6 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\6", inc.sub$win[i])
    tmp5 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\5", inc.sub$win[i])
    tmp4 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\4", inc.sub$win[i])
    tmp3 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\3", inc.sub$win[i])
    tmp2 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\2", inc.sub$win[i])
    tmp1 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\1", inc.sub$win[i])
    if ( length(grep(pattern = tmp7, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp6, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp5, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp4, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp3, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp2, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp1, inc.sub$win.prior[i]))>0) {
        inc.sub$manipule[i] <- "Term-limited-p-won"
    } else {
        inc.sub$manipule[i] <- "Term-limited-p-lost"
    }
}
sel <- which(num==6) # subset coals with six members
for (i in sel){
    tmp6 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\6", inc.sub$win[i])
    tmp5 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\5", inc.sub$win[i])
    tmp4 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\4", inc.sub$win[i])
    tmp3 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\3", inc.sub$win[i])
    tmp2 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\2", inc.sub$win[i])
    tmp1 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\1", inc.sub$win[i])
    if ( length(grep(pattern = tmp6, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp5, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp4, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp3, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp2, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp1, inc.sub$win.prior[i]))>0) {
        inc.sub$manipule[i] <- "Term-limited-p-won"
    } else {
        inc.sub$manipule[i] <- "Term-limited-p-lost"
    }
}
sel <- which(num==5) # subset coals with five members
for (i in sel){
    tmp5 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\5", inc.sub$win[i])
    tmp4 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\4", inc.sub$win[i])
    tmp3 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\3", inc.sub$win[i])
    tmp2 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\2", inc.sub$win[i])
    tmp1 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\1", inc.sub$win[i])
    if ( length(grep(pattern = tmp5, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp4, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp3, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp2, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp1, inc.sub$win.prior[i]))>0) {
        inc.sub$manipule[i] <- "Term-limited-p-won"
    } else {
        inc.sub$manipule[i] <- "Term-limited-p-lost"
    }
}
sel <- which(num==4) # subset coals with four members
for (i in sel){
    tmp4 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\4", inc.sub$win[i])
    tmp3 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\3", inc.sub$win[i])
    tmp2 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\2", inc.sub$win[i])
    tmp1 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\1", inc.sub$win[i])
    if ( length(grep(pattern = tmp4, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp3, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp2, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp1, inc.sub$win.prior[i]))>0) {
        inc.sub$manipule[i] <- "Term-limited-p-won"
    } else {
        inc.sub$manipule[i] <- "Term-limited-p-lost"
    }
}
sel <- which(num==3) # subset coals with three members
for (i in sel){
    tmp3 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\3", inc.sub$win[i])
    tmp2 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\2", inc.sub$win[i])
    tmp1 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\1", inc.sub$win[i])
    if ( length(grep(pattern = tmp3, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp2, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp1, inc.sub$win.prior[i]))>0) {
        inc.sub$manipule[i] <- "Term-limited-p-won"
    } else {
        inc.sub$manipule[i] <- "Term-limited-p-lost"
    }
}
sel <- which(num==2) # subset coals with two members
for (i in sel){
    tmp2 <- gsub("^([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\2", inc.sub$win[i])
    tmp1 <- gsub("^([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\1", inc.sub$win[i])
    if ( length(grep(pattern = tmp2, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp1, inc.sub$win.prior[i]))>0) {
        inc.sub$manipule[i] <- "Term-limited-p-won"
    } else {
        inc.sub$manipule[i] <- "Term-limited-p-lost"
    }
}
sel <- which(num==1) # subset coals with single member
for (i in sel){
    tmp1 <- gsub("^([a-z0-9]+)$", replacement = "\\1", inc.sub$win[i])
    if ( length(grep(pattern = tmp1, inc.sub$win.prior[i]))>0) {
        inc.sub$manipule[i] <- "Term-limited-p-won"
    } else {
        inc.sub$manipule[i] <- "Term-limited-p-lost"
    }
}
inc[sel.tl,] <- inc.sub # return to data
inc$race.prior <- inc$manipule # return to data 
inc$win <- duplic.win; inc$win.prior <- duplic.win.prior  # return unmanipulated winners
table(inc$race.prior[inc$win!="" & inc$win.prior!=""]) # check classification
rm(tmp, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, sel.tl, sel.e, sel.m, sel.7, duplic.win, duplic.win.prior)
#
## table(inc$manipule[inc$win!="" & inc$win.prior!=""]) # debug

table(inc$yr)                                   # debug
table(inc.sub$win2)                                   # debug
table(inc$win2, inc$race.prior)                                   # debug


inc.sub

## data.frame(inc.sub$win.prior[sel], inc.sub$win[sel]) # debug
## data.frame(inc.sub$win.prior[sel1], inc.sub$win[sel1]) # debug
#


#############################################
# classify create dummy for reelected party #
#############################################
inc$dptyReelected <- NA
#
duplic.win <- inc$win; duplic.win.prior <- inc$win.prior # duplicate
inc$win <- gsub("conve", "mc", inc$win)
inc$win.prior <- gsub("conve", "mc", inc$win.prior)
#
sel.sub <- which(inc$win!="" & inc$win.prior!="" & is.na(inc$win)==FALSE & is.na(inc$win.prior)==FALSE)
inc.sub <- inc[sel.sub,] # subset
# n coalition members
num <- gsub(pattern = "[^-]*", replacement = "", inc.sub$win, perl = TRUE) # keep hyphens only
num <- sapply(num, function(x) nchar(x)+1); names(num) <- NULL#n hyphens
table(num)
sel <- which(num==7) # subset coals with seven members
for (i in sel){
    tmp7 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\7", inc.sub$win[i])
    tmp6 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\6", inc.sub$win[i])
    tmp5 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\5", inc.sub$win[i])
    tmp4 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\4", inc.sub$win[i])
    tmp3 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\3", inc.sub$win[i])
    tmp2 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\2", inc.sub$win[i])
    tmp1 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\1", inc.sub$win[i])
    if ( length(grep(pattern = tmp7, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp6, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp5, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp4, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp3, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp2, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp1, inc.sub$win.prior[i]))>0) {
        inc.sub$dptyReelected[i] <- 1
    } else {
        inc.sub$dptyReelected[i] <- 0
    }
}
sel <- which(num==6) # subset coals with six members
for (i in sel){
    tmp6 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\6", inc.sub$win[i])
    tmp5 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\5", inc.sub$win[i])
    tmp4 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\4", inc.sub$win[i])
    tmp3 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\3", inc.sub$win[i])
    tmp2 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\2", inc.sub$win[i])
    tmp1 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\1", inc.sub$win[i])
    if ( length(grep(pattern = tmp6, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp5, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp4, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp3, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp2, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp1, inc.sub$win.prior[i]))>0) {
        inc.sub$dptyReelected[i] <- 1
    } else {
        inc.sub$dptyReelected[i] <- 0
    }
}
sel <- which(num==5) # subset coals with five members
for (i in sel){
    tmp5 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\5", inc.sub$win[i])
    tmp4 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\4", inc.sub$win[i])
    tmp3 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\3", inc.sub$win[i])
    tmp2 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\2", inc.sub$win[i])
    tmp1 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\1", inc.sub$win[i])
    if ( length(grep(pattern = tmp5, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp4, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp3, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp2, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp1, inc.sub$win.prior[i]))>0) {
        inc.sub$dptyReelected[i] <- 1
    } else {
        inc.sub$dptyReelected[i] <- 0
    }
}
sel <- which(num==4) # subset coals with four members
for (i in sel){
    tmp4 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\4", inc.sub$win[i])
    tmp3 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\3", inc.sub$win[i])
    tmp2 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\2", inc.sub$win[i])
    tmp1 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\1", inc.sub$win[i])
    if ( length(grep(pattern = tmp4, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp3, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp2, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp1, inc.sub$win.prior[i]))>0) {
        inc.sub$dptyReelected[i] <- 1
    } else {
        inc.sub$dptyReelected[i] <- 0
    }
}
sel <- which(num==3) # subset coals with three members
for (i in sel){
    tmp3 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\3", inc.sub$win[i])
    tmp2 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\2", inc.sub$win[i])
    tmp1 <- gsub("^([a-z0-9]+)-([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\1", inc.sub$win[i])
    if ( length(grep(pattern = tmp3, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp2, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp1, inc.sub$win.prior[i]))>0) {
        inc.sub$dptyReelected[i] <- 1
    } else {
        inc.sub$dptyReelected[i] <- 0
    }
}
sel <- which(num==2) # subset coals with two members
for (i in sel){
    tmp2 <- gsub("^([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\2", inc.sub$win[i])
    tmp1 <- gsub("^([a-z0-9]+)-([a-z0-9]+)$", replacement = "\\1", inc.sub$win[i])
    if ( length(grep(pattern = tmp2, inc.sub$win.prior[i]))>0 |
         length(grep(pattern = tmp1, inc.sub$win.prior[i]))>0) {
        inc.sub$dptyReelected[i] <- 1
    } else {
        inc.sub$dptyReelected[i] <- 0
    }
}
sel <- which(num==1) # subset coals with single member
for (i in sel){
    tmp1 <- gsub("^([a-z0-9]+)$", replacement = "\\1", inc.sub$win[i])
    if ( length(grep(pattern = tmp1, inc.sub$win.prior[i]))>0) {
        inc.sub$dptyReelected[i] <- 1
    } else {
        inc.sub$dptyReelected[i] <- 0
    }
}
inc[sel.sub,] <- inc.sub # return to data
inc$win <- duplic.win; inc$win.prior <- duplic.win.prior  # return unmanipulated winners
table(inc$dptyReelected[inc$win!="" & inc$win.prior!="" & is.na(inc$win)==FALSE & is.na(inc$win.prior)==FALSE], useNA = "always") # check classification
rm(tmp, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, sel.sub, sel.e, sel.m, sel.7, duplic.win, duplic.win.prior)
#
## table(inc$manipule[inc$win!="" & inc$win.prior!=""]) # debug
## table(inc.sub$win)                                   # debug
## data.frame(inc.sub$win.prior[sel], inc.sub$win[sel]) # debug
## data.frame(inc.sub$win.prior[sel1], inc.sub$win[sel1]) # debug

sel <- which(inc$win=="" & inc$win.prior=="" & inc$yr>=2000)

table(inc$dptyReelected[sel], useNA = "always") # check classification

inc[sel[2],]
data.frame(inc$win.prior[sel[1]], inc$win[sel[1]])
