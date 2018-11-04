rm(list = ls())
wd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
setwd(wd)

# alcaldes
inc <- read.csv(file = "aymu1985-present.incumbents.csv", stringsAsFactors = FALSE)

# change pt mc panal labels
sel <- grep("pt1", inc$win, ignore.case = TRUE)
inc$win[sel] <- "pt"
sel <- grep("pmc", inc$win, ignore.case = TRUE)
inc$win[sel] <- "mc"
sel <- grep("panal", inc$win, ignore.case = TRUE)
inc$win[sel] <- "pna"

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
sel <- grep("prd-", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "prd"
sel <- grep("morena-|-morena", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "morena"
sel <- grep("pvem-|-pvem", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "pvem"
sel <- grep("mc-|-mc", inc$win2, ignore.case = TRUE)
inc$win2[sel] <- "mc"
sel <- grep("mas|paz|pcp|phm|pmr|ppg|psd1|psi|pudc|pup|via_|pchu|pmch|pver|prs|prv|ps1|poc|pjs|pd1|pec|pasd|pac1|npp|pcu|pcdt|pmac|pcm2|pdm|pps|ppt|ph|pmp|fc1|pcd1|psn", inc$win2, ignore.case = TRUE)
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


# subset: cases allowing reelection in 2018
sel <- which(inc$yr==2018 & inc$edon!=9 & inc$edon!=21)
inc.sub <- inc[sel,]

sel <- which(inc.sub$race.prior=="pending")
sel
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


# classify term-limited cases according to party reelected/not
duplic.win <- inc$win; duplic.win.prior <- inc$win.prior # duplicate
inc$win <- gsub("conve", "mc", inc$win)
inc$win.prior <- gsub("conve", "mc", inc$win.prior)

inc$manipule <- inc$race.prior # debug
table(inc$manipule[inc$win!="" & inc$win.prior!=""]) # debug
# 
sel.tl <- which(inc$manipule=="Term-limited" & inc$win!="" & inc$win.prior!="")
inc.sub <- inc[sel.tl,] # subset
sel1 <- grep(pattern = "-", inc.sub$win.prior, invert = TRUE) # no coalition
for (i in sel1){
    #j <- j+1; i <- sel1[j] # debug
    tmp <- inc.sub$win.prior[i]
    tmp2 <- grep(pattern = tmp, inc.sub$win[i])
    if (length(tmp2)==0) {
        inc.sub$manipule[i] <- "Term-limited-p-lost"
    } else {
        inc.sub$manipule[i] <- "Term-limited-p-won"
    }
}
inc[sel.tl,] <- inc.sub # return to data
#
sel.tl <- which(inc$manipule=="Term-limited" & inc$win!="" & inc$win.prior!="")
inc.sub <- inc[sel.tl,] # subset
sel2 <- grep(pattern = "-", inc.sub$win, invert = TRUE) # no coalition
for (i in sel2){
    #i <- 1478 # debug
    tmp <- inc.sub$win[i]
    tmp2 <- grep(pattern = tmp, inc.sub$win.prior[i])
    if (length(tmp2)==0) {
        inc.sub$manipule[i] <- "Term-limited-p-lost"
    } else {
        inc.sub$manipule[i] <- "Term-limited-p-won"
    }
}
inc[sel.tl,] <- inc.sub # return to data
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

inc$win <- duplic.win; inc$win.prior <- duplic.win.prior  # return duplicates

#
# not pan nor pri nor prd in one, but all three in coalition
sel.tl <- which(inc$manipule=="Term-limited" & inc$win!="" & inc$win.prior!="")
inc.sub <- inc[sel.tl,] # subset
sel1 <- grep(pattern = "pan|pri|prd", inc.sub$win.prior)
sel2 <- grep(pattern = "pan-pri-prd|pan-prd-pri|pri-pan-prd|pri-prd-pan|prd-pan-pri|prd-pri-pan", inc.sub$win)
sel <- intersect(sel1, sel2)
inc.sub$manipule[sel] <- "Term-limited-p-won"
inc[sel.tl,] <- inc.sub # return to data
#
sel.tl <- which(inc$manipule=="Term-limited" & inc$win!="" & inc$win.prior!="")
inc.sub <- inc[sel.tl,] # subset
sel1 <- grep(pattern = "pan-pri-prd|pan-prd-pri|pri-pan-prd|pri-prd-pan|prd-pan-pri|prd-pri-pan", inc.sub$win.prior)
sel2 <- grep(pattern = "pan|pri|prd", inc.sub$win)
sel <- intersect(sel1, sel2)
sel <- intersect(sel1, sel2)
inc.sub$manipule[sel] <- "Term-limited-p-won"
inc[sel.tl,] <- inc.sub # return to data
#
sel.tl <- which(inc$manipule=="Term-limited" & inc$win!="" & inc$win.prior!="")
inc.sub <- inc[sel.tl,] # subset
sel1 <- grep(pattern = "pan", inc.sub$win.prior)
sel2 <- grep(pattern = "pan", inc.sub$win)
sel <- intersect(sel1, sel2)
inc.sub$manipule[sel] <- "Term-limited-p-won"
inc[sel.tl,] <- inc.sub # return to data
#
sel.tl <- which(inc$manipule=="Term-limited" & inc$win!="" & inc$win.prior!="")
inc.sub <- inc[sel.tl,] # subset
sel1 <- grep(pattern = "pri", inc.sub$win.prior)
sel2 <- grep(pattern = "pri", inc.sub$win)
sel <- intersect(sel1, sel2)
inc.sub$manipule[sel] <- "Term-limited-p-won"
inc[sel.tl,] <- inc.sub # return to data
#
sel.tl <- which(inc$manipule=="Term-limited" & inc$win!="" & inc$win.prior!="")
inc.sub <- inc[sel.tl,] # subset
sel1 <- grep(pattern = "prd", inc.sub$win.prior)
sel2 <- grep(pattern = "prd", inc.sub$win)
sel <- intersect(sel1, sel2)
inc.sub$manipule[sel] <- "Term-limited-p-won"
inc[sel.tl,] <- inc.sub # return to data
#
sel.tl <- which(inc$manipule=="Term-limited" & inc$win!="" & inc$win.prior!="")
inc.sub <- inc[sel.tl,] # subset
sel1 <- grep(pattern = "pvem", inc.sub$win.prior)
sel2 <- grep(pattern = "pvem", inc.sub$win)
sel <- intersect(sel1, sel2)
inc.sub$manipule[sel] <- "Term-limited-p-won"
inc[sel.tl,] <- inc.sub # return to data
#
sel.tl <- which(inc$manipule=="Term-limited" & inc$win!="" & inc$win.prior!="")
inc.sub <- inc[sel.tl,] # subset
sel1 <- grep(pattern = "mc|conve", inc.sub$win.prior)
sel2 <- grep(pattern = "mc|conve", inc.sub$win)
sel <- intersect(sel1, sel2)
inc.sub$manipule[sel] <- "Term-limited-p-won"
inc[sel.tl,] <- inc.sub # return to data
#
sel.tl <- which(inc$manipule=="Term-limited" & inc$win!="" & inc$win.prior!="")
inc.sub <- inc[sel.tl,] # subset
sel11 <- grep(pattern = "pan", inc.sub$win.prior, invert = TRUE)
sel12 <- grep(pattern = "pri", inc.sub$win.prior, invert = TRUE)
sel2 <- grep(pattern = "pan-pri|pri-pan", inc.sub$win)
sel <- Reduce(intersect, list(sel11,sel12,sel2))
inc.sub$manipule[sel] <- "Term-limited-p-lost"
inc[sel.tl,] <- inc.sub # return to data
#
sel.tl <- which(inc$manipule=="Term-limited" & inc$win!="" & inc$win.prior!="")
inc.sub <- inc[sel.tl,] # subset
sel1 <- grep(pattern = "pan-pri|pri-pan", inc.sub$win.prior)
sel21 <- grep(pattern = "pan", inc.sub$win, invert = TRUE)
sel22 <- grep(pattern = "pri", inc.sub$win, invert = TRUE)
sel <- Reduce(intersect, list(sel1,sel21,sel22))
inc.sub$manipule[sel] <- "Term-limited-p-lost"
inc[sel.tl,] <- inc.sub # return to data
#
sel.tl <- which(inc$manipule=="Term-limited" & inc$win!="" & inc$win.prior!="")
inc.sub <- inc[sel.tl,] # subset
sel2 <- grep(pattern = "^prd-pri$|^pri-prd$", inc.sub$win)
sel11 <- grep(pattern = "prd", inc.sub$win.prior, invert = TRUE)
sel12 <- grep(pattern = "pri", inc.sub$win.prior, invert = TRUE)
sel <- Reduce(intersect, list(sel11,sel12,sel2))
inc.sub$manipule[sel] <- "Term-limited-p-lost"
inc[sel.tl,] <- inc.sub # return to data
#
sel.tl <- which(inc$manipule=="Term-limited" & inc$win!="" & inc$win.prior!="")
inc.sub <- inc[sel.tl,] # subset
sel21 <- grep(pattern = "pan", inc.sub$win, invert = TRUE)
sel22 <- grep(pattern = "prd", inc.sub$win, invert = TRUE)
sel1 <- grep(pattern = "^pan-prd$", inc.sub$win.prior)
sel <- Reduce(intersect, list(sel1,sel21,sel22))
inc.sub$manipule[sel] <- "Term-limited-p-lost"
inc[sel.tl,] <- inc.sub # return to data
#
sel.tl <- which(inc$manipule=="Term-limited" & inc$win!="" & inc$win.prior!="")
inc.sub <- inc[sel.tl,] # subset
sel11 <- grep(pattern = "pan", inc.sub$win.prior, invert = TRUE)
sel12 <- grep(pattern = "prd", inc.sub$win.prior, invert = TRUE)
sel2 <- grep(pattern = "^pan-prd$", inc.sub$win)
sel <- Reduce(intersect, list(sel11,sel12,sel2))
inc.sub$manipule[sel] <- "Term-limited-p-lost"
inc[sel.tl,] <- inc.sub # return to data
#
sel.tl <- which(inc$manipule=="Term-limited" & inc$win!="" & inc$win.prior!="")
inc.sub <- inc[sel.tl,] # subset
sel21 <- grep(pattern = "pri", inc.sub$win, invert = TRUE)
sel22 <- grep(pattern = "pvem", inc.sub$win, invert = TRUE)
sel1 <- grep(pattern = "^pri-pvem$", inc.sub$win.prior)
sel <- Reduce(intersect, list(sel1,sel21,sel22))
inc.sub$manipule[sel] <- "Term-limited-p-lost"
inc[sel.tl,] <- inc.sub # return to data
#
sel.tl <- which(inc$manipule=="Term-limited" & inc$win!="" & inc$win.prior!="")
inc.sub <- inc[sel.tl,] # subset
sel11 <- grep(pattern = "pri", inc.sub$win.prior, invert = TRUE)
sel12 <- grep(pattern = "pvem", inc.sub$win.prior, invert = TRUE)
sel2 <- grep(pattern = "^pri-pvem$", inc.sub$win)
sel <- Reduce(intersect, list(sel11,sel12,sel2))
inc.sub$manipule[sel] <- "Term-limited-p-lost"
inc[sel.tl,] <- inc.sub # return to data
#

need grep that will pick all N>0 parties in one elec, 1-by-1, and look for each in other elec...

# ojo: should not recode false positives because other partners that returned have been captured above
sel.tl <- which(inc$manipule=="Term-limited" & inc$win!="" & inc$win.prior!="")
inc.sub <- inc[sel.tl,] # subset
sel2 <- grep(pattern = "pan-prd", inc.sub$win) # pan-prd and other(s)
sel11 <- grep(pattern = "pan", inc.sub$win.prior, invert = TRUE)
sel12 <- grep(pattern = "prd", inc.sub$win.prior, invert = TRUE)
sel <- Reduce(intersect, list(sel11,sel12,sel2))
inc.sub$manipule[sel] <- "Term-limited-p-lost"
inc[sel.tl,] <- inc.sub # return to data
#
# ojo: should not recode false positives because other partners that returned have been captured above
sel.tl <- which(inc$manipule=="Term-limited" & inc$win!="" & inc$win.prior!="")
inc.sub <- inc[sel.tl,] # subset
sel1 <- grep(pattern = "pan-prd", inc.sub$win.prior) # pan-prd and other(s)
sel21 <- grep(pattern = "pan", inc.sub$win, invert = TRUE)
sel22 <- grep(pattern = "prd", inc.sub$win, invert = TRUE)
sel <- Reduce(intersect, list(sel1,sel21,sel22))
inc.sub$manipule[sel] <- "Term-limited-p-lost"
inc[sel.tl,] <- inc.sub # return to data

#
# ojo: should not recode false positives because other partners that returned have been captured above
sel.tl <- which(inc$manipule=="Term-limited" & inc$win!="" & inc$win.prior!="")
inc.sub <- inc[sel.tl,] # subset
sel2 <- grep(pattern = "pri-pvem", inc.sub$win) # pri-pvem and other(s)
sel11 <- grep(pattern = "pri", inc.sub$win.prior, invert = TRUE)
sel12 <- grep(pattern = "pvem", inc.sub$win.prior, invert = TRUE)
sel <- Reduce(intersect, list(sel11,sel12,sel2))
inc.sub$manipule[sel] <- "Term-limited-p-lost"
inc[sel.tl,] <- inc.sub # return to data
#
# ojo: should not recode false positives because other partners that returned have been captured above
sel.tl <- which(inc$manipule=="Term-limited" & inc$win!="" & inc$win.prior!="")
inc.sub <- inc[sel.tl,] # subset
sel1 <- grep(pattern = "pri-pvem", inc.sub$win.prior) # pri-pvem and other(s)
sel21 <- grep(pattern = "pri", inc.sub$win, invert = TRUE)
sel22 <- grep(pattern = "pvem", inc.sub$win, invert = TRUE)
sel <- Reduce(intersect, list(sel1,sel21,sel22))
inc.sub$manipule[sel] <- "Term-limited-p-lost"
inc[sel.tl,] <- inc.sub # return to data


eric11
sel.tl <- which(inc$manipule=="Term-limited" & inc$win!="" & inc$win.prior!="")
inc.sub <- inc[sel.tl,] # subset
sel1 <- grep(pattern = "^pan$", inc.sub$win.prior)
sel2 <- grep(pattern = "^pan$", inc.sub$win)
sel <- intersect(sel1, sel2)
table(inc$manipule[inc$win!="" & inc$win.prior!=""]) # debug
table(inc.sub$win)                                   # debug
data.frame(inc.sub$win.prior[sel], inc.sub$win[sel]) # debug
data.frame(inc.sub$win.prior[sel1], inc.sub$win[sel1]) # debug
inc.sub$manipule[sel] <- "Term-limited-p-won"
inc[sel.tl,] <- inc.sub # return to data

data.frame(inc.sub$win.prior[sel1], inc.sub$win[sel1], inc.sub$manipule[sel1]) # verify


