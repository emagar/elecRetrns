rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
setwd(dd)

# read raw data file
dat <- read.csv(file = "dfdf1979-on.csv", stringsAsFactors = FALSE)
table(dat$yr, dat$dextra, useNA = "always")
dim(dat)

# drop void elections
sel <- which(dat$danul==1)
if (length(sel)>0){
    dat <- dat[-sel,]
    dat$danul <- NULL
}

# re-compute effective and total votes
sel <- grep("^v[0-9]{2}", colnames(dat))
v <- dat[,sel] # subset votes columns
v[is.na(v)==TRUE] <- 0 # replace NAs with zeroes
v[v==""|v=="NA"|v=="-"|v=="N/R"] <- 0 # replace with zeroes
for (i in 1:ncol(v)){
    v[,i] <- as.numeric(v[,i])
}
dat[,sel] <- v # return votes without missing to data
v$efec <- round(rowSums(v), 0)
dat$efec <- v$efec
#v$tot <- dat$efec + dat$nr + dat$nulos
#dat$tot <- v$tot
rm(v)

# verify that coalition member separator is always a - 
sel <- grep("^l[0-9]{2}", colnames(dat))
l <- dat[,sel] # subset label columns
for (i in 1:ncol(l)){
    #i <- 18 # debug
    tmp <- l[,i] # duplicate column for manipulation
    #table(gsub(pattern = "([0-9a-zA-Záéíóúñ])", replacement = "", tmp)) # keep only non-letter-number characters
    tmp <- gsub(pattern = "[ ]", replacement = "", tmp)
    tmp <- gsub(pattern = "[.]", replacement = "-", tmp)
    #sel1 <- grep("[ ]", tmp)
    #tmp[sel1]
    l[,i] <- tmp # return to data
}
dat[,sel] <- l # return to data
rm(l)

# any votes with no labels?
sel.l <- grep("^l[0-9]{2}", colnames(dat))
l <- dat[,sel.l] # subset label columns
sel.v <- grep("^v[0-9]{2}", colnames(dat))
v <- dat[,sel.v] # subset vote columns
for (i in 1:ncol(l)){
    #i <- 2 # debug
    sel <- which(v[,i]>0 & l[,i]=="0")
    if (length(sel)>0) print(paste("Warning!!!  i=", i, sep = ""))
    print(paste("column", i , "ok"))
}
rm(l,v)

# drop upper case from labels
sel.l <- grep("^l[0-9]{2}", colnames(dat))
l <- dat[,sel.l] # subset labels columns
for (i in 1:ncol(l)){
    l[,i] <- tolower(l[,i])
    }
dat[,sel.l] <- l # return to data
rm(l)

# remove useless labels
sel.l <- grep("^l[0-9]{2}", colnames(dat))
l <- dat[,sel.l] # subset label columns
sel.v <- grep("^v[0-9]{2}", colnames(dat))
v <- dat[,sel.v] # subset vote columns
for (i in 1:ncol(l)){
    #i <- 1 # debug
    sel <- which(v[,i]==0 & l[,i]!="0")
    l[sel,i] <- "0"
}
dat[,sel.l] <- l # return to data
rm(l,v)

# change confusing party labels
sel.l <- grep("^l[0-9]{2}", colnames(dat))
l <- dat[,sel.l] # subset labels columns
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^c$|^cd$|^cdppn$|^pc$|^conver(gencia)?$", replacement = "conve", l[,i])
    l[,i] <- gsub(pattern = "-c-|-cd-|-cdppn-|-pc-|-conver(gencia)?-", replacement = "-conve-", l[,i])
    l[,i] <- gsub(pattern = "-c$|-cd$|-cdppn$|-pc$|-conver(gencia)?$", replacement = "-conve", l[,i])
    }
#
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^mrn$", replacement = "morena", l[,i])
    l[,i] <- gsub(pattern = "-mrn-", replacement = "-morena-", l[,i])
    l[,i] <- gsub(pattern = "-mrn$", replacement = "-morena", l[,i])
    }
#
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^cc$", replacement = "cc1", l[,i])
    l[,i] <- gsub(pattern = "-cc-", replacement = "-cc1-", l[,i])
    l[,i] <- gsub(pattern = "-cc$", replacement = "-cc1", l[,i])
    }
#
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^pmc$", replacement = "mc", l[,i])
    l[,i] <- gsub(pattern = "-pmc-", replacement = "-mc-", l[,i])
    l[,i] <- gsub(pattern = "-pmc$", replacement = "-mc", l[,i])
    }
#
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^mp$", replacement = "mexpos", l[,i])
    l[,i] <- gsub(pattern = "-mp-", replacement = "-mexpos-", l[,i])
    l[,i] <- gsub(pattern = "-mp$", replacement = "-mexpos", l[,i])
    }
#
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^pd$", replacement = "pd1", l[,i])
    l[,i] <- gsub(pattern = "-pd-", replacement = "-pd1-", l[,i])
    l[,i] <- gsub(pattern = "-pd$", replacement = "-pd1", l[,i])
    }
#
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^pj$", replacement = "pj1", l[,i])
    l[,i] <- gsub(pattern = "-pj-", replacement = "-pj1-", l[,i])
    l[,i] <- gsub(pattern = "-pj$", replacement = "-pj1", l[,i])
    }
#
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^p?fc$|^fzaciud$|^fuerciud$", replacement = "fc1", l[,i])
    l[,i] <- gsub(pattern = "-p?fc-|-fzaciud-|-fuerciud-", replacement = "-fc1-", l[,i])
    l[,i] <- gsub(pattern = "-p?fc$|-fzaciud$|-fuerciud$", replacement = "-fc1", l[,i])
    }
#
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^p?asd?c?$|^alter(nativa)?$", replacement = "pasd", l[,i])
    l[,i] <- gsub(pattern = "-p?asd?c?-|-alter(nativa)?-", replacement = "-pasd-", l[,i])
    l[,i] <- gsub(pattern = "-p?asd?c?$|-alter(nativa)?$", replacement = "-pasd", l[,i])
    l[,i] <- gsub(pattern = "^p?asd?c?-|^alter(nativa)?-", replacement = "pasd-", l[,i])
    }
#
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^pl$", replacement = "pl1", l[,i])
    l[,i] <- gsub(pattern = "-pl-", replacement = "-pl1-", l[,i])
    l[,i] <- gsub(pattern = "-pl$", replacement = "-pl1", l[,i])
    }
#
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^pp$", replacement = "pp1", l[,i])
    l[,i] <- gsub(pattern = "-pp-", replacement = "-pp1-", l[,i])
    l[,i] <- gsub(pattern = "-pp$", replacement = "-pp1", l[,i])
    }
#
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^pr$", replacement = "pr1", l[,i])
    l[,i] <- gsub(pattern = "-pr-", replacement = "-pr1-", l[,i])
    l[,i] <- gsub(pattern = "-pr$", replacement = "-pr1", l[,i])
    }
#
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^ps$", replacement = "ps1", l[,i])
    l[,i] <- gsub(pattern = "-ps-", replacement = "-ps1-", l[,i])
    l[,i] <- gsub(pattern = "-ps$", replacement = "-ps1", l[,i])
    }
#
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^pt$", replacement = "pt1", l[,i])
    l[,i] <- gsub(pattern = "-pt-", replacement = "-pt1-", l[,i])
    l[,i] <- gsub(pattern = "-pt$", replacement = "-pt1", l[,i])
    l[,i] <- gsub(pattern = "^pt-", replacement = "pt1-", l[,i])
    }
#
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^pac$", replacement = "pac1", l[,i])
    l[,i] <- gsub(pattern = "-pac-", replacement = "-pac1-", l[,i])
    l[,i] <- gsub(pattern = "-pac$", replacement = "-pac1", l[,i])
    l[,i] <- gsub(pattern = "^pac-", replacement = "pac1-", l[,i])
    }
#
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^panal$", replacement = "pna", l[,i])
    l[,i] <- gsub(pattern = "-panal-", replacement = "-pna-", l[,i])
    l[,i] <- gsub(pattern = "-panal$", replacement = "-pna", l[,i])
    l[,i] <- gsub(pattern = "^panal-", replacement = "pna-", l[,i])
    }
#
# avoid "na" in csv, recognized as missing...
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^na$", replacement = "pna", l[,i])
    l[,i] <- gsub(pattern = "-na-", replacement = "-pna-", l[,i])
    l[,i] <- gsub(pattern = "-na$", replacement = "-pna", l[,i])
    l[,i] <- gsub(pattern = "^na-", replacement = "pna-", l[,i])
    }
#
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^pbc$", replacement = "ppbc", l[,i])
    l[,i] <- gsub(pattern = "-pbc-", replacement = "-ppbc-", l[,i])
    l[,i] <- gsub(pattern = "-pbc$", replacement = "-ppbc", l[,i])
    l[,i] <- gsub(pattern = "^pbc-", replacement = "ppbc-", l[,i])
    }
#
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^pcd$", replacement = "pcd1", l[,i])
    l[,i] <- gsub(pattern = "-pcd-", replacement = "-pcd1-", l[,i])
    l[,i] <- gsub(pattern = "-pcd$", replacement = "-pcd1", l[,i])
    l[,i] <- gsub(pattern = "^pcd-", replacement = "pcd1-", l[,i])
    }
#
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^pcm$", replacement = "pcm1", l[,i])
    l[,i] <- gsub(pattern = "-pcm-", replacement = "-pcm1-", l[,i])
    l[,i] <- gsub(pattern = "-pcm$", replacement = "-pcm1", l[,i])
    l[,i] <- gsub(pattern = "^pcm-", replacement = "pcm1-", l[,i])
    }
#
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^pds$", replacement = "pds1", l[,i])
    l[,i] <- gsub(pattern = "-pds-", replacement = "-pds1-", l[,i])
    l[,i] <- gsub(pattern = "-pds$", replacement = "-pds1", l[,i])
    l[,i] <- gsub(pattern = "^pds-", replacement = "pds1-", l[,i])
    }
#
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^pfd$", replacement = "pfd1", l[,i])
    l[,i] <- gsub(pattern = "-pfd-", replacement = "-pfd1-", l[,i])
    l[,i] <- gsub(pattern = "-pfd$", replacement = "-pfd1", l[,i])
    l[,i] <- gsub(pattern = "^pfd-", replacement = "pfd1-", l[,i])
    }
#
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^ppn$", replacement = "ppn1", l[,i])
    l[,i] <- gsub(pattern = "-ppn-", replacement = "-ppn1-", l[,i])
    l[,i] <- gsub(pattern = "-ppn$", replacement = "-ppn1", l[,i])
    l[,i] <- gsub(pattern = "^ppn-", replacement = "ppn1-", l[,i])
    }
#
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^psd$", replacement = "psd1", l[,i])
    l[,i] <- gsub(pattern = "-psd-", replacement = "-psd1-", l[,i])
    l[,i] <- gsub(pattern = "-psd$", replacement = "-psd1", l[,i])
    l[,i] <- gsub(pattern = "^psd-", replacement = "psd1-", l[,i])
    }
#
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^udc$", replacement = "pudc", l[,i])
    l[,i] <- gsub(pattern = "-udc-", replacement = "-pudc-", l[,i])
    l[,i] <- gsub(pattern = "-udc$", replacement = "-pudc", l[,i])
    l[,i] <- gsub(pattern = "^udc-", replacement = "pudc-", l[,i])
    }
#
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^mrps$", replacement = "pmrps", l[,i])
    l[,i] <- gsub(pattern = "-mrps-", replacement = "-pmrps-", l[,i])
    l[,i] <- gsub(pattern = "-mrps$", replacement = "-pmrps", l[,i])
    l[,i] <- gsub(pattern = "^mrps-", replacement = "pmrps-", l[,i])
    }
#
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^otro$", replacement = "otros", l[,i])
    l[,i] <- gsub(pattern = "-otro-", replacement = "-otros-", l[,i])
    l[,i] <- gsub(pattern = "-otro$", replacement = "-otros", l[,i])
    l[,i] <- gsub(pattern = "^otro-", replacement = "otros-", l[,i])
    }
#
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^parme.$", replacement = "parm", l[,i])
    l[,i] <- gsub(pattern = "-parme.-", replacement = "-parm-", l[,i])
    l[,i] <- gsub(pattern = "-parme.$", replacement = "-parm", l[,i])
    l[,i] <- gsub(pattern = "^parme.-", replacement = "parm-", l[,i])
    }
#
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^indep$", replacement = "indep1", l[,i])
    l[,i] <- gsub(pattern = "-indep-", replacement = "-indep1-", l[,i])
    l[,i] <- gsub(pattern = "-indep$", replacement = "-indep1", l[,i])
    l[,i] <- gsub(pattern = "^indep-", replacement = "indep1-", l[,i])
    }
#
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^alianza$", replacement = "alianza1", l[,i])
    l[,i] <- gsub(pattern = "-alianza-", replacement = "-alianza1-", l[,i])
    l[,i] <- gsub(pattern = "-alianza$", replacement = "-alianza1", l[,i])
    l[,i] <- gsub(pattern = "^alianza-", replacement = "alianza1-", l[,i])
    }
#
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^es$", replacement = "pes", l[,i])
    l[,i] <- gsub(pattern = "-es-", replacement = "-pes-", l[,i])
    l[,i] <- gsub(pattern = "-es$", replacement = "-pes", l[,i])
    l[,i] <- gsub(pattern = "^es-", replacement = "pes-", l[,i])
    }
#
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^pmac$", replacement = "pmch", l[,i])
    l[,i] <- gsub(pattern = "-pmac-", replacement = "-pmch-", l[,i])
    l[,i] <- gsub(pattern = "-pmac$", replacement = "-pmch", l[,i])
    l[,i] <- gsub(pattern = "^pmac-", replacement = "pmch-", l[,i])
    }
#
#
sel.r <- which(dat$yr>2006)
#table(l[sel.r,18]) # easier to verify one column at a time
dat[,sel.l] <- l # return to data
rm(l)

## # shorter pty labels included in longer ones (eg pan panal) give false positives in regexes
## # figure which and change them
## sel.l <- grep("^l[0-9]{2}", colnames(dat))
## l <- dat[,sel.l] # subset label columns
## lab.tmp <- c(t(l)) # vectorize
## lab.tmp <- lab.tmp[lab.tmp!="0"] # drop empty cells
## lab.tmp <- unique(lab.tmp)
## lab.tmp <- strsplit(lab.tmp, split = "-") # break into component character vector
## lab.tmp <- unlist(lab.tmp)
## lab.tmp <- unique(lab.tmp)
## lab.tmp <- lab.tmp[lab.tmp!="0"] # drop empty cells
## lab.tmp <- lab.tmp[order(lab.tmp)]        # sort
## lab.tmp <- lab.tmp[order(nchar(lab.tmp))] # put shorter labels first
## # verify that all labels unique
## i <- 1
## i <- i+1; grep(lab.tmp[i], lab.tmp)
## lab.tmp[grep(lab.tmp[i], lab.tmp)]
## x



######################################################
######################################################
# identify coalitions by searching for "-" in labels #
######################################################
######################################################
sel.l <- grep("^l[0-9]{2}", colnames(dat))
l <- dat[,sel.l] # subset label columns
sel.v <- grep("^v[0-9]{2}", colnames(dat))
v <- dat[,sel.v] # subset vote columns
#
# dummy had at least one coalition
dat$dcoal <- 0
for (i in 1:ncol(l)){
    dat$dcoal[grep("-", l[,i])] <- 1
}

######################################
## Duplicate for split-vote version ##
######################################
dat.split <- dat


# create objects with vote for coalition(s) added and redudant columns dropped
cv <- v; # will receive votes with coalitions aggregated
cl <- l; # will keep coalition labels but drop coalition member member labels 
# create "split" objects for votes contrinuted by each coalition member and joint column dropped
sv <- v;
sl <- l;
##
ci <- data.frame(dcoal=dat$dcoal, ncoal=NA, coal1="none", coal2="none", coal3="none", coal4="none", stringsAsFactors = FALSE) # coalition summary info
ci$ncoal[ci$dcoal==0] <- 0 # 0=no coalition
# ci$coal1 coal2 coal3 coal4 pre-filled
  # n is object reporting how many parties reported in a v/l cell?
  ln1 <- v # duplicate votes
  ln1[] <- 0 # will receive info
  ln2 <- ln1 # duplicate
  # replace each label with its character length
  for (i in 1:ncol(ln1)){
      #i <- 16 # debug
      tmp <- l[,i] # pick ith column as vector
      ln1[,i] <- nchar(tmp) # replace items with num characters
  }
  # drop hyphens to count character difference
  l.tmp <- l # duplicate
  for (i in 1:ncol(l.tmp)){
      #i <- 1 # debug
      l.tmp[,i] <- gsub("-", "", l.tmp[,i]) # remove hyphens to count char dif
      ln2[,i] <- nchar(l.tmp[,i]) # replace items with num characters
  }
  n <- ln1 - ln2 + 1 # 1 means no coalition, single party
  n[l=="0"] <- 0     # empty votes
  rm(i, ln1, ln2, l.tmp, sel, tmp)
##
max.tmp <- apply(n, 1, max) # max parties reported in a row's cell
table(max.tmp) # coal w most members has 7
I <- nrow(v)
c1 <- as.data.frame(matrix("0", I, 7), col.names = paste("p", 1:7, sep = ""), stringsAsFactors = FALSE) # will receive vector of 1st coalition's members
c2 <- as.data.frame(matrix("0", I, 7), col.names = paste("p", 1:7, sep = ""), stringsAsFactors = FALSE) # 2nd coalition's members
c3 <- as.data.frame(matrix("0", I, 7), col.names = paste("p", 1:7, sep = ""), stringsAsFactors = FALSE) # 3rd coalition's members
c4 <- as.data.frame(matrix("0", I, 7), col.names = paste("p", 1:7, sep = ""), stringsAsFactors = FALSE) # 4th coalition's members
##
## will receive columns corresponding to coalition members in v/l for use when weighting votes contributed by each member
w1 <- as.list(rep("noCoal",I))
w2 <- as.list(rep("noCoal",I))
w3 <- as.list(rep("noCoal",I))
w4 <- as.list(rep("noCoal",I))
##
## ## fill in easy cases with no coalition --- 4jun2023 no longer needed with new procedure
## sel <- which(dat$dcoal==0)
## cv[sel,] <- v[sel,]
## cl[sel,] <- l[sel,]
## # c1 c2 c3 c4 are pre-filled
## # w1 w2 w3 w4 are pre-filled

# fill in info selecting cases of coalitions with most members 
sel7 <- which(max.tmp>1) 
tmp.v  <- cv[sel7,] # subset for manipulation --- 4jun2023 used to pick from v, forgetting manip in later rounds
tmp.vw  <- sv[sel7,] # will receive votes contributed by each coalition member
tmp.l   <- cl[sel7,]
tmp.lw  <- sl[sel7,] # will receive labels of vote-contributing parties
tmp.n   <- n[sel7,]
tmp.c1  <- c1[sel7,]
tmp.w1  <- w1[sel7] # will receive indices to manipulate
tmp.vw1 <- w1[sel7] # will receive manipulated votes
max.tmp <- max.tmp[sel7]
tmp.ci <- ci[sel7,]

# debug
#i <- which(dat$ord[sel7]==847)

for (i in 1:length(sel7)){
    #i <- length(sel7) # debug
    #i <- 50 # debug
    #tmp.l[i,]
    message(sprintf("loop %s of %s", i, length(sel7)))
    tmp.ci$ncoal[i] <- 1               # running tally ncoal
    save.col <- which(tmp.n[i,]==max.tmp[i])[1] # spare this column from erasure in process below (1st if multiple hits)
    save.label <- tmp.l[i, save.col]            # keep coalition full label
    tmp.ci$coal1[i] <- save.label      # plug coal label into summary
    tmp <- strsplit(save.label, split = "-") # break into component character vector
    tmp.c1[i,1:length(tmp[[1]])] <- tmp[[1]] # fill in coalition members
    #
    target.cols <- numeric()           # initialize empty vector for vote sum
    for (j in 1:7){
        #j <- 1 # debug
        if (tmp.c1[i,j]=="0") next
        pat <- paste("^", tmp.c1[i,j], "$|", tmp.c1[i,j], "-|-", tmp.c1[i,j], sep="") # searches ^pty$, pty- or -pty (avoids panal hit when searching for pan) --- reduntant given label changes
        tmp.target <- grep(pattern = pat, x = tmp.l[i,])
        #tmp.target <- grep(pattern = tmp.c1[i,j], x = tmp.l[i,]) # version hits panal when searching pan
        if (length(tmp.target)>0) {
            target.cols <- c(target.cols, tmp.target)
        }
    }
    target.cols <- unique(target.cols); target.cols <- target.cols[order(target.cols)]
    tmp.w1[[i]] <- target.cols # cols to use when replacing coalition members' contributed votes
    sel <- setdiff(target.cols, save.col)  # setdiffs joint votes
    if (length(sel)==0) sel <- target.cols # or all if singleton (ie setdiff empty)
    tmp.vw1[[i]] <- tmp.v[i, sel] # keep to fill coalition members' contribution 
    save.vote <- sum(tmp.v[i,target.cols])
    tmp.vw1[[i]] <- tmp.vw1[[i]] / sum(tmp.v[i,sel])   # relative contributions
    tmp.vw1[[i]] <- round(tmp.vw1[[i]] * save.vote, 2) # relative votes contributed
    if (length(sel)>1){ # do not manipulate if singleton
        tmp.v[i, target.cols] <- 0   # erase votes to keep only aggregate
        tmp.l[i, target.cols] <- "0" # erase labels to keep only full coalition label
        tmp.v[i, save.col] <- save.vote  # place aggregate vote back in
        tmp.l[i, save.col] <- save.label # place coalition label back in
        # votes contributed
        tmp.vw[i, save.col] <- 0   # erase joint votes
        tmp.lw[i, save.col] <- "0" # erase joint label
        tmp.vw[i, sel] <- tmp.vw1[[i]]  # place contributed votes back in
    }
    tmp.n[i, target.cols] <- 0   # erase ns, prep for obs's next coalition
}
# return to data
cv[sel7,] <- tmp.v
cl[sel7,] <- tmp.l
n[sel7,] <- tmp.n
c1[sel7,] <- tmp.c1
w1[sel7] <- tmp.w1
ci[sel7,] <- tmp.ci
sv[sel7,] <- tmp.vw
sl[sel7,] <- tmp.lw
#tail(w1[sel7])

####################################
# process cases with 2nd coalition #
####################################
max.tmp <- apply(n, 1, max) # max parties reported in a row's cell
table(max.tmp) # coal w most members has 7
sel7 <- which(max.tmp>1) 
#grep("ags-18", dat$emm[sel7]) # debug
tmp.v   <- cv[sel7,] # subset for manipulation --- 3sep2021 used to pick from v, forgetting manip in later rounds
tmp.vw  <- sv[sel7,] # will receive votes contributed by each coalition member
tmp.l   <- cl[sel7,]
tmp.lw  <- sl[sel7,] # will receive labels of vote-contributing parties
tmp.n   <- n[sel7,]
tmp.c2  <- c2[sel7,]
tmp.w2  <- w2[sel7]
tmp.vw2 <- w2[sel7] # will receive manipulated votes
max.tmp <- max.tmp[sel7]
tmp.ci  <- ci[sel7,]

# debug
#i <- which(dat$ord[sel7]==847)

for (i in 1:length(sel7)){
    #i <- 3079 # debug
    tmp.l[i,] # debug
    #tmp.n[i,] # debug
    message(sprintf("loop %s of %s", i, length(sel7)))
    tmp.ci$ncoal[i] <- 2               # running tally co ncoal
    save.col <- which(tmp.n[i,]==max.tmp[i])[1] # spare this column from erasure in process below (1st if multiple hits)
    save.label <- tmp.l[i, save.col]            # keep coalition full label
    tmp.ci$coal2[i] <- save.label      # plug coal label into summary
    tmp <- strsplit(save.label, split = "-") # break into component character vector
    tmp.c2[i,1:length(tmp[[1]])] <- tmp[[1]] # fill in coalition members
    #
    target.cols <- numeric() # initialize empty vector
    for (j in 1:7){
        #j <- 2 # debug
        if (tmp.c2[i,j]=="0") next
        pat <- paste("^", tmp.c2[i,j], "$|", tmp.c2[i,j], "-|-", tmp.c2[i,j], sep="") # searches ^pty$, pty- or -pty (avoids panal hit when searching for pan) --- unnecessary given label changes
        tmp.target <- grep(pattern = pat, x = tmp.l[i,])
        #tmp.target <- grep(pattern = tmp.c2[i,j], x = tmp.l[i,]) # version hits panal when searching pan
        if (length(tmp.target)>0) {
            target.cols <- c(target.cols, tmp.target)
        }
    }
    target.cols <- unique(target.cols); target.cols <- target.cols[order(target.cols)]
    tmp.w2[[i]] <- target.cols # for use when computing coalition members' contribution
    sel <- setdiff(target.cols, save.col)  # setdiffs joint votes
    if (length(sel)==0) sel <- target.cols # or all if singleton
    tmp.vw2[[i]] <- tmp.v[i, sel] # keep to fill coalition members' contribution (setdiffs joint votes)
    save.vote <- sum(tmp.v[i,target.cols])
    tmp.vw2[[i]] <- tmp.vw2[[i]] / sum(tmp.v[i,sel])   # relative contributions
    tmp.vw2[[i]] <- round(tmp.vw2[[i]] * save.vote, 2) # relative votes contributed
    if (length(sel)>1){ # do not manipulate if singleton
        tmp.v[i, target.cols] <- 0   # erase votes to keep only aggregate
        tmp.l[i, target.cols] <- "0" # erase labels to keep only full coalition label
        tmp.v[i, save.col] <- save.vote  # place aggregate vote back in
        tmp.l[i, save.col] <- save.label # place coalition label back in
        # votes contributed
        tmp.vw[i, save.col] <- 0   # erase joint votes
        tmp.lw[i, save.col] <- "0" # erase joint label
        tmp.vw[i, sel] <- tmp.vw2[[i]]  # place contributed votes back in
    }
    tmp.n[i, target.cols] <- 0   # erase ns
}
# return to data
cv[sel7,] <- tmp.v
cl[sel7,] <- tmp.l
n[sel7,] <- tmp.n
c2[sel7,] <- tmp.c2
w2[sel7] <- tmp.w2
ci[sel7,] <- tmp.ci
sv[sel7,] <- tmp.vw
sl[sel7,] <- tmp.lw
# tail(w2[sel7])

####################################
# process cases with 3rd coalition #
####################################
max.tmp <- apply(n, 1, max) # max parties reported in a row's cell
table(max.tmp) # coal w most members has 7
sel7 <- which(max.tmp>1) 
tmp.v  <- cv[sel7,] # subset for manipulation --- 3sep2021 used to pick from v, forgetting manip in later rounds
tmp.vw <- sv[sel7,] # will receive votes contributed by each coalition member
tmp.l  <- cl[sel7,]
tmp.lw <- sl[sel7,] # will receive labels of vote-contributing parties
tmp.n <- n[sel7,]
tmp.c3 <- c3[sel7,]
tmp.w3 <- w3[sel7]
tmp.vw3 <- w1[sel7] # will receive manipulated votes
max.tmp <- max.tmp[sel7]
tmp.ci <- ci[sel7,]

for (i in 1:length(sel7)){
    #i <- 1 # debug
    #tmp.l[i,] # debug
    #tmp.n[i,] # debug
    message(sprintf("loop %s of %s", i, length(sel7)))
    tmp.ci$ncoal[i] <- 3               # running tally co ncoal
    save.col <- which(tmp.n[i,]==max.tmp[i])[1] # spare this column from erasure in process below (1st if multiple hits)
    save.label <- tmp.l[i, save.col]            # keep coalition full label
    tmp.ci$coal3[i] <- save.label      # plug coal label into summary
    tmp <- strsplit(save.label, split = "-") # break into component character vector
    tmp.c3[i,1:length(tmp[[1]])] <- tmp[[1]] # fill in coalition members
    #
    target.cols <- numeric() # initialize empty vector
    for (j in 1:7){
        #j <- 2 # debug
        if (tmp.c3[i,j]=="0") next
        pat <- paste("^", tmp.c3[i,j], "$|", tmp.c3[i,j], "-|-", tmp.c3[i,j], sep="") # searches ^pty$, pty- or -pty (avoids panal hit when searching for pan) --- unnecessary given label changes
        tmp.target <- grep(pattern = pat, x = tmp.l[i,])
        #tmp.target <- grep(pattern = tmp.c3[i,j], x = tmp.l[i,]) # version hits panal when searching pan
        if (length(tmp.target)>0) {
            target.cols <- c(target.cols, tmp.target)
        }
    }
    target.cols <- unique(target.cols); target.cols <- target.cols[order(target.cols)]
    tmp.w3[[i]] <- target.cols # for use when computing coalition members' contribution
    sel <- setdiff(target.cols, save.col)  # setdiffs joint votes
    if (length(sel)==0) sel <- target.cols # or all if singleton
    tmp.vw3[[i]] <- tmp.v[i, sel] # keep to fill coalition members' contribution (setdiffs joint votes)
    save.vote <- sum(tmp.v[i,target.cols])
    tmp.vw3[[i]] <- tmp.vw3[[i]] / sum(tmp.v[i,sel])   # relative contributions
    tmp.vw3[[i]] <- round(tmp.vw3[[i]] * save.vote, 2) # relative votes contributed
    if (length(sel)>1){ # do not manipulate if singleton
        tmp.v[i, target.cols] <- 0   # erase votes to keep only aggregate
        tmp.l[i, target.cols] <- "0" # erase labels to keep only full coalition label
        tmp.v[i, save.col] <- save.vote  # place aggregate vote back in
        tmp.l[i, save.col] <- save.label # place coalition label back in
        # votes contributed
        tmp.vw[i, save.col] <- 0   # erase joint votes
        tmp.lw[i, save.col] <- "0" # erase joint label
        tmp.vw[i, sel] <- tmp.vw3[[i]]  # place contributed votes back in
    }
    tmp.n[i, target.cols] <- 0   # erase ns
}
# return to data
cv[sel7,] <- tmp.v
cl[sel7,] <- tmp.l
n[sel7,]  <- tmp.n
c3[sel7,] <- tmp.c3
w3[sel7]  <- tmp.w3
ci[sel7,] <- tmp.ci
sv[sel7,] <- tmp.vw
sl[sel7,] <- tmp.lw
# tail(w3[sel7])

max.tmp <- apply(n, 1, max) # max parties reported in a row's cell
print("Table must have 0s and 1s only, else coalitions remain to manipulate")
table(max.tmp) # must have 0s and 1s only (number of parties being reported by remaining columns)

## # debug
## sel <- which(max.tmp==2)
## cv[sel[2],]
## cl[sel[2],]
## sv[sel[2],]
## sl[sel[2],]
## dat[sel[2],]
## dat$emm[sel]
## x

# plug ncoal into data
dat$ncoal  <- ci$ncoal

## prepare object with coalition party weights
w <- as.list(rep("noCoal",I))
sel <- which(ci$ncoal>=1)
for (i in sel){
    w[[i]] <- list(coal1="no", coal2="no", coal3="no", coal4="no")
    #i <- sel[1] # debug
    w[[i]]$coal1 <- v[i,w1[[i]]]
    names(w[[i]]$coal1) <- l[i,w1[[i]]]
}
#
sel <- which(ci$ncoal>=2)
for (i in sel){
    #i <- sel[1] # debug
    w[[i]]$coal2 <- v[i,w2[[i]]]
    names(w[[i]]$coal2) <- l[i,w2[[i]]]
}
#
sel <- which(ci$ncoal>=3)
for (i in sel){
    #i <- sel[1] # debug
    w[[i]]$coal3 <- v[i,w3[[i]]]
    names(w[[i]]$coal3) <- l[i,w3[[i]]]
}
#
coal.weights <- w # rename
coal.info <- ci
rm(w, ci)

rm(c1, c2, c3, c4, I, i, j, max.tmp, n, pat, save.col, save.label, save.vote, sel, sel7, target.cols, tmp, tmp.c1, tmp.c2, tmp.c3, tmp.ci, tmp.l, tmp.n, tmp.target, tmp.v, tmp.w1, tmp.w2, tmp.w3, w1, w2, w3, w4, tmp.lw, tmp.vw, tmp.vw1, tmp.vw2, tmp.vw3) # housecleaning


## winner (sorts data to have largest vote-winning party in column 1)
# handy function to sort one data frame's rows by order of another, matching data frame
# if my machine use scripts in disk
pth <- ifelse (Sys.info()["user"] %in% c("eric", "magar"),
    "~/Dropbox/data/useful-functions",
    "https://raw.githubusercontent.com/emagar/useful-functions/master"
    )
# Reads sortBy function
source( paste(pth, "sortBy.r", sep = "/") )
rm(pth)

## # debug
## grep("ags-18", dat$emm)
## sl[156,]
## x

# sort coalition-aggregated data
tail(cv)
tail(cl)
###########################################
cv.sorted <- sortBy(target = cv, By = cv) # slow! better wait for process end before continuing  
###########################################
cl.sorted <- sortBy(target = cl, By = cv) # slow! better wait for process end before continuing
###########################################
cv.sorted <- as.data.frame(cv.sorted, stringsAsFactors = FALSE) # return matrix to dataframe
cl.sorted <- as.data.frame(cl.sorted, stringsAsFactors = FALSE) # return matrix to dataframe
colnames(cv.sorted) <- colnames(v); colnames(cl.sorted) <- colnames(l)
cv.sorted <- transform(cv.sorted, v01 = as.numeric(v01), v02 = as.numeric(v02), v03 = as.numeric(v03), v04 = as.numeric(v04), v05 = as.numeric(v05), v06 = as.numeric(v06), v07 = as.numeric(v07), v08 = as.numeric(v08), v09 = as.numeric(v09), v10 = as.numeric(v10), v11 = as.numeric(v11), v12 = as.numeric(v12), v13 = as.numeric(v13), v14 = as.numeric(v14)) # return to numeric format
tail(cv.sorted)
tail(cl.sorted)

# rename objects so that dat now has coalition aggregates
dat.orig <- dat # duplicate original data
dat[,sel.l] <- cl.sorted # return manipulated labels to data
dat[,sel.v] <- cv.sorted # return manipulated votes to data
head(dat)
# prepare coalition-split object for export
dat.split <- dat.orig
dat.split[,sel.l] <- sl # return manipulated labels to data
dat.split[,sel.v] <- sv # return manipulated votes to data
# move dcoal and ncoal columns before v01
tmp <- dat # duplicate if I mess up
tmp1 <- grep("v01", colnames(dat))
tmp2 <- grep("dcoal", colnames(dat))
tmp3 <- grep("ncoal", colnames(dat))
dat <- dat[, c(1:(tmp1-1), tmp2, tmp3, tmp1:(tmp2-1))]
colnames(dat)
dat[1,]
rm(sv, sl, cv, cl, cv.sorted, cl.sorted, sel.l, sel.v, v, l, tmp, tmp1, tmp2, tmp3)

table(dat$v12) # check that only has zeroes
dat$v12 <- dat$l12 <- dat$v13 <- dat$l13 <- dat$v14 <- dat$l14 <- NULL # redundant columns
dat$win <- dat$l01
# move win column before v01
tmp <- dat # duplicate if I mess up
tmp1 <- grep("v01", colnames(dat))
tmp2 <- grep("win", colnames(dat))
dat <- dat[, c(1:(tmp1-1), tmp2, tmp1:(tmp2-1))]
colnames(dat)

sel.l <- grep("^l[0-9]{2}", colnames(dat))
l <- dat[,sel.l] # subset label columns
sel.v <- grep("^v[0-9]{2}", colnames(dat))
v <- dat[,sel.v] # subset vote columns
dat$ncand <- apply(v, 1, function(x) length(x[x>0]))
# check that coal aggregation produces same efec as before
check <- rowSums(v) - dat$efec
table(check==0) # all must be true
## sel <- which(check!=0)
## dat[sel[2],]
## dat.orig[sel[2],]
# move ncand column before dcoal
tmp <- dat # duplicate if I mess up
tmp1 <- grep("dcoal", colnames(dat))
tmp2 <- grep("ncand", colnames(dat))
dat <- dat[, c(1:(tmp1-1), tmp2, tmp1:(tmp2-1))]
colnames(dat)

# sort columns
pth <- ifelse (Sys.info()["user"] %in% c("eric", "magar"),
    "~/Dropbox/data/useful-functions",
    "https://raw.githubusercontent.com/emagar/useful-functions/master"
    )
# Reads function
source( paste(pth, "moveme.r", sep = "/") )
source( paste(pth, "notin.r", sep = "/") )
rm(pth)
colnames(dat)
dat <- dat[moveme(names(dat), "efec before nr")]


################################
### export a coalAgg version ###
################################
#sel <- dat$yr>=1985
#dat <- dat[sel,] # drop early years
dat[1,]
dat$ord <- 1:nrow(dat)
dat <- within(dat, nota <- fuente <- NULL)
write.csv(dat, file = "dfdf1979-present.coalAgg.csv", row.names = FALSE)

nrow(dat) * ncol(dat) < 400000

#######################################
## Manipulate for split-vote version ##
#######################################
dat.split$ncand <- dat$ncand # import from manipulated file
dat.split[1,]

##########################################################################################
## possible to manipulate coalitions remaining in split vote object by hand (see ay.r)  ##
## these should all be cases where no vote split is available                           ##
##########################################################################################
sel.l <- grep("^l[0-9]{2}", colnames(dat.split))
l <- dat.split[,sel.l] # subset label columns
sel.v <- grep("^v[0-9]{2}", colnames(dat.split))
v <- dat.split[,sel.v] # subset vote columns
# need to manipulate indicator: dummy had at least one coalition
dat.split$dhascoal <- rep(0, nrow(dat.split))
for (i in 1:ncol(l)){
    dat.split$dhascoal[grep("-", l[,i])] <- 1
}
table(dat.split$dhascoal)
#
# drop little-use columns
dat.split[1,]
dat.split <- within(dat.split,
                    dhascoal <- ord <- fuente <- nota <- tot <- nr <- nul <- NULL)
table(dat.split$l14)
write.csv(dat.split, file = "dfdf1979-present.coalSplit.csv", row.names = FALSE)

nrow(dat.split) * ncol(dat.split) < 400000
