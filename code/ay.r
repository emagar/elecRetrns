rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
setwd(dd)

# read raw data file
dat <- read.csv(file = "aymu1977-present.csv", stringsAsFactors = FALSE)
dim(dat)

#########################################################################
#########################################################################
##  script ayClean.r has routines used once to save a cleaner dataset  ##
#########################################################################
#########################################################################

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
tmp <- dat$nr # remove NAs
tmp[is.na(tmp)] <- 0
dat$nr <- tmp
tmp <- as.numeric(dat$nulos)  # make numeric & remove NAs
tmp[is.na(tmp)] <- 0
dat$nulos <- tmp
v$tot <- dat$efec + dat$nr + dat$nulos
dat$tot <- v$tot
rm(v, tmp)

# verify that coalition member separator is always a - 
sel <- grep("^l[0-9]{2}", colnames(dat))
l <- dat[,sel] # subset label columns
for (i in 1:ncol(l)){
    #i <- 18 # debug
    tmp <- l[,i] # duplicate column for manipulation
    #table(gsub(pattern = "([0-9a-zA-Záéíóúñ])", replacement = "", tmp)) # keep only non-letter-number characters
    tmp <- gsub(pattern = "[ ]", replacement = "", tmp)
    tmp <- gsub(pattern = "[.,]", replacement = "-", tmp)
    #sel1 <- grep("[ ]", tmp)
    #tmp[sel1]
    l[,i] <- tmp # return to data
}
dat[,sel] <- l # return to data
rm(l)

## # replace NAs with zeroes (no longer needed)
## sel.v <- grep("^v[0-9]{2}", colnames(dat))
## v <- dat[,sel.v] # subset vote columns
## v[is.na(v)] <- 0
## dat[,sel.v] <- v # return to data
## rm(v)

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
    l[,i] <- gsub(pattern = "^mc$", replacement = "pmc", l[,i])
    l[,i] <- gsub(pattern = "-mc-", replacement = "-pmc-", l[,i])
    l[,i] <- gsub(pattern = "-mc$", replacement = "-pmc", l[,i])
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

save.image(file = "tmp.RData")

## consider running script above to make sure data is up-to-date with latest csv
rm(list = ls())
dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
setwd(dd)
load(file = "tmp.RData")


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

# create objects with vote for coalition(s) added and redudant columns dropped
cv <- v; cv[] <- NA # will receive votes with coalitions aggregated
cl <- l; cl[] <- NA # will keep coalition labels but drop coalition member labels 
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
#
max.tmp <- apply(n, 1, max) # max parties reported in a row's cell
table(max.tmp) # coal w most members has 7
I <- nrow(v)
c1 <- as.data.frame(matrix("0", I, 7), col.names = paste("p", 1:7, sep = ""), stringsAsFactors = FALSE) # will receive vector of 1st coalition's members
c2 <- as.data.frame(matrix("0", I, 7), col.names = paste("p", 1:7, sep = ""), stringsAsFactors = FALSE) # 2nd coalition's members
c3 <- as.data.frame(matrix("0", I, 7), col.names = paste("p", 1:7, sep = ""), stringsAsFactors = FALSE) # 3rd coalition's members
c4 <- as.data.frame(matrix("0", I, 7), col.names = paste("p", 1:7, sep = ""), stringsAsFactors = FALSE) # 3rd coalition's members
#
# will receive columns corresponding to coalition members in v/l for use when weighting votes contributed by each member
w1 <- as.list(rep("noCoal",I))
w2 <- as.list(rep("noCoal",I))
w3 <- as.list(rep("noCoal",I))
w4 <- as.list(rep("noCoal",I))
#
# fill in easy cases with no coalition
sel <- which(dat$dcoal==0)
cv[sel,] <- v[sel,]
cl[sel,] <- l[sel,]
# c1 c2 c3 c4 are pre-filled
# w1 w2 w3 w4 are pre-filled

# fill in info selecting cases of coalitions with most members 
sel7 <- which(max.tmp>1) 
tmp.v <- v[sel7,] # subset for manipulation
tmp.l <- l[sel7,]
tmp.n <- n[sel7,]
max.tmp <- max.tmp[sel7]
tmp.c1 <- c1[sel7,]
tmp.w1 <- w1[sel7]
tmp.ci <- ci[sel7,]

# debug
#i <- which(dat$ord[sel7]==847)

for (i in 1:length(sel7)){
    #i <- length(sel7) # debug
    message(sprintf("loop %s of %s", i, length(sel7)))
    tmp.ci$ncoal[i] <- 1               # running tally co ncoal
    save.col <- which(tmp.n[i,]==max.tmp[i])[1] # spare this column from erasure in process below (1st if multiple hits)
    save.label <- tmp.l[i, save.col]            # keep coalition full label
    tmp.ci$coal1[i] <- save.label      # plug coal label into summary
    tmp <- strsplit(save.label, split = "-") # break into component character vector
    tmp.c1[i,1:length(tmp[[1]])] <- tmp[[1]] # fill in coalition members
    #
    target.cols <- numeric() # initialize empty vector
    for (j in 1:7){
        #j <- 1 # debug
        if (tmp.c1[i,j]=="0") next
        pat <- paste("^", tmp.c1[i,j], "$|", tmp.c1[i,j], "-|-", tmp.c1[i,j], sep="") # searches ^pty$, pty- or -pty (avoids panal hit when searching for pan) --- unnecessary given label changes
        tmp.target <- grep(pattern = pat, x = tmp.l[i,])
        #tmp.target <- grep(pattern = tmp.c1[i,j], x = tmp.l[i,]) # version hits panal when searching pan
        if (length(tmp.target)>0) {
            target.cols <- c(target.cols, tmp.target)
        }
    }
    target.cols <- unique(target.cols); target.cols <- target.cols[order(target.cols)]
    tmp.w1[[i]] <- target.cols # for use when computing coalition members' contribution
    save.vote <- sum(tmp.v[i,target.cols])
    tmp.v[i, target.cols] <- 0   # erase votes to keep only aggregate
    tmp.l[i, target.cols] <- "0" # erase labels to keep only full coalition label
    tmp.n[i, target.cols] <- 0   # erase ns
    tmp.v[i, save.col] <- save.vote  # place aggregate vote back in
    tmp.l[i, save.col] <- save.label # place coalition label back in
}
# return to data
cv[sel7,] <- tmp.v
cl[sel7,] <- tmp.l
n[sel7,] <- tmp.n
c1[sel7,] <- tmp.c1
w1[sel7] <- tmp.w1
ci[sel7,] <- tmp.ci
#tail(w1[sel7])

####################################
# process cases with 2nd coalition #
####################################
max.tmp <- apply(n, 1, max) # max parties reported in a row's cell
table(max.tmp) # coal w most members has 7
sel7 <- which(max.tmp>1) 
tmp.v <- cv[sel7,] # subset for manipulation
tmp.l <- cl[sel7,]
tmp.n <- n[sel7,]
tmp.c2 <- c2[sel7,]
tmp.w2 <- w2[sel7]
max.tmp <- max.tmp[sel7]
tmp.ci <- ci[sel7,]

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
    save.vote <- sum(tmp.v[i,target.cols])
    tmp.v[i, target.cols] <- 0   # erase votes to keep only aggregate
    tmp.l[i, target.cols] <- "0" # erase labels to keep only full coalition label
    tmp.n[i, target.cols] <- 0   # erase ns
    tmp.v[i, save.col] <- save.vote  # place aggregate vote back in
    tmp.l[i, save.col] <- save.label # place coalition label back in
}
# return to data
cv[sel7,] <- tmp.v
cl[sel7,] <- tmp.l
n[sel7,] <- tmp.n
c2[sel7,] <- tmp.c2
w2[sel7] <- tmp.w2
ci[sel7,] <- tmp.ci
# tail(w2[sel7])

####################################
# process cases with 3rd coalition #
####################################
max.tmp <- apply(n, 1, max) # max parties reported in a row's cell
table(max.tmp) # coal w most members has 7
sel7 <- which(max.tmp>1) 
tmp.v <- cv[sel7,] # subset for manipulation
tmp.l <- cl[sel7,]
tmp.n <- n[sel7,]
tmp.c3 <- c3[sel7,]
tmp.w3 <- w3[sel7]
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
    save.vote <- sum(tmp.v[i,target.cols])
    tmp.v[i, target.cols] <- 0   # erase votes to keep only aggregate
    tmp.l[i, target.cols] <- "0" # erase labels to keep only full coalition label
    tmp.n[i, target.cols] <- 0   # erase ns
    tmp.v[i, save.col] <- save.vote  # place aggregate vote back in
    tmp.l[i, save.col] <- save.label # place coalition label back in
}
# return to data
cv[sel7,] <- tmp.v
cl[sel7,] <- tmp.l
n[sel7,] <- tmp.n
c3[sel7,] <- tmp.c3
w3[sel7] <- tmp.w3
ci[sel7,] <- tmp.ci
# tail(w3[sel7])

####################################
# process cases with 4th coalition #
####################################
max.tmp <- apply(n, 1, max) # max parties reported in a row's cell
table(max.tmp) # coal w most members has 7
sel7 <- which(max.tmp>1) 
tmp.v <- cv[sel7,] # subset for manipulation
tmp.l <- cl[sel7,]
tmp.n <- n[sel7,]
tmp.c4 <- c4[sel7,]
tmp.w4 <- w4[sel7]
max.tmp <- max.tmp[sel7]
tmp.ci <- ci[sel7,]

for (i in 1:length(sel7)){
    #i <- 1 # debug
    #tmp.l[i,] # debug
    #tmp.n[i,] # debug
    message(sprintf("loop %s of %s", i, length(sel7)))
    tmp.ci$ncoal[i] <- 4               # running tally co ncoal
    save.col <- which(tmp.n[i,]==max.tmp[i])[1] # spare this column from erasure in process below (1st if multiple hits)
    save.label <- tmp.l[i, save.col]            # keep coalition full label
    tmp.ci$coal4[i] <- save.label      # plug coal label into summary
    tmp <- strsplit(save.label, split = "-") # break into component character vector
    tmp.c4[i,1:length(tmp[[1]])] <- tmp[[1]] # fill in coalition members
    #
    target.cols <- numeric() # initialize empty vector
    for (j in 1:7){
        #j <- 2 # debug
        if (tmp.c4[i,j]=="0") next
        pat <- paste("^", tmp.c4[i,j], "$|", tmp.c4[i,j], "-|-", tmp.c4[i,j], sep="") # searches ^pty$, pty- or -pty (avoids panal hit when searching for pan) --- unnecessary given label changes
        tmp.target <- grep(pattern = pat, x = tmp.l[i,])
        #tmp.target <- grep(pattern = tmp.c4[i,j], x = tmp.l[i,]) # version hits panal when searching pan
        if (length(tmp.target)>0) {
            target.cols <- c(target.cols, tmp.target)
        }
    }
    target.cols <- unique(target.cols); target.cols <- target.cols[order(target.cols)]
    tmp.w4[[i]] <- target.cols # for use when computing coalition members' contribution
    save.vote <- sum(tmp.v[i,target.cols])
    tmp.v[i, target.cols] <- 0   # erase votes to keep only aggregate
    tmp.l[i, target.cols] <- "0" # erase labels to keep only full coalition label
    tmp.n[i, target.cols] <- 0   # erase ns
    tmp.v[i, save.col] <- save.vote  # place aggregate vote back in
    tmp.l[i, save.col] <- save.label # place coalition label back in
}
# return to data
cv[sel7,] <- tmp.v
cl[sel7,] <- tmp.l
n[sel7,] <- tmp.n
c4[sel7,] <- tmp.c4
w4[sel7] <- tmp.w4
ci[sel7,] <- tmp.ci

max.tmp <- apply(n, 1, max) # max parties reported in a row's cell
table(max.tmp) # coal w most members has 7

# prepare object with coalition party weights
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
sel <- which(ci$ncoal>=4)
for (i in sel){
    #i <- sel[1] # debug
    w[[i]]$coal4 <- v[i,w4[[i]]]
    names(w[[i]]$coal4) <- l[i,w4[[i]]]
}
coal.weights <- w # rename
coal.info <- ci
rm(w, ci)

rm(c1, c2, c3, c4, I, i, j, max.tmp, n, pat, save.col, save.label, save.vote, sel, sel7, target.cols, tmp, tmp.c1, tmp.c2, tmp.c3, tmp.c4, tmp.ci, tmp.l, tmp.n, tmp.target, tmp.v, tmp.w1, tmp.w2, tmp.w3, tmp.w4, w1, w2, w3, w4) # housecleaning

## winner (sorts data to have largest vote-winning party in column 1)
# handy function to sort one data frame's rows by order of another, matching data frame
sortBy <- function(target, By){
    t <- target; b <- By;
    do.call(rbind, lapply(seq_len(nrow(b)), 
            function(i) as.character(unlist(t[i,])[order(unlist(-b[i,]))]))) # change to -b for decreasing order
}
# example
## v1 <- data.frame(c1=c(30,15,3), c2=c(10,25,2), c3=c(20,35,4))
## l1 <- data.frame(c1=c("thirty","fifteen","three"), c2=c("ten","twenty-five","two"), c3=c("twenty","thirty-five","four"))
## v1.sorted <- t(apply(v1, 1, function(x) sort(x, decreasing = TRUE))) # sort each row of df -- http://stackoverflow.com/questions/6063881/sorting-rows-alphabetically
## l1.sorted <- sortBy(target = l1, By = v1)
## sortBy(target = v1, By = v1)
## 
## this sorts matrix rows faster than function above
## vot <- t(apply(v1, 1, function(x) sort(x, decreasing = TRUE)))

# sort coalition-aggregated data
tail(cv)
tail(cl)
###########################################
cv.sorted <- sortBy(target = cv, By = cv) # slow!
###########################################
cl.sorted <- sortBy(target = cl, By = cv) # slow!
###########################################
cv.sorted <- as.data.frame(cv.sorted, stringsAsFactors = FALSE) # return matrix to dataframe
cl.sorted <- as.data.frame(cl.sorted, stringsAsFactors = FALSE) # return matrix to dataframe
colnames(cv.sorted) <- colnames(v); colnames(cl.sorted) <- colnames(l)
cv.sorted <- transform(cv.sorted, v01 = as.numeric(v01), v02 = as.numeric(v02), v03 = as.numeric(v03), v04 = as.numeric(v04), v05 = as.numeric(v05), v06 = as.numeric(v06), v07 = as.numeric(v07), v08 = as.numeric(v08), v09 = as.numeric(v09), v10 = as.numeric(v10), v11 = as.numeric(v11), v12 = as.numeric(v12), v13 = as.numeric(v13), v14 = as.numeric(v14), v15 = as.numeric(v15), v16 = as.numeric(v16)) # return to numeric format
tail(cv.sorted)
tail(cl.sorted)

# rename objects so that dat now has coalition aggregates
dat.orig <- dat # original data
dat[,sel.l] <- cl.sorted # return manipulated labels to data
dat[,sel.v] <- cv.sorted # return manipulated votes to data
head(dat)
# move dcoal column before v01
tmp <- dat # duplicate if I mess up
tmp1 <- grep("v01", colnames(dat))
tmp2 <- grep("dcoal", colnames(dat))
dat <- dat[, c(1:(tmp1-1), tmp2, tmp1:(tmp2-1))]
colnames(dat)
rm(cv, cl, cv.sorted, cl.sorted, sel.l, sel.v, v, l, tmp)

table(dat$v15) # check that only has zeroes
dat$v15 <- dat$l15 <- dat$v16 <- dat$l16 <- dat$v17 <- dat$l17 <- dat$v18 <- dat$l18 <- NULL # redundant columns
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
table(check==0)
## sel <- which(check!=0)
## dat[sel[2],]
## dat.orig[sel[2],]
# move ncand column before dcoal
tmp <- dat # duplicate if I mess up
tmp1 <- grep("dcoal", colnames(dat))
tmp2 <- grep("ncand", colnames(dat))
dat <- dat[, c(1:(tmp1-1), tmp2, tmp1:(tmp2-1))]
colnames(dat)

## #################################
## #### DROPS USOS Y COSTUMBRES ####
## #################################
## # any dusos==1?
## table(dat$dusos)
## # caso Cherán
## sel <- which(dat$dusos==1 & dat$edon==16)
## dat$munn[sel]
## sel <- which(dat$edon==16 & dat$munn==24)
## dat[sel, c("yr","efec","status")]
## # drop uyc
## sel <- which(dat$dusos==1)
## table(dat$v01[sel]==0) # all report no votes?
## dat <- dat[-sel,]

################################
### export a coalAgg version ###
################################
sel <- dat$yr>=1985
dat <- dat[sel,] # drop early years
dat$ord <- 1:nrow(dat)
write.csv(dat, file = "aymu1985-present.coalAgg.csv", row.names = FALSE)

sel.l <- grep("^l[0-9]{2}", colnames(dat))
l <- dat[,sel.l] # subset label columns
sel.v <- grep("^v[0-9]{2}", colnames(dat))
v <- dat[,sel.v] # subset vote columns

ci <- coal.info
head(ci[ci$ncoal==3,])
compare this to pancoal etc columns

#table(gsub(pattern = "([0-9a-zA-Záéíóúñ])", replacement = "", tmp)) # keep only non-letter-number characters

## # julio: lista de datos faltantes
## ver2013 pdf difícil




###########################
# ESTO ANALIZA INCUMBENTS #
###########################

dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
setwd(dd)

# read raw data file
inc <- read.csv(file = "aymu1985-present.incumbents.csv", stringsAsFactors = FALSE)
dim(inc)
colnames(inc)

sel <- which(inc$edon==24)
tmp <- inc[sel,]

table(tmp$reelec.post)


i <- 0

i <- i+1
sel <- which(tmp$munn==i)
tmp[sel, c("yr","incumbent","dreran.post","dreelec.post")]



