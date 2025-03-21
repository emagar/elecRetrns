###############################################################################
## SCRIPT PROCESSES RAW DATA AND GENERATES DISTRIBUTED MUNICIPAL VOTE RETURNS ##
## USED ROUTINELY                                                             ##
## AUTHOR: Eric Magar                                                         ##
## emagar at itam dot org                                                     ##
################################################################################

rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
setwd(dd)

############################################
## Read raw data file with all municipios ##
############################################
dat <- read.csv(file = "aymu1970-on.csv", stringsAsFactors = FALSE)
dat$nr <- as.numeric(dat$nr)
## ## Read this instead to prep Chihuahua síndicos 
## dat <- read.csv(file = "ay-nonfused/aymu1998-on-Chihuahua-sind.csv", stringsAsFactors = FALSE)


####################################################################################
####################################################################################
##  Deprecated script ayClean.r has routines used once to save a cleaner dataset  ##
####################################################################################
####################################################################################

## re-compute effective and total votes
sel <- grep("^v[0-9]{2}", colnames(dat))
v <- dat[,sel] # subset votes columns
v[is.na(v)==TRUE] <- 0 # replace NAs with zeroes
v[v==""|v=="NA"|v=="-"|v=="N/R"] <- 0 # replace with zeroes
v[] <- lapply(v, as.numeric) # anything non numeric to that
sapply(v, class)
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

## verify that coalition member separator is always a - 
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

## any votes with no labels?
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

## drop upper case from labels
sel.l <- grep("^l[0-9]{2}", colnames(dat))
l <- dat[,sel.l] # subset labels columns
for (i in 1:ncol(l)){
    l[,i] <- tolower(l[,i])
    }
dat[,sel.l] <- l # return to data
rm(l)

## remove useless labels
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

## change confusing party labels
sel.l <- grep("^l[0-9]{2}", colnames(dat))
l <- dat[,sel.l] # subset labels columns
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^c$|^cd$|^cdppn$|^pc$|^conver(gencia)?$", replacement = "conve", l[,i])
    l[,i] <- gsub(pattern = "-c-|-cd-|-cdppn-|-pc-|-conver(gencia)?-", replacement = "-conve-", l[,i])
    l[,i] <- gsub(pattern = "-c$|-cd$|-cdppn$|-pc$|-conver(gencia)?$", replacement = "-conve", l[,i])
    }
##
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^pmc$", replacement = "mc", l[,i])
    l[,i] <- gsub(pattern = "^pmc-", replacement = "mc-", l[,i])
    l[,i] <- gsub(pattern = "-pmc-", replacement = "-mc-", l[,i])
    l[,i] <- gsub(pattern = "-pmc$", replacement = "-mc", l[,i])
    }
##
## for (i in 1:ncol(l)){
##     l[,i] <- gsub(pattern = "^panal$", replacement = "pna", l[,i])
##     l[,i] <- gsub(pattern = "-panal-", replacement = "-pna-", l[,i])
##     l[,i] <- gsub(pattern = "-panal$", replacement = "-pna", l[,i])
##     l[,i] <- gsub(pattern = "^panal-", replacement = "pna-", l[,i])
##     }
##
## avoid "na" (Nueva Alianza party) in csv, recognized as missing...
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^na$", replacement = "pna", l[,i])
    l[,i] <- gsub(pattern = "-na-", replacement = "-pna-", l[,i])
    l[,i] <- gsub(pattern = "-na$", replacement = "-pna", l[,i])
    l[,i] <- gsub(pattern = "^na-", replacement = "pna-", l[,i])
    }
##
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^pd$", replacement = "pd1", l[,i])
    l[,i] <- gsub(pattern = "^pd-", replacement = "pd1-", l[,i])
    l[,i] <- gsub(pattern = "-pd-", replacement = "-pd1-", l[,i])
    l[,i] <- gsub(pattern = "-pd$", replacement = "-pd1", l[,i])
    }
## for (i in 1:ncol(l)){
##     l[,i] <- gsub(pattern = "^p?asd?c?$|^alter(nativa)?$", replacement = "pasd", l[,i])
##     l[,i] <- gsub(pattern = "-p?asd?c?-|-alter(nativa)?-", replacement = "-pasd-", l[,i])
##     l[,i] <- gsub(pattern = "-p?asd?c?$|-alter(nativa)?$", replacement = "-pasd", l[,i])
##     l[,i] <- gsub(pattern = "^p?asd?c?-|^alter(nativa)?-", replacement = "pasd-", l[,i])
##     }
##
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^alter(nativa)?$", replacement = "pasd", l[,i])
    l[,i] <- gsub(pattern = "-alter(nativa)?-", replacement = "-pasd-", l[,i])
    l[,i] <- gsub(pattern = "-alter(nativa)?$", replacement = "-pasd", l[,i])
    l[,i] <- gsub(pattern = "^alter(nativa)?-", replacement = "pasd-", l[,i])
    }
##
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^pl$", replacement = "pl1", l[,i])
    l[,i] <- gsub(pattern = "^pl-", replacement = "pl1-", l[,i])
    l[,i] <- gsub(pattern = "-pl-", replacement = "-pl1-", l[,i])
    l[,i] <- gsub(pattern = "-pl$", replacement = "-pl1", l[,i])
    }
##
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^pp$", replacement = "pp1", l[,i])
    l[,i] <- gsub(pattern = "^pp-", replacement = "pp1-", l[,i])
    l[,i] <- gsub(pattern = "-pp-", replacement = "-pp1-", l[,i])
    l[,i] <- gsub(pattern = "-pp$", replacement = "-pp1", l[,i])
    }
##
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^pr$", replacement = "pr1", l[,i])
    l[,i] <- gsub(pattern = "^pr-", replacement = "pr1-", l[,i])
    l[,i] <- gsub(pattern = "-pr-", replacement = "-pr1-", l[,i])
    l[,i] <- gsub(pattern = "-pr$", replacement = "-pr1", l[,i])
    }
##
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^ps$", replacement = "ps1", l[,i])
    l[,i] <- gsub(pattern = "^ps-", replacement = "ps1-", l[,i])
    l[,i] <- gsub(pattern = "-ps-", replacement = "-ps1-", l[,i])
    l[,i] <- gsub(pattern = "-ps$", replacement = "-ps1", l[,i])
    }
##
## for (i in 1:ncol(l)){
##     l[,i] <- gsub(pattern = "^pt$", replacement = "pt1", l[,i])
##     l[,i] <- gsub(pattern = "-pt-", replacement = "-pt1-", l[,i])
##     l[,i] <- gsub(pattern = "-pt$", replacement = "-pt1", l[,i])
##     l[,i] <- gsub(pattern = "^pt-", replacement = "pt1-", l[,i])
##     }
#
## for (i in 1:ncol(l)){
##     l[,i] <- gsub(pattern = "^pac$", replacement = "pac1", l[,i])
##     l[,i] <- gsub(pattern = "-pac-", replacement = "-pac1-", l[,i])
##     l[,i] <- gsub(pattern = "-pac$", replacement = "-pac1", l[,i])
##     l[,i] <- gsub(pattern = "^pac-", replacement = "pac1-", l[,i])
##     }
## #
## for (i in 1:ncol(l)){
##     l[,i] <- gsub(pattern = "^pbc$", replacement = "ppbc", l[,i])
##     l[,i] <- gsub(pattern = "-pbc-", replacement = "-ppbc-", l[,i])
##     l[,i] <- gsub(pattern = "-pbc$", replacement = "-ppbc", l[,i])
##     l[,i] <- gsub(pattern = "^pbc-", replacement = "ppbc-", l[,i])
##     }
## #
## for (i in 1:ncol(l)){
##     l[,i] <- gsub(pattern = "^pcd$", replacement = "pcd1", l[,i])
##     l[,i] <- gsub(pattern = "-pcd-", replacement = "-pcd1-", l[,i])
##     l[,i] <- gsub(pattern = "-pcd$", replacement = "-pcd1", l[,i])
##     l[,i] <- gsub(pattern = "^pcd-", replacement = "pcd1-", l[,i])
##     }
#
## for (i in 1:ncol(l)){
##     l[,i] <- gsub(pattern = "^pcm$", replacement = "pcm1", l[,i])
##     l[,i] <- gsub(pattern = "-pcm-", replacement = "-pcm1-", l[,i])
##     l[,i] <- gsub(pattern = "-pcm$", replacement = "-pcm1", l[,i])
##     l[,i] <- gsub(pattern = "^pcm-", replacement = "pcm1-", l[,i])
##     }
## #
## for (i in 1:ncol(l)){
##     l[,i] <- gsub(pattern = "^pds$", replacement = "pds1", l[,i])
##     l[,i] <- gsub(pattern = "-pds-", replacement = "-pds1-", l[,i])
##     l[,i] <- gsub(pattern = "-pds$", replacement = "-pds1", l[,i])
##     l[,i] <- gsub(pattern = "^pds-", replacement = "pds1-", l[,i])
##     }
## #
## for (i in 1:ncol(l)){
##     l[,i] <- gsub(pattern = "^pfd$", replacement = "pfd1", l[,i])
##     l[,i] <- gsub(pattern = "-pfd-", replacement = "-pfd1-", l[,i])
##     l[,i] <- gsub(pattern = "-pfd$", replacement = "-pfd1", l[,i])
##     l[,i] <- gsub(pattern = "^pfd-", replacement = "pfd1-", l[,i])
##     }
## #
## for (i in 1:ncol(l)){
##     l[,i] <- gsub(pattern = "^ppn$", replacement = "ppn1", l[,i])
##     l[,i] <- gsub(pattern = "-ppn-", replacement = "-ppn1-", l[,i])
##     l[,i] <- gsub(pattern = "-ppn$", replacement = "-ppn1", l[,i])
##     l[,i] <- gsub(pattern = "^ppn-", replacement = "ppn1-", l[,i])
##     }
## #
## for (i in 1:ncol(l)){
##     l[,i] <- gsub(pattern = "^psd$", replacement = "psd1", l[,i])
##     l[,i] <- gsub(pattern = "-psd-", replacement = "-psd1-", l[,i])
##     l[,i] <- gsub(pattern = "-psd$", replacement = "-psd1", l[,i])
##     l[,i] <- gsub(pattern = "^psd-", replacement = "psd1-", l[,i])
##     }
## #
## for (i in 1:ncol(l)){
##     l[,i] <- gsub(pattern = "^udc$", replacement = "pudc", l[,i])
##     l[,i] <- gsub(pattern = "-udc-", replacement = "-pudc-", l[,i])
##     l[,i] <- gsub(pattern = "-udc$", replacement = "-pudc", l[,i])
##     l[,i] <- gsub(pattern = "^udc-", replacement = "pudc-", l[,i])
##     }
## #
## for (i in 1:ncol(l)){
##     l[,i] <- gsub(pattern = "^mrps$", replacement = "pmrps", l[,i])
##     l[,i] <- gsub(pattern = "-mrps-", replacement = "-pmrps-", l[,i])
##     l[,i] <- gsub(pattern = "-mrps$", replacement = "-pmrps", l[,i])
##     l[,i] <- gsub(pattern = "^mrps-", replacement = "pmrps-", l[,i])
##     }
##
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^otro$", replacement = "otros", l[,i])
    l[,i] <- gsub(pattern = "-otro-", replacement = "-otros-", l[,i])
    l[,i] <- gsub(pattern = "-otro$", replacement = "-otros", l[,i])
    l[,i] <- gsub(pattern = "^otro-", replacement = "otros-", l[,i])
    }
##
## for (i in 1:ncol(l)){
##     l[,i] <- gsub(pattern = "^parme.$", replacement = "parm", l[,i])
##     l[,i] <- gsub(pattern = "-parme.-", replacement = "-parm-", l[,i])
##     l[,i] <- gsub(pattern = "-parme.$", replacement = "-parm", l[,i])
##     l[,i] <- gsub(pattern = "^parme.-", replacement = "parm-", l[,i])
##     }
##
## for (i in 1:ncol(l)){
##     l[,i] <- gsub(pattern = "^indep$", replacement = "indep1", l[,i])
##     l[,i] <- gsub(pattern = "-indep-", replacement = "-indep1-", l[,i])
##     l[,i] <- gsub(pattern = "-indep$", replacement = "-indep1", l[,i])
##     l[,i] <- gsub(pattern = "^indep-", replacement = "indep1-", l[,i])
##     }
##
## for (i in 1:ncol(l)){
##     l[,i] <- gsub(pattern = "^alianza$", replacement = "alianza1", l[,i])
##     l[,i] <- gsub(pattern = "-alianza-", replacement = "-alianza1-", l[,i])
##     l[,i] <- gsub(pattern = "-alianza$", replacement = "-alianza1", l[,i])
##     l[,i] <- gsub(pattern = "^alianza-", replacement = "alianza1-", l[,i])
##     }
##
## ## pes and pesm both present in mor 2021
## for (i in 1:ncol(l)){
##     l[,i] <- gsub(pattern = "^es$|^pesm$|^pest$", replacement = "pes",   l[,i])
##     l[,i] <- gsub(pattern = "-es-|-pesm-|-pest-", replacement = "-pes-", l[,i])
##     l[,i] <- gsub(pattern = "-es$|-pesm$|-pest$", replacement = "-pes",  l[,i])
##     l[,i] <- gsub(pattern = "^es-|^pesm-|^pest-", replacement = "pes-",  l[,i])
##     }
##
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^pmac$|^moveracps$|^mach$", replacement = "pmch", l[,i])
    l[,i] <- gsub(pattern = "-pmac-|-moveracps-|-mach-", replacement = "-pmch-", l[,i])
    l[,i] <- gsub(pattern = "-pmac$|-moveracps$|-mach$", replacement = "-pmch", l[,i])
    l[,i] <- gsub(pattern = "^pmac-|^moveracps-|^mach-", replacement = "pmch-", l[,i])
    }
##
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^pcu$", replacement = "pchu", l[,i])
    l[,i] <- gsub(pattern = "-pcu-", replacement = "-pchu-", l[,i])
    l[,i] <- gsub(pattern = "-pcu$", replacement = "-pchu", l[,i])
    l[,i] <- gsub(pattern = "^pcu-", replacement = "pchu-", l[,i])
    }
##
for (i in 1:ncol(l)){
    l[,i] <- gsub(pattern = "^fsm$", replacement = "fxm", l[,i])
    l[,i] <- gsub(pattern = "-fsm-", replacement = "-fxm-", l[,i])
    l[,i] <- gsub(pattern = "-fsm$", replacement = "-fxm", l[,i])
    l[,i] <- gsub(pattern = "^fsm-", replacement = "fxm-", l[,i])
    }
##
##
sel.r <- which(dat$yr>2006)
##table(l[sel.r,18]) # easier to verify one column at a time
dat[,sel.l] <- l # return to data
rm(l)

## # some longer pty labels include shorter ones (eg pan panal) giving false positives in regexes
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
##
## dummy had at least one coalition
dat$dcoal <- 0
for (i in 1:ncol(l)){
    dat$dcoal[grep("-", l[,i])] <- 1
}

## create objects with vote for coalition(s) sumed and redudant columns dropped
cv <- v; # will receive votes with coalitions aggregated
cl <- l; # will keep coalition labels but drop coalition member labels 
## create "split" objects for votes contrinuted by each coalition member and joint column dropped
sv <- v;
sl <- l;
##
## coalition summary info
ci <- data.frame(dcoal=dat$dcoal, ncoal=NA, coal1="none", coal2="none", coal3="none", coal4="none", stringsAsFactors = FALSE)
ci$ncoal[ci$dcoal==0] <- 0 # 0=no coalition
## ci$coal1 coal2 coal3 coal4 pre-filled
## n is object reporting how many parties reported in a v/l cell?
ln1 <- v # duplicate votes
ln1[] <- 0 # will receive info
ln2 <- ln1 # duplicate
## replace each label by its character length
for (i in 1:ncol(ln1)){
                                        #i <- 16 # debug
    tmp <- l[,i] # pick ith column as vector
    ln1[,i] <- nchar(tmp) # replace items with num characters
}
## drop hyphens to count character difference
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
c4 <- c3 <- c2 <- c1 <- as.data.frame(matrix("0", I, 7), col.names = paste("p", 1:7, sep = ""), stringsAsFactors = FALSE) # will receive vector of 1st 2nd 3rd 4th coalition's members
##
# will receive columns corresponding to coalition members in v/l for use when weighting votes contributed by each member
w4 <- w3 <- w2 <- w1 <- as.list(rep("noCoal",I))
##
## # fill in easy cases with no coalition --- 3sep2021 no longer needed with new procedure
## sel <- which(dat$dcoal==0)
## cv[sel,] <- v[sel,]
## cl[sel,] <- l[sel,]
## sv[sel,] <- v[sel,]
## sl[sel,] <- l[sel,]
## # c1 c2 c3 c4 are pre-filled
## # w1 w2 w3 w4 are pre-filled

## fill in info selecting cases of coalitions with most members 
sel7 <- which(max.tmp>1) 
#grep("ags-18", dat$emm[sel7]) # debug
tmp.v  <- cv[sel7,] # subset for manipulation --- 3sep2021 used to pick from v, forgetting manip in later rounds
tmp.vw <- sv[sel7,] # will receive votes contributed by each coalition member
tmp.l  <- cl[sel7,]
tmp.lw <- sl[sel7,] # will receive labels of vote-contributing parties
tmp.n  <- n[sel7,]
tmp.c1 <- c1[sel7,]
tmp.w1  <- w1[sel7] # will receive indices to manipulate
tmp.vw1 <- w1[sel7] # will receive manipulated votes
max.tmp <- max.tmp[sel7]
tmp.ci <- ci[sel7,]

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
    target.cols <- numeric()           # initialize empty vector 
    for (j in 1:7){
        #j <- 1 # debug
        if (tmp.c1[i,j]=="0") next
        ## searches ^pty$, pty- or -pty (avoids panal hit when searching for pan) --- reduntant given label changes
        pat <- paste("^", tmp.c1[i,j], "$|", tmp.c1[i,j], "-|-", tmp.c1[i,j], sep="")
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
        tmp.l[i, target.cols] <- "0" # erase labels to keep only full coalition label manipulation
        tmp.v[i, save.col] <- save.vote  # place aggregate vote back in
        tmp.l[i, save.col] <- save.label # place coalition label back in
        ## votes contributed
        tmp.vw[i, save.col] <- 0   # erase joint votes
        tmp.lw[i, save.col] <- "0" # erase joint label
        tmp.vw[i, sel] <- tmp.vw1[[i]]  # place contributed votes back in
    }
    tmp.n[i, target.cols] <- 0   # erase ns, prep for obs's next coalition
}
## return to data
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
tmp.v  <- cv[sel7,] # subset for manipulation --- 3sep2021 used to pick from v, forgetting manip in later rounds
tmp.vw <- sv[sel7,] # will receive votes contributed by each coalition member
tmp.l  <- cl[sel7,]
tmp.lw <- sl[sel7,] # will receive labels of vote-contributing parties
tmp.n <- n[sel7,]
tmp.c2 <- c2[sel7,]
tmp.w2 <- w2[sel7]
tmp.vw2 <- w2[sel7] # will receive manipulated votes
max.tmp <- max.tmp[sel7]
tmp.ci <- ci[sel7,]

for (i in 1:length(sel7)){
    #i <- 50 # debug
    #tmp.l[i,] # debug
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
        ## searches ^pty$, pty- or -pty (avoids panal hit when searching for pan) --- unnecessary given label changes
        pat <- paste("^", tmp.c2[i,j], "$|", tmp.c2[i,j], "-|-", tmp.c2[i,j], sep="")
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
        ## votes contributed
        tmp.vw[i, save.col] <- 0   # erase joint votes
        tmp.lw[i, save.col] <- "0" # erase joint label
        tmp.vw[i, sel] <- tmp.vw2[[i]]  # place contributed votes back in
    }
    tmp.n[i, target.cols] <- 0   # erase ns
}
## return to data
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
        ## searches ^pty$, pty- or -pty (avoids panal hit when searching for pan) --- unnecessary given label changes
        pat <- paste("^", tmp.c3[i,j], "$|", tmp.c3[i,j], "-|-", tmp.c3[i,j], sep="")
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
        ## votes contributed
        tmp.vw[i, save.col] <- 0   # erase joint votes
        tmp.lw[i, save.col] <- "0" # erase joint label
        tmp.vw[i, sel] <- tmp.vw3[[i]]  # place contributed votes back in
    }
    tmp.n[i, target.cols] <- 0   # erase ns
}
## return to data
cv[sel7,] <- tmp.v
cl[sel7,] <- tmp.l
n[sel7,]  <- tmp.n
c3[sel7,] <- tmp.c3
w3[sel7]  <- tmp.w3
ci[sel7,] <- tmp.ci
sv[sel7,] <- tmp.vw
sl[sel7,] <- tmp.lw
# tail(w3[sel7])

####################################
# process cases with 4th coalition #
####################################
max.tmp <- apply(n, 1, max) # max parties reported in a row's cell
table(max.tmp) # coal w most members has 7
sel7 <- which(max.tmp>1) 
tmp.v  <- cv[sel7,] # subset for manipulation --- 3sep2021 used to pick from v, forgetting manip in later rounds
tmp.vw <- sv[sel7,] # will receive votes contributed by each coalition member
tmp.l  <- cl[sel7,]
tmp.lw <- sl[sel7,] # will receive labels of vote-contributing parties
tmp.n <- n[sel7,]
tmp.c4 <- c4[sel7,]
tmp.w4 <- w4[sel7]
tmp.vw4 <- w4[sel7] # will receive manipulated votes
max.tmp <- max.tmp[sel7]
tmp.ci <- ci[sel7,]

for (i in 1:length(sel7)){
    ## i <- 5 # debug
    ## tmp.l[i,] # debug
    ## tmp.n[i,] # debug
    ## dat[sel7[i],] # debug
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
        ## searches ^pty$, pty- or -pty (avoids panal hit when searching for pan) --- unnecessary given label changes
        pat <- paste("^", tmp.c4[i,j], "$|", tmp.c4[i,j], "-|-", tmp.c4[i,j], sep="")
        tmp.target <- grep(pattern = pat, x = tmp.l[i,])
        #tmp.target <- grep(pattern = tmp.c4[i,j], x = tmp.l[i,]) # version hits panal when searching pan
        if (length(tmp.target)>0) {
            target.cols <- c(target.cols, tmp.target)
        }
    }
    target.cols <- unique(target.cols); target.cols <- target.cols[order(target.cols)]
    tmp.w4[[i]] <- target.cols # for use when computing coalition members' contribution
    sel <- setdiff(target.cols, save.col)  # setdiffs joint votes
    if (length(sel)==0) sel <- target.cols # or all if singleton
    tmp.vw4[[i]] <- tmp.v[i, sel] # keep to fill coalition members' contribution (setdiffs joint votes)
    save.vote <- sum(tmp.v[i,target.cols])
    tmp.vw4[[i]] <- tmp.vw4[[i]] / sum(tmp.v[i,sel])   # relative contributions
    tmp.vw4[[i]] <- round(tmp.vw4[[i]] * save.vote, 2) # relative votes contributed
    if (length(sel)>1){ # do not manipulate if singleton
        tmp.v[i, target.cols] <- 0   # erase votes to keep only aggregate
        tmp.l[i, target.cols] <- "0" # erase labels to keep only full coalition label
        tmp.v[i, save.col] <- save.vote  # place aggregate vote back in
        tmp.l[i, save.col] <- save.label # place coalition label back in
        ## votes contributed
        tmp.vw[i, save.col] <- 0   # erase joint votes
        tmp.lw[i, save.col] <- "0" # erase joint label
        tmp.vw[i, sel] <- tmp.vw4[[i]]  # place contributed votes back in
    }
    tmp.n[i, target.cols] <- 0   # erase ns
}
## return to data
cv[sel7,] <- tmp.v
cl[sel7,] <- tmp.l
n[sel7,] <- tmp.n
c4[sel7,] <- tmp.c4
w4[sel7] <- tmp.w4
ci[sel7,] <- tmp.ci
sv[sel7,] <- tmp.vw
sl[sel7,] <- tmp.lw

max.tmp <- apply(n, 1, max) # max parties reported in a row's cell
print("Table must have 0s and 1s only, else coalitions remain that need manipulation")
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

## plug ncoal into data
dat$ncoal  <- ci$ncoal
## plug coal descriptions into data
dat$coal1 <- ci$coal1
dat$coal2 <- ci$coal2
dat$coal3 <- ci$coal3
dat$coal4 <- ci$coal4

## prepare object with coalition party weights
w <- as.list(rep("noCoal",I))
sel <- which(ci$ncoal>=1)
for (i in sel){
    w[[i]] <- list(coal1="no", coal2="no", coal3="no", coal4="no")
    #i <- sel[1] # debug
    w[[i]]$coal1 <- v[i,w1[[i]]]
    names(w[[i]]$coal1) <- l[i,w1[[i]]]
}
##
sel <- which(ci$ncoal>=2)
for (i in sel){
    #i <- sel[1] # debug
    w[[i]]$coal2 <- v[i,w2[[i]]]
    names(w[[i]]$coal2) <- l[i,w2[[i]]]
}
##
sel <- which(ci$ncoal>=3)
for (i in sel){
    #i <- sel[1] # debug
    w[[i]]$coal3 <- v[i,w3[[i]]]
    names(w[[i]]$coal3) <- l[i,w3[[i]]]
}
##
sel <- which(ci$ncoal>=4)
for (i in sel){
    #i <- sel[1] # debug
    w[[i]]$coal4 <- v[i,w4[[i]]]
    names(w[[i]]$coal4) <- l[i,w4[[i]]]
}
coal.weights <- w # rename
coal.info <- ci
rm(w, ci)
## housecleaning
rm(c1, c2, c3, c4, I, i, j, max.tmp, n, pat, save.col, save.label, save.vote, sel, sel7, target.cols, tmp, tmp.c1, tmp.c2, tmp.c3, tmp.c4, tmp.ci, tmp.l, tmp.n, tmp.target, tmp.v, tmp.w1, tmp.w2, tmp.w3, tmp.w4, w1, w2, w3, w4, tmp.lw, tmp.vw, tmp.vw1, tmp.vw2, tmp.vw3, tmp.vw4)
                                        

## winner (sorts data to have largest vote-winning party in column 1)
## handy function to sort one data frame's rows by order of another, matching data frame
## if my machine use scripts in disk
pth <- ifelse (Sys.info()["user"] %in% c("eric", "magar"),
    "~/Dropbox/data/useful-functions",
    "https://raw.githubusercontent.com/emagar/useful-functions/master"
    )
## Reads sortBy function
source( paste(pth, "sortBy.r", sep = "/") )
rm(pth)

## # debug
## grep("ags-18", dat$emm)
## sl[156,]
## x

###############################################
## sort coalition-aggregated data columnwise ##
###############################################
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
cv.sorted <- transform(cv.sorted, v01 = as.numeric(v01), v02 = as.numeric(v02), v03 = as.numeric(v03), v04 = as.numeric(v04), v05 = as.numeric(v05), v06 = as.numeric(v06), v07 = as.numeric(v07), v08 = as.numeric(v08), v09 = as.numeric(v09), v10 = as.numeric(v10), v11 = as.numeric(v11), v12 = as.numeric(v12), v13 = as.numeric(v13), v14 = as.numeric(v14), v15 = as.numeric(v15), v16 = as.numeric(v16), v17 = as.numeric(v17), v18 = as.numeric(v18), v19 = as.numeric(v19) , v20 = as.numeric(v20) , v21 = as.numeric(v21) , v22 = as.numeric(v22) , v23 = as.numeric(v23) , v24 = as.numeric(v24) , v25 = as.numeric(v25)) # return to numeric format
tail(cv.sorted)
tail(cl.sorted)

## save image for debug
save.image("tmp.RData")
## ## reload image for debug
## rm(list = ls())
## dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
## setwd(dd)
## load("tmp.RData")

## Reads functions
pth <- ifelse (Sys.info()["user"] %in% c("eric", "magar"),
    "~/Dropbox/data/useful-functions",
    "https://raw.githubusercontent.com/emagar/useful-functions/master"
    )
source( paste(pth, "moveme.r", sep = "/") )
source( paste(pth, "notin.r", sep = "/") )
rm(pth)

## rename objects so that dat now has coalition aggregates
dat.orig <- dat # duplicate original data
dat[,sel.l] <- cl.sorted # return manipulated labels to data
dat[,sel.v] <- cv.sorted # return manipulated votes to data
dat[,sel.v] <- lapply(dat[,sel.v],as.numeric) # make numeric (in síndicos cv.sorted became str)
head(dat)
str(dat)

## prepare coalition-split object for export
dat.split <- dat.orig
dat.split[,sel.l] <- sl # return manipulated labels to data
dat.split[,sel.v] <- sv # return manipulated votes to data
## move dcoal and ncoal columns before v01
tmp <- dat # duplicate if I mess up
colnames(dat)
dat <- dat[moveme(names(dat), "dcoal before v01")]
dat <- dat[moveme(names(dat), "ncoal before v01")]
colnames(dat)
dat[1,]
rm(sv, sl, cv, cl, cv.sorted, cl.sorted, sel.l, sel.v, v, l, tmp)

## # 23 cols needed thanks to mor 2021...
## table(dat$v24) # check that only has zeroes
## table(dat$v09) # check that only has zeroes (for síndicos)
## # if not, which cases remain? 
## #sel <- which(dat$v14>0)
## #dat[sel,]
dat$v24 <- dat$l24 <- dat$v25 <- dat$l25 <- NULL # drop redundant columns
##dat$v09 <- dat$l09 <- dat$v10 <- dat$l10 <- dat$v11 <- dat$l11 <- dat$v12 <- dat$l12 <- dat$v13 <- dat$l13 <- dat$v14 <- dat$l14 <- NULL # drop redundant columns (for síndicos)
dat$win <- dat$l01
## move win column before v01
tmp <- dat # duplicate if I mess up
dat <- dat[moveme(names(dat), "win before v01")]
colnames(dat)

sel.l <- grep("^l[0-9]{2}", colnames(dat))
l <- dat[,sel.l] # subset label columns
sel.v <- grep("^v[0-9]{2}", colnames(dat))
v <- dat[,sel.v] # subset vote columns
dat$ncand <- apply(v, 1, function(x) length(x[x>0]))
## check that coal aggregation produces same efec as before
check <- round(rowSums(v)) - dat$efec
table(check==0) # all must be true
## summary(check)
## sel <- which(check>.01)
## check[sel]
## dat[sel[1],]
## dat.orig[sel[1],]
## rowSums(v)[sel]
## dat$efec[sel]
##
## move ncand column before dcoal
dat <- dat[moveme(names(dat), "ncand before dcoal")]
##
## compute margin
dat$mg <- round( (dat$v01 - dat$v02) / dat$efec , 4)
## move ncand column after dcoal
dat <- dat[moveme(names(dat), "mg after win")]

#################################
#### DROPS USOS Y COSTUMBRES ####
#################################
## any uyc?
table(dat$status)
sel <- grep("uyc", dat$status)
dat[sel, c("emm","mun","yr","efec","status")] ## includes case out of uyc
sel <- grep("to-uyc|new--uyc", dat$status)
dat[sel, c("emm","mun","yr","efec","status")]
## drop uyc
table(dat$v01[sel]==0) # all report no votes?
if (length(sel)>0) dat <- dat[-sel,]

## sort columns
dat <- dat[moveme(names(dat), "efec before nr")]

## Drop ord, dcoal for saving
dat$ord <- dat$dcoal <- NULL

#########################################################
## ################################################### ##
## ## Export a coalAgg version of votes returns to  ## ##
## ## the same directory where aymu1970-on.csv is   ## ##
## ################################################### ##
#########################################################

## add dextra dummy
dat$dextra <- 0
dat$dextra[grep("extra", dat$status)] <- 1
table(dat$dextra)

## drop these obs from analysis
table(dat$status)
##drop.r <- grep("cancelled|missing|litigio|pending", dat$status)
drop.r <- grep("pending", dat$status)

## drop these cols to trim file size for gsheets
drop.c <- c("ord", "status", "dcoal", "win", "mg", "nr", "nulos", "tot", "fuente", "notas", "x", "X", "check", "dpre1997")
ncol(dat[,      colnames(dat) %notin% drop.c])
drop.c <- which(colnames(dat) %in%    drop.c)

if (length(drop.r)==0) {
    dat2 <- dat
} else {
    dat2 <- dat[-drop.r,]
}

## drop coal1 coal2 coal3 coal4 (they're for coalSplit only)
dat2 <- dat2[, -which(colnames(dat2) %in% c("coal1", "coal2", "coal3", "coal4"))]
ncol(dat2)

## subsets by yrs
sel1 <- which(dat2$yr>=1970 & dat2$yr<1982)
sel2 <- which(dat2$yr>=1982 & dat2$yr<1990)
sel3 <- which(dat2$yr>=1990 & dat2$yr<2000)
sel4 <- which(dat2$yr>=2000 & dat2$yr<2010)
sel5 <- which(dat2$yr>=2010 & dat2$yr<2020)
sel6 <- which(dat2$yr>=2020 & dat2$yr<2030)
length(sel1); length(sel1)*58 < 400000 # 400k cells is gsheets max
length(sel2); length(sel2)*58 < 400000
length(sel3); length(sel3)*58 < 400000
length(sel4); length(sel4)*58 < 400000
length(sel5); length(sel5)*58 < 400000
length(sel6); length(sel6)*58 < 400000
#6756*58

#################################
## 16aug2024: add mg after win ##
#################################

## save full file
write.csv(dat2, file = "aymu1970-on.coalAgg.csv", row.names = FALSE) ## use dat instead if keeping drop.r obs
##write.csv(dat2, file = "ay-nonfused/aymu1998-on-Chihuahua-sind.coalAgg.csv", row.names = FALSE) # for cua síndicos

## subset ~1970s
if (length(drop.r)>0) { # restore for manipulation
    dat2 <- dat[-drop.r, -drop.c]
} else {
    dat2 <- dat[       , -drop.c]
}
dat2 <- dat2[, -which(colnames(dat2) %in% c("coal1", "coal2", "coal3", "coal4"))]
dat2 <- dat2[sel1,] # subset
write.csv(dat2, file = "smaller-for-gsheets/aymu.coalAgg1970s.csv", row.names = FALSE)
## subset ~1980s (1980s need to be split due to oax pre-usos y costumbres) 
if (length(drop.r)>0) { # restore for manipulation
    dat2 <- dat[-drop.r, -drop.c]
} else {
    dat2 <- dat[       , -drop.c]
}
dat2 <- dat2[, -which(colnames(dat2) %in% c("coal1", "coal2", "coal3", "coal4"))]
dat2 <- dat2[sel2,] # subset
write.csv(dat2, file = "smaller-for-gsheets/aymu.coalAgg1980s.csv", row.names = FALSE)
## subset 1990s
if (length(drop.r)>0) { # restore for manipulation
    dat2 <- dat[-drop.r, -drop.c]
} else {
    dat2 <- dat[       , -drop.c]
}
dat2 <- dat2[, -which(colnames(dat2) %in% c("coal1", "coal2", "coal3", "coal4"))]
dat2 <- dat2[sel3,] # subset
write.csv(dat2, file = "smaller-for-gsheets/aymu.coalAgg1990s.csv", row.names = FALSE)
## subset 2000s
if (length(drop.r)>0) { # restore for manipulation
    dat2 <- dat[-drop.r, -drop.c]
} else {
    dat2 <- dat[       , -drop.c]
}
dat2 <- dat2[, -which(colnames(dat2) %in% c("coal1", "coal2", "coal3", "coal4"))]
dat2 <- dat2[sel4,] # subset
write.csv(dat2, file = "smaller-for-gsheets/aymu.coalAgg2000s.csv", row.names = FALSE)
## subset 2010s
if (length(drop.r)>0) { # restore for manipulation
    dat2 <- dat[-drop.r, -drop.c]
} else {
    dat2 <- dat[       , -drop.c]
}
dat2 <- dat2[sel5,] # subset
write.csv(dat2, file = "smaller-for-gsheets/aymu.coalAgg2010s.csv", row.names = FALSE)
## subset 2020s
if (length(drop.r)>0) { # restore for manipulation
    dat2 <- dat[-drop.r, -drop.c]
} else {
    dat2 <- dat[       , -drop.c]
}
dat2 <- dat2[, -which(colnames(dat2) %in% c("coal1", "coal2", "coal3", "coal4"))]
dat2 <- dat2[sel6,] # subset
write.csv(dat2, file = "smaller-for-gsheets/aymu.coalAgg2020s.csv", row.names = FALSE)

rm(dat2)


#########################################################
## ################################################### ##
## ## Export splitCoal version of votes returns to  ## ##
## ## the same directory where aymu1979-on.csv is   ## ##
## ################################################### ##
#########################################################
##
## any uyc?
table(dat.split$status)
sel <- grep("uyc", dat.split$status)
dat.split[sel, c("emm","mun","yr","efec","status")] ## includes case out of uyc
sel <- grep("to-uyc|new--uyc", dat.split$status)
dat.split[sel, c("emm","mun","yr","efec","status")]
## drop uyc
table(dat.split$v01[sel]==0) # all report no votes?
if (length(sel)>0) dat.split <- dat.split[-sel,]
##
################################################################
## manipulate coalitions remaining in split vote object       ##
## these should all be cases where no vote split is available ##
################################################################
sel.l <- grep("^l[0-9]{2}", colnames(dat.split))
l <- dat.split[,sel.l] # subset label columns
sel.v <- grep("^v[0-9]{2}", colnames(dat.split))
v <- dat.split[,sel.v] # subset vote columns
## need to manipulate indicator: dummy had at least one coalition
dat.split$dhascoal <- rep(0, nrow(dat.split))
for (i in 1:ncol(l)){
    dat.split$dhascoal[grep("-", l[,i])] <- 1
}
table(dhascoal=dat.split$dhascoal)
##
## function manipulate cases by hand: select state-year function grants all joint votes to party reported first in label
by.hand <- function(edo.cyc = NA){ # example: edo.cyc = "ags-12"
    sel <- grep(edo.cyc, dat.split$emm)
    for (i in 1:ncol(l)){
        #i <- 2
        l[sel,i] <- sub("^([a-z]+)[-].+$", "\\1", l[sel,i]) # keeps first pty in coal label
    }
    dat.split[sel,sel.l] <- l[sel,] # return manipulated labels to data
    dat.split$dhascoal[sel] <- 0 # reset indices
    return(dat.split)
}
##
## manipulate cases where source offers no clue about intra-coal vote split --- all votes to 1st pty listed
dat.split <- by.hand("ags-12")
dat.split <- by.hand("ags-13")
dat.split <- by.hand("bc-11")
dat.split <- by.hand("bc-12")
dat.split <- by.hand("bc-13")
dat.split <- by.hand("bc-14")
dat.split <- by.hand("bc-15")
dat.split <- by.hand("bcs-10")
dat.split <- by.hand("bcs-11")
dat.split <- by.hand("bcs-12")
dat.split <- by.hand("bcs-13")
dat.split <- by.hand("bcs-14")
dat.split <- by.hand("bcs-15")
dat.split <- by.hand("cam-11")
dat.split <- by.hand("cam-13")
dat.split <- by.hand("cam-14")
dat.split <- by.hand("cam-15")
dat.split <- by.hand("coa-08")
dat.split <- by.hand("coa-10") # 1999 pan-prd to pan
dat.split <- by.hand("col-11")
dat.split <- by.hand("col-13")
dat.split <- by.hand("col-14")
dat.split <- by.hand("col-15")
dat.split <- by.hand("cps-11")
dat.split <- by.hand("cps-12") # 2004 prd-pan to prd
dat.split <- by.hand("cps-13")
dat.split <- by.hand("cps-14")
dat.split <- by.hand("cps-15")
dat.split <- by.hand("cps-16")
dat.split <- by.hand("cps-17a")
dat.split <- by.hand("cua-09") # 1995 pcdp-pt to pcdp
dat.split <- by.hand("cua-11") # 2001 pan-prd to pan
dat.split <- by.hand("cua-12") # 2004 pan-prd to pan pri-pvem to pri
dat.split <- by.hand("cua-13") # 2007 pri-pna to pri
dat.split <- by.hand("cua-14") # 2010 pri-pvem-pna to pri
dat.split <- by.hand("cua-15") # 2013 pri-pvem-pna to pri
dat.split <- by.hand("cua-16") # 2016 pri-pvem-pna to pri
dat.split <- by.hand("df-11") # 2000 pan-pvem to pan
dat.split <- by.hand("dgo-11") # 2000 pt-prd to pt
dat.split <- by.hand("dgo-13") # 2007 pri-pna to pri pt-c to pt
dat.split <- by.hand("dgo-14") # 2010 pri-pvem-pna to pri
dat.split <- by.hand("dgo-15") # 2013 pri-pvem-pna to pri
dat.split <- by.hand("dgo-16") # 2016 pri-pvem-pna to pri
dat.split <- by.hand("gua-11") # 2000 prd-pt-ps to prd
dat.split <- by.hand("gua-13") # 2006 prd-pt to prd
dat.split <- by.hand("gue-09") # 1996 pps-c to pps
dat.split <- by.hand("gue-11") # 2002 prd-pt to prd
dat.split <- by.hand("gue-12") # 2005 prd-prs to prd
dat.split <- by.hand("gue-13") # 2008 pri-pvem to pri c-pt to c
dat.split <- by.hand("gue-14") # 2012 pri-pvem to pri prd-pt-c to prd
dat.split <- by.hand("gue-16") # 2018 prd-mc to prd
dat.split <- by.hand("hgo-11") # 2002 pri-pvem to pri
dat.split <- by.hand("hgo-13") # 2008 pri-pna to pri
dat.split <- by.hand("hgo-14") # 2011 pt-conve to pt
dat.split <- by.hand("jal-13") # 2006 prd-pt to prd
dat.split <- by.hand("jal-07") # 1988 prd-pps-pfcrn to prd
dat.split <- by.hand("jal-15") # 2012 pt-mc to pt
dat.split <- by.hand("mex-12") # 2006 pri-pvem to pri
dat.split <- by.hand("mex-14") # 2012 pt-mc to pt
dat.split <- by.hand("mic-11") # 2001 prd+5 to prd
dat.split <- by.hand("mic-12") # 2004 pri-prd to pri
dat.split <- by.hand("mic-13") # 2007 prd-pt-conve to prd
dat.split <- by.hand("mic-14") # 2011 prd-pt to prd
dat.split <- by.hand("mic-15") # 2015 prd-pt to prd
dat.split <- by.hand("mor-09") # 1994 pps-pt to pps
dat.split <- by.hand("mor-13") # 2006 prd-pt-c to prd
dat.split <- by.hand("nay-09") # 1996 pps-oth to pps
dat.split <- by.hand("nay-13") # 2008 pri-pna to pri prd-pvem to prd
dat.split <- by.hand("nay-14") # 2011 pri-pvem-pna to pri
dat.split <- by.hand("nay-15") # 2014 pri-pvem-pna to pri
dat.split <- by.hand("nl-10") # 1997 prd-pvem to prd
dat.split <- by.hand("nl-11") # 2000 prd-oth to prd
dat.split <- by.hand("nl-12") # 2003 pri-pvem to pri
dat.split <- by.hand("nl-13") # 2006 pri-pvem to pri
dat.split <- by.hand("nl-14") # 2009 pri-pvem to pri
dat.split <- by.hand("nl-15") # 2012 pri-pvem to pri
dat.split <- by.hand("pue-13") # 2007 pri-pvem to pri prd-mc to prd
dat.split <- by.hand("pue-14") # 2010 pri-pvem to pri
dat.split <- by.hand("pue-15") # 2013 pri-pvem to pri
dat.split <- by.hand("que-11") # 2000 pt-conve-psn to pt
dat.split <- by.hand("que-12") # 2003 pri-pvem to pri
dat.split <- by.hand("que-13") # 2006 pri-pvem to pri
dat.split <- by.hand("que-14") # 2009 pri-pna to pri
dat.split <- by.hand("que-15") # 2012 pri-pvem-pna to pri
dat.split <- by.hand("qui-13.009") # 2009 tulum--new pri-pvem to pri
dat.split <- by.hand("qui-15") # 2013 pri-pvem-pna to pri
dat.split <- by.hand("san-11") # 2000 prd-etc to prd
dat.split <- by.hand("san-12") # 2003 pri-pvem to pri
dat.split <- by.hand("sin-11") # 2001 pt-pas to pt
dat.split <- by.hand("sin-13") # 2007 pri-pna to pri
dat.split <- by.hand("sin-15") # 2013 pri-pvem-pna to pri
dat.split <- by.hand("son-12") # 2003 pas-mexpos to pas
dat.split <- by.hand("son-13") # 2006 pri-pna to pri
dat.split <- by.hand("son-14") # 2009 pri-pvem to pri (except few cases where pvem nominated in 2006)
dat.split <- by.hand("son-15.029") # 2012 pri-pvem to pri
dat.split <- by.hand("son-15.030") # 2012 pri-pvem to pri
dat.split <- by.hand("tam-08") # 1992 pan-prd to pan
dat.split <- by.hand("tam-11.021") # 2001 pt-pvem to pt
dat.split <- by.hand("tam-11.032") # 2001 pt-pvem-pas-local to pt
dat.split <- by.hand("tam-13") # 2007 pri-pna to pri
dat.split <- by.hand("tam-14") # 2010 pri-pna or pri-pvem-pna to pri
dat.split <- by.hand("tam-15") # 2013 pri-pvem-pna to pri
dat.split <- by.hand("tla-07") # 1988 prd-fdn to prd
dat.split <- by.hand("tla-11.016") # 2002 pt-oth to pt
dat.split <- by.hand("tla-11.044") # 2002 pas-loc to pas
dat.split <- by.hand("tla-12") # 2004 pri-pvem to pri
dat.split <- by.hand("tla-13") # 2007 pri-pvem to pri pan-pac to pan
dat.split <- by.hand("tla-14") # 2010 pri-pvem to pri pan-panal-pac to pan
dat.split <- by.hand("tla-15") # 2013 pt-pac to pt pvem-ps to pvem
dat.split <- by.hand("tla-16") # 2016 pvem-ps to pvem
dat.split <- by.hand("ver-11") # 2000 pri-chicos to pri
dat.split <- by.hand("yuc-11") # 2001 pt-conve to pt
dat.split <- by.hand("yuc-13") # 2007 pt-conve to pt
dat.split <- by.hand("yuc-16") # 2015 pes-ph to pes
##
table(dat.split$dhascoal) ## check no coals to split missing
##dat.split$emm[which(dat.split$dhascoal==1)]
##
## drop little-use columns
dat.split$dhascoal <- dat.split$dcoal <- NULL
dat.split$ord <- dat.split$fuente <- NULL
dat.split$tot <- dat.split$nr <- dat.split$nulos <- NULL
##
## # debug
## tmp <- dat.split[dat.split$dhascoal==1,]; i <- 1
## tmp[i,]
## tmp[i,grep("^l[0-9]{2}", colnames(dat.split))]
## tmp$emm[i] 
## tmp$yr[i] 
## #
## pat <- "sin-13.018"
## tmp2 <- dat.split[grep(pat, dat.split$emm), grep("^[lv][0-9]{2}", colnames(dat.split))]
## #tmp2
## round(tmp2$v02/(tmp2$v02+tmp2$v05), 3) - .001
## table(dat.split$dhascoal)

##########
## save ##
##########
## repeat this block from above, in case dat.split is sorted differently

## drop these obs for analysis
table(dat.split$status)
#drop.r <- grep("cancelled|missing|litigio|pending", dat$status)
drop.r <- grep("pending", dat$status)

## could import ncand from dat here
table(dat$emm==dat.split$emm)
dat.split$ncand <- dat$ncand

## drop these cols to trim file size for gsheets
drop.c <- c("ord", "status", "dcoal", "win", "nr", "nulos", "tot", "fuente", "notas",
            "date", "edon", "dextra", "coal1", "coal2", "coal3", "coal4", "check", "dpre1997") # drops these additional cols to accommodate 4 more v/l cols
ncol(dat.split[, colnames(dat.split) %notin% drop.c])
drop.c <- which(colnames(dat.split) %in% drop.c)

## ####################################################
## ## OPTIONAL: sort coalition-split data columnwise ##
## ####################################################
## ## Extract vote and label objects for manip
## sel.l <- grep("^l[0-9]{2}", colnames(dat.split))
## sl <- dat.split[,sel.l] # subset label columns
## sel.v <- grep("^v[0-9]{2}", colnames(dat.split))
## sv <- dat.split[,sel.v] # subset vote columns
## #########################################
## tail(sv)
## tail(sl)
## ###########################################
## sv.sorted <- sortBy(target = sv, By = sv) # slow! better wait for process end before continuing  
## ###########################################
## sl.sorted <- sortBy(target = sl, By = sv) # slow! better wait for process end before continuing
## ###########################################
## sv.sorted <- as.data.frame(sv.sorted, stringsAsFactors = FALSE) # return matrix to dataframe
## sl.sorted <- as.data.frame(sl.sorted, stringsAsFactors = FALSE) # return matrix to dataframe
## colnames(sv.sorted) <- colnames(v); colnames(sl.sorted) <- colnames(l)
## sv.sorted <- transform(sv.sorted, v01 = as.numeric(v01), v02 = as.numeric(v02), v03 = as.numeric(v03), v04 = as.numeric(v04), v05 = as.numeric(v05), v06 = as.numeric(v06), v07 = as.numeric(v07), v08 = as.numeric(v08), v09 = as.numeric(v09), v10 = as.numeric(v10), v11 = as.numeric(v11), v12 = as.numeric(v12), v13 = as.numeric(v13), v14 = as.numeric(v14), v15 = as.numeric(v15), v16 = as.numeric(v16), v17 = as.numeric(v17), v18 = as.numeric(v18), v19 = as.numeric(v19) , v20 = as.numeric(v20) , v21 = as.numeric(v21) , v22 = as.numeric(v22) , v23 = as.numeric(v23) , v24 = as.numeric(v24) , v25 = as.numeric(v25)) # return to numeric format
## tail(sv.sorted)
## tail(sl.sorted)
## ##
## ## Return manipulated columns to data
## #sel.l <- grep("^l[0-9]{2}", colnames(dat.split))
## sl.sorted -> dat.split[,sel.l] # subset label columns
## #sel.v <- grep("^v[0-9]{2}", colnames(dat.split))
## sv.sorted -> dat.split[,sel.v] # subset vote columns

if (length(drop.r)>0) {
    dat.split2 <- dat.split[-drop.r,]
} else {
    dat.split2 <- dat.split
}
## subsets by yrs
sel1 <- which(dat.split2$yr>=1970 & dat.split2$yr<1982)
sel2 <- which(dat.split2$yr>=1982 & dat.split2$yr<1990)
sel3 <- which(dat.split2$yr>=1990 & dat.split2$yr<2000)
sel4 <- which(dat.split2$yr>=2000 & dat.split2$yr<2010)
sel5 <- which(dat.split2$yr>=2010 & dat.split2$yr<2020)
sel6 <- which(dat.split2$yr>=2020 & dat.split2$yr<2030)

length(sel1); length(sel1)*59 < 400000 # 400k cells is gsheets max
length(sel2); length(sel2)*59 < 400000
length(sel3); length(sel3)*59 < 400000
length(sel4); length(sel4)*59 < 400000
length(sel5); length(sel5)*59 < 400000
length(sel6); length(sel6)*59 < 400000
##6756*58

## save full file
write.csv(dat.split2, file = "aymu1970-on.coalSplit.csv", row.names = FALSE)  ## use dat.split instead if keeping drop.r obs


## subset ~1970s
if (length(drop.r)>0) {  # restore for manipulation
    dat.split2 <- dat.split[-drop.r, -drop.c]
} else {
    dat.split2 <- dat.split[       , -drop.c]
}
dat.split2 <- dat.split2[sel1,] # subset
write.csv(dat.split2, file = "smaller-for-gsheets/aymu.coalSplit1970s.csv", row.names = FALSE)
## subset ~1980s (1980s need to be split due to oax pre-usos y costumbres) 
dat.split2 <- dat.split[-drop.r, -drop.c] # restore for manipulation
dat.split2 <- dat.split2[sel2,] # subset
write.csv(dat.split2, file = "smaller-for-gsheets/aymu.coalSplit1980s.csv", row.names = FALSE)
## subset 1990s
if (length(drop.r)>0) {  # restore for manipulation
    dat.split2 <- dat.split[-drop.r, -drop.c]
} else {
    dat.split2 <- dat.split[       , -drop.c]
}
dat.split2 <- dat.split2[sel3,] # subset
write.csv(dat.split2, file = "smaller-for-gsheets/aymu.coalSplit1990s.csv", row.names = FALSE)
## subset 2000s
if (length(drop.r)>0) {  # restore for manipulation
    dat.split2 <- dat.split[-drop.r, -drop.c]
} else {
    dat.split2 <- dat.split[       , -drop.c]
}
dat.split2 <- dat.split2[sel4,] # subset
write.csv(dat.split2, file = "smaller-for-gsheets/aymu.coalSplit2000s.csv", row.names = FALSE)
## subset 2010s
if (length(drop.r)>0) {  # restore for manipulation
    dat.split2 <- dat.split[-drop.r, -drop.c]
} else {
    dat.split2 <- dat.split[       , -drop.c]
}
dat.split2 <- dat.split2[sel5,] # subset
write.csv(dat.split2, file = "smaller-for-gsheets/aymu.coalSplit2010s.csv", row.names = FALSE)
## subset 2020s
if (length(drop.r)>0) {  # restore for manipulation
    dat.split2 <- dat.split[-drop.r, -drop.c]
} else {
    dat.split2 <- dat.split[       , -drop.c]
}
dat.split2 <- dat.split2[sel6,] # subset
write.csv(dat.split2, file = "smaller-for-gsheets/aymu.coalSplit2020s.csv", row.names = FALSE)



#########################################
## SUBSET INCUMBENTS FOR GOOGLE SHEETS ##
#########################################
dat.i <- read.csv(file = "aymu1989-on.incumbents.csv", stringsAsFactors = FALSE)
quantile(dat.i$edon, probs = c(.33,.5,.67))
write.csv(dat.i[dat.i$edon <15,],                 file = "smaller-for-gsheets/aymu.incumbents-ags-jal.csv", row.names = FALSE)
write.csv(dat.i[dat.i$edon>=15 & dat.i$edon<22,], file = "smaller-for-gsheets/aymu.incumbents-mex-pue.csv", row.names = FALSE)
write.csv(dat.i[dat.i$edon>=22,],                 file = "smaller-for-gsheets/aymu.incumbents-que-zac.csv", row.names = FALSE)
## write.csv(dat.i[dat.i$edon <20,], file = "smaller-for-gsheets/aymu.incumbents-ags-nl.csv", row.names = FALSE)
## write.csv(dat.i[dat.i$edon>=20,], file = "smaller-for-gsheets/aymu.incumbents-oax-zac.csv", row.names = FALSE)

## clean
rm(tmp,sel.l,sel.r,sel.v,i)


## # check lisnom
## sel.c <- grep("^v", colnames(dat))
## summary(dat$lisnom - rowSums(dat[,sel.c]))
## #
## sel <- which(dat$lisnom - rowSums(dat[,sel.c]) < 0 & dat$yr > 1997)
## length(sel)
## #
## dat[sel[2],]


## # save coalition weights and info
## save(coal.weights # list with total vote
##    , coal.info # data frame with coalitions
##      , file = "aymu1989-on.coalAgg-weights-info.RData")
## coal.weights[100]
## coal.info[100,]
## dat.orig[100,]
