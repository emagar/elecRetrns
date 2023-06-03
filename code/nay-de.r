rm(list = ls())
##
dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
setwd(dd)

## read data
dat <- read.csv(file = "ayde2008-on-Nayarit-regid.csv")
dat[1,]
##
## drop void elections
sel <- which(dat$status=="anulada")
##dat[sel,]
if (length(sel)>0) dat <- dat[-sel,]

## subset label and vote columns for manipulation
sel.l <- grep("^l[0-9]{2}", colnames(dat))
l <- dat[,sel.l] # subset label columns
sel.v <- grep("^v[0-9]{2}", colnames(dat))
v <- dat[,sel.v] # subset vote columns
sel.c <- grep("^c[0-9]{2}", colnames(dat))
c <- dat[,sel.c] # subset cand columns
##
# in object v, replace NAs with 0s
v[is.na(v)] <- 0
##
## in object c, replace c03 c05 with actual candidate names
for (i in 1:nrow(c)){
    ##i <- 691 # debug
    tmp <- c[i,] ## subset one line of object c
    sel <- grep("c[0-9]{2}", tmp) ## spot c01 c01 etc
    if (length(sel)==0) next
    for (j in sel) {
        ##j <- 15 # debug
        tmp[j] <- tmp[eval(as.character(tmp[j]))] ## replace tmp[j] with target name
    }
    c[i,] <- tmp # return names to candidate object
}
##
## recompute efec
dat$efec <- rowSums(v)


################################################
## Duplicate data for split-vote manipulation ##
################################################
dat4split <- dat


###############################
## AGGREGATE COALITION VOTES ##
###############################
## dummy had at least one coalition
dat$dcoal <- 0
for (i in 1:ncol(l)){
    dat$dcoal[grep("-", l[,i])] <- 1
}
## create objects with vote for coalition(s) added and redudant columns dropped
cv <- v; # will receive votes with coalitions aggregated
cl <- l; # will keep coalition labels but drop coalition member labels
cc <- c; # will keep coalition candidates but drop redundant names
##
ci <- data.frame(dcoal=dat$dcoal, ncoal=NA, coal1="none", coal2="none", coal3="none", coal4="none", stringsAsFactors = FALSE) # coalition summary info
ci$ncoal[ci$dcoal==0] <- 0 # 0=no coalition
## ci$coal1 coal2 coal3 coal4 pre-filled
## n is object reporting how many parties reported in a v/l cell?
ln1 <- v # duplicate votes
ln1[] <- 0 # will receive info
ln2 <- ln1 # duplicate
## replace each label by its character length
for (i in 1:ncol(ln1)){
    ##i <- 16 # debug
    tmp <- l[,i] # pick ith column as vector
    ln1[,i] <- nchar(tmp) # replace items with num characters
}
## drop hyphens to count character difference
l.tmp <- l # duplicate
for (i in 1:ncol(l.tmp)){
    ##i <- 1 # debug
    l.tmp[,i] <- gsub("-", "", l.tmp[,i]) # remove hyphens to count char dif
    ln2[,i] <- nchar(l.tmp[,i]) # replace items with num characters
}
n <- ln1 - ln2 + 1 # 1 means no coalition, single party
n[l=="0"] <- 0     # empty votes
rm(i, ln1, ln2, l.tmp, sel, tmp)
##
max.tmp <- apply(n, 1, max) # max parties reported in a row's cell
table(max.tmp) # coal w most members has 4
I <- nrow(v)
c4 <- c3 <- c2 <- c1 <- as.data.frame(matrix("0", I, max(max.tmp)), col.names = paste("p", 1:max(max.tmp), sep = ""), stringsAsFactors = FALSE) ## will receive vector of 1st 2nd 3rd 4th coalition's members

## fill in info selecting cases of coalitions with most members 
sel7 <- which(max.tmp>1) 
##grep("ags-18", dat$emm[sel7]) # debug
tmp.v  <- cv[sel7,] # subset for manipulation --- 3sep2021 it used to pick from v, forgetting manip in later rounds
tmp.l  <- cl[sel7,]
tmp.c  <- cc[sel7,]
tmp.n  <- n[sel7,]
tmp.c1 <- c1[sel7,]
max.tmp <- max.tmp[sel7]
tmp.ci <- ci[sel7,]

for (i in 1:length(sel7)){
    ##i <- length(sel7) # debug
    ##i <- 50 # debug
    ##tmp.l[i,]
    message(sprintf("loop %s of %s", i, length(sel7)))
    tmp.ci$ncoal[i] <- 1                               # running tally ncoal
    save.col <- which(tmp.n[i,]==max.tmp[i])[1]        # spare this column from erasure in process below (1st if multiple hits)
    save.label <- tmp.l[i, save.col]                   # keep coalition full label
    save.cand <-  tmp.c[i, save.col]                   # keep coalition candidate
    tmp.ci$coal1[i] <- save.label                      # plug coal label into summary
    tmp <- strsplit(save.label, split = "-")           # break into component character vector
    tmp.c1[i,1:length(tmp[[1]])] <- tmp[[1]]           # fill in coalition members
    #
    target.cols <- numeric()                           # initialize empty vector for vote sum
    for (j in 1:4){                                    # 4 is max observed partners in a coalition
        ##j <- 1 # debug
        if (tmp.c1[i,j]=="0") next
        pat <- paste0("^", tmp.c1[i,j], "$|",
                      tmp.c1[i,j], "-|-", tmp.c1[i,j]) # searches ^pty$, pty- or -pty
        tmp.target <- grep(pattern = pat, x = tmp.l[i,])
        if (length(tmp.target)>0) {
            target.cols <- c(target.cols, tmp.target)
        }
    }
    target.cols <- unique(target.cols); target.cols <- target.cols[order(target.cols)]
    ##tmp.w1[[i]] <- target.cols # cols to use when replacing coalition members' contributed votes
    sel <- setdiff(target.cols, save.col)  # setdiffs joint votes
    if (length(sel)==0) sel <- target.cols # or all if singleton (ie setdiff empty)
    ##tmp.vw1[[i]] <- tmp.v[i, sel] # keep to fill coalition members' contribution 
    save.vote <- sum(tmp.v[i,target.cols])
    ##tmp.vw1[[i]] <- tmp.vw1[[i]] / sum(tmp.v[i,sel])   # relative contributions
    ##tmp.vw1[[i]] <- round(tmp.vw1[[i]] * save.vote, 2) # relative votes contributed
    if (length(sel)>1){ # do not manipulate if singleton
        tmp.v[i, target.cols] <- 0   # erase votes to keep only aggregate
        tmp.l[i, target.cols] <- "0" # erase labels to keep only full coalition label manipulation
        tmp.c[i, target.cols] <- "0" # erase names to keep only full coalition candidate manipulation
        tmp.v[i, save.col] <- save.vote  # place aggregate vote back in
        tmp.l[i, save.col] <- save.label # place coalition label back in
        tmp.c[i, save.col] <- save.cand # place coalition label back in
        ## votes contributed
        ##tmp.vw[i, save.col] <- 0   # erase joint votes
        ##tmp.lw[i, save.col] <- "0" # erase joint label
        ##tmp.vw[i, sel] <- tmp.vw1[[i]]  # place contributed votes back in
    }
    tmp.n[i, target.cols] <- 0   # erase ns, prep for obs's next coalition
}
## return to data
cv[sel7,] <- tmp.v
cl[sel7,] <- tmp.l
cc[sel7,] <- tmp.c
n[sel7,]  <- tmp.n
c1[sel7,] <- tmp.c1
##w1[sel7] <- tmp.w1
ci[sel7,] <- tmp.ci
##sv[sel7,] <- tmp.vw
##sl[sel7,] <- tmp.lw
##tail(w1[sel7])

####################################
# process cases with 2nd coalition #
####################################
max.tmp <- apply(n, 1, max) # max parties reported in a row's cell
table(max.tmp) # coal w most members has 7
sel7 <- which(max.tmp>1) 
##grep("ags-18", dat$emm[sel7]) # debug
tmp.v  <- cv[sel7,] # subset for manipulation --- 3sep2021 used to pick from v, forgetting manip in later rounds
tmp.l  <- cl[sel7,]
tmp.c  <- cc[sel7,]
tmp.n <- n[sel7,]
tmp.c2 <- c2[sel7,]
max.tmp <- max.tmp[sel7]
tmp.ci <- ci[sel7,]

for (i in 1:length(sel7)){
    ##i <- 50 # debug
    ##tmp.l[i,] # debug
    ##tmp.n[i,] # debug
    message(sprintf("loop %s of %s", i, length(sel7)))
    tmp.ci$ncoal[i] <- 2               # running tally co ncoal
    save.col <- which(tmp.n[i,]==max.tmp[i])[1] # spare this column from erasure in process below (1st if multiple hits)
    save.label <- tmp.l[i, save.col]            # keep coalition full label
    save.cand <-  tmp.c[i, save.col]            # keep coalition candidate
    tmp.ci$coal2[i] <- save.label      # plug coal label into summary
    tmp <- strsplit(save.label, split = "-") # break into component character vector
    tmp.c2[i,1:length(tmp[[1]])] <- tmp[[1]] # fill in coalition members
    ##
    target.cols <- numeric() # initialize empty vector
    for (j in 1:4){
        ##j <- 2 # debug
        if (tmp.c2[i,j]=="0") next
        pat <- paste("^", tmp.c2[i,j], "$|", tmp.c2[i,j], "-|-", tmp.c2[i,j], sep="") # searches ^pty$, pty- or -pty
        tmp.target <- grep(pattern = pat, x = tmp.l[i,])
        ##tmp.target <- grep(pattern = tmp.c2[i,j], x = tmp.l[i,]) # version hits panal when searching pan
        if (length(tmp.target)>0) {
            target.cols <- c(target.cols, tmp.target)
        }
    }
    target.cols <- unique(target.cols); target.cols <- target.cols[order(target.cols)]
    ##tmp.w2[[i]] <- target.cols # for use when computing coalition members' contribution
    sel <- setdiff(target.cols, save.col)  # setdiffs joint votes
    if (length(sel)==0) sel <- target.cols # or all if singleton
    ##tmp.vw2[[i]] <- tmp.v[i, sel] # keep to fill coalition members' contribution (setdiffs joint votes)
    save.vote <- sum(tmp.v[i,target.cols])
    ##tmp.vw2[[i]] <- tmp.vw2[[i]] / sum(tmp.v[i,sel])   # relative contributions
    ##tmp.vw2[[i]] <- round(tmp.vw2[[i]] * save.vote, 2) # relative votes contributed
    if (length(sel)>1){ # do not manipulate if singleton
        tmp.v[i, target.cols] <- 0   # erase votes to keep only aggregate
        tmp.l[i, target.cols] <- "0" # erase labels to keep only full coalition label manipulation
        tmp.c[i, target.cols] <- "0" # erase names to keep only full coalition candidate manipulation
        tmp.v[i, save.col] <- save.vote  # place aggregate vote back in
        tmp.l[i, save.col] <- save.label # place coalition label back in
        tmp.c[i, save.col] <- save.cand # place coalition label back in
        ## votes contributed
        ##tmp.vw[i, save.col] <- 0   # erase joint votes
        ##tmp.lw[i, save.col] <- "0" # erase joint label
        ##tmp.vw[i, sel] <- tmp.vw2[[i]]  # place contributed votes back in
    }
    tmp.n[i, target.cols] <- 0   # erase ns
}
## return to data
cv[sel7,] <- tmp.v
cl[sel7,] <- tmp.l
cc[sel7,] <- tmp.c
n[sel7,]  <- tmp.n
c2[sel7,] <- tmp.c2
ci[sel7,] <- tmp.ci

####################################
# process cases with 3rd coalition #
####################################
max.tmp <- apply(n, 1, max) # max parties reported in a row's cell
table(max.tmp) # coal w most members has 7
sel7 <- which(max.tmp>1) 
tmp.v  <- cv[sel7,] # subset for manipulation --- 3sep2021 used to pick from v, forgetting manip in later rounds
tmp.l  <- cl[sel7,]
tmp.c  <- cc[sel7,]
tmp.n <- n[sel7,]
tmp.c3 <- c3[sel7,]
max.tmp <- max.tmp[sel7]
tmp.ci <- ci[sel7,]

for (i in 1:length(sel7)){
    ##i <- 1 # debug
    ##tmp.l[i,] # debug
    ##tmp.n[i,] # debug
    message(sprintf("loop %s of %s", i, length(sel7)))
    tmp.ci$ncoal[i] <- 3               # running tally co ncoal
    save.col <- which(tmp.n[i,]==max.tmp[i])[1] # spare this column from erasure in process below (1st if multiple hits)
    save.label <- tmp.l[i, save.col]            # keep coalition full label
    save.cand <-  tmp.c[i, save.col]            # keep coalition candidate
    tmp.ci$coal3[i] <- save.label      # plug coal label into summary
    tmp <- strsplit(save.label, split = "-") # break into component character vector
    tmp.c3[i,1:length(tmp[[1]])] <- tmp[[1]] # fill in coalition members
    ##
    target.cols <- numeric() # initialize empty vector
    for (j in 1:4){
        ##j <- 2 # debug
        if (tmp.c3[i,j]=="0") next
        pat <- paste("^", tmp.c3[i,j], "$|", tmp.c3[i,j], "-|-", tmp.c3[i,j], sep="") # searches ^pty$, pty- or -pty
        tmp.target <- grep(pattern = pat, x = tmp.l[i,])
        ##tmp.target <- grep(pattern = tmp.c3[i,j], x = tmp.l[i,]) # version hits panal when searching pan
        if (length(tmp.target)>0) {
            target.cols <- c(target.cols, tmp.target)
        }
    }
    target.cols <- unique(target.cols); target.cols <- target.cols[order(target.cols)]
    ##tmp.w3[[i]] <- target.cols # for use when computing coalition members' contribution
    sel <- setdiff(target.cols, save.col)  # setdiffs joint votes
    if (length(sel)==0) sel <- target.cols # or all if singleton
    ##tmp.vw3[[i]] <- tmp.v[i, sel] # keep to fill coalition members' contribution (setdiffs joint votes)
    save.vote <- sum(tmp.v[i,target.cols])
    ##tmp.vw3[[i]] <- tmp.vw3[[i]] / sum(tmp.v[i,sel])   # relative contributions
    ##tmp.vw3[[i]] <- round(tmp.vw3[[i]] * save.vote, 2) # relative votes contributed
    if (length(sel)>1){ # do not manipulate if singleton
        tmp.v[i, target.cols] <- 0   # erase votes to keep only aggregate
        tmp.l[i, target.cols] <- "0" # erase labels to keep only full coalition label manipulation
        tmp.c[i, target.cols] <- "0" # erase names to keep only full coalition candidate manipulation
        tmp.v[i, save.col] <- save.vote  # place aggregate vote back in
        tmp.l[i, save.col] <- save.label # place coalition label back in
        tmp.c[i, save.col] <- save.cand # place coalition label back in
        ## votes contributed
        ##tmp.vw[i, save.col] <- 0   # erase joint votes
        ##tmp.lw[i, save.col] <- "0" # erase joint label
        ##tmp.vw[i, sel] <- tmp.vw3[[i]]  # place contributed votes back in
    }
    tmp.n[i, target.cols] <- 0   # erase ns
}
## return to data
cv[sel7,] <- tmp.v
cl[sel7,] <- tmp.l
cc[sel7,] <- tmp.c
n[sel7,]  <- tmp.n
c3[sel7,] <- tmp.c3
ci[sel7,] <- tmp.ci

##############################################
## CHECK NO COALITIONS REMAIN UNMANIPULATED ##
##############################################
max.tmp <- apply(n, 1, max) # max parties reported in a row's cell
print("Table must have 0s and 1s only, else coalitions remain to manipulate"); table(max.tmp) # must have 0s and 1s only (number of parties being reported by remaining columns)

## plug ncoal into data
dat$ncoal  <- ci$ncoal
## number of candidates
dat$ncand <- apply(cv, 1, FUN = function(x) sum(as.numeric(x>0)))
##
rm(c1, c2, c3, c4, I, i, j, max.tmp, n, pat, save.col, save.label, save.vote, sel, sel7, target.cols, tmp, tmp.c1, tmp.c2, tmp.c3, tmp.ci, tmp.l, tmp.n, tmp.target, tmp.v) # housecleaning

############################################
## SORTS COLUMNS IN VOTE DECREASING ORDER ##
############################################
## Reads sortBy function
pth <- ifelse (Sys.info()["user"] %in% c("eric", "magar"),
    "~/Dropbox/data/useful-functions",
    "https://raw.githubusercontent.com/emagar/useful-functions/master"
    )
source( paste(pth, "sortBy.r", sep = "/") )
rm(pth)

## sort coalition-aggregated data
tail(cv)
tail(cl)
tail(cc)
###########################################
cv.sorted <- sortBy(target = cv, By = cv) # slow! better wait for process end before continuing  
###########################################
cl.sorted <- sortBy(target = cl, By = cv) # slow! better wait for process end before continuing
###########################################
cc.sorted <- sortBy(target = cc, By = cv) # slow! better wait for process end before continuing
###########################################
cv.sorted <- as.data.frame(cv.sorted, stringsAsFactors = FALSE) # return matrix to dataframe
cl.sorted <- as.data.frame(cl.sorted, stringsAsFactors = FALSE) # return matrix to dataframe
cc.sorted <- as.data.frame(cc.sorted, stringsAsFactors = FALSE) # return matrix to dataframe
colnames(cv.sorted) <- colnames(v); colnames(cl.sorted) <- colnames(l); colnames(cc.sorted) <- colnames(c)
cv.sorted <- transform(cv.sorted, v01 = as.numeric(v01), v02 = as.numeric(v02), v03 = as.numeric(v03), v04 = as.numeric(v04), v05 = as.numeric(v05), v06 = as.numeric(v06), v07 = as.numeric(v07), v08 = as.numeric(v08), v09 = as.numeric(v09), v10 = as.numeric(v10), v11 = as.numeric(v11), v12 = as.numeric(v12), v13 = as.numeric(v13), v14 = as.numeric(v14), v15 = as.numeric(v15)) # return to numeric format
tail(cv.sorted)
tail(cl.sorted)
tail(cc.sorted)

## rename objects so that dat now has coalition aggregates
dat.orig <- dat # duplicate original data
dat[,sel.l] <- cl.sorted # return manipulated labels to data
dat[,sel.v] <- cv.sorted # return manipulated votes to data
dat[,sel.c] <- cc.sorted # return manipulated votes to data
head(dat)

## move dcoal and ncoal columns before v01
tmp <- dat # duplicate if I mess up
tmp1 <- grep("v01", colnames(dat))
tmp2 <- grep("dcoal", colnames(dat))
tmp3 <- grep("ncoal", colnames(dat))
tmp4 <- grep("ncand", colnames(dat))
dat <- dat[, c(1:(tmp1-1), tmp2, tmp3, tmp4, tmp1:(tmp2-1))]
colnames(dat)
dat[1,]
rm(cv, cl, cv.sorted, cl.sorted, sel.l, sel.v, v, l, tmp, tmp1, tmp2, tmp3)

## # only 11 cols needed
table(dat$v12) # check that only has zeroes
## # if not, which cases remain? 
## #sel <- which(dat$v14>0)
## #dat[sel,]
dat$v12 <- dat$v13 <- dat$v14 <- dat$v15 <- NULL # drop redundant columns
dat$l12 <- dat$l13 <- dat$l14 <- dat$l15 <- NULL # drop redundant columns
dat$c12 <- dat$c13 <- dat$c14 <- dat$c15 <- NULL # drop redundant columns
dat$win <- dat$l01
## move win column before v01
tmp <- dat # duplicate if I mess up
tmp1 <- grep("v01", colnames(dat))
tmp2 <- grep("win", colnames(dat))
dat <- dat[, c(1:(tmp1-1), tmp2, tmp1:(tmp2-1))]
colnames(dat)

sel.l <- grep("^l[0-9]{2}", colnames(dat))
l <- dat[,sel.l] # subset label columns
sel.v <- grep("^v[0-9]{2}", colnames(dat))
v <- dat[,sel.v] # subset vote columns
sel.c <- grep("^c[0-9]{2}", colnames(dat))
c <- dat[,sel.c] # subset vote columns
## check that coal aggregation produces same efec as before
check <- rowSums(v) - dat$efec
table(check==0) # all must be true
##sel <- which(check!=0)
##dat[sel[2],]
##dat.orig[sel[2],]

#######################################################
## Export a coalAgg version of votes returns to the  ##
## same directory where ayde2008-presentNayRegid.csv ##
#######################################################
## clean NAs an zeroes
dat[is.na(dat)] <- ""
sel.r <- which(dat$c11==0)
if (length(sel.r)>0) dat$c11[sel.r] <- ""
if (length(sel.r)>0) dat$v11[sel.r] <- ""
if (length(sel.r)>0) dat$l11[sel.r] <- ""
sel.r <- which(dat$c10==0)
if (length(sel.r)>0) dat$c10[sel.r] <- ""
if (length(sel.r)>0) dat$v10[sel.r] <- ""
if (length(sel.r)>0) dat$l10[sel.r] <- ""
sel.r <- which(dat$c09==0)
if (length(sel.r)>0) dat$c09[sel.r] <- ""
if (length(sel.r)>0) dat$v09[sel.r] <- ""
if (length(sel.r)>0) dat$l09[sel.r] <- ""
sel.r <- which(dat$c08==0)
if (length(sel.r)>0) dat$c08[sel.r] <- ""
if (length(sel.r)>0) dat$v08[sel.r] <- ""
if (length(sel.r)>0) dat$l08[sel.r] <- ""
sel.r <- which(dat$c07==0)
if (length(sel.r)>0) dat$c07[sel.r] <- ""
if (length(sel.r)>0) dat$v07[sel.r] <- ""
if (length(sel.r)>0) dat$l07[sel.r] <- ""
sel.r <- which(dat$c06==0)
if (length(sel.r)>0) dat$c06[sel.r] <- ""
if (length(sel.r)>0) dat$v06[sel.r] <- ""
if (length(sel.r)>0) dat$l06[sel.r] <- ""
sel.r <- which(dat$c05==0)
if (length(sel.r)>0) dat$c05[sel.r] <- ""
if (length(sel.r)>0) dat$v05[sel.r] <- ""
if (length(sel.r)>0) dat$l05[sel.r] <- ""
##
sel.r <- which(dat$v11==0 & dat$l11=="")
if (length(sel.r)>0) dat$v11[sel.r] <- ""
sel.r <- which(dat$v10==0 & dat$l10=="")
if (length(sel.r)>0) dat$v10[sel.r] <- ""
sel.r <- which(dat$v09==0 & dat$l09=="")
if (length(sel.r)>0) dat$v09[sel.r] <- ""
sel.r <- which(dat$v08==0 & dat$l08=="")
if (length(sel.r)>0) dat$v08[sel.r] <- ""
sel.r <- which(dat$v07==0 & dat$l07=="")
if (length(sel.r)>0) dat$v07[sel.r] <- ""
sel.r <- which(dat$v06==0 & dat$l06=="")
if (length(sel.r)>0) dat$v06[sel.r] <- ""
sel.r <- which(dat$v05==0 & dat$l05=="")
if (length(sel.r)>0) dat$v05[sel.r] <- ""
sel.r <- which(dat$v04==0 & dat$l04=="")
if (length(sel.r)>0) dat$v04[sel.r] <- ""
sel.r <- which(dat$v03==0 & dat$l03=="")
if (length(sel.r)>0) dat$v03[sel.r] <- ""
##
dat$fuente <- dat$notes <- NULL # drop these columns
dat[1,]
##dat$ord <- 1:nrow(dat)
write.csv(dat, file = "ayde2008-on-Nayarit-regid.coalAgg.csv", row.names = FALSE)


################################
## Generate splitCoal version ##
################################
## rename object
dat.split <- dat4split
rm(dat4split)
## verify dimensionality
table(dat.split$emm == dat$emm)
## import useful vars
dat.split$dcoal <- dat$dcoal
dat.split$ncoal <- dat$ncoal
dat.split$ncand <- dat$ncand
## drop candidates, reported in coalagg object
sel.c <- grep("^c[0-9]{2}", colnames(dat.split))
dat.split <- dat.split[,-sel.c]
rm(sel.c)

##################################################################################
## Up to 2021, only the 2021 election allows to split coalition votes, and they ##
## already come pre-split from the source. If the source offered splitable data ##
## in the future, import code to carry this from ay.r.                          ##
## Save output                                                                  ##
##################################################################################
dat.split[is.na(dat.split)] <- ""
dat.split$fuente <- dat.split$notes <- NULL # drop these columns
dat.split[1,]
write.csv(dat.split, file = "ayde2008-on-Nayarit-regid.coalSplit.csv", row.names = FALSE)


