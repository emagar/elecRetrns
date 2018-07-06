rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
setwd(dd)

# read raw data file
dat <- read.csv(file = "dfdf1979-on.csv", stringsAsFactors = FALSE)
dim(dat)

dat$pancoal <- dat$pricoal <- dat$pricoal <- NULL # drop coal cols, confuses code (taken from ay.r)

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
l    l[,i] <- gsub(pattern = "-ps-", replacement = "-ps1-", l[,i])
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


OJO: NEED TO TAKE WHAT COMES BELOW NEXT COMMENT FROM ay.r
######################################################
######################################################
# identify coalitions by searching for "-" in labels #
######################################################
######################################################

# process 2015 only to produce national aggregates
sel <- which(dat$yr==2015)
d <- dat[sel,]

# replace extraordinaria ags
sel <- which(d$nota=="extraordinaria")
tmp <- d[sel,]
d <- d[-sel,]
sel <- which(d$edon==tmp$edon & d$disn==tmp$disn)
d[sel,] <- tmp
rm(tmp)


sel.l <- grep("^l[0-9]{2}", colnames(dat))
l <- d[,sel.l] # subset labels columns
colnames(l)

table(l[,1])
table(l[,2])
table(l[,3])
table(l[,4])
table(l[,5])
table(l[,6])
table(l[,7])
table(l[,8])
table(l[,9])
table(l[,10])
table(l[,11])
table(l[,12])
table(l[,13])
table(l[,14])

d$pan <- 

colnames(dat)
dat$nota


dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data"

setwd("/home/eric/Desktop/MXelsCalendGovt/presentations/wilsonCenter2018")





head(dat)

dat$pan <- 0

sel <- grep("l[0-9]{2}", colnames(d)) # select labels cols

fn <- function(x){which(x=="pan")}
apply(x = d[,sel], 1, FUN=fn)

which(d[1,sel]=="pan")
