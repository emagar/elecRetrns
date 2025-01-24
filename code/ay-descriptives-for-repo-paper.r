rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/"
setwd(dd)

#####################
## Handy functions ##
#####################
pth <- ifelse (Sys.info()["user"] %in% c("eric", "magar"),
    "~/Dropbox/data/useful-functions",
    "https://raw.githubusercontent.com/emagar/useful-functions/master"
    )
source( paste(pth, "edo2edon.r", sep = "/") )  ## translates state codes
source( paste(pth, "inegi2ife.r", sep = "/") ) ## translates municipio codes
rm(pth)

#####################
## Read data files ##
#####################
## choose one
d <- read.csv(file = "aymu1970-on.csv"          , stringsAsFactors = FALSE) # raw
d <- read.csv(file = "aymu1970-on.coalAgg.csv"  , stringsAsFactors = FALSE) # coalitions aggregated
d <- read.csv(file = "aymu1970-on.coalSplit.csv", stringsAsFactors = FALSE) # split coalitions

##################################################################################
## Election cycle from emm (consecutive but not necessarilly aligned w federal) ##
##################################################################################
d$cycle <- sub(pattern="^[a-z]+.([0-9]+).*$", replacement = "\\1", d$emm, perl=TRUE)
d$cycle <- as.numeric(d$cycle)
table(d$cycle)
##
###########################################################################
## Use yr to make cycle aligned w federal (some extras may fall in next) ##
###########################################################################
d$cyclef <- cut(d$yr, breaks = seq(1970,2027, by=3), right = FALSE)
table(d$cyclef)
## ## This other, more precise route aborted, too many exceptions
## ## make coa cycle 13 cycle 14 and so forth
## sel <- which(d$edon==5 & d$cycle>=13)
## d$cycle[sel] <- d$cycle[sel] + 1
## ## make hgo cycle 15 cycle 16, cycle 16 cycle 18, cycle 17 cycle 18
## sel <- which(d$edon==13 & d$cycle==17)
## d$cycle[sel] <- d$cycle[sel] + 2
## sel <- which(d$edon==13 & d$cycle==16)
## d$cycle[sel] <- d$cycle[sel] + 2
## sel <- which(d$edon==13 & d$cycle==15)
## d$cycle[sel] <- d$cycle[sel] + 1
## ## make mex cycle 10 cycle 11 and so forth
## sel <- which(d$edon==15 & d$cycle>=10)
## d$cycle[sel] <- d$cycle[sel] + 1
## ## make ver cycle 17 cycle 18 and so forth
## sel <- which(d$edon==30 & d$cycle>=17)
## d$cycle[sel] <- d$cycle[sel] + 1
## ## create cycle2 that actually follows fed three-year terms
## d$cycle2 <- (d$cycle-1)*3+1970
## d$cycle2 <- paste(d$cycle2,d$cycle, sep="-")
## table(d$yr[d$edon==30],d$cycle2[d$edon==30])
## table(d$yr,d$cycle2)
## which(d$cycle2==1994 & d$yr==1997)


##############
## Coverage ##
##############
##
## drop cancelled and runoff races 
table(d$status)
sel.r <- grep("cancelled|to-runoff", d$status); ds <- ds <- d[-sel.r,] # subset wo cancelled races keeps actual number
##
i <- 0
i <- i+1; edon2edo(i); table(ds$cycle[ds$edon==i]); table(ds$yr[ds$edon==i])
## Overall
table(ds$cycle)
table(ds$yr)
##
d2 <- read.csv(file = "../ancillary/mun.yrs.csv"          , stringsAsFactors = FALSE) # raw
d2[1,]
table(d2$yr01, d2$edo, useNA = "ifany")
table(d2$yr01,         useNA = "ifany")
table(is.na(d2$yr01))
table(is.na(d2$yr02))
table(is.na(d2$yr03))
table(is.na(d2$yr04))
table(is.na(d2$yr05))
table(is.na(d2$yr06))
table(is.na(d2$yr07))
table(is.na(d2$yr08))
table(is.na(d2$yr09))
table(is.na(d2$yr10))
table(is.na(d2$yr11))
table(is.na(d2$yr12))
table(is.na(d2$yr13))
table(is.na(d2$yr14))
table(is.na(d2$yr15))
table(is.na(d2$yr16))
table(is.na(d2$yr17))
table(is.na(d2$yr18))
table(is.na(d2$yr19))

#####################################
## drop un-analyzable obs for good ##
#####################################
sel.r <- grep("cancelled|to-runoff", d$status); d <- d[-sel.r,] # subset wo cancelled races keeps actual number
sel.r <- grep("missing|appointed|litigio", d$status); d <- d[-sel.r,]
sel.r <- grep("proj", d$status); d <- d[-sel.r,] ## drop missing cases projected from df for lags
table(d$status)


#########################################
## prep party shares w split coal data ##
#########################################
d5 <- d
d5$pan    <- 0
d5$pri    <- 0
d5$prd    <- 0
d5$morena <- 0
d5$pvem   <- 0
d5$oth    <- 0
sel <- grep("v[0-9]{2}", colnames(d5)); v <- d5[,sel]; d5 <- d5[,-sel] ## extract votes
sel <- grep("l[0-9]{2}", colnames(d5)); l <- d5[,sel]; d5 <- d5[,-sel] ## extract party labels
## revise efec
d5$efec <- rowSums(v)
## replace indep_name w indep
for (i in 1:ncol(l)){
    l[grep("indep_", l[,i]),i] <- "indep"
}
## initial assignment pan pri etc
tmp <- rep(0,nrow(d5)); ddone <- data.frame(pan=tmp, pri=tmp, prd=tmp, morena=tmp, pvem=tmp) ## will indicate solo vote has been assigned
for (i in 1:ncol(l)){
    #i <- 1 # debug
    sel <- grep("^pan$", l[,i]) ## find pan solo obs and assign to $pan
    d5$pan[sel] <- v[sel,i]; ddone$pan[sel] <- 1
    sel <- grep("^pri$", l[,i]) ## find pri solo obs and assign to $pri
    d5$pri[sel] <- v[sel,i]; ddone$pri[sel] <- 1
    sel <- grep("^prd$", l[,i]) ## find prd solo obs and assign to $prd
    d5$prd[sel] <- v[sel,i]; ddone$prd[sel] <- 1
    sel <- grep("^morena$", l[,i]) ## find morena solo obs and assign to $morena
    d5$morena[sel] <- v[sel,i]; ddone$morena[sel] <- 1
    sel <- grep("^pvem$", l[,i]) ## find pvem solo obs and assign to $pvem
    d5$pvem[sel] <- v[sel,i]; ddone$pvem[sel] <- 1
}
## second-step assignment: for cases where no breakdown, all coal vote to pty (except pan-pri...)

## full list of labels


table(as.matrix(l))
x

######################################
## plot party shares by year/cyclef ##
######################################
plot(x=c(1979,2024), y=c(0,100), type="n", axes = FALSE, xlab = "", ylab = "% vote")
axis(1, at = seq(1979,2024,1), labels = FALSE)
tmp <- c(1979, paste0("'", seq(82,97,3)), 2000, paste0("'", formatC(seq(3,21,3), width = 2, format = "d", flag = "0")), 2024) # xlabels
axis(1, at = seq(1979,2024,3), labels = tmp)
axis(2, at = seq(0,100,5), labels = FALSE)
axis(2, at = seq(0,100,25))
d[1,]
 
