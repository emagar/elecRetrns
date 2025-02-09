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

###################################
## Define party colors for plots ##
###################################
col.pan    <- function(alpha=1) return(rgb(0,0,1                  , alpha=alpha))
col.pri    <- function(alpha=1) return(rgb(1,0,0                  , alpha=alpha))
col.pt     <- function(alpha=1) return(rgb(1,70/255,0             , alpha=alpha))
col.prd    <- function(alpha=1) return(rgb(245/255,189/255,2/255  , alpha=alpha))
col.morena <- function(alpha=1) return(rgb(114/255,0/255,18/255   , alpha=alpha))
col.pvem   <- function(alpha=1) return(rgb(0,128/255,0            , alpha=alpha))
col.mc     <- function(alpha=1) return(rgb(255/255,145/255,0/255  , alpha=alpha))
col.oth    <- function(alpha=1) return(rgb(113/255,113/255,113/255, alpha=alpha))

#####################
## Read data files ##
#####################
## choose one
dr <- read.csv(file = "aymu1970-on.csv"          , stringsAsFactors = FALSE) # raw
da <- read.csv(file = "aymu1970-on.coalAgg.csv"  , stringsAsFactors = FALSE) # coalitions aggregated
ds <- read.csv(file = "aymu1970-on.coalSplit.csv", stringsAsFactors = FALSE) # split coalitions
##
###################
## Drop pre-1979 ##
###################
dr <- dr[dr$yr>1978,]
da <- da[da$yr>1978,]
ds <- ds[ds$yr>1978,]
##
#####################################
## drop un-analyzable obs for good ##
#####################################
d <- dr
sel.r <- grep("cancelled", d$status); d <- d[-sel.r,]
sel.r <- grep("to-runoff", d$status); d <- d[-sel.r,]
sel.r <- grep("missing|appointed|litigio", d$status); d <- d[-sel.r,]
sel.r <- grep("proj", d$status); d <- d[-sel.r,] ## drop missing cases projected from df for lags
d -> dr
d <- da
sel.r <- grep("cancelled", d$status); d <- d[-sel.r,]
sel.r <- grep("to-runoff", d$status); d <- d[-sel.r,]
sel.r <- grep("missing|appointed|litigio", d$status); d <- d[-sel.r,]
sel.r <- grep("proj", d$status); d <- d[-sel.r,] ## drop missing cases projected from df for lags
d -> da
d <- ds
sel.r <- grep("cancelled", d$status); d <- d[-sel.r,]
sel.r <- grep("to-runoff", d$status); d <- d[-sel.r,]
sel.r <- grep("missing|appointed|litigio", d$status); d <- d[-sel.r,]
sel.r <- grep("proj", d$status); d <- d[-sel.r,] ## drop missing cases projected from df for lags
d -> ds
##
##################################################################################
## Election cycle from emm (consecutive but not necessarilly aligned w federal) ##
##################################################################################
d <- dr ## manipulate one dataset
d$cycle <- sub(pattern="^[a-z]+.([0-9]+).*$", replacement = "\\1", d$emm, perl=TRUE) # extract cycle from emm code
d$cycle <- as.numeric(d$cycle)
table(d$cycle)
## Use yr to make cycle aligned w federal (some extras may fall in next)
d$cyclef <- cut(d$yr, breaks = seq(1979,2027, by=3), right = FALSE)
table(d$cyclef)
d -> dr ## return manipulation
##
d <- da ## manipulate one dataset
d$cycle <- sub(pattern="^[a-z]+.([0-9]+).*$", replacement = "\\1", d$emm, perl=TRUE) # extract cycle from emm code
d$cycle <- as.numeric(d$cycle)
table(d$cycle)
## Use yr to make cycle aligned w federal (some extras may fall in next)
d$cyclef <- cut(d$yr, breaks = seq(1979,2027, by=3), right = FALSE)
table(d$cyclef)
d -> da ## return manipulation
##
d <- ds ## manipulate one dataset
d$cycle <- sub(pattern="^[a-z]+.([0-9]+).*$", replacement = "\\1", d$emm, perl=TRUE) # extract cycle from emm code
d$cycle <- as.numeric(d$cycle)
table(d$cycle)
## Use yr to make cycle aligned w federal (some extras may fall in next)
d$cyclef <- cut(d$yr, breaks = seq(1979,2027, by=3), right = FALSE)
table(d$cyclef)
d -> ds ## return manipulation
##
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
##
###########################################
## Change conve with mc across the board ##
###########################################
d <- dr
sel.c <- grep("l[0-9]{2}|win", colnames(d))
l <- d[,sel.c] ## subset labels for manipulation
for (i in 1:ncol(l)) l[,i] <- sub("conve|^cp$", "mc", l[,i])
l -> d[,sel.c] ## return after manipulation
d -> dr
##
d <- da
sel.c <- grep("l[0-9]{2}|win", colnames(d))
l <- d[,sel.c] ## subset labels for manipulation
for (i in 1:ncol(l)) l[,i] <- sub("conve|^cp$", "mc", l[,i])
l -> d[,sel.c] ## return after manipulation
d -> da
##
d <- ds
sel.c <- grep("l[0-9]{2}|win", colnames(d))
l <- d[,sel.c] ## subset labels for manipulation
for (i in 1:ncol(l)) l[,i] <- sub("conve|^cp$", "mc", l[,i])
l -> d[,sel.c] ## return after manipulation
d -> ds

#########################
## Coalition frequency ##
#########################
##############################################################################################
## Replace coalition labels with major-minor, among-majors, or minor-minor                  ##
## (see commented block in reelec/code/ay-vote.r for original version, also vote splitting) ##
##############################################################################################
d <- ds ## Requires coalSplit data
for (i in 1:4){ ## loop over 4 coalition columns
    ## i <- 1
    d$manip <- NA ## will receive manipulated coalition labels
    d$l <- d[, c("coal1","coal2","coal3","coal4")[i]] ## pick one label for manipulation
    ##
    d$manip[d$l=="none"] <- "none"
    ## major-party coalition below
    sel <- grep("(?=.*pan)(?=.*prd)", d$l, perl = TRUE)
    d$manip[sel] <- "double-major"
    sel <- grep("(?=.*pan)(?=.*pri)", d$l, perl = TRUE)
    d$manip[sel] <- "double-major"
    sel <- grep("(?=.*pri)(?=.*prd)", d$l, perl = TRUE)
    d$manip[sel] <- "double-major"
    sel <- grep("(?=.*pan)(?=.*pri)(?=.*prd)", d$l, perl = TRUE)
    d$manip[sel] <- "triple-major"
    ##
    ## Rest are major-minor
    sel1 <- which(is.na(d$manip))
    sel <- grep("pan-|-pan", d$l[sel1])
    d$manip[sel1][sel] <- "pan-minor"
    ##
    sel1 <- which(is.na(d$manip))
    sel <- grep("pri-|-pri", d$l[sel1])
    d$manip[sel1][sel] <- "pri-minor"
    ##
    sel1 <- which(is.na(d$manip))
    sel <- grep("prd-|-prd", d$l[sel1])
    d$manip[sel1][sel] <- "prd-minor"
    ##
    sel1 <- which(is.na(d$manip))
    sel <- grep("morena-|-morena", d$l[sel1])
    d$manip[sel1][sel] <- "morena-minor"
    ##
    ## rest are other coals
    sel1 <- which(is.na(d$manip)) 
    d$manip[sel1] <- "minor-minor"
    ##
    ## ## used to check by hand
    ## table(d$manip, useNA = "always")
    ## sel <- which(d$manip=="minor-minor") 
    ## table(d$l[sel])
    ##
    d$manip -> d[, c("coal1","coal2","coal3","coal4")[i]] ## return manipulation
}
## check
table(d$coal1, useNA = "always")
table(d$coal2, useNA = "always")
table(d$coal3, useNA = "always")
table(d$coal4, useNA = "always")
## clean
d$l <- d$manip <- NULL
## summarize all
library(plyr)
d$coal.prof <- paste(d$coal1,d$coal2,d$coal3,d$coal4)
sel <- which(d$coal.prof=="none none none none"); d$coal.prof[sel] <- "none" ## no coalitions
d$coal.prof <- gsub(" none", "", d$coal.prof)                                ## drop remaining nones anteceded by space
d$coal.prof <- gsub(" minor-minor|minor-minor ", "", d$coal.prof)                         ## drop minor-minor coalitions
d$coal.prof <- gsub("minor-minor", "none", d$coal.prof)                      ## drop minor-minor coalitions
d$coal.prof <- gsub("(pan|pri|prd|morena)[-]minor", "major-minor", d$coal.prof) ## generalize major minor
d$coal.prof <-  sub("major-minor major-minor major-minor", "major-minor-x3", d$coal.prof) ## generalize major minor
d$coal.prof <-  sub("major-minor major-minor", "major-minor-x2", d$coal.prof)
d$coal.prof <-  sub("major-minor double-major", "double-major major-minor", d$coal.prof)
d$coal.prof <-  sub("major-minor-x2 double-major", "double-major major-minor-x2", d$coal.prof)
d$coal.prof <-  sub("major-minor triple-major", "triple-major major-minor", d$coal.prof)
d$coal.prof <-  sub("double-major major-minor major-minor", "double-major major-minor-x2", d$coal.prof)
table(d$coal.prof, useNA = "always")
## shorten labels so reports single row per cycle
d$coal.prof <- mapvalues(d$coal.prof,
                         from=c("none", "major-minor", "major-minor-x2", "major-minor-x3", "double-major", "double-major major-minor", "double-major major-minor-x2", "triple-major", "triple-major major-minor"),
                         to=c("0 none", "1 1maj", "2 1majx2", "3 1majx3", "4 2maj", "5 2maj 1maj", "6 2maj 1majx2", "7 3maj", "8 3maj 1maj"))
##
## rel frequencies by election cycle for one table
t <- table(d$cyclef, d$coal.prof)
round(t*100/rowSums(t),1)
## avg num coals by cycle
aggregate(d$ncoal ~ d$cyclef, FUN = mean)

#############
## Winners ##
#############
d <- da
## There are still missing listas nominales, but few and pre-1997 (lisnoms used to gauge mun size)
table(is.na(d$lisnom), edon2edo(d$edon))
table(is.na(d$lisnom), d$yr)
##
## simplify winners
d$l <- NA ## will receive manipulated coalition labels
##
## major-party coalitions: drop minors (will be assigned to one major below)
sel <- grep("(?=.*pan)(?=.*prd)", d$win, perl = TRUE)
d$l[sel] <- "pan-prd"
sel <- grep("(?=.*pan)(?=.*pri)", d$win, perl = TRUE)
d$l[sel] <- "pan-pri"
sel <- grep("(?=.*pri)(?=.*prd)", d$win, perl = TRUE)
d$l[sel] <- "pri-prd"
sel <- grep("(?=.*pan)(?=.*pri)(?=.*prd)", d$win, perl = TRUE)
d$l[sel] <- "pan-pri-prd"
##
## Rest NAs are major-minor --- will be coded as major solo
sel1 <- which(is.na(d$l))
sel <- grep("pan-|-pan", d$win[sel1])
##d$l[sel1][sel] <- "pan-minor"
d$l[sel1][sel] <- "2pan"
##
sel1 <- which(is.na(d$l))
sel <- grep("pri-|-pri", d$win[sel1])
##d$l[sel1][sel] <- "pri-minor"
d$l[sel1][sel] <- "1pri"
##
sel1 <- which(is.na(d$l))
sel <- grep("prd-|-prd", d$win[sel1])
##d$l[sel1][sel] <- "prd-minor"
d$l[sel1][sel] <- "3prd"
##
sel1 <- which(is.na(d$l))
sel <- grep("morena-|-morena", d$win[sel1])
##d$l[sel1][sel] <- "morena-minor"
d$l[sel1][sel] <- "8morena"
##
sel1 <- which(is.na(d$l))
sel <- grep("indep[_0-9]", d$win[sel1])
d$l[sel1][sel] <- "7oth"
##
sel1 <- which(is.na(d$l))
sel <- grep("mc-|-mc", d$win[sel1]) ## give remaining mc-coals to mc solo
table(d$win[sel1][sel])
d$l[sel1][sel] <- "4mc"
##
sel1 <- which(is.na(d$l))
sel <- grep("pvem-|-pvem", d$win[sel1]) ## give remaining pvem-coals to pvem solo
table(d$win[sel1][sel])
d$l[sel1][sel] <- "5pvem"
##
sel1 <- which(is.na(d$l))
sel <- grep("pt-|-pt", d$win[sel1]) ## give remaining pt-coals to pt solo
table(d$win[sel1][sel])
d$l[sel1][sel] <- "6pt"
##
sel <- grep("^pan$",    d$win, perl = TRUE); d$l[sel] <- "2pan"
sel <- grep("^pri$",    d$win, perl = TRUE); d$l[sel] <- "1pri"
sel <- grep("^prd$",    d$win, perl = TRUE); d$l[sel] <- "3prd"
sel <- grep("^pt$",     d$win, perl = TRUE); d$l[sel] <- "6pt"
sel <- grep("^pvem$",   d$win, perl = TRUE); d$l[sel] <- "5pvem"
sel <- grep("^mc$",     d$win, perl = TRUE); d$l[sel] <- "4mc"
sel <- grep("^morena$", d$win, perl = TRUE); d$l[sel] <- "8morena"
##
## rest bunched as other
table(d$win[is.na(d$l)])
sel1 <- which(is.na(d$l))
d$l[sel1] <- "7oth"
##
## BREAK MAJOR-PARTY COALITIONS BASED ON WHICH PARTY IS BIGGER IN STATE
## triple majors
sel <- which(d$l=="pan-pri-prd")
table(d$cyclef[sel], edon2edo(d$edon[sel]))
##
sel1 <- which(d$edon[sel]==1);  d$l[sel][sel1] <- "2pan"
sel1 <- which(d$edon[sel]==3);  d$l[sel][sel1] <- "2pan"
sel1 <- which(d$edon[sel]==4);  d$l[sel][sel1] <- "1pri"
sel1 <- which(d$edon[sel]==6);  d$l[sel][sel1] <- "1pri"
sel1 <- which(d$edon[sel]==7);  d$l[sel][sel1] <- "3prd"
sel1 <- which(d$edon[sel]==8);  d$l[sel][sel1] <- "2pan"
sel1 <- which(d$edon[sel]==9);  d$l[sel][sel1] <- "2pan"
sel1 <- which(d$edon[sel]==10); d$l[sel][sel1] <- "1pri"
sel1 <- which(d$edon[sel]==11); d$l[sel][sel1] <- "2pan"
sel1 <- which(d$edon[sel]==12); d$l[sel][sel1] <- "1pri"
sel1 <- which(d$edon[sel]==13); d$l[sel][sel1] <- "1pri"
sel1 <- which(d$edon[sel]==14); d$l[sel][sel1] <- "2pan"
sel1 <- which(d$edon[sel]==15); d$l[sel][sel1] <- "1pri"
sel1 <- which(d$edon[sel]==16); d$l[sel][sel1] <- "3prd"
sel1 <- which(d$edon[sel]==17); d$l[sel][sel1] <- "2pan"
sel1 <- which(d$edon[sel]==18); d$l[sel][sel1] <- "1pri"
sel1 <- which(d$edon[sel]==19); d$l[sel][sel1] <- "2pan"
sel1 <- which(d$edon[sel]==20); d$l[sel][sel1] <- "1pri"
sel1 <- which(d$edon[sel]==21); d$l[sel][sel1] <- "2pan"
sel1 <- which(d$edon[sel]==22); d$l[sel][sel1] <- "2pan"
sel1 <- which(d$edon[sel]==23); d$l[sel][sel1] <- "1pri"
sel1 <- which(d$edon[sel]==24); d$l[sel][sel1] <- "1pri"
sel1 <- which(d$edon[sel]==25); d$l[sel][sel1] <- "1pri"
sel1 <- which(d$edon[sel]==26); d$l[sel][sel1] <- "1pri"
sel1 <- which(d$edon[sel]==30); d$l[sel][sel1] <- "2pan"
sel1 <- which(d$edon[sel]==32); d$l[sel][sel1] <- "1pri"
##
# pan-pri
sel <- which(d$l=="pan-pri")
table(d$cyclef[sel], edon2edo(d$edon[sel]))
##
sel1 <- which(d$edon[sel]==6);  d$l[sel][sel1] <- "1pri"
sel1 <- which(d$edon[sel]==16); d$l[sel][sel1] <- "1pri"
sel1 <- which(d$edon[sel]==20); d$l[sel][sel1] <- "1pri"
sel1 <- which(d$edon[sel]==21); d$l[sel][sel1] <- "2pan"
sel1 <- which(d$edon[sel]==22); d$l[sel][sel1] <- "2pan"
sel1 <- which(d$edon[sel]==24); d$l[sel][sel1] <- "2pan"
sel1 <- which(d$edon[sel]==28); d$l[sel][sel1] <- "2pan"
sel1 <- which(d$edon[sel]==29); d$l[sel][sel1] <- "1pri"
sel1 <- which(d$edon[sel]==31); d$l[sel][sel1] <- "2pan"
##
# pan-prd
sel <- which(d$l=="pan-prd")
table(d$cyclef[sel], edon2edo(d$edon[sel]))
##
sel1 <- which(d$edon[sel]==1);  d$l[sel][sel1] <- "2pan"
sel1 <- which(d$edon[sel]==2);  d$l[sel][sel1] <- "2pan"
sel1 <- which(d$edon[sel]==3);  d$l[sel][sel1] <- "2pan"
sel1 <- which(d$edon[sel]==5);  d$l[sel][sel1] <- "2pan"
sel1 <- which(d$edon[sel]==6);  d$l[sel][sel1] <- "2pan"
sel1 <- which(d$edon[sel]==7);  d$l[sel][sel1] <- "3prd"
sel1 <- which(d$edon[sel]==8);  d$l[sel][sel1] <- "2pan"
sel1 <- which(d$edon[sel]==9);  d$l[sel][sel1] <- "2pan"
sel1 <- which(d$edon[sel]==10); d$l[sel][sel1] <- "2pan"
sel1 <- which(d$edon[sel]==12); d$l[sel][sel1] <- "3prd"
sel1 <- which(d$edon[sel]==13); d$l[sel][sel1] <- "3prd"
sel1 <- which(d$edon[sel]==14); d$l[sel][sel1] <- "2pan"
sel1 <- which(d$edon[sel]==15); d$l[sel][sel1] <- "2pan"
sel1 <- which(d$edon[sel]==16); d$l[sel][sel1] <- "3prd"
sel1 <- which(d$edon[sel]==18); d$l[sel][sel1] <- "2pan"
sel1 <- which(d$edon[sel]==20); d$l[sel][sel1] <- "3prd"
sel1 <- which(d$edon[sel]==21); d$l[sel][sel1] <- "2pan"
sel1 <- which(d$edon[sel]==22); d$l[sel][sel1] <- "2pan"
sel1 <- which(d$edon[sel]==23); d$l[sel][sel1] <- "3prd"
sel1 <- which(d$edon[sel]==24); d$l[sel][sel1] <- "2pan"
sel1 <- which(d$edon[sel]==22); d$l[sel][sel1] <- "2pan"
sel1 <- which(d$edon[sel]==23); d$l[sel][sel1] <- "3prd"
sel1 <- which(d$edon[sel]==24); d$l[sel][sel1] <- "2pan"
sel1 <- which(d$edon[sel]==25); d$l[sel][sel1] <- "2pan"
sel1 <- which(d$edon[sel]==26); d$l[sel][sel1] <- "2pan"
sel1 <- which(d$edon[sel]==27); d$l[sel][sel1] <- "3prd"
sel1 <- which(d$edon[sel]==28); d$l[sel][sel1] <- "2pan"
sel1 <- which(d$edon[sel]==30); d$l[sel][sel1] <- "2pan"
sel1 <- which(d$edon[sel]==31); d$l[sel][sel1] <- "2pan"
sel1 <- which(d$edon[sel]==32); d$l[sel][sel1] <- "3prd"
##
# pri-prd all to pri
sel <- which(d$l=="pri-prd")
table(d$cyclef[sel], edon2edo(d$edon[sel]))
d$l[sel] <- "1pri"
##
## check
table(d$l[d$yr>1978], useNA = "ifany")
##
## winners by cycle
t <- table(d$cyclef, d$l)
t <- round(t*100/rowSums(t),1)
rownames(t) <- seq(1979,2024,3)
t <- t[order(rownames(t), decreasing = TRUE),]
t(t)

pdf(file = "../plots/pctwin1979-2024.pdf", width=10, height=5)
par(mar=c(3,4,0,2)+0.1) # drop title space and xlab space
barplot(t(t)
      , horiz = TRUE
      , col = c(col.pri(), col.pan(), col.prd(), col.mc(), col.pvem(), col.pt(), col.oth(), col.morena())
      , las=1
      ##, main = "% municipalities won"
        )
## tx <- seq(2, 96, length.out = 8)
## text(x = tx[1], y = 19.65, labels = c("PRI"), col = col.pri())
## text(x = tx[2], y = 19.65, labels = c("PAN"), col = col.pan())
## text(x = tx[3], y = 19.65, labels = c("PRD"), col = col.prd())
## text(x = tx[4], y = 19.65, labels = c("MC"),  col = col.mc())
## text(x = tx[5], y = 19.65, labels = c("PVEM"), col = col.pvem())
## text(x = tx[6], y = 19.65, labels = c("PT"), col = col.pt())
## text(x = tx[7], y = 19.65, labels = c("Other"), col = col.oth())
## text(x = tx[8], y = 19.65, labels = c("Morena"), col = col.morena())
dev.off()

## Self-standing legend for both plots in latex
pdf(file = "../plots/pctwin1979-2024-legend.pdf", width=10, height=.35)
par(mar=c(0,4,0,2)+0.1) # drop title space and xlab space
plot(x=c(0,100), y=c(0,1), type = "n", axes = FALSE, xlab = "", ylab = "")
tx <- seq(2, 96, length.out = 8)
text(x = tx[1], y = .2, labels = c("PRI"), col = col.pri())
text(x = tx[2], y = .2, labels = c("PAN"), col = col.pan())
text(x = tx[3], y = .2, labels = c("PRD"), col = col.prd())
text(x = tx[4], y = .2, labels = c("MC"),  col = col.mc())
text(x = tx[5], y = .2, labels = c("PVEM"), col = col.pvem())
text(x = tx[6], y = .2, labels = c("PT"), col = col.pt())
text(x = tx[7], y = .2, labels = c("Other"), col = col.oth())
text(x = tx[8], y = .2, labels = c("Morena"), col = col.morena())
dev.off()

#######################################
## lisnom-weighted winners by cycle  ##
#######################################
## make lisnom rel to state-year (very rough population-weight)
d$lisnom <- d$lisnom *100 / ave(d$lisnom, as.factor(d$edon+as.numeric(d$cyclef)/10), FUN=function(x) sum(x, na.rm=TRUE))
d[1,]
## aggregate(lisnom ~ l, data = d[!is.na(d$lisnom),], FUN = sum)
t <- aggregate(lisnom ~ l + cyclef, data = d[!is.na(d$lisnom),], FUN = sum)
t$lisnom <- t$lisnom *100 / ave(t$lisnom, t$cyclef, FUN=function(x) sum(x)) ## make cycle add to 100
t <- as.data.frame(t)
t1  <- t[as.numeric(t$cyclef)==1,];  colnames(t1 )[3] <- levels(t1$cyclef)[1]
t2  <- t[as.numeric(t$cyclef)==2,];  colnames(t2 )[3] <- levels(t1$cyclef)[2]
t3  <- t[as.numeric(t$cyclef)==3,];  colnames(t3 )[3] <- levels(t1$cyclef)[3]
t4  <- t[as.numeric(t$cyclef)==4,];  colnames(t4 )[3] <- levels(t1$cyclef)[4]
t5  <- t[as.numeric(t$cyclef)==5,];  colnames(t5 )[3] <- levels(t1$cyclef)[5]
t6  <- t[as.numeric(t$cyclef)==6,];  colnames(t6 )[3] <- levels(t1$cyclef)[6]
t7  <- t[as.numeric(t$cyclef)==7,];  colnames(t7 )[3] <- levels(t1$cyclef)[7]
t8  <- t[as.numeric(t$cyclef)==8,];  colnames(t8 )[3] <- levels(t1$cyclef)[8]
t9  <- t[as.numeric(t$cyclef)==9,];  colnames(t9 )[3] <- levels(t1$cyclef)[9]
t10 <- t[as.numeric(t$cyclef)==10,]; colnames(t10)[3] <- levels(t1$cyclef)[10]
t11 <- t[as.numeric(t$cyclef)==11,]; colnames(t11)[3] <- levels(t1$cyclef)[11]
t12 <- t[as.numeric(t$cyclef)==12,]; colnames(t12)[3] <- levels(t1$cyclef)[12]
t13 <- t[as.numeric(t$cyclef)==13,]; colnames(t13)[3] <- levels(t1$cyclef)[13]
t14 <- t[as.numeric(t$cyclef)==14,]; colnames(t14)[3] <- levels(t1$cyclef)[14]
t15 <- t[as.numeric(t$cyclef)==15,]; colnames(t15)[3] <- levels(t1$cyclef)[15]
t16 <- t[as.numeric(t$cyclef)==16,]; colnames(t16)[3] <- levels(t1$cyclef)[16]
t <- merge(t1, t2[,c(1,3)], by = "l", all = TRUE)
t <- merge(t, t3[,c(1,3)], by = "l", all = TRUE)
t <- merge(t, t4[,c(1,3)], by = "l", all = TRUE)
t <- merge(t, t5[,c(1,3)], by = "l", all = TRUE)
t <- merge(t, t6[,c(1,3)], by = "l", all = TRUE)
t <- merge(t, t7[,c(1,3)], by = "l", all = TRUE)
t <- merge(t, t8[,c(1,3)], by = "l", all = TRUE)
t <- merge(t, t9[,c(1,3)], by = "l", all = TRUE)
t <- merge(t, t10[,c(1,3)], by = "l", all = TRUE)
t <- merge(t, t11[,c(1,3)], by = "l", all = TRUE)
t <- merge(t, t12[,c(1,3)], by = "l", all = TRUE)
t <- merge(t, t13[,c(1,3)], by = "l", all = TRUE)
t <- merge(t, t14[,c(1,3)], by = "l", all = TRUE)
t <- merge(t, t15[,c(1,3)], by = "l", all = TRUE)
t <- merge(t, t16[,c(1,3)], by = "l", all = TRUE)
rm(list = paste0("t",1:16)) ## clean
rownames(t) <- t$l
t$cyclef <- t$l <- NULL
colnames(t) <- seq(1979,2024,3)
t <- t[,order(colnames(t), decreasing = TRUE)]
t <- as.matrix(t)
t[is.na(t)] <- 0
##
pdf(file = "../plots/pctwin-popw1979-2024.pdf", width=10, height=5)
par(mar=c(3,4,0,2)+0.1) # drop title space and xlab space
barplot(t
      , horiz = TRUE
      , col = c(col.pri(), col.pan(), col.prd(), col.mc(), col.pvem(), col.pt(), col.oth(), col.morena())
      , las=1
      ##, main = "Size-weighted % municipalities won"
        )
## tx <- seq(2, 96, length.out = 8)
## text(x = tx[1], y = 19.65, labels = c("PRI"), col = col.pri())
## text(x = tx[2], y = 19.65, labels = c("PAN"), col = col.pan())
## text(x = tx[3], y = 19.65, labels = c("PRD"), col = col.prd())
## text(x = tx[4], y = 19.65, labels = c("MC"),  col = col.mc())
## text(x = tx[5], y = 19.65, labels = c("PVEM"), col = col.pvem())
## text(x = tx[6], y = 19.65, labels = c("PT"), col = col.pt())
## text(x = tx[7], y = 19.65, labels = c("Other"), col = col.oth())
## text(x = tx[8], y = 19.65, labels = c("Morena"), col = col.morena())
dev.off()


###########################################################
## Effective number of electoral parties (coalition agg) ##
###########################################################
## compute vote shares
d <- da
v <- d[, grep("v[0-9]{2}", colnames(d))]
v <- v / rowSums(v)
d$enp <- apply(v, 1, FUN = function(x) 1/sum(x^2))
table(d$edon[is.na(enp)] + d$yr[is.na(enp)]/10000) ## NAs from 2025 els
t <- aggregate(enp ~ cyclef, data = d, FUN = mean)
colnames(t)[2] <- "enpv.agg"
rownames(t) <- levels(t$cyclef); t$cyclef <- NULL
##
#############################################################
## Effective number of electoral parties (coalition split) ##
#############################################################
## compute vote shares
d <- ds
v <- d[, grep("v[0-9]{2}", colnames(d))]
v <- v / rowSums(v)
d$enp <- apply(v, 1, FUN = function(x) 1/sum(x^2))
table(d$edon[is.na(enp)] + d$yr[is.na(enp)]/10000) ## NAs from 2025 els
t2 <- aggregate(enp ~ cyclef, data = d, FUN = mean)
t$enpv.spl <- t2$enp
## report
round(t,2)


##############
## Coverage ##
##############
##
table(d$status)
## drop cancelled races 
sel.r <- grep("cancelled", d$status); if (length(sel.r)>0) ds <- d[-sel.r,] # subset wo cancelled races keeps actual number
## drop runoffs (comment if data is v7, ay-vote.r dropped 1st round keeping runoff only with regular emm code and note in status)
sel.r <- grep("to-runoff", d$status); if (length(sel.r)>0) ds <- d[-sel.r,] # subset wo runoff races keeps actual number
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



######################################
## plot party shares by year/cyclef ##
######################################
#
###################################################################################################################
## imports object with split-coal ay data since 1988 including vote shares for pan pri prd morena mc pvem pt oth ##
## To update, run code in reelec/code/ay-vote.r from start to where this file is saved to disk                   ##
###################################################################################################################
library(lubridate)

getwd()
d <- read.csv(file = "../../reelec/data/aymu1988-on-v7-coalSplit.csv")
d <- d[d$yr>1978,] ## drop pre 1979
d$morena[d$yr<2015] <- NA ## drop zeroes from pre-morena
d$pvem[d$yr<1993] <- NA ## drop zeroes from pre-pvem
d$mc[d$yr<2000] <- NA ## drop zeroes from pre-conve
table(d$status)
d[1,]
##################
## format dates ##
##################
date <- d$date
sel.r <- which(date<19790000) # cases where dy missing
table(date[sel.r]) ## check only dy missing
date[sel.r] <- date[sel.r] * 100 + 15 ## arbitrary put at mid month
date <- ymd(date)
table(date)
date -> d$date ## return manipulation

## add +/- 4 weeks as jitter for plot
x.jitter <- round(runif(n=nrow(d),-28,28),0)
x.jitter <- days(x.jitter)
date <- date + x.jitter

## add pt to other category for plotting
d$oth <- d$oth + d$pt 

## yearly means quartiles etc
## mn <- data.frame(
##                  pan   =aggregate(pan    ~ yr, data = d, FUN = mean),
##                  pri   =aggregate(pri    ~ yr, data = d, FUN = mean),
##                  prd   =aggregate(prd    ~ yr, data = d, FUN = mean),
##                  morena=aggregate(morena ~ yr, data = d, FUN = mean)
## )
## mn$yr <- ymd(mn$pan.yr*10000+701)
## mn$pan <- mn$pan.pan; 
## mn$pri <- mn$pri.pri; 
## mn$prd <- mn$prd.prd; 
## mn$morena <- mn$morena.morena; 
##

q9 <- merge(x=aggregate(x=pan    ~ yr, data = d, FUN = 'quantile', probs=(.75)),
            y=aggregate(x=pri    ~ yr, data = d, FUN = 'quantile', probs=(.75)), by = "yr", all = TRUE)
q9 <- merge(x=q9,
            y=aggregate(x=prd    ~ yr, data = d, FUN = 'quantile', probs=(.75)), by = "yr", all = TRUE)
q9 <- merge(x=q9,
            y=aggregate(x=mc     ~ yr, data = d, FUN = 'quantile', probs=(.75)), by = "yr", all = TRUE)
q9 <- merge(x=q9,
            y=aggregate(x=pvem   ~ yr, data = d, FUN = 'quantile', probs=(.75)), by = "yr", all = TRUE)
q9 <- merge(x=q9,
            y=aggregate(x=morena ~ yr, data = d, FUN = 'quantile', probs=(.75)), by = "yr", all = TRUE)
q9 <- merge(x=q9,
            y=aggregate(x=oth    ~ yr, data = d, FUN = 'quantile', probs=(.75)), by = "yr", all = TRUE)
q9$yr <- ymd(q9$yr*10000+701)
##
q1 <- merge(x=aggregate(x=pan    ~ yr, data = d, FUN = 'quantile', probs=(.25)),
            y=aggregate(x=pri    ~ yr, data = d, FUN = 'quantile', probs=(.25)), by = "yr", all = TRUE)
q1 <- merge(x=q1,
            y=aggregate(x=prd    ~ yr, data = d, FUN = 'quantile', probs=(.25)), by = "yr", all = TRUE)
q1 <- merge(x=q1,
            y=aggregate(x=mc     ~ yr, data = d, FUN = 'quantile', probs=(.25)), by = "yr", all = TRUE)
q1 <- merge(x=q1,
            y=aggregate(x=pvem   ~ yr, data = d, FUN = 'quantile', probs=(.25)), by = "yr", all = TRUE)
q1 <- merge(x=q1,
            y=aggregate(x=morena ~ yr, data = d, FUN = 'quantile', probs=(.25)), by = "yr", all = TRUE)
q1 <- merge(x=q1,
            y=aggregate(x=oth    ~ yr, data = d, FUN = 'quantile', probs=(.25)), by = "yr", all = TRUE)
q1$yr <- ymd(q1$yr*10000+701)
##
q5 <- merge(x=aggregate(x=pan    ~ yr, data = d, FUN = 'quantile', probs=(.5)),
            y=aggregate(x=pri    ~ yr, data = d, FUN = 'quantile', probs=(.5)), by = "yr", all = TRUE)
q5 <- merge(x=q5,
            y=aggregate(x=prd    ~ yr, data = d, FUN = 'quantile', probs=(.5)), by = "yr", all = TRUE)
q5 <- merge(x=q5,
            y=aggregate(x=mc     ~ yr, data = d, FUN = 'quantile', probs=(.5)), by = "yr", all = TRUE)
q5 <- merge(x=q5,
            y=aggregate(x=pvem   ~ yr, data = d, FUN = 'quantile', probs=(.5)), by = "yr", all = TRUE)
q5 <- merge(x=q5,
            y=aggregate(x=morena ~ yr, data = d, FUN = 'quantile', probs=(.5)), by = "yr", all = TRUE)
q5 <- merge(x=q5,
            y=aggregate(x=oth    ~ yr, data = d, FUN = 'quantile', probs=(.5)), by = "yr", all = TRUE)
q5$yr <- ymd(q5$yr*10000+701)

## function to draw interquartile range spline
gen.ci <- function(pty=c("pan","pri","prd","morena","oth")[1]){
    pty <- paste0("^", pty, "$")
    sel <- grep(pty, colnames(q9))
    tmp <- smooth.spline(x = q9$yr, y = q9[,sel]   *100, df = 10)
    tmp.hilo <- data.frame(xhi = tmp$x, yhi = tmp$y)
    tmp <- smooth.spline(x = q1$yr, y = q1[,sel]   *100, df = 10)
    tmp.hilo <- cbind(tmp.hilo, data.frame(xlo = rev(tmp$x), ylo = rev(tmp$y)))
    tmp.hilo$yhi[tmp.hilo$yhi<0] <- 0; tmp.hilo$yhi[tmp.hilo$yhi>100] <- 100 ## deal w out-of-range
    tmp.hilo$ylo[tmp.hilo$ylo<0] <- 0; tmp.hilo$ylo[tmp.hilo$ylo>100] <- 100 ## deal w out-of-range
    tmp.hilo$yhi[tmp.hilo$yhi<0] <- 0; tmp.hilo$yhi[tmp.hilo$yhi>100] <- 100 ## deal w out-of-range
    sel <- which(tmp.hilo$yhi < tmp.hilo$ylo) ## which lo are above hi? invert them
    tmp.hilo[sel, c("yhi", "ylo")] <- tmp.hilo[sel, c("ylo", "yhi")] ## swap lo and hi
    return(tmp.hilo)
}

getwd()

pdf(file = "../plots/vpct1979-2024.pdf", width=10, height=7)
par(mar=c(3,4,2,2)+0.1) # drop title space and xlab space
plot(x=c(ymd(19790601),ymd(20241231)), y=c(0,100), type="n", axes = FALSE, xlab = "", ylab = "% vote", main = "")
axis(1, at = seq(from=ymd(19790101), to=ymd(20240101), by = 'years'), labels = FALSE)
tmp <- c(1979, paste0("'", seq(82,97,3)), 2000, paste0("'", formatC(seq(3,21,3), width = 2, format = "d", flag = "0")), 2024) # xlabels
axis(1, at = seq(from=ymd(19790101), to=ymd(20240101), by = '3 year'), labels = tmp)
axis(2, at = seq(0,100,5), labels = FALSE)
axis(2, at = seq(0,100,25))
abline(h=seq(0,100,5), col="gray", lwd=.5)
abline(h=seq(0,100,10), col="gray")
abline(h=50, col="gray", lwd=2)
##
## ## quartile splines ... no jala
## tmp.hilo <- gen.ci("pan")    
## polygon(x = c(tmp.hilo$xhi, tmp.hilo$xlo), y = c(tmp.hilo$yhi, tmp.hilo$ylo), border = NA, col = col.pan(alpha=.4) )
## tmp.hilo <- gen.ci("pri")    
## polygon(x = c(tmp.hilo$xhi, tmp.hilo$xlo), y = c(tmp.hilo$yhi, tmp.hilo$ylo), border = NA, col = col.pri(alpha=.4) )
## tmp.hilo <- gen.ci("prd")    
## polygon(x = c(tmp.hilo$xhi, tmp.hilo$xlo), y = c(tmp.hilo$yhi, tmp.hilo$ylo), border = NA, col = col.prd(alpha=.4) )
## tmp.hilo <- gen.ci("morena")    
## polygon(x = c(tmp.hilo$xhi, tmp.hilo$xlo), y = c(tmp.hilo$yhi, tmp.hilo$ylo), border = NA, col = col.morena(alpha=.4) )
##
## ## grayish traces
## lines( smooth.spline(x = date[is.na(d$pan)   ==FALSE], y = d$pan   [is.na(d$pan)   ==FALSE]*100, df = 10), lty = 1, lwd = .75, col = col.pan   (alpha=.2) )
## lines( smooth.spline(x = date[is.na(d$pri)   ==FALSE], y = d$pri   [is.na(d$pri)   ==FALSE]*100, df = 10), lty = 1, lwd = .75, col = col.pri   (alpha=.2) )
## lines( smooth.spline(x = date[is.na(d$prd)   ==FALSE], y = d$prd   [is.na(d$prd)   ==FALSE]*100, df = 10), lty = 1, lwd = .75, col = col.prd   (alpha=.2) )
## lines( smooth.spline(x = date[is.na(d$morena)==FALSE], y = d$morena[is.na(d$morena)==FALSE]*100, df =  4), lty = 1, lwd = .75, col = col.morena(alpha=.2) )
##
## points(date, d$pan*100, col=col.pan(alpha=.02), pch = 1, cex = .5)
## lines( smooth.spline(x = date[is.na(d$pan)==FALSE], y = d$pan[is.na(d$pan)==FALSE]*100, df = 10), lty = 1, lwd = 2, col = col.pan(alpha=1) )
## ##
## points(date, d$pri*100, col=col.pri(alpha=.02), pch = 1, cex = .5)
## lines( smooth.spline(x = date[is.na(d$pri)==FALSE], y = d$pri[is.na(d$pri)==FALSE]*100, df = 10), lty = 1, lwd = 2, col = col.pri(alpha=1) )
## ##
## points(date, d$prd*100, col=col.prd(alpha=.02), pch = 1, cex = .5)
## lines( smooth.spline(x = date[is.na(d$prd)==FALSE], y = d$prd[is.na(d$prd)==FALSE]*100, df = 10), lty = 1, lwd = 2, col = col.prd(alpha=1) )
## ##
## points(date, d$morena*100, col=col.morena(alpha=.02), pch = 1, cex = .5)
## lines( smooth.spline(x = date[is.na(d$morena)==FALSE], y = d$morena[is.na(d$morena)==FALSE]*100, df = 4), lty = 1, lwd = 2, col = col.morena(alpha=1) )
## ##
## points(date, d$mc*100, col=col.mc(alpha=.02), pch = 1, cex = .5)
## lines( smooth.spline(x = date[is.na(d$mc)==FALSE], y = d$mc[is.na(d$mc)==FALSE]*100, df = 8), lty = 1, lwd = 2, col = col.mc(alpha=1) )
## ##
## points(date, d$pvem*100, col=col.pvem(alpha=.02), pch = 1, cex = .5)
## lines( smooth.spline(x = date[is.na(d$pvem)==FALSE], y = d$pvem[is.na(d$pvem)==FALSE]*100, df = 8), lty = 1, lwd = 2, col = col.pvem(alpha=1) )
##
## put all party points in a sorted object so they are not plotted party-by-party (too much emphasis on last party added)
allpts <- rbind(
    data.frame(date=date, point=d$pan,    clr=col.pan   (alpha=.05)),
    data.frame(date=date, point=d$pri   , clr=col.pri   (alpha=.05)),
    data.frame(date=date, point=d$prd   , clr=col.prd   (alpha=.05)),
    data.frame(date=date, point=d$pvem  , clr=col.pvem  (alpha=.05)),
    data.frame(date=date, point=d$mc    , clr=col.mc    (alpha=.05)),
    data.frame(date=date, point=d$morena, clr=col.morena(alpha=.05))
)

legend(x=15000, y=102, legend = c(c("Center-right (PAN)","PRI","Left (PRD)","Morena","Mov. Ciud.","Green (PVEM)","Others")), col = c(col.pan(),col.pri(),col.prd(),col.morena(),col.mc(),col.pvem(),col.oth()), lty = 1, lwd=1.5, bg = "white")
##
## add legend before points so the few in region are visible
allpts <- allpts[order(allpts$point),] # sort so that points are not plottet party by party
points(allpts$date, allpts$point*100, col=allpts$clr, pch = 1, cex = .5)
##
## ## smooth mean
## lines( smooth.spline(x = date[is.na(d$pan)==FALSE],    y = d$pan[is.na(d$pan)==FALSE]*100,       df = 10), lty = 1, lwd = 2, col = col.pan() )
## lines( smooth.spline(x = date[is.na(d$pri)==FALSE],    y = d$pri[is.na(d$pri)==FALSE]*100,       df = 10), lty = 1, lwd = 2, col = col.pri() )
## lines( smooth.spline(x = date[is.na(d$prd)==FALSE],    y = d$prd[is.na(d$prd)==FALSE]*100,       df = 10), lty = 1, lwd = 2, col = col.prd() )
## lines( smooth.spline(x = date[is.na(d$morena)==FALSE], y = d$morena[is.na(d$morena)==FALSE]*100, df = 4),  lty = 1, lwd = 2, col = col.morena() )
## lines( smooth.spline(x = date[is.na(d$mc)==FALSE],     y = d$mc[is.na(d$mc)==FALSE]*100,         df = 10), lty = 1, lwd = 2, col = col.mc() )
## lines( smooth.spline(x = date[is.na(d$pvem)==FALSE],   y = d$pvem[is.na(d$pvem)==FALSE]*100,     df = 10), lty = 1, lwd = 2, col = col.pvem() )
##
## smooth median
tmp <- smooth.spline(x = q5$yr[!is.na(q5$oth)],    y = q5$oth[!is.na(q5$oth)]*100,       df = 7)
tmp$y[tmp$y<0] <- 0; tmp$y[tmp$y>100] <- 100
lines(tmp , lty = 1, lwd = 2, col = col.oth() )
##
tmp <- smooth.spline(x = q5$yr[!is.na(q5$prd)],    y = q5$prd[!is.na(q5$prd)]*100,       df = 7)
tmp$y[tmp$y<0] <- 0; tmp$y[tmp$y>100] <- 100
lines(tmp , lty = 1, lwd = 2, col = col.prd() )
##
tmp <- smooth.spline(x = q5$yr[!is.na(q5$pan)],    y = q5$pan[!is.na(q5$pan)]*100,       df = 7)
tmp$y[tmp$y<0] <- 0; tmp$y[tmp$y>100] <- 100
lines(tmp , lty = 1, lwd = 2, col = col.pan() )
##
tmp <- smooth.spline(x = q5$yr[!is.na(q5$pri)],    y = q5$pri[!is.na(q5$pri)]*100,       df = 7)
tmp$y[tmp$y<0] <- 0; tmp$y[tmp$y>100] <- 100
lines(tmp , lty = 1, lwd = 2, col = col.pri() )
##
tmp <- smooth.spline(x = q5$yr[!is.na(q5$morena)], y = q5$morena[!is.na(q5$morena)]*100, df = 4)
tmp$y[tmp$y<0] <- 0; tmp$y[tmp$y>100] <- 100
lines(tmp , lty = 1, lwd = 2, col = col.morena() )
##
tmp <- smooth.spline(x = q5$yr[!is.na(q5$mc)],     y = q5$mc[!is.na(q5$mc)]*100,         df = 7)
tmp$y[tmp$y<0] <- 0; tmp$y[tmp$y>100] <- 100
lines(tmp , lty = 1, lwd = 2, col = col.mc() )
##
tmp <- smooth.spline(x = q5$yr[!is.na(q5$pvem)],   y = q5$pvem[!is.na(q5$pvem)]*100,     df = 7)
tmp$y[tmp$y<0] <- 0; tmp$y[tmp$y>100] <- 100
lines(tmp , lty = 1, lwd = 2, col = col.pvem() )
##
dev.off()

d[1,] 

