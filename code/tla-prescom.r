rm(list=ls())
setwd("~/Desktop/MXelsCalendGovt/elecReturns")

## ## consolidate 2016 demn-level from casillas
## d <- read.csv("~/Desktop/MXelsCalendGovt/elecReturns/data/ay-nonfused/casillas/tla-pres-com-casilla-2013-on.csv")
## d[1,]
## str(d)
## colnames(d)
## ## NAs to zero
## sel.c <- c("pan","pri","prd", "pt","pvem","mc", "pna","pac","ps", "morena","pes","pest", "piss","rsp","fxm", "pri.pvem.ps","indep","indep2", "efec","nr","NUM_VOTOS_VALIDOS", "nul","tot","lisnom")
## tmp <- d[,sel.c] # subset
## head(tmp)
## tmp[is.na(tmp)] <- 0
## tmp -> d[,sel.c] # return
## ## drop efec=0
## sel.r <- which(d$efec==0)
## d <- d[-sel.r,]
## # consolidate mun votes
## for (i in sel.c){
##     d[,i] <- ave(d[,i], as.factor(d$emm), FUN=function(x) sum(x, na.rm=TRUE))
## }
## ## drop redundants
## d <- d[duplicated(d$emm)==FALSE,]
## d$seccion <- d$casilla <- d$status <- NULL
## ## save
## write.csv(d, file = "~/Downloads/tmp.csv", row.names=FALSE)

d <- read.csv("data/ay-nonfused/tla-pres-com-dem-2013-on.csv")
str(d)

## drop uyc and municipios undivided into comunidades
sel <- grep(pattern = "uyc", d$status, ignore.case = TRUE)
d <- d[-sel,]
sel <- which(d$status=="sin presidencias comunitarias")
d <- d[-sel,]
table(d$status)

## lisnom has "." which are NAs
d$lisnom <- as.numeric(d$lisnom)

## replace NAs w 0s
sel.c <- c("pan", "pri", "prd", "pt", "pvem", "mc", "pna", "pac", "ps", "morena", "pes", "pest", "piss", "rsp", "fxm", "pri.pvem", "pt.pac", "pri.pvem.ps", "indep", "indep2", "nr", "nul", "tot")
tmp <- d[, which(colnames(d) %in% sel.c)]
tmp[is.na(tmp)] <- 0
tmp -> d[, which(colnames(d) %in% sel.c)]
## recompute tot
sel.c <- c("pan", "pri", "prd", "pt", "pvem", "mc", "pna", "pac", "ps", "morena", "pes", "pest", "piss", "rsp", "fxm", "pri.pvem", "pt.pac", "pri.pvem.ps", "indep", "indep2", "nr", "nul")
d$tot <- rowSums(d[, which(colnames(d) %in% sel.c)])
## compute efec
sel.c <- c("pan", "pri", "prd", "pt", "pvem", "mc", "pna", "pac", "ps", "morena", "pes", "pest", "piss", "rsp", "fxm", "pri.pvem", "pt.pac", "pri.pvem.ps", "indep", "indep2")
d$efec <- rowSums(d[, which(colnames(d) %in% sel.c)])
##
## compute participacion (INCLUDING nr + nul)
d$partic <- d$tot / d$lisnom
summary(d$partic)
## two cases with >1 that i cannot figuro from sources (seccion split between several demarcaciones)
sel <- which(d$partic>1)
d$emm[sel]

table(d$ndemmaj)
table(d$ndem)

f <- function(x){round(c(mean(x, na.rm=TRUE), sd(x, na.rm=TRUE), length(x)), 2)}

sel <- which(d$ndem>1)
tapply(d$partic[sel], factor(paste0(d$yr[sel], d$ife[sel])), function(x) f(x))

par(mfrow = c(1, 2))
sel <- which(d$ndem>1 & d$yr==2016)
boxplot(d$partic[sel] ~ d$dscjn[sel]
      , ylim = c(0,1)
      , ylab = "Participación por demarcación"
      , xlab = "1=controversia"
      , main = "Prerreforma (2016)")
sel <- which(d$ndem>1 & d$yr>=2021)
boxplot(d$partic[sel] ~ d$dscjn[sel]
      , ylim = c(0,1)
      , ylab = "Participación por demarcación"
      , xlab = "1=controversia"
      , main = "Posreforma (2021 y 24)")

Dime cuánto será por abono para justificar al SAT y cuánto en efectivo. Podría llevar parte de esto también


