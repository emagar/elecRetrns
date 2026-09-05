setwd("~/Desktop/MXelsCalendGovt/elecReturns")

## consolidate 2016 demn-level from casillas
d <- read.csv("~/Desktop/MXelsCalendGovt/elecReturns/data/ay-nonfused/casillas/tla-pres-com-casilla-2013-on.csv")
d[1,]
str(d)
colnames(d)
## NAs to zero
sel.c <- c("pan","pri","prd", "pt","pvem","mc", "pna","pac","ps", "morena","pes","pest", "piss","rsp","fxm", "pri.pvem.ps","indep","indep2", "efec","nr","NUM_VOTOS_VALIDOS", "nul","tot","lisnom")
tmp <- d[,sel.c] # subset
head(tmp)
tmp[is.na(tmp)] <- 0
tmp -> d[,sel.c] # return


d <- read.csv("data/ay-nonfused/casillas/tla-pres-com-casilla-2013-on.csv")
str(d)

# drop A casillas max 
sel <- which(d$casilla=="A")
d$efec[sel]
d <- d[-sel,]

d$partic <- (d$efec + d$nr) / d$lisnom

summary(d$partic)
sel <- which(d$partic>1)

d$ord[sel]
