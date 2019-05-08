;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

wd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/data/casillas"
setwd(wd)

########
# 2006 #
########
pr <- read.csv(file = "pre2006.csv", sep = ",", stringsAsFactors = FALSE)
pr[1,]
# adds voto efectivo
sel <- which(colnames(pr) %in% c("pan","pri.pvem", "prd.pt.c","panal","asdc"))
tmp <- pr[,sel]
pr$efec <- rowSums(tmp)
pr$fch <- pr$pan
pr$rmp <- pr$pri.pvem
pr$amlo <- pr$prd.pt.c
pr$pna <- pr$panal
pr <- pr[, c("edon", "disn","seccion","casilla","munn","fch","rmp", "amlo","pna","asdc","efec","lisnom")]
# drop casillas especiales votos extranjero
sel <- which(pr$seccion==0)
pr <- pr[-sel,]
# cambia NAs por ceros
pr[is.na(pr)==TRUE] <- 0
# drop unreported casillas
sel <- which(pr$efec==0)
pr[sel,]
pr <- pr[-sel,]

# consolida secciones
pr$fch <- ave(pr$fch, as.factor(pr$edon*10000+pr$seccion), FUN=sum, na.rm=TRUE)
pr$amlo <- ave(pr$amlo, as.factor(pr$edon*10000+pr$seccion), FUN=sum, na.rm=TRUE)
pr$rmp <- ave(pr$rmp, as.factor(pr$edon*10000+pr$seccion), FUN=sum, na.rm=TRUE)
pr$pna <- ave(pr$pna, as.factor(pr$edon*10000+pr$seccion), FUN=sum, na.rm=TRUE)
pr$asdc <- ave(pr$asdc, as.factor(pr$edon*10000+pr$seccion), FUN=sum, na.rm=TRUE)
pr$efec <- ave(pr$efec, as.factor(pr$edon*10000+pr$seccion), FUN=sum, na.rm=TRUE)
pr <- pr[duplicated(pr$edon*10000+pr$seccion)==FALSE, ]

# version secciones
pr.se <- pr
# shares
pr.se$fch  <- pr.se$fch/pr.se$efec
pr.se$rmp <- pr.se$rmp/pr.se$efec
pr.se$amlo06 <- pr.se$amlo/pr.se$efec
pr.se$pna  <- pr.se$pna/pr.se$efec
pr.se$asdc  <- pr.se$asdc/pr.se$efec
pr.se$amlo <- NULL
pr.se$casilla <- NULL

# consolida municipios
pr$fch <- ave(pr$fch, as.factor(pr$edon*1000+pr$munn), FUN=sum, na.rm=TRUE)
pr$rmp <- ave(pr$rmp, as.factor(pr$edon*1000+pr$munn), FUN=sum, na.rm=TRUE)
pr$amlo <- ave(pr$amlo, as.factor(pr$edon*1000+pr$munn), FUN=sum, na.rm=TRUE)
pr$pna <- ave(pr$pna, as.factor(pr$edon*1000+pr$munn), FUN=sum, na.rm=TRUE)
pr$asdc <- ave(pr$asdc, as.factor(pr$edon*1000+pr$munn), FUN=sum, na.rm=TRUE)
pr$efec <- ave(pr$efec, as.factor(pr$edon*1000+pr$munn), FUN=sum, na.rm=TRUE)
pr <- pr[duplicated(pr$edon*1000+pr$munn)==FALSE, ]
pr$seccion <- NULL
pr$disn <- pr$casilla <- NULL
# export munic aggregates
write.csv(pr, file = "../municipios/pre2006.csv", row.names = FALSE)

# shares
pr$fch  <- pr$fch/pr$efec
pr$rmp <- pr$rmp/pr$efec
pr$amlo06 <- pr$amlo/pr$efec
pr$pna <- pr$pna/pr$efec
pr$asdc <- pr$asdc/pr$efec
pr$amlo <- NULL
pr <- pr[,c("edon","munn","fch","rmp","amlo06","pna","asdc","efec","lisnom")]

# version municipios
pr.mu <- pr
rm(pr)



