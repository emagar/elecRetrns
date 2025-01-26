setwd("/home/eric/Desktop/MXelsCalendGovt/elecReturns/datosBrutos/")

file <- "tab2018ayca.csv"
d <- read.csv(file = file, stringsAsFactors = FALSE, fileEncoding="utf-8")
## or from clipboard
d <- read.delim("clipboard")


d[1,]
str(d)
colnames(d)

# make numeric
sel <- grep("v[.]*|nr|nul|tot|lisnom", colnames(d))
sel <- c(7:39,50)
sel <- c("pan","pri","prd","pvem","pt","mc","morena","indep","pan.pri.prd","pan.pri","pan.prd","pri.prd","pvem.pt.morena","pvem.pt","pvem.morena","pt.morena","nr","nul","tot","lisnom")
v <- d[,sel]
v[1,]
for (i in 1:ncol(v)){
    v[,i] <- as.numeric(v[,i])
}
v[is.na(v)] <- 0
d[,sel] <- v

head(d)
str(d)
colnames(d)

# consolidate mun votes
for (i in sel){
    d[,i] <- ave(d[,i], as.factor(d$ife), FUN=sum, na.rm=TRUE)
}

head(d)

# drop redundant obs
d <- d[duplicated(d$ife)==FALSE,]

colnames(d)
sel <- c(5:6,17:33,36)
d <- d[,sel]

data.frame(d$mun)

d$seccion <- NULL
d$casilla <- NULL
d$tipo <- NULL
d$ext <- NULL
d$nota <- NULL

colnames(d)
head(d)

## add inegi
pth <- ifelse (Sys.info()["user"] %in% c("eric", "magar"),
    "~/Dropbox/data/useful-functions",
    "https://raw.githubusercontent.com/emagar/useful-functions/master"
    )
source( paste(pth, "inegi2ife.r", sep = "/") )
rm(pth)
d$ife <- d$ife + 16000
d$inegi <- ife2inegi(d$ife)

setwd("~/Downloads")
file2 <- "tmp.csv"
write.csv(d, file = file2, row.names = FALSE)


# re-read after hand cleaning
d <- read.csv(file = file2, stringsAsFactors = FALSE)

head(d)
colnames(d)

# pick cols with coalitions
v <- d[,c(18:28)]

sel <- which(v$pri.pvem.panal.pd>0)
v[sel,][1,]

v[sel,1] <- rowSums(v[sel,])
v[sel,2:ncol(v)] <- 0

sel <- which(v$pri.pvem.panal.pd==0)
v[sel,]

d[sel, c(18:28)] <- v[sel,]


d <- d[,-c(19:28)]

head(d)
colnames(d)

l <- colnames(d)[5:19]
l2 <- data.frame(matrix(rep(l, nrow(d)), ncol = length(l), nrow = nrow(d), byrow = TRUE))
for (i in 1:ncol(l2)){
    l2[,i] <- gsub("[.]", "-", l2[,i])
}

n <- c(paste("l0", 1:9, sep = ""), paste("l", 10:15, sep = ""))

colnames(l2) <- n

l2

d <- cbind(d, l2)

colnames(d)

n <- c(paste("v0", 1:9, sep = ""), paste("v", 10:15, sep = ""))

colnames(d)[5:19] <- n

d <- d[, c("Municipio", "disn", "lisnom", "v01","l01", "v02","l02", "v03","l03", "v04","l04", "v05","l05", "v06","l06", "v07","l07", "v08","l08", "v09","l09", "v10","l10", "v11","l11", "v12","l12", "v13","l13", "v14","l14", "v15","l15", "nul","Total")]

write.csv(d, file = file2, row.names = FALSE)


