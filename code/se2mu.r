setwd("/home/eric/Desktop/MXelsCalendGovt/elecReturns/datosBrutos/")

file <- "zac2018ayca.csv"
d <- read.csv(file = file, stringsAsFactors = FALSE, fileEncoding="utf-8")

colnames(d)
str(d)
d[1,]

# make numeric
sel <- grep("v[.]*|nr|nul", colnames(d))
sel <- c(8:38)
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
    d[,i] <- ave(d[,i], as.factor(d$mun), FUN=sum, na.rm=TRUE)
}

head(d)

# drop redundant obs
d <- d[duplicated(d$mun)==FALSE,]

colnames(d)
sel <- c(2,11,14:34)
d <- d[,sel]

d$seccion <- NULL
d$casilla <- NULL
d$tipo <- NULL
d$ext <- NULL

colnames(d)



file2 <- "zac2018aymu.csv"
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


