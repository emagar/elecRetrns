################################################################
## Code aggregates casilla votes to federal district returns  ##
## Prepared for prep results, re-use with final results       ##
################################################################

setwd("/home/eric/Downloads/Desktop/MXelsCalendGovt/elecReturns/datosBrutos/not-in-git/resultCasillas/eum2021")

d <- read.csv("eum2021dfca-prep.csv", stringsAsFactors = FALSE)

# make numeric
d$PAN <- as.numeric(d$PAN)
d$PRI <- as.numeric(d$PRI)
d$PRD <- as.numeric(d$PRD)
d$PVEM <- as.numeric(d$PVEM)
d$PT <- as.numeric(d$PT)
d$MC <- as.numeric(d$MC)
d$MORENA <- as.numeric(d$MORENA)
d$PES <- as.numeric(d$PES)
d$RSP <- as.numeric(d$RSP)
d$FXM <- as.numeric(d$FXM)
d$CAND_IND_1 <- as.numeric(d$CAND_IND_1)
d$CAND_IND_2 <- as.numeric(d$CAND_IND_2)
d$CAND_IND_3 <- as.numeric(d$CAND_IND_3)
d$PAN.PRI.PRD <- as.numeric(d$PAN.PRI.PRD)
d$PAN.PRI <- as.numeric(d$PAN.PRI)
d$PAN.PRD <- as.numeric(d$PAN.PRD)
d$PRI.PRD <- as.numeric(d$PRI.PRD)
d$PVEM.PT.MORENA <- as.numeric(d$PVEM.PT.MORENA)
d$PVEM.PT <- as.numeric(d$PVEM.PT)
d$PVEM.MORENA <- as.numeric(d$PVEM.MORENA)
d$PT.MORENA <- as.numeric(d$PT.MORENA)
d$NO_REGISTRADOS <- as.numeric(d$NO_REGISTRADOS)
d$NULOS <- as.numeric(d$NULOS)
d$TOTAL_VOTOS_ASENTADO <- as.numeric(d$TOTAL_VOTOS_ASENTADO)
d$TOTAL_VOTOS_CALCULADOS <- as.numeric(d$TOTAL_VOTOS_CALCULADOS)
d$lisnom <- as.numeric(d$lisnom)

# NAs to 0
d[is.na(d)] <- 0

# aggregate districts
d$PAN <- ave(d$PAN, as.factor(paste(d$edon, d$disn)), FUN=sum, na.rm=TRUE)
d$PRI <- ave(d$PRI, as.factor(paste(d$edon, d$disn)), FUN=sum, na.rm=TRUE)
d$PRD <- ave(d$PRD, as.factor(paste(d$edon, d$disn)), FUN=sum, na.rm=TRUE)
d$PVEM <- ave(d$PVEM, as.factor(paste(d$edon, d$disn)), FUN=sum, na.rm=TRUE)
d$PT <- ave(d$PT, as.factor(paste(d$edon, d$disn)), FUN=sum, na.rm=TRUE)
d$MC <- ave(d$MC, as.factor(paste(d$edon, d$disn)), FUN=sum, na.rm=TRUE)
d$MORENA <- ave(d$MORENA, as.factor(paste(d$edon, d$disn)), FUN=sum, na.rm=TRUE)
d$PES <- ave(d$PES, as.factor(paste(d$edon, d$disn)), FUN=sum, na.rm=TRUE)
d$RSP <- ave(d$RSP, as.factor(paste(d$edon, d$disn)), FUN=sum, na.rm=TRUE)
d$FXM <- ave(d$FXM, as.factor(paste(d$edon, d$disn)), FUN=sum, na.rm=TRUE)
d$CAND_IND_1 <- ave(d$CAND_IND_1, as.factor(paste(d$edon, d$disn)), FUN=sum, na.rm=TRUE)
d$CAND_IND_2 <- ave(d$CAND_IND_2, as.factor(paste(d$edon, d$disn)), FUN=sum, na.rm=TRUE)
d$CAND_IND_3 <- ave(d$CAND_IND_3, as.factor(paste(d$edon, d$disn)), FUN=sum, na.rm=TRUE)
d$PAN.PRI.PRD <- ave(d$PAN.PRI.PRD, as.factor(paste(d$edon, d$disn)), FUN=sum, na.rm=TRUE)
d$PAN.PRI <- ave(d$PAN.PRI, as.factor(paste(d$edon, d$disn)), FUN=sum, na.rm=TRUE)
d$PAN.PRD <- ave(d$PAN.PRD, as.factor(paste(d$edon, d$disn)), FUN=sum, na.rm=TRUE)
d$PRI.PRD <- ave(d$PRI.PRD, as.factor(paste(d$edon, d$disn)), FUN=sum, na.rm=TRUE)
d$PVEM.PT.MORENA <- ave(d$PVEM.PT.MORENA, as.factor(paste(d$edon, d$disn)), FUN=sum, na.rm=TRUE)
d$PVEM.PT <- ave(d$PVEM.PT, as.factor(paste(d$edon, d$disn)), FUN=sum, na.rm=TRUE)
d$PVEM.MORENA <- ave(d$PVEM.MORENA, as.factor(paste(d$edon, d$disn)), FUN=sum, na.rm=TRUE)
d$PT.MORENA <- ave(d$PT.MORENA, as.factor(paste(d$edon, d$disn)), FUN=sum, na.rm=TRUE)
d$NO_REGISTRADOS <- ave(d$NO_REGISTRADOS, as.factor(paste(d$edon, d$disn)), FUN=sum, na.rm=TRUE)
d$NULOS <- ave(d$NULOS, as.factor(paste(d$edon, d$disn)), FUN=sum, na.rm=TRUE)
d$TOTAL_VOTOS_ASENTADO <- ave(d$TOTAL_VOTOS_ASENTADO, as.factor(paste(d$edon, d$disn)), FUN=sum, na.rm=TRUE)
d$TOTAL_VOTOS_CALCULADOS <- ave(d$TOTAL_VOTOS_CALCULADOS, as.factor(paste(d$edon, d$disn)), FUN=sum, na.rm=TRUE)
d$lisnom <- ave(d$lisnom, as.factor(paste(d$edon, d$disn)), FUN=sum, na.rm=TRUE)

# drop dedundant secciones
d <- d[duplicated(as.factor(paste(d$edon, d$disn)))==FALSE,]

# aggreg coalition votes
d$PAN.PRI.PRD <- d$PAN.PRI.PRD + d$PRI.PRD + d$PAN.PRD + d$PAN.PRI  
d$PRI.PRD <- d$PAN.PRD <- d$PAN.PRI <- NULL
sel <- which(d$PAN.PRI.PRD > 0)
d$PAN.PRI.PRD[sel] <- d$PAN.PRI.PRD[sel] + d$PAN[sel] + d$PRI[sel] + d$PRD[sel]  
d$PAN[sel] <- d$PRI[sel] <- d$PRD[sel] <- 0  
#
d$PVEM.PT.MORENA <- d$PVEM.PT.MORENA + d$PT.MORENA + d$PVEM.MORENA + d$PVEM.PT  
d$PT.MORENA <- d$PVEM.MORENA <- d$PVEM.PT <- NULL
sel <- which(d$PVEM.PT.MORENA > 0)
d$PVEM.PT.MORENA[sel] <- d$PVEM.PT.MORENA[sel] + d$PVEM[sel] + d$PT[sel] + d$MORENA[sel]  
d$PVEM[sel] <- d$PT[sel] <- d$MORENA[sel] <- 0  

# votes only
sel <- which(colnames(d) %in% c("PAN", "PRI", "PRD", "PVEM", "PT", "MC", "MORENA", "PES", "RSP", "FXM", "CAND_IND_1", "CAND_IND_2", "CAND_IND_3", "PAN.PRI.PRD", "PVEM.PT.MORENA"))

wv <- apply(d[,sel], 1, max) # max vote

f <- function(x,y) which(x == y)

apply(d[,sel], 1, FUN = function(X) max(X))

d$win <- colnames(d[,sel])[apply(d[,sel], 1, FUN = function(X) which(X == max(X)))]

write.table(d[,"win"], "clipboard")



colnames(d)

View(d$win)

d[,grep("PVEM|PT|MORENA", colnames(d))]



