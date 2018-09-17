rm(list=ls())
wd <- "/home/eric/Desktop/MXelsCalendGovt/elecReturns/datosBrutos/resultCasillas"
setwd(wd)

# secc-munn
sm <- read.csv(file = "../../../redistrict/ife.ine/equivSecc/tablaEquivalenciasSeccionalesDesde1994.csv", stringsAsFactors = FALSE)
sm <- sm[,c("edon","seccion","munn")]

###########
# df 2018 #
###########
df <- read.csv(file = "eum2018all/eum2018dfca.csv", stringsAsFactors = FALSE)
df$casn <- paste(df$casn, df$TIPO_CASILLA, sep = "")


# hace numéricos los votos
df$edon <- as.numeric(df$edon)
df$disn <- as.numeric(df$disn)
df$seccion <- as.numeric(df$seccion)
df$pan <- as.numeric(df$pan)
df$pri <- as.numeric(df$pri)
df$prd <- as.numeric(df$prd)
df$pvem <- as.numeric(df$pvem)
df$pt <- as.numeric(df$pt)
df$mc <- as.numeric(df$mc)
df$panal <- as.numeric(df$panal)
df$morena <- as.numeric(df$morena)
df$pes <- as.numeric(df$pes)
df$pan.prd.mc <- as.numeric(df$pan.prd.mc)
df$pri.pvem.panal <- as.numeric(df$pri.pvem.panal)
df$pt.morena.pes <- as.numeric(df$pt.morena.pes)
df$indep1 <- as.numeric(df$indep1)
df$indep2 <- as.numeric(df$indep2)
df$nr <- as.numeric(df$nr)
df$nul <- as.numeric(df$nul)
df$tot <- as.numeric(df$tot)
df$lisnom <- as.numeric(df$lisnom)

# cambia NAs por ceros
tmp <- df[, c("edon", "disn", "seccion", "pan", "pri", "prd", "pvem", "pt", "mc", "panal", "morena", "pes", "pan.prd.mc", "pri.pvem.panal", "pt.morena.pes", "indep1", "indep2", "nr", "nul", "lisnom")]
tmp[is.na(tmp)==TRUE] <- 0
df[, c("edon", "disn", "seccion", "pan", "pri", "prd", "pvem", "pt", "mc", "panal", "morena", "pes", "pan.prd.mc", "pri.pvem.panal", "pt.morena.pes", "indep1", "indep2", "nr", "nul", "lisnom")] <- tmp

# efec
df$efec <- df$tot - df$nr - df$nul

## # consolida frente
## df$cpan <- 0
## sel <- which(df$edon!=17 & df$edon!=19)
## tmp <- df[sel,]
## tmp$cpan <- tmp$pan + tmp$prd + tmp$mc + tmp$pan.prd.mc
## tmp$pan <- tmp$prd <- tmp$mc <- 0
## df[sel,] <- tmp
## df$pan.prd.mc <- NULL
## # consolida coalicion amlo
## df$cmorena <- 0
## sel <- which(df$edon!=13)
## tmp <- df[sel,]
## tmp$cmorena <- tmp$morena + tmp$pt + tmp$pes + tmp$pt.morena.pes
## tmp$morena <- tmp$pt <- tmp$pes <- 0
## df[sel,] <- tmp
## df$pt.morena.pes <- NULL
## # consolida coal pri
## df$cpri <- 0
## sel <- which(df$edon!=13) # hay que hacer un listado de dónde no hubo coal pri
## tmp <- df[sel,]
## tmp$cpri <- tmp$pri + tmp$pvem + tmp$panal + tmp$pri.pvem.panal
## tmp$pri <- tmp$pvem <- tmp$panal <- 0
## df[sel,] <- tmp
## df$pri.pvem.panal <- NULL
## #
## rm(sel)

# clean
df$tot <- df$nr <- df$nul <- NULL
df$CASILLA <- df$TIPO_CASILLA <- df$EXT_CONTIGUA <- df$CASILLA <- df$OBSERVACIONES <- df$FECHA_HORA <- NULL
#df <- df[, c("edon", "edo", "disn", "cabecera", "seccion", "casn", "pan", "cpan", "pri", "cpri", "prd", "pvem", "pt", "mc", "panal", "morena", "cmorena", "pes", "indep1", "indep2", "efec", "lisnom")] # sort columns
df <- df[, c("edon", "edo", "disn", "cabecera", "seccion", "casn", "pan", "pri", "prd", "pvem", "pt", "mc", "panal", "morena", "pes", "pan.prd.mc", "pri.pvem.panal", "pt.morena.pes", "indep1", "indep2", "efec", "lisnom")] # sort columns

# consolida secciones
df2 <- df # duplica datos
df$pan <- ave(df$pan, as.factor(df$edon*10000+df$seccion), FUN=sum, na.rm=TRUE)
df$pri <- ave(df$pri, as.factor(df$edon*10000+df$seccion), FUN=sum, na.rm=TRUE)
df$prd <- ave(df$prd, as.factor(df$edon*10000+df$seccion), FUN=sum, na.rm=TRUE)
df$pvem <- ave(df$pvem, as.factor(df$edon*10000+df$seccion), FUN=sum, na.rm=TRUE)
df$pt <- ave(df$pt, as.factor(df$edon*10000+df$seccion), FUN=sum, na.rm=TRUE)
df$mc <- ave(df$mc, as.factor(df$edon*10000+df$seccion), FUN=sum, na.rm=TRUE)
df$panal <- ave(df$panal, as.factor(df$edon*10000+df$seccion), FUN=sum, na.rm=TRUE)
df$morena <- ave(df$morena, as.factor(df$edon*10000+df$seccion), FUN=sum, na.rm=TRUE)
df$pes <- ave(df$pes, as.factor(df$edon*10000+df$seccion), FUN=sum, na.rm=TRUE)
df$indep1 <- ave(df$indep1, as.factor(df$edon*10000+df$seccion), FUN=sum, na.rm=TRUE)
df$indep2 <- ave(df$indep2, as.factor(df$edon*10000+df$seccion), FUN=sum, na.rm=TRUE)
df$lisnom <- ave(df$lisnom, as.factor(df$edon*10000+df$seccion), FUN=sum, na.rm=TRUE)
df$efec <- ave(df$efec, as.factor(df$edon*10000+df$seccion), FUN=sum, na.rm=TRUE)
df$pan.prd.mc <- ave(df$pan.prd.mc, as.factor(df$edon*10000+df$seccion), FUN=sum, na.rm=TRUE)
df$pri.pvem.panal <- ave(df$pri.pvem.panal, as.factor(df$edon*10000+df$seccion), FUN=sum, na.rm=TRUE)
df$pt.morena.pes <- ave(df$pt.morena.pes, as.factor(df$edon*10000+df$seccion), FUN=sum, na.rm=TRUE)
## df$cpan <- ave(df$cpan, as.factor(df$edon*10000+df$seccion), FUN=sum, na.rm=TRUE)
## df$cpri <- ave(df$cpri, as.factor(df$edon*10000+df$seccion), FUN=sum, na.rm=TRUE)
## df$cmorena <- ave(df$cmorena, as.factor(df$edon*10000+df$seccion), FUN=sum, na.rm=TRUE)
#
df <- df[duplicated(df$edon*10000+df$seccion)==FALSE, ]
df$casn <- NULL
#
dfse <- df # renames data approriately
df <- df2  # recupera datos casilla

# consolida distritos
df2 <- df # duplica datos
df$pan <- ave(df$pan, as.factor(df$edon*100+df$disn), FUN=sum, na.rm=TRUE)
df$pri <- ave(df$pri, as.factor(df$edon*100+df$disn), FUN=sum, na.rm=TRUE)
df$prd <- ave(df$prd, as.factor(df$edon*100+df$disn), FUN=sum, na.rm=TRUE)
df$pvem <- ave(df$pvem, as.factor(df$edon*100+df$disn), FUN=sum, na.rm=TRUE)
df$pt <- ave(df$pt, as.factor(df$edon*100+df$disn), FUN=sum, na.rm=TRUE)
df$mc <- ave(df$mc, as.factor(df$edon*100+df$disn), FUN=sum, na.rm=TRUE)
df$panal <- ave(df$panal, as.factor(df$edon*100+df$disn), FUN=sum, na.rm=TRUE)
df$morena <- ave(df$morena, as.factor(df$edon*100+df$disn), FUN=sum, na.rm=TRUE)
df$pes <- ave(df$pes, as.factor(df$edon*100+df$disn), FUN=sum, na.rm=TRUE)
df$indep1 <- ave(df$indep1, as.factor(df$edon*100+df$disn), FUN=sum, na.rm=TRUE)
df$indep2 <- ave(df$indep2, as.factor(df$edon*100+df$disn), FUN=sum, na.rm=TRUE)
df$lisnom <- ave(df$lisnom, as.factor(df$edon*100+df$disn), FUN=sum, na.rm=TRUE)
df$efec <- ave(df$efec, as.factor(df$edon*100+df$disn), FUN=sum, na.rm=TRUE)
df$pan.prd.mc <- ave(df$pan.prd.mc, as.factor(df$edon*100+df$disn), FUN=sum, na.rm=TRUE)
df$pri.pvem.panal <- ave(df$pri.pvem.panal, as.factor(df$edon*100+df$disn), FUN=sum, na.rm=TRUE)
df$pt.morena.pes <- ave(df$pt.morena.pes, as.factor(df$edon*100+df$disn), FUN=sum, na.rm=TRUE)
## df$cpan <- ave(df$cpan, as.factor(df$edon*100+df$disn), FUN=sum, na.rm=TRUE)
## df$cpri <- ave(df$cpri, as.factor(df$edon*100+df$disn), FUN=sum, na.rm=TRUE)
## df$cmorena <- ave(df$cmorena, as.factor(df$edon*100+df$disn), FUN=sum, na.rm=TRUE)
#
df <- df[duplicated(df$edon*100+df$disn)==FALSE, ]
dim(df)
df$casn <- df$seccion <- NULL
#
dfdf <- df # renames data approriately
dfca <- df2  # recupera datos casilla
rm(df)

dim(dfdf)
dfdf[1,]
write.csv(dfdf, file = "eum2018all/eum2018dfdf.csv", row.names = FALSE)

# who won
dfdf$win <- "."
v <- dfdf[, c("pan","cpan","pri","cpri","prd","pvem","pt","mc","panal","morena","cmorena","pes","indep1","indep2")] # saca votos solamente
tmp <- apply(v, 1, max)
sel <- which(v$pan==tmp); dfdf$win[sel] <- "pan"
sel <- which(v$cpan==tmp); dfdf$win[sel] <- "cpan"
sel <- which(v$pri==tmp); dfdf$win[sel] <- "pri"
sel <- which(v$cpri==tmp); dfdf$win[sel] <- "cpri"
sel <- which(v$prd==tmp); dfdf$win[sel] <- "prd"
sel <- which(v$pt==tmp); dfdf$win[sel] <- "pt"
sel <- which(v$pvem==tmp); dfdf$win[sel] <- "pvem"
sel <- which(v$mc==tmp); dfdf$win[sel] <- "mc"
sel <- which(v$panal==tmp); dfdf$win[sel] <- "panal"
sel <- which(v$morena==tmp); dfdf$win[sel] <- "morena"
sel <- which(v$cmorena==tmp); dfdf$win[sel] <- "cmorena"
sel <- which(v$pes==tmp); dfdf$win[sel] <- "pes"
sel <- which(v$indep1==tmp); dfdf$win[sel] <- "indep"
sel <- which(v$indep2==tmp); dfdf$win[sel] <- "indep"

sel <- which(dfdf$win=="cmorena")
dfdf[sel, c("edon", "disn")]
