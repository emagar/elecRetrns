rm(list = ls())
wdir <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns")
setwd(wdir)

df <- read.csv("dfSeccion2012.csv", header=TRUE)
df$casillasNoInstaladas <- NULL; df$casillasNoEntregadas <- NULL; df$ord <- NULL
df$listanomcasilla <- NULL # OJO: averiguar diferencia con pr$lisnom
df <- df[order(df$edon,df$seccion),]
df$efec <- df$pan+df$pri+df$prd+df$pvem+df$pt+df$mc+df$panal+df$pripvem+df$prdptmc+df$prdpt+df$prdmc+df$ptmc

pr <- read.csv("prSeccion2012.csv", header=TRUE)
pr$ord <- NULL
pr$listanomcasilla <- NULL # OJO: averiguar diferencia con pr$lisnom
pr <- pr[order(pr$edon,pr$seccion),]
pr$efec <- pr$pan+pr$pri+pr$prd+pr$pvem+pr$pt+pr$mc+pr$panal+pr$pripvem+pr$prdptmc+pr$prdpt+pr$prdmc+pr$ptmc

# VERIFY SECCIONES IN BOTH  DATASETS MATCH (SUM MUST BE ZERO)
tmp <- data.frame(df=df$edon*10000+df$seccion, pr=pr$edon*10000+pr$seccion); tmp$dif <- tmp$df-tmp$pr
sum(tmp$dif)
rm(tmp)

el <- data.frame(edon=df$edon, disn=df$disn, munn=df$munn, seccion=df$seccion, urbrur=df$urbrur, lisnom=df$lisnom, df.pan=df$pan, df.pri=df$pri, df.prd=df$prd, df.pt=df$pt, df.pvem=df$pvem, df.mc=df$mc, df.panal=df$panal, df.pripvem=df$pripvem, df.prdptmc=df$prdptmc, df.prdpt=df$prdpt, df.prdmc=df$prdmc, df.ptmc=df$ptmc, df.efec=df$efec, df.nul=df$nul)
rm(df)
el$pr.pan <- pr$pan; el$pr.pri <- pr$pri; el$pr.prd <- pr$prd; el$pr.pt <- pr$pt; el$pr.pvem <- pr$pvem; el$pr.mc <- pr$mc; el$pr.panal <- pr$panal; el$pr.pripvem <- pr$pripvem; el$pr.prdptmc <- pr$prdptmc; el$pr.prdpt <- pr$prdpt; el$pr.prdmc <- pr$prdmc; el$pr.ptmc <- pr$ptmc; el$pr.efec <- pr$efec; el$pr.nul <- pr$nul
rm(pr)
head(el)

#AQUI NECESITO INFO SOCIODEMOGRAFICA DE LAS SECCIONES!!!!
#AQUI NECESITO MATRIZ DE VECINDAD DE LAS SECCIONES!!!!

# COALICIONES COMO UdeA
el$df.coalpri <- el$df.pri+el$df.pvem+el$df.pripvem
el$df.coalprd <- el$df.prd+el$df.pt+el$df.mc+el$df.prdptmc+el$df.prdpt+el$df.prdmc+el$df.ptmc
el$pr.coalpri <- el$pr.pri+el$pr.pvem+el$pr.pripvem
el$pr.coalprd <- el$pr.prd+el$pr.pt+el$pr.mc+el$pr.prdptmc+el$pr.prdpt+el$pr.prdmc+el$pr.ptmc
##
## DF
xx <- data.frame(pan=el$df.pan, pri=el$df.coalpri, prd=el$df.coalprd, panal=el$df.panal)
xxx <- apply(xx, 1, max)
win.df.coal <- rep(NA, nrow(xx))
win.df.coal[xxx==xx[,1]] <- "pan"; win.df.coal[xxx==xx[,2]] <- "pri"; win.df.coal[xxx==xx[,3]] <- "prd"; win.df.coal[xxx==xx[,4]] <- "panal" 
##
## MATRIX OF SECTION PARTY RANKS
f = function(x) { 5-rank(x, na.last = FALSE, ties.method = "random") }
rank.df <- as.data.frame(t(apply(xx, 1, f)))
rank.pr <- as.data.frame(t(apply(xx, 1, f)))
## VOTES FOR 2nd, 3rd PARTIES IN SECTION
xxx <- cbind(rank.df, xx)
f = function(x) { as.numeric(x[c(rep(FALSE,4),x[1:4]==2)]) } ## x is a vote vector, returns second largest element
v2nd.df.coal <- apply(xxx, 1, f)
f = function(x) { as.numeric(x[c(rep(FALSE,4),x[1:4]==3)]) } ## x is a vote vector, returns third largest element
v3rd.df.coal <- apply(xxx, 1, f)
rm(xx,xxx)
## Sección SF Ratios
sf.df.coal <- v3rd.df.coal/v2nd.df.coal
##
## PR
xx <- data.frame(pan=el$pr.pan, pri=el$pr.coalpri, prd=el$pr.coalprd, panal=el$pr.panal)
xxx <- apply(xx, 1, max)
win.pr.coal <- rep(NA, nrow(xx))
win.pr.coal[xxx==xx[,1]] <- "pan"; win.pr.coal[xxx==xx[,2]] <- "pri"; win.pr.coal[xxx==xx[,3]] <- "prd"; win.pr.coal[xxx==xx[,4]] <- "panal" 
##
## MATRIX OF SECTION PARTY RANKS
f = function(x) { 5-rank(x, na.last = FALSE, ties.method = "random") }
rank.pr <- as.data.frame(t(apply(xx, 1, f)))
rank.pr <- as.data.frame(t(apply(xx, 1, f)))
## VOTES FOR 2nd, 3rd PARTIES IN SECTION
xxx <- cbind(rank.pr, xx)
f = function(x) { as.numeric(x[c(rep(FALSE,4),x[1:4]==2)]) } ## x is a vote vector, returns second largest element
v2nd.pr.coal <- apply(xxx, 1, f)
f = function(x) { as.numeric(x[c(rep(FALSE,4),x[1:4]==3)]) } ## x is a vote vector, returns third largest element
v3rd.pr.coal <- apply(xxx, 1, f)
rm(xx,xxx)
## Sección SF Ratios
sf.pr.coal <- v3rd.pr.coal/v2nd.pr.coal
##
## PLOT SF ratios by seccion
hist(sf.df.coal)
hist(sf.pr.coal)

#PLOT MARGINS
mg <- data.frame(df.pripan=el$df.coalpri-el$df.pan, df.priprd=el$df.coalpri-el$df.coalprd, df.prdpan=el$df.coalprd-el$df.pan, pr.pripan=el$pr.coalpri-el$pr.pan, pr.priprd=el$pr.coalpri-el$pr.coalprd, pr.prdpan=el$pr.coalprd-el$pr.pan)
#PRI-PAN
uno.1  <- round(sum(mg$df.pripan>=0 & mg$pr.pripan>=0 & mg$df.pripan<mg$pr.pripan)/nrow(mg),digits=2)
uno.2  <- round(sum(mg$df.pripan>=0 & mg$pr.pripan>=0 & mg$df.pripan>=mg$pr.pripan)/nrow(mg),digits=2)
dos    <- round(sum(mg$df.pripan>=0 & mg$pr.pripan<0)/nrow(mg),digits=2)
tres.1 <- round(sum(mg$df.pripan<0 & mg$pr.pripan<0 & mg$df.pripan>=mg$pr.pripan)/nrow(mg),digits=2)
tres.2 <- round(sum(mg$df.pripan<0 & mg$pr.pripan<0 & mg$df.pripan<mg$pr.pripan)/nrow(mg),digits=2)
cuatro <- round(sum(mg$df.pripan<0 & mg$pr.pripan>=0)/nrow(mg),digits=2)
#plot(c(min(mg$df.pripan), max(mg$df.pripan)), c(min(mg$pr.pripan), max(mg$pr.pripan)), type="n")
plot(c(-2000,2000), c(-2000,2000), type="n", xlab = "PRI-PAN diputados margin", ylab = "Peña-Vázquez Mota margin")
points(mg$df.pripan, mg$pr.pripan, pch=".", cex=1)
abline(a=0,b=1,col="red")
abline(h=0, col="red"); abline(v=0, col="red");
#abline(lm(formula= mg$pr.pripan ~ mg$df.pripan))
text(500,1500,uno.1)
text(1500,500,uno.2)
text(1000,-1000,dos)
text(-500,-1500,tres.1)
text(-1500,-500,tres.2)
text(-1000,1000,cuatro)
##
res.pripan <- mg$pr.pripan-mg$df.pripan
res.pripan.plus <- res.pripan[res.pripan>0]
res.pripan.minus <- res.pripan[res.pripan<=0]
length(res.pripan); length(res.pripan.plus); length(res.pripan.minus)
mean(res.pripan.plus)
mean(res.pripan.minus)
mean(res.pripan)
##
## FOR USE IN TEXT: Distinguishing positive residuals from the 45-degree line (points above the line) from negative residuals is illustrative. The mean positive residual is 42.5---implying that in secciones where Peña overperformed his party's deputy candidate, his margin over the PAN was 42.5 votes larger than the PRI--PAN deputy margin. At $-44-7$, the mean negative residual was larger, but a much larger number of secciones above (about 38,500) than below (about 28,000) gave Peña a mean margin 6 votes larger in each sección than his fellow deputies.   

plot(mg$df.pripan[res.pripan>0],res.pripan.plus)
plot(mg$df.pripan[res.pripan<=0],res.pripan.minus)
points(mg$df.pripan, mg$pr.pripan, pch=".", cex=1)



#PRI-PRD
uno.1  <- round(sum(mg$df.priprd>=0 & mg$pr.priprd>=0 & mg$df.priprd<mg$pr.priprd)/nrow(mg),digits=2)
uno.2  <- round(sum(mg$df.priprd>=0 & mg$pr.priprd>=0 & mg$df.priprd>=mg$pr.priprd)/nrow(mg),digits=2)
dos    <- round(sum(mg$df.priprd>=0 & mg$pr.priprd<0)/nrow(mg),digits=2)
tres.1 <- round(sum(mg$df.priprd<0 & mg$pr.priprd<0 & mg$df.priprd>=mg$pr.priprd)/nrow(mg),digits=2)
tres.2 <- round(sum(mg$df.priprd<0 & mg$pr.priprd<0 & mg$df.priprd<mg$pr.priprd)/nrow(mg),digits=2)
cuatro <- round(sum(mg$df.priprd<0 & mg$pr.priprd>=0)/nrow(mg),digits=2)
#plot(c(min(mg$df.priprd), max(mg$df.priprd)), c(min(mg$pr.priprd), max(mg$pr.priprd)), type="n")
plot(c(-2000,2000), c(-2000,2000), type="n", xlab = "PRI-PRD diputados margin", ylab = "Peña-AMLO margin")
points(mg$df.priprd, mg$pr.priprd, pch=".", cex=1)
abline(a=0,b=1,col="red")
abline(h=0, col="red"); abline(v=0, col="red");
#abline(lm(formula= mg$pr.priprd ~ mg$df.priprd))
text(500,1500,uno.1)
text(1500,500,uno.2)
text(1000,-1000,dos)
text(-500,-1500,tres.1)
text(-1500,-500,tres.2)
text(-1000,1000,cuatro)

#PRD-PAN
uno.1  <- round(sum(mg$df.prdpan>=0 & mg$pr.prdpan>=0 & mg$df.prdpan<mg$pr.prdpan)/nrow(mg),digits=2)
uno.2  <- round(sum(mg$df.prdpan>=0 & mg$pr.prdpan>=0 & mg$df.prdpan>=mg$pr.prdpan)/nrow(mg),digits=2)
dos    <- round(sum(mg$df.prdpan>=0 & mg$pr.prdpan<0)/nrow(mg),digits=2)
tres.1 <- round(sum(mg$df.prdpan<0 & mg$pr.prdpan<0 & mg$df.prdpan>=mg$pr.prdpan)/nrow(mg),digits=2)
tres.2 <- round(sum(mg$df.prdpan<0 & mg$pr.prdpan<0 & mg$df.prdpan<mg$pr.prdpan)/nrow(mg),digits=2)
cuatro <- round(sum(mg$df.prdpan<0 & mg$pr.prdpan>=0)/nrow(mg),digits=2)
#plot(c(min(mg$df.prdpan), max(mg$df.prdpan)), c(min(mg$pr.prdpan), max(mg$pr.prdpan)), type="n")
plot(c(-2000,2000), c(-2000,2000), type="n", xlab = "PRD-PAN diputados margin", ylab = "AMLO-Vázquez Mota margin")
points(mg$df.prdpan, mg$pr.prdpan, pch=".", cex=1)
abline(a=0,b=1,col="red")
abline(h=0, col="red"); abline(v=0, col="red");
#abline(lm(formula= mg$pr.prdpan ~ mg$df.prdpan))
text(500,1500,uno.1)
text(1500,500,uno.2)
text(1000,-1000,dos)
text(-500,-1500,tres.1)
text(-1500,-500,tres.2)
text(-1000,1000,cuatro)

## Seccion shares
#el$df.pan.sh <- el$df.pan / el$df.efec
#el$df.coalpri.sh <- el$df.coalpri / el$df.efec
#el$df.coalprd.sh <- el$df.coalprd / el$df.efec
#el$pr.pan.sh <- el$pr.pan / el$pr.efec
#el$pr.coalpri.sh <- el$pr.coalpri / el$pr.efec
#el$pr.coalprd.sh <- el$pr.coalprd / el$pr.efec
## TRIPLOT HERE
library(vcd)
xx <- el$df.pan+el$df.coalpri+el$df.coalprd
xxx <- data.frame(PRD=el$df.coalprd/xx, PAN=el$df.pan/xx, PRI=el$df.coalpri/xx)
color <- rep("black", length(win.df.coal));
for (i in 1:length(win.df.coal)){
  color[i] <- ifelse(win.df.coal[i]=="pan", "blue",
                     ifelse(win.df.coal[i]=="pri", "red",
                            ifelse(win.df.coal[i]=="prd", "gold", "black")))
                                }
color <- color[is.na(xxx[,1])==FALSE]; xxx <- xxx[is.na(xxx[,1])==FALSE,]
gdir <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elRef/graphs")
setwd(gdir)
pdf(file="ternary.df.pdf",width=7, height=7)
ternaryplot(xxx, scale = 1, dimnames_position = "corner", labels = "outside", pch=".", main="Deputies", col=color)
dev.off()
setwd(wdir)

xx <- el$pr.pan+el$pr.coalpri+el$pr.coalprd
xxx <- data.frame(AMLO=el$pr.coalprd/xx, JVM=el$pr.pan/xx, Peña=el$pr.coalpri/xx)
color <- rep("black", length(win.pr.coal));
for (i in 1:length(win.pr.coal)){
  color[i] <- ifelse(win.pr.coal[i]=="pan", "blue",
                     ifelse(win.pr.coal[i]=="pri", "red",
                            ifelse(win.pr.coal[i]=="prd", "gold", "black")))
                                }
color <- color[is.na(xxx[,1])==FALSE]; xxx <- xxx[is.na(xxx[,1])==FALSE,]
library(vcd)
gdir <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elRef/graphs")
setwd(gdir)
#pdf(file="ternary.pr.pdf",width=7, height=7)
ternaryplot(xxx, scale = 1, dimnames_position = "corner", labels = "outside", pch=".", main="President", col=color)
#dev.off()
setwd(wdir)

##OJO: ESTOS SON DATOS DEL CONTEO 2005. EN /mapas/secciones/edo/ HAY DBF CON CENSO 2010
library(foreign)
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/ags/01_ags_lengind.dbf")
lengind <- data.frame(edon=tmp$ENTIDAD, seccion=tmp$SECCION, habli=tmp$HABLEIN1)
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/bc/02_bc_lengind.dbf")
old <- nrow(lengind); new <- nrow(tmp);
lengind[(old+1):(old+new),1] <- tmp$ENTIDAD; lengind[(old+1):(old+new),2] <- tmp$SECCION; lengind[(old+1):(old+new),3] <- tmp$HABLEIN1; 
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/bcs/03_bcs_lengind.dbf")
old <- nrow(lengind); new <- nrow(tmp);
lengind[(old+1):(old+new),1] <- tmp$ENTIDAD; lengind[(old+1):(old+new),2] <- tmp$SECCION; lengind[(old+1):(old+new),3] <- tmp$HABLEIN1; 
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/cam/04_camp_lengind.dbf")
old <- nrow(lengind); new <- nrow(tmp);
lengind[(old+1):(old+new),1] <- tmp$ENTIDAD; lengind[(old+1):(old+new),2] <- tmp$SECCION; lengind[(old+1):(old+new),3] <- tmp$HABLEIN1; 
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/coa/05_coah_lengind.dbf")
old <- nrow(lengind); new <- nrow(tmp);
lengind[(old+1):(old+new),1] <- tmp$ENTIDAD; lengind[(old+1):(old+new),2] <- tmp$SECCION; lengind[(old+1):(old+new),3] <- tmp$HABLEIN1;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/col/06_col_lengind.dbf")
old <- nrow(lengind); new <- nrow(tmp);
lengind[(old+1):(old+new),1] <- tmp$ENTIDAD; lengind[(old+1):(old+new),2] <- tmp$SECCION; lengind[(old+1):(old+new),3] <- tmp$HABLEIN1; 
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/cps/07_chiap_lengind.dbf")
old <- nrow(lengind); new <- nrow(tmp);
lengind[(old+1):(old+new),1] <- tmp$ENTIDAD; lengind[(old+1):(old+new),2] <- tmp$SECCION; lengind[(old+1):(old+new),3] <- tmp$HABLEIN1; 
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/cua/08_chih_lengind.dbf")
old <- nrow(lengind); new <- nrow(tmp);
lengind[(old+1):(old+new),1] <- tmp$ENTIDAD; lengind[(old+1):(old+new),2] <- tmp$SECCION; lengind[(old+1):(old+new),3] <- tmp$HABLEIN1; 
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/df/09_df_lengind.dbf")
old <- nrow(lengind); new <- nrow(tmp);
lengind[(old+1):(old+new),1] <- tmp$ENTIDAD; lengind[(old+1):(old+new),2] <- tmp$SECCION; lengind[(old+1):(old+new),3] <- tmp$HABLEIN1; 
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/dgo/10_dgo_lengind.dbf")
old <- nrow(lengind); new <- nrow(tmp);
lengind[(old+1):(old+new),1] <- tmp$ENTIDAD; lengind[(old+1):(old+new),2] <- tmp$SECCION; lengind[(old+1):(old+new),3] <- tmp$HABLEIN1; 
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/gua/11_gto_lengind.dbf")
old <- nrow(lengind); new <- nrow(tmp);
lengind[(old+1):(old+new),1] <- tmp$ENTIDAD; lengind[(old+1):(old+new),2] <- tmp$SECCION; lengind[(old+1):(old+new),3] <- tmp$HABLEIN1; 
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/gue/12_gro_lengind.dbf")
old <- nrow(lengind); new <- nrow(tmp);
lengind[(old+1):(old+new),1] <- tmp$ENTIDAD; lengind[(old+1):(old+new),2] <- tmp$SECCION; lengind[(old+1):(old+new),3] <- tmp$HABLEIN1; 
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/hgo/13_hgo_lengind.dbf")
old <- nrow(lengind); new <- nrow(tmp);
lengind[(old+1):(old+new),1] <- tmp$ENTIDAD; lengind[(old+1):(old+new),2] <- tmp$SECCION; lengind[(old+1):(old+new),3] <- tmp$HABLEIN1; 
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/jal/14_jal_lengind.dbf")
old <- nrow(lengind); new <- nrow(tmp);
lengind[(old+1):(old+new),1] <- tmp$ENTIDAD; lengind[(old+1):(old+new),2] <- tmp$SECCION; lengind[(old+1):(old+new),3] <- tmp$HABLEIN1; 
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/mex/15_mex_lengind.dbf")
old <- nrow(lengind); new <- nrow(tmp);
lengind[(old+1):(old+new),1] <- tmp$ENTIDAD; lengind[(old+1):(old+new),2] <- tmp$SECCION; lengind[(old+1):(old+new),3] <- tmp$HABLEIN1; 
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/mic/16_mich_lengind.dbf")
old <- nrow(lengind); new <- nrow(tmp);
lengind[(old+1):(old+new),1] <- tmp$ENTIDAD; lengind[(old+1):(old+new),2] <- tmp$SECCION; lengind[(old+1):(old+new),3] <- tmp$HABLEIN1; 
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/mor/17_mor_lengind.dbf")
old <- nrow(lengind); new <- nrow(tmp);
lengind[(old+1):(old+new),1] <- tmp$ENTIDAD; lengind[(old+1):(old+new),2] <- tmp$SECCION; lengind[(old+1):(old+new),3] <- tmp$HABLEIN1; 
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/nay/18_nay_lengind.dbf")
old <- nrow(lengind); new <- nrow(tmp);
lengind[(old+1):(old+new),1] <- tmp$ENTIDAD; lengind[(old+1):(old+new),2] <- tmp$SECCION; lengind[(old+1):(old+new),3] <- tmp$HABLEIN1; 
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/nl/19_nl_lengind.dbf")
old <- nrow(lengind); new <- nrow(tmp);
lengind[(old+1):(old+new),1] <- tmp$ENTIDAD; lengind[(old+1):(old+new),2] <- tmp$SECCION; lengind[(old+1):(old+new),3] <- tmp$HABLEIN1; 
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/oax/20_oax_lengind.dbf")
old <- nrow(lengind); new <- nrow(tmp);
lengind[(old+1):(old+new),1] <- tmp$ENTIDAD; lengind[(old+1):(old+new),2] <- tmp$SECCION; lengind[(old+1):(old+new),3] <- tmp$HABLEIN1; 
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/pue/21_pue_lengind.dbf")
old <- nrow(lengind); new <- nrow(tmp);
lengind[(old+1):(old+new),1] <- tmp$ENTIDAD; lengind[(old+1):(old+new),2] <- tmp$SECCION; lengind[(old+1):(old+new),3] <- tmp$HABLEIN1; 
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/que/22_qro_lengind.dbf")
old <- nrow(lengind); new <- nrow(tmp);
lengind[(old+1):(old+new),1] <- tmp$ENTIDAD; lengind[(old+1):(old+new),2] <- tmp$SECCION; lengind[(old+1):(old+new),3] <- tmp$HABLEIN1; 
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/qui/23_qroo_lengind.dbf")
old <- nrow(lengind); new <- nrow(tmp);
lengind[(old+1):(old+new),1] <- tmp$ENTIDAD; lengind[(old+1):(old+new),2] <- tmp$SECCION; lengind[(old+1):(old+new),3] <- tmp$HABLEIN1; 
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/san/24_slp_lengind.dbf")
old <- nrow(lengind); new <- nrow(tmp);
lengind[(old+1):(old+new),1] <- tmp$ENTIDAD; lengind[(old+1):(old+new),2] <- tmp$SECCION; lengind[(old+1):(old+new),3] <- tmp$HABLEIN1; 
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/sin/25_sin_lengind.dbf")
old <- nrow(lengind); new <- nrow(tmp);
lengind[(old+1):(old+new),1] <- tmp$ENTIDAD; lengind[(old+1):(old+new),2] <- tmp$SECCION; lengind[(old+1):(old+new),3] <- tmp$HABLEIN1; 
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/son/26_son_lengind.dbf")
old <- nrow(lengind); new <- nrow(tmp);
lengind[(old+1):(old+new),1] <- tmp$ENTIDAD; lengind[(old+1):(old+new),2] <- tmp$SECCION; lengind[(old+1):(old+new),3] <- tmp$HABLEIN1; 
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/tab/27_tab_lengind.dbf")
old <- nrow(lengind); new <- nrow(tmp);
lengind[(old+1):(old+new),1] <- tmp$ENTIDAD; lengind[(old+1):(old+new),2] <- tmp$SECCION; lengind[(old+1):(old+new),3] <- tmp$HABLEIN1; 
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/tam/28_tamp_lengind.dbf")
old <- nrow(lengind); new <- nrow(tmp);
lengind[(old+1):(old+new),1] <- tmp$ENTIDAD; lengind[(old+1):(old+new),2] <- tmp$SECCION; lengind[(old+1):(old+new),3] <- tmp$HABLEIN1; 
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/tla/29_tlax_lengind.dbf")
old <- nrow(lengind); new <- nrow(tmp);
lengind[(old+1):(old+new),1] <- tmp$ENTIDAD; lengind[(old+1):(old+new),2] <- tmp$SECCION; lengind[(old+1):(old+new),3] <- tmp$HABLEIN1; 
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/ver/30_ver_lengind.dbf")
old <- nrow(lengind); new <- nrow(tmp);
lengind[(old+1):(old+new),1] <- tmp$ENTIDAD; lengind[(old+1):(old+new),2] <- tmp$SECCION; lengind[(old+1):(old+new),3] <- tmp$HABLEIN1; 
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/yuc/31_yuc_lengind.dbf")
old <- nrow(lengind); new <- nrow(tmp);
lengind[(old+1):(old+new),1] <- tmp$ENTIDAD; lengind[(old+1):(old+new),2] <- tmp$SECCION; lengind[(old+1):(old+new),3] <- tmp$HABLEIN1; 
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/zac/32_zac_lengind.dbf")
old <- nrow(lengind); new <- nrow(tmp);
lengind[(old+1):(old+new),1] <- tmp$ENTIDAD; lengind[(old+1):(old+new),2] <- tmp$SECCION; lengind[(old+1):(old+new),3] <- tmp$HABLEIN1; 
rm(tmp,old,new)

library(foreign)
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/ags/01_ags_viv.dbf")
viv <- data.frame(edon=tmp$ENTIDAD, disn=tmp$DISTRITO, munn=tmp$MUNICIPIO, seccion=tmp$SECCION, vivTot=tmp$VIV_TOT, tierra=tmp$MAT_PISO1,
                  tv=tmp$DIS_TELE1, compu=tmp$DIS_COMP1, sinluz=tmp$DIS_ELEC2, aguaRio=tmp$DIS_AGUA7, sinWc=tmp$DIS_SANI2+tmp$CON_AGUA5,
                  avgRoomOccup=tmp$PTOPERVIV/tmp$PCUARDOM)
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/bc/02_bc_viv.dbf")
old <- nrow(viv); new <- nrow(tmp);
viv[(old+1):(old+new),1] <- tmp$ENTIDAD; viv[(old+1):(old+new),2] <- tmp$DISTRITO; viv[(old+1):(old+new),3] <- tmp$MUNICIPIO; viv[(old+1):(old+new),4] <- tmp$SECCION; viv[(old+1):(old+new),5] <- tmp$VIV_TOT; viv[(old+1):(old+new),6] <- tmp$MAT_PISO1; viv[(old+1):(old+new),7] <- tmp$DIS_TELE1; viv[(old+1):(old+new),8] <- tmp$DIS_COMP1; viv[(old+1):(old+new),9] <- tmp$DIS_ELEC2; viv[(old+1):(old+new),10] <- tmp$DIS_AGUA7; viv[(old+1):(old+new),11] <- tmp$DIS_SANI2+tmp$CON_AGUA5; viv[(old+1):(old+new),12] <- tmp$PTOPERVIV/tmp$PCUARDOM;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/bcs/03_bcs_viv.dbf")
old <- nrow(viv); new <- nrow(tmp);
viv[(old+1):(old+new),1] <- tmp$ENTIDAD; viv[(old+1):(old+new),2] <- tmp$DISTRITO; viv[(old+1):(old+new),3] <- tmp$MUNICIPIO; viv[(old+1):(old+new),4] <- tmp$SECCION; viv[(old+1):(old+new),5] <- tmp$VIV_TOT; viv[(old+1):(old+new),6] <- tmp$MAT_PISO1; viv[(old+1):(old+new),7] <- tmp$DIS_TELE1; viv[(old+1):(old+new),8] <- tmp$DIS_COMP1; viv[(old+1):(old+new),9] <- tmp$DIS_ELEC2; viv[(old+1):(old+new),10] <- tmp$DIS_AGUA7; viv[(old+1):(old+new),11] <- tmp$DIS_SANI2+tmp$CON_AGUA5; viv[(old+1):(old+new),12] <- tmp$PTOPERVIV/tmp$PCUARDOM;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/cam/04_camp_viv.dbf")
old <- nrow(viv); new <- nrow(tmp);
viv[(old+1):(old+new),1] <- tmp$ENTIDAD; viv[(old+1):(old+new),2] <- tmp$DISTRITO; viv[(old+1):(old+new),3] <- tmp$MUNICIPIO; viv[(old+1):(old+new),4] <- tmp$SECCION; viv[(old+1):(old+new),5] <- tmp$VIV_TOT; viv[(old+1):(old+new),6] <- tmp$MAT_PISO1; viv[(old+1):(old+new),7] <- tmp$DIS_TELE1; viv[(old+1):(old+new),8] <- tmp$DIS_COMP1; viv[(old+1):(old+new),9] <- tmp$DIS_ELEC2; viv[(old+1):(old+new),10] <- tmp$DIS_AGUA7; viv[(old+1):(old+new),11] <- tmp$DIS_SANI2+tmp$CON_AGUA5; viv[(old+1):(old+new),12] <- tmp$PTOPERVIV/tmp$PCUARDOM;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/coa/05_coah_viv.dbf")
old <- nrow(viv); new <- nrow(tmp);
viv[(old+1):(old+new),1] <- tmp$ENTIDAD; viv[(old+1):(old+new),2] <- tmp$DISTRITO; viv[(old+1):(old+new),3] <- tmp$MUNICIPIO; viv[(old+1):(old+new),4] <- tmp$SECCION; viv[(old+1):(old+new),5] <- tmp$VIV_TOT; viv[(old+1):(old+new),6] <- tmp$MAT_PISO1; viv[(old+1):(old+new),7] <- tmp$DIS_TELE1; viv[(old+1):(old+new),8] <- tmp$DIS_COMP1; viv[(old+1):(old+new),9] <- tmp$DIS_ELEC2; viv[(old+1):(old+new),10] <- tmp$DIS_AGUA7; viv[(old+1):(old+new),11] <- tmp$DIS_SANI2+tmp$CON_AGUA5; viv[(old+1):(old+new),12] <- tmp$PTOPERVIV/tmp$PCUARDOM;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/col/06_col_viv.dbf")
old <- nrow(viv); new <- nrow(tmp);
viv[(old+1):(old+new),1] <- tmp$ENTIDAD; viv[(old+1):(old+new),2] <- tmp$DISTRITO; viv[(old+1):(old+new),3] <- tmp$MUNICIPIO; viv[(old+1):(old+new),4] <- tmp$SECCION; viv[(old+1):(old+new),5] <- tmp$VIV_TOT; viv[(old+1):(old+new),6] <- tmp$MAT_PISO1; viv[(old+1):(old+new),7] <- tmp$DIS_TELE1; viv[(old+1):(old+new),8] <- tmp$DIS_COMP1; viv[(old+1):(old+new),9] <- tmp$DIS_ELEC2; viv[(old+1):(old+new),10] <- tmp$DIS_AGUA7; viv[(old+1):(old+new),11] <- tmp$DIS_SANI2+tmp$CON_AGUA5; viv[(old+1):(old+new),12] <- tmp$PTOPERVIV/tmp$PCUARDOM;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/cps/07_chiap_viv.dbf")
old <- nrow(viv); new <- nrow(tmp);
viv[(old+1):(old+new),1] <- tmp$ENTIDAD; viv[(old+1):(old+new),2] <- tmp$DISTRITO; viv[(old+1):(old+new),3] <- tmp$MUNICIPIO; viv[(old+1):(old+new),4] <- tmp$SECCION; viv[(old+1):(old+new),5] <- tmp$VIV_TOT; viv[(old+1):(old+new),6] <- tmp$MAT_PISO1; viv[(old+1):(old+new),7] <- tmp$DIS_TELE1; viv[(old+1):(old+new),8] <- tmp$DIS_COMP1; viv[(old+1):(old+new),9] <- tmp$DIS_ELEC2; viv[(old+1):(old+new),10] <- tmp$DIS_AGUA7; viv[(old+1):(old+new),11] <- tmp$DIS_SANI2+tmp$CON_AGUA5; viv[(old+1):(old+new),12] <- tmp$PTOPERVIV/tmp$PCUARDOM;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/cua/08_chih_viv.dbf")
old <- nrow(viv); new <- nrow(tmp);
viv[(old+1):(old+new),1] <- tmp$ENTIDAD; viv[(old+1):(old+new),2] <- tmp$DISTRITO; viv[(old+1):(old+new),3] <- tmp$MUNICIPIO; viv[(old+1):(old+new),4] <- tmp$SECCION; viv[(old+1):(old+new),5] <- tmp$VIV_TOT; viv[(old+1):(old+new),6] <- tmp$MAP_PISO1; viv[(old+1):(old+new),7] <- tmp$DIS_TELE1; viv[(old+1):(old+new),8] <- tmp$DIS_COMP1; viv[(old+1):(old+new),9] <- tmp$DIS_ELEC2; viv[(old+1):(old+new),10] <- tmp$DIS_AGUA7; viv[(old+1):(old+new),11] <- tmp$DIS_SANI2+tmp$CON_AGUA5; viv[(old+1):(old+new),12] <- tmp$PTOPERVIV/tmp$PCUARDOM;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/df/09_df_viv.dbf")
old <- nrow(viv); new <- nrow(tmp);
viv[(old+1):(old+new),1] <- tmp$ENTIDAD; viv[(old+1):(old+new),2] <- tmp$DISTRITO; viv[(old+1):(old+new),3] <- tmp$MUNICIPIO; viv[(old+1):(old+new),4] <- tmp$SECCION; viv[(old+1):(old+new),5] <- tmp$VIV_TOT; viv[(old+1):(old+new),6] <- tmp$MAP_PISO1; viv[(old+1):(old+new),7] <- tmp$DIS_TELE1; viv[(old+1):(old+new),8] <- tmp$DIS_COMP1; viv[(old+1):(old+new),9] <- tmp$DIS_ELEC2; viv[(old+1):(old+new),10] <- tmp$DIS_AGUA7; viv[(old+1):(old+new),11] <- tmp$DIS_SANI2+tmp$CON_AGUA5; viv[(old+1):(old+new),12] <- tmp$PTOPERVIV/tmp$PCUARDOM;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/dgo/10_dgo_viv.dbf")
old <- nrow(viv); new <- nrow(tmp);
viv[(old+1):(old+new),1] <- tmp$ENTIDAD; viv[(old+1):(old+new),2] <- tmp$DISTRITO; viv[(old+1):(old+new),3] <- tmp$MUNICIPIO; viv[(old+1):(old+new),4] <- tmp$SECCION; viv[(old+1):(old+new),5] <- tmp$VIV_TOT; viv[(old+1):(old+new),6] <- tmp$MAT_PISO1; viv[(old+1):(old+new),7] <- tmp$DIS_TELE1; viv[(old+1):(old+new),8] <- tmp$DIS_COMP1; viv[(old+1):(old+new),9] <- tmp$DIS_ELEC2; viv[(old+1):(old+new),10] <- tmp$DIS_AGUA7; viv[(old+1):(old+new),11] <- tmp$DIS_SANI2+tmp$CON_AGUA5; viv[(old+1):(old+new),12] <- tmp$PTOPERVIV/tmp$PCUARDOM;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/gua/11_gto_viv.dbf")
old <- nrow(viv); new <- nrow(tmp);
viv[(old+1):(old+new),1] <- tmp$ENTIDAD; viv[(old+1):(old+new),2] <- tmp$DISTRITO; viv[(old+1):(old+new),3] <- tmp$MUNICIPIO; viv[(old+1):(old+new),4] <- tmp$SECCION; viv[(old+1):(old+new),5] <- tmp$VIV_TOT; viv[(old+1):(old+new),6] <- tmp$MAT_PISO1; viv[(old+1):(old+new),7] <- tmp$DIS_TELE1; viv[(old+1):(old+new),8] <- tmp$DIS_COMP1; viv[(old+1):(old+new),9] <- tmp$DIS_ELEC2; viv[(old+1):(old+new),10] <- tmp$DIS_AGUA7; viv[(old+1):(old+new),11] <- tmp$DIS_SANI2+tmp$CON_AGUA5; viv[(old+1):(old+new),12] <- tmp$PTOPERVIV/tmp$PCUARDOM;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/gue/12_gro_viv.dbf")
old <- nrow(viv); new <- nrow(tmp);
viv[(old+1):(old+new),1] <- tmp$ENTIDAD; viv[(old+1):(old+new),2] <- tmp$DISTRITO; viv[(old+1):(old+new),3] <- tmp$MUNICIPIO; viv[(old+1):(old+new),4] <- tmp$SECCION; viv[(old+1):(old+new),5] <- tmp$VIV_TOT; viv[(old+1):(old+new),6] <- tmp$MAT_PISO1; viv[(old+1):(old+new),7] <- tmp$DIS_TELE1; viv[(old+1):(old+new),8] <- tmp$DIS_COMP1; viv[(old+1):(old+new),9] <- tmp$DIS_ELEC2; viv[(old+1):(old+new),10] <- tmp$DIS_AGUA7; viv[(old+1):(old+new),11] <- tmp$DIS_SANI2+tmp$CON_AGUA5; viv[(old+1):(old+new),12] <- tmp$PTOPERVIV/tmp$PCUARDOM;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/hgo/13_hgo_viv.dbf")
old <- nrow(viv); new <- nrow(tmp);
viv[(old+1):(old+new),1] <- tmp$ENTIDAD; viv[(old+1):(old+new),2] <- tmp$DISTRITO; viv[(old+1):(old+new),3] <- tmp$MUNICIPIO; viv[(old+1):(old+new),4] <- tmp$SECCION; viv[(old+1):(old+new),5] <- tmp$VIV_TOT; viv[(old+1):(old+new),6] <- tmp$MAT_PISO1; viv[(old+1):(old+new),7] <- tmp$DIS_TELE1; viv[(old+1):(old+new),8] <- tmp$DIS_COMP1; viv[(old+1):(old+new),9] <- tmp$DIS_ELEC2; viv[(old+1):(old+new),10] <- tmp$DIS_AGUA7; viv[(old+1):(old+new),11] <- tmp$DIS_SANI2+tmp$CON_AGUA5; viv[(old+1):(old+new),12] <- tmp$PTOPERVIV/tmp$PCUARDOM;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/jal/14_jal_viv.dbf")
old <- nrow(viv); new <- nrow(tmp);
viv[(old+1):(old+new),1] <- tmp$ENTIDAD; viv[(old+1):(old+new),2] <- tmp$DISTRITO; viv[(old+1):(old+new),3] <- tmp$MUNICIPIO; viv[(old+1):(old+new),4] <- tmp$SECCION; viv[(old+1):(old+new),5] <- tmp$VIV_TOT; viv[(old+1):(old+new),6] <- tmp$MAT_PISO1; viv[(old+1):(old+new),7] <- tmp$DIS_TELE1; viv[(old+1):(old+new),8] <- tmp$DIS_COMP1; viv[(old+1):(old+new),9] <- tmp$DIS_ELEC2; viv[(old+1):(old+new),10] <- tmp$DIS_AGUA7; viv[(old+1):(old+new),11] <- tmp$DIS_SANI2+tmp$CON_AGUA5; viv[(old+1):(old+new),12] <- tmp$PTOPERVIV/tmp$PCUARDOM;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/mex/15_mex_viv.dbf")
old <- nrow(viv); new <- nrow(tmp);
viv[(old+1):(old+new),1] <- tmp$ENTIDAD; viv[(old+1):(old+new),2] <- tmp$DISTRITO; viv[(old+1):(old+new),3] <- tmp$MUNICIPIO; viv[(old+1):(old+new),4] <- tmp$SECCION; viv[(old+1):(old+new),5] <- tmp$VIV_TOT; viv[(old+1):(old+new),6] <- tmp$MAP_PISO1; viv[(old+1):(old+new),7] <- tmp$DIS_TELE1; viv[(old+1):(old+new),8] <- tmp$DIS_COMP1; viv[(old+1):(old+new),9] <- tmp$DIS_ELEC2; viv[(old+1):(old+new),10] <- tmp$DIS_AGUA7; viv[(old+1):(old+new),11] <- tmp$DIS_SANI2+tmp$CON_AGUA5; viv[(old+1):(old+new),12] <- tmp$PTOPERVIV/tmp$PCUARDOM;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/mic/16_mich_viv.dbf")
old <- nrow(viv); new <- nrow(tmp);
viv[(old+1):(old+new),1] <- tmp$ENTIDAD; viv[(old+1):(old+new),2] <- tmp$DISTRITO; viv[(old+1):(old+new),3] <- tmp$MUNICIPIO; viv[(old+1):(old+new),4] <- tmp$SECCION; viv[(old+1):(old+new),5] <- tmp$VIV_TOT; viv[(old+1):(old+new),6] <- tmp$MAT_PISO1; viv[(old+1):(old+new),7] <- tmp$DIS_TELE1; viv[(old+1):(old+new),8] <- tmp$DIS_COMP1; viv[(old+1):(old+new),9] <- tmp$DIS_ELEC2; viv[(old+1):(old+new),10] <- tmp$DIS_AGUA7; viv[(old+1):(old+new),11] <- tmp$DIS_SANI2+tmp$CON_AGUA5; viv[(old+1):(old+new),12] <- tmp$PTOPERVIV/tmp$PCUARDOM;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/mor/17_mor_viv.dbf")
old <- nrow(viv); new <- nrow(tmp);
viv[(old+1):(old+new),1] <- tmp$ENTIDAD; viv[(old+1):(old+new),2] <- tmp$DISTRITO; viv[(old+1):(old+new),3] <- tmp$MUNICIPIO; viv[(old+1):(old+new),4] <- tmp$SECCION; viv[(old+1):(old+new),5] <- tmp$VIV_TOT; viv[(old+1):(old+new),6] <- tmp$MAT_PISO1; viv[(old+1):(old+new),7] <- tmp$DIS_TELE1; viv[(old+1):(old+new),8] <- tmp$DIS_COMP1; viv[(old+1):(old+new),9] <- tmp$DIS_ELEC2; viv[(old+1):(old+new),10] <- tmp$DIS_AGUA7; viv[(old+1):(old+new),11] <- tmp$DIS_SANI2+tmp$CON_AGUA5; viv[(old+1):(old+new),12] <- tmp$PTOPERVIV/tmp$PCUARDOM;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/nay/18_nay_viv.dbf")
old <- nrow(viv); new <- nrow(tmp);
viv[(old+1):(old+new),1] <- tmp$ENTIDAD; viv[(old+1):(old+new),2] <- tmp$DISTRITO; viv[(old+1):(old+new),3] <- tmp$MUNICIPIO; viv[(old+1):(old+new),4] <- tmp$SECCION; viv[(old+1):(old+new),5] <- tmp$VIV_TOT; viv[(old+1):(old+new),6] <- tmp$MAT_PISO1; viv[(old+1):(old+new),7] <- tmp$DIS_TELE1; viv[(old+1):(old+new),8] <- tmp$DIS_COMP1; viv[(old+1):(old+new),9] <- tmp$DIS_ELEC2; viv[(old+1):(old+new),10] <- tmp$DIS_AGUA7; viv[(old+1):(old+new),11] <- tmp$DIS_SANI2+tmp$CON_AGUA5; viv[(old+1):(old+new),12] <- tmp$PTOPERVIV/tmp$PCUARDOM;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/nl/19_nl_viv.dbf")
old <- nrow(viv); new <- nrow(tmp);
viv[(old+1):(old+new),1] <- tmp$ENTIDAD; viv[(old+1):(old+new),2] <- tmp$DISTRITO; viv[(old+1):(old+new),3] <- tmp$MUNICIPIO; viv[(old+1):(old+new),4] <- tmp$SECCION; viv[(old+1):(old+new),5] <- tmp$VIV_TOT; viv[(old+1):(old+new),6] <- tmp$MAT_PISO1; viv[(old+1):(old+new),7] <- tmp$DIS_TELE1; viv[(old+1):(old+new),8] <- tmp$DIS_COMP1; viv[(old+1):(old+new),9] <- tmp$DIS_ELEC2; viv[(old+1):(old+new),10] <- tmp$DIS_AGUA7; viv[(old+1):(old+new),11] <- tmp$DIS_SANI2+tmp$CON_AGUA5; viv[(old+1):(old+new),12] <- tmp$PTOPERVIV/tmp$PCUARDOM;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/oax/20_oax_viv.dbf")
old <- nrow(viv); new <- nrow(tmp);
viv[(old+1):(old+new),1] <- tmp$ENTIDAD; viv[(old+1):(old+new),2] <- tmp$DISTRITO; viv[(old+1):(old+new),3] <- tmp$MUNICIPIO; viv[(old+1):(old+new),4] <- tmp$SECCION; viv[(old+1):(old+new),5] <- tmp$VIV_TOT; viv[(old+1):(old+new),6] <- tmp$MAT_PISO1; viv[(old+1):(old+new),7] <- tmp$DIS_TELE1; viv[(old+1):(old+new),8] <- tmp$DIS_COMP1; viv[(old+1):(old+new),9] <- tmp$DIS_ELEC2; viv[(old+1):(old+new),10] <- tmp$DIS_AGUA7; viv[(old+1):(old+new),11] <- tmp$DIS_SANI2+tmp$CON_AGUA5; viv[(old+1):(old+new),12] <- tmp$PTOPERVIV/tmp$PCUARDOM;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/pue/21_pue_viv.dbf")
old <- nrow(viv); new <- nrow(tmp);
viv[(old+1):(old+new),1] <- tmp$ENTIDAD; viv[(old+1):(old+new),2] <- tmp$DISTRITO; viv[(old+1):(old+new),3] <- tmp$MUNICIPIO; viv[(old+1):(old+new),4] <- tmp$SECCION; viv[(old+1):(old+new),5] <- tmp$VIV_TOT; viv[(old+1):(old+new),6] <- tmp$MAP_PISO1; viv[(old+1):(old+new),7] <- tmp$DIS_TELE1; viv[(old+1):(old+new),8] <- tmp$DIS_COMP1; viv[(old+1):(old+new),9] <- tmp$DIS_ELEC2; viv[(old+1):(old+new),10] <- tmp$DIS_AGUA7; viv[(old+1):(old+new),11] <- tmp$DIS_SANI2+tmp$CON_AGUA5; viv[(old+1):(old+new),12] <- tmp$PTOPERVIV/tmp$PCUARDOM;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/que/22_qro_viv.dbf")
old <- nrow(viv); new <- nrow(tmp);
viv[(old+1):(old+new),1] <- tmp$ENTIDAD; viv[(old+1):(old+new),2] <- tmp$DISTRITO; viv[(old+1):(old+new),3] <- tmp$MUNICIPIO; viv[(old+1):(old+new),4] <- tmp$SECCION; viv[(old+1):(old+new),5] <- tmp$VIV_TOT; viv[(old+1):(old+new),6] <- tmp$MAT_PISO1; viv[(old+1):(old+new),7] <- tmp$DIS_TELE1; viv[(old+1):(old+new),8] <- tmp$DIS_COMP1; viv[(old+1):(old+new),9] <- tmp$DIS_ELEC2; viv[(old+1):(old+new),10] <- tmp$DIS_AGUA7; viv[(old+1):(old+new),11] <- tmp$DIS_SANI2+tmp$CON_AGUA5; viv[(old+1):(old+new),12] <- tmp$PTOPERVIV/tmp$PCUARDOM;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/qui/23_qroo_viv.dbf")
old <- nrow(viv); new <- nrow(tmp);
viv[(old+1):(old+new),1] <- tmp$ENTIDAD; viv[(old+1):(old+new),2] <- tmp$DISTRITO; viv[(old+1):(old+new),3] <- tmp$MUNICIPIO; viv[(old+1):(old+new),4] <- tmp$SECCION; viv[(old+1):(old+new),5] <- tmp$VIV_TOT; viv[(old+1):(old+new),6] <- tmp$MAT_PISO1; viv[(old+1):(old+new),7] <- tmp$DIS_TELE1; viv[(old+1):(old+new),8] <- tmp$DIS_COMP1; viv[(old+1):(old+new),9] <- tmp$DIS_ELEC2; viv[(old+1):(old+new),10] <- tmp$DIS_AGUA7; viv[(old+1):(old+new),11] <- tmp$DIS_SANI2+tmp$CON_AGUA5; viv[(old+1):(old+new),12] <- tmp$PTOPERVIV/tmp$PCUARDOM;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/san/_4_slp_viv.dbf")
old <- nrow(viv); new <- nrow(tmp);
viv[(old+1):(old+new),1] <- tmp$ENTIDAD; viv[(old+1):(old+new),2] <- tmp$DISTRITO; viv[(old+1):(old+new),3] <- tmp$MUNICIPIO; viv[(old+1):(old+new),4] <- tmp$SECCION; viv[(old+1):(old+new),5] <- tmp$VIV_TOT; viv[(old+1):(old+new),6] <- tmp$MAT_PISO1; viv[(old+1):(old+new),7] <- tmp$DIS_TELE1; viv[(old+1):(old+new),8] <- tmp$DIS_COMP1; viv[(old+1):(old+new),9] <- tmp$DIS_ELEC2; viv[(old+1):(old+new),10] <- tmp$DIS_AGUA7; viv[(old+1):(old+new),11] <- tmp$DIS_SANI2+tmp$CON_AGUA5; viv[(old+1):(old+new),12] <- tmp$PTOPERVIV/tmp$PCUARDOM;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/sin/25_sin_viv.dbf")
old <- nrow(viv); new <- nrow(tmp);
viv[(old+1):(old+new),1] <- tmp$ENTIDAD; viv[(old+1):(old+new),2] <- tmp$DISTRITO; viv[(old+1):(old+new),3] <- tmp$MUNICIPIO; viv[(old+1):(old+new),4] <- tmp$SECCION; viv[(old+1):(old+new),5] <- tmp$VIV_TOT; viv[(old+1):(old+new),6] <- tmp$MAT_PISO1; viv[(old+1):(old+new),7] <- tmp$DIS_TELE1; viv[(old+1):(old+new),8] <- tmp$DIS_COMP1; viv[(old+1):(old+new),9] <- tmp$DIS_ELEC2; viv[(old+1):(old+new),10] <- tmp$DIS_AGUA7; viv[(old+1):(old+new),11] <- tmp$DIS_SANI2+tmp$CON_AGUA5; viv[(old+1):(old+new),12] <- tmp$PTOPERVIV/tmp$PCUARDOM;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/son/26_son_viv.dbf")
old <- nrow(viv); new <- nrow(tmp);
viv[(old+1):(old+new),1] <- tmp$ENTIDAD; viv[(old+1):(old+new),2] <- tmp$DISTRITO; viv[(old+1):(old+new),3] <- tmp$MUNICIPIO; viv[(old+1):(old+new),4] <- tmp$SECCION; viv[(old+1):(old+new),5] <- tmp$VIV_TOT; viv[(old+1):(old+new),6] <- tmp$MAT_PISO1; viv[(old+1):(old+new),7] <- tmp$DIS_TELE1; viv[(old+1):(old+new),8] <- tmp$DIS_COMP1; viv[(old+1):(old+new),9] <- tmp$DIS_ELEC2; viv[(old+1):(old+new),10] <- tmp$DIS_AGUA7; viv[(old+1):(old+new),11] <- tmp$DIS_SANI2+tmp$CON_AGUA5; viv[(old+1):(old+new),12] <- tmp$PTOPERVIV/tmp$PCUARDOM;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/tab/27_tab_viv.dbf")
old <- nrow(viv); new <- nrow(tmp);
viv[(old+1):(old+new),1] <- tmp$ENTIDAD; viv[(old+1):(old+new),2] <- tmp$DISTRITO; viv[(old+1):(old+new),3] <- tmp$MUNICIPIO; viv[(old+1):(old+new),4] <- tmp$SECCION; viv[(old+1):(old+new),5] <- tmp$VIV_TOT; viv[(old+1):(old+new),6] <- tmp$MAT_PISO1; viv[(old+1):(old+new),7] <- tmp$DIS_TELE1; viv[(old+1):(old+new),8] <- tmp$DIS_COMP1; viv[(old+1):(old+new),9] <- tmp$DIS_ELEC2; viv[(old+1):(old+new),10] <- tmp$DIS_AGUA7; viv[(old+1):(old+new),11] <- tmp$DIS_SANI2+tmp$CON_AGUA5; viv[(old+1):(old+new),12] <- tmp$PTOPERVIV/tmp$PCUARDOM;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/tam/28_tamp_viv.dbf")
old <- nrow(viv); new <- nrow(tmp);
viv[(old+1):(old+new),1] <- tmp$ENTIDAD; viv[(old+1):(old+new),2] <- tmp$DISTRITO; viv[(old+1):(old+new),3] <- tmp$MUNICIPIO; viv[(old+1):(old+new),4] <- tmp$SECCION; viv[(old+1):(old+new),5] <- tmp$VIV_TOT; viv[(old+1):(old+new),6] <- tmp$MAT_PISO1; viv[(old+1):(old+new),7] <- tmp$DIS_TELE1; viv[(old+1):(old+new),8] <- tmp$DIS_COMP1; viv[(old+1):(old+new),9] <- tmp$DIS_ELEC2; viv[(old+1):(old+new),10] <- tmp$DIS_AGUA7; viv[(old+1):(old+new),11] <- tmp$DIS_SANI2+tmp$CON_AGUA5; viv[(old+1):(old+new),12] <- tmp$PTOPERVIV/tmp$PCUARDOM;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/tla/_9_tlax_viv.dbf")
old <- nrow(viv); new <- nrow(tmp);
viv[(old+1):(old+new),1] <- tmp$ENTIDAD; viv[(old+1):(old+new),2] <- tmp$DISTRITO; viv[(old+1):(old+new),3] <- tmp$MUNICIPIO; viv[(old+1):(old+new),4] <- tmp$SECCION; viv[(old+1):(old+new),5] <- tmp$VIV_TOT; viv[(old+1):(old+new),6] <- tmp$MAT_PISO1; viv[(old+1):(old+new),7] <- tmp$DIS_TELE1; viv[(old+1):(old+new),8] <- tmp$DIS_COMP1; viv[(old+1):(old+new),9] <- tmp$DIS_ELEC2; viv[(old+1):(old+new),10] <- tmp$DIS_AGUA7; viv[(old+1):(old+new),11] <- tmp$DIS_SANI2+tmp$CON_AGUA5; viv[(old+1):(old+new),12] <- tmp$PTOPERVIV/tmp$PCUARDOM;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/ver/30_ver_viv.dbf")
old <- nrow(viv); new <- nrow(tmp);
viv[(old+1):(old+new),1] <- tmp$ENTIDAD; viv[(old+1):(old+new),2] <- tmp$DISTRITO; viv[(old+1):(old+new),3] <- tmp$MUNICIPIO; viv[(old+1):(old+new),4] <- tmp$SECCION; viv[(old+1):(old+new),5] <- tmp$VIV_TOT; viv[(old+1):(old+new),6] <- tmp$MAT_PISO1; viv[(old+1):(old+new),7] <- tmp$DIS_TELE1; viv[(old+1):(old+new),8] <- tmp$DIS_COMP1; viv[(old+1):(old+new),9] <- tmp$DIS_ELEC2; viv[(old+1):(old+new),10] <- tmp$DIS_AGUA7; viv[(old+1):(old+new),11] <- tmp$DIS_SANI2+tmp$CON_AGUA5; viv[(old+1):(old+new),12] <- tmp$PTOPERVIV/tmp$PCUARDOM;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/yuc/31_yuc_viv.dbf")
old <- nrow(viv); new <- nrow(tmp);
viv[(old+1):(old+new),1] <- tmp$ENTIDAD; viv[(old+1):(old+new),2] <- tmp$DISTRITO; viv[(old+1):(old+new),3] <- tmp$MUNICIPIO; viv[(old+1):(old+new),4] <- tmp$SECCION; viv[(old+1):(old+new),5] <- tmp$VIV_TOT; viv[(old+1):(old+new),6] <- tmp$MAT_PISO1; viv[(old+1):(old+new),7] <- tmp$DIS_TELE1; viv[(old+1):(old+new),8] <- tmp$DIS_COMP1; viv[(old+1):(old+new),9] <- tmp$DIS_ELEC2; viv[(old+1):(old+new),10] <- tmp$DIS_AGUA7; viv[(old+1):(old+new),11] <- tmp$DIS_SANI2+tmp$CON_AGUA5; viv[(old+1):(old+new),12] <- tmp$PTOPERVIV/tmp$PCUARDOM;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/zac/32_zac_viv.dbf")
old <- nrow(viv); new <- nrow(tmp);
viv[(old+1):(old+new),1] <- tmp$ENTIDAD; viv[(old+1):(old+new),2] <- tmp$DISTRITO; viv[(old+1):(old+new),3] <- tmp$MUNICIPIO; viv[(old+1):(old+new),4] <- tmp$SECCION; viv[(old+1):(old+new),5] <- tmp$VIV_TOT; viv[(old+1):(old+new),6] <- tmp$MAT_PISO1; viv[(old+1):(old+new),7] <- tmp$DIS_TELE1; viv[(old+1):(old+new),8] <- tmp$DIS_COMP1; viv[(old+1):(old+new),9] <- tmp$DIS_ELEC2; viv[(old+1):(old+new),10] <- tmp$DIS_AGUA7; viv[(old+1):(old+new),11] <- tmp$DIS_SANI2+tmp$CON_AGUA5; viv[(old+1):(old+new),12] <- tmp$PTOPERVIV/tmp$PCUARDOM;
rm(tmp,old,new)

library(foreign)
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/ags/01_ags_pob.dbf")
pob <- data.frame(edon=tmp$ENTIDAD, seccion=tmp$SECCION, pobTot=tmp$POB_TOT, pob15plus=tmp$POB_TOT-tmp$EDQUI01-tmp$EDQUI02-tmp$EDQUI03, shWomen=tmp$SEXO2/tmp$POB_TOT, young15.29=tmp$EDQUI04+tmp$EDQUI05+tmp$EDQUI06, old65plus=tmp$EDQUI14+tmp$EDQUI15+tmp$EDQUI16+tmp$EDQUI17+tmp$EDQUI18+tmp$EDQUI19+tmp$EDQUI20+tmp$EDQUI21, imss=tmp$IMSS1, issste=tmp$ISSSTE1, segPop=tmp$SEGU_POP1, analfab=tmp$ALFABET2, rural=tmp$TAM_LOC01+tmp$TAM_LOC02+tmp$TAM_LOC03+tmp$TAM_LOC04+tmp$TAM_LOC05+tmp$TAM_LOC06, sinPrim=tmp$NIV_ESCO00+tmp$NIV_ESCO01)
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/bc/02_bc_pob.dbf")
old <- nrow(pob); new <- nrow(tmp);
pob[(old+1):(old+new),1] <- tmp$ENTIDAD; pob[(old+1):(old+new),2] <- tmp$SECCION; pob[(old+1):(old+new),3] <- tmp$POB_TOT; pob[(old+1):(old+new),4] <- tmp$POB_TOT-tmp$EDQUI01-tmp$EDQUI02-tmp$EDQUI03; pob[(old+1):(old+new),5] <- tmp$SEXO2/tmp$POB_TOT; pob[(old+1):(old+new),6] <- tmp$EDQUI04+tmp$EDQUI05+tmp$EDQUI06; pob[(old+1):(old+new),7] <- tmp$EDQUI14+tmp$EDQUI15+tmp$EDQUI16+tmp$EDQUI17+tmp$EDQUI18+tmp$EDQUI19+tmp$EDQUI20+tmp$EDQUI21; pob[(old+1):(old+new),8] <- tmp$IMSS1; pob[(old+1):(old+new),9] <- tmp$ISSSTE1; pob[(old+1):(old+new),10] <- tmp$SEGU_POP1; pob[(old+1):(old+new),11] <- tmp$ALFABET2; pob[(old+1):(old+new),12] <- tmp$TAM_LOC01+tmp$TAM_LOC02+tmp$TAM_LOC03+tmp$TAM_LOC04+tmp$TAM_LOC05+tmp$TAM_LOC06; pob[(old+1):(old+new),13] <- tmp$NIV_ESCO00+tmp$NIV_ESCO01;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/bcs/03_bcs_pob.dbf")
old <- nrow(pob); new <- nrow(tmp);
pob[(old+1):(old+new),1] <- tmp$ENTIDAD; pob[(old+1):(old+new),2] <- tmp$SECCION; pob[(old+1):(old+new),3] <- tmp$POB_TOT; pob[(old+1):(old+new),4] <- tmp$POB_TOT-tmp$EDQUI01-tmp$EDQUI02-tmp$EDQUI03; pob[(old+1):(old+new),5] <- tmp$SEXO2/tmp$POB_TOT; pob[(old+1):(old+new),6] <- tmp$EDQUI04+tmp$EDQUI05+tmp$EDQUI06; pob[(old+1):(old+new),7] <- tmp$EDQUI14+tmp$EDQUI15+tmp$EDQUI16+tmp$EDQUI17+tmp$EDQUI18+tmp$EDQUI19+tmp$EDQUI20+tmp$EDQUI21; pob[(old+1):(old+new),8] <- tmp$IMSS1; pob[(old+1):(old+new),9] <- tmp$ISSSTE1; pob[(old+1):(old+new),10] <- tmp$SEGU_POP1; pob[(old+1):(old+new),11] <- tmp$ALFABET2; pob[(old+1):(old+new),12] <- tmp$TAM_LOC01+tmp$TAM_LOC02+tmp$TAM_LOC03+tmp$TAM_LOC04+tmp$TAM_LOC05+tmp$TAM_LOC06; pob[(old+1):(old+new),13] <- tmp$NIV_ESCO00+tmp$NIV_ESCO01;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/cam/04_camp_pob.dbf")
old <- nrow(pob); new <- nrow(tmp);
pob[(old+1):(old+new),1] <- tmp$ENTIDAD; pob[(old+1):(old+new),2] <- tmp$SECCION; pob[(old+1):(old+new),3] <- tmp$POB_TOT; pob[(old+1):(old+new),4] <- tmp$POB_TOT-tmp$EDQUI01-tmp$EDQUI02-tmp$EDQUI03; pob[(old+1):(old+new),5] <- tmp$SEXO2/tmp$POB_TOT; pob[(old+1):(old+new),6] <- tmp$EDQUI04+tmp$EDQUI05+tmp$EDQUI06; pob[(old+1):(old+new),7] <- tmp$EDQUI14+tmp$EDQUI15+tmp$EDQUI16+tmp$EDQUI17+tmp$EDQUI18+tmp$EDQUI19+tmp$EDQUI20+tmp$EDQUI21; pob[(old+1):(old+new),8] <- tmp$IMSS1; pob[(old+1):(old+new),9] <- tmp$ISSSTE1; pob[(old+1):(old+new),10] <- tmp$SEGU_POP1; pob[(old+1):(old+new),11] <- tmp$ALFABET2; pob[(old+1):(old+new),12] <- tmp$TAM_LOC01+tmp$TAM_LOC02+tmp$TAM_LOC03+tmp$TAM_LOC04+tmp$TAM_LOC05+tmp$TAM_LOC06; pob[(old+1):(old+new),13] <- tmp$NIV_ESCO00+tmp$NIV_ESCO01;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/coa/05_coah_pob.dbf")
old <- nrow(pob); new <- nrow(tmp);
pob[(old+1):(old+new),1] <- tmp$ENTIDAD; pob[(old+1):(old+new),2] <- tmp$SECCION; pob[(old+1):(old+new),3] <- tmp$POB_TOT; pob[(old+1):(old+new),4] <- tmp$POB_TOT-tmp$EDQUI01-tmp$EDQUI02-tmp$EDQUI03; pob[(old+1):(old+new),5] <- tmp$SEXO2/tmp$POB_TOT; pob[(old+1):(old+new),6] <- tmp$EDQUI04+tmp$EDQUI05+tmp$EDQUI06; pob[(old+1):(old+new),7] <- tmp$EDQUI14+tmp$EDQUI15+tmp$EDQUI16+tmp$EDQUI17+tmp$EDQUI18+tmp$EDQUI19+tmp$EDQUI20+tmp$EDQUI21; pob[(old+1):(old+new),8] <- tmp$IMSS1; pob[(old+1):(old+new),9] <- tmp$ISSSTE1; pob[(old+1):(old+new),10] <- tmp$SEGU_POP1; pob[(old+1):(old+new),11] <- tmp$ALFABET2; pob[(old+1):(old+new),12] <- tmp$TAM_LOC01+tmp$TAM_LOC02+tmp$TAM_LOC03+tmp$TAM_LOC04+tmp$TAM_LOC05+tmp$TAM_LOC06; pob[(old+1):(old+new),13] <- tmp$NIV_ESCO00+tmp$NIV_ESCO01;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/col/06_col_pob.dbf")
old <- nrow(pob); new <- nrow(tmp);
pob[(old+1):(old+new),1] <- tmp$ENTIDAD; pob[(old+1):(old+new),2] <- tmp$SECCION; pob[(old+1):(old+new),3] <- tmp$POB_TOT; pob[(old+1):(old+new),4] <- tmp$POB_TOT-tmp$EDQUI01-tmp$EDQUI02-tmp$EDQUI03; pob[(old+1):(old+new),5] <- tmp$SEXO2/tmp$POB_TOT; pob[(old+1):(old+new),6] <- tmp$EDQUI04+tmp$EDQUI05+tmp$EDQUI06; pob[(old+1):(old+new),7] <- tmp$EDQUI14+tmp$EDQUI15+tmp$EDQUI16+tmp$EDQUI17+tmp$EDQUI18+tmp$EDQUI19+tmp$EDQUI20+tmp$EDQUI21; pob[(old+1):(old+new),8] <- tmp$IMSS1; pob[(old+1):(old+new),9] <- tmp$ISSSTE1; pob[(old+1):(old+new),10] <- tmp$SEGU_POP1; pob[(old+1):(old+new),11] <- tmp$ALFABET2; pob[(old+1):(old+new),12] <- tmp$TAM_LOC01+tmp$TAM_LOC02+tmp$TAM_LOC03+tmp$TAM_LOC04+tmp$TAM_LOC05+tmp$TAM_LOC06; pob[(old+1):(old+new),13] <- tmp$NIV_ESCO00+tmp$NIV_ESCO01;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/cps/07_chiap_pob.dbf")
old <- nrow(pob); new <- nrow(tmp);
pob[(old+1):(old+new),1] <- tmp$ENTIDAD; pob[(old+1):(old+new),2] <- tmp$SECCION; pob[(old+1):(old+new),3] <- tmp$POB_TOT; pob[(old+1):(old+new),4] <- tmp$POB_TOT-tmp$EDQUI01-tmp$EDQUI02-tmp$EDQUI03; pob[(old+1):(old+new),5] <- tmp$SEXO2/tmp$POB_TOT; pob[(old+1):(old+new),6] <- tmp$EDQUI04+tmp$EDQUI05+tmp$EDQUI06; pob[(old+1):(old+new),7] <- tmp$EDQUI14+tmp$EDQUI15+tmp$EDQUI16+tmp$EDQUI17+tmp$EDQUI18+tmp$EDQUI19+tmp$EDQUI20+tmp$EDQUI21; pob[(old+1):(old+new),8] <- tmp$IMSS1; pob[(old+1):(old+new),9] <- tmp$ISSSTE1; pob[(old+1):(old+new),10] <- tmp$SEGU_POP1; pob[(old+1):(old+new),11] <- tmp$ALFABET2; pob[(old+1):(old+new),12] <- tmp$TAM_LOC01+tmp$TAM_LOC02+tmp$TAM_LOC03+tmp$TAM_LOC04+tmp$TAM_LOC05+tmp$TAM_LOC06; pob[(old+1):(old+new),13] <- tmp$NIV_ESCO00+tmp$NIV_ESCO01;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/cua/08_chih_pob.dbf")
old <- nrow(pob); new <- nrow(tmp);
pob[(old+1):(old+new),1] <- tmp$ENTIDAD; pob[(old+1):(old+new),2] <- tmp$SECCION; pob[(old+1):(old+new),3] <- tmp$POB_TOT; pob[(old+1):(old+new),4] <- tmp$POB_TOT-tmp$EDQUI01-tmp$EDQUI02-tmp$EDQUI03; pob[(old+1):(old+new),5] <- tmp$SEXO2/tmp$POB_TOT; pob[(old+1):(old+new),6] <- tmp$EDQUI04+tmp$EDQUI05+tmp$EDQUI06; pob[(old+1):(old+new),7] <- tmp$EDQUI14+tmp$EDQUI15+tmp$EDQUI16+tmp$EDQUI17+tmp$EDQUI18+tmp$EDQUI19+tmp$EDQUI20+tmp$EDQUI21; pob[(old+1):(old+new),8] <- tmp$IMSS1; pob[(old+1):(old+new),9] <- tmp$ISSSTE1; pob[(old+1):(old+new),10] <- tmp$SEGU_POP1; pob[(old+1):(old+new),11] <- tmp$ALFABET2; pob[(old+1):(old+new),12] <- tmp$TAM_LOC01+tmp$TAM_LOC02+tmp$TAM_LOC03+tmp$TAM_LOC04+tmp$TAM_LOC05+tmp$TAM_LOC06; pob[(old+1):(old+new),13] <- tmp$NIV_ESCO00+tmp$NIV_ESCO01;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/df/09_df_pob.dbf")
old <- nrow(pob); new <- nrow(tmp);
pob[(old+1):(old+new),1] <- tmp$ENTIDAD; pob[(old+1):(old+new),2] <- tmp$SECCION; pob[(old+1):(old+new),3] <- tmp$POB_TOT; pob[(old+1):(old+new),4] <- tmp$POB_TOT-tmp$EDQUI01-tmp$EDQUI02-tmp$EDQUI03; pob[(old+1):(old+new),5] <- tmp$SEXO2/tmp$POB_TOT; pob[(old+1):(old+new),6] <- tmp$EDQUI04+tmp$EDQUI05+tmp$EDQUI06; pob[(old+1):(old+new),7] <- tmp$EDQUI14+tmp$EDQUI15+tmp$EDQUI16+tmp$EDQUI17+tmp$EDQUI18+tmp$EDQUI19+tmp$EDQUI20+tmp$EDQUI21; pob[(old+1):(old+new),8] <- tmp$IMSS1; pob[(old+1):(old+new),9] <- tmp$ISSSTE1; pob[(old+1):(old+new),10] <- tmp$SEGU_POP1; pob[(old+1):(old+new),11] <- tmp$ALFABET2; pob[(old+1):(old+new),12] <- tmp$TAM_LOC01+tmp$TAM_LOC02+tmp$TAM_LOC03+tmp$TAM_LOC04+tmp$TAM_LOC05; pob[(old+1):(old+new),13] <- tmp$NIV_ESCO00+tmp$NIV_ESCO01;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/dgo/10_dgo_pob.dbf")
old <- nrow(pob); new <- nrow(tmp);
pob[(old+1):(old+new),1] <- tmp$ENTIDAD; pob[(old+1):(old+new),2] <- tmp$SECCION; pob[(old+1):(old+new),3] <- tmp$POB_TOT; pob[(old+1):(old+new),4] <- tmp$POB_TOT-tmp$EDQUI01-tmp$EDQUI02-tmp$EDQUI03; pob[(old+1):(old+new),5] <- tmp$SEXO2/tmp$POB_TOT; pob[(old+1):(old+new),6] <- tmp$EDQUI04+tmp$EDQUI05+tmp$EDQUI06; pob[(old+1):(old+new),7] <- tmp$EDQUI14+tmp$EDQUI15+tmp$EDQUI16+tmp$EDQUI17+tmp$EDQUI18+tmp$EDQUI19+tmp$EDQUI20+tmp$EDQUI21; pob[(old+1):(old+new),8] <- tmp$IMSS1; pob[(old+1):(old+new),9] <- tmp$ISSSTE1; pob[(old+1):(old+new),10] <- tmp$SEGU_POP1; pob[(old+1):(old+new),11] <- tmp$ALFABET2; pob[(old+1):(old+new),12] <- tmp$TAM_LOC01+tmp$TAM_LOC02+tmp$TAM_LOC03+tmp$TAM_LOC04+tmp$TAM_LOC05+tmp$TAM_LOC06; pob[(old+1):(old+new),13] <- tmp$NIV_ESCO00+tmp$NIV_ESCO01;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/gua/11_gto_pob.dbf")
old <- nrow(pob); new <- nrow(tmp);
pob[(old+1):(old+new),1] <- tmp$ENTIDAD; pob[(old+1):(old+new),2] <- tmp$SECCION; pob[(old+1):(old+new),3] <- tmp$POB_TOT; pob[(old+1):(old+new),4] <- tmp$POB_TOT-tmp$EDQUI01-tmp$EDQUI02-tmp$EDQUI03; pob[(old+1):(old+new),5] <- tmp$SEXO2/tmp$POB_TOT; pob[(old+1):(old+new),6] <- tmp$EDQUI04+tmp$EDQUI05+tmp$EDQUI06; pob[(old+1):(old+new),7] <- tmp$EDQUI14+tmp$EDQUI15+tmp$EDQUI16+tmp$EDQUI17+tmp$EDQUI18+tmp$EDQUI19+tmp$EDQUI20+tmp$EDQUI21; pob[(old+1):(old+new),8] <- tmp$IMSS1; pob[(old+1):(old+new),9] <- tmp$ISSSTE1; pob[(old+1):(old+new),10] <- tmp$SEGU_POP1; pob[(old+1):(old+new),11] <- tmp$ALFABET2; pob[(old+1):(old+new),12] <- tmp$TAM_LOC01+tmp$TAM_LOC02+tmp$TAM_LOC03+tmp$TAM_LOC04+tmp$TAM_LOC05+tmp$TAM_LOC06; pob[(old+1):(old+new),13] <- tmp$NIV_ESCO00+tmp$NIV_ESCO01;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/gue/12_gro_pob.dbf")
old <- nrow(pob); new <- nrow(tmp);
pob[(old+1):(old+new),1] <- tmp$ENTIDAD; pob[(old+1):(old+new),2] <- tmp$SECCION; pob[(old+1):(old+new),3] <- tmp$POB_TOT; pob[(old+1):(old+new),4] <- tmp$POB_TOT-tmp$EDQUI01-tmp$EDQUI02-tmp$EDQUI03; pob[(old+1):(old+new),5] <- tmp$SEXO2/tmp$POB_TOT; pob[(old+1):(old+new),6] <- tmp$EDQUI04+tmp$EDQUI05+tmp$EDQUI06; pob[(old+1):(old+new),7] <- tmp$EDQUI14+tmp$EDQUI15+tmp$EDQUI16+tmp$EDQUI17+tmp$EDQUI18+tmp$EDQUI19+tmp$EDQUI20+tmp$EDQUI21; pob[(old+1):(old+new),8] <- tmp$IMSS1; pob[(old+1):(old+new),9] <- tmp$ISSSTE1; pob[(old+1):(old+new),10] <- tmp$SEGU_POP1; pob[(old+1):(old+new),11] <- tmp$ALFABET2; pob[(old+1):(old+new),12] <- tmp$TAM_LOC01+tmp$TAM_LOC02+tmp$TAM_LOC03+tmp$TAM_LOC04+tmp$TAM_LOC05+tmp$TAM_LOC06; pob[(old+1):(old+new),13] <- tmp$NIV_ESCO00+tmp$NIV_ESCO01;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/hgo/13_hgo_pob.dbf")
old <- nrow(pob); new <- nrow(tmp);
pob[(old+1):(old+new),1] <- tmp$ENTIDAD; pob[(old+1):(old+new),2] <- tmp$SECCION; pob[(old+1):(old+new),3] <- tmp$POB_TOT; pob[(old+1):(old+new),4] <- tmp$POB_TOT-tmp$EDQUI01-tmp$EDQUI02-tmp$EDQUI03; pob[(old+1):(old+new),5] <- tmp$SEXO2/tmp$POB_TOT; pob[(old+1):(old+new),6] <- tmp$EDQUI04+tmp$EDQUI05+tmp$EDQUI06; pob[(old+1):(old+new),7] <- tmp$EDQUI14+tmp$EDQUI15+tmp$EDQUI16+tmp$EDQUI17+tmp$EDQUI18+tmp$EDQUI19+tmp$EDQUI20+tmp$EDQUI21; pob[(old+1):(old+new),8] <- tmp$IMSS1; pob[(old+1):(old+new),9] <- tmp$ISSSTE1; pob[(old+1):(old+new),10] <- tmp$SEGU_POP1; pob[(old+1):(old+new),11] <- tmp$ALFABET2; pob[(old+1):(old+new),12] <- tmp$TAM_LOC01+tmp$TAM_LOC02+tmp$TAM_LOC03+tmp$TAM_LOC04+tmp$TAM_LOC05+tmp$TAM_LOC06; pob[(old+1):(old+new),13] <- tmp$NIV_ESCO00+tmp$NIV_ESCO01;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/jal/14_jal_pob.dbf")
old <- nrow(pob); new <- nrow(tmp);
pob[(old+1):(old+new),1] <- tmp$ENTIDAD; pob[(old+1):(old+new),2] <- tmp$SECCION; pob[(old+1):(old+new),3] <- tmp$POB_TOT; pob[(old+1):(old+new),4] <- tmp$POB_TOT-tmp$EDQUI01-tmp$EDQUI02-tmp$EDQUI03; pob[(old+1):(old+new),5] <- tmp$SEXO2/tmp$POB_TOT; pob[(old+1):(old+new),6] <- tmp$EDQUI04+tmp$EDQUI05+tmp$EDQUI06; pob[(old+1):(old+new),7] <- tmp$EDQUI14+tmp$EDQUI15+tmp$EDQUI16+tmp$EDQUI17+tmp$EDQUI18+tmp$EDQUI19+tmp$EDQUI20+tmp$EDQUI21; pob[(old+1):(old+new),8] <- tmp$IMSS1; pob[(old+1):(old+new),9] <- tmp$ISSSTE1; pob[(old+1):(old+new),10] <- tmp$SEGU_POP1; pob[(old+1):(old+new),11] <- tmp$ALFABET2; pob[(old+1):(old+new),12] <- tmp$TAM_LOC01+tmp$TAM_LOC02+tmp$TAM_LOC03+tmp$TAM_LOC04+tmp$TAM_LOC05+tmp$TAM_LOC06; pob[(old+1):(old+new),13] <- tmp$NIV_ESCO00+tmp$NIV_ESCO01;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/mex/_5_mex_pob.dbf")
old <- nrow(pob); new <- nrow(tmp);
pob[(old+1):(old+new),1] <- tmp$ENTIDAD; pob[(old+1):(old+new),2] <- tmp$SECCION; pob[(old+1):(old+new),3] <- tmp$POB_TOT; pob[(old+1):(old+new),4] <- tmp$POB_TOT-tmp$EDQUI01-tmp$EDQUI02-tmp$EDQUI03; pob[(old+1):(old+new),5] <- tmp$SEXO2/tmp$POB_TOT; pob[(old+1):(old+new),6] <- tmp$EDQUI04+tmp$EDQUI05+tmp$EDQUI06; pob[(old+1):(old+new),7] <- tmp$EDQUI14+tmp$EDQUI15+tmp$EDQUI16+tmp$EDQUI17+tmp$EDQUI18+tmp$EDQUI19+tmp$EDQUI20+tmp$EDQUI21; pob[(old+1):(old+new),8] <- tmp$IMSS1; pob[(old+1):(old+new),9] <- tmp$ISSSTE1; pob[(old+1):(old+new),10] <- tmp$SEGU_POP1; pob[(old+1):(old+new),11] <- tmp$ALFABET2; pob[(old+1):(old+new),12] <- tmp$TAM_LOC01+tmp$TAM_LOC02+tmp$TAM_LOC03+tmp$TAM_LOC04+tmp$TAM_LOC05+tmp$TAM_LOC06; pob[(old+1):(old+new),13] <- tmp$NIV_ESCO00+tmp$NIV_ESCO01;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/mic/16_mich_pob.dbf")
old <- nrow(pob); new <- nrow(tmp);
pob[(old+1):(old+new),1] <- tmp$ENTIDAD; pob[(old+1):(old+new),2] <- tmp$SECCION; pob[(old+1):(old+new),3] <- tmp$POB_TOT; pob[(old+1):(old+new),4] <- tmp$POB_TOT-tmp$EDQUI01-tmp$EDQUI02-tmp$EDQUI03; pob[(old+1):(old+new),5] <- tmp$SEXO2/tmp$POB_TOT; pob[(old+1):(old+new),6] <- tmp$EDQUI04+tmp$EDQUI05+tmp$EDQUI06; pob[(old+1):(old+new),7] <- tmp$EDQUI14+tmp$EDQUI15+tmp$EDQUI16+tmp$EDQUI17+tmp$EDQUI18+tmp$EDQUI19+tmp$EDQUI20+tmp$EDQUI21; pob[(old+1):(old+new),8] <- tmp$IMSS1; pob[(old+1):(old+new),9] <- tmp$ISSSTE1; pob[(old+1):(old+new),10] <- tmp$SEGU_POP1; pob[(old+1):(old+new),11] <- tmp$ALFABET2; pob[(old+1):(old+new),12] <- tmp$TAM_LOC01+tmp$TAM_LOC02+tmp$TAM_LOC03+tmp$TAM_LOC04+tmp$TAM_LOC05+tmp$TAM_LOC06; pob[(old+1):(old+new),13] <- tmp$NIV_ESCO00+tmp$NIV_ESCO01;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/mor/17_mor_pob.dbf")
old <- nrow(pob); new <- nrow(tmp);
pob[(old+1):(old+new),1] <- tmp$ENTIDAD; pob[(old+1):(old+new),2] <- tmp$SECCION; pob[(old+1):(old+new),3] <- tmp$POB_TOT; pob[(old+1):(old+new),4] <- tmp$POB_TOT-tmp$EDQUI01-tmp$EDQUI02-tmp$EDQUI03; pob[(old+1):(old+new),5] <- tmp$SEXO2/tmp$POB_TOT; pob[(old+1):(old+new),6] <- tmp$EDQUI04+tmp$EDQUI05+tmp$EDQUI06; pob[(old+1):(old+new),7] <- tmp$EDQUI14+tmp$EDQUI15+tmp$EDQUI16+tmp$EDQUI17+tmp$EDQUI18+tmp$EDQUI19+tmp$EDQUI20+tmp$EDQUI21; pob[(old+1):(old+new),8] <- tmp$IMSS1; pob[(old+1):(old+new),9] <- tmp$ISSSTE1; pob[(old+1):(old+new),10] <- tmp$SEGU_POP1; pob[(old+1):(old+new),11] <- tmp$ALFABET2; pob[(old+1):(old+new),12] <- tmp$TAM_LOC01+tmp$TAM_LOC02+tmp$TAM_LOC03+tmp$TAM_LOC04+tmp$TAM_LOC05+tmp$TAM_LOC06; pob[(old+1):(old+new),13] <- tmp$NIV_ESCO00+tmp$NIV_ESCO01;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/nay/18_nay_pob.dbf")
old <- nrow(pob); new <- nrow(tmp);
pob[(old+1):(old+new),1] <- tmp$ENTIDAD; pob[(old+1):(old+new),2] <- tmp$SECCION; pob[(old+1):(old+new),3] <- tmp$POB_TOT; pob[(old+1):(old+new),4] <- tmp$POB_TOT-tmp$EDQUI01-tmp$EDQUI02-tmp$EDQUI03; pob[(old+1):(old+new),5] <- tmp$SEXO2/tmp$POB_TOT; pob[(old+1):(old+new),6] <- tmp$EDQUI04+tmp$EDQUI05+tmp$EDQUI06; pob[(old+1):(old+new),7] <- tmp$EDQUI14+tmp$EDQUI15+tmp$EDQUI16+tmp$EDQUI17+tmp$EDQUI18+tmp$EDQUI19+tmp$EDQUI20+tmp$EDQUI21; pob[(old+1):(old+new),8] <- tmp$IMSS1; pob[(old+1):(old+new),9] <- tmp$ISSSTE1; pob[(old+1):(old+new),10] <- tmp$SEGU_POP1; pob[(old+1):(old+new),11] <- tmp$ALFABET2; pob[(old+1):(old+new),12] <- tmp$TAM_LOC01+tmp$TAM_LOC02+tmp$TAM_LOC03+tmp$TAM_LOC04+tmp$TAM_LOC05+tmp$TAM_LOC06; pob[(old+1):(old+new),13] <- tmp$NIV_ESCO00+tmp$NIV_ESCO01;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/nl/19_nl_pob.dbf")
old <- nrow(pob); new <- nrow(tmp);
pob[(old+1):(old+new),1] <- tmp$ENTIDAD; pob[(old+1):(old+new),2] <- tmp$SECCION; pob[(old+1):(old+new),3] <- tmp$POB_TOT; pob[(old+1):(old+new),4] <- tmp$POB_TOT-tmp$EDQUI01-tmp$EDQUI02-tmp$EDQUI03; pob[(old+1):(old+new),5] <- tmp$SEXO2/tmp$POB_TOT; pob[(old+1):(old+new),6] <- tmp$EDQUI04+tmp$EDQUI05+tmp$EDQUI06; pob[(old+1):(old+new),7] <- tmp$EDQUI14+tmp$EDQUI15+tmp$EDQUI16+tmp$EDQUI17+tmp$EDQUI18+tmp$EDQUI19+tmp$EDQUI20+tmp$EDQUI21; pob[(old+1):(old+new),8] <- tmp$IMSS1; pob[(old+1):(old+new),9] <- tmp$ISSSTE1; pob[(old+1):(old+new),10] <- tmp$SEGU_POP1; pob[(old+1):(old+new),11] <- tmp$ALFABET2; pob[(old+1):(old+new),12] <- tmp$TAM_LOC01+tmp$TAM_LOC02+tmp$TAM_LOC03+tmp$TAM_LOC04+tmp$TAM_LOC05+tmp$TAM_LOC06; pob[(old+1):(old+new),13] <- tmp$NIV_ESCO00+tmp$NIV_ESCO01;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/oax/20_oax_pob.dbf")
old <- nrow(pob); new <- nrow(tmp);
pob[(old+1):(old+new),1] <- tmp$ENTIDAD; pob[(old+1):(old+new),2] <- tmp$SECCION; pob[(old+1):(old+new),3] <- tmp$POB_TOT; pob[(old+1):(old+new),4] <- tmp$POB_TOT-tmp$EDQUI01-tmp$EDQUI02-tmp$EDQUI03; pob[(old+1):(old+new),5] <- tmp$SEXO2/tmp$POB_TOT; pob[(old+1):(old+new),6] <- tmp$EDQUI04+tmp$EDQUI05+tmp$EDQUI06; pob[(old+1):(old+new),7] <- tmp$EDQUI14+tmp$EDQUI15+tmp$EDQUI16+tmp$EDQUI17+tmp$EDQUI18+tmp$EDQUI19+tmp$EDQUI20+tmp$EDQUI21; pob[(old+1):(old+new),8] <- tmp$IMSS1; pob[(old+1):(old+new),9] <- tmp$ISSSTE1; pob[(old+1):(old+new),10] <- tmp$SEGU_POP1; pob[(old+1):(old+new),11] <- tmp$ALFABET2; pob[(old+1):(old+new),12] <- tmp$TAM_LOC01+tmp$TAM_LOC02+tmp$TAM_LOC03+tmp$TAM_LOC04+tmp$TAM_LOC05+tmp$TAM_LOC06; pob[(old+1):(old+new),13] <- tmp$NIV_ESCO00+tmp$NIV_ESCO01;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/pue/21_pue_pob.dbf")
old <- nrow(pob); new <- nrow(tmp);
pob[(old+1):(old+new),1] <- tmp$ENTIDAD; pob[(old+1):(old+new),2] <- tmp$SECCION; pob[(old+1):(old+new),3] <- tmp$POB_TOT; pob[(old+1):(old+new),4] <- tmp$POB_TOT-tmp$EDQUI01-tmp$EDQUI02-tmp$EDQUI03; pob[(old+1):(old+new),5] <- tmp$SEXO2/tmp$POB_TOT; pob[(old+1):(old+new),6] <- tmp$EDQUI04+tmp$EDQUI05+tmp$EDQUI06; pob[(old+1):(old+new),7] <- tmp$EDQUI14+tmp$EDQUI15+tmp$EDQUI16+tmp$EDQUI17+tmp$EDQUI18+tmp$EDQUI19+tmp$EDQUI20+tmp$EDQUI21; pob[(old+1):(old+new),8] <- tmp$IMSS1; pob[(old+1):(old+new),9] <- tmp$ISSSTE1; pob[(old+1):(old+new),10] <- tmp$SEGU_POP1; pob[(old+1):(old+new),11] <- tmp$ALFABET2; pob[(old+1):(old+new),12] <- tmp$TAM_LOC01+tmp$TAM_LOC02+tmp$TAM_LOC03+tmp$TAM_LOC04+tmp$TAM_LOC05+tmp$TAM_LOC06; pob[(old+1):(old+new),13] <- tmp$NIV_ESCO00+tmp$NIV_ESCO01;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/que/22_qro_pob.dbf")
old <- nrow(pob); new <- nrow(tmp);
pob[(old+1):(old+new),1] <- tmp$ENTIDAD; pob[(old+1):(old+new),2] <- tmp$SECCION; pob[(old+1):(old+new),3] <- tmp$POB_TOT; pob[(old+1):(old+new),4] <- tmp$POB_TOT-tmp$EDQUI01-tmp$EDQUI02-tmp$EDQUI03; pob[(old+1):(old+new),5] <- tmp$SEXO2/tmp$POB_TOT; pob[(old+1):(old+new),6] <- tmp$EDQUI04+tmp$EDQUI05+tmp$EDQUI06; pob[(old+1):(old+new),7] <- tmp$EDQUI14+tmp$EDQUI15+tmp$EDQUI16+tmp$EDQUI17+tmp$EDQUI18+tmp$EDQUI19+tmp$EDQUI20+tmp$EDQUI21; pob[(old+1):(old+new),8] <- tmp$IMSS1; pob[(old+1):(old+new),9] <- tmp$ISSSTE1; pob[(old+1):(old+new),10] <- tmp$SEGU_POP1; pob[(old+1):(old+new),11] <- tmp$ALFABET2; pob[(old+1):(old+new),12] <- tmp$TAM_LOC01+tmp$TAM_LOC02+tmp$TAM_LOC03+tmp$TAM_LOC04+tmp$TAM_LOC05+tmp$TAM_LOC06; pob[(old+1):(old+new),13] <- tmp$NIV_ESCO00+tmp$NIV_ESCO01;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/qui/23_qroo_pob.dbf")
old <- nrow(pob); new <- nrow(tmp);
pob[(old+1):(old+new),1] <- tmp$ENTIDAD; pob[(old+1):(old+new),2] <- tmp$SECCION; pob[(old+1):(old+new),3] <- tmp$POB_TOT; pob[(old+1):(old+new),4] <- tmp$POB_TOT-tmp$EDQUI01-tmp$EDQUI02-tmp$EDQUI03; pob[(old+1):(old+new),5] <- tmp$SEXO2/tmp$POB_TOT; pob[(old+1):(old+new),6] <- tmp$EDQUI04+tmp$EDQUI05+tmp$EDQUI06; pob[(old+1):(old+new),7] <- tmp$EDQUI14+tmp$EDQUI15+tmp$EDQUI16+tmp$EDQUI17+tmp$EDQUI18+tmp$EDQUI19+tmp$EDQUI20+tmp$EDQUI21; pob[(old+1):(old+new),8] <- tmp$IMSS1; pob[(old+1):(old+new),9] <- tmp$ISSSTE1; pob[(old+1):(old+new),10] <- tmp$SEGU_POP1; pob[(old+1):(old+new),11] <- tmp$ALFABET2; pob[(old+1):(old+new),12] <- tmp$TAM_LOC01+tmp$TAM_LOC02+tmp$TAM_LOC03+tmp$TAM_LOC04+tmp$TAM_LOC05+tmp$TAM_LOC06; pob[(old+1):(old+new),13] <- tmp$NIV_ESCO00+tmp$NIV_ESCO01;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/san/24_slp_pob.dbf")
old <- nrow(pob); new <- nrow(tmp);
pob[(old+1):(old+new),1] <- tmp$ENTIDAD; pob[(old+1):(old+new),2] <- tmp$SECCION; pob[(old+1):(old+new),3] <- tmp$POB_TOT; pob[(old+1):(old+new),4] <- tmp$POB_TOT-tmp$EDQUI01-tmp$EDQUI02-tmp$EDQUI03; pob[(old+1):(old+new),5] <- tmp$SEXO2/tmp$POB_TOT; pob[(old+1):(old+new),6] <- tmp$EDQUI04+tmp$EDQUI05+tmp$EDQUI06; pob[(old+1):(old+new),7] <- tmp$EDQUI14+tmp$EDQUI15+tmp$EDQUI16+tmp$EDQUI17+tmp$EDQUI18+tmp$EDQUI19+tmp$EDQUI20+tmp$EDQUI21; pob[(old+1):(old+new),8] <- tmp$IMSS1; pob[(old+1):(old+new),9] <- tmp$ISSSTE1; pob[(old+1):(old+new),10] <- tmp$SEGU_POP1; pob[(old+1):(old+new),11] <- tmp$ALFABET2; pob[(old+1):(old+new),12] <- tmp$TAM_LOC01+tmp$TAM_LOC02+tmp$TAM_LOC03+tmp$TAM_LOC04+tmp$TAM_LOC05+tmp$TAM_LOC06; pob[(old+1):(old+new),13] <- tmp$NIV_ESCO00+tmp$NIV_ESCO01;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/sin/_5_sin_pob.dbf")
old <- nrow(pob); new <- nrow(tmp);
pob[(old+1):(old+new),1] <- tmp$ENTIDAD; pob[(old+1):(old+new),2] <- tmp$SECCION; pob[(old+1):(old+new),3] <- tmp$POB_TOT; pob[(old+1):(old+new),4] <- tmp$POB_TOT-tmp$EDQUI01-tmp$EDQUI02-tmp$EDQUI03; pob[(old+1):(old+new),5] <- tmp$SEXO2/tmp$POB_TOT; pob[(old+1):(old+new),6] <- tmp$EDQUI04+tmp$EDQUI05+tmp$EDQUI06; pob[(old+1):(old+new),7] <- tmp$EDQUI14+tmp$EDQUI15+tmp$EDQUI16+tmp$EDQUI17+tmp$EDQUI18+tmp$EDQUI19+tmp$EDQUI20+tmp$EDQUI21; pob[(old+1):(old+new),8] <- tmp$IMSS1; pob[(old+1):(old+new),9] <- tmp$ISSSTE1; pob[(old+1):(old+new),10] <- tmp$SEGU_POP1; pob[(old+1):(old+new),11] <- tmp$ALFABET2; pob[(old+1):(old+new),12] <- tmp$TAM_LOC01+tmp$TAM_LOC02+tmp$TAM_LOC03+tmp$TAM_LOC04+tmp$TAM_LOC05+tmp$TAM_LOC06; pob[(old+1):(old+new),13] <- tmp$NIV_ESCO00+tmp$NIV_ESCO01;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/son/26_son_pob.dbf")
old <- nrow(pob); new <- nrow(tmp);
pob[(old+1):(old+new),1] <- tmp$ENTIDAD; pob[(old+1):(old+new),2] <- tmp$SECCION; pob[(old+1):(old+new),3] <- tmp$POB_TOT; pob[(old+1):(old+new),4] <- tmp$POB_TOT-tmp$EDQUI01-tmp$EDQUI02-tmp$EDQUI03; pob[(old+1):(old+new),5] <- tmp$SEXO2/tmp$POB_TOT; pob[(old+1):(old+new),6] <- tmp$EDQUI04+tmp$EDQUI05+tmp$EDQUI06; pob[(old+1):(old+new),7] <- tmp$EDQUI14+tmp$EDQUI15+tmp$EDQUI16+tmp$EDQUI17+tmp$EDQUI18+tmp$EDQUI19+tmp$EDQUI20+tmp$EDQUI21; pob[(old+1):(old+new),8] <- tmp$IMSS1; pob[(old+1):(old+new),9] <- tmp$ISSSTE1; pob[(old+1):(old+new),10] <- tmp$SEGU_POP1; pob[(old+1):(old+new),11] <- tmp$ALFABET2; pob[(old+1):(old+new),12] <- tmp$TAM_LOC01+tmp$TAM_LOC02+tmp$TAM_LOC03+tmp$TAM_LOC04+tmp$TAM_LOC05+tmp$TAM_LOC06; pob[(old+1):(old+new),13] <- tmp$NIV_ESCO00+tmp$NIV_ESCO01;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/tab/27_tab_pob.dbf")
old <- nrow(pob); new <- nrow(tmp);
pob[(old+1):(old+new),1] <- tmp$ENTIDAD; pob[(old+1):(old+new),2] <- tmp$SECCION; pob[(old+1):(old+new),3] <- tmp$POB_TOT; pob[(old+1):(old+new),4] <- tmp$POB_TOT-tmp$EDQUI01-tmp$EDQUI02-tmp$EDQUI03; pob[(old+1):(old+new),5] <- tmp$SEXO2/tmp$POB_TOT; pob[(old+1):(old+new),6] <- tmp$EDQUI04+tmp$EDQUI05+tmp$EDQUI06; pob[(old+1):(old+new),7] <- tmp$EDQUI14+tmp$EDQUI15+tmp$EDQUI16+tmp$EDQUI17+tmp$EDQUI18+tmp$EDQUI19+tmp$EDQUI20+tmp$EDQUI21; pob[(old+1):(old+new),8] <- tmp$IMSS1; pob[(old+1):(old+new),9] <- tmp$ISSSTE1; pob[(old+1):(old+new),10] <- tmp$SEGU_POP1; pob[(old+1):(old+new),11] <- tmp$ALFABET2; pob[(old+1):(old+new),12] <- tmp$TAM_LOC01+tmp$TAM_LOC02+tmp$TAM_LOC03+tmp$TAM_LOC04+tmp$TAM_LOC05+tmp$TAM_LOC06; pob[(old+1):(old+new),13] <- tmp$NIV_ESCO00+tmp$NIV_ESCO01;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/tam/28_tamp_pob.dbf")
old <- nrow(pob); new <- nrow(tmp);
pob[(old+1):(old+new),1] <- tmp$ENTIDAD; pob[(old+1):(old+new),2] <- tmp$SECCION; pob[(old+1):(old+new),3] <- tmp$POB_TOT; pob[(old+1):(old+new),4] <- tmp$POB_TOT-tmp$EDQUI01-tmp$EDQUI02-tmp$EDQUI03; pob[(old+1):(old+new),5] <- tmp$SEXO2/tmp$POB_TOT; pob[(old+1):(old+new),6] <- tmp$EDQUI04+tmp$EDQUI05+tmp$EDQUI06; pob[(old+1):(old+new),7] <- tmp$EDQUI14+tmp$EDQUI15+tmp$EDQUI16+tmp$EDQUI17+tmp$EDQUI18+tmp$EDQUI19+tmp$EDQUI20+tmp$EDQUI21; pob[(old+1):(old+new),8] <- tmp$IMSS1; pob[(old+1):(old+new),9] <- tmp$ISSSTE1; pob[(old+1):(old+new),10] <- tmp$SEGU_POP1; pob[(old+1):(old+new),11] <- tmp$ALFABET2; pob[(old+1):(old+new),12] <- tmp$TAM_LOC01+tmp$TAM_LOC02+tmp$TAM_LOC03+tmp$TAM_LOC04+tmp$TAM_LOC05+tmp$TAM_LOC06; pob[(old+1):(old+new),13] <- tmp$NIV_ESCO00+tmp$NIV_ESCO01;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/tla/29_tlax_pob.dbf")
old <- nrow(pob); new <- nrow(tmp);
pob[(old+1):(old+new),1] <- tmp$ENTIDAD; pob[(old+1):(old+new),2] <- tmp$SECCION; pob[(old+1):(old+new),3] <- tmp$POB_TOT; pob[(old+1):(old+new),4] <- tmp$POB_TOT-tmp$EDQUI01-tmp$EDQUI02-tmp$EDQUI03; pob[(old+1):(old+new),5] <- tmp$SEXO2/tmp$POB_TOT; pob[(old+1):(old+new),6] <- tmp$EDQUI04+tmp$EDQUI05+tmp$EDQUI06; pob[(old+1):(old+new),7] <- tmp$EDQUI14+tmp$EDQUI15+tmp$EDQUI16+tmp$EDQUI17+tmp$EDQUI18+tmp$EDQUI19+tmp$EDQUI20+tmp$EDQUI21; pob[(old+1):(old+new),8] <- tmp$IMSS1; pob[(old+1):(old+new),9] <- tmp$ISSSTE1; pob[(old+1):(old+new),10] <- tmp$SEGU_POP1; pob[(old+1):(old+new),11] <- tmp$ALFABET2; pob[(old+1):(old+new),12] <- tmp$TAM_LOC01+tmp$TAM_LOC02+tmp$TAM_LOC03+tmp$TAM_LOC04+tmp$TAM_LOC05+tmp$TAM_LOC06; pob[(old+1):(old+new),13] <- tmp$NIV_ESCO00+tmp$NIV_ESCO01;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/ver/30_ver_pob.dbf")
old <- nrow(pob); new <- nrow(tmp);
pob[(old+1):(old+new),1] <- tmp$ENTIDAD; pob[(old+1):(old+new),2] <- tmp$SECCION; pob[(old+1):(old+new),3] <- tmp$POB_TOT; pob[(old+1):(old+new),4] <- tmp$POB_TOT-tmp$EDQUI01-tmp$EDQUI02-tmp$EDQUI03; pob[(old+1):(old+new),5] <- tmp$SEXO2/tmp$POB_TOT; pob[(old+1):(old+new),6] <- tmp$EDQUI04+tmp$EDQUI05+tmp$EDQUI06; pob[(old+1):(old+new),7] <- tmp$EDQUI14+tmp$EDQUI15+tmp$EDQUI16+tmp$EDQUI17+tmp$EDQUI18+tmp$EDQUI19+tmp$EDQUI20+tmp$EDQUI21; pob[(old+1):(old+new),8] <- tmp$IMSS1; pob[(old+1):(old+new),9] <- tmp$ISSSTE1; pob[(old+1):(old+new),10] <- tmp$SEGU_POP1; pob[(old+1):(old+new),11] <- tmp$ALFABET2; pob[(old+1):(old+new),12] <- tmp$TAM_LOC01+tmp$TAM_LOC02+tmp$TAM_LOC03+tmp$TAM_LOC04+tmp$TAM_LOC05+tmp$TAM_LOC06; pob[(old+1):(old+new),13] <- tmp$NIV_ESCO00+tmp$NIV_ESCO01;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/yuc/31_yuc_pob.dbf")
old <- nrow(pob); new <- nrow(tmp);
pob[(old+1):(old+new),1] <- tmp$ENTIDAD; pob[(old+1):(old+new),2] <- tmp$SECCION; pob[(old+1):(old+new),3] <- tmp$POB_TOT; pob[(old+1):(old+new),4] <- tmp$POB_TOT-tmp$EDQUI01-tmp$EDQUI02-tmp$EDQUI03; pob[(old+1):(old+new),5] <- tmp$SEXO2/tmp$POB_TOT; pob[(old+1):(old+new),6] <- tmp$EDQUI04+tmp$EDQUI05+tmp$EDQUI06; pob[(old+1):(old+new),7] <- tmp$EDQUI14+tmp$EDQUI15+tmp$EDQUI16+tmp$EDQUI17+tmp$EDQUI18+tmp$EDQUI19+tmp$EDQUI20+tmp$EDQUI21; pob[(old+1):(old+new),8] <- tmp$IMSS1; pob[(old+1):(old+new),9] <- tmp$ISSSTE1; pob[(old+1):(old+new),10] <- tmp$SEGU_POP1; pob[(old+1):(old+new),11] <- tmp$ALFABET2; pob[(old+1):(old+new),12] <- tmp$TAM_LOC01+tmp$TAM_LOC02+tmp$TAM_LOC03+tmp$TAM_LOC04+tmp$TAM_LOC05+tmp$TAM_LOC06; pob[(old+1):(old+new),13] <- tmp$NIV_ESCO00+tmp$NIV_ESCO01;
tmp <- read.dbf("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/zac/_2_zac_pob.dbf")
old <- nrow(pob); new <- nrow(tmp);
pob[(old+1):(old+new),1] <- tmp$ENTIDAD; pob[(old+1):(old+new),2] <- tmp$SECCION; pob[(old+1):(old+new),3] <- tmp$POB_TOT; pob[(old+1):(old+new),4] <- tmp$POB_TOT-tmp$EDQUI01-tmp$EDQUI02-tmp$EDQUI03; pob[(old+1):(old+new),5] <- tmp$SEXO2/tmp$POB_TOT; pob[(old+1):(old+new),6] <- tmp$EDQUI04+tmp$EDQUI05+tmp$EDQUI06; pob[(old+1):(old+new),7] <- tmp$EDQUI14+tmp$EDQUI15+tmp$EDQUI16+tmp$EDQUI17+tmp$EDQUI18+tmp$EDQUI19+tmp$EDQUI20+tmp$EDQUI21; pob[(old+1):(old+new),8] <- tmp$IMSS1; pob[(old+1):(old+new),9] <- tmp$ISSSTE1; pob[(old+1):(old+new),10] <- tmp$SEGU_POP1; pob[(old+1):(old+new),11] <- tmp$ALFABET_2; pob[(old+1):(old+new),12] <- tmp$TAM_LOC01+tmp$TAM_LOC02+tmp$TAM_LOC03+tmp$TAM_LOC04+tmp$TAM_LOC05+tmp$TAM_LOC06; pob[(old+1):(old+new),13] <- tmp$NIV_ESCO00+tmp$NIV_ESCO01;
rm(tmp,old,new)
