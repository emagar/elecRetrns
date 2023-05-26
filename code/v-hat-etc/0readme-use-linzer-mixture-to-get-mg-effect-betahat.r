Para interpretar el electo marginal de betahat tendré que importar el método de Linzer.

Intenté usar mean_t(mean_u(r.pan)) +/- media sd pero llego al problema de cómo jugar con morena y oth simultáneamente.
Linzer usa el mixture model para determinar cómo co-varían las composiciones. Eso es lo que debo usar.

Lo que intenté:
1. determinar valor típico de v.hat para el municipio --- quizás mean sum_t (v.hat_t) / 10
2. determinar desviación típica --- +/-5 ó +/- 1sd
3. hacer sims object con estos valores
4. montecarlo prediction

rm(list = ls())
dd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/")
wd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/ife.ine/")
setwd(wd)
load("data/too-big-4-github/tmp3.RData")

sel.col <- grep("mean", colnames(yr.means))
apply(yr.means[,sel.col], 2, mean)
apply(yr.means[,sel.col], 2, sd)

apply(yr.means[,sel.col], 2, mean) + apply(yr.means[,sel.col], 2, sd)/2
apply(yr.means[,sel.col], 2, mean) - apply(yr.means[,sel.col], 2, sd)/2

i <- 1
data.tmp <- extendCoal[[i]]
summary(mean.regs$pan)
reg.tmp <- mean.regs[[i]]

r.bar.bar

# std error version
sims <- with(data.tmp,
              data.frame(dsameCoal=c(0,1),
                         dmultiRef=0,
#                         dmocion= 0,
                         drefHda=0,
                         dmajSen=0,
                         dinSen=0,
                         legyrR=seq(from=(min(legyrR)-.05), to=(max(legyrR)+.05), length.out = 100),
                         legyrR2=seq(from=(min(legyrR)-.05), to=(max(legyrR)+.05), length.out = 100)^2,
                         dreform2010=0,
                         netApprovR=0.33, # median(netApprovR),
#                         yr14 = 3,
                         legis = 2006
                         )
              )
sims2$pr <- predict(fit2e, newdata = sims2, type = "response")
sims2 <- cbind(sims2, predict(fit2e, newdata = sims2, type="link", se=TRUE))
sims2 <- within(sims2, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
sims2$legyr <- seq(from=1, to=0, length.out = 100) # for plot
head(sims2)
library(ggplot2)
gr <- "../graphs/"
#pdf (file = paste(gr, "predictedPr.pdf", sep = ""), width = 7, height = 4)
ggplot(sims2, aes(x = legyr, y = PredictedProb)) +
    geom_ribbon(aes(ymin = LL, ymax = UL, fill = factor(dsameCoal)), alpha = .2) +
    geom_line(aes(colour = factor(dsameCoal)), size=1) +
    labs(fill = "Coalition chair", colour = "Coalition chair",
         x = "Legislative year remaining (in months)",
         y = "Predicted probability") +
    scale_x_continuous(breaks=seq(from=0, to=1, length.out=7), labels=seq(from=12, to=0, by=-2))
#dev.off()






