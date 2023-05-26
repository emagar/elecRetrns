##################
## party colors ##
##################
col.pan <-    rgb(.18,.24,.73, alpha = .2) # moderate blue
col.vxm <-    rgb(  0,.75,  1, alpha = .2) # deepskyblue
col.pri <-    rgb(.89,.17,.17, alpha = .2) # brightred
#col.morena <- rgb(.55,.27,.07, alpha = .2) # saddlebrown
col.morena <- rgb(.55,.10,.10, alpha = .2) # firebrick4
#col.prd <-    rgb(  1,.84,  0, alpha = .2) # gold
col.prd <-    rgb(  1,.76,.15, alpha = .2) # goldenrod1
col.pvem   <- rgb(  0,.80,  0, alpha = .2) # green 3
col.mc     <- rgb(.93,.46,  0, alpha = .2) # darkorange2
col.oth    <- rgb(.55,.55,.55, alpha = .2) # gray 59
#col.oth    <- rgb(.94, .5, .5, alpha = .2) # lightcoral

###################################
## function to compute residuals ##
###################################
add.res <- function(x){
    within(x, {
        res.pan <- pan - vhat.pan;
        res.pri <- pri - vhat.pri;
        res.left <- left - vhat.left;
    })
}

############################################################
## ###################################################### ##
## ## Load data or use elec-data-for-maps to generate  ## ##
## ###################################################### ##
############################################################
wd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/ife.ine/")

#####################
## municipio-level ##
#####################
pty <- "PRI"
yr <- 2006
alfa <- "PRI" # what alpha in x-axis?
save.to.disk <- FALSE
##
if (yr==1988) dat <- read.csv(file = paste(wd, "data/mun/dipfed-municipio-vhat-1988.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==1991) dat <- read.csv(file = paste(wd, "data/mun/dipfed-municipio-vhat-1991.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==1994) dat <- read.csv(file = paste(wd, "data/mun/dipfed-municipio-vhat-1994.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==1997) dat <- read.csv(file = paste(wd, "data/mun/dipfed-municipio-vhat-1997.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==2000) dat <- read.csv(file = paste(wd, "data/mun/dipfed-municipio-vhat-2000.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==2003) dat <- read.csv(file = paste(wd, "data/mun/dipfed-municipio-vhat-2003.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==2006) dat <- read.csv(file = paste(wd, "data/mun/dipfed-municipio-vhat-2006.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==2009) dat <- read.csv(file = paste(wd, "data/mun/dipfed-municipio-vhat-2009.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==2012) dat <- read.csv(file = paste(wd, "data/mun/dipfed-municipio-vhat-2012.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==2015) dat <- read.csv(file = paste(wd, "data/mun/dipfed-municipio-vhat-2015.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==2018) dat <- read.csv(file = paste(wd, "data/mun/dipfed-municipio-vhat-2018.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==2021) dat <- read.csv(file = paste(wd, "data/mun/dipfed-municipio-vhat-2021.csv", sep = ""), stringsAsFactors = FALSE)
##
dat <- add.res(dat)
##
tit <- paste(pty, "in", yr, "(municipios)")
xl  <- ifelse(alfa=="PRI", "PRI's alpha", "alpha")
with(dat, {
    if (save.to.disk==TRUE) pdf(file=paste0("../../mail/", pty, yr, "alpha", alfa, ".mun.pdf"))
    plot(x=c(0,1), y=c(-.85,.85), type="n", xlab = xl, ylab = "residual (v - v.hat)", axes = FALSE, main = tit)
    axis(1, at = seq(0,1,.2))
    axis(2, at = seq(-1,1,.2))
    abline(h=0, col = "gray")
    if (pty=="PAN"    & alfa=="PAN")    points(alphahat.pan,  res.pan,  col = col.pan,    pch = 19, cex = .6)
    if (pty=="PRI"    & alfa=="PRI")    points(alphahat.pri,  res.pri,  col = col.pri,    pch = 19, cex = .6)
    if (pty=="MORENA" & alfa=="MORENA") points(alphahat.left, res.left, col = col.morena, pch = 19, cex = .6)
    if (pty=="PRD"    & alfa=="PRD")    points(alphahat.left, res.left, col = col.prd,    pch = 19, cex = .6)
    if (pty=="PAN"    & alfa=="PRI")    points(alphahat.pri,  res.pan,  col = col.pan,    pch = 19, cex = .6)
    if (pty=="PRI"    & alfa=="PRI")    points(alphahat.pri,  res.pri,  col = col.pri,    pch = 19, cex = .6)
    if (pty=="MORENA" & alfa=="PRI")    points(alphahat.pri, res.left, col = col.morena, pch = 19, cex = .6)
    if (pty=="PRD"    & alfa=="PRI")    points(alphahat.pri, res.left, col = col.prd,    pch = 19, cex = .6)
    ## add regression line
    if (pty=="PAN"    & alfa=="PAN")    abline(reg=lm(res.pan  ~ alphahat.pan))
    if (pty=="PRI"    & alfa=="PRI")    abline(reg=lm(res.pri  ~ alphahat.pri))
    if (pty=="MORENA" & alfa=="MORENA") abline(reg=lm(res.left ~ alphahat.left))
    if (pty=="PRD"    & alfa=="PRD")    abline(reg=lm(res.left ~ alphahat.left))
    if (pty=="PAN"    & alfa=="PRI")    abline(reg=lm(res.pan  ~ alphahat.pri))
    if (pty=="PRI"    & alfa=="PRI")    abline(reg=lm(res.pri  ~ alphahat.pri))
    if (pty=="MORENA" & alfa=="PRI")    abline(reg=lm(res.left ~ alphahat.pri))
    if (pty=="PRD"    & alfa=="PRI")    abline(reg=lm(res.left ~ alphahat.pri))
    if (save.to.disk==TRUE) dev.off()
})

 

####################
## distrito-level ##
####################
pty <- "PRI"
yr <- 1991
alfa <- "PRI" # what alpha in x-axis?
save.to.disk <- TRUE
##
if (yr==1988) dat <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-1988.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==1991) dat <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-1991.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==1994) dat <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-1994.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==1997) dat <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-1997.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==2000) dat <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-2000.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==2003) dat <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-2003.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==2006) dat <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-2006.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==2009) dat <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-2009.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==2012) dat <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-2012.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==2015) dat <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-2015.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==2018) dat <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-2018.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==2021) dat <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-2021.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==2024) dat <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-2024.csv", sep = ""), stringsAsFactors = FALSE)
##
dat <- add.res(dat)
##
tit <- paste(pty, "in", yr, "(districts)")
xl  <- ifelse(alfa=="PRI", "PRI's alpha", "alpha")
with(dat, {
    if (save.to.disk==TRUE) pdf(file=paste0("../../mail/", pty, yr, "alpha", alfa, ".pdf"))
    plot(x=c(0,1), y=c(-.85,.85), type="n", xlab = xl, ylab = "residual (v - v.hat)", axes = FALSE, main = tit)
    axis(1, at = seq(0,1,.2))
    axis(2, at = seq(-1,1,.2))
    abline(h=0, col = "gray")
    if (pty=="PAN"    & alfa=="PAN")    points(alphahat.pan,  res.pan,  col = col.pan,    pch = 19, cex = .6)
    if (pty=="PRI"    & alfa=="PRI")    points(alphahat.pri,  res.pri,  col = col.pri,    pch = 19, cex = .6)
    if (pty=="MORENA" & alfa=="MORENA") points(alphahat.left, res.left, col = col.morena, pch = 19, cex = .6)
    if (pty=="PRD"    & alfa=="PRD")    points(alphahat.left, res.left, col = col.prd,    pch = 19, cex = .6)
    if (pty=="PAN"    & alfa=="PRI")    points(alphahat.pri,  res.pan,  col = col.pan,    pch = 19, cex = .6)
    if (pty=="PRI"    & alfa=="PRI")    points(alphahat.pri,  res.pri,  col = col.pri,    pch = 19, cex = .6)
    if (pty=="MORENA" & alfa=="PRI")    points(alphahat.pri, res.left, col = col.morena, pch = 19, cex = .6)
    if (pty=="PRD"    & alfa=="PRI")    points(alphahat.pri, res.left, col = col.prd,    pch = 19, cex = .6)
    ## add regression line
    if (pty=="PAN"    & alfa=="PAN")    abline(reg=lm(res.pan  ~ alphahat.pan))
    if (pty=="PRI"    & alfa=="PRI")    abline(reg=lm(res.pri  ~ alphahat.pri))
    if (pty=="MORENA" & alfa=="MORENA") abline(reg=lm(res.left ~ alphahat.left))
    if (pty=="PRD"    & alfa=="PRD")    abline(reg=lm(res.left ~ alphahat.left))
    if (pty=="PAN"    & alfa=="PRI")    abline(reg=lm(res.pan  ~ alphahat.pri))
    if (pty=="PRI"    & alfa=="PRI")    abline(reg=lm(res.pri  ~ alphahat.pri))
    if (pty=="MORENA" & alfa=="PRI")    abline(reg=lm(res.left ~ alphahat.pri))
    if (pty=="PRD"    & alfa=="PRI")    abline(reg=lm(res.left ~ alphahat.pri))
    if (save.to.disk==TRUE) dev.off()
})



###############
## BROUILLON ##
###############
pty <- "PRI"
yr1 <- 1988
yr2 <- 2018
save.to.disk <- FALSE
##
if (yr1==1988) dat1 <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-1988.csv", sep = ""), stringsAsFactors = FALSE)
if (yr1==1991) dat1 <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-1991.csv", sep = ""), stringsAsFactors = FALSE)
if (yr1==1994) dat1 <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-1994.csv", sep = ""), stringsAsFactors = FALSE)
if (yr1==1997) dat1 <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-1997.csv", sep = ""), stringsAsFactors = FALSE)
if (yr1==2000) dat1 <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-2000.csv", sep = ""), stringsAsFactors = FALSE)
if (yr1==2003) dat1 <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-2003.csv", sep = ""), stringsAsFactors = FALSE)
if (yr1==2006) dat1 <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-2006.csv", sep = ""), stringsAsFactors = FALSE)
if (yr1==2009) dat1 <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-2009.csv", sep = ""), stringsAsFactors = FALSE)
if (yr1==2012) dat1 <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-2012.csv", sep = ""), stringsAsFactors = FALSE)
if (yr1==2015) dat1 <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-2015.csv", sep = ""), stringsAsFactors = FALSE)
if (yr1==2018) dat1 <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-2018.csv", sep = ""), stringsAsFactors = FALSE)
if (yr1==2021) dat1 <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-2021.csv", sep = ""), stringsAsFactors = FALSE)
##
if (yr2==1988) dat2 <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-1988.csv", sep = ""), stringsAsFactors = FALSE)
if (yr2==1991) dat2 <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-1991.csv", sep = ""), stringsAsFactors = FALSE)
if (yr2==1994) dat2 <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-1994.csv", sep = ""), stringsAsFactors = FALSE)
if (yr2==1997) dat2 <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-1997.csv", sep = ""), stringsAsFactors = FALSE)
if (yr2==2000) dat2 <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-2000.csv", sep = ""), stringsAsFactors = FALSE)
if (yr2==2003) dat2 <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-2003.csv", sep = ""), stringsAsFactors = FALSE)
if (yr2==2006) dat2 <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-2006.csv", sep = ""), stringsAsFactors = FALSE)
if (yr2==2009) dat2 <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-2009.csv", sep = ""), stringsAsFactors = FALSE)
if (yr2==2012) dat2 <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-2012.csv", sep = ""), stringsAsFactors = FALSE)
if (yr2==2015) dat2 <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-2015.csv", sep = ""), stringsAsFactors = FALSE)
if (yr2==2018) dat2 <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-2018.csv", sep = ""), stringsAsFactors = FALSE)
if (yr2==2021) dat2 <- read.csv(file = paste(wd, "data/dis/dipfed-distrito-vhat-2021.csv", sep = ""), stringsAsFactors = FALSE)
##
if (pty=="PAN")    cl <- col.pan
if (pty=="PRI")    cl <- col.pri
if (pty=="MORENA") cl <- col.morena
if (pty=="PRD")    cl <- col.prd
##
dat1 <- add.res(dat1)
dat2 <- add.res(dat2)
##
plot(c(-.5,.5), c(-.5,.5), xlab=paste("residual",yr1), ylab=paste("residual",yr2), type="n")
abline(v=0,h=0)
points(x=dat1$res.pri, y=dat2$res.pri, col = cl, pch = 19, cex = .6)



dat1[1,]
x

















