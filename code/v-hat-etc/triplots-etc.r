# workdir
wd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/ife.ine/")


######################################################################
## Function transforming ternary into cartesian coordinates         ##
## https://stackoverflow.com/questions/11623602/shaded-triplot-in-r ##
######################################################################
tern2cart <- function(coord){
    coord[1] -> x
    coord[2] -> y
    coord[3] -> z
    x+y+z -> tot
    x/tot -> x
    y/tot -> y
    z/tot -> z
    (2*y + z)/(2*(x+y+z)) -> x1
    sqrt(3)*z/(2*(x+y+z)) -> y1
    return(c(x1,y1))
    }
# function to add vértice labels
add.lab <- function(labels=c("no","label","defined"), add.sign=TRUE){
    left  <- labels[2];
    right <- labels[1];
    up    <- labels[3];
    text(0,0,labels=left,pos=1)         # left
    text(1,0,labels=right,pos=1)        # right
    text(0.5,sqrt(3)/2,labels=up,pos=3) # up
    if (add.sign==TRUE) text(0.5,0,labels="@emagar",pos=1,cex=.75,col="gray")
}

###############################
## function wrapping triplot ##
###############################
la.ternera <- function(datos, color = rgb(.55,.27,.07, alpha = .2), cex.pts = .15, main = NA, labs=c("PAN","PRI","Morena"), left.right.up=c("pri","pan","left"), add.sign=TRUE){
    # Prepare data: re-arrange so pan is lower right, pri lower left, morena is above 
    datos <- datos[,left.right.up] # subset
    #Then transformed into cartesian coordinates:
    datos <- t(apply(datos,1,tern2cart))
    # Draw the empty ternary diagram:
    par(mar=c(2.1, 2.1, 4.1, 2.1)) ## SETS B L U R MARGIN SIZES
    plot(NA, NA, xlim=c(0,1), ylim=c(0,sqrt(3)/2), asp=1, bty="n", axes=F, xlab="", ylab="", main = main)
    segments(0,0,0.5,sqrt(3)/2)
    segments(0.5,sqrt(3)/2,1,0)
    segments(1,0,0,0)
    # add vértice labels
    add.lab(labs, add.sign=add.sign)
    ## # add a grid:
    ## a <- seq(0.9,0.1,by=-0.1)
    ## b <- rep(0,9)
    ## c <- seq(0.1,0.9,by=0.1)
    ## grid <- data.frame(x=c(a, b, c, a, c, b),y=c(b, c, a, c, b, a),z=c(c, a, b, b, a, c))
    ## t(apply(grid,1,tern2cart)) -> grid.tern
    ## cbind(grid.tern[1:27,],grid.tern[28:54,]) -> grid
    ## apply(grid,1,function(x){segments(x0=x[1],y0=x[2],x1=x[3],y1=x[4],lty=2,col="grey80")})
    ## # axis labels
    ## paste(seq(10,90,by=10),"%")->lab
    ## text(grid.tern[9:1,],paste(lab,"\n(PAN)"),col="grey80",cex=0.7, pos=2)
    ## text(grid.tern[18:10,],paste(lab,"\n(PRI)"),col="grey80",cex=0.7, pos=4)
    ## text(grid.tern[27:19,],paste(lab,"\n(Morena)"),col="grey80",cex=0.7, pos=1)
    # or 50-50 to 33-33-33 lines instead
    ## a <- c(1,1,0)
    ## b <- c(1,0,1)
    ## c <- c(0,1,1)
    ## d <- c(1,1,1)
    ## grid <- data.frame(matrix(c(a,b,c,d),nrow=4,byrow=TRUE))
    ## grid.tern <- t(apply(grid,1,tern2cart))
    ## for (i in 1:3){
    ##     segments(x0=grid.tern[i,1],y0=grid.tern[i,2],x1=grid.tern[4,1],y1=grid.tern[4,2],lty=2,col="grey10")
    ## }
    # or 10 percent bands
    a <- c( 57.5,42.5,   0)
    b <- c( 57.5,   0,42.5)
    c <- c(130/3,85/3,85/3)
    grid <- data.frame(matrix(c(a,b,c),nrow=3,byrow=TRUE))
    grid.tern <- t(apply(grid,1,tern2cart))
    for (i in 1:2){
        #i <- 1 # debug
        segments(x0=grid.tern[i,1],y0=grid.tern[i,2],x1=grid.tern[3,1],y1=grid.tern[3,2],lty=2,col="grey10")
    }
    a <- c(42.5, 57.5,   0)
    b <- c(   0, 57.5,42.5)
    c <- c(85/3,130/3,85/3)
    grid <- data.frame(matrix(c(a,b,c),nrow=3,byrow=TRUE))
    grid.tern <- t(apply(grid,1,tern2cart))
    for (i in 1:2){
        #i <- 1 # debug
        segments(x0=grid.tern[i,1],y0=grid.tern[i,2],x1=grid.tern[3,1],y1=grid.tern[3,2],lty=2,col="grey10")
    }
    a <- c(42.5,   0, 57.5)
    b <- c(   0,42.5, 57.5)
    c <- c(85/3,85/3,130/3)
    grid <- data.frame(matrix(c(a,b,c),nrow=3,byrow=TRUE))
    grid.tern <- t(apply(grid,1,tern2cart))
    for (i in 1:2){
        #i <- 1 # debug
        segments(x0=grid.tern[i,1],y0=grid.tern[i,2],x1=grid.tern[3,1],y1=grid.tern[3,2],lty=2,col="grey10")
    }
    # Plot points:
    points(datos, pch = 20, cex = cex.pts, col = color)
}

##################
## party colors ##
##################
col.pan <-    rgb(.18,.24,.73, alpha = .2) # moderate blue
col.vxm <-    rgb(  0,.75,  1, alpha = .2) # deepskyblue
col.pri <-    rgb(.89,.17,.17, alpha = .2) # brightred
col.morena <- rgb(.55,.27,.07, alpha = .2) # saddlebrown
col.pvem   <- rgb(  0,.80,  0, alpha = .2) # green 3
col.mc     <- rgb(.93,.46,  0, alpha = .2) # darkorange2
col.oth    <- rgb(.55,.55,.55, alpha = .2) # gray 59
#col.oth    <- rgb(.94, .5, .5, alpha = .2) # lightcoral
#19/255

############################################################
## ###################################################### ##
## ## Load data or use elec-data-for-maps to generate  ## ##
## ###################################################### ##
############################################################

#####################
## municipio-level ##
#####################
ls()
extendCoal.2015 <- read.csv(file = paste(wd, "data/dipfed-municipio-vhat-2015.csv", sep = ""), stringsAsFactors = FALSE)
extendCoal.2018 <- read.csv(file = paste(wd, "data/dipfed-municipio-vhat-2018.csv", sep = ""), stringsAsFactors = FALSE)
extendCoal.2021 <- read.csv(file = paste(wd, "data/dipfed-municipio-vhat-2021.csv", sep = ""), stringsAsFactors = FALSE)


# for pan/pri/morena
raw.2021 <- read.csv(file = paste(wd, "data/dipfed-municipio-vraw-2021.csv", sep = ""), stringsAsFactors = FALSE)
raw.2021 <- within(raw.2021, {
    pan <- pan + panc + prd;
    #pri <- pri + pric; 
    left <- morena + morenac + pt;
    #pvem <- pvem + pvemc;
    oth <- pes + rsp + fxm + indep;
    panc <- pric <- morena <- morenac <- pvemc <- prd <- pt <- pes <- rsp <- fxm <- indep <- NULL;
})

# alianzas/oth
raw.2021 <- read.csv(file = paste(wd, "data/dipfed-municipio-vraw-2021.csv", sep = ""), stringsAsFactors = FALSE)
raw.2021 <- within(raw.2021, {
    #vxm <- pan + panc + pri + pric + prd;
    vxm <- pan + panc + pri + prd;
    #left <- morena + morenac + pt + pvem + pvemc;
    left <- morena + morenac + pt + pvem;
    oth <- efec - vxm - left;
})

# oth breakdown
raw.2021 <- read.csv(file = paste(wd, "data/dipfed-municipio-vraw-2021.csv", sep = ""), stringsAsFactors = FALSE)
raw.2021 <- raw.2021[,c("ife","pvem","pt","mc","lisnom")]
raw.2021 <- within(raw.2021, {
    efec <- pvem + pt + mc;
})

####################
## ############## ##
## ## triplots ## ##
## ############## ##
####################
#
##########
## 2021 ##
##########
# triplot observed
#tmp <- extendCoal.2021[,c("pan", "pri", "left")] # subset
tmp <- raw.2021[,c("mc", "pt", "pvem")] # subset
tri.color <- apply(tmp, 1, which.max); names(tri.color) <- NULL
tri.color[tri.color==1] <- col.mc
tri.color[tri.color==2] <- col.pri
tri.color[tri.color==3] <- col.pvem
pdf(file = paste(wd, "graph/triplot2021-v-mu-pob.pdf", sep = ""))
png(file = paste(wd, "graph/triplot2021-v-mu-pob.png", sep = ""))
la.ternera(datos = tmp, cex.pts = sqrt(raw.2021$lisnom)/100, color = tri.color, main ="Tres oportunistas en 2021 (excluyendo demás partidos)", labs=c("mc","pt","pvem"), left.right.up=c("pt","mc","pvem"), add.sign=TRUE)
dev.off()

summary(sqrt(raw.2021$lisnom)/100)
x


# triplot predicted
tmp <- extendCoal.2021[,c("vhat.pan", "vhat.pri", "vhat.left")] # subset
colnames(tmp) <- c("pan", "pri", "left")
tri.color <- apply(tmp, 1, which.max); names(tri.color) <- NULL
tri.color[tri.color==1] <- col.pan
tri.color[tri.color==2] <- col.pri
tri.color[tri.color==3] <- col.morena
#pdf(file = paste(wd, "graph/triplot2021-vhat-mu.pdf", sep = ""))
#png(file = paste(wd, "graph/triplot2021-vhat-mu.png", sep = ""))
la.ternera(datos = tmp, cex.pts = 2, color = tri.color, main ="Pronóstico 2021", labs=c("pan","pri","morena"), left.right.up=c("pri","pan","left"), add.sign=TRUE)# expression(hat(v)[2018]))
#dev.off()

##########
## 2018 ##
##########
# triplot observed
tmp <- extendCoal.2018[,c("pan", "pri", "left")] # subset
tri.color <- apply(tmp, 1, which.max); names(tri.color) <- NULL
tri.color[tri.color==1] <- col.pan
tri.color[tri.color==2] <- col.pri
tri.color[tri.color==3] <- col.morena
#pdf(file = paste(wd, "graph/triplot2018-v-mu.pdf", sep = ""))
#png(file = paste(wd, "graph/triplot2018-v-mu.png", sep = ""))
la.ternera(datos = tmp, cex.pts = 2, color = tri.color, main ="Observación 2018")
#dev.off()

# triplot predicted
tmp <- extendCoal.2018[,c("vhat.pan", "vhat.pri", "vhat.left")] # subset
colnames(tmp) <- c("pan", "pri", "left")
tri.color <- apply(tmp, 1, which.max); names(tri.color) <- NULL
tri.color[tri.color==1] <- col.pan
tri.color[tri.color==2] <- col.pri
tri.color[tri.color==3] <- col.morena
#pdf(file = paste(wd, "graph/triplot2018-vhat-mu.pdf", sep = ""))
#png(file = paste(wd, "graph/triplot2018-vhat-mu.png", sep = ""))
la.ternera(datos = tmp, cex.pts = 2, color = tri.color, main ="Pronóstico 2018")# expression(hat(v)[2018]))
#dev.off()

##########
## 2015 ##
##########
# triplot observed
tmp <- extendCoal.2015[,c("pan", "pri", "left")] # subset
tri.color <- apply(tmp, 1, which.max); names(tri.color) <- NULL
tri.color[tri.color==1] <- col.pan
tri.color[tri.color==2] <- col.pri
tri.color[tri.color==3] <- col.morena
#pdf(file = paste(wd, "graph/triplot2015-v-mu.pdf", sep = ""))
#png(file = paste(wd, "graph/triplot2015-v-mu.png", sep = ""))
la.ternera(datos = tmp, cex.pts = 2, color = tri.color, main = "Observación 2015")
#dev.off()

# triplot predicted
tmp <- extendCoal.2015[,c("vhat.pan", "vhat.pri", "vhat.left")] # subset
colnames(tmp) <- c("pan", "pri", "left")
tri.color <- apply(tmp, 1, which.max); names(tri.color) <- NULL
tri.color[tri.color==1] <- col.pan
tri.color[tri.color==2] <- col.pri
tri.color[tri.color==3] <- col.morena
#pdf(file = paste(wd, "graph/triplot2015-vhat-mu.pdf", sep = ""))
#png(file = paste(wd, "graph/triplot2015-vhat-mu.png", sep = ""))
la.ternera(datos = tmp, cex.pts = 2, color = tri.color, main = "Pronóstico 2015")
#dev.off()


###################
## sección-level ##
###################
extendCoal.2018 <- read.csv(file = paste(wd, "data/dipfed2018seccion-vhat.csv", sep = ""), stringsAsFactors = FALSE)

##########
## 2018 ##
##########
# triplot observed
tmp <- extendCoal.2018[,c("pan", "pri", "left")] # subset
tmp <- tmp[-which(is.na(rowSums(tmp))==TRUE),] # drop NAs
tri.color <- apply(tmp, 1, which.max); names(tri.color) <- NULL
tri.color[tri.color==1] <- col.pan
tri.color[tri.color==2] <- col.pri
tri.color[tri.color==3] <- col.morena
#pdf(file = paste(wd, "graph/triplot2018-v-se.pdf", sep = ""))
#png(file = paste(wd, "graph/triplot2018-v-se.png", sep = ""))
la.ternera(datos = tmp, cex.pts = .2, color = tri.color, main = "Voto 2018 observado")
#dev.off()

# triplot predicted
tmp <- extendCoal.2018[,c("vhat.pan", "vhat.pri", "vhat.left")] # subset
tmp <- tmp[-which(is.na(rowSums(tmp))==TRUE),] # drop NAs
colnames(tmp) <- c("pan", "pri", "left")
tri.color <- apply(tmp, 1, which.max); names(tri.color) <- NULL
tri.color[tri.color==1] <- col.pan
tri.color[tri.color==2] <- col.pri
tri.color[tri.color==3] <- col.morena
#pdf(file = paste(wd, "graph/triplot2018-vhat-se.pdf", sep = ""))
#png(file = paste(wd, "graph/triplot2018-vhat-se.png", sep = ""))
la.ternera(datos = tmp, cex.pts = .2, color = tri.color, main = "Voto 2018 pronosticado")
#dev.off()


##################################
## ############################ ##
## ## residual vs core plots ## ##
## ############################ ##
##################################

# data
tmp <- extendCoal.2018
dim(tmp)
tmp <- tmp[-which(is.na(rowSums(tmp))==TRUE),] # drop NAs

#####################
## pri vs pri core ##
#####################
#png(file = paste(wd, "graph/resid-pri-2018-vs-pri-core-se.png", sep = ""))
plot(x = tmp$alphahat.pri, y = (tmp$pri - tmp$vhat.pri), type = "n", xlab = "Tamaño del núcleo del PRI", ylab = expression(Voto~observado - pronóstico), main = "El PRI en 2018: residual vs núcleos")
abline(h = 0, col = "black", lty = 2)
points(x = tmp$alphahat.pri, y = (tmp$pri - tmp$vhat.pri), pch = 20, cex = .15, col = col.pri)
fit <- lm((pri - vhat.pri) ~ poly(alphahat.pri,3), data = tmp)
lo.hi <- round(quantile(tmp$alphahat.pri, probs = c(.02,.98)),2)
xx <- seq(from = lo.hi[1], to = lo.hi[2], by = .01)
lines(xx, predict.lm(fit, data.frame(alphahat.pri=xx)), col = "black", lwd = 1.5)
#dev.off()



########################
## morena vs pan core ##
########################
#png(file = paste(wd, "graph/resid-morena-2018-vs-pan-core-se.png", sep = ""))
plot(x = tmp$alphahat.pan, y = (tmp$morena - tmp$vhat.morena), type = "n", xlab = "Tamaño del núcleo del PAN", ylab = expression(Voto~observado - pronóstico), main = "Morena en 2018: residual vs núcleos")
abline(h = 0, col = "black", lty = 2)
points(x = tmp$alphahat.pan, y = (tmp$morena - tmp$vhat.morena), pch = 20, cex = .15, col = col.morena)
fit <- lm((morena - vhat.morena) ~ poly(alphahat.pan,3), data = tmp)
lo.hi <- round(quantile(tmp$alphahat.pan, probs = c(.02,.98)),2)
xx <- seq(from = lo.hi[1], to = lo.hi[2], by = .01)
lines(xx, predict.lm(fit, data.frame(alphahat.pan=xx)), col = "black", lwd = 1.5)
#dev.off()

########################
## morena vs pri core ##
########################
#png(file = paste(wd, "graph/resid-morena-2018-vs-pri-core-se.png", sep = ""))
plot(x = tmp$alphahat.pri, y = (tmp$morena - tmp$vhat.morena), type = "n", xlab = "Tamaño del núcleo del PRI", ylab = expression(Voto~observado - pronóstico), main = "Morena en 2018: residual vs núcleos")
abline(h = 0, col = "black", lty = 2)
points(x = tmp$alphahat.pri, y = (tmp$morena - tmp$vhat.morena), pch = 20, cex = .15, col = col.morena)
fit <- lm((morena - vhat.morena) ~ poly(alphahat.pri,3), data = tmp)
lo.hi <- round(quantile(tmp$alphahat.pri, probs = c(.02,.98)),2)
xx <- seq(from = lo.hi[1], to = lo.hi[2], by = .01)
lines(xx, predict.lm(fit, data.frame(alphahat.pri=xx)), col = "black", lwd = 1.5)
#dev.off()

###########################
## morena vs morena core ##
###########################
#png(file = paste(wd, "graph/resid-morena-2018-vs-morena-core-se.png", sep = ""))
plot(x = tmp$alphahat.morena, y = (tmp$morena - tmp$vhat.morena), type = "n", xlab = "Tamaño del núcleo de Morena", ylab = expression(Voto~observado - pronóstico), main = "Morena en 2018: residual vs núcleos")
abline(h = 0, col = "black", lty = 2)
points(x = tmp$alphahat.morena, y = (tmp$morena - tmp$vhat.morena), pch = 20, cex = .15, col = col.morena)
fit <- lm((morena - vhat.morena) ~ poly(alphahat.morena,3), data = tmp)
lo.hi <- round(quantile(tmp$alphahat.morena, probs = c(.02,.98)),2)
xx <- seq(from = lo.hi[1], to = lo.hi[2], by = .01)
lines(xx, predict.lm(fit, data.frame(alphahat.morena=xx)), col = "black", lwd = 1.5)
#dev.off()

#####################
## pan vs pri core ##
#####################
#png(file = paste(wd, "graph/resid-pan-2018-vs-pri-core-se.png", sep = ""))
plot(x = tmp$alphahat.pri, y = (tmp$pan - tmp$vhat.pan), type = "n", xlab = "Tamaño del núcleo del PRI", ylab = expression(Voto~observado - pronóstico), main = "El PAN en 2018: residual vs núcleos")
abline(h = 0, col = "black", lty = 2)
points(x = tmp$alphahat.pri, y = (tmp$pan - tmp$vhat.pan), pch = 20, cex = .15, col = col.pan)
fit <- lm((pan - vhat.pan) ~ poly(alphahat.pri,3), data = tmp)
lo.hi <- round(quantile(tmp$alphahat.pri, probs = c(.02,.98)),2)
xx <- seq(from = lo.hi[1], to = lo.hi[2], by = .01)
lines(xx, predict.lm(fit, data.frame(alphahat.pri=xx)), col = "black", lwd = 1.5)
#dev.off()

#####################
## pan vs pan core ##
#####################
#png(file = paste(wd, "graph/resid-pan-2018-vs-pan-core-se.png", sep = ""))
plot(x = tmp$alphahat.pan, y = (tmp$pan - tmp$vhat.pan), type = "n", xlab = "Tamaño del núcleo del PAN", ylab = expression(Voto~observado - pronóstico), main = "El PAN en 2018: residual vs núcleos")
abline(h = 0, col = "black", lty = 2)
points(x = tmp$alphahat.pan, y = (tmp$pan - tmp$vhat.pan), pch = 20, cex = .15, col = col.pan)
fit <- lm((pan - vhat.pan) ~ poly(alphahat.pan,3), data = tmp)
lo.hi <- round(quantile(tmp$alphahat.pan, probs = c(.02,.98)),2)
xx <- seq(from = lo.hi[1], to = lo.hi[2], by = .01)
lines(xx, predict.lm(fit, data.frame(alphahat.pan=xx)), col = "black", lwd = 1.5)
#dev.off()


###################
## alpha vs beta ##
###################
d <- read.csv(file = paste(wd, "data/dipfed2015seccion-vhat.csv", sep = ""), stringsAsFactors = FALSE)
head(d)

#png(file = paste(wd, "graph/tmp.png", sep = ""))
plot(d$alphahat.pan, d$betahat.pan, type = "n")
abline(h=0, lty = 2)
points(x = d$alphahat.pan, y = d$betahat.pan, pch = 20, cex = .15, col = col.pan)
#dev.off()

#png(file = paste(wd, "graph/tmp.png", sep = ""))
plot(d$alphahat.morena, d$betahat.morena, type = "n")
abline(h=0, lty = 2)
points(x = d$alphahat.morena, y = d$betahat.morena, pch = 20, cex = .15, col = col.morena)
#dev.off()



