# Obtiene datos elección 2000 de veracruz

setwd("d:/01/data/elecs/MXelsCalendGovt/elecReturns/DatosBrutos")
rm(list = ls())


# PARTE 1: EXTRAE Y GUARDA INFO DE LA WEB
rawdata <- "En sus marcas..."
path<-c("http://www.iev.org.mx/1resutadoselec/dto_")
for (n in 1:9){
    direc<-paste(path,0,n,".html",sep="")
    url<-url(direc,open="r")
    tmp<-c(paste("cacapunchis distrito",n),readLines(url))
    close(url)
    rawdata <- append(rawdata, tmp)
}
for (n in 10:24){
    direc<-paste(path,n,".html",sep="")
    url<-url(direc,open="r")
    tmp<-c(paste("cacapunchis distrito",n),readLines(url))
    close(url)
    rawdata <- append(rawdata, tmp)
}

filename<-paste("part",n,".txt",sep="")
write.table(rawdata, "ver00ayMu.txt", sep="\t")


setwd("d:/01/data/elecs/uk")

# PARTE 2: ORGANIZA INFO

setwd("d:/01/data/elecs/MXelsCalendGovt/elecReturns/DatosBrutos")

rawData<-c("AQUI EMMPIEZA")
rawData<-append(rawData, readLines("prepAyYuc10.txt"))

### #SI FUERAN MUCHOS ARCHIVOS...
### for (n in 1:9){
###     fileName<-paste("part0",n,".txt",sep="")
###     file<-file(fileName,open="r")
###     text<-readLines(file)
###     rawData<-append(rawData,text)
###     close(file)
### }
### for (n in 10:22){
###     fileName<-paste("part",n,".txt",sep="")
###     file<-file(fileName,open="r")
###     text<-readLines(file)
###     rawData<-append(rawData,text)
###     close(file)
### }


# PARTE 3: EXTRAE CONSTITUENCY DATA

##where in rawData are constituency starting lines

tmp<- grep(rawData,
           pattern=".*tendencia.*ayuntamiento_id=1")
constRows<-c(tmp[1]-1,length(rawData))
tmp<- grep(rawData,
           pattern=".*tendencia.*ayuntamiento_id")
munNameRows <- tmp+3
N<-length(munNameRows)
munNums<-rep(NA,N)
munNames<-rep(NA,N)

for(i in 1:N){
    munNames[i] <-
    sub(rawData[munNameRows[i]],
    pattern=".*<td.*texto\">(.*)</td>",
    replacement="\\1")
}

for(i in 1:N){
    munNums[i] <-
    sub(rawData[munNameRows[i]-1],
    pattern=".*numero\">(.*)</td>.*",
    replacement="\\1")
}
munNums<-as.numeric(munNums)

tmp<-c(munNums,107)
tmp2<-rep(NA,times=N)
for (i in 1:N){
    tmp2[i]<- tmp[i+1]-tmp[i]-1
}
tmp3<-data.frame(a=munNums,b=tmp2,c=rep(0,times=N))
tmp3$c<-tmp3$a+tmp3$b
missingMuns <- tmp3$c[tmp3$b==1]

totalActas<-rep(NA,times=N)
for(i in 1:N){
    totalActas[i]<-
    sub(rawData[munNameRows[i]+3],
    pattern=".*numero\">(.*)<.*",
    replacement="\\1")
}
totalActas<-as.numeric(totalActas)

actasCapt<-rep(NA,times=N)
for(i in 1:N){
    actasCapt[i]<-
    sub(rawData[munNameRows[i]+4],
    pattern=".*numero\">(.*)<.*",
    replacement="\\1")
}
actasCapt<-as.numeric(totalActas)

lisNom<-rep(NA,N); pan<-rep(NA,N); pri<-rep(NA,N); prd<-rep(NA,N); pt<-rep(NA,N); pvem<-rep(NA,N); conve<-rep(NA,N); local<-rep(NA,N); panal<-rep(NA,N); cc1<-rep(NA,N); cc2<-rep(NA,N); nul<-rep(NA,N)

for(i in 1:N){
    lisNom[i]<-
    sub(rawData[munNameRows[i]+5],
    pattern=".*class=\"numero\">(.*)</td.*class.*>(.*)<.*class.*>(.*)<.*class.*>(.*)<.*class.*>(.*)<.*class.*>(.*)<.*class.*>(.*)<.*class.*>(.*)<.*class.*>(.*)<.*class.*>(.*)<.*class.*>(.*)<.*class.*>(.*)<.*class=\"numero\".*",
    replacement="\\1")
    pan[i]<-
    sub(rawData[munNameRows[i]+5],
    pattern=".*numero.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*numero(.*)",
    replacement="\\1")
    pri[i]<-
    sub(rawData[munNameRows[i]+5],
    pattern=".*numero.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*numero(.*)",
    replacement="\\2")
    prd[i]<-
    sub(rawData[munNameRows[i]+5],
    pattern=".*numero.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*numero(.*)",
    replacement="\\3")
    pt[i]<-
    sub(rawData[munNameRows[i]+5],
    pattern=".*numero.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*numero(.*)",
    replacement="\\4")
    pvem[i]<-
    sub(rawData[munNameRows[i]+5],
    pattern=".*numero.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*numero(.*)",
    replacement="\\5")
    conve[i]<-
    sub(rawData[munNameRows[i]+5],
    pattern=".*numero.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*numero(.*)",
    replacement="\\6")
    local[i]<-
    sub(rawData[munNameRows[i]+5],
    pattern=".*numero.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*numero(.*)",
    replacement="\\7")
    panal[i]<-
    sub(rawData[munNameRows[i]+5],
    pattern=".*numero.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*numero(.*)",
    replacement="\\8")
    cc1[i]<-
    sub(rawData[munNameRows[i]+5],
    pattern=".*numero.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*numero(.*)",
    replacement="\\9")
#    cc2[i]<-
#    sub(rawData[munNameRows[i]+5],
#    pattern=".*numero.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*dato.>([0-9]{0,6})<.*numero(.*)",
#    replacement="\\10")
    nul[i]<-
    sub(rawData[munNameRows[i]+5],
    pattern=".*numero.*>([0-9]{0,6})</td><td class=\"numero(.*)",
    replacement="\\1")
}



###                                                                         1                                         2                                         3                                    4                                    5                                       6                                    7                                    8                                   9                                     10                                  11 
### "\t\t<td width='47' class=\"numero\">4348</td><td width='43px' class=\"dato\">1684</td><td width='43px' class=\"dato\">1659</td><td width='43px' class='nodato'></td><td width='43px' class='nodato'></td><td width='43px' class=\"dato\">391</td><td width='43px' class='nodato'></td><td width='43px' class='nodato'></td><td width='43px' class='nodato'></td><td width='43px' class='nodato'></td><td width='43px' class='nodato'></td><td width='43px' class=\"dato\">54</td><td class=\"numero\"><a href=\"detalle_ayuntamiento.php?ayuntamiento_id=1\"><img src=\"images/detalle.gif\" title='Detalle' width=\"20\" height=\"16\"></a></td></tr>"

lisNom<-as.numeric(lisNom)
pan<-as.numeric(pan)
pri<-as.numeric(pri)
prd<-as.numeric(prd)
pt<-as.numeric(pt)
pvem<-as.numeric(pvem)
conve<-as.numeric(conve)
local<-as.numeric(local)
panal<-as.numeric(panal)
cc1<-as.numeric(cc1)
#cc2<-as.numeric(cc2)
nul<-as.numeric(nul)


### # USA GSUB EN VEZ DE SUB PARA QUE REPITA EL PATRÓN DE BÚSQUEDA SI FUERA NECESARIO (EJ. 1,234,567 VOTOS)
### votes<-rep(NA,nCand)
### for (j in 1:nCand){
###     votes[j]<-
###     gsub(rawData[candStartRow[i]+j-1],
###     pattern=".*<tr><td( nowrap)?>.*</td><td>.*</td><td align=right>([0-9]{0,3}[,]{0,1}[0-9]{1,3})</td><td align=right>.*</td></tr>.*",
###     replacement="\\2")
### }
### #QUITA COMAS
### votes<-gsub(votes,pattern=",",replacement="")
### votes<-as.numeric(votes)

tmp<-actasCapt*100/totalActas

aymuyuc2010prep<-data.frame(num=munNums,mun=munNames,actasTot=totalActas,actasCapt=tmp,lisnom=lisNom,pan=pan,pri=pri,
                            prd=prd,pt=pt,pvem=pvem,conve=conve,local=local,panal=panal,cc1=cc1,cc2=cc2,nul=nul)

#ADDS MISSING MUNICIPIOS
M<-length(missingMuns)
for (i in N+1:M){
    aymuyuc2010prep[i,]<-NA
    }
for (i in N+1:M){
    aymuyuc2010prep$num[i]<-missingMuns[i-N]
    }

#EXPORTA
write.csv(aymuyuc2010prep, file="aymuyuc2010prep.csv")

