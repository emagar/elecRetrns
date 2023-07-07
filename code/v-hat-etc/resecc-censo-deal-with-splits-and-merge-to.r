###################################################################
## ############################################################# ##
## ## code to re-construct most re-seccionamiento populations ## ##
## ## invoked from elec-data-for-maps                         ## ##
## ############################################################# ##
###################################################################

1 backup censo
2 subset censo.w = cases to manipulate
3 function for one census at a time = year.var
4 which columns to add-up
5 loop over censo.w rows
6 find indices in censo to be summed-up for that row
7 plug into censo.w
8 next

#censo.backup <- censo # backup
censo <- censo.backup # restore


########################################################################
## change orig.dest with non-adjacent seccion nums to vector notation ##
########################################################################
sel <- grep("[|]", censo$orig.dest)
#censo$orig.dest[sel]
if (length(sel)>0){
    tmp <- gsub("[|]", ",", censo$orig.dest[sel], perl = TRUE)
    tmp <- paste0("c(", tmp, ")")
    censo$orig.dest[sel] <- tmp
}
##
#################################################
## add dummy dmanuip indicating a manipulation ##
#################################################
censo$dmanip <- 0


##############################################################
## manipulate secciones that suffered some reseccionamiento ##
##############################################################
sum.split <- function(d=censo, year.var=2020, rnd=1) {
    ##
    ## if (year.var %notin% c(2005,2010,2020)){
    ##     print("Warning: year.var is not a sección-level census year")
    ##     stop
    ## }
    #d <- censo.w # debug
    d <- d
    ## exclude non-numeric columns that needn't sum-up
    sel.col <- setdiff(colnames(d),
                       c("ord", "edon", "edo", "seccion", "ife", "inegi", "mun", "dmanip", "OBSERVACIONES", "coment"
                       , "alta", "baja", "dmunchg"
                       , "action", "orig.dest", "when", "action2", "orig.dest2", "when2", "action3", "orig.dest3", "when3"
                       , "ife1991", "ife1994", "ife1997", "ife2000", "ife2003", "ife2006"
                       , "ife2009", "ife2012", "ife2015", "ife2018", "ife2021", "ife2024"
                       , "dis1979", "dis1997", "dis2006", "dis2013", "dis2018"
                         ))
    sel.col <- sel.col[grep(year.var, sel.col)] # crop sel.col to sum only year.var
    ##
    for (i in 1:nrow(d)){
        ##i <- 1 # debug
        if (rnd==1){
            year <- d$when[i]  # reseccionamiento year (modify data in subsequent years only)
            sel.to <- eval(str2expression(d$orig.dest [i])) # turn string w vector def into vector of target secciones to sum-up
        }
        if (rnd==2){
            year <- d$when2[i]  # reseccionamiento year (modify data in subsequent years only)
            sel.to <- eval(str2expression(d$orig.dest2[i])) # turn string w vector def into vector of target secciones to sum-up
        }
        if (rnd==3){
            year <- d$when3[i]  # reseccionamiento year (modify data in subsequent years only)
            sel.to <- eval(str2expression(d$orig.dest3[i])) # turn string w vector def into vector of target secciones to sum-up
        }
        sel.to <- d$edon[i] * 10000 + sel.to
        ##
        ##sel.agg <- which(d$seccion %in% sel.to) ## sel.to must be defined before invoking this function below
        sel.agg <- which(censo$seccion %in% sel.to) ## indices to be summed-up from censo
        ## sum population
        totals <- colSums(censo[sel.agg, sel.col], na.rm = TRUE)
        ## paste them into manipulated sección
        d[i,sel.col] <- totals;
        d$dmanip[i] <- 1;  # indicates manipulation
    }
    ##
    ## return manipulated data
    return(d)
}

## handle split cases --- use aggregated pop to generate census projections, then drop after dada de baja

since sección "dada de baja", can easily reaggregate population after 
## split's year w/o causing trouble, sección unused afterwards
## (sums-up new secciones to add votes to split seccion after it was dropped)

## add 2020 for secciones split inter-census
sel.r <- which(censo$action3=="split.to")
table(censo$when3[sel.r])
##
sel.r <- which(censo$action3=="split.to" & censo$when3==2005)
censo.w <- censo[sel.r,]  ## subset
censo.w <- sum.split(d=censo.w, year.var=2020, rnd=3)
censo.w <- sum.split(d=censo.w, year.var=2010, rnd=3)
censo[sel.r,] <- censo.w  ## return
##
sel.r <- which(censo$action3=="split.to" & censo$when3==2005)
censo.w <- censo[sel.r,]  ## subset
censo.w <- sum.split(d=censo.w, year.var=2020, rnd=3)
censo.w <- sum.split(d=censo.w, year.var=2010, rnd=3)
censo[sel.r,] <- censo.w  ## return


censo.w[sel.r, c("p18_2005","p18_2010","p18_2020","ptot_2005","ptot_2010","ptot_2020")]
censo.w[3,]

data.frame(sec=censo$seccion[sel.r], to=censo$orig.dest[sel.r], p18=censo$p18_2020[sel.r])
table(is.na(censo$p18_2020[sel.r]))
censo[sel.r[218],]


#####################################################################
## Will sum-up offsprings' vote for years after these parents      ##
## were removed (needed for backwards autoregressive prediction)   ##
#####################################################################
for (rnd in 1:3){ ## loop over three possible succesive reseccionamiento actions in each seccion in censo
    #rnd <- 1 # debug
    if (rnd==1) sel.resec <- which(censo$action =="split.to")
    if (rnd==2) sel.resec <- which(censo$action2=="split.to")
    if (rnd==3) sel.resec <- which(censo$action3=="split.to")
    ##table(censo$when[sel.resec])
    ## stop if empty
    if (length(sel.resec)==0) next
    for (i in sel.resec){
        ##i <- sel.resec[626] # debug
        ## get basic info to re-aggregate post-split seccciones
        info <- censo[i, c("edon","seccion","action","orig.dest","when","action2","orig.dest2","when2","action3","orig.dest3","when3")]
        if (rnd==1) {
            year <- info$when  # reseccionamiento year (modify data in subsequent years only)
            sel.to <- eval(str2expression(info$orig.dest )) # turn string w vector def into vector of target secciones to sum-up
        }
        if (rnd==2) {
            year <- info$when2
            sel.to <- eval(str2expression(info$orig.dest2)) # turn string w vector def into vector of target secciones to sum-up
        }
        if (rnd==3) {
            year <- info$when3
            sel.to <- eval(str2expression(info$orig.dest3)) # turn string w vector def into vector of target secciones to sum-up
        }
        ## use edosecn to single-out state and match seccion
        sel.to <- info$edon*10000 + sel.to
        ##
        ## now run function
        censo <- sum.split(year.var); #debug: print("2005 done")
    }
}



#######################################################################
## Will use father's vote for years before these offspring created   ##
#######################################################################
for (rnd in 1:3){ ## loop over three possible succesive reseccionamiento actions in each seccion in censo
    #rnd <- 1 # debug
    if (rnd==1) sel.resec <- which(censo$action== "split.from")
    if (rnd==2) sel.resec <- which(censo$action2=="split.from")
    if (rnd==3) sel.resec <- which(censo$action3=="split.from")
    ##table(censo$when[sel.resec])
    ## stop if empty
    if (length(sel.resec)==0) next
    for (i in sel.resec){
        ##i <- sel.resec[1] # debug
        ## get basic info to re-aggregate post-split seccciones
        info <- censo[i, c("edon","seccion","action","orig.dest","when","action2","orig.dest2","when2","action3","orig.dest3","when3")]
        if (rnd==1) {
            year <- info$when  # reseccionamiento year (modify data in subsequent years only)
            sel.to <- eval(str2expression(info$orig.dest )) # turn string w vector def into vector of target secciones to sum-up
        }
        if (rnd==2) {
            year <- info$when2
            sel.to <- eval(str2expression(info$orig.dest2)) # turn string w vector def into vector of target secciones to sum-up
        }
        if (rnd==3) {
            year <- info$when3
            sel.to <- eval(str2expression(info$orig.dest3)) # turn string w vector def into vector of target secciones to sum-up
        }
        ## use edosecn to single-out state and match seccion
        sel.to <- info$edon*10000 + sel.to               
        ##
        ## now run function
        if (year > 1994) v94s <- sum.split(1994);
        if (year > 1997) v97s <- sum.split(1997);
        if (year > 2000) v00s <- sum.split(2000);
        if (year > 2003) v03s <- sum.split(2003);
        if (year > 2006) v06s <- sum.split(2006);
        if (year > 2009) v09s <- sum.split(2009);
        if (year > 2012) v12s <- sum.split(2012);
        if (year > 2015) v15s <- sum.split(2015);
        if (year > 2018) v18s <- sum.split(2018);
        if (year > 2021) v21s <- sum.split(2021);
    }
}



###################################################################
## Will use phagocyter's vote for years after merged seccion     ##
## disappeared (needed for backwards autoregressive prediction)  ##
###################################################################
for (rnd in 1:3){ ## loop over three possible succesive reseccionamiento actions in each seccion in censo
    #rnd <- 1 # debug
    if (rnd==1) sel.resec <- which(censo$action== "merged.to")
    if (rnd==2) sel.resec <- which(censo$action2=="merged.to")
    if (rnd==3) sel.resec <- which(censo$action3=="merged.to")
    ##table(censo$when[sel.resec])
    ## stop if empty
    if (length(sel.resec)==0) next
    for (i in sel.resec){
        ##i <- sel.resec[1] # debug
        ## get basic info to re-aggregate post-split seccciones
        info <- censo[i, c("edon","seccion","action","orig.dest","when","action2","orig.dest2","when2","action3","orig.dest3","when3")]
        if (rnd==1) {
            year <- info$when  # reseccionamiento year (modify data in subsequent years only)
            sel.to <- eval(str2expression(info$orig.dest )) # turn string w vector def into vector of target secciones to sum-up
        }
        if (rnd==2) {
            year <- info$when2
            sel.to <- eval(str2expression(info$orig.dest2)) # turn string w vector def into vector of target secciones to sum-up
        }
        if (rnd==3) {
            year <- info$when3
            sel.to <- eval(str2expression(info$orig.dest3)) # turn string w vector def into vector of target secciones to sum-up
        }
        ## use edosecn to single-out state and match seccion
        sel.to <- info$edon*10000 + sel.to
        ##
        ## now run function
        if (year < 1994) v94s <- sum.split(1994);
        if (year < 1997) v97s <- sum.split(1997);
        if (year < 2000) v00s <- sum.split(2000);
        if (year < 2003) v03s <- sum.split(2003);
        if (year < 2006) v06s <- sum.split(2006);
        if (year < 2009) v09s <- sum.split(2009);
        if (year < 2012) v12s <- sum.split(2012);
        if (year < 2015) v15s <- sum.split(2015);
        if (year < 2018) v18s <- sum.split(2018);
        if (year < 2021) v21s <- sum.split(2021);
    }
}

## clean
rm(i, info, sel, sel.resec, sel.to, sum.split, tmp, year, rnd)

