#############################################################
## ####################################################### ##
## ## code to re-construct most re-seccionamiento cases ## ##
## ## invoked from elec-data-for-maps                   ## ##
## ####################################################### ##
#############################################################


########################################################################
## change orig.dest with non-adjacent seccion nums to vector notation ##
########################################################################
sel <- grep("[|]", eq$orig.dest)
#eq$orig.dest[sel]
if (length(sel)>0){
    tmp <- gsub("[|]", ",", eq$orig.dest[sel], perl = TRUE)
    tmp <- paste0("c(", tmp, ")")
    eq$orig.dest[sel] <- tmp
}
##
#############################################################
## change seccion to edosecn to avoid need to specify edon ##
## add dummy dunbaja indicating a manipulation             ##
#############################################################
eq$seccion <- eq$edon * 10000 + eq$seccion
v94s <- within(v94s, {
    seccion <- edon * 10000 + seccion;
    dunbaja <- 0 })
v97s <- within(v97s, {
    seccion <- edon * 10000 + seccion;
    dunbaja <- 0 })
v00s <- within(v00s, {
    seccion <- edon * 10000 + seccion;
    dunbaja <- 0 })
v03s <- within(v03s, {
    seccion <- edon * 10000 + seccion;
    dunbaja <- 0 })
v06s <- within(v06s, {
    seccion <- edon * 10000 + seccion;
    dunbaja <- 0 })
v09s <- within(v09s, {
    seccion <- edon * 10000 + seccion;
    dunbaja <- 0 })
v12s <- within(v12s, {
    seccion <- edon * 10000 + seccion;
    dunbaja <- 0 })
v15s <- within(v15s, {
    seccion <- edon * 10000 + seccion;
    dunbaja <- 0 })
v18s <- within(v18s, {
    seccion <- edon * 10000 + seccion;
    dunbaja <- 0 })
v21s <- within(v21s, {
    seccion <- edon * 10000 + seccion;
    dunbaja <- 0 })



##############################################################
## manipulate secciones that suffered some reseccionamiento ##
##############################################################
sum.split <- function(year.var = NA) {
    ##
    #year.var <- 2015 # debug
    if (year.var==1991) d <- v91s
    if (year.var==1994) d <- v94s
    if (year.var==1997) d <- v97s
    if (year.var==2000) d <- v00s
    if (year.var==2003) d <- v03s
    if (year.var==2006) d <- v06s
    if (year.var==2009) d <- v09s
    if (year.var==2012) d <- v12s
    if (year.var==2015) d <- v15s
    if (year.var==2018) d <- v18s
    if (year.var==2021) d <- v21s
    ##
    ##d <- v03s # debug
    ##
    sel.agg <- which(d$seccion %in% sel.to) ## sel.to must be defined before invoking this function below
    sel.col <- setdiff(colnames(d),
                       c("edosecn", "edon", "seccion", "disn", "ife", "inegi", "mun", "dunbaja", "nota"
                       ## , "d94", "d97", "d00", "d03", "d06", "d09", "d12", "d15", "d18", "d21"
                       ## , "ife1994", "ife1997", "ife2000", "ife2003", "ife2006", "ife2009", "ife2012", "ife2015", "ife2018", "ife2021"
                       ## , "dis1979", "dis1997", "dis2006", "dis2013", "dis2018"
                         )) # exclude non-numeric columns that needn't sum-up
    ##
    ## sum votes
    totals <- colSums(d[sel.agg, sel.col], na.rm = TRUE)
    ## dummies
    if (!is.na(totals["dpanc"])    & totals["dpanc"]>0   )  totals["dpanc"]    <- 1
    if (!is.na(totals["dpric"])    & totals["dpric"]>0   )  totals["dpric"]    <- 1
    if (!is.na(totals["dprdc"])    & totals["dprdc"]>0   )  totals["dprdc"]    <- 1
    if (!is.na(totals["dmorenac"]) & totals["dmorenac"]>0)  totals["dmorenac"] <- 1
    if (!is.na(totals["dptc"])     & totals["dptc"]>0    )  totals["dptc"]     <- 1
    ## paste them into eliminated sección
    sel.target <- which(d$seccion==info$seccion) ## info must be defined before invoking this function below
    d[sel.target,sel.col] <- totals;
    d$dunbaja[sel.target] <- 1;  # indicates manipulation
    ##
    ## return manipulated data
    return(d)
}

# handle split cases --- since sección "dada de baja", can easily reaggregate votes after 
# split's year w/o causing trouble, sección unused afterwards
# (sums-up new secciones to add votes to split seccion after it was dropped)


#####################################################################
## Will sum-up offsprings' vote for years after these parents      ##
## were removed (needed for backwards autoregressive prediction)   ##
#####################################################################
for (rnd in 1:3){ ## loop over three possible succesive reseccionamiento actions in each seccion in eq
    #rnd <- 1 # debug
    if (rnd==1) sel.resec <- which(eq$action== "split.to")
    if (rnd==2) sel.resec <- which(eq$action2=="split.to")
    if (rnd==3) sel.resec <- which(eq$action3=="split.to")
    ##table(eq$when[sel.resec])
    ## stop if empty
    if (length(sel.resec)==0) next
    for (i in sel.resec){
        ##i <- sel.resec[626] # debug
        ## get basic info to re-aggregate post-split seccciones
        info <- eq[i, c("edon","seccion","action","orig.dest","when","action2","orig.dest2","when2","action3","orig.dest3","when3")]
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
        if (year < 1994) v94s <- sum.split(1994); #debug: print("1994 done")
        if (year < 1997) v97s <- sum.split(1997); #debug: print("1997 done")
        if (year < 2000) v00s <- sum.split(2000); #debug: print("2000 done")
        if (year < 2003) v03s <- sum.split(2003); #debug: print("2003 done")
        if (year < 2006) v06s <- sum.split(2006); #debug: print("2006 done")
        if (year < 2009) v09s <- sum.split(2009); #debug: print("2009 done")
        if (year < 2012) v12s <- sum.split(2012); #debug: print("2012 done")
        if (year < 2015) v15s <- sum.split(2015); #debug: print("2015 done")
        if (year < 2018) v18s <- sum.split(2018); #debug: print("2018 done")
        if (year < 2021) v21s <- sum.split(2021); #debug: print("2021 done")
    }
}



#######################################################################
## Will use father's vote for years before these offspring created   ##
#######################################################################
for (rnd in 1:3){ ## loop over three possible succesive reseccionamiento actions in each seccion in eq
    #rnd <- 1 # debug
    if (rnd==1) sel.resec <- which(eq$action== "split.from")
    if (rnd==2) sel.resec <- which(eq$action2=="split.from")
    if (rnd==3) sel.resec <- which(eq$action3=="split.from")
    ##table(eq$when[sel.resec])
    ## stop if empty
    if (length(sel.resec)==0) next
    for (i in sel.resec){
        ##i <- sel.resec[1] # debug
        ## get basic info to re-aggregate post-split seccciones
        info <- eq[i, c("edon","seccion","action","orig.dest","when","action2","orig.dest2","when2","action3","orig.dest3","when3")]
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
for (rnd in 1:3){ ## loop over three possible succesive reseccionamiento actions in each seccion in eq
    #rnd <- 1 # debug
    if (rnd==1) sel.resec <- which(eq$action== "merged.to")
    if (rnd==2) sel.resec <- which(eq$action2=="merged.to")
    if (rnd==3) sel.resec <- which(eq$action3=="merged.to")
    ##table(eq$when[sel.resec])
    ## stop if empty
    if (length(sel.resec)==0) next
    for (i in sel.resec){
        ##i <- sel.resec[1] # debug
        ## get basic info to re-aggregate post-split seccciones
        info <- eq[i, c("edon","seccion","action","orig.dest","when","action2","orig.dest2","when2","action3","orig.dest3","when3")]
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

