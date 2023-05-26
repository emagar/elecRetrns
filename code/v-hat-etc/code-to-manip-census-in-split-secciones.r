#####################################################################
## Code invoked from within elec-data-for-maps.r                   ##
## manipulates seccion-level census accounting for split secciones ##
#####################################################################

# paste generic census object to eq2
eq2 <- eq
eq2 <- eq2[, c("edosecn","edon","seccion","inegi","ife","action","orig.dest","when","action2","orig.dest2","when2","action3","orig.dest3","when3")]
eq2 <- merge(x = eq2, y = generic, by = "edosecn", all.x = TRUE, all.y = FALSE)
# will receive counterfactual sums
eq2$cen_2020b <- eq2$cen_2010b <- eq2$cen_2005b <- NA

# monitor orig/new obs in eq2 after merge
#dim(eq); dim(eq2)
#which(eq$edosecn %notin% eq2$edosecn)
#which(eq2$edosecn %notin% eq$edosecn)
#sel <- which(duplicated(generic$edosecn)==TRUE)
#generic$edosecn[sel[1]]
#sel <- which(generic$edosecn==70004)
#pob18[sel,]

eq2 <- within(eq2, {
    pi <- NA;
    case <- NA;
    times.manip <- 0;
    #a <- b <- NA;
    y1997 <- y1998 <- y1999 <- y2000 <- y2001 <- y2002 <- y2003 <- y2004 <- y2005 <- y2006 <- y2007 <- y2008 <- y2009 <- y2010 <- y2011 <- y2012 <- y2013 <- y2014 <- y2015 <- y2016 <- y2017 <- y2018 <- y2019 <- y2020 <- y2021 <- y2022 <- NA;
})

##############################
## municipio changes ignored #
##############################
sel <- which(eq2$action3 == "mun.chg")
length(sel) # is zero, so ignore
# action2
sel <- which(eq2$action2 == "mun.chg")
manip <- eq2[sel,]
manip <- within(manip, {
    action2 <- action3; when2 <- when3; orig.dest2 <- orig.dest3;
    action3 <- "";      when3 <- NA;    orig.dest3 <- "";
})
eq2[sel,] <- manip
# action
sel <- which(eq2$action == "mun.chg")
manip <- eq2[sel,]
manip <- within(manip, {
    action  <- action2; when  <- when2; orig.dest  <- orig.dest2;
    action2 <- action3; when2 <- when3; orig.dest2 <- orig.dest3;
    action3 <- "";      when3 <- NA;    orig.dest3 <- "";
})
eq2[sel,] <- manip

# eq3 will receive manipulation indications and generate NAs needed in time series projections
eq3 <- eq2
eq3 <- eq3[, c("edosecn","action","orig.dest","when","action2","orig.dest2","when2","action3","orig.dest3","when3",
               paste0("y", 1997:2022))]
eq3[,paste0("y", 1997:2022)] <- 1

## # DROP
## # secciones that cannot be fixed for census data projection
## #no.manip <- c(10487) # eg. 10487 created in 2004 with bits of three secciones (mun limits), then merged to 10378 in 2008
## sel <- which(eq2$edosecn==10487);
## manip <- eq2[sel,]
## # given no way to get change, assign 2005 pop=31 along seccion's lifespan 2004:2008 and return to data
## manip <- within(manip, {
##     y2004 <- y2005 <- y2006 <- y2007 <- y2008 <- cen_2005; 
## })
## eq2[sel,] <- manip
## # indicate manip
## manip <- eq3[sel,]
## manip <- within(manip, {
##     y1997 <- y1998 <- y1999 <- y2000 <- y2001 <- y2002 <- y2003 <- NA;
##     y2009 <- y2010 <- y2011 <- y2012 <- y2013 <- y2014 <- y2015 <- y2016 <- y2017 <- y2018 <- y2019 <- NA;
##     y2020 <- y2021 <- y2022 <- NA
##     action  <- action2 <- action3 <- "";
##     orig.dest  <- orig.dest2 <- orig.dest3 <- "";
##     when  <- when2 <- when3 <- NA;
## })
## eq3[sel,] <- manip

tmp.proj <- function(baseyr = NA, yr = NA){
    selc <- grep(pattern = paste0("y", baseyr), colnames(manip))
    base <- manip[,selc];
    pi <- manip$pi;
    #
    ## # exp version
    ## if ((baseyr - yr) >= 0) tmpy <- base / pi ^ (abs(baseyr - yr)) # backward
    ## if ((baseyr - yr) <  0) tmpy <- base * pi ^ (abs(baseyr - yr)) # forwward
    # linear version
    if ((baseyr - yr) >= 0) tmpy <- base - pi * (abs(baseyr - yr)) # backward
    if ((baseyr - yr) <  0) tmpy <- base + pi * (abs(baseyr - yr)) # forward
    tmpy <- round(tmpy, 1)
    return(tmpy)
    }

############################################################
## manipulate all --- will gen NAs when data missing in   ##
## new secciones and some errors that will be fixed later ##
############################################################
manip <- eq2 # duplicate for manipulation
manip <- within(manip, {y2005 <- cen_2005; y2010 <- cen_2010; y2020 <- cen_2020;})     
#
# project 2005-2010
#manip <- within(manip, pi <-  (y2010 / y2005) ^ (1 / (2010 - 2005)))
manip <- within(manip, pi <-  (y2010 - y2005) / (2010 - 2005))
manip <- within(manip, {
    y1997 <- tmp.proj(baseyr = 2005, yr = 1997);
    y1998 <- tmp.proj(baseyr = 2005, yr = 1998);
    y1999 <- tmp.proj(baseyr = 2005, yr = 1999);
    y2000 <- tmp.proj(baseyr = 2005, yr = 2000);
    y2001 <- tmp.proj(baseyr = 2005, yr = 2001);
    y2002 <- tmp.proj(baseyr = 2005, yr = 2002);
    y2003 <- tmp.proj(baseyr = 2005, yr = 2003);
    y2004 <- tmp.proj(baseyr = 2005, yr = 2004);
    y2006 <- tmp.proj(baseyr = 2005, yr = 2006);
    y2007 <- tmp.proj(baseyr = 2005, yr = 2007);
    y2008 <- tmp.proj(baseyr = 2005, yr = 2008);
    y2009 <- tmp.proj(baseyr = 2005, yr = 2009);
})
# project 2010-2020
manip <- within(manip, pi <-  (y2020 - y2010) / (2020 - 2010) );
manip <- within(manip, {
        y2011 <- tmp.proj(baseyr = 2010, yr = 2011);
        y2012 <- tmp.proj(baseyr = 2010, yr = 2012);
        y2013 <- tmp.proj(baseyr = 2010, yr = 2013);
        y2014 <- tmp.proj(baseyr = 2010, yr = 2014);
        y2015 <- tmp.proj(baseyr = 2010, yr = 2015);
        y2016 <- tmp.proj(baseyr = 2010, yr = 2016);
        y2017 <- tmp.proj(baseyr = 2010, yr = 2017);
        y2018 <- tmp.proj(baseyr = 2010, yr = 2018);
        y2019 <- tmp.proj(baseyr = 2010, yr = 2019);
        y2021 <- tmp.proj(baseyr = 2010, yr = 2021);
        y2022 <- tmp.proj(baseyr = 2010, yr = 2022);
})
manip <- within(manip, times.manip <- times.manip + 1)
#
# return manipulated data
eq2 <- manip
##################################################
## secciones that never changed: indicate manip ##
##################################################
sel <- which(is.na(manip$when))
length(sel)
manip <- eq3
manip[sel,] <- within(manip[sel,], {
    action  <- action2; when  <- when2; orig.dest  <- orig.dest2;
    action2 <- action3; when2 <- when3; orig.dest2 <- orig.dest3;
    action3 <- "";      when3 <- NA;    orig.dest3 <- "";
})
# return manipulated data
eq3 <- manip
rm(manip)

###############################################################################
## secciones modified up to 1997 will be ignored (analysis to start in 1997) ##
###############################################################################
sel <- which(eq3$when3 <= 1997)
length(sel) # is zero, ignore
sel <- which(eq3$when2 <= 1997)
length(sel) # is zero, ignore
sel <- which(eq3$when <= 1997)
manip <- eq3[sel,]
manip <- within(manip, {
    action  <- action2; when  <- when2; orig.dest  <- orig.dest2;
    action2 <- action3; when2 <- when3; orig.dest2 <- orig.dest3;
    action3 <- "";      when3 <- NA;    orig.dest3 <- "";
})
eq3[sel,] <- manip
# do this in eq2 too
manip <- eq2[sel,]
manip <- within(manip, {
    action  <- action2; when  <- when2; orig.dest  <- orig.dest2;
    action2 <- action3; when2 <- when3; orig.dest2 <- orig.dest3;
    action3 <- "";      when3 <- NA;    orig.dest3 <- "";
})
eq2[sel,] <- manip


################################
## handle special cases first ##
################################
# fix 10487
## 2003  2004 2005  2008  2010
##  366   366  366   366
##  408   408  408   408
##  459   459  459   459
##      \ 487  487---378
#no.manip <- c(10487) # eg. 10487 created in 2004 with bits of three secciones (mun limits), then merged to 10378 in 2008
sel <- which(eq2$edosecn %in% c(10366,10378,10408,10459,10487))
manip <- eq2[sel,]
# aggregate split populations
manip$cen_2005b <- sum(eq2$cen_2005[sel], na.rm = TRUE)
manip$cen_2010b <- sum(eq2$cen_2010[sel], na.rm = TRUE)
manip <- within(manip, pi <-  (cen_2010b - cen_2005b) / (2010 - 2005) ); # rate of change
# return to data for piecemeal manip
eq2[sel,] <- manip
#
# manip 487
manip <- eq2[sel[5],]
# project 2004-2007
manip <- within(manip, y2004 <- tmp.proj(baseyr = 2005, yr = 2004))
manip <- within(manip, y2005 <- tmp.proj(baseyr = 2005, yr = 2005))
manip <- within(manip, y2006 <- tmp.proj(baseyr = 2005, yr = 2006))
manip <- within(manip, y2007 <- tmp.proj(baseyr = 2005, yr = 2007))
tmp <- manip$y2005 # keep to subtract below
# return to data and indicate manipulation
manip <- within(manip, times.manip <- times.manip + 1)
eq2[sel[5],] <- manip
manip <- eq3[sel[5],]
manip <- within(manip, {
    action  <- "";      when  <- NA;    orig.dest  <- "";
    action2 <- "";      when2 <- NA;    orig.dest2 <- "";
    action3 <- "";      when3 <- NA;    orig.dest3 <- "";
})
eq3[sel[5],] <- manip
# manip 366 408 459: since split seccion has pop2005=31, will leave these three untouched
## manip <- eq2[sel[c(1,3,4)],]
## manip$y2005 <- manip$y2005 + tmp/3 # add split seccion by thirds
## # re-project
## manip <- within(manip, y1997 <- tmp.proj(baseyr = 2005, yr = 1997))
## manip <- within(manip, y1998 <- tmp.proj(baseyr = 2005, yr = 1998))
## manip <- within(manip, y1999 <- tmp.proj(baseyr = 2005, yr = 1999))
## manip <- within(manip, y2000 <- tmp.proj(baseyr = 2005, yr = 2000))
## manip <- within(manip, y2001 <- tmp.proj(baseyr = 2005, yr = 2001))
## manip <- within(manip, y2002 <- tmp.proj(baseyr = 2005, yr = 2002))
## manip <- within(manip, y2003 <- tmp.proj(baseyr = 2005, yr = 2003))
## manip <- within(manip, y2005 <- cen_2005)
## # return to data
## manip <- within(manip, times.manip <- times.manip + 1)
## eq2[sel[c(1,3,4)],] <- manip
# indicate manipulation
manip <- eq3[sel[c(1,3,4)],]
manip <- within(manip, {
    action  <- "";      when  <- NA;    orig.dest  <- "";
    action2 <- "";      when2 <- NA;    orig.dest2 <- "";
    action3 <- "";      when3 <- NA;    orig.dest3 <- "";
})
eq3[sel[c(1,3,4)],] <- manip
# manip 378
manip <- eq2[sel[2],]
manip$cen_2005b <- manip$y2005 + tmp 
manip <- within(manip, pi <-  (cen_2010 - cen_2005b) / (2010 - 2005) ); # rate of change
## # project 2008-09
## manip <- within(manip, y2008 <- tmp.proj(baseyr = 2010, yr = 2008))
## manip <- within(manip, y2009 <- tmp.proj(baseyr = 2010, yr = 2009))
# project pre-2008
manip <- within(manip, y1997 <- tmp.proj(baseyr = 2005, yr = 1997))
manip <- within(manip, y1998 <- tmp.proj(baseyr = 2005, yr = 1998))
manip <- within(manip, y1999 <- tmp.proj(baseyr = 2005, yr = 1999))
manip <- within(manip, y2000 <- tmp.proj(baseyr = 2005, yr = 2000))
manip <- within(manip, y2001 <- tmp.proj(baseyr = 2005, yr = 2001))
manip <- within(manip, y2002 <- tmp.proj(baseyr = 2005, yr = 2002))
manip <- within(manip, y2003 <- tmp.proj(baseyr = 2005, yr = 2003))
manip <- within(manip, y2004 <- tmp.proj(baseyr = 2005, yr = 2004))
manip <- within(manip, y2005 <- tmp.proj(baseyr = 2005, yr = 2005))
manip <- within(manip, y2006 <- tmp.proj(baseyr = 2005, yr = 2006))
manip <- within(manip, y2007 <- tmp.proj(baseyr = 2005, yr = 2007))
# indicate and return to data
manip <- within(manip, times.manip <- times.manip + 1)
eq2[sel[2],] <- manip
manip <- eq3[sel[2],]
manip <- within(manip, {
    action  <- "";      when  <- NA;    orig.dest  <- "";
    action2 <- "";      when2 <- NA;    orig.dest2 <- "";
    action3 <- "";      when3 <- NA;    orig.dest3 <- "";
})
eq3[sel[2],] <- manip

#
# bits of 19 852&2333 split to create 2727:2731 --- when 3
sel <- which(eq2$edosecn %in% c(190852, 192333, 192727:192731)); # ignoro 190852, chica y cuya pob cambió poco
manip <- eq2[sel,]
# aggregate split populations
manip$cen_2010b <- sum(eq2$cen_2010[sel[1:2]])
manip$cen_2020b <- sum(eq2$cen_2020[sel])
#manip <- within(manip, {y2005 <- cen_2005b; y2010 <- cen_2010b;}) # use aggregates to compute pi
manip <- within(manip, pi <-  (cen_2020b - cen_2010b) / (2020 - 2010) ); # rate of change
# re-project full pop 2011:2016
manip <- within(manip, y2011 <- tmp.proj(baseyr = 2010, yr = 2011))
manip <- within(manip, y2012 <- tmp.proj(baseyr = 2010, yr = 2012))
manip <- within(manip, y2013 <- tmp.proj(baseyr = 2010, yr = 2013))
manip <- within(manip, y2014 <- tmp.proj(baseyr = 2010, yr = 2014))
manip <- within(manip, y2015 <- tmp.proj(baseyr = 2010, yr = 2015))
manip <- within(manip, y2016 <- tmp.proj(baseyr = 2010, yr = 2016))
#
manip <- within(manip, y2017 <- tmp.proj(baseyr = 2020, yr = 2017))
manip <- within(manip, y2018 <- tmp.proj(baseyr = 2020, yr = 2018))
manip <- within(manip, y2019 <- tmp.proj(baseyr = 2020, yr = 2019))
manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
# indicate pt1 and return to data
manip <- within(manip, times.manip <- times.manip + 1)
eq2[sel,] <- manip
# indicate pt2 (including 190852)
sel <- which(eq2$edosecn %in% c(190852,192333,192727:192731));
manip <- eq3[sel,]
tmp <- manip[2, c("action","orig.dest","when")]; # retain 2009 change in 2333 to paste below
manip <- within(manip, {
    action  <- "";      when  <- NA;    orig.dest  <- "";
    action2 <- "";      when2 <- NA;    orig.dest2 <- "";
    action3 <- "";      when3 <- NA;    orig.dest3 <- "";
})
manip[-1:-2,] <- within(manip[-1:-2,], {
    y1997 <- y1998 <- y1999 <- NA;
    y2000 <- y2001 <- y2002 <- y2003 <- y2004 <- y2005 <- y2006 <- y2007 <- y2008 <- y2009 <- NA;
    y2010 <- y2011 <- y2012 <- y2013 <- y2014 <- y2015 <- y2016 <- NA;
})
manip[2, c("action","orig.dest","when")] <- tmp # restore 2009 change
eq3[sel,] <- manip
#
# bits of 19 568&588 split to create 2725
sel <- which(eq2$edosecn %in% c(190568,190588,192725));
manip <- eq2[sel,]
# aggregate split populations
manip$cen_2010b <- sum(eq2$cen_2010[sel[-3]])
manip$cen_2020b <- sum(eq2$cen_2020[sel])
#manip <- within(manip, {y2005 <- cen_2005b; y2010 <- cen_2010b;}) # use aggregates to compute pi
manip <- within(manip, pi <-  (cen_2020b - cen_2010b) / (2020 - 2010) ); # rate of change
# re-project full pop up to 2001
manip <- within(manip, y2011 <- tmp.proj(baseyr = 2010, yr = 2011))
manip <- within(manip, y2012 <- tmp.proj(baseyr = 2010, yr = 2012))
manip <- within(manip, y2013 <- tmp.proj(baseyr = 2010, yr = 2013))
manip <- within(manip, y2014 <- tmp.proj(baseyr = 2010, yr = 2014))
manip <- within(manip, y2015 <- tmp.proj(baseyr = 2010, yr = 2015))
manip <- within(manip, y2016 <- tmp.proj(baseyr = 2010, yr = 2016))
#
manip <- within(manip, y2017 <- tmp.proj(baseyr = 2020, yr = 2017))
manip <- within(manip, y2018 <- tmp.proj(baseyr = 2020, yr = 2018))
manip <- within(manip, y2019 <- tmp.proj(baseyr = 2020, yr = 2019))
manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
# indicate pt1 and return to data
manip <- within(manip, times.manip <- times.manip + 1)
eq2[sel,] <- manip
# indicate pt2
manip <- eq3[sel,]
manip <- within(manip, {
    action  <- "";      when  <- NA;    orig.dest  <- "";
    action2 <- "";      when2 <- NA;    orig.dest2 <- "";
    action3 <- "";      when3 <- NA;    orig.dest3 <- "";
})
eq3[sel,] <- manip
#
# bits of 19 620&647 split to create 2732
sel <- which(eq2$edosecn %in% c(190620,190647,192732));
manip <- eq2[sel,]
# aggregate split populations
manip$cen_2010b <- sum(eq2$cen_2010[sel[-3]])
manip$cen_2020b <- sum(eq2$cen_2020[sel])
#manip <- within(manip, {y2005 <- cen_2005b; y2010 <- cen_2010b;}) # use aggregates to compute pi
manip <- within(manip, pi <-  (cen_2020b - cen_2010b) / (2020 - 2010) ); # rate of change
# re-project full pop up to 2001
manip <- within(manip, y2011 <- tmp.proj(baseyr = 2010, yr = 2011))
manip <- within(manip, y2012 <- tmp.proj(baseyr = 2010, yr = 2012))
manip <- within(manip, y2013 <- tmp.proj(baseyr = 2010, yr = 2013))
manip <- within(manip, y2014 <- tmp.proj(baseyr = 2010, yr = 2014))
manip <- within(manip, y2015 <- tmp.proj(baseyr = 2010, yr = 2015))
manip <- within(manip, y2016 <- tmp.proj(baseyr = 2010, yr = 2016))
#
manip <- within(manip, y2017 <- tmp.proj(baseyr = 2020, yr = 2017))
manip <- within(manip, y2018 <- tmp.proj(baseyr = 2020, yr = 2018))
manip <- within(manip, y2019 <- tmp.proj(baseyr = 2020, yr = 2019))
manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
# indicate pt1 and return to data
manip <- within(manip, times.manip <- times.manip + 1)
eq2[sel,] <- manip
# indicate pt2
manip <- eq3[sel,]
manip <- within(manip, {
    action  <- "";      when  <- NA;    orig.dest  <- "";
    action2 <- "";      when2 <- NA;    orig.dest2 <- "";
    action3 <- "";      when3 <- NA;    orig.dest3 <- "";
})
eq3[sel,] <- manip
#
# bits of 20 1726&1728 split to create 2455
sel <- which(eq2$edosecn %in% c(201726,201728,202455));
manip <- eq2[sel,]
# aggregate split populations
manip$cen_2010b <- sum(eq2$cen_2010[sel[-3]])
manip$cen_2020b <- sum(eq2$cen_2020[sel])
#manip <- within(manip, {y2005 <- cen_2005b; y2010 <- cen_2010b;}) # use aggregates to compute pi
manip <- within(manip, pi <-  (cen_2020b - cen_2010b) / (2020 - 2010) ); # rate of change
# re-project full pop up to 2001
manip <- within(manip, y2011 <- tmp.proj(baseyr = 2010, yr = 2011))
manip <- within(manip, y2012 <- tmp.proj(baseyr = 2010, yr = 2012))
manip <- within(manip, y2013 <- tmp.proj(baseyr = 2010, yr = 2013))
manip <- within(manip, y2014 <- tmp.proj(baseyr = 2010, yr = 2014))
manip <- within(manip, y2015 <- tmp.proj(baseyr = 2010, yr = 2015))
manip <- within(manip, y2016 <- tmp.proj(baseyr = 2010, yr = 2016))
#
manip <- within(manip, y2017 <- tmp.proj(baseyr = 2020, yr = 2017))
manip <- within(manip, y2018 <- tmp.proj(baseyr = 2020, yr = 2018))
manip <- within(manip, y2019 <- tmp.proj(baseyr = 2020, yr = 2019))
manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
# indicate pt1 and return to data
manip <- within(manip, times.manip <- times.manip + 1)
eq2[sel,] <- manip
# indicate pt2
manip <- eq3[sel,]
manip <- within(manip, {
    action  <- "";      when  <- NA;    orig.dest  <- "";
    action2 <- "";      when2 <- NA;    orig.dest2 <- "";
    action3 <- "";      when3 <- NA;    orig.dest3 <- "";
})
eq3[sel,] <- manip
#
# bits of 23 172&175&180 split to create 964
sel <- which(eq2$edosecn %in% c(230172,230175,230180,230964));
manip <- eq2[sel,]
# aggregate split populations
manip$cen_2010b <- sum(eq2$cen_2010[sel[-4]])
manip$cen_2020b <- sum(eq2$cen_2020[sel])
#manip <- within(manip, {y2005 <- cen_2005b; y2010 <- cen_2010b;}) # use aggregates to compute pi
manip <- within(manip, pi <-  (cen_2020b - cen_2010b) / (2020 - 2010) ); # rate of change
# re-project full pop up to 2001
manip <- within(manip, y2011 <- tmp.proj(baseyr = 2010, yr = 2011))
manip <- within(manip, y2012 <- tmp.proj(baseyr = 2010, yr = 2012))
manip <- within(manip, y2013 <- tmp.proj(baseyr = 2010, yr = 2013))
manip <- within(manip, y2014 <- tmp.proj(baseyr = 2010, yr = 2014))
manip <- within(manip, y2015 <- tmp.proj(baseyr = 2010, yr = 2015))
manip <- within(manip, y2016 <- tmp.proj(baseyr = 2010, yr = 2016))
#
manip <- within(manip, y2017 <- tmp.proj(baseyr = 2020, yr = 2017))
manip <- within(manip, y2018 <- tmp.proj(baseyr = 2020, yr = 2018))
manip <- within(manip, y2019 <- tmp.proj(baseyr = 2020, yr = 2019))
manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
# indicate pt1 and return to data
manip <- within(manip, times.manip <- times.manip + 1)
eq2[sel,] <- manip
# indicate pt2
manip <- eq3[sel,]
manip <- within(manip, {
    action  <- "";      when  <- NA;    orig.dest  <- "";
    action2 <- "";      when2 <- NA;    orig.dest2 <- "";
    action3 <- "";      when3 <- NA;    orig.dest3 <- "";
})
eq3[sel,] <- manip

# bits of 23 290&295 split to create 971&973
## 290 -> 290 295 973
## 295 -> 295 969 971
sel <- which(eq2$edosecn %in% c(230290,230295,230969,230971,230973));
manip <- eq2[sel,]
# aggregate split populations
manip$cen_2010b <- sum(eq2$cen_2010[sel], na.rm = TRUE)
manip$cen_2020b <- sum(eq2$cen_2020[sel], na.rm = TRUE)
#manip <- within(manip, {y2005 <- cen_2005b; y2010 <- cen_2010b;}) # use aggregates to compute pi
manip <- within(manip, pi <-  (cen_2020b - cen_2010b) / (2020 - 2010) ); # rate of change
# re-project 2011:17
manip <- within(manip, y2011 <- tmp.proj(baseyr = 2010, yr = 2011))
manip <- within(manip, y2012 <- tmp.proj(baseyr = 2010, yr = 2012))
manip <- within(manip, y2013 <- tmp.proj(baseyr = 2010, yr = 2013))
manip <- within(manip, y2014 <- tmp.proj(baseyr = 2010, yr = 2014))
manip <- within(manip, y2015 <- tmp.proj(baseyr = 2010, yr = 2015))
manip <- within(manip, y2016 <- tmp.proj(baseyr = 2010, yr = 2016))
# re-project 2017-on
manip$y2020[3] <- 1 # 969 not in censo 2020?
manip <- within(manip, y2017 <- tmp.proj(baseyr = 2020, yr = 2017))
manip <- within(manip, y2018 <- tmp.proj(baseyr = 2020, yr = 2018))
manip <- within(manip, y2019 <- tmp.proj(baseyr = 2020, yr = 2019))
manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
manip[3,]
# indicate and return to data
manip <- within(manip, times.manip <- times.manip + 1)
eq2[sel,] <- manip
manip <- eq3[sel,]
manip <- within(manip, {
    action  <- "";      when  <- NA;    orig.dest  <- "";
    action2 <- "";      when2 <- NA;    orig.dest2 <- "";
    action3 <- "";      when3 <- NA;    orig.dest3 <- "";
})
eq3[sel,] <- manip


############################
## cases with when3 record #
############################
## sel2 <- which(!is.na(eq3$when3)) # three cases only
## eq2$edosecn[sel2]
## eq2[sel2[1],]
# hand manipulation
sel <- which(eq2$edosecn==161976)
manip <- eq2[sel,]
vec <- c(1976,2675,2677)
vec <- manip$edon*10000 + vec   # turn into edosecn
manip$case <- "no.baja"
vec <- which(eq2$edosecn %in% vec) # turn to indices
vec.minus <- which(eq2$edosecn==162675) # targets w/o seccion (for use when seccion was dropped)
# aggregate split populations
manip$cen_2005b <- sum(eq2$cen_2005[vec])
manip$cen_2010b <- sum(eq2$cen_2010[vec]) # needed for backwards projection
manip <- within(manip, {y2005 <- cen_2005b; y2010 <- cen_2010b; y2020 <- cen_2020;}) # use aggregates to compute pi
manip <- within(manip, pi <-  (y2010 - y2005) / (2010 - 2005) ); # rate of change
# re-project where needed (targets unneeded before 2005)
if (manip$when>1997) manip <- within(manip, y1997 <- tmp.proj(baseyr = 2005, yr = 1997))
# return manipulation to data
manip$times.manip <- manip$times.manip + 1
eq2[sel,] <- manip
# fix proj in non-existing secciones and return manipulation to data
manip <- eq3[vec.minus,]
if (manip$when>1997) manip <- within(manip, y1997 <- NA)
manip <- within(manip, {
    action  <- "";      when <- NA;    orig.dest <- "";
})
eq3[vec.minus,] <- manip
#
# do when2 change
sel <- which(eq3$edosecn==161976)
manip <- eq2[sel,]
vec <- c(1976,2677)
vec <- manip$edon*10000 + vec   # turn into edosecn
manip$case <- "no.baja"
vec <- which(eq2$edosecn %in% vec) # turn to indices
vec.minus <- which(eq2$edosecn==162677)
# aggregate split populations
manip$cen_2005b <- sum(eq2$cen_2005[vec])
manip$cen_2010b <- sum(eq2$cen_2010[vec]) # needed for backwards projection
manip <- within(manip, {y2005 <- cen_2005b; y2010 <- cen_2010b; y2020 <- cen_2020;}) # use aggregates to compute pi
manip <- within(manip, pi <-  (y2010 - y2005) / (2010 - 2005) ); # rate of change
# re-project where needed (targets unneeded before 2005)
if (manip$when2>1998) manip <- within(manip, y1998 <- tmp.proj(baseyr = 2005, yr = 1998))
if (manip$when2>1999) manip <- within(manip, y1999 <- tmp.proj(baseyr = 2005, yr = 1999))
if (manip$when2>2000) manip <- within(manip, y2000 <- tmp.proj(baseyr = 2005, yr = 2000))
if (manip$when2>2001) manip <- within(manip, y2001 <- tmp.proj(baseyr = 2005, yr = 2001))
# restore un-manipulated 2005 in cases where when is below
if (manip$when<2005) manip <- within(manip, y2005 <- cen_2005)
# indicate manip and return to data
manip <- within(manip, {
    times.manip <- times.manip + 1;
})
eq2[sel,] <- manip
# further indicate manip and return to data
manip <- eq3[sel,]
manip <- within(manip, {
    action  <- action3; when  <- when3; orig.dest <- orig.dest3;
    action2 <- "";      when2 <- NA;    orig.dest2 <- "";
    action3 <- "";      when3 <- NA;    orig.dest3 <- "";
})
eq3[sel,] <- manip
# fix proj in non-existing secciones and return manipulation to data
manip <- eq3[vec.minus,]
if (manip$when>1997) manip <- within(manip, y1997 <- NA)
if (manip$when>1998) manip <- within(manip, y1998 <- NA)
if (manip$when>1999) manip <- within(manip, y1999 <- NA)
if (manip$when>2000) manip <- within(manip, y2000 <- NA)
if (manip$when>2001) manip <- within(manip, y2001 <- NA)
manip <- within(manip, {
    action  <- "";      when <- NA;    orig.dest <- "";
})
eq3[vec.minus,] <- manip
#
# item 2
# get pi
sel <- which(eq2$edosecn==192136)
manip <- eq2[sel,]
vec <- c(2136,2437,2809:2822)
vec <- manip$edon*10000 + vec   # turn into edosecn
manip$case <- "baja"
vec <- which(eq2$edosecn %in% vec) # turn to indices
vec <- vec[-1]
# aggregate split populations
manip$cen_2020b <- sum(eq2$cen_2020[vec]) # needed for backwards projection
manip <- within(manip, {y2020 <- cen_2020b;}) # use aggregates to compute pi
manip <- within(manip, pi <-  (y2020 - y2010) / (2020 - 2010) ); # rate of change
## # re-project where needed (targets unneeded before 2005)
## manip <- within(manip, y2011 <- tmp.proj(baseyr = 2020, yr = 2011))
## manip <- within(manip, y2012 <- tmp.proj(baseyr = 2020, yr = 2012))
## manip <- within(manip, y2013 <- tmp.proj(baseyr = 2020, yr = 2013))
## manip <- within(manip, y2014 <- tmp.proj(baseyr = 2020, yr = 2014))
## manip <- within(manip, y2015 <- tmp.proj(baseyr = 2020, yr = 2015))
## manip <- within(manip, y2016 <- tmp.proj(baseyr = 2020, yr = 2016))
## manip <- within(manip, y2017 <- tmp.proj(baseyr = 2020, yr = 2017))
## manip <- within(manip, y2018 <- tmp.proj(baseyr = 2020, yr = 2018))
tmp <- manip$pi # keep pi for orig.dest3
#manip <- within(manip, {y2020 <- when3 <- NA; action3 <- orig.dest3 <- ""}) # clean
# return manipulation to data
## manip$times.manip <- manip$times.manip + 1
eq2[sel,] <- manip
# fix proj in non-existing secciones and return manipulation to data
manip <- eq2[vec,]
manip$pi <- tmp
manip <- within(manip, y2019 <- tmp.proj(baseyr = 2020, yr = 2019))
manip <- within(manip, y2020 <- tmp.proj(baseyr = 2020, yr = 2020))
manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
manip <- within(manip, {
    times.manip <- times.manip + 1;
})
eq2[vec,] <- manip
# indicate manip
manip <- eq3[vec,]
manip <- within(manip, {
    action  <- "";      when <- NA;    orig.dest <- "";
})
eq3[vec,] <- manip
# fix sección 2437 created in 2011 and return to data
manip <- eq2[vec[1],]
manip <- within(manip, y2011 <- tmp.proj(baseyr = 2020, yr = 2011))
manip <- within(manip, y2012 <- tmp.proj(baseyr = 2020, yr = 2012))
manip <- within(manip, y2013 <- tmp.proj(baseyr = 2020, yr = 2013))
manip <- within(manip, y2014 <- tmp.proj(baseyr = 2020, yr = 2014))
manip <- within(manip, y2015 <- tmp.proj(baseyr = 2020, yr = 2015))
manip <- within(manip, y2016 <- tmp.proj(baseyr = 2020, yr = 2016))
manip <- within(manip, y2017 <- tmp.proj(baseyr = 2020, yr = 2017))
manip <- within(manip, y2018 <- tmp.proj(baseyr = 2020, yr = 2018))
tmp <- manip$y2010 # retain 2010 pop
eq2[vec,] <- manip
# inidicate manip
manip <- eq3[vec,]
manip <- within(manip, y2010 <- NA)
eq3[vec,] <- manip
# fix sección 2136 and return to data
sel <- which(eq2$edosecn==192136)
manip <- eq2[sel,]
manip$y2010 <- manip$y2010 - tmp # subtract split sección 2437 pop in 2010 
# re-project where needed
manip <- within(manip, y2011 <- tmp.proj(baseyr = 2010, yr = 2011))
manip <- within(manip, y2012 <- tmp.proj(baseyr = 2010, yr = 2012))
manip <- within(manip, y2013 <- tmp.proj(baseyr = 2010, yr = 2013))
manip <- within(manip, y2014 <- tmp.proj(baseyr = 2010, yr = 2014))
manip <- within(manip, y2015 <- tmp.proj(baseyr = 2010, yr = 2015))
manip <- within(manip, y2016 <- tmp.proj(baseyr = 2010, yr = 2016))
manip <- within(manip, y2017 <- tmp.proj(baseyr = 2010, yr = 2017))
manip <- within(manip, y2018 <- tmp.proj(baseyr = 2010, yr = 2018))
# return manipulation to data
manip$y2010 <- manip$cen_2010 # restore pre-split pop in 2010
manip$times.manip <- manip$times.manip + 1
eq2[sel,] <- manip
# indicate manip
manip <- eq3[sel,]
manip <- within(manip, {y2020 <- when3 <- when2 <- NA; action3 <- orig.dest3 <- action2 <- orig.dest2 <- ""}) # clean
eq3[sel,] <- manip

# item 3
#sel <- c(205,875,970,975:1003)
sel <- c(205,874,875,876:922,965,967,970,975:1003)
sel <- 230000 + sel
sel <- which(eq2$edosecn %in% sel)
manip <- eq2[sel,]
## manip$cen_2020 # 970 vanished in 2020?
## manip$edosecn  # 970 vanished in 2020?
vec <- sel
# aggregate split populations
manip$cen_2010b <- sum(eq2$cen_2010[sel], na.rm = TRUE)
manip$cen_2020b <- sum(eq2$cen_2020[sel], na.rm = TRUE)
manip <- within(manip, pi <-  (cen_2020b - cen_2010b) / (2020 - 2010) ); # rate of change
# return to data for piecemeal manipulation
eq2[sel,] <- manip
# fix 205 and return to data
manip <- eq2[sel[1],]
manip <- within(manip, y2011 <- tmp.proj(baseyr = 2010, yr = 2011))
manip <- within(manip, times.manip <- times.manip + 1)
eq2[sel[1],] <- manip
# fix 874 2012:16
manip <- eq2[sel[2],]
manip$y2010 <- 10718 # use lisnom 2012 to infer pocen
manip <- within(manip, y2011 <- tmp.proj(baseyr = 2010, yr = 2011))
manip <- within(manip, y2012 <- tmp.proj(baseyr = 2010, yr = 2012))
manip <- within(manip, y2013 <- tmp.proj(baseyr = 2010, yr = 2013))
manip <- within(manip, y2014 <- tmp.proj(baseyr = 2010, yr = 2014))
manip <- within(manip, {
    y2016 <- y2014; 
    y2015 <- y2013; 
    y2014 <- y2012; 
    y2013 <- y2011; 
    y2012 <- y2010;
    y2010 <- y2011 <- NA;
}) # move two years up
# fix 874 2017:21 and return to data
manip <- within(manip, y2017 <- tmp.proj(baseyr = 2020, yr = 2017))
manip <- within(manip, y2018 <- tmp.proj(baseyr = 2020, yr = 2018))
manip <- within(manip, y2019 <- tmp.proj(baseyr = 2020, yr = 2019))
manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
manip <- within(manip, times.manip <- times.manip + 1)
eq2[sel[2],] <- manip
#
# fix 875 2012:16
manip <- eq2[sel[3],]
manip$y2010 <- 19223 # use lisnom 2012 to infer pocen
manip <- within(manip, y2011 <- tmp.proj(baseyr = 2010, yr = 2011))
manip <- within(manip, y2012 <- tmp.proj(baseyr = 2010, yr = 2012))
manip <- within(manip, y2013 <- tmp.proj(baseyr = 2010, yr = 2013))
manip <- within(manip, y2014 <- tmp.proj(baseyr = 2010, yr = 2014))
manip <- within(manip, {
    y2016 <- y2014; 
    y2015 <- y2013; 
    y2014 <- y2012; 
    y2013 <- y2011; 
    y2012 <- y2010;
    y2010 <- y2011 <- NA;
}) # move two years up
# fix 875 2017:19 and return to data
vec <- c(230975:231003)
vec <- which(eq2$edosecn %in% vec)
manip$y2020 <- sum(eq2$cen_2020[vec], na.rm = TRUE) # agg 2020 pop
manip <- within(manip, y2017 <- tmp.proj(baseyr = 2020, yr = 2017))
manip <- within(manip, y2018 <- tmp.proj(baseyr = 2020, yr = 2018))
manip <- within(manip, y2019 <- tmp.proj(baseyr = 2020, yr = 2019))
manip$y2020 <- NA
manip <- within(manip, times.manip <- times.manip + 1)
eq2[sel[2],] <- manip
#
# fix 876:922 2012:on and return to data
vec <- c(230876:230922)
vec <- which(eq2$edosecn %in% vec)
manip <- eq2[vec,]
manip <- within(manip, y2012 <- tmp.proj(baseyr = 2020, yr = 2012))
manip <- within(manip, y2013 <- tmp.proj(baseyr = 2020, yr = 2013))
manip <- within(manip, y2014 <- tmp.proj(baseyr = 2020, yr = 2014))
manip <- within(manip, y2015 <- tmp.proj(baseyr = 2020, yr = 2015))
manip <- within(manip, y2016 <- tmp.proj(baseyr = 2020, yr = 2016))
manip <- within(manip, y2017 <- tmp.proj(baseyr = 2020, yr = 2017))
manip <- within(manip, y2018 <- tmp.proj(baseyr = 2020, yr = 2018))
manip <- within(manip, y2019 <- tmp.proj(baseyr = 2020, yr = 2019))
manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
manip <- within(manip, times.manip <- times.manip + 1)
eq2[vec,] <- manip
#
# fix 965 967 970 2017:on and return to data
vec <- c(230965,230967,230970)
vec <- which(eq2$edosecn %in% vec)
manip <- eq2[vec,]
manip <- within(manip, y2017 <- tmp.proj(baseyr = 2020, yr = 2017))
manip <- within(manip, y2018 <- tmp.proj(baseyr = 2020, yr = 2018))
manip <- within(manip, y2019 <- tmp.proj(baseyr = 2020, yr = 2019))
manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
manip <- within(manip, times.manip <- times.manip + 1)
eq2[vec,] <- manip
#
# fix 975:1003 2021:on and return to data
vec <- c(230975:231003)
vec <- which(eq2$edosecn %in% vec)
manip <- eq2[vec,]
manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
manip <- within(manip, times.manip <- times.manip + 1)
eq2[vec,] <- manip
#
# indicate manip
manip <- eq3[sel,]
manip <- within(manip, {
    action  <- "";      when  <- NA;    orig.dest  <- "";
    action2 <- "";      when2 <- NA;    orig.dest2 <- "";
    action3 <- "";      when3 <- NA;    orig.dest3 <- "";
})
eq3[sel,] <- manip
#
table(eq3$action3)


############################
## cases with when2 record #
############################
table(eq3$action2)
# items where change1 occurred before 2010 census and change2 before/at 2020 = mode
sel <- which(eq3$action2=="split.to" & eq3$when2>2010 & eq3$when2 - eq3$when >= 10)
# monitor
eq2[sel,c(1,6:11)]
#
for (i in sel){
    #i <- sel[1]
    manip <- eq2[i,] # duplicate obs for manipulation
    vec <- eval(parse(text = manip$orig.dest2)) # https://stackoverflow.com/questions/1743698/evaluate-expression-given-as-a-string
    vec <- manip$edon*10000 + vec   # turn into edosecn
    manip$case <- ifelse(manip$edosecn %in% vec, "no.baja", "baja") # did seccion survive the split? (baja means no) 
    vec <- which(eq2$edosecn %in% vec) # turn to indices
    vec.plus  <- union(vec, i)   # seccion and its targets (in case vec excludes dropped seccion) 
    vec.minus <- setdiff(vec, i) # targets w/o seccion (for use when seccion was dropped)
    # aggregate split populations
    #manip$cen_2010b <- sum(eq2$cen_2010[vec])
    manip$cen_2020b <- sum(eq2$cen_2020[vec]) # needed for backwards projection
    manip <- within(manip, {y2010 <- cen_2010; y2020 <- cen_2020b;}) # use aggregates to compute pi
    manip <- within(manip, pi <-  (y2020 - y2010) / (2020 - 2010) ); # rate of change
    # re-project where needed
    if (manip$when2>2011) manip <- within(manip, y2011 <- tmp.proj(baseyr = 2020, yr = 2011))
    if (manip$when2>2012) manip <- within(manip, y2012 <- tmp.proj(baseyr = 2020, yr = 2012))
    if (manip$when2>2013) manip <- within(manip, y2013 <- tmp.proj(baseyr = 2020, yr = 2013))
    if (manip$when2>2014) manip <- within(manip, y2014 <- tmp.proj(baseyr = 2020, yr = 2014))
    if (manip$when2>2015) manip <- within(manip, y2015 <- tmp.proj(baseyr = 2020, yr = 2015))
    if (manip$when2>2016) manip <- within(manip, y2016 <- tmp.proj(baseyr = 2020, yr = 2016))
    if (manip$when2>2017) manip <- within(manip, y2017 <- tmp.proj(baseyr = 2020, yr = 2017))
    if (manip$when2>2018) manip <- within(manip, y2018 <- tmp.proj(baseyr = 2020, yr = 2018))
    if (manip$when2>2019) manip <- within(manip, y2019 <- tmp.proj(baseyr = 2020, yr = 2019))
    if (manip$when2>2020) manip <- within(manip, y2020 <- tmp.proj(baseyr = 2020, yr = 2020))
    if (manip$when2>2021) manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
    if (manip$when2>2022) manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
    # restore un-manipulated 2020 in cases where when is below
    if (manip$when2<=2020) manip <- within(manip, y2020 <- cen_2020)
    # return manipulation to data
    tmp <- manip$pi # save to plug into split.from
    eq2[i,] <- manip
    # fix proj in non-existing secciones and return manipulation to data
    manip <- eq2[vec.minus,]
    manip <- within(manip, {y2020 <- cen_2020; pi <- tmp}) # plug pi
    if (manip$when[1]<2012) manip <- within(manip, y2011 <- tmp.proj(baseyr = 2020, yr = 2011))
    if (manip$when[1]<2013) manip <- within(manip, y2012 <- tmp.proj(baseyr = 2020, yr = 2012))
    if (manip$when[1]<2014) manip <- within(manip, y2013 <- tmp.proj(baseyr = 2020, yr = 2013))
    if (manip$when[1]<2015) manip <- within(manip, y2014 <- tmp.proj(baseyr = 2020, yr = 2014))
    if (manip$when[1]<2016) manip <- within(manip, y2015 <- tmp.proj(baseyr = 2020, yr = 2015))
    if (manip$when[1]<2017) manip <- within(manip, y2016 <- tmp.proj(baseyr = 2020, yr = 2016))
    if (manip$when[1]<2018) manip <- within(manip, y2017 <- tmp.proj(baseyr = 2020, yr = 2017))
    if (manip$when[1]<2019) manip <- within(manip, y2018 <- tmp.proj(baseyr = 2020, yr = 2018))
    if (manip$when[1]<2020) manip <- within(manip, y2019 <- tmp.proj(baseyr = 2020, yr = 2019))
    if (manip$when[1]<2021) manip <- within(manip, y2020 <- tmp.proj(baseyr = 2020, yr = 2020))
    if (manip$when[1]<2022) manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
    if (manip$when[1]<2023) manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
    # indicate manipulation pt 1 and return to data
    manip <- within(manip, {
        times.manip <- times.manip + 1;
    })
    eq2[vec.minus,] <- manip
    # indicate manipulation pt 2 and return to data
    manip <- eq3[vec.minus,]
    manip <- within(manip, {
        action <- "";      when <- NA;    orig.dest <- "";
    })
    eq3[vec.minus,] <- manip
    # indicate sección split.from and return to data
    manip <- eq3[i,] # duplicate obs for manipulation
    manip <- within(manip, {
        action2 <- "";      when2 <- NA;    orig.dest2 <- "";
    })
    eq3[i,] <- manip
}

# items where change1 occurred before 2005 census and change2 at 2010
sel <- which(eq3$action2=="split.to" & eq3$when2==2010 & eq3$when2 - eq3$when >= 5)
# monitor
eq2[sel,c(1,6:11)]
#
manip <- eq2[sel,] # duplicate obs for manipulation
vec <- eval(parse(text = manip$orig.dest2)) # https://stackoverflow.com/questions/1743698/evaluate-expression-given-as-a-string
vec <- manip$edon*10000 + vec   # turn into edosecn
manip$case <- ifelse(manip$edosecn %in% vec, "no.baja", "baja") # did seccion survive the split? (baja means no) 
vec <- which(eq2$edosecn %in% vec) # turn to indices
vec.plus  <- union(vec, sel)   # seccion and its targets (in case vec excludes dropped seccion) 
vec.minus <- setdiff(vec, sel) # targets w/o seccion (for use when seccion was dropped)
# aggregate split populations
manip$cen_2010b <- sum(eq2$cen_2010[vec])
#manip$cen_2020b <- sum(eq2$cen_2020[vec]) # needed for backwards projection
manip <- within(manip, {y2005 <- cen_2005; y2010 <- cen_2010b;}) # use aggregates to compute pi
manip <- within(manip, pi <-  (y2010 - y2005) / (2010 - 2005) ); # rate of change
# re-project where needed
if (manip$when2>2002) manip <- within(manip, y2002 <- tmp.proj(baseyr = 2010, yr = 2002))
if (manip$when2>2003) manip <- within(manip, y2003 <- tmp.proj(baseyr = 2010, yr = 2003))
if (manip$when2>2004) manip <- within(manip, y2004 <- tmp.proj(baseyr = 2010, yr = 2004))
if (manip$when2>2006) manip <- within(manip, y2006 <- tmp.proj(baseyr = 2010, yr = 2006))
if (manip$when2>2007) manip <- within(manip, y2007 <- tmp.proj(baseyr = 2010, yr = 2007))
if (manip$when2>2008) manip <- within(manip, y2008 <- tmp.proj(baseyr = 2010, yr = 2008))
if (manip$when2>2009) manip <- within(manip, y2009 <- tmp.proj(baseyr = 2010, yr = 2009))
# return manipulation to data
tmp <- manip$pi # save to plug into split.from
manip <- within(manip, {
    times.manip <- times.manip + 1
})
eq2[sel,] <- manip
# non-existing secciones need no further manipulation, indicate done and return to data
manip <- eq3[vec.minus,]
manip <- within(manip, {
    action <- "";      when <- NA;    orig.dest <- "";
})
eq3[vec.minus,] <- manip
#
# get vec to manipulate action1 in seccion 1913
manip <- eq3[sel,] # duplicate obs for manipulation
vec <- eval(parse(text = manip$orig.dest)) # https://stackoverflow.com/questions/1743698/evaluate-expression-given-as-a-string
vec <- 80000 + vec   # turn into edosecn
vec <- which(eq2$edosecn %in% vec) # turn to indices
# indicate previous manip in 2828 and return to data
manip <- within(manip, {
    action  <- "";    when  <- NA;   orig.dest  <- "";
    action2 <- "";    when2 <- NA;   orig.dest2 <- "";
})
eq3[sel,] <- manip # duplicate obs for manipulation --- sel can be reset
# get seccion 1913 for manipulation
sel <- vec # vec can be reset
manip <- eq2[sel,] # duplicate obs for manipulation
manip$pi <- tmp # plug last known pi
vec <- eval(parse(text = manip$orig.dest)) # https://stackoverflow.com/questions/1743698/evaluate-expression-given-as-a-string
vec <- manip$edon*10000 + vec   # turn into edosecn
manip$case <- ifelse(manip$edosecn %in% vec, "no.baja", "baja") # did seccion survive the split? (baja means no) 
vec <- which(eq2$edosecn %in% vec) # turn to indices
vec.plus  <- union(vec, sel)   # seccion and its targets (in case vec excludes dropped seccion) 
vec.minus <- setdiff(vec, sel) # targets w/o seccion (for use when seccion was dropped)
# aggregate split populations
manip$cen_2005b <- sum(eq2$cen_2005[vec])
#manip$cen_2020b <- sum(eq2$cen_2020[vec]) # needed for backwards projection
manip <- within(manip, {y2005 <- cen_2005b;}) # use aggregates to compute pi
# re-project where needed
manip <- within(manip, y1997 <- tmp.proj(baseyr = 2005, yr = 1997))
manip <- within(manip, y1998 <- tmp.proj(baseyr = 2005, yr = 1998))
manip <- within(manip, y1999 <- tmp.proj(baseyr = 2005, yr = 1999))
manip <- within(manip, y2000 <- tmp.proj(baseyr = 2005, yr = 2000))
manip <- within(manip, y2001 <- tmp.proj(baseyr = 2005, yr = 2001))
manip <- within(manip, y2005 <- NA) # 2005 no longer needed
                
# indicate manipulation pt 1 and return manipulation to data
manip <- within(manip, {
    times.manip <- times.manip + 1;
})
eq2[sel,] <- manip # sel can be reset
# indicate manipulation pt 2 and return manipulation to data
manip <- eq3[sel,]
manip <- within(manip, {
    action  <- "";    when  <- NA;   orig.dest  <- "";
    y2002 <- y2003 <- y2004 <- y2005 <- NA;
})
eq3[sel,] <- manip # sel can be reset
# finally, make pre 2002 pop NA in new secciones and indicate manipulation
sel <- vec
manip <- eq3[sel,]
manip <- within(manip, {
    y1997 <- y1998 <- y1999 <- y2000 <- y2001 <- NA;
    action  <- "";    when  <- NA;   orig.dest  <- "";
})
eq3[sel,] <- manip # return to data

# two cases must be treated together 192589,192614
#sel <- c(353,2589,2614,2717,2719)
sel <- c(353,2547:2618,2717,2719)
sel <- 190000 + sel   # turn into edosecn
sel <- which(eq2$edosecn %in% sel) # turn to indices
manip <- eq2[sel,]
# aggregate split populations
manip$cen_2010b <- sum(eq2$cen_2010[sel[1]])
manip$cen_2020b <- sum(eq2$cen_2020[sel[c(-1,-75)]]) # why 2719 not in censo 2020?
manip <- within(manip, pi <-  (cen_2020b - cen_2010b) / (2020 - 2010) ); # rate of change
# return to data for piecemeal manipulation
eq2[sel,] <- manip
# fix 353
manip <- eq2[sel[1],]
# project, indicate, and return to data
manip <- within(manip, y2011 <- tmp.proj(baseyr = 2010, yr = 2011))
manip <- within(manip, times.manip <- times.manip + 1)
eq2[sel[1],] <- manip
manip <- eq3[sel[1],]
manip <- within(manip, {action <- orig.dest <- ""; when <- NA})
manip <- within(manip, {y2012 <- y2013 <- y2014 <- y2015 <- y2016 <- y2017 <- y2018 <- y2019 <- y2020 <- y2021 <- y2022 <- NA})
eq3[sel[1],] <- manip
# fix 2589 and 2614
sel <- which(eq2$edosecn %in% c(192589,192614))
manip <- eq2[sel,]
# project post 2017
manip <- within(manip, y2018 <- tmp.proj(baseyr = 2020, yr = 2018))
manip <- within(manip, y2019 <- tmp.proj(baseyr = 2020, yr = 2019))
manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
# gen pop for 2012-2017
manip$cen_2020b[1] <- manip$y2020[1] #- eq2$y2020[which(eq2$edosecn==192719)] # why 2719 missing fr 2020 census?
manip$y2020[1] <- manip$cen_2020b[1]
manip$cen_2020b[2] <- manip$y2020[2] - eq2$y2020[which(eq2$edosecn==192717)]
manip$y2020[2] <- manip$cen_2020b[2]
# project
manip <- within(manip, y2012 <- tmp.proj(baseyr = 2020, yr = 2012))
manip <- within(manip, y2013 <- tmp.proj(baseyr = 2020, yr = 2013))
manip <- within(manip, y2014 <- tmp.proj(baseyr = 2020, yr = 2014))
manip <- within(manip, y2015 <- tmp.proj(baseyr = 2020, yr = 2015))
manip <- within(manip, y2016 <- tmp.proj(baseyr = 2020, yr = 2016))
manip <- within(manip, y2017 <- tmp.proj(baseyr = 2020, yr = 2017))
# restore pop 2020, indicate manip, and return to data
manip <- within(manip, y2020 <- cen_2020)
manip <- within(manip, times.manip <- times.manip + 1)
eq2[sel,] <- manip
manip <- eq3[sel,]
manip <- within(manip, {
    action  <- orig.dest  <- ""; when  <- NA;
    action2 <- orig.dest2 <- ""; when2 <- NA;
})
manip <- within(manip, {y1997 <- y1998 <- y1999 <- y2000 <- y2001 <- y2002 <- y2003 <- y2004 <- y2005 <- y2006 <- y2007 <- y2008 <- y2009 <- y2010 <- y2011 <- NA})
eq3[sel,] <- manip
# fix largest subset
sel <- which(eq2$edosecn %in% c(192547:192588,192590:192613,192615:192618))
manip <- eq2[sel,]
# project 2012 on
manip <- within(manip, y2012 <- tmp.proj(baseyr = 2020, yr = 2012))
manip <- within(manip, y2013 <- tmp.proj(baseyr = 2020, yr = 2013))
manip <- within(manip, y2014 <- tmp.proj(baseyr = 2020, yr = 2014))
manip <- within(manip, y2015 <- tmp.proj(baseyr = 2020, yr = 2015))
manip <- within(manip, y2016 <- tmp.proj(baseyr = 2020, yr = 2016))
manip <- within(manip, y2017 <- tmp.proj(baseyr = 2020, yr = 2017))
manip <- within(manip, y2018 <- tmp.proj(baseyr = 2020, yr = 2018))
manip <- within(manip, y2019 <- tmp.proj(baseyr = 2020, yr = 2019))
manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
# indicate manip, and return to data
manip <- within(manip, times.manip <- times.manip + 1)
eq2[sel,] <- manip
manip <- eq3[sel,]
manip <- within(manip, {action <- orig.dest <- ""; when <- NA})
manip <- within(manip, {y1997 <- y1998 <- y1999 <- y2000 <- y2001 <- y2002 <- y2003 <- y2004 <- y2005 <- y2006 <- y2007 <- y2008 <- y2009 <- y2010 <- y2011 <- NA})
eq3[sel,] <- manip
# fix final 2 secciones
sel <- which(eq2$edosecn %in% c(192717,192719)) # why 2719 missing in 2020 census?
manip <- eq2[sel,]
manip <- within(manip, y2018 <- tmp.proj(baseyr = 2020, yr = 2018))
manip <- within(manip, y2019 <- tmp.proj(baseyr = 2020, yr = 2019))
manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
# indicate manip, and return to data
manip <- within(manip, times.manip <- times.manip + 1)
eq2[sel,] <- manip
manip <- eq3[sel,]
manip <- within(manip, {action <- orig.dest <- ""; when <- NA})
manip <- within(manip, {y1997 <- y1998 <- y1999 <- y2000 <- y2001 <- y2002 <- y2003 <- y2004 <- y2005 <- y2006 <- y2007 <- y2008 <- y2009 <- y2010 <- y2011 <- y2012 <- y2013 <- y2014 <- y2015 <- y2016 <- NA})
eq3[sel,] <- manip
#
# two more items where change1 and change2 occurred between 2010 and 2020 censuses
sel <- which(eq3$when2<2020 & eq3$when>=2010)
# monitor
eq2[sel,c(1,6:11)] # all sp.fr then sp.to
# item 1
sel <- c(1976,2678:2699,2703)
sel <- 160000 + sel   # turn into edosecn
sel <- which(eq2$edosecn %in% sel) # turn to indices
manip <- eq2[sel,]
# aggregate split populations for pi
manip$cen_2010b <- sum(eq2$cen_2010[sel], na.rm = TRUE)
manip$cen_2020b <- sum(eq2$cen_2020[sel], na.rm = TRUE)
manip <- within(manip, pi <-  (cen_2020b - cen_2010b) / (2020 - 2010) ); # rate of change
# return to data for piecemeal manip
eq2[sel,] <- manip
# fix last link of 1976
manip <- eq2[sel[1],]
# project 11-13, indicate, and return to data
manip <- within(manip, y2011 <- tmp.proj(baseyr = 2010, yr = 2011))
manip <- within(manip, y2012 <- tmp.proj(baseyr = 2010, yr = 2012))
manip <- within(manip, y2013 <- tmp.proj(baseyr = 2010, yr = 2013))
manip <- within(manip, times.manip <- times.manip + 1)
eq2[sel[1],] <- manip
manip <- eq3[sel[1],]
manip <- within(manip, {action <- orig.dest <- ""; when <- NA})
manip <- within(manip, {y2014 <- y2015 <- y2016 <- y2017 <- y2018 <- y2019 <- y2020 <- y2021 <- y2022 <-  NA})
eq3[sel[1],] <- manip
# fix new splits but one split twice
sel <- c(2678:2691,2693:2699)
sel <- 160000 + sel   # turn into edosecn
sel <- which(eq2$edosecn %in% sel) # turn to indices
manip <- eq2[sel,]
# project 14 on, indicate, and return to data
manip <- within(manip, y2014 <- tmp.proj(baseyr = 2020, yr = 2014))
manip <- within(manip, y2015 <- tmp.proj(baseyr = 2020, yr = 2015))
manip <- within(manip, y2016 <- tmp.proj(baseyr = 2020, yr = 2016))
manip <- within(manip, y2017 <- tmp.proj(baseyr = 2020, yr = 2017))
manip <- within(manip, y2018 <- tmp.proj(baseyr = 2020, yr = 2018))
manip <- within(manip, y2019 <- tmp.proj(baseyr = 2020, yr = 2019))
manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
manip <- within(manip, times.manip <- times.manip + 1)
eq2[sel,] <- manip
manip <- eq3[sel,]
manip <- within(manip, {
    action <- orig.dest <- ""; when <- NA;
    y1997 <- y1998 <- y1999 <- NA;
    y2000 <- y2001 <- y2002 <- y2003 <- y2004 <- y2005 <- y2006 <- y2007 <- y2008 <- y2009 <- NA;
    y2010 <- y2011 <- y2012 <- y2013 <- NA;
})
eq3[sel,] <- manip
#
# fix split twice
sel <- 2692
sel <- 160000 + sel   # turn into edosecn
sel <- which(eq2$edosecn %in% sel) # turn to indices
manip <- eq2[sel,]
# project 15 on
manip <- within(manip, y2015 <- tmp.proj(baseyr = 2020, yr = 2015))
manip <- within(manip, y2016 <- tmp.proj(baseyr = 2020, yr = 2016))
manip <- within(manip, y2017 <- tmp.proj(baseyr = 2020, yr = 2017))
manip <- within(manip, y2018 <- tmp.proj(baseyr = 2020, yr = 2018))
manip <- within(manip, y2019 <- tmp.proj(baseyr = 2020, yr = 2019))
manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
# project 20145, indicate, and return to data
manip$y2020 <- manip$y2020 - eq2$y2020[which(eq2$edosecn==162703)]
manip <- within(manip, y2014 <- tmp.proj(baseyr = 2020, yr = 2014))
manip <- within(manip, y2020 <- cen_2020) # restore 2020 pop
manip <- within(manip, times.manip <- times.manip + 1)
eq2[sel,] <- manip
manip <- eq3[sel,]
manip <- within(manip, {
    action <- orig.dest <- ""; when <- NA;
    action2 <- orig.dest2 <- ""; when2 <- NA;
    y1997 <- y1998 <- y1999 <- NA;
    y2000 <- y2001 <- y2002 <- y2003 <- y2004 <- y2005 <- y2006 <- y2007 <- y2008 <- y2009 <- NA;
    y2010 <- y2011 <- y2012 <- y2013 <- NA;
})
eq3[sel,] <- manip
#
# fix last link
sel <- 2703
sel <- 160000 + sel   # turn into edosecn
sel <- which(eq2$edosecn %in% sel) # turn to indices
manip <- eq2[sel,]
# project 15 on
manip <- within(manip, y2015 <- tmp.proj(baseyr = 2020, yr = 2015))
manip <- within(manip, y2016 <- tmp.proj(baseyr = 2020, yr = 2016))
manip <- within(manip, y2017 <- tmp.proj(baseyr = 2020, yr = 2017))
manip <- within(manip, y2018 <- tmp.proj(baseyr = 2020, yr = 2018))
manip <- within(manip, y2019 <- tmp.proj(baseyr = 2020, yr = 2019))
manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
# project 15 on, indicate, and return to data
manip$y2020 <- manip$y2020 - eq2$y2020[which(eq2$edosecn==162703)]
manip <- within(manip, y2014 <- tmp.proj(baseyr = 2020, yr = 2014))
manip <- within(manip, y2020 <- cen_2020) # restore 2020 pop
manip <- within(manip, times.manip <- times.manip + 1)
eq2[sel,] <- manip
manip <- eq3[sel,]
manip <- within(manip, {
    action <- orig.dest <- ""; when <- NA;
    y1997 <- y1998 <- y1999 <- NA;
    y2000 <- y2001 <- y2002 <- y2003 <- y2004 <- y2005 <- y2006 <- y2007 <- y2008 <- y2009 <- NA;
    y2010 <- y2011 <- y2012 <- y2013 <- NA;
})
eq3[sel,] <- manip


## # item 2 ---  should have been fixed in "item 3" above 
## sel <- c(205,874,875:922,965,967,970,975:1003)
## sel <- 230000 + sel   # turn into edosecn
## sel <- which(eq2$edosecn %in% sel) # turn to indices
## manip <- eq2[sel,]
## # aggregate split populations for pi
## manip$cen_2010b <- sum(eq2$cen_2010[sel], na.rm = TRUE)
## manip$cen_2020b <- sum(eq2$cen_2020[sel], na.rm = TRUE)
## manip <- within(manip, pi <-  (cen_2020b - cen_2010b) / (2020 - 2010) ); # rate of change
## # return to data for piecemeal manip
## eq2[sel,] <- manip
## # fix seccion 874
## sel <- c(874,965,967)
## sel <- 230000 + sel   # turn into edosecn
## sel <- which(eq2$edosecn %in% sel) # turn to indices
## manip <- eq2[sel,]
## manip$y2020[2:3] <- 0 # none appears in 2018 lisnom, jungle?
## # project 17-on and return to data
## manip <- within(manip, y2017 <- tmp.proj(baseyr = 2020, yr = 2017))
## manip <- within(manip, y2018 <- tmp.proj(baseyr = 2020, yr = 2018))
## manip <- within(manip, y2019 <- tmp.proj(baseyr = 2020, yr = 2019))
## manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
## manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
## # project 12-on in 874 (should be done separately, but 965 967 have non pop
## manip <- within(manip, y2012 <- tmp.proj(baseyr = 2020, yr = 2012))
## manip <- within(manip, y2013 <- tmp.proj(baseyr = 2020, yr = 2013))
## manip <- within(manip, y2014 <- tmp.proj(baseyr = 2020, yr = 2014))
## manip <- within(manip, y2015 <- tmp.proj(baseyr = 2020, yr = 2015))
## manip <- within(manip, y2016 <- tmp.proj(baseyr = 2020, yr = 2016))
## manip$y2012[2:3] <- manip$y2013[2:3] <- manip$y2014[2:3] <- manip$y2015[2:3] <- manip$y2016[2:3] <- NA
## # return to data and indicate manip
## manip <- within(manip, times.manip <- times.manip + 1)
## eq2[sel,] <- manip
## manip <- eq3[sel,]
## manip <- within(manip, {
##     action <-  orig.dest <- "";  when <- NA;
##     action2 <- orig.dest2 <- ""; when2 <- NA;
## })
## eq3[sel,] <- manip

## LA CAGUE, LO DE JUSTO ARRIBA YA LO HABIA HECHO AQUI ABAJO
## # item 1
## i <- which(eq2$edosecn==161976)
## manip <- eq2[i,]
## # for 2010 seccion 1976 pop only, already there
## # for 2020 bunch of other pops
## j <- which(eq2$edosecn %in% 162678:162699 | eq2$edosecn %in% c(162692,162703))
## manip$cen_2020b <- sum(eq2$cen_2020[j])
## manip <- within(manip, y2020 <- cen_2020b)
## manip <- within(manip, pi <-  (y2020 - y2010) / (2020 - 2010) ); # rate of change
## # project
## manip <- within(manip, y2011 <- tmp.proj(baseyr = 2010, yr = 2011))
## manip <- within(manip, y2012 <- tmp.proj(baseyr = 2010, yr = 2012))
## manip <- within(manip, y2013 <- tmp.proj(baseyr = 2010, yr = 2013))
## # keep piand return to data
## tmp <- manip$pi
## manip <- within(manip, {
##     times.manip <- times.manip + 1;
## })
## eq2[i,] <- manip
## # inidacte manip
## manip <- eq3[i,]
## manip <- within(manip, {
##     y2020 <- NA;
##     action <- orig.dest <- "";   when  <- NA;
##     action2 <- orig.dest2 <- ""; when2 <- NA;
## })
## eq3[i,] <- manip
## # plug pi across the board and return to data before loop
## sel <- which(eq2$edosecn %in% 162678:162699 | eq2$edosecn %in% c(162692,162703))
## manip <- eq2[sel,]
## manip$pi <- tmp
## eq2[sel,] <- manip
## # loop over items
## for (i in sel){
##     #i <- sel[1]
##     manip <- eq2[i,]
##     manip$y2020 <- manip$cen_2020
##     # re-project where needed
##     if (manip$when<2011) manip <- within(manip, y2010 <- tmp.proj(baseyr = 2020, yr = 2010))
##     if (manip$when<2012) manip <- within(manip, y2011 <- tmp.proj(baseyr = 2020, yr = 2011))
##     if (manip$when<2013) manip <- within(manip, y2012 <- tmp.proj(baseyr = 2020, yr = 2012))
##     if (manip$when<2014) manip <- within(manip, y2013 <- tmp.proj(baseyr = 2020, yr = 2013))
##     if (manip$when<2015) manip <- within(manip, y2014 <- tmp.proj(baseyr = 2020, yr = 2014))
##     if (manip$when<2016) manip <- within(manip, y2015 <- tmp.proj(baseyr = 2020, yr = 2015))
##     if (manip$when<2017) manip <- within(manip, y2016 <- tmp.proj(baseyr = 2020, yr = 2016))
##     if (manip$when<2018) manip <- within(manip, y2017 <- tmp.proj(baseyr = 2020, yr = 2017))
##     if (manip$when<2019) manip <- within(manip, y2018 <- tmp.proj(baseyr = 2020, yr = 2018))
##     if (manip$when<2021) manip <- within(manip, y2019 <- tmp.proj(baseyr = 2020, yr = 2019))
##     if (manip$when<2022) manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
##     if (manip$when<2023) manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
##     # return manipulation to data
##     manip <- within(manip, {
##         times.manip <- times.manip + 1;
##     })
##     eq2[i,] <- manip
##     # indicate manip and return to data
##     manip <- eq3[i,]
##     manip <- within(manip, {
##         action <- orig.dest <- "";   when  <- NA;
##         action2 <- orig.dest2 <- ""; when2 <- NA;
##     })
##     eq3[i,] <- manip
## }
## #
## # item 2
## i <- which(eq2$edosecn==230205)
## manip <- eq2[i,]
## # for 2010 seccion 1976 pop only, already there
## # for 2020 bunch of other pops
## j <- which(eq2$edosecn %in% c(230874,230876:230922,230975:231003))
## manip$cen_2020b <- sum(eq2$cen_2020[j])
## manip <- within(manip, y2020 <- cen_2020b)
## manip <- within(manip, pi <-  (y2020 - y2010) / (2020 - 2010) ); # rate of change
## # project
## manip <- within(manip, y2011 <- tmp.proj(baseyr = 2010, yr = 2011))
## # keep pi, indicate manip and return to data
## tmp <- manip$pi
## manip <- within(manip, {
##     times.manip <- times.manip + 1;
## })
## eq2[i,] <- manip
## # indicate manip pt 2
## manip <- eq3[i,]
## manip <- within(manip, {
##     y2020 <- NA;
##     action <- orig.dest <- "";   when  <- NA;
##     action2 <- orig.dest2 <- ""; when2 <- NA;
## })
## eq3[i,] <- manip
## # fix seccion 875 pop 2020 and return to data before loop
## sel <- which(eq2$edosecn==230875)
## manip <- eq2[sel,]
## j <- which(eq2$edosecn %in% c(230975:231003))
## manip$cen_2020 <- sum(eq2$cen_2020[j]) # not use b to match other items in next block
## eq2[sel,] <- manip
## # plug pi across the board and return to ddata before block manip
## sel <- which(eq2$edosecn %in% c(230874:230922,230975:231003))
## manip <- eq2[sel,]
## manip$pi <- tmp
## eq2[sel,] <- manip
## #
## # 1st block of items
## i <- which(eq2$edosecn %in% c(230874:230922))
## manip <- eq2[i,]
## manip$y2020 <- manip$cen_2020
## # re-project where needed
## manip <- within(manip, y2012 <- tmp.proj(baseyr = 2020, yr = 2012))
## manip <- within(manip, y2013 <- tmp.proj(baseyr = 2020, yr = 2013))
## manip <- within(manip, y2014 <- tmp.proj(baseyr = 2020, yr = 2014))
## manip <- within(manip, y2015 <- tmp.proj(baseyr = 2020, yr = 2015))
## manip <- within(manip, y2016 <- tmp.proj(baseyr = 2020, yr = 2016))
## manip <- within(manip, y2017 <- tmp.proj(baseyr = 2020, yr = 2017))
## manip <- within(manip, y2018 <- tmp.proj(baseyr = 2020, yr = 2018))
## manip <- within(manip, y2019 <- tmp.proj(baseyr = 2020, yr = 2019))
## manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
## manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
## # indicate manip pt1 and return to data 
## manip <- within(manip, {
##     times.manip <- times.manip + 1;
## })
## eq2[i,] <- manip
## # indicate manip pt2 and return to data 
## manip <- eq3[i,]
## # fix seccion 875
## manip$y2020[which(manip$edosecn==230875)] <- manip$y2021[which(manip$edosecn==230875)] <- manip$y2022[which(manip$edosecn==230875)] <- NA
## manip[2,]
## manip <- within(manip, {
##     action <- orig.dest <- "";   when  <- NA;
##     action2 <- orig.dest2 <- ""; when2 <- NA;
## })
## eq3[i,] <- manip
## #
## # other block of split secciones
## i <- which(eq2$edosecn %in% c(230975:231003))
## manip <- eq2[i,]
## manip$y2020 <- manip$cen_2020
## # re-project where needed
## manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
## manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
## # indicate manip and return to data 
## manip <- within(manip, {
##     times.manip <- times.manip + 1;
## })
## eq2[i,] <- manip
## # indicate manip and return to data 
## manip <- eq3[i,]
## manip <- within(manip, {
##     action <- orig.dest <- "";   when  <- NA;
##     action2 <- orig.dest2 <- ""; when2 <- NA;
## })
## eq3[i,] <- manip

# item where changes occurred before 2005 both
sel <- which(eq2$edosecn==190439)
manip <- eq2[sel,] # duplicate obs for manipulation
vec <- c(2124,2136:2165)
vec <- manip$edon*10000 + vec   # turn into edosecn
vec <- which(eq2$edosecn %in% vec) # turn to indices
# aggregate split populations
manip$cen_2005b <- sum(eq2$cen_2005[vec])
manip$cen_2010b <- sum(eq2$cen_2010[vec])
manip <- within(manip, {y2005 <- cen_2005b; y2010 <- cen_2010b;}) # use aggregates to compute pi
manip <- within(manip, pi <-  (y2010 - y2005) / (2010 - 2005) ); # rate of change
# re-project full pop up to 2001
manip <- within(manip, y1997 <- tmp.proj(baseyr = 2005, yr = 1997))
manip <- within(manip, y1998 <- tmp.proj(baseyr = 2005, yr = 1998))
manip <- within(manip, y1999 <- tmp.proj(baseyr = 2005, yr = 1999))
manip <- within(manip, y2000 <- tmp.proj(baseyr = 2005, yr = 2000))
manip <- within(manip, y2001 <- tmp.proj(baseyr = 2005, yr = 2001))
manip <- within(manip, y2002 <- tmp.proj(baseyr = 2005, yr = 2002))
# use lisnom 2003=12021 minus 11% = 10696 for 2002:2004
manip$y2005 <- 10696
# project 2002:2004
manip <- within(manip, y2003 <- tmp.proj(baseyr = 2005, yr = 2003))
manip <- within(manip, y2004 <- tmp.proj(baseyr = 2005, yr = 2004))
# indicate manip pt1 and return to data
tmp <- manip$pi # save to plug into split.from
manip <- within(manip, {
    times.manip <- times.manip + 2
})
eq2[sel,] <- manip
# indicate manip pt2
manip <- eq3[sel,]
manip <- within(manip, {
    y2005 <- y2010 <- NA;
    action  <- "";      when  <- NA;    orig.dest  <- "";
    action2 <- "";      when2 <- NA;    orig.dest2 <- "";
})
eq3[sel,] <- manip
#
# new secciones need no manipulation (only one is 2136, which cannot be manip bec chg in 2005, so 2005-10 trend used)
sel <- which(eq2$edosecn %in% c(192124,192136:192165))
manip <- eq3[sel,] # duplicate obs for manipulation
manip <- within(manip, {
    y1997 <- y1998 <- y1999 <- y2000 <- y2001 <- NA;
    action  <- "";      when  <- NA;    orig.dest  <- "";
    action2 <- "";      when2 <- NA;    orig.dest2 <- "";
})
eq3[sel,] <- manip

# two post 2010 changes
sel <- which(eq3$edosecn %in% c(191706,192722,192883:192897))
# monitor
eq2[sel,c(1,6:11)] # all sp.fr then sp.to
#
manip <- eq2[sel,]
# aggregate split populations for pi
manip$cen_2010b <- sum(eq2$cen_2010[sel], na.rm = TRUE)
manip$cen_2020b <- sum(eq2$cen_2020[sel], na.rm = TRUE)
manip <- within(manip, pi <-  (cen_2020b - cen_2010b) / (2020 - 2010) ); # rate of change
tmp <- manip$pi[1] # save to paste across the board
# return to data for piecemeal manip
eq2[sel,] <- manip
# manip seccion 1706 (simplified since seccion 2722 has no pop in 2020)
sel <- which(eq3$edosecn %in% c(191706,192722))
manip <- eq2[sel,]
# project 2011:19 (omits 2722 split)
manip <- within(manip, y2011 <- tmp.proj(baseyr = 2010, yr = 2011))
manip <- within(manip, y2012 <- tmp.proj(baseyr = 2010, yr = 2012))
manip <- within(manip, y2013 <- tmp.proj(baseyr = 2010, yr = 2013))
manip <- within(manip, y2014 <- tmp.proj(baseyr = 2010, yr = 2014))
manip <- within(manip, y2015 <- tmp.proj(baseyr = 2010, yr = 2015))
manip <- within(manip, y2016 <- tmp.proj(baseyr = 2010, yr = 2016))
manip <- within(manip, y2017 <- tmp.proj(baseyr = 2010, yr = 2017))
manip <- within(manip, y2018 <- tmp.proj(baseyr = 2010, yr = 2018))
manip <- within(manip, y2019 <- tmp.proj(baseyr = 2010, yr = 2019))
# seccion 2722
manip$y2017[2] <- manip$y2018[2] <- manip$y2019[2] <- manip$y2020[2] <- manip$y2021[2] <- manip$y2022[2] <- 0
# return to data and indicate manip
manip <- within(manip, times.manip <- times.manip + 1)
eq2[sel,] <- manip
manip <- eq3[sel,]
manip <- within(manip, {
    action <-  orig.dest <- "";  when <- NA;
    action2 <- orig.dest2 <- ""; when2 <- NA;
})
eq3[sel,] <- manip
# 
# new secciones post 2020
sel <- which(eq3$edosecn %in% c(192883:192897))
manip <- eq2[sel,]
manip$pi <- tmp
# project 21:22
manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
# return to data and indicate manip
manip <- within(manip, times.manip <- times.manip + 1)
eq2[sel,] <- manip
manip <- eq3[sel,]
manip <- within(manip, {
    action <-  orig.dest <- "";  when <- NA;
    action2 <- orig.dest2 <- ""; when2 <- NA;
})
eq3[sel,] <- manip

# 2014 merged.to2
sel <- which(eq2$edosecn %in% c(155930,155931))
manip <- eq2[sel,]
# aggregate split populations for pi
manip$cen_2010b <- sum(eq2$cen_2010[sel], na.rm = TRUE)
manip$cen_2020b <- sum(eq2$cen_2020[sel], na.rm = TRUE)
manip <- within(manip, pi <-  (cen_2020b - cen_2010b) / (2020 - 2010) ); # rate of change
# project 2011-13
manip <- within(manip, y2011 <- tmp.proj(baseyr = 2010, yr = 2011))
manip <- within(manip, y2012 <- tmp.proj(baseyr = 2010, yr = 2012))
manip <- within(manip, y2013 <- tmp.proj(baseyr = 2010, yr = 2013))
# project 2014 on
manip <- within(manip, y2014 <- tmp.proj(baseyr = 2020, yr = 2014))
manip <- within(manip, y2015 <- tmp.proj(baseyr = 2020, yr = 2015))
manip <- within(manip, y2016 <- tmp.proj(baseyr = 2020, yr = 2016))
manip <- within(manip, y2017 <- tmp.proj(baseyr = 2020, yr = 2017))
manip <- within(manip, y2018 <- tmp.proj(baseyr = 2020, yr = 2018))
manip <- within(manip, y2019 <- tmp.proj(baseyr = 2020, yr = 2019))
manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
# return to data and indicate manip
manip <- within(manip, times.manip <- times.manip + 1)
eq2[sel,] <- manip
manip <- eq3[sel,]
manip <- within(manip, {
    action2 <- orig.dest2 <- ""; when2 <- NA;
})
eq3[sel,] <- manip

# 2017 split.to2
sel <- which(eq2$edosecn %in% c(192197,192711:192716))
manip <- eq2[sel,]
# aggregate split populations for pi
manip$cen_2010b <- sum(eq2$cen_2010[sel], na.rm = TRUE)
manip$cen_2020b <- sum(eq2$cen_2020[sel], na.rm = TRUE)
manip <- within(manip, pi <-  (cen_2020b - cen_2010b) / (2020 - 2010) ); # rate of change for other secciones
# 2011-16
manip <- within(manip, y2011 <- tmp.proj(baseyr = 2010, yr = 2011))
manip <- within(manip, y2012 <- tmp.proj(baseyr = 2010, yr = 2012))
manip <- within(manip, y2013 <- tmp.proj(baseyr = 2010, yr = 2013))
manip <- within(manip, y2014 <- tmp.proj(baseyr = 2010, yr = 2014))
manip <- within(manip, y2015 <- tmp.proj(baseyr = 2010, yr = 2015))
manip <- within(manip, y2016 <- tmp.proj(baseyr = 2010, yr = 2016))
# 2017-22
manip$y2020[7] <- 0 # why seccion 2716 missing fr censo 2020?
manip <- within(manip, y2017 <- tmp.proj(baseyr = 2020, yr = 2017))
manip <- within(manip, y2018 <- tmp.proj(baseyr = 2020, yr = 2018))
manip <- within(manip, y2019 <- tmp.proj(baseyr = 2020, yr = 2019))
manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
# return to data and indicate manip
manip <- within(manip, times.manip <- times.manip + 1)
eq2[sel,] <- manip
manip <- eq3[sel,]
manip <- within(manip, {
    action <- orig.dest <- ""; when <- NA;
    action2 <- orig.dest2 <- ""; when2 <- NA;
})
eq3[sel,] <- manip
#
# fix 2009 change
sel <- which(eq2$edosecn %in% 190129)
manip <- eq2[sel,]
vec <- which(eq2$edosecn %in% c(190129,192196:192229))
# aggregate split populations for pi
manip$cen_2010b <- sum(eq2$cen_2010[vec], na.rm = TRUE)
manip <- within(manip, pi <-  (cen_2010b - cen_2005) / (2010 - 2005) ); # rate of change for other secciones
# project 1997-2008
manip <- within(manip, y2010 <- cen_2010b)
manip <- within(manip, y1997 <- tmp.proj(baseyr = 2010, yr = 1997))
manip <- within(manip, y1998 <- tmp.proj(baseyr = 2010, yr = 1998))
manip <- within(manip, y1999 <- tmp.proj(baseyr = 2010, yr = 1999))
manip <- within(manip, y2000 <- tmp.proj(baseyr = 2010, yr = 2000))
manip <- within(manip, y2001 <- tmp.proj(baseyr = 2010, yr = 2001))
manip <- within(manip, y2002 <- tmp.proj(baseyr = 2010, yr = 2002))
manip <- within(manip, y2003 <- tmp.proj(baseyr = 2010, yr = 2003))
manip <- within(manip, y2004 <- tmp.proj(baseyr = 2010, yr = 2004))
manip <- within(manip, y2005 <- tmp.proj(baseyr = 2010, yr = 2005))
manip <- within(manip, y2006 <- tmp.proj(baseyr = 2010, yr = 2006))
manip <- within(manip, y2007 <- tmp.proj(baseyr = 2010, yr = 2007))
manip <- within(manip, y2008 <- tmp.proj(baseyr = 2010, yr = 2008))
manip <- within(manip, y2010 <- NA)
# return to data and indicate manip
manip <- within(manip, times.manip <- times.manip + 1)
eq2[sel,] <- manip
manip <- eq3[sel,]
manip <- within(manip, {
    action <- orig.dest <- ""; when <- NA;
})
eq3[sel,] <- manip
#
# fix 2196:2229 in 2009
sel <- which(eq2$edosecn %in% c(192196:192229))
manip <- eq2[sel,]
manip <- within(manip, y2009 <- tmp.proj(baseyr = 2010, yr = 2009))
# return to data and indicate manip
manip <- within(manip, times.manip <- times.manip + 1)
eq2[sel,] <- manip
manip <- eq3[sel,]
manip <- within(manip, {
    action <- orig.dest <- ""; when <- NA;
})
eq3[sel,] <- manip
#
# monitor no when2 remaining
sel <- which(!is.na(eq3$when2))
eq2[sel,c(1,6:11)] # all sp.fr then sp.to

## # appears redundant, manip above
## # fix merged.to case
## # item 1
## sel <- which(eq2$edosecn==154308)
## manip <- eq3[sel,] # duplicate obs for manipulation
## vec <- c(4308,5931)
## vec <- manip$edon*10000 + vec   # turn into edosecn
## vec <- which(eq2$edosecn %in% vec) # turn to indices
## # aggregate split populations
## manip$cen_2005b <- sum(eq2$cen_2005[vec])
## manip$cen_2010b <- sum(eq2$cen_2010[vec])
## manip <- within(manip, {y2005 <- cen_2005b; y2010 <- cen_2010b;}) # use aggregates to compute pi
## manip <- within(manip, pi <-  (y2010 - y2005) / (2010 - 2005) ); # rate of change
## # re-project full pop up to 2001
## manip <- within(manip, y1997 <- tmp.proj(baseyr = 2005, yr = 1997))
## manip <- within(manip, y1998 <- tmp.proj(baseyr = 2005, yr = 1998))
## manip <- within(manip, y1999 <- tmp.proj(baseyr = 2005, yr = 1999))
## manip <- within(manip, y2000 <- tmp.proj(baseyr = 2005, yr = 2000))
## manip <- within(manip, y2001 <- tmp.proj(baseyr = 2005, yr = 2001))
## # restore 2005 and 2010 and return to data
## manip <- within(manip, {y2005 <- cen_2005; y2010 <- cen_2010;}) 
## eq2[sel,] <- manip
## # indicate manip
## manip <- eq3[sel,]
## manip <- within(manip, {
##     action  <- "";      when  <- NA;    orig.dest  <- "";
## })
## eq3[sel,] <- manip
## # item 2
## sel <- which(eq2$edosecn==155931)
## manip <- eq2[sel,] # duplicate obs for manipulation
## manip <- within(manip, pi <-  (y2010 - y2005) / (2010 - 2005) ); # rate of change
## manip <- within(manip, y2011 <- tmp.proj(baseyr = 2010, yr = 2011))
## manip <- within(manip, y2012 <- tmp.proj(baseyr = 2010, yr = 2012))
## manip <- within(manip, y2013 <- tmp.proj(baseyr = 2010, yr = 2013))
## # indicate manip pt1 and return to data
## manip <- within(manip, {
##     times.manip <- times.manip + 1
## })
## eq2[sel,] <- manip
## # indicate manip and return to data
## manip <- eq3[sel,]
## manip <- within(manip, {
##     action  <- "";      when  <- NA;    orig.dest  <- "";
##     action2 <- "";      when2 <- NA;    orig.dest2 <- "";
## })
## eq3[sel,] <- manip
## # item 3
## sel <- which(eq2$edosecn==155930)
## manip <- eq2[sel,] # duplicate obs for manipulation
## vec <- c(5930,5931)
## vec <- manip$edon*10000 + vec   # turn into edosecn
## vec <- which(eq2$edosecn %in% vec) # turn to indices
## # aggregate split populations
## manip$cen_2010b <- sum(eq2$cen_2010[vec])
## manip <- within(manip, y2010 <- cen_2010b)
## manip <- within(manip, pi <-  (y2010 - y2005) / (2010 - 2005) ); # rate of change
## # re-project 2014-on
## manip <- within(manip, y2014 <- tmp.proj(baseyr = 2020, yr = 2014))
## manip <- within(manip, y2015 <- tmp.proj(baseyr = 2020, yr = 2015))
## manip <- within(manip, y2016 <- tmp.proj(baseyr = 2020, yr = 2016))
## manip <- within(manip, y2017 <- tmp.proj(baseyr = 2020, yr = 2017))
## manip <- within(manip, y2018 <- tmp.proj(baseyr = 2020, yr = 2018))
## manip <- within(manip, y2019 <- tmp.proj(baseyr = 2020, yr = 2019))
## manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
## manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
## # restore 2005 and 2010
## manip <- within(manip, y2010 <- cen_2010)
## # indicate manip pt1 and return to data
## manip <- within(manip, {
##     times.manip <- times.manip + 1
## })
## eq2[sel,] <- manip
## # indicate manip pt2
## manip <- eq3[sel,]
## manip <- within(manip, {
##     action  <- "";      when  <- NA;    orig.dest  <- "";
## })
## eq3[sel,] <- manip

# vectors picking census column that is not NA
#aorb05 <- apply(X=eq2[,c("cen_2005","cen_2005b","y2005")], MARGIN=1, FUN=function(X)
#    ifelse(!is.na(X[1]), X[1], ifelse(!is.na(X[2]), X[2], X[3]))) # try b if pop2010 is NA
aorb10 <- apply(X=eq2[,c("cen_2010","cen_2010b","y2010")], MARGIN=1, FUN=function(X)
    ifelse(!is.na(X[1]), X[1], ifelse(!is.na(X[2]), X[2], X[3]))) # try b if pop2010 is NA
#aorb20 <- apply(X=eq2[,c("cen_2020","cen_2020b","y2020")], MARGIN=1, FUN=function(X)
#    ifelse(!is.na(X[1]), X[1], ifelse(!is.na(X[2]), X[2], X[3]))) # try b if pop2010 is NA
#################################
## split secciones before 2005 ##
#################################
sel <- which(eq3$action=="split.to" & eq3$when > 1996 & eq3$when <  2005)
# add target secciones
for (i in sel){
    #i <- sel[15]
    manip <- eq2[i,] # duplicate obs for manipulation
    vec <- eval(parse(text = manip$orig.dest)) # https://stackoverflow.com/questions/1743698/evaluate-expression-given-as-a-string
    vec <- manip$edon*10000 + vec   # turn into edosecn
    manip$case <- ifelse(manip$edosecn %in% vec, "no.baja", "baja") # did seccion survive the split? (baja means no) 
    vec <- which(eq2$edosecn %in% vec) # turn to indices
    vec.plus  <- union(vec, i)   # seccion and its targets (in case vec excludes dropped seccion) 
    vec.minus <- setdiff(vec, i) # targets w/o seccion (for use when seccion was dropped)
    # aggregate split populations
    manip$cen_2005b <- with(eq2, sum(cen_2005[vec]))
    manip$cen_2010b <- with(eq2, sum(aorb10[vec])) # needed for backwards projection
    manip <- within(manip, {y2005 <- cen_2005b; y2010 <- cen_2010b;}) # use aggregates to compute pi
    manip <- within(manip, pi <-  (y2010 - y2005) / (2010 - 2005) ); # rate of change
    # re-project where needed (targets unneeded before 2005)
    if (manip$when>1997) manip <- within(manip, y1997 <- tmp.proj(baseyr = 2005, yr = 1997))
    if (manip$when>1998) manip <- within(manip, y1998 <- tmp.proj(baseyr = 2005, yr = 1998))
    if (manip$when>1999) manip <- within(manip, y1999 <- tmp.proj(baseyr = 2005, yr = 1999))
    if (manip$when>2000) manip <- within(manip, y2000 <- tmp.proj(baseyr = 2005, yr = 2000))
    if (manip$when>2001) manip <- within(manip, y2001 <- tmp.proj(baseyr = 2005, yr = 2001))
    if (manip$when>2002) manip <- within(manip, y2002 <- tmp.proj(baseyr = 2005, yr = 2002))
    if (manip$when>2003) manip <- within(manip, y2003 <- tmp.proj(baseyr = 2005, yr = 2003))
    if (manip$when>2004) manip <- within(manip, y2004 <- tmp.proj(baseyr = 2005, yr = 2004))
    # restore un-manipulated 2005 in cases where when is below
    if (manip$when<2005 & !is.na(manip$cen_2005)) manip <- within(manip, y2005 <- cen_2005) # if is.na then indicated in eq3
    if (manip$when<2010 & !is.na(manip$cen_2010)) manip <- within(manip, y2010 <- cen_2010) # if is.na then indicated in eq3
    # indicate manip pt1 and return to data
    manip <- within(manip, {
        times.manip <- times.manip + 1
    })
    eq2[i,] <- manip
    tmp <- manip$case # plug below
    # fix proj for eliminated secciones
    manip <- eq3[i,]
    manip$case <- tmp
    if (manip$case=="baja") {
        if (eq2$when[i]<=1998) manip <- within(manip, y1998 <- NA) # use eq2$when bec eq3$when may be NA if prior manip
        if (eq2$when[i]<=1999) manip <- within(manip, y1999 <- NA)
        if (eq2$when[i]<=2000) manip <- within(manip, y2000 <- NA)
        if (eq2$when[i]<=2001) manip <- within(manip, y2001 <- NA)
        if (eq2$when[i]<=2002) manip <- within(manip, y2002 <- NA)
        if (eq2$when[i]<=2003) manip <- within(manip, y2003 <- NA)
        if (eq2$when[i]<=2004) manip <- within(manip, y2004 <- NA)
        if (eq2$when[i]<=2005) manip <- within(manip, y2005 <- NA)
        if (eq2$when[i]<=2006) manip <- within(manip, y2006 <- NA)
        if (eq2$when[i]<=2007) manip <- within(manip, y2007 <- NA)
        if (eq2$when[i]<=2008) manip <- within(manip, y2008 <- NA)
        if (eq2$when[i]<=2009) manip <- within(manip, y2009 <- NA)
        if (eq2$when[i]<=2010) manip <- within(manip, y2010 <- NA)
        if (eq2$when[i]<=2011) manip <- within(manip, y2011 <- NA)
        if (eq2$when[i]<=2012) manip <- within(manip, y2012 <- NA)
        if (eq2$when[i]<=2013) manip <- within(manip, y2013 <- NA)
        if (eq2$when[i]<=2014) manip <- within(manip, y2014 <- NA)
        if (eq2$when[i]<=2015) manip <- within(manip, y2015 <- NA)
        if (eq2$when[i]<=2016) manip <- within(manip, y2016 <- NA)
        if (eq2$when[i]<=2017) manip <- within(manip, y2017 <- NA)
        if (eq2$when[i]<=2018) manip <- within(manip, y2018 <- NA)
        if (eq2$when[i]<=2019) manip <- within(manip, y2019 <- NA)
        if (eq2$when[i]<=2020) manip <- within(manip, y2020 <- NA)
        if (eq2$when[i]<=2021) manip <- within(manip, y2021 <- NA)
        if (eq2$when[i]<=2022) manip <- within(manip, y2022 <- NA)
    }
    # indicate manip pt2
    manip <- within(manip, {
        action  <- "";      when  <- NA;    orig.dest  <- "";
    })
    manip$case <- NULL
    eq3[i,] <- manip
    # fix proj in non-existing secciones and return manipulation to data
    manip <- eq3[vec.minus,]
    if (eq2$when[i]>1997) manip <- within(manip, y1997 <- NA) # use eq2$when bec eq3$when may be NA if prior manip
    if (eq2$when[i]>1998) manip <- within(manip, y1998 <- NA)
    if (eq2$when[i]>1999) manip <- within(manip, y1999 <- NA)
    if (eq2$when[i]>2000) manip <- within(manip, y2000 <- NA)
    if (eq2$when[i]>2001) manip <- within(manip, y2001 <- NA)
    if (eq2$when[i]>2002) manip <- within(manip, y2002 <- NA)
    if (eq2$when[i]>2003) manip <- within(manip, y2003 <- NA)
    if (eq2$when[i]>2004) manip <- within(manip, y2004 <- NA)
    if (eq2$when[i]>2005) manip <- within(manip, y2005 <- NA)
    # indicate manipulation and return to data
    manip <- within(manip, {
        action  <- "";      when  <- NA;    orig.dest  <- "";
    })
    eq3[vec.minus,] <- manip
}

####################################################
## single change split.from secciones before 2005 ##
## only need NAs added where appropriate          ##
####################################################
sel <- which(eq3$action=="split.from" & eq3$when > 1996 & eq3$when <  2005)
manip <- eq3[sel,] # duplicate obs for manipulation
# add NAs
manip[manip$when>1997,] <- within(manip[manip$when>1997,], y1997 <- NA)
manip[manip$when>1998,] <- within(manip[manip$when>1998,], y1998 <- NA)
manip[manip$when>1999,] <- within(manip[manip$when>1999,], y1999 <- NA)
manip[manip$when>2000,] <- within(manip[manip$when>2000,], y2000 <- NA)
manip[manip$when>2001,] <- within(manip[manip$when>2001,], y2001 <- NA)
manip[manip$when>2002,] <- within(manip[manip$when>2002,], y2002 <- NA)
manip[manip$when>2003,] <- within(manip[manip$when>2003,], y2003 <- NA)
# indicate manipulation and return to data
manip <- within(manip, {
    action  <- "";      when  <- NA;    orig.dest  <- "";
})
eq3[sel,] <- manip
#
# verify what's left before 2005
with(eq3, table(pre05=when<2005, action, useNA = "always"))

# vectors picking census column that is not NA
aorb05 <- apply(X=eq2[,c("cen_2005","cen_2005b","y2005")], MARGIN=1, FUN=function(X)
    ifelse(!is.na(X[1]), X[1], ifelse(!is.na(X[2]), X[2], X[3]))) # try b if pop2010 is NA
aorb10 <- apply(X=eq2[,c("cen_2010","cen_2010b","y2010")], MARGIN=1, FUN=function(X)
    ifelse(!is.na(X[1]), X[1], ifelse(!is.na(X[2]), X[2], X[3]))) # try b if pop2010 is NA
#aorb20 <- apply(X=eq2[,c("cen_2020","cen_2020b","y2020")], MARGIN=1, FUN=function(X)
#    ifelse(!is.na(X[1]), X[1], ifelse(!is.na(X[2]), X[2], X[3]))) # try b if pop2010 is NA
#######################################
## single change secciones 2005-2010 ##
#######################################
#############################
## split.to and split.from ##
#############################
sel <- which(eq3$action=="split.to" & eq3$when >= 2005 & eq3$when < 2010)
for (i in sel){
    #i <- sel[12]
    manip <- eq2[i,] # duplicate obs for manipulation
    vec <- eval(parse(text = manip$orig.dest))
    vec <- manip$edon*10000 + vec   # turn into edosecn
    manip$case <- ifelse(manip$edosecn %in% vec, "no.baja", "baja") # did seccion survive the split? (baja means no) 
    vec <- which(eq2$edosecn %in% vec) # turn to indices
    vec.plus  <- union(vec, i)   # seccion and its targets (in case vec excludes dropped seccion) 
    vec.minus <- setdiff(vec, i) # targets w/o seccion (for use when seccion was dropped)
    # aggregate split populations
    manip$cen_2005b <- with(eq2, sum(aorb05[vec]))
    manip$cen_2010b <- with(eq2, sum(aorb10[vec]))
    #manip$cen_2020b <- with(eq2, sum(aorb20[vec]))
    if (manip$when==2005) manip <- within(manip, {y2005 <- cen_2005b; y2010 <- cen_2010b;}) # use aggregates to compute pi
    if (manip$when>2005)  manip <- within(manip, {y2005 <- cen_2005; y2010 <- cen_2010b;}) # use aggregates to compute pi
    manip <- within(manip, pi <-  (y2010 - y2005) / (2010 - 2005) ); # rate of change
    # re-project pre-2010 where needed
    if (manip$when>1997) manip <- within(manip, y1997 <- tmp.proj(baseyr = 2005, yr = 1997))
    if (manip$when>1998) manip <- within(manip, y1998 <- tmp.proj(baseyr = 2005, yr = 1998))
    if (manip$when>1999) manip <- within(manip, y1999 <- tmp.proj(baseyr = 2005, yr = 1999))
    if (manip$when>2000) manip <- within(manip, y2000 <- tmp.proj(baseyr = 2005, yr = 2000))
    if (manip$when>2001) manip <- within(manip, y2001 <- tmp.proj(baseyr = 2005, yr = 2001))
    if (manip$when>2002) manip <- within(manip, y2002 <- tmp.proj(baseyr = 2005, yr = 2002))
    if (manip$when>2003) manip <- within(manip, y2003 <- tmp.proj(baseyr = 2005, yr = 2003))
    if (manip$when>2004) manip <- within(manip, y2004 <- tmp.proj(baseyr = 2005, yr = 2004))
    if (manip$when>2005) manip <- within(manip, y2005 <- tmp.proj(baseyr = 2005, yr = 2005))
    if (manip$when>2006) manip <- within(manip, y2006 <- tmp.proj(baseyr = 2005, yr = 2006))
    if (manip$when>2007) manip <- within(manip, y2007 <- tmp.proj(baseyr = 2005, yr = 2007))
    if (manip$when>2008) manip <- within(manip, y2008 <- tmp.proj(baseyr = 2005, yr = 2008))
    #if (manip$when>2009) manip <- within(manip, y2009 <- tmp.proj(baseyr = 2005, yr = 2009))
    # indicate manip pt1 and return to data
    manip <- within(manip, {
        times.manip <- times.manip + 1;
    })
    eq2[i,] <- manip
    # indicate manip pt2
    manip <- eq3[i,]
    manip <- within(manip, {
        action  <- "";      when  <- NA;    orig.dest  <- "";
    })
    eq3[i,] <- manip
    #
    # fix proj in non-existing secciones and return manipulation to data
    manip <- eq2[vec.minus,]
    if (eq2$when[i]<=1997) manip <- within(manip, y1997 <- tmp.proj(baseyr = 2010, yr = 1997)) # use eq2$when bec eq3$when may be NA if prior manip
    if (eq2$when[i]<=1998) manip <- within(manip, y1998 <- tmp.proj(baseyr = 2010, yr = 1998))
    if (eq2$when[i]<=1999) manip <- within(manip, y1999 <- tmp.proj(baseyr = 2010, yr = 1999))
    if (eq2$when[i]<=2000) manip <- within(manip, y2000 <- tmp.proj(baseyr = 2010, yr = 2000))
    if (eq2$when[i]<=2001) manip <- within(manip, y2001 <- tmp.proj(baseyr = 2010, yr = 2001))
    if (eq2$when[i]<=2002) manip <- within(manip, y2002 <- tmp.proj(baseyr = 2010, yr = 2002))
    if (eq2$when[i]<=2003) manip <- within(manip, y2003 <- tmp.proj(baseyr = 2010, yr = 2003))
    if (eq2$when[i]<=2004) manip <- within(manip, y2004 <- tmp.proj(baseyr = 2010, yr = 2004))
    if (eq2$when[i]<=2005) manip <- within(manip, y2005 <- tmp.proj(baseyr = 2010, yr = 2005))
    if (eq2$when[i]<=2006) manip <- within(manip, y2006 <- tmp.proj(baseyr = 2010, yr = 2006))
    if (eq2$when[i]<=2007) manip <- within(manip, y2007 <- tmp.proj(baseyr = 2010, yr = 2007))
    if (eq2$when[i]<=2008) manip <- within(manip, y2008 <- tmp.proj(baseyr = 2010, yr = 2008))
    if (eq2$when[i]<=2009) manip <- within(manip, y2009 <- tmp.proj(baseyr = 2010, yr = 2009))
    #if (eq2$when[i]<=2010) manip <- within(manip, y2010 <- tmp.proj(baseyr = 2010, yr = 2010))
    # return to data
    manip <- within(manip, {
        times.manip <- times.manip + 1;
    })
    eq2[vec.minus,] <- manip
    # indicate manip in new secciones and return to data
    manip <- eq3[vec.minus,]
    manip <- within(manip, {
        action  <- "";      when  <- NA;    orig.dest  <- "";
    })
    eq3[vec.minus,] <- manip
}

###############
## merged.to ##
###############
sel <- which(eq3$action=="merged.to" & eq3$when >= 2005 & eq3$when < 2010)
#eq3$when[sel]
for (i in sel){
    #i <- sel[60]
    manip <- eq2[i,] # duplicate obs for manipulation
    vec <- eval(parse(text = manip$orig.dest))
    vec <- manip$edon*10000 + vec   # turn into edosecn
    manip$case <- ifelse(manip$edosecn %in% vec, "no.baja", "baja") # did seccion survive the split? (baja means no) 
    vec <- which(eq2$edosecn %in% vec) # turn to indices
    vec.plus  <- union(vec, i)   # seccion and its targets (in case vec excludes dropped seccion) 
    vec.minus <- setdiff(vec, i) # targets w/o seccion (for use when seccion was dropped)
    # aggregate split populations
    manip$cen_2005b <- with(eq2, sum(aorb05[vec.plus]))
    manip$cen_2010b <- with(eq2, sum(aorb10[vec]))
    #manip$cen_2020b <- with(eq2, sum(aorb20[vec]))
    manip <- within(manip, {y2005 <- cen_2005b; y2010 <- cen_2010b;}) # use aggregates to compute pi
    manip <- within(manip, pi <-  (y2010 - y2005) / (2010 - 2005) ); # rate of change
    # recover original pop 2005 for projection
    manip <- within(manip, y2005 <- cen_2005)
    # re-project pre-2010 where needed
    if (manip$when>1997) manip <- within(manip, y1997 <- tmp.proj(baseyr = 2005, yr = 1997))
    if (manip$when>1998) manip <- within(manip, y1998 <- tmp.proj(baseyr = 2005, yr = 1998))
    if (manip$when>1999) manip <- within(manip, y1999 <- tmp.proj(baseyr = 2005, yr = 1999))
    if (manip$when>2000) manip <- within(manip, y2000 <- tmp.proj(baseyr = 2005, yr = 2000))
    if (manip$when>2001) manip <- within(manip, y2001 <- tmp.proj(baseyr = 2005, yr = 2001))
    if (manip$when>2002) manip <- within(manip, y2002 <- tmp.proj(baseyr = 2005, yr = 2002))
    if (manip$when>2003) manip <- within(manip, y2003 <- tmp.proj(baseyr = 2005, yr = 2003))
    if (manip$when>2004) manip <- within(manip, y2004 <- tmp.proj(baseyr = 2005, yr = 2004))
    if (manip$when>2005) manip <- within(manip, y2005 <- tmp.proj(baseyr = 2005, yr = 2005))
    if (manip$when>2006) manip <- within(manip, y2006 <- tmp.proj(baseyr = 2005, yr = 2006))
    if (manip$when>2007) manip <- within(manip, y2007 <- tmp.proj(baseyr = 2005, yr = 2007))
    if (manip$when>2008) manip <- within(manip, y2008 <- tmp.proj(baseyr = 2005, yr = 2008))
    if (manip$when>2009) manip <- within(manip, y2009 <- tmp.proj(baseyr = 2005, yr = 2009))
    # return to data
    manip <- within(manip, times.manip <- times.manip + 1)
    eq2[i,] <- manip
    # indicate manipulation
    manip <- eq3[i,] 
    manip <- within(manip, {
        action  <- "";      when  <- NA;    orig.dest  <- "";
    })
    eq3[i,] <- manip
}
# verify if any unmanip left
with(eq3, table(pre10=when<2010, action, useNA = "always"))

# 2010-20 here
# re-do vectors picking census column that is not NA
#aorb05 <- apply(X=eq2[,c("cen_2005","cen_2005b","y2005")], MARGIN=1, FUN=function(X)
#    ifelse(!is.na(X[1]), X[1], ifelse(!is.na(X[2]), X[2], X[3]))) # try b if pop2010 is NA
aorb10 <- apply(X=eq2[,c("cen_2010","cen_2010b","y2010")], MARGIN=1, FUN=function(X)
    ifelse(!is.na(X[1]), X[1], ifelse(!is.na(X[2]), X[2], X[3]))) # try b if pop2010 is NA
aorb20 <- apply(X=eq2[,c("cen_2020","cen_2020b","y2020")], MARGIN=1, FUN=function(X)
    ifelse(!is.na(X[1]), X[1], ifelse(!is.na(X[2]), X[2], X[3]))) # try b if pop2010 is NA
##################################
## single change secciones 2010 ##
## resecc 2010 before census    ##
##################################
#############################
## split.to and split.from ##
#############################
sel <- which(eq3$action=="split.to" & eq3$when==2010)
## sel <- which(eq2$edosecn %in% c(10081,10594:10607))
## eq2$cen_2010[sel]
for (i in sel){
    #i <- sel[1]
    manip <- eq2[i,] # duplicate obs for manipulation
    vec <- eval(parse(text = manip$orig.dest))
    vec <- manip$edon*10000 + vec   # turn into edosecn
    manip$case <- ifelse(manip$edosecn %in% vec, "no.baja", "baja") # did seccion survive the split? (baja means no) 
    vec <- which(eq2$edosecn %in% vec) # turn to indices
    vec.plus  <- union(vec, i)   # seccion and its targets (in case vec excludes dropped seccion) 
    vec.minus <- setdiff(vec, i) # targets w/o seccion (for use when seccion was dropped)
    # aggregate split populations
    manip$cen_2010b <- with(eq2, sum(aorb10[vec]))
    manip$cen_2020b <- with(eq2, sum(aorb20[vec]))
    #
    manip <- within(manip, {y2005 <- cen_2005; y2010 <- cen_2010b; y2020 <- cen_2020b}) # use aggregates to compute pi
    # 2005-10 needed since 2010 was missing in first pass
    manip <- within(manip, pi <-  (y2010 - y2005) / (2010 - 2005) ); # rate of change
    # re-project pre-2010 where needed
    if (manip$when>1997) manip <- within(manip, y1997 <- tmp.proj(baseyr = 2005, yr = 1997))
    if (manip$when>1998) manip <- within(manip, y1998 <- tmp.proj(baseyr = 2005, yr = 1998))
    if (manip$when>1999) manip <- within(manip, y1999 <- tmp.proj(baseyr = 2005, yr = 1999))
    if (manip$when>2000) manip <- within(manip, y2000 <- tmp.proj(baseyr = 2005, yr = 2000))
    if (manip$when>2001) manip <- within(manip, y2001 <- tmp.proj(baseyr = 2005, yr = 2001))
    if (manip$when>2002) manip <- within(manip, y2002 <- tmp.proj(baseyr = 2005, yr = 2002))
    if (manip$when>2003) manip <- within(manip, y2003 <- tmp.proj(baseyr = 2005, yr = 2003))
    if (manip$when>2004) manip <- within(manip, y2004 <- tmp.proj(baseyr = 2005, yr = 2004))
    if (manip$when>2005) manip <- within(manip, y2005 <- tmp.proj(baseyr = 2005, yr = 2005))
    if (manip$when>2006) manip <- within(manip, y2006 <- tmp.proj(baseyr = 2005, yr = 2006))
    if (manip$when>2007) manip <- within(manip, y2007 <- tmp.proj(baseyr = 2005, yr = 2007))
    if (manip$when>2008) manip <- within(manip, y2008 <- tmp.proj(baseyr = 2005, yr = 2008))
    if (manip$when>2009) manip <- within(manip, y2009 <- tmp.proj(baseyr = 2005, yr = 2009))
    # now pi for 2010-20
    manip <- within(manip, pi <-  (y2020 - y2010) / (2020 - 2010) ); # rate of change
    tmp <- manip$pi # save pi for new secciones
    # indicate manip pt1 and return to data
    manip <- within(manip, {
        times.manip <- times.manip + 1;
    })
    eq2[i,] <- manip
    # indicate manip pt2
    manip <- eq3[i,]
    manip <- within(manip, {
        action  <- "";      when  <- NA;    orig.dest  <- "";
    })
    eq3[i,] <- manip
    #
    # fix proj in non-existing secciones and return manipulation to data
    # 2010 chg need no manip, just indicate it's done
    manip <- eq3[vec.minus,]
    manip <- within(manip, {
        y1997 <- y1998 <- y1999 <- NA;
        y2000 <- y2001 <- y2002 <- y2003 <- y2004 <- y2005 <- y2006 <- y2007 <- y2008 <- y2009 <- NA;
        action  <- "";      when  <- NA;    orig.dest  <- "";
    })
    eq3[vec.minus,] <- manip
}

######################
## post2010 pre2020 ##
######################
sel <- which(eq3$action=="split.to" & eq3$when>2010 & eq3$when<2020)
for (i in sel){
    #i <- sel[6]
    manip <- eq2[i,] # duplicate obs for manipulation
    vec <- eval(parse(text = manip$orig.dest))
    vec <- manip$edon*10000 + vec   # turn into edosecn
    manip$case <- ifelse(manip$edosecn %in% vec, "no.baja", "baja") # did seccion survive the split? (baja means no) 
    vec <- which(eq2$edosecn %in% vec) # turn to indices
    vec.plus  <- union(vec, i)   # seccion and its targets (in case vec excludes dropped seccion) 
    vec.minus <- setdiff(vec, i) # targets w/o seccion (for use when seccion was dropped)
    # aggregate split populations
    #manip$cen_2010b <- with(eq2, sum(aorb10[vec]))
    manip$cen_2020b <- with(eq2, sum(aorb20[vec]))
    manip <- within(manip, {y2020 <- cen_2020b}) # use aggregates to compute pi
    manip <- within(manip, pi <-  (y2020 - y2010) / (2020 - 2010) ); # rate of change
    tmp <- manip$pi # save pi for new secciones
    if (manip$when>2011) manip <- within(manip, y2011 <- tmp.proj(baseyr = 2010, yr = 2011))
    if (manip$when>2012) manip <- within(manip, y2012 <- tmp.proj(baseyr = 2010, yr = 2012))
    if (manip$when>2013) manip <- within(manip, y2013 <- tmp.proj(baseyr = 2010, yr = 2013))
    if (manip$when>2014) manip <- within(manip, y2014 <- tmp.proj(baseyr = 2010, yr = 2014))
    if (manip$when>2015) manip <- within(manip, y2015 <- tmp.proj(baseyr = 2010, yr = 2015))
    if (manip$when>2016) manip <- within(manip, y2016 <- tmp.proj(baseyr = 2010, yr = 2016))
    if (manip$when>2017) manip <- within(manip, y2017 <- tmp.proj(baseyr = 2010, yr = 2017))
    if (manip$when>2018) manip <- within(manip, y2018 <- tmp.proj(baseyr = 2010, yr = 2018))
    if (manip$when>2019) manip <- within(manip, y2019 <- tmp.proj(baseyr = 2010, yr = 2019))
    #if (manip$when>2021) manip <- within(manip, y2021 <- tmp.proj(baseyr = 2010, yr = 2021))
    #if (manip$when>2022) manip <- within(manip, y2022 <- tmp.proj(baseyr = 2010, yr = 2022))
    # indicate manip pt1 and return to data
    manip <- within(manip, {
        times.manip <- times.manip + 1;
    })
    eq2[i,] <- manip
    # indicate manip pt2
    manip <- eq3[i,]
    manip <- within(manip, {
        action  <- "";      when  <- NA;    orig.dest  <- "";
    })
    eq3[i,] <- manip
    #
    # fix proj in non-existing secciones and return manipulation to data
    # 2010 chg need no manip, just indicate it's done
    manip <- eq2[vec.minus,]
    manip$pi <- tmp # paste agg's rate of change
    if (eq2$when[i]<=2011) manip <- within(manip, y2011 <- tmp.proj(baseyr = 2020, yr = 2011))
    if (eq2$when[i]<=2012) manip <- within(manip, y2012 <- tmp.proj(baseyr = 2020, yr = 2012))
    if (eq2$when[i]<=2013) manip <- within(manip, y2013 <- tmp.proj(baseyr = 2020, yr = 2013))
    if (eq2$when[i]<=2014) manip <- within(manip, y2014 <- tmp.proj(baseyr = 2020, yr = 2014))
    if (eq2$when[i]<=2015) manip <- within(manip, y2015 <- tmp.proj(baseyr = 2020, yr = 2015))
    if (eq2$when[i]<=2016) manip <- within(manip, y2016 <- tmp.proj(baseyr = 2020, yr = 2016))
    if (eq2$when[i]<=2017) manip <- within(manip, y2017 <- tmp.proj(baseyr = 2020, yr = 2017))
    if (eq2$when[i]<=2018) manip <- within(manip, y2018 <- tmp.proj(baseyr = 2020, yr = 2018))
    if (eq2$when[i]<=2019) manip <- within(manip, y2019 <- tmp.proj(baseyr = 2020, yr = 2019))
    if (eq2$when[i]<=2021) manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
    if (eq2$when[i]<=2022) manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
    manip[1,]
    # return to data
    manip <- within(manip, {
        times.manip <- times.manip + 1;
    })
    eq2[vec.minus,] <- manip
    # indicate manip in new secciones and return to data
    manip <- eq3[vec.minus,]
    manip <- within(manip, {
        action  <- "";      when  <- NA;    orig.dest  <- "";
    })
    eq3[vec.minus,] <- manip
}


#########################
## merged.to 2010:2020 ##
#########################
sel <- which(eq3$action=="merged.to" & eq3$when>2010 & eq3$when<2020)
for (i in sel){
    #i <- sel[1]
    manip <- eq2[i,] # duplicate obs for manipulation
    vec <- eval(parse(text = manip$orig.dest))
    vec <- manip$edon*10000 + vec   # turn into edosecn
    vec <- which(eq2$edosecn %in% vec) # turn to indices
    # expand manip to include receiving sección
    vec <- unique(union(i, vec))
    vec.minus <- setdiff(vec,i)
    manip <- eq2[vec,] # duplicate obs for manipulation
    # aggregate split populations
    ## manip$cen_2010b <- with(eq2, sum(aorb10[vec]))
    ## manip$cen_2020b <- with(eq2, sum(aorb20[vec]))
    manip$cen_2010b <- sum(eq2$cen_2010[vec], na.rm = TRUE)
    manip$cen_2020b <- sum(eq2$cen_2020[vec], na.rm = TRUE)
    manip <- within(manip, pi <-  (cen_2020b - cen_2010b) / (2020 - 2010) ); # rate of change
    if (manip$when[1]>2011) manip <- within(manip, y2011 <- tmp.proj(baseyr = 2010, yr = 2011))
    if (manip$when[1]>2012) manip <- within(manip, y2012 <- tmp.proj(baseyr = 2010, yr = 2012))
    if (manip$when[1]>2013) manip <- within(manip, y2013 <- tmp.proj(baseyr = 2010, yr = 2013))
    if (manip$when[1]>2014) manip <- within(manip, y2014 <- tmp.proj(baseyr = 2010, yr = 2014))
    if (manip$when[1]>2015) manip <- within(manip, y2015 <- tmp.proj(baseyr = 2010, yr = 2015))
    if (manip$when[1]>2016) manip <- within(manip, y2016 <- tmp.proj(baseyr = 2010, yr = 2016))
    if (manip$when[1]>2017) manip <- within(manip, y2017 <- tmp.proj(baseyr = 2010, yr = 2017))
    if (manip$when[1]>2018) manip <- within(manip, y2018 <- tmp.proj(baseyr = 2010, yr = 2018))
    if (manip$when[1]>2019) manip <- within(manip, y2019 <- tmp.proj(baseyr = 2010, yr = 2019))
    # fix receiving end (NAs reemerge in vanishing sección)
    if (manip$when[1]<=2011) manip <- within(manip, y2011 <- tmp.proj(baseyr = 2020, yr = 2011))
    if (manip$when[1]<=2012) manip <- within(manip, y2012 <- tmp.proj(baseyr = 2020, yr = 2012))
    if (manip$when[1]<=2013) manip <- within(manip, y2013 <- tmp.proj(baseyr = 2020, yr = 2013))
    if (manip$when[1]<=2014) manip <- within(manip, y2014 <- tmp.proj(baseyr = 2020, yr = 2014))
    if (manip$when[1]<=2015) manip <- within(manip, y2015 <- tmp.proj(baseyr = 2020, yr = 2015))
    if (manip$when[1]<=2016) manip <- within(manip, y2016 <- tmp.proj(baseyr = 2020, yr = 2016))
    if (manip$when[1]<=2017) manip <- within(manip, y2017 <- tmp.proj(baseyr = 2020, yr = 2017))
    if (manip$when[1]<=2018) manip <- within(manip, y2018 <- tmp.proj(baseyr = 2020, yr = 2018))
    if (manip$when[1]<=2019) manip <- within(manip, y2019 <- tmp.proj(baseyr = 2020, yr = 2019))
    # return to data
    manip <- within(manip, {
        times.manip <- times.manip + 1;
    })
    eq2[vec,] <- manip
    # indicate manip
    manip <- eq3[vec,]
    manip <- within(manip, {
        action  <- "";      when  <- NA;    orig.dest  <- "";
    })
    eq3[vec,] <- manip
}

# merged.to 2010 need 1-on-1 manip bec 3 merged to a splitting seccion
sel <- which(eq3$action=="merged.to" & eq3$when==2010)
eq2[sel,c(1,6:13)]
# fix 21369
sel <- which(eq3$edosecn %in% c(21294,21369,21738:21806)) #21786
manip <- eq2[sel,] # duplicate obs for manipulation
## sel <- eval(parse(text = manip$orig.dest))
## sel <- manip$edon*10000 + sel   # turn into edosecn
## sel <- which(eq2$edosecn %in% sel) # turn to indices
# aggregate split populations
## manip$cen_2010b <- with(eq2, sum(aorb10[sel]))
## manip$cen_2020b <- with(eq2, sum(aorb20[sel]))
manip$cen_2005b <- sum(eq2$cen_2005[sel], na.rm = TRUE)
manip$cen_2010b <- sum(eq2$cen_2010[sel], na.rm = TRUE)
manip$cen_2020b <- sum(eq2$cen_2020[sel], na.rm = TRUE)
manip <- within(manip, pi <-  (cen_2020b - cen_2010b) / (2020 - 2010) ); # rate of change
# return to data for piecemeal manip
eq2[sel,] <- manip
# 1369
manip <- eq2[sel[2],]
manip <- within(manip, y1997 <- tmp.proj(baseyr = 2005, yr = 1997))
manip <- within(manip, y1998 <- tmp.proj(baseyr = 2005, yr = 1998))
manip <- within(manip, y1999 <- tmp.proj(baseyr = 2005, yr = 1999))
manip <- within(manip, y2000 <- tmp.proj(baseyr = 2005, yr = 2000))
manip <- within(manip, y2001 <- tmp.proj(baseyr = 2005, yr = 2001))
manip <- within(manip, y2002 <- tmp.proj(baseyr = 2005, yr = 2002))
manip <- within(manip, y2003 <- tmp.proj(baseyr = 2005, yr = 2003))
manip <- within(manip, y2004 <- tmp.proj(baseyr = 2005, yr = 2004))
manip <- within(manip, y2006 <- tmp.proj(baseyr = 2005, yr = 2006))
manip <- within(manip, y2007 <- tmp.proj(baseyr = 2005, yr = 2007))
manip <- within(manip, y2008 <- tmp.proj(baseyr = 2005, yr = 2008))
manip <- within(manip, y2009 <- tmp.proj(baseyr = 2005, yr = 2009))
# return to data
manip <- within(manip, {
    times.manip <- times.manip + 1;
})
eq2[sel,] <- manip
# indicate manip
manip <- eq3[sel,]
manip <- within(manip, {
    action  <- "";      when  <- NA;    orig.dest  <- "";
    action2  <- "";      when2  <- NA;    orig.dest2  <- "";
    action3  <- "";      when3  <- NA;    orig.dest3  <- "";
})
eq3[sel,] <- manip
#
# fix 10776 777 119 
sel <- which(eq3$edosecn %in% c(100776,100777,100779,101392,101393))
manip <- eq2[sel,] # duplicate obs for manipulation
# aggregate split populations
## manip$cen_2010b <- with(eq2, sum(aorb10[sel]))
## manip$cen_2020b <- with(eq2, sum(aorb20[sel]))
manip$cen_2005b <- sum(eq2$cen_2005[sel], na.rm = TRUE)
manip$cen_2010b <- sum(eq2$cen_2010[sel], na.rm = TRUE)
manip$cen_2020b <- sum(eq2$cen_2020[sel], na.rm = TRUE)
manip <- within(manip, pi <-  (cen_2020b - cen_2010b) / (2020 - 2010) ); # rate of change
# return to data for piecemeal manip
eq2[sel,] <- manip
# pre-2010
manip <- eq2[sel[1:3],]
manip <- within(manip, y1997 <- tmp.proj(baseyr = 2005, yr = 1997))
manip <- within(manip, y1998 <- tmp.proj(baseyr = 2005, yr = 1998))
manip <- within(manip, y1999 <- tmp.proj(baseyr = 2005, yr = 1999))
manip <- within(manip, y2000 <- tmp.proj(baseyr = 2005, yr = 2000))
manip <- within(manip, y2001 <- tmp.proj(baseyr = 2005, yr = 2001))
manip <- within(manip, y2002 <- tmp.proj(baseyr = 2005, yr = 2002))
manip <- within(manip, y2003 <- tmp.proj(baseyr = 2005, yr = 2003))
manip <- within(manip, y2004 <- tmp.proj(baseyr = 2005, yr = 2004))
manip <- within(manip, y2006 <- tmp.proj(baseyr = 2005, yr = 2006))
manip <- within(manip, y2007 <- tmp.proj(baseyr = 2005, yr = 2007))
manip <- within(manip, y2008 <- tmp.proj(baseyr = 2005, yr = 2008))
manip <- within(manip, y2009 <- tmp.proj(baseyr = 2005, yr = 2009))
manip <- within(manip, y2010 <- NA)
manip <- within(manip, y2020 <- NA)
# return to data
manip <- within(manip, {
    times.manip <- times.manip + 1;
})
eq2[sel[1:3],] <- manip
# indicate manip
manip <- eq3[sel,]
manip <- within(manip, {
    action  <- "";      when  <- NA;    orig.dest  <- "";
    action2  <- "";      when2  <- NA;    orig.dest2  <- "";
    action3  <- "";      when3  <- NA;    orig.dest3  <- "";
})
eq3[sel,] <- manip
#
# fix 152716 2717
sel <- which(eq3$edosecn %in% c(152716,152717))
manip <- eq2[sel,] # duplicate obs for manipulation
# aggregate split populations
manip$cen_2005b <- 318 # use 2006 lisnom in 2717
manip$cen_2010b <- sum(eq2$cen_2010[sel], na.rm = TRUE)
manip$y2010[1] <- 1 # 2716 not in conteo nor elec2006
manip <- within(manip, pi <-  (cen_2010b - cen_2005b) / (2010 - 2005) ); # rate of change
manip <- within(manip, y1997 <- tmp.proj(baseyr = 2010, yr = 1997))
manip <- within(manip, y1998 <- tmp.proj(baseyr = 2010, yr = 1998))
manip <- within(manip, y1999 <- tmp.proj(baseyr = 2010, yr = 1999))
manip <- within(manip, y2000 <- tmp.proj(baseyr = 2010, yr = 2000))
manip <- within(manip, y2001 <- tmp.proj(baseyr = 2010, yr = 2001))
manip <- within(manip, y2002 <- tmp.proj(baseyr = 2010, yr = 2002))
manip <- within(manip, y2003 <- tmp.proj(baseyr = 2010, yr = 2003))
manip <- within(manip, y2004 <- tmp.proj(baseyr = 2010, yr = 2004))
manip <- within(manip, y2005 <- tmp.proj(baseyr = 2010, yr = 2005))
manip <- within(manip, y2006 <- tmp.proj(baseyr = 2010, yr = 2006))
manip <- within(manip, y2007 <- tmp.proj(baseyr = 2010, yr = 2007))
manip <- within(manip, y2008 <- tmp.proj(baseyr = 2010, yr = 2008))
manip <- within(manip, y2009 <- tmp.proj(baseyr = 2010, yr = 2009))
# return to data
manip <- within(manip, {
    times.manip <- times.manip + 1;
})
eq2[sel,] <- manip
# indicate manip
manip <- eq3[sel,]
manip <- within(manip, {
    action  <- "";      when  <- NA;    orig.dest  <- "";
    action2  <- "";      when2  <- NA;    orig.dest2  <- "";
    action3  <- "";      when3  <- NA;    orig.dest3  <- "";
})
eq3[sel,] <- manip

##########
## 2020 ##
##########
sel <- which(eq3$action=="split.to" & eq3$when==2020)
for (i in sel){
    #i <- sel[1]
    manip <- eq2[i,] # duplicate obs for manipulation
    vec <- eval(parse(text = manip$orig.dest))
    vec <- manip$edon*10000 + vec   # turn into edosecn
    manip$case <- ifelse(manip$edosecn %in% vec, "no.baja", "baja") # did seccion survive the split? (baja means no) 
    vec <- which(eq2$edosecn %in% vec) # turn to indices
    vec.plus  <- union(i,vec)   # seccion and its targets (in case vec excludes dropped seccion) 
    vec.minus <- setdiff(vec, i) # targets w/o seccion (for use when seccion was dropped)
    # aggregate split populations
    #manip$cen_2010b <- with(eq2, sum(aorb10[vec]))
    manip$cen_2010b <- sum(eq2$cen_2010[vec.plus], na.rm = TRUE)
    manip$cen_2020b <- sum(eq2$cen_2020[vec.plus], na.rm = TRUE)
    manip <- within(manip, pi <-  (cen_2020b - cen_2010) / (2020 - 2010) ); # rate of change
    tmp <- manip$pi # save to paste across the board
    # expand to include new secciones
    manip <- eq2[vec.plus,] # duplicate obs for manipulation
    manip$pi <- tmp
    # fix 2011:19
    manip <- within(manip, y2011 <- tmp.proj(baseyr = 2010, yr = 2011))
    manip <- within(manip, y2012 <- tmp.proj(baseyr = 2010, yr = 2012))
    manip <- within(manip, y2013 <- tmp.proj(baseyr = 2010, yr = 2013))
    manip <- within(manip, y2014 <- tmp.proj(baseyr = 2010, yr = 2014))
    manip <- within(manip, y2015 <- tmp.proj(baseyr = 2010, yr = 2015))
    manip <- within(manip, y2016 <- tmp.proj(baseyr = 2010, yr = 2016))
    manip <- within(manip, y2017 <- tmp.proj(baseyr = 2010, yr = 2017))
    manip <- within(manip, y2018 <- tmp.proj(baseyr = 2010, yr = 2018))
    manip <- within(manip, y2019 <- tmp.proj(baseyr = 2010, yr = 2019))
    # fix 2021:22
    manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
    manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
    # return to data
    manip <- within(manip, {
        times.manip <- times.manip + 1;
    })
    eq2[vec.plus,] <- manip
    # indicate manip
    manip <- eq3[vec.plus,]
    manip <- within(manip, {
        action  <- "";      when  <- NA;    orig.dest  <- "";
#        action2  <- "";      when2  <- NA;    orig.dest2  <- "";
#        action3  <- "";      when3  <- NA;    orig.dest3  <- "";
    })
    eq3[vec.plus,] <- manip
}

## # Now check remaining split.from ??
## with(eq3, table(when, action, useNA = "always"))
## sel <- which(eq3$action=="?")
## eq2$edosecn[sel]
## eq2[sel,c(1,6:13)]
