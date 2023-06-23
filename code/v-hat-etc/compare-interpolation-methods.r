source("../../code/v-hat-etc/interpolate-census.r")
## default applies proper district/municipio map when generating interpolations
##
## ######################################
## ## Add p18 to district vote objects ##
## ######################################
## ## 1991
## tmp2 <- censod79[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=1991, unit="d")
## tmp2$pob18e <- interlog(what="p18", yr=1991, unit="d")
## tmp2$pob18l <- interlog(what="p18", yr=1991, unit="d", frm="dv~iv")
## v91d$pob182 <- tmp2$pob182 # paste interpolation
## v91d$pob18e <- tmp2$pob18e # paste interpolation
## v91d$pob18l <- tmp2$pob18l # paste interpolation
## ## 1994
## tmp2 <- censod79[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=1994, unit="d")
## tmp2$pob18e <- interlog(what="p18", yr=1994, unit="d")
## tmp2$pob18l <- interlog(what="p18", yr=1994, unit="d", frm="dv~iv")
## ##v94d$disn==tmp2$disn
## v94d$pob182 <- tmp2$pob182 # paste interpolation
## v94d$pob18e <- tmp2$pob18e # paste interpolation
## v94d$pob18l <- tmp2$pob18l # paste interpolation
## ## 1994d97
## tmp2 <- censod97[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=1994, unit="d", census.data = censod97)
## tmp2$pob18e <- interlog(what="p18", yr=1994, unit="d", census.data = censod97)
## tmp2$pob18l <- interlog(what="p18", yr=1994, unit="d", census.data = censod97, frm="dv~iv")
## v94d97$pob182 <- tmp2$pob182 # paste interpolation
## v94d97$pob18e <- tmp2$pob18e # paste interpolation
## v94d97$pob18l <- tmp2$pob18l # paste interpolation
## ## 1994d06
## tmp2 <- censod06[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=1994, unit="d", census.data = censod06)
## tmp2$pob18e <- interlog(what="p18", yr=1994, unit="d", census.data = censod06)
## tmp2$pob18l <- interlog(what="p18", yr=1994, unit="d", census.data = censod06, frm="dv~iv")
## v94d06$pob182 <- tmp2$pob182 # paste interpolation
## v94d06$pob18e <- tmp2$pob18e # paste interpolation
## v94d06$pob18l <- tmp2$pob18l # paste interpolation
## ## 1994d18
## tmp2 <- censod18[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=1994, unit="d", census.data = censod18)
## tmp2$pob18e <- interlog(what="p18", yr=1994, unit="d", census.data = censod18)
## tmp2$pob18l <- interlog(what="p18", yr=1994, unit="d", census.data = censod18, frm="dv~iv")
## v94d18$pob182 <- tmp2$pob182 # paste interpolation
## v94d18$pob18e <- tmp2$pob18e # paste interpolation
## v94d18$pob18l <- tmp2$pob18l # paste interpolation
## ## 1997
## tmp2 <- censod97[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=1997, unit="d")
## tmp2$pob18e <- interlog(what="p18", yr=1997, unit="d")
## tmp2$pob18l <- interlog(what="p18", yr=1997, unit="d", frm="dv~iv")
## v97d$pob182 <- tmp2$pob182 # paste interpolation
## v97d$pob18e <- tmp2$pob18e # paste interpolation
## v97d$pob18l <- tmp2$pob18l # paste interpolation
## ## 1997d79
## tmp2 <- censod79[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=1997, unit="d", census.data = censod79)
## tmp2$pob18e <- interlog(what="p18", yr=1997, unit="d", census.data = censod79)
## tmp2$pob18l <- interlog(what="p18", yr=1997, unit="d", census.data = censod79, frm="dv~iv")
## v97d79$pob182 <- tmp2$pob182 # paste interpolation
## v97d79$pob18e <- tmp2$pob18e # paste interpolation
## v97d79$pob18l <- tmp2$pob18l # paste interpolation
## ## 1997d06
## tmp2 <- censod06[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=1997, unit="d", census.data = censod06)
## tmp2$pob18e <- interlog(what="p18", yr=1997, unit="d", census.data = censod06)
## tmp2$pob18l <- interlog(what="p18", yr=1997, unit="d", census.data = censod06, frm="dv~iv")
## v97d06$pob182 <- tmp2$pob182 # paste interpolation
## v97d06$pob18e <- tmp2$pob18e # paste interpolation
## v97d06$pob18l <- tmp2$pob18l # paste interpolation
## ## 1997d18
## tmp2 <- censod18[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=1997, unit="d", census.data = censod18)
## tmp2$pob18e <- interlog(what="p18", yr=1997, unit="d", census.data = censod18)
## tmp2$pob18l <- interlog(what="p18", yr=1997, unit="d", census.data = censod18, frm="dv~iv")
## v97d18$pob182 <- tmp2$pob182 # paste interpolation
## v97d18$pob18e <- tmp2$pob18e # paste interpolation
## v97d18$pob18l <- tmp2$pob18l # paste interpolation
## ## 2000
## tmp2 <- censod97[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=2000, unit="d")
## tmp2$pob18e <- interlog(what="p18", yr=2000, unit="d")
## tmp2$pob18l <- interlog(what="p18", yr=2000, unit="d", frm="dv~iv")
## v00d$pob182 <- tmp2$pob182 # paste interpolation
## v00d$pob18e <- tmp2$pob18e # paste interpolation
## v00d$pob18l <- tmp2$pob18l # paste interpolation
## ## 2000d79
## tmp2 <- censod79[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=2000, unit="d", census.data = censod79)
## tmp2$pob18e <- interlog(what="p18", yr=2000, unit="d", census.data = censod79)
## tmp2$pob18l <- interlog(what="p18", yr=2000, unit="d", census.data = censod79, frm="dv~iv")
## v00d79$pob182 <- tmp2$pob182 # paste interpolation
## v00d79$pob18e <- tmp2$pob18e # paste interpolation
## v00d79$pob18l <- tmp2$pob18l # paste interpolation
## ## 2000d06
## tmp2 <- censod06[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=2000, unit="d", census.data = censod06)
## tmp2$pob18e <- interlog(what="p18", yr=2000, unit="d", census.data = censod06)
## tmp2$pob18l <- interlog(what="p18", yr=2000, unit="d", census.data = censod06, frm="dv~iv")
## v00d06$pob182 <- tmp2$pob182 # paste interpolation
## v00d06$pob18e <- tmp2$pob18e # paste interpolation
## v00d06$pob18l <- tmp2$pob18l # paste interpolation
## ## 2000d18
## tmp2 <- censod18[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=2000, unit="d", census.data = censod18)
## tmp2$pob18e <- interlog(what="p18", yr=2000, unit="d", census.data = censod18)
## tmp2$pob18l <- interlog(what="p18", yr=2000, unit="d", census.data = censod18, frm="dv~iv")
## v00d18$pob182 <- tmp2$pob182 # paste interpolation
## v00d18$pob18e <- tmp2$pob18e # paste interpolation
## v00d18$pob18l <- tmp2$pob18l # paste interpolation
## ## 2003
## tmp2 <- censod97[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=2003, unit="d")
## tmp2$pob18e <- interlog(what="p18", yr=2003, unit="d")
## tmp2$pob18l <- interlog(what="p18", yr=2003, unit="d", frm="dv~iv")
## v03d$pob182 <- tmp2$pob182 # paste interpolation
## v03d$pob18e <- tmp2$pob18e # paste interpolation
## v03d$pob18l <- tmp2$pob18l # paste interpolation
## ## 2003d79
## tmp2 <- censod79[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=2003, unit="d", census.data = censod79)
## tmp2$pob18e <- interlog(what="p18", yr=2003, unit="d", census.data = censod79)
## tmp2$pob18l <- interlog(what="p18", yr=2003, unit="d", census.data = censod79, frm="dv~iv")
## v03d79$pob182 <- tmp2$pob182 # paste interpolation
## v03d79$pob18e <- tmp2$pob18e # paste interpolation
## v03d79$pob18l <- tmp2$pob18l # paste interpolation
## ## 2003d06
## tmp2 <- censod06[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=2003, unit="d", census.data = censod06)
## tmp2$pob18e <- interlog(what="p18", yr=2003, unit="d", census.data = censod06)
## tmp2$pob18l <- interlog(what="p18", yr=2003, unit="d", census.data = censod06, frm="dv~iv")
## v03d06$pob182 <- tmp2$pob182 # paste interpolation
## v03d06$pob18e <- tmp2$pob18e # paste interpolation
## v03d06$pob18l <- tmp2$pob18l # paste interpolation
## ## 2003d18
## tmp2 <- censod18[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=2003, unit="d", census.data = censod18)
## tmp2$pob18e <- interlog(what="p18", yr=2003, unit="d", census.data = censod18)
## tmp2$pob18l <- interlog(what="p18", yr=2003, unit="d", census.data = censod18, frm="dv~iv")
## v03d18$pob182 <- tmp2$pob182 # paste interpolation
## v03d18$pob18e <- tmp2$pob18e # paste interpolation
## v03d18$pob18l <- tmp2$pob18l # paste interpolation
## ## 2006
## tmp2 <- censod06[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=2006, unit="d")
## tmp2$pob18e <- interlog(what="p18", yr=2006, unit="d")
## tmp2$pob18l <- interlog(what="p18", yr=2006, unit="d", frm="dv~iv")
## v06d$pob182 <- tmp2$pob182 # paste interpolation
## v06d$pob18e <- tmp2$pob18e # paste interpolation
## v06d$pob18l <- tmp2$pob18l # paste interpolation
## ## 2006d79
## tmp2 <- censod79[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=2006, unit="d", census.data = censod79)
## tmp2$pob18e <- interlog(what="p18", yr=2006, unit="d", census.data = censod79)
## tmp2$pob18l <- interlog(what="p18", yr=2006, unit="d", census.data = censod79, frm="dv~iv")
## v06d79$pob182 <- tmp2$pob182 # paste interpolation
## v06d79$pob18e <- tmp2$pob18e # paste interpolation
## v06d79$pob18l <- tmp2$pob18l # paste interpolation
## ## 2006d97
## tmp2 <- censod97[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=2006, unit="d", census.data = censod97)
## tmp2$pob18e <- interlog(what="p18", yr=2006, unit="d", census.data = censod97)
## tmp2$pob18l <- interlog(what="p18", yr=2006, unit="d", census.data = censod97, frm="dv~iv")
## v06d97$pob182 <- tmp2$pob182 # paste interpolation
## v06d97$pob18e <- tmp2$pob18e # paste interpolation
## v06d97$pob18l <- tmp2$pob18l # paste interpolation
## ## 2006d18
## tmp2 <- censod18[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=2006, unit="d", census.data = censod18)
## tmp2$pob18e <- interlog(what="p18", yr=2006, unit="d", census.data = censod18)
## tmp2$pob18l <- interlog(what="p18", yr=2006, unit="d", census.data = censod18, frm="dv~iv")
## v06d18$pob182 <- tmp2$pob182 # paste interpolation
## v06d18$pob18e <- tmp2$pob18e # paste interpolation
## v06d18$pob18l <- tmp2$pob18l # paste interpolation
## ## 2009
## tmp2 <- censod06[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=2009, unit="d")
## tmp2$pob18e <- interlog(what="p18", yr=2009, unit="d")
## tmp2$pob18l <- interlog(what="p18", yr=2009, unit="d", frm="dv~iv")
## v09d$pob182 <- tmp2$pob182 # paste interpolation
## v09d$pob18e <- tmp2$pob18e # paste interpolation
## v09d$pob18l <- tmp2$pob18l # paste interpolation
## ## 2006d79
## tmp2 <- censod79[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=2009, unit="d", census.data = censod79)
## tmp2$pob18e <- interlog(what="p18", yr=2009, unit="d", census.data = censod79)
## tmp2$pob18l <- interlog(what="p18", yr=2009, unit="d", census.data = censod79, frm="dv~iv")
## v09d79$pob182 <- tmp2$pob182 # paste interpolation
## v09d79$pob18e <- tmp2$pob18e # paste interpolation
## v09d79$pob18l <- tmp2$pob18l # paste interpolation
## ## 2009d97
## tmp2 <- censod97[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=2009, unit="d", census.data = censod97)
## tmp2$pob18e <- interlog(what="p18", yr=2009, unit="d", census.data = censod97)
## tmp2$pob18l <- interlog(what="p18", yr=2009, unit="d", census.data = censod97, frm="dv~iv")
## v09d97$pob182 <- tmp2$pob182 # paste interpolation
## v09d97$pob18e <- tmp2$pob18e # paste interpolation
## v09d97$pob18l <- tmp2$pob18l # paste interpolation
## ## 2009d18
## tmp2 <- censod18[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=2009, unit="d", census.data = censod18)
## tmp2$pob18e <- interlog(what="p18", yr=2009, unit="d", census.data = censod18)
## tmp2$pob18l <- interlog(what="p18", yr=2009, unit="d", census.data = censod18, frm="dv~iv")
## v09d18$pob182 <- tmp2$pob182 # paste interpolation
## v09d18$pob18e <- tmp2$pob18e # paste interpolation
## v09d18$pob18l <- tmp2$pob18l # paste interpolation
## ## 2012
## tmp2 <- censod06[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=2012, unit="d")
## tmp2$pob18e <- interlog(what="p18", yr=2012, unit="d")
## tmp2$pob18l <- interlog(what="p18", yr=2012, unit="d", frm="dv~iv")
## v12d$pob182 <- tmp2$pob182 # paste interpolation
## v12d$pob18e <- tmp2$pob18e # paste interpolation
## v12d$pob18l <- tmp2$pob18l # paste interpolation
## ## 2012d79
## tmp2 <- censod79[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=2012, unit="d", census.data = censod79)
## tmp2$pob18e <- interlog(what="p18", yr=2012, unit="d", census.data = censod79)
## tmp2$pob18l <- interlog(what="p18", yr=2012, unit="d", census.data = censod79, frm="dv~iv")
## v12d79$pob182 <- tmp2$pob182 # paste interpolation
## v12d79$pob18e <- tmp2$pob18e # paste interpolation
## v12d79$pob18l <- tmp2$pob18l # paste interpolation
## ## 2012d97
## tmp2 <- censod97[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=2012, unit="d", census.data = censod97)
## tmp2$pob18e <- interlog(what="p18", yr=2012, unit="d", census.data = censod97)
## tmp2$pob18l <- interlog(what="p18", yr=2012, unit="d", census.data = censod97, frm="dv~iv")
## v12d97$pob182 <- tmp2$pob182 # paste interpolation
## v12d97$pob18e <- tmp2$pob18e # paste interpolation
## v12d97$pob18l <- tmp2$pob18l # paste interpolation
## ## 2012d18
## tmp2 <- censod18[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=2012, unit="d", census.data = censod18)
## tmp2$pob18e <- interlog(what="p18", yr=2012, unit="d", census.data = censod18)
## tmp2$pob18l <- interlog(what="p18", yr=2012, unit="d", census.data = censod18, frm="dv~iv")
## v12d18$pob182 <- tmp2$pob182 # paste interpolation
## v12d18$pob18e <- tmp2$pob18e # paste interpolation
## v12d18$pob18l <- tmp2$pob18l # paste interpolation
## ## 2015
## tmp2 <- censod06[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=2015, unit="d")
## tmp2$pob18e <- interlog(what="p18", yr=2015, unit="d")
## tmp2$pob18l <- interlog(what="p18", yr=2015, unit="d", frm="dv~iv")
## v15d$pob182 <- tmp2$pob182 # paste interpolation
## v15d$pob18e <- tmp2$pob18e # paste interpolation
## v15d$pob18l <- tmp2$pob18l # paste interpolation
## ## 2015d79
## tmp2 <- censod79[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=2015, unit="d", census.data = censod79)
## tmp2$pob18e <- interlog(what="p18", yr=2015, unit="d", census.data = censod79)
## tmp2$pob18l <- interlog(what="p18", yr=2015, unit="d", census.data = censod79, frm="dv~iv")
## v15d79$pob182 <- tmp2$pob182 # paste interpolation
## v15d79$pob18e <- tmp2$pob18e # paste interpolation
## v15d79$pob18l <- tmp2$pob18l # paste interpolation
## ## 2015d97
## tmp2 <- censod97[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=2015, unit="d", census.data = censod97)
## tmp2$pob18e <- interlog(what="p18", yr=2015, unit="d", census.data = censod97)
## tmp2$pob18l <- interlog(what="p18", yr=2015, unit="d", census.data = censod97, frm="dv~iv")
## v15d97$pob182 <- tmp2$pob182 # paste interpolation
## v15d97$pob18e <- tmp2$pob18e # paste interpolation
## v15d97$pob18l <- tmp2$pob18l # paste interpolation
## ## 2015d18
## tmp2 <- censod18[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=2015, unit="d", census.data = censod18)
## tmp2$pob18e <- interlog(what="p18", yr=2015, unit="d", census.data = censod18)
## tmp2$pob18l <- interlog(what="p18", yr=2015, unit="d", census.data = censod18, frm="dv~iv")
## v15d18$pob182 <- tmp2$pob182 # paste interpolation
## v15d18$pob18e <- tmp2$pob18e # paste interpolation
## v15d18$pob18l <- tmp2$pob18l # paste interpolation
## ## 2018
## tmp2 <- censod18[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=2018, unit="d")
## tmp2$pob18e <- interlog(what="p18", yr=2018, unit="d")
## tmp2$pob18l <- interlog(what="p18", yr=2018, unit="d", frm="dv~iv")
## v18d$pob182 <- tmp2$pob182 # paste interpolation
## v18d$pob18e <- tmp2$pob18e # paste interpolation
## v18d$pob18l <- tmp2$pob18l # paste interpolation
## ## 2018d79
## tmp2 <- censod79[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=2018, unit="d", census.data = censod79)
## tmp2$pob18e <- interlog(what="p18", yr=2018, unit="d", census.data = censod79)
## tmp2$pob18l <- interlog(what="p18", yr=2018, unit="d", census.data = censod79, frm="dv~iv")
## v18d79$pob182 <- tmp2$pob182 # paste interpolation
## v18d79$pob18e <- tmp2$pob18e # paste interpolation
## v18d79$pob18l <- tmp2$pob18l # paste interpolation
## ## 2018d97
## tmp2 <- censod97[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=2018, unit="d", census.data = censod97)
## tmp2$pob18e <- interlog(what="p18", yr=2018, unit="d", census.data = censod97)
## tmp2$pob18l <- interlog(what="p18", yr=2018, unit="d", census.data = censod97, frm="dv~iv")
## v18d97$pob182 <- tmp2$pob182 # paste interpolation
## v18d97$pob18e <- tmp2$pob18e # paste interpolation
## v18d97$pob18l <- tmp2$pob18l # paste interpolation
## ## 2018d06
## tmp2 <- censod06[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=2018, unit="d", census.data = censod06)
## tmp2$pob18e <- interlog(what="p18", yr=2018, unit="d", census.data = censod06)
## tmp2$pob18l <- interlog(what="p18", yr=2018, unit="d", census.data = censod06, frm="dv~iv")
## v18d06$pob182 <- tmp2$pob182 # paste interpolation
## v18d06$pob18e <- tmp2$pob18e # paste interpolation
## v18d06$pob18l <- tmp2$pob18l # paste interpolation
## ## 2021
## tmp2 <- censod18[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=2021, unit="d")
## tmp2$pob18e <- interlog(what="p18", yr=2021, unit="d")
## tmp2$pob18l <- interlog(what="p18", yr=2021, unit="d", frm="dv~iv")
## v21d$pob182 <- tmp2$pob182 # paste interpolation
## v21d$pob18e <- tmp2$pob18e # paste interpolation
## v21d$pob18l <- tmp2$pob18l # paste interpolation
## ## 2021d79
## tmp2 <- censod79[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=2021, unit="d", census.data = censod79)
## tmp2$pob18e <- interlog(what="p18", yr=2021, unit="d", census.data = censod79)
## tmp2$pob18l <- interlog(what="p18", yr=2021, unit="d", census.data = censod79, frm="dv~iv")
## v21d79$pob182 <- tmp2$pob182 # paste interpolation
## v21d79$pob18e <- tmp2$pob18e # paste interpolation
## v21d79$pob18l <- tmp2$pob18l # paste interpolation
## ## 2021d97
## tmp2 <- censod97[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=2021, unit="d", census.data = censod97)
## tmp2$pob18e <- interlog(what="p18", yr=2021, unit="d", census.data = censod97)
## tmp2$pob18l <- interlog(what="p18", yr=2021, unit="d", census.data = censod97, frm="dv~iv")
## v21d97$pob182 <- tmp2$pob182 # paste interpolation
## v21d97$pob18e <- tmp2$pob18e # paste interpolation
## v21d97$pob18l <- tmp2$pob18l # paste interpolation
## ## 2021d06
## tmp2 <- censod06[, c("edon","disn")]
## tmp2$pob182 <- interpol(what="p18", yr=2021, unit="d", census.data = censod06)
## tmp2$pob18e <- interlog(what="p18", yr=2021, unit="d", census.data = censod06)
## tmp2$pob18l <- interlog(what="p18", yr=2021, unit="d", census.data = censod06, frm="dv~iv")
## v21d06$pob182 <- tmp2$pob182 # paste interpolation
## v21d06$pob18e <- tmp2$pob18e # paste interpolation
## v21d06$pob18l <- tmp2$pob18l # paste interpolation

#######################################
## Add p18 to municipal vote objects ##
#######################################
## 1991
tmp2 <- censom94[, c("edon","inegi","ife")]
tmp2$pob182 <- interpol(what="p18", yr=1991, unit="m")
tmp2$pob18e <- interlog(what="p18", yr=1991, unit="m", frm="log(dv)~iv")
tmp2$pob18l <- interlog(what="p18", yr=1991, unit="m", frm="dv~iv")
v91m$pob182 <- tmp2$pob182 # paste interpolation
v91m$pob18e <- tmp2$pob18e # paste interpolation
v91m$pob18l <- tmp2$pob18l # paste interpolation
## ## 1994
## tmp2 <- censom94[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=1994, unit="m")
## tmp2$pob18e <- interlog(what="p18", yr=1994, unit="m", frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=1994, unit="m", frm="dv~iv")
## v94m$pob182 <- tmp2$pob182 # paste interpolation
## v94m$pob18e <- tmp2$pob18e # paste interpolation
## v94m$pob18l <- tmp2$pob18l # paste interpolation
## ## 1994m97
## tmp2 <- censom97[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=1994, unit="m", census.data = censom97)
## tmp2$pob18e <- interlog(what="p18", yr=1994, unit="m", census.data = censom97, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=1994, unit="m", census.data = censom97, frm="dv~iv")
## v94m97$pob182 <- tmp2$pob182 # paste interpolation
## v94m97$pob18e <- tmp2$pob18e # paste interpolation
## v94m97$pob18l <- tmp2$pob18l # paste interpolation
## 1994m00
tmp2 <- censom00[, c("edon","inegi","ife")]
tmp2$pob182 <- interpol(what="p18", yr=1994, unit="m", census.data = censom00)
tmp2$pob18e <- interlog(what="p18", yr=1994, unit="m", census.data = censom00, frm="log(dv)~iv")
tmp2$pob18l <- interlog(what="p18", yr=1994, unit="m", census.data = censom00, frm="dv~iv")
v94m00$pob182 <- tmp2$pob182 # paste interpolation
v94m00$pob18e <- tmp2$pob18e # paste interpolation
v94m00$pob18l <- tmp2$pob18l # paste interpolation
## ## 1994m03
## tmp2 <- censom03[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=1994, unit="m", census.data = censom03)
## tmp2$pob18e <- interlog(what="p18", yr=1994, unit="m", census.data = censom03, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=1994, unit="m", census.data = censom03, frm="dv~iv")
## v94m03$pob182 <- tmp2$pob182 # paste interpolation
## v94m03$pob18e <- tmp2$pob18e # paste interpolation
## v94m03$pob18l <- tmp2$pob18l # paste interpolation
## ## 1994m06
## tmp2 <- censom06[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=1994, unit="m", census.data = censom06)
## tmp2$pob18e <- interlog(what="p18", yr=1994, unit="m", census.data = censom06, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=1994, unit="m", census.data = censom06, frm="dv~iv")
## v94m06$pob182 <- tmp2$pob182 # paste interpolation
## v94m06$pob18e <- tmp2$pob18e # paste interpolation
## v94m06$pob18l <- tmp2$pob18l # paste interpolation
## ## 1994m09
## tmp2 <- censom09[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=1994, unit="m", census.data = censom09)
## tmp2$pob18e <- interlog(what="p18", yr=1994, unit="m", census.data = censom09, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=1994, unit="m", census.data = censom09, frm="dv~iv")
## v94m09$pob182 <- tmp2$pob182 # paste interpolation
## v94m09$pob18e <- tmp2$pob18e # paste interpolation
## v94m09$pob18l <- tmp2$pob18l # paste interpolation
## ## 1994m12
## tmp2 <- censom12[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=1994, unit="m", census.data = censom12)
## tmp2$pob18e <- interlog(what="p18", yr=1994, unit="m", census.data = censom12, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=1994, unit="m", census.data = censom12, frm="dv~iv")
## v94m12$pob182 <- tmp2$pob182 # paste interpolation
## v94m12$pob18e <- tmp2$pob18e # paste interpolation
## v94m12$pob18l <- tmp2$pob18l # paste interpolation
## ## 1994m15
## tmp2 <- censom15[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=1994, unit="m", census.data = censom15)
## tmp2$pob18e <- interlog(what="p18", yr=1994, unit="m", census.data = censom15, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=1994, unit="m", census.data = censom15, frm="dv~iv")
## v94m15$pob182 <- tmp2$pob182 # paste interpolation
## v94m15$pob18e <- tmp2$pob18e # paste interpolation
## v94m15$pob18l <- tmp2$pob18l # paste interpolation
## ## 1994m18
## tmp2 <- censom18[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=1994, unit="m", census.data = censom18)
## tmp2$pob18e <- interlog(what="p18", yr=1994, unit="m", census.data = censom18, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=1994, unit="m", census.data = censom18, frm="dv~iv")
## v94m18$pob182 <- tmp2$pob182 # paste interpolation
## v94m18$pob18e <- tmp2$pob18e # paste interpolation
## v94m18$pob18l <- tmp2$pob18l # paste interpolation
## ## 1994m21
## tmp2 <- censom21[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=1994, unit="m", census.data = censom21)
## tmp2$pob18e <- interlog(what="p18", yr=1994, unit="m", census.data = censom21, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=1994, unit="m", census.data = censom21, frm="dv~iv")
## v94m21$pob182 <- tmp2$pob182 # paste interpolation
## v94m21$pob18e <- tmp2$pob18e # paste interpolation
## v94m21$pob18l <- tmp2$pob18l # paste interpolation

## ## 1997
## tmp2 <- censom97[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=1997, unit="m")
## tmp2$pob18e <- interlog(what="p18", yr=1997, unit="m", frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=1997, unit="m", frm="dv~iv")
## v97m$pob182 <- tmp2$pob182 # paste interpolation
## v97m$pob18e <- tmp2$pob18e # paste interpolation
## v97m$pob18l <- tmp2$pob18l # paste interpolation
## ## 1997m94
## tmp2 <- censom94[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=1997, unit="m", census.data = censom94)
## tmp2$pob18e <- interlog(what="p18", yr=1997, unit="m", census.data = censom94, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=1997, unit="m", census.data = censom94, frm="dv~iv")
## v97m94$pob182 <- tmp2$pob182 # paste interpolation
## v97m94$pob18e <- tmp2$pob18e # paste interpolation
## v97m94$pob18l <- tmp2$pob18l # paste interpolation
## 1997m00
tmp2 <- censom00[, c("edon","inegi","ife")]
tmp2$pob182 <- interpol(what="p18", yr=1997, unit="m", census.data = censom00)
tmp2$pob18e <- interlog(what="p18", yr=1997, unit="m", census.data = censom00, frm="log(dv)~iv")
tmp2$pob18l <- interlog(what="p18", yr=1997, unit="m", census.data = censom00, frm="dv~iv")
v97m00$pob182 <- tmp2$pob182 # paste interpolation
v97m00$pob18e <- tmp2$pob18e # paste interpolation
v97m00$pob18l <- tmp2$pob18l # paste interpolation
## ## 1997m03
## tmp2 <- censom03[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=1997, unit="m", census.data = censom03)
## tmp2$pob18e <- interlog(what="p18", yr=1997, unit="m", census.data = censom03, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=1997, unit="m", census.data = censom03, frm="dv~iv")
## v97m03$pob182 <- tmp2$pob182 # paste interpolation
## v97m03$pob18e <- tmp2$pob18e # paste interpolation
## v97m03$pob18l <- tmp2$pob18l # paste interpolation
## ## 1997m06
## tmp2 <- censom06[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=1997, unit="m", census.data = censom06)
## tmp2$pob18e <- interlog(what="p18", yr=1997, unit="m", census.data = censom06, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=1997, unit="m", census.data = censom06, frm="dv~iv")
## v97m06$pob182 <- tmp2$pob182 # paste interpolation
## v97m06$pob18e <- tmp2$pob18e # paste interpolation
## v97m06$pob18l <- tmp2$pob18l # paste interpolation
## ## 1997m09
## tmp2 <- censom09[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=1997, unit="m", census.data = censom09)
## tmp2$pob18e <- interlog(what="p18", yr=1997, unit="m", census.data = censom09, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=1997, unit="m", census.data = censom09, frm="dv~iv")
## v97m09$pob182 <- tmp2$pob182 # paste interpolation
## v97m09$pob18e <- tmp2$pob18e # paste interpolation
## v97m09$pob18l <- tmp2$pob18l # paste interpolation
## ## 1997m12
## tmp2 <- censom12[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=1997, unit="m", census.data = censom12)
## tmp2$pob18e <- interlog(what="p18", yr=1997, unit="m", census.data = censom12, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=1997, unit="m", census.data = censom12, frm="dv~iv")
## v97m12$pob182 <- tmp2$pob182 # paste interpolation
## v97m12$pob18e <- tmp2$pob18e # paste interpolation
## v97m12$pob18l <- tmp2$pob18l # paste interpolation
## ## 1997m15
## tmp2 <- censom15[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=1997, unit="m", census.data = censom15)
## tmp2$pob18e <- interlog(what="p18", yr=1997, unit="m", census.data = censom15, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=1997, unit="m", census.data = censom15, frm="dv~iv")
## v97m15$pob182 <- tmp2$pob182 # paste interpolation
## v97m15$pob18e <- tmp2$pob18e # paste interpolation
## v97m15$pob18l <- tmp2$pob18l # paste interpolation
## ## 1997m18
## tmp2 <- censom18[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=1997, unit="m", census.data = censom18)
## tmp2$pob18e <- interlog(what="p18", yr=1997, unit="m", census.data = censom18, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=1997, unit="m", census.data = censom18, frm="dv~iv")
## v97m18$pob182 <- tmp2$pob182 # paste interpolation
## v97m18$pob18e <- tmp2$pob18e # paste interpolation
## v97m18$pob18l <- tmp2$pob18l # paste interpolation
## ## 1997m21
## tmp2 <- censom21[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=1997, unit="m", census.data = censom21)
## tmp2$pob18e <- interlog(what="p18", yr=1997, unit="m", census.data = censom21, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=1997, unit="m", census.data = censom21, frm="dv~iv")
## v97m21$pob182 <- tmp2$pob182 # paste interpolation
## v97m21$pob18e <- tmp2$pob18e # paste interpolation
## v97m21$pob18l <- tmp2$pob18l # paste interpolation

## 2000
tmp2 <- censom00[, c("edon","inegi","ife")]
tmp2$pob182 <- interpol(what="p18", yr=2000, unit="m")
tmp2$pob18e <- interlog(what="p18", yr=2000, unit="m", frm="log(dv)~iv")
tmp2$pob18l <- interlog(what="p18", yr=2000, unit="m", frm="dv~iv")
v00m$pob182 <- tmp2$pob182 # paste interpolation
v00m$pob18e <- tmp2$pob18e # paste interpolation
v00m$pob18l <- tmp2$pob18l # paste interpolation
## ## 2000m94
## tmp2 <- censom94[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2000, unit="m", census.data = censom94)
## tmp2$pob18e <- interlog(what="p18", yr=2000, unit="m", census.data = censom94, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2000, unit="m", census.data = censom94, frm="dv~iv")
## v00m94$pob182 <- tmp2$pob182 # paste interpolation
## v00m94$pob18e <- tmp2$pob18e # paste interpolation
## v00m94$pob18l <- tmp2$pob18l # paste interpolation
## ## 2000m97
## tmp2 <- censom97[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2000, unit="m", census.data = censom97)
## tmp2$pob18e <- interlog(what="p18", yr=2000, unit="m", census.data = censom97, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2000, unit="m", census.data = censom97, frm="dv~iv")
## v00m97$pob182 <- tmp2$pob182 # paste interpolation
## v00m97$pob18e <- tmp2$pob18e # paste interpolation
## v00m97$pob18l <- tmp2$pob18l # paste interpolation
## ## 2000m03
## tmp2 <- censom03[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2000, unit="m", census.data = censom03)
## tmp2$pob18e <- interlog(what="p18", yr=2000, unit="m", census.data = censom03, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2000, unit="m", census.data = censom03, frm="dv~iv")
## v00m03$pob182 <- tmp2$pob182 # paste interpolation
## v00m03$pob18e <- tmp2$pob18e # paste interpolation
## v00m03$pob18l <- tmp2$pob18l # paste interpolation
## ## 2000m06
## tmp2 <- censom06[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2000, unit="m", census.data = censom06)
## tmp2$pob18e <- interlog(what="p18", yr=2000, unit="m", census.data = censom06, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2000, unit="m", census.data = censom06, frm="dv~iv")
## v00m06$pob182 <- tmp2$pob182 # paste interpolation
## v00m06$pob18e <- tmp2$pob18e # paste interpolation
## v00m06$pob18l <- tmp2$pob18l # paste interpolation
## ## 2000m09
## tmp2 <- censom09[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2000, unit="m", census.data = censom09)
## tmp2$pob18e <- interlog(what="p18", yr=2000, unit="m", census.data = censom09, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2000, unit="m", census.data = censom09, frm="dv~iv")
## v00m09$pob182 <- tmp2$pob182 # paste interpolation
## v00m09$pob18e <- tmp2$pob18e # paste interpolation
## v00m09$pob18l <- tmp2$pob18l # paste interpolation
## ## 2000m12
## tmp2 <- censom12[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2000, unit="m", census.data = censom12)
## tmp2$pob18e <- interlog(what="p18", yr=2000, unit="m", census.data = censom12, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2000, unit="m", census.data = censom12, frm="dv~iv")
## v00m12$pob182 <- tmp2$pob182 # paste interpolation
## v00m12$pob18e <- tmp2$pob18e # paste interpolation
## v00m12$pob18l <- tmp2$pob18l # paste interpolation
## ## 2000m15
## tmp2 <- censom15[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2000, unit="m", census.data = censom15)
## tmp2$pob18e <- interlog(what="p18", yr=2000, unit="m", census.data = censom15, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2000, unit="m", census.data = censom15, frm="dv~iv")
## v00m15$pob182 <- tmp2$pob182 # paste interpolation
## v00m15$pob18e <- tmp2$pob18e # paste interpolation
## v00m15$pob18l <- tmp2$pob18l # paste interpolation
## ## 2000m18
## tmp2 <- censom18[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2000, unit="m", census.data = censom18)
## tmp2$pob18e <- interlog(what="p18", yr=2000, unit="m", census.data = censom18, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2000, unit="m", census.data = censom18, frm="dv~iv")
## v00m18$pob182 <- tmp2$pob182 # paste interpolation
## v00m18$pob18e <- tmp2$pob18e # paste interpolation
## v00m18$pob18l <- tmp2$pob18l # paste interpolation
## ## 2000m21
## tmp2 <- censom21[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2000, unit="m", census.data = censom21)
## tmp2$pob18e <- interlog(what="p18", yr=2000, unit="m", census.data = censom21, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2000, unit="m", census.data = censom21, frm="dv~iv")
## v00m21$pob182 <- tmp2$pob182 # paste interpolation
## v00m21$pob18e <- tmp2$pob18e # paste interpolation
## v00m21$pob18l <- tmp2$pob18l # paste interpolation

## ## 2003
## tmp2 <- censom03[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2003, unit="m")
## tmp2$pob18e <- interlog(what="p18", yr=2003, unit="m", frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2003, unit="m", frm="dv~iv")
## v03m$pob182 <- tmp2$pob182 # paste interpolation
## v03m$pob18e <- tmp2$pob18e # paste interpolation
## v03m$pob18l <- tmp2$pob18l # paste interpolation
## ## 2003m94
## tmp2 <- censom94[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2003, unit="m", census.data = censom94)
## tmp2$pob18e <- interlog(what="p18", yr=2003, unit="m", census.data = censom94, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2003, unit="m", census.data = censom94, frm="dv~iv")
## v03m94$pob182 <- tmp2$pob182 # paste interpolation
## v03m94$pob18e <- tmp2$pob18e # paste interpolation
## v03m94$pob18l <- tmp2$pob18l # paste interpolation
## ## 2003m97
## tmp2 <- censom97[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2003, unit="m", census.data = censom97)
## tmp2$pob18e <- interlog(what="p18", yr=2003, unit="m", census.data = censom97, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2003, unit="m", census.data = censom97, frm="dv~iv")
## v03m97$pob182 <- tmp2$pob182 # paste interpolation
## v03m97$pob18e <- tmp2$pob18e # paste interpolation
## v03m97$pob18l <- tmp2$pob18l # paste interpolation
## 2003m00
tmp2 <- censom00[, c("edon","inegi","ife")]
tmp2$pob182 <- interpol(what="p18", yr=2003, unit="m", census.data = censom00)
tmp2$pob18e <- interlog(what="p18", yr=2003, unit="m", census.data = censom00, frm="log(dv)~iv")
tmp2$pob18l <- interlog(what="p18", yr=2003, unit="m", census.data = censom00, frm="dv~iv")
v03m00$pob182 <- tmp2$pob182 # paste interpolation
v03m00$pob18e <- tmp2$pob18e # paste interpolation
v03m00$pob18l <- tmp2$pob18l # paste interpolation
## ## 2003m06
## tmp2 <- censom06[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2003, unit="m", census.data = censom06)
## tmp2$pob18e <- interlog(what="p18", yr=2003, unit="m", census.data = censom06, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2003, unit="m", census.data = censom06, frm="dv~iv")
## v03m06$pob182 <- tmp2$pob182 # paste interpolation
## v03m06$pob18e <- tmp2$pob18e # paste interpolation
## v03m06$pob18l <- tmp2$pob18l # paste interpolation
## ## 2003m09
## tmp2 <- censom09[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2003, unit="m", census.data = censom09)
## tmp2$pob18e <- interlog(what="p18", yr=2003, unit="m", census.data = censom09, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2003, unit="m", census.data = censom09, frm="dv~iv")
## v03m09$pob182 <- tmp2$pob182 # paste interpolation
## v03m09$pob18e <- tmp2$pob18e # paste interpolation
## v03m09$pob18l <- tmp2$pob18l # paste interpolation
## ## 2003m12
## tmp2 <- censom12[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2003, unit="m", census.data = censom12)
## tmp2$pob18e <- interlog(what="p18", yr=2003, unit="m", census.data = censom12, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2003, unit="m", census.data = censom12, frm="dv~iv")
## v03m12$pob182 <- tmp2$pob182 # paste interpolation
## v03m12$pob18e <- tmp2$pob18e # paste interpolation
## v03m12$pob18l <- tmp2$pob18l # paste interpolation
## ## 2003m15
## tmp2 <- censom15[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2003, unit="m", census.data = censom15)
## tmp2$pob18e <- interlog(what="p18", yr=2003, unit="m", census.data = censom15, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2003, unit="m", census.data = censom15, frm="dv~iv")
## v03m15$pob182 <- tmp2$pob182 # paste interpolation
## v03m15$pob18e <- tmp2$pob18e # paste interpolation
## v03m15$pob18l <- tmp2$pob18l # paste interpolation
## ## 2003m18
## tmp2 <- censom18[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2003, unit="m", census.data = censom18)
## tmp2$pob18e <- interlog(what="p18", yr=2003, unit="m", census.data = censom18, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2003, unit="m", census.data = censom18, frm="dv~iv")
## v03m18$pob182 <- tmp2$pob182 # paste interpolation
## v03m18$pob18e <- tmp2$pob18e # paste interpolation
## v03m18$pob18l <- tmp2$pob18l # paste interpolation
## ## 2003m21
## tmp2 <- censom21[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2003, unit="m", census.data = censom21)
## tmp2$pob18e <- interlog(what="p18", yr=2003, unit="m", census.data = censom21, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2003, unit="m", census.data = censom21, frm="dv~iv")
## v03m21$pob182 <- tmp2$pob182 # paste interpolation
## v03m21$pob18e <- tmp2$pob18e # paste interpolation
## v03m21$pob18l <- tmp2$pob18l # paste interpolation

## ## 2006
## tmp2 <- censom06[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2006, unit="m")
## tmp2$pob18e <- interlog(what="p18", yr=2006, unit="m", frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2006, unit="m", frm="dv~iv")
## v06m$pob182 <- tmp2$pob182 # paste interpolation
## v06m$pob18e <- tmp2$pob18e # paste interpolation
## v06m$pob18l <- tmp2$pob18l # paste interpolation
## ## 2006m94
## tmp2 <- censom94[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2006, unit="m", census.data = censom94)
## tmp2$pob18e <- interlog(what="p18", yr=2006, unit="m", census.data = censom94, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2006, unit="m", census.data = censom94, frm="dv~iv")
## v06m94$pob182 <- tmp2$pob182 # paste interpolation
## v06m94$pob18e <- tmp2$pob18e # paste interpolation
## v06m94$pob18l <- tmp2$pob18l # paste interpolation
## ## 2006m97
## tmp2 <- censom97[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2006, unit="m", census.data = censom97)
## tmp2$pob18e <- interlog(what="p18", yr=2006, unit="m", census.data = censom97, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2006, unit="m", census.data = censom97, frm="dv~iv")
## v06m97$pob182 <- tmp2$pob182 # paste interpolation
## v06m97$pob18e <- tmp2$pob18e # paste interpolation
## v06m97$pob18l <- tmp2$pob18l # paste interpolation
## 2006m00
tmp2 <- censom00[, c("edon","inegi","ife")]
tmp2$pob182 <- interpol(what="p18", yr=2006, unit="m", census.data = censom00)
tmp2$pob18e <- interlog(what="p18", yr=2006, unit="m", census.data = censom00, frm="log(dv)~iv")
tmp2$pob18l <- interlog(what="p18", yr=2006, unit="m", census.data = censom00, frm="dv~iv")
v06m00$pob182 <- tmp2$pob182 # paste interpolation
v06m00$pob18e <- tmp2$pob18e # paste interpolation
v06m00$pob18l <- tmp2$pob18l # paste interpolation
## ## 2006m03
## tmp2 <- censom03[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2006, unit="m", census.data = censom03)
## tmp2$pob18e <- interlog(what="p18", yr=2006, unit="m", census.data = censom03, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2006, unit="m", census.data = censom03, frm="dv~iv")
## v06m03$pob182 <- tmp2$pob182 # paste interpolation
## v06m03$pob18e <- tmp2$pob18e # paste interpolation
## v06m03$pob18l <- tmp2$pob18l # paste interpolation
## ## 2006m09
## tmp2 <- censom09[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2006, unit="m", census.data = censom09)
## tmp2$pob18e <- interlog(what="p18", yr=2006, unit="m", census.data = censom09, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2006, unit="m", census.data = censom09, frm="dv~iv")
## v06m09$pob182 <- tmp2$pob182 # paste interpolation
## v06m09$pob18e <- tmp2$pob18e # paste interpolation
## v06m09$pob18l <- tmp2$pob18l # paste interpolation
## ## 2006m12
## tmp2 <- censom12[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2006, unit="m", census.data = censom12)
## tmp2$pob18e <- interlog(what="p18", yr=2006, unit="m", census.data = censom12, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2006, unit="m", census.data = censom12, frm="dv~iv")
## v06m12$pob182 <- tmp2$pob182 # paste interpolation
## v06m12$pob18e <- tmp2$pob18e # paste interpolation
## v06m12$pob18l <- tmp2$pob18l # paste interpolation
## ## 2006m15
## tmp2 <- censom15[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2006, unit="m", census.data = censom15)
## tmp2$pob18e <- interlog(what="p18", yr=2006, unit="m", census.data = censom15, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2006, unit="m", census.data = censom15, frm="dv~iv")
## v06m15$pob182 <- tmp2$pob182 # paste interpolation
## v06m15$pob18e <- tmp2$pob18e # paste interpolation
## v06m15$pob18l <- tmp2$pob18l # paste interpolation
## ## 2006m18
## tmp2 <- censom18[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2006, unit="m", census.data = censom18)
## tmp2$pob18e <- interlog(what="p18", yr=2006, unit="m", census.data = censom18, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2006, unit="m", census.data = censom18, frm="dv~iv")
## v06m18$pob182 <- tmp2$pob182 # paste interpolation
## v06m18$pob18e <- tmp2$pob18e # paste interpolation
## v06m18$pob18l <- tmp2$pob18l # paste interpolation
## ## 2006m21
## tmp2 <- censom21[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2006, unit="m", census.data = censom21)
## tmp2$pob18e <- interlog(what="p18", yr=2006, unit="m", census.data = censom21, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2006, unit="m", census.data = censom21, frm="dv~iv")
## v06m21$pob182 <- tmp2$pob182 # paste interpolation
## v06m21$pob18e <- tmp2$pob18e # paste interpolation
## v06m21$pob18l <- tmp2$pob18l # paste interpolation

## ## 2009
## tmp2 <- censom09[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2009, unit="m")
## tmp2$pob18e <- interlog(what="p18", yr=2009, unit="m", frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2009, unit="m", frm="dv~iv")
## v09m$pob182 <- tmp2$pob182 # paste interpolation
## v09m$pob18e <- tmp2$pob18e # paste interpolation
## v09m$pob18l <- tmp2$pob18l # paste interpolation
## ## 2009m94
## tmp2 <- censom94[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2009, unit="m", census.data = censom94)
## tmp2$pob18e <- interlog(what="p18", yr=2009, unit="m", census.data = censom94, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2009, unit="m", census.data = censom94, frm="dv~iv")
## v09m94$pob182 <- tmp2$pob182 # paste interpolation
## v09m94$pob18e <- tmp2$pob18e # paste interpolation
## v09m94$pob18l <- tmp2$pob18l # paste interpolation
## ## 2009m97
## tmp2 <- censom97[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2009, unit="m", census.data = censom97)
## tmp2$pob18e <- interlog(what="p18", yr=2009, unit="m", census.data = censom97, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2009, unit="m", census.data = censom97, frm="dv~iv")
## v09m97$pob182 <- tmp2$pob182 # paste interpolation
## v09m97$pob18e <- tmp2$pob18e # paste interpolation
## v09m97$pob18l <- tmp2$pob18l # paste interpolation
## 2009m00
tmp2 <- censom00[, c("edon","inegi","ife")]
tmp2$pob182 <- interpol(what="p18", yr=2009, unit="m", census.data = censom00)
tmp2$pob18e <- interlog(what="p18", yr=2009, unit="m", census.data = censom00, frm="log(dv)~iv")
tmp2$pob18l <- interlog(what="p18", yr=2009, unit="m", census.data = censom00, frm="dv~iv")
v09m00$pob182 <- tmp2$pob182 # paste interpolation
v09m00$pob18e <- tmp2$pob18e # paste interpolation
v09m00$pob18l <- tmp2$pob18l # paste interpolation
## ## 2009m03
## tmp2 <- censom03[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2009, unit="m", census.data = censom03)
## tmp2$pob18e <- interlog(what="p18", yr=2009, unit="m", census.data = censom03, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2009, unit="m", census.data = censom03, frm="dv~iv")
## v09m03$pob182 <- tmp2$pob182 # paste interpolation
## v09m03$pob18e <- tmp2$pob18e # paste interpolation
## v09m03$pob18l <- tmp2$pob18l # paste interpolation
## ## 2009m06
## tmp2 <- censom06[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2009, unit="m", census.data = censom06)
## tmp2$pob18e <- interlog(what="p18", yr=2009, unit="m", census.data = censom06, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2009, unit="m", census.data = censom06, frm="dv~iv")
## v09m06$pob182 <- tmp2$pob182 # paste interpolation
## v09m06$pob18e <- tmp2$pob18e # paste interpolation
## v09m06$pob18l <- tmp2$pob18l # paste interpolation
## ## 2009m12
## tmp2 <- censom12[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2009, unit="m", census.data = censom12)
## tmp2$pob18e <- interlog(what="p18", yr=2009, unit="m", census.data = censom12, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2009, unit="m", census.data = censom12, frm="dv~iv")
## v09m12$pob182 <- tmp2$pob182 # paste interpolation
## v09m12$pob18e <- tmp2$pob18e # paste interpolation
## v09m12$pob18l <- tmp2$pob18l # paste interpolation
## ## 2009m15
## tmp2 <- censom15[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2009, unit="m", census.data = censom15)
## tmp2$pob18e <- interlog(what="p18", yr=2009, unit="m", census.data = censom15, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2009, unit="m", census.data = censom15, frm="dv~iv")
## v09m15$pob182 <- tmp2$pob182 # paste interpolation
## v09m15$pob18e <- tmp2$pob18e # paste interpolation
## v09m15$pob18l <- tmp2$pob18l # paste interpolation
## ## 2009m18
## tmp2 <- censom18[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2009, unit="m", census.data = censom18)
## tmp2$pob18e <- interlog(what="p18", yr=2009, unit="m", census.data = censom18, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2009, unit="m", census.data = censom18, frm="dv~iv")
## v09m18$pob182 <- tmp2$pob182 # paste interpolation
## v09m18$pob18e <- tmp2$pob18e # paste interpolation
## v09m18$pob18l <- tmp2$pob18l # paste interpolation
## ## 2009m21
## tmp2 <- censom21[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2009, unit="m", census.data = censom21)
## tmp2$pob18e <- interlog(what="p18", yr=2009, unit="m", census.data = censom21, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2009, unit="m", census.data = censom21, frm="dv~iv")
## v09m21$pob182 <- tmp2$pob182 # paste interpolation
## v09m21$pob18e <- tmp2$pob18e # paste interpolation
## v09m21$pob18l <- tmp2$pob18l # paste interpolation

## ## 2012
## tmp2 <- censom12[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2012, unit="m")
## tmp2$pob18e <- interlog(what="p18", yr=2012, unit="m", frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2012, unit="m", frm="dv~iv")
## v12m$pob182 <- tmp2$pob182 # paste interpolation
## v12m$pob18e <- tmp2$pob18e # paste interpolation
## v12m$pob18l <- tmp2$pob18l # paste interpolation
## ## 2012m94
## tmp2 <- censom94[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2012, unit="m", census.data = censom94)
## tmp2$pob18e <- interlog(what="p18", yr=2012, unit="m", census.data = censom94, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2012, unit="m", census.data = censom94, frm="dv~iv")
## v12m94$pob182 <- tmp2$pob182 # paste interpolation
## v12m94$pob18e <- tmp2$pob18e # paste interpolation
## v12m94$pob18l <- tmp2$pob18l # paste interpolation
## ## 2012m97
## tmp2 <- censom97[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2012, unit="m", census.data = censom97)
## tmp2$pob18e <- interlog(what="p18", yr=2012, unit="m", census.data = censom97, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2012, unit="m", census.data = censom97, frm="dv~iv")
## v12m97$pob182 <- tmp2$pob182 # paste interpolation
## v12m97$pob18e <- tmp2$pob18e # paste interpolation
## v12m97$pob18l <- tmp2$pob18l # paste interpolation
## 2012m00
tmp2 <- censom00[, c("edon","inegi","ife")]
tmp2$pob182 <- interpol(what="p18", yr=2012, unit="m", census.data = censom00)
tmp2$pob18e <- interlog(what="p18", yr=2012, unit="m", census.data = censom00, frm="log(dv)~iv")
tmp2$pob18l <- interlog(what="p18", yr=2012, unit="m", census.data = censom00, frm="dv~iv")
v12m00$pob182 <- tmp2$pob182 # paste interpolation
v12m00$pob18e <- tmp2$pob18e # paste interpolation
v12m00$pob18l <- tmp2$pob18l # paste interpolation
## ## 2012m03
## tmp2 <- censom03[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2012, unit="m", census.data = censom03)
## tmp2$pob18e <- interlog(what="p18", yr=2012, unit="m", census.data = censom03, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2012, unit="m", census.data = censom03, frm="dv~iv")
## v12m03$pob182 <- tmp2$pob182 # paste interpolation
## v12m03$pob18e <- tmp2$pob18e # paste interpolation
## v12m03$pob18l <- tmp2$pob18l # paste interpolation
## ## 2012m06
## tmp2 <- censom06[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2012, unit="m", census.data = censom06)
## tmp2$pob18e <- interlog(what="p18", yr=2012, unit="m", census.data = censom06, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2012, unit="m", census.data = censom06, frm="dv~iv")
## v12m06$pob182 <- tmp2$pob182 # paste interpolation
## v12m06$pob18e <- tmp2$pob18e # paste interpolation
## v12m06$pob18l <- tmp2$pob18l # paste interpolation
## ## 2012m09
## tmp2 <- censom09[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2012, unit="m", census.data = censom09)
## tmp2$pob18e <- interlog(what="p18", yr=2012, unit="m", census.data = censom09, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2012, unit="m", census.data = censom09, frm="dv~iv")
## v12m09$pob182 <- tmp2$pob182 # paste interpolation
## v12m09$pob18e <- tmp2$pob18e # paste interpolation
## v12m09$pob18l <- tmp2$pob18l # paste interpolation
## ## 2012m15
## tmp2 <- censom15[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2012, unit="m", census.data = censom15)
## tmp2$pob18e <- interlog(what="p18", yr=2012, unit="m", census.data = censom15, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2012, unit="m", census.data = censom15, frm="dv~iv")
## v12m15$pob182 <- tmp2$pob182 # paste interpolation
## v12m15$pob18e <- tmp2$pob18e # paste interpolation
## v12m15$pob18l <- tmp2$pob18l # paste interpolation
## ## 2012m18
## tmp2 <- censom18[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2012, unit="m", census.data = censom18)
## tmp2$pob18e <- interlog(what="p18", yr=2012, unit="m", census.data = censom18, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2012, unit="m", census.data = censom18, frm="dv~iv")
## v12m18$pob182 <- tmp2$pob182 # paste interpolation
## v12m18$pob18e <- tmp2$pob18e # paste interpolation
## v12m18$pob18l <- tmp2$pob18l # paste interpolation
## ## 2012m21
## tmp2 <- censom21[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2012, unit="m", census.data = censom21)
## tmp2$pob18e <- interlog(what="p18", yr=2012, unit="m", census.data = censom21, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2012, unit="m", census.data = censom21, frm="dv~iv")
## v12m21$pob182 <- tmp2$pob182 # paste interpolation
## v12m21$pob18e <- tmp2$pob18e # paste interpolation
## v12m21$pob18l <- tmp2$pob18l # paste interpolation

## ## 2015
## tmp2 <- censom15[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2015, unit="m")
## tmp2$pob18e <- interlog(what="p18", yr=2015, unit="m", frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2015, unit="m", frm="dv~iv")
## v15m$pob182 <- tmp2$pob182 # paste interpolation
## v15m$pob18e <- tmp2$pob18e # paste interpolation
## v15m$pob18l <- tmp2$pob18l # paste interpolation
## ## 2015m94
## tmp2 <- censom94[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2015, unit="m", census.data = censom94)
## tmp2$pob18e <- interlog(what="p18", yr=2015, unit="m", census.data = censom94, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2015, unit="m", census.data = censom94, frm="dv~iv")
## v15m94$pob182 <- tmp2$pob182 # paste interpolation
## v15m94$pob18e <- tmp2$pob18e # paste interpolation
## v15m94$pob18l <- tmp2$pob18l # paste interpolation
## ## 2015m97
## tmp2 <- censom97[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2015, unit="m", census.data = censom97)
## tmp2$pob18e <- interlog(what="p18", yr=2015, unit="m", census.data = censom97, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2015, unit="m", census.data = censom97, frm="dv~iv")
## v15m97$pob182 <- tmp2$pob182 # paste interpolation
## v15m97$pob18e <- tmp2$pob18e # paste interpolation
## v15m97$pob18l <- tmp2$pob18l # paste interpolation
## 2015m00
tmp2 <- censom00[, c("edon","inegi","ife")]
tmp2$pob182 <- interpol(what="p18", yr=2015, unit="m", census.data = censom00)
tmp2$pob18e <- interlog(what="p18", yr=2015, unit="m", census.data = censom00, frm="log(dv)~iv")
tmp2$pob18l <- interlog(what="p18", yr=2015, unit="m", census.data = censom00, frm="dv~iv")
v15m00$pob182 <- tmp2$pob182 # paste interpolation
v15m00$pob18e <- tmp2$pob18e # paste interpolation
v15m00$pob18l <- tmp2$pob18l # paste interpolation
## ## 2015m03
## tmp2 <- censom03[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2015, unit="m", census.data = censom03)
## tmp2$pob18e <- interlog(what="p18", yr=2015, unit="m", census.data = censom03, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2015, unit="m", census.data = censom03, frm="dv~iv")
## v15m03$pob182 <- tmp2$pob182 # paste interpolation
## v15m03$pob18e <- tmp2$pob18e # paste interpolation
## v15m03$pob18l <- tmp2$pob18l # paste interpolation
## ## 2015m06
## tmp2 <- censom06[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2015, unit="m", census.data = censom06)
## tmp2$pob18e <- interlog(what="p18", yr=2015, unit="m", census.data = censom06, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2015, unit="m", census.data = censom06, frm="dv~iv")
## v15m06$pob182 <- tmp2$pob182 # paste interpolation
## v15m06$pob18e <- tmp2$pob18e # paste interpolation
## v15m06$pob18l <- tmp2$pob18l # paste interpolation
## ## 2015m09
## tmp2 <- censom09[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2015, unit="m", census.data = censom09)
## tmp2$pob18e <- interlog(what="p18", yr=2015, unit="m", census.data = censom09, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2015, unit="m", census.data = censom09, frm="dv~iv")
## v15m09$pob182 <- tmp2$pob182 # paste interpolation
## v15m09$pob18e <- tmp2$pob18e # paste interpolation
## v15m09$pob18l <- tmp2$pob18l # paste interpolation
## ## 2015m12
## tmp2 <- censom12[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2015, unit="m", census.data = censom12)
## tmp2$pob18e <- interlog(what="p18", yr=2015, unit="m", census.data = censom12, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2015, unit="m", census.data = censom12, frm="dv~iv")
## v15m12$pob182 <- tmp2$pob182 # paste interpolation
## v15m12$pob18e <- tmp2$pob18e # paste interpolation
## v15m12$pob18l <- tmp2$pob18l # paste interpolation
## ## 2015m18
## tmp2 <- censom18[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2015, unit="m", census.data = censom18)
## tmp2$pob18e <- interlog(what="p18", yr=2015, unit="m", census.data = censom18, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2015, unit="m", census.data = censom18, frm="dv~iv")
## v15m18$pob182 <- tmp2$pob182 # paste interpolation
## v15m18$pob18e <- tmp2$pob18e # paste interpolation
## v15m18$pob18l <- tmp2$pob18l # paste interpolation
## ## 2015m21
## tmp2 <- censom21[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2015, unit="m", census.data = censom21)
## tmp2$pob18e <- interlog(what="p18", yr=2015, unit="m", census.data = censom21, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2015, unit="m", census.data = censom21, frm="dv~iv")
## v15m21$pob182 <- tmp2$pob182 # paste interpolation
## v15m21$pob18e <- tmp2$pob18e # paste interpolation
## v15m21$pob18l <- tmp2$pob18l # paste interpolation

## ## 2018
## tmp2 <- censom18[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2018, unit="m")
## tmp2$pob18e <- interlog(what="p18", yr=2018, unit="m", frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2018, unit="m", frm="dv~iv")
## v18m$pob182 <- tmp2$pob182 # paste interpolation
## v18m$pob18e <- tmp2$pob18e # paste interpolation
## v18m$pob18l <- tmp2$pob18l # paste interpolation
## ## 2018m94
## tmp2 <- censom94[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2018, unit="m", census.data = censom94)
## tmp2$pob18e <- interlog(what="p18", yr=2018, unit="m", census.data = censom94, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2018, unit="m", census.data = censom94, frm="dv~iv")
## v18m94$pob182 <- tmp2$pob182 # paste interpolation
## v18m94$pob18e <- tmp2$pob18e # paste interpolation
## v18m94$pob18l <- tmp2$pob18l # paste interpolation
## ## 2018m97
## tmp2 <- censom97[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2018, unit="m", census.data = censom97)
## tmp2$pob18e <- interlog(what="p18", yr=2018, unit="m", census.data = censom97, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2018, unit="m", census.data = censom97, frm="dv~iv")
## v18m97$pob182 <- tmp2$pob182 # paste interpolation
## v18m97$pob18e <- tmp2$pob18e # paste interpolation
## v18m97$pob18l <- tmp2$pob18l # paste interpolation
## 2018m00
tmp2 <- censom00[, c("edon","inegi","ife")]
tmp2$pob182 <- interpol(what="p18", yr=2018, unit="m", census.data = censom00)
tmp2$pob18e <- interlog(what="p18", yr=2018, unit="m", census.data = censom00, frm="log(dv)~iv")
tmp2$pob18l <- interlog(what="p18", yr=2018, unit="m", census.data = censom00, frm="dv~iv")
v18m00$pob182 <- tmp2$pob182 # paste interpolation
v18m00$pob18e <- tmp2$pob18e # paste interpolation
v18m00$pob18l <- tmp2$pob18l # paste interpolation
## ## 2018m03
## tmp2 <- censom03[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2018, unit="m", census.data = censom03)
## tmp2$pob18e <- interlog(what="p18", yr=2018, unit="m", census.data = censom03, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2018, unit="m", census.data = censom03, frm="dv~iv")
## v18m03$pob182 <- tmp2$pob182 # paste interpolation
## v18m03$pob18e <- tmp2$pob18e # paste interpolation
## v18m03$pob18l <- tmp2$pob18l # paste interpolation
## ## 2018m06
## tmp2 <- censom06[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2018, unit="m", census.data = censom06)
## tmp2$pob18e <- interlog(what="p18", yr=2018, unit="m", census.data = censom06, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2018, unit="m", census.data = censom06, frm="dv~iv")
## v18m06$pob182 <- tmp2$pob182 # paste interpolation
## v18m06$pob18e <- tmp2$pob18e # paste interpolation
## v18m06$pob18l <- tmp2$pob18l # paste interpolation
## ## 2018m09
## tmp2 <- censom09[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2018, unit="m", census.data = censom09)
## tmp2$pob18e <- interlog(what="p18", yr=2018, unit="m", census.data = censom09, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2018, unit="m", census.data = censom09, frm="dv~iv")
## v18m09$pob182 <- tmp2$pob182 # paste interpolation
## v18m09$pob18e <- tmp2$pob18e # paste interpolation
## v18m09$pob18l <- tmp2$pob18l # paste interpolation
## ## 2018m12
## tmp2 <- censom12[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2018, unit="m", census.data = censom12)
## tmp2$pob18e <- interlog(what="p18", yr=2018, unit="m", census.data = censom12, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2018, unit="m", census.data = censom12, frm="dv~iv")
## v18m12$pob182 <- tmp2$pob182 # paste interpolation
## v18m12$pob18e <- tmp2$pob18e # paste interpolation
## v18m12$pob18l <- tmp2$pob18l # paste interpolation
## ## 2018m15
## tmp2 <- censom15[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2018, unit="m", census.data = censom15)
## tmp2$pob18e <- interlog(what="p18", yr=2018, unit="m", census.data = censom15, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2018, unit="m", census.data = censom15, frm="dv~iv")
## v18m15$pob182 <- tmp2$pob182 # paste interpolation
## v18m15$pob18e <- tmp2$pob18e # paste interpolation
## v18m15$pob18l <- tmp2$pob18l # paste interpolation
## ## 2018m21
## tmp2 <- censom21[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2018, unit="m", census.data = censom21)
## tmp2$pob18e <- interlog(what="p18", yr=2018, unit="m", census.data = censom21, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2018, unit="m", census.data = censom21, frm="dv~iv")
## v18m21$pob182 <- tmp2$pob182 # paste interpolation
## v18m21$pob18e <- tmp2$pob18e # paste interpolation
## v18m21$pob18l <- tmp2$pob18l # paste interpolation

## ## 2021
## tmp2 <- censom21[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2021, unit="m")
## tmp2$pob18e <- interlog(what="p18", yr=2021, unit="m", frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2021, unit="m", frm="dv~iv")
## v21m$pob182 <- tmp2$pob182 # paste interpolation
## v21m$pob18e <- tmp2$pob18e # paste interpolation
## v21m$pob18l <- tmp2$pob18l # paste interpolation
## ## 2021m94
## tmp2 <- censom94[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2021, unit="m", census.data = censom94)
## tmp2$pob18e <- interlog(what="p18", yr=2021, unit="m", census.data = censom94, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2021, unit="m", census.data = censom94, frm="dv~iv")
## v21m94$pob182 <- tmp2$pob182 # paste interpolation
## v21m94$pob18e <- tmp2$pob18e # paste interpolation
## v21m94$pob18l <- tmp2$pob18l # paste interpolation
## ## 2021m97
## tmp2 <- censom97[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2021, unit="m", census.data = censom97)
## tmp2$pob18e <- interlog(what="p18", yr=2021, unit="m", census.data = censom97, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2021, unit="m", census.data = censom97, frm="dv~iv")
## v21m97$pob182 <- tmp2$pob182 # paste interpolation
## v21m97$pob18e <- tmp2$pob18e # paste interpolation
## v21m97$pob18l <- tmp2$pob18l # paste interpolation
## 2021m00
tmp2 <- censom00[, c("edon","inegi","ife")]
tmp2$pob182 <- interpol(what="p18", yr=2021, unit="m", census.data = censom00)
tmp2$pob18e <- interlog(what="p18", yr=2021, unit="m", census.data = censom00, frm="log(dv)~iv")
tmp2$pob18l <- interlog(what="p18", yr=2021, unit="m", census.data = censom00, frm="dv~iv")
v21m00$pob182 <- tmp2$pob182 # paste interpolation
v21m00$pob18e <- tmp2$pob18e # paste interpolation
v21m00$pob18l <- tmp2$pob18l # paste interpolation
## ## 2021m03
## tmp2 <- censom03[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2021, unit="m", census.data = censom03)
## tmp2$pob18e <- interlog(what="p18", yr=2021, unit="m", census.data = censom03, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2021, unit="m", census.data = censom03, frm="dv~iv")
## v21m03$pob182 <- tmp2$pob182 # paste interpolation
## v21m03$pob18e <- tmp2$pob18e # paste interpolation
## v21m03$pob18l <- tmp2$pob18l # paste interpolation
## ## 2021m06
## tmp2 <- censom06[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2021, unit="m", census.data = censom06)
## tmp2$pob18e <- interlog(what="p18", yr=2021, unit="m", census.data = censom06, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2021, unit="m", census.data = censom06, frm="dv~iv")
## v21m06$pob182 <- tmp2$pob182 # paste interpolation
## v21m06$pob18e <- tmp2$pob18e # paste interpolation
## v21m06$pob18l <- tmp2$pob18l # paste interpolation
## ## 2021m09
## tmp2 <- censom09[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2021, unit="m", census.data = censom09)
## tmp2$pob18e <- interlog(what="p18", yr=2021, unit="m", census.data = censom09, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2021, unit="m", census.data = censom09, frm="dv~iv")
## v21m09$pob182 <- tmp2$pob182 # paste interpolation
## v21m09$pob18e <- tmp2$pob18e # paste interpolation
## v21m09$pob18l <- tmp2$pob18l # paste interpolation
## ## 2021m12
## tmp2 <- censom12[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2021, unit="m", census.data = censom12)
## tmp2$pob18e <- interlog(what="p18", yr=2021, unit="m", census.data = censom12, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2021, unit="m", census.data = censom12, frm="dv~iv")
## v21m12$pob182 <- tmp2$pob182 # paste interpolation
## v21m12$pob18e <- tmp2$pob18e # paste interpolation
## v21m12$pob18l <- tmp2$pob18l # paste interpolation
## ## 2021m15
## tmp2 <- censom15[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2021, unit="m", census.data = censom15)
## tmp2$pob18e <- interlog(what="p18", yr=2021, unit="m", census.data = censom15, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2021, unit="m", census.data = censom15, frm="dv~iv")
## v21m15$pob182 <- tmp2$pob182 # paste interpolation
## v21m15$pob18e <- tmp2$pob18e # paste interpolation
## v21m15$pob18l <- tmp2$pob18l # paste interpolation
## ## 2021m18
## tmp2 <- censom18[, c("edon","inegi","ife")]
## tmp2$pob182 <- interpol(what="p18", yr=2021, unit="m", census.data = censom18)
## tmp2$pob18e <- interlog(what="p18", yr=2021, unit="m", census.data = censom18, frm="log(dv)~iv")
## tmp2$pob18l <- interlog(what="p18", yr=2021, unit="m", census.data = censom18, frm="dv~iv")
## v21m18$pob182 <- tmp2$pob182 # paste interpolation
## v21m18$pob18e <- tmp2$pob18e # paste interpolation
## v21m18$pob18l <- tmp2$pob18l # paste interpolation



## all same sorted?
v94m00[1,]
censom00[1,]
table(v94m00$ife==censom00$ife)


## compare linear and log projections in census years
tmp <- data.frame(
    edon = censom00$edon,
    inegi= censom00$inegi,
    ife  = censom00$ife
)
tmp$c95  <- censom00$p18_1995
tmp$c00  <- censom00$p18_2000
tmp$c05  <- censom00$p18_2005
tmp$c10  <- censom00$p18_2010
tmp$c20  <- censom00$p18_2020
tmp$li95 <- interlog(what="p18", yr=1995, unit="m", census.data = censom00, frm="dv~iv")
tmp$li00 <- interlog(what="p18", yr=2000, unit="m", census.data = censom00, frm="dv~iv")
tmp$li05 <- interlog(what="p18", yr=2005, unit="m", census.data = censom00, frm="dv~iv")
tmp$li10 <- interlog(what="p18", yr=2010, unit="m", census.data = censom00, frm="dv~iv")
tmp$li20 <- interlog(what="p18", yr=2020, unit="m", census.data = censom00, frm="dv~iv")
tmp$lg95 <- interlog(what="p18", yr=1995, unit="m", census.data = censom00, frm="log(dv)~iv")
tmp$lg00 <- interlog(what="p18", yr=2000, unit="m", census.data = censom00, frm="log(dv)~iv")
tmp$lg05 <- interlog(what="p18", yr=2005, unit="m", census.data = censom00, frm="log(dv)~iv")
tmp$lg10 <- interlog(what="p18", yr=2010, unit="m", census.data = censom00, frm="log(dv)~iv")
tmp$lg20 <- interlog(what="p18", yr=2020, unit="m", census.data = censom00, frm="log(dv)~iv")
tmp$tw95 <- interpol(what="p18", yr=1995, unit="m", census.data = censom00)
tmp$tw00 <- interpol(what="p18", yr=2000, unit="m", census.data = censom00)
tmp$tw05 <- interpol(what="p18", yr=2005, unit="m", census.data = censom00)
tmp$tw10 <- interpol(what="p18", yr=2010, unit="m", census.data = censom00)
tmp$tw20 <- interpol(what="p18", yr=2020, unit="m", census.data = censom00)

tmp <- tmp[moveme(names(tmp), "li95 after lg95")]
tmp <- tmp[moveme(names(tmp), "tw95 after li95")]
tmp <- tmp[moveme(names(tmp), "li00 after lg00")]
tmp <- tmp[moveme(names(tmp), "tw00 after li00")]
tmp <- tmp[moveme(names(tmp), "li05 after lg05")]
tmp <- tmp[moveme(names(tmp), "tw05 after li05")]
tmp <- tmp[moveme(names(tmp), "li10 after lg10")]
tmp <- tmp[moveme(names(tmp), "tw10 after li10")]
tmp <- tmp[moveme(names(tmp), "li20 after lg20")]
tmp <- tmp[moveme(names(tmp), "tw20 after li20")]

str(tmp)
tmp[1:5,]

## stats to compare li and lg census projections
tmp <- within(tmp, {
    tmp1 <- ifelse(lg95 - c95==0, 1, lg95 - c95) ## if 0 make 1
    tmp2 <- ifelse(li95 - c95==0, 1, li95 - c95)
    tmp3 <- ifelse(tw95 - c95==0, 1, tw95 - c95)
    dlg95 <- tmp1 / c95
    dli95 <- tmp2 / c95
    dtw95 <- tmp3 / c95
    dif95 <-   c95/abs(tmp1) - c95/abs(tmp2)
    off95 <- ((   abs(tmp1) +   abs(tmp2) ) / 2) / c95
    tmp1 <-   ifelse(lg00 - c00==0, 1, lg00 - c00)
    tmp2 <-   ifelse(li00 - c00==0, 1, li00 - c00)
    tmp3 <-   ifelse(tw00 - c00==0, 1, tw00 - c00)
    dlg00 <- tmp1 / c00
    dli00 <- tmp2 / c00
    dtw00 <- tmp3 / c00
    dif00 <-   c00/abs(tmp1) - c00/abs(tmp2)
    off00 <- ((   abs(tmp1) +   abs(tmp2) ) / 2) / c00
    tmp1 <-   ifelse(lg05 - c05==0, 1, lg05 - c05)
    tmp2 <-   ifelse(li05 - c05==0, 1, li05 - c05)
    tmp2 <-   ifelse(tw05 - c05==0, 1, tw05 - c05)
    dlg05 <- tmp1 / c05
    dli05 <- tmp2 / c05
    dtw05 <- tmp3 / c05
    dif05 <-   c05/abs(tmp1) - c05/abs(tmp2)
    off05 <- ((   abs(tmp1) +   abs(tmp2) ) / 2) / c05
    tmp1 <-   ifelse(lg10 - c10==0, 1, lg10 - c10)
    tmp2 <-   ifelse(li10 - c10==0, 1, li10 - c10)
    tmp3 <-   ifelse(tw10 - c10==0, 1, tw10 - c10)
    dlg10 <- tmp1 / c10
    dli10 <- tmp2 / c10
    dtw10 <- tmp3 / c10
    dif10 <-   c10/abs(tmp1) - c10/abs(tmp2)
    off10 <- ((   abs(tmp1) +   abs(tmp2) ) / 2) / c10
    tmp1 <-   ifelse(lg20 - c20==0, 1, lg20 - c20)
    tmp2 <-   ifelse(li20 - c20==0, 1, li20 - c20)
    tmp3 <-   ifelse(tw20 - c20==0, 1, tw20 - c20)
    dlg20 <- tmp1 / c20
    dli20 <- tmp2 / c20
    dtw20 <- tmp3 / c20
    dif20 <-   c20/abs(tmp1) - c20/abs(tmp2)
    off20 <- ((   abs(tmp1) +   abs(tmp2) ) / 2) / c20
})
tmp$tmp1 <- tmp$tmp2 <- tmp$tmp3 <- NULL
tmp[,grep("^dif|^off|^dlg|^dli|^dtw", colnames(tmp))] <- round(tmp[,grep("^dif|^off|^dlg|^dli|^dtw", colnames(tmp))], 2)
tmp <- tmp[moveme(names(tmp), "dlg00 after dlg95")]
tmp <- tmp[moveme(names(tmp), "dlg05 after dlg00")]
tmp <- tmp[moveme(names(tmp), "dlg10 after dlg05")]
tmp <- tmp[moveme(names(tmp), "dlg20 after dlg10")]
tmp <- tmp[moveme(names(tmp), "dli95 after dlg95")]
tmp <- tmp[moveme(names(tmp), "dli00 after dlg00")]
tmp <- tmp[moveme(names(tmp), "dli05 after dlg05")]
tmp <- tmp[moveme(names(tmp), "dli10 after dlg10")]
tmp <- tmp[moveme(names(tmp), "dli20 after dlg20")]
tmp <- tmp[moveme(names(tmp), "dtw95 after dli95")]
tmp <- tmp[moveme(names(tmp), "dtw00 after dli00")]
tmp <- tmp[moveme(names(tmp), "dtw05 after dli05")]
tmp <- tmp[moveme(names(tmp), "dtw10 after dli10")]
tmp <- tmp[moveme(names(tmp), "dtw20 after dli20")]
tmp <- tmp[moveme(names(tmp), "dif95 after dtw95")]
tmp <- tmp[moveme(names(tmp), "dif00 after dtw00")]
tmp <- tmp[moveme(names(tmp), "dif05 after dtw05")]
tmp <- tmp[moveme(names(tmp), "dif10 after dtw10")]
tmp <- tmp[moveme(names(tmp), "dif20 after dtw20")]
tmp <- tmp[moveme(names(tmp), "off95 after dif95")]
tmp <- tmp[moveme(names(tmp), "off00 after dif00")]
tmp <- tmp[moveme(names(tmp), "off05 after dif05")]
tmp <- tmp[moveme(names(tmp), "off10 after dif10")]
tmp <- tmp[moveme(names(tmp), "off20 after dif20")]

tmp[1,]

## Interpretation:
## dif=0 -- log and linear perform identical 
## dif>0 -- log   outperforms linear
## dif<0 -- log underperforms linear
## off is mean absolute dev from census
tmp[1:10,grep("inegi|^dlg|^dli|^dif|^off", colnames(tmp))]
tmp[1:10,grep("inegi|^dlg95|^dli95|^dif95|^off95", colnames(tmp))]
tmp[1:10,grep("inegi|95", colnames(tmp))]
round(quantile(tmp$dif95, probs = seq(0,1,.025), na.rm=TRUE),1)
round(quantile(tmp$dlg95, probs = seq(0,1,.025), na.rm=TRUE),1)
round(quantile(tmp$dli95, probs = seq(0,1,.025), na.rm=TRUE),1)
round(quantile(tmp$dif00, probs = seq(0,1,.025), na.rm=TRUE),1)
round(quantile(tmp$dif05, probs = seq(0,1,.025), na.rm=TRUE),1)
summary(tmp$off95)
round(apply(tmp[,grep("^dif|^off", colnames(tmp))], 2, summary), 3)

summary(tmp$dli95 - tmp$dtw95)



## ## yearly estimates prep for use in plots
## tmp2 <- data.frame(
##     li94=interlog(what="p18", yr=1994, unit="m", census.data = censom00, frm="dv~iv")
##     )
## tmp2$li95 <- interlog(what="p18", yr=1995, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li96 <- interlog(what="p18", yr=1996, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li97 <- interlog(what="p18", yr=1997, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li98 <- interlog(what="p18", yr=1998, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li99 <- interlog(what="p18", yr=1999, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li00 <- interlog(what="p18", yr=2000, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li01 <- interlog(what="p18", yr=2001, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li02 <- interlog(what="p18", yr=2002, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li03 <- interlog(what="p18", yr=2003, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li04 <- interlog(what="p18", yr=2004, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li05 <- interlog(what="p18", yr=2005, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li06 <- interlog(what="p18", yr=2006, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li07 <- interlog(what="p18", yr=2007, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li08 <- interlog(what="p18", yr=2008, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li09 <- interlog(what="p18", yr=2009, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li10 <- interlog(what="p18", yr=2010, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li11 <- interlog(what="p18", yr=2011, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li12 <- interlog(what="p18", yr=2012, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li13 <- interlog(what="p18", yr=2013, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li14 <- interlog(what="p18", yr=2014, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li15 <- interlog(what="p18", yr=2015, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li16 <- interlog(what="p18", yr=2016, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li17 <- interlog(what="p18", yr=2017, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li18 <- interlog(what="p18", yr=2018, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li19 <- interlog(what="p18", yr=2019, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li20 <- interlog(what="p18", yr=2020, unit="m", census.data = censom00, frm="dv~iv")
## tmp2$li21 <- interlog(what="p18", yr=2021, unit="m", census.data = censom00, frm="dv~iv")
## ##
## tmp2$lg94 <- interlog(what="p18", yr=1994, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg95 <- interlog(what="p18", yr=1995, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg96 <- interlog(what="p18", yr=1996, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg97 <- interlog(what="p18", yr=1997, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg98 <- interlog(what="p18", yr=1998, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg99 <- interlog(what="p18", yr=1999, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg00 <- interlog(what="p18", yr=2000, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg01 <- interlog(what="p18", yr=2001, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg02 <- interlog(what="p18", yr=2002, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg03 <- interlog(what="p18", yr=2003, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg04 <- interlog(what="p18", yr=2004, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg05 <- interlog(what="p18", yr=2005, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg06 <- interlog(what="p18", yr=2006, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg07 <- interlog(what="p18", yr=2007, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg08 <- interlog(what="p18", yr=2008, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg09 <- interlog(what="p18", yr=2009, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg10 <- interlog(what="p18", yr=2010, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg11 <- interlog(what="p18", yr=2011, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg12 <- interlog(what="p18", yr=2012, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg13 <- interlog(what="p18", yr=2013, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg14 <- interlog(what="p18", yr=2014, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg15 <- interlog(what="p18", yr=2015, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg16 <- interlog(what="p18", yr=2016, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg17 <- interlog(what="p18", yr=2017, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg18 <- interlog(what="p18", yr=2018, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg19 <- interlog(what="p18", yr=2019, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg20 <- interlog(what="p18", yr=2020, unit="m", census.data = censom00, frm="log(dv)~iv")
## tmp2$lg21 <- interlog(what="p18", yr=2021, unit="m", census.data = censom00, frm="log(dv)~iv")


## Inspect census figs
i <- i+1
ls <- c(
    v94m00$lisnom[i],
    v97m00$lisnom[i],
    v00m  $lisnom[i],
    v03m00$lisnom[i],
    v06m00$lisnom[i],
    v09m00$lisnom[i],
    v12m00$lisnom[i],
    v15m00$lisnom[i],
    v18m00$lisnom[i],
    v21m00$lisnom[i]
)
ef <- c(
    v94m00$efec[i],
    v97m00$efec[i],
    v00m  $efec[i],
    v03m00$efec[i],
    v06m00$efec[i],
    v09m00$efec[i],
    v12m00$efec[i],
    v15m00$efec[i],
    v18m00$efec[i],
    v21m00$efec[i]
)
li <- c(
    v94m00$pob18l[i],
    v97m00$pob18l[i],
    v00m  $pob18l[i],
    v03m00$pob18l[i],
    v06m00$pob18l[i],
    v09m00$pob18l[i],
    v12m00$pob18l[i],
    v15m00$pob18l[i],
    v18m00$pob18l[i],
    v21m00$pob18l[i]
)
lg <- c(
    v94m00$pob18e[i],
    v97m00$pob18e[i],
    v00m  $pob18e[i],
    v03m00$pob18e[i],
    v06m00$pob18e[i],
    v09m00$pob18e[i],
    v12m00$pob18e[i],
    v15m00$pob18e[i],
    v18m00$pob18e[i],
    v21m00$pob18e[i]
)
tw <- c(
    v94m00$pob182[i],
    v97m00$pob182[i],
    v00m  $pob182[i],
    v03m00$pob182[i],
    v06m00$pob182[i],
    v09m00$pob182[i],
    v12m00$pob182[i],
    v15m00$pob182[i],
    v18m00$pob182[i],
    v21m00$pob182[i]
)
c1 <- as.numeric(censom00$p18_1995[i])
c2 <- as.numeric(censom00$p18_2000[i])
##
plot(  seq(1994,2021,3), li, ylim = c(min(li,lg,ls   ),max(li,lg,ls   )), main = paste(i, censom00$mun[i], "bck=line, red=log, blue=lis, solid=cs"))
##plot(  seq(1994,2021,3), li, ylim = c(min(li,lg,ls,ef),max(li,lg,ls,ef)), main = paste(i, "bck=line, red=log, blue=lis, grn=efec"))
points(seq(1994,2021,3), lg, col = "red")
##points(seq(1994,2021,3), ls, col = "blue")
points(seq(1994,2021,3), ef, col = "green")
points(1995, c1, pch = 19, cex = .75)
points(  2000,            c2, pch = 19, cex = .75)
points(c(2005,2010,2020), censom00[i,c("p18_2005","p18_2010","p18_2020")], pch=19, cex=.75)
##abline(v=2000, lty=2)
##


