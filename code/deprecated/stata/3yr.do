do df

sort yr
by yr: egen pan1=sum(pan)
by yr: egen pri1=sum(pri)
by yr: egen prd1=sum(prd)
by yr: egen efec1=sum(votoefec)

drop if edon~=15

keep yr pan1 pri1 prd1 efec1

save "C:\DATA\3yrtmp.dta", replace

use "c:\data\go ed 1961-2004.dta", clear

gen tmp=.
replace tmp=1 if mo=="ene"
replace tmp=2 if mo=="feb"
replace tmp=3 if mo=="mar"
replace tmp=4 if mo=="abr"
replace tmp=5 if mo=="may"
replace tmp=6 if mo=="jun"
replace tmp=7 if mo=="jul"
replace tmp=8 if mo=="ago"
replace tmp=9 if mo=="sep"
replace tmp=10 if mo=="oct"
replace tmp=11 if mo=="nov"
replace tmp=12 if mo=="dic"

move tmp mo
drop mo
rename tmp mo

gen yr3=1976
replace yr3=1979 if yr==1976 & mo>7
replace yr3=1979 if yr==1977 | yr==1978
replace yr3=1979 if yr==1979 & mo<=7
replace yr3=1982 if yr==1979 & mo>7
replace yr3=1982 if yr==1980 | yr==1981
replace yr3=1982 if yr==1982 & mo<=7
replace yr3=1985 if yr==1982 & mo>7
replace yr3=1985 if yr==1983 | yr==1984
replace yr3=1985 if yr==1985 & mo<=7
replace yr3=1988 if yr==1985 & mo>7
replace yr3=1988 if yr==1986 | yr==1987
replace yr3=1988 if yr==1988 & mo<=7
replace yr3=1991 if yr==1988 & mo>7
replace yr3=1991 if yr==1989 | yr==1990
replace yr3=1991 if yr==1991 & mo<=8
replace yr3=1994 if yr==1991 & mo>8
replace yr3=1994 if yr==1992 | yr==1993
replace yr3=1994 if yr==1994 & mo<=8
replace yr3=1997 if yr==1994 & mo>8
replace yr3=1997 if yr==1995 | yr==1996
replace yr3=1997 if yr==1997 & mo<=7
replace yr3=2000 if yr==1997 & mo>7
replace yr3=2000 if yr==1998 | yr==1999
replace yr3=2000 if yr==2000 & mo<=7
replace yr3=2003 if yr==2000 & mo>7
replace yr3=2003 if yr==2001 | yr==2002
replace yr3=2003 if yr==2003 & mo<=7

move yr3 yr
drop yr
rename yr3 yr

*quita la eleccion extraordinaria de colima de 2003
drop if edon==6 & yr==2003 & mo==12

gen tmp1=pan+vcoalpan*distpan
move tmp1 pan
gen tmp2=pri+vcoalpri*distpri
move tmp2 pri
gen tmp3=prd+vcoalprd*distprd
move tmp3 prd
gen dcoalpan=0
gen dcoalpri=0
gen dcoalprd=0
replace dcoalpan=1 if dcoal==1 & coalpan~="no"
replace dcoalpri=1 if dcoal==1 & coalpri~="no"
replace dcoalprd=1 if dcoal==1 & coalprd~="no"
move dcoalpan vcoalpan
move dcoalpri vcoalprd
move dcoalprd vcoalprd
drop ordorig pan pri prd vcoalpan vcoalpri vcoalprd distpan distpri distprd nr nulos vtot lisnom fuente nota 
rename tmp1 pan
rename tmp2 pri
rename tmp3 prd

gen pansh=pan/efec
gen prish=pri/efec
gen prdsh=prd/efec

gsort yr
by yr: egen gopan1=sum(pan)
by yr: egen gopri1=sum(pri)
by yr: egen goprd1=sum(prd)
by yr: egen goefec1=sum(efec)

keep yr gopan1 gopri1 goprd1 goefec1
drop if gopan1==gopan1[_n-1]

merge yr using 3yrtmp
drop if yr>2003
drop _merge
sort yr
save "C:\DATA\3yrtmp.dta", replace

use "c:\data\pr ed 1964-2000.dta", clear

gen tmp=.
replace tmp=1 if mo=="ene"
replace tmp=2 if mo=="feb"
replace tmp=3 if mo=="mar"
replace tmp=4 if mo=="abr"
replace tmp=5 if mo=="may"
replace tmp=6 if mo=="jun"
replace tmp=7 if mo=="jul"
replace tmp=8 if mo=="ago"
replace tmp=9 if mo=="sep"
replace tmp=10 if mo=="oct"
replace tmp=11 if mo=="nov"
replace tmp=12 if mo=="dic"

move tmp mo
drop mo
rename tmp mo

gen tmp1=pan+vcoalpan*distpan
move tmp1 pan
gen tmp2=pri+vcoalpri*distpri
move tmp2 pri
gen tmp3=prd+vcoalprd*distprd
move tmp3 prd
gen dcoalpan=0
gen dcoalpri=0
gen dcoalprd=0
replace dcoalpan=1 if dcoal==1 & coalpan~="no"
replace dcoalpri=1 if dcoal==1 & coalpri~="no"
replace dcoalprd=1 if dcoal==1 & coalprd~="no"
move dcoalpan vcoalpan
move dcoalpri vcoalprd
move dcoalprd vcoalprd
drop ordorig pan pri prd vcoalpan vcoalpri vcoalprd distpan distpri distprd nr nulos vtot lisnom fuente notas 
rename tmp1 pan
rename tmp2 pri
rename tmp3 prd

gen pansh=pan/efec
gen prish=pri/efec
gen prdsh=prd/efec

sort yr
by yr: egen prpan1=sum(pan)
by yr: egen prpri1=sum(pri)
by yr: egen prprd1=sum(prd)
by yr: egen prefec1=sum(efec)

drop if edon~=1

keep yr prpan1 prpri1 prprd1 prefec1
merge yr using 3yrtmp

sort yr

gen pan1sh=pan1*100/efec1
gen pri1sh=pri1*100/efec1
gen prd1sh=prd1*100/efec1

gen gopan1sh=gopan1*100/goefec1
gen gopri1sh=gopri1*100/goefec1
gen goprd1sh=goprd1*100/goefec1

gen prpan1sh=prpan1*100/prefec1
gen prpri1sh=prpri1*100/prefec1
gen prprd1sh=prprd1*100/prefec1

*para que al quitarle pvem no parezca que fox y flo empataron
replace prpan1sh=40 if yr==2000

gen str1 prlab="p"
gen str1 golab="g"
gen str1 dflab="d"

gen tmp=prpan1sh
gen tmp2=gopan1sh

replace pan1sh=pan1sh/100
replace prpan1sh=prpan1sh/100
replace tmp=tmp/100
replace pri1sh=pri1sh/100
replace prpri1sh=prpri1sh/100
replace gopan1sh=gopan1sh/100
replace tmp2=tmp2/100
replace gopri1sh=gopri1sh/100

gr7 pan1sh prpan1sh tmp pri1sh prpri1sh yr if yr>1976,  xlabel(1979[3]2003) ylab(0[.25]1) c(l..l.) symbol(.Op.O) t1 ("lines = deputy vote (PRI starts above, PAN below)")t2("circles = presidential vote (PRI hollow, PAN shaded)") b2("year") l1("vote share") pen(22233) saving(c:\data\pres, replace)

gr7 pan1sh gopan1sh tmp2 pri1sh gopri1sh yr if yr>1976,  xlabel(1979[3]2003) ylab(0[.25]1) c(l..l.) symbol(.Tp.T) t1 ("lines = deputy vote (PRI starts above, PAN below)")t2("triangles = governor vote (PRI hollow, PAN shaded)") b2("year") l1("vote share") pen(22233) saving(c:\data\gob, replace) 



