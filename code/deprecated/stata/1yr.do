do df

sort yr
by yr: egen pan1=sum(pan)
by yr: egen pri1=sum(pri)
by yr: egen prd1=sum(prd)
by yr: egen efec1=sum(votoefec)

drop if edon~=15

keep yr pan1 pri1 prd1 efec1

save "C:\DATA\yrtmp.dta", replace

do gotmp

gsort yr
by yr: egen gopan1=sum(pan)
by yr: egen gopri1=sum(pri)
by yr: egen goprd1=sum(prd)
by yr: egen goefec1=sum(efec)

keep yr gopan1 gopri1 goprd1 goefec1
drop if gopan1==gopan1[_n-1]

merge yr using yrtmp
drop if yr>2003
drop _merge
sort yr
save "C:\DATA\yrtmp.dta", replace

do prtmp

sort yr
by yr: egen prpan1=sum(pan)
by yr: egen prpri1=sum(pri)
by yr: egen prprd1=sum(prd)
by yr: egen prefec1=sum(efec)

drop if edon~=1

keep yr prpan1 prpri1 prprd1 prefec1
merge yr using yrtmp

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


gr7 pan1sh gopan1sh prpan1sh pri1sh gopri1sh prpri1sh yr if yr>=1976, c(llllll) xlabel(1976[3]2003)xtick(1977[1]2001)


