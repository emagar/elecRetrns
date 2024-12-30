use "c:\data\dl ed 1979-2004.dta", clear

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

**********************************************************************
*hay que verificar que lo que está entre estas líneas estrelladas funcione... lo hice para otro do file

gen midterm=0
replace midterm=1 if edon==1 & yr==1989
replace midterm=1 if edon==1 & yr==1995
replace midterm=1 if edon==1 & yr==2001
replace midterm=1 if edon==2 & yr==1992
replace midterm=1 if edon==2 & yr==1998
replace midterm=1 if edon==2 & yr==2004
replace midterm=1 if edon==3 & yr==1990
replace midterm=1 if edon==3 & yr==1996
replace midterm=1 if edon==3 & yr==2002
replace midterm=1 if edon==4 & yr==1989
replace midterm=1 if edon==4 & yr==1994
replace midterm=1 if edon==4 & yr==2000
replace midterm=1 if edon==5 & yr==1991
replace midterm=1 if edon==5 & yr==1996
replace midterm=1 if edon==5 & yr==2002
replace midterm=1 if edon==6 & yr==1988
replace midterm=1 if edon==6 & yr==1994
replace midterm=1 if edon==6 & yr==2000
replace midterm=1 if edon==7 & yr==1991
replace midterm=1 if edon==7 & yr==1998
replace midterm=1 if edon==7 & yr==2004
replace midterm=1 if edon==8 & yr==1989
replace midterm=1 if edon==8 & yr==1995
replace midterm=1 if edon==8 & yr==2001
replace midterm=0 if edon==9 & yr==1991  // df's cases dropped
replace midterm=0 if edon==9 & yr==1997  
replace midterm=0 if edon==9 & yr==2003  
replace midterm=1 if edon==10 & yr==1989
replace midterm=1 if edon==10 & yr==1995
replace midterm=1 if edon==10 & yr==2001
replace midterm=1 if edon==11 & yr==1988
replace midterm=1 if edon==11 & yr==1994
replace midterm=1 if edon==11 & yr==1997
replace midterm=1 if edon==11 & yr==2003
replace midterm=1 if edon==12 & yr==1989
replace midterm=1 if edon==12 & yr==1996
replace midterm=1 if edon==12 & yr==2002
replace midterm=1 if edon==13 & yr==1990
replace midterm=1 if edon==13 & yr==1996
replace midterm=1 if edon==13 & yr==2002
replace midterm=1 if edon==14 & yr==1992
replace midterm=1 if edon==14 & yr==1997
replace midterm=1 if edon==14 & yr==2003
replace midterm=1 if edon==15 & yr==1990
replace midterm=1 if edon==15 & yr==1996
replace midterm=1 if edon==15 & yr==2003
replace midterm=1 if edon==16 & yr==1989
replace midterm=1 if edon==16 & yr==1998
replace midterm=1 if edon==16 & yr==2004
replace midterm=1 if edon==17 & yr==1991
replace midterm=1 if edon==17 & yr==1997
replace midterm=1 if edon==17 & yr==2003
replace midterm=1 if edon==18 & yr==1990
replace midterm=1 if edon==18 & yr==1996
replace midterm=1 if edon==18 & yr==2002
replace midterm=1 if edon==19 & yr==1988
replace midterm=1 if edon==19 & yr==1994
replace midterm=1 if edon==19 & yr==2000
replace midterm=1 if edon==20 & yr==1989
replace midterm=1 if edon==20 & yr==1995
replace midterm=1 if edon==20 & yr==2001
replace midterm=1 if edon==21 & yr==1989
replace midterm=1 if edon==21 & yr==1995
replace midterm=1 if edon==21 & yr==2001
replace midterm=1 if edon==22 & yr==1988
replace midterm=1 if edon==22 & yr==1994
replace midterm=1 if edon==22 & yr==2000
replace midterm=1 if edon==23 & yr==1990
replace midterm=1 if edon==23 & yr==1996
replace midterm=1 if edon==23 & yr==2002
replace midterm=1 if edon==24 & yr==1991
replace midterm=1 if edon==24 & yr==2000
replace midterm=1 if edon==25 & yr==1989
replace midterm=1 if edon==25 & yr==1995
replace midterm=1 if edon==25 & yr==2001
replace midterm=1 if edon==26 & yr==1988
replace midterm=1 if edon==26 & yr==1994
replace midterm=1 if edon==26 & yr==2000
replace midterm=1 if edon==27 & yr==1991
replace midterm=1 if edon==27 & yr==1997
replace midterm=1 if edon==27 & yr==2001
replace midterm=1 if edon==28 & yr==1989
replace midterm=1 if edon==28 & yr==1995
replace midterm=1 if edon==28 & yr==2001
replace midterm=1 if edon==29 & yr==1989
replace midterm=1 if edon==29 & yr==1995
replace midterm=1 if edon==29 & yr==2001
replace midterm=1 if edon==30 & yr==1989
replace midterm=1 if edon==30 & yr==1995
replace midterm=1 if edon==30 & yr==2000
replace midterm=1 if edon==31 & yr==1990
replace midterm=1 if edon==31 & yr==1998
replace midterm=1 if edon==31 & yr==2004
replace midterm=1 if edon==32 & yr==1989
replace midterm=1 if edon==32 & yr==1995
replace midterm=1 if edon==32 & yr==2001
***********************************************************

* como solo meti los distpan distpri y distprd para los años en que dl concurre con df calculo distinto
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

gen panbar=.
gen pribar=.
gen prdbar=.
gen panse=.
gen prise=.
gen prdse=.
gen panres=.
gen prires=.
gen prdres=.

gen panshbar=.
gen prishbar=.
gen prdshbar=.
gen panshse=.
gen prishse=.
gen prdshse=.
gen panshres=.
gen prishres=.
gen prdshres=.

local i = 1
while `i' <= 32   {
	quietly reg pan yr if edon==`i'
	quietly predict bar
	quietly predict se, stdp
	quietly predict e, resid
	quietly replace panbar=bar if edon==`i'
	quietly replace panse=se if edon==`i'
	quietly replace panres=e if edon==`i'
	drop bar se e
	quietly reg pri yr if edon==`i'
	quietly predict bar
	quietly predict se, stdp
	quietly predict e, resid
	quietly replace pribar=bar if edon==`i'
	quietly replace prise=se if edon==`i'
	quietly replace prires=e if edon==`i'
	drop bar se e
	quietly reg prd yr if edon==`i'
	quietly predict bar
	quietly predict se, stdp
	quietly predict e, resid
	quietly replace prdbar=bar if edon==`i'
	quietly replace prdse=se if edon==`i'
	quietly replace prdres=e if edon==`i'
	drop bar se e
	quietly reg pansh yr if edon==`i'
	quietly predict bar
	quietly predict se, stdp
	quietly predict e, resid
	quietly replace panshbar=bar if edon==`i'
	quietly replace panshse=se if edon==`i'
	quietly replace panshres=e if edon==`i'
	drop bar se e
	quietly reg prish yr if edon==`i'
	quietly predict bar
	quietly predict se, stdp
	quietly predict e, resid
	quietly replace prishbar=bar if edon==`i'
	quietly replace prishse=se if edon==`i'
	quietly replace prishres=e if edon==`i'
	drop bar se e
	quietly reg prdsh yr if edon==`i'
	quietly predict bar
	quietly predict se, stdp
	quietly predict e, resid
	quietly replace prdshbar=bar if edon==`i'
	quietly replace prdshse=se if edon==`i'
	quietly replace prdshres=e if edon==`i'
	drop bar se e
	local i = `i' + 1
}


gen str3 panlab="pan"
gen str3 prilab="pri"
gen str3 prdlab="prd"

sort edon
gen tmp1=panres^2
gen tmp2=prires^2
gen tmp3=prdres^2
by edon: egen tmp4=sum(tmp1)
by edon: egen tmp5=count(tmp1)
by edon: egen tmp6=sum(tmp2)
by edon: egen tmp7=count(tmp2)
by edon: egen tmp8=sum(tmp3)
by edon: egen tmp9=count(tmp3)
gen pansd=sqrt(tmp4/tmp5)
gen prisd=sqrt(tmp6/tmp7)
gen prdsd=sqrt(tmp8/tmp9)
drop tmp1 tmp2 tmp3 tmp4 tmp5 tmp6 tmp7 tmp8 tmp9

gen tmp1=panshres^2
gen tmp2=prishres^2
gen tmp3=prdshres^2
by edon: egen tmp4=sum(tmp1)
by edon: egen tmp5=count(tmp1)
by edon: egen tmp6=sum(tmp2)
by edon: egen tmp7=count(tmp2)
by edon: egen tmp8=sum(tmp3)
by edon: egen tmp9=count(tmp3)
gen panshsd=sqrt(tmp4/tmp5)
gen prishsd=sqrt(tmp6/tmp7)
gen prdshsd=sqrt(tmp8/tmp9)
drop tmp1 tmp2 tmp3 tmp4 tmp5 tmp6 tmp7 tmp8 tmp9


gen panhi=panbar+pansd
gen panlo=panbar-pansd
gen prihi=pribar+prisd
gen prilo=pribar-prisd
gen prdhi=prdbar+prdsd
gen prdlo=prdbar-prdsd

gen panshhi=panshbar+panshsd
gen panshlo=panshbar-panshsd
gen prishhi=prishbar+prishsd
gen prishlo=prishbar-prishsd
gen prdshhi=prdshbar+prdshsd
gen prdshlo=prdshbar-prdshsd

gen panstres=panres/pansd
gen pristres=prires/prisd
gen prdstres=prdres/prdsd

gen panshstres=panshres/panshsd
gen prishstres=prishres/prishsd
gen prdshstres=prdshres/prdshsd


rename panres dlpanres
rename prires dlprires
rename prdres dlprdres
rename panshres dlpanshres
rename prishres dlprishres
rename prdshres dlprdshres
rename panstres dlpanstres
rename pristres dlpristres
rename prdstres dlprdstres
rename panshstres dlpanshstres
rename prishstres dlprishstres
rename prdshstres dlprdshstres
xx
keep clave elec yr edon edo dlpanres dlprires dlprdres dlpanshres dlprishres dlprdshres dlpanstres dlpristres dlprdstres dlpanshstres dlprishstres dlprdshstres 

sort clave
save "c:\data\tmp3.dta", replace

xx

gr7 pan pri prd panbar pribar prdbar panhi panlo prihi prilo prdhi prdlo yr if edon==1, c(...lllllllll) s([panlab][prilab][prdlab]iiiiiiiii) psize(150) xlab(1976[3]2003) ylab pen(234234223344) t1(Aguascalientes)


gr7 pan pri prd panbar pribar prdbar yr if edon==1, c(...lll) s([panlab][prilab][prdlab]iii) psize(150) xlab(1976[3]2003) ylab pen(234234) l2title("Votos, elección federal de diputados") b2(año) t1(Aguascalientes)  saving(c:\data\ags, replace)
gr7 pan pri prd panbar pribar prdbar yr if edon==2, c(...lll) s([panlab][prilab][prdlab]iii) psize(150) xlab(1976[3]2003) ylab pen(234234) l2title("Votos, elección federal de diputados") b2(año) t1(Baja California Norte)  saving(c:\data\bcn, replace)
gr7 pan pri prd panbar pribar prdbar yr if edon==3, c(...lll) s([panlab][prilab][prdlab]iii) psize(150) xlab(1976[3]2003) ylab pen(234234) l2title("Votos, elección federal de diputados") b2(año) t1(Baja California Sur)  saving(c:\data\bcs, replace)
gr7 pan pri prd panbar pribar prdbar yr if edon==4, c(...lll) s([panlab][prilab][prdlab]iii) psize(150) xlab(1976[3]2003) ylab pen(234234) l2title("Votos, elección federal de diputados") b2(año) t1(Campeche)  saving(c:\data\cam, replace)
gr7 pan pri prd panbar pribar prdbar yr if edon==5, c(...lll) s([panlab][prilab][prdlab]iii) psize(150) xlab(1976[3]2003) ylab pen(234234) l2title("Votos, elección federal de diputados") b2(año) t1(Coahuila)  saving(c:\data\coa, replace)
gr7 pan pri prd panbar pribar prdbar yr if edon==6, c(...lll) s([panlab][prilab][prdlab]iii) psize(150) xlab(1976[3]2003) ylab pen(234234) l2title("Votos, elección federal de diputados") b2(año) t1(Colima)  saving(c:\data\col, replace)
gr7 pan pri prd panbar pribar prdbar yr if edon==7, c(...lll) s([panlab][prilab][prdlab]iii) psize(150) xlab(1976[3]2003) ylab pen(234234) l2title("Votos, elección federal de diputados") b2(año) t1(Chiapas)  saving(c:\data\cps, replace)
gr7 pan pri prd panbar pribar prdbar yr if edon==8, c(...lll) s([panlab][prilab][prdlab]iii) psize(150) xlab(1976[3]2003) ylab pen(234234) l2title("Votos, elección federal de diputados") b2(año) t1(Chihuahua)  saving(c:\data\cua, replace)
gr7 pan pri prd panbar pribar prdbar yr if edon==9, c(...lll) s([panlab][prilab][prdlab]iii) psize(150) xlab(1976[3]2003) ylab pen(234234) l2title("Votos, elección federal de diputados") b2(año) t1(Distrito Federal)  saving(c:\data\df, replace)
gr7 pan pri prd panbar pribar prdbar yr if edon==10, c(...lll) s([panlab][prilab][prdlab]iii) psize(150) xlab(1976[3]2003) ylab pen(234234) l2title("Votos, elección federal de diputados") b2(año) t1(Durango)  saving(c:\data\dgo, replace)
gr7 pan pri prd panbar pribar prdbar yr if edon==11, c(...lll) s([panlab][prilab][prdlab]iii) psize(150) xlab(1976[3]2003) ylab pen(234234) l2title("Votos, elección federal de diputados") b2(año) t1(Guanajuato)  saving(c:\data\gua, replace)
gr7 pan pri prd panbar pribar prdbar yr if edon==12, c(...lll) s([panlab][prilab][prdlab]iii) psize(150) xlab(1976[3]2003) ylab pen(234234) l2title("Votos, elección federal de diputados") b2(año) t1(Guerrero)  saving(c:\data\gue, replace)
gr7 pan pri prd panbar pribar prdbar yr if edon==13, c(...lll) s([panlab][prilab][prdlab]iii) psize(150) xlab(1976[3]2003) ylab pen(234234) l2title("Votos, elección federal de diputados") b2(año) t1(Hidalgo)  saving(c:\data\hgo, replace)
gr7 pan pri prd panbar pribar prdbar yr if edon==14, c(...lll) s([panlab][prilab][prdlab]iii) psize(150) xlab(1976[3]2003) ylab pen(234234) l2title("Votos, elección federal de diputados") b2(año) t1(Jalisco)  saving(c:\data\jal, replace)
gr7 pan pri prd panbar pribar prdbar yr if edon==15, c(...lll) s([panlab][prilab][prdlab]iii) psize(150) xlab(1976[3]2003) ylab pen(234234) l2title("Votos, elección federal de diputados") b2(año) t1(México)  saving(c:\data\mex, replace)
gr7 pan pri prd panbar pribar prdbar yr if edon==16, c(...lll) s([panlab][prilab][prdlab]iii) psize(150) xlab(1976[3]2003) ylab pen(234234) l2title("Votos, elección federal de diputados") b2(año) t1(Michoacán)  saving(c:\data\mic, replace)
gr7 pan pri prd panbar pribar prdbar yr if edon==17, c(...lll) s([panlab][prilab][prdlab]iii) psize(150) xlab(1976[3]2003) ylab pen(234234) l2title("Votos, elección federal de diputados") b2(año) t1(Morelos)  saving(c:\data\mor, replace)
gr7 pan pri prd panbar pribar prdbar yr if edon==18, c(...lll) s([panlab][prilab][prdlab]iii) psize(150) xlab(1976[3]2003) ylab pen(234234) l2title("Votos, elección federal de diputados") b2(año) t1(Nayarit)  saving(c:\data\nay, replace)
gr7 pan pri prd panbar pribar prdbar yr if edon==19, c(...lll) s([panlab][prilab][prdlab]iii) psize(150) xlab(1976[3]2003) ylab pen(234234) l2title("Votos, elección federal de diputados") b2(año) t1(Nuevo León)  saving(c:\data\nl, replace)
gr7 pan pri prd panbar pribar prdbar yr if edon==20, c(...lll) s([panlab][prilab][prdlab]iii) psize(150) xlab(1976[3]2003) ylab pen(234234) l2title("Votos, elección federal de diputados") b2(año) t1(Oaxaca)  saving(c:\data\oax, replace)
gr7 pan pri prd panbar pribar prdbar yr if edon==21, c(...lll) s([panlab][prilab][prdlab]iii) psize(150) xlab(1976[3]2003) ylab pen(234234) l2title("Votos, elección federal de diputados") b2(año) t1(Puebla)  saving(c:\data\pue, replace)
gr7 pan pri prd panbar pribar prdbar yr if edon==22, c(...lll) s([panlab][prilab][prdlab]iii) psize(150) xlab(1976[3]2003) ylab pen(234234) l2title("Votos, elección federal de diputados") b2(año) t1(Querétaro)  saving(c:\data\que, replace)
gr7 pan pri prd panbar pribar prdbar yr if edon==23, c(...lll) s([panlab][prilab][prdlab]iii) psize(150) xlab(1976[3]2003) ylab pen(234234) l2title("Votos, elección federal de diputados") b2(año) t1(Quintana Roo)  saving(c:\data\qui, replace)
gr7 pan pri prd panbar pribar prdbar yr if edon==24, c(...lll) s([panlab][prilab][prdlab]iii) psize(150) xlab(1976[3]2003) ylab pen(234234) l2title("Votos, elección federal de diputados") b2(año) t1(San Luis Potosí)  saving(c:\data\san, replace)
gr7 pan pri prd panbar pribar prdbar yr if edon==25, c(...lll) s([panlab][prilab][prdlab]iii) psize(150) xlab(1976[3]2003) ylab pen(234234) l2title("Votos, elección federal de diputados") b2(año) t1(Sinaloa)  saving(c:\data\sin, replace)
gr7 pan pri prd panbar pribar prdbar yr if edon==26, c(...lll) s([panlab][prilab][prdlab]iii) psize(150) xlab(1976[3]2003) ylab pen(234234) l2title("Votos, elección federal de diputados") b2(año) t1(Sonora)  saving(c:\data\son, replace)
gr7 pan pri prd panbar pribar prdbar yr if edon==27, c(...lll) s([panlab][prilab][prdlab]iii) psize(150) xlab(1976[3]2003) ylab pen(234234) l2title("Votos, elección federal de diputados") b2(año) t1(Tabasco)  saving(c:\data\tab, replace)
gr7 pan pri prd panbar pribar prdbar yr if edon==28, c(...lll) s([panlab][prilab][prdlab]iii) psize(150) xlab(1976[3]2003) ylab pen(234234) l2title("Votos, elección federal de diputados") b2(año) t1(Tamaulipas)  saving(c:\data\tam, replace)
gr7 pan pri prd panbar pribar prdbar yr if edon==29, c(...lll) s([panlab][prilab][prdlab]iii) psize(150) xlab(1976[3]2003) ylab pen(234234) l2title("Votos, elección federal de diputados") b2(año) t1(Tlaxcala)  saving(c:\data\tla, replace)
gr7 pan pri prd panbar pribar prdbar yr if edon==30, c(...lll) s([panlab][prilab][prdlab]iii) psize(150) xlab(1976[3]2003) ylab pen(234234) l2title("Votos, elección federal de diputados") b2(año) t1(Veracruz)  saving(c:\data\ver, replace)
gr7 pan pri prd panbar pribar prdbar yr if edon==31, c(...lll) s([panlab][prilab][prdlab]iii) psize(150) xlab(1976[3]2003) ylab pen(234234) l2title("Votos, elección federal de diputados") b2(año) t1(Yucatán)  saving(c:\data\yuc, replace)
gr7 pan pri prd panbar pribar prdbar yr if edon==32, c(...lll) s([panlab][prilab][prdlab]iii) psize(150) xlab(1976[3]2003) ylab pen(234234) l2title("Votos, elección federal de diputados") b2(año) t1(Zacatecas) saving(c:\data\zac, replace)

gr7 using ags bcn bcs cam coa col cps cua df dgo gua gue hgo jal mex mic 

gr7 using mor nay nl oax pue que qui san sin son tab tam tla ver yuc zac




xx
erase c:\data\ags.gph
erase c:\data\bcn.gph
erase c:\data\bcs.gph
erase c:\data\cam.gph
erase c:\data\coa.gph
erase c:\data\col.gph
erase c:\data\cps.gph
erase c:\data\cua.gph
erase c:\data\df.gph
erase c:\data\dgo.gph
erase c:\data\gua.gph
erase c:\data\gue.gph
erase c:\data\hgo.gph
erase c:\data\jal.gph
erase c:\data\mex.gph
erase c:\data\mic.gph
erase c:\data\mor.gph
erase c:\data\nay.gph
erase c:\data\nl.gph
erase c:\data\oax.gph
erase c:\data\pue.gph
erase c:\data\que.gph
erase c:\data\qui.gph
erase c:\data\san.gph
erase c:\data\sin.gph
erase c:\data\son.gph
erase c:\data\tab.gph
erase c:\data\tam.gph
erase c:\data\tla.gph
erase c:\data\ver.gph
erase c:\data\yuc.gph
erase c:\data\zac.gph



