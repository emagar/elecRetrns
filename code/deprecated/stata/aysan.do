version 8
clear
set mem 30000

use "C:\DATA\ay mu 1977-2007.dta", clear

*UNSTAR TO EXPORT DATA TO IFE SHAPEFILES
*quietly do ife_ids

*drop dy

*Llena los missing de Oaxaca antes de 1989
replace win="pri" if edon==20 & yr<1989 & ncand==.
replace rankpri=1 if edon==20 & yr<1989 & ncand==.
replace oth3a=0 if edon==20 & yr<1989 & ncand==.
replace oth3b=0 if edon==20 & yr<1989 & ncand==.
replace oth4on=0 if edon==20 & yr<1989 & ncand==.
replace coalpan="no" if edon==20 & yr<1989 & ncand==.
replace coalpri="no" if edon==20 & yr<1989 & ncand==.
replace coalprd="no" if edon==20 & yr<1989 & ncand==.
replace vcpan=0 if edon==20 & yr<1989 & ncand==.
replace vcpri=0 if edon==20 & yr<1989 & ncand==.
replace vcprd=0 if edon==20 & yr<1989 & ncand==.
replace pan=0 if edon==20 & yr<1989 & ncand==.
replace pri=100 if edon==20 & yr<1989 & ncand==.
replace prd=0 if edon==20 & yr<1989 & ncand==.
replace efec=100 if edon==20 & yr<1989 & ncand==.
replace notas="inventado" if edon==20 & yr<1989 & ncand==.
replace ncand=1 if edon==20 & yr<1989 & ncand==.

*LLENA ELECCIONES FALTANTES CON LOS DATOS DE LA EXTRAORDINARIA (MIENTRAS SE CONSIGUEN LOS DE LA GENERAL...)
replace win="pri" if inegi==29016 & yr==2001 & ncand==.
replace rankpan=5 if inegi==29016 & yr==2001 & ncand==.
replace rankpri=1 if inegi==29016 & yr==2001 & ncand==.
replace rankprd=4 if inegi==29016 & yr==2001 & ncand==.
replace oth3a=549 if inegi==29016 & yr==2001 & ncand==.
replace alab="pt-pas-pjs" if inegi==29016 & yr==2001 & ncand==.
replace oth3b=546 if inegi==29016 & yr==2001 & ncand==.
replace blab="conve" if inegi==29016 & yr==2001 & ncand==.
replace oth4on=35 if inegi==29016 & yr==2001 & ncand==.
replace coalpan="no" if inegi==29016 & yr==2001 & ncand==.
replace coalpri="no" if inegi==29016 & yr==2001 & ncand==.
replace coalprd="no" if inegi==29016 & yr==2001 & ncand==.
replace vcpan=0 if inegi==29016 & yr==2001 & ncand==.
replace vcpri=0 if inegi==29016 & yr==2001 & ncand==.
replace vcprd=0 if inegi==29016 & yr==2001 & ncand==.
replace pan=38 if inegi==29016 & yr==2001 & ncand==.
replace pri=783 if inegi==29016 & yr==2001 & ncand==.
replace prd=250 if inegi==29016 & yr==2001 & ncand==.
replace efec=2201 if inegi==29016 & yr==2001 & ncand==.
replace notas="se usa result de extraord" if inegi==29016 & yr==2001 & ncand==.
replace ncand=6 if inegi==29016 & yr==2001 & ncand==.
*falta hacer el cambio para tres munics de hidalgo anulados en 2008 y con dato en 2009

replace win="pri" if inegi==29048 & yr==1998 & ncand==.
replace rankpan=2 if inegi==29048 & yr==1998 & ncand==.
replace rankpri=1 if inegi==29048 & yr==1998 & ncand==.
replace rankprd=3 if inegi==29048 & yr==1998 & ncand==.
replace oth3a=0 if inegi==29048 & yr==1998 & ncand==.
replace oth3b=0 if inegi==29048 & yr==1998 & ncand==.
replace oth4on=0 if inegi==29048 & yr==1998 & ncand==.
replace coalpan="no" if inegi==29048 & yr==1998 & ncand==.
replace coalpri="no" if inegi==29048 & yr==1998 & ncand==.
replace coalprd="no" if inegi==29048 & yr==1998 & ncand==.
replace vcpan=0 if inegi==29048 & yr==1998 & ncand==.
replace vcpri=0 if inegi==29048 & yr==1998 & ncand==.
replace vcprd=0 if inegi==29048 & yr==1998 & ncand==.
replace pan=1682 if inegi==29048 & yr==1998 & ncand==.
replace pri=1794 if inegi==29048 & yr==1998 & ncand==.
replace prd=754 if inegi==29048 & yr==1998 & ncand==.
replace efec=4230 if inegi==29048 & yr==1998 & ncand==.
replace notas="se usa result de extraord" if inegi==29048 & yr==1998 & ncand==.
replace ncand=3 if inegi==29048 & yr==1998 & ncand==.

gen win3c="."
replace win3c="pan" if win=="pan"
replace win3c="pri" if win=="pri"
replace win3c="prd" if win=="prd"
replace win3c="prd-coal" if win=="coalfdn"
replace win3c="oth" if win=="oth"
replace win3c="oth" if win=="alter"
replace win3c="oth" if win=="conve"
replace win3c="oth" if win=="npp"
replace win3c="oth" if win=="panal"
replace win3c="oth" if win=="parm"
replace win3c="oth" if win=="pas"
replace win3c="oth" if win=="pcd"
replace win3c="oth" if win=="pcdt"
replace win3c="oth" if win=="pcm"
replace win3c="oth" if win=="pcp"
replace win3c="oth" if win=="pd"
replace win3c="oth" if win=="pdm"
replace win3c="oth" if win=="pfc"
replace win3c="oth" if win=="pfcrn"
replace win3c="oth" if win=="pjs"
replace win3c="oth" if win=="pmp"
replace win3c="oth" if win=="pps"
replace win3c="oth" if win=="prt"
replace win3c="oth" if win=="prv"
replace win3c="oth" if win=="psn"
replace win3c="oth" if win=="pt"
replace win3c="oth" if win=="pt-conve"
replace win3c="oth" if win=="pt-conve-pbs"
replace win3c="oth" if win=="pt-pcp-pc"
replace win3c="oth" if win=="pt-pvem"
replace win3c="oth" if win=="pt-pvem-conve-pas-ds"
replace win3c="oth" if win=="pvem"
replace win3c="oth" if win=="indep"
replace win3c="tie pan pri" if win=="tie pan pri"
replace win3c="pan-prd-coal" if regexm(win, "^pan-prd") & win3c=="."
replace win3c="pan-coal" if regexm(win, "^pan") & win3c=="."         /* REGULAR EXPRESSION */
replace win3c="pri-coal" if regexm(win, "^pri") & win3c=="."
replace win3c="prd-coal" if regexm(win, "^prd") & win3c=="."

gen d2avue=0
replace d2avue=1 if checkmun=="2a vuelta"
gen dnew=0
replace dnew=1 if checkmun=="new" | checkmun=="new verif"
gen dextra=0
replace dextra=1 if checkmun=="extra"

*drop lisnom
*drop fuente
*drop notas

gen cpri=.
replace cpri=0 if vcpri==0 & pri==0
replace cpri=1 if (vcpri>0 & vcpri~=.) | (pri>0 & pri~=.)

gen cpan=.
replace cpan=0 if vcpan==0 & pan==0
replace cpan=1 if (vcpan>0 & vcpan~=.) | (pan>0 & pan~=.)

gen cprd=.
replace cprd=0 if vcprd==0 & prd==0
replace cprd=1 if (vcprd>0 & vcprd~=.) | (prd>0 & prd~=.)

*MANEJO DE COALICIONES QUE REPORTAN VOTO 2 VECES
    *COA 2002
    replace prd=0 if regexm(coalprd, "prd") & edon==5 & yr==2002
    *COA 2005
    replace prd=0 if regexm(coalprd, "prd") & edon==5 & yr==2005
    *CPS 2004
    replace vcpan=vcprd if regexm(coalpan, "prd") & edon==7 & yr==2004
    replace vcprd=0 if regexm(coalpan, "prd") & edon==7 & yr==2004
    *PAN-PRD EDOMEX 2006
    replace rankprd=. if inegi==15001 & yr==2006
    replace vcprd=0 if inegi==15001 & yr==2006
    replace pan=0 if inegi==15001 & yr==2006
    replace prd=0 if inegi==15001 & yr==2006
    replace rankprd=. if inegi==15085 & yr==2006
    replace vcprd=0 if inegi==15085 & yr==2006
    replace pan=0 if inegi==15085 & yr==2006
    replace prd=0 if inegi==15085 & yr==2006
    replace rankprd=. if inegi==15087 & yr==2006
    replace vcprd=0 if inegi==15087 & yr==2006
    replace pan=0 if inegi==15087 & yr==2006
    replace prd=0 if inegi==15087 & yr==2006
    *PAN-OTH EDOMEX 2006
    replace pan=0 if pan>0 & vcpan>0 & edon==15 & yr==2006
    *PRD-OTH EDOMEX 2006
    replace prd=0 if prd>0 & vcprd>0 & edon==15 & yr==2006
    *GUA 2006
    replace pan=0 if vcpan>0 & edon==11 & yr==2006
    replace pri=0 if vcpri>0 & edon==11 & yr==2006
    *MOR 2006
    replace pri=0 if regexm(coalpri, "pvem") & edon==17 & yr==2006
    *SAN 2006
    replace pan=0 if vcpan>0 & edon==24 & yr==2006
    replace pri=0 if vcpri>0 & edon==24 & yr==2006
    *SIN 2001
    gen tmp=0
    replace tmp=pan+prd+vcpan+vcprd if regexm(coalpan, "prd") & edon==25 & yr==2001
    replace prd=0 if tmp>0
    replace tmp=pan+vcpan if regexm(coalpan, "pan-") & edon==25 & yr==2001 & tmp==0
    replace vcpan=tmp if tmp>0
    replace pan=0 if tmp>0
    drop tmp
    replace vcprd=prd+vcprd if regexm(coalprd, "prd-") & edon==25 & yr==2001
    replace prd=0 if regexm(coalprd, "prd-") & edon==25 & yr==2001
    *TAM 1986
    replace vcpan=pan+vcpan if regexm(coalpan, "prd") & edon==28 & yr==1986
    replace pan=0 if regexm(coalpan, "prd") & edon==28 & yr==1986
    *YUC 2007
    replace vcpri=pri+vcpri if regexm(coalpri, "pvem") & edon==31 & yr==2007
    replace pri=0 if regexm(coalpri, "pvem") & edon==31 & yr==2007

*CONSOLIDA VOTOS
replace pan=pan+vcpan
replace pri=pri+vcpri
replace prd=prd+vcprd
drop vcpan vcpri vcprd

*CASOS EN QUE COLUMNA OTHER-B REPORTA MAS VOTOS QUE COLUMNA A
gen tmp=oth3b>oth3a & oth3b<.
gen tmpa=oth3a
gen tmpaa=alab
replace oth3a=oth3b if tmp==1
replace alab=blab if tmp==1
replace oth3b=tmpa if tmp==1
replace blab=tmpaa if tmp==1
drop tmp tmpa tmpaa

*CORRIGE DENOMINADOR
replace efec=pan+pri+prd+oth3a+oth3b+oth4 if edon==4 & yr==2006
replace efec=pan+pri+prd+oth3a+oth3b+oth4 if edon==5 & yr==2002
replace efec=pan+pri+prd+oth3a+oth3b+oth4 if edon==7 & yr==2001
replace efec=pan+pri+prd+oth3a+oth3b+oth4 if edon==10 & (yr==1989 | yr==1992)
replace efec=pan+pri+prd+oth3a+oth3b+oth4 if edon==11 & (yr==1994 | yr==1997 | yr==2006)
replace efec=pan+pri+prd+oth3a+oth3b+oth4 if edon==14 & yr==1992
replace efec=pan+pri+prd+oth3a+oth3b+oth4 if edon==19 & (yr==1991 | yr==1994)
replace efec=pan+pri+prd+oth3a+oth3b+oth4 if edon==20 & yr==1992
replace efec=pan+pri+prd+oth3a+oth3b+oth4 if edon==24 & (yr==2000 | yr==2006 | yr==2009)
replace efec=pan+pri+prd+oth3a+oth3b+oth4 if edon==25 & (yr==1992 | yr==2001)
replace efec=pan+pri+prd+oth3a+oth3b+oth4 if edon==29 & (yr==1985 | yr==1994 | yr==1996 | yr==1998)
replace efec=pan+pri+prd+oth3a+oth3b+oth4 if edon==30 & yr==2000
replace efec=pan+pri+prd+oth3a+oth3b+oth4 if edon==32 & yr==2004

gen ppan=pan*100/efec
gen ppri=pri*100/efec
gen pprd=prd*100/efec
gen p3a=oth3a*100/efec
gen p3b=oth3b*100/efec
gen p4=oth4on*100/efec
format ppan-p4 %5.0g

*RECALCULA LOS RANKS
gen rpan=0
gen rpri=0
gen rprd=0
gen r3a=0
gen r3b=0
gen tie1=0
gen tie2=0
gen tie3=0
gen tie4=0
replace rpan=1 if ppan==max(ppan,ppri,pprd,p3a,p3b) & ppan<.
replace rpri=1 if ppri==max(ppan,ppri,pprd,p3a,p3b) & ppri<.
replace rprd=1 if pprd==max(ppan,ppri,pprd,p3a,p3b) & pprd<.
replace r3a=1 if p3a==max(ppan,ppri,pprd,p3a,p3b) & p3a<.
gen tmp=rpan+rpri+rprd+r3a+r3b
replace tie1=1 if tmp>1 & tmp<.
drop tmp

gen ppan2=(1-rpan)*ppan
gen ppri2=(1-rpri)*ppri
gen pprd2=(1-rprd)*pprd
gen p3a2= (1-r3a) *p3a
gen p3b2= (1-r3b) *p3b
gen rpan2= ppan2==max(ppan2,ppri2,pprd2,p3a2,p3b2) & ppan2>0 & ppan<.
gen rpri2= ppri2==max(ppan2,ppri2,pprd2,p3a2,p3b2) & ppri2>0 & ppri<.
gen rprd2= pprd2==max(ppan2,ppri2,pprd2,p3a2,p3b2) & pprd2>0 & pprd<.
gen r3a2=  p3a2== max(ppan2,ppri2,pprd2,p3a2,p3b2) & p3a2>0 & p3a<.
gen r3b2=  p3b2== max(ppan2,ppri2,pprd2,p3a2,p3b2) & p3b2>0 & p3b<.
replace rpan=2+tie1 if rpan2==1 & rpan==0
replace rpri=2+tie1 if rpri2==1 & rpri==0
replace rprd=2+tie1 if rprd2==1 & rprd==0
replace r3a=2+tie1  if r3a2==1  & r3a==0
replace r3b=2+tie1  if r3b2==1  & r3b==0
gen tmp=rpan2+rpri2+rprd2+r3a2+r3b2
replace tie2=1 if tmp>1 & tmp<.
drop tmp

gen ppan3=(1-rpan2)*ppan2
gen ppri3=(1-rpri2)*ppri2
gen pprd3=(1-rprd2)*pprd2
gen p3a3= (1-r3a2) *p3a2
gen p3b3= (1-r3b2) *p3b2
gen rpan3= ppan3==max(ppan3,ppri3,pprd3,p3a3,p3b3) & ppan3>0 & ppan<.
gen rpri3= ppri3==max(ppan3,ppri3,pprd3,p3a3,p3b3) & ppri3>0 & ppri<.
gen rprd3= pprd3==max(ppan3,ppri3,pprd3,p3a3,p3b3) & pprd3>0 & pprd<.
gen r3a3=  p3a3== max(ppan3,ppri3,pprd3,p3a3,p3b3) & p3a3>0 & p3a<.
gen r3b3=  p3b3== max(ppan3,ppri3,pprd3,p3a3,p3b3) & p3b3>0 & p3b<.
replace rpan=3+tie1+tie2 if rpan3==1 & rpan==0
replace rpri=3+tie1+tie2 if rpri3==1 & rpri==0
replace rprd=3+tie1+tie2 if rprd3==1 & rprd==0
replace r3a=3+tie1+tie2  if r3a3==1  & r3a==0
replace r3b=3+tie1+tie2  if r3b3==1  & r3b==0
gen tmp=rpan3+rpri3+rprd3+r3a3+r3b3
replace tie3=1 if tmp>1 & tmp<.
drop tmp

gen ppan4=(1-rpan3)*ppan3
gen ppri4=(1-rpri3)*ppri3
gen pprd4=(1-rprd3)*pprd3
gen p3a4= (1-r3a3) *p3a3
gen p3b4= (1-r3b3) *p3b3
gen rpan4=ppan4==max(ppan4,ppri4,pprd4,p3a4,p3b4) & ppan4>0 & ppan<.
gen rpri4=ppri4==max(ppan4,ppri4,pprd4,p3a4,p3b4) & ppri4>0 & ppri<.
gen rprd4=pprd4==max(ppan4,ppri4,pprd4,p3a4,p3b4) & pprd4>0 & pprd<.
gen r3a4= p3a4== max(ppan4,ppri4,pprd4,p3a4,p3b4) & p3a4>0 & p3a<.
gen r3b4= p3b4== max(ppan4,ppri4,pprd4,p3a4,p3b4) & p3b4>0 & p3b<.
replace rpan=4+tie1+tie2+tie3 if rpan4==1 & rpan==0
replace rpri=4+tie1+tie2+tie3 if rpri4==1 & rpri==0
replace rprd=4+tie1+tie2+tie3 if rprd4==1 & rprd==0
replace r3a=4+tie1+tie2+tie3  if r3a4==1  & r3a==0
replace r3b=4+tie1+tie2+tie3  if r3b4==1  & r3b==0
gen tmp=rpan4+rpri4+rprd4+r3a4+r3b4
replace tie4=1 if tmp>1 & tmp<.
drop tmp

replace rpan=5 if rpan==0 & ppan<.
replace rpri=5 if rpri==0 & ppri<.
replace rprd=5 if rprd==0 & pprd<.
replace r3a=5  if r3a==0  & p3a<.
replace r3b=5  if r3b==0  & p3b<.

replace rpan=. if ppan==. | ppan==0
replace rpri=. if ppri==. | ppri==0
replace rprd=. if pprd==. | pprd==0
replace r3a= . if p3a== . | p3a==0
replace r3b= . if p3b== . | p3b==0

drop ppan2-r3b4

*ARREGLA COALICIONES
gen dpanprd=regexm(coalprd, "^pan")
replace rankprd=. if dpanprd==1 & rankpan==rankprd

*LIMPIEZA
replace rankpan=rpan if inegi==4001 & yr==1985
replace rankpan=rpan if inegi==5020 & yr==2005
replace rankpan=rpan if inegi==20007 & yr==2004
replace rankpan=rpan if inegi==20067 & yr==2004
replace rankpan=rpan if inegi==28009 & yr==1986
replace rankprd=rprd if inegi==28009 & yr==1986
replace rankpan=rpan if inegi==14077 & yr==2003
replace rankpan=rpan if edon==30 & yr==2004
replace rankpri=rpri if edon==30 & yr==2004
replace rankprd=rprd if edon==30 & yr==2004
replace rankpan=rpan if edon==28 & yr==1986
replace rankpri=rpri if edon==28 & yr==1986
replace rankprd=rprd if edon==28 & yr==1986
replace rankpan=rpan if edon==21 & yr==1992
replace rankpri=rpri if edon==21 & yr==1992
replace rankprd=rprd if edon==21 & yr==1992
replace rankpri=rpri if inegi==6001 & yr==2000
replace rankpan=rpan if edon==7 & yr==2004
replace rankpri=rpri if edon==7 & yr==2004
replace rankprd=rprd if edon==7 & yr==2004
replace rankpri=rpri if inegi==11013 & yr==2006
replace rankprd=rprd if inegi==20530 & yr==1983
replace rankprd=rprd if inegi==20184 & yr==1992
replace rankprd=rprd if inegi==20470 & yr==1992
replace rankprd=rprd if inegi==20472 & yr==1992
replace rankprd=rprd if inegi==20505 & yr==1992
*ESTOS SE TIENEN QUE CONSERVAR
replace rpan=rankpan if inegi==29019 & yr==2004  /* CASOS DE 3 PRIMEROS NO MAJORS */
replace rpri=rankpri if inegi==29019 & yr==2004  /* CASOS DE 3 PRIMEROS NO MAJORS */
replace rprd=rankprd if inegi==29019 & yr==2004  /* CASOS DE 3 PRIMEROS NO MAJORS */
replace rpri=rankpri if inegi==26044 & yr==1988  /* CASOS DE 3 PRIMEROS NO MAJORS */
replace rpan=rankpan if inegi==20565 & yr==2004  /* CASOS DE 3 PRIMEROS NO MAJORS */
replace rpri=rankpri if inegi==20565 & yr==2004  /* CASOS DE 3 PRIMEROS NO MAJORS */
replace rprd=rankprd if inegi==20565 & yr==2004  /* CASOS DE 3 PRIMEROS NO MAJORS */
*tab rankprd rprd
*list inegi edo yr coalpan coalpri coalprd ppan-p3b rankpan-rankprd rpan-r3b win if rprd==3 & rankprd==4

gen first="."
gen second="."
gen third="."

replace first="pan" if rpan==1 & coalpan=="no"
replace first="pan-coal" if rpan==1 & regexm(coalpan, "pan")
replace second="pan" if rpan==2 & coalpan=="no"
replace second="pan-coal" if rpan==2 & regexm(coalpan, "pan")
replace third="pan" if rpan==3 & coalpan=="no"
replace third="pan-coal" if rpan==3 & regexm(coalpan, "pan")

replace first="pri" if rpri==1 & coalpri=="no"
replace first="pri-coal" if rpri==1 & regexm(coalpri, "pri")
replace second="pri" if rpri==2 & coalpri=="no"
replace second="pri-coal" if rpri==2 & regexm(coalpri, "pri")
replace third="pri" if rpri==3 & coalpri=="no"
replace third="pri-coal" if rpri==3 & regexm(coalpri, "pri")

replace first="prd" if rprd==1 & coalprd=="no"
replace first="prd-coal" if rprd==1 & (regexm(coalprd, "prd") | regexm(coalprd, "fdn"))
replace second="prd" if rprd==2 & coalprd=="no"
replace second="prd-coal" if rprd==2 & regexm(coalprd, "prd")
replace third="prd" if rprd==3 & coalprd=="no"
replace third="prd-coal" if rprd==3 & regexm(coalprd, "prd")

replace first="oth" if r3a==1
replace second="oth" if r3a==2
replace third="oth" if r3a==3

replace second="oth" if r3b==2
replace third="oth" if r3b==3

*replace first="XXX" if tie1==1
*replace second="XXX" if tie2==1
*replace third="XXX" if tie3==1
replace first="tie pan-pri" if inegi==14107 & yr==2006
replace first="tie pri-oth" if inegi==20372 & yr==1980
replace first="tie pan-pri" if inegi==20402 & yr==2004
replace first="tie pri-oth" if inegi==21147 & yr==2004
replace first="tie pri-oth" if inegi==29004 & yr==1988
replace first="tie pan-pri" if inegi==29009 & yr==1991
replace second="tied" if inegi==14107 & yr==2006
replace second="tied" if inegi==20372 & yr==1980
replace second="tied" if inegi==20402 & yr==2004
replace second="tied" if inegi==21147 & yr==2004
replace second="tied" if inegi==29004 & yr==1988
replace second="tied" if inegi==29009 & yr==1991
replace second="tie pan-prd" if inegi==5029 & yr==1990
replace second="tie pan-oth" if inegi==8008 & yr==1989
replace second="tie pan-oth" if inegi==8014 & yr==1989
replace second="tie pan-oth" if inegi==12034 & yr==1977
replace second="tie pan-prd" if inegi==12049 & yr==1977
replace second="tie pan-prd" if inegi==12024 & yr==1983
replace second="tie pri-prd" if inegi==12025 & yr==1986
replace second="tie pan-oth" if inegi==12027 & yr==1986
replace second="tie pan-oth" if inegi==12064 & yr==1986
replace second="tie pan-oth" if inegi==13047 & yr==1990
replace second="tie oth-oth" if inegi==14097 & yr==1976
replace second="tie prd-oth" if inegi==14090 & yr==1982
replace second="tie oth-oth" if inegi==14060 & yr==1985
replace second="tie pan-oth" if inegi==14095 & yr==1988
replace second="tie pan-oth" if inegi==16052 & yr==1977
replace second="tie pan-prd" if inegi==16103 & yr==1980
replace second="tie pan-prd" if inegi==16106 & yr==1980
replace second="tie pan-prd" if inegi==20530 & yr==1983
replace second="tie pan-oth" if inegi==20173 & yr==1986
replace second="tie pan-oth" if inegi==20301 & yr==1986
replace second="tie pan-prd" if inegi==20109 & yr==1989
replace second="tie pan-prd" if inegi==20327 & yr==1989
replace second="tie pan-prd" if inegi==21169 & yr==1983
replace second="tie pan-oth" if inegi==21192 & yr==1983
replace second="tie pan-prd" if inegi==21024 & yr==1995
replace second="tie pan-prd" if inegi==21159 & yr==1995
replace second="tie oth-oth" if inegi==21165 & yr==1995
replace second="tie oth-oth" if inegi==23004 & yr==1987
replace second="tie pan-oth" if inegi==24018 & yr==1988
replace second="tie oth-oth" if inegi==26021 & yr==1979
replace second="tie pri-prd" if inegi==26045 & yr==2006
replace second="tie prd-oth" if inegi==27009 & yr==1991
replace second="tie pan-oth" if inegi==28019 & yr==1986
replace second="tie pan-oth" if inegi==28031 & yr==1989
replace second="tie pan-prd" if inegi==29030 & yr==1982
replace second="tie oth-oth" if inegi==29018 & yr==1985
replace second="tie pan-oth" if inegi==29023 & yr==1985
replace second="tie pan-oth" if inegi==30026 & yr==1976
replace second="tie pan-oth" if inegi==30067 & yr==1976
replace second="tie oth-oth" if inegi==30107 & yr==1976
replace second="tie oth-oth" if inegi==30100 & yr==1985
replace second="tie pan-prd" if inegi==31084 & yr==1984
replace second="tie prd-oth" if inegi==32012 & yr==1985
replace third="tied" if inegi==5029 & yr==1990
replace third="tied" if inegi==8008 & yr==1989
replace third="tied" if inegi==8014 & yr==1989
replace third="tied" if inegi==12034 & yr==1977
replace third="tied" if inegi==12049 & yr==1977
replace third="tied" if inegi==12024 & yr==1983
replace third="tied" if inegi==12025 & yr==1986
replace third="tied" if inegi==12027 & yr==1986
replace third="tied" if inegi==12064 & yr==1986
replace third="tied" if inegi==13047 & yr==1990
replace third="tied" if inegi==14097 & yr==1976
replace third="tied" if inegi==14090 & yr==1982
replace third="tied" if inegi==14060 & yr==1985
replace third="tied" if inegi==14095 & yr==1988
replace third="tied" if inegi==16052 & yr==1977
replace third="tied" if inegi==16103 & yr==1980
replace third="tied" if inegi==16106 & yr==1980
replace third="tied" if inegi==20530 & yr==1983
replace third="tied" if inegi==20173 & yr==1986
replace third="tied" if inegi==20301 & yr==1986
replace third="tied" if inegi==20109 & yr==1989
replace third="tied" if inegi==20327 & yr==1989
replace third="tied" if inegi==21169 & yr==1983
replace third="tied" if inegi==21192 & yr==1983
replace third="tied" if inegi==21024 & yr==1995
replace third="tied" if inegi==21159 & yr==1995
replace third="tied" if inegi==21165 & yr==1995
replace third="tied" if inegi==23004 & yr==1987
replace third="tied" if inegi==24018 & yr==1988
replace third="tied" if inegi==26021 & yr==1979
replace third="tied" if inegi==26045 & yr==2006
replace third="tied" if inegi==27009 & yr==1991
replace third="tied" if inegi==28019 & yr==1986
replace third="tied" if inegi==28031 & yr==1989
replace third="tied" if inegi==29030 & yr==1982
replace third="tied" if inegi==29018 & yr==1985
replace third="tied" if inegi==29023 & yr==1985
replace third="tied" if inegi==30026 & yr==1976
replace third="tied" if inegi==30067 & yr==1976
replace third="tied" if inegi==30107 & yr==1976
replace third="tied" if inegi==30100 & yr==1985
replace third="tied" if inegi==31084 & yr==1984
replace third="tied" if inegi==32012 & yr==1985
replace third="tie pan-prd" if inegi==12065 & yr==1986
replace third="tie pan-prd" if inegi==13074 & yr==1990
replace third="tie pan-prd" if inegi==13033 & yr==1993
replace third="tie oth-oth" if inegi==14060 & yr==1992
replace third="tie pan-prd" if inegi==15034 & yr==1987
replace third="tie pan-prd" if inegi==15041 & yr==1987
replace third="tie prd-oth" if inegi==20523 & yr==1986
replace third="tie pan-prd" if inegi==21115 & yr==1992
replace third="tie pan-prd" if inegi==21106 & yr==1995
replace third="tie prd-oth" if inegi==27009 & yr==1988
replace third="tie pan-prd" if inegi==28008 & yr==1989
replace third="tie pan-prd" if inegi==29019 & yr==1991
replace third="tie pan-prd" if inegi==29022 & yr==1991
replace third="tie pan-prd" if inegi==29043 & yr==1991
replace third="tie pan-oth" if inegi==32041 & yr==1992

move rpan rankpan
move rpri rankpan
move rprd rankpan
move r3a rankpan
move r3b rankpan
drop rankpan-rankprd

move win3c win
move first win
move second win
move third win
move tie1 win
move tie2 win
move tie3 win
move tie4 win

*ELECCIONES EXTRAORDINARIAS O 2DAS RONDAS PUEDEN CAMBIAR EL DESENLACE
*FIRST RETIENE INFORMACION DE 1ER LUGAR EN LA ELECCION GENERAL, WIN3C LA DEL GANADOR FINAL
replace win3c="pri" if inegi==20402 & yr==2004     /* CASO DE EMPATE EN 1er LUGAR */
replace win3c="pan" if inegi==14107 & yr==2006     /* CASO DE EMPATE EN 1er LUGAR */
replace win3c="?" if inegi==20372 & yr==1980       /* CASO DE EMPATE EN 1er LUGAR */
replace win3c="pri" if inegi==29004 & yr==1988       /* EMPATE EN 1er LUGAR QUE DE REMMES REPORTA COMO PRI */
replace win3c="pan" if inegi==29009 & yr==1991       /* EMPATE RESUELTO EN FAVOR PAN (Dra. Angélica Cazarín Martínez de El Colegio de Tlaxcala, el artículo está adjunto FRANCISCO GARFIAS) */
*** replace win3c="pri" if inegi==24002 & yr==1997 & d2a==0
*** replace win3c="prd" if inegi==24005 & yr==1997 & d2a==0
*** replace win3c="pri" if inegi==24006 & yr==1997 & d2a==0
*** replace win3c="oth" if inegi==24007 & yr==1997 & d2a==0
*** replace win3c="pan" if inegi==24010 & yr==1997 & d2a==0
*** replace win3c="oth" if inegi==24013 & yr==1997 & d2a==0
*** replace win3c="pri" if inegi==24015 & yr==1997 & d2a==0
*** replace win3c="pri" if inegi==24020 & yr==1997 & d2a==0
*** replace win3c="pan" if inegi==24023 & yr==1997 & d2a==0
*** replace win3c="pan" if inegi==24024 & yr==1997 & d2a==0
*** replace win3c="pri" if inegi==24029 & yr==1997 & d2a==0
*** replace win3c="pri" if inegi==24030 & yr==1997 & d2a==0
*** replace win3c="oth" if inegi==24033 & yr==1997 & d2a==0
*** replace win3c="prd" if inegi==24036 & yr==1997 & d2a==0
*** replace win3c="pri" if inegi==24039 & yr==1997 & d2a==0
*** replace win3c="pri" if inegi==24040 & yr==1997 & d2a==0
*** replace win3c="pri" if inegi==24042 & yr==1997 & d2a==0
*** replace win3c="pri" if inegi==24045 & yr==1997 & d2a==0
*** replace win3c="pan" if inegi==24047 & yr==1997 & d2a==0
*** replace win3c="oth" if inegi==24049 & yr==1997 & d2a==0
*** replace win3c="pri" if inegi==24053 & yr==1997 & d2a==0
*** replace win3c="pri" if inegi==24055 & yr==1997 & d2a==0
*** replace win3c="pan" if inegi==24056 & yr==1997 & d2a==0
*** replace win3c="pan" if inegi==24003 & yr==2000 & d2a==0
*** replace win3c="prd" if inegi==24009 & yr==2000 & d2a==0
*** replace win3c="pri" if inegi==24013 & yr==2000 & d2a==0
*** replace win3c="pan" if inegi==24016 & yr==2000 & d2a==0
*** replace win3c="pan" if inegi==24020 & yr==2000 & d2a==0
*** replace win3c="pri" if inegi==24023 & yr==2000 & d2a==0
*** replace win3c="pan" if inegi==24024 & yr==2000 & d2a==0
*** replace win3c="pan" if inegi==24026 & yr==2000 & d2a==0
*** replace win3c="pan" if inegi==24028 & yr==2000 & d2a==0
*** replace win3c="pan" if inegi==24032 & yr==2000 & d2a==0
*** replace win3c="pan" if inegi==24033 & yr==2000 & d2a==0
*** replace win3c="prd-coal" if inegi==24036 & yr==2000 & d2a==0
*** replace win3c="prd-coal" if inegi==24041 & yr==2000 & d2a==0
*** replace win3c="pri" if inegi==24043 & yr==2000 & d2a==0
*** replace win3c="pan" if inegi==24045 & yr==2000 & d2a==0
*** replace win3c="pan" if inegi==24046 & yr==2000 & d2a==0
*** replace win3c="oth" if inegi==24049 & yr==2000 & d2a==0
*** replace win3c="pan" if inegi==24055 & yr==2000 & d2a==0
*** replace win3c="pri-coal" if inegi==24013 & yr==2003 & d2a==0
*** replace win3c="pan" if inegi==24021 & yr==2003 & d2a==0
*** replace win3c="pri-coal" if inegi==24024 & yr==2003 & d2a==0
*** replace win3c="prd" if inegi==24035 & yr==2003 & d2a==0
*** replace win3c="pan" if inegi==24054 & yr==2003 & d2a==0

gen win3=win3c
replace win3="pan" if win3c=="pan-coal"
replace win3="pri" if win3c=="pri-coal"
replace win3="prd" if win3c=="prd-coal"

*SI SE VA A TRABAJAR CON SF RATIOS HAY CASOS EN QUE LOS 3 GRANDES QUEDAN DEBAJO 3ER LUGAR
*gen oth3c=0
*gen clab="."
*replace oth3c=884 if inegi==29019 & yr==2004
*replace clab="pjs" if inegi==29019 & yr==2004
*replace oth3c=200 if inegi==26044 & yr==1988
*replace clab="prt" if inegi==26044 & yr==1988
*replace oth3c=1230 if inegi==20565 & yr==2004
*replace clab="pup" if inegi==20565 & yr==2004

*LO USE PARA LIMPIEZA: PERMITE COMPARAR SUMA RANKS VIEJOS Y NUEVOS
*gen tmp1=rankpan>0&rankpan<.
*gen tmp2=rankpri>0&rankpri<.
*gen tmp3=rankprd>0&rankprd<.
*replace tmp1=rankpan if tmp1==1
*replace tmp2=rankpri if tmp2==1
*replace tmp3=rankprd if tmp3==1
*gen old=tmp1+tmp2+tmp3
*drop tmp1-tmp3
*gen tmp1=rpan>0&rpan<.
*gen tmp2=rpri>0&rpri<.
*gen tmp3=rprd>0&rprd<.
*replace tmp1=rpan if tmp1==1
*replace tmp2=rpri if tmp2==1
*replace tmp3=rprd if tmp3==1
*gen new=tmp1+tmp2+tmp3
*drop tmp1-tmp3

*LO USE PARA LIMPIEZA
*gen superavit=efec-pan-pri-prd- oth3a- oth3b- oth4on
*gen percent=superavit*100/efec
*list edo inegi mun yr pan pri prd oth3a oth3b oth4 efec superavit percent if abs(percent)>0 & superavit<.
*list edo inegi mun yr win rankpri pan pri prd oth3a oth3b oth4 if rankpri==. & pri>0 & pri<.
*list edo inegi mun yr win rankpan pan pri prd oth3a oth3b oth4 if rankpan==. & pan>0 & pan<.
*list edo inegi mun yr pan pri prd oth3a oth3b oth4 efec if win3c=="." & efec>0 & efec<.

*posibles siguientes pasos para concluir limpieza:
*-ver que ncand=X tenga votos en sólo X columnas
*-ver que ncand=X con votos en sólo X columnas tenga también oth4==0
*-buscar casos en que oth4 el igual a otra columna: posible repetición
*-leer columna notas en busca de info que no haya incorporado

drop if dextra==1
*** drop if d2avue==1
*** drop dextra
*** drop d2avue

*LOS TRIENIOS EMPIEZAN EL 1 DE ENE de t=año de la federal Y ACABAN 31 DIC t+2
gen trien=0
replace trien=1  if yr==1976 | yr==1977 | yr==1978
replace trien=2  if yr==1979 | yr==1980 | yr==1981
replace trien=3  if yr==1982 | yr==1983 | yr==1984
replace trien=4  if yr==1985 | yr==1986 | yr==1987
replace trien=5  if yr==1988 | yr==1989 | yr==1990
replace trien=6  if yr==1991 | yr==1992 | yr==1993
replace trien=7  if yr==1994 | yr==1995 | yr==1996
replace trien=8  if yr==1997 | yr==1998 | yr==1999
replace trien=9  if yr==2000 | yr==2001 | yr==2002
replace trien=10 if yr==2003 | yr==2004 | yr==2005
replace trien=11 if yr==2006 | yr==2007 | yr==2008
replace trien=12 if yr==2009 | yr==2010 | yr==2011
*label define tri 1 "1 1976-78" 2 "2 1979-81" 3 "3 1982-84" 4 "4 1985-87" 5 "5 1988-90" /*
* */ 6 "6 1991-93" 7 "7 1994-96" 8 "8 1997-99" 9 "9 2000-02" 10 "10 2003-05" 11 "11 2006-08"
*label values trien tri
gen     trienio="1976-78" if yr==1976 | yr==1977 | yr==1978
replace trienio="1979-81" if yr==1979 | yr==1980 | yr==1981
replace trienio="1982-84" if yr==1982 | yr==1983 | yr==1984
replace trienio="1985-87" if yr==1985 | yr==1986 | yr==1987
replace trienio="1988-90" if yr==1988 | yr==1989 | yr==1990
replace trienio="1991-93" if yr==1991 | yr==1992 | yr==1993
replace trienio="1994-96" if yr==1994 | yr==1995 | yr==1996
replace trienio="1997-99" if yr==1997 | yr==1998 | yr==1999
replace trienio="2000-02" if yr==2000 | yr==2001 | yr==2002
replace trienio="2003-05" if yr==2003 | yr==2004 | yr==2005
replace trienio="2006-08" if yr==2006 | yr==2007 | yr==2008
replace trienio="2009-11" if yr==2009 | yr==2010 | yr==2011

*vote for winner
gen p1st=ppan if first=="pan" | first=="pan-coal"
replace p1st=ppri if first=="pri" | first=="pri-coal" | first=="tie pan-pri" | first=="tie pri-oth"
replace p1st=pprd if first=="prd" | first=="prd-coal"
replace p1st=p3a if first=="oth"

*vote for second
gen p2nd=0 if p1st~=.
replace p2nd=ppan if second=="pan" | second=="pan-coal"
replace p2nd=ppri if second=="pri" | second=="pri-coal" | second=="tie pan-pri" | second=="tie pri-oth"
replace p2nd=pprd if second=="prd" | second=="prd-coal"
replace p2nd=p3a if second=="oth"
replace p2nd=p3b if second=="oth" & first=="oth"

*Margen de c/ partido con el partido al que mejor le fue
gen mgpri=ppri-p2nd if rpri==1 & cpri==1
replace mgpri=ppri-p1st if rpri>1 & cpri==1
gen mgpan=ppan-p2nd if rpan==1 & cpan==1
replace mgpan=ppan-p1st if rpan>1 & cpan==1
gen mgprd=pprd-p2nd if rprd==1 & cprd==1
replace mgprd=pprd-p1st if rprd>1 & cprd==1
gen mgoth=p1st-p2nd if win3c=="oth"
replace mgoth=p3a-p1st if win3c~="oth" & ncand~=. & ncand-cpan-cpri-cprd>0

gen m010pan=(p1st-ppan<=10 & first~="pan" & first~="pan-coal")
gen m010pri=(p1st-ppri<=10 & first~="pri" & first~="pri-coal")
gen m010prd=(p1st-pprd<=10 & first~="prd" & first~="prd-coal")
gen m1020pan=(p1st-ppan<=20 & first~="pan" & first~="pan-coal" & m010pan==0)
gen m1020pri=(p1st-ppri<=20 & first~="pri" & first~="pri-coal" & m010pri==0)
gen m1020prd=(p1st-pprd<=20 & first~="prd" & first~="prd-coal" & m010prd==0)
gen m2030pan=(p1st-ppan<=30 & first~="pan" & first~="pan-coal" & m010pan==0 & m1020pan==0)
gen m2030pri=(p1st-ppri<=30 & first~="pri" & first~="pri-coal" & m010pri==0 & m1020pri==0)
gen m2030prd=(p1st-pprd<=30 & first~="prd" & first~="prd-coal" & m010prd==0 & m1020prd==0)

sort trienio
preserve
gen winpan=(first=="pan" | first=="pan-coal")
gen winpri=(first=="pri" | first=="pri-coal")
gen winprd=(first=="prd" | first=="prd-coal")
by trienio: egen races=count(efec)
by trienio: egen c_pan=sum(cpan)
by trienio: egen c_pri=sum(cpri)
by trienio: egen c_prd=sum(cprd)
by trienio: egen w_pan=sum(winpan)
by trienio: egen w_pri=sum(winpri)
by trienio: egen w_prd=sum(winprd)
by trienio: egen m010_pan=sum(m010pan)
by trienio: egen m010_pri=sum(m010pri)
by trienio: egen m010_prd=sum(m010prd)
by trienio: egen m1020_pan=sum(m1020pan)
by trienio: egen m1020_pri=sum(m1020pri)
by trienio: egen m1020_prd=sum(m1020prd)
by trienio: egen m2030_pan=sum(m2030pan)
by trienio: egen m2030_pri=sum(m2030pri)
by trienio: egen m2030_prd=sum(m2030prd)
drop if trienio==trienio[_n-1]
replace w_pan=w_pan*100/c_pan
replace w_pri=w_pri*100/c_pri
replace w_prd=w_prd*100/c_prd
replace m010_pan=m010_pan*100/c_pan
replace m010_pri=m010_pri*100/c_pri
replace m010_prd=m010_prd*100/c_prd
replace m1020_pan=m1020_pan*100/c_pan
replace m1020_pri=m1020_pri*100/c_pri
replace m1020_prd=m1020_prd*100/c_prd
replace m2030_pan=m2030_pan*100/c_pan
replace m2030_pri=m2030_pri*100/c_pri
replace m2030_prd=m2030_prd*100/c_prd
replace c_pan=c_pan*100/races
replace c_pri=c_pri*100/races
replace c_prd=c_prd*100/races
format c_pan-c_prd %9.0f
format w_pan-m2030_prd %9.0f
list trienio c_pan w_pan m010_pan m1020_pan m2030_pan
list trienio c_pri w_pri m010_pri m1020_pri m2030_pri
list trienio c_prd w_prd m010_prd m1020_prd m2030_prd
restore

*Pone elección extraordinaria de Tlax 1996 en nuevos munic como si fuese parte de la de 1994
drop if inegi>=29045 & inegi<=29060 & yr==1994
drop if inegi>=29001 & inegi<=29044 & yr==1996

*quita años previos a 1976
*drop if trienio==0
drop if trienio==""

gen dballot=(d2a==0 & pri[_n+1]~=. & yr>=1997 & yr<=2003) /*hubo ballotage*/

*stop

*Clona los nuevos municipios para que al usar mapa 2005 no aparezcan en blanco
*(duplica los datos del municipio de origen)
* UNSTAR SOLO SI SE VAN A HACER MAPAS... TARDA 6 MINUTOS
*quietly do CloneNewMunics

*Exporta datos para incorporarlos a un mapa en mapiux.do
preserve
*para mantener matriz cuadrada a pesar de recalendarizacion Edomex y Coahuila
local i = _N+1                       /*         */
local j = _N+125                     /**        */
set obs `j'                          /***       */
replace edon=15 in `i'/`j'           /**** mex  */
replace yr=1999 in `i'/`j'           /***       */
replace trien=8 in `i'/`j'           /**        */
replace ife=_n-`i'+15001 in `i'/`j'  /*         */
local i = _N+1                       /*         */
local j = _N+38                      /**        */
set obs `j'                          /***       */
replace edon=5 in `i'/`j'            /**** coa  */
replace yr=2008 in `i'/`j'           /***       */
replace trien=11 in `i'/`j'          /**        */
replace ife=_n-`i'+5001 in `i'/`j'   /*         */
sort edon yr ife
drop if d2avue==1
outsheet edon inegi ife mun yr trienio trien d2avue dballot mgpri mgpan mgprd mgoth using c:\data\1new\mapas\municipio\dataFrStataSAN1, comma replace
restore

*SEGUNDA VUELTA
preserve
*para mantener matriz cuadrada a pesar de recalendarizacion Edomex y Coahuila
local i = _N+1                       /*         */
local j = _N+125                     /**        */
set obs `j'                          /***       */
replace edon=15 in `i'/`j'           /**** mex  */
replace yr=1999 in `i'/`j'           /***       */
replace trien=8 in `i'/`j'           /**        */
replace ife=_n-`i'+15001 in `i'/`j'  /*         */
local i = _N+1                       /*         */
local j = _N+38                      /**        */
set obs `j'                          /***       */
replace edon=5 in `i'/`j'            /**** coa  */
replace yr=2008 in `i'/`j'           /***       */
replace trien=11 in `i'/`j'          /**        */
replace ife=_n-`i'+5001 in `i'/`j'   /*         */
sort edon yr ife
drop if edon==24 & d2avue==0 & yr>=1997 & yr<=2003
outsheet edon inegi ife mun yr trienio trien d2avue dballot mgpri mgpan mgprd mgoth using c:\data\1new\mapas\municipio\dataFrStataSAN2, comma replace
restore

xx

*quita missing
drop if efec==.

sort edon munn yr
gen ncandlag1=ncand[_n-1] if munn==munn[_n-1]
gen cpanlag1=cpan[_n-1] if munn==munn[_n-1]
gen cpanlag2=cpan[_n-2] if munn==munn[_n-2]
gen cpanlag3=cpan[_n-3] if munn==munn[_n-3]
gen cpanlag4=cpan[_n-4] if munn==munn[_n-4]
gen cpanlag5=cpan[_n-5] if munn==munn[_n-5]
gen cpanlag6=cpan[_n-6] if munn==munn[_n-6]
gen cpanlag7=cpan[_n-7] if munn==munn[_n-7]
gen cpanlag8=cpan[_n-8] if munn==munn[_n-8]
gen cpanlag9=cpan[_n-9] if munn==munn[_n-9]
gen cprilag1=cpri[_n-1] if munn==munn[_n-1]
gen cprilag2=cpri[_n-2] if munn==munn[_n-2]
gen cprilag3=cpri[_n-3] if munn==munn[_n-3]
gen cprilag4=cpri[_n-4] if munn==munn[_n-4]
gen cprilag5=cpri[_n-5] if munn==munn[_n-5]
gen cprilag6=cpri[_n-6] if munn==munn[_n-6]
gen cprilag7=cpri[_n-7] if munn==munn[_n-7]
gen cprilag8=cpri[_n-8] if munn==munn[_n-8]
gen cprilag9=cpri[_n-9] if munn==munn[_n-9]
gen cprdlag1=cprd[_n-1] if munn==munn[_n-1]
gen cprdlag2=cprd[_n-2] if munn==munn[_n-2]
gen cprdlag3=cprd[_n-3] if munn==munn[_n-3]
gen cprdlag4=cprd[_n-4] if munn==munn[_n-4]
gen cprdlag5=cprd[_n-5] if munn==munn[_n-5]
gen cprdlag6=cprd[_n-6] if munn==munn[_n-6]
gen cprdlag7=cprd[_n-7] if munn==munn[_n-7]
gen cprdlag8=cprd[_n-8] if munn==munn[_n-8]
gen cprdlag9=cprd[_n-9] if munn==munn[_n-9]
gen winlag1=win[_n-1] if munn==munn[_n-1]
gen panlag1=pan[_n-1] if munn==munn[_n-1]
gen prilag1=pri[_n-1] if munn==munn[_n-1]
gen prdlag1=prd[_n-1] if munn==munn[_n-1]
gen vcpanlag1=vcpan[_n-1] if munn==munn[_n-1]
gen vcprilag1=vcpri[_n-1] if munn==munn[_n-1]
gen vcprdlag1=vcprd[_n-1] if munn==munn[_n-1]


preserve
drop if yr==2007
drop if yr<2004
gen tmppan1=0
gen tmppan2=0
gen tmppan3=0
gen tmppan4=0
gen tmppan5=0
gen tmppan6=0
gen tmppan7=0
gen tmppan8=0
gen tmppan9=0
gen tmppri1=0
gen tmppri2=0
gen tmppri3=0
gen tmppri4=0
gen tmppri5=0
gen tmppri6=0
gen tmppri7=0
gen tmppri8=0
gen tmppri9=0
gen tmpprd1=0
gen tmpprd2=0
gen tmpprd3=0
gen tmpprd4=0
gen tmpprd5=0
gen tmpprd6=0
gen tmpprd7=0
gen tmpprd8=0
gen tmpprd9=0

replace tmppan1=cpanlag1 if cpanlag1~=.
replace tmppan2=cpanlag2 if cpanlag2~=.
replace tmppan3=cpanlag3 if cpanlag3~=.
replace tmppan4=cpanlag4 if cpanlag4~=.
replace tmppan5=cpanlag5 if cpanlag5~=.
replace tmppan6=cpanlag6 if cpanlag6~=.
replace tmppan7=cpanlag7 if cpanlag7~=.
replace tmppan8=cpanlag8 if cpanlag8~=.
replace tmppan9=cpanlag9 if cpanlag9~=.
replace tmppri1=cprilag1 if cprilag1~=.
replace tmppri2=cprilag2 if cprilag2~=.
replace tmppri3=cprilag3 if cprilag3~=.
replace tmppri4=cprilag4 if cprilag4~=.
replace tmppri5=cprilag5 if cprilag5~=.
replace tmppri6=cprilag6 if cprilag6~=.
replace tmppri7=cprilag7 if cprilag7~=.
replace tmppri8=cprilag8 if cprilag8~=.
replace tmppri9=cprilag9 if cprilag9~=.
replace tmpprd1=cprdlag1 if cprdlag1~=.
replace tmpprd2=cprdlag2 if cprdlag2~=.
replace tmpprd3=cprdlag3 if cprdlag3~=.
replace tmpprd4=cprdlag4 if cprdlag4~=.
replace tmpprd5=cprdlag5 if cprdlag5~=.
replace tmpprd6=cprdlag6 if cprdlag6~=.
replace tmpprd7=cprdlag7 if cprdlag7~=.
replace tmpprd8=cprdlag8 if cprdlag8~=.
replace tmpprd9=cprdlag9 if cprdlag9~=.

gen candspan=tmppan1+tmppan2+tmppan3+tmppan4+tmppan5+tmppan6+tmppan7+tmppan8+tmppan9

gen candspri=tmppri1+tmppri2+tmppri3+tmppri4+tmppri5+tmppri6+tmppri7+tmppri8+tmppri9

gen candsprd=tmpprd1+tmpprd2+tmpprd3+tmpprd4+tmpprd5+tmpprd6+tmpprd7+tmpprd8+tmpprd9

xx
drop tmp*
drop if ID==.

keep ID candspan candspri candsprd
sort ID

restore

stop

gen mon=.
replace mon=1 if mo=="ene"
replace mon=2 if mo=="feb"
replace mon=3 if mo=="mar"
replace mon=4 if mo=="abr"
replace mon=5 if mo=="may"
replace mon=6 if mo=="jun"
replace mon=7 if mo=="jul"
replace mon=8 if mo=="ago"
replace mon=9 if mo=="sep"
replace mon=10 if mo=="oct"
replace mon=11 if mo=="nov"
replace mon=12 if mo=="dic"

gen dconcpr=0
replace dconcpr=1 if yr==1970 & mon==7
replace dconcpr=1 if yr==1976 & mon==7
replace dconcpr=1 if yr==1982 & mon==7
replace dconcpr=1 if yr==1988 & mon==7
replace dconcpr=1 if yr==1994 & mon==8
replace dconcpr=1 if yr==2000 & mon==7
replace dconcpr=1 if yr==2006 & mon==7

gen dconcmid=0
replace dconcmid=1 if yr==1973 & mon==7
replace dconcmid=1 if yr==1979 & mon==7
replace dconcmid=1 if yr==1985 & mon==7
replace dconcmid=1 if yr==1991 & mon==8
replace dconcmid=1 if yr==1997 & mon==8
replace dconcmid=1 if yr==2003 & mon==7

gen dconcgo=0
replace dconcgo=1 if edon==17 & yr==1970 & mon==4
replace dconcgo=1 if edon==7 & yr==1970 & mon==7
replace dconcgo=1 if edon==27 & yr==1970 & mon==11
replace dconcgo=1 if edon==14 & yr==1970 & mon==12
replace dconcgo=1 if edon==2 & yr==1971 & mon==8
replace dconcgo=1 if edon==6 & yr==1973 & mon==7
replace dconcgo=1 if edon==11 & yr==1973 & mon==7
replace dconcgo=1 if edon==19 & yr==1973 & mon==7
replace dconcgo=1 if edon==22 & yr==1973 & mon==7
replace dconcgo=1 if edon==8 & yr==1974 & mon==7
replace dconcgo=1 if edon==32 & yr==1974 & mon==7
replace dconcgo=1 if edon==1 & yr==1974 & mon==8
replace dconcgo=1 if edon==20 & yr==1974 & mon==8
replace dconcgo=1 if edon==30 & yr==1974 & mon==9
replace dconcgo=1 if edon==29 & yr==1974 & mon==10
replace dconcgo=1 if edon==25 & yr==1974 & mon==11
replace dconcgo=1 if edon==28 & yr==1974 & mon==12
replace dconcgo=1 if edon==23 & yr==1975 & mon==3
replace dconcgo=1 if edon==18 & yr==1975 & mon==6
replace dconcgo=1 if edon==5 & yr==1975 & mon==8
replace dconcgo=1 if edon==3 & yr==1975 & mon==11
replace dconcgo=1 if edon==31 & yr==1975 & mon==11
replace dconcgo=1 if edon==17 & yr==1976 & mon==3
replace dconcgo=1 if edon==7 & yr==1976 & mon==7
replace dconcgo=1 if edon==27 & yr==1976 & mon==11
replace dconcgo=1 if edon==14 & yr==1976 & mon==12
replace dconcgo=1 if edon==2 & yr==1977 & mon==7
replace dconcgo=1 if edon==4 & yr==1979 & mon==7
replace dconcgo=1 if edon==6 & yr==1979 & mon==7
replace dconcgo=1 if edon==11 & yr==1979 & mon==7
replace dconcgo=1 if edon==19 & yr==1979 & mon==7
replace dconcgo=1 if edon==22 & yr==1979 & mon==7
replace dconcgo=1 if edon==24 & yr==1979 & mon==7
replace dconcgo=1 if edon==26 & yr==1979 & mon==7
replace dconcgo=1 if edon==8 & yr==1980 & mon==7
replace dconcgo=1 if edon==10 & yr==1980 & mon==7
replace dconcgo=1 if edon==16 & yr==1980 & mon==7
replace dconcgo=1 if edon==32 & yr==1980 & mon==7
replace dconcgo=1 if edon==1 & yr==1980 & mon==8
replace dconcgo=1 if edon==30 & yr==1980 & mon==9
replace dconcgo=1 if edon==25 & yr==1980 & mon==10
replace dconcgo=1 if edon==20 & yr==1980 & mon==11
replace dconcgo=1 if edon==21 & yr==1980 & mon==11
replace dconcgo=1 if edon==29 & yr==1980 & mon==11
replace dconcgo=1 if edon==12 & yr==1980 & mon==12
replace dconcgo=1 if edon==28 & yr==1980 & mon==12
replace dconcgo=1 if edon==13 & yr==1981 & mon==1
replace dconcgo=1 if edon==23 & yr==1981 & mon==3
replace dconcgo=1 if edon==18 & yr==1981 & mon==6
replace dconcgo=1 if edon==15 & yr==1981 & mon==7
replace dconcgo=1 if edon==5 & yr==1981 & mon==8
replace dconcgo=1 if edon==3 & yr==1981 & mon==11
replace dconcgo=1 if edon==31 & yr==1981 & mon==11
replace dconcgo=1 if edon==17 & yr==1982 & mon==4
replace dconcgo=1 if edon==7 & yr==1982 & mon==7
replace dconcgo=1 if edon==27 & yr==1982 & mon==11
replace dconcgo=1 if edon==14 & yr==1982 & mon==12
replace dconcgo=1 if edon==2 & yr==1983 & mon==9
replace dconcgo=1 if edon==4 & yr==1985 & mon==7
replace dconcgo=1 if edon==6 & yr==1985 & mon==7
replace dconcgo=1 if edon==11 & yr==1985 & mon==7
replace dconcgo=1 if edon==19 & yr==1985 & mon==7
replace dconcgo=1 if edon==22 & yr==1985 & mon==7
replace dconcgo=1 if edon==24 & yr==1985 & mon==7
replace dconcgo=1 if edon==26 & yr==1985 & mon==7
replace dconcgo=1 if edon==8 & yr==1986 & mon==7
replace dconcgo=1 if edon==10 & yr==1986 & mon==7
replace dconcgo=1 if edon==16 & yr==1986 & mon==7
replace dconcgo=1 if edon==32 & yr==1986 & mon==7
replace dconcgo=1 if edon==20 & yr==1986 & mon==8
replace dconcgo=1 if edon==30 & yr==1986 & mon==9
replace dconcgo=1 if edon==1 & yr==1986 & mon==10
replace dconcgo=1 if edon==25 & yr==1986 & mon==10
replace dconcgo=1 if edon==21 & yr==1986 & mon==11
replace dconcgo=1 if edon==29 & yr==1986 & mon==11
replace dconcgo=1 if edon==12 & yr==1986 & mon==12
replace dconcgo=1 if edon==28 & yr==1986 & mon==12
replace dconcgo=1 if edon==13 & yr==1987 & mon==1
replace dconcgo=1 if edon==3 & yr==1987 & mon==2
replace dconcgo=1 if edon==23 & yr==1987 & mon==3
replace dconcgo=1 if edon==18 & yr==1987 & mon==6
replace dconcgo=1 if edon==15 & yr==1987 & mon==7
replace dconcgo=1 if edon==5 & yr==1987 & mon==10
replace dconcgo=1 if edon==31 & yr==1987 & mon==11
replace dconcgo=1 if edon==17 & yr==1988 & mon==3
replace dconcgo=1 if edon==7 & yr==1988 & mon==7
replace dconcgo=1 if edon==27 & yr==1988 & mon==11
replace dconcgo=1 if edon==14 & yr==1988 & mon==12
replace dconcgo=1 if edon==2 & yr==1989 & mon==7
replace dconcgo=1 if edon==19 & yr==1991 & mon==7
replace dconcgo=1 if edon==4 & yr==1991 & mon==8
replace dconcgo=1 if edon==6 & yr==1991 & mon==8
replace dconcgo=1 if edon==11 & yr==1991 & mon==8
replace dconcgo=1 if edon==22 & yr==1991 & mon==8
replace dconcgo=1 if edon==24 & yr==1991 & mon==8
replace dconcgo=1 if edon==26 & yr==1991 & mon==8
replace dconcgo=1 if edon==8 & yr==1992 & mon==7
replace dconcgo=1 if edon==16 & yr==1992 & mon==7
replace dconcgo=1 if edon==1 & yr==1992 & mon==8
replace dconcgo=1 if edon==10 & yr==1992 & mon==8
replace dconcgo=1 if edon==20 & yr==1992 & mon==8
replace dconcgo=1 if edon==30 & yr==1992 & mon==8
replace dconcgo=1 if edon==32 & yr==1992 & mon==8
replace dconcgo=1 if edon==21 & yr==1992 & mon==11
replace dconcgo=1 if edon==25 & yr==1992 & mon==11
replace dconcgo=1 if edon==28 & yr==1992 & mon==11
replace dconcgo=1 if edon==29 & yr==1992 & mon==11
replace dconcgo=1 if edon==3 & yr==1993 & mon==2
replace dconcgo=1 if edon==12 & yr==1993 & mon==2
replace dconcgo=1 if edon==13 & yr==1993 & mon==2
replace dconcgo=1 if edon==23 & yr==1993 & mon==2
replace dconcgo=1 if edon==24 & yr==1993 & mon==4
replace dconcgo=1 if edon==15 & yr==1993 & mon==7
replace dconcgo=1 if edon==18 & yr==1993 & mon==7
replace dconcgo=1 if edon==5 & yr==1993 & mon==9
replace dconcgo=1 if edon==31 & yr==1993 & mon==11
replace dconcgo=1 if edon==17 & yr==1994 & mon==3
replace dconcgo=1 if edon==7 & yr==1994 & mon==8
replace dconcgo=1 if edon==27 & yr==1994 & mon==11
replace dconcgo=1 if edon==14 & yr==1995 & mon==2
replace dconcgo=1 if edon==11 & yr==1995 & mon==5
replace dconcgo=1 if edon==31 & yr==1995 & mon==5
replace dconcgo=1 if edon==2 & yr==1995 & mon==8
replace dconcgo=1 if edon==16 & yr==1995 & mon==11
replace dconcgo=1 if edon==4 & yr==1997 & mon==7
replace dconcgo=1 if edon==6 & yr==1997 & mon==7
replace dconcgo=1 if edon==9 & yr==1997 & mon==7
replace dconcgo=1 if edon==19 & yr==1997 & mon==7
replace dconcgo=1 if edon==22 & yr==1997 & mon==7
replace dconcgo=1 if edon==24 & yr==1997 & mon==7
replace dconcgo=1 if edon==26 & yr==1997 & mon==7
replace dconcgo=1 if edon==8 & yr==1998 & mon==7
replace dconcgo=1 if edon==10 & yr==1998 & mon==7
replace dconcgo=1 if edon==32 & yr==1998 & mon==7
replace dconcgo=1 if edon==1 & yr==1998 & mon==8
replace dconcgo=1 if edon==20 & yr==1998 & mon==8
replace dconcgo=1 if edon==30 & yr==1998 & mon==8
replace dconcgo=1 if edon==28 & yr==1998 & mon==10
replace dconcgo=1 if edon==21 & yr==1998 & mon==11
replace dconcgo=1 if edon==25 & yr==1998 & mon==11
replace dconcgo=1 if edon==29 & yr==1998 & mon==11
replace dconcgo=1 if edon==3 & yr==1999 & mon==2
replace dconcgo=1 if edon==12 & yr==1999 & mon==2
replace dconcgo=1 if edon==13 & yr==1999 & mon==2
replace dconcgo=1 if edon==23 & yr==1999 & mon==2
replace dconcgo=1 if edon==15 & yr==1999 & mon==7
replace dconcgo=1 if edon==18 & yr==1999 & mon==7
replace dconcgo=1 if edon==5 & yr==1999 & mon==9
replace dconcgo=1 if edon==9 & yr==2000 & mon==7
replace dconcgo=1 if edon==11 & yr==2000 & mon==7
replace dconcgo=1 if edon==17 & yr==2000 & mon==7
replace dconcgo=1 if edon==7 & yr==2000 & mon==8
replace dconcgo=1 if edon==27 & yr==2000 & mon==10
replace dconcgo=1 if edon==14 & yr==2000 & mon==11
replace dconcgo=1 if edon==31 & yr==2001 & mon==5
replace dconcgo=1 if edon==2 & yr==2001 & mon==7
replace dconcgo=1 if edon==27 & yr==2001 & mon==8
replace dconcgo=1 if edon==16 & yr==2001 & mon==11
replace dconcgo=1 if edon==4 & yr==2003 & mon==7
replace dconcgo=1 if edon==6 & yr==2003 & mon==7
replace dconcgo=1 if edon==19 & yr==2003 & mon==7
replace dconcgo=1 if edon==22 & yr==2003 & mon==7
replace dconcgo=1 if edon==24 & yr==2003 & mon==7
replace dconcgo=1 if edon==26 & yr==2003 & mon==7
replace dconcgo=1 if edon==8 & yr==2004 & mon==7
replace dconcgo=1 if edon==10 & yr==2004 & mon==7
replace dconcgo=1 if edon==32 & yr==2004 & mon==7
replace dconcgo=1 if edon==1 & yr==2004 & mon==8
replace dconcgo=1 if edon==20 & yr==2004 & mon==8
replace dconcgo=1 if edon==30 & yr==2004 & mon==9
replace dconcgo=1 if edon==21 & yr==2004 & mon==11
replace dconcgo=1 if edon==25 & yr==2004 & mon==11
replace dconcgo=1 if edon==28 & yr==2004 & mon==11
replace dconcgo=1 if edon==29 & yr==2004 & mon==11
replace dconcgo=1 if edon==3 & yr==2005 & mon==2
replace dconcgo=1 if edon==12 & yr==2005 & mon==2
replace dconcgo=1 if edon==13 & yr==2005 & mon==2
replace dconcgo=1 if edon==23 & yr==2005 & mon==2
replace dconcgo=1 if edon==6 & yr==2005 & mon==4
replace dconcgo=1 if edon==15 & yr==2005 & mon==7
replace dconcgo=1 if edon==18 & yr==2005 & mon==7
replace dconcgo=1 if edon==5 & yr==2005 & mon==9
replace dconcgo=1 if edon==9 & yr==2006 & mon==7
replace dconcgo=1 if edon==14 & yr==2006 & mon==7
replace dconcgo=1 if edon==17 & yr==2006 & mon==7
replace dconcgo=1 if edon==7 & yr==2006 & mon==8
replace dconcgo=1 if edon==27 & yr==2006 & mon==10
replace dconcgo=1 if edon==31 & yr==2007 & mon==5
replace dconcgo=1 if edon==2 & yr==2007 & mon==8
replace dconcgo=1 if edon==17 & yr==2007 & mon==11

gen dpangov=0
gen dprigov=0
gen dprdgov=0
replace dprigov=1 if yr<1989
replace dprigov=1 if edon==1 & yr==1988 & dconcgo==0
replace dprigov=1 if edon==1 & yr==1989 & dconcgo==0
replace dprigov=1 if edon==1 & yr==1990 & dconcgo==0
replace dprigov=1 if edon==1 & yr==1991 & dconcgo==0
replace dprigov=1 if edon==1 & yr==1992 & dconcgo==0
replace dprigov=1 if edon==1 & yr==1993 & dconcgo==0
replace dprigov=1 if edon==1 & yr==1994 & dconcgo==0
replace dprigov=1 if edon==1 & yr==1995 & dconcgo==0
replace dprigov=1 if edon==1 & yr==1996 & dconcgo==0
replace dprigov=1 if edon==1 & yr==1997 & dconcgo==0
replace dpangov=1 if edon==1 & yr==1998 & dconcgo==0
replace dpangov=1 if edon==1 & yr==1999 & dconcgo==0
replace dpangov=1 if edon==1 & yr==2000 & dconcgo==0
replace dpangov=1 if edon==1 & yr==2001 & dconcgo==0
replace dpangov=1 if edon==1 & yr==2002 & dconcgo==0
replace dpangov=1 if edon==1 & yr==2003 & dconcgo==0
replace dpangov=1 if edon==1 & yr==2004 & dconcgo==0
replace dpangov=1 if edon==1 & yr==2005 & dconcgo==0
replace dpangov=1 if edon==1 & yr==2006 & dconcgo==0
replace dprigov=1 if edon==2 & yr==1988 & dconcgo==0
replace dpangov=1 if edon==2 & yr==1989 & dconcgo==0
replace dpangov=1 if edon==2 & yr==1990 & dconcgo==0
replace dpangov=1 if edon==2 & yr==1991 & dconcgo==0
replace dpangov=1 if edon==2 & yr==1992 & dconcgo==0
replace dpangov=1 if edon==2 & yr==1993 & dconcgo==0
replace dpangov=1 if edon==2 & yr==1994 & dconcgo==0
replace dpangov=1 if edon==2 & yr==1995 & dconcgo==0
replace dpangov=1 if edon==2 & yr==1996 & dconcgo==0
replace dpangov=1 if edon==2 & yr==1997 & dconcgo==0
replace dpangov=1 if edon==2 & yr==1998 & dconcgo==0
replace dpangov=1 if edon==2 & yr==1999 & dconcgo==0
replace dpangov=1 if edon==2 & yr==2000 & dconcgo==0
replace dpangov=1 if edon==2 & yr==2001 & dconcgo==0
replace dpangov=1 if edon==2 & yr==2002 & dconcgo==0
replace dpangov=1 if edon==2 & yr==2003 & dconcgo==0
replace dpangov=1 if edon==2 & yr==2004 & dconcgo==0
replace dpangov=1 if edon==2 & yr==2005 & dconcgo==0
replace dpangov=1 if edon==2 & yr==2006 & dconcgo==0
replace dprigov=1 if edon==3 & yr==1988 & dconcgo==0
replace dprigov=1 if edon==3 & yr==1989 & dconcgo==0
replace dprigov=1 if edon==3 & yr==1990 & dconcgo==0
replace dprigov=1 if edon==3 & yr==1991 & dconcgo==0
replace dprigov=1 if edon==3 & yr==1992 & dconcgo==0
replace dprigov=1 if edon==3 & yr==1993 & dconcgo==0
replace dprigov=1 if edon==3 & yr==1994 & dconcgo==0
replace dprigov=1 if edon==3 & yr==1995 & dconcgo==0
replace dprigov=1 if edon==3 & yr==1996 & dconcgo==0
replace dprigov=1 if edon==3 & yr==1997 & dconcgo==0
replace dprigov=1 if edon==3 & yr==1998 & dconcgo==0
replace dprdgov=1 if edon==3 & yr==1999 & dconcgo==0
replace dprdgov=1 if edon==3 & yr==2000 & dconcgo==0
replace dprdgov=1 if edon==3 & yr==2001 & dconcgo==0
replace dprdgov=1 if edon==3 & yr==2002 & dconcgo==0
replace dprdgov=1 if edon==3 & yr==2003 & dconcgo==0
replace dprdgov=1 if edon==3 & yr==2004 & dconcgo==0
replace dprdgov=1 if edon==3 & yr==2005 & dconcgo==0
replace dprdgov=1 if edon==3 & yr==2006 & dconcgo==0
replace dprigov=1 if edon==4 & yr==1988 & dconcgo==0
replace dprigov=1 if edon==4 & yr==1989 & dconcgo==0
replace dprigov=1 if edon==4 & yr==1990 & dconcgo==0
replace dprigov=1 if edon==4 & yr==1991 & dconcgo==0
replace dprigov=1 if edon==4 & yr==1992 & dconcgo==0
replace dprigov=1 if edon==4 & yr==1993 & dconcgo==0
replace dprigov=1 if edon==4 & yr==1994 & dconcgo==0
replace dprigov=1 if edon==4 & yr==1995 & dconcgo==0
replace dprigov=1 if edon==4 & yr==1996 & dconcgo==0
replace dprigov=1 if edon==4 & yr==1997 & dconcgo==0
replace dprigov=1 if edon==4 & yr==1998 & dconcgo==0
replace dprigov=1 if edon==4 & yr==1999 & dconcgo==0
replace dprigov=1 if edon==4 & yr==2000 & dconcgo==0
replace dprigov=1 if edon==4 & yr==2001 & dconcgo==0
replace dprigov=1 if edon==4 & yr==2002 & dconcgo==0
replace dprigov=1 if edon==4 & yr==2003 & dconcgo==0
replace dprigov=1 if edon==4 & yr==2004 & dconcgo==0
replace dprigov=1 if edon==4 & yr==2005 & dconcgo==0
replace dprigov=1 if edon==4 & yr==2006 & dconcgo==0
replace dprigov=1 if edon==5 & yr==1988 & dconcgo==0
replace dprigov=1 if edon==5 & yr==1989 & dconcgo==0
replace dprigov=1 if edon==5 & yr==1990 & dconcgo==0
replace dprigov=1 if edon==5 & yr==1991 & dconcgo==0
replace dprigov=1 if edon==5 & yr==1992 & dconcgo==0
replace dprigov=1 if edon==5 & yr==1993 & dconcgo==0
replace dprigov=1 if edon==5 & yr==1994 & dconcgo==0
replace dprigov=1 if edon==5 & yr==1995 & dconcgo==0
replace dprigov=1 if edon==5 & yr==1996 & dconcgo==0
replace dprigov=1 if edon==5 & yr==1997 & dconcgo==0
replace dprigov=1 if edon==5 & yr==1998 & dconcgo==0
replace dprigov=1 if edon==5 & yr==1999 & dconcgo==0
replace dprigov=1 if edon==5 & yr==2000 & dconcgo==0
replace dprigov=1 if edon==5 & yr==2001 & dconcgo==0
replace dprigov=1 if edon==5 & yr==2002 & dconcgo==0
replace dprigov=1 if edon==5 & yr==2003 & dconcgo==0
replace dprigov=1 if edon==5 & yr==2004 & dconcgo==0
replace dprigov=1 if edon==5 & yr==2005 & dconcgo==0
replace dprigov=1 if edon==5 & yr==2006 & dconcgo==0
replace dprigov=1 if edon==6 & yr==1988 & dconcgo==0
replace dprigov=1 if edon==6 & yr==1989 & dconcgo==0
replace dprigov=1 if edon==6 & yr==1990 & dconcgo==0
replace dprigov=1 if edon==6 & yr==1991 & dconcgo==0
replace dprigov=1 if edon==6 & yr==1992 & dconcgo==0
replace dprigov=1 if edon==6 & yr==1993 & dconcgo==0
replace dprigov=1 if edon==6 & yr==1994 & dconcgo==0
replace dprigov=1 if edon==6 & yr==1995 & dconcgo==0
replace dprigov=1 if edon==6 & yr==1996 & dconcgo==0
replace dprigov=1 if edon==6 & yr==1997 & dconcgo==0
replace dprigov=1 if edon==6 & yr==1998 & dconcgo==0
replace dprigov=1 if edon==6 & yr==1999 & dconcgo==0
replace dprigov=1 if edon==6 & yr==2000 & dconcgo==0
replace dprigov=1 if edon==6 & yr==2001 & dconcgo==0
replace dprigov=1 if edon==6 & yr==2002 & dconcgo==0
replace dprigov=1 if edon==6 & yr==2003 & dconcgo==0
replace dprigov=1 if edon==6 & yr==2004 & dconcgo==0
replace dprigov=1 if edon==6 & yr==2005 & dconcgo==0
replace dprigov=1 if edon==6 & yr==2006 & dconcgo==0
replace dprigov=1 if edon==7 & yr==1988 & dconcgo==0
replace dprigov=1 if edon==7 & yr==1989 & dconcgo==0
replace dprigov=1 if edon==7 & yr==1990 & dconcgo==0
replace dprigov=1 if edon==7 & yr==1991 & dconcgo==0
replace dprigov=1 if edon==7 & yr==1992 & dconcgo==0
replace dprigov=1 if edon==7 & yr==1993 & dconcgo==0
replace dprigov=1 if edon==7 & yr==1994 & dconcgo==0
replace dprigov=1 if edon==7 & yr==1995 & dconcgo==0
replace dprigov=1 if edon==7 & yr==1996 & dconcgo==0
replace dprigov=1 if edon==7 & yr==1997 & dconcgo==0
replace dprigov=1 if edon==7 & yr==1998 & dconcgo==0
replace dprigov=1 if edon==7 & yr==1999 & dconcgo==0
replace dprdgov=1 if edon==7 & yr==2000 & dconcgo==0
replace dprdgov=1 if edon==7 & yr==2001 & dconcgo==0
replace dprdgov=1 if edon==7 & yr==2002 & dconcgo==0
replace dprdgov=1 if edon==7 & yr==2003 & dconcgo==0
replace dprdgov=1 if edon==7 & yr==2004 & dconcgo==0
replace dprdgov=1 if edon==7 & yr==2005 & dconcgo==0
replace dprdgov=1 if edon==7 & yr==2006 & dconcgo==0
replace dprigov=1 if edon==8 & yr==1988 & dconcgo==0
replace dprigov=1 if edon==8 & yr==1989 & dconcgo==0
replace dprigov=1 if edon==8 & yr==1990 & dconcgo==0
replace dprigov=1 if edon==8 & yr==1991 & dconcgo==0
replace dpangov=1 if edon==8 & yr==1992 & dconcgo==0
replace dpangov=1 if edon==8 & yr==1993 & dconcgo==0
replace dpangov=1 if edon==8 & yr==1994 & dconcgo==0
replace dpangov=1 if edon==8 & yr==1995 & dconcgo==0
replace dpangov=1 if edon==8 & yr==1996 & dconcgo==0
replace dpangov=1 if edon==8 & yr==1997 & dconcgo==0
replace dprigov=1 if edon==8 & yr==1998 & dconcgo==0
replace dprigov=1 if edon==8 & yr==1999 & dconcgo==0
replace dprigov=1 if edon==8 & yr==2000 & dconcgo==0
replace dprigov=1 if edon==8 & yr==2001 & dconcgo==0
replace dprigov=1 if edon==8 & yr==2002 & dconcgo==0
replace dprigov=1 if edon==8 & yr==2003 & dconcgo==0
replace dprigov=1 if edon==8 & yr==2004 & dconcgo==0
replace dprigov=1 if edon==8 & yr==2005 & dconcgo==0
replace dprigov=1 if edon==8 & yr==2006 & dconcgo==0
replace dprigov=1 if edon==9 & yr==1988 & dconcgo==0
replace dprigov=1 if edon==9 & yr==1989 & dconcgo==0
replace dprigov=1 if edon==9 & yr==1990 & dconcgo==0
replace dprigov=1 if edon==9 & yr==1991 & dconcgo==0
replace dprigov=1 if edon==9 & yr==1992 & dconcgo==0
replace dprigov=1 if edon==9 & yr==1993 & dconcgo==0
replace dprigov=1 if edon==9 & yr==1994 & dconcgo==0
replace dprigov=1 if edon==9 & yr==1995 & dconcgo==0
replace dprigov=1 if edon==9 & yr==1996 & dconcgo==0
replace dprdgov=1 if edon==9 & yr==1997 & dconcgo==0
replace dprdgov=1 if edon==9 & yr==1998 & dconcgo==0
replace dprdgov=1 if edon==9 & yr==1999 & dconcgo==0
replace dprdgov=1 if edon==9 & yr==2000 & dconcgo==0
replace dprdgov=1 if edon==9 & yr==2001 & dconcgo==0
replace dprdgov=1 if edon==9 & yr==2002 & dconcgo==0
replace dprdgov=1 if edon==9 & yr==2003 & dconcgo==0
replace dprdgov=1 if edon==9 & yr==2004 & dconcgo==0
replace dprdgov=1 if edon==9 & yr==2005 & dconcgo==0
replace dprdgov=1 if edon==9 & yr==2006 & dconcgo==0
replace dprigov=1 if edon==10 & yr==1988 & dconcgo==0
replace dprigov=1 if edon==10 & yr==1989 & dconcgo==0
replace dprigov=1 if edon==10 & yr==1990 & dconcgo==0
replace dprigov=1 if edon==10 & yr==1991 & dconcgo==0
replace dprigov=1 if edon==10 & yr==1992 & dconcgo==0
replace dprigov=1 if edon==10 & yr==1993 & dconcgo==0
replace dprigov=1 if edon==10 & yr==1994 & dconcgo==0
replace dprigov=1 if edon==10 & yr==1995 & dconcgo==0
replace dprigov=1 if edon==10 & yr==1996 & dconcgo==0
replace dprigov=1 if edon==10 & yr==1997 & dconcgo==0
replace dprigov=1 if edon==10 & yr==1998 & dconcgo==0
replace dprigov=1 if edon==10 & yr==1999 & dconcgo==0
replace dprigov=1 if edon==10 & yr==2000 & dconcgo==0
replace dprigov=1 if edon==10 & yr==2001 & dconcgo==0
replace dprigov=1 if edon==10 & yr==2002 & dconcgo==0
replace dprigov=1 if edon==10 & yr==2003 & dconcgo==0
replace dprigov=1 if edon==10 & yr==2004 & dconcgo==0
replace dprigov=1 if edon==10 & yr==2005 & dconcgo==0
replace dprigov=1 if edon==10 & yr==2006 & dconcgo==0
replace dprigov=1 if edon==11 & yr==1988 & dconcgo==0
replace dprigov=1 if edon==11 & yr==1989 & dconcgo==0
replace dprigov=1 if edon==11 & yr==1990 & dconcgo==0
replace dpangov=1 if edon==11 & yr==1991 & dconcgo==0
replace dpangov=1 if edon==11 & yr==1992 & dconcgo==0
replace dpangov=1 if edon==11 & yr==1993 & dconcgo==0
replace dpangov=1 if edon==11 & yr==1994 & dconcgo==0
replace dpangov=1 if edon==11 & yr==1995 & dconcgo==0
replace dpangov=1 if edon==11 & yr==1996 & dconcgo==0
replace dpangov=1 if edon==11 & yr==1997 & dconcgo==0
replace dpangov=1 if edon==11 & yr==1998 & dconcgo==0
replace dpangov=1 if edon==11 & yr==1999 & dconcgo==0
replace dpangov=1 if edon==11 & yr==2000 & dconcgo==0
replace dpangov=1 if edon==11 & yr==2001 & dconcgo==0
replace dpangov=1 if edon==11 & yr==2002 & dconcgo==0
replace dpangov=1 if edon==11 & yr==2003 & dconcgo==0
replace dpangov=1 if edon==11 & yr==2004 & dconcgo==0
replace dpangov=1 if edon==11 & yr==2005 & dconcgo==0
replace dpangov=1 if edon==11 & yr==2006 & dconcgo==0
replace dprigov=1 if edon==12 & yr==1988 & dconcgo==0
replace dprigov=1 if edon==12 & yr==1989 & dconcgo==0
replace dprigov=1 if edon==12 & yr==1990 & dconcgo==0
replace dprigov=1 if edon==12 & yr==1991 & dconcgo==0
replace dprigov=1 if edon==12 & yr==1992 & dconcgo==0
replace dprigov=1 if edon==12 & yr==1993 & dconcgo==0
replace dprigov=1 if edon==12 & yr==1994 & dconcgo==0
replace dprigov=1 if edon==12 & yr==1995 & dconcgo==0
replace dprigov=1 if edon==12 & yr==1996 & dconcgo==0
replace dprigov=1 if edon==12 & yr==1997 & dconcgo==0
replace dprigov=1 if edon==12 & yr==1998 & dconcgo==0
replace dprigov=1 if edon==12 & yr==1999 & dconcgo==0
replace dprigov=1 if edon==12 & yr==2000 & dconcgo==0
replace dprigov=1 if edon==12 & yr==2001 & dconcgo==0
replace dprigov=1 if edon==12 & yr==2002 & dconcgo==0
replace dprigov=1 if edon==12 & yr==2003 & dconcgo==0
replace dprigov=1 if edon==12 & yr==2004 & dconcgo==0
replace dprdgov=1 if edon==12 & yr==2005 & dconcgo==0
replace dprdgov=1 if edon==12 & yr==2006 & dconcgo==0
replace dprigov=1 if edon==13 & yr==1988 & dconcgo==0
replace dprigov=1 if edon==13 & yr==1989 & dconcgo==0
replace dprigov=1 if edon==13 & yr==1990 & dconcgo==0
replace dprigov=1 if edon==13 & yr==1991 & dconcgo==0
replace dprigov=1 if edon==13 & yr==1992 & dconcgo==0
replace dprigov=1 if edon==13 & yr==1993 & dconcgo==0
replace dprigov=1 if edon==13 & yr==1994 & dconcgo==0
replace dprigov=1 if edon==13 & yr==1995 & dconcgo==0
replace dprigov=1 if edon==13 & yr==1996 & dconcgo==0
replace dprigov=1 if edon==13 & yr==1997 & dconcgo==0
replace dprigov=1 if edon==13 & yr==1998 & dconcgo==0
replace dprigov=1 if edon==13 & yr==1999 & dconcgo==0
replace dprigov=1 if edon==13 & yr==2000 & dconcgo==0
replace dprigov=1 if edon==13 & yr==2001 & dconcgo==0
replace dprigov=1 if edon==13 & yr==2002 & dconcgo==0
replace dprigov=1 if edon==13 & yr==2003 & dconcgo==0
replace dprigov=1 if edon==13 & yr==2004 & dconcgo==0
replace dprigov=1 if edon==13 & yr==2005 & dconcgo==0
replace dprigov=1 if edon==13 & yr==2006 & dconcgo==0
replace dprigov=1 if edon==14 & yr==1988 & dconcgo==0
replace dprigov=1 if edon==14 & yr==1989 & dconcgo==0
replace dprigov=1 if edon==14 & yr==1990 & dconcgo==0
replace dprigov=1 if edon==14 & yr==1991 & dconcgo==0
replace dprigov=1 if edon==14 & yr==1992 & dconcgo==0
replace dprigov=1 if edon==14 & yr==1993 & dconcgo==0
replace dprigov=1 if edon==14 & yr==1994 & dconcgo==0
replace dpangov=1 if edon==14 & yr==1995 & dconcgo==0
replace dpangov=1 if edon==14 & yr==1996 & dconcgo==0
replace dpangov=1 if edon==14 & yr==1997 & dconcgo==0
replace dpangov=1 if edon==14 & yr==1998 & dconcgo==0
replace dpangov=1 if edon==14 & yr==1999 & dconcgo==0
replace dpangov=1 if edon==14 & yr==2000 & dconcgo==0
replace dpangov=1 if edon==14 & yr==2001 & dconcgo==0
replace dpangov=1 if edon==14 & yr==2002 & dconcgo==0
replace dpangov=1 if edon==14 & yr==2003 & dconcgo==0
replace dpangov=1 if edon==14 & yr==2004 & dconcgo==0
replace dpangov=1 if edon==14 & yr==2005 & dconcgo==0
replace dpangov=1 if edon==14 & yr==2006 & dconcgo==0
replace dprigov=1 if edon==15 & yr==1988 & dconcgo==0
replace dprigov=1 if edon==15 & yr==1989 & dconcgo==0
replace dprigov=1 if edon==15 & yr==1990 & dconcgo==0
replace dprigov=1 if edon==15 & yr==1991 & dconcgo==0
replace dprigov=1 if edon==15 & yr==1992 & dconcgo==0
replace dprigov=1 if edon==15 & yr==1993 & dconcgo==0
replace dprigov=1 if edon==15 & yr==1994 & dconcgo==0
replace dprigov=1 if edon==15 & yr==1995 & dconcgo==0
replace dprigov=1 if edon==15 & yr==1996 & dconcgo==0
replace dprigov=1 if edon==15 & yr==1997 & dconcgo==0
replace dprigov=1 if edon==15 & yr==1998 & dconcgo==0
replace dprigov=1 if edon==15 & yr==1999 & dconcgo==0
replace dprigov=1 if edon==15 & yr==2000 & dconcgo==0
replace dprigov=1 if edon==15 & yr==2001 & dconcgo==0
replace dprigov=1 if edon==15 & yr==2002 & dconcgo==0
replace dprigov=1 if edon==15 & yr==2003 & dconcgo==0
replace dprigov=1 if edon==15 & yr==2004 & dconcgo==0
replace dprigov=1 if edon==15 & yr==2005 & dconcgo==0
replace dprigov=1 if edon==15 & yr==2006 & dconcgo==0
replace dprigov=1 if edon==16 & yr==1988 & dconcgo==0
replace dprigov=1 if edon==16 & yr==1989 & dconcgo==0
replace dprigov=1 if edon==16 & yr==1990 & dconcgo==0
replace dprigov=1 if edon==16 & yr==1991 & dconcgo==0
replace dprigov=1 if edon==16 & yr==1992 & dconcgo==0
replace dprigov=1 if edon==16 & yr==1993 & dconcgo==0
replace dprigov=1 if edon==16 & yr==1994 & dconcgo==0
replace dprigov=1 if edon==16 & yr==1995 & dconcgo==0
replace dprigov=1 if edon==16 & yr==1996 & dconcgo==0
replace dprigov=1 if edon==16 & yr==1997 & dconcgo==0
replace dprigov=1 if edon==16 & yr==1998 & dconcgo==0
replace dprigov=1 if edon==16 & yr==1999 & dconcgo==0
replace dprigov=1 if edon==16 & yr==2000 & dconcgo==0
replace dprdgov=1 if edon==16 & yr==2001 & dconcgo==0
replace dprdgov=1 if edon==16 & yr==2002 & dconcgo==0
replace dprdgov=1 if edon==16 & yr==2003 & dconcgo==0
replace dprdgov=1 if edon==16 & yr==2004 & dconcgo==0
replace dprdgov=1 if edon==16 & yr==2005 & dconcgo==0
replace dprdgov=1 if edon==16 & yr==2006 & dconcgo==0
replace dprigov=1 if edon==17 & yr==1988 & dconcgo==0
replace dprigov=1 if edon==17 & yr==1989 & dconcgo==0
replace dprigov=1 if edon==17 & yr==1990 & dconcgo==0
replace dprigov=1 if edon==17 & yr==1991 & dconcgo==0
replace dprigov=1 if edon==17 & yr==1992 & dconcgo==0
replace dprigov=1 if edon==17 & yr==1993 & dconcgo==0
replace dprigov=1 if edon==17 & yr==1994 & dconcgo==0
replace dprigov=1 if edon==17 & yr==1995 & dconcgo==0
replace dprigov=1 if edon==17 & yr==1996 & dconcgo==0
replace dprigov=1 if edon==17 & yr==1997 & dconcgo==0
replace dprigov=1 if edon==17 & yr==1998 & dconcgo==0
replace dprigov=1 if edon==17 & yr==1999 & dconcgo==0
replace dpangov=1 if edon==17 & yr==2000 & dconcgo==0
replace dpangov=1 if edon==17 & yr==2001 & dconcgo==0
replace dpangov=1 if edon==17 & yr==2002 & dconcgo==0
replace dpangov=1 if edon==17 & yr==2003 & dconcgo==0
replace dpangov=1 if edon==17 & yr==2004 & dconcgo==0
replace dpangov=1 if edon==17 & yr==2005 & dconcgo==0
replace dpangov=1 if edon==17 & yr==2006 & dconcgo==0
replace dprigov=1 if edon==18 & yr==1988 & dconcgo==0
replace dprigov=1 if edon==18 & yr==1989 & dconcgo==0
replace dprigov=1 if edon==18 & yr==1990 & dconcgo==0
replace dprigov=1 if edon==18 & yr==1991 & dconcgo==0
replace dprigov=1 if edon==18 & yr==1992 & dconcgo==0
replace dprigov=1 if edon==18 & yr==1993 & dconcgo==0
replace dprigov=1 if edon==18 & yr==1994 & dconcgo==0
replace dprigov=1 if edon==18 & yr==1995 & dconcgo==0
replace dprigov=1 if edon==18 & yr==1996 & dconcgo==0
replace dprigov=1 if edon==18 & yr==1997 & dconcgo==0
replace dprigov=1 if edon==18 & yr==1998 & dconcgo==0
replace dpangov=1 if edon==18 & yr==1999 & dconcgo==0
replace dpangov=1 if edon==18 & yr==2000 & dconcgo==0
replace dpangov=1 if edon==18 & yr==2001 & dconcgo==0
replace dpangov=1 if edon==18 & yr==2002 & dconcgo==0
replace dpangov=1 if edon==18 & yr==2003 & dconcgo==0
replace dpangov=1 if edon==18 & yr==2004 & dconcgo==0
replace dprigov=1 if edon==18 & yr==2005 & dconcgo==0
replace dprigov=1 if edon==18 & yr==2006 & dconcgo==0
replace dprigov=1 if edon==19 & yr==1988 & dconcgo==0
replace dprigov=1 if edon==19 & yr==1989 & dconcgo==0
replace dprigov=1 if edon==19 & yr==1990 & dconcgo==0
replace dprigov=1 if edon==19 & yr==1991 & dconcgo==0
replace dprigov=1 if edon==19 & yr==1992 & dconcgo==0
replace dprigov=1 if edon==19 & yr==1993 & dconcgo==0
replace dprigov=1 if edon==19 & yr==1994 & dconcgo==0
replace dprigov=1 if edon==19 & yr==1995 & dconcgo==0
replace dprigov=1 if edon==19 & yr==1996 & dconcgo==0
replace dpangov=1 if edon==19 & yr==1997 & dconcgo==0
replace dpangov=1 if edon==19 & yr==1998 & dconcgo==0
replace dpangov=1 if edon==19 & yr==1999 & dconcgo==0
replace dpangov=1 if edon==19 & yr==2000 & dconcgo==0
replace dpangov=1 if edon==19 & yr==2001 & dconcgo==0
replace dpangov=1 if edon==19 & yr==2002 & dconcgo==0
replace dprigov=1 if edon==19 & yr==2003 & dconcgo==0
replace dprigov=1 if edon==19 & yr==2004 & dconcgo==0
replace dprigov=1 if edon==19 & yr==2005 & dconcgo==0
replace dprigov=1 if edon==19 & yr==2006 & dconcgo==0
replace dprigov=1 if edon==20 & yr==1988 & dconcgo==0
replace dprigov=1 if edon==20 & yr==1989 & dconcgo==0
replace dprigov=1 if edon==20 & yr==1990 & dconcgo==0
replace dprigov=1 if edon==20 & yr==1991 & dconcgo==0
replace dprigov=1 if edon==20 & yr==1992 & dconcgo==0
replace dprigov=1 if edon==20 & yr==1993 & dconcgo==0
replace dprigov=1 if edon==20 & yr==1994 & dconcgo==0
replace dprigov=1 if edon==20 & yr==1995 & dconcgo==0
replace dprigov=1 if edon==20 & yr==1996 & dconcgo==0
replace dprigov=1 if edon==20 & yr==1997 & dconcgo==0
replace dprigov=1 if edon==20 & yr==1998 & dconcgo==0
replace dprigov=1 if edon==20 & yr==1999 & dconcgo==0
replace dprigov=1 if edon==20 & yr==2000 & dconcgo==0
replace dprigov=1 if edon==20 & yr==2001 & dconcgo==0
replace dprigov=1 if edon==20 & yr==2002 & dconcgo==0
replace dprigov=1 if edon==20 & yr==2003 & dconcgo==0
replace dprigov=1 if edon==20 & yr==2004 & dconcgo==0
replace dprigov=1 if edon==20 & yr==2005 & dconcgo==0
replace dprigov=1 if edon==20 & yr==2006 & dconcgo==0
replace dprigov=1 if edon==21 & yr==1988 & dconcgo==0
replace dprigov=1 if edon==21 & yr==1989 & dconcgo==0
replace dprigov=1 if edon==21 & yr==1990 & dconcgo==0
replace dprigov=1 if edon==21 & yr==1991 & dconcgo==0
replace dprigov=1 if edon==21 & yr==1992 & dconcgo==0
replace dprigov=1 if edon==21 & yr==1993 & dconcgo==0
replace dprigov=1 if edon==21 & yr==1994 & dconcgo==0
replace dprigov=1 if edon==21 & yr==1995 & dconcgo==0
replace dprigov=1 if edon==21 & yr==1996 & dconcgo==0
replace dprigov=1 if edon==21 & yr==1997 & dconcgo==0
replace dprigov=1 if edon==21 & yr==1998 & dconcgo==0
replace dprigov=1 if edon==21 & yr==1999 & dconcgo==0
replace dprigov=1 if edon==21 & yr==2000 & dconcgo==0
replace dprigov=1 if edon==21 & yr==2001 & dconcgo==0
replace dprigov=1 if edon==21 & yr==2002 & dconcgo==0
replace dprigov=1 if edon==21 & yr==2003 & dconcgo==0
replace dprigov=1 if edon==21 & yr==2004 & dconcgo==0
replace dprigov=1 if edon==21 & yr==2005 & dconcgo==0
replace dprigov=1 if edon==21 & yr==2006 & dconcgo==0
replace dprigov=1 if edon==22 & yr==1988 & dconcgo==0
replace dprigov=1 if edon==22 & yr==1989 & dconcgo==0
replace dprigov=1 if edon==22 & yr==1990 & dconcgo==0
replace dprigov=1 if edon==22 & yr==1991 & dconcgo==0
replace dprigov=1 if edon==22 & yr==1992 & dconcgo==0
replace dprigov=1 if edon==22 & yr==1993 & dconcgo==0
replace dprigov=1 if edon==22 & yr==1994 & dconcgo==0
replace dprigov=1 if edon==22 & yr==1995 & dconcgo==0
replace dprigov=1 if edon==22 & yr==1996 & dconcgo==0
replace dpangov=1 if edon==22 & yr==1997 & dconcgo==0
replace dpangov=1 if edon==22 & yr==1998 & dconcgo==0
replace dpangov=1 if edon==22 & yr==1999 & dconcgo==0
replace dpangov=1 if edon==22 & yr==2000 & dconcgo==0
replace dpangov=1 if edon==22 & yr==2001 & dconcgo==0
replace dpangov=1 if edon==22 & yr==2002 & dconcgo==0
replace dpangov=1 if edon==22 & yr==2003 & dconcgo==0
replace dpangov=1 if edon==22 & yr==2004 & dconcgo==0
replace dpangov=1 if edon==22 & yr==2005 & dconcgo==0
replace dpangov=1 if edon==22 & yr==2006 & dconcgo==0
replace dprigov=1 if edon==23 & yr==1988 & dconcgo==0
replace dprigov=1 if edon==23 & yr==1989 & dconcgo==0
replace dprigov=1 if edon==23 & yr==1990 & dconcgo==0
replace dprigov=1 if edon==23 & yr==1991 & dconcgo==0
replace dprigov=1 if edon==23 & yr==1992 & dconcgo==0
replace dprigov=1 if edon==23 & yr==1993 & dconcgo==0
replace dprigov=1 if edon==23 & yr==1994 & dconcgo==0
replace dprigov=1 if edon==23 & yr==1995 & dconcgo==0
replace dprigov=1 if edon==23 & yr==1996 & dconcgo==0
replace dprigov=1 if edon==23 & yr==1997 & dconcgo==0
replace dprigov=1 if edon==23 & yr==1998 & dconcgo==0
replace dprigov=1 if edon==23 & yr==1999 & dconcgo==0
replace dprigov=1 if edon==23 & yr==2000 & dconcgo==0
replace dprigov=1 if edon==23 & yr==2001 & dconcgo==0
replace dprigov=1 if edon==23 & yr==2002 & dconcgo==0
replace dprigov=1 if edon==23 & yr==2003 & dconcgo==0
replace dprigov=1 if edon==23 & yr==2004 & dconcgo==0
replace dprigov=1 if edon==23 & yr==2005 & dconcgo==0
replace dprigov=1 if edon==23 & yr==2006 & dconcgo==0
replace dprigov=1 if edon==24 & yr==1988 & dconcgo==0
replace dprigov=1 if edon==24 & yr==1989 & dconcgo==0
replace dprigov=1 if edon==24 & yr==1990 & dconcgo==0
replace dprigov=1 if edon==24 & yr==1991 & dconcgo==0
replace dprigov=1 if edon==24 & yr==1992 & dconcgo==0
replace dprigov=1 if edon==24 & yr==1993 & dconcgo==0
replace dprigov=1 if edon==24 & yr==1994 & dconcgo==0
replace dprigov=1 if edon==24 & yr==1995 & dconcgo==0
replace dprigov=1 if edon==24 & yr==1996 & dconcgo==0
replace dprigov=1 if edon==24 & yr==1997 & dconcgo==0
replace dprigov=1 if edon==24 & yr==1998 & dconcgo==0
replace dprigov=1 if edon==24 & yr==1999 & dconcgo==0
replace dprigov=1 if edon==24 & yr==2000 & dconcgo==0
replace dprigov=1 if edon==24 & yr==2001 & dconcgo==0
replace dprigov=1 if edon==24 & yr==2002 & dconcgo==0
replace dpangov=1 if edon==24 & yr==2003 & dconcgo==0
replace dpangov=1 if edon==24 & yr==2004 & dconcgo==0
replace dpangov=1 if edon==24 & yr==2005 & dconcgo==0
replace dpangov=1 if edon==24 & yr==2006 & dconcgo==0
replace dprigov=1 if edon==25 & yr==1988 & dconcgo==0
replace dprigov=1 if edon==25 & yr==1989 & dconcgo==0
replace dprigov=1 if edon==25 & yr==1990 & dconcgo==0
replace dprigov=1 if edon==25 & yr==1991 & dconcgo==0
replace dprigov=1 if edon==25 & yr==1992 & dconcgo==0
replace dprigov=1 if edon==25 & yr==1993 & dconcgo==0
replace dprigov=1 if edon==25 & yr==1994 & dconcgo==0
replace dprigov=1 if edon==25 & yr==1995 & dconcgo==0
replace dprigov=1 if edon==25 & yr==1996 & dconcgo==0
replace dprigov=1 if edon==25 & yr==1997 & dconcgo==0
replace dprigov=1 if edon==25 & yr==1998 & dconcgo==0
replace dprigov=1 if edon==25 & yr==1999 & dconcgo==0
replace dprigov=1 if edon==25 & yr==2000 & dconcgo==0
replace dprigov=1 if edon==25 & yr==2001 & dconcgo==0
replace dprigov=1 if edon==25 & yr==2002 & dconcgo==0
replace dprigov=1 if edon==25 & yr==2003 & dconcgo==0
replace dprigov=1 if edon==25 & yr==2004 & dconcgo==0
replace dprigov=1 if edon==25 & yr==2005 & dconcgo==0
replace dprigov=1 if edon==25 & yr==2006 & dconcgo==0
replace dprigov=1 if edon==26 & yr==1988 & dconcgo==0
replace dprigov=1 if edon==26 & yr==1989 & dconcgo==0
replace dprigov=1 if edon==26 & yr==1990 & dconcgo==0
replace dprigov=1 if edon==26 & yr==1991 & dconcgo==0
replace dprigov=1 if edon==26 & yr==1992 & dconcgo==0
replace dprigov=1 if edon==26 & yr==1993 & dconcgo==0
replace dprigov=1 if edon==26 & yr==1994 & dconcgo==0
replace dprigov=1 if edon==26 & yr==1995 & dconcgo==0
replace dprigov=1 if edon==26 & yr==1996 & dconcgo==0
replace dprigov=1 if edon==26 & yr==1997 & dconcgo==0
replace dprigov=1 if edon==26 & yr==1998 & dconcgo==0
replace dprigov=1 if edon==26 & yr==1999 & dconcgo==0
replace dprigov=1 if edon==26 & yr==2000 & dconcgo==0
replace dprigov=1 if edon==26 & yr==2001 & dconcgo==0
replace dprigov=1 if edon==26 & yr==2002 & dconcgo==0
replace dprigov=1 if edon==26 & yr==2003 & dconcgo==0
replace dprigov=1 if edon==26 & yr==2004 & dconcgo==0
replace dprigov=1 if edon==26 & yr==2005 & dconcgo==0
replace dprigov=1 if edon==26 & yr==2006 & dconcgo==0
replace dprigov=1 if edon==27 & yr==1988 & dconcgo==0
replace dprigov=1 if edon==27 & yr==1989 & dconcgo==0
replace dprigov=1 if edon==27 & yr==1990 & dconcgo==0
replace dprigov=1 if edon==27 & yr==1991 & dconcgo==0
replace dprigov=1 if edon==27 & yr==1992 & dconcgo==0
replace dprigov=1 if edon==27 & yr==1993 & dconcgo==0
replace dprigov=1 if edon==27 & yr==1994 & dconcgo==0
replace dprigov=1 if edon==27 & yr==1995 & dconcgo==0
replace dprigov=1 if edon==27 & yr==1996 & dconcgo==0
replace dprigov=1 if edon==27 & yr==1997 & dconcgo==0
replace dprigov=1 if edon==27 & yr==1998 & dconcgo==0
replace dprigov=1 if edon==27 & yr==1999 & dconcgo==0
replace dprigov=1 if edon==27 & yr==2000 & dconcgo==0
replace dprigov=1 if edon==27 & yr==2001 & dconcgo==0
replace dprigov=1 if edon==27 & yr==2002 & dconcgo==0
replace dprigov=1 if edon==27 & yr==2003 & dconcgo==0
replace dprigov=1 if edon==27 & yr==2004 & dconcgo==0
replace dprigov=1 if edon==27 & yr==2005 & dconcgo==0
replace dprigov=1 if edon==27 & yr==2006 & dconcgo==0
replace dprigov=1 if edon==28 & yr==1988 & dconcgo==0
replace dprigov=1 if edon==28 & yr==1989 & dconcgo==0
replace dprigov=1 if edon==28 & yr==1990 & dconcgo==0
replace dprigov=1 if edon==28 & yr==1991 & dconcgo==0
replace dprigov=1 if edon==28 & yr==1992 & dconcgo==0
replace dprigov=1 if edon==28 & yr==1993 & dconcgo==0
replace dprigov=1 if edon==28 & yr==1994 & dconcgo==0
replace dprigov=1 if edon==28 & yr==1995 & dconcgo==0
replace dprigov=1 if edon==28 & yr==1996 & dconcgo==0
replace dprigov=1 if edon==28 & yr==1997 & dconcgo==0
replace dprigov=1 if edon==28 & yr==1998 & dconcgo==0
replace dprigov=1 if edon==28 & yr==1999 & dconcgo==0
replace dprigov=1 if edon==28 & yr==2000 & dconcgo==0
replace dprigov=1 if edon==28 & yr==2001 & dconcgo==0
replace dprigov=1 if edon==28 & yr==2002 & dconcgo==0
replace dprigov=1 if edon==28 & yr==2003 & dconcgo==0
replace dprigov=1 if edon==28 & yr==2004 & dconcgo==0
replace dprigov=1 if edon==28 & yr==2005 & dconcgo==0
replace dprigov=1 if edon==28 & yr==2006 & dconcgo==0
replace dprigov=1 if edon==29 & yr==1988 & dconcgo==0
replace dprigov=1 if edon==29 & yr==1989 & dconcgo==0
replace dprigov=1 if edon==29 & yr==1990 & dconcgo==0
replace dprigov=1 if edon==29 & yr==1991 & dconcgo==0
replace dprigov=1 if edon==29 & yr==1992 & dconcgo==0
replace dprigov=1 if edon==29 & yr==1993 & dconcgo==0
replace dprigov=1 if edon==29 & yr==1994 & dconcgo==0
replace dprigov=1 if edon==29 & yr==1995 & dconcgo==0
replace dprigov=1 if edon==29 & yr==1996 & dconcgo==0
replace dprigov=1 if edon==29 & yr==1997 & dconcgo==0
replace dprigov=1 if edon==29 & yr==1998 & dconcgo==0
replace dprdgov=1 if edon==29 & yr==1999 & dconcgo==0
replace dprdgov=1 if edon==29 & yr==2000 & dconcgo==0
replace dprdgov=1 if edon==29 & yr==2001 & dconcgo==0
replace dprdgov=1 if edon==29 & yr==2002 & dconcgo==0
replace dprdgov=1 if edon==29 & yr==2003 & dconcgo==0
replace dprdgov=1 if edon==29 & yr==2004 & dconcgo==0
replace dpangov=1 if edon==29 & yr==2005 & dconcgo==0
replace dpangov=1 if edon==29 & yr==2006 & dconcgo==0
replace dprigov=1 if edon==30 & yr==1988 & dconcgo==0
replace dprigov=1 if edon==30 & yr==1989 & dconcgo==0
replace dprigov=1 if edon==30 & yr==1990 & dconcgo==0
replace dprigov=1 if edon==30 & yr==1991 & dconcgo==0
replace dprigov=1 if edon==30 & yr==1992 & dconcgo==0
replace dprigov=1 if edon==30 & yr==1993 & dconcgo==0
replace dprigov=1 if edon==30 & yr==1994 & dconcgo==0
replace dprigov=1 if edon==30 & yr==1995 & dconcgo==0
replace dprigov=1 if edon==30 & yr==1996 & dconcgo==0
replace dprigov=1 if edon==30 & yr==1997 & dconcgo==0
replace dprigov=1 if edon==30 & yr==1998 & dconcgo==0
replace dprigov=1 if edon==30 & yr==1999 & dconcgo==0
replace dprigov=1 if edon==30 & yr==2000 & dconcgo==0
replace dprigov=1 if edon==30 & yr==2001 & dconcgo==0
replace dprigov=1 if edon==30 & yr==2002 & dconcgo==0
replace dprigov=1 if edon==30 & yr==2003 & dconcgo==0
replace dprigov=1 if edon==30 & yr==2004 & dconcgo==0
replace dprigov=1 if edon==30 & yr==2005 & dconcgo==0
replace dprigov=1 if edon==30 & yr==2006 & dconcgo==0
replace dprigov=1 if edon==31 & yr==1988 & dconcgo==0
replace dprigov=1 if edon==31 & yr==1989 & dconcgo==0
replace dprigov=1 if edon==31 & yr==1990 & dconcgo==0
replace dprigov=1 if edon==31 & yr==1991 & dconcgo==0
replace dprigov=1 if edon==31 & yr==1992 & dconcgo==0
replace dprigov=1 if edon==31 & yr==1993 & dconcgo==0
replace dprigov=1 if edon==31 & yr==1994 & dconcgo==0
replace dprigov=1 if edon==31 & yr==1995 & dconcgo==0
replace dprigov=1 if edon==31 & yr==1996 & dconcgo==0
replace dprigov=1 if edon==31 & yr==1997 & dconcgo==0
replace dprigov=1 if edon==31 & yr==1998 & dconcgo==0
replace dprigov=1 if edon==31 & yr==1999 & dconcgo==0
replace dprigov=1 if edon==31 & yr==2000 & dconcgo==0
replace dpangov=1 if edon==31 & yr==2001 & dconcgo==0
replace dpangov=1 if edon==31 & yr==2002 & dconcgo==0
replace dpangov=1 if edon==31 & yr==2003 & dconcgo==0
replace dpangov=1 if edon==31 & yr==2004 & dconcgo==0
replace dpangov=1 if edon==31 & yr==2005 & dconcgo==0
replace dpangov=1 if edon==31 & yr==2006 & dconcgo==0
replace dprigov=1 if edon==32 & yr==1988 & dconcgo==0
replace dprigov=1 if edon==32 & yr==1989 & dconcgo==0
replace dprigov=1 if edon==32 & yr==1990 & dconcgo==0
replace dprigov=1 if edon==32 & yr==1991 & dconcgo==0
replace dprigov=1 if edon==32 & yr==1992 & dconcgo==0
replace dprigov=1 if edon==32 & yr==1993 & dconcgo==0
replace dprigov=1 if edon==32 & yr==1994 & dconcgo==0
replace dprigov=1 if edon==32 & yr==1995 & dconcgo==0
replace dprigov=1 if edon==32 & yr==1996 & dconcgo==0
replace dprigov=1 if edon==32 & yr==1997 & dconcgo==0
replace dprdgov=1 if edon==32 & yr==1998 & dconcgo==0
replace dprdgov=1 if edon==32 & yr==1999 & dconcgo==0
replace dprdgov=1 if edon==32 & yr==2000 & dconcgo==0
replace dprdgov=1 if edon==32 & yr==2001 & dconcgo==0
replace dprdgov=1 if edon==32 & yr==2002 & dconcgo==0
replace dprdgov=1 if edon==32 & yr==2003 & dconcgo==0
replace dprdgov=1 if edon==32 & yr==2004 & dconcgo==0
replace dprdgov=1 if edon==32 & yr==2005 & dconcgo==0
replace dprdgov=1 if edon==32 & yr==2006 & dconcgo==0
replace dprigov=1 if edon==1 & yr==1989 & dconcgo==1
replace dprigov=1 if edon==1 & yr==1990 & dconcgo==1
replace dprigov=1 if edon==1 & yr==1991 & dconcgo==1
replace dprigov=1 if edon==1 & yr==1992 & dconcgo==1
replace dprigov=1 if edon==1 & yr==1993 & dconcgo==1
replace dprigov=1 if edon==1 & yr==1994 & dconcgo==1
replace dprigov=1 if edon==1 & yr==1995 & dconcgo==1
replace dprigov=1 if edon==1 & yr==1996 & dconcgo==1
replace dprigov=1 if edon==1 & yr==1997 & dconcgo==1
replace dprigov=1 if edon==1 & yr==1998 & dconcgo==1
replace dpangov=1 if edon==1 & yr==1999 & dconcgo==1
replace dpangov=1 if edon==1 & yr==2000 & dconcgo==1
replace dpangov=1 if edon==1 & yr==2001 & dconcgo==1
replace dpangov=1 if edon==1 & yr==2002 & dconcgo==1
replace dpangov=1 if edon==1 & yr==2003 & dconcgo==1
replace dpangov=1 if edon==1 & yr==2004 & dconcgo==1
replace dpangov=1 if edon==1 & yr==2005 & dconcgo==1
replace dpangov=1 if edon==1 & yr==2006 & dconcgo==1
replace dprigov=1 if edon==2 & yr==1989 & dconcgo==1
replace dpangov=1 if edon==2 & yr==1990 & dconcgo==1
replace dpangov=1 if edon==2 & yr==1991 & dconcgo==1
replace dpangov=1 if edon==2 & yr==1992 & dconcgo==1
replace dpangov=1 if edon==2 & yr==1993 & dconcgo==1
replace dpangov=1 if edon==2 & yr==1994 & dconcgo==1
replace dpangov=1 if edon==2 & yr==1995 & dconcgo==1
replace dpangov=1 if edon==2 & yr==1996 & dconcgo==1
replace dpangov=1 if edon==2 & yr==1997 & dconcgo==1
replace dpangov=1 if edon==2 & yr==1998 & dconcgo==1
replace dpangov=1 if edon==2 & yr==1999 & dconcgo==1
replace dpangov=1 if edon==2 & yr==2000 & dconcgo==1
replace dpangov=1 if edon==2 & yr==2001 & dconcgo==1
replace dpangov=1 if edon==2 & yr==2002 & dconcgo==1
replace dpangov=1 if edon==2 & yr==2003 & dconcgo==1
replace dpangov=1 if edon==2 & yr==2004 & dconcgo==1
replace dpangov=1 if edon==2 & yr==2005 & dconcgo==1
replace dpangov=1 if edon==2 & yr==2006 & dconcgo==1
replace dprigov=1 if edon==3 & yr==1989 & dconcgo==1
replace dprigov=1 if edon==3 & yr==1990 & dconcgo==1
replace dprigov=1 if edon==3 & yr==1991 & dconcgo==1
replace dprigov=1 if edon==3 & yr==1992 & dconcgo==1
replace dprigov=1 if edon==3 & yr==1993 & dconcgo==1
replace dprigov=1 if edon==3 & yr==1994 & dconcgo==1
replace dprigov=1 if edon==3 & yr==1995 & dconcgo==1
replace dprigov=1 if edon==3 & yr==1996 & dconcgo==1
replace dprigov=1 if edon==3 & yr==1997 & dconcgo==1
replace dprigov=1 if edon==3 & yr==1998 & dconcgo==1
replace dprigov=1 if edon==3 & yr==1999 & dconcgo==1
replace dprdgov=1 if edon==3 & yr==2000 & dconcgo==1
replace dprdgov=1 if edon==3 & yr==2001 & dconcgo==1
replace dprdgov=1 if edon==3 & yr==2002 & dconcgo==1
replace dprdgov=1 if edon==3 & yr==2003 & dconcgo==1
replace dprdgov=1 if edon==3 & yr==2004 & dconcgo==1
replace dprdgov=1 if edon==3 & yr==2005 & dconcgo==1
replace dprdgov=1 if edon==3 & yr==2006 & dconcgo==1
replace dprigov=1 if edon==4 & yr==1989 & dconcgo==1
replace dprigov=1 if edon==4 & yr==1990 & dconcgo==1
replace dprigov=1 if edon==4 & yr==1991 & dconcgo==1
replace dprigov=1 if edon==4 & yr==1992 & dconcgo==1
replace dprigov=1 if edon==4 & yr==1993 & dconcgo==1
replace dprigov=1 if edon==4 & yr==1994 & dconcgo==1
replace dprigov=1 if edon==4 & yr==1995 & dconcgo==1
replace dprigov=1 if edon==4 & yr==1996 & dconcgo==1
replace dprigov=1 if edon==4 & yr==1997 & dconcgo==1
replace dprigov=1 if edon==4 & yr==1998 & dconcgo==1
replace dprigov=1 if edon==4 & yr==1999 & dconcgo==1
replace dprigov=1 if edon==4 & yr==2000 & dconcgo==1
replace dprigov=1 if edon==4 & yr==2001 & dconcgo==1
replace dprigov=1 if edon==4 & yr==2002 & dconcgo==1
replace dprigov=1 if edon==4 & yr==2003 & dconcgo==1
replace dprigov=1 if edon==4 & yr==2004 & dconcgo==1
replace dprigov=1 if edon==4 & yr==2005 & dconcgo==1
replace dprigov=1 if edon==4 & yr==2006 & dconcgo==1
replace dprigov=1 if edon==5 & yr==1989 & dconcgo==1
replace dprigov=1 if edon==5 & yr==1990 & dconcgo==1
replace dprigov=1 if edon==5 & yr==1991 & dconcgo==1
replace dprigov=1 if edon==5 & yr==1992 & dconcgo==1
replace dprigov=1 if edon==5 & yr==1993 & dconcgo==1
replace dprigov=1 if edon==5 & yr==1994 & dconcgo==1
replace dprigov=1 if edon==5 & yr==1995 & dconcgo==1
replace dprigov=1 if edon==5 & yr==1996 & dconcgo==1
replace dprigov=1 if edon==5 & yr==1997 & dconcgo==1
replace dprigov=1 if edon==5 & yr==1998 & dconcgo==1
replace dprigov=1 if edon==5 & yr==1999 & dconcgo==1
replace dprigov=1 if edon==5 & yr==2000 & dconcgo==1
replace dprigov=1 if edon==5 & yr==2001 & dconcgo==1
replace dprigov=1 if edon==5 & yr==2002 & dconcgo==1
replace dprigov=1 if edon==5 & yr==2003 & dconcgo==1
replace dprigov=1 if edon==5 & yr==2004 & dconcgo==1
replace dprigov=1 if edon==5 & yr==2005 & dconcgo==1
replace dprigov=1 if edon==5 & yr==2006 & dconcgo==1
replace dprigov=1 if edon==6 & yr==1989 & dconcgo==1
replace dprigov=1 if edon==6 & yr==1990 & dconcgo==1
replace dprigov=1 if edon==6 & yr==1991 & dconcgo==1
replace dprigov=1 if edon==6 & yr==1992 & dconcgo==1
replace dprigov=1 if edon==6 & yr==1993 & dconcgo==1
replace dprigov=1 if edon==6 & yr==1994 & dconcgo==1
replace dprigov=1 if edon==6 & yr==1995 & dconcgo==1
replace dprigov=1 if edon==6 & yr==1996 & dconcgo==1
replace dprigov=1 if edon==6 & yr==1997 & dconcgo==1
replace dprigov=1 if edon==6 & yr==1998 & dconcgo==1
replace dprigov=1 if edon==6 & yr==1999 & dconcgo==1
replace dprigov=1 if edon==6 & yr==2000 & dconcgo==1
replace dprigov=1 if edon==6 & yr==2001 & dconcgo==1
replace dprigov=1 if edon==6 & yr==2002 & dconcgo==1
replace dprigov=1 if edon==6 & yr==2003 & dconcgo==1
replace dprigov=1 if edon==6 & yr==2004 & dconcgo==1
replace dprigov=1 if edon==6 & yr==2005 & dconcgo==1
replace dprigov=1 if edon==6 & yr==2006 & dconcgo==1
replace dprigov=1 if edon==7 & yr==1989 & dconcgo==1
replace dprigov=1 if edon==7 & yr==1990 & dconcgo==1
replace dprigov=1 if edon==7 & yr==1991 & dconcgo==1
replace dprigov=1 if edon==7 & yr==1992 & dconcgo==1
replace dprigov=1 if edon==7 & yr==1993 & dconcgo==1
replace dprigov=1 if edon==7 & yr==1994 & dconcgo==1
replace dprigov=1 if edon==7 & yr==1995 & dconcgo==1
replace dprigov=1 if edon==7 & yr==1996 & dconcgo==1
replace dprigov=1 if edon==7 & yr==1997 & dconcgo==1
replace dprigov=1 if edon==7 & yr==1998 & dconcgo==1
replace dprigov=1 if edon==7 & yr==1999 & dconcgo==1
replace dprigov=1 if edon==7 & yr==2000 & dconcgo==1
replace dprdgov=1 if edon==7 & yr==2001 & dconcgo==1
replace dprdgov=1 if edon==7 & yr==2002 & dconcgo==1
replace dprdgov=1 if edon==7 & yr==2003 & dconcgo==1
replace dprdgov=1 if edon==7 & yr==2004 & dconcgo==1
replace dprdgov=1 if edon==7 & yr==2005 & dconcgo==1
replace dprdgov=1 if edon==7 & yr==2006 & dconcgo==1
replace dprigov=1 if edon==8 & yr==1989 & dconcgo==1
replace dprigov=1 if edon==8 & yr==1990 & dconcgo==1
replace dprigov=1 if edon==8 & yr==1991 & dconcgo==1
replace dprigov=1 if edon==8 & yr==1992 & dconcgo==1
replace dpangov=1 if edon==8 & yr==1993 & dconcgo==1
replace dpangov=1 if edon==8 & yr==1994 & dconcgo==1
replace dpangov=1 if edon==8 & yr==1995 & dconcgo==1
replace dpangov=1 if edon==8 & yr==1996 & dconcgo==1
replace dpangov=1 if edon==8 & yr==1997 & dconcgo==1
replace dpangov=1 if edon==8 & yr==1998 & dconcgo==1
replace dprigov=1 if edon==8 & yr==1999 & dconcgo==1
replace dprigov=1 if edon==8 & yr==2000 & dconcgo==1
replace dprigov=1 if edon==8 & yr==2001 & dconcgo==1
replace dprigov=1 if edon==8 & yr==2002 & dconcgo==1
replace dprigov=1 if edon==8 & yr==2003 & dconcgo==1
replace dprigov=1 if edon==8 & yr==2004 & dconcgo==1
replace dprigov=1 if edon==8 & yr==2005 & dconcgo==1
replace dprigov=1 if edon==8 & yr==2006 & dconcgo==1
replace dprigov=1 if edon==9 & yr==1989 & dconcgo==1
replace dprigov=1 if edon==9 & yr==1990 & dconcgo==1
replace dprigov=1 if edon==9 & yr==1991 & dconcgo==1
replace dprigov=1 if edon==9 & yr==1992 & dconcgo==1
replace dprigov=1 if edon==9 & yr==1993 & dconcgo==1
replace dprigov=1 if edon==9 & yr==1994 & dconcgo==1
replace dprigov=1 if edon==9 & yr==1995 & dconcgo==1
replace dprigov=1 if edon==9 & yr==1996 & dconcgo==1
replace dprigov=1 if edon==9 & yr==1997 & dconcgo==1
replace dprdgov=1 if edon==9 & yr==1998 & dconcgo==1
replace dprdgov=1 if edon==9 & yr==1999 & dconcgo==1
replace dprdgov=1 if edon==9 & yr==2000 & dconcgo==1
replace dprdgov=1 if edon==9 & yr==2001 & dconcgo==1
replace dprdgov=1 if edon==9 & yr==2002 & dconcgo==1
replace dprdgov=1 if edon==9 & yr==2003 & dconcgo==1
replace dprdgov=1 if edon==9 & yr==2004 & dconcgo==1
replace dprdgov=1 if edon==9 & yr==2005 & dconcgo==1
replace dprdgov=1 if edon==9 & yr==2006 & dconcgo==1
replace dprigov=1 if edon==10 & yr==1989 & dconcgo==1
replace dprigov=1 if edon==10 & yr==1990 & dconcgo==1
replace dprigov=1 if edon==10 & yr==1991 & dconcgo==1
replace dprigov=1 if edon==10 & yr==1992 & dconcgo==1
replace dprigov=1 if edon==10 & yr==1993 & dconcgo==1
replace dprigov=1 if edon==10 & yr==1994 & dconcgo==1
replace dprigov=1 if edon==10 & yr==1995 & dconcgo==1
replace dprigov=1 if edon==10 & yr==1996 & dconcgo==1
replace dprigov=1 if edon==10 & yr==1997 & dconcgo==1
replace dprigov=1 if edon==10 & yr==1998 & dconcgo==1
replace dprigov=1 if edon==10 & yr==1999 & dconcgo==1
replace dprigov=1 if edon==10 & yr==2000 & dconcgo==1
replace dprigov=1 if edon==10 & yr==2001 & dconcgo==1
replace dprigov=1 if edon==10 & yr==2002 & dconcgo==1
replace dprigov=1 if edon==10 & yr==2003 & dconcgo==1
replace dprigov=1 if edon==10 & yr==2004 & dconcgo==1
replace dprigov=1 if edon==10 & yr==2005 & dconcgo==1
replace dprigov=1 if edon==10 & yr==2006 & dconcgo==1
replace dprigov=1 if edon==11 & yr==1989 & dconcgo==1
replace dprigov=1 if edon==11 & yr==1990 & dconcgo==1
replace dprigov=1 if edon==11 & yr==1991 & dconcgo==1
replace dpangov=1 if edon==11 & yr==1992 & dconcgo==1
replace dpangov=1 if edon==11 & yr==1993 & dconcgo==1
replace dpangov=1 if edon==11 & yr==1994 & dconcgo==1
replace dpangov=1 if edon==11 & yr==1995 & dconcgo==1
replace dpangov=1 if edon==11 & yr==1996 & dconcgo==1
replace dpangov=1 if edon==11 & yr==1997 & dconcgo==1
replace dpangov=1 if edon==11 & yr==1998 & dconcgo==1
replace dpangov=1 if edon==11 & yr==1999 & dconcgo==1
replace dpangov=1 if edon==11 & yr==2000 & dconcgo==1
replace dpangov=1 if edon==11 & yr==2001 & dconcgo==1
replace dpangov=1 if edon==11 & yr==2002 & dconcgo==1
replace dpangov=1 if edon==11 & yr==2003 & dconcgo==1
replace dpangov=1 if edon==11 & yr==2004 & dconcgo==1
replace dpangov=1 if edon==11 & yr==2005 & dconcgo==1
replace dpangov=1 if edon==11 & yr==2006 & dconcgo==1
replace dprigov=1 if edon==12 & yr==1989 & dconcgo==1
replace dprigov=1 if edon==12 & yr==1990 & dconcgo==1
replace dprigov=1 if edon==12 & yr==1991 & dconcgo==1
replace dprigov=1 if edon==12 & yr==1992 & dconcgo==1
replace dprigov=1 if edon==12 & yr==1993 & dconcgo==1
replace dprigov=1 if edon==12 & yr==1994 & dconcgo==1
replace dprigov=1 if edon==12 & yr==1995 & dconcgo==1
replace dprigov=1 if edon==12 & yr==1996 & dconcgo==1
replace dprigov=1 if edon==12 & yr==1997 & dconcgo==1
replace dprigov=1 if edon==12 & yr==1998 & dconcgo==1
replace dprigov=1 if edon==12 & yr==1999 & dconcgo==1
replace dprigov=1 if edon==12 & yr==2000 & dconcgo==1
replace dprigov=1 if edon==12 & yr==2001 & dconcgo==1
replace dprigov=1 if edon==12 & yr==2002 & dconcgo==1
replace dprigov=1 if edon==12 & yr==2003 & dconcgo==1
replace dprigov=1 if edon==12 & yr==2004 & dconcgo==1
replace dprigov=1 if edon==12 & yr==2005 & dconcgo==1
replace dprdgov=1 if edon==12 & yr==2006 & dconcgo==1
replace dprigov=1 if edon==13 & yr==1989 & dconcgo==1
replace dprigov=1 if edon==13 & yr==1990 & dconcgo==1
replace dprigov=1 if edon==13 & yr==1991 & dconcgo==1
replace dprigov=1 if edon==13 & yr==1992 & dconcgo==1
replace dprigov=1 if edon==13 & yr==1993 & dconcgo==1
replace dprigov=1 if edon==13 & yr==1994 & dconcgo==1
replace dprigov=1 if edon==13 & yr==1995 & dconcgo==1
replace dprigov=1 if edon==13 & yr==1996 & dconcgo==1
replace dprigov=1 if edon==13 & yr==1997 & dconcgo==1
replace dprigov=1 if edon==13 & yr==1998 & dconcgo==1
replace dprigov=1 if edon==13 & yr==1999 & dconcgo==1
replace dprigov=1 if edon==13 & yr==2000 & dconcgo==1
replace dprigov=1 if edon==13 & yr==2001 & dconcgo==1
replace dprigov=1 if edon==13 & yr==2002 & dconcgo==1
replace dprigov=1 if edon==13 & yr==2003 & dconcgo==1
replace dprigov=1 if edon==13 & yr==2004 & dconcgo==1
replace dprigov=1 if edon==13 & yr==2005 & dconcgo==1
replace dprigov=1 if edon==13 & yr==2006 & dconcgo==1
replace dprigov=1 if edon==14 & yr==1989 & dconcgo==1
replace dprigov=1 if edon==14 & yr==1990 & dconcgo==1
replace dprigov=1 if edon==14 & yr==1991 & dconcgo==1
replace dprigov=1 if edon==14 & yr==1992 & dconcgo==1
replace dprigov=1 if edon==14 & yr==1993 & dconcgo==1
replace dprigov=1 if edon==14 & yr==1994 & dconcgo==1
replace dprigov=1 if edon==14 & yr==1995 & dconcgo==1
replace dpangov=1 if edon==14 & yr==1996 & dconcgo==1
replace dpangov=1 if edon==14 & yr==1997 & dconcgo==1
replace dpangov=1 if edon==14 & yr==1998 & dconcgo==1
replace dpangov=1 if edon==14 & yr==1999 & dconcgo==1
replace dpangov=1 if edon==14 & yr==2000 & dconcgo==1
replace dpangov=1 if edon==14 & yr==2001 & dconcgo==1
replace dpangov=1 if edon==14 & yr==2002 & dconcgo==1
replace dpangov=1 if edon==14 & yr==2003 & dconcgo==1
replace dpangov=1 if edon==14 & yr==2004 & dconcgo==1
replace dpangov=1 if edon==14 & yr==2005 & dconcgo==1
replace dpangov=1 if edon==14 & yr==2006 & dconcgo==1
replace dprigov=1 if edon==15 & yr==1989 & dconcgo==1
replace dprigov=1 if edon==15 & yr==1990 & dconcgo==1
replace dprigov=1 if edon==15 & yr==1991 & dconcgo==1
replace dprigov=1 if edon==15 & yr==1992 & dconcgo==1
replace dprigov=1 if edon==15 & yr==1993 & dconcgo==1
replace dprigov=1 if edon==15 & yr==1994 & dconcgo==1
replace dprigov=1 if edon==15 & yr==1995 & dconcgo==1
replace dprigov=1 if edon==15 & yr==1996 & dconcgo==1
replace dprigov=1 if edon==15 & yr==1997 & dconcgo==1
replace dprigov=1 if edon==15 & yr==1998 & dconcgo==1
replace dprigov=1 if edon==15 & yr==1999 & dconcgo==1
replace dprigov=1 if edon==15 & yr==2000 & dconcgo==1
replace dprigov=1 if edon==15 & yr==2001 & dconcgo==1
replace dprigov=1 if edon==15 & yr==2002 & dconcgo==1
replace dprigov=1 if edon==15 & yr==2003 & dconcgo==1
replace dprigov=1 if edon==15 & yr==2004 & dconcgo==1
replace dprigov=1 if edon==15 & yr==2005 & dconcgo==1
replace dprigov=1 if edon==15 & yr==2006 & dconcgo==1
replace dprigov=1 if edon==16 & yr==1989 & dconcgo==1
replace dprigov=1 if edon==16 & yr==1990 & dconcgo==1
replace dprigov=1 if edon==16 & yr==1991 & dconcgo==1
replace dprigov=1 if edon==16 & yr==1992 & dconcgo==1
replace dprigov=1 if edon==16 & yr==1993 & dconcgo==1
replace dprigov=1 if edon==16 & yr==1994 & dconcgo==1
replace dprigov=1 if edon==16 & yr==1995 & dconcgo==1
replace dprigov=1 if edon==16 & yr==1996 & dconcgo==1
replace dprigov=1 if edon==16 & yr==1997 & dconcgo==1
replace dprigov=1 if edon==16 & yr==1998 & dconcgo==1
replace dprigov=1 if edon==16 & yr==1999 & dconcgo==1
replace dprigov=1 if edon==16 & yr==2000 & dconcgo==1
replace dprigov=1 if edon==16 & yr==2001 & dconcgo==1
replace dprdgov=1 if edon==16 & yr==2002 & dconcgo==1
replace dprdgov=1 if edon==16 & yr==2003 & dconcgo==1
replace dprdgov=1 if edon==16 & yr==2004 & dconcgo==1
replace dprdgov=1 if edon==16 & yr==2005 & dconcgo==1
replace dprdgov=1 if edon==16 & yr==2006 & dconcgo==1
replace dprigov=1 if edon==17 & yr==1989 & dconcgo==1
replace dprigov=1 if edon==17 & yr==1990 & dconcgo==1
replace dprigov=1 if edon==17 & yr==1991 & dconcgo==1
replace dprigov=1 if edon==17 & yr==1992 & dconcgo==1
replace dprigov=1 if edon==17 & yr==1993 & dconcgo==1
replace dprigov=1 if edon==17 & yr==1994 & dconcgo==1
replace dprigov=1 if edon==17 & yr==1995 & dconcgo==1
replace dprigov=1 if edon==17 & yr==1996 & dconcgo==1
replace dprigov=1 if edon==17 & yr==1997 & dconcgo==1
replace dprigov=1 if edon==17 & yr==1998 & dconcgo==1
replace dprigov=1 if edon==17 & yr==1999 & dconcgo==1
replace dprigov=1 if edon==17 & yr==2000 & dconcgo==1
replace dpangov=1 if edon==17 & yr==2001 & dconcgo==1
replace dpangov=1 if edon==17 & yr==2002 & dconcgo==1
replace dpangov=1 if edon==17 & yr==2003 & dconcgo==1
replace dpangov=1 if edon==17 & yr==2004 & dconcgo==1
replace dpangov=1 if edon==17 & yr==2005 & dconcgo==1
replace dpangov=1 if edon==17 & yr==2006 & dconcgo==1
replace dprigov=1 if edon==18 & yr==1989 & dconcgo==1
replace dprigov=1 if edon==18 & yr==1990 & dconcgo==1
replace dprigov=1 if edon==18 & yr==1991 & dconcgo==1
replace dprigov=1 if edon==18 & yr==1992 & dconcgo==1
replace dprigov=1 if edon==18 & yr==1993 & dconcgo==1
replace dprigov=1 if edon==18 & yr==1994 & dconcgo==1
replace dprigov=1 if edon==18 & yr==1995 & dconcgo==1
replace dprigov=1 if edon==18 & yr==1996 & dconcgo==1
replace dprigov=1 if edon==18 & yr==1997 & dconcgo==1
replace dprigov=1 if edon==18 & yr==1998 & dconcgo==1
replace dprigov=1 if edon==18 & yr==1999 & dconcgo==1
replace dpangov=1 if edon==18 & yr==2000 & dconcgo==1
replace dpangov=1 if edon==18 & yr==2001 & dconcgo==1
replace dpangov=1 if edon==18 & yr==2002 & dconcgo==1
replace dpangov=1 if edon==18 & yr==2003 & dconcgo==1
replace dpangov=1 if edon==18 & yr==2004 & dconcgo==1
replace dpangov=1 if edon==18 & yr==2005 & dconcgo==1
replace dprigov=1 if edon==18 & yr==2006 & dconcgo==1
replace dprigov=1 if edon==19 & yr==1989 & dconcgo==1
replace dprigov=1 if edon==19 & yr==1990 & dconcgo==1
replace dprigov=1 if edon==19 & yr==1991 & dconcgo==1
replace dprigov=1 if edon==19 & yr==1992 & dconcgo==1
replace dprigov=1 if edon==19 & yr==1993 & dconcgo==1
replace dprigov=1 if edon==19 & yr==1994 & dconcgo==1
replace dprigov=1 if edon==19 & yr==1995 & dconcgo==1
replace dprigov=1 if edon==19 & yr==1996 & dconcgo==1
replace dprigov=1 if edon==19 & yr==1997 & dconcgo==1
replace dpangov=1 if edon==19 & yr==1998 & dconcgo==1
replace dpangov=1 if edon==19 & yr==1999 & dconcgo==1
replace dpangov=1 if edon==19 & yr==2000 & dconcgo==1
replace dpangov=1 if edon==19 & yr==2001 & dconcgo==1
replace dpangov=1 if edon==19 & yr==2002 & dconcgo==1
replace dpangov=1 if edon==19 & yr==2003 & dconcgo==1
replace dprigov=1 if edon==19 & yr==2004 & dconcgo==1
replace dprigov=1 if edon==19 & yr==2005 & dconcgo==1
replace dprigov=1 if edon==19 & yr==2006 & dconcgo==1
replace dprigov=1 if edon==20 & yr==1989 & dconcgo==1
replace dprigov=1 if edon==20 & yr==1990 & dconcgo==1
replace dprigov=1 if edon==20 & yr==1991 & dconcgo==1
replace dprigov=1 if edon==20 & yr==1992 & dconcgo==1
replace dprigov=1 if edon==20 & yr==1993 & dconcgo==1
replace dprigov=1 if edon==20 & yr==1994 & dconcgo==1
replace dprigov=1 if edon==20 & yr==1995 & dconcgo==1
replace dprigov=1 if edon==20 & yr==1996 & dconcgo==1
replace dprigov=1 if edon==20 & yr==1997 & dconcgo==1
replace dprigov=1 if edon==20 & yr==1998 & dconcgo==1
replace dprigov=1 if edon==20 & yr==1999 & dconcgo==1
replace dprigov=1 if edon==20 & yr==2000 & dconcgo==1
replace dprigov=1 if edon==20 & yr==2001 & dconcgo==1
replace dprigov=1 if edon==20 & yr==2002 & dconcgo==1
replace dprigov=1 if edon==20 & yr==2003 & dconcgo==1
replace dprigov=1 if edon==20 & yr==2004 & dconcgo==1
replace dprigov=1 if edon==20 & yr==2005 & dconcgo==1
replace dprigov=1 if edon==20 & yr==2006 & dconcgo==1
replace dprigov=1 if edon==21 & yr==1989 & dconcgo==1
replace dprigov=1 if edon==21 & yr==1990 & dconcgo==1
replace dprigov=1 if edon==21 & yr==1991 & dconcgo==1
replace dprigov=1 if edon==21 & yr==1992 & dconcgo==1
replace dprigov=1 if edon==21 & yr==1993 & dconcgo==1
replace dprigov=1 if edon==21 & yr==1994 & dconcgo==1
replace dprigov=1 if edon==21 & yr==1995 & dconcgo==1
replace dprigov=1 if edon==21 & yr==1996 & dconcgo==1
replace dprigov=1 if edon==21 & yr==1997 & dconcgo==1
replace dprigov=1 if edon==21 & yr==1998 & dconcgo==1
replace dprigov=1 if edon==21 & yr==1999 & dconcgo==1
replace dprigov=1 if edon==21 & yr==2000 & dconcgo==1
replace dprigov=1 if edon==21 & yr==2001 & dconcgo==1
replace dprigov=1 if edon==21 & yr==2002 & dconcgo==1
replace dprigov=1 if edon==21 & yr==2003 & dconcgo==1
replace dprigov=1 if edon==21 & yr==2004 & dconcgo==1
replace dprigov=1 if edon==21 & yr==2005 & dconcgo==1
replace dprigov=1 if edon==21 & yr==2006 & dconcgo==1
replace dprigov=1 if edon==22 & yr==1989 & dconcgo==1
replace dprigov=1 if edon==22 & yr==1990 & dconcgo==1
replace dprigov=1 if edon==22 & yr==1991 & dconcgo==1
replace dprigov=1 if edon==22 & yr==1992 & dconcgo==1
replace dprigov=1 if edon==22 & yr==1993 & dconcgo==1
replace dprigov=1 if edon==22 & yr==1994 & dconcgo==1
replace dprigov=1 if edon==22 & yr==1995 & dconcgo==1
replace dprigov=1 if edon==22 & yr==1996 & dconcgo==1
replace dprigov=1 if edon==22 & yr==1997 & dconcgo==1
replace dpangov=1 if edon==22 & yr==1998 & dconcgo==1
replace dpangov=1 if edon==22 & yr==1999 & dconcgo==1
replace dpangov=1 if edon==22 & yr==2000 & dconcgo==1
replace dpangov=1 if edon==22 & yr==2001 & dconcgo==1
replace dpangov=1 if edon==22 & yr==2002 & dconcgo==1
replace dpangov=1 if edon==22 & yr==2003 & dconcgo==1
replace dpangov=1 if edon==22 & yr==2004 & dconcgo==1
replace dpangov=1 if edon==22 & yr==2005 & dconcgo==1
replace dpangov=1 if edon==22 & yr==2006 & dconcgo==1
replace dprigov=1 if edon==23 & yr==1989 & dconcgo==1
replace dprigov=1 if edon==23 & yr==1990 & dconcgo==1
replace dprigov=1 if edon==23 & yr==1991 & dconcgo==1
replace dprigov=1 if edon==23 & yr==1992 & dconcgo==1
replace dprigov=1 if edon==23 & yr==1993 & dconcgo==1
replace dprigov=1 if edon==23 & yr==1994 & dconcgo==1
replace dprigov=1 if edon==23 & yr==1995 & dconcgo==1
replace dprigov=1 if edon==23 & yr==1996 & dconcgo==1
replace dprigov=1 if edon==23 & yr==1997 & dconcgo==1
replace dprigov=1 if edon==23 & yr==1998 & dconcgo==1
replace dprigov=1 if edon==23 & yr==1999 & dconcgo==1
replace dprigov=1 if edon==23 & yr==2000 & dconcgo==1
replace dprigov=1 if edon==23 & yr==2001 & dconcgo==1
replace dprigov=1 if edon==23 & yr==2002 & dconcgo==1
replace dprigov=1 if edon==23 & yr==2003 & dconcgo==1
replace dprigov=1 if edon==23 & yr==2004 & dconcgo==1
replace dprigov=1 if edon==23 & yr==2005 & dconcgo==1
replace dprigov=1 if edon==23 & yr==2006 & dconcgo==1
replace dprigov=1 if edon==24 & yr==1989 & dconcgo==1
replace dprigov=1 if edon==24 & yr==1990 & dconcgo==1
replace dprigov=1 if edon==24 & yr==1991 & dconcgo==1
replace dprigov=1 if edon==24 & yr==1992 & dconcgo==1
replace dprigov=1 if edon==24 & yr==1993 & dconcgo==1
replace dprigov=1 if edon==24 & yr==1994 & dconcgo==1
replace dprigov=1 if edon==24 & yr==1995 & dconcgo==1
replace dprigov=1 if edon==24 & yr==1996 & dconcgo==1
replace dprigov=1 if edon==24 & yr==1997 & dconcgo==1
replace dprigov=1 if edon==24 & yr==1998 & dconcgo==1
replace dprigov=1 if edon==24 & yr==1999 & dconcgo==1
replace dprigov=1 if edon==24 & yr==2000 & dconcgo==1
replace dprigov=1 if edon==24 & yr==2001 & dconcgo==1
replace dprigov=1 if edon==24 & yr==2002 & dconcgo==1
replace dprigov=1 if edon==24 & yr==2003 & dconcgo==1
replace dpangov=1 if edon==24 & yr==2004 & dconcgo==1
replace dpangov=1 if edon==24 & yr==2005 & dconcgo==1
replace dpangov=1 if edon==24 & yr==2006 & dconcgo==1
replace dprigov=1 if edon==25 & yr==1989 & dconcgo==1
replace dprigov=1 if edon==25 & yr==1990 & dconcgo==1
replace dprigov=1 if edon==25 & yr==1991 & dconcgo==1
replace dprigov=1 if edon==25 & yr==1992 & dconcgo==1
replace dprigov=1 if edon==25 & yr==1993 & dconcgo==1
replace dprigov=1 if edon==25 & yr==1994 & dconcgo==1
replace dprigov=1 if edon==25 & yr==1995 & dconcgo==1
replace dprigov=1 if edon==25 & yr==1996 & dconcgo==1
replace dprigov=1 if edon==25 & yr==1997 & dconcgo==1
replace dprigov=1 if edon==25 & yr==1998 & dconcgo==1
replace dprigov=1 if edon==25 & yr==1999 & dconcgo==1
replace dprigov=1 if edon==25 & yr==2000 & dconcgo==1
replace dprigov=1 if edon==25 & yr==2001 & dconcgo==1
replace dprigov=1 if edon==25 & yr==2002 & dconcgo==1
replace dprigov=1 if edon==25 & yr==2003 & dconcgo==1
replace dprigov=1 if edon==25 & yr==2004 & dconcgo==1
replace dprigov=1 if edon==25 & yr==2005 & dconcgo==1
replace dprigov=1 if edon==25 & yr==2006 & dconcgo==1
replace dprigov=1 if edon==26 & yr==1989 & dconcgo==1
replace dprigov=1 if edon==26 & yr==1990 & dconcgo==1
replace dprigov=1 if edon==26 & yr==1991 & dconcgo==1
replace dprigov=1 if edon==26 & yr==1992 & dconcgo==1
replace dprigov=1 if edon==26 & yr==1993 & dconcgo==1
replace dprigov=1 if edon==26 & yr==1994 & dconcgo==1
replace dprigov=1 if edon==26 & yr==1995 & dconcgo==1
replace dprigov=1 if edon==26 & yr==1996 & dconcgo==1
replace dprigov=1 if edon==26 & yr==1997 & dconcgo==1
replace dprigov=1 if edon==26 & yr==1998 & dconcgo==1
replace dprigov=1 if edon==26 & yr==1999 & dconcgo==1
replace dprigov=1 if edon==26 & yr==2000 & dconcgo==1
replace dprigov=1 if edon==26 & yr==2001 & dconcgo==1
replace dprigov=1 if edon==26 & yr==2002 & dconcgo==1
replace dprigov=1 if edon==26 & yr==2003 & dconcgo==1
replace dprigov=1 if edon==26 & yr==2004 & dconcgo==1
replace dprigov=1 if edon==26 & yr==2005 & dconcgo==1
replace dprigov=1 if edon==26 & yr==2006 & dconcgo==1
replace dprigov=1 if edon==27 & yr==1989 & dconcgo==1
replace dprigov=1 if edon==27 & yr==1990 & dconcgo==1
replace dprigov=1 if edon==27 & yr==1991 & dconcgo==1
replace dprigov=1 if edon==27 & yr==1992 & dconcgo==1
replace dprigov=1 if edon==27 & yr==1993 & dconcgo==1
replace dprigov=1 if edon==27 & yr==1994 & dconcgo==1
replace dprigov=1 if edon==27 & yr==1995 & dconcgo==1
replace dprigov=1 if edon==27 & yr==1996 & dconcgo==1
replace dprigov=1 if edon==27 & yr==1997 & dconcgo==1
replace dprigov=1 if edon==27 & yr==1998 & dconcgo==1
replace dprigov=1 if edon==27 & yr==1999 & dconcgo==1
replace dprigov=1 if edon==27 & yr==2000 & dconcgo==1
replace dprigov=1 if edon==27 & yr==2001 & dconcgo==1
replace dprigov=1 if edon==27 & yr==2002 & dconcgo==1
replace dprigov=1 if edon==27 & yr==2003 & dconcgo==1
replace dprigov=1 if edon==27 & yr==2004 & dconcgo==1
replace dprigov=1 if edon==27 & yr==2005 & dconcgo==1
replace dprigov=1 if edon==27 & yr==2006 & dconcgo==1
replace dprigov=1 if edon==28 & yr==1989 & dconcgo==1
replace dprigov=1 if edon==28 & yr==1990 & dconcgo==1
replace dprigov=1 if edon==28 & yr==1991 & dconcgo==1
replace dprigov=1 if edon==28 & yr==1992 & dconcgo==1
replace dprigov=1 if edon==28 & yr==1993 & dconcgo==1
replace dprigov=1 if edon==28 & yr==1994 & dconcgo==1
replace dprigov=1 if edon==28 & yr==1995 & dconcgo==1
replace dprigov=1 if edon==28 & yr==1996 & dconcgo==1
replace dprigov=1 if edon==28 & yr==1997 & dconcgo==1
replace dprigov=1 if edon==28 & yr==1998 & dconcgo==1
replace dprigov=1 if edon==28 & yr==1999 & dconcgo==1
replace dprigov=1 if edon==28 & yr==2000 & dconcgo==1
replace dprigov=1 if edon==28 & yr==2001 & dconcgo==1
replace dprigov=1 if edon==28 & yr==2002 & dconcgo==1
replace dprigov=1 if edon==28 & yr==2003 & dconcgo==1
replace dprigov=1 if edon==28 & yr==2004 & dconcgo==1
replace dprigov=1 if edon==28 & yr==2005 & dconcgo==1
replace dprigov=1 if edon==28 & yr==2006 & dconcgo==1
replace dprigov=1 if edon==29 & yr==1989 & dconcgo==1
replace dprigov=1 if edon==29 & yr==1990 & dconcgo==1
replace dprigov=1 if edon==29 & yr==1991 & dconcgo==1
replace dprigov=1 if edon==29 & yr==1992 & dconcgo==1
replace dprigov=1 if edon==29 & yr==1993 & dconcgo==1
replace dprigov=1 if edon==29 & yr==1994 & dconcgo==1
replace dprigov=1 if edon==29 & yr==1995 & dconcgo==1
replace dprigov=1 if edon==29 & yr==1996 & dconcgo==1
replace dprigov=1 if edon==29 & yr==1997 & dconcgo==1
replace dprigov=1 if edon==29 & yr==1998 & dconcgo==1
replace dprigov=1 if edon==29 & yr==1999 & dconcgo==1
replace dprdgov=1 if edon==29 & yr==2000 & dconcgo==1
replace dprdgov=1 if edon==29 & yr==2001 & dconcgo==1
replace dprdgov=1 if edon==29 & yr==2002 & dconcgo==1
replace dprdgov=1 if edon==29 & yr==2003 & dconcgo==1
replace dprdgov=1 if edon==29 & yr==2004 & dconcgo==1
replace dprdgov=1 if edon==29 & yr==2005 & dconcgo==1
replace dpangov=1 if edon==29 & yr==2006 & dconcgo==1
replace dprigov=1 if edon==30 & yr==1989 & dconcgo==1
replace dprigov=1 if edon==30 & yr==1990 & dconcgo==1
replace dprigov=1 if edon==30 & yr==1991 & dconcgo==1
replace dprigov=1 if edon==30 & yr==1992 & dconcgo==1
replace dprigov=1 if edon==30 & yr==1993 & dconcgo==1
replace dprigov=1 if edon==30 & yr==1994 & dconcgo==1
replace dprigov=1 if edon==30 & yr==1995 & dconcgo==1
replace dprigov=1 if edon==30 & yr==1996 & dconcgo==1
replace dprigov=1 if edon==30 & yr==1997 & dconcgo==1
replace dprigov=1 if edon==30 & yr==1998 & dconcgo==1
replace dprigov=1 if edon==30 & yr==1999 & dconcgo==1
replace dprigov=1 if edon==30 & yr==2000 & dconcgo==1
replace dprigov=1 if edon==30 & yr==2001 & dconcgo==1
replace dprigov=1 if edon==30 & yr==2002 & dconcgo==1
replace dprigov=1 if edon==30 & yr==2003 & dconcgo==1
replace dprigov=1 if edon==30 & yr==2004 & dconcgo==1
replace dprigov=1 if edon==30 & yr==2005 & dconcgo==1
replace dprigov=1 if edon==30 & yr==2006 & dconcgo==1
replace dprigov=1 if edon==31 & yr==1989 & dconcgo==1
replace dprigov=1 if edon==31 & yr==1990 & dconcgo==1
replace dprigov=1 if edon==31 & yr==1991 & dconcgo==1
replace dprigov=1 if edon==31 & yr==1992 & dconcgo==1
replace dprigov=1 if edon==31 & yr==1993 & dconcgo==1
replace dprigov=1 if edon==31 & yr==1994 & dconcgo==1
replace dprigov=1 if edon==31 & yr==1995 & dconcgo==1
replace dprigov=1 if edon==31 & yr==1996 & dconcgo==1
replace dprigov=1 if edon==31 & yr==1997 & dconcgo==1
replace dprigov=1 if edon==31 & yr==1998 & dconcgo==1
replace dprigov=1 if edon==31 & yr==1999 & dconcgo==1
replace dprigov=1 if edon==31 & yr==2000 & dconcgo==1
replace dprigov=1 if edon==31 & yr==2001 & dconcgo==1
replace dpangov=1 if edon==31 & yr==2002 & dconcgo==1
replace dpangov=1 if edon==31 & yr==2003 & dconcgo==1
replace dpangov=1 if edon==31 & yr==2004 & dconcgo==1
replace dpangov=1 if edon==31 & yr==2005 & dconcgo==1
replace dpangov=1 if edon==31 & yr==2006 & dconcgo==1
replace dpangov=1 if edon==31 & yr==2007 & dconcgo==1
replace dprigov=1 if edon==32 & yr==1989 & dconcgo==1
replace dprigov=1 if edon==32 & yr==1990 & dconcgo==1
replace dprigov=1 if edon==32 & yr==1991 & dconcgo==1
replace dprigov=1 if edon==32 & yr==1992 & dconcgo==1
replace dprigov=1 if edon==32 & yr==1993 & dconcgo==1
replace dprigov=1 if edon==32 & yr==1994 & dconcgo==1
replace dprigov=1 if edon==32 & yr==1995 & dconcgo==1
replace dprigov=1 if edon==32 & yr==1996 & dconcgo==1
replace dprigov=1 if edon==32 & yr==1997 & dconcgo==1
replace dprigov=1 if edon==32 & yr==1998 & dconcgo==1
replace dprdgov=1 if edon==32 & yr==1999 & dconcgo==1
replace dprdgov=1 if edon==32 & yr==2000 & dconcgo==1
replace dprdgov=1 if edon==32 & yr==2001 & dconcgo==1
replace dprdgov=1 if edon==32 & yr==2002 & dconcgo==1
replace dprdgov=1 if edon==32 & yr==2003 & dconcgo==1
replace dprdgov=1 if edon==32 & yr==2004 & dconcgo==1
replace dprdgov=1 if edon==32 & yr==2005 & dconcgo==1
replace dprdgov=1 if edon==32 & yr==2006 & dconcgo==1

sort inegi
merge inegi using "colind munic intra edos.dta"
drop _merge

replace c1=14008 if edon==14 & munn==125 & yr>=2005
replace c2=14013 if edon==14 & munn==125 & yr>=2005
replace c3=14078 if edon==14 & munn==125 & yr>=2005
replace c4=14093 if edon==14 & munn==125 & yr>=2005
replace colintra=4 if edon==14 & munn==125 & yr>=2005

replace c1=14093 if edon==14 & munn==126 & yr>=2005
replace c2=14013 if edon==14 & munn==126 & yr>=2005
replace c3=14105 if edon==14 & munn==126 & yr>=2005
replace c4=14125 if edon==14 & munn==126 & yr>=2005
replace c5=14111 if edon==14 & munn==126 & yr>=2005
replace c6=14078 if edon==14 & munn==126 & yr>=2005
replace colintra=6 if edon==14 & munn==126 & yr>=2005

replace c1=7119 if edon==7 & munn==113 & yr>=1999
replace c2=7049 if edon==7 & munn==113 & yr>=1999
replace c3=7023 if edon==7 & munn==113 & yr>=1999
replace c4=7026 if edon==7 & munn==113 & yr>=1999
replace c5=7022 if edon==7 & munn==113 & yr>=1999
replace colintra=5 if edon==7 & munn==113 & yr>=1999

replace c1=7059 if edon==7 & munn==114 & yr>=1999
replace c2=7116 if edon==7 & munn==114 & yr>=1999
replace colintra=2 if edon==7 & munn==114 & yr>=1999

replace c1=7059 if edon==7 & munn==115 & yr>=1999
replace c2=7116 if edon==7 & munn==115 & yr>=1999
replace c2=7052 if edon==7 & munn==115 & yr>=1999
replace colintra=3 if edon==7 & munn==115 & yr>=1999

replace c1=7059 if edon==7 & munn==116 & yr>=1999
replace c2=7114 if edon==7 & munn==116 & yr>=1999
replace c2=7115 if edon==7 & munn==116 & yr>=1999
replace colintra=3 if edon==7 & munn==116 & yr>=1999

replace c1=7008 if edon==7 & munn==117 & yr>=1999
replace c2=7051 if edon==7 & munn==117 & yr>=1999
replace c3=7080 if edon==7 & munn==117 & yr>=1999
replace colintra=3 if edon==7 & munn==117 & yr>=1999

replace c1=7072 if edon==7 & munn==118 & yr>=1999
replace c2=7081 if edon==7 & munn==118 & yr>=1999
replace c3=7047 if edon==7 & munn==118 & yr>=1999
replace colintra=3 if edon==7 & munn==118 & yr>=1999

replace c1=7014 if edon==7 & munn==119 & yr>=1999
replace c2=7049 if edon==7 & munn==119 & yr>=1999
replace c3=7113 if edon==7 & munn==119 & yr>=1999
replace c4=7026 if edon==7 & munn==119 & yr>=1999
replace c5=7022 if edon==7 & munn==119 & yr>=1999
replace colintra=5 if edon==7 & munn==119 & yr>=1999

replace c1=7022 if edon==7 & munn==26 & yr>=1999
replace c2=7023 if edon==7 & munn==26 & yr>=1999
replace c3=7056 if edon==7 & munn==26 & yr>=1999
replace c4=7066 if edon==7 & munn==26 & yr>=1999
replace c5=7093 if edon==7 & munn==26 & yr>=1999
replace c6=7112 if edon==7 & munn==26 & yr>=1999
replace c7=7113 if edon==7 & munn==26 & yr>=1999
replace colintra=7 if edon==7 & munn==26 & yr>=1999

replace c1=7114 if edon==7 & munn==59 & yr>=1999
replace c2=7116 if edon==7 & munn==59 & yr>=1999
replace c3=7115 if edon==7 & munn==59 & yr>=1999
replace c4=7052 if edon==7 & munn==59 & yr>=1999
replace c5=7004 if edon==7 & munn==59 & yr>=1999
replace c6=7065 if edon==7 & munn==59 & yr>=1999
replace c7=7031 if edon==7 & munn==59 & yr>=1999
replace c8=7112 if edon==7 & munn==59 & yr>=1999
replace c9=7064 if edon==7 & munn==59 & yr>=1999
replace colintra=9 if edon==7 & munn==59 & yr>=1999

replace c1=7059 if edon==7 & munn==52 & yr>=1999
replace c2=7004 if edon==7 & munn==52 & yr>=1999
replace c3=7024 if edon==7 & munn==52 & yr>=1999
replace c4=7019 if edon==7 & munn==52 & yr>=1999
replace c5=7041 if edon==7 & munn==52 & yr>=1999
replace c6=7099 if edon==7 & munn==52 & yr>=1999
replace colintra=6 if edon==7 & munn==52 & yr>=1999

replace c1=7020 if edon==7 & munn==8 & yr>=1999
replace c2=7051 if edon==7 & munn==8 & yr>=1999
replace c3=7117 if edon==7 & munn==8 & yr>=1999
replace c4=7080 if edon==7 & munn==8 & yr>=1999
replace c5=7030 if edon==7 & munn==8 & yr>=1999
replace colintra=5 if edon==7 & munn==8 & yr>=1999

replace c10=7118 if edon==7 & munn==81 & yr>=1999
replace colintra=10 if edon==7 & munn==81 & yr>=1999

replace c1=7014 if edon==7 & munn==49 & yr>=1999
replace c2=7013 if edon==7 & munn==49 & yr>=1999
replace c3=7044 if edon==7 & munn==49 & yr>=1999
replace c4=7023 if edon==7 & munn==49 & yr>=1999
replace c5=7113 if edon==7 & munn==49 & yr>=1999
replace c6=7119 if edon==7 & munn==49 & yr>=1999
replace colintra=6 if edon==7 & munn==49 & yr>=1999

*marquelia
replace c1=12052 if edon==12 & munn==77 & yr>=2002
replace c2=12013 if edon==12 & munn==77 & yr>=2002
replace c3=12080 if edon==12 & munn==77 & yr>=2002
replace c4=12018 if edon==12 & munn==77 & yr>=2002
replace colintra=4 if edon==12 & munn==77 & yr>=2002

*cochoapa el grande
replace c1=12043 if edon==12 & munn==78 & yr>=2002
replace c2=12062 if edon==12 & munn==78 & yr>=2002
replace colintra=2 if edon==12 & munn==78 & yr>=2002

*josé joaquín de herrera
replace c1=12028 if edon==12 & munn==79 & yr>=2002
replace c2=12010 if edon==12 & munn==79 & yr>=2002
replace colintra=2 if edon==12 & munn==79 & yr>=2002

*juchitán
replace c1=12077 if edon==12 & munn==80 & yr>=2002
replace c2=12013 if edon==12 & munn==80 & yr>=2002
replace c3=12023 if edon==12 & munn==80 & yr>=2002
replace colintra=3 if edon==12 & munn==80 & yr>=2002

*iliatenco
replace c1=12041 if edon==12 & munn==81 & yr>=2002
replace c2=12052 if edon==12 & munn==81 & yr>=2002
replace c3=12043 if edon==12 & munn==81 & yr>=2002
replace colintra=3 if edon==12 & munn==81 & yr>=2002

*cambios en azoyú para crear marquelia
replace c1=12036 if edon==12 & munn==13 & yr>=2001
replace c2=12046 if edon==12 & munn==13 & yr>=2001
replace c3=12023 if edon==12 & munn==13 & yr>=2001
replace c4=12077 if edon==12 & munn==13 & yr>=2001
replace c5=12052 if edon==12 & munn==13 & yr>=2001
replace colintra=5 if edon==12 & munn==13 & yr>=2001

*como no se reportan datos en 2005 para los mun 78-81 les puse estrellas a los municipios que les cedieron territorio, que habrá que quitar cuando haya reporte de datos para 78-81 o se explique qué pasa
*cambios en azoyú para crear juchitán
*replace c1=12036 if edon==12 & munn==13 & yr>=2003
*replace c2=12046 if edon==12 & munn==13 & yr>=2003
*replace c3=12023 if edon==12 & munn==13 & yr>=2003
*replace c4=12080 if edon==12 & munn==13 & yr>=2003
*replace c5=12077 if edon==12 & munn==13 & yr>=2003
*replace c6=12052 if edon==12 & munn==13 & yr>=2003
*replace colintra=5 if edon==12 & munn==13 & yr>=2003

*replace c1=12004 if edon==12 & munn==43 & yr>=2003
*replace c2=12069 if edon==12 & munn==43 & yr>=2003
*replace c3=12009 if edon==12 & munn==43 & yr>=2003
*replace c4=12041 if edon==12 & munn==43 & yr>=2003
*replace c5=12081 if edon==12 & munn==43 & yr>=2003
*replace c6=12052 if edon==12 & munn==43 & yr>=2003
*replace c7=12036 if edon==12 & munn==43 & yr>=2003
*replace c8=12062 if edon==12 & munn==43 & yr>=2003
*replace c9=12078 if edon==12 & munn==43 & yr>=2003
*replace colintra=9 if edon==12 & munn==43 & yr>=2003

*chilapa para josé Joaquín herrera
*replace c1=12002 if edon==12 & munn==28 & yr>=2003
*replace c2=12074 if edon==12 & munn==28 & yr>=2003
*replace c3=12061 if edon==12 & munn==28 & yr>=2003
*replace c4=12044 if edon==12 & munn==28 & yr>=2003
*replace c5=12051 if edon==12 & munn==28 & yr>=2003
*replace c6=12076 if edon==12 & munn==28 & yr>=2003
*replace c7=12010 if edon==12 & munn==28 & yr>=2003
*replace c8=12079 if edon==12 & munn==28 & yr>=2003
*replace colintra=8 if edon==12 & munn==28 & yr>=2003

*atlixtac para josé Joaquín herrera
*replace c1=12002 if edon==12 & munn==10 & yr>=2003
*replace c2=12028 if edon==12 & munn==10 & yr>=2003
*replace c3=12076 if edon==12 & munn==10 & yr>=2003
*replace c4=12072 if edon==12 & munn==10 & yr>=2003
*replace c5=12020 if edon==12 & munn==10 & yr>=2003
*replace c6=12066 if edon==12 & munn==10 & yr>=2003
*replace c7=12079 if edon==12 & munn==10 & yr>=2003
*replace colintra=7 if edon==12 & munn==10 & yr>=2003

*azoyú para juchitán
*replace c1=12052 if edon==12 & munn==13 & yr>=2003
*replace c2=12077 if edon==12 & munn==13 & yr>=2003
*replace c3=12023 if edon==12 & munn==13 & yr>=2003
*replace c4=12046 if edon==12 & munn==13 & yr>=2003
*replace c5=12036 if edon==12 & munn==13 & yr>=2003
*replace c6=12080 if edon==12 & munn==13 & yr>=2003
*replace colintra=6 if edon==12 & munn==13 & yr>=2003

*malinaltepec para iliatenco
*replace c1=12009 if edon==12 & munn==41 & yr>=2003
*replace c2=12020 if edon==12 & munn==41 & yr>=2003
*replace c3=12063 if edon==12 & munn==41 & yr>=2003
*replace c4=12052 if edon==12 & munn==41 & yr>=2003
*replace c5=12043 if edon==12 & munn==41 & yr>=2003
*replace c6=12081 if edon==12 & munn==41 & yr>=2003
*replace colintra=6 if edon==12 & munn==41 & yr>=2003

*list edo munn mun yr if colintra==.

gen tmpsum= oth3a+ oth3b+ oth4on+ vcpan+ vcpri+ vcprd+ pan+ pri+ prd
move tmpsum efec
drop efec
rename tmpsum efec

gen pansh=.
replace pansh=(pan+vcpan)/efec
gen prish=.
replace prish=(pri+vcpri)/efec
gen prdsh=.
replace prdsh=(prd+vcprd)/efec
*gen winsh=.
*replace winsh=
*gen secsh=.
*replace secsh=
*gen thdsh=.
*replace thdsh=

sort yr inegi

foreach i of numlist 1/23 {
    rename c`i' nei`i'
    }

foreach i of numlist 1/23 {
    gen cpannei`i' = .
    }

foreach i of numlist 1/23 {
    gen cprdnei`i' = .
    }

foreach i of numlist 1/23 {
    gen move`i' = inegi-nei`i'
    }

*estos sirven para checar que move le atine
*foreach i of numlist 1/23 {
*    replace cpannei`i' =  inegi[_n-move`i']
*    }
*foreach i of numlist 1/23 {
*    gen tmp`i' =  nei`i'-cpannei`i'
*    }
*foreach i of numlist 1/23 {
*    sum tmp`i'
*    }

foreach i of numlist 1/23 {
    replace cpannei`i' = cpan[_n-move`i']
    }

foreach i of numlist 1/23 {
    replace cprdnei`i' = cprd[_n-move`i']
    }

foreach i of numlist 1/23 {
    gen panshnei`i' = .
    }

foreach i of numlist 1/23 {
    replace panshnei`i' = pansh[_n-move`i']
    }

foreach i of numlist 1/23 {
    gen prishnei`i' = .
    }

foreach i of numlist 1/23 {
    replace prishnei`i' = prish[_n-move`i']
    }

foreach i of numlist 1/23 {
    gen prdshnei`i' = .
    }

foreach i of numlist 1/23 {
    replace prdshnei`i' = prdsh[_n-move`i']
    }

egen meancpan = rmean(cpannei1-cpannei23)
egen meancprd = rmean(cprdnei1-cprdnei23)

xx
*si se quiere cambiar udea
do dyad

drop c1-c23



xx
*todo compute de colindancias y lags debe hacerse antes de tirar casos...
drop if dusos==1 & yr>=1995
drop if checkmun=="."
drop checkmun

move mon mo
drop mo
drop if yr==1971
drop if edon==10 & yr==1974

sort edon yr munn
list edon yr munn ncand prish if ncand==1 & rankpri>1

xx

logit cpan yr dconcpr dconcmid dconcgo dpangov meancpan if yr>1975
