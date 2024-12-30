gsort edon yr
set obs 960
gen str3 pty="pan" in 1/320
replace pty="pri" in 321/640
replace pty="prd" in 641/960
replace yr=yr[_n-320] in 321/960
replace mo=mo[_n-320] in 321/960
replace edoemm=edoemm[_n-320] in 321/960
replace edo=edo[_n-320] in 321/960
gen vdf=pan in 1/320
replace vdf=pri[_n-320] in 321/640
replace vdf=prd[_n-640] in 641/960
replace votoefec=votoefec[_n-320] in 321/960
replace pryr=pryr[_n-320] in 321/960
replace prmo=prmo[_n-320] in 321/960
gen vpr=prpan in 1/320
replace vpr=prpri[_n-320] in 321/640
replace vpr=prprd[_n-640] in 641/960
replace prefec=prefec[_n-320] in 321/960
replace prwin=prwin[_n-320] in 321/960
replace goyr=goyr[_n-320] in 321/960
replace gomo=gomo[_n-320] in 321/960
gen vgo=gopan in 1/320
replace vgo=gopri[_n-320] in 321/640
replace vgo=goprd[_n-640] in 641/960
replace goefec=goefec[_n-320] in 321/960
replace gowin=gowin[_n-320] in 321/960
replace dlyr=dlyr[_n-320] in 321/960
replace dlmo=dlmo[_n-320] in 321/960
gen vdl=dlpan in 1/320
replace vdl=dlpri[_n-320] in 321/640
replace vdl=dlprd[_n-640] in 641/960
replace dlefec=dlefec[_n-320] in 321/960
gen dfsh=pansh in 1/320
replace dfsh=prish[_n-320] in 321/640
replace dfsh=prdsh[_n-640] in 641/960
*gen dfbar=panbar in 1/320
*replace dfbar=pribar[_n-320] in 321/640
*replace dfbar=prdbar[_n-640] in 641/960
gen dfshbar=panshbar in 1/320
replace dfshbar=prishbar[_n-320] in 321/640
replace dfshbar=prdshbar[_n-640] in 641/960
gen dfshres=panshres in 1/320
replace dfshres=prishres[_n-320] in 321/640
replace dfshres=prdshres[_n-640] in 641/960
*gen dfstres=panstres in 1/320
*replace dfstres=pristres[_n-320] in 321/640
*replace dfstres=prdstres[_n-640] in 641/960
gen dfshstres=panshstres in 1/320
replace dfshstres=prishstres[_n-320] in 321/640
replace dfshstres=prdshstres[_n-640] in 641/960
replace dconcpr=dconcpr[_n-320] in 321/960
replace dconcgo=dconcgo[_n-320] in 321/960
replace dconcdl=dconcdl[_n-320] in 321/960
replace dconcgowopr=dconcgowopr[_n-320] in 321/960
replace dconcgoandpr=dconcgoandpr[_n-320] in 321/960
*gen prres=prpanres in 1/320
*replace prres=prprires[_n-320] in 321/640
*replace prres=prprdres[_n-640] in 641/960
gen prshres=prpanshres in 1/320
replace prshres=prprishres[_n-320] in 321/640
replace prshres=prprdshres[_n-640] in 641/960
*gen prstres=prpanstres in 1/320
*replace prstres=prpristres[_n-320] in 321/640
*replace prstres=prprdstres[_n-640] in 641/960
gen prshstres=prpanshstres in 1/320
replace prshstres=prprishstres[_n-320] in 321/640
replace prshstres=prprdshstres[_n-640] in 641/960
*gen gores=gopanres in 1/320
*replace gores=goprires[_n-320] in 321/640
*replace gores=goprdres[_n-640] in 641/960
gen goshres=gopanshres in 1/320
replace goshres=goprishres[_n-320] in 321/640
replace goshres=goprdshres[_n-640] in 641/960
*gen gostres=gopanstres in 1/320
*replace gostres=gopristres[_n-320] in 321/640
*replace gostres=goprdstres[_n-640] in 641/960
gen goshstres=gopanshstres in 1/320
replace goshstres=goprishstres[_n-320] in 321/640
replace goshstres=goprdshstres[_n-640] in 641/960
*gen dlres=dlpanres in 1/320
*replace dlres=dlprires[_n-320] in 321/640
*replace dlres=dlprdres[_n-640] in 641/960
gen dlshres=dlpanshres in 1/320
replace dlshres=dlprishres[_n-320] in 321/640
replace dlshres=dlprdshres[_n-640] in 641/960
*gen dlstres=dlpanstres in 1/320
*replace dlstres=dlpristres[_n-320] in 321/640
*replace dlstres=dlprdstres[_n-640] in 641/960
gen dlshstres=dlpanshstres in 1/320
replace dlshstres=dlprishstres[_n-320] in 321/640
replace dlshstres=dlprdshstres[_n-640] in 641/960
replace ptot=ptot[_n-320] in 321/960
replace chggsp1=chggsp1[_n-320] in 321/960
replace chggsp2=chggsp2[_n-320] in 321/960
replace chggsp3=chggsp3[_n-320] in 321/960
gen dfcgo=goshstres*dconcgo
gen dfcgowopr=goshstres*dconcgowopr
gen dfcgoandpr=goshstres*dconcgoandpr
gen dfcpr=prshstres*dconcpr
gen dfcdl=dlshstres*dconcdl
replace incugo=incugo[_n-320] in 321/960
replace gspbar=gspbar[_n-320] in 321/960

gen dpan=0
gen dpri=0
gen dprd=0
replace dpan=1 in 1/320
replace dpri=1 in 321/640
replace dprd=1 in 641/960
gen dincupr=0
replace dincupr=1 if dpri==1 & yr<=2000
replace dincupr=1 if dpan==1 & yr>2000
gen dincugo=0
replace dincugo=1 if incugo=="pan" in 1/320
replace dincugo=1 if incugo=="pri" in 321/640
replace dincugo=1 if incugo=="prd" in 641/960

gen gspfx=gspbar if dincupr==1
replace gspfx=-gspbar if dincupr==1

log on
reg dfshstres dconcpr dfcpr dconcgo dfcgoandpr dfcgowopr dincugo dincupr gspbar dpan dprd if yr>=1979
log close

