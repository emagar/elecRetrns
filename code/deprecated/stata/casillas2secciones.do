version 8.0

******************************
** DIPUTADOS FEDERALES 2012 **
******************************

cd "D:\01\Dropbox\data\elecs\MXelsCalendGovt\elecReturns"
use "dfCasillas2012", clear

drop if tipo=="S" /* drops casillas especiales to match what is done with president vote. The votes here belong in the district (so voter was given ballot for diputados). But I could not distinguish these exact voters in president database from voters in especiales who were only given ballots for president. If exact district totals were needed, casillas espaciales should go back in */
sort edon seccion casilla

gen drop=0
replace drop=1 if edon[_n]==edon[_n-1] & seccion[_n]==seccion[_n-1]
replace drop=0 in 1

gen tmp=statusacta==3
by edon seccion: egen casillasNoInstaladas=sum(tmp)
drop tmp
gen tmp=statusacta==4
by edon seccion: egen casillasNoEntregadas=sum(tmp)
drop tmp
* OJO: hay info en statusCasilla sobre recuentos, actas de Consejo etc que se pierde

by edon seccion: egen tmp=sum(nul)
move tmp nul
drop nul
rename tmp nul

rename nreg nr
by edon seccion: egen tmp=sum(nr)
move tmp nr
drop nr
rename tmp nr

by edon seccion: egen tmp=sum(pan)
move tmp pan
drop pan
rename tmp pan

by edon seccion: egen tmp=sum(pri)
move tmp pri
drop pri
rename tmp pri

by edon seccion: egen tmp=sum(prd)
move tmp prd
drop prd
rename tmp prd

by edon seccion: egen tmp=sum(pvem)
move tmp pvem
drop pvem
rename tmp pvem

by edon seccion: egen tmp=sum(pt)
move tmp pt
drop pt
rename tmp pt

by edon seccion: egen tmp=sum(mc)
move tmp mc
drop mc
rename tmp mc

by edon seccion: egen tmp=sum(panal)
move tmp panal
drop panal
rename tmp panal

by edon seccion: egen tmp=sum(pripvem)
move tmp pripvem
drop pripvem
rename tmp pripvem

by edon seccion: egen tmp=sum(prdptmc)
move tmp prdptmc
drop prdptmc
rename tmp prdptmc

by edon seccion: egen tmp=sum(prdpt)
move tmp prdpt
drop prdpt
rename tmp prdpt

by edon seccion: egen tmp=sum(prdmc)
move tmp prdmc
drop prdmc
rename tmp prdmc

by edon seccion: egen tmp=sum(lisnom)
move tmp lisnom
drop lisnom
rename tmp lisnom

by edon seccion: egen tmp=sum(listanomcasilla)
move tmp listanomcasilla
drop listanomcasilla
rename tmp listanomcasilla

drop if drop==1
drop drop casilla tipo extcontig statusacta

save "dfSeccion2012", replace
outsheet using "dfSeccion2012.csv", comma replace

xx

*********************
*********************
** PRESIDENTE 2012 **
*********************
*********************

*cd "D:\01\Dropbox\data\elecs\MXelsCalendGovt\elecReturns"
*use "prCasillas2012", clear
*
*drop if tipo=="M" /* drops votos desde el extranjero */
*drop if tipo=="S" /* drops casillas especiales */
*sort edon seccion casilla
*
*gen drop=0
*replace drop=1 if edon[_n]==edon[_n-1] & seccion[_n]==seccion[_n-1]
*replace drop=0 in 1
*
**gen tmp=statusacta==3
**by edon seccion: egen casillasNoInstaladas=sum(tmp)
**drop tmp
**gen tmp=statusacta==4
**by edon seccion: egen casillasNoEntregadas=sum(tmp)
**drop tmp
** OJO: hay info en statusCasilla sobre recuentos, actas de Consejo etc que se pierde
*
*by edon seccion: egen tmp=sum(nul)
*move tmp nul
*drop nul
*rename tmp nul
*
*by edon seccion: egen tmp=sum(nr)
*move tmp nr
*drop nr
*rename tmp nr
*
*by edon seccion: egen tmp=sum(pan)
*move tmp pan
*drop pan
*rename tmp pan
*
*by edon seccion: egen tmp=sum(pri)
*move tmp pri
*drop pri
*rename tmp pri
*
*by edon seccion: egen tmp=sum(prd)
*move tmp prd
*drop prd
*rename tmp prd
*
*by edon seccion: egen tmp=sum(pvem)
*move tmp pvem
*drop pvem
*rename tmp pvem
*
*by edon seccion: egen tmp=sum(pt)
*move tmp pt
*drop pt
*rename tmp pt
*
*by edon seccion: egen tmp=sum(mc)
*move tmp mc
*drop mc
*rename tmp mc
*
*by edon seccion: egen tmp=sum(panal)
*move tmp panal
*drop panal
*rename tmp panal
*
*by edon seccion: egen tmp=sum(pripvem)
*move tmp pripvem
*drop pripvem
*rename tmp pripvem
*
*by edon seccion: egen tmp=sum(prdptmc)
*move tmp prdptmc
*drop prdptmc
*rename tmp prdptmc
*
*by edon seccion: egen tmp=sum(prdpt)
*move tmp prdpt
*drop prdpt
*rename tmp prdpt
*
*by edon seccion: egen tmp=sum(prdmc)
*move tmp prdmc
*drop prdmc
*rename tmp prdmc
*
*by edon seccion: egen tmp=sum(lisnom)
*move tmp lisnom
*drop lisnom
*rename tmp lisnom
*
*by edon seccion: egen tmp=sum(listanomcasilla)
*move tmp listanomcasilla
*drop listanomcasilla
*rename tmp listanomcasilla
*
*drop if drop==1
*drop drop casilla tipo extcontig tipocandidatura statusacta nota
*
*save "prSeccion2012", replace
*outsheet using "prSeccion2012.csv", comma replace
