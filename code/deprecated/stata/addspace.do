clear

set mem 16m

use "ay mu 1977-2006 nvo.dta", clear
gen edomun=edon*1000+munn
sort edomun
merge edomun using "colind munic intra edos.dta"

tab _merge

*corrige por remunicipalizacion
replace c1=. if edomun==1001 & yr<1992
replace c5=1006 if edomun==1001 & yr<1992
replace c5=. if edomun==1002 & yr<1992
replace c2=. if edomun==1005 & yr<1992
replace c2=1001 if edomun==1006 & yr<1992
replace c1=. if edomun==2001 & yr<2001
replace c1=. if edomun==2004 & yr<2001
replace c2=. if edomun==3003 & yr<1980
replace c2=. if edomun==3001 & yr<1993
replace c1=. if edomun==3002 & yr<1993
replace c2=. if edomun==4003 & yr<1991
replace c4=. if edomun==4003 & yr<1991
replace c1=. if edomun==4004 & yr<1991
replace c5=. if edomun==4004 & yr<1991
replace c5=. if edomun==4006 & yr<1997
replace c4=. if edomun==4009 & yr<1997
replace c1=. if edomun==4010 & yr<2000
replace c1=. if edomun==4009 & yr<2000
replace c4=. if edomun==4003 & yr<2000
replace c5=. if edomun==10001 & yr<1989
replace c4=. if edomun==10032 & yr<1989
replace c4=. if edomun==10003 & yr<1989
replace c1=12012 if edomun==12072 & yr<1996
replace c5=12051 if edomun==12072 & yr<1996
replace c6=12052 if edomun==12072 & yr<1996
replace c7=12072 if edomun==12072 & yr<1996
replace c1=. if edomun==12010 & yr<1996
replace c3=. if edomun==12012 & yr<1996
replace c3=. if edomun==12051 & yr<1996
replace c2=. if edomun==12052 & yr<1996
replace c1=. if edomun==12072 & yr<1996
replace c4=. if edomun==12063 & yr<1996
replace c1=. if edomun==15025 & yr<1993
replace c1=. if edomun==15029 & yr<1993
replace c1=. if edomun==15039 & yr<1993
replace c1=. if edomun==15070 & yr<1993
replace c1=15082 if edomun==15066 & yr<2003
replace c1=15082 if edomun==15116 & yr<2003
replace c1=15066 if edomun==15082 & yr<2003
replace c7=15116 if edomun==15082 & yr<2003
replace c1=15111 if edomun==15074 & yr<2003
replace c1=15074 if edomun==15111 & yr<2003
replace c1=. if edomun==15114 & yr<2003
replace c1=. if edomun==15064 & yr<2003
replace c1=. if edomun==18004 & yr<1990
replace c1=. if edomun==23001
replace c1=23002 if edomun==23001 & yr<1996
replace c2=23005 if edomun==23001 & yr<1996
replace c3=23007 if edomun==23001 & yr<1996
replace c8=24013 if edomun==24010 & yr<1997
replace c2=24010 if edomun==24013 & yr<1997
replace c1=. if edomun==24036 & yr<1997
replace c3=. if edomun==24037 & yr<1997
replace c4=. if edomun==24038 & yr<1997
replace c3=. if edomun==24054 & yr<1997
replace c5=25002 if edomun==25006 & yr<1983
replace c3=25006 if edomun==25002 & yr<1983
replace c5=. if edomun==25013 & yr<1983
replace c2=. if edomun==26048 & yr<1991
replace c4=. if edomun==26017 & yr<1991
replace c3=26018 if edomun==26026 & yr<1997
replace c2=. if edomun==26042 & yr<1997
replace c6=. if edomun==26029 & yr<1997
replace c2=. if edomun==26012 & yr<1997
replace c1=7026 if edomun==7059 & yr<1991
replace c7=7066 if edomun==7059 & yr<1991
replace c8=7082 if edomun==7059 & yr<1991
replace c9=7093 if edomun==7059 & yr<1991
replace c4=. if edomun==7031 & yr<1991
replace c2=. if edomun==7064 & yr<1991
replace c5=. if edomun==30161 & yr<1982
replace c2=. if edomun==30123 & yr<1982
replace c5=30061 if edomun==30039 & yr<1991
replace c2=. if edomun==30111 & yr<1991
replace c3=. if edomun==30039 & yr<1991
replace c5=. if edomun==30048 & yr<1991
replace c1=. if edomun==30174 & yr<1997
replace c3=. if edomun==30054 & yr<1997
replace c3=. if edomun==30084 & yr<1997
replace c2=30012 if edomun==30045 & yr<1997
replace c9=30169 if edomun==30045 & yr<1997
replace c2=30045. if edomun==30012 & yr<1997
replace c6=30045 if edomun==30169 & yr<1997
replace c4=. if edomun==30122 & yr<1997
replace c1=. if edomun==30104 & yr<1997
replace c4=. if edomun==30149 & yr<1997
replace c3=. if edomun==30032 & yr<1997
replace c1=. if edomun==30061 & yr<1997
replace c1=. if edomun==30108 & yr<1997
replace c3=. if edomun==30070 & yr<1997
replace c1=. if edomun==30091 & yr<1997
replace c1=30114 if edomun==30102 & yr<2004
replace c13=. if edomun==30109 & yr<2004
replace c2=. if edomun==30158 & yr<2004
replace c3=30102 if edomun==30114 & yr<2004
replace c1=. if edomun==30130 & yr<2004
replace c2=. if edomun==32017 & yr<2001
replace c4=. if edomun==32016 & yr<2001
replace c8=. if edomun==32036 & yr<2001

*no tengo info de colindancias de nuevos municipios cps
drop if edomun==7113
drop if edomun==7114
drop if edomun==7115
drop if edomun==7116
drop if edomun==7117
drop if edomun==7118
drop if edomun==7119

*tlaxcala
replace c1=29034 if edomun==29030 & yr<1996
replace c1=29030 if edomun==29034 & yr<1996
replace c6=. if edomun==29020 & yr<1996
replace c5=. if edomun==29020 & yr>1995
replace c6=29028 if edomun==29010 & yr<1996
replace c6=29050 if edomun==29010 & yr>1995
replace c4=29010 if edomun==29028 & yr<1996
replace c6=29024 if edomun==29032 & yr<1996
replace c5=29029 if edomun==29032 & yr<1996
replace c7=. if edomun==29032 & yr<1996
replace c3=. if edomun==29032 & yr<1996
replace c4=29032 if edomun==29024 & yr<1996
replace c5=29032 if edomun==29029 & yr<1996
replace c2=29018 if edomun==29038 & yr<1996
replace c1=29038 if edomun==29018 & yr<1996
replace c4=29032 if edomun==29029 & yr<1996
replace c5=29010 if edomun==29029 & yr<1996
replace c7=. if edomun==29029 & yr<1996
replace c8=. if edomun==29029 & yr<1996
replace c3=29029 if edomun==29032 & yr<1996
replace c1=29029 if edomun==29010 & yr<1996
replace c2=29022 if edomun==29044 & yr<1996
replace c4=29041 if edomun==29044 & yr<1996
replace c5=. if edomun==29044 & yr<1996
replace c8=. if edomun==29044 & yr<1996
replace c1=29044 if edomun==29022 & yr<1996
replace c3=29044 if edomun==29041 & yr<1996
replace c1=29017 if edomun==29022 & yr<1996
replace c2=29025 if edomun==29022 & yr<1996
replace c5=29041 if edomun==29022 & yr<1996
replace c6=29044 if edomun==29022 & yr<1996
replace c1=29022 if edomun==29017 & yr<1996
replace c2=29022 if edomun==29025 & yr<1996
replace c5=29022 if edomun==29041 & yr<1996
replace c6=29022 if edomun==29044 & yr<1996

replace c1=. if edomun==29011 & yr<1996
replace c2=29010 if edomun==29013 & yr<1996
replace c3=. if edomun==29013 & yr<1996
replace c4=. if edomun==29013 & yr<1996
replace c4=. if edomun==29014 & yr<1996
replace c8=. if edomun==29014 & yr<1996
replace c3=29015 if edomun==29015 & yr<1996
replace c2=29022 if edomun==29017 & yr<1996
replace c2=. if edomun==29019 & yr<1996
replace c4=29023 if edomun==29023 & yr<1996
replace c5=. if edomun==29023 & yr<1996
replace c6=. if edomun==29023 & yr<1996
replace c5=. if edomun==29025 & yr<1996
replace c5=. if edomun==29026 & yr<1996
replace c2=. if edomun==29028 & yr<1996
replace c5=. if edomun==29028 & yr<1996
replace c6=. if edomun==29028 & yr<1996
replace c3=. if edomun==29033 & yr<1996
replace c4=. if edomun==29033 & yr<1996
replace c5=. if edomun==29033 & yr<1996
replace c10=. if edomun==29033 & yr<1996
replace c2=. if edomun==29034 & yr<1996
replace c4=. if edomun==29034 & yr<1996
replace c1=29040 if edomun==29040 & yr<1996
replace c4=. if edomun==29041 & yr<1996
replace c3=. if edomun==29042 & yr<1996

sort edon yr edomun

xxxxxxxxxxxxxx
xxx desvan xxx
xxxxxxxxxxxxxx

replace c=. if edomun==290 & yr<1996
replace c=. if edomun==290 & yr<1996
replace c=. if edomun==290 & yr<1996
replace c=. if edomun==290 & yr<1996
replace c=. if edomun==290 & yr<1996
replace c=. if edomun==290 & yr<1996
replace c=. if edomun==290 & yr<1996
replace c=. if edomun==290 & yr<1996
replace c=. if edomun==290 & yr<1996



xx

*quita colindancias con municipios de otros estados
replace c1=. if int(c1/1000)~=int(edomun/1000)
replace c2=. if int(c2/1000)~=int(edomun/1000)
replace c3=. if int(c3/1000)~=int(edomun/1000)
etc

