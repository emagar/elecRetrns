ej para reshape

munn	n1	n2	pansh	panshn1	panshn2
1	2	.	.1	.2	.
2	1	3	.2	.1	.3
3	4	2	.3	.2	.3
4	3	.	.4	.3	.

reshape long n panshn, i(munn) j(pair)



gen edomun=edon*1000+munn
sort edomun yr
reshape long nei panshnei prishnei prdshnei cpannei cprdnei, i(munn) j(pair)
drop if nei==.


