Notes on vhat/alpha regressions estimation post 2021.

- When 2024 returns are available, code for district and municipal regressions is set to re-estimate everything. Not a problem because estimation completes fast. For secciones, code was adapted to allow estimation of 2024, 2027, and alpha regressions only (erasing larger regression object to free memory as it goes).

- Run script elec-data-for-maps.r from scratch to incorporate new votes into estimation objects as well as new secciones/reseccionamiento/municipios/districts.

- Sacar vhats de mxDistritos y pasar todo a elecRetrns. El readme.md del primero muestra lo que hay que llevarse.


