- [Description of *Recent Mexican Election Vote Returns* repository<a id="orge791881"></a>](#org8b22c2f)
- [Files in the repository and how to cite them](#orgee625af)
  - [File naming conventions](#org79a5483)
  - [Election returns for municipal offices](#org632452c)
  - [Election returns for Congress](#org57dc45e)
  - [Presidential election returns](#orga6b216e)
  - [Gubernatorial election returns](#org30f6374)
  - [Other](#org3a06537)
- [Historical party performance statistics](#org9db0a42)
  - [Backwards prediction](#orgc7dfe19)
- [Codebook](#orga6aede7)
- [Coding procedure for the incumbent's status<a id="orgf690d01"></a>](#orgadbbd9c)
- [Procedimiento para codificar el estatus del ocupante<a id="org013874b"></a>](#orgd96b7be)
- [<a id="org2d1f482"></a>Basic instructions to inspect data online:](#orgad4fe67)
- [Sources](#org1e69022)
- [Acknowledgements](#org18b8b42)

Last revision: 2023-05-27

---

<h2> Recent news </h2>

<sup><sub>2023-05-26</sub></sup> **v.hat and alpha regressions**, summaring each unit's electoral history, migrated from <https://github.com/emagar/mxDistritos>. Despite relying on electoral maps to maintain unit geography longitudinally constant, these are statistics directly relevant to this repository's content. Estimates and predictions for districts added, including predicted votes for the suspicious 1988 election, and the script for split secciones was finalized.

<sup><sub>2023-03-10</sub></sup> **Inspect election returns online in Google sheets [here](https://emagar.github.io/view-in-gSheets/) (Beta)**

<sup><sub>2023-02-22</sub></sup> **Casilla latitude/longitude** added for federal elections between 2006 and 2018 (excluding 2012, source needs debugging).

<sup><sub>2022-11-18</sub></sup> **Casilla-level lista nominal** added to 1991-2003 federal deputy files.

**New script** `code/extract-state-yr-mu-returns.r` exports municipal election returns. Focus in a single state-year allows votes received by each party across municipalities grouped in one column each &#x2014; easier to describe.

---


<a id="org8b22c2f"></a>

# Description of *Recent Mexican Election Vote Returns* repository<a id="orge791881"></a>

-   Author: Eric Magar
-   Location <https://github.com/emagar/elecRetrns>
-   Email: emagar at itam dot mx
-   Citation for the data: see 'About' on the repository landing page

The repository contains voting data for recent Mexican elections for certain offices at different levels of aggregation. Data has been compiled from many sources. More recent years tend to be coded from official vote returns. Earlier elections tend to be from secondary sources (see Souces section). Data includes district-level federal deputy vote returns since 1979 and district-level presidential vote returns since 2006; and municipality-level municipal president vote returns (except in the state of Nayarit, votes cast for municipal president also elect a municipal council in a fused ballot).

*Important note:* older incarnations of this repository contain LFS (Large File System) files. Make sure to install [LFS](https://git-lfs.github.com/) in your machine before checking out older commits of the repository.


<a id="orgee625af"></a>

# Files in the repository and how to cite them

You are free to download and modify the data (see the LICENSE document for details) provided you give proper credit to this source. Unless otherwise noted in the file descriptor, the cite is Eric Magar (2018) Recent Mexican election vote returns repository, <https://github.com/emagar/elecReturns>.


<a id="org79a5483"></a>

## File naming conventions

In general, file names identify the office elected (i.e., **df**, **se**, **pr**, **dl**, **go**, **ay** for *diputados federales*, *senadores*, *presidente*, *diputados locales*, *gobernador*, and *ayuntamiento*, respectively), followed by the unit of observation (i.e., **ed**, **df**, **dl**, **mu**, **de**, **se**, **ca** for *estado*, *distrito federal*, *distrito local*, *municipio*, *demarcación*, *sección*, and *casilla*, respectively), then the years included.

So, for example `aymu1970-on.csv` are *ayuntamiento* votes at the municipio level since 1970.


<a id="org632452c"></a>

## Election returns for municipal offices

Municipal election data updated to 2022. Other than in the state of Nayarit since 2008 (and, pending a court case, Mexico City since 2018), *ayuntamientos* are elected in fused ballots for a *presidente municipal* and a fraction of the municipal council (*regidores* and *síndicos*). Nayarit elects these members of the municipal council in single-member plurality districts called *demarcaciones*. Two versions of municipal election returns are included: one with municipal votes aggregated by candidate, another aggregated by party (the script [`code/ay.r`](./code/ay.r) was used for these manipulations):

-   [`data/aymu1970-on.coalAgg.csv`](./data/aymu1989-present.coalAgg.csv) = *ayuntamiento* election returns, coalition votes aggregated by candidate.
-   [`data/aymu1970-on.coalSplit.csv`](./data/aymu1989-present.coalSplit.csv) = *ayuntamiento* election returns, coalition votes split among parties where source allows it.
-   [`data/aymu1989-on.incumbents.csv`](./data/aymu1989-present.incumbents.csv) = names of elected municipal officers (*presidente municipal* only) since 1989. Includes reelection status since 2018.
-   [`data/ayde2008-on-Nayarit-regid.coalAgg.csv`](./data/ayde2008-on-Nayarit-regid.coalAgg.csv) = Nayarit's municipal council election demarcación-level returns since 2008, coalition votes aggregated by candidate.
-   [`data/ayde2008-on-Nayarit-regid.coalSplit.csv`](./data/ayde2008-on-Nayarit-regid.coalSplit.csv) = Nayarit's municipal demarcaciones vote returns since 2008, coalition votes split among parties where source allows it.
-   **Inspect the data above online** in Google sheets (basic instructions [here](#org2d1f482); data split by approximate decades or groups of states to comply with spreadsheet size limits (400k cells); files have identical columns to ease appending one another off Google drive):
    -   `*.coalAgg` versions: [1970s](https://docs.google.com/spreadsheets/d/10DjanWnuvGUqO8AFDb3yky8Pa7ciMhf_MbthCmKCloI/copy) [1980s](https://docs.google.com/spreadsheets/d/1hqAyWaewUKwA-CKgXgcg-p4aqPQxmuTbxmcKSQgjfDE/copy) [1990s](https://docs.google.com/spreadsheets/d/1nwEO4u4ddn4kGlHUM9dc-ueD6L7IXXkLNHRBDPrB9Nk/copy) [2000s](https://docs.google.com/spreadsheets/d/1WBmHm1yqgXO6qjj8czROZNcZNS_G82Z-UT0vtTxSVFI/copy) [2010s](https://docs.google.com/spreadsheets/d/1TgdTRdN5wqLPdV4j2CvvhvsXbFnMFMSho653XQHQsNs/copy) [2020s](https://docs.google.com/spreadsheets/d/1jXzjWBfQrpFTHahXDW9i3nyFL0bjYqSeIMwS-CGA3KQ/copy) [Nayarit](https://docs.google.com/spreadsheets/d/1buoVi7UlVPoApm7nan-ixb3ts8Sraj_V86mK-3UeH3w/copy)
    -   `*.coalSplit` versions : [1970s](https://docs.google.com/spreadsheets/d/10xIcX83xTi-YI1PmdmdpTGpItBtwndOZILZZugdMpVo/copy) [1980s](https://docs.google.com/spreadsheets/d/1yqCFBtr8Z2sCya7CT9LMPifU_kA4wlqSTLjrW-KiALc/copy) [1990s](https://docs.google.com/spreadsheets/d/1rE5KHwvuVglV0rLI70P4PgtOmWSiUGtS92G4QbN4zz0/copy) [2000s](https://docs.google.com/spreadsheets/d/1GgG7SSeJptJ-uGmIgBck3mniL2HR1gn6efebBMlEpXQ/copy) [2010s](https://docs.google.com/spreadsheets/d/1nKyNzZuLyDWxqIfC6MiRDmvzligGQ2v_YrqppcEVa1Q/copy) [2020s](https://docs.google.com/spreadsheets/d/1xg9GvjPzOq7TxxkkebMGuOBz50WU_RVEaadLsIWRWHU/copy) [Nayarit](https://docs.google.com/spreadsheets/d/1C2OvOsSBaOqMOj1KEV6F2dgq0fGBwLJJYSnvBKpmTkM/copy)
    -   `incumbents`: [Aguascalientes (1)&#x2013;Nuevo León (19)](https://docs.google.com/spreadsheets/d/1lgJJ2f8O_MHe18q3OekRylgxOXpKGrcm6ABQPVhmlf4/copy) [Oaxaca (20)&#x2013;Zacatecas (32)](https://docs.google.com/spreadsheets/d/1ZabVHORN0uOU8AX7bZGiQY1JEhncG6SodLfn6DXW4zQ/copy)


<a id="org57dc45e"></a>

## Election returns for Congress

Congressional election data updated to 2021. A mixed majority system is in place since 1979 for the chamber of deputies, since 1997 for the senate (Weldon 2001 describes the system well). 300 deputy seats are elected by plurality rule in single member districts. Two versions of federal deputy returns are included: one with district votes aggregated by candidate, another by party (the script [`code/df.r`](./code/ay.r) was used for these manipulations):

**Citation for Congressional election data**: Eric Magar, Alejandro Trelles, Micah Altman, and Michael P. McDonald (2017) Components of partisan bias originating from single-member districts in multi-party systems: An application to Mexico, *Political Geography* 57(1):1-12.

-   [`data/dfdf1979-on.coalAgg.csv`](./data/dfdf1979-on.coalAgg.csv) = federal deputy returns, coalition votes aggregated by candidate.
-   [`data/dfdf1979-on.coalSplit.csv`](./data/dfdf1979-on.coalSplit.csv) = federal deputy returns, coalition votes split among parties where source allows it.
-   [`data/dfdf1997-on.incumbents.csv`](./data/dfdf1979-on.coalSplit.csv) = names of elected federal deputies (SMD tier only) since 1997. Includes reelection status since 2021.
-   [`data/dfdf2006-on-candidates.csv`](./data/dfdf2006-on-candidates.csv) = names of all federal deputy candidates in districts and party lists since 2006.
-   [`data/seed2012-on.candidates.csv`](./data/seed2012-on.candidates.csv) = names of all senatorial candidates in states and party lists since 2012.
-   **Inspect the data above online** in Google sheets (basic instructions [here](#org2d1f482)):
    -   [`coalAgg`](https://docs.google.com/spreadsheets/d/1cUfi1BlpVVeBKo-vI2lbQAwtUGpGFlGAqcdHZ01BtRo/copy) version
    -   [`coalSplit`](https://docs.google.com/spreadsheets/d/1c57io0aooj54elYxw2Ya0QO1_tRWd-QWadKYCLU3CiA/copy) version
    -   [`incumbents`](https://docs.google.com/spreadsheets/d/1r6BER0cmm4MNwNiy7ZdAwALzQn9QiEGg_9TfALumbPU/copy)


<a id="orga6b216e"></a>

## Presidential election returns

Presidential election data updated to 2018. Presidents elected by plurality in a nationwide race every six years.

**Citation for the presidential dataset**: Eric Magar (2012) Gubernatorial Coattails in Mexican Congressional Elections, *The Journal of Politics* 74(2):383-399.

-   [`data/prdf2006-on.csv`](./data/prdf2006-on.csv)
-   [`data/pred1964-on.csv`](./data/pred1964-on.csv)


<a id="org30f6374"></a>

## Gubernatorial election returns

**Citation for the gubernatorial dataset**: Eric Magar (2012) Gubernatorial Coattails in Mexican Congressional Elections, *The Journal of Politics* 74(2):383-399.

-   [`data/goed1961-on.csv`](./data/goed1961-on.csv) = governor statewide election returns since 1961, updated to 2022.
-   [`data/goed1985-on.incumbents.csv`](./data/goed1985-on.incumbents.csv) = elected governors since 1985, updated to 2019.


<a id="org3a06537"></a>

## Other

-   [`code/extract-state-yr-mu-returns.r`](./code/extract-state-yr-mu-returns.r) = script exports municipal coalition-aggregates election returns. Select one state and year to get csv file with votes received by each party across municipalities grouped in one column each.
-   [`datosBrutos/`](./datosBrutos/) = directory containing selected primary sources. Files for state elections were kept out from the repository due to sizes exceeding github's limit&#x2026; [e-mail me](mailto:emagar@itam.mx) if you need any of these.


<a id="org9db0a42"></a>

# Historical party performance statistics

Measures of recent electoral history, that [this blog entry](https://emagar.github.io/residuales-2018-english/) describes, offer indicators of party competitiveness at different units of aggregation. This includes quantities of substantive interest, such as parties' predicted vote share in the unit (`v.hats`) based on their performance in the previous five congressional elections, and how sensitive the party's vote share in the unit is to national vote swings (`alphas`) between 1994 and 2021.

| Level             | 2009                                                                                                                                                         | 2012                                                                                                                                                         | 2015                                                                                                                                                         | 2018                                                                                                                                                         | 2021                                                                                                                                                         | 2024                                                                                                                                                         |
|----------------- |------------------------------------------------------------------------------------------------------------------------------------------------------------ |------------------------------------------------------------------------------------------------------------------------------------------------------------ |------------------------------------------------------------------------------------------------------------------------------------------------------------ |------------------------------------------------------------------------------------------------------------------------------------------------------------ |------------------------------------------------------------------------------------------------------------------------------------------------------------ |------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| District          | [csv](./data/v-hats-etc/dis/dipfed-distrito-vhat-2009.csv) [view](https://docs.google.com/spreadsheets/d/1E9hffMdeTqOG5V8z7YEwCRRvazpYaj5HSa5xqOn4WJs/copy)  | [csv](./data/v-hats-etc/dis/dipfed-distrito-vhat-2012.csv) [view](https://docs.google.com/spreadsheets/d/1vze9n0HwIw8RC68Ie6lRB1x-pgg0purapbF04iywKdc/copy)  | [csv](./data/v-hats-etc/dis/dipfed-distrito-vhat-2015.csv) [view](https://docs.google.com/spreadsheets/d/1YM8g_tmfNtnyJQva6N6HH6NnLa2Si40amO9di40lw8c/copy)  | [csv](./data/v-hats-etc/dis/dipfed-distrito-vhat-2018.csv) [view](https://docs.google.com/spreadsheets/d/1xP4ABf7VvSLefRRyScdBxVGarWOr_hYZK956hIPZkIY/copy)  | [csv](./data/v-hats-etc/dis/dipfed-distrito-vhat-2021.csv) [view](https://docs.google.com/spreadsheets/d/1Oce9stn05v9M-T8YusF2d7nPxy3J8_OqRVdiDnOI9mQ/copy)  | [csv](./data/v-hats-etc/dis/dipfed-distrito-vhat-2024.csv) [view](https://docs.google.com/spreadsheets/d/1p-EH1pahzgoMgF6yVpFp-_L6Byma0ZmBUfi0e1zanl0/copy)  |
| Municipio         | [csv](./data/v-hats-etc/mun/dipfed-municipio-vhat-2009.csv) [view](https://docs.google.com/spreadsheets/d/1Y3VipbSzmhbfWUXBnNVHGS8mDjHxaEW1lYoccQ9tr48/copy) | [csv](./data/v-hats-etc/mun/dipfed-municipio-vhat-2012.csv) [view](https://docs.google.com/spreadsheets/d/1LwuFkzPVLVwL2kkOEBBz54g7ZDM6RMJ4mlQv7awIDtg/copy) | [csv](./data/v-hats-etc/mun/dipfed-municipio-vhat-2015.csv) [view](https://docs.google.com/spreadsheets/d/18XlMxG4HN_vrDdyPYtqJbQXKpM-1LR5MVq598Any2nw/copy) | [csv](./data/v-hats-etc/mun/dipfed-municipio-vhat-2018.csv) [view](https://docs.google.com/spreadsheets/d/1L9SeCXUpHkhk4K1Xagv34Z65i8WvmW9E-ApeNB36_v4/copy) | [csv](./data/v-hats-etc/mun/dipfed-municipio-vhat-2021.csv) [view](https://docs.google.com/spreadsheets/d/14vSmGXfQc5BXvZ32nKAVyaQdDV1dz6cXgL1MGMxxt94/copy) | [csv](./data/v-hats-etc/mun/dipfed-municipio-vhat-2024.csv) [view](https://docs.google.com/spreadsheets/d/13FnRerpuxIM-RZfgzkjUiPXKPKL1JACkBIk0oa2kSfI/copy) |
| Sección electoral | [csv](./data/v-hats-etc/sec/dipfed-seccion-vhat-2009.csv)                                                                                                    | [csv](./data/v-hats-etc/sec/dipfed-seccion-vhat-2012.csv)                                                                                                    | [csv](./data/v-hats-etc/sec/dipfed-seccion-vhat-2015.csv)                                                                                                    | [csv](./data/v-hats-etc/sec/dipfed-seccion-vhat-2018.csv)                                                                                                    | [csv](./data/v-hats-etc/sec/dipfed-seccion-vhat-2021.csv)                                                                                                    | [csv](./data/v-hats-etc/sec/dipfed-seccion-vhat-2024.csv)                                                                                                    |

[<sub>Go to top</sub>](#orge791881)


<a id="orgc7dfe19"></a>

## Backwards prediction

Vote returns prior to 1991 are unavailable at lower units of aggregation. Pre-2009 `v.hats` are obtained by using same general approach, but backwards, "predicting" from subsequent party performance. Year t's predicted vote up to 2006 is a linear projection of the unit's vote observed in the next five elections (years t+15, t+12 &#x2026; t+3).

| Level             | 1988                                                                                                                                                       | 1991                                                                                                                                                       | 1994                                                                                                                                                       | 1997                                                                                                                                                       | 2000                                                                                                                                                       | 2003                                                                                                                                                       | 2006                                                                                                                                                       |
|----------------- |---------------------------------------------------------------------------------------------------------------------------------------------------------- |---------------------------------------------------------------------------------------------------------------------------------------------------------- |---------------------------------------------------------------------------------------------------------------------------------------------------------- |---------------------------------------------------------------------------------------------------------------------------------------------------------- |---------------------------------------------------------------------------------------------------------------------------------------------------------- |---------------------------------------------------------------------------------------------------------------------------------------------------------- |---------------------------------------------------------------------------------------------------------------------------------------------------------- |
| District          | [csv](./data/v-hats-etc/dis/dipfed-distrito-vhat-19.csv) [view](https://docs.google.com/spreadsheets/d/1KL3Cu9B-xvMSkyc0FLosi69xns7Pk6yx8zdyU-aLkhw/copy)  | [csv](./data/v-hats-etc/dis/dipfed-distrito-vhat-19.csv) [view](https://docs.google.com/spreadsheets/d/1eU_LXZhj-Lcd4E2OdJUj-w3yBIkWovXZFK7RPtqy08E/copy)  | [csv](./data/v-hats-etc/dis/dipfed-distrito-vhat-19.csv) [view](https://docs.google.com/spreadsheets/d/1CPkki2zC5KnsNu-iuY3j4WRVv1eZooyjB6UWf3JXQTI/copy)  | [csv](./data/v-hats-etc/dis/dipfed-distrito-vhat-19.csv) [view](https://docs.google.com/spreadsheets/d/1BWnGr8jtI6ezdK1ZQIIRhnY7ACI8mcPf5weF5z97tho/copy)  | [csv](./data/v-hats-etc/dis/dipfed-distrito-vhat-20.csv) [view](https://docs.google.com/spreadsheets/d/1_Qx5trIPmc5oAhXBNRHl_fOR92JYTpXE4y6YpNN1Yss/copy)  | [csv](./data/v-hats-etc/dis/dipfed-distrito-vhat-20.csv) [view](https://docs.google.com/spreadsheets/d/1ONYLsWjmNxzY0h2p0x_waYUYditYF2lO0rDaONzwihY/copy)  | [csv](./data/v-hats-etc/dis/dipfed-distrito-vhat-20.csv) [view](https://docs.google.com/spreadsheets/d/1CZ2pzL3g4XcAnPeWs-TFh9m3qK0iaJX_z9cvhWjCzWk/copy)  |
| Municipio         | [csv](./data/v-hats-etc/mun/dipfed-municipio-vhat-19.csv) [view](https://docs.google.com/spreadsheets/d/1kKC6rnp9rgXTv6aEQSvvGT9NY5J3J6D1UtNiJOwEJgo/copy) | [csv](./data/v-hats-etc/mun/dipfed-municipio-vhat-19.csv) [view](https://docs.google.com/spreadsheets/d/1JrjGS1pZ0CrDcjFd23RUTMdosOwWce568ra2E1pQxu4/copy) | [csv](./data/v-hats-etc/mun/dipfed-municipio-vhat-19.csv) [view](https://docs.google.com/spreadsheets/d/1Y01BdrOr15ei2pDeGrXtTEz6nqP6IS-USvoiZLoXWqk/copy) | [csv](./data/v-hats-etc/mun/dipfed-municipio-vhat-19.csv) [view](https://docs.google.com/spreadsheets/d/1ulwgVYCbgQeC_5FuXCYkilP6yfpPc69NR_gZpXf6QEM/copy) | [csv](./data/v-hats-etc/mun/dipfed-municipio-vhat-20.csv) [view](https://docs.google.com/spreadsheets/d/1M90-ZuW3SRnqxfRjboeWWvU2wt-YPnlp7nT4n7fM6zI/copy) | [csv](./data/v-hats-etc/mun/dipfed-municipio-vhat-20.csv) [view](https://docs.google.com/spreadsheets/d/1ky0Eris0cU3OaEA35kQPhjEsR_kxilWnx-iEun8D06M/copy) | [csv](./data/v-hats-etc/mun/dipfed-municipio-vhat-20.csv) [view](https://docs.google.com/spreadsheets/d/1HfzYxOxRcFfVgJivvX7nYeaiMbILM7ePAGSbL_jcv74/copy) |
| Sección electoral |                                                                                                                                                            | [csv](./data/v-hats-etc/sec/dipfed-seccion-vhat-19.csv)                                                                                                    | [csv](./data/v-hats-etc/sec/dipfed-seccion-vhat-19.csv)                                                                                                    | [csv](./data/v-hats-etc/sec/dipfed-seccion-vhat-19.csv)                                                                                                    | [csv](./data/v-hats-etc/sec/dipfed-seccion-vhat-20.csv)                                                                                                    | [csv](./data/v-hats-etc/sec/dipfed-seccion-vhat-20.csv)                                                                                                    | [csv](./data/v-hats-etc/sec/dipfed-seccion-vhat-20.csv)                                                                                                    |


<a id="orga6aede7"></a>

# Codebook

Most variables are included in every file, some appear in selected files only.

-   *edon* = state number 1:32.
-   *edo* = state abbreviation (may differ from commonly used abbreviations so that sorting them alphabetically preserves the order set by *edon*, eg. Chiapas is cps, not chis).
-   *disn* = *edon* \* 100 + district number.
-   *emm* = municipal indentifying code (edo-electionCycle.inegi).
-   *mun* = municipality's name.
-   *inegi*, *ife* = municipal identifier codes used by the INEGI and the IFE/INE, respectively.
-   *yr*, *mo*, *dy* = year, month, day of the election.
-   *cab* = cabecera, district's administrative center.
-   *circ* = second-tier proportional representation district (circunscripcion electoral).
-   *v01*, *v02*, &#x2026; = raw vote for candidate 1, 2, etc.
-   *l01*, *l02*, &#x2026; = label of candidate 1's, 2's, &#x2026; party or coalition.
-   *c01*, *c02*, &#x2026; = candidate 1's, 2's, &#x2026; name.
-   *s01*, *s02*, &#x2026; = suplente (substitute) for candidate 1, 2, etc.
-   *efec* = effective votes, equal the total raw votes minus votes for write-in candidates minus invalid ballots. This is the proper denominator for vote shares.
-   *nr* = votes for write-in candidates (void in Mexican election law).
-   *nul* = invalid ballots.
-   *tot* = total raw votes.
-   *lisnom* = eligible voters (*lista nominal*).
-   *latitude*, *longitude* = coordinates indicating a precinct's (casilla) north&#x2013;south and east&#x2013;west position in a map. Available for federal deputy and presidential casilla-level returns in the 2006, 2009, 2015, and 2018 elections.
-   *nota* = notes.
-   *fuente* = source.
-   *ncand* = number of candidates running.
-   *dcoal* = dummy equal 1 if at least one candidate ran on a multi-party pre-electoral coalition, 0 otherwise.
-   *ncoal* = number of candidates who ran on multi-party pre-electoral coalitions.
-   *coalpan*, *coalpri*, *coalprd* = members of major-party coalitions ('no' indidates no coalition).
-   *imputacion*, *distpan*, *distpri*, *distprd* = when some parties coelesced in such way that only their pooled vote was reported, an attempt is made to infer how many votes each coalition member contributed to team. Variable *imputacion* lists what earlier election was used for this purpose ('no' if none carried); *dist* variables report the share of the coalition total attributable to PAN, PRI, and PRD, respectively. See [this](https://github.com/emagar/replicationMaterial/blob/master/gubCoat/onlineAppendix.pdf) for details.
-   *seyr*, *semo* = year of the previous/concurrent senatorial election.
-   *sepan*, *sepri*, *seprd* = votes won by major parties in previous/concurrent senatorial election.
-   *seefec* = effective votes in previous/concurrent senatorial election.
-   *fake* = indicates fake data for hegemonic era elections, made up of best guesses about what happened in the state's race for the purpose of computing vote lags. Will normally be dropped from analysis.
-   *win* = winner's party or coalition.
-   *incumbent* = winning candidate's name.
-   *race.after* = incumbent's status in the subsequent race. See [this](#orgf690d01) for categories and coding procedure ([aquí](#org013874b) la versión en español del procedimiento codificador).
-   *dcarta* = dummy equal 1 if member filed a letter of intent with the chamber's Junta to run for office again; 0 otherwise. Inapplicable before 2018. See [this](http://eleccionconsecutiva.diputados.gob.mx/contendientes).


<a id="orgadbbd9c"></a>

# Coding procedure for the incumbent's status<a id="orgf690d01"></a>

In file `data/aymu1985-on.incumbents.csv`, variable *race.after* equals one of the following categories:

1.  'Beaten' if the incumbent re-ran and lost;
2.  'Reelected' if the incumbent re-ran and won;
3.  'Renom-killed' if the incumbent re-ran and was killed in the campaign;
4.  'Hi-office' if the incumbent ran for higher office;
5.  'Out' if the incumbent withdrew or was not renominated;
6.  'Term-limited' if the incumbent was ineligible for reelection due to a term limit;
7.  A year indicates that it is too early to know the incumbent's status (and the year of the next race).

In categories other than the first two above, a suffix may be present.

-   Suffix '-p-lost' indicates that the party lost the subsequent race (or, in case of incumbents elected by a multi-party coalition, that none of them won or was part of the winning coalition).
-   Suffix '-p-won' indicates that the party won the subsequent race (or, in case of incumbents elected by a multi-party coalition, that one of them won or at least one of them was in the winning coalition).


<a id="orgd96b7be"></a>

# Procedimiento para codificar el estatus del ocupante<a id="org013874b"></a>

En el archivo `data/aymu1985-on.incumbents.csv`, la variable *race.after* indica el estatus del ocupante en la elección subsecuente. El estatus puede ser una de las categorías siguientes:

1.  'Beaten' si el ocupante volvió a contender y perdió;
2.  'Reelected' si el ocupante volvió a contender y ganó;
3.  'Renom-killed' si el ocupante volvió a contender y fue asesinado en la campaña;
4.  'Hi-office' si el ocupante contendió por otro cargo de elección (p.ej. gobernador o senador);
5.  'Out' si el ocupante se retiró o no fue repostulado por el partido;
6.  'Term-limited' si el ocupante estaba constitucionalmente impedido para aspirar a reelegirse;
7.  Un año indica que aún es temprano para conocer el estatus (y el año de la próxima elección).

En las categorías 3 en adelante, un sufijo puede estar presente.

-   El sufijo '-p-lost' indica que el partido perdió la elección subsecuente (o, para ocupantes electos por una coalición multi-partidista, que ninguno de esos partidos ganó o fue parte de la coalición ganadora).
-   El sufijo '-p-won' indica que el partido ganó la elección subsecuente (o, para ocupantes electos por una coalición multi-partidista, que uno de esos partidos ganó o que por lo menos uno fue parte de la coalición ganadora).


<a id="orgad4fe67"></a>

# <a id="org2d1f482"></a>Basic instructions to inspect data online:

You can open election returns in online spreadsheet form.

a. To use this feature, you must first log into a Google account. Then click the desired file's link, and confirm you wish a copy. A Google spreadsheet will open in your browser.

b. If you wish to manipulate the data (eg. re-sorting rows by year or keeping a subset of the observations only), or save the file to your hard drive, you must unlink the data from the repository. To do this type CTRL+A (ie., select all) then CTRL+SHIFT+V (ie., paste values only).

c. Linked data updates about every hour. If a refresh were needed sooner, erase the function in cell A1 and undo the change.


<a id="org1e69022"></a>

# Sources

Work in progress

-   *Fuente* = iee/ife/ine indicates data obtained from the primary source, the state/federal election board's web site.
-   *Fuente* = tesis Melissa
-   *Fuente* = Magar 1994
-   *Fuente* = Mexico Electoral Banamex
-   *Fuente* = Toledo Patiño paper
-   *Fuente* = UAM Iztapalapa for older state races
-   *Fuente* = voz y voto


<a id="org18b8b42"></a>

# Acknowledgements

Eric Magar acknowledges financial support from the Asociación Mexicana de Cultura A.C. He is responsible for mistakes and shortcomings in the data.

Many students over the years have provided research assistance to retrieve and systematize the information reported here.

-   Under construction
-   Daniela Guzmán Lerma
-   Eugenio Solís Flores
-   Francisco Garfias
-   José Angel Torrens Hernández
-   Julio Solís Ríos
-   Lucía Motolinia
-   Mauricio Fernández Duque
-   Odette
-   Sonia Kuri Kosegarten
-   Vidal Mendoza Tinoco
