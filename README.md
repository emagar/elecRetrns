
# Table of Contents

1.  [Description of *Recent Mexican Election Vote Returns* repository](#orgfb663a6)
2.  [Files in the repository and how to cite them](#org845ca5d)
3.  [Codebook](#org1ea53ef)
4.  [Sources](#orgc2488b5)
5.  [Acknowledgements](#org9e6918a)

Last revision: 2018-03-08


<a id="orgfb663a6"></a>

# Description of *Recent Mexican Election Vote Returns* repository

-   Author: Eric Magar
-   Email: emagar at itam dot mx

The repository contains voting data for recent Mexican elections for certain offices at different levels of aggregation. Data has been compiled from many sources. More recent years tend to be coded from official vote returns. Earlier elections tend to be from secondary sources (see Souces section). Data inludes district-level federal deputy vote returns since 1979 and district-level presidential vote returns since 2006; and municipality-level municipal president vote returns (except in the state of Nayarit, votes cast for municipal president also elect a municipal council in a fused ballot). 

*Important note:* older incarnations of this this repository contain LFS (Large File System) parts. Make sure to install [LFS](https://git-lfs.github.com/) in your machine before cloning previous commits of the repository.


<a id="org845ca5d"></a>

# Files in the repository and how to cite them

You are free to download and modify the data (see the LICENSE document for details) provided you give proper credit to this source. Unless otherwise noted next to the file descriptor, the cite is Eric Magar (2018) Recent Mexican election vote returns repository, <https://github.com/emagar/elecReturns>.

In general, file names identify the office elected (i.e., **df**, **se**, **pr**, **dl**, **go**, **ay** for *diputados federales*, *senadores*, *presidente*, *diputados locales*, *gobernador*, and *ayuntamiento*, respectively), followed by the unit of observation (i.e., **ed**, **df**, **dl**, **mu**, **de**, **se**, **ca** for *estado*, *distrito federal*, *distrito local*, *municipio*, *demarcación*, *sección*, and *casilla* respectively), and the years included. Other than in Nayarit since 2008 (and, pending a court case, Mexico City since 2018), *ayuntamientos* are elected in fused ballots for a *presidente municipal* and a fraction of the municipal council (*regidores* and *síndicos*). Nayarit elects these members of the municipal council in single-member plurality districts called *demarcaciones*.

-   `data/aymu1977-present.csv` = updated to 2013, can be processed with code/ay.r in order to systematize coalitions (ie., aggregate votes when member parties' returns are reported separately and remove redundant columns).
-   `data/aymu1985-present.coalAgg.csv` = pre-processed version of the above (starting in 1985) so that coalition votes appear properly aggregated.
-   `data/ayde2008-presentNayRegid.csv` = Nayarit's municipal demarcaciones vote returns since 2008.
-   `code/ay.r` = script to manipulate *ayuntamiento* returns.
-   `code/ayClean.r` = script used to clean *ayuntamiento* returns, should be unnecessary unless new data are added because output has been saved into csv file.
-   `data/dfdf1979-on.csv`
    -   **Citation for this dataset**: Eric Magar, Alejandro Trelles, Micah Altman, and Michael P. McDonald (2017) Components of partisan bias originating from single-member districts in multi-party systems: An application to Mexico, *Political Geography* 57(1):1-12.
-   `data/dfdfcandidates2015-on.csv` = names of all federal deputy candidates in districts and party lists since 2015.
-   `data/seedcandidates2018.csv` = names of all senatorial candidates in states and party lists in 2018.
-   `data/goed1961-on.csv` = updated to 2010
    -   **Citation for this dataset**: Eric Magar (2012) Gubernatorial Coattails in Mexican Congressional Elections, *The Journal of Politics* 74(2):383-399.
-   `data/prdf2006-on.csv`
    -   **Citation for this dataset**: Eric Magar (2012) Gubernatorial Coattails in Mexican Congressional Elections, *The Journal of Politics* 74(2):383-399.
-   <del>`datosBrutos/` = large directory containing primary sources</del> (dropped from repo due to large size&#x2026; [mail me](mailto:emagar@itam.mx) if you need this).


<a id="org1ea53ef"></a>

# Codebook

Most variables are included in every file, some appear in selected files only.  

-   *edon* = state number 1:32.
-   *edo* = state abbreviation (may differ form the 'official' abbreviations so that sorting them alphabetically preserves the order set by *edon*).
-   *disn* = district number.
-   *emm* = municipal indentifying code (*edo*-electionCycle./munn/).
-   *mun* = municipality.
-   *munn*, *inegi*, *ife* = municipal identifier, reporting the number and the codes used by INEGI and IFE, respectively.
-   *yr*, *mo*, *dy* = year, month, day of the election.
-   *cab* = cabecera, district's administrative center.
-   *circ* = PR district (circunscripcion electoral, 2nd tier).
-   *v01*, *v02*, &#x2026; = raw vote for candidate 1, 2, etc.
-   *l01*, *l02*, &#x2026; = label of candidate 1's, 2's, &#x2026; party or coalition.
-   *c01*, *c02*, &#x2026; = candidate 1's, 2's, &#x2026; name.
-   *s01*, *s02*, &#x2026; = suplente (substitute) for candidate 1, 2, etc.
-   *efec* = effective votes, equal the total raw votes minus votes for write-in candidates and invalid ballots.
-   *nr* = votes for write-in candidates.
-   *nul* = invalid ballots.
-   *tot* = total raw votes.
-   *lisnom* = eligible voters (*lista nominal*).
-   *nota* = notes.
-   *fuente* = source.
-   *ncand* = number of candidates running.
-   *dcoal* = dummy equal 1 if at least one major party candidate ran on a multi-party pre-electoral coalition, 0 otherwise.
-   *coalpan*, *coalpri*, *coalprd* = members of major-party coalitions ('no' indidates no coalition).
-   *imputacion*, *distpan*, *distpri*, *distprd* = when some parties coelesced in such way that only their pooled vote was reported, an attempt is made to infer how many votes each coalition member contributed to team. Variable *imputacion* lists what earlier election was used for this purpose ('no' if none carried); *dist* variables report the share of the coalition total attributable to PAN, PRI, and PRD, respectively. See [this](https://github.com/emagar/replicationMaterial/blob/master/gubCoat/onlineAppendix.pdf) for details.
-   *seyr*, *semo* = year of the previous/concurrent senatorial election.
-   *sepan*, *sepri*, *seprd* = votes won by major parties in previous/concurrent senatorial election.
-   *seefec* = effective votes in previous/concurrent senatorial election.
-   *fake* = indicates fake data for hegemonic era elections, made up of best guesses about what happened in the state's race for the purpose of computing vote lags. Will normally be dropped from analysis.


<a id="orgc2488b5"></a>

# Sources

Work in progress&#x2026;

-   *Fuente* = iee indicates data obtined from the primary source, the state's election board's web site.
-   *Fuente* = tesis Melissa
-   *Fuente* = Mexico Electoral Banamex
-   *Fuente* = prep
-   *Fuente* = Toledo Patiño paper
-   *Fuente* = UAM Iztapalapa
-   *Fuente* = voz y voto


<a id="org9e6918a"></a>

# Acknowledgements

Eric Magar acknowledges financial support from the Asociación Mexicana de Cultura A.C. and CONACYT's Sistema Nacional de Investigadores. He is responsible for mistakes and shortcomings in the data. 

