
# Table of Contents

1.  [Description of *Recent Mexican Election Vote Returns* repository](#orgd87ac14)
2.  [Files in the repository and how to cite them](#org2a381e9)
3.  [Codebook](#org6f14531)
4.  [Coding procedure for the incumbent's status](#org0b79ac1)
5.  [Procedimiento para codificar el estatus del ocupante](#orgd4a279d)
6.  [Sources](#org13c850d)
7.  [Acknowledgements](#org2c3467d)

Last revision: 2021-02-22


<a id="orgd87ac14"></a>

# Description of *Recent Mexican Election Vote Returns* repository

-   Author: Eric Magar
-   Email: emagar at itam dot mx

The repository contains voting data for recent Mexican elections for certain offices at different levels of aggregation. Data has been compiled from many sources. More recent years tend to be coded from official vote returns. Earlier elections tend to be from secondary sources (see Souces section). Data inludes district-level federal deputy vote returns since 1979 and district-level presidential vote returns since 2006; and municipality-level municipal president vote returns (except in the state of Nayarit, votes cast for municipal president also elect a municipal council in a fused ballot). 

*Important note:* older incarnations of this this repository contain LFS (Large File System) files. Make sure to install [LFS](https://git-lfs.github.com/) in your machine before checking out older commits of the repository.


<a id="org2a381e9"></a>

# Files in the repository and how to cite them

You are free to download and modify the data (see the LICENSE document for details) provided you give proper credit to this source. Unless otherwise noted next to the file descriptor, the cite is Eric Magar (2018) Recent Mexican election vote returns repository, <https://github.com/emagar/elecReturns>.

In general, file names identify the office elected (i.e., **df**, **se**, **pr**, **dl**, **go**, **ay** for *diputados federales*, *senadores*, *presidente*, *diputados locales*, *gobernador*, and *ayuntamiento*, respectively), followed by the unit of observation (i.e., **ed**, **df**, **dl**, **mu**, **de**, **se**, **ca** for *estado*, *distrito federal*, *distrito local*, *municipio*, *demarcación*, *sección*, and *casilla* respectively), and the years included. Other than in Nayarit since 2008 (and, pending a court case, Mexico City since 2018), *ayuntamientos* are elected in fused ballots for a *presidente municipal* and a fraction of the municipal council (*regidores* and *síndicos*). Nayarit elects these members of the municipal council in single-member plurality districts called *demarcaciones*.

-   `data/aymu1977-present.csv` = updated to 2018, can be processed with code/ay.r in order to systematize coalitions (ie., aggregate votes when member parties' returns are reported separately and remove redundant columns).
-   `data/aymu1989-present.coalAgg.csv` = pre-processed version of the above (starting in 1989) so that coalition votes appear properly aggregated.
-   `data/aymu1989-present.incumbents.csv` = names of municipal election winning candidates (*presidente municipal* only) since 1989.
-   `data/ayde2008-presentNayRegid.csv` = Nayarit's municipal demarcaciones vote returns since 2008.
-   `code/ay.r` = script to manipulate *ayuntamiento* returns.
-   `code/ayClean.r` = script used to clean *ayuntamiento* returns, should be unnecessary unless new data are added because output has been saved into csv file.
-   `data/dfdf1979-on.csv`
    -   **Citation for this dataset**: Eric Magar, Alejandro Trelles, Micah Altman, and Michael P. McDonald (2017) Components of partisan bias originating from single-member districts in multi-party systems: An application to Mexico, *Political Geography* 57(1):1-12.
-   `data/dfdf1979-on.coalAgg.csv` = pre-processed version of the above so that coalition votes appear properly aggregated.
    -   **Citation for this dataset**: Eric Magar, Alejandro Trelles, Micah Altman, and Michael P. McDonald (2017) Components of partisan bias originating from single-member districts in multi-party systems: An application to Mexico, *Political Geography* 57(1):1-12.
-   `data/dfdf2012-onCandidates.csv` = names of all federal deputy candidates in districts and party lists since 2012.
-   `data/seedcandidates2018.csv` = names of all senatorial candidates in states and party lists in 2018.
-   `data/goed1961-on.csv` = updated to 2010
    -   **Citation for this dataset**: Eric Magar (2012) Gubernatorial Coattails in Mexican Congressional Elections, *The Journal of Politics* 74(2):383-399.
-   `data/prdf2006-on.csv`
    -   **Citation for this dataset**: Eric Magar (2012) Gubernatorial Coattails in Mexican Congressional Elections, *The Journal of Politics* 74(2):383-399.
-   `datosBrutos/` = directory containing selected primary sources. Files for state elections were kept out from the repository due to sizes exceeding github's limit&#x2026; [mail me](mailto:emagar@itam.mx) if you need these.


<a id="org6f14531"></a>

# Codebook

Most variables are included in every file, some appear in selected files only.  

-   *edon* = state number 1:32.
-   *edo* = state abbreviation (may differ from the 'official' abbreviations so that sorting them alphabetically preserves the order set by *edon*).
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
-   *race.after* = incumbent's status in the subsequent race. See [this](#org0265336) for categories and coding procedure ([aquí](#orgd62e835) la versión en español del procedimiento codificador).


<a id="org0b79ac1"></a>

# Coding procedure for the incumbent's status<a id="org0265336"></a>

In file `data/aymu1985-present.incumbents.csv`, variable *race.after* equals one of the following categories: 

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


<a id="orgd4a279d"></a>

# Procedimiento para codificar el estatus del ocupante<a id="orgd62e835"></a>

En el archivo `data/aymu1985-present.incumbents.csv`, la variable *race.after* indica el estatus del ocupante en la elección subsecuente. El estatus puede ser una de las categorías siguientes: 

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


<a id="org13c850d"></a>

# Sources

Work in progress&#x2026;

-   *Fuente* = iee indicates data obtined from the primary source, the state's election board's web site.
-   *Fuente* = tesis Melissa
-   *Fuente* = Mexico Electoral Banamex
-   *Fuente* = prep
-   *Fuente* = Toledo Patiño paper
-   *Fuente* = UAM Iztapalapa
-   *Fuente* = voz y voto


<a id="org2c3467d"></a>

# Acknowledgements

Eric Magar acknowledges financial support from the Asociación Mexicana de Cultura A.C. and CONACYT's Sistema Nacional de Investigadores. He is responsible for mistakes and shortcomings in the data. 

Many students over many years have provided extraordinary research assistance to retrieve and systematize information reported here. 

-   Under construction
-   Eugenio Solís Flores
-   Daniela Guzmán Lerma
-   Mauricio Fernández Duque

