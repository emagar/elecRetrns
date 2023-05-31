####################################################################
## Script for autoregressive vote estimates and alpha regressions ##
## invoked from code/elec-data-for-maps.r                         ##
##                                                                ##
## Author: Eric Magar                                             ##
## emagar at itam dot mx                                          ##
## Date: 17apr2023                                                ##
## Last modified: 30may2023                                       ##
####################################################################

###################################################################
## Note: Search function estim_dis below, that wraps estimation. ##
## Script above that preps time-series data.                     ##
## Script below that saves output and cleans.                    ##
###################################################################

############################################################################
## # backwards estimation/prediction, unit = districts                    ##
## 1988d <- 1991d   1994d   1997d79 2000d79 2003d79                       ##
## 1991d <- 1994d   1997d79 2000d79 2003d79 2006d79                       ##
## 1994d <- 1997d79 2000d79 2003d79 2006d79 2009d79                       ##
## 1997d <- 2000d   2003d   2006d97 2009d97 2012d97                       ##
## 2000d <- 2003d   2006d97 2009d97 2012d97 2015d97                       ##
## 2003d <- 2006d97 2009d97 2012d97 2015d97 2018d97                       ##
## 2006d <- 2009d   2012d   2015d   2018d06 2021d06                       ##
## # forward estimation/prediction, unit = districts                      ##
##          1991d06 1994d06 1997d06 2000d06 2003d06 -> 2006d # no 1991d06 ##
##          1994d06 1997d06 2000d06 2003d06 2006d   -> 2009d              ##
##          1997d06 2000d06 2003d06 2006d   2009d   -> 2012d              ##
##          2000d06 2003d06 2006d   2009d   2012d   -> 2015d              ##
##          2003d18 2006d18 2009d18 2012d18 2015d18 -> 2018d              ##
##          2006d18 2009d18 2012d18 2015d18 2018d   -> 2021d              ##
##          2009d18 2012d18 2015d18 2018d   2021d   -> 2024d18            ##
############################################################################


############################################
## prepare manipulated party objects      ##
## for time-series and alpha regressions  ##
## *** One object per district map ***    ##
############################################
#
# version 1: extend partial coalitions across the board
# shares
pand18 <- data.frame(
    #v91 =  with(v91d18, ifelse(efec==0, NA,  pan               / efec)),
    v94 =  with(v94d18, ifelse(efec==0, NA,  pan               / efec)),
    v97 =  with(v97d18, ifelse(efec==0, NA,  pan               / efec)),
    v00 =  with(v00d18, ifelse(efec==0, NA,  panc              / efec)),
    v03 =  with(v03d18, ifelse(efec==0, NA,  pan               / efec)),
    v06 =  with(v06d18, ifelse(efec==0, NA,  pan               / efec)),
    v09 =  with(v09d18, ifelse(efec==0, NA,  pan               / efec)),
    v12 =  with(v12d18, ifelse(efec==0, NA,  pan               / efec)),
    v15 =  with(v15d18, ifelse(efec==0, NA,  pan               / efec)),
    v18 =  with(v18d,   ifelse(efec==0, NA, (pan + panc + prd) / efec)),  # dropped mc
    v21 =  with(v21d,   ifelse(efec==0, NA, (pan + panc + prd) / efec))   # drop prd?
)
pand06 <- data.frame(
    #v91 =  with(v91d06, ifelse(efec==0, NA,  pan               / efec)), #comment if agg=="s"
    v94 =  with(v94d06, ifelse(efec==0, NA,  pan               / efec)),
    v97 =  with(v97d06, ifelse(efec==0, NA,  pan               / efec)),
    v00 =  with(v00d06, ifelse(efec==0, NA,  panc              / efec)),
    v03 =  with(v03d06, ifelse(efec==0, NA,  pan               / efec)),
    v06 =  with(v06d,   ifelse(efec==0, NA,  pan               / efec)),
    v09 =  with(v09d,   ifelse(efec==0, NA,  pan               / efec)),
    v12 =  with(v12d,   ifelse(efec==0, NA,  pan               / efec)),
    v15 =  with(v15d,   ifelse(efec==0, NA,  pan               / efec)),
    v18 =  with(v18d06, ifelse(efec==0, NA, (pan + panc + prd) / efec)),  # dropped mc
    v21 =  with(v21d06, ifelse(efec==0, NA, (pan + panc + prd) / efec))   # drop prd?
)
pand97 <- data.frame(
    #v91 =  with(v91d97, ifelse(efec==0, NA,  pan               / efec)), # comment if agg=="s"
    v94 =  with(v94d97, ifelse(efec==0, NA,  pan               / efec)),
    v97 =  with(v97d,   ifelse(efec==0, NA,  pan               / efec)),
    v00 =  with(v00d,   ifelse(efec==0, NA,  panc              / efec)),
    v03 =  with(v03d,   ifelse(efec==0, NA,  pan               / efec)),
    v06 =  with(v06d97, ifelse(efec==0, NA,  pan               / efec)),
    v09 =  with(v09d97, ifelse(efec==0, NA,  pan               / efec)),
    v12 =  with(v12d97, ifelse(efec==0, NA,  pan               / efec)),
    v15 =  with(v15d97, ifelse(efec==0, NA,  pan               / efec)),
    v18 =  with(v18d97, ifelse(efec==0, NA, (pan + panc + prd) / efec)),  # dropped mc
    v21 =  with(v21d97, ifelse(efec==0, NA, (pan + panc + prd) / efec))   # drop prd?
)
pand79 <- data.frame(
    v91 =  with(v91d,   ifelse(efec==0, NA,  pan               / efec)), # comment if agg=="s"
    v94 =  with(v94d,   ifelse(efec==0, NA,  pan               / efec)),
    v97 =  with(v97d79, ifelse(efec==0, NA,  pan               / efec)),
    v00 =  with(v00d79, ifelse(efec==0, NA,  panc              / efec)),
    v03 =  with(v03d79, ifelse(efec==0, NA,  pan               / efec)),
    v06 =  with(v06d79, ifelse(efec==0, NA,  pan               / efec)),
    v09 =  with(v09d79, ifelse(efec==0, NA,  pan               / efec)),
    v12 =  with(v12d79, ifelse(efec==0, NA,  pan               / efec)),
    v15 =  with(v15d79, ifelse(efec==0, NA,  pan               / efec)),
    v18 =  with(v18d79, ifelse(efec==0, NA, (pan + panc + prd) / efec)),  # dropped mc
    v21 =  with(v21d79, ifelse(efec==0, NA, (pan + panc + prd) / efec))   # drop prd?
)
pand18 <- round(pand18, 3)
pand06 <- round(pand06, 3)
pand97 <- round(pand97, 3)
pand79 <- round(pand79, 3)
#
prid18 <- data.frame(
    #v91 =  with(v91d18, ifelse(efec==0, NA,  pri                      / efec)),
    v94 =  with(v94d18, ifelse(efec==0, NA,  pri                      / efec)),
    v97 =  with(v97d18, ifelse(efec==0, NA,  pri                      / efec)),
    v00 =  with(v00d18, ifelse(efec==0, NA,  pri                      / efec)),
    v03 =  with(v03d18, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v06 =  with(v06d18, ifelse(efec==0, NA,  pric                     / efec)),
    v09 =  with(v09d18, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v12 =  with(v12d18, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v15 =  with(v15d18, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v18 =  with(v18d,   ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)), # drop pvem + pna?
    v21 =  with(v21d,   ifelse(efec==0, NA,  pri                      / efec))  # coal vote to pan+prd ok?
)
prid06 <- data.frame(
    #v91 =  with(v91d06, ifelse(efec==0, NA,  pri                      / efec)),
    v94 =  with(v94d06, ifelse(efec==0, NA,  pri                      / efec)),
    v97 =  with(v97d06, ifelse(efec==0, NA,  pri                      / efec)),
    v00 =  with(v00d06, ifelse(efec==0, NA,  pri                      / efec)),
    v03 =  with(v03d06, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v06 =  with(v06d,   ifelse(efec==0, NA,  pric                     / efec)),
    v09 =  with(v09d,   ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v12 =  with(v12d,   ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v15 =  with(v15d,   ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v18 =  with(v18d06, ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)), # drop pvem + pna?
    v21 =  with(v21d06, ifelse(efec==0, NA,  pri                      / efec))  # coal vote to pan+prd ok?
)
prid97 <- data.frame(
    #v91 =  with(v91d97, ifelse(efec==0, NA,  pri                      / efec)),
    v94 =  with(v94d97, ifelse(efec==0, NA,  pri                      / efec)),
    v97 =  with(v97d,   ifelse(efec==0, NA,  pri                      / efec)),
    v00 =  with(v00d,   ifelse(efec==0, NA,  pri                      / efec)),
    v03 =  with(v03d,   ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v06 =  with(v06d97, ifelse(efec==0, NA,  pric                     / efec)),
    v09 =  with(v09d97, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v12 =  with(v12d97, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v15 =  with(v15d97, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v18 =  with(v18d97, ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)), # drop pvem + pna?
    v21 =  with(v21d97, ifelse(efec==0, NA,  pri                      / efec))  # coal vote to pan+prd ok?
)
prid79 <- data.frame(
    v91 =  with(v91d,   ifelse(efec==0, NA,  pri                      / efec)),
    v94 =  with(v94d,   ifelse(efec==0, NA,  pri                      / efec)),
    v97 =  with(v97d79, ifelse(efec==0, NA,  pri                      / efec)),
    v00 =  with(v00d79, ifelse(efec==0, NA,  pri                      / efec)),
    v03 =  with(v03d79, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v06 =  with(v06d79, ifelse(efec==0, NA,  pric                     / efec)),
    v09 =  with(v09d79, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v12 =  with(v12d79, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v15 =  with(v15d79, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v18 =  with(v18d79, ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)), # drop pvem + pna?
    v21 =  with(v21d79, ifelse(efec==0, NA,  pri                      / efec))  # coal vote to pan+prd ok?
)
prid79 <- round(prid79, 3)
prid97 <- round(prid97, 3)
prid06 <- round(prid06, 3)
prid18 <- round(prid18, 3)
#
leftd18 <- data.frame(
    #v91 = with(v91d18, ifelse(efec==0, NA,  prd                             / efec)),
    v94 = with(v94d18, ifelse(efec==0, NA,  prd                             / efec)),
    v97 = with(v97d18, ifelse(efec==0, NA,  prd                             / efec)),
    v00 = with(v00d18, ifelse(efec==0, NA,  prdc                            / efec)),
    v03 = with(v03d18, ifelse(efec==0, NA, (prd + pt + conve)               / efec)),
    v06 = with(v06d18, ifelse(efec==0, NA,  prdc                            / efec)),
    v09 = with(v09d18, ifelse(efec==0, NA, (prd + pt + ptc + conve)         / efec)),
    v12 = with(v12d18, ifelse(efec==0, NA, (prd + prdc + pt + mc)           / efec)),
    v15 = with(v15d18, ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
    v18 = with(v18d,   ifelse(efec==0, NA, (morena + morenac + pt + pes)    / efec)),
    v21 = with(v21d,   ifelse(efec==0, NA, (morena + morenac + pt + pvem)   / efec))  # drop pt + pvem?
)
leftd06 <- data.frame(
    #v91 = with(v91d06, ifelse(efec==0, NA,  prd                             / efec)),
    v94 = with(v94d06, ifelse(efec==0, NA,  prd                             / efec)),
    v97 = with(v97d06, ifelse(efec==0, NA,  prd                             / efec)),
    v00 = with(v00d06, ifelse(efec==0, NA,  prdc                            / efec)),
    v03 = with(v03d06, ifelse(efec==0, NA, (prd + pt + conve)               / efec)),
    v06 = with(v06d,   ifelse(efec==0, NA,  prdc                            / efec)),
    v09 = with(v09d,   ifelse(efec==0, NA, (prd + pt + ptc + conve)         / efec)),
    v12 = with(v12d,   ifelse(efec==0, NA, (prd + prdc + pt + mc)           / efec)),
    v15 = with(v15d,   ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
    v18 = with(v18d06, ifelse(efec==0, NA, (morena + morenac + pt + pes)    / efec)),
    v21 = with(v21d06, ifelse(efec==0, NA, (morena + morenac + pt + pvem)   / efec))  # drop pt + pvem?
)
leftd97 <- data.frame(
    #v91 = with(v91d97, ifelse(efec==0, NA,  prd                             / efec)),
    v94 = with(v94d97, ifelse(efec==0, NA,  prd                             / efec)),
    v97 = with(v97d,   ifelse(efec==0, NA,  prd                             / efec)),
    v00 = with(v00d,   ifelse(efec==0, NA,  prdc                            / efec)),
    v03 = with(v03d,   ifelse(efec==0, NA, (prd + pt + conve)               / efec)),
    v06 = with(v06d97, ifelse(efec==0, NA,  prdc                            / efec)),
    v09 = with(v09d97, ifelse(efec==0, NA, (prd + pt + ptc + conve)         / efec)),
    v12 = with(v12d97, ifelse(efec==0, NA, (prd + prdc + pt + mc)           / efec)),
    v15 = with(v15d97, ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
    v18 = with(v18d97, ifelse(efec==0, NA, (morena + morenac + pt + pes)    / efec)),
    v21 = with(v21d97, ifelse(efec==0, NA, (morena + morenac + pt + pvem)   / efec))  # drop pt + pvem?
)
leftd79 <- data.frame(
    v91 = with(v91d,   ifelse(efec==0, NA,  prd                             / efec)),
    v94 = with(v94d,   ifelse(efec==0, NA,  prd                             / efec)),
    v97 = with(v97d79, ifelse(efec==0, NA,  prd                             / efec)),
    v00 = with(v00d79, ifelse(efec==0, NA,  prdc                            / efec)),
    v03 = with(v03d79, ifelse(efec==0, NA, (prd + pt + conve)               / efec)),
    v06 = with(v06d79, ifelse(efec==0, NA,  prdc                            / efec)),
    v09 = with(v09d79, ifelse(efec==0, NA, (prd + pt + ptc + conve)         / efec)),
    v12 = with(v12d79, ifelse(efec==0, NA, (prd + prdc + pt + mc)           / efec)),
    v15 = with(v15d79, ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
    v18 = with(v18d79, ifelse(efec==0, NA, (morena + morenac + pt + pes)    / efec)),
    v21 = with(v21d79, ifelse(efec==0, NA, (morena + morenac + pt + pvem)   / efec))  # drop pt + pvem?
)
leftd79 <- round(leftd79, 3)
leftd97 <- round(leftd97, 3)
leftd06 <- round(leftd06, 3)
leftd18 <- round(leftd18, 3)
#
othd18 <- data.frame(
    #v91 =  with(v91d18, ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt)     / efec)),
    v94 =  with(v94d18, ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
    v97 =  with(v97d18, ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm)               / efec)),
    v00 =  with(v00d18, ifelse(efec==0, NA, (pcd + parm + dsppn)                       / efec)),
    v03 =  with(v03d18, ifelse(efec==0, NA, (psn + pas + mp + plm + fc)                / efec)),
    v06 =  with(v06d18, ifelse(efec==0, NA, (pna + asdc)                               / efec)),
    v09 =  with(v09d18, ifelse(efec==0, NA, (pna + psd)                                / efec)),
    v12 =  with(v12d18, ifelse(efec==0, NA,  pna                                       / efec)),
    v15 =  with(v15d18, ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2)          / efec)),
    v18 =  with(v18d,   ifelse(efec==0, NA, (mc + indep1 + indep2)                     / efec)),
    v21 =  with(v21d,   ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep)             / efec))
)
othd06 <- data.frame(
    #v91 =  with(v91d06, ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt)     / efec)),
    v94 =  with(v94d06, ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
    v97 =  with(v97d06, ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm)               / efec)),
    v00 =  with(v00d06, ifelse(efec==0, NA, (pcd + parm + dsppn)                       / efec)),
    v03 =  with(v03d06, ifelse(efec==0, NA, (psn + pas + mp + plm + fc)                / efec)),
    v06 =  with(v06d,   ifelse(efec==0, NA, (pna + asdc)                               / efec)),
    v09 =  with(v09d,   ifelse(efec==0, NA, (pna + psd)                                / efec)),
    v12 =  with(v12d,   ifelse(efec==0, NA,  pna                                       / efec)),
    v15 =  with(v15d,   ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2)          / efec)),
    v18 =  with(v18d06, ifelse(efec==0, NA, (mc + indep1 + indep2)                     / efec)),
    v21 =  with(v21d06, ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep)             / efec))
)
othd97 <- data.frame(
    #v91 =  with(v91d97, ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt)     / efec)),
    v94 =  with(v94d97, ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
    v97 =  with(v97d,   ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm)               / efec)),
    v00 =  with(v00d,   ifelse(efec==0, NA, (pcd + parm + dsppn)                       / efec)),
    v03 =  with(v03d,   ifelse(efec==0, NA, (psn + pas + mp + plm + fc)                / efec)),
    v06 =  with(v06d97, ifelse(efec==0, NA, (pna + asdc)                               / efec)),
    v09 =  with(v09d97, ifelse(efec==0, NA, (pna + psd)                                / efec)),
    v12 =  with(v12d97, ifelse(efec==0, NA,  pna                                       / efec)),
    v15 =  with(v15d97, ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2)          / efec)),
    v18 =  with(v18d97, ifelse(efec==0, NA, (mc + indep1 + indep2)                     / efec)),
    v21 =  with(v21d97, ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep)             / efec))
)
othd79 <- data.frame(
    v91 =  with(v91d,   ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt)     / efec)),
    v94 =  with(v94d,   ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
    v97 =  with(v97d79, ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm)               / efec)),
    v00 =  with(v00d79, ifelse(efec==0, NA, (pcd + parm + dsppn)                       / efec)),
    v03 =  with(v03d79, ifelse(efec==0, NA, (psn + pas + mp + plm + fc)                / efec)),
    v06 =  with(v06d79, ifelse(efec==0, NA, (pna + asdc)                               / efec)),
    v09 =  with(v09d79, ifelse(efec==0, NA, (pna + psd)                                / efec)),
    v12 =  with(v12d79, ifelse(efec==0, NA,  pna                                       / efec)),
    v15 =  with(v15d79, ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2)          / efec)),
    v18 =  with(v18d79, ifelse(efec==0, NA, (mc + indep1 + indep2)                     / efec)),
    v21 =  with(v21d79, ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep)             / efec))
)
othd79 <- round(othd79, 3)
othd97 <- round(othd97, 3)
othd06 <- round(othd06, 3)
othd18 <- round(othd18, 3)
#
efecd18 <- data.frame(
    #v91 = v91d18$efec,
    v94 = v94d18$efec,
    v97 = v97d18$efec,
    v00 = v00d18$efec,
    v03 = v03d18$efec,
    v06 = v06d18$efec,
    v09 = v09d18$efec,
    v12 = v12d18$efec,
    v15 = v15d18$efec,
    v18 = v18d  $efec,
    v21 = v21d  $efec
)
efecd06 <- data.frame(
    #v91 = v91d06$efec,
    v94 = v94d06$efec,
    v97 = v97d06$efec,
    v00 = v00d06$efec,
    v03 = v03d06$efec,
    v06 = v06d  $efec,
    v09 = v09d  $efec,
    v12 = v12d  $efec,
    v15 = v15d  $efec,
    v18 = v18d06$efec,
    v21 = v21d06$efec
)
efecd97 <- data.frame(
    #v91 = v91d97$efec,
    v94 = v94d97$efec,
    v97 = v97d  $efec,
    v00 = v00d  $efec,
    v03 = v03d  $efec,
    v06 = v06d97$efec,
    v09 = v09d97$efec,
    v12 = v12d97$efec,
    v15 = v15d97$efec,
    v18 = v18d97$efec,
    v21 = v21d97$efec
)
efecd79 <- data.frame(
    v91 = v91d  $efec,
    v94 = v94d  $efec,
    v97 = v97d79$efec,
    v00 = v00d79$efec,
    v03 = v03d79$efec,
    v06 = v06d79$efec,
    v09 = v09d79$efec,
    v12 = v12d79$efec,
    v15 = v15d79$efec,
    v18 = v18d79$efec,
    v21 = v21d79$efec
)
#
lisnomd18 <- data.frame(
    #v91 = v91d18$lisnom,
    v94 = v94d18$lisnom,
    v97 = v97d18$lisnom,
    v00 = v00d18$lisnom,
    v03 = v03d18$lisnom,
    v06 = v06d18$lisnom,
    v09 = v09d18$lisnom,
    v12 = v12d18$lisnom,
    v15 = v15d18$lisnom,
    v18 = v18d  $lisnom,
    v21 = v21d  $lisnom
)
lisnomd06 <- data.frame(
    #v91 = v91d06$lisnom,
    v94 = v94d06$lisnom,
    v97 = v97d06$lisnom,
    v00 = v00d06$lisnom,
    v03 = v03d06$lisnom,
    v06 = v06d  $lisnom,
    v09 = v09d  $lisnom,
    v12 = v12d  $lisnom,
    v15 = v15d  $lisnom,
    v18 = v18d06$lisnom,
    v21 = v21d06$lisnom
)
lisnomd97 <- data.frame(
    #v91 = v91d97$lisnom,
    v94 = v94d97$lisnom,
    v97 = v97d  $lisnom,
    v00 = v00d  $lisnom,
    v03 = v03d  $lisnom,
    v06 = v06d97$lisnom,
    v09 = v09d97$lisnom,
    v12 = v12d97$lisnom,
    v15 = v15d97$lisnom,
    v18 = v18d97$lisnom,
    v21 = v21d97$lisnom
)
lisnomd79 <- data.frame(
    v91 = v91d  $lisnom,
    v94 = v94d  $lisnom,
    v97 = v97d79$lisnom,
    v00 = v00d79$lisnom,
    v03 = v03d79$lisnom,
    v06 = v06d79$lisnom,
    v09 = v09d79$lisnom,
    v12 = v12d79$lisnom,
    v15 = v15d79$lisnom,
    v18 = v18d79$lisnom,
    v21 = v21d79$lisnom
)

# transpose to plug columns (units) into new time-series data.frames
pand79    <- t(pand79)
pand97    <- t(pand97)
pand06    <- t(pand06)
pand18    <- t(pand18)
prid79    <- t(prid79)
prid97    <- t(prid97)
prid06    <- t(prid06)
prid18    <- t(prid18)
leftd79   <- t(leftd79)
leftd97   <- t(leftd97)
leftd06   <- t(leftd06)
leftd18   <- t(leftd18)
othd79    <- t(othd79)
othd97    <- t(othd97)
othd06    <- t(othd06)
othd18    <- t(othd18)
efecd79   <- t(efecd79)
efecd97   <- t(efecd97)
efecd06   <- t(efecd06)
efecd18   <- t(efecd18)
lisnomd79 <- t(lisnomd79)
lisnomd97 <- t(lisnomd97)
lisnomd06 <- t(lisnomd06)
lisnomd18 <- t(lisnomd18)

#################################################################
## extendCoal.. will receive data for regressions, one per map ##
#################################################################
extendCoald79 <- extendCoald97 <- extendCoald06 <- extendCoald18 <- as.list(rep(NA, 300))
#
# loop over districts
for (i in 1:300){
    #i <- 81 # debug
    message(sprintf("loop %s of %s", i, nrow(v00d)))
    #########################
    ## votes with 1979 map ##
    #########################
    tmp <- data.frame(yr     = seq(from=1991, to=2021, by=3),
                      pan    = pand79[,i],
                      pri    = prid79[,i],
                      left   = leftd79[,i],
                      oth    = othd79[,i],
                      efec   = efecd79[,i],
                      lisnom = lisnomd79[,i])
    # replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan   [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri   [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left  [is.na(tmp$left)] <- per.means["left"];
        tmp$oth   [is.na(tmp$oth)]  <- per.means["oth"];
        tmp$efec  [is.na(tmp$efec)   | tmp$efec==0]   <- 1
        tmp$lisnom[is.na(tmp$lisnom) | tmp$lisnom==0] <- 2
    }
    # add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    # re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    # add id
    tmp$disn    <- v94d$disn[i]
    # fill info to new list
    extendCoald79[[i]] <- tmp
    # name list object
    names(extendCoald79)[i] <- tmp$disn[1]
    #
    #########################
    ## votes with 1997 map ##
    #########################
    tmp <- data.frame(yr     = seq(from=1994, to=2021, by=3),
                      pan    = pand97[,i],
                      pri    = prid97[,i],
                      left   = leftd97[,i],
                      oth    = othd97[,i],
                      efec   = efecd97[,i],
                      lisnom = lisnomd97[,i])
    tmp <- rbind(v91=c(1991,NA,NA,NA,NA,NA,NA), tmp) # add 1991 with no counterfactuals
    # replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan   [is.na(tmp$pan)]   <- per.means["pan"];
        tmp$pri   [is.na(tmp$pri)]   <- per.means["pri"];
        tmp$left  [is.na(tmp$left)]  <- per.means["left"];
        tmp$oth   [is.na(tmp$oth)]   <- per.means["oth"];
        tmp$efec  [is.na(tmp$efec)   | tmp$efec==0]   <- 1
        tmp$lisnom[is.na(tmp$lisnom) | tmp$lisnom==0] <- 2
    }
    # add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    # re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    # add id
    tmp$disn    <- v97d$disn[i]
    # fill info to new list
    extendCoald97[[i]] <- tmp
    # name list object
    names(extendCoald97)[i] <- tmp$disn[1]
    #
    #########################
    ## votes with 2006 map ##
    #########################
    tmp <- data.frame(yr   = seq(from=1994, to=2021, by=3),
                      pan  = pand06[,i],
                      pri  = prid06[,i],
                      left = leftd06[,i],
                      oth  = othd06[,i],
                      efec = efecd06[,i],
                      lisnom = lisnomd06[,i])
    tmp <- rbind(v91=c(1991,NA,NA,NA,NA,NA,NA), tmp) # add 1991 with no counterfactuals
    # replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan   [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri   [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left  [is.na(tmp$left)] <- per.means["left"];
        tmp$oth   [is.na(tmp$oth)]  <- per.means["oth"];
        tmp$efec  [is.na(tmp$efec)   | tmp$efec==0]   <- 1
        tmp$lisnom[is.na(tmp$lisnom) | tmp$lisnom==0] <- 2
    }
    # add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    # re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    # add id
    tmp$disn    <- v06d$disn[i]
    # fill info to new list
    extendCoald06[[i]] <- tmp
    # name list object
    names(extendCoald06)[i] <- tmp$disn[1]
    #
    #########################
    ## votes with 2018 map ##
    #########################
    tmp <- data.frame(yr   = seq(from=1994, to=2021, by=3),
                      pan  = pand18[,i],
                      pri  = prid18[,i],
                      left = leftd18[,i],
                      oth  = othd18[,i],
                      efec = efecd18[,i],
                      lisnom = lisnomd18[,i])
    tmp <- rbind(v91=c(1991,NA,NA,NA,NA,NA,NA), tmp) # add 1991 with no counterfactuals
    # replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan   [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri   [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left  [is.na(tmp$left)] <- per.means["left"];
        tmp$oth   [is.na(tmp$oth)]  <- per.means["oth"];
        tmp$efec  [is.na(tmp$efec)   | tmp$efec==0]   <- 1
        tmp$lisnom[is.na(tmp$lisnom) | tmp$lisnom==0] <- 2
    }
    # add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    # re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    # add id
    tmp$disn    <- v18d$disn[i]
    # fill info to new list
    extendCoald18[[i]] <- tmp
    # name list object
    names(extendCoald18)[i] <- tmp$disn[1]
}
# clean
rm(pand79,prid79,leftd79,othd79,efecd79,lisnomd79)
rm(pand97,prid97,leftd97,othd97,efecd97,lisnomd97)
rm(pand06,prid06,leftd06,othd06,efecd06,lisnomd06)
rm(pand18,prid18,leftd18,othd18,efecd18,lisnomd18)

##################################
## datos para regresión de alfa ##
##################################
#
#############################################################################################################
## Nota 16jul2021: al añadir datos 2021 cambiarán todas las alfas y betas! Una solución fácil (que usaré)  ##
## y otra que requiere más coco. La fácil es reestimar con 2021 e identificar el commit que reportaba la   ##
## versión hasta 2018. La otra: definir una ventana temporal (como cinco elecciones) para producir alfas   ##
## y betas cada elección: alfa.2006, alfa.2009, etc. Debería poder investigar cómo usan esto en el Capital ##
## Asset Pricing Model...                                                                                  ##
#############################################################################################################
yr.means <- data.frame(yr = seq(1991,2021,3), # 11 election-years
                       pan    = rep(NA,11),
                       pri    = rep(NA,11),
                       left   = rep(NA,11),
                       oth    = rep(NA,11))
# function to sum numeric columns
cs <- function(x){
    sel.nums <- unlist(lapply(x, is.numeric), use.names = FALSE) # selects only numeric columns in data frame
    res <- colSums(x[,sel.nums], na.rm=TRUE)
    return(res)
}
#
# compute national mean vote
yr.means$pan [1] <-  cs(v91s)["pan"]  /  cs(v91s)["efec"]
yr.means$pri [1] <-  cs(v91s)["pri"]  /  cs(v91s)["efec"]
yr.means$left[1] <-  cs(v91s)["prd"]  /  cs(v91s)["efec"]
yr.means$oth [1] <-  with(yr.means[1,], 1 - pan - pri - left)
#
yr.means$pan [2] <-  cs(v94s)["pan"]  / cs(v94s)["efec"]
yr.means$pri [2] <-  cs(v94s)["pri"]  / cs(v94s)["efec"]
yr.means$left[2] <-  cs(v94s)["prd"]  / cs(v94s)["efec"]
yr.means$oth [2] <-  with(yr.means[2,], 1 - pan - pri - left)
#                
yr.means$pan [3] <-  cs(v97s)["pan"]  / cs(v97s)["efec"]
yr.means$pri [3] <-  cs(v97s)["pri"]  / cs(v97s)["efec"]
yr.means$left[3] <-  cs(v97s)["prd"]  / cs(v97s)["efec"]
yr.means$oth [3] <-  with(yr.means[3,], 1 - pan - pri - left)
#                
yr.means$pan [4] <-  cs(v00s)["panc"]  / cs(v00s)["efec"]
yr.means$pri [4] <-  cs(v00s)["pri"]   / cs(v00s)["efec"]
yr.means$left[4] <-  cs(v00s)["prdc"]  / cs(v00s)["efec"]
yr.means$oth [4] <-  with(yr.means[4,], 1 - pan - pri - left)
#                
yr.means$pan [5] <-  cs(v03s)["pan"]                                         / cs(v03s)["efec"]
yr.means$pri [5] <- (cs(v03s)["pri"] + cs(v03s)["pric"] + cs(v03s)["pvem"])  / cs(v03s)["efec"]
yr.means$left[5] <- (cs(v03s)["prd"] + cs(v03s)["pt"]   + cs(v03s)["conve"]) / cs(v03s)["efec"]
yr.means$oth [5] <-  with(yr.means[5,], 1 - pan - pri - left)
#                
yr.means$pan [6] <-  cs(v06s)["pan"]   / cs(v06s)["efec"]
yr.means$pri [6] <-  cs(v06s)["pric"]  / cs(v06s)["efec"]
yr.means$left[6] <-  cs(v06s)["prdc"]  / cs(v06s)["efec"]
yr.means$oth [6] <-  with(yr.means[6,], 1 - pan - pri - left)
#                
yr.means$pan [7] <-  cs(v09s)["pan"]                                                           / cs(v09s)["efec"]
yr.means$pri [7] <- (cs(v09s)["pri"] + cs(v09s)["pric"] + cs(v09s)["pvem"])                    / cs(v09s)["efec"]
yr.means$left[7] <- (cs(v09s)["prd"] + cs(v09s)["pt"]   + cs(v09s)["ptc"] + cs(v09s)["conve"]) / cs(v09s)["efec"]
yr.means$oth [7] <-  with(yr.means[7,], 1 - pan - pri - left)
#                
yr.means$pan [8] <-  cs(v12s)["pan"]                                                       / cs(v12s)["efec"]
yr.means$pri [8] <- (cs(v12s)["pri"] + cs(v12s)["pric"] + cs(v12s)["pvem"])                / cs(v12s)["efec"]
yr.means$left[8] <- (cs(v12s)["prd"] + cs(v12s)["prdc"] + cs(v12s)["pt"] + cs(v12s)["mc"]) / cs(v12s)["efec"]
yr.means$oth [8] <-  with(yr.means[8,], 1 - pan - pri - left)
#                
yr.means$pan [9] <-  cs(v15s)["pan"]                                                            / cs(v15s)["efec"]
yr.means$pri [9] <- (cs(v15s)["pri"] + cs(v15s)["pric"] + cs(v15s)["pvem"])                     / cs(v15s)["efec"]
yr.means$left[9] <- (cs(v15s)["prd"] + cs(v15s)["prdc"] + cs(v15s)["pt"] + cs(v15s)["morena"] ) / cs(v15s)["efec"] # dropped cs(v15s)["pes"]
yr.means$oth [9] <-  with(yr.means[9,], 1 - pan - pri - left)
#
yr.means$pan [10] <- (cs(v18s)["pan"]    + cs(v18s)["panc"]    + cs(v18s)["prd"]  + cs(v18s)["mc"])  / cs(v18s)["efec"]
yr.means$pri [10] <- (cs(v18s)["pri"]    + cs(v18s)["pric"]    + cs(v18s)["pvem"] + cs(v18s)["pna"]) / cs(v18s)["efec"]
yr.means$left[10] <- (cs(v18s)["morena"] + cs(v18s)["morenac"] + cs(v18s)["pt"]   + cs(v18s)["pes"]) / cs(v18s)["efec"]
yr.means$oth [10] <-  with(yr.means[10,], 1 - pan - pri - left)
#
yr.means$pan [11] <- (cs(v21s)["pan"]    + cs(v21s)["panc"]    + cs(v21s)["prd"])                    / cs(v21s)["efec"]
yr.means$pri [11] <-  cs(v21s)["pri"]                                                                / cs(v21s)["efec"] # dropped cs(v21s)["pric"]
yr.means$left[11] <- (cs(v21s)["morena"] + cs(v21s)["morenac"] + cs(v21s)["pt"]  + cs(v21s)["pvem"]) / cs(v21s)["efec"]
yr.means$oth [11] <-  with(yr.means[11,], 1 - pan - pri - left)
#
#############################
## votes relative to pri's ##
#############################
yr.means <- within(yr.means, mean.rpan    <- pan  / pri)
yr.means <- within(yr.means, mean.rleft   <- left / pri)
yr.means <- within(yr.means, mean.roth    <- oth  / pri)
#
yr.means[,2:8] <- round(yr.means[,2:8], 3)
#
# plug into data
for (i in 1:300){
    #i <- 2 # debug
    extendCoald18[[i]] <- cbind(extendCoald18[[i]], yr.means[,6:8])
    extendCoald06[[i]] <- cbind(extendCoald06[[i]], yr.means[,6:8])
    extendCoald97[[i]] <- cbind(extendCoald97[[i]], yr.means[,6:8])
    extendCoald79[[i]] <- cbind(extendCoald79[[i]], yr.means[,6:8])
}

#################################################################################################
## - should also try jags estimation to get post-sample of vhats and alphas below              ##
## - report mg effect of unit change in bar(v) at year's level instead of betahat (cf. Linzer) ##
#################################################################################################

###############################
## código de las regresiones ##
###############################
vhat.2024 <-                 # <--- prospective, with up-to 2021 returns, assumes no redistricting in 2024
vhat.2021 <- vhat.2018 <- vhat.2015 <- vhat.2012 <- vhat.2009 <- vhat.2006 <- 
vhat.2003 <- vhat.2000 <- vhat.1997 <- vhat.1994 <-
vhat.1991 <- vhat.1988 <-
    data.frame(pan       = rep(NA, 300),
               pri       = rep(NA, 300),
               left      = rep(NA, 300), # will receive vote estimates
               dbackward = rep(NA, 300)) # will indicate backwards predictions
#
alphahat <- data.frame(pan    = rep(NA, 300),
                       pri    = rep(NA, 300),
                       left   = rep(NA, 300)) # will receive municipio's alphas
betahat <- data.frame (pan    = rep(NA, 300),
                       left   = rep(NA, 300),
                       oth    = rep(NA, 300)) # will receive municipio's betas (none for pri)
#
tmp <- as.list(rep(NA, 300)) # empty list will receive one time-series
                                    # regression per unit, each used to
                                    # predict votes in 2006:2024
# add district names---must be done by map due to redistricting
tmp18 <- tmp06 <- tmp97 <- tmp79 <- tmp
names(tmp79) <- v94d$disn
names(tmp97) <- v97d$disn
names(tmp06) <- v06d$disn
names(tmp18) <- v18d$disn
#
regs.1988 <- regs.1991 <- regs.1994 <- 
    list(pan    = tmp79,
         left   = tmp79,
         oth    = tmp79,
         readme = "No pri regs because DVs are pri-ratios")
regs.1997 <- regs.2000 <- regs.2003 <- 
    list(pan    = tmp97,
         left   = tmp97,
         oth    = tmp97,
         readme = "No pri regs because DVs are pri-ratios")
regs.2006 <- regs.2009 <- regs.2012 <- regs.2015 <- 
    list(pan    = tmp06,
         left   = tmp06,
         oth    = tmp06,
         readme = "No pri regs because DVs are pri-ratios")
regs.2018 <- regs.2021 <- regs.2024 <-  
    list(pan    = tmp18,
         left   = tmp18,
         oth    = tmp18,
         readme = "No pri regs because DVs are pri-ratios")
# for districts, one mean.reg per map
mean.regs.d79 <-
    list(pan    = tmp79,
         left   = tmp79,
         oth    = tmp79,
         readme = "No pri regs bec DVs are pri-ratios")
mean.regs.d97 <-
    list(pan    = tmp97,
         left   = tmp97,
         oth    = tmp97,
         readme = "No pri regs bec DVs are pri-ratios")
mean.regs.d06 <-
    list(pan    = tmp06,
         left   = tmp06,
         oth    = tmp06,
         readme = "No pri regs bec DVs are pri-ratios")
mean.regs.d18 <-
    list(pan    = tmp18,
         left   = tmp18,
         oth    = tmp18,
         readme = "No pri regs bec DVs are pri-ratios")
rm(tmp,tmp79,tmp97,tmp06,tmp18)


###############################################################
## District 5-yr estimates that can be computed before 2024  ##
## |      | map    |        |        |       |               ##
## | vhat | 1979   | 1997   | 2006   | 2018  |               ##
## |------+--------+--------+--------+-------|               ##
## | 1988 | *back* |   XX   |   XX   |   XX  |               ##
## | 1991 | *back* |  back  |  back  |  back |               ##
## | 1994 | *back* |  back  |  back  |  back |               ##
## | 1997 |  back  | *back* |  back  |  back |               ##
## | 2000 |  back  | *back* |  back  |  back |               ##
## | 2003 |  BOTH  | *back* |  back  |  back |               ##
## | 2006 |  BOTH  |  back  | *back* |  back |               ##
## | 2009 |  fwd   |  fwd   | *fwd*  |  fwd  |               ##
## | 2012 |  fwd   |  fwd   | *fwd*  |  fwd  |               ##
## | 2015 |  fwd   |  fwd   | *fwd*  |  fwd  |               ##
## | 2018 |  fwd   |  fwd   |  fwd   | *fwd* |               ##
## | 2021 |  fwd   |  fwd   |  fwd   | *fwd* |               ##
## | 2024 |  fwd   |  fwd   |  fwd   | *fwd* |               ##
## Notes:                                                    ##
## - Starred are needed                                      ##
## - get 2006 estimates backwards                            ##
## - use 1979 BOTH to compare 2003 and 2006 fwd/backwd vhats ##
###############################################################


#############################################
## wrap district estimations in a function ##
#############################################
estim_dis <- function(sel.map){
    #
    #######################################################################
    ## read 1988dfdf from external file to add votes when using 1979 map ##
    #######################################################################
    if (sel.map==1979){
        tmp <- read.csv(file="../dfdf1979-on.csv")
        tmp <- tmp[tmp$yr==1988,]
        tmp.v <- tmp[, grep("^v[0-9]{2}$", colnames(tmp))]
        tmp.l <- tmp[, grep("^l[0-9]{2}$", colnames(tmp))]
        tmp$efec <- rowSums(tmp.v, na.rm=TRUE)               # recompute efec
        tmp.v <- round(tmp.v / tmp$efec, 3)                  # shares
        sel.c <- apply(tmp.l, 1, function(x) which(x=="pan"))
        tmp$pan <- apply(cbind(tmp.v,sel.c=sel.c), 1, function(x) x[x["sel.c"]])
        sel.c <- apply(tmp.l, 1, function(x) which(x=="pri"))
        tmp$pri <- apply(cbind(tmp.v,sel.c=sel.c), 1, function(x) x[x["sel.c"]])
        tmp$left <- NA # left will sum fdn (not prt)
        sel.c <- apply(tmp.l, 1, function(x) which(x=="pdm"))
        tmp$oth <- apply(cbind(tmp.v,sel.c=sel.c), 1, function(x) x[x["sel.c"]])
        sel.c <- apply(tmp.l, 1, function(x) which(x=="prt"))
        tmp$oth2 <- apply(cbind(tmp.v,sel.c=sel.c), 1, function(x) x[x["sel.c"]])
        tmp$oth <- tmp$oth+tmp$oth2; tmp$oth2 <- NULL 
        tmp <- within(tmp, left <- 1 - pan - pri - oth)
        tmp <- tmp[,c("disn","pan","pri","left","oth","efec","lisnom")]
        tmp.1988 <- tmp
        rm(tmp,sel.c,tmp.v,tmp.l)
        #
        # if map is 1979 will estimate 1988 backwards, add row in data frames
        add1988 <- function(x) rbind(v88=c(1988,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA), x)
        tmp <- extendCoald79 # duplicate for manipulation
        ## OJO: should replace NAs above with 1988 district returns in file
        tmp <- lapply(extendCoald79, add1988) # add row for 1988 to each data frame in list
        # add corresponding 1988 shares
        table(as.numeric(names(tmp))==tmp.1988$disn) # check items in same order 
        data.frame(ext=as.numeric(names(tmp)), v88=tmp.1988$disn) # check items in same order 
        for (i in 1:300){
            tmp[[i]][1,c("pan","pri","left","oth","efec","lisnom","disn")] <-
                tmp.1988[i,c("pan","pri","left","oth","efec","lisnom","disn")] # fill 1st row w ith obs
        }
        extendCoald79 <- tmp
        rm(add1988,tmp,tmp.1988)
    }
    #
    if (sel.map==2018){
        # if map is 2018 will estimate 2024, add row in data frames
        # this block will change to 2027 after 24's election
        add2024 <- function(x) rbind(x, v24=c(2024,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
        tmp <- extendCoald18 # duplicate for manipulation
        tmp <- lapply(extendCoald18, add2024) # add row for 2024 to each data frame in list
        extendCoald18 <- tmp
        rm(add2024,tmp)
    }
    #
    for (i in 1:300){  # loop over ditricts
        #i <- 81 # debug
        message(sprintf("loop %s of %s", i, 300))
        # subset data to single unit
        if (sel.map==1979) data.tmp <- extendCoald79[[i]]
        if (sel.map==1997) data.tmp <- extendCoald97[[i]]
        if (sel.map==2006) data.tmp <- extendCoald06[[i]]
        if (sel.map==2018) data.tmp <- extendCoald18[[i]]
        #data.tmp <- extendCoal[[i]]
        ##
        ## add first-differences
        tmp.ln <- nrow(data.tmp)
        data.tmp$d.pan    <- data.tmp$pan    - c(NA, data.tmp$pan   [-tmp.ln])
        data.tmp$d.pri    <- data.tmp$pri    - c(NA, data.tmp$pri   [-tmp.ln])
        data.tmp$d.left   <- data.tmp$left   - c(NA, data.tmp$left  [-tmp.ln])
        rm(tmp.ln)
        ##
        ############################################
        ## backwards-predict 1988 with next 5 els ##
        ############################################
        if (sel.map==1979){
            tmp.back <- 1 # will indicate backwards prediction
            year <- 1988
            reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            #
            new.d <- data.frame(yr = year)
            rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
            rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
            rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
            vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left + rhat.oth), 3)
            vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left + rhat.oth), 3)
            vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left + rhat.oth), 3)
            bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
            bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
            #
            ## plug into results objects ##
            vhat.1988[i,] <- c(vhat.pan, vhat.pri, vhat.left, tmp.back)
            regs.1988$pan [[i]]   <- reg.pan
            regs.1988$left[[i]]   <- reg.left
            regs.1988$oth [[i]]   <- reg.oth
            #
            # add slot for projections/estimates if absent
            if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA 
            if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA 
            if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA
            if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA 
            if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA
            if ("dbackward" %notin% colnames(data.tmp)) data.tmp$dbackward <- NA
            #
            data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
            data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
            data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
            data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
            data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
            data.tmp$dbackward  [data.tmp$yr==year] <- tmp.back   # input fwd/back
        }
        #
        ############################################
        ## backwards-predict 1991 with next 5 els ##
        ############################################
        if (sel.map==1979){
            tmp.back <- 1 # will indicate backwards prediction
            year <- 1991
            reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            #
            new.d <- data.frame(yr = year)
            rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
            rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
            rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
            vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
            bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
            #
            ## plug into results objects ##
            vhat.1991[i,] <- c(vhat.pan, vhat.pri, vhat.left, tmp.back)
            regs.1991$pan [[i]]   <- reg.pan
            regs.1991$left[[i]]   <- reg.left
            regs.1991$oth [[i]]   <- reg.oth
            #
            # add slot for projections/estimates if absent
            if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA 
            if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA 
            if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA
            if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA 
            if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA
            if ("dbackward" %notin% colnames(data.tmp)) data.tmp$dbackward <- NA
            #
            data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
            data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
            data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
            data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
            data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
            data.tmp$dbackward  [data.tmp$yr==year] <- tmp.back   # input fwd/back
        }
        #
        ############################################
        ## backwards-predict 1994 with next 5 els ##
        ############################################
        if (sel.map==1979){
            tmp.back <- 1 # will indicate backwards prediction
            year <- 1994
            reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            #
            new.d <- data.frame(yr = year)
            rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
            rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
            rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
            vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
            bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
            #
            ## plug into results objects ##
            vhat.1994[i,] <- c(vhat.pan, vhat.pri, vhat.left, tmp.back)
            regs.1994$pan [[i]]   <- reg.pan
            regs.1994$left[[i]]   <- reg.left
            regs.1994$oth [[i]]   <- reg.oth
            #
            # add slot for projections/estimates if absent
            if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA 
            if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA 
            if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA
            if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA 
            if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA
            if ("dbackward" %notin% colnames(data.tmp)) data.tmp$dbackward <- NA
            #
            data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
            data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
            data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
            data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
            data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
            data.tmp$dbackward  [data.tmp$yr==year] <- tmp.back   # input fwd/back
        }
        #
        ############################################
        ## backwards-predict 1997 with next 5 els ##
        ############################################
        if (sel.map==1997){
            tmp.back <- 1 # will indicate backwards prediction
            year <- 1997
            reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            #
            new.d <- data.frame(yr = year)
            rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
            rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
            rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
            vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
            bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
            #
            ## plug into results objects ##
            vhat.1997[i,] <- c(vhat.pan, vhat.pri, vhat.left, tmp.back)
            regs.1997$pan [[i]]   <- reg.pan
            regs.1997$left[[i]]   <- reg.left
            regs.1997$oth [[i]]   <- reg.oth
            #
            # add slot for projections/estimates if absent
            if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA 
            if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA 
            if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA
            if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA 
            if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA
            if ("dbackward" %notin% colnames(data.tmp)) data.tmp$dbackward <- NA
            #
            data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
            data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
            data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
            data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
            data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
            data.tmp$dbackward  [data.tmp$yr==year] <- tmp.back   # input fwd/back
        }
        #
        ############################################
        ## backwards-predict 2000 with next 5 els ##
        ############################################
        if (sel.map==1997){
            tmp.back <- 1 # will indicate backwards prediction
            year <- 2000
            reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            #
            new.d <- data.frame(yr = year)
            rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
            rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
            rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
            vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
            bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
            #
            ## plug into results objects ##
            vhat.2000[i,] <- c(vhat.pan, vhat.pri, vhat.left, tmp.back)
            regs.2000$pan [[i]]   <- reg.pan
            regs.2000$left[[i]]   <- reg.left
            regs.2000$oth [[i]]   <- reg.oth
            #
            # add slot for projections/estimates if absent
            if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA 
            if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA 
            if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA
            if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA 
            if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA
            if ("dbackward" %notin% colnames(data.tmp)) data.tmp$dbackward <- NA
            #
            data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
            data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
            data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
            data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
            data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
            data.tmp$dbackward  [data.tmp$yr==year] <- tmp.back   # input fwd/back
        }
        #
        ############################################
        ## backwards-predict 2003 with next 5 els ##
        ############################################
        if (sel.map==1997){
            tmp.back <- 1 # will indicate backwards prediction
            year <- 2003
            reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            #
            new.d <- data.frame(yr = year)
            rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
            rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
            rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
            vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
            bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
            #
            ## plug into results objects ##
            vhat.2003[i,] <- c(vhat.pan, vhat.pri, vhat.left, tmp.back)
            regs.2003$pan [[i]]   <- reg.pan
            regs.2003$left[[i]]   <- reg.left
            regs.2003$oth [[i]]   <- reg.oth
                                        #
                                        # add slot for projections/estimates if absent
            if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA 
            if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA 
            if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA
            if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA 
            if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA
            if ("dbackward" %notin% colnames(data.tmp)) data.tmp$dbackward <- NA
                                        #
            data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
            data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
            data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
            data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
            data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
            data.tmp$dbackward  [data.tmp$yr==year] <- tmp.back   # input fwd/back
        }
                                        #
        ############################################
        ## backwards-predict 2006 with next 5 els ##
        ############################################
        if (sel.map==2006){
            tmp.back <- 1 # will indicate backwards prediction
            year <- 2006
            reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
            reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
                                        #
            new.d <- data.frame(yr = year)
            rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
            rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
            rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
            vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
            bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
                                        #
            ## plug into results objects ##
            vhat.2006[i,] <- c(vhat.pan, vhat.pri, vhat.left, tmp.back)
            regs.2006$pan [[i]]   <- reg.pan
            regs.2006$left[[i]]   <- reg.left
            regs.2006$oth [[i]]   <- reg.oth
                                        #
            # add slot for projections/estimates if absent
            if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA 
            if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA 
            if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA
            if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA 
            if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA
            if ("dbackward" %notin% colnames(data.tmp)) data.tmp$dbackward <- NA
                                        #
            data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
            data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
            data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
            data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
            data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
            data.tmp$dbackward  [data.tmp$yr==year] <- tmp.back   # input fwd/back
        }
                                        #
        ##################################
        ## predict 2009 with last 5 els ##
        ##################################
        if (sel.map==2006){
            tmp.back <- 0 # will indicate forward prediction
            year <- 2009
            reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
            reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
            reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
                                        #
            new.d <- data.frame(yr = year)
            rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
            rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
            rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
            vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
            bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
                                        #
            ## plug into results objects ##
            vhat.2009[i,] <- c(vhat.pan, vhat.pri, vhat.left, tmp.back)
            regs.2009$pan [[i]]   <- reg.pan
            regs.2009$left[[i]]   <- reg.left
            regs.2009$oth [[i]]   <- reg.oth
                                        #
            # add slot for projections/estimates if absent
            if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA 
            if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA 
            if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA
            if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA 
            if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA
            if ("dbackward" %notin% colnames(data.tmp)) data.tmp$dbackward <- NA
                                        #
            data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
            data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
            data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
            data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
            data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
            data.tmp$dbackward  [data.tmp$yr==year] <- tmp.back   # input fwd/back
        }
                                        #
        ##################################
        ## predict 2012 with last 5 els ##
        ##################################
        if (sel.map==2006){
            tmp.back <- 0 # will indicate forward prediction
            year <- 2012
            reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
            reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
            reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
                                        #
            new.d <- data.frame(yr = year)
            rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
            rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
            rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
            vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
            bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
                                        #
            ## plug into results objects ##
            vhat.2012[i,] <- c(vhat.pan, vhat.pri, vhat.left, tmp.back)
            regs.2012$pan [[i]]   <- reg.pan
            regs.2012$left[[i]]   <- reg.left
            regs.2012$oth [[i]]   <- reg.oth
                                        #
            # add slot for projections/estimates if absent
            if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA 
            if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA 
            if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA
            if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA 
            if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA
            if ("dbackward" %notin% colnames(data.tmp)) data.tmp$dbackward <- NA
                                        #
            data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
            data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
            data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
            data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
            data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
            data.tmp$dbackward  [data.tmp$yr==year] <- tmp.back   # input fwd/back
        }
                                        #
        ##################################
        ## predict 2015 with last 5 els ##
        ##################################
        if (sel.map==2006){
            tmp.back <- 0 # will indicate forward prediction
            year <- 2015
            reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
            reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
            reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
                                        #
            new.d <- data.frame(yr = year)
            rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
            rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
            rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
            vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
            bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
                                        #
            ## plug into results objects ##
            vhat.2015[i,] <- c(vhat.pan, vhat.pri, vhat.left, tmp.back)
            regs.2015$pan [[i]]   <- reg.pan
            regs.2015$left[[i]]   <- reg.left
            regs.2015$oth [[i]]   <- reg.oth
                                        #
            # add slot for projections/estimates if absent
            if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA 
            if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA 
            if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA
            if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA 
            if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA
            if ("dbackward" %notin% colnames(data.tmp)) data.tmp$dbackward <- NA
                                        #
            data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
            data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
            data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
            data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
            data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
            data.tmp$dbackward  [data.tmp$yr==year] <- tmp.back   # input fwd/back
        }
                                        #
        ##################################
        ## predict 2018 with last 5 els ##
        ##################################
        if (sel.map==2018){
            tmp.back <- 0 # will indicate forward prediction
            year <- 2018
            reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
            reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
            reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
                                        #
            new.d <- data.frame(yr = year)
            rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
            rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
            rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
            vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
            bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
                                        #
            ## plug into results objects ##
            vhat.2018[i,] <- c(vhat.pan, vhat.pri, vhat.left, tmp.back)
            regs.2018$pan [[i]]   <- reg.pan
            regs.2018$left[[i]]   <- reg.left
            regs.2018$oth [[i]]   <- reg.oth
                                        #
            # add slot for projections/estimates if absent
            if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA 
            if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA 
            if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA
            if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA 
            if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA
            if ("dbackward" %notin% colnames(data.tmp)) data.tmp$dbackward <- NA
                                        #
            data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
            data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
            data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
            data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
            data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
            data.tmp$dbackward  [data.tmp$yr==year] <- tmp.back   # input fwd/back
        }
                                        #
        ##################################
        ## predict 2021 with last 5 els ##
        ##################################
        if (sel.map==2018){
            tmp.back <- 0 # will indicate forward prediction
            year <- 2021
            reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
            reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
            reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
                                        #
            new.d <- data.frame(yr = year)
            rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
            rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
            rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
            vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
            bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
                                        #
            ## plug into results objects ##
            vhat.2021[i,] <- c(vhat.pan, vhat.pri, vhat.left, tmp.back)
            regs.2021$pan [[i]]   <- reg.pan
            regs.2021$left[[i]]   <- reg.left
            regs.2021$oth [[i]]   <- reg.oth
                                        #
            # add slot for projections/estimates if absent
            if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA 
            if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA 
            if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA
            if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA 
            if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA
            if ("dbackward" %notin% colnames(data.tmp)) data.tmp$dbackward <- NA
                                        #
            data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
            data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
            data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
            data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
            data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
            data.tmp$dbackward  [data.tmp$yr==year] <- tmp.back   # input fwd/back
        }
                                        #
        ##################################
        ## predict 2024 with last 5 els ##
        ##################################
        if (sel.map==2018){
            tmp.back <- 0 # will indicate forward prediction
            year <- 2024
            reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
            reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
            reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
                                        #
            new.d <- data.frame(yr = year)
            rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
            rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
            rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
            vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
            bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
            bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
                                        #
            ## plug into results objects ##
            vhat.2024[i,] <- c(vhat.pan, vhat.pri, vhat.left, tmp.back)
            regs.2024$pan [[i]]   <- reg.pan
            regs.2024$left[[i]]   <- reg.left
            regs.2024$oth [[i]]   <- reg.oth
                                        #
            # add slot for projections/estimates if absent
            if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan  <- NA 
            if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri  <- NA 
            if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA
            if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan  <- NA 
            if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA
            if ("dbackward" %notin% colnames(data.tmp)) data.tmp$dbackward <- NA
                                        #
            data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
            data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
            data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
            data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
            data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
            data.tmp$dbackward  [data.tmp$yr==year] <- tmp.back   # input fwd/back
        }
        #
        ## ######################################################################
        ## alpha regressions (cf. Díaz Cayeros, Estévez, Magaloni 2016, p. 90) ##
        ## ######################################################################
        reg.pan   <-  lm(formula = log(pan /pri)  ~  mean.rpan,  data = data.tmp)
        reg.left  <-  lm(formula = log(left/pri)  ~  mean.rleft, data = data.tmp)
        reg.oth   <-  lm(formula = log(oth /pri)  ~  mean.roth,  data = data.tmp)
        ##
        ## point prediction alpha with mean at zero 
        new.d <- data.frame(mean.rpan = 0)
        rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
        new.d <- data.frame(mean.rleft = 0)
        rhat.left   <- exp(predict.lm(reg.left  , newdata = new.d))#, interval = "confidence")
        new.d <- data.frame(mean.roth = 0)
        rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
        vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left + rhat.oth), 3)
        vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left + rhat.oth), 3)
        vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left + rhat.oth), 3)
        #
        #c(vhat.pan, vhat.pri, vhat.left, 1-vhat.pan-vhat.pri-vhat.left)
        alphahat[i,] <- c(vhat.pan, vhat.pri, vhat.left  )
        betahat[i,1] <- coef(reg.pan)   [2]
        betahat[i,2] <- coef(reg.left  )[2]
        betahat[i,3] <- coef(reg.oth)   [2]
        #
        if (sel.map==1979){
            mean.regs.d79$pan   [[i]] <- reg.pan
            mean.regs.d79$left  [[i]] <- reg.left  
            mean.regs.d79$oth   [[i]] <- reg.oth
        }
        if (sel.map==1997){
            mean.regs.d97$pan   [[i]] <- reg.pan
            mean.regs.d97$left  [[i]] <- reg.left  
            mean.regs.d97$oth   [[i]] <- reg.oth
        }
        if (sel.map==2006){
            mean.regs.d06$pan   [[i]] <- reg.pan
            mean.regs.d06$left  [[i]] <- reg.left  
            mean.regs.d06$oth   [[i]] <- reg.oth
        }
        if (sel.map==2018){
            mean.regs.d18$pan   [[i]] <- reg.pan
            mean.regs.d18$left  [[i]] <- reg.left  
            mean.regs.d18$oth   [[i]] <- reg.oth
        }
        ##
        ## add alphas and betas for whole period
        data.tmp$alphahat.left  <- data.tmp$alphahat.pri <- data.tmp$alphahat.pan <- NA # open slots for alphas
        data.tmp$betahat.left   <- data.tmp$betahat.pan <- NA                           # open slots for betas
        data.tmp$alphahat.pan   <- alphahat$pan [i]
        data.tmp$alphahat.pri   <- alphahat$pri [i]
        data.tmp$alphahat.left  <- alphahat$left[i]
        data.tmp$betahat.pan    <- betahat$pan  [i]
        data.tmp$betahat.left   <- betahat$left [i]
        data.tmp$betahat.oth    <- betahat$oth  [i]
        data.tmp <- round(data.tmp,3)
        #
        ########################################################
        ## optional: plug vhats alphas betas back into data   ##
        ########################################################
        data.tmp <- within(data.tmp, {
            mean.rpan <- mean.rleft   <- mean.roth <- NULL; # drop mean ratios
            oth <- NULL; # drop compositional vote complement
            betahat.oth <- NULL; # drop this beta
            #betahat.pan <- betahat.left   <- betahat.oth <- NULL; # drop betas
        })
        # return estimates to data object
        if (sel.map==1979) extendCoald79[[i]] <- data.tmp
        if (sel.map==1997) extendCoald97[[i]] <- data.tmp
        if (sel.map==2006) extendCoald06[[i]] <- data.tmp
        if (sel.map==2018) extendCoald18[[i]] <- data.tmp
    }
    ###################################################
    ## warnings correspond to units with no variance ##
    ## (eg. period mean in new municipio in 2017)    ##
    ###################################################
    # function will return this:
    if (sel.map==1979){
        res.tmp <-
            list(ec = extendCoald79,
                 rgs88 = regs.1988,
                 rgs91 = regs.1991,
                 rgs94 = regs.1994,
                 m.rgs = mean.regs.d79
                 )
        return(res.tmp)
    }
    if (sel.map==1997){
         res.tmp <-
            list(ec = extendCoald97,
                 rgs97 = regs.1997,
                 rgs00 = regs.2000,
                 rgs03 = regs.2003,
                 m.rgs = mean.regs.d97
                 )
        return(res.tmp)
    }
    if (sel.map==2006){
        res.tmp <-
            list(ec = extendCoald06,
                 rgs06 = regs.2006,
                 rgs09 = regs.2009,
                 rgs12 = regs.2012,
                 rgs15 = regs.2015,
                 m.rgs = mean.regs.d06
                 )
        return(res.tmp)
    }
    if (sel.map==2018){
        res.tmp <-
            list(ec = extendCoald18,
                 rgs18 = regs.2018,
                 rgs21 = regs.2021,
                 rgs24 = regs.2024,
                 m.rgs = mean.regs.d18
                 )
        return(res.tmp)
    }
}

#########################################################################################
## REGRESSIONS ESTIMATION ROUTINE HERE                                                 ##
## district estimates are contingent on chosen map, run each once for full estimates   ##
#########################################################################################
for (j in 1:4){
    sel.map <- c(1979,1997,2006,2018)[j]
    res.tmp <- estim_dis(sel.map)
    #
    if (sel.map==1979){
        extendCoald79 <- res.tmp$ec
        regs.1988     <- res.tmp$rgs88
        regs.1991     <- res.tmp$rgs91
        regs.1994     <- res.tmp$rgs94
        mean.regs.d79 <- res.tmp$m.rgs
    }
    if (sel.map==1997){
        extendCoald97 <- res.tmp$ec
        regs.1997     <- res.tmp$rgs97
        regs.2000     <- res.tmp$rgs00
        regs.2003     <- res.tmp$rgs03
        mean.regs.d97 <- res.tmp$m.rgs
    }
    if (sel.map==2006){
        extendCoald06 <- res.tmp$ec
        regs.2006     <- res.tmp$rgs06
        regs.2009     <- res.tmp$rgs09
        regs.2012     <- res.tmp$rgs12
        regs.2015     <- res.tmp$rgs15
        mean.regs.d06 <- res.tmp$m.rgs
    }
    if (sel.map==2018){
        extendCoald18 <- res.tmp$ec
        regs.2018     <- res.tmp$rgs18
        regs.2021     <- res.tmp$rgs21
        regs.2024     <- res.tmp$rgs24
        mean.regs.d18 <- res.tmp$m.rgs
    }
}

rm(res.tmp)

# clean, all this is saved in extendCoal, mean.regs, regs.1988 ... regs.2024
#ls()
rm(alphahat, betahat, cs, estim_dis, per.means, yr.means,
   vhat.1988, vhat.1991, vhat.1994,
   vhat.1997, vhat.2000, vhat.2003,
   vhat.2006, vhat.2009, vhat.2012, vhat.2015,
   vhat.2018, vhat.2021, vhat.2024)
rm(v94d97, v94d06, v94d18,
   v97d79, v97d06, v97d18,
   v00d79, v00d06, v00d18,
   v03d79, v03d06, v03d18,
   v06d79, v06d97, v06d18,
   v09d79, v09d97, v09d18,
   v12d79, v12d97, v12d18,
   v15d79, v15d97, v15d18,
   v18d79, v18d97, v18d06,
   v21d79, v21d97, v21d06)
rm(i, j, sel.map)


##########################################################################
## generate data frame with one year's predictions/estimates for export ##
##########################################################################
tmp.func <- function(year) {
    #year <- 2009         # debug
    #X <- extendCoal[[1]] # debug
    ## select relevant results object
    if (year %in% c(1985,1988,1991,1994)) tmp.dat <- extendCoald79
    if (year %in% c(1997,2000,2003))      tmp.dat <- extendCoald97
    if (year %in% c(2006,2009,2012,2015)) tmp.dat <- extendCoald06
    if (year %in% c(2018,2021,2024))      tmp.dat <- extendCoald18
    sel.row <- which(tmp.dat[[1]]$yr==year) # which row reports year sought (symmetric across objects in list)
    # generate list with selected row only in every district
    tmp.out <- lapply(tmp.dat, FUN = function(X) {
        prune <- X[sel.row,]
        return(prune)
    })
    ## # spot NAs in list
    ## tmp.sel <- setdiff(1:length(extendCoal), non.nas)
    ## # fill with same-dim NA data.frame
    ## tmp.manip <- tmp.out[[non.nas[1]]]
    ## tmp.manip[,-1] <- NA # all but 1st col (yr) to NA
    ## if (length(tmp.sel)>0) tmp.out[tmp.sel] <- lapply(tmp.out[tmp.sel], function(x) tmp.manip)
    # turn into one dataframe
    # table(summary(tmp)) # debug
    tmp.out <- do.call("rbind", tmp.out)
    rownames(tmp.out) <- NULL
    return(tmp.out)
}

out.y1988 <- tmp.func(year=1988)
out.y1991 <- tmp.func(year=1991)
out.y1994 <- tmp.func(year=1994)
out.y1997 <- tmp.func(year=1997)
out.y2000 <- tmp.func(year=2000)
out.y2003 <- tmp.func(year=2003)
out.y2006 <- tmp.func(year=2006)
out.y2009 <- tmp.func(year=2009)
out.y2012 <- tmp.func(year=2012)
out.y2015 <- tmp.func(year=2015)
out.y2018 <- tmp.func(year=2018)
out.y2021 <- tmp.func(year=2021)
out.y2024 <- tmp.func(year=2024)

##################
## save to disk ##
##################
write.csv(out.y1988,
          file = paste(sd, "dis/dipfed-distrito-vhat-1988.csv", sep = ""), row.names = FALSE)
##
write.csv(out.y1991,
          file = paste(sd, "dis/dipfed-distrito-vhat-1991.csv", sep = ""), row.names = FALSE)
##
write.csv(out.y1994,
          file = paste(sd, "dis/dipfed-distrito-vhat-1994.csv", sep = ""), row.names = FALSE)
##
write.csv(out.y1997,
          file = paste(sd, "dis/dipfed-distrito-vhat-1997.csv", sep = ""), row.names = FALSE)
##
write.csv(out.y2000,
          file = paste(sd, "dis/dipfed-distrito-vhat-2000.csv", sep = ""), row.names = FALSE)
##
write.csv(out.y2003,
          file = paste(sd, "dis/dipfed-distrito-vhat-2003.csv", sep = ""), row.names = FALSE)
##
write.csv(out.y2006,
          file = paste(sd, "dis/dipfed-distrito-vhat-2006.csv", sep = ""), row.names = FALSE)
##
write.csv(out.y2009,
          file = paste(sd, "dis/dipfed-distrito-vhat-2009.csv", sep = ""), row.names = FALSE)
##
write.csv(out.y2012,
          file = paste(sd, "dis/dipfed-distrito-vhat-2012.csv", sep = ""), row.names = FALSE)
##
write.csv(out.y2015,
          file = paste(sd, "dis/dipfed-distrito-vhat-2015.csv", sep = ""), row.names = FALSE)
##
write.csv(out.y2018,
          file = paste(sd, "dis/dipfed-distrito-vhat-2018.csv", sep = ""), row.names = FALSE)
##
write.csv(out.y2021,
          file = paste(sd, "dis/dipfed-distrito-vhat-2021.csv", sep = ""), row.names = FALSE)
##
write.csv(out.y2024,
          file = paste(sd, "dis/dipfed-distrito-vhat-2024.csv", sep = ""), row.names = FALSE)
##

## save district regression objects
save(mean.regs.d79, file = paste(sd, "dis/dipfed-distrito-mean-regs-map1979.RData", sep = ""), compress = c("gzip", "bzip2", "xz")[3])
save(mean.regs.d97, file = paste(sd, "dis/dipfed-distrito-mean-regs-map1997.RData", sep = ""), compress = c("gzip", "bzip2", "xz")[3])
save(mean.regs.d06, file = paste(sd, "dis/dipfed-distrito-mean-regs-map2006.RData", sep = ""), compress = c("gzip", "bzip2", "xz")[3])
save(mean.regs.d18, file = paste(sd, "dis/dipfed-distrito-mean-regs-map2018.RData", sep = ""), compress = c("gzip", "bzip2", "xz")[3])
##
save(regs.1988, file = paste(sd, "dis/dipfed-distrito-regs-1988.RData", sep = ""), compress = "gzip")
save(regs.1991, file = paste(sd, "dis/dipfed-distrito-regs-1991.RData", sep = ""), compress = "gzip")
save(regs.1994, file = paste(sd, "dis/dipfed-distrito-regs-1994.RData", sep = ""), compress = "gzip")
save(regs.1997, file = paste(sd, "dis/dipfed-distrito-regs-1997.RData", sep = ""), compress = "gzip")
save(regs.2000, file = paste(sd, "dis/dipfed-distrito-regs-2000.RData", sep = ""), compress = "gzip")
save(regs.2003, file = paste(sd, "dis/dipfed-distrito-regs-2003.RData", sep = ""), compress = "gzip")
save(regs.2006, file = paste(sd, "dis/dipfed-distrito-regs-2006.RData", sep = ""), compress = "gzip")
save(regs.2009, file = paste(sd, "dis/dipfed-distrito-regs-2009.RData", sep = ""), compress = "gzip")
save(regs.2012, file = paste(sd, "dis/dipfed-distrito-regs-2012.RData", sep = ""), compress = "gzip")
save(regs.2015, file = paste(sd, "dis/dipfed-distrito-regs-2015.RData", sep = ""), compress = "gzip")
save(regs.2018, file = paste(sd, "dis/dipfed-distrito-regs-2018.RData", sep = ""), compress = "gzip")
save(regs.2021, file = paste(sd, "dis/dipfed-distrito-regs-2021.RData", sep = ""), compress = "gzip")
save(regs.2024, file = paste(sd, "dis/dipfed-distrito-regs-2024.RData", sep = ""), compress = "gzip")

###########
## clean ##
###########
rm(
    out.y1988,
    out.y1991,
    out.y1994,
    out.y1997,
    out.y2000,
    out.y2003,
    out.y2006,
    out.y2009,
    out.y2012,
    out.y2015,
    out.y2018,
    out.y2021,
    out.y2024
)
rm(
    regs.1988,
    regs.1991,
    regs.1994,
    regs.1997,
    regs.2000,
    regs.2003,
    regs.2006,
    regs.2009,
    regs.2012,
    regs.2015,
    regs.2018,
    regs.2021,
    regs.2024
)
rm(
    mean.regs.d79,
    mean.regs.d97,
    mean.regs.d06,
    mean.regs.d18
    )
rm(
    extendCoald79,
    extendCoald97,
    extendCoald06,
    extendCoald18
)

## ###########################
## ## inspect saved objects ##
## ###########################
## # load regression object
## load(file = paste(wd, "data/dipfed-municipio-regs-2015.RData", sep = ""))
## ls()
## summary(regs.2015)
## summary.lm(regs.2015$oth[[1]])$coef[2,1]



# version 2: coalitions only in districts where they happened
## leftRealm <- data.frame(v00 = v00m$prdc / v00m$efec,
##                         v03 = v03m$prd / v03m$efec,
##                         v06 = v06m$prdc / v06m$efec,
##                         v09 = v09m$prd / v09m$efec,
##                         v12 = v12m$prdc / v12m$efec,
##                         v15 = (v15m$prd * (1 - v15m$dprdc) + v15m$prdc * v15m$dprdc + v15m$morena) / v15m$efec,
##                         v18 = (v18m$morena * (1 - v18m$dmorenac) + v18m$morenac * v18m$dmorenac) / v18m$efec)
## esto no jala, parece que morena!=0 cuando dmorenac==1
## morenaRealm[1,]
# version 3: party's own vote plus proportional part of coal
## morenaBrkm
