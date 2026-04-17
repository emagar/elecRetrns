####################################################################
## Comprehensive Stress Test: xport() / extract-state-yr-mu-returns.r
## Generates stress_test_electoral.xlsx with 8 analysis tabs.
##
## Run from: any directory (paths are absolute)
## Requires: openxlsx (auto-installed if missing)
##
## Tabs:
##  1. Summary
##  2. Missing_Data            (state-year combos that return 0 rows)
##  3. Input_Errors            (robustness to bad inputs)
##  4. Output_Errors           (duplicates, sum mismatches, impossible values)
##  5. Verbosity               (quality of error messages)
##  6. Municipalities_Coverage (FIXED: absent + present-but-incomplete)
##  7. Non_Election_Years      (behavior on gap years)
##  8. CoalAgg_Tests           (NEW: coal.agg=TRUE vs FALSE consistency)
####################################################################

# 0. SETUP ----
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  install.packages("openxlsx", repos = "https://cloud.r-project.org")
}
library(openxlsx)

cat("=== ELECTORAL DATA STRESS TEST ===\n")
cat("Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

BASE_DIR    <- "C:/Users/rodri/OneDrive - INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO/Escritorio/elecRetrns"
DATA_DIR    <- file.path(BASE_DIR, "data")
ANC_DIR     <- file.path(BASE_DIR, "ancillary")
OUTPUT_FILE <- file.path(BASE_DIR, "stress_test_electoral2.xlsx")

# Source xport() from GitHub
cat("Loading xport() from GitHub...\n")
xport_loaded <- tryCatch({
  source("https://raw.githubusercontent.com/emagar/useful-functions/master/xport-function.r")
  TRUE
}, error = function(e) { cat("  ERROR:", conditionMessage(e), "\n"); FALSE })
if (!xport_loaded) stop("Cannot continue without xport(). Check internet connection.")
cat("  xport() loaded OK\n\n")

# xport() looks for the CSV in the working directory
setwd(DATA_DIR)

# Load data directly (fast analysis without repeated xport() calls)
cat("Loading data files...\n")
dat_agg   <- read.csv("aymu1970-on.coalAgg.csv",   stringsAsFactors = FALSE)
dat_split <- read.csv("aymu1970-on.coalSplit.csv", stringsAsFactors = FALSE)
mun_yrs   <- read.csv(file.path(ANC_DIR, "mun.yrs.csv"), stringsAsFactors = FALSE)
cat("  coalAgg.csv  :", nrow(dat_agg),   "rows\n")
cat("  coalSplit.csv:", nrow(dat_split), "rows\n")
cat("  mun.yrs.csv  :", nrow(mun_yrs),   "rows\n\n")

# State metadata
estados_nom <- c(
  "Aguascalientes","Baja California","Baja California Sur","Campeche",
  "Coahuila","Colima","Chiapas","Chihuahua","Distrito Federal/CDMX","Durango",
  "Guanajuato","Guerrero","Hidalgo","Jalisco","México","Michoacán","Morelos",
  "Nayarit","Nuevo León","Oaxaca","Puebla","Querétaro","Quintana Roo",
  "San Luis Potosí","Sinaloa","Sonora","Tabasco","Tamaulipas","Tlaxcala",
  "Veracruz","Yucatán","Zacatecas")
estados_abb <- c(
  "ags","bc","bcs","cam","coa","col","cps","cua","df","dgo","gua","gue",
  "hgo","jal","mex","mic","mor","nay","nl","oax","pue","que","qui","san",
  "sin","son","tab","tam","tla","ver","yuc","zac")

all_years_in_data <- sort(unique(dat_agg$yr))
v_cols_master     <- grep("^v[0-9]{2}$", colnames(dat_agg), value = TRUE)
required_cols     <- c("edon", "yr", "inegi", "mun", "efec", "tot")

cat("Years present:", min(all_years_in_data), "–", max(all_years_in_data),
    "(", length(all_years_in_data), "distinct years)\n")
cat("Missing 2023?", !2023 %in% all_years_in_data, "\n\n")

# ---------------------------------------------------------------------------
# Helper: call xport() safely, capturing errors / warnings / print() output
# ---------------------------------------------------------------------------
capture_xport <- function(e_val, y_val, coal.agg = TRUE) {
  err_msg   <- NA_character_
  warn_msgs <- character(0)
  out       <- NULL
  success   <- FALSE

  printed <- capture.output(type = "output", {
    tryCatch({
      withCallingHandlers({
        out     <- xport(e = e_val, y = y_val, coal.agg = coal.agg)
        success <- !is.null(out) && is.data.frame(out)
      },
      warning = function(w) {
        warn_msgs <<- c(warn_msgs, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    }, error = function(err) {
      err_msg <<- conditionMessage(err)
    })
  })

  list(
    success = success,
    output  = out,
    nrow    = if (!is.null(out) && is.data.frame(out)) nrow(out) else 0L,
    ncol    = if (!is.null(out) && is.data.frame(out)) ncol(out) else 0L,
    error   = err_msg,
    warning = paste(warn_msgs, collapse = "; "),
    printed = paste(printed,   collapse = "; ")
  )
}

all_msg <- function(res) {
  parts <- c(
    if (!is.na(res$error)   && nchar(res$error)   > 0) res$error,
    if (!is.na(res$warning) && nchar(res$warning) > 0) res$warning,
    if (nchar(res$printed)  > 0)                       res$printed
  )
  trimws(paste(parts, collapse = " | "))
}

################################################################################
## SECTION A — Data Coverage: all 32 states × all years in data              ##
################################################################################
cat("=== A: Data Coverage ===\n")

cover_grid <- expand.grid(edon = 1:32, yr = all_years_in_data, stringsAsFactors = FALSE)
n_cover    <- nrow(cover_grid)
cat("  Testing", n_cover, "state-year combinations...\n")

coverage_rows <- vector("list", n_cover)
for (i in seq_len(n_cover)) {
  e  <- cover_grid$edon[i]; y <- cover_grid$yr[i]
  sb <- dat_agg[dat_agg$edon == e & dat_agg$yr == y, ]
  coverage_rows[[i]] <- list(
    edon = e, estado = estados_nom[e], yr = y,
    has_data = nrow(sb) > 0, n_mun_returned = nrow(sb)
  )
}
coverage_df     <- do.call(rbind, lapply(coverage_rows, as.data.frame, stringsAsFactors = FALSE))
coverage_w_data <- coverage_df[coverage_df$has_data, ]
coverage_no_data<- coverage_df[!coverage_df$has_data, ]
cat("  With data:", nrow(coverage_w_data), "| Without:", nrow(coverage_no_data), "\n\n")

################################################################################
## SECTION B — Non-election year tests                                        ##
################################################################################
cat("=== B: Non-Election Year Tests ===\n")

non_elec_list <- list()
for (e in 1:32) {
  state_yrs   <- unique(dat_agg$yr[dat_agg$edon == e])
  cand_yrs    <- setdiff(seq(min(state_yrs), max(state_yrs)), state_yrs)
  test_sample <- head(cand_yrs, 3)
  for (y in test_sample) {
    res <- capture_xport(e, y)
    msg <- all_msg(res)
    non_elec_list[[length(non_elec_list)+1]] <- data.frame(
      edon = e, estado = estados_nom[e], yr = y,
      year_in_data = FALSE, success = res$success, nrows = res$nrow,
      message_returned = msg,
      distinguishes_no_election = grepl(
        "election|electoral|year|año|proper|data|found|no row",
        msg, ignore.case = TRUE),
      stringsAsFactors = FALSE
    )
  }
}
non_elec_df <- do.call(rbind, non_elec_list)
cat("  Tests run:", nrow(non_elec_df), "\n")
cat("  Informative messages:", sum(non_elec_df$distinguishes_no_election, na.rm=TRUE), "\n\n")

################################################################################
## SECTION C — Input Error Tests                                              ##
################################################################################
cat("=== C: Input Error Tests ===\n")

input_cases <- list(
  list(e="aguascalientes",    y=2019,  desc="Lowercase full state name"),
  list(e="Jalisco",           y=2018,  desc="Proper-case full state name"),
  list(e="JALISCO",           y=2018,  desc="Uppercase full state name"),
  list(e="jal",               y=2018,  desc="State abbreviation (supported but buggy)"),
  list(e="jalicso",           y=2018,  desc="Misspelled state name"),
  list(e="Mexico",            y=2018,  desc="No accent: Mexico"),
  list(e="Queretaro",         y=2018,  desc="No accent: Queretaro"),
  list(e=0,                   y=2019,  desc="State = 0 (below range)"),
  list(e=33,                  y=2019,  desc="State = 33 (above range)"),
  list(e=-5,                  y=2019,  desc="State = -5 (negative)"),
  list(e=999,                 y=2019,  desc="State = 999 (far out of range)"),
  list(e=14,                  y=1800,  desc="Year = 1800 (too early)"),
  list(e=14,                  y=1969,  desc="Year = 1969 (just before data)"),
  list(e=14,                  y=2030,  desc="Year = 2030 (future)"),
  list(e=14,                  y=-1990, desc="Year = -1990 (negative)"),
  list(e=14,                  y=0,     desc="Year = 0"),
  list(e="xyz",               y=2019,  desc="State as non-numeric string"),
  list(e=14,                  y="dos mil dieciocho", desc="Year as text"),
  list(e=14,                  y="2018",desc="Year as coercible string"),
  list(e=NA_real_,            y=2019,  desc="State = NA"),
  list(e=14,                  y=NA_real_,desc="Year = NA"),
  list(e=NULL,                y=2019,  desc="State = NULL"),
  list(e=14,                  y=NULL,  desc="Year = NULL"),
  list(e=1.5,                 y=2019,  desc="State = 1.5 (non-integer)"),
  list(e=14,                  y=2018.7,desc="Year = 2018.7 (non-integer)"),
  list(e=c(1,2),              y=2019,  desc="State = c(1,2) (vector)"),
  list(e=14,                  y=c(2018,2019),desc="Year = c(2018,2019) (vector)"),
  list(e=1,                   y=min(all_years_in_data),desc=paste("Earliest year in data:", min(all_years_in_data))),
  list(e=1,                   y=max(all_years_in_data),desc=paste("Latest year in data:", max(all_years_in_data))),
  list(e=32,                  y=2018,  desc="Last state (Zacatecas), valid year"),
  list(e=9,                   y=2018,  desc="CDMX (edon=9), valid year")
)

graceful_pat <- "no loop for break|subscript out of bounds|object .* not found|cannot open|cannot allocate"

input_rows <- lapply(input_cases, function(case) {
  res <- capture_xport(case$e, case$y)
  msg <- all_msg(res)
  is_invalid <- tryCatch({
    is.null(case$e) || is.null(case$y) ||
      any(is.na(case$e)) || any(is.na(case$y)) ||
      (!is.numeric(case$e) && !is.integer(case$e)) ||
      (!is.numeric(case$y) && !is.integer(case$y)) ||
      (is.numeric(case$e) && (case$e[1] < 1 || case$e[1] > 32)) ||
      (is.numeric(case$y) && (case$y[1] < 1970 || case$y[1] > 2025))
  }, error = function(e) TRUE)
  data.frame(
    test_case          = case$desc,
    input_e            = paste(deparse(case$e), collapse=""),
    input_y            = paste(deparse(case$y), collapse=""),
    expected_behavior  = ifelse(is_invalid, "Should return informative error", "May return data"),
    success            = res$success,
    nrows_returned     = res$nrow,
    error_msg          = ifelse(is.na(res$error), "", res$error),
    warning_msg        = res$warning,
    printed_msg        = res$printed,
    handles_gracefully = !grepl(graceful_pat, msg, ignore.case=TRUE),
    stringsAsFactors   = FALSE
  )
})
input_df <- do.call(rbind, input_rows)
cat("  Input tests:", nrow(input_df), "\n")
cat("  Handled gracefully:", sum(input_df$handles_gracefully, na.rm=TRUE), "/", nrow(input_df), "\n\n")

################################################################################
## SECTION D — Output Quality Checks                                          ##
################################################################################
cat("=== D: Output Quality Checks ===\n")

out_err_list <- list()
for (i in seq_len(nrow(coverage_w_data))) {
  e  <- coverage_w_data$edon[i]; y <- coverage_w_data$yr[i]
  sb <- dat_agg[dat_agg$edon == e & dat_agg$yr == y, ]

  # D1: Duplicate INEGI codes
  dup <- sb$inegi[duplicated(sb$inegi) & !is.na(sb$inegi)]
  if (length(dup)) {
    out_err_list[[length(out_err_list)+1]] <- data.frame(
      edon=e, estado=estados_nom[e], yr=y,
      inegi=paste(unique(dup), collapse=","),
      mun=paste(sb$mun[sb$inegi %in% dup], collapse=","),
      error_type="Duplicate INEGI code",
      details=paste("Duplicated codes:", paste(unique(dup), collapse=", ")),
      stringsAsFactors=FALSE)
  }

  # D2: sum(v01..v23) ≠ efec
  if (all(v_cols_master %in% colnames(sb)) && "efec" %in% colnames(sb)) {
    v_sum <- rowSums(sb[, v_cols_master], na.rm=TRUE)
    mis   <- which(abs(v_sum - sb$efec) > 1 & !is.na(sb$efec))
    for (j in mis) {
      out_err_list[[length(out_err_list)+1]] <- data.frame(
        edon=e, estado=estados_nom[e], yr=y, inegi=sb$inegi[j], mun=sb$mun[j],
        error_type="Vote sum ≠ efec",
        details=paste0("sum(v cols)=",v_sum[j]," efec=",sb$efec[j],
                       " diff=",v_sum[j]-sb$efec[j]),
        stringsAsFactors=FALSE)
    }
  }

  # D3: efec > tot (logically impossible)
  if (all(c("efec","tot") %in% colnames(sb))) {
    imp <- which(!is.na(sb$efec) & !is.na(sb$tot) & sb$efec > sb$tot + 1)
    for (j in imp) {
      out_err_list[[length(out_err_list)+1]] <- data.frame(
        edon=e, estado=estados_nom[e], yr=y, inegi=sb$inegi[j], mun=sb$mun[j],
        error_type="efec > tot (impossible)",
        details=paste0("efec=",sb$efec[j]," tot=",sb$tot[j]),
        stringsAsFactors=FALSE)
    }
  }

  # D4: Negative vote counts
  if (length(v_cols_master) && all(v_cols_master %in% colnames(sb))) {
    neg <- which(apply(sb[, v_cols_master, drop=FALSE], 1,
                       function(x) any(x < 0, na.rm=TRUE)))
    for (j in neg) {
      out_err_list[[length(out_err_list)+1]] <- data.frame(
        edon=e, estado=estados_nom[e], yr=y, inegi=sb$inegi[j], mun=sb$mun[j],
        error_type="Negative vote count",
        details="At least one v## column < 0",
        stringsAsFactors=FALSE)
    }
  }

  # D5: Missing required columns
  mc <- required_cols[!required_cols %in% colnames(sb)]
  if (length(mc)) {
    out_err_list[[length(out_err_list)+1]] <- data.frame(
      edon=e, estado=estados_nom[e], yr=y, inegi="ALL", mun="ALL",
      error_type="Missing required column",
      details=paste("Missing:", paste(mc, collapse=", ")),
      stringsAsFactors=FALSE)
  }

  # D6: NA or empty municipality name
  if ("mun" %in% colnames(sb)) {
    na_rows <- which(is.na(sb$mun) | trimws(sb$mun) == "")
    if (length(na_rows)) {
      out_err_list[[length(out_err_list)+1]] <- data.frame(
        edon=e, estado=estados_nom[e], yr=y,
        inegi=paste(sb$inegi[na_rows], collapse=","), mun="NA/empty",
        error_type="Missing municipality name",
        details=paste(length(na_rows),"rows with empty mun field"),
        stringsAsFactors=FALSE)
    }
  }

  # D7: efec=0 but tot>0
  if (all(c("efec","tot") %in% colnames(sb))) {
    z <- which(!is.na(sb$efec) & sb$efec==0 & !is.na(sb$tot) & sb$tot>0)
    for (j in z) {
      out_err_list[[length(out_err_list)+1]] <- data.frame(
        edon=e, estado=estados_nom[e], yr=y, inegi=sb$inegi[j], mun=sb$mun[j],
        error_type="efec=0 but tot>0",
        details=paste0("efec=0, tot=",sb$tot[j]),
        stringsAsFactors=FALSE)
    }
  }
}

out_err_df <- if (length(out_err_list)) do.call(rbind, out_err_list) else
  data.frame(edon=integer(),estado=character(),yr=integer(),inegi=character(),
             mun=character(),error_type=character(),details=character(),
             stringsAsFactors=FALSE)
cat("  Issues found:", nrow(out_err_df), "\n")
if (nrow(out_err_df)) print(sort(table(out_err_df$error_type), decreasing=TRUE))
cat("\n")

################################################################################
## SECTION E — Verbosity                                                      ##
################################################################################
cat("=== E: Verbosity Tests ===\n")

verb_cases <- list(
  list(e=1,         y=2019,       desc="Valid: Aguascalientes 2019"),
  list(e=14,        y=2018,       desc="Valid: Jalisco 2018"),
  list(e=32,        y=2018,       desc="Valid: Zacatecas 2018"),
  list(e=9,         y=2018,       desc="Valid: CDMX 2018"),
  list(e=1,         y=1800,       desc="Invalid year (1800)"),
  list(e=1,         y=1975,       desc="Year before any data (1975)"),
  list(e=1,         y=2023,       desc="Year not in data (2023)"),
  list(e=1,         y=2030,       desc="Future year (2030)"),
  list(e=NA_real_,  y=2019,       desc="State = NA"),
  list(e=14,        y=NA_real_,   desc="Year = NA"),
  list(e=NULL,      y=2019,       desc="State = NULL"),
  list(e=14,        y=NULL,       desc="Year = NULL"),
  list(e=0,         y=2019,       desc="State = 0 (invalid)"),
  list(e=33,        y=2019,       desc="State = 33 (invalid)"),
  list(e="jalisco", y=2018,       desc="State as string"),
  list(e=14,        y="dos mil",  desc="Year as text")
)

verb_rows <- lapply(verb_cases, function(case) {
  res <- capture_xport(case$e, case$y)
  msg <- all_msg(res)
  is_break_bug   <- grepl("no loop for break",           msg, ignore.case=TRUE)
  is_cryptic     <- grepl("subscript out of bounds|object .* not found|NAs introduced",
                           msg, ignore.case=TRUE)
  is_informative <- grepl("election|electoral|year|año|proper|data|found|no row|invalid|state|estado",
                           msg, ignore.case=TRUE)
  assessment <- if (res$success) "YES" else
                if (is_break_bug || is_cryptic) "NO" else
                if (is_informative) "YES" else
                if (nchar(msg)==0) "NO" else "PARTIAL"
  suggestion <- if (res$success) {
    "Correct — no message needed on success"
  } else if (is_break_bug) {
    "BUG: replace 'break' with return(NULL) or stop() + informative message"
  } else if (is_cryptic) {
    "Add input validation before data operations; current base-R error is not user-friendly"
  } else if (is_informative) {
    "Reasonable. Could also list valid states/years available."
  } else if (nchar(msg)==0) {
    "Silent failure — always inform the user when no data is returned"
  } else {
    "Message exists but could be more specific about cause and valid inputs"
  }
  data.frame(test_case=case$desc,
             input_e=paste(deparse(case$e),collapse=""),
             input_y=paste(deparse(case$y),collapse=""),
             actual_msg=msg, success=res$success, nrows=res$nrow,
             assessment=assessment, suggestion=suggestion,
             stringsAsFactors=FALSE)
})
verb_df <- do.call(rbind, verb_rows)
cat("  Tests:", nrow(verb_df), "| YES:", sum(verb_df$assessment=="YES"),
    "| PARTIAL:", sum(verb_df$assessment=="PARTIAL"),
    "| NO:", sum(verb_df$assessment=="NO"), "\n\n")

################################################################################
## SECTION F — Municipalities Coverage (FIXED)                               ##
##                                                                            ##
## Part F1: Municipality IS in dataset but data is INCOMPLETE                ##
## Part F2: Municipality is EXPECTED (mun.yrs.csv) but COMPLETELY ABSENT     ##
################################################################################
cat("=== F: Municipality Coverage ===\n")

## ---- F1: Present but incomplete ----
cat("  F1: Checking for municipalities with incomplete data...\n")

f1_list <- list()
for (i in seq_len(nrow(coverage_w_data))) {
  e  <- coverage_w_data$edon[i]; y <- coverage_w_data$yr[i]
  sb <- dat_agg[dat_agg$edon == e & dat_agg$yr == y, ]

  for (j in seq_len(nrow(sb))) {
    issues <- character(0)

    # F1.1 efec missing or zero
    if ("efec" %in% colnames(sb)) {
      if (is.na(sb$efec[j]))
        issues <- c(issues, "efec = NA")
      else if (sb$efec[j] == 0 && !is.na(sb$tot[j]) && sb$tot[j] > 0)
        issues <- c(issues, "efec = 0 but tot > 0")
    }

    # F1.2 Total votes missing or zero
    if ("tot" %in% colnames(sb)) {
      if (is.na(sb$tot[j]))
        issues <- c(issues, "tot = NA")
      else if (sb$tot[j] == 0)
        issues <- c(issues, "tot = 0 (no votes recorded)")
    }

    # F1.3 All vote columns are 0 or NA (no party data at all)
    if (all(v_cols_master %in% colnames(sb))) {
      v_row <- as.numeric(sb[j, v_cols_master])
      if (all(is.na(v_row) | v_row == 0))
        issues <- c(issues, "all vote columns empty (0 or NA)")
    }

    # F1.4 Status is not "ok"
    if ("status" %in% colnames(sb)) {
      st <- trimws(tolower(sb$status[j]))
      if (!is.na(st) && st != "" && st != "ok")
        issues <- c(issues, paste0("status = '", sb$status[j], "'"))
    }

    # F1.5 Voter list (lisnom) missing or zero
    if ("lisnom" %in% colnames(sb)) {
      if (is.na(sb$lisnom[j]))
        issues <- c(issues, "lisnom = NA")
      else if (sb$lisnom[j] == 0)
        issues <- c(issues, "lisnom = 0 (no registered voters)")
    }

    # F1.6 Winner missing
    if ("win" %in% colnames(sb)) {
      w <- trimws(sb$win[j])
      if (is.na(w) || w == "" || w == "0")
        issues <- c(issues, "winner (win) missing")
    }

    # F1.7 ncand = 0
    if ("ncand" %in% colnames(sb)) {
      if (!is.na(sb$ncand[j]) && sb$ncand[j] == 0)
        issues <- c(issues, "ncand = 0 (no candidates)")
    }

    # F1.8 source (fuente) missing
    if ("fuente" %in% colnames(sb)) {
      f <- trimws(sb$fuente[j])
      if (is.na(f) || f == "")
        issues <- c(issues, "fuente missing (no source documented)")
    }

    if (length(issues)) {
      f1_list[[length(f1_list)+1]] <- data.frame(
        type         = "INCOMPLETE DATA",
        edon         = e,
        estado       = estados_nom[e],
        yr           = y,
        inegi        = sb$inegi[j],
        mun          = sb$mun[j],
        issues_found = paste(issues, collapse=" | "),
        n_issues     = length(issues),
        stringsAsFactors = FALSE
      )
    }
  }
}
f1_df <- if (length(f1_list)) do.call(rbind, f1_list) else
  data.frame(type=character(),edon=integer(),estado=character(),yr=integer(),
             inegi=character(),mun=character(),issues_found=character(),
             n_issues=integer(),stringsAsFactors=FALSE)
cat("  F1: Municipalities with incomplete data:", nrow(f1_df), "\n")
if (nrow(f1_df)) {
  # Most frequent issues
  all_issues <- unlist(strsplit(f1_df$issues_found, " \\| "))
  print(sort(table(all_issues), decreasing=TRUE))
}

## ---- F2: Expected but completely absent ----
cat("  F2: Checking for completely absent municipalities...\n")

yr_cols  <- grep("^yr[0-9]+$", colnames(mun_yrs), value=TRUE)
mun_long <- do.call(rbind, Filter(Negate(is.null), lapply(seq_len(nrow(mun_yrs)), function(i) {
  row  <- mun_yrs[i, ]
  yrs  <- suppressWarnings(as.numeric(unlist(row[yr_cols])))
  yrs  <- yrs[!is.na(yrs) & yrs > 0]
  if (!length(yrs)) return(NULL)
  data.frame(inegi=row$inegi, edon=row$edon, mun=row$mun, yr=yrs,
             stringsAsFactors=FALSE)
})))

mun_long_recent <- mun_long[mun_long$yr >= 1988, ]

f2_list <- list()
for (i in seq_len(nrow(mun_long_recent))) {
  e     <- mun_long_recent$edon[i]
  y     <- mun_long_recent$yr[i]
  inegi <- mun_long_recent$inegi[i]
  in_d  <- dat_agg[dat_agg$edon == e & dat_agg$yr == y & dat_agg$inegi == inegi, ]
  if (!nrow(in_d)) {
    f2_list[[length(f2_list)+1]] <- data.frame(
      type   = "COMPLETELY ABSENT",
      edon   = e, estado = estados_nom[e], yr = y,
      inegi  = inegi, mun = mun_long_recent$mun[i],
      issues_found = "Expected per mun.yrs.csv but absent from coalAgg dataset",
      n_issues = 1L,
      stringsAsFactors = FALSE
    )
  }
}
f2_df <- if (length(f2_list)) {
  tmp <- do.call(rbind, f2_list)
  tmp[order(tmp$edon, tmp$yr, tmp$mun), ]
} else {
  data.frame(type=character(),edon=integer(),estado=character(),yr=integer(),
             inegi=character(),mun=character(),issues_found=character(),
             n_issues=integer(),stringsAsFactors=FALSE)
}
cat("  F2: Completely absent municipalities (1988+):", nrow(f2_df), "\n\n")

# Combined F1 + F2
mun_coverage_df <- rbind(f1_df, f2_df)
mun_coverage_df <- mun_coverage_df[order(mun_coverage_df$type,
                                         mun_coverage_df$edon,
                                         mun_coverage_df$yr,
                                         mun_coverage_df$mun), ]

################################################################################
## SECTION G — coal.agg Consistency Tests (NEW)                              ##
##                                                                            ##
## G1: Sample of state-years: compare coalAgg vs coalSplit                   ##
## G2: Invalid coal.agg parameter values                                     ##
################################################################################
cat("=== G: coal.agg Consistency Tests ===\n")

## ---- G1: coalAgg vs coalSplit comparison ----
cat("  G1: Comparing coal.agg=TRUE vs FALSE on sample state-years...\n")

# Sample: one per region (north, center-north, center, south), 5 years each
sample_states <- c(2, 5, 8, 14, 15, 16, 20, 21, 27, 30)  # bc, coa, cua, jal, mex, mic, oax, pue, tab, ver
sample_years  <- c(2006, 2009, 2012, 2015, 2018)

g1_list <- list()
for (e in sample_states) {
  state_yrs <- unique(dat_agg$yr[dat_agg$edon == e])
  test_yrs  <- intersect(sample_years, state_yrs)
  for (y in test_yrs) {
    res_agg   <- capture_xport(e, y, coal.agg = TRUE)
    res_split <- capture_xport(e, y, coal.agg = FALSE)

    # Compare nrows
    nrow_match <- res_agg$nrow == res_split$nrow

    # Compare efec totals (must be identical — aggregation doesn't change efec)
    efec_match <- NA
    if (res_agg$success && res_split$success &&
        "efec" %in% colnames(res_agg$output) &&
        "efec" %in% colnames(res_split$output)) {
      agg_efec   <- sort(res_agg$output$efec)
      split_efec <- sort(res_split$output$efec)
      efec_match <- isTRUE(all.equal(agg_efec, split_efec))
    }

    # Compare tot totals (must be identical)
    tot_match <- NA
    if (res_agg$success && res_split$success &&
        "tot" %in% colnames(res_agg$output) &&
        "tot" %in% colnames(res_split$output)) {
      agg_tot   <- sort(res_agg$output$tot)
      split_tot <- sort(res_split$output$tot)
      tot_match <- isTRUE(all.equal(agg_tot, split_tot))
    }

    # Compare number of party columns (split should have >= agg)
    ncols_agg   <- if (res_agg$success)   res_agg$ncol   else NA
    ncols_split <- if (res_split$success) res_split$ncol else NA
    more_cols_in_split <- if (!is.na(ncols_agg) && !is.na(ncols_split))
      ncols_split >= ncols_agg else NA

    # Shared party columns vs unique ones
    shared_parties <- NA_character_
    unique_to_split <- NA_character_
    unique_to_agg   <- NA_character_
    if (res_agg$success && res_split$success) {
      base_cols  <- c("emm","yr","inegi","ife","mun","edon","date","status",
                      "ncand","ncoal","win","mg","efec","nr","nulos","tot",
                      "lisnom","fuente","notas","dextra")
      pcols_agg   <- setdiff(colnames(res_agg$output),   base_cols)
      pcols_split <- setdiff(colnames(res_split$output), base_cols)
      shared_parties   <- paste(intersect(pcols_agg, pcols_split), collapse=", ")
      unique_to_split  <- paste(setdiff(pcols_split, pcols_agg),  collapse=", ")
      unique_to_agg    <- paste(setdiff(pcols_agg,  pcols_split), collapse=", ")
    }

    g1_list[[length(g1_list)+1]] <- data.frame(
      edon            = e,
      estado          = estados_nom[e],
      yr              = y,
      agg_success     = res_agg$success,
      split_success   = res_split$success,
      nrow_agg        = res_agg$nrow,
      nrow_split      = res_split$nrow,
      nrow_match      = nrow_match,
      efec_match      = ifelse(is.na(efec_match), "N/A", as.character(efec_match)),
      tot_match       = ifelse(is.na(tot_match),  "N/A", as.character(tot_match)),
      ncols_agg       = ifelse(is.na(ncols_agg),  NA, ncols_agg),
      ncols_split     = ifelse(is.na(ncols_split),NA, ncols_split),
      split_has_more_cols = ifelse(is.na(more_cols_in_split), "N/A",
                                   as.character(more_cols_in_split)),
      shared_parties  = shared_parties,
      unique_to_split = unique_to_split,
      unique_to_agg   = unique_to_agg,
      agg_error       = ifelse(is.na(res_agg$error),   "", res_agg$error),
      split_error     = ifelse(is.na(res_split$error), "", res_split$error),
      stringsAsFactors = FALSE
    )
  }
}
g1_df <- if (length(g1_list)) do.call(rbind, g1_list) else
  data.frame(stringsAsFactors=FALSE)

n_efec_ok <- sum(g1_df$efec_match == "TRUE",  na.rm=TRUE)
n_tot_ok  <- sum(g1_df$tot_match  == "TRUE",  na.rm=TRUE)
n_efec_bad<- sum(g1_df$efec_match == "FALSE", na.rm=TRUE)
n_tot_bad <- sum(g1_df$tot_match  == "FALSE", na.rm=TRUE)
cat("  G1 tests:", nrow(g1_df), "\n")
cat("  efec matches (agg==split):", n_efec_ok, "| mismatches:", n_efec_bad, "\n")
cat("  tot  matches (agg==split):", n_tot_ok,  "| mismatches:", n_tot_bad,  "\n")

## ---- G2: Invalid coal.agg values ----
cat("  G2: Testing invalid coal.agg values...\n")

g2_cases <- list(
  list(ca=NULL,        desc="coal.agg = NULL"),
  list(ca=NA,          desc="coal.agg = NA"),
  list(ca="yes",       desc="coal.agg = 'yes' (string)"),
  list(ca="TRUE",      desc="coal.agg = 'TRUE' (string)"),
  list(ca=1L,          desc="coal.agg = 1 (integer, coercible)"),
  list(ca=0L,          desc="coal.agg = 0 (integer, coercible to FALSE)"),
  list(ca=2,           desc="coal.agg = 2 (out of range logical)"),
  list(ca=c(TRUE,FALSE),desc="coal.agg = c(TRUE,FALSE) (vector)"),
  list(ca=FALSE,       desc="coal.agg = FALSE (valid split)"),
  list(ca=TRUE,        desc="coal.agg = TRUE  (valid agg)")
)

g2_rows <- lapply(g2_cases, function(case) {
  res <- capture_xport(14, 2018, coal.agg=case$ca)
  msg <- all_msg(res)
  data.frame(
    test_case      = case$desc,
    coal_agg_input = paste(deparse(case$ca), collapse=""),
    success        = res$success,
    nrows          = res$nrow,
    file_used      = ifelse(res$success,
                      ifelse(isTRUE(case$ca) || identical(case$ca, 1L),
                             "coalAgg.csv (correct)",
                             ifelse(identical(case$ca, FALSE) || identical(case$ca, 0L),
                                    "coalSplit.csv (correct)", "unknown")),
                      "N/A"),
    error_msg      = ifelse(is.na(res$error), "", res$error),
    printed_msg    = res$printed,
    handles_gracefully = !grepl(graceful_pat, msg, ignore.case=TRUE),
    stringsAsFactors = FALSE
  )
})
g2_df <- do.call(rbind, g2_rows)
cat("  G2 tests:", nrow(g2_df),
    "| Graceful:", sum(g2_df$handles_gracefully, na.rm=TRUE), "\n\n")

################################################################################
## SUMMARY NUMBERS                                                            ##
################################################################################
n_input_graceful <- sum(input_df$handles_gracefully, na.rm=TRUE)
n_input_crash    <- nrow(input_df) - n_input_graceful
n_verb_yes       <- sum(verb_df$assessment == "YES")
n_verb_no        <- sum(verb_df$assessment == "NO")
n_g2_graceful    <- sum(g2_df$handles_gracefully, na.rm=TRUE)

################################################################################
## GENERATE EXCEL REPORT                                                      ##
################################################################################
cat("=== Generating Excel Report ===\n")

wb <- createWorkbook()

st_title <- createStyle(fontSize=13, fontColour="#FFFFFF", fgFill="#2E4057",
                        textDecoration="bold", halign="left")
st_head  <- createStyle(fontSize=10, fontColour="#FFFFFF", fgFill="#1B6CA8",
                        textDecoration="bold", halign="center", wrapText=TRUE,
                        border="TopBottomLeftRight", borderColour="#AAAAAA")
st_ok    <- createStyle(fgFill="#C8E6C9", fontColour="#1B5E20")
st_warn  <- createStyle(fgFill="#FFF9C4", fontColour="#7B4F00")
st_err   <- createStyle(fgFill="#FFCDD2", fontColour="#B71C1C")
st_info  <- createStyle(fgFill="#E3F2FD", fontColour="#0D47A1")
st_orange<- createStyle(fgFill="#FFE0B2", fontColour="#BF360C")

write_title <- function(wb, sheet, title, ncols) {
  writeData(wb, sheet, title, startRow=1, startCol=1)
  if (ncols > 1) mergeCells(wb, sheet, cols=1:ncols, rows=1)
  addStyle(wb, sheet, st_title, rows=1, cols=1:ncols, gridExpand=TRUE)
}

color_rows <- function(wb, sheet, df, start_row, ncols, style_fn) {
  for (ri in seq_len(nrow(df))) {
    sty <- style_fn(df, ri)
    if (!is.null(sty)) addStyle(wb, sheet, sty, rows=ri+start_row, cols=1:ncols, gridExpand=TRUE)
  }
}

## ---- TAB 1: Summary ----
cat("  Writing Summary...\n")
addWorksheet(wb, "Summary")
write_title(wb, "Summary", "STRESS TEST SUMMARY — Electoral Returns Extraction (xport)", 4)

smry <- data.frame(
  Category = c(
    "General","General","General",
    "A. Data Coverage","A. Data Coverage","A. Data Coverage",
    "B. Non-Election Years","B. Non-Election Years",
    "C. Input Errors","C. Input Errors","C. Input Errors",
    "D. Output Quality","D. Output Quality",
    "E. Verbosity","E. Verbosity","E. Verbosity",
    "F1. Incomplete Mun Data","F1. Incomplete Mun Data",
    "F2. Absent Municipalities","F2. Absent Municipalities",
    "G1. coal.agg Consistency","G1. coal.agg Consistency","G1. coal.agg Consistency",
    "G2. coal.agg Invalid Inputs","G2. coal.agg Invalid Inputs"
  ),
  Metric = c(
    "Report generated","Data file","Years in dataset",
    "State-year pairs tested","Pairs WITH data","Pairs WITHOUT data",
    "Non-election year tests run","Tests with informative error message",
    "Input error cases tested","Cases handled gracefully","Cases crashing / cryptic error",
    "Output quality checks (state-year)","Issues found (D1–D7)",
    "Verbosity tests run","Responses sufficiently informative","Responses NOT informative",
    "Municipalities with incomplete data (F1)","Most common issue",
    "Municipalities expected but absent, 1988+ (F2)","States most affected",
    "State-year comparisons (agg vs split)","efec totals match across both files","tot totals match across both files",
    "Invalid coal.agg cases tested","Cases handled gracefully"
  ),
  Value = c(
    format(Sys.time(),"%Y-%m-%d %H:%M"),
    "aymu1970-on.coalAgg.csv + coalSplit.csv",
    paste0(min(all_years_in_data),"–",max(all_years_in_data)," (",length(all_years_in_data)," years)"),
    n_cover, nrow(coverage_w_data), nrow(coverage_no_data),
    nrow(non_elec_df), sum(non_elec_df$distinguishes_no_election,na.rm=TRUE),
    nrow(input_df), n_input_graceful, n_input_crash,
    nrow(coverage_w_data), nrow(out_err_df),
    nrow(verb_df), n_verb_yes, n_verb_no,
    nrow(f1_df),
    if(nrow(f1_df)>0) names(sort(table(unlist(strsplit(f1_df$issues_found," \\| "))),decreasing=TRUE))[1] else "none",
    nrow(f2_df),
    if(nrow(f2_df)>0) paste(head(names(sort(table(f2_df$edon),decreasing=TRUE)),3),collapse=", ") else "none",
    nrow(g1_df), paste0(n_efec_ok,"/",nrow(g1_df)), paste0(n_tot_ok,"/",nrow(g1_df)),
    nrow(g2_df), n_g2_graceful
  ),
  Status = c(
    "INFO","INFO","INFO",
    "INFO","INFO","INFO",
    "INFO", ifelse(sum(non_elec_df$distinguishes_no_election,na.rm=TRUE) > nrow(non_elec_df)*0.5,"WARN","FAIL"),
    "INFO",
    ifelse(n_input_graceful==nrow(input_df),"PASS","FAIL"),
    ifelse(n_input_crash==0,"PASS","FAIL"),
    "INFO", ifelse(nrow(out_err_df)==0,"PASS","WARN"),
    "INFO",
    ifelse(n_verb_yes>=nrow(verb_df)*0.7,"PASS","FAIL"),
    ifelse(n_verb_no==0,"PASS","FAIL"),
    ifelse(nrow(f1_df)==0,"PASS","WARN"),"INFO",
    ifelse(nrow(f2_df)==0,"PASS","WARN"),"INFO",
    "INFO",
    ifelse(n_efec_bad==0,"PASS","FAIL"),
    ifelse(n_tot_bad==0,"PASS","FAIL"),
    "INFO",
    ifelse(n_g2_graceful==nrow(g2_df),"PASS","WARN")
  ),
  stringsAsFactors=FALSE
)
writeData(wb, "Summary", smry, startRow=3, headerStyle=st_head)
setColWidths(wb, "Summary", cols=1:4, widths=c(30,55,38,10))
color_rows(wb,"Summary",smry,3,4,function(df,ri){
  switch(df$Status[ri],PASS=st_ok,FAIL=st_err,WARN=st_warn,INFO=st_info,NULL)})

## ---- TAB 2: Missing_Data ----
cat("  Writing Missing_Data...\n")
addWorksheet(wb, "Missing_Data")
write_title(wb,"Missing_Data","MISSING DATA: State-year combinations with zero rows in dataset",4)
no_data_out <- coverage_no_data[order(coverage_no_data$edon,coverage_no_data$yr),]
no_data_out$note <- "xport() returns 0 rows — no election held this year, or data not yet loaded"
writeData(wb,"Missing_Data",no_data_out[,c("edon","estado","yr","note")],startRow=3,headerStyle=st_head)
setColWidths(wb,"Missing_Data",cols=1:4,widths=c(8,30,8,55))

## ---- TAB 3: Input_Errors ----
cat("  Writing Input_Errors...\n")
addWorksheet(wb, "Input_Errors")
write_title(wb,"Input_Errors","INPUT ERROR TESTS: Robustness of xport() to invalid / unexpected inputs",10)
writeData(wb,"Input_Errors",input_df,startRow=3,headerStyle=st_head)
setColWidths(wb,"Input_Errors",cols=1:10,widths=c(35,15,22,38,10,12,55,30,40,18))
color_rows(wb,"Input_Errors",input_df,3,10,function(df,ri){
  if(df$handles_gracefully[ri]) st_ok else st_err})

## ---- TAB 4: Output_Errors ----
cat("  Writing Output_Errors...\n")
addWorksheet(wb, "Output_Errors")
write_title(wb,"Output_Errors","OUTPUT ERRORS: Data quality issues found in coalAgg dataset",7)
if (nrow(out_err_df)) {
  oe <- out_err_df[order(out_err_df$edon,out_err_df$yr),]
  writeData(wb,"Output_Errors",oe,startRow=3,headerStyle=st_head)
  setColWidths(wb,"Output_Errors",cols=1:7,widths=c(8,28,8,15,35,30,65))
  color_rows(wb,"Output_Errors",oe,3,7,function(df,ri) st_warn)
} else {
  writeData(wb,"Output_Errors",
            data.frame(result="No output errors — all D1-D7 checks passed"),
            startRow=3,headerStyle=st_head)
  addStyle(wb,"Output_Errors",st_ok,rows=4,cols=1)
}

## ---- TAB 5: Verbosity ----
cat("  Writing Verbosity...\n")
addWorksheet(wb,"Verbosity")
write_title(wb,"Verbosity","VERBOSITY: Quality of error messages and user feedback from xport()",8)
writeData(wb,"Verbosity",verb_df,startRow=3,headerStyle=st_head)
setColWidths(wb,"Verbosity",cols=1:8,widths=c(30,12,20,65,10,8,10,70))
color_rows(wb,"Verbosity",verb_df,3,8,function(df,ri){
  switch(df$assessment[ri],YES=st_ok,NO=st_err,PARTIAL=st_warn,NULL)})

## ---- TAB 6: Municipalities_Coverage (FIXED) ----
cat("  Writing Municipalities_Coverage...\n")
addWorksheet(wb,"Municipalities_Coverage")
write_title(wb,"Municipalities_Coverage",
  "MUNICIPALITY COVERAGE: F1=incomplete data present | F2=completely absent (expected per mun.yrs.csv, 1988+)",8)

if (nrow(mun_coverage_df)) {
  writeData(wb,"Municipalities_Coverage",mun_coverage_df,startRow=3,headerStyle=st_head)
  setColWidths(wb,"Municipalities_Coverage",cols=1:8,widths=c(18,8,28,8,12,35,60,10))
  color_rows(wb,"Municipalities_Coverage",mun_coverage_df,3,8,function(df,ri){
    if(df$type[ri]=="INCOMPLETE DATA") st_warn else st_orange})
} else {
  writeData(wb,"Municipalities_Coverage",
            data.frame(result="No coverage issues found"),startRow=3,headerStyle=st_head)
  addStyle(wb,"Municipalities_Coverage",st_ok,rows=4,cols=1)
}

## ---- TAB 7: Non_Election_Years ----
cat("  Writing Non_Election_Years...\n")
addWorksheet(wb,"Non_Election_Years")
write_title(wb,"Non_Election_Years",
  "NON-ELECTION YEAR TESTS: xport() behaviour when queried for years with no election in that state",8)
ne_sorted <- non_elec_df[order(non_elec_df$edon,non_elec_df$yr),]
writeData(wb,"Non_Election_Years",ne_sorted,startRow=3,headerStyle=st_head)
setColWidths(wb,"Non_Election_Years",cols=1:8,widths=c(8,28,8,10,10,10,65,22))
color_rows(wb,"Non_Election_Years",ne_sorted,3,8,function(df,ri){
  if(df$distinguishes_no_election[ri]) st_ok else st_warn})

## ---- TAB 8: CoalAgg_Tests (NEW) ----
cat("  Writing CoalAgg_Tests...\n")
addWorksheet(wb,"CoalAgg_Tests")

# G1
write_title(wb,"CoalAgg_Tests",
  "COAL.AGG CONSISTENCY TESTS — G1: coalAgg vs coalSplit comparison | G2: invalid coal.agg values",18)

writeData(wb,"CoalAgg_Tests","G1 — coalAgg vs coalSplit: efec & tot must match; split should have ≥ party columns",
          startRow=3,startCol=1)
addStyle(wb,"CoalAgg_Tests",st_info,rows=3,cols=1:18,gridExpand=TRUE)

if (nrow(g1_df)) {
  writeData(wb,"CoalAgg_Tests",g1_df,startRow=4,headerStyle=st_head)
  setColWidths(wb,"CoalAgg_Tests",cols=1:18,
               widths=c(8,25,8,10,10,10,10,12,12,12,12,12,18,30,25,18,35,35))
  for (ri in seq_len(nrow(g1_df))) {
    efec_ok <- g1_df$efec_match[ri]=="TRUE"
    tot_ok  <- g1_df$tot_match[ri]=="TRUE"
    sty <- if (efec_ok && tot_ok) st_ok else if (!efec_ok || !tot_ok) st_err else st_warn
    addStyle(wb,"CoalAgg_Tests",sty,rows=ri+4,cols=1:18,gridExpand=TRUE)
  }
}

# G2
g2_start <- nrow(g1_df) + 7
writeData(wb,"CoalAgg_Tests","G2 — Invalid coal.agg values: should be handled gracefully",
          startRow=g2_start,startCol=1)
addStyle(wb,"CoalAgg_Tests",st_info,rows=g2_start,cols=1:8,gridExpand=TRUE)
writeData(wb,"CoalAgg_Tests",g2_df,startRow=g2_start+1,headerStyle=st_head)
for (ri in seq_len(nrow(g2_df))) {
  sty <- if(g2_df$handles_gracefully[ri]) st_ok else st_err
  addStyle(wb,"CoalAgg_Tests",sty,rows=ri+g2_start+1,cols=1:8,gridExpand=TRUE)
}

## Save
saveWorkbook(wb, OUTPUT_FILE, overwrite=TRUE)
cat("\n=== DONE ===\n")
cat("Saved:", OUTPUT_FILE, "\n")
cat("Finished:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
