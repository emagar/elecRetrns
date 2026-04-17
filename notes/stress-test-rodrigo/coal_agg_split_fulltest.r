####################################################################
## coal_agg_split_fulltest.r
##
## Compares coalAgg vs coalSplit for ALL 32 states × ALL years
## in the dataset. Only state-year-municipality combinations
## with discrepancies are written to the output Excel file.
##
## Comparisons per state-year:
##   C1. Same municipalities in both files?            (by INEGI code)
##   C2. efec identical per municipality?
##   C3. tot  identical per municipality?
##   C4. nulos identical per municipality?
##   C5. nr    identical per municipality?
##   C6. lisnom identical per municipality?
##   C7. ncand identical per municipality?
##   C8. Number of unique party labels differs?
##       (proxy for how many columns xport() would create)
##   C9. Sum of votes ≠ efec in either file?           (internal check)
##
## Output: coal_agg_split_discrepancies.xlsx
##   Tab 1 — Summary       (counts by state and check type)
##   Tab 2 — Discrepancies (only rows/combos with errors)
##   Tab 3 — Coverage      (all state-years compared, pass/fail)
####################################################################

# 0. SETUP ----
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  install.packages("openxlsx", repos = "https://cloud.r-project.org")
}
library(openxlsx)

cat("=== coalAgg vs coalSplit Full Comparison ===\n")
cat("Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

BASE_DIR    <- "C:/Users/rodri/OneDrive - INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO/Escritorio/elecRetrns"
DATA_DIR    <- file.path(BASE_DIR, "data")
OUTPUT_FILE <- file.path(BASE_DIR, "coal_agg_split_discrepancies.xlsx")

# Load both files
cat("Loading data files...\n")
agg   <- read.csv(file.path(DATA_DIR, "aymu1970-on.coalAgg.csv"),   stringsAsFactors = FALSE)
split <- read.csv(file.path(DATA_DIR, "aymu1970-on.coalSplit.csv"), stringsAsFactors = FALSE)
cat("  coalAgg.csv  :", nrow(agg),   "rows,", ncol(agg),   "cols\n")
cat("  coalSplit.csv:", nrow(split), "rows,", ncol(split), "cols\n\n")

# State names
estados_nom <- c(
  "Aguascalientes","Baja California","Baja California Sur","Campeche",
  "Coahuila","Colima","Chiapas","Chihuahua","Distrito Federal/CDMX","Durango",
  "Guanajuato","Guerrero","Hidalgo","Jalisco","México","Michoacán","Morelos",
  "Nayarit","Nuevo León","Oaxaca","Puebla","Querétaro","Quintana Roo",
  "San Luis Potosí","Sinaloa","Sonora","Tabasco","Tamaulipas","Tlaxcala",
  "Veracruz","Yucatán","Zacatecas")

# Column groups
v_cols <- grep("^v[0-9]{2}$", colnames(agg), value = TRUE)
l_cols <- grep("^l[0-9]{2}$", colnames(agg), value = TRUE)
meta_cols <- c("efec","nr","nulos","tot","lisnom","ncand","ncoal","win","mg","status")

# All state-year combinations present in EITHER file
sy_agg   <- unique(agg[,   c("edon","yr")])
sy_split <- unique(split[, c("edon","yr")])
sy_all   <- unique(rbind(sy_agg, sy_split))
sy_all   <- sy_all[order(sy_all$edon, sy_all$yr), ]
N        <- nrow(sy_all)
cat("Total state-year combinations to compare:", N, "\n\n")

################################################################################
## MAIN COMPARISON LOOP                                                       ##
################################################################################
disc_list    <- list()   # rows with discrepancies (Tab 2)
coverage_list<- list()   # one row per state-year (Tab 3)

for (i in seq_len(N)) {
  if (i %% 50 == 0) cat("  Progress:", i, "/", N, "\n")

  e <- sy_all$edon[i]
  y <- sy_all$yr[i]

  sb_agg   <- agg[agg$edon     == e & agg$yr     == y, ]
  sb_split <- split[split$edon == e & split$yr   == y, ]

  in_agg_only   <- FALSE
  in_split_only <- FALSE
  n_checks_failed <- 0L
  checks_failed   <- character(0)

  # Only in one file?
  if (nrow(sb_agg) == 0 && nrow(sb_split) == 0) next
  if (nrow(sb_agg) == 0) {
    in_split_only <- TRUE
    n_checks_failed <- n_checks_failed + 1L
    checks_failed <- c(checks_failed, "C1: state-year only in coalSplit")
  }
  if (nrow(sb_split) == 0) {
    in_agg_only <- TRUE
    n_checks_failed <- n_checks_failed + 1L
    checks_failed <- c(checks_failed, "C1: state-year only in coalAgg")
  }

  if (!in_agg_only && !in_split_only) {

    # C1: Same set of INEGI codes?
    inegi_agg   <- sort(unique(sb_agg$inegi))
    inegi_split <- sort(unique(sb_split$inegi))
    missing_in_split <- setdiff(inegi_agg,   inegi_split)
    missing_in_agg   <- setdiff(inegi_split, inegi_agg)
    if (length(missing_in_split) || length(missing_in_agg)) {
      n_checks_failed <- n_checks_failed + 1L
      checks_failed   <- c(checks_failed, "C1: municipality set differs")
      # Record per-municipality discrepancy
      for (code in missing_in_split) {
        mun_name <- sb_agg$mun[sb_agg$inegi == code][1]
        disc_list[[length(disc_list)+1]] <- data.frame(
          edon=e, estado=estados_nom[e], yr=y, inegi=code, mun=mun_name,
          check="C1", description="Municipality in coalAgg but ABSENT in coalSplit",
          value_agg=as.character(nrow(sb_agg[sb_agg$inegi==code,])),
          value_split="0",
          stringsAsFactors=FALSE)
      }
      for (code in missing_in_agg) {
        mun_name <- sb_split$mun[sb_split$inegi == code][1]
        disc_list[[length(disc_list)+1]] <- data.frame(
          edon=e, estado=estados_nom[e], yr=y, inegi=code, mun=mun_name,
          check="C1", description="Municipality in coalSplit but ABSENT in coalAgg",
          value_agg="0",
          value_split=as.character(nrow(sb_split[sb_split$inegi==code,])),
          stringsAsFactors=FALSE)
      }
    }

    # Merge on INEGI for municipality-level comparisons
    common_inegi <- intersect(inegi_agg, inegi_split)
    if (length(common_inegi)) {

      # Only select columns that actually exist in each file
      want_cols    <- c("inegi", "mun", meta_cols, v_cols, l_cols)
      cols_agg     <- intersect(want_cols, colnames(sb_agg))
      cols_split   <- intersect(want_cols, colnames(sb_split))

      merged <- merge(
        sb_agg[sb_agg$inegi     %in% common_inegi, cols_agg],
        sb_split[sb_split$inegi %in% common_inegi, cols_split],
        by = "inegi", suffixes = c(".agg", ".split")
      )

      for (chk in c("efec","tot","nulos","nr","lisnom","ncand")) {
        col_agg   <- paste0(chk, ".agg")
        col_split <- paste0(chk, ".split")
        check_id  <- switch(chk,
          efec="C2", tot="C3", nulos="C4", nr="C5", lisnom="C6", ncand="C7")

        if (all(c(col_agg, col_split) %in% colnames(merged))) {
          va <- suppressWarnings(as.numeric(merged[[col_agg]]))
          vs <- suppressWarnings(as.numeric(merged[[col_split]]))
          bad <- which(!is.na(va) & !is.na(vs) & abs(va - vs) > 0)

          if (length(bad)) {
            n_checks_failed <- n_checks_failed + 1L
            checks_failed   <- c(checks_failed,
                                  paste0(check_id, ": ", chk, " differs"))
            for (j in bad) {
              disc_list[[length(disc_list)+1]] <- data.frame(
                edon=e, estado=estados_nom[e], yr=y,
                inegi=merged$inegi[j],
                mun=if("mun.agg" %in% colnames(merged)) merged$mun.agg[j] else merged$mun[j],
                check=check_id,
                description=paste0(chk, " differs between files"),
                value_agg=as.character(va[j]),
                value_split=as.character(vs[j]),
                stringsAsFactors=FALSE)
            }
          }
        }
      }

      # C8: Number of unique party labels differs?
      l_cols_agg   <- intersect(l_cols, colnames(sb_agg))
      l_cols_split <- intersect(l_cols, colnames(sb_split))
      labels_agg   <- as.vector(unlist(sb_agg[,   l_cols_agg,   drop = FALSE]))
      labels_split <- as.vector(unlist(sb_split[, l_cols_split, drop = FALSE]))
      n_labels_agg   <- length(unique(labels_agg[!is.na(labels_agg)   & labels_agg   != "0" & labels_agg   != ""]))
      n_labels_split <- length(unique(labels_split[!is.na(labels_split) & labels_split != "0" & labels_split != ""]))

      if (n_labels_split < n_labels_agg) {
        # Split should have >= unique labels than agg (more granular)
        n_checks_failed <- n_checks_failed + 1L
        checks_failed   <- c(checks_failed, "C8: coalSplit has FEWER party labels than coalAgg (unexpected)")
        disc_list[[length(disc_list)+1]] <- data.frame(
          edon=e, estado=estados_nom[e], yr=y, inegi="ALL", mun="ALL",
          check="C8",
          description="coalSplit has fewer unique party labels than coalAgg (should be ≥)",
          value_agg=as.character(n_labels_agg),
          value_split=as.character(n_labels_split),
          stringsAsFactors=FALSE)
      }

      # C9: Internal check — sum(v cols) vs efec in each file
      for (file_tag in c("agg","split")) {
        sb_chk   <- if (file_tag=="agg") sb_agg else sb_split
        v_cols_chk <- intersect(v_cols, colnames(sb_chk))
        if (length(v_cols_chk) && "efec" %in% colnames(sb_chk)) {
          v_sum <- rowSums(sb_chk[, v_cols_chk, drop=FALSE], na.rm=TRUE)
          bad9  <- which(!is.na(sb_chk$efec) & abs(v_sum - sb_chk$efec) > 1)
          if (length(bad9)) {
            n_checks_failed <- n_checks_failed + 1L
            checks_failed   <- c(checks_failed,
                                  paste0("C9: sum(votes) ≠ efec in coal",
                                         ifelse(file_tag=="agg","Agg","Split")))
            for (j in bad9) {
              disc_list[[length(disc_list)+1]] <- data.frame(
                edon=e, estado=estados_nom[e], yr=y,
                inegi=sb_chk$inegi[j], mun=sb_chk$mun[j],
                check="C9",
                description=paste0("sum(v01..v23) ≠ efec in coal",
                                   ifelse(file_tag=="agg","Agg","Split")),
                value_agg=ifelse(file_tag=="agg",  as.character(v_sum[j]), "—"),
                value_split=ifelse(file_tag=="split",as.character(v_sum[j]),"—"),
                stringsAsFactors=FALSE)
            }
          }
        }
      }
    }
  }

  # Coverage record (one per state-year)
  coverage_list[[length(coverage_list)+1]] <- data.frame(
    edon            = e,
    estado          = estados_nom[e],
    yr              = y,
    nrow_agg        = nrow(sb_agg),
    nrow_split      = nrow(sb_split),
    nrow_match      = nrow(sb_agg) == nrow(sb_split),
    n_checks_failed = n_checks_failed,
    checks_failed   = paste(checks_failed, collapse=" | "),
    result          = ifelse(n_checks_failed == 0, "PASS", "FAIL"),
    stringsAsFactors = FALSE
  )
}

cat("\nComparison complete.\n")

disc_df     <- if (length(disc_list))
  do.call(rbind, disc_list) else
  data.frame(edon=integer(),estado=character(),yr=integer(),inegi=character(),
             mun=character(),check=character(),description=character(),
             value_agg=character(),value_split=character(),stringsAsFactors=FALSE)

coverage_df <- do.call(rbind, coverage_list)

n_pass <- sum(coverage_df$result == "PASS")
n_fail <- sum(coverage_df$result == "FAIL")
cat("  State-year pairs compared:", nrow(coverage_df), "\n")
cat("  PASS (no discrepancies)  :", n_pass, "\n")
cat("  FAIL (at least 1 issue)  :", n_fail, "\n")
cat("  Total discrepancy rows   :", nrow(disc_df), "\n\n")

################################################################################
## GENERATE EXCEL                                                             ##
################################################################################
cat("Generating Excel report...\n")

wb <- createWorkbook()

st_title <- createStyle(fontSize=13, fontColour="#FFFFFF", fgFill="#2E4057",
                        textDecoration="bold", halign="left")
st_head  <- createStyle(fontSize=10, fontColour="#FFFFFF", fgFill="#1B6CA8",
                        textDecoration="bold", halign="center", wrapText=TRUE,
                        border="TopBottomLeftRight", borderColour="#AAAAAA")
st_ok    <- createStyle(fgFill="#C8E6C9", fontColour="#1B5E20")
st_err   <- createStyle(fgFill="#FFCDD2", fontColour="#B71C1C")
st_warn  <- createStyle(fgFill="#FFF9C4", fontColour="#7B4F00")
st_info  <- createStyle(fgFill="#E3F2FD", fontColour="#0D47A1")

write_title <- function(wb, sheet, title, ncols) {
  writeData(wb, sheet, title, startRow=1, startCol=1)
  if (ncols > 1) mergeCells(wb, sheet, cols=1:ncols, rows=1)
  addStyle(wb, sheet, st_title, rows=1, cols=1:ncols, gridExpand=TRUE)
}

## ---- TAB 1: Summary ----
addWorksheet(wb, "Summary")
write_title(wb, "Summary",
  "coalAgg vs coalSplit — Full Comparison Summary (ALL states × ALL years)", 4)

# Discrepancies by check type
disc_by_check <- if (nrow(disc_df)) as.data.frame(table(check=disc_df$check)) else
  data.frame(check=character(), Freq=integer())
disc_by_state <- if (nrow(disc_df)) {
  tmp <- as.data.frame(table(edon=disc_df$edon))
  tmp$estado <- estados_nom[as.integer(as.character(tmp$edon))]
  tmp[order(-tmp$Freq), ]
} else data.frame(edon=character(), Freq=integer(), estado=character())

smry <- data.frame(
  Metric = c(
    "Report generated",
    "coalAgg file",
    "coalSplit file",
    "State-year pairs compared",
    "PASS — no discrepancies",
    "FAIL — at least one issue",
    "Total discrepancy rows written",
    "C1: Municipality set differs",
    "C2: efec differs",
    "C3: tot differs",
    "C4: nulos differs",
    "C5: nr differs",
    "C6: lisnom differs",
    "C7: ncand differs",
    "C8: coalSplit has fewer party labels (unexpected)",
    "C9: sum(votes) ≠ efec (internal check)"
  ),
  Value = c(
    format(Sys.time(), "%Y-%m-%d %H:%M"),
    "aymu1970-on.coalAgg.csv",
    "aymu1970-on.coalSplit.csv",
    nrow(coverage_df),
    n_pass,
    n_fail,
    nrow(disc_df),
    sum(grepl("C1", disc_df$check)),
    sum(grepl("C2", disc_df$check)),
    sum(grepl("C3", disc_df$check)),
    sum(grepl("C4", disc_df$check)),
    sum(grepl("C5", disc_df$check)),
    sum(grepl("C6", disc_df$check)),
    sum(grepl("C7", disc_df$check)),
    sum(grepl("C8", disc_df$check)),
    sum(grepl("C9", disc_df$check))
  ),
  Status = c(
    "INFO","INFO","INFO","INFO",
    ifelse(n_pass == nrow(coverage_df), "PASS", "INFO"),
    ifelse(n_fail == 0, "PASS", "FAIL"),
    ifelse(nrow(disc_df) == 0, "PASS", "FAIL"),
    rep(ifelse(nrow(disc_df) == 0, "PASS", "WARN"), 9)
  ),
  stringsAsFactors = FALSE
)

writeData(wb, "Summary", smry, startRow=3, headerStyle=st_head)
setColWidths(wb, "Summary", cols=1:3, widths=c(50, 40, 10))
for (ri in seq_len(nrow(smry))) {
  sty <- switch(smry$Status[ri], PASS=st_ok, FAIL=st_err, WARN=st_warn, INFO=st_info, NULL)
  if (!is.null(sty)) addStyle(wb, "Summary", sty, rows=ri+3, cols=1:3, gridExpand=TRUE)
}

# Top states with most discrepancies
if (nrow(disc_by_state)) {
  top_row <- nrow(smry) + 6
  writeData(wb, "Summary", "Top states with most discrepancies:", startRow=top_row)
  addStyle(wb, "Summary", st_title, rows=top_row, cols=1:3, gridExpand=TRUE)
  writeData(wb, "Summary", disc_by_state[, c("edon","estado","Freq")],
            startRow=top_row+1, headerStyle=st_head)
  for (ri in seq_len(nrow(disc_by_state)))
    addStyle(wb, "Summary", st_err, rows=ri+top_row+1, cols=1:3, gridExpand=TRUE)
}

## ---- TAB 2: Discrepancies ----
addWorksheet(wb, "Discrepancies")
write_title(wb, "Discrepancies",
  "DISCREPANCIES: Only state-year-municipality combinations where coalAgg ≠ coalSplit", 9)

if (nrow(disc_df)) {
  disc_sorted <- disc_df[order(disc_df$check, disc_df$edon, disc_df$yr, disc_df$mun), ]
  writeData(wb, "Discrepancies", disc_sorted, startRow=3, headerStyle=st_head)
  setColWidths(wb, "Discrepancies", cols=1:9,
               widths=c(8, 28, 8, 12, 35, 6, 55, 20, 20))
  # Color by check type
  check_colors <- list(
    C1=st_err, C2=st_warn, C3=st_warn, C4=st_warn,
    C5=st_warn, C6=st_warn, C7=st_warn, C8=st_err, C9=st_err
  )
  for (ri in seq_len(nrow(disc_sorted))) {
    sty <- check_colors[[disc_sorted$check[ri]]]
    if (!is.null(sty)) addStyle(wb, "Discrepancies", sty, rows=ri+3, cols=1:9, gridExpand=TRUE)
  }
} else {
  writeData(wb, "Discrepancies",
            data.frame(result="No discrepancies found — coalAgg and coalSplit are fully consistent"),
            startRow=3, headerStyle=st_head)
  addStyle(wb, "Discrepancies", st_ok, rows=4, cols=1)
}

## ---- TAB 3: Coverage ----
addWorksheet(wb, "Coverage")
write_title(wb, "Coverage",
  "COVERAGE: All state-year combinations compared — green = PASS, red = FAIL", 9)

cov_sorted <- coverage_df[order(coverage_df$edon, coverage_df$yr), ]
writeData(wb, "Coverage", cov_sorted, startRow=3, headerStyle=st_head)
setColWidths(wb, "Coverage", cols=1:9, widths=c(8,28,8,12,12,12,15,60,10))
for (ri in seq_len(nrow(cov_sorted))) {
  sty <- if (cov_sorted$result[ri] == "PASS") st_ok else st_err
  addStyle(wb, "Coverage", sty, rows=ri+3, cols=1:9, gridExpand=TRUE)
}

## Save
saveWorkbook(wb, OUTPUT_FILE, overwrite=TRUE)
cat("Saved:", OUTPUT_FILE, "\n")
cat("Finished:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
