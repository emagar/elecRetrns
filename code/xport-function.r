####################################################################
## Script to export single state-year returns of municipal races. ##
## Invoked from extract-state-yr-mu-returns.r in                  ##
## https://github.com/emagar/elecRetrns.git                       ##
##                                                                ##
## Author: Eric Magar emagar at itam dot mx                       ##
## Created:      13-mar-2021                                      ##
## Last revised: 4-apr-2025                                       ##
## Local copy with verbosity improvements: 5-apr-2026             ##
## Changes: Tier 2 (input validation + state abbreviation         ##
##   support) and Tier 3 (diagnostic messages when no data found, ##
##   cross-referencing resumen-haber-datos). Also fixes break bug ##
##   and silent paste() on write.to.file success.                 ##
####################################################################

xport <- function(e = NA, y = NA, coal.agg = TRUE, write.to.file=FALSE){
    ## Function will take municipal vote returns (with coalition aggregates if coal.agg is TRUE, split otherwise)
    ## and export a single state-year (or write it to file if write.to.file set to TRUE),
    ## re-arranging the data frame so that vote returns appear in columns named after the corresponding party/coalition.
    ## Choose a state number (eg. e=2 for Baja California) and a known electoral year (eg. y=2019) to output a dataframe
    ## with municipalities reported in each row.
    ##
    ## State abbreviations
    edos <- c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac")
    ## State names
    estados <- c("Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua", "Distrito Federal/CDMX", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "M\u00e9xico (Estado de)", "Michoac\u00e1n", "Morelos", "Nayarit", "Nuevo Le\u00f3n", "Oaxaca", "Puebla", "Quer\u00e9taro", "Quintana Roo", "San Luis Potos\u00ed", "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucat\u00e1n", "Zacatecas")
    ##
    ## Helper: format state list for display (used in error messages)
    .state_table <- function() {
        paste(sprintf("  %2d = %-4s (%s)", 1:32, edos, estados), collapse = "\n")
    }
    ##
    ## ================================================================
    ## TIER 2: Input validation
    ## ================================================================
    ##
    ## --- Validate e (state) -----------------------------------------
    ## Accepts: integer 1-32, abbreviation string, or full state name
    if (length(e) != 1 || is.na(e)) {
        stop("Argument 'e' is missing or NA. Must be a state number (1-32), ",
             "abbreviation, or name.\nValid states:\n", .state_table())
    }
    if (is.character(e)) {
        e_input <- e  # preserve original for error messages
        e_lower <- tolower(trimws(e))
        ## Try exact abbreviation match first
        idx <- which(edos == e_lower)
        if (length(idx) == 1) {
            e <- idx
            message("xport: Resolved abbreviation '", e_input, "' -> edon=", e,
                    " (", estados[e], ")")
        } else {
            ## Try prefix match on abbreviations (anchored to avoid bc/bcs problem)
            idx <- which(startsWith(edos, e_lower))
            if (length(idx) == 1) {
                e <- idx
                message("xport: Resolved abbreviation '", e_input, "' -> edon=", e,
                        " (", estados[e], ")")
            } else if (length(idx) > 1) {
                candidates <- paste(sprintf("  %d = %s (%s)", idx, edos[idx], estados[idx]),
                                    collapse = "\n")
                stop("Ambiguous abbreviation '", e_input, "'. Matches multiple states:\n",
                     candidates, "\n  Please use the exact abbreviation or edon number.")
            } else {
                ## Try partial match on full state names (case-insensitive)
                idx <- grep(e_lower, tolower(estados))
                if (length(idx) == 1) {
                    e <- idx
                    message("xport: Resolved state name '", e_input, "' -> edon=", e,
                            " (", estados[e], ")")
                } else if (length(idx) > 1) {
                    candidates <- paste(sprintf("  %d = %s (%s)", idx, edos[idx], estados[idx]),
                                        collapse = "\n")
                    stop("Ambiguous state name '", e_input, "'. Matches multiple states:\n",
                         candidates, "\n  Please use the exact abbreviation or edon number.")
                } else {
                    stop("Unrecognized state '", e_input, "'. Not a valid abbreviation or name.\n",
                         "Valid states:\n", .state_table())
                }
            }
        }
    } else if (is.numeric(e)) {
        if (e != floor(e) || e < 1 || e > 32) {
            stop("Argument 'e' out of range. Must be an integer 1-32. Received: ", e,
                 "\nValid states:\n", .state_table())
        }
    } else {
        stop("Argument 'e' must be numeric (1-32) or character (abbreviation/name). ",
             "Received class: ", class(e))
    }
    ##
    ## --- Validate y (year) ------------------------------------------
    if (length(y) != 1 || is.na(y)) {
        stop("Argument 'y' is missing or NA. Must be a numeric election year (e.g. y=2019).")
    }
    if (!is.numeric(y)) {
        stop("Argument 'y' must be numeric. Received: ", deparse(y),
             " (class: ", class(y), ")")
    }
    if (y != floor(y)) {
        stop("Argument 'y' must be an integer year. Received: ", y)
    }
    if (y < 1970 || y > 2100) {
        stop("Argument 'y' out of range (1970-2100). Received: ", y)
    }
    ##
    ## --- Validate coal.agg ------------------------------------------
    if (length(coal.agg) != 1 || is.na(coal.agg) || !is.logical(coal.agg)) {
        stop("Argument 'coal.agg' must be TRUE or FALSE. Received: ", deparse(coal.agg))
    }
    ##
    ## --- Validate write.to.file -------------------------------------
    if (length(write.to.file) != 1 || is.na(write.to.file) || !is.logical(write.to.file)) {
        stop("Argument 'write.to.file' must be TRUE or FALSE. Received: ", deparse(write.to.file))
    }
    if (write.to.file == TRUE && !dir.exists("xport")) {
        message("xport: Creating 'xport/' directory (did not exist).")
        dir.create("xport", recursive = TRUE)
    }
    ##
    ## --- Confirmation message ---------------------------------------
    file_label <- ifelse(coal.agg, "coalAgg", "coalSplit")
    message("xport: Processing ", estados[e], " (", edos[e], ", edon=", e,
            "), year=", y, ", file=", file_label)
    ##
    ## ================================================================
    ## Load data
    ## ================================================================
    file_name <- ifelse(coal.agg == TRUE, "aymu1970-on.coalAgg.csv", "aymu1970-on.coalSplit.csv")
    pth <- paste0("https://raw.githubusercontent.com/emagar/elecRetrns/refs/heads/master/data/", file_name)
    if (!file.exists(file_name)) {
        message("xport: Downloading ", file_name, " from GitHub...")
        download.file(pth, destfile = file_name)
    }
    dat <- read.csv(file_name, stringsAsFactors = FALSE)
    #
    ## subset data to selection
    dat <- dat[dat$edon==e & dat$yr==y,]
    ##
    ## ================================================================
    ## Remove cancelled/appointed rows to avoid duplicates
    ## ================================================================
    ## Some municipalities appear twice: once for the ordinary election
    ## (status "ok" or "extra") and again for a cancelled or appointed
    ## outcome. Keeping both would duplicate the municipality in the
    ## exported data. Remove rows whose status contains "cancelled" or
    ## "appointed" ONLY when there is another row for the same
    ## municipality (same inegi) that is not cancelled/appointed.
    if (nrow(dat) > 0 && "status" %in% colnames(dat)) {
        is_cancel <- grepl("cancelled|appointed", dat$status, ignore.case = TRUE)
        if (any(is_cancel)) {
            ## Which inegi codes have at least one non-cancelled row?
            inegi_ok <- unique(dat$inegi[!is_cancel])
            ## Drop cancelled rows only for those municipios that have
            ## a valid alternative row. Cancelled-only municipios are kept
            ## so they are not silently lost.
            drop <- is_cancel & (dat$inegi %in% inegi_ok)
            if (any(drop)) {
                message("xport: Removing ", sum(drop),
                        " duplicate row(s) with status cancelled/appointed ",
                        "(municipality also present with valid status).")
            }
            dat <- dat[!drop, ]
        }
    }
    ##
    ## ================================================================
    ## TIER 3: Diagnostic message when no data found
    ## ================================================================
    if (nrow(dat)==0){
        ## Gather context for a useful error message
        ## 1. What years exist in the dataset for this state?
        dat_full <- read.csv(file_name, stringsAsFactors = FALSE)
        years_this_state <- sort(unique(dat_full$yr[dat_full$edon == e]))
        ## 2. Does this year exist for ANY state?
        year_exists_any <- y %in% unique(dat_full$yr)
        rm(dat_full)
        ##
        ## 3. Cross-reference resumen-haber-datos-ay-ca.csv
        resumen_detail <- NULL  # will hold a list(status, note) if found
        resumen_paths <- c("ancillary/resumen-haber-datos-ay-ca.csv",
                           "../ancillary/resumen-haber-datos-ay-ca.csv",
                           "../../ancillary/resumen-haber-datos-ay-ca.csv")
        for (rp in resumen_paths) {
            if (file.exists(rp)) {
                resumen <- read.csv(rp, stringsAsFactors = FALSE)
                row_edo <- resumen[resumen$edon == e, ]
                if (nrow(row_edo) == 1) {
                    yr_cols <- c("a","b","c","d","e","f","g","h","I")
                    st_cols <- c("a2","b2","c2","d2","e2","f2","g2","h2","l2")
                    for (k in seq_along(yr_cols)) {
                        val <- row_edo[[yr_cols[k]]]
                        if (!is.na(val) && val != "" && as.numeric(val) == y) {
                            resumen_detail <- list(
                                status = trimws(row_edo[[st_cols[k]]]),
                                note   = if ("nota" %in% names(row_edo)) trimws(row_edo[["nota"]]) else ""
                            )
                            break
                        }
                    }
                }
                break  # stop searching paths once file found
            }
        }
        ##
        ## 4. Suggest nearest available years
        nearest_msg <- ""
        if (length(years_this_state) > 0) {
            diffs <- abs(years_this_state - y)
            closest <- years_this_state[order(diffs)][1:min(3, length(years_this_state))]
            nearest_msg <- paste0("\n  Nearest available years: ", paste(closest, collapse = ", "))
        }
        ##
        ## 5. Build the error message
        header <- paste0("No rows found for ", estados[e], " (", edos[e],
                         ", edon=", e, ") in year ", y, ".")
        ##
        ## Determine the reason
        if (length(years_this_state) == 0) {
            ## No data at all for this state
            reason <- paste0(
                "\n  No data exists for ", estados[e], " in any year in the ",
                file_label, " file.",
                "\n  Verify that edon=", e, " is correct.")
        } else if (!is.null(resumen_detail)) {
            ## Year IS a known election year per resumen
            st <- resumen_detail$status
            if (grepl("^ya", st, ignore.case = TRUE)) {
                reason <- paste0(
                    "\n  ", y, " IS a known election year for ", edos[e],
                    " (resumen status: '", st, "').",
                    "\n  Data should be available but was not found.",
                    "\n  >>> This may indicate a problem in the dataset. <<<")
            } else if (grepl("falta", st, ignore.case = TRUE)) {
                reason <- paste0(
                    "\n  ", y, " IS a known election year for ", edos[e],
                    " but data is marked as MISSING (resumen status: '", st, "').",
                    "\n  The data has not yet been incorporated into the dataset.")
            } else if (is.na(st) || st == "" || st == "NA") {
                reason <- paste0(
                    "\n  ", y, " IS a known election year for ", edos[e],
                    " (resumen status: NA/unknown).",
                    "\n  It is unclear whether data should be available.")
            } else {
                reason <- paste0(
                    "\n  ", y, " IS a known election year for ", edos[e],
                    " (resumen status: '", st, "').",
                    "\n  Check resumen-haber-datos for details.")
            }
            ## Append note from resumen if available
            if (!is.null(resumen_detail$note) && resumen_detail$note != "") {
                reason <- paste0(reason, "\n  Note: ", resumen_detail$note)
            }
        } else if (year_exists_any) {
            ## Year exists for other states but not this one
            reason <- paste0(
                "\n  ", y, " is not an election year for ", edos[e],
                " (though other states have data for ", y, ").",
                "\n  Each state has its own electoral calendar.")
        } else {
            ## Year does not exist anywhere in dataset
            reason <- paste0(
                "\n  ", y, " does not appear in the dataset for any state.",
                "\n  It is likely not a municipal election year.")
        }
        ##
        years_msg <- ""
        if (length(years_this_state) > 0) {
            years_msg <- paste0(
                "\n  All years with data for ", edos[e], ": ",
                paste(years_this_state, collapse = ", "))
        }
        ##
        stop(header, reason, years_msg, nearest_msg, call. = FALSE)
    }
    ##
    ## ================================================================
    ## Transform data: pivot vote columns by party label
    ## ================================================================
    ##
    ## extract votes and labels columns as separate objects
    sel.col <- grep("v[0-9]{2}", colnames(dat))
    v <- dat[, sel.col]
    sel.col <- grep("l[0-9]{2}", colnames(dat))
    l <- dat[, sel.col]
    ##
    ## replace - with . in labels (colnames can't have -)
    tmp <- gsub("-", ".", as.vector(t(l)))
    tmp <- data.frame(matrix(tmp, nrow = nrow(l), ncol = ncol(l), byrow = TRUE), stringsAsFactors = FALSE)
    colnames(tmp) <- colnames(l)
    l <- tmp
    ##
    ## generate vl = empty votes columns with party labels as names
    cols <- unique(as.vector(t(l)))
    if (length(which(cols=="0"))>0) cols <- cols[-which(cols=="0")]
    vl <- matrix(0, ncol = length(cols), nrow = nrow(dat))
    vl <- as.data.frame(vl)
    colnames(vl)  <- cols
    ##
    ## loop over vl's columns to fill votes accordingly
    for (c in cols){
        #c <- cols[4] # debug
        # target column indices
        index <- apply(l, 1, FUN = function(x){
            tmp <- which(x==c);
            tmp <- ifelse(length(tmp)==0, 0, tmp)
            return(tmp)
        })
        # rows and col to manipulate in vl
        a <- which(index!=0)        # vl rows to manipulate
        b <- which(colnames(vl)==c) # vl col  to manipulate
        #
        # add index to v as last col
        v <- cbind(v, index=index)
        #
        # manipulate vl accordingly
        vl[,b] <- apply(v, 1, FUN = function(x){ifelse (x["index"]==0, 0, x[x["index"]])})
        #
        v["index"] <- NULL # clean v for next round
        #
        # clean
        rm(index, a, b)
    }
    #
    ## # verify efec
    ## rowSums(vl)==dat$efec
    #
    # break pre- and post votes labels columns to use with manipulated vl
    sel.col <- grep("[vl][0-9]{2}", colnames(dat))
    dat1 <- dat[, 1:(min(sel.col)-1)]         # columns before votes and labels
    dat2 <- dat[, (max(sel.col)+1):ncol(dat)] # columns after  votes and labels
    #
    ##  manipulated data
    export <- cbind(dat1, vl, dat2)
    ##
    message("xport: Done. ", nrow(export), " municipalities, ", ncol(vl), " party columns.")
    ##
    if (write.to.file==FALSE) return(export)
    if (write.to.file==TRUE){
        out_file <- paste0("xport/", edos[e], y, "aymu.csv")
        write.csv(export, file = out_file, row.names = FALSE)
        message("xport: ", y, " municipal vote returns for ", estados[e],
                " exported as ", out_file)
    }
    #
    ## # clean
    ## ls()
}
