########################################################################
## coalAgg_vs_coalSplit_fulltest.r
##
## Compares aymu1970-on.coalAgg.csv vs aymu1970-on.coalSplit.csv
## for EVERY state-year-municipality combination.
##
## Solo genera el Excel si encuentra discrepancias reales.
##
## Columnas invariantes (deben ser idénticas en ambos archivos):
##   efec, tot, nulos, nr, lisnom, win, mg, ncand, date
##
## Columnas que PUEDEN diferir legítimamente (no se comparan):
##   v01..v23  — distribución de votos entre partidos/coaliciones
##   l01..l23  — etiquetas de partido (coalición vs partidos individuales)
##   ncoal     — número de coaliciones (puede diferir)
##   notas     — notas editoriales
##
## Output: coalAgg_vs_coalSplit_discrepancias.xlsx
##         (solo se crea si hay discrepancias)
########################################################################

if (!requireNamespace("openxlsx", quietly = TRUE))
  install.packages("openxlsx", repos = "https://cloud.r-project.org")
library(openxlsx)

cat("=== coalAgg vs coalSplit Full Comparison ===\n")
cat("Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

BASE_DIR    <- "C:/Users/rodri/OneDrive - INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO/Escritorio/elecRetrns"
DATA_DIR    <- file.path(BASE_DIR, "data")
OUTPUT_FILE <- file.path(BASE_DIR, "coalAgg_vs_coalSplit_discrepancias.xlsx")

# ---- Cargar ambos archivos ----
cat("Cargando archivos...\n")
agg   <- read.csv(file.path(DATA_DIR, "aymu1970-on.coalAgg.csv"),   stringsAsFactors = FALSE)
split <- read.csv(file.path(DATA_DIR, "aymu1970-on.coalSplit.csv"), stringsAsFactors = FALSE)
cat("  coalAgg  :", nrow(agg),   "filas,", ncol(agg),   "columnas\n")
cat("  coalSplit:", nrow(split), "filas,", ncol(split), "columnas\n\n")

# Columnas que deben ser idénticas en ambos archivos
INVARIANT_COLS <- c("efec", "tot", "nulos", "nr", "lisnom", "win", "mg", "ncand", "date")

# Solo comparar las que realmente existen en ambos
invariant_available <- intersect(
  intersect(INVARIANT_COLS, colnames(agg)),
  INVARIANT_COLS
)
cat("Columnas invariantes a comparar:", paste(invariant_available, collapse = ", "), "\n\n")

# ---- Obtener todos los state-year disponibles ----
state_years_agg   <- unique(agg[,   c("edon", "yr")])
state_years_split <- unique(split[, c("edon", "yr")])

# State-years presentes en uno pero no en el otro
only_in_agg   <- merge(state_years_agg,   state_years_split, all.x = TRUE)
only_in_split <- merge(state_years_split, state_years_agg,   all.x = TRUE)

cat("State-years en coalAgg  :", nrow(state_years_agg), "\n")
cat("State-years en coalSplit:", nrow(state_years_split), "\n")

all_state_years <- merge(state_years_agg, state_years_split)  # intersección
cat("State-years en comun    :", nrow(all_state_years), "\n\n")

# ---- Listas para acumular discrepancias ----
disc_nrow    <- list()   # distinto número de municipios
disc_missing <- list()   # municipio presente en uno pero no en otro
disc_values  <- list()   # valores invariantes distintos

cat("Comparando", nrow(all_state_years), "state-year combinations...\n")
total <- nrow(all_state_years)

for (i in seq_len(total)) {
  if (i %% 200 == 0) cat("  Progreso:", i, "/", total, "\n")

  e <- all_state_years$edon[i]
  y <- all_state_years$yr[i]

  sb_agg   <- agg  [agg$edon   == e & agg$yr   == y, ]
  sb_split <- split[split$edon == e & split$yr == y, ]

  # ---- CHECK 1: Mismo número de municipios ----
  if (nrow(sb_agg) != nrow(sb_split)) {
    disc_nrow[[length(disc_nrow) + 1]] <- data.frame(
      edon       = e,
      yr         = y,
      n_mun_agg  = nrow(sb_agg),
      n_mun_split= nrow(sb_split),
      diferencia = nrow(sb_agg) - nrow(sb_split),
      stringsAsFactors = FALSE
    )
  }

  # ---- CHECK 2: Municipios presentes en uno pero no en el otro ----
  inegi_agg   <- sb_agg$inegi
  inegi_split <- sb_split$inegi

  solo_en_agg   <- setdiff(inegi_agg,   inegi_split)
  solo_en_split <- setdiff(inegi_split, inegi_agg)

  for (inegi_code in solo_en_agg) {
    mun_name <- sb_agg$mun[sb_agg$inegi == inegi_code][1]
    disc_missing[[length(disc_missing) + 1]] <- data.frame(
      edon       = e,
      yr         = y,
      inegi      = inegi_code,
      mun        = mun_name,
      presente_en = "solo en coalAgg",
      stringsAsFactors = FALSE
    )
  }
  for (inegi_code in solo_en_split) {
    mun_name <- sb_split$mun[sb_split$inegi == inegi_code][1]
    disc_missing[[length(disc_missing) + 1]] <- data.frame(
      edon       = e,
      yr         = y,
      inegi      = inegi_code,
      mun        = mun_name,
      presente_en = "solo en coalSplit",
      stringsAsFactors = FALSE
    )
  }

  # ---- CHECK 3: Valores invariantes por municipio ----
  common_inegi <- intersect(inegi_agg, inegi_split)
  if (!length(common_inegi)) next

  # Subsets alineados por inegi
  row_agg   <- sb_agg  [match(common_inegi, sb_agg$inegi),   ]
  row_split <- sb_split[match(common_inegi, sb_split$inegi), ]

  for (col in invariant_available) {
    if (!col %in% colnames(sb_agg) || !col %in% colnames(sb_split)) next

    val_agg   <- row_agg[[col]]
    val_split <- row_split[[col]]

    # Comparación tolerante a NA
    differs <- mapply(function(a, b) {
      if (is.na(a) && is.na(b)) return(FALSE)
      if (is.na(a) || is.na(b)) return(TRUE)
      if (is.numeric(a)) return(abs(a - b) > 0.5)  # tolerancia para redondeo
      return(as.character(a) != as.character(b))
    }, val_agg, val_split)

    bad_idx <- which(differs)
    for (j in bad_idx) {
      disc_values[[length(disc_values) + 1]] <- data.frame(
        edon      = e,
        yr        = y,
        inegi     = common_inegi[j],
        mun       = row_agg$mun[j],
        columna   = col,
        valor_agg  = as.character(val_agg[j]),
        valor_split= as.character(val_split[j]),
        diferencia = if (is.numeric(val_agg[j]))
                       as.character(round(val_agg[j] - val_split[j], 2))
                     else "≠",
        stringsAsFactors = FALSE
      )
    }
  }
}

# ---- Consolidar resultados ----
df_nrow    <- if (length(disc_nrow))    do.call(rbind, disc_nrow)    else NULL
df_missing <- if (length(disc_missing)) do.call(rbind, disc_missing) else NULL
df_values  <- if (length(disc_values))  do.call(rbind, disc_values)  else NULL

total_disc <- sum(
  ifelse(is.null(df_nrow),    0, nrow(df_nrow)),
  ifelse(is.null(df_missing), 0, nrow(df_missing)),
  ifelse(is.null(df_values),  0, nrow(df_values))
)

cat("\n=== RESULTADOS ===\n")
cat("Discrepancias en nrow (municipios)    :", ifelse(is.null(df_nrow),    0, nrow(df_nrow)),    "\n")
cat("Municipios presentes en solo un archivo:", ifelse(is.null(df_missing), 0, nrow(df_missing)), "\n")
cat("Discrepancias en valores invariantes  :", ifelse(is.null(df_values),  0, nrow(df_values)),  "\n")
cat("TOTAL DE DISCREPANCIAS                :", total_disc, "\n\n")

# ---- Solo genera el Excel si hay discrepancias ----
if (total_disc == 0) {
  cat("✓ Ninguna discrepancia encontrada.\n")
  cat("  coalAgg y coalSplit son consistentes en todas las columnas invariantes.\n")
  cat("  No se genera archivo Excel.\n")
} else {
  cat("Generando Excel:", OUTPUT_FILE, "\n")

  wb <- createWorkbook()

  st_title <- createStyle(fontSize = 13, fontColour = "#FFFFFF", fgFill = "#2E4057",
                          textDecoration = "bold", halign = "left")
  st_head  <- createStyle(fontSize = 10, fontColour = "#FFFFFF", fgFill = "#1B6CA8",
                          textDecoration = "bold", halign = "center", wrapText = TRUE,
                          border = "TopBottomLeftRight", borderColour = "#AAAAAA")
  st_ok    <- createStyle(fgFill = "#C8E6C9", fontColour = "#1B5E20")
  st_warn  <- createStyle(fgFill = "#FFF9C4", fontColour = "#7B4F00")
  st_err   <- createStyle(fgFill = "#FFCDD2", fontColour = "#B71C1C")
  st_info  <- createStyle(fgFill = "#E3F2FD", fontColour = "#0D47A1")

  write_title <- function(wb, sheet, title, ncols) {
    writeData(wb, sheet, title, startRow = 1, startCol = 1)
    if (ncols > 1) mergeCells(wb, sheet, cols = 1:ncols, rows = 1)
    addStyle(wb, sheet, st_title, rows = 1, cols = 1:ncols, gridExpand = TRUE)
  }

  # ---- TAB 1: Resumen ----
  addWorksheet(wb, "Resumen")
  write_title(wb, "Resumen", "RESUMEN — Discrepancias coalAgg vs coalSplit", 3)

  smry <- data.frame(
    Categoria = c(
      "General", "General", "General", "General",
      "Check 1 — Conteo de municipios",
      "Check 2 — Municipios en solo un archivo",
      "Check 3 — Valores invariantes",
      "Check 3 — Columna con más discrepancias",
      "Check 3 — State-years afectados"
    ),
    Metrica = c(
      "Fecha de análisis",
      "State-years comparados",
      "Columnas invariantes verificadas",
      "Total de discrepancias encontradas",
      "State-years con distinto número de municipios",
      "Municipios presentes en solo un archivo",
      "Filas con valor distinto en columnas invariantes",
      "Columna más conflictiva",
      "State-years con al menos una discrepancia de valor"
    ),
    Valor = c(
      format(Sys.time(), "%Y-%m-%d %H:%M"),
      nrow(all_state_years),
      paste(invariant_available, collapse = ", "),
      total_disc,
      ifelse(is.null(df_nrow),    0, nrow(df_nrow)),
      ifelse(is.null(df_missing), 0, nrow(df_missing)),
      ifelse(is.null(df_values),  0, nrow(df_values)),
      if (!is.null(df_values) && nrow(df_values) > 0)
        names(sort(table(df_values$columna), decreasing = TRUE))[1]
      else "ninguna",
      if (!is.null(df_values) && nrow(df_values) > 0)
        length(unique(paste(df_values$edon, df_values$yr)))
      else 0
    ),
    stringsAsFactors = FALSE
  )

  writeData(wb, "Resumen", smry, startRow = 3, headerStyle = st_head)
  setColWidths(wb, "Resumen", cols = 1:3, widths = c(35, 55, 40))
  # Assign styles row-by-row; avoid as.numeric() on non-numeric Valor entries
  row_styles <- list(
    st_info,  # 1: Fecha de análisis
    st_info,  # 2: State-years comparados
    st_info,  # 3: Columnas invariantes verificadas
    if (total_disc == 0) st_ok else st_err,                              # 4: Total
    if (ifelse(is.null(df_nrow),    0, nrow(df_nrow))    == 0) st_ok else st_err,  # 5
    if (ifelse(is.null(df_missing), 0, nrow(df_missing)) == 0) st_ok else st_err,  # 6
    if (ifelse(is.null(df_values),  0, nrow(df_values))  == 0) st_ok else st_err,  # 7
    if (is.null(df_values) || nrow(df_values) == 0) st_ok else st_warn, # 8: columna conflictiva
    if (ifelse(is.null(df_values),  0, nrow(df_values))  == 0) st_ok else st_err   # 9
  )
  for (ri in seq_len(nrow(smry)))
    addStyle(wb, "Resumen", row_styles[[ri]], rows = ri + 3, cols = 1:3, gridExpand = TRUE)

  # ---- TAB 2: Distinto_nrow ----
  if (!is.null(df_nrow)) {
    addWorksheet(wb, "Distinto_nrow")
    write_title(wb, "Distinto_nrow",
      "CHECK 1: State-years donde coalAgg y coalSplit tienen distinto número de municipios", 5)
    df_nrow_sorted <- df_nrow[order(df_nrow$edon, df_nrow$yr), ]
    writeData(wb, "Distinto_nrow", df_nrow_sorted, startRow = 3, headerStyle = st_head)
    setColWidths(wb, "Distinto_nrow", cols = 1:5, widths = c(8, 8, 12, 12, 12))
    for (ri in seq_len(nrow(df_nrow_sorted)))
      addStyle(wb, "Distinto_nrow", st_err, rows = ri + 3, cols = 1:5, gridExpand = TRUE)
  }

  # ---- TAB 3: Municipios_Asimetricos ----
  if (!is.null(df_missing)) {
    addWorksheet(wb, "Municipios_Asimetricos")
    write_title(wb, "Municipios_Asimetricos",
      "CHECK 2: Municipios presentes en un archivo pero no en el otro", 5)
    df_miss_sorted <- df_missing[order(df_missing$edon, df_missing$yr, df_missing$inegi), ]
    writeData(wb, "Municipios_Asimetricos", df_miss_sorted, startRow = 3, headerStyle = st_head)
    setColWidths(wb, "Municipios_Asimetricos", cols = 1:5, widths = c(8, 8, 12, 35, 22))
    for (ri in seq_len(nrow(df_miss_sorted)))
      addStyle(wb, "Municipios_Asimetricos", st_warn, rows = ri + 3, cols = 1:5, gridExpand = TRUE)
  }

  # ---- TAB 4: Valores_Distintos ----
  if (!is.null(df_values)) {
    addWorksheet(wb, "Valores_Distintos")
    write_title(wb, "Valores_Distintos",
      "CHECK 3: Municipios con valores distintos en columnas que deben ser idénticas (efec, tot, nulos, etc.)", 8)
    df_val_sorted <- df_values[order(df_values$columna, df_values$edon, df_values$yr), ]
    writeData(wb, "Valores_Distintos", df_val_sorted, startRow = 3, headerStyle = st_head)
    setColWidths(wb, "Valores_Distintos", cols = 1:8, widths = c(8, 8, 12, 35, 12, 15, 15, 12))
    for (ri in seq_len(nrow(df_val_sorted)))
      addStyle(wb, "Valores_Distintos", st_err, rows = ri + 3, cols = 1:8, gridExpand = TRUE)
  }

  saveWorkbook(wb, OUTPUT_FILE, overwrite = TRUE)
  cat("Guardado en:", OUTPUT_FILE, "\n")
}

cat("\nFinalizado:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
