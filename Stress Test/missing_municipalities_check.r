########################################################################
## missing_municipalities_check.r
##
## Detecta municipios que DEBERIAN estar en un año electoral pero no
## están, usando únicamente el propio dataset como referencia.
##
## Lógica (sin depender de mun.yrs.csv para evitar falsos positivos
## por años electorales mal registrados, como el caso Yucatán):
##
##  CASO A — Año interior:
##    Si un municipio estuvo en la elección ANTERIOR y en la SIGUIENTE
##    pero NO en la del medio → ausencia sospechosa (gap).
##
##  CASO B — Año más reciente (sin siguiente elección):
##    Si un municipio estuvo en las DOS elecciones previas pero no en
##    la última → ausencia sospechosa (como San Baltazar Chichicapam).
##
##  CASO C — Año más antiguo (sin anterior):
##    Si un municipio estuvo en las DOS elecciones siguientes pero no
##    en la primera → ausencia sospechosa.
##
## Output: missing_municipalities_check.xlsx
########################################################################

if (!requireNamespace("openxlsx", quietly = TRUE))
  install.packages("openxlsx", repos = "https://cloud.r-project.org")
library(openxlsx)

cat("=== MISSING MUNICIPALITIES CHECK ===\n")
cat("Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

BASE_DIR    <- "C:/Users/rodri/OneDrive - INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO/Escritorio/elecRetrns"
DATA_DIR    <- file.path(BASE_DIR, "data")
ANC_DIR     <- file.path(BASE_DIR, "ancillary")
OUTPUT_FILE <- file.path(BASE_DIR, "missing_municipalities_check.xlsx")

# ---- Cargar datos ----
cat("Cargando datos...\n")
dat <- read.csv(file.path(DATA_DIR, "aymu1970-on.coalAgg.csv"), stringsAsFactors = FALSE)
cat("  Filas:", nrow(dat), "\n")
cat("  Estados:", length(unique(dat$edon)), "\n")
cat("  Años:", min(dat$yr), "–", max(dat$yr), "\n\n")

# Cargar new-mun-parents para identificar municipios legítimamente nuevos
new_mun_file <- file.path(ANC_DIR, "new-mun-parents-1989on.csv")
new_mun <- if (file.exists(new_mun_file)) {
  tmp <- read.csv(new_mun_file, stringsAsFactors = FALSE)
  cat("  new-mun-parents cargado:", nrow(tmp), "municipios nuevos\n\n")
  tmp
} else {
  cat("  AVISO: new-mun-parents-1989on.csv no encontrado — no se filtrará por municipios nuevos\n\n")
  NULL
}

# Estado metadata
estados_nom <- c(
  "Aguascalientes","Baja California","Baja California Sur","Campeche",
  "Coahuila","Colima","Chiapas","Chihuahua","Distrito Federal/CDMX","Durango",
  "Guanajuato","Guerrero","Hidalgo","Jalisco","México","Michoacán","Morelos",
  "Nayarit","Nuevo León","Oaxaca","Puebla","Querétaro","Quintana Roo",
  "San Luis Potosí","Sinaloa","Sonora","Tabasco","Tamaulipas","Tlaxcala",
  "Veracruz","Yucatán","Zacatecas")

# INEGI de municipios legítimamente nuevos (creados en algún año, no falta de datos)
new_inegi_set <- if (!is.null(new_mun) && "inegi" %in% colnames(new_mun))
  as.character(new_mun$inegi) else character(0)

################################################################################
## DETECCIÓN DE AUSENCIAS SOSPECHOSAS                                         ##
################################################################################
cat("Analizando ausencias por estado...\n\n")

results_list <- list()

for (e in 1:32) {

  # Años electorales de este estado en el dataset, ordenados
  state_yrs <- sort(unique(dat$yr[dat$edon == e]))
  n_yrs     <- length(state_yrs)

  if (n_yrs < 2) next  # necesitamos al menos 2 elecciones para comparar

  # Para cada año electoral, obtener el set de INEGIs
  munis_by_yr <- lapply(state_yrs, function(y) {
    as.character(dat$inegi[dat$edon == e & dat$yr == y])
  })
  names(munis_by_yr) <- as.character(state_yrs)

  for (i in seq_along(state_yrs)) {
    yr_curr <- state_yrs[i]
    munis_curr <- munis_by_yr[[i]]

    # ---- Determinar conjunto de referencia según posición ----
    if (i == 1 && n_yrs >= 3) {
      # CASO C: primer año — referencia = años i+1 e i+2
      ref <- intersect(munis_by_yr[[i+1]], munis_by_yr[[i+2]])
      caso <- "C"
      ref_yrs <- paste(state_yrs[i+1], state_yrs[i+2], sep=" & ")

    } else if (i == 1 && n_yrs == 2) {
      # Solo 2 años: comparar directamente con el siguiente
      ref <- munis_by_yr[[i+1]]
      caso <- "C"
      ref_yrs <- as.character(state_yrs[i+1])

    } else if (i == n_yrs && n_yrs >= 3) {
      # CASO B: último año — referencia = años i-1 e i-2
      ref <- intersect(munis_by_yr[[i-1]], munis_by_yr[[i-2]])
      caso <- "B"
      ref_yrs <- paste(state_yrs[i-2], state_yrs[i-1], sep=" & ")

    } else if (i == n_yrs && n_yrs == 2) {
      # Solo 2 años: comparar con el anterior
      ref <- munis_by_yr[[i-1]]
      caso <- "B"
      ref_yrs <- as.character(state_yrs[i-1])

    } else {
      # CASO A: año interior — referencia = anterior Y siguiente
      ref <- intersect(munis_by_yr[[i-1]], munis_by_yr[[i+1]])
      caso <- "A"
      ref_yrs <- paste(state_yrs[i-1], state_yrs[i+1], sep=" & ")
    }

    # Municipios en referencia pero ausentes en año actual
    absent <- setdiff(ref, munis_curr)
    if (!length(absent)) next

    for (inegi_code in absent) {

      # Obtener nombre del municipio desde años adyacentes
      mun_name <- NA_character_
      for (adj_i in c(i-1, i+1, i-2, i+2)) {
        if (adj_i < 1 || adj_i > n_yrs) next
        tmp_name <- dat$mun[dat$edon == e & dat$yr == state_yrs[adj_i] &
                            dat$inegi == inegi_code]
        if (length(tmp_name) > 0 && !is.na(tmp_name[1]) && tmp_name[1] != "") {
          mun_name <- tmp_name[1]
          break
        }
      }

      # Clasificar si es municipio nuevo legítimo
      is_new_mun <- inegi_code %in% new_inegi_set
      nota <- if (is_new_mun) "Posible municipio nuevo — verificar" else
              if (caso == "B") "Ausente en elección más reciente — PRIORITARIO" else
              if (caso == "C") "Ausente en elección más antigua — verificar" else
              "Ausente entre dos elecciones con datos — GAP"

      results_list[[length(results_list)+1]] <- data.frame(
        edon         = e,
        estado       = estados_nom[e],
        yr_ausente   = yr_curr,
        inegi        = inegi_code,
        municipio    = ifelse(is.na(mun_name), "[nombre no encontrado]", mun_name),
        caso         = caso,
        yrs_referencia = ref_yrs,
        n_muns_estado_ese_yr = length(munis_curr),
        n_muns_referencia    = length(ref),
        posible_mun_nuevo    = is_new_mun,
        nota         = nota,
        stringsAsFactors = FALSE
      )
    }
  }
}

results_df <- if (length(results_list)) {
  tmp <- do.call(rbind, results_list)
  tmp[order(tmp$caso, tmp$edon, tmp$yr_ausente, tmp$municipio), ]
} else {
  data.frame(stringsAsFactors = FALSE)
}

cat("=== RESULTADOS ===\n")
cat("Ausencias sospechosas encontradas:", nrow(results_df), "\n")
if (nrow(results_df)) {
  cat("\nPor tipo de caso:\n")
  print(table(results_df$caso))
  cat("\nPor estado (top 10):\n")
  print(head(sort(table(results_df$estado), decreasing=TRUE), 10))
  cat("\nPrioritarios (Caso B — ausentes en elección más reciente):\n")
  caso_b <- results_df[results_df$caso == "B", c("estado","yr_ausente","inegi","municipio")]
  print(caso_b)
}

################################################################################
## GENERAR EXCEL                                                               ##
################################################################################
cat("\nGenerando Excel:", OUTPUT_FILE, "\n")

wb <- createWorkbook()

st_title  <- createStyle(fontSize=13, fontColour="#FFFFFF", fgFill="#2E4057",
                         textDecoration="bold", halign="left")
st_head   <- createStyle(fontSize=10, fontColour="#FFFFFF", fgFill="#1B6CA8",
                         textDecoration="bold", halign="center", wrapText=TRUE,
                         border="TopBottomLeftRight", borderColour="#AAAAAA")
st_ok     <- createStyle(fgFill="#C8E6C9", fontColour="#1B5E20")
st_warn   <- createStyle(fgFill="#FFF9C4", fontColour="#7B4F00")
st_err    <- createStyle(fgFill="#FFCDD2", fontColour="#B71C1C")
st_orange <- createStyle(fgFill="#FFE0B2", fontColour="#BF360C")
st_info   <- createStyle(fgFill="#E3F2FD", fontColour="#0D47A1")

write_title <- function(wb, sheet, title, ncols) {
  writeData(wb, sheet, title, startRow=1, startCol=1)
  if (ncols > 1) mergeCells(wb, sheet, cols=1:ncols, rows=1)
  addStyle(wb, sheet, st_title, rows=1, cols=1:ncols, gridExpand=TRUE)
}

# ---- TAB 1: Resumen ----
addWorksheet(wb, "Resumen")
write_title(wb, "Resumen", "DETECCIÓN DE MUNICIPIOS FALTANTES — Basado en continuidad entre elecciones", 3)

caso_b_n <- if (nrow(results_df)) sum(results_df$caso=="B", na.rm=TRUE) else 0
caso_a_n <- if (nrow(results_df)) sum(results_df$caso=="A", na.rm=TRUE) else 0
caso_c_n <- if (nrow(results_df)) sum(results_df$caso=="C", na.rm=TRUE) else 0
new_mun_n<- if (nrow(results_df)) sum(results_df$posible_mun_nuevo, na.rm=TRUE) else 0
estados_afectados <- if (nrow(results_df)) length(unique(results_df$edon)) else 0

smry <- data.frame(
  Metrica = c(
    "Fecha de análisis",
    "Archivo fuente",
    "Total de ausencias sospechosas",
    "Caso A — GAP interior (ausente entre dos elecciones con datos)",
    "Caso B — Ausente en elección MÁS RECIENTE (prioritario)",
    "Caso C — Ausente en elección más antigua",
    "Casos que podrían ser municipios nuevos legítimos",
    "Estados afectados",
    "Metodología"
  ),
  Valor = c(
    format(Sys.time(), "%Y-%m-%d %H:%M"),
    "aymu1970-on.coalAgg.csv",
    nrow(results_df),
    caso_a_n,
    caso_b_n,
    caso_c_n,
    new_mun_n,
    estados_afectados,
    "Referencia = dataset propio (NO mun.yrs.csv) → sin falsos positivos por año electoral incorrecto"
  ),
  stringsAsFactors = FALSE
)

writeData(wb, "Resumen", smry, startRow=3, headerStyle=st_head)
setColWidths(wb, "Resumen", cols=1:2, widths=c(62, 70))

# Colorear por severidad
styles_smry <- c("info", "info",
                 ifelse(nrow(results_df)==0, "ok", "err"),
                 ifelse(caso_a_n==0, "ok", "warn"),
                 ifelse(caso_b_n==0, "ok", "err"),
                 ifelse(caso_c_n==0, "ok", "warn"),
                 "info", "info", "info")
for (ri in seq_len(nrow(smry))) {
  sty <- switch(styles_smry[ri],
    ok   = st_ok, err  = st_err,
    warn = st_warn, info = st_info, st_info)
  addStyle(wb, "Resumen", sty, rows=ri+3, cols=1:2, gridExpand=TRUE)
}

# ---- TAB 2: Caso B — Prioritarios (ausentes en elección más reciente) ----
addWorksheet(wb, "Caso_B_Recientes")
write_title(wb, "Caso_B_Recientes",
  "CASO B — Municipios ausentes en la elección MÁS RECIENTE de su estado (PRIORITARIO)", 11)

writeData(wb, "Caso_B_Recientes",
  "Un municipio que estuvo en las 2 elecciones previas pero NO aparece en la última elección del estado.",
  startRow=2, startCol=1)
addStyle(wb, "Caso_B_Recientes", st_info, rows=2, cols=1:11, gridExpand=TRUE)

caso_b_df <- if (nrow(results_df)) results_df[results_df$caso=="B", ] else
  data.frame(stringsAsFactors=FALSE)

if (nrow(caso_b_df)) {
  writeData(wb, "Caso_B_Recientes", caso_b_df, startRow=4, headerStyle=st_head)
  setColWidths(wb, "Caso_B_Recientes", cols=1:11,
               widths=c(7,25,12,10,35,7,18,15,15,15,45))
  for (ri in seq_len(nrow(caso_b_df))) {
    sty <- if (caso_b_df$posible_mun_nuevo[ri]) st_warn else st_err
    addStyle(wb, "Caso_B_Recientes", sty, rows=ri+4, cols=1:11, gridExpand=TRUE)
  }
} else {
  writeData(wb, "Caso_B_Recientes",
            data.frame(resultado="✓ Ningún municipio ausente en elección más reciente"),
            startRow=4, headerStyle=st_head)
  addStyle(wb, "Caso_B_Recientes", st_ok, rows=5, cols=1)
}

# ---- TAB 3: Caso A — GAPs interiores ----
addWorksheet(wb, "Caso_A_Gaps")
write_title(wb, "Caso_A_Gaps",
  "CASO A — GAPs interiores: municipio ausente entre dos elecciones donde sí aparece", 11)

writeData(wb, "Caso_A_Gaps",
  "Municipio presente en elección Y-1 Y en elección Y+1, pero ausente en Y. Posible dato faltante o municipio temporalmente fusionado.",
  startRow=2, startCol=1)
addStyle(wb, "Caso_A_Gaps", st_info, rows=2, cols=1:11, gridExpand=TRUE)

caso_a_df <- if (nrow(results_df)) results_df[results_df$caso=="A", ] else
  data.frame(stringsAsFactors=FALSE)

if (nrow(caso_a_df)) {
  writeData(wb, "Caso_A_Gaps", caso_a_df, startRow=4, headerStyle=st_head)
  setColWidths(wb, "Caso_A_Gaps", cols=1:11,
               widths=c(7,25,12,10,35,7,18,15,15,15,45))
  for (ri in seq_len(nrow(caso_a_df))) {
    sty <- if (caso_a_df$posible_mun_nuevo[ri]) st_warn else st_orange
    addStyle(wb, "Caso_A_Gaps", sty, rows=ri+4, cols=1:11, gridExpand=TRUE)
  }
} else {
  writeData(wb, "Caso_A_Gaps",
            data.frame(resultado="✓ Ningún GAP interior detectado"),
            startRow=4, headerStyle=st_head)
  addStyle(wb, "Caso_A_Gaps", st_ok, rows=5, cols=1)
}

# ---- TAB 4: Caso C — Ausentes en primera elección ----
addWorksheet(wb, "Caso_C_Primeros")
write_title(wb, "Caso_C_Primeros",
  "CASO C — Municipios ausentes en la elección más ANTIGUA de su estado", 11)

writeData(wb, "Caso_C_Primeros",
  "Municipio presente en las 2 elecciones más tempranas con datos, pero no en la primera. Probablemente municipio recién creado.",
  startRow=2, startCol=1)
addStyle(wb, "Caso_C_Primeros", st_info, rows=2, cols=1:11, gridExpand=TRUE)

caso_c_df <- if (nrow(results_df)) results_df[results_df$caso=="C", ] else
  data.frame(stringsAsFactors=FALSE)

if (nrow(caso_c_df)) {
  writeData(wb, "Caso_C_Primeros", caso_c_df, startRow=4, headerStyle=st_head)
  setColWidths(wb, "Caso_C_Primeros", cols=1:11,
               widths=c(7,25,12,10,35,7,18,15,15,15,45))
  for (ri in seq_len(nrow(caso_c_df))) {
    sty <- if (caso_c_df$posible_mun_nuevo[ri]) st_ok else st_warn
    addStyle(wb, "Caso_C_Primeros", sty, rows=ri+4, cols=1:11, gridExpand=TRUE)
  }
} else {
  writeData(wb, "Caso_C_Primeros",
            data.frame(resultado="✓ Ningún caso detectado"),
            startRow=4, headerStyle=st_head)
  addStyle(wb, "Caso_C_Primeros", st_ok, rows=5, cols=1)
}

# ---- TAB 5: Todos los casos (completo) ----
addWorksheet(wb, "Todos_los_casos")
write_title(wb, "Todos_los_casos", "TODOS LOS CASOS — Tabla completa ordenada por caso y estado", 11)

if (nrow(results_df)) {
  writeData(wb, "Todos_los_casos", results_df, startRow=3, headerStyle=st_head)
  setColWidths(wb, "Todos_los_casos", cols=1:11,
               widths=c(7,25,12,10,35,7,18,15,15,15,45))
  for (ri in seq_len(nrow(results_df))) {
    sty <- switch(results_df$caso[ri],
      B = if (results_df$posible_mun_nuevo[ri]) st_warn else st_err,
      A = if (results_df$posible_mun_nuevo[ri]) st_warn else st_orange,
      C = if (results_df$posible_mun_nuevo[ri]) st_ok   else st_warn,
      st_info)
    addStyle(wb, "Todos_los_casos", sty, rows=ri+3, cols=1:11, gridExpand=TRUE)
  }
} else {
  writeData(wb, "Todos_los_casos",
            data.frame(resultado="✓ No se detectaron ausencias sospechosas"),
            startRow=3, headerStyle=st_head)
  addStyle(wb, "Todos_los_casos", st_ok, rows=4, cols=1)
}

saveWorkbook(wb, OUTPUT_FILE, overwrite=TRUE)

cat("\n=== DONE ===\n")
cat("Guardado:", OUTPUT_FILE, "\n")
cat("Finalizado:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
