# Script per generar la plantilla Excel per a la pestanya "Alumnat antic"
# Executa des del directori del TFG:
#   source("shiny_absentisme/crear_plantilla_alumnat_antic.R")
# Requereix: install.packages("openxlsx")

library(openxlsx)

# ── Valors vàlids variables categòriques ─────────────────────
t_aval_vals  <- c("Continuada", "Única")
curs_vals    <- c("1r", "2n", "3r", "4t", "5è", "6è")
genere_vals  <- c("Home", "Dona", "Altre")
grau_doble   <- c("Estadística", "Doble Eco+Est", "Doble ADE+Soc",
                  "Doble ADE+Mat", "Doble ADE+Dret", "Doble ADE+Qui")
grau_simples <- c("ADE", "ADE (EP)", "Economia", "Economia (EP)", "GE")
grau_vals    <- c(grau_simples, grau_doble)
dedic_vals   <- c("Estudiant a TC", "Treballa ocasionalment", "T.Parcial", "T.Complet")

# ── Ítems EFA ────────────────────────────────────────────────
items_mot <- c("M_PASSIU","M_TEOR","M_AVORR","M_PROF","M_AMICS",   # MOT_DESMOTIVACIO
               "M_AUTON","M_CV","M_UTIL","M_EXAM","M_REPET",        # MOT_AUTOGESTIO
               "M_FAM","M_SALUT","M_TREB")                          # MOT_FORCA_MAJOR
items_est <- c("E_EXPL","E_RITME","E_CLIMA","E_PROP",               # EST_QUALITAT_DOC
               "E_ACT_AC","E_PES_AC","E_PART","E_DINAM",            # EST_AVALUACIO_AC
               "E_DESC","E_CURT",                                    # EST_TEMPS_CLASSE
               "E_REDU")                                             # EST_GRUPS_REDUITS

# ── Dimensions ───────────────────────────────────────────────
N_ROWS     <- 50
ROW_TITLE  <- 1
ROW_NOTE   <- 2
ROW_LEGEND <- 3
ROW_SECCAP <- 4
ROW_COLCAP <- 5
ROW_DATA   <- 6

# Secció input: id + pre-curs (10) + IA (7) + M (13) + E (11)
inp_precurs <- c("EDAT","DESPL","N_ASSIG","NOTA","T_AVAL","CURS","GENERE","GRAU","DEDIC")
inp_ia      <- c("IA_HABIT","IA_COMPR","IA_REND","IA_PDFS","IA_SUBST","IA_ATENC","IA_CONF")
inp_noms    <- c("id", inp_precurs, inp_ia, items_mot, items_est)
N_INP       <- length(inp_noms)   # 1 + 9 + 7 + 13 + 11 = 41

# Secció model
comp_precurs <- c("EDAT","DESPL","N_ASSIG","NOTA_num","T_AVAL_num","CURS_1R",
                  "GENERE_Home","DOBLE_GRAU_EST","TREB_INTENS")
comp_noms    <- c(comp_precurs, inp_ia, items_mot, items_est)   # 9+7+13+11 = 40
N_COMP       <- length(comp_noms)

COL_SEP  <- N_INP + 1
COL_COMP <- N_INP + 2

# ── Fórmules ─────────────────────────────────────────────────
col_letter <- function(n) {
  if (n <= 26) LETTERS[n]
  else paste0(LETTERS[(n - 1) %/% 26], LETTERS[(n - 1) %% 26 + 1])
}
# Columnes input per nom
inp_col <- setNames(seq_along(inp_noms), inp_noms)

make_formulas <- function(r) {
  cl <- function(nm) paste0(col_letter(inp_col[nm]), r)

  grau_or <- paste(paste0(cl("GRAU"), '="', grau_doble, '"'), collapse = ",")

  # Pre-curs (9 variables model)
  fm_precurs <- list(
    EDAT           = paste0("=IF(", cl("EDAT"),  '="","",', cl("EDAT"),  ")"),
    DESPL          = paste0("=IF(", cl("DESPL"), '="","",', cl("DESPL"), ")"),
    N_ASSIG        = paste0("=IF(", cl("N_ASSIG"), '="","",', cl("N_ASSIG"), ")"),
    NOTA_num       = paste0("=IF(", cl("NOTA"),  '="","",', cl("NOTA"),  ")"),
    T_AVAL_num     = paste0("=IF(", cl("T_AVAL"), '="","",IF(', cl("T_AVAL"), '="Continuada",1,0))'),
    CURS_1R        = paste0("=IF(", cl("CURS"),   '="","",IF(', cl("CURS"),  '="1r",1,0))'),
    GENERE_Home    = paste0("=IF(", cl("GENERE"), '="","",IF(', cl("GENERE"), '="Home",1,0))'),
    DOBLE_GRAU_EST = paste0("=IF(", cl("GRAU"),   '="","",IF(OR(', grau_or, '),1,0))'),
    TREB_INTENS    = paste0("=IF(", cl("DEDIC"),  '="","",IF(OR(', cl("DEDIC"),
                               '="T.Parcial",', cl("DEDIC"), '="T.Complet"),1,0))')
  )

  # IA (7): copia directa
  fm_ia <- lapply(inp_ia, function(v)
    paste0("=IF(", cl(v), '="","",', cl(v), ")"))
  names(fm_ia) <- inp_ia

  # M_* (13): copia directa
  fm_mot <- lapply(items_mot, function(v)
    paste0("=IF(", cl(v), '="","",', cl(v), ")"))
  names(fm_mot) <- items_mot

  # E_* (11): copia directa
  fm_est <- lapply(items_est, function(v)
    paste0("=IF(", cl(v), '="","",', cl(v), ")"))
  names(fm_est) <- items_est

  c(fm_precurs, fm_ia, fm_mot, fm_est)
}

# ── Estils ───────────────────────────────────────────────────
brd <- list(border = "TopBottomLeftRight", borderStyle = "thin")

st_titol <- createStyle(
  fontSize = 13, fontColour = "white", fgFill = "#2C3E50",
  fontName = "Calibri", textDecoration = "bold",
  halign = "center", valign = "center"
)
st_nota_inst <- createStyle(
  fontSize = 9, fontColour = "#555555", fontName = "Calibri",
  halign = "left", valign = "center", textDecoration = "italic"
)
st_seccap_inp <- createStyle(
  fontSize = 10, fontColour = "white", fgFill = "#1A6E9E",
  fontName = "Calibri", textDecoration = "bold",
  halign = "center", valign = "center",
  border = "TopBottomLeftRight", borderStyle = "thin"
)
st_seccap_mod <- createStyle(
  fontSize = 10, fontColour = "#2C3E50", fgFill = "#A8D1E7",
  fontName = "Calibri", textDecoration = "bold",
  halign = "center", valign = "center",
  border = "TopBottomLeftRight", borderStyle = "thin"
)
st_cap_precurs <- createStyle(
  fontSize = 9, fontColour = "white", fgFill = "#4A90B8",
  fontName = "Calibri", textDecoration = "bold",
  halign = "center", valign = "center",
  wrapText = TRUE, border = "TopBottomLeftRight", borderStyle = "thin"
)
st_cap_ia <- createStyle(
  fontSize = 9, fontColour = "white", fgFill = "#2E86AB",
  fontName = "Calibri", textDecoration = "bold",
  halign = "center", valign = "center",
  wrapText = TRUE, border = "TopBottomLeftRight", borderStyle = "thin"
)
st_cap_mot <- createStyle(
  fontSize = 9, fontColour = "white", fgFill = "#7B3F00",
  fontName = "Calibri", textDecoration = "bold",
  halign = "center", valign = "center",
  wrapText = TRUE, border = "TopBottomLeftRight", borderStyle = "thin"
)
st_cap_est <- createStyle(
  fontSize = 9, fontColour = "white", fgFill = "#145A32",
  fontName = "Calibri", textDecoration = "bold",
  halign = "center", valign = "center",
  wrapText = TRUE, border = "TopBottomLeftRight", borderStyle = "thin"
)
st_cap_mod <- createStyle(
  fontSize = 9, fontColour = "#2C3E50", fgFill = "#D5E9F5",
  fontName = "Calibri", textDecoration = "bold",
  halign = "center", valign = "center",
  wrapText = TRUE, border = "TopBottomLeftRight", borderStyle = "thin"
)
st_inp <- createStyle(
  fontSize = 10, fontName = "Calibri",
  halign = "center", valign = "center",
  border = "TopBottomLeftRight", borderStyle = "thin"
)
st_mod <- createStyle(
  fontSize = 10, fontName = "Calibri", fgFill = "#EBF5FB",
  halign = "center", valign = "center",
  border = "TopBottomLeftRight", borderStyle = "thin"
)

# ── Crear workbook ───────────────────────────────────────────
wb <- createWorkbook()
addWorksheet(wb, "Plantilla")
addWorksheet(wb, "Instruccions")
addWorksheet(wb, "Llistes")

freezePane(wb, "Plantilla", firstActiveRow = ROW_DATA, firstActiveCol = 2)

tot_cols <- COL_COMP + N_COMP - 1

# ── Fila títol ───────────────────────────────────────────────
mergeCells(wb, "Plantilla", cols = 1:tot_cols, rows = ROW_TITLE)
writeData(wb, "Plantilla",
  "PLANTILLA ALUMNAT ANTIC — Predicció de risc d'absentisme (TFG FEE/UB)",
  startCol = 1, startRow = ROW_TITLE)
addStyle(wb, "Plantilla", st_titol,
  rows = ROW_TITLE, cols = 1:tot_cols, gridExpand = TRUE)
setRowHeights(wb, "Plantilla", rows = ROW_TITLE, heights = 26)

# ── Fila notes ───────────────────────────────────────────────
mergeCells(wb, "Plantilla", cols = 1:tot_cols, rows = ROW_NOTE)
writeData(wb, "Plantilla", paste0(
  "Emplena totes les columnes BLAVES. Les columnes GRISES es calculen automàticament. ",
  "Exporta les columnes GRISES (", col_letter(COL_COMP), ":",
  col_letter(tot_cols), ") com a CSV i puja-les al Shiny (pestanya Alumnat antic)."),
  startCol = 1, startRow = ROW_NOTE)
addStyle(wb, "Plantilla", st_nota_inst,
  rows = ROW_NOTE, cols = 1:tot_cols, gridExpand = TRUE)
setRowHeights(wb, "Plantilla", rows = ROW_NOTE, heights = 18)

# ── Fila llegenda ────────────────────────────────────────────
mergeCells(wb, "Plantilla", cols = 1:tot_cols, rows = ROW_LEGEND)
writeData(wb, "Plantilla", paste0(
  "NOTA: 1=5.0-5.9  |  2=6.0-6.9  |  3=7.0-7.9  |  4=8.0-8.9  |  5=≥9.0     ",
  "IA_*/M_*/E_* (escala Likert):  1=mai  2=gairebé mai  3=de vegades  ",
  "4=sovint  5=molt sovint  6=sempre"),
  startCol = 1, startRow = ROW_LEGEND)
addStyle(wb, "Plantilla", st_nota_inst,
  rows = ROW_LEGEND, cols = 1:tot_cols, gridExpand = TRUE)
setRowHeights(wb, "Plantilla", rows = ROW_LEGEND, heights = 16)

# ── Capçaleres de secció (fila 4) ────────────────────────────
mergeCells(wb, "Plantilla", cols = 1:N_INP, rows = ROW_SECCAP)
writeData(wb, "Plantilla", "ENTRADA DE DADES (emplenar)",
  startCol = 1, startRow = ROW_SECCAP)
addStyle(wb, "Plantilla", st_seccap_inp,
  rows = ROW_SECCAP, cols = 1:N_INP, gridExpand = TRUE)

mergeCells(wb, "Plantilla", cols = COL_COMP:tot_cols, rows = ROW_SECCAP)
writeData(wb, "Plantilla",
  "VARIABLES DEL MODEL + ÍTEMS EFA — exportar com a CSV per al Shiny",
  startCol = COL_COMP, startRow = ROW_SECCAP)
addStyle(wb, "Plantilla", st_seccap_mod,
  rows = ROW_SECCAP, cols = COL_COMP:tot_cols, gridExpand = TRUE)
setRowHeights(wb, "Plantilla", rows = ROW_SECCAP, heights = 16)

# ── Capçaleres de columna (fila 5) ───────────────────────────
# INPUT: colors per grup
cols_inp_id      <- 1
cols_inp_precurs <- 2:10
cols_inp_ia      <- 11:17
cols_inp_mot     <- 18:30
cols_inp_est     <- 31:41

for (ci in seq_along(inp_noms))
  writeData(wb, "Plantilla", inp_noms[ci], startCol = ci, startRow = ROW_COLCAP)
addStyle(wb, "Plantilla", st_cap_precurs,
  rows = ROW_COLCAP, cols = c(cols_inp_id, cols_inp_precurs), gridExpand = TRUE)
addStyle(wb, "Plantilla", st_cap_ia,
  rows = ROW_COLCAP, cols = cols_inp_ia, gridExpand = TRUE)
addStyle(wb, "Plantilla", st_cap_mot,
  rows = ROW_COLCAP, cols = cols_inp_mot, gridExpand = TRUE)
addStyle(wb, "Plantilla", st_cap_est,
  rows = ROW_COLCAP, cols = cols_inp_est, gridExpand = TRUE)

# MODEL capçaleres
for (ci in seq_along(comp_noms))
  writeData(wb, "Plantilla", comp_noms[ci],
    startCol = COL_COMP + ci - 1, startRow = ROW_COLCAP)
addStyle(wb, "Plantilla", st_cap_mod,
  rows = ROW_COLCAP, cols = COL_COMP:tot_cols, gridExpand = TRUE)
setRowHeights(wb, "Plantilla", rows = ROW_COLCAP, heights = 32)

# ── Files de dades ────────────────────────────────────────────
rows_data <- ROW_DATA:(ROW_DATA + N_ROWS - 1)
addStyle(wb, "Plantilla", st_inp,
  rows = rows_data, cols = 1:N_INP, gridExpand = TRUE)
addStyle(wb, "Plantilla", st_mod,
  rows = rows_data, cols = COL_COMP:tot_cols, gridExpand = TRUE)

for (r in rows_data) {
  fmls <- make_formulas(r)
  for (ci in seq_along(fmls))
    writeFormula(wb, "Plantilla", x = fmls[[ci]],
      startCol = COL_COMP + ci - 1, startRow = r)
}

# ── Validació de dades ────────────────────────────────────────
# T_AVAL (col 5)
dataValidation(wb, "Plantilla", col = inp_col["T_AVAL"],
  rows = rows_data,
  type = "list", value = paste0('"', paste(t_aval_vals, collapse=","), '"'))
# CURS (col 6)
dataValidation(wb, "Plantilla", col = inp_col["CURS"],
  rows = rows_data,
  type = "list", value = paste0('"', paste(curs_vals, collapse=","), '"'))
# GENERE (col 7)
dataValidation(wb, "Plantilla", col = inp_col["GENERE"],
  rows = rows_data,
  type = "list", value = paste0('"', paste(genere_vals, collapse=","), '"'))
# GRAU (col 8) — via full auxiliar
writeData(wb, "Llistes", data.frame(GRAU = grau_vals), startRow = 1)
dataValidation(wb, "Plantilla", col = inp_col["GRAU"],
  rows = rows_data, type = "list",
  value = paste0("Llistes!$A$2:$A$", 1 + length(grau_vals)))
# DEDIC (col 9)
dataValidation(wb, "Plantilla", col = inp_col["DEDIC"],
  rows = rows_data,
  type = "list", value = paste0('"', paste(dedic_vals, collapse=","), '"'))

# NOTA 1-5
dataValidation(wb, "Plantilla", col = inp_col["NOTA"],
  rows = rows_data, type = "whole", operator = "between", value = c(1, 5))
# EDAT 17-70
dataValidation(wb, "Plantilla", col = inp_col["EDAT"],
  rows = rows_data, type = "whole", operator = "between", value = c(17, 70))
# DESPL 0-300
dataValidation(wb, "Plantilla", col = inp_col["DESPL"],
  rows = rows_data, type = "whole", operator = "between", value = c(0, 300))
# N_ASSIG 1-16
dataValidation(wb, "Plantilla", col = inp_col["N_ASSIG"],
  rows = rows_data, type = "whole", operator = "between", value = c(1, 16))

# IA_*, M_*, E_* 1-6
likert_cols <- c(inp_col[inp_ia], inp_col[items_mot], inp_col[items_est])
for (col_lik in likert_cols)
  dataValidation(wb, "Plantilla", col = col_lik,
    rows = rows_data, type = "whole", operator = "between", value = c(1, 6))

# ── Amplades de columna ───────────────────────────────────────
setColWidths(wb, "Plantilla", cols = 1,             widths = 9)   # id
setColWidths(wb, "Plantilla", cols = 2:4,           widths = 8)   # EDAT, DESPL, N_ASSIG
setColWidths(wb, "Plantilla", cols = 5,             widths = 8)   # NOTA
setColWidths(wb, "Plantilla", cols = 6:7,           widths = 11)  # T_AVAL, CURS
setColWidths(wb, "Plantilla", cols = 8,             widths = 9)   # GENERE
setColWidths(wb, "Plantilla", cols = 9,             widths = 15)  # GRAU
setColWidths(wb, "Plantilla", cols = 10,            widths = 17)  # DEDIC
setColWidths(wb, "Plantilla", cols = 11:17,         widths = 8)   # IA_*
setColWidths(wb, "Plantilla", cols = 18:30,         widths = 8)   # M_*
setColWidths(wb, "Plantilla", cols = 31:41,         widths = 8)   # E_*
setColWidths(wb, "Plantilla", cols = COL_SEP,       widths = 2)   # separador
setColWidths(wb, "Plantilla",
  cols = COL_COMP:tot_cols,
  widths = c(rep(7, 9), rep(7, 7), rep(7, 13), rep(7, 11)))

# ── Full Instruccions ────────────────────────────────────────
st_h <- createStyle(fontSize = 12, textDecoration = "bold",
  fontName = "Calibri", fgFill = "#2C3E50", fontColour = "white",
  halign = "left", indent = 1)
st_b <- createStyle(fontSize = 10, fontName = "Calibri",
  wrapText = TRUE, halign = "left")

instruccions <- data.frame(Text = c(
  "COM USAR AQUESTA PLANTILLA (Alumnat Antic)",
  "",
  "1. Omple les columnes BLAVES (A fins AO), una fila per alumne.",
  "   - Blau fosc (A-J): variables pre-curs i demogràfiques",
  "   - Blau mig (K-Q): ítems d'ús d'IA (escala 1-6)",
  "   - Marró (R-AD): ítems de motius d'absentisme [M_*] (escala 1-6)",
  "   - Verd (AE-AO): ítems d'estratègies docents [E_*] (escala 1-6)",
  "2. Les columnes GRISES es calculen automàticament.",
  "3. Un cop omplert:",
  "   a. Selecciona les columnes grises.",
  "   b. Copia i enganxa en un nou full (Enganxa especial > Valors).",
  "   c. Desa com a CSV (UTF-8).",
  "4. Puja el CSV al Shiny, pestanya 'Alumnat antic'.",
  "",
  "CODIFICACIÓ DE LES VARIABLES",
  "",
  "NOTA (1-5): 1=5.0-5.9 | 2=6.0-6.9 | 3=7.0-7.9 | 4=8.0-8.9 | 5=≥9.0",
  "T_AVAL: Continuada / Única",
  "CURS: 1r / 2n / 3r / 4t / 5è / 6è",
  "GENERE: Home / Dona / Altre",
  paste0("GRAU (dobles grau → DOBLE_GRAU_EST=1): ",
         paste(grau_doble, collapse=" | ")),
  "DEDIC: Estudiant a TC / Treballa ocasionalment / T.Parcial / T.Complet",
  "  (TREB_INTENS=1 si T.Parcial o T.Complet)",
  "",
  "IA_*, M_*, E_* (escala Likert 1-6):",
  "  1=mai  2=gairebé mai  3=de vegades  4=sovint  5=molt sovint  6=sempre",
  "",
  "ÍTEMS M_* (motius absentisme):",
  "  M_PASSIU-M_AMICS: Desmotivació pedagògica",
  "  M_AUTON-M_REPET: Autogestió (no necessita classe)",
  "  M_FAM-M_TREB: Força major (família/salut/feina)",
  "",
  "ÍTEMS E_* (estratègies docents):",
  "  E_EXPL-E_PROP: Qualitat docent percebuda",
  "  E_ACT_AC-E_DINAM: Avaluació continuada",
  "  E_DESC-E_CURT: Gestió temps de classe",
  "  E_REDU: Preferència grups reduïts",
  "",
  "VARIABLES DEL MODEL (columnes grises — no editar):",
  paste(comp_noms, collapse = ", ")
), stringsAsFactors = FALSE)

writeData(wb, "Instruccions", instruccions, colNames = FALSE)
addStyle(wb, "Instruccions", st_h,
  rows = c(1, 15, 17, 27, 31, 35),
  cols = 1, gridExpand = TRUE)
addStyle(wb, "Instruccions", st_b,
  rows = 2:nrow(instruccions), cols = 1, gridExpand = TRUE)
setColWidths(wb, "Instruccions", cols = 1, widths = 90)

# Amagar full Llistes
sheetVisibility(wb)[3] <- "hidden"

# ── Desar ─────────────────────────────────────────────────────
out_path <- file.path("shiny_absentisme", "plantilla_alumnat_antic.xlsx")
saveWorkbook(wb, out_path, overwrite = TRUE)
cat(sprintf("Plantilla generada: %s\n", normalizePath(out_path)))
