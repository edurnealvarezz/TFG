# Script per generar la plantilla Excel per a la pestanya "Alumnat antic"
# La secció grisa calcula automàticament els factor scores EFA i les
# variables pre-curs transformades que necessita el model RF-A.
#
# Executa des del directori del TFG:
#   source("5. shiny_absentisme/crear_plantilla_alumnat_antic.R")
# Requereix: install.packages("openxlsx")

library(openxlsx)

# ── Valors vàlids variables categòriques ─────────────────────
t_aval_vals <- c("Continuada", "Única")
curs_vals <- c("1r", "2n", "3r", "4t", "5è", "6è+")
genere_vals <- c("Home", "Dona", "No binari", "Prefereixo no respondre")
grau_doble <- c("Estadística", "Doble Eco+Est", "Doble ADE+Soc",
                  "Doble ADE+Mat", "Doble ADE+Dret", "Doble ADE+Qui")
grau_simples <- c("ADE", "Economia", "Emp.Int", "Sociologia")
grau_vals <- c(grau_simples, grau_doble)
dedic_vals <- c("E.Complet", "T.Ocasional", "T.Parcial", "T.Complet")

# ── Ítems d'entrada ──────────────────────────────────────────
# IA (7 ítems): els primers 4 formen IA_EINA_ESTUDI; IA_SUBST va a RF-A directament
inp_ia <- c("IA_HABIT","IA_COMPR","IA_REND","IA_PDFS","IA_SUBST","IA_ATENC","IA_CONF")
# Motius (13 ítems): 3 factors EFA
items_mot <- c("M_PASSIU","M_TEOR","M_AVORR","M_PROF","M_AMICS",  # MOT_DESMOTIVACIO
               "M_AUTON","M_CV","M_UTIL","M_EXAM","M_REPET",       # MOT_AUTOGESTIO
               "M_FAM","M_SALUT","M_TREB")                          # MOT_FORCA_MAJOR
# Estratègies (11 ítems): 4 factors EFA
items_est <- c("E_EXPL","E_RITME","E_CLIMA","E_PROP",              # EST_QUALITAT_DOC
               "E_ACT_AC","E_PES_AC","E_PART","E_DINAM",           # EST_AVALUACIO_AC
               "E_DESC","E_CURT",                                   # EST_TEMPS_CLASSE
               "E_REDU")                                            # EST_GRUPS_REDUITS

# ── Pesos EFA (λ de la càrrega principal) ────────────────────
pesos_efa <- list(
  MOT_DESMOTIVACIO  = c(M_PASSIU = 0.867, M_TEOR = 0.854, M_AVORR = 0.702,
                         M_PROF   = 0.561, M_AMICS = 0.485),
  MOT_AUTOGESTIO    = c(M_AUTON = 0.830, M_CV = 0.739, M_UTIL = 0.607,
                         M_EXAM  = 0.368, M_REPET = 0.335),
  MOT_FORCA_MAJOR   = c(M_FAM = 0.957, M_SALUT = 0.840, M_TREB = 0.662),
  EST_QUALITAT_DOC  = c(E_EXPL = 0.985, E_RITME = 0.799,
                         E_CLIMA = 0.782, E_PROP = 0.588),
  EST_AVALUACIO_AC  = c(E_ACT_AC = 0.910, E_PES_AC = 0.720,
                         E_PART   = 0.627, E_DINAM  = 0.385),
  EST_TEMPS_CLASSE  = c(E_DESC = 0.813, E_CURT = 0.665),
  EST_GRUPS_REDUITS = c(E_REDU = 0.863),
  IA_EINA_ESTUDI    = c(IA_COMPR = 0.903, IA_HABIT = 0.880,
                         IA_PDFS  = 0.762, IA_REND  = 0.740)
)

# ── Dimensions ───────────────────────────────────────────────
N_ROWS <- 50
ROW_TITLE <- 1
ROW_NOTE <- 2
ROW_LEGEND <- 3
ROW_SECCAP <- 4
ROW_COLCAP <- 5
ROW_DATA <- 6

# Columnes INPUT: id + 9 pre-curs + 7 IA + 13 M + 11 E = 41
inp_precurs <- c("EDAT","DESPL","N_ASSIG","NOTA","T_AVAL","CURS","GENERE","GRAU","DEDIC")
inp_noms <- c("id", inp_precurs, inp_ia, items_mot, items_est)
N_INP <- length(inp_noms)   # 41

# Columnes MODEL (grises): factor scores EFA + IA_SUBST + pre-curs transformades = 17 vars RF-A
comp_noms <- c(
  # Factor scores EFA (8)
  "MOT_DESMOTIVACIO", "MOT_AUTOGESTIO", "MOT_FORCA_MAJOR",
  "EST_QUALITAT_DOC", "EST_AVALUACIO_AC", "EST_TEMPS_CLASSE", "EST_GRUPS_REDUITS",
  "IA_EINA_ESTUDI",
  # IA individual (1)
  "IA_SUBST_num",
  # Variables pre-curs transformades (8)
  "T_AVAL_num", "CURS_1R_num", "EDAT", "NOTA_num",
  "DOBLE_GRAU_EST", "TREB_INTENS", "DESPL", "N_ASSIG"
)
N_COMP <- length(comp_noms)   # 17

COL_SEP <- N_INP + 1         # 42: columna separadora
COL_COMP <- N_INP + 2         # 43: primera columna del model

# ── Helpers ──────────────────────────────────────────────────
col_letter <- function(n) {
  if (n <= 26) LETTERS[n]
  else paste0(LETTERS[(n - 1) %/% 26], LETTERS[(n - 1) %% 26 + 1])
}

inp_col <- setNames(seq_along(inp_noms), inp_noms)

# Fórmula de mitjana ponderada per a un factor EFA
# Exclou ítems buits del numerador i denominador
wgt_avg_formula <- function(r, items_vec) {
  nms <- names(items_vec)
  wts <- unname(items_vec)
  cl <- function(nm) paste0(col_letter(inp_col[nm]), r)
  num <- mapply(function(nm, wt) sprintf('IF(%s="",0,%g*%s)', cl(nm), wt, cl(nm)),
                nms, wts, SIMPLIFY = TRUE)
  den <- mapply(function(nm, wt) sprintf('IF(%s="",0,%g)', cl(nm), wt),
                nms, wts, SIMPLIFY = TRUE)
  sprintf('=IFERROR((%s)/(%s),"")',
          paste(num, collapse = "+"), paste(den, collapse = "+"))
}

make_formulas <- function(r) {
  cl <- function(nm) paste0(col_letter(inp_col[nm]), r)
  grau_or <- paste(paste0(cl("GRAU"), '="', grau_doble, '"'), collapse = ",")

  c(
    # ── Factor scores EFA ──────────────────────────────────────
    lapply(names(pesos_efa), function(fn)
      wgt_avg_formula(r, pesos_efa[[fn]])) |>
      setNames(names(pesos_efa)),

    # ── IA_SUBST directa (a RF-A s'anomena IA_SUBST_num) ──────
    list(IA_SUBST_num = sprintf('=IF(%s="","",AS.NUMERIC(%s))',
                                 cl("IA_SUBST"), cl("IA_SUBST")) |>
           gsub("AS.NUMERIC(", "", x = _, fixed = TRUE) |>
           gsub(")", "", x = _, fixed = TRUE)),

    # ── Pre-curs transformades ─────────────────────────────────
    list(
      T_AVAL_num     = sprintf('=IF(%s="","",IF(%s="Continuada",1,0))',
                                cl("T_AVAL"), cl("T_AVAL")),
      CURS_1R_num    = sprintf('=IF(%s="","",IF(%s="1r",1,0))',
                                cl("CURS"), cl("CURS")),
      EDAT           = sprintf('=IF(%s="","",%s)', cl("EDAT"),   cl("EDAT")),
      NOTA_num       = sprintf('=IF(%s="","",%s)', cl("NOTA"),   cl("NOTA")),
      DOBLE_GRAU_EST = sprintf('=IF(%s="","",IF(OR(%s),1,0))',
                                cl("GRAU"), grau_or),
      TREB_INTENS    = sprintf('=IF(%s="","",IF(OR(%s="T.Parcial",%s="T.Complet"),1,0))',
                                cl("DEDIC"), cl("DEDIC"), cl("DEDIC")),
      DESPL          = sprintf('=IF(%s="","",%s)', cl("DESPL"),  cl("DESPL")),
      N_ASSIG        = sprintf('=IF(%s="","",%s)', cl("N_ASSIG"), cl("N_ASSIG"))
    )
  )
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
st_cap_mod_efa <- createStyle(
  fontSize = 9, fontColour = "#2C3E50", fgFill = "#D5E9F5",
  fontName = "Calibri", textDecoration = "bold",
  halign = "center", valign = "center",
  wrapText = TRUE, border = "TopBottomLeftRight", borderStyle = "thin"
)
st_cap_mod_pre <- createStyle(
  fontSize = 9, fontColour = "#2C3E50", fgFill = "#E8F5E9",
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
  "Exporta TOTES les columnes grises (",
  col_letter(COL_COMP), ":", col_letter(tot_cols),
  ") com a CSV i puja-les al Shiny (pestanya Alumnat antic)."),
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
  "VARIABLES MODEL RF-A (factor scores EFA + pre-curs) — exportar com a CSV per al Shiny",
  startCol = COL_COMP, startRow = ROW_SECCAP)
addStyle(wb, "Plantilla", st_seccap_mod,
  rows = ROW_SECCAP, cols = COL_COMP:tot_cols, gridExpand = TRUE)
setRowHeights(wb, "Plantilla", rows = ROW_SECCAP, heights = 16)

# ── Capçaleres de columna (fila 5) ───────────────────────────
# INPUT
cols_inp_id <- 1
cols_inp_precurs <- 2:10
cols_inp_ia <- 11:17
cols_inp_mot <- 18:30
cols_inp_est <- 31:41

for (ci in seq_along(inp_noms))
  writeData(wb, "Plantilla", inp_noms[ci], startCol = ci, startRow = ROW_COLCAP)
addStyle(wb, "Plantilla", st_cap_precurs,
  rows = ROW_COLCAP, cols = c(cols_inp_id, cols_inp_precurs), gridExpand = TRUE)
addStyle(wb, "Plantilla", st_cap_ia,
  rows = ROW_COLCAP, cols = cols_inp_ia,  gridExpand = TRUE)
addStyle(wb, "Plantilla", st_cap_mot,
  rows = ROW_COLCAP, cols = cols_inp_mot, gridExpand = TRUE)
addStyle(wb, "Plantilla", st_cap_est,
  rows = ROW_COLCAP, cols = cols_inp_est, gridExpand = TRUE)

# MODEL: factor scores (cols 43-51) en blau clar; pre-curs (52-59) en verd clar
cols_mod_efa <- COL_COMP:(COL_COMP + 8)       # 9 factors/IA_SUBST_num
cols_mod_pre <- (COL_COMP + 9):(COL_COMP + N_COMP - 1)  # 8 pre-curs

for (ci in seq_along(comp_noms))
  writeData(wb, "Plantilla", comp_noms[ci],
    startCol = COL_COMP + ci - 1, startRow = ROW_COLCAP)
addStyle(wb, "Plantilla", st_cap_mod_efa,
  rows = ROW_COLCAP, cols = cols_mod_efa, gridExpand = TRUE)
addStyle(wb, "Plantilla", st_cap_mod_pre,
  rows = ROW_COLCAP, cols = cols_mod_pre, gridExpand = TRUE)
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
dataValidation(wb, "Plantilla", col = inp_col["T_AVAL"], rows = rows_data,
  type = "list", value = paste0('"', paste(t_aval_vals, collapse = ","), '"'))
dataValidation(wb, "Plantilla", col = inp_col["CURS"], rows = rows_data,
  type = "list", value = paste0('"', paste(curs_vals, collapse = ","), '"'))
dataValidation(wb, "Plantilla", col = inp_col["GENERE"], rows = rows_data,
  type = "list", value = paste0('"', paste(genere_vals, collapse = ","), '"'))
writeData(wb, "Llistes", data.frame(GRAU = grau_vals), startRow = 1)
dataValidation(wb, "Plantilla", col = inp_col["GRAU"], rows = rows_data,
  type = "list", value = paste0("Llistes!$A$2:$A$", 1 + length(grau_vals)))
dataValidation(wb, "Plantilla", col = inp_col["DEDIC"], rows = rows_data,
  type = "list", value = paste0('"', paste(dedic_vals, collapse = ","), '"'))

dataValidation(wb, "Plantilla", col = inp_col["NOTA"], rows = rows_data,
  type = "whole", operator = "between", value = c(1, 5))
dataValidation(wb, "Plantilla", col = inp_col["EDAT"], rows = rows_data,
  type = "whole", operator = "between", value = c(17, 70))
dataValidation(wb, "Plantilla", col = inp_col["DESPL"], rows = rows_data,
  type = "whole", operator = "between", value = c(0, 300))
dataValidation(wb, "Plantilla", col = inp_col["N_ASSIG"], rows = rows_data,
  type = "whole", operator = "between", value = c(1, 16))

likert_cols <- c(inp_col[inp_ia], inp_col[items_mot], inp_col[items_est])
for (col_lik in likert_cols)
  dataValidation(wb, "Plantilla", col = col_lik, rows = rows_data,
    type = "whole", operator = "between", value = c(1, 6))

# ── Amplades de columna ───────────────────────────────────────
setColWidths(wb, "Plantilla", cols = 1,       widths = 9)    # id
setColWidths(wb, "Plantilla", cols = 2:4,     widths = 8)    # EDAT, DESPL, N_ASSIG
setColWidths(wb, "Plantilla", cols = 5,       widths = 8)    # NOTA
setColWidths(wb, "Plantilla", cols = 6:7,     widths = 11)   # T_AVAL, CURS
setColWidths(wb, "Plantilla", cols = 8,       widths = 9)    # GENERE
setColWidths(wb, "Plantilla", cols = 9,       widths = 15)   # GRAU
setColWidths(wb, "Plantilla", cols = 10,      widths = 17)   # DEDIC
setColWidths(wb, "Plantilla", cols = 11:41,   widths = 8)    # IA + M + E
setColWidths(wb, "Plantilla", cols = COL_SEP, widths = 2)    # separador
setColWidths(wb, "Plantilla", cols = COL_COMP:(COL_COMP + 8 - 1), widths = 14)  # factor scores
setColWidths(wb, "Plantilla", cols = (COL_COMP + 8):(tot_cols),   widths = 9)   # pre-curs

# ── Full Instruccions ────────────────────────────────────────
st_h <- createStyle(fontSize = 12, textDecoration = "bold",
  fontName = "Calibri", fgFill = "#2C3E50", fontColour = "white",
  halign = "left", indent = 1)
st_b <- createStyle(fontSize = 10, fontName = "Calibri",
  wrapText = TRUE, halign = "left")

instruccions <- data.frame(Text = c(
  "COM USAR AQUESTA PLANTILLA (Alumnat Antic — Model RF-A)",
  "",
  "1. Omple les columnes BLAVES (columnes A fins AO), una fila per alumne.",
  "   Blau fosc (A-J): variables pre-curs i demogràfiques.",
  "   Blau mig (K-Q):  ítems d'ús d'IA (escala 1-6).",
  "   Marró (R-AD):    ítems de motius d'absentisme M_* (escala 1-6).",
  "   Verd (AE-AO):    ítems d'estratègies docents E_* (escala 1-6).",
  "2. Les columnes GRISES (factor scores EFA + pre-curs) es calculen automàticament.",
  "   NO les editis manualment.",
  "3. Un cop omplert:",
  "   a. Selecciona les columnes grises (AQ fins au aprox.).",
  "   b. Copia i enganxa en un nou full (Enganxa especial > Valors).",
  "   c. Desa com a CSV (UTF-8).",
  "4. Puja el CSV al Shiny, pestanya 'Alumnat antic'.",
  "",
  "CODIFICACIÓ DE LES VARIABLES",
  "",
  "NOTA (1-5): 1=5.0-5.9 | 2=6.0-6.9 | 3=7.0-7.9 | 4=8.0-8.9 | 5=≥9.0",
  "T_AVAL: Continuada / Única",
  "CURS: 1r / 2n / 3r / 4t / 5è / 6è+",
  "GENERE: Home / Dona / No binari / Prefereixo no respondre",
  paste0("GRAU (dobles → DOBLE_GRAU_EST=1): ", paste(grau_doble, collapse = " | ")),
  "DEDIC: E.Complet / T.Ocasional / T.Parcial / T.Complet",
  "  (TREB_INTENS=1 si T.Parcial o T.Complet)",
  "",
  "Escala Likert IA_*/M_*/E_* (1-6):",
  "  1=mai  2=gairebé mai  3=de vegades  4=sovint  5=molt sovint  6=sempre",
  "",
  "FACTORS EFA — ítems que els componen:",
  "  MOT_DESMOTIVACIO: M_PASSIU, M_TEOR, M_AVORR, M_PROF, M_AMICS",
  "  MOT_AUTOGESTIO:   M_AUTON, M_CV, M_UTIL, M_EXAM, M_REPET",
  "  MOT_FORCA_MAJOR:  M_FAM, M_SALUT, M_TREB",
  "  EST_QUALITAT_DOC: E_EXPL, E_RITME, E_CLIMA, E_PROP",
  "  EST_AVALUACIO_AC: E_ACT_AC, E_PES_AC, E_PART, E_DINAM",
  "  EST_TEMPS_CLASSE: E_DESC, E_CURT",
  "  EST_GRUPS_REDUITS: E_REDU",
  "  IA_EINA_ESTUDI:   IA_COMPR, IA_HABIT, IA_PDFS, IA_REND",
  "  IA_SUBST_num:     IA_SUBST (valor directe, no factor)",
  "",
  "VARIABLES DEL MODEL RF-A (columnes grises — no editar):",
  paste(comp_noms, collapse = ", ")
), stringsAsFactors = FALSE)

writeData(wb, "Instruccions", instruccions, colNames = FALSE)
addStyle(wb, "Instruccions", st_h,
  rows = c(1, 16, 18, 26, 29),
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
cat(sprintf("Input: %d columnes blaves | Model: %d columnes grises (vars RF-A)\n",
            N_INP, N_COMP))
