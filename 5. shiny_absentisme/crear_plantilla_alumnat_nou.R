# Script per generar la plantilla Excel per a la pestanya "Alumnat nou"
# Executa des del directori del TFG:
#   source("5. shiny_absentisme/crear_plantilla_alumnat_nou.R")
# Requereix: install.packages("openxlsx")

library(openxlsx)

# ── Valors vàlids ────────────────────────────────────────────
t_aval_vals <- c("Continuada", "Única")
curs_vals <- c("1r", "2n", "3r", "4t", "5è", "6è+")
genere_vals <- c("Home", "Dona", "No binari", "Prefereixo no respondre")
grau_doble <- c("Estadística", "Doble Eco+Est", "Doble ADE+Soc",
                  "Doble ADE+Mat", "Doble ADE+Dret", "Doble ADE+Qui")
grau_simples <- c("ADE", "Economia", "Emp.Int", "Sociologia")
grau_vals <- c(grau_simples, grau_doble)
dedic_vals <- c("E.Complet", "T.Ocasional", "T.Parcial", "T.Complet")

# ── Dimensions ───────────────────────────────────────────────
N_ROWS <- 50   # files de dades buides
ROW_TITLE <- 1
ROW_NOTE <- 2
ROW_LEGEND <- 3
ROW_SECCAP <- 4    # capçalera de secció (INPUT / MODEL)
ROW_COLCAP <- 5    # noms de columna
ROW_DATA <- 6    # primera fila de dades

N_INP <- 17   # id + 16 inputs
COL_SEP <- N_INP + 1          # columna separadora buida
COL_COMP <- N_INP + 2          # primera columna de variables model
N_COMP <- 16

# ── Mapping columnes input ────────────────────────────────────
# Col 1=id  2=EDAT  3=DESPL  4=N_ASSIG  5=NOTA  6=T_AVAL  7=CURS
# Col 8=GENERE  9=GRAU  10=DEDIC  11=IA_HABIT ... 17=IA_CONF
inp_noms <- c("id", "EDAT", "DESPL", "N_ASSIG", "NOTA",
              "T_AVAL", "CURS", "GENERE", "GRAU", "DEDIC",
              "IA_HABIT", "IA_COMPR", "IA_REND", "IA_PDFS",
              "IA_SUBST", "IA_ATENC", "IA_CONF")

inp_desc <- c(
  "Identificador\n(opcional)",
  "Edat\n(anys)",
  "Desplaçament\nal campus (min)",
  "Nombre\nassignatures",
  "Nota d'accés\n(1=5.0-5.9 … 5=≥9.0)",
  "Tipus\nd'avaluació",
  "Curs\nacadèmic",
  "Gènere",
  "Grau que\ncursa",
  "Dedicació\nlaboral",
  "IA: ús habitual\nestudi",
  "IA: ajuda a\nentendre",
  "IA: millora\nrendiment",
  "IA: processa\nPDFs",
  "IA: substitut\nde classe",
  "IA: redueix\natenció",
  "Confia IA més\nque professor"
)

comp_noms <- c("EDAT", "DESPL", "N_ASSIG", "NOTA_num", "T_AVAL_num",
               "CURS_1R", "GENERE_Home", "DOBLE_GRAU_EST", "TREB_INTENS",
               "IA_HABIT", "IA_COMPR", "IA_REND", "IA_PDFS",
               "IA_SUBST", "IA_ATENC", "IA_CONF")

# ── Fórmules per a cada fila ─────────────────────────────────
make_formulas <- function(r) {
  b <- paste0("B", r); c <- paste0("C", r); d <- paste0("D", r)
  e <- paste0("E", r); f <- paste0("F", r); g <- paste0("G", r)
  h <- paste0("H", r); i <- paste0("I", r); j <- paste0("J", r)
  k <- paste0("K", r); l <- paste0("L", r); m <- paste0("M", r)
  n <- paste0("N", r); o <- paste0("O", r); p <- paste0("P", r)
  q <- paste0("Q", r)

  grau_or <- paste(paste0(i, '="', grau_doble, '"'), collapse = ",")

  list(
    EDAT           = paste0("=IF(", b, '="","",', b, ")"),
    DESPL          = paste0("=IF(", c, '="","",', c, ")"),
    N_ASSIG        = paste0("=IF(", d, '="","",', d, ")"),
    NOTA_num       = paste0("=IF(", e, '="","",', e, ")"),
    T_AVAL_num     = paste0("=IF(", f, '="","",IF(', f, '="Continuada",1,0))'),
    CURS_1R        = paste0("=IF(", g, '="","",IF(', g, '="1r",1,0))'),
    GENERE_Home    = paste0("=IF(", h, '="","",IF(', h, '="Home",1,0))'),
    DOBLE_GRAU_EST = paste0("=IF(", i, '="","",IF(OR(', grau_or, '),1,0))'),
    TREB_INTENS    = paste0("=IF(", j, '="","",IF(OR(', j, '="T.Parcial",',
                               j, '="T.Complet"),1,0))'),
    IA_HABIT       = paste0("=IF(", k, '="","",', k, ")"),
    IA_COMPR       = paste0("=IF(", l, '="","",', l, ")"),
    IA_REND        = paste0("=IF(", m, '="","",', m, ")"),
    IA_PDFS        = paste0("=IF(", n, '="","",', n, ")"),
    IA_SUBST       = paste0("=IF(", o, '="","",', o, ")"),
    IA_ATENC       = paste0("=IF(", p, '="","",', p, ")"),
    IA_CONF        = paste0("=IF(", q, '="","",', q, ")")
  )
}

# ── Estils ───────────────────────────────────────────────────
st_titol <- createStyle(
  fontSize = 13, fontColour = "white", fgFill = "#2C3E50",
  fontName = "Calibri", textDecoration = "bold",
  halign = "center", valign = "center"
)
st_nota_inst <- createStyle(
  fontSize = 9, fontColour = "#555555",
  fontName = "Calibri", halign = "left", valign = "center",
  textDecoration = "italic"
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
st_cap_inp <- createStyle(
  fontSize = 9, fontColour = "white", fgFill = "#4A90B8",
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
st_mod_header_note <- createStyle(
  fontSize = 8, fontColour = "#888888",
  fontName = "Calibri", halign = "center"
)

# ── Crear workbook ───────────────────────────────────────────
wb <- createWorkbook()
addWorksheet(wb, "Plantilla")
addWorksheet(wb, "Instruccions")

# freeze panes
freezePane(wb, "Plantilla", firstActiveRow = ROW_DATA, firstActiveCol = 2)

# ── Fila títol ───────────────────────────────────────────────
tot_cols <- COL_COMP + N_COMP - 1
mergeCells(wb, "Plantilla", cols = 1:tot_cols, rows = ROW_TITLE)
writeData(wb, "Plantilla",
          "PLANTILLA ALUMNAT NOU — Predicció de risc d'absentisme (TFG FEE/UB)",
          startCol = 1, startRow = ROW_TITLE)
addStyle(wb, "Plantilla", st_titol,
         rows = ROW_TITLE, cols = 1:tot_cols, gridExpand = TRUE)
setRowHeights(wb, "Plantilla", rows = ROW_TITLE, heights = 26)

# ── Fila notes / instruccions ────────────────────────────────
mergeCells(wb, "Plantilla", cols = 1:tot_cols, rows = ROW_NOTE)
writeData(wb, "Plantilla",
  paste0("Emplena les columnes BLAVES (A-Q). Les columnes GRISES es calculen automàticament. ",
         "Un cop omplert, exporta les columnes S:", LETTERS[COL_COMP + N_COMP - 1],
         " com a CSV i puja-les al Shiny (pestanya Alumnat nou)."),
  startCol = 1, startRow = ROW_NOTE)
addStyle(wb, "Plantilla", st_nota_inst,
         rows = ROW_NOTE, cols = 1:tot_cols, gridExpand = TRUE)
setRowHeights(wb, "Plantilla", rows = ROW_NOTE, heights = 18)

# ── Fila llegenda ────────────────────────────────────────────
mergeCells(wb, "Plantilla", cols = 1:tot_cols, rows = ROW_LEGEND)
writeData(wb, "Plantilla",
  paste0("NOTA: 1=5.0-5.9  |  2=6.0-6.9  |  3=7.0-7.9  |  4=8.0-8.9  |  5=≥9.0     ",
         "IA_* (escala Likert):  1=mai  2=gairebé mai  3=de vegades  4=sovint  5=molt sovint  6=sempre"),
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

mergeCells(wb, "Plantilla", cols = COL_COMP:(COL_COMP + N_COMP - 1), rows = ROW_SECCAP)
writeData(wb, "Plantilla",
          "VARIABLES DEL MODEL — calculades automàticament (exportar com a CSV per al Shiny)",
          startCol = COL_COMP, startRow = ROW_SECCAP)
addStyle(wb, "Plantilla", st_seccap_mod,
         rows = ROW_SECCAP, cols = COL_COMP:(COL_COMP + N_COMP - 1), gridExpand = TRUE)
setRowHeights(wb, "Plantilla", rows = ROW_SECCAP, heights = 16)

# ── Capçaleres de columna (fila 5) ───────────────────────────
for (ci in seq_along(inp_noms)) {
  writeData(wb, "Plantilla", inp_noms[ci], startCol = ci, startRow = ROW_COLCAP)
}
addStyle(wb, "Plantilla", st_cap_inp,
         rows = ROW_COLCAP, cols = 1:N_INP, gridExpand = TRUE)

for (ci in seq_along(comp_noms)) {
  writeData(wb, "Plantilla", comp_noms[ci],
            startCol = COL_COMP + ci - 1, startRow = ROW_COLCAP)
}
addStyle(wb, "Plantilla", st_cap_mod,
         rows = ROW_COLCAP,
         cols = COL_COMP:(COL_COMP + N_COMP - 1), gridExpand = TRUE)
setRowHeights(wb, "Plantilla", rows = ROW_COLCAP, heights = 32)

# ── Descripcions (tooltip visual a fila capçalera) ───────────
# Les afegim com a comentaris de cel·la
for (ci in seq_along(inp_noms)) {
  writeComment(wb, "Plantilla",
               col = ci, row = ROW_COLCAP,
               comment = createComment(inp_desc[ci], author = "Plantilla"))
}

# ── Files de dades ────────────────────────────────────────────
rows_data <- ROW_DATA:(ROW_DATA + N_ROWS - 1)

# Estil columnes input
addStyle(wb, "Plantilla", st_inp,
         rows = rows_data, cols = 1:N_INP, gridExpand = TRUE)

# Estil columnes model
addStyle(wb, "Plantilla", st_mod,
         rows = rows_data,
         cols = COL_COMP:(COL_COMP + N_COMP - 1), gridExpand = TRUE)

# Fórmules a les columnes model
for (r in rows_data) {
  fmls <- make_formulas(r)
  for (ci in seq_along(fmls)) {
    writeFormula(wb, "Plantilla",
                 x = fmls[[ci]],
                 startCol = COL_COMP + ci - 1,
                 startRow = r)
  }
}

# ── Validació de dades (dropdowns) ────────────────────────────
dv_rows <- paste0(ROW_DATA, ":", ROW_DATA + N_ROWS - 1)

# T_AVAL (col F = 6)
dataValidation(wb, "Plantilla", col = 6, rows = ROW_DATA:(ROW_DATA + N_ROWS - 1),
               type = "list", value = paste0('"', paste(t_aval_vals, collapse = ","), '"'))

# CURS (col G = 7)
dataValidation(wb, "Plantilla", col = 7, rows = ROW_DATA:(ROW_DATA + N_ROWS - 1),
               type = "list", value = paste0('"', paste(curs_vals, collapse = ","), '"'))

# GENERE (col H = 8)
dataValidation(wb, "Plantilla", col = 8, rows = ROW_DATA:(ROW_DATA + N_ROWS - 1),
               type = "list", value = paste0('"', paste(genere_vals, collapse = ","), '"'))

# GRAU (col I = 9) — llista en un full auxiliar per la longitud
addWorksheet(wb, "Llistes")
writeData(wb, "Llistes", data.frame(GRAU = grau_vals), startRow = 1)
dataValidation(wb, "Plantilla", col = 9, rows = ROW_DATA:(ROW_DATA + N_ROWS - 1),
               type = "list",
               value = paste0("Llistes!$A$2:$A$", 1 + length(grau_vals)))

# DEDIC (col J = 10)
dataValidation(wb, "Plantilla", col = 10, rows = ROW_DATA:(ROW_DATA + N_ROWS - 1),
               type = "list", value = paste0('"', paste(dedic_vals, collapse = ","), '"'))

# NOTA (col E = 5): enter 1-5
dataValidation(wb, "Plantilla", col = 5, rows = ROW_DATA:(ROW_DATA + N_ROWS - 1),
               type = "whole", operator = "between", value = c(1, 5))

# IA (cols K-Q = 11-17): enter 1-6
for (col_ia in 11:17) {
  dataValidation(wb, "Plantilla", col = col_ia, rows = ROW_DATA:(ROW_DATA + N_ROWS - 1),
                 type = "whole", operator = "between", value = c(1, 6))
}

# EDAT: 17-70
dataValidation(wb, "Plantilla", col = 2, rows = ROW_DATA:(ROW_DATA + N_ROWS - 1),
               type = "whole", operator = "between", value = c(17, 70))

# DESPL: 0-300
dataValidation(wb, "Plantilla", col = 3, rows = ROW_DATA:(ROW_DATA + N_ROWS - 1),
               type = "whole", operator = "between", value = c(0, 300))

# N_ASSIG: 1-16
dataValidation(wb, "Plantilla", col = 4, rows = ROW_DATA:(ROW_DATA + N_ROWS - 1),
               type = "whole", operator = "between", value = c(1, 16))

# ── Amplades de columna ───────────────────────────────────────
setColWidths(wb, "Plantilla", cols = 1,    widths = 10)   # id
setColWidths(wb, "Plantilla", cols = 2:4,  widths = 9)    # EDAT, DESPL, N_ASSIG
setColWidths(wb, "Plantilla", cols = 5,    widths = 9)    # NOTA
setColWidths(wb, "Plantilla", cols = 6:7,  widths = 12)   # T_AVAL, CURS
setColWidths(wb, "Plantilla", cols = 8,    widths = 10)   # GENERE
setColWidths(wb, "Plantilla", cols = 9,    widths = 16)   # GRAU
setColWidths(wb, "Plantilla", cols = 10,   widths = 18)   # DEDIC
setColWidths(wb, "Plantilla", cols = 11:17, widths = 9)   # IA_*
setColWidths(wb, "Plantilla", cols = COL_SEP, widths = 2) # separador
# Columnes model — amplades ajustades als noms
comp_widths <- c(7, 7, 8, 9, 10, 8, 12, 15, 12, 8, 8, 8, 8, 8, 8, 8)
setColWidths(wb, "Plantilla",
             cols = COL_COMP:(COL_COMP + N_COMP - 1),
             widths = comp_widths)

# ── Full Instruccions ────────────────────────────────────────
st_h <- createStyle(fontSize = 12, textDecoration = "bold",
                    fontName = "Calibri", fgFill = "#2C3E50", fontColour = "white",
                    halign = "left", indent = 1)
st_b <- createStyle(fontSize = 10, fontName = "Calibri", wrapText = TRUE, halign = "left")

instruccions <- data.frame(Text = c(
  "COM USAR AQUESTA PLANTILLA",
  "",
  "1. Omple les columnes BLAVES (A fins Q), una fila per alumne.",
  "2. Les columnes GRISES (a la dreta) es calculen soles amb fórmules d'Excel.",
  "3. Un cop omplert:",
  "   a. Selecciona les columnes grises (EDAT fins IA_CONF del model).",
  "   b. Copia-les i enganxa-les en un nou full/arxiu (Enganxa especial > Valors).",
  "   c. Desa com a CSV (UTF-8).",
  "4. Puja el CSV al Shiny, pestanya 'Alumnat nou'.",
  "",
  "CODIFICACIÓ DE LES VARIABLES",
  "",
  "NOTA (nota d'accés a la universitat o expedient previ):",
  "  1 = 5.0 - 5.9   2 = 6.0 - 6.9   3 = 7.0 - 7.9   4 = 8.0 - 8.9   5 = 9.0 o més",
  "",
  "T_AVAL (tipus d'avaluació de les assignatures):",
  "  Continuada = avaluació continuada   |   Única = examen únic",
  "",
  "CURS: 1r / 2n / 3r / 4t / 5è / 6è+",
  "",
  "GENERE: Home / Dona / No binari / Prefereixo no respondre",
  "",
  "GRAU (afecta DOBLE_GRAU_EST=1 si és un dels dobles graus):",
  paste0("  Dobles grau: ", paste(grau_doble, collapse = " | ")),
  paste0("  Graus simples: ", paste(grau_simples, collapse = " | ")),
  "",
  "DEDIC (dedicació laboral):",
  "  E.Complet = estudia a temps complet, no treballa",
  "  T.Ocasional = treballa ocasionalment",
  "  T.Parcial = treballa a temps parcial (TREB_INTENS = 1)",
  "  T.Complet = treballa a temps complet (TREB_INTENS = 1)",
  "",
  "IA_* (escala Likert 1-6):",
  "  1=mai  2=gairebé mai  3=de vegades  4=sovint  5=molt sovint  6=sempre",
  "",
  "VARIABLES DEL MODEL (columnes grises — no editar):",
  paste(comp_noms, collapse = ", ")
), stringsAsFactors = FALSE)

writeData(wb, "Instruccions", instruccions, colNames = FALSE)
addStyle(wb, "Instruccions", st_h,
         rows = c(1, 11, 13, 16, 18, 20, 22, 25, 27, 32, 35),
         cols = 1, gridExpand = TRUE)
addStyle(wb, "Instruccions", st_b,
         rows = 2:nrow(instruccions), cols = 1, gridExpand = TRUE)
setColWidths(wb, "Instruccions", cols = 1, widths = 90)

# Amagar full Llistes
sheetVisibility(wb)[3] <- "hidden"

# ── Desar ─────────────────────────────────────────────────────
out_path <- file.path("shiny_absentisme", "plantilla_alumnat_nou.xlsx")
saveWorkbook(wb, out_path, overwrite = TRUE)
cat(sprintf("Plantilla generada: %s\n", normalizePath(out_path)))
