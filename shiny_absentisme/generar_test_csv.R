# Genera CSV de prova per testejar el Shiny.
# Executa des del directori del TFG:
#   source("shiny_absentisme/generar_test_csv.R")
# Genera:
#   shiny_absentisme/test_alumnat_nou.csv   → puja a pestanya "Alumnat nou"
#   shiny_absentisme/test_alumnat_antic.csv → puja a pestanya "Alumnat antic"

set.seed(42)
n <- 10

# ── 1. Alumnat NOU (columnes RAW — el Shiny les transforma internament) ────
# Columnes: vars_nou_raw del Shiny (NOTA=1-5 numèric, T_AVAL/CURS/GENERE/GRAU/DEDIC text)
nou <- data.frame(
  EDAT    = c(21, 19, 23, 22, 20, 24, 21, 19, 25, 22),
  DESPL   = c(25, 10, 45,  0, 30, 60, 15, 20, 90,  5),
  N_ASSIG = c( 5,  6,  4,  5,  6,  3,  5,  6,  4,  5),
  NOTA    = c( 3,  4,  2,  3,  4,  2,  5,  3,  2,  4),
  T_AVAL  = c("Continuada","Única","Continuada","Continuada","Continuada",
               "Única","Continuada","Continuada","Única","Continuada"),
  CURS    = c("1r","1r","3r","2n","1r","4t","1r","1r","2n","3r"),
  GENERE  = c("Home","Dona","Dona","Home","Dona","Home","Home","Dona","Dona","Home"),
  GRAU    = c("ADE","Emp.Int","Economia","Doble ADE+Dret","Sociologia",
               "ADE","Estadística","ADE","Economia","Emp.Int"),
  DEDIC   = c("E.Complet","E.Complet","T.Parcial",
               "E.Complet","E.Complet","T.Complet",
               "E.Complet","T.Ocasional","T.Parcial","E.Complet"),
  IA_HABIT = c( 4,  5,  3,  4,  5,  2,  4,  3,  2,  5),
  IA_COMPR = c( 3,  5,  2,  4,  4,  2,  5,  3,  2,  5),
  IA_REND  = c( 3,  4,  3,  4,  5,  2,  4,  3,  1,  5),
  IA_PDFS  = c( 2,  3,  2,  4,  3,  1,  3,  2,  1,  4),
  IA_SUBST = c( 2,  1,  3,  2,  2,  4,  1,  3,  5,  1),
  IA_ATENC = c( 4,  2,  5,  3,  3,  6,  2,  4,  6,  2),
  IA_CONF  = c( 3,  2,  4,  3,  2,  5,  1,  3,  6,  2),
  stringsAsFactors = FALSE
)

write.csv(nou, "shiny_absentisme/test_alumnat_nou.csv", row.names = FALSE)
cat("Generat: shiny_absentisme/test_alumnat_nou.csv  (", nrow(nou), "alumnes)\n")
print(nou)

# ── 2. Alumnat ANTIC ───────────────────────────────────────────────────────
# El Shiny requereix DOS blocs al mateix CSV:
#   a) vars_nou_raw  → validació obligatòria (mateixes columnes raw que alumnat nou)
#   b) factor scores EFA + binàries RF-A → perquè el model pugui predir
# (equivalent a exportar totes les columnes de la plantilla_alumnat_antic.xlsx)

antic <- data.frame(
  # ── Alumnes 1-10: perfil REGULAR ─────────────────────────────
  # ── Alumnes 11-20: perfil IRREGULAR ──────────────────────────

  # Bloc raw (vars_nou_raw)
  EDAT    = c(19, 20, 21, 22, 20, 19, 23, 21, 20, 22,
              24, 27, 23, 26, 28, 25, 24, 23, 26, 25),
  DESPL   = c(10, 15,  5, 20,  8, 12, 25, 18, 10, 15,
              60, 80, 55, 75, 90, 50, 65, 45, 70, 60),
  N_ASSIG = c( 6,  5,  6,  4,  6,  5,  4,  5,  6,  5,
               5,  6,  5,  4,  6,  5,  6,  5,  4,  5),
  NOTA    = c( 5,  4,  4,  3,  5,  4,  4,  3,  5,  4,
               2,  1,  2,  1,  1,  2,  1,  2,  1,  2),
  T_AVAL  = c(rep("Continuada", 10), rep("Única", 10)),
  CURS    = c("1r","2n","1r","3r","1r","2n","2n","1r","3r","2n",
              "3r","4t","2n","3r","4t","3r","4t","2n","3r","2n"),
  GENERE  = c("Dona","Home","Dona","Home","Dona","Dona","Home","Dona","Home","Dona",
              "Home","Dona","Home","Dona","Home","Dona","Home","Dona","Home","Dona"),
  GRAU    = c("ADE","Economia","ADE","Sociologia","Doble ADE+Dret",
               "ADE","Economia","ADE","Estadística","Economia",
               "ADE","Economia","ADE","Economia","Sociologia",
               "ADE","Economia","Emp.Int","ADE","Economia"),
  DEDIC   = c("E.Complet","E.Complet","E.Complet","T.Ocasional","E.Complet",
               "E.Complet","T.Ocasional","E.Complet","E.Complet","T.Ocasional",
               "T.Parcial","T.Complet","T.Parcial","T.Complet","T.Parcial",
               "T.Complet","T.Parcial","T.Parcial","T.Complet","T.Parcial"),
  # IA raw — Regular: usa IA per aprendre (habit/compr/rend alts, subst baix)
  #         Irregular: IA com a substitut de classe (subst/atenc/conf alts)
  IA_HABIT = c(5, 4, 5, 4, 5, 4, 5, 4, 5, 4,  2, 2, 3, 2, 2, 3, 2, 3, 2, 2),
  IA_COMPR = c(5, 4, 5, 4, 5, 5, 4, 5, 4, 5,  2, 1, 2, 2, 1, 2, 1, 2, 2, 2),
  IA_REND  = c(4, 4, 5, 4, 5, 4, 4, 5, 4, 4,  2, 2, 2, 1, 2, 2, 2, 2, 1, 2),
  IA_PDFS  = c(4, 3, 4, 4, 4, 3, 4, 4, 3, 4,  2, 1, 2, 2, 1, 2, 1, 2, 2, 2),
  IA_SUBST = c(1, 2, 1, 2, 1, 1, 2, 1, 2, 1,  6, 5, 5, 6, 6, 5, 5, 6, 6, 5),
  IA_ATENC = c(2, 2, 1, 2, 1, 2, 2, 1, 2, 2,  6, 5, 5, 6, 6, 5, 6, 5, 6, 5),
  IA_CONF  = c(1, 2, 1, 2, 1, 1, 2, 1, 2, 1,  5, 5, 4, 6, 5, 5, 5, 4, 5, 4),

  # Bloc computat — scores EFA (mitjana ponderada Likert, calculats de la plantilla)
  # Regular: MOT_DESM baix, MOT_AUTO alt, MOT_FM baix, EST_* alts, IA_EINA alt
  # Irregular: MOT_DESM alt, MOT_AUTO baix, MOT_FM alt, EST_* baixos, IA_EINA baix
  MOT_DESMOTIVACIO  = c(1.8, 2.1, 1.6, 2.4, 1.5, 1.9, 2.3, 1.7, 2.0, 2.2,
                        5.3, 5.6, 5.1, 5.5, 5.7, 5.2, 5.4, 5.0, 5.6, 5.2),
  MOT_AUTOGESTIO    = c(4.9, 4.6, 5.1, 4.4, 5.2, 4.8, 4.5, 5.0, 4.7, 4.6,
                        1.7, 1.5, 1.9, 1.6, 1.4, 1.8, 1.6, 2.0, 1.5, 1.8),
  MOT_FORCA_MAJOR   = c(1.3, 1.5, 1.2, 1.8, 1.2, 1.4, 1.7, 1.3, 1.5, 1.6,
                        4.8, 5.3, 4.5, 5.0, 5.5, 4.7, 5.1, 4.4, 5.2, 4.6),
  EST_QUALITAT_DOC  = c(4.9, 4.5, 5.0, 4.3, 4.8, 4.6, 4.4, 5.0, 4.5, 4.6,
                        1.7, 1.5, 1.9, 1.6, 1.4, 1.8, 1.6, 2.0, 1.5, 1.8),
  EST_AVALUACIO_AC  = c(4.8, 4.4, 4.9, 4.2, 4.7, 4.5, 4.3, 4.8, 4.4, 4.5,
                        1.8, 1.6, 2.0, 1.7, 1.5, 1.9, 1.7, 2.1, 1.6, 1.9),
  EST_TEMPS_CLASSE  = c(5.0, 4.6, 5.1, 4.4, 4.9, 4.7, 4.5, 5.0, 4.6, 4.7,
                        1.9, 1.7, 2.1, 1.8, 1.6, 2.0, 1.8, 2.2, 1.7, 2.0),
  EST_GRUPS_REDUITS = c(4.8, 4.5, 5.0, 4.3, 4.9, 4.7, 4.4, 4.9, 4.5, 4.6,
                        2.0, 1.8, 2.2, 1.9, 1.7, 2.1, 1.9, 2.3, 1.8, 2.1),
  IA_EINA_ESTUDI    = c(4.8, 4.5, 4.9, 4.4, 4.9, 4.6, 4.7, 4.8, 4.5, 4.6,
                        2.1, 1.8, 2.3, 2.0, 1.7, 2.2, 1.9, 2.4, 1.8, 2.1),

  # Binàries RF-A (noms exactes que usa vars_rfa)
  IA_SUBST_num  = c(rep(0, 10), rep(1, 10)),         # IA_SUBST >= 4
  T_AVAL_num    = c(rep(1, 10), rep(0, 10)),          # Continuada=1
  CURS_1R_num   = c(1,0,1,0,1,0,0,1,0,0, rep(0,10)), # 1r=1
  NOTA_num      = c(5,4,4,3,5,4,4,3,5,4, 2,1,2,1,1,2,1,2,1,2),
  DOBLE_GRAU_EST = c(0,0,0,0,1,0,0,0,1,0, rep(0,10)), # Doble ADE+Dret, Estadística → 1
  TREB_INTENS   = c(rep(0, 10), rep(1, 10)),          # T.Parcial/T.Complet=1
  stringsAsFactors = FALSE
)

write.csv(antic, "shiny_absentisme/test_alumnat_antic.csv", row.names = FALSE)
cat("Generat: shiny_absentisme/test_alumnat_antic.csv  (", nrow(antic), "alumnes)\n")
print(antic)

cat("\nPer testejar el Shiny:\n")
cat("  Pestanya 'Alumnat nou'  → puja test_alumnat_nou.csv\n")
cat("  Pestanya 'Alumnat antic' → puja test_alumnat_antic.csv\n")
