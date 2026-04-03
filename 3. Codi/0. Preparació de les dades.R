packages <- c("dplyr", "forcats","readxl")

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

lapply(packages, install_if_missing); rm(packages)


#### ============================================================ ####
####              LECTURA DE DADES                                ####
#### ============================================================ ####

#setwd("C:/Users/edurn/OneDrive/Escritorio/Universitat/TFG---Github")
setwd("C:/Users/Edurne/OneDrive/Escritorio/Universitat/TFG---Github")

dades <- read_excel("2. Dades/ Anàlisi de l'assistència a les aules.xlsx")
dades <- dades %>%
  select(-contains("Puntos"), -contains("Comentarios"))

#### ============================================================ ####
####          RENOMBRAR COLUMNES                                  ####
#### ============================================================ ####

names(dades) <- c(
  # Secció 1
  "GRAU", "CURS", "N_ASSIG", "NOTA", "T_AVAL", "P_ASSIST",
  # Secció 2
  "GENERE", "EDAT", "DEDIC", "DESPL",
  # Secció 3 – Motius de NO assistència
  "M_TREB", "M_FAM", "M_SALUT", "M_DIST", "M_AUTON", "M_CV",
  "M_EXAM", "M_UTIL", "M_AVORR", "M_PASSIU", "M_TEOR", "M_PROF",
  "M_REPET", "M_ACAD", "M_AMICS",
  # Secció 3 – Estratègies d'assistència
  "E_PES_AC", "E_PART", "E_DINAM", "E_REDU", "E_CURT", "E_DESC",
  "E_CLIMA", "E_EXPL", "E_RITME", "E_ACT_AC", "E_PROP", "E_HORA", "E_PES_AS",
  # Secció 3 – Preguntes obertes
  "EXP_POS", "EXP_NEG", "PROP_MOT",
  # Secció 4 - IA
  "IA_HABIT", "IA_COMPR", "IA_SUBST", "IA_CONF",
  "IA_ATENC", "IA_PREOC", "IA_REND", "IA_PDFS"
)


dades$GRAU <- chartr("\u2019", "'", dades$GRAU) # per normalitzar apostrof
dades$GRAU <- gsub("[\u2018\u2019\u201A\u201B]", "'", dades$GRAU)
dades$GRAU <- gsub("\u00A0", " ", dades$GRAU)

#### ============================================================ ####
####             TRANSFORMACIÓ VARIABLES                          ####
#### ============================================================ ####

# --- SECCIÓ 1: Dades acadèmiques ---

dades$GRAU <- factor(dades$GRAU,
                     levels = c(
                       "Administració i Direcció d'Empreses",
                       "Economia",
                       "Empresa Internacional",
                       "Estadística",
                       "Sociologia",
                       "Doble grau: Economia + Estadística",
                       "Doble Grau: ADE + Dret",
                       "Doble Grau: ADE + Matemàtiques",
                       "Doble Grau: ADE + Química",
                       "Doble Grau: ADE + Sociologia"
                     ),
                     labels = c("ADE", "Economia", "Emp.Int", "Estadística", "Sociologia",
                                "Doble Eco+Est", "Doble ADE+Dret", "Doble ADE+Mat",
                                "Doble ADE+Qui", "Doble ADE+Soc")
)


dades$CURS <- factor(dades$CURS,
                     levels = c("1r", "2n", "3r", "4t", "5è", "A partir de 6è"),
                     labels = c("1r", "2n", "3r", "4t", "5è", "6è+"),
                     ordered = TRUE
)

dades$N_ASSIG <- as.integer(dades$N_ASSIG)

dades$NOTA <- factor(dades$NOTA,
                     levels = c("Entre 5 i 5,9", "Entre 6 i 6,9", "Entre 7 i 7,9",
                                "Entre 8 i 8,9", "Igual o superior a 9"),
                     labels = c("[5-5.9]", "[6-6.9]", "[7-7.9]", "[8-8.9]", "≥9"),
                     ordered = TRUE
)

dades$T_AVAL <- factor(dades$T_AVAL,
                       levels = c("Avaluació Unica", "Avaluació Continuada"),
                       labels = c("Única", "Continuada")
)

dades$P_ASSIST <- as.numeric(dades$P_ASSIST)

# --- SECCIÓ 2: Situació personal ---

dades$GENERE <- factor(dades$GENERE,
                       levels = c("Dona", "Home", "No binari", "Prefereixo no respondre")
)


dades$EDAT <- as.integer(dades$EDAT)


dades$DEDIC <- factor(dades$DEDIC,
                      levels = c(
                        "Estudio a temps complet (només estudio).",
                        "Estudio però també treballo ocasionalment",
                        "Estudio i treballo a temps parcial (menys de 35 hores setmanals)",
                        "Treballo a temps complet (més de 35 hores setmanals) i estudio"
                      ),
                      labels = c("E.Complet", "T.Ocasional", "T.Parcial", "T.Complet"),
                      ordered = TRUE
)

# DESPL: numèrica contínua (minuts)
dades$DESPL <- as.numeric(dades$DESPL)

# --- SECCIÓ 3: Opinió personal ---

extreu_likert <- function(x, max_nivell) {
  num <- as.integer(gsub("^(\\d+).*", "\\1", trimws(x)))
  factor(num, levels = 1:max_nivell, ordered = TRUE)
} # extreu 1r num del text (per eliminar totalment d'acord o gens d'acord)

motius_vars <- c("M_TREB", "M_FAM", "M_SALUT", "M_DIST", "M_AUTON", "M_CV",
                 "M_EXAM", "M_UTIL", "M_AVORR", "M_PASSIU", "M_TEOR", "M_PROF",
                 "M_REPET", "M_ACAD", "M_AMICS")

dades[motius_vars] <- lapply(dades[motius_vars], extreu_likert, max_nivell = 5)

estrategies_vars <- c("E_PES_AC", "E_PART", "E_DINAM", "E_REDU", "E_CURT", "E_DESC",
                      "E_CLIMA", "E_EXPL", "E_RITME", "E_ACT_AC", "E_PROP",
                      "E_HORA", "E_PES_AS")

dades[estrategies_vars] <- lapply(dades[estrategies_vars], extreu_likert, max_nivell = 6)


# --- SECCIÓ 3: Preguntes obertes (text) ---
dades$EXP_POS  <- as.character(dades$EXP_POS)
dades$EXP_NEG  <- as.character(dades$EXP_NEG)
dades$PROP_MOT <- as.character(dades$PROP_MOT)

# --- SECCIÓ 4: Ús de la IA (Likert 1-6) ---

ia_vars <- c("IA_HABIT", "IA_COMPR", "IA_SUBST", "IA_CONF",
             "IA_ATENC", "IA_PREOC", "IA_REND", "IA_PDFS")

dades[ia_vars] <- lapply(dades[ia_vars], extreu_likert, max_nivell = 6)

#### ============================================================ ####
####                        VALIDACIÓ                             ####
#### ============================================================ ####

str(dades)
summary(dades)

# Possibles errors
dades %>% filter(EDAT > 30) %>% select(EDAT, GRAU, CURS)
# hi ha algú de 82 anys que acaba de començar ade + dret? de totes formes
# no és outlier 

save(dades, file = "0. Dades inicials.RData")
saveRDS(estrategies_vars, "2. Dades/estrategies_vars.rds")
saveRDS(ia_vars, "2. Dades/ia_vars.rds")
saveRDS(motius_vars, "2. Dades/motius_vars.rds")
