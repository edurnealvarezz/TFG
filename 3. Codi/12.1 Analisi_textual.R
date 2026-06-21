packages <- c("tidytext", "wordcloud", "dplyr", "stringr", "ggplot2",
              "tibble", "tidyr", "RColorBrewer", "stopwords", "igraph", "ggraph",
              "ggrepel", "topicmodels", "tm", "syuzhet", "cld2", "httr")
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}
lapply(packages, install_if_missing)
rm(packages)

setwd("C:/Users/edurn/Downloads/TFG")
#setwd("C:/Users/Edurne/Downloads/TFG")

load("2. Dades/11. Dades SVM.RData")
source("3. Codi/12.0 Funcions textual.R")

sink("4. Outputs/12. Analisi textual/12.1 Output_text_textual.txt")
png("4. Outputs/12. Analisi textual/grafic_%02d.png", width = 8, height = 6, units = "in", res = 300)

on.exit({
  if (dev.cur() > 1) dev.off()
  if (sink.number() > 0) sink()
}, add = TRUE)

#### ============================================================ ####
####                   0. PREPARACIÓ DE DADES                     ####
#### ============================================================ ####

cat("========== 0. PREPARACIÓ ==========\n\n")

# Stopwords: català + castellà
sw_cat <- tryCatch(
  stopwords::stopwords("ca", source = "stopwords-iso"),
  error = function(e) character(0)
)
sw_es <- tryCatch(
  stopwords::stopwords("es", source = "stopwords-iso"),
  error = function(e) character(0)
)

sw_cat_extra <- c(
  "i", "de", "que", "el", "la", "les", "els", "una", "un", "en", "a",
  "per", "però", "com", "amb", "ha", "he", "han", "és", "no", "sí",
  "molt", "més", "quan", "si", "tot", "tots", "totes", "sense", "cada",
  "fins", "sobre", "entre", "altre", "altres", "això", "aquí", "ja",
  "també", "tan", "tant", "ara", "sempre", "mai", "hi", "ho", "se",
  "me", "te", "ne", "li", "m", "l", "d", "s", "n",
  "crec", "penso", "podria", "seria", "hauria", "voldria", "fare",
  "ens", "us", "vos", "seu", "seus", "seva", "seves", "meu", "meus",
  "meva", "teu", "teus", "teva", "nostre", "nostres", "vostre",
  "al", "del", "pel", "cal", "fa", "fer", "fet", "feta", "fetes", "fets",
  "o", "ni", "doncs", "perquè", "perque", "perqué", "ja", "be", "bé", "tenir",
  "tinc", "té", "tenim", "teniu", "tenen", "nomes", "només", "sols", "solament"
)

sw_es_extra <- c(
  "y", "de", "que", "el", "la", "los", "las", "una", "un", "en", "a",
  "por", "pero", "como", "con", "ha", "he", "han", "es", "no", "si",
  "muy", "más", "cuando", "todo", "todos", "todas", "sin", "cada",
  "hasta", "sobre", "entre", "otro", "otros", "esto", "aquí", "ya",
  "también", "tan", "tanto", "ahora", "siempre", "nunca", "se", "me",
  "te", "nos", "os", "le", "les", "mi", "tu", "su", "nuestro", "vuestro",
  "al", "del", "creo", "pienso", "podría", "sería", "habría", "quería",
  "hacer", "hecho", "tener", "tengo", "tiene", "tenemos", "tienen",
  "o", "ni", "pues", "porque", "ya", "bien", "ser", "estar", "soy",
  "fue", "era", "son", "están", "hay"
)

sw_all <- unique(c(sw_cat, sw_es, sw_cat_extra, sw_es_extra))
cat(sprintf("Total stopwords carregades (cat+es): %d\n\n", length(sw_all)))

# Diccionari de normalització lingüística (lemmatització manual)
# per unificar variants perquè després sortien totes com a paraules diferents

lemma_dict <- c(
  "classe" = "clase", "classes" = "clase", "clases" = "clase",
  "professor" = "profesor", "professors" = "profesor",
  "professora" = "profesor", "professores" = "profesor",
  "profesora" = "profesor", "profesoras" = "profesor", "profesores" = "profesor",
  "assignatura" = "asignatura", "assignatures" = "asignatura",
  "asignaturas" = "asignatura",
   "pràctica" = "practica", "pràctiques" = "practica",
  "práctica" = "practica", "prácticas" = "practica", "practiques" = "practica",
  "examens" = "examen", "exàmen" = "examen", "exàmens" = "examen",
  "examenes" = "examen", "exámenes" = "examen",
  "estudiant" = "estudiante", "estudiants" = "estudiante",
  "estudiantes" = "estudiante",
  "contingut" = "contenido", "continguts" = "contenido",
  "contenidos" = "contenido",
  "teoría" = "teoria", "teorías" = "teoria", "teories" = "teoria",
  "motivació" = "motivacion", "motivacio" = "motivacion",
  "motivaciones" = "motivacion",
  "aprenentatge" = "aprendizaje", "aprendizajes" = "aprendizaje",
  "dinàmica" = "dinamica", "dinàmiques" = "dinamica",
  "dinamiques" = "dinamica", "dinámica" = "dinamica", "dinámicas" = "dinamica",
  "activitat" = "actividad", "activitats" = "actividad",
  "actividades" = "actividad",
  "matèria" = "materia", "matèries" = "materia", "materias" = "materia",
  "treball" = "trabajo", "treballs" = "trabajo", "trabajos" = "trabajo",
  "grup" = "grupo", "grups" = "grupo", "grupos" = "grupo",
  "notes" = "nota", "notas" = "nota",
  "horari" = "horario", "horaris" = "horario", "horarios" = "horario",
  "interessant" = "interesante", "interesantes" = "interesante",
  "metodologies" = "metodologia", "metodologias" = "metodologia",
  "metodologías" = "metodologia",
  "participació" = "participacion", "participacio" = "participacion",
  "participaciones" = "participacion",
  "avaluació" = "evaluacion", "avaluacio" = "evaluacion",
  "evaluación" = "evaluacion", "evaluaciones" = "evaluacion",
  "teoriques" = "teoricas", "teòriques" = "teoricas", "teòrica" = "teorica",
  "contabilitat" = "contabilidad", "comptabilitat" = "contabilidad"
)

cat(sprintf("Diccionari de lemmatització: %d variants normalitzades\n\n",
            length(lemma_dict)))

# Extreure les 3 variables textuals
vars_text <- c("EXP_POS", "EXP_NEG", "PROP_MOT")
titols <- c(
  EXP_POS = "Experiència positiva (EXP_POS)",
  EXP_NEG = "Experiència negativa (EXP_NEG)",
  PROP_MOT = "Propostes de motivació (PROP_MOT)"
)
preguntes <- c(
  EXP_POS = "Experiència d'una assignatura que t'hagi interessat/agradat",
  EXP_NEG = "Experiència d'una assignatura que NO t'hagi interessat/agradat",
  PROP_MOT = "Proposta que et motivaria a assistir a classe"
)

# Resum d'estadístiques de les respostes obertes
cat("Resum de respostes obertes:\n\n")
for (v in vars_text) {
  vals <- dades_def[[v]]
  vals_net <- vals[!is.na(vals) & str_squish(vals) != ""]
  cat(sprintf("%-12s | N respostes: %3d | N missing: %3d | Mitjana paraules: %.1f\n",
              v,
              length(vals_net),
              sum(is.na(vals) | str_squish(vals) == ""),
              mean(str_count(vals_net, "\\S+"), na.rm = TRUE)))
}
cat("\n")

# traducció cat -> es
for (v in vars_text) {
  dades_def[[v]] <- vapply(dades_def[[v]], normalitzar_a_es,
                           character(1), USE.NAMES = FALSE)
  cat(sprintf("  %s: completat\n", v))
}
cat("\n")


#### ============================================================ ####
####           1. EXP_POS — Experiències positives                ####
#### ============================================================ ####

cat("\n========== 1. EXP_POS — Experiències positives ==========\n\n")
cat("Pregunta: ", preguntes["EXP_POS"], "\n\n")

tok_pos <- tokenitzar("EXP_POS", dades_def)

cat("Paraules úniques (sense stopwords, lemmatitzades):", n_distinct(tok_pos$word), "\n")
cat("Total ocurrències:", nrow(tok_pos), "\n\n")

cat("Top 30 paraules més freqüents (EXP_POS):\n")
freq_pos <- tok_pos %>% count(word, sort = TRUE) %>% slice_head(n = 30)
print(freq_pos, n = 30)

plot_freq(tok_pos, titols["EXP_POS"], color_fill = "#1A5276", top_n = 20)
plot_wordcloud(tok_pos, titols["EXP_POS"], paleta = "Blues")

cat("\nTop bigrams (EXP_POS):\n")
bi_pos <- calc_bigrams("EXP_POS", dades_def)
print(bi_pos)
plot_bigrams(bi_pos, titols["EXP_POS"], color_fill = "#1A5276")

cat("\nTopics LDA (EXP_POS, k=3):\n")
analisi_lda(tok_pos, "EXP_POS", k = 3,
            etiquetes = c("Docència participativa",
                          "Qualitat explicativa del professor",
                          "Metodologia i avaluació"))

cat("\nComparativa per grup d'assistència (EXP_POS):\n")
tok_pos_grup <- tokenitzar_grup("EXP_POS", dades_def)
comparativa_subgrup(tok_pos_grup, "GRUP_ASSIST", titols["EXP_POS"],
                    "grup d'assistència",
                    colors = c("#1A5276", "#922B21"))

#### ============================================================ ####
####           2. EXP_NEG — Experiències negatives                ####
#### ============================================================ ####

cat("\n========== 2. EXP_NEG — Experiències negatives ==========\n\n")
cat("Pregunta: ", preguntes["EXP_NEG"], "\n\n")

tok_neg <- tokenitzar("EXP_NEG", dades_def)

cat("Paraules úniques (sense stopwords, lemmatitzades):", n_distinct(tok_neg$word), "\n")
cat("Total ocurrències:", nrow(tok_neg), "\n\n")

cat("Top 30 paraules més freqüents (EXP_NEG):\n")
freq_neg <- tok_neg %>% count(word, sort = TRUE) %>% slice_head(n = 30)
print(freq_neg, n = 30)

plot_freq(tok_neg, titols["EXP_NEG"], color_fill = "#922B21", top_n = 20)
plot_wordcloud(tok_neg, titols["EXP_NEG"], paleta = "Reds")

cat("\nTop bigrams (EXP_NEG):\n")
bi_neg <- calc_bigrams("EXP_NEG", dades_def)
print(bi_neg)
plot_bigrams(bi_neg, titols["EXP_NEG"], color_fill = "#922B21")

cat("\nTopics LDA (EXP_NEG, k=3):\n")
analisi_lda(tok_neg, "EXP_NEG", k = 3,
            etiquetes = c("Manca de rellevancia percebuda",
                          "Passivitat i falta de practica",
                          "Mala comunicacio docent"))

cat("\nComparativa per grup d'assistència (EXP_NEG):\n")
tok_neg_grup <- tokenitzar_grup("EXP_NEG", dades_def)
comparativa_subgrup(tok_neg_grup, "GRUP_ASSIST", titols["EXP_NEG"],
                    "grup d'assistència",
                    colors = c("#1A5276", "#922B21"))

#### ============================================================ ####
####           3. PROP_MOT — Propostes de motivació               ####
#### ============================================================ ####

cat("\n========== 3. PROP_MOT — Propostes de motivació ==========\n\n")
cat("Pregunta: ", preguntes["PROP_MOT"], "\n\n")

tok_mot <- tokenitzar("PROP_MOT", dades_def)

cat("Paraules úniques (sense stopwords, lemmatitzades):", n_distinct(tok_mot$word), "\n")
cat("Total ocurrències:", nrow(tok_mot), "\n\n")

cat("Top 30 paraules més freqüents (PROP_MOT):\n")
freq_mot <- tok_mot %>% count(word, sort = TRUE) %>% slice_head(n = 30)
print(freq_mot, n = 30)

plot_freq(tok_mot, titols["PROP_MOT"], color_fill = "#1E8449", top_n = 20)
plot_wordcloud(tok_mot, titols["PROP_MOT"], paleta = "Greens")

cat("\nTop bigrams (PROP_MOT):\n")
bi_mot <- calc_bigrams("PROP_MOT", dades_def)
print(bi_mot)
plot_bigrams(bi_mot, titols["PROP_MOT"], color_fill = "#1E8449")

cat("\nTopics LDA (PROP_MOT, k=3):\n")
analisi_lda(tok_mot, "PROP_MOT", k = 3,
            etiquetes = c("Avaluacio continua com a incentiu",
                          "Classes mes dinamiques i practiques",
                          "Mes pes de l'avaluacio continuada"))

cat("\nComparativa per grup d'assistència (PROP_MOT):\n")
tok_mot_grup <- tokenitzar_grup("PROP_MOT", dades_def)
comparativa_subgrup(tok_mot_grup, "GRUP_ASSIST", titols["PROP_MOT"],
                    "grup d'assistència",
                    colors = c("#1A5276", "#922B21"))

#### ============================================================ ####
####         4. COMPARATIVA EXP_POS vs EXP_NEG                   ####
#### ============================================================ ####

cat("\n========== 4. COMPARATIVA EXP_POS vs EXP_NEG ==========\n\n")

freq_comp <- bind_rows(
  tok_pos %>% count(word) %>% mutate(variable = "EXP_POS"),
  tok_neg %>% count(word) %>% mutate(variable = "EXP_NEG")
) %>%
  group_by(variable) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

log_ratio <- freq_comp %>%
  select(word, variable, prop) %>%
  pivot_wider(names_from = variable, values_from = prop, values_fill = 0.0001) %>%
  mutate(log_ratio = log2(EXP_POS / EXP_NEG)) %>%
  filter(EXP_POS > 0.0001 | EXP_NEG > 0.0001) %>%
  arrange(desc(abs(log_ratio)))

cat("Paraules més diferenciadores entre experiències positives i negatives:\n\n")
cat(">> Més característiques d'EXP_POS (log_ratio positiu):\n")
print(log_ratio %>% filter(log_ratio > 0) %>% slice_head(n = 15) %>%
        select(word, EXP_POS, EXP_NEG, log_ratio) %>%
        mutate(across(c(EXP_POS, EXP_NEG), ~ round(. * 1000, 2)),
               log_ratio = round(log_ratio, 2)), n = 15)

cat("\n>> Més característiques d'EXP_NEG (log_ratio negatiu):\n")
print(log_ratio %>% filter(log_ratio < 0) %>% slice_head(n = 15) %>%
        select(word, EXP_POS, EXP_NEG, log_ratio) %>%
        mutate(across(c(EXP_POS, EXP_NEG), ~ round(. * 1000, 2)),
               log_ratio = round(log_ratio, 2)), n = 15)

top_diff <- log_ratio %>%
  filter(EXP_POS > 0.0002 | EXP_NEG > 0.0002) %>%
  slice_head(n = 30) %>%
  mutate(
    word = reorder(word, log_ratio),
    grup = ifelse(log_ratio > 0, "Més a EXP_POS", "Més a EXP_NEG")
  )

print(
  ggplot(top_diff, aes(x = log_ratio, y = word, fill = grup)) +
    geom_col(alpha = 0.85) +
    geom_text(aes(label = round(log_ratio, 2),
                  hjust = ifelse(log_ratio >= 0, -0.1, 1.1)),
              size = 3.5) +
    geom_vline(xintercept = 0, color = "grey30") +
    scale_fill_manual(values = c("Més a EXP_POS" = "#1A5276",
                                 "Més a EXP_NEG" = "#922B21")) +
    scale_x_continuous(expand = expansion(mult = 0.18)) +
    labs(title = "Paraules diferenciadores: EXP_POS vs EXP_NEG",
         subtitle = "Log2-ratio de freqüències relatives (positiu = EXP_POS, negatiu = EXP_NEG)",
         x = "Log2-ratio", y = NULL, fill = NULL) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"),
          legend.position = "top",
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          legend.text = element_text(size = 12))
)

#### ============================================================ ####
####              5. ANÀLISI DE SENTIMENT (NRC)                   ####
#### ============================================================ ####

cat("\n========== 5. ANÀLISI DE SENTIMENT ==========\n\n")
cat("Mètode: NRC Emotion Lexicon (syuzhet, language='spanish')\n")
cat("Nota: cobertura parcial del català; les paraules sense traducció al\n")
cat("lexicó espanyol no puntuen. El biaix és consistent entre grups, per\n")
cat("tant les comparacions relatives segueixen sent vàlides.\n\n")

sent_pos <- score_sentiment("EXP_POS", titols["EXP_POS"])
sent_neg <- score_sentiment("EXP_NEG", titols["EXP_NEG"])
sent_mot <- score_sentiment("PROP_MOT", titols["PROP_MOT"])

#### ============================================================ ####
####         6. ANÀLISI PER SUBGRUPS                              ####
#### ============================================================ ####

cat("\n========== 6. ANÀLISI PER SUBGRUPS ==========\n\n")
cat("Preguntes: parlen diferent els estudiants que treballen vs. els que no?\n")
cat("Les propostes de primer curs difereixen de la resta?\n\n")

# --------- 6.1 PROP_MOT per situació laboral (TREB_INT) ---------
cat("--- 6.1 PROP_MOT per situació laboral (TREB_INT) ---\n")
cat("TREB_INTENS: 0=No treballa o treballa ocasionalment | 1=Treballa\n")


if ("TREB_INTENS" %in% names(tok_mot_grup)) {
  # Distribució de respostes per nivell
  n_dedic <- tok_mot_grup %>%
    distinct(id, TREB_INTENS) %>%
    filter(!is.na(TREB_INTENS)) %>%
    count(TREB_INTENS) %>%
    mutate(etiqueta = recode(as.character(TREB_INTENS),
    "0" = "No treballa", "1" = "Treballa"))
  cat("Distribució de respostes per TREB_INTENS:\n")
  print(n_dedic)
  cat("\n")

  # Comparativa binària: no treballa (1) vs. treballa (2+3+4)
  # Agrupa 2/3/4 perquè la distinció rellevant és tenir activitat laboral
  tok_mot_dedic <- tok_mot_grup %>%
    filter(!is.na(TREB_INTENS)) %>%
    mutate(DEDIC_BIN = ifelse(TREB_INTENS == 1, "No treballa", "Treballa"))

  cat("Comparativa binària: No treballa (TREB_INTENS=0) vs. Treballa (TREB_INTENS=1)\n")
  comparativa_subgrup(tok_mot_dedic, "DEDIC_BIN",
                      titols["PROP_MOT"], "situació laboral",
                      colors = c("#E67E22", "#2ECC71"))
} else {
  cat("  Variable TREB_INTENS no disponible al dataset\n")
}

# --------- 6.2 PROP_MOT per CURS_1R ---------
cat("\n--- 6.2 PROP_MOT per curs (CURS_1R: 1r curs vs. resta) ---\n")
comparativa_subgrup(tok_mot_grup, "CURS_1R",
                    titols["PROP_MOT"], "curs",
                    colors = c("#8E44AD", "#27AE60"))

# --------- 6.3 EXP_NEG per GRUP_ASSIST (lectura narrativa) ---------
# Quins aspectes negatius mencionen els estudiants amb baixa assistència?
cat("\n--- 6.3 EXP_NEG per grup d'assistència (lectura narrativa) ---\n")
cat("(Quin vocabulari diferencia els estudiants que falten dels que assisteixen?)\n")
comparativa_subgrup(tok_neg_grup, "GRUP_ASSIST",
                    titols["EXP_NEG"], "grup d'assistència",
                    colors = c("#1A5276", "#922B21"))

# --------- 6.4 PROP_MOT per CURS (si hi ha la variable categòrica completa) ---------
if ("CURS" %in% names(tok_mot_grup)) {
  cat("\n--- 6.4 Distribució PROP_MOT per curs (freqüències) ---\n")
  n_per_curs <- tok_mot_grup %>%
    distinct(id, CURS) %>%
    count(CURS) %>%
    arrange(CURS)
  cat("N de respostes obertes per curs:\n")
  print(n_per_curs)

  # Top paraules per curs (si hi ha prou dades per almenys 2 cursos)
  cursos_n <- n_per_curs %>% filter(n >= 10) %>% pull(CURS)
  if (length(cursos_n) >= 2) {
    freq_curs <- tok_mot_grup %>%
      filter(CURS %in% cursos_n) %>%
      group_by(CURS) %>%
      count(word) %>%
      mutate(prop = n / sum(n)) %>%
      slice_max(prop, n = 8, with_ties = FALSE) %>%
      ungroup() %>%
      mutate(word = reorder_within(word, prop, CURS))

    print(
      ggplot(freq_curs, aes(prop, word, fill = factor(CURS))) +
        geom_col(show.legend = FALSE, alpha = 0.85) +
        geom_text(aes(label = round(prop, 3)), hjust = -0.1, size = 3) +
        facet_wrap(~ paste("Curs", CURS), scales = "free_y") +
        scale_y_reordered() +
        scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
        labs(title = "Top paraules PROP_MOT per curs",
             subtitle = "Proporció relativa dins cada curs (n ≥ 10 respostes)",
             x = "Proporció", y = NULL) +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold"),
              strip.text = element_text(face = "bold"),
              axis.text.y = element_text(size = 12),
              axis.text.x = element_text(size = 12),
              legend.text = element_text(size = 12))
    )
  }
}

#### ============================================================ ####
####         7. WORD CLOUD COMBINAT (les 3 variables)             ####
#### ============================================================ ####

cat("\n========== 7. WORD CLOUD COMBINAT ==========\n\n")

tok_tot <- bind_rows(tok_pos, tok_neg, tok_mot)
freq_tot <- tok_tot %>% count(word, sort = TRUE)

cat("Total paraules úniques (totes 3 variables):", nrow(freq_tot), "\n")
cat("Top 30 paraules globals:\n")
print(slice_head(freq_tot, n = 30))

set.seed(1234)
colors_comb <- brewer.pal(8, "Set2")
tryCatch(
  wordcloud(
    words = freq_tot$word, freq = freq_tot$n,
    min.freq = 2, max.words = 100,
    random.order = FALSE, rot.per = 0.15,
    colors = colors_comb, scale = c(5, 0.6)
  ),
  error = function(e) cat(sprintf("  wordcloud combinat error: %s\n", conditionMessage(e)))
)
title(main = "Word cloud combinat — EXP_POS + EXP_NEG + PROP_MOT",
      cex.main = 1.2, font.main = 2)

#### ============================================================ ####
####              8. XARXA DE CO-OCURRÈNCIA                       ####
#### ============================================================ ####

cat("\n========== 8. XARXA DE CO-OCURRÈNCIA ==========\n\n")

cat("Xarxes de co-ocurrència:\n")
cooc_net("EXP_POS",  dades_def, min_cooc = 2, color_edge = "#1A5276")
cooc_net("EXP_NEG",  dades_def, min_cooc = 2, color_edge = "#922B21")
cooc_net("PROP_MOT", dades_def, min_cooc = 2, color_edge = "#1E8449")

dev.off()
sink()
