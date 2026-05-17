packages <- c("tidytext", "wordcloud", "dplyr", "stringr", "ggplot2",
              "tibble", "tidyr", "RColorBrewer", "stopwords", "igraph", "ggraph",
              "topicmodels", "tm", "syuzhet")
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

load("2. Dades/10. Dades SVM.RData")

sink("4. Outputs/13.1 Output_text_textual.txt")
pdf("4. Outputs/13.2 Output_grafics_textual.pdf", width = 12, height = 8)

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
  "o", "ni", "doncs", "perquè", "perque", "ja", "be", "bé", "tenir",
  "tinc", "té", "tenim", "teniu", "tenen"
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

# ── Diccionari de normalització lingüística (lemmatització manual) ──────────
# Unifica variants català/castellà al mateix token canònic en català.
# Cobreix les paraules acadèmiques més freqüents en enquestes universitàries.
lemma_dict <- c(
  # classe
  "clase" = "classe", "clases" = "classe", "classes" = "classe",
  # professor
  "profesor" = "professor", "profesora" = "professor", "professors" = "professor",
  "profesores" = "professor", "profesoras" = "professor", "professora" = "professor",
  "professores" = "professor",
  # assignatura
  "asignatura" = "assignatura", "asignaturas" = "assignatura", "assignatures" = "assignatura",
  # pràctica
  "practica" = "practica", "practicas" = "practica", "practiques" = "practica",
  "pràctica" = "practica", "pràctiques" = "practica", "prácticas" = "practica",
  "práctica" = "practica",
  # examen
  "examenes" = "examen", "examens" = "examen", "exàmen" = "examen",
  "exàmens" = "examen", "exámenes" = "examen",
  # estudiant
  "estudiante" = "estudiant", "estudiantes" = "estudiant", "estudiants" = "estudiant",
  # contingut
  "contenido" = "contingut", "contenidos" = "contingut", "continguts" = "contingut",
  # teoria
  "teoría" = "teoria", "teorías" = "teoria", "teories" = "teoria",
  # motivació
  "motivacion" = "motivacio", "motivaciones" = "motivacio",
  "motivació" = "motivacio", "motivacion" = "motivacio",
  # aprenentatge
  "aprendizaje" = "aprenentatge", "aprendizajes" = "aprenentatge",
  # dinàmica
  "dinamica" = "dinamica", "dinamiques" = "dinamica", "dinámica" = "dinamica",
  "dinámicas" = "dinamica", "dinàmica" = "dinamica", "dinàmiques" = "dinamica",
  # activitat
  "activitats" = "activitat", "actividad" = "activitat", "actividades" = "activitat",
  # matèria
  "materia" = "materia", "materias" = "materia", "matèria" = "materia",
  "matèries" = "materia",
  # treball
  "trabajo" = "treball", "trabajos" = "treball", "treballs" = "treball",
  # grup
  "grupo" = "grup", "grupos" = "grup", "grups" = "grup",
  # nota
  "notas" = "nota", "notes" = "nota",
  # horari
  "horario" = "horari", "horarios" = "horari", "horaris" = "horari",
  # interessant
  "interesante" = "interessant", "interesantes" = "interessant",
  # metodologia
  "metodologia" = "metodologia", "metodologias" = "metodologia",
  "metodologies" = "metodologia", "metodologías" = "metodologia",
  # participació
  "participacion" = "participacio", "participaciones" = "participacio",
  "participació" = "participacio", "participacion" = "participacio",
  # avaluació
  "evaluacion" = "avaluacio", "evaluaciones" = "avaluacio",
  "avaluació" = "avaluacio", "evaluación" = "avaluacio"
)

lematitzar <- function(words) {
  ifelse(words %in% names(lemma_dict), lemma_dict[words], words)
}

cat(sprintf("Diccionari de lemmatització: %d variants normalitzades\n\n",
            length(lemma_dict)))

# Funció de neteja de text
clean_text <- function(x) {
  x <- tolower(x)
  x <- str_replace_all(x, "’|‘|“|”|`", " ")
  x <- str_replace_all(x, "[[:punct:]]", " ")
  x <- str_replace_all(x, "[[:digit:]]", " ")
  x <- str_squish(x)
  x
}

# Extreure les 3 variables textuals
vars_text <- c("EXP_POS", "EXP_NEG", "PROP_MOT")
titols <- c(
  EXP_POS  = "Experiència positiva (EXP_POS)",
  EXP_NEG  = "Experiència negativa (EXP_NEG)",
  PROP_MOT = "Propostes de motivació (PROP_MOT)"
)
preguntes <- c(
  EXP_POS  = "Experiència d'una assignatura que t'hagi interessat/agradat",
  EXP_NEG  = "Experiència d'una assignatura que NO t'hagi interessat/agradat",
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

# ── Funció per tokenitzar (sense metadades) ───────────────────────────────
tokenitzar <- function(var_nom, dades) {
  textos <- dades[[var_nom]]
  textos <- textos[!is.na(textos) & str_squish(textos) != ""]
  tibble(
    id = seq_along(textos),
    text = clean_text(textos)
  ) %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% sw_all, nchar(word) > 2) %>%
    mutate(word = lematitzar(word), variable = var_nom)
}

# ── Funció per tokenitzar amb metadades (GRUP_ASSIST, TREBALL, CURS...) ──
tokenitzar_grup <- function(var_nom, dades) {
  meta_cols <- intersect(c("GRUP_ASSIST", "P_ASSIST", "TREB_INTENS", "CURS", "CURS_1R"),
                         names(dades))
  dades %>%
    filter(!is.na(.data[[var_nom]]), str_squish(.data[[var_nom]]) != "") %>%
    mutate(id = row_number(), text = clean_text(.data[[var_nom]])) %>%
    select(id, text, all_of(meta_cols)) %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% sw_all, nchar(word) > 2) %>%
    mutate(word = lematitzar(word))
}

# ── Funció per fer el gràfic de freqüències ───────────────────────────────
plot_freq <- function(tokens_df, titol, color_fill, top_n = 20) {
  freq <- tokens_df %>%
    count(word, sort = TRUE) %>%
    slice_head(n = top_n) %>%
    mutate(word = reorder(word, n))
  print(
    ggplot(freq, aes(x = n, y = word)) +
      geom_col(fill = color_fill, alpha = 0.85) +
      geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
      labs(title = paste0("Top ", top_n, " paraules — ", titol),
           subtitle = "Sense stopwords, amb lemmatització cat/es",
           x = "Freqüència", y = NULL) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank())
  )
  invisible(freq)
}

# ── Funció per fer el word cloud ──────────────────────────────────────────
plot_wordcloud <- function(tokens_df, titol, paleta) {
  freq <- tokens_df %>% count(word, sort = TRUE)
  if (nrow(freq) == 0) {
    cat("  (sense paraules suficients per al word cloud)\n")
    return(invisible(NULL))
  }
  colors_wc <- brewer.pal(max(3, min(8, nrow(freq))), paleta)
  set.seed(42)
  wordcloud(
    words = freq$word, freq = freq$n,
    min.freq = 2, max.words = 80,
    random.order = FALSE, rot.per = 0.15,
    colors = colors_wc, scale = c(4.5, 0.6)
  )
  title(main = paste0("Word cloud — ", titol), cex.main = 1.2, font.main = 2)
  invisible(freq)
}

# ── Funció per calcular i graficar bigrams ────────────────────────────────
calc_bigrams <- function(var_nom, dades, top_n = 15) {
  textos <- dades[[var_nom]]
  textos <- textos[!is.na(textos) & str_squish(textos) != ""]
  tibble(text = clean_text(textos)) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    filter(!is.na(bigram)) %>%
    separate(bigram, c("w1", "w2"), sep = " ") %>%
    filter(!w1 %in% sw_all, !w2 %in% sw_all,
           nchar(w1) > 2, nchar(w2) > 2) %>%
    mutate(w1 = lematitzar(w1), w2 = lematitzar(w2)) %>%
    unite(bigram, w1, w2, sep = " ") %>%
    count(bigram, sort = TRUE) %>%
    slice_head(n = top_n)
}

plot_bigrams <- function(bigrams_df, titol, color_fill) {
  if (nrow(bigrams_df) == 0) {
    cat("  (sense bigrams amb freqüència suficient)\n")
    return(invisible(NULL))
  }
  bigrams_df <- bigrams_df %>% mutate(bigram = reorder(bigram, n))
  print(
    ggplot(bigrams_df, aes(x = n, y = bigram)) +
      geom_col(fill = color_fill, alpha = 0.80) +
      geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
      labs(title = paste0("Top bigrams — ", titol),
           subtitle = "Parelles consecutives sense stopwords (freqüències baixes: interpretar amb cautela)",
           x = "Freqüència", y = NULL) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank())
  )
  invisible(bigrams_df)
}

# ── Funció per anàlisi de topics LDA ─────────────────────────────────────
analisi_lda <- function(tokens_df, var_nom, k = 3, etiquetes = NULL) {
  dtm <- tokens_df %>%
    count(id, word) %>%
    cast_dtm(id, word, n)

  row_tots <- rowSums(as.matrix(dtm))
  dtm <- dtm[row_tots > 0, ]

  if (nrow(dtm) < k * 3) {
    cat(sprintf("  LDA %s: corpus insuficient (%d docs)\n", var_nom, nrow(dtm)))
    return(invisible(NULL))
  }

  set.seed(42)
  lda_fit <- LDA(dtm, k = k, control = list(seed = 42))

  # Etiquetes temàtiques (proporcionades externament o genèriques)
  if (!is.null(etiquetes) && length(etiquetes) == k) {
    topic_noms <- setNames(paste0("T", seq_len(k), ": ", etiquetes), seq_len(k))
  } else {
    topic_noms <- setNames(paste0("Tema ", seq_len(k)), seq_len(k))
  }

  top_terms <- tidy(lda_fit, matrix = "beta") %>%
    group_by(topic) %>%
    slice_max(beta, n = 8, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(
      topic_label = factor(topic, levels = seq_len(k), labels = topic_noms),
      term = reorder_within(term, beta, topic)
    )

  print(
    ggplot(top_terms, aes(beta, term, fill = factor(topic))) +
      geom_col(show.legend = FALSE, alpha = 0.85) +
      facet_wrap(~ topic_label, scales = "free_y") +
      scale_y_reordered() +
      labs(title = paste0("Topics LDA (k=", k, ") — ", var_nom),
           subtitle = "Top 8 paraules per tema (probabilitat β)",
           x = "Probabilitat (β)", y = NULL) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold"),
            strip.text = element_text(face = "bold", size = 9))
  )

  gamma_df <- tidy(lda_fit, matrix = "gamma") %>%
    group_by(document) %>%
    slice_max(gamma, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(topic_label = topic_noms[as.character(topic)])

  cat(sprintf("  Distribució de respostes per tema LDA (k=%d):\n", k))
  print(table(gamma_df$topic_label))

  invisible(lda_fit)
}

# ── Funció comparativa log-ratio per subgrup ─────────────────────────────
# Aplica a qualsevol variable binària: GRUP_ASSIST, TREBALL, CURS_1R, etc.
comparativa_subgrup <- function(tok_grup_df, subgrup_var, titol_text, titol_subgrup,
                                 colors = c("#1A5276", "#922B21"), min_freq = 3) {
  if (!subgrup_var %in% names(tok_grup_df)) {
    cat(sprintf("  Variable '%s' no disponible al dataset\n", subgrup_var))
    return(invisible(NULL))
  }

  tok_sub <- tok_grup_df %>%
    filter(!is.na(.data[[subgrup_var]])) %>%
    mutate(subgrup = as.character(.data[[subgrup_var]]))

  nivells <- sort(unique(tok_sub$subgrup))
  if (length(nivells) < 2) {
    cat(sprintf("  '%s': menys de 2 nivells disponibles\n", subgrup_var))
    return(invisible(NULL))
  }

  g1 <- nivells[1]; g2 <- nivells[2]

  n_per_grup <- tok_sub %>% distinct(id, subgrup) %>% count(subgrup)
  cat(sprintf("  N respostes per '%s':\n", subgrup_var))
  print(n_per_grup)

  freq_sub <- tok_sub %>%
    group_by(subgrup) %>%
    count(word) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup()

  # Filtre de freqüència mínima: elimina paraules amb n total < min_freq
  # per evitar que log-ratios extrems (±6) d'hapax dominin visualment.
  paraules_valides <- freq_sub %>%
    group_by(word) %>%
    summarise(total_n = sum(n), .groups = "drop") %>%
    filter(total_n >= min_freq) %>%
    pull(word)

  cat(sprintf("  Paraules amb freqüència total >= %d: %d (de %d úniques)\n",
              min_freq, length(paraules_valides), n_distinct(freq_sub$word)))

  log_r <- freq_sub %>%
    filter(word %in% paraules_valides) %>%
    select(word, subgrup, prop) %>%
    pivot_wider(names_from = subgrup, values_from = prop, values_fill = 0.0001) %>%
    mutate(log_ratio = log2(.data[[g2]] / .data[[g1]])) %>%
    arrange(desc(abs(log_ratio))) %>%
    slice_head(n = 30) %>%
    mutate(
      word = reorder(word, log_ratio),
      grup_label = ifelse(log_ratio > 0,
                          paste0("Més a '", g2, "'"),
                          paste0("Més a '", g1, "'"))
    )

  if (nrow(log_r) < 3) {
    cat(sprintf("  %s per %s: no hi ha prou diferències lèxiques\n",
                titol_text, subgrup_var))
    return(invisible(NULL))
  }

  etiqueta_colors <- setNames(colors, c(paste0("Més a '", g1, "'"),
                                         paste0("Més a '", g2, "'")))

  print(
    ggplot(log_r, aes(x = log_ratio, y = word, fill = grup_label)) +
      geom_col(alpha = 0.85) +
      geom_vline(xintercept = 0, color = "grey30") +
      scale_fill_manual(values = etiqueta_colors) +
      labs(title = paste0(titol_text, " — per ", titol_subgrup),
           subtitle = paste0("Log2-ratio (positiu = més freqüent a '", g2, "')"),
           x = "Log2-ratio", y = NULL, fill = NULL) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"),
            legend.position = "top",
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank())
  )
  invisible(log_r)
}

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
                          "Escepticisme i desvinculacio"))

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
    geom_vline(xintercept = 0, color = "grey30") +
    scale_fill_manual(values = c("Més a EXP_POS" = "#1A5276",
                                 "Més a EXP_NEG" = "#922B21")) +
    labs(title = "Paraules diferenciadores: EXP_POS vs EXP_NEG",
         subtitle = "Log2-ratio de freqüències relatives (positiu = EXP_POS, negatiu = EXP_NEG)",
         x = "Log2-ratio", y = NULL, fill = NULL) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"),
          legend.position = "top",
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank())
)

#### ============================================================ ####
####              5. ANÀLISI DE SENTIMENT (NRC)                   ####
#### ============================================================ ####

cat("\n========== 5. ANÀLISI DE SENTIMENT ==========\n\n")
cat("Mètode: NRC Emotion Lexicon (syuzhet, language='spanish')\n")
cat("Nota: cobertura parcial del català; les paraules sense traducció al\n")
cat("lexicó espanyol no puntuen. El biaix és consistent entre grups, per\n")
cat("tant les comparacions relatives segueixen sent vàlides.\n\n")

score_sentiment <- function(var_nom, titol) {
  dades_v <- dades_def %>%
    filter(!is.na(.data[[var_nom]]), str_squish(.data[[var_nom]]) != "")

  textos_nets <- clean_text(dades_v[[var_nom]])

  nrc_scores <- tryCatch(
    get_nrc_sentiment(textos_nets, language = "spanish"),
    error = function(e) {
      cat(sprintf("  NRC no disponible per %s: %s\n", var_nom, conditionMessage(e)))
      NULL
    }
  )
  if (is.null(nrc_scores)) return(invisible(NULL))

  scores_df <- bind_cols(
    dades_v %>% select(any_of(c("GRUP_ASSIST", "P_ASSIST", "TREBALL",
                                 "CURS", "CURS_1R"))),
    nrc_scores
  ) %>%
    mutate(sentiment_net = positive - negative)

  cat(sprintf("\n%s:\n", titol))
  cat(sprintf("  N respostes puntuades: %d\n", nrow(scores_df)))
  cat(sprintf("  Sentiment net M=%.2f (SD=%.2f); rang [%d, %d]\n",
              mean(scores_df$sentiment_net), sd(scores_df$sentiment_net),
              min(scores_df$sentiment_net), max(scores_df$sentiment_net)))

  if ("P_ASSIST" %in% names(scores_df)) {
    r_val <- cor(scores_df$sentiment_net, scores_df$P_ASSIST, use = "complete.obs")
    cat(sprintf("  Correlació sentiment_net ~ P_ASSIST: r = %.3f\n", r_val))
  }

  cat(sprintf("  Respostes sentiment positiu net (>0): %d (%.0f%%)\n",
              sum(scores_df$sentiment_net > 0),
              100 * mean(scores_df$sentiment_net > 0)))
  cat(sprintf("  Respostes sentiment negatiu net (<0): %d (%.0f%%)\n",
              sum(scores_df$sentiment_net < 0),
              100 * mean(scores_df$sentiment_net < 0)))

  # Distribució d'emocions (top 8 emocions NRC)
  emocions <- scores_df %>%
    summarise(across(c(anger, anticipation, disgust, fear, joy,
                       sadness, surprise, trust), mean)) %>%
    pivot_longer(everything(), names_to = "emocio", values_to = "mitjana") %>%
    mutate(emocio = recode(emocio,
      anger = "ira", anticipation = "anticipació", disgust = "fàstic",
      fear = "por", joy = "alegria", sadness = "tristesa",
      surprise = "sorpresa", trust = "confiança"
    )) %>%
    arrange(desc(mitjana))

  cat("  Mitjana d'emocions NRC:\n")
  print(emocions)

  print(
    ggplot(emocions, aes(x = reorder(emocio, mitjana), y = mitjana,
                          fill = mitjana)) +
      geom_col(alpha = 0.85) +
      coord_flip() +
      scale_fill_gradient(low = "#AED6F1", high = "#1A5276", guide = "none") +
      labs(title = paste0("Perfil d'emocions NRC — ", titol),
           subtitle = "Mitjana de mencions per resposta (escala NRC, cobertura espanyol)",
           x = NULL, y = "Mitjana per resposta") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"))
  )

  # Boxplot sentiment net per GRUP_ASSIST
  if ("GRUP_ASSIST" %in% names(scores_df) &&
      length(unique(na.omit(scores_df$GRUP_ASSIST))) > 1) {
    scores_df_plot <- scores_df %>% filter(!is.na(GRUP_ASSIST))
    print(
      ggplot(scores_df_plot, aes(x = factor(GRUP_ASSIST), y = sentiment_net,
                                  fill = factor(GRUP_ASSIST))) +
        geom_boxplot(alpha = 0.70, outlier.alpha = 0.5) +
        geom_jitter(width = 0.15, alpha = 0.30, size = 1.5) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
        labs(title = paste0("Sentiment per grup d'assistència — ", titol),
             subtitle = "Puntuació neta = positiu − negatiu (NRC espanyol)",
             x = "Grup d'assistència", y = "Sentiment net",
             fill = "Grup") +
        theme_minimal(base_size = 13) +
        theme(plot.title = element_text(face = "bold"),
              legend.position = "none")
    )

    test_res <- tryCatch(
      wilcox.test(sentiment_net ~ GRUP_ASSIST, data = scores_df_plot),
      error = function(e) NULL
    )
    if (!is.null(test_res)) {
      cat(sprintf("  Test Wilcoxon sentiment_net ~ GRUP_ASSIST: W=%.0f, p=%.4f\n",
                  test_res$statistic, test_res$p.value))
    }
  }

  invisible(scores_df)
}

sent_pos <- score_sentiment("EXP_POS", titols["EXP_POS"])
sent_neg <- score_sentiment("EXP_NEG", titols["EXP_NEG"])
sent_mot <- score_sentiment("PROP_MOT", titols["PROP_MOT"])

#### ============================================================ ####
####         6. ANÀLISI PER SUBGRUPS                              ####
#### ============================================================ ####

cat("\n========== 6. ANÀLISI PER SUBGRUPS ==========\n\n")
cat("Preguntes: parlen diferent els estudiants que treballen vs. els que no?\n")
cat("Les propostes de primer curs difereixen de la resta?\n\n")

# ── 6.1 PROP_MOT per situació laboral (TREB_INT) ────────────────────────────
cat("--- 6.1 PROP_MOT per situació laboral (TREB_INT) ---\n")
cat("TREB_INTENS: 0=No treballa o treballa ocasionalment | 1=Treballa\n")


if ("TREB_INTENS" %in% names(tok_mot_grup)) {
  # Distribució de respostes per nivell
  n_dedic <- tok_mot_grup %>%
    distinct(id, TREB_INTENS) %>%
    filter(!is.na(TREB_INTENS)) %>%
    count(TREB_INTENS) %>%
    mutate(etiqueta = recode(as.character(TREB_INTENS),
      "1" = "Estudi complet", "2" = "Treball ocasional",
      "3" = "Treball parcial", "4" = "Treball complet"))
  cat("Distribució de respostes per TREB_INTENS:\n")
  print(n_dedic)
  cat("\n")

  # Comparativa binària: no treballa (1) vs. treballa (2+3+4)
  # Agrupa 2/3/4 perquè la distinció rellevant és tenir activitat laboral
  tok_mot_dedic <- tok_mot_grup %>%
    filter(!is.na(TREB_INTENS)) %>%
    mutate(DEDIC_BIN = ifelse(TREB_INTENS == 1, "No treballa", "Treballa"))

  cat("Comparativa binària: No treballa (TREB_INTENS=1) vs. Treballa (TREB_INTENS=2,3,4)\n")
  comparativa_subgrup(tok_mot_dedic, "DEDIC_BIN",
                      titols["PROP_MOT"], "situació laboral",
                      colors = c("#E67E22", "#2ECC71"))
} else {
  cat("  Variable TREB_INTENS no disponible al dataset\n")
}

# ── 6.2 PROP_MOT per CURS_1R ─────────────────────────────────────────────
cat("\n--- 6.2 PROP_MOT per curs (CURS_1R: 1r curs vs. resta) ---\n")
comparativa_subgrup(tok_mot_grup, "CURS_1R",
                    titols["PROP_MOT"], "curs",
                    colors = c("#8E44AD", "#27AE60"))

# ── 6.3 EXP_NEG per GRUP_ASSIST (lectura narrativa) ──────────────────────
# Quins aspectes negatius mencionen els estudiants amb baixa assistència?
cat("\n--- 6.3 EXP_NEG per grup d'assistència (lectura narrativa) ---\n")
cat("(Quin vocabulari diferencia els estudiants que falten dels que assisteixen?)\n")
comparativa_subgrup(tok_neg_grup, "GRUP_ASSIST",
                    titols["EXP_NEG"], "grup d'assistència",
                    colors = c("#1A5276", "#922B21"))

# ── 6.4 PROP_MOT per CURS (si hi ha la variable categòrica completa) ─────
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
        facet_wrap(~ paste("Curs", CURS), scales = "free_y") +
        scale_y_reordered() +
        labs(title = "Top paraules PROP_MOT per curs",
             subtitle = "Proporció relativa dins cada curs (n ≥ 10 respostes)",
             x = "Proporció", y = NULL) +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold"),
              strip.text = element_text(face = "bold"))
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

set.seed(42)
colors_comb <- brewer.pal(8, "Set2")
wordcloud(
  words = freq_tot$word, freq = freq_tot$n,
  min.freq = 2, max.words = 100,
  random.order = FALSE, rot.per = 0.15,
  colors = colors_comb, scale = c(5, 0.6)
)
title(main = "Word cloud combinat — EXP_POS + EXP_NEG + PROP_MOT",
      cex.main = 1.2, font.main = 2)

#### ============================================================ ####
####              8. XARXA DE CO-OCURRÈNCIA                       ####
#### ============================================================ ####

cat("\n========== 8. XARXA DE CO-OCURRÈNCIA ==========\n\n")

cooc_net <- function(var_nom, dades, min_cooc = 2, color_edge = "#555555") {
  textos <- dades[[var_nom]]
  textos <- textos[!is.na(textos) & str_squish(textos) != ""]

  toks <- tibble(id = seq_along(textos), text = clean_text(textos)) %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% sw_all, nchar(word) > 2) %>%
    mutate(word = lematitzar(word))

  top40 <- toks %>% count(word, sort = TRUE) %>% slice_head(n = 40) %>% pull(word)
  toks <- toks %>% filter(word %in% top40)

  pairwise <- toks %>%
    inner_join(toks, by = "id", suffix = c("_1", "_2"),
               relationship = "many-to-many") %>%
    filter(word_1 < word_2) %>%
    count(word_1, word_2, sort = TRUE) %>%
    filter(n >= min_cooc)

  if (nrow(pairwise) < 3) {
    cat(sprintf("  %s: co-ocurrències insuficients (n=%d)\n", var_nom, nrow(pairwise)))
    return(invisible(NULL))
  }

  cat(sprintf("  %s: %d parelles de co-ocurrència\n", var_nom, nrow(pairwise)))

  g <- graph_from_data_frame(pairwise, directed = FALSE)
  E(g)$weight <- pairwise$n

  print(
    ggraph(g, layout = "fr") +
      geom_edge_link(aes(alpha = n, width = n), color = color_edge) +
      geom_node_point(size = 5, color = color_edge, alpha = 0.8) +
      geom_node_label(aes(label = name), repel = TRUE, size = 3.5,
                      fontface = "bold", fill = "white", label.size = 0.3) +
      scale_edge_alpha(range = c(0.2, 0.9), guide = "none") +
      scale_edge_width(range = c(0.4, 2.5), guide = "none") +
      labs(title = paste0("Xarxa de co-ocurrència — ", var_nom),
           subtitle = paste0("Paraules que apareixen juntes a la mateixa resposta (n ≥ ", min_cooc, ")")) +
      theme_void(base_size = 13) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5, color = "grey40"))
  )
}

cat("Xarxes de co-ocurrència:\n")
cooc_net("EXP_POS",  dades_def, min_cooc = 2, color_edge = "#1A5276")
cooc_net("EXP_NEG",  dades_def, min_cooc = 2, color_edge = "#922B21")
cooc_net("PROP_MOT", dades_def, min_cooc = 2, color_edge = "#1E8449")

#### ============================================================ ####
####                  GUARDAR I TANCAR                            ####
#### ============================================================ ####

cat("\n========== GUARDAT ==========\n\n")
cat("-> 13.1 Output_text_textual.txt\n")
cat("-> 13.2 Output_grafics_textual.pdf\n")

dev.off()
sink()
