packages <- c("tidytext", "wordcloud", "dplyr", "stringr", "ggplot2",
              "tibble", "tidyr", "RColorBrewer", "stopwords", "igraph", "ggraph")
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

# Stopwords: català + castellà (les respostes poden ser en ambdós idiomes)
# NOTA: sense API externa (DeepL, Google Translate) no és possible traduir
# automàticament al català. S'utilitzen stopwords bilingües per garantir que
# les paraules buides de tots dos idiomes s'eliminin correctament.
sw_cat <- tryCatch(
  stopwords::stopwords("ca", source = "stopwords-iso"),
  error = function(e) character(0)
)
sw_es <- tryCatch(
  stopwords::stopwords("es", source = "stopwords-iso"),
  error = function(e) character(0)
)

# Stopwords manuals de reforç (català)
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
  "tenir", "tinc", "té", "tenim", "teniu", "tenen"
)

# Stopwords manuals de reforç (castellà)
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

# Funció de neteja de text
clean_text <- function(x) {
  x <- tolower(x)
  x <- str_replace_all(x, "’|‘|'|`", " ")  # cometes tipogràfiques → espai
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

# Funció per tokenitzar una variable i eliminar stopwords
tokenitzar <- function(var_nom, dades) {
  textos <- dades[[var_nom]]
  textos <- textos[!is.na(textos) & str_squish(textos) != ""]
  tibble(
    id = seq_along(textos),
    text = clean_text(textos)
  ) %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% sw_all, nchar(word) > 2) %>%
    mutate(variable = var_nom)
}

# Funció per fer el gràfic de freqüències (bar chart)
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
           subtitle = "Sense stopwords (català + castellà)",
           x = "Freqüència", y = NULL) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank())
  )
  invisible(freq)
}

# Funció per fer el word cloud
plot_wordcloud <- function(tokens_df, titol, paleta) {
  freq <- tokens_df %>% count(word, sort = TRUE)
  if (nrow(freq) == 0) {
    cat("  (sense paraules suficients per al word cloud)\n")
    return(invisible(NULL))
  }
  colors_wc <- brewer.pal(max(3, min(8, nrow(freq))), paleta)
  set.seed(42)
  wordcloud(
    words = freq$word,
    freq = freq$n,
    min.freq = 2,
    max.words = 80,
    random.order = FALSE,
    rot.per = 0.15,
    colors = colors_wc,
    scale = c(4.5, 0.6)
  )
  title(main = paste0("Word cloud — ", titol), cex.main = 1.2, font.main = 2)
  invisible(freq)
}

# Funció per calcular bigrams
calc_bigrams <- function(var_nom, dades, top_n = 15) {
  textos <- dades[[var_nom]]
  textos <- textos[!is.na(textos) & str_squish(textos) != ""]
  tibble(text = clean_text(textos)) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    filter(!is.na(bigram)) %>%
    separate(bigram, c("w1", "w2"), sep = " ") %>%
    filter(!w1 %in% sw_all, !w2 %in% sw_all,
           nchar(w1) > 2, nchar(w2) > 2) %>%
    unite(bigram, w1, w2, sep = " ") %>%
    count(bigram, sort = TRUE) %>%
    slice_head(n = top_n)
}

# Funció per graficar bigrams
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
           subtitle = "Parelles de paraules consecutives (sense stopwords)",
           x = "Freqüència", y = NULL) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank())
  )
  invisible(bigrams_df)
}

#### ============================================================ ####
####           1. EXP_POS — Experiències positives                ####
#### ============================================================ ####

cat("\n========== 1. EXP_POS — Experiències positives ==========\n\n")
cat("Pregunta: ", preguntes["EXP_POS"], "\n\n")

tok_pos <- tokenitzar("EXP_POS", dades_def)

cat("Paraules úniques (sense stopwords):", n_distinct(tok_pos$word), "\n")
cat("Total ocurrències:", nrow(tok_pos), "\n\n")

cat("Top 30 paraules més freqüents (EXP_POS):\n")
freq_pos <- tok_pos %>% count(word, sort = TRUE) %>% slice_head(n = 30)
print(freq_pos, n = 30)

# Gràfic de freqüències
plot_freq(tok_pos, titols["EXP_POS"], color_fill = "#1A5276", top_n = 20)

# Word cloud
plot_wordcloud(tok_pos, titols["EXP_POS"], paleta = "Blues")

# Bigrams
cat("\nTop bigrams (EXP_POS):\n")
bi_pos <- calc_bigrams("EXP_POS", dades_def)
print(bi_pos)
plot_bigrams(bi_pos, titols["EXP_POS"], color_fill = "#1A5276")

#### ============================================================ ####
####           2. EXP_NEG — Experiències negatives                ####
#### ============================================================ ####

cat("\n========== 2. EXP_NEG — Experiències negatives ==========\n\n")
cat("Pregunta: ", preguntes["EXP_NEG"], "\n\n")

tok_neg <- tokenitzar("EXP_NEG", dades_def)

cat("Paraules úniques (sense stopwords):", n_distinct(tok_neg$word), "\n")
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

#### ============================================================ ####
####           3. PROP_MOT — Propostes de motivació               ####
#### ============================================================ ####

cat("\n========== 3. PROP_MOT — Propostes de motivació ==========\n\n")
cat("Pregunta: ", preguntes["PROP_MOT"], "\n\n")

tok_mot <- tokenitzar("PROP_MOT", dades_def)

cat("Paraules úniques (sense stopwords):", n_distinct(tok_mot$word), "\n")
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

#### ============================================================ ####
####         4. COMPARATIVA EXP_POS vs EXP_NEG                   ####
#### ============================================================ ####

cat("\n========== 4. COMPARATIVA EXP_POS vs EXP_NEG ==========\n\n")

# Freqüències relatives de cada paraula en cada variable
freq_comp <- bind_rows(
  tok_pos %>% count(word) %>% mutate(variable = "EXP_POS"),
  tok_neg %>% count(word) %>% mutate(variable = "EXP_NEG")
) %>%
  group_by(variable) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

# Log-ratio: paraules diferenciadores (positiu = més freqüent a POS, negatiu = a NEG)
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

# Gràfic log-ratio
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
####         5. WORD CLOUD COMBINAT (les 3 variables)             ####
#### ============================================================ ####

cat("\n========== 5. WORD CLOUD COMBINAT ==========\n\n")

tok_tot <- bind_rows(tok_pos, tok_neg, tok_mot)
freq_tot <- tok_tot %>% count(word, sort = TRUE)

cat("Total paraules úniques (totes 3 variables):", nrow(freq_tot), "\n")
cat("Top 30 paraules globals:\n")
print(slice_head(freq_tot, n = 30))

set.seed(42)
colors_comb <- brewer.pal(8, "Set2")
wordcloud(
  words = freq_tot$word,
  freq = freq_tot$n,
  min.freq = 2,
  max.words = 100,
  random.order = FALSE,
  rot.per = 0.15,
  colors = colors_comb,
  scale = c(5, 0.6)
)
title(main = "Word cloud combinat — EXP_POS + EXP_NEG + PROP_MOT",
      cex.main = 1.2, font.main = 2)

#### ============================================================ ####
####              6. XARXA DE CO-OCURRÈNCIA                       ####
#### ============================================================ ####

cat("\n========== 6. XARXA DE CO-OCURRÈNCIA ==========\n\n")

# Co-ocurrència dins la mateixa resposta (per a cada variable)
cooc_net <- function(var_nom, dades, min_cooc = 2, color_edge = "#555555") {
  textos <- dades[[var_nom]]
  textos <- textos[!is.na(textos) & str_squish(textos) != ""]

  toks <- tibble(id = seq_along(textos), text = clean_text(textos)) %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% sw_all, nchar(word) > 2)

  # Top 40 paraules (les més freqüents) per simplificar la xarxa
  top40 <- toks %>% count(word, sort = TRUE) %>% slice_head(n = 40) %>% pull(word)
  toks <- toks %>% filter(word %in% top40)

  # Parelles de paraules que apareixen juntes a la mateixa resposta
  pairwise <- toks %>%
    inner_join(toks, by = "id", suffix = c("_1", "_2"), relationship = "many-to-many") %>%
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
