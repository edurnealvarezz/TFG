
library(cld2)
library(httr)

lematitzar <- function(words) {
  ifelse(words %in% names(lemma_dict), lemma_dict[words], words)
}

detectar_idioma <- function(texto) {
  tryCatch(cld2::detect_language(texto), error = function(e) NA_character_)
}

traduir_ca_es <- function(texto) {
  tryCatch({
    res <- httr::POST(
      "https://www.softcatala.org/apertium/json/translate",
      body = list(langpair = "ca|es", q = texto),
      encode = "form",
      httr::timeout(15)
    )
    out <- httr::content(res, "parsed")$responseData$translatedText
    if (is.null(out) || nchar(trimws(out)) == 0) texto else out
  }, error = function(e) texto)
}

normalitzar_a_es <- function(texto) {
  if (is.na(texto) || str_squish(texto) == "") return(texto)
  lang <- detectar_idioma(texto)
  if (!is.na(lang) && lang == "ca") traduir_ca_es(texto) else texto
}

clean_text <- function(x) {
  x <- tolower(x)
  x <- str_replace_all(x, "’|‘|“|”|`", " ")
  x <- str_replace_all(x, "[[:punct:]]", " ")
  x <- str_replace_all(x, "[[:digit:]]", " ")
  x <- str_squish(x)
  x
}

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
           subtitle = "Textos normalitzats al castellà (cat→es), sense stopwords",
           x = "Freqüència", y = NULL) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            legend.text = element_text(size = 12))
  )
  invisible(freq)
}

plot_wordcloud <- function(tokens_df, titol, paleta) {
  freq <- tokens_df %>% count(word, sort = TRUE)
  if (nrow(freq) == 0) {
    cat("  (sense paraules suficients per al word cloud)\n")
    return(invisible(NULL))
  }
  colors_wc <- brewer.pal(max(3, min(8, nrow(freq))), paleta)
  set.seed(1234)
  tryCatch(
    wordcloud(
      words = freq$word, freq = freq$n,
      min.freq = 2, max.words = 80,
      random.order = FALSE, rot.per = 0.15,
      colors = colors_wc, scale = c(4.5, 0.6)
    ),
    error = function(e) cat(sprintf("  wordcloud error: %s\n", conditionMessage(e)))
  )
  title(main = paste0("Word cloud — ", titol), cex.main = 1.2, font.main = 2)
  invisible(freq)
}

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
            panel.grid.major.y = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            legend.text = element_text(size = 12))
  )
  invisible(bigrams_df)
}

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

  set.seed(1234)
  lda_fit <- LDA(dtm, k = k, control = list(seed = 1234))

  if (!is.null(etiquetes) && length(etiquetes) == k) {
    topic_noms <- setNames(
      str_wrap(paste0("T", seq_len(k), ": ", etiquetes), width = 25),
      seq_len(k)
    )
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
      geom_text(aes(label = round(beta, 3)), hjust = -0.1, size = 3) +
      facet_wrap(~ topic_label, scales = "free_y") +
      scale_y_reordered() +
      scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
      labs(title = paste0("Topics LDA (k=", k, ") — ", var_nom),
           subtitle = "Top 8 paraules per tema (probabilitat β)",
           x = "Probabilitat (β)", y = NULL) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold"),
            strip.text = element_text(face = "bold", size = 9),
            axis.text.y = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            legend.text = element_text(size = 12))
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
      geom_text(aes(label = round(log_ratio, 2),
                    hjust = ifelse(log_ratio >= 0, -0.1, 1.1)),
                size = 3.5) +
      geom_vline(xintercept = 0, color = "grey30") +
      scale_fill_manual(values = etiqueta_colors) +
      scale_x_continuous(expand = expansion(mult = 0.18)) +
      labs(title = paste0(titol_text, " — per ", titol_subgrup),
           subtitle = paste0("Log2-ratio (positiu = més freqüent a '", g2, "')"),
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
  invisible(log_r)
}

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
      geom_text(aes(label = round(mitjana, 2)), hjust = -0.1, size = 3.5) +
      coord_flip() +
      scale_fill_gradient(low = "#AED6F1", high = "#1A5276", guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
      labs(title = paste0("Perfil d'emocions NRC — ", titol),
           subtitle = "Mitjana de mencions per resposta (escala NRC, cobertura espanyol)",
           x = NULL, y = "Mitjana per resposta") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"),
            axis.text.y = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            legend.text = element_text(size = 12))
  )

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
              legend.position = "none",
              axis.text.y = element_text(size = 12),
              axis.text.x = element_text(size = 12))
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
