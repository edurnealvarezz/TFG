
library(cld2)
library(httr)

detectar_idioma <- function(texto) {
  cld2::detect_language(texto)
}

# funcio per traduir amb el softcatala

traducir_sc <- function(texto, origen = "es", destino = "ca") {

  res <- httr::POST(
    "https://www.softcatala.org/apertium/json/translate",
    body = list(
      langpair = paste0(origen, "|", destino),
      q = texto
    ),
    encode = "form"
  )

  httr::content(res, "parsed")$responseData$translatedText
}

# traduir només si està en castellà

procesar <- function(texto){
  if(is.na(texto) || texto == "") return(texto)
  lang <- detectar_idioma(texto)

  if(!is.na(lang) && lang == "es") {
    traducir_sc(texto, "es", "ca")
  } else {
    texto
  }
}


# Funció per tokenitzar i lematitzar una variable (udpipe catalan-ancora)
tokenitzar <- function(var_nom, dades) {
  textos <- dades[[var_nom]]
  textos <- textos[!is.na(textos) & str_squish(textos) != ""]
  ann <- udpipe::udpipe_annotate(ud_ca, x = clean_text(textos),
                                 doc_id = seq_along(textos))
  as.data.frame(ann) %>%
    filter(!is.na(lemma), nchar(lemma) > 2) %>%
    mutate(word = tolower(lemma)) %>%
    filter(!word %in% sw_all) %>%
    select(word) %>%
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

# Funció per calcular bigrams sobre lemes (udpipe)
calc_bigrams <- function(var_nom, dades, top_n = 15) {
  textos <- dades[[var_nom]]
  textos <- textos[!is.na(textos) & str_squish(textos) != ""]
  ann <- udpipe::udpipe_annotate(ud_ca, x = clean_text(textos),
                                 doc_id = seq_along(textos))
  as.data.frame(ann) %>%
    filter(!is.na(lemma), nchar(lemma) > 2) %>%
    mutate(word = tolower(lemma)) %>%
    filter(!word %in% sw_all) %>%
    group_by(doc_id) %>%
    summarise(text = paste(word, collapse = " "), .groups = "drop") %>%
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
