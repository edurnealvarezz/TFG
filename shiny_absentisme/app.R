# ============================================================
#  app.R — Predicció Absentisme Universitari (TFG FEE/UB)
# ============================================================

suppressPackageStartupMessages({
  library(shiny)
  library(shinythemes)
  library(dplyr)
  library(ggplot2)
  library(DT)
})

# ── Constants ────────────────────────────────────────────────
COL_C1    <- "#4A90B8"
COL_C2    <- "#E07B54"
COL_REG   <- "#27AE60"
COL_IRR   <- "#E74C3C"
THRESH_RF <- 0.572
K_VEINS   <- 11

# ── Rutes (relatives al directori de l'app) ──────────────────
# Opció 1: fitxers copiats a data/  (producció)
# Opció 2: fitxers del pipeline TFG (desenvolupament)
resolve_path <- function(...) {
  candidates <- list(...)
  for (p in candidates) if (file.exists(p)) return(p)
  candidates[[1]]
}

PATH_RDATA <- resolve_path(
  "data/fuzzy_clustering_model_complet.RData",
  "../2. Dades/fuzzy_clustering_model_complet.RData"
)
PATH_RF <- resolve_path(
  "data/model_rf_a.rds",
  "../4. Outputs/Metriques i models/model_rf_a.rds"
)
PATH_MET <- if (dir.exists("data")) "data" else "../4. Outputs/Metriques i models"

# ── Carregar RData ───────────────────────────────────────────
env_m   <- new.env()
DATA_OK <- tryCatch({
  load(PATH_RDATA, envir = env_m)
  TRUE
}, error = function(e) {
  message("ERROR carregant RData: ", e$message)
  FALSE
})

if (DATA_OK) {
  for (nm in ls(env_m)) assign(nm, get(nm, envir = env_m))
  rm(env_m)
}

# ── Índexos de X_train dins dades_def ───────────────────────
# Reconstrueix la mateixa partició 80/20 que el script 16
idx_global_train <- NULL
if (DATA_OK && requireNamespace("caret", quietly = TRUE)) {
  tryCatch({
    df_tmp <- dades_def %>% transmute(
      EDAT           = as.numeric(EDAT),
      DESPL          = as.numeric(DESPL),
      N_ASSIG        = as.numeric(N_ASSIG),
      NOTA_num       = as.numeric(NOTA),
      T_AVAL_num     = as.integer(T_AVAL == "Continuada"),
      CURS_1R        = as.integer(as.numeric(CURS_1R)),
      GENERE_Home    = as.integer(GENERE == "Home"),
      DOBLE_GRAU_EST = as.integer(as.numeric(DOBLE_GRAU_EST)),
      DEDIC_num      = as.integer(as.numeric(DEDIC)),
      IA_HABIT       = as.integer(IA_HABIT),
      IA_COMPR       = as.integer(IA_COMPR),
      IA_REND        = as.integer(IA_REND),
      IA_PDFS        = as.integer(IA_PDFS),
      IA_SUBST       = as.integer(IA_SUBST),
      IA_ATENC       = as.integer(IA_ATENC),
      IA_CONF        = as.integer(IA_CONF),
      GRUP_ASSIST    = as.character(GRUP_ASSIST)
    )
    cc <- complete.cases(df_tmp[, vars_clust])
    set.seed(1234)
    idx_part         <- caret::createDataPartition(df_tmp$GRUP_ASSIST[cc], p = 0.80, list = FALSE)
    idx_global_train <- which(cc)[idx_part]
  }, error = function(e) {
    message("Fallback idx_global_train seqüencial: ", e$message)
    idx_global_train <<- seq_len(nrow(X_train))
  })
} else if (DATA_OK) {
  idx_global_train <- seq_len(nrow(X_train))
}

# ── Model RF (opcional) ──────────────────────────────────────
rf_model <- NULL
if (requireNamespace("ranger", quietly = TRUE) && file.exists(PATH_RF)) {
  rf_model <- tryCatch(readRDS(PATH_RF), error = function(e) NULL)
}

# ── Funcions helpers ─────────────────────────────────────────
construir_alumne <- function(inp) {
  c(EDAT           = as.numeric(inp$edat),
    DESPL          = as.numeric(inp$despl),
    N_ASSIG        = as.numeric(inp$n_assig),
    NOTA_num       = as.numeric(inp$nota_num),
    T_AVAL_num     = as.numeric(inp$t_aval_num),
    CURS_1R        = as.numeric(inp$curs_1r),
    GENERE_Home    = as.numeric(inp$genere_home),
    DOBLE_GRAU_EST = as.numeric(inp$doble_grau),
    DEDIC_num      = as.numeric(inp$dedic_num),
    IA_HABIT       = as.numeric(inp$ia_habit),
    IA_COMPR       = as.numeric(inp$ia_compr),
    IA_REND        = as.numeric(inp$ia_rend),
    IA_PDFS        = as.numeric(inp$ia_pdfs),
    IA_SUBST       = as.numeric(inp$ia_subst),
    IA_ATENC       = as.numeric(inp$ia_atenc),
    IA_CONF        = as.numeric(inp$ia_conf))
}

pred_rf <- function(av) {
  if (is.null(rf_model)) return(NULL)
  x_df <- as.data.frame(matrix(av[vars_clust], nrow = 1,
                                dimnames = list(NULL, vars_clust)))
  tryCatch({
    preds <- predict(rf_model, data = x_df)$predictions
    if (is.matrix(preds)) {
      col_r <- grep("Regular", colnames(preds), value = FALSE)[1]
      prob  <- preds[1, col_r]
    } else {
      prob <- as.numeric(preds[1])
    }
    list(prob = round(as.numeric(prob), 4),
         pred = ifelse(prob >= THRESH_RF, "Regular", "Irregular"))
  }, error = function(e) NULL)
}

pred_knn <- function(av) {
  tryCatch(predict_nou_alumne(av), error = function(e) NULL)
}

trobar_veins <- function(av) {
  x_sc  <- (av[names(scale_params$mean)] - scale_params$mean) / scale_params$sd
  dists <- sqrt(rowSums(sweep(X_train, 2, x_sc) ^ 2))
  order(dists)[seq_len(K_VEINS)]
}

info_veins <- function(idx_tr) {
  idx_def <- idx_global_train[idx_tr]
  sub     <- dades_def[idx_def, , drop = FALSE]
  efa     <- intersect(
    c("MOT_DESMOTIVACIO", "MOT_AUTOGESTIO", "MOT_FORCA_MAJOR", "IA_SUBSTITUCIO"),
    names(sub)
  )
  df <- data.frame(
    Alumne          = paste0("V", seq_along(idx_tr)),
    `Grup real`     = as.character(sub$GRUP_ASSIST),
    `Assistència %` = round(as.numeric(sub$P_ASSIST), 1),
    check.names     = FALSE,
    stringsAsFactors = FALSE
  )
  for (col in efa) df[[col]] <- round(as.numeric(sub[[col]]), 2)
  df
}

carregar_metriques <- function() {
  fitxers <- c(
    "Logit Predictiu"  = "metriques_logit_pred.rds",
    "RF-A (principal)" = "metriques_rf_a.rds",
    "RF-B"             = "metriques_rf_b.rds",
    "XGBoost"          = "metriques_xgb.rds",
    "CatBoost"         = "metriques_catboost.rds",
    "SVM-RBF"          = "metriques_svm.rds",
    "KNN fuzzy"        = "metriques_knn.rds"
  )
  rows <- lapply(names(fitxers), function(nm) {
    f <- file.path(PATH_MET, fitxers[[nm]])
    if (!file.exists(f)) return(NULL)
    m <- tryCatch(readRDS(f), error = function(e) NULL)
    if (is.null(m)) return(NULL)
    data.frame(
      Model        = nm,
      AUC          = round(as.numeric(m$AUC), 4),
      Accuracy     = round(as.numeric(m$accuracy), 4),
      Precisio     = round(as.numeric(m$precision), 4),
      Recall       = round(as.numeric(m$recall), 4),
      F1           = round(as.numeric(m$F1), 4),
      Balanced_Acc = round(as.numeric(m$balanced_accuracy), 4),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# ── UI ───────────────────────────────────────────────────────
ui <- navbarPage(
  title = tags$span(
    tags$img(src = NULL, height = "20px"),
    "Absentisme Universitari — TFG FEE/UB"
  ),
  theme      = shinythemes::shinytheme("flatly"),
  header     = tags$head(tags$link(rel = "stylesheet", href = "custom.css")),
  collapsible = TRUE,

  # ── Pestanya 1: Predicció individual ──────────────────────
  tabPanel("Predicció individual",
    sidebarLayout(
      sidebarPanel(
        width = 4,
        tags$h4(tags$i(class = "glyphicon glyphicon-education"), " Dades acadèmiques"),
        numericInput("edat",    "Edat (anys)", 21, 17, 60),
        sliderInput("despl",    "Desplaçament a la universitat (min)",
                    min = 0, max = 180, value = 30, step = 5),
        numericInput("n_assig", "Assignatures matriculades", 8, 1, 15),
        selectInput("nota_num", "Nota d'accés / expedient",
          choices = c("5.0–5.9 (Aprovat just)" = 1, "6.0–6.9" = 2,
                      "7.0–7.9" = 3, "8.0–8.9" = 4, "≥ 9 (Excel·lent)" = 5),
          selected = 3),
        radioButtons("t_aval_num", "Tipus d'avaluació",
          c("Continuada" = 1, "Única" = 0), inline = TRUE),
        radioButtons("curs_1r", "Primer curs?",
          c("Sí" = 1, "No" = 0), inline = TRUE),
        radioButtons("genere_home", "Gènere",
          c("Home" = 1, "Dona / Altre" = 0), inline = TRUE),
        radioButtons("doble_grau", "Doble grau?",
          c("Sí" = 1, "No" = 0), inline = TRUE, selected = 0),
        selectInput("dedic_num", "Dedicació",
          c("Només estudio"            = 1,
            "Treballo ocasionalment"   = 2,
            "Treballo a temps parcial" = 3,
            "Treballo a temps complet" = 4)),
        tags$hr(),
        tags$h4(tags$i(class = "glyphicon glyphicon-phone"), " Ús de la IA"),
        tags$p(tags$em("Escala: 1 = mai · 6 = sempre")),
        sliderInput("ia_habit",  "Ús habitual de la IA per estudiar",          1, 6, 3, 1),
        sliderInput("ia_compr",  "Uso la IA per entendre continguts",           1, 6, 3, 1),
        sliderInput("ia_rend",   "Crec que la IA millora el meu rendiment",     1, 6, 3, 1),
        sliderInput("ia_pdfs",   "Uso la IA per processar PDFs i apunts",       1, 6, 3, 1),
        sliderInput("ia_subst",  "Uso la IA com a substitut d'anar a classe",   1, 6, 3, 1),
        sliderInput("ia_atenc",  "La IA fa que pari menys atenció a classe",    1, 6, 3, 1),
        sliderInput("ia_conf",   "Confio més en la IA que en el professor",     1, 6, 3, 1),
        tags$hr(),
        actionButton("btn_avaluar", "Avaluar alumne",
          class = "btn-primary",
          style = "width:100%; font-size:16px; padding:12px;")
      ),

      mainPanel(
        width = 8,
        # Bloc 1 — RF
        uiOutput("bloc_rf"),
        # Bloc 2 — Fuzzy
        uiOutput("bloc_fuzzy"),
        # Bloc 3 — Veïns
        uiOutput("bloc_veins_header"),
        DT::dataTableOutput("taula_veins"),
        plotOutput("grafic_veins", height = "230px")
      )
    )
  ),

  # ── Pestanya 2: Càrrega massiva ────────────────────────────
  tabPanel("Càrrega massiva",
    fluidRow(style = "padding: 20px;",
      column(4,
        tags$h4("Pujar fitxer CSV"),
        fileInput("csv_file", NULL, accept = ".csv",
          buttonLabel  = "Triar fitxer…",
          placeholder  = "Cap fitxer seleccionat"),
        tags$p(tags$em(
          "El CSV ha de tenir les 16 columnes amb els mateixos noms que les variables d'entrada."
        )),
        tags$details(
          tags$summary("Variables requerides"),
          tags$code(paste(
            c("EDAT","DESPL","N_ASSIG","NOTA_num","T_AVAL_num","CURS_1R",
              "GENERE_Home","DOBLE_GRAU_EST","DEDIC_num",
              "IA_HABIT","IA_COMPR","IA_REND","IA_PDFS",
              "IA_SUBST","IA_ATENC","IA_CONF"),
            collapse = ", "))
        ),
        tags$br(),
        actionButton("btn_processar", "Processar tots els alumnes",
          class = "btn-success", style = "width:100%;"),
        tags$br(), tags$br(),
        downloadButton("btn_descarregar", "Descarregar resultats (CSV)",
          style = "width:100%;")
      ),
      column(8,
        uiOutput("preview_csv"),
        DT::dataTableOutput("taula_preview"),
        tags$br(),
        uiOutput("resum_massiu"),
        DT::dataTableOutput("taula_massiva"),
        plotOutput("grafic_massiu", height = "260px")
      )
    )
  ),

  # ── Pestanya 3: Comparativa de models ─────────────────────
  tabPanel("Comparativa de models",
    tags$div(style = "padding: 24px;",
      tags$h4("Comparativa de models predictius"),
      tags$p(
        "Tots els models entrenats sobre les mateixes observacions (partició 80/20 estratificada, seed 1234). ",
        "El model principal ", tags$strong("RF-A"), " ha estat re-entrenat exclusivament amb les ",
        "16 variables observables pre-curs."
      ),
      DT::dataTableOutput("taula_models"),
      tags$br(),
      tags$p(tags$em(
        "AUC: àrea sota la corba ROC | Precisió: PPV | Recall: Sensibilitat | ",
        "Balanced Acc: (Recall + Especificitat) / 2"
      ))
    )
  )
)

# ── SERVER ───────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Resultat reactiu Tab 1 ─────────────────────────────────
  res <- eventReactive(input$btn_avaluar, {
    validate(need(DATA_OK,
      "No s'ha pogut carregar el model. Comprova la ruta del fitxer RData."))
    av <- construir_alumne(input)
    list(
      av       = av,
      rf       = pred_rf(av),
      knn      = pred_knn(av),
      idx_tr   = tryCatch(trobar_veins(av), error = function(e) NULL)
    )
  })

  # ── Bloc 1: Predicció principal ────────────────────────────
  output$bloc_rf <- renderUI({
    r  <- req(res())
    rf <- r$rf; knn <- r$knn

    if (!is.null(rf)) {
      prob <- rf$prob; pred <- rf$pred
      nota <- sprintf("Random Forest RF-A | Llindar PR: %.1f%%", THRESH_RF * 100)
    } else if (!is.null(knn)) {
      prob <- knn$prob_regular; pred <- knn$prediccio
      nota <- "KNN fuzzy-aware calibrat (Platt) — model RF-A no disponible"
    } else {
      return(tags$div(class = "alert alert-danger",
        "No s'ha pogut fer la predicció. Comprova que el model s'ha carregat correctament."))
    }

    pct  <- round(prob * 100, 1)
    col  <- if (grepl("Regular", pred, fixed = TRUE)) COL_REG else COL_IRR
    ico  <- if (grepl("Regular", pred, fixed = TRUE)) "✔" else "✖"
    etiq <- if (grepl("Regular", pred, fixed = TRUE)) "REGULAR" else "IRREGULAR"

    tags$div(
      tags$hr(),
      tags$h4("Predicció principal"),
      tags$div(
        style = sprintf(
          "background:%s; color:white; border-radius:8px; padding:18px; margin-bottom:10px;", col),
        tags$span(style = "font-size:26px; font-weight:bold;",
          sprintf("%s  Perfil %s", ico, etiq)),
        tags$br(),
        tags$span(style = "font-size:15px;",
          sprintf("Probabilitat d'assistència regular: %.1f%%", pct))
      ),
      tags$div(class = "prob-bar-bg",
        tags$div(class = "prob-bar-fill",
          style = sprintf("width:%.0f%%; background:%s;", pct, col))
      ),
      tags$small(tags$em(nota)),
      tags$hr()
    )
  })

  # ── Bloc 2: Perfil fuzzy ───────────────────────────────────
  output$bloc_fuzzy <- renderUI({
    r   <- req(res()); knn <- r$knn
    if (is.null(knn)) return(NULL)

    u1  <- knn$u1; u2 <- knn$u2

    interp <- if (u2 > 0.6)
      "Alta afinitat amb el Perfil Intensiu IA. El risc principal d'absentisme en alumnes similars és la substitució de la presencialitat per eines d'IA."
    else if (u1 > 0.6)
      "Alta afinitat amb el Perfil Tradicional. El risc principal en alumnes similars és la desmotivació pedagògica i factors de força major (feina, família, salut)."
    else
      "Perfil mixt. L'alumne comparteix característiques dels dos perfils i no té una afinitat dominant."

    barra <- function(u, col, label, assist_mit) {
      tags$div(style = "margin-bottom: 14px;",
        tags$div(style = "display:flex; justify-content:space-between; margin-bottom:4px;",
          tags$strong(label),
          tags$span(style = sprintf("color:%s;", col),
            sprintf("%.1f%% afinitat", u * 100))
        ),
        tags$div(class = "prob-bar-bg",
          tags$div(class = "prob-bar-fill",
            style = sprintf("width:%.0f%%; background:%s;", u * 100, col))
        ),
        tags$small(tags$em(sprintf("Assistència mitjana de la mostra: %.1f%%", assist_mit)))
      )
    }

    tags$div(
      tags$h4("Perfil fuzzy (KNN + FCM)"),
      tags$p(tags$em("A quin dels dos perfils s'assembla més aquest alumne?")),
      barra(u1, COL_C1, "Cluster 1 — Perfil Tradicional", 74.4),
      barra(u2, COL_C2, "Cluster 2 — Perfil Intensiu IA", 66.5),
      tags$div(class = "alert alert-info", style = "margin-top: 10px;", interp),
      tags$hr()
    )
  })

  # ── Bloc 3: Veïns ─────────────────────────────────────────
  output$bloc_veins_header <- renderUI({
    r <- req(res())
    if (is.null(r$idx_tr) || is.null(idx_global_train)) return(NULL)

    df_v  <- info_veins(r$idx_tr)
    n_irr <- sum(grepl("Irregular", df_v$`Grup real`), na.rm = TRUE)
    prob_knn <- if (!is.null(r$knn))
      sprintf("%.1f%%", r$knn$prob_regular * 100) else "N/D"

    tags$div(
      tags$h4(sprintf("Els %d alumnes més similars de la mostra (train)", K_VEINS)),
      tags$div(class = "alert alert-secondary",
        tags$strong(sprintf(
          "%d de %d alumnes similars eren Irregulars (%.0f%% observat)",
          n_irr, K_VEINS, n_irr / K_VEINS * 100)),
        tags$br(),
        sprintf("Probabilitat KNN fuzzy-aware calibrada (Platt): %s", prob_knn)
      )
    )
  })

  output$taula_veins <- DT::renderDataTable({
    r <- req(res())
    if (is.null(r$idx_tr) || is.null(idx_global_train))
      return(DT::datatable(data.frame()))
    df_v <- info_veins(r$idx_tr)
    DT::datatable(
      df_v,
      rownames = FALSE,
      options  = list(dom = "t", pageLength = K_VEINS, scrollX = TRUE)
    ) |>
      DT::formatStyle(
        "Grup real",
        backgroundColor = DT::styleEqual(
          c("Regular (≥80%)", "Irregular (<80%)"),
          c("#d4edda", "#f8d7da")
        )
      ) |>
      DT::formatStyle(
        "Assistència %",
        background       = DT::styleColorBar(c(0, 100), "#4A90B8"),
        backgroundSize   = "98% 60%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
  })

  output$grafic_veins <- renderPlot({
    r <- req(res())
    if (is.null(r$idx_tr) || is.null(idx_global_train)) return(NULL)
    df_v  <- info_veins(r$idx_tr)
    df_pl <- data.frame(
      v        = factor(paste0("V", seq_len(K_VEINS)), levels = paste0("V", seq_len(K_VEINS))),
      assist   = df_v$`Assistència %`,
      grup     = df_v$`Grup real`
    )
    ggplot(df_pl, aes(x = v, y = assist, fill = grup)) +
      geom_col(alpha = 0.85, width = 0.7) +
      geom_hline(yintercept = 80, linetype = "dashed", color = "grey40", linewidth = 0.8) +
      annotate("text", x = 0.6, y = 82, label = "Llindar 80%",
               color = "grey40", size = 3.5, hjust = 0) +
      scale_fill_manual(
        values = c("Regular (≥80%)" = COL_REG, "Irregular (<80%)" = COL_IRR),
        name   = NULL, na.value = "grey70"
      ) +
      scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 20)) +
      labs(title = sprintf("% Assistència dels %d veïns més propers", K_VEINS),
           x = NULL, y = "% Assistència") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top", panel.grid.major.x = element_blank())
  })

  # ── Tab 2: Càrrega massiva ─────────────────────────────────
  df_csv <- reactive({
    req(input$csv_file)
    tryCatch(
      read.csv(input$csv_file$datapath, stringsAsFactors = FALSE),
      error = function(e) NULL
    )
  })

  output$preview_csv <- renderUI({
    df <- df_csv()
    if (is.null(df))
      return(tags$p(class = "text-danger", "Error llegint el fitxer CSV."))
    if (!DATA_OK)
      return(tags$div(class = "alert alert-danger", "El model no s'ha pogut carregar."))
    manquen <- setdiff(vars_clust, names(df))
    if (length(manquen) > 0)
      return(tags$div(class = "alert alert-danger",
        tags$strong("Columnes que falten: "),
        paste(manquen, collapse = ", ")))
    tags$div(
      tags$h5(sprintf("Previsualització — %d alumnes, %d columnes:", nrow(df), ncol(df)))
    )
  })

  output$taula_preview <- DT::renderDataTable({
    df <- df_csv()
    if (is.null(df) || !DATA_OK) return(DT::datatable(data.frame()))
    if (length(setdiff(vars_clust, names(df))) > 0) return(DT::datatable(data.frame()))
    DT::datatable(head(df, 5), rownames = FALSE,
                  options = list(dom = "t", scrollX = TRUE))
  })

  res_massius <- eventReactive(input$btn_processar, {
    req(df_csv(), DATA_OK)
    df <- df_csv()
    validate(need(
      length(setdiff(vars_clust, names(df))) == 0,
      "El CSV no té totes les columnes necessàries."
    ))
    withProgress(message = "Processant alumnes…", value = 0, {
      n    <- nrow(df)
      rows <- lapply(seq_len(n), function(i) {
        incProgress(1 / n)
        av   <- setNames(as.numeric(unlist(df[i, vars_clust])), vars_clust)
        rf_r <- tryCatch(pred_rf(av),  error = function(e) NULL)
        kn_r <- tryCatch(pred_knn(av), error = function(e) NULL)
        data.frame(
          Alumne       = if ("id" %in% names(df)) df[i, "id"] else i,
          Prediccio_RF = if (!is.null(rf_r)) rf_r$pred else NA_character_,
          Prob_RF_pct  = if (!is.null(rf_r)) round(rf_r$prob * 100, 1) else NA_real_,
          Prob_KNN_pct = if (!is.null(kn_r)) round(kn_r$prob_regular * 100, 1) else NA_real_,
          u1_pct       = if (!is.null(kn_r)) round(kn_r$u1 * 100, 1) else NA_real_,
          u2_pct       = if (!is.null(kn_r)) round(kn_r$u2 * 100, 1) else NA_real_,
          Cluster      = if (!is.null(kn_r)) kn_r$cluster_dominant else NA_integer_,
          stringsAsFactors = FALSE
        )
      })
    })
    do.call(rbind, rows)
  })

  output$resum_massiu <- renderUI({
    df <- res_massius()
    if (is.null(df)) return(NULL)
    n     <- nrow(df)
    n_irr <- sum(grepl("Irregular", df$Prediccio_RF), na.rm = TRUE)
    tags$div(
      class = "alert alert-info",
      style = "margin-top:10px;",
      tags$strong(sprintf(
        "%d alumnes analitzats | %d en risc d'absentisme irregular (%.1f%%)",
        n, n_irr, n_irr / n * 100
      ))
    )
  })

  output$taula_massiva <- DT::renderDataTable({
    df <- req(res_massius())
    DT::datatable(
      df,
      rownames = FALSE,
      options  = list(pageLength = 15, scrollX = TRUE)
    ) |>
      DT::formatStyle(
        "Prediccio_RF",
        backgroundColor = DT::styleEqual(
          c("Regular", "Irregular"),
          c("#d4edda", "#f8d7da")
        )
      )
  })

  output$grafic_massiu <- renderPlot({
    df <- req(res_massius())
    prob_col <- if (!all(is.na(df$Prob_RF_pct))) "Prob_RF_pct" else "Prob_KNN_pct"
    llindar  <- if (prob_col == "Prob_RF_pct") THRESH_RF * 100 else 50
    ggplot(df, aes(x = .data[[prob_col]], fill = Prediccio_RF)) +
      geom_histogram(bins = 20, color = "white", alpha = 0.85) +
      geom_vline(xintercept = llindar, linetype = "dashed", color = "grey30", linewidth = 0.9) +
      annotate("text", x = llindar + 1, y = Inf, label = sprintf("Llindar %.1f%%", llindar),
               vjust = 2, hjust = 0, color = "grey30", size = 3.5) +
      scale_fill_manual(
        values   = c("Regular" = COL_REG, "Irregular" = COL_IRR),
        na.value = "grey70", name = NULL
      ) +
      labs(title = "Distribució de probabilitats predites",
           x = "P(Regular) %", y = "Alumnes") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top")
  })

  output$btn_descarregar <- downloadHandler(
    filename = function() paste0("prediccions_absentisme_", Sys.Date(), ".csv"),
    content  = function(file) {
      df <- res_massius()
      if (!is.null(df)) write.csv(df, file, row.names = FALSE)
    }
  )

  # ── Tab 3: Comparativa de models ───────────────────────────
  output$taula_models <- DT::renderDataTable({
    df <- carregar_metriques()
    if (is.null(df) || nrow(df) == 0) {
      return(DT::datatable(
        data.frame(Info = paste(
          "No s'han trobat fitxers de mètriques.",
          "Executa els scripts del pipeline per generar-los."
        )),
        rownames = FALSE,
        options  = list(dom = "t")
      ))
    }
    df       <- df[order(-df$AUC), ]
    is_ppal  <- grepl("RF-A", df$Model)
    DT::datatable(
      df,
      rownames  = FALSE,
      options   = list(dom = "t", pageLength = 20),
      escape    = FALSE
    ) |>
      DT::formatStyle(
        "Model",
        fontWeight       = DT::styleEqual(df$Model[is_ppal], "bold"),
        backgroundColor  = DT::styleEqual(df$Model[is_ppal], "#fff3cd")
      ) |>
      DT::formatStyle(
        "AUC",
        background         = DT::styleColorBar(c(0.5, 1), COL_C1),
        backgroundSize     = "98% 70%",
        backgroundRepeat   = "no-repeat",
        backgroundPosition = "center"
      ) |>
      DT::formatRound(c("AUC","Accuracy","Precisio","Recall","F1","Balanced_Acc"), digits = 4)
  })
}

shinyApp(ui, server)
