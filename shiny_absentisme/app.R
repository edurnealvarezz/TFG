# ============================================================
#  app.R — Predicció Absentisme Universitari (TFG FEE/UB)
#  2 pestanyes: Alumnat nou (16 vars) | Alumnat antic (+ EFA)
# ============================================================

suppressPackageStartupMessages({
  library(shiny)
  library(shinythemes)
  library(dplyr)
  library(ggplot2)
  library(DT)
  library(tidyr)
})

# ── Constants ────────────────────────────────────────────────
COL_C1  <- "#4A90B8"
COL_C2  <- "#E07B54"
COL_REG <- "#27AE60"
COL_IRR <- "#E74C3C"

THRESH_RF <- 0.572
K_VEINS   <- 11

# ── Localitzar directori base del TFG ───────────────────────
# Busca el directori que conté "2. Dades/fuzzy_clustering_model_complet.RData"
# des de múltiples candidats: wd actual, pare del wd, o ruta absoluta
find_tfg_dir <- function() {
  target <- file.path("2. Dades", "fuzzy_clustering_model_complet.RData")
  dirs <- c(
    getwd(),
    dirname(getwd()),
    "C:/Users/Edurne/Downloads/TFG"
  )
  for (d in dirs) {
    if (file.exists(file.path(d, target))) return(normalizePath(d))
  }
  getwd()
}
TFG_DIR      <- find_tfg_dir()
PATH_RDATA   <- file.path(TFG_DIR, "2. Dades", "fuzzy_clustering_model_complet.RData")
PATH_RF      <- file.path(TFG_DIR, "2. Dades", "model_rf_a.rds")
PATH_MET_DIR <- file.path(TFG_DIR, "2. Dades")

# ── Carregar RData principal ─────────────────────────────────
env_m   <- new.env()
DATA_OK <- tryCatch({
  load(PATH_RDATA, envir = env_m); TRUE
}, error = function(e) {
  message("RData no trobat: ", PATH_RDATA, "\nError: ", e$message); FALSE
})
if (DATA_OK) { for (nm in ls(env_m)) assign(nm, get(nm, envir = env_m)) }
rm(env_m)

# ── Índexos X_train → dades_def ─────────────────────────────
idx_global_train <- NULL
if (DATA_OK && requireNamespace("caret", quietly = TRUE)) {
  tryCatch({
    df_tmp <- dades_def %>% transmute(
      EDAT = as.numeric(EDAT), DESPL = as.numeric(DESPL),
      N_ASSIG = as.numeric(N_ASSIG), NOTA_num = as.numeric(NOTA),
      T_AVAL_num = as.integer(T_AVAL == "Continuada"),
      CURS_1R = as.integer(CURS == "1r"),
      GENERE_Home = as.integer(GENERE == "Home"),
      DOBLE_GRAU_EST = as.integer(GRAU %in% c("Estadística","Doble Eco+Est",
        "Doble ADE+Soc","Doble ADE+Mat","Doble ADE+Dret","Doble ADE+Qui")),
      TREB_INTENS = as.integer(DEDIC %in% c("T.Parcial","T.Complet")),
      IA_HABIT = as.integer(IA_HABIT), IA_COMPR = as.integer(IA_COMPR),
      IA_REND = as.integer(IA_REND),   IA_PDFS = as.integer(IA_PDFS),
      IA_SUBST = as.integer(IA_SUBST), IA_ATENC = as.integer(IA_ATENC),
      IA_CONF = as.integer(IA_CONF),   GRUP_ASSIST = as.character(GRUP_ASSIST)
    )
    cc <- complete.cases(df_tmp[, vars_clust])
    set.seed(1234)
    idx_part <- caret::createDataPartition(df_tmp$GRUP_ASSIST[cc], p = 0.80, list = FALSE)
    idx_global_train <- which(cc)[idx_part]
  }, error = function(e) {
    idx_global_train <<- seq_len(nrow(X_train))
    message("Fallback idx: ", e$message)
  })
} else if (DATA_OK) {
  idx_global_train <- seq_len(nrow(X_train))
}

# ── Model RF (opcional) ──────────────────────────────────────
rf_model <- if (requireNamespace("ranger", quietly = TRUE) && file.exists(PATH_RF)) {
  tryCatch(readRDS(PATH_RF), error = function(e) NULL)
} else NULL

# ── Estructura factors EFA (EFA definitiva) ──────────────────
# Ítems i pesos (λ ≥ 0.30 per al factor primari, EFA oblimin final)
efa_factors <- list(
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

efa_labels <- c(
  MOT_DESMOTIVACIO  = "Desmotivació pedagògica",
  MOT_AUTOGESTIO    = "Autogestió (no necessita classe)",
  MOT_FORCA_MAJOR   = "Força major (feina/família/salut)",
  EST_QUALITAT_DOC  = "Qualitat docent percebuda",
  EST_AVALUACIO_AC  = "Avaluació continuada",
  EST_TEMPS_CLASSE  = "Gestió del temps de classe",
  EST_GRUPS_REDUITS = "Preferència grups reduïts",
  IA_EINA_ESTUDI    = "IA com a eina d'estudi"
)

# Direcció de risc (signe rho Spearman amb GRUP_ASSIST = Regular)
# -1 = factor de risc (alt → irregular)  |  +1 = protector o ns
efa_risc_dir <- c(
  MOT_DESMOTIVACIO  = -1,
  MOT_AUTOGESTIO    = -1,
  MOT_FORCA_MAJOR   = +1,
  EST_QUALITAT_DOC  = +1,
  EST_AVALUACIO_AC  = +1,
  EST_TEMPS_CLASSE  = +1,
  EST_GRUPS_REDUITS = +1,
  IA_EINA_ESTUDI    = -1
)

items_mot  <- unique(unlist(lapply(
  efa_factors[c("MOT_DESMOTIVACIO","MOT_AUTOGESTIO","MOT_FORCA_MAJOR")], names)))
items_est  <- unique(unlist(lapply(
  efa_factors[c("EST_QUALITAT_DOC","EST_AVALUACIO_AC","EST_TEMPS_CLASSE","EST_GRUPS_REDUITS")], names)))

vars_nou   <- if (DATA_OK) vars_clust else character(0)
# IA items ja estan a vars_clust → no cal duplicar-los a vars_antic
vars_antic_extra <- if (DATA_OK) c(items_mot, items_est) else character(0)
vars_antic       <- if (DATA_OK) c(vars_clust, vars_antic_extra) else character(0)

# Columnes RAW que l'usuari omple a la plantilla Excel (secció blava)
vars_nou_raw <- c("EDAT","DESPL","N_ASSIG","NOTA","T_AVAL","CURS","GENERE","GRAU","DEDIC",
                  "IA_HABIT","IA_COMPR","IA_REND","IA_PDFS","IA_SUBST","IA_ATENC","IA_CONF")
grau_doble_vals <- c("Estadística","Doble Eco+Est","Doble ADE+Soc",
                     "Doble ADE+Mat","Doble ADE+Dret","Doble ADE+Qui")

# Transforma les columnes RAW de la plantilla a les variables model
preparar_df <- function(df) {
  if (all(c("NOTA_num","T_AVAL_num","CURS_1R","TREB_INTENS") %in% names(df))) return(df)
  df$NOTA_num       <- suppressWarnings(as.numeric(df$NOTA))
  df$T_AVAL_num     <- as.integer(!is.na(df$T_AVAL) & df$T_AVAL == "Continuada")
  df$CURS_1R        <- as.integer(!is.na(df$CURS)   & df$CURS   == "1r")
  df$GENERE_Home    <- as.integer(!is.na(df$GENERE) & df$GENERE == "Home")
  df$DOBLE_GRAU_EST <- as.integer(!is.na(df$GRAU)   & df$GRAU   %in% grau_doble_vals)
  df$TREB_INTENS    <- as.integer(!is.na(df$DEDIC)  & df$DEDIC  %in% c("T.Parcial","T.Complet"))
  df
}

# ── Funció: calcular scores EFA ponderats ────────────────────
# Mitjana ponderada pels pesos (λ) dels ítems disponibles
calcular_scores_efa <- function(fila) {
  sapply(names(efa_factors), function(fn) {
    items <- efa_factors[[fn]]
    nms   <- names(items)
    vals  <- suppressWarnings(as.numeric(fila[nms]))
    ok    <- !is.na(vals)
    if (sum(ok) == 0) return(NA_real_)
    round(sum(vals[ok] * items[ok]) / sum(items[ok]), 3)
  })
}

# ── Funcions predicció ───────────────────────────────────────
pred_rf <- function(av) {
  if (is.null(rf_model)) return(NULL)
  x_df <- as.data.frame(matrix(av[vars_clust], nrow = 1,
                                dimnames = list(NULL, vars_clust)))
  tryCatch({
    preds <- predict(rf_model, data = x_df)$predictions
    if (is.matrix(preds)) {
      col_r <- grep("Regular", colnames(preds), fixed = FALSE)[1]
      prob  <- preds[1, col_r]
    } else prob <- as.numeric(preds[1])
    list(prob = round(as.numeric(prob), 4),
         pred = ifelse(prob >= THRESH_RF, "Regular", "Irregular"))
  }, error = function(e) NULL)
}

pred_knn <- function(av) {
  tryCatch(predict_nou_alumne(av), error = function(e) NULL)
}

# ── Helper: processar batch CSV ──────────────────────────────
processar_batch <- function(df, mode = c("nou","antic")) {
  mode <- match.arg(mode)
  n    <- nrow(df)
  withProgress(message = "Processant alumnes…", value = 0, {
    rows <- lapply(seq_len(n), function(i) {
      incProgress(1 / n)
      av    <- setNames(as.numeric(unlist(df[i, vars_clust])), vars_clust)
      rf_r  <- tryCatch(pred_rf(av),  error = function(e) NULL)
      knn_r <- tryCatch(pred_knn(av), error = function(e) NULL)

      base <- data.frame(
        Alumne    = if ("id" %in% names(df)) df[i, "id"] else i,
        Prediccio = if (!is.null(rf_r)) rf_r$pred
                    else if (!is.null(knn_r)) knn_r$prediccio
                    else NA_character_,
        Prob_pct  = if (!is.null(rf_r)) round(rf_r$prob * 100, 1)
                    else if (!is.null(knn_r)) round(knn_r$prob_regular * 100, 1)
                    else NA_real_,
        u1_pct    = if (!is.null(knn_r)) round(knn_r$u1 * 100, 1) else NA_real_,
        u2_pct    = if (!is.null(knn_r)) round(knn_r$u2 * 100, 1) else NA_real_,
        Cluster   = if (!is.null(knn_r)) knn_r$cluster_dominant else NA_integer_,
        stringsAsFactors = FALSE
      )

      if (mode == "antic") {
        fila_raw <- setNames(as.numeric(unlist(df[i, ])), names(df))
        scores   <- calcular_scores_efa(fila_raw)
        for (nm in names(scores)) base[[nm]] <- scores[[nm]]
      }
      base
    })
  })
  do.call(rbind, rows)
}

# ── UI ───────────────────────────────────────────────────────
ui <- navbarPage(
  "Absentisme Universitari — TFG FEE/UB",
  theme       = shinythemes::shinytheme("flatly"),
  header      = tags$head(tags$link(rel = "stylesheet", href = "custom.css")),
  collapsible = TRUE,

  # ════════════════════════════════════════════════════════════
  # TAB 1 — ALUMNAT NOU
  # ════════════════════════════════════════════════════════════
  tabPanel("Alumnat nou",
    fluidRow(style = "padding: 24px;",
      column(4,
        tags$h4("Pujar fitxer CSV — alumnat nou"),
        fileInput("csv_nou", NULL, accept = ".csv",
                  buttonLabel = "Triar fitxer…",
                  placeholder = "Cap fitxer seleccionat"),
        tags$p(tags$em(
          "CSV exportat de la plantilla Excel (secció blava). Les transformacions es calculen automàticament."
        )),
        tags$details(style = "margin-bottom: 10px;",
          tags$summary("Columnes requerides (16)"),
          tags$code(style = "font-size:11px;",
            paste(c("EDAT","DESPL","N_ASSIG","NOTA","T_AVAL","CURS",
                    "GENERE","GRAU","DEDIC",
                    "IA_HABIT","IA_COMPR","IA_REND","IA_PDFS",
                    "IA_SUBST","IA_ATENC","IA_CONF"), collapse = ", "))
        ),
        actionButton("btn_nou", "Processar alumnat nou",
          class = "btn-primary", style = "width:100%;"),
        tags$br(), tags$br(),
        downloadButton("dl_nou", "Descarregar resultats (CSV)",
          style = "width:100%;")
      ),
      column(8,
        uiOutput("nou_validacio"),
        uiOutput("nou_resum"),
        DT::dataTableOutput("nou_taula"),
        plotOutput("nou_grafic", height = "260px")
      )
    )
  ),

  # ════════════════════════════════════════════════════════════
  # TAB 2 — ALUMNAT ANTIC
  # ════════════════════════════════════════════════════════════
  tabPanel("Alumnat antic",
    fluidRow(style = "padding: 24px;",
      column(4,
        tags$h4("Pujar fitxer CSV — alumnat antic"),
        fileInput("csv_antic", NULL, accept = ".csv",
                  buttonLabel = "Triar fitxer…",
                  placeholder = "Cap fitxer seleccionat"),
        tags$p(tags$em(
          "CSV exportat de la plantilla Excel (secció blava) incloent ítems de motius (M_*) i estratègies (E_*)."
        )),
        tags$details(style = "margin-bottom: 10px;",
          tags$summary("Columnes requerides"),
          tags$div(
            tags$p(tags$strong("Pre-curs:"), style = "margin:4px 0;"),
            tags$code(style = "font-size:11px;",
              paste(c("EDAT","DESPL","N_ASSIG","NOTA","T_AVAL","CURS",
                      "GENERE","GRAU","DEDIC",
                      "IA_HABIT","IA_COMPR","IA_REND","IA_PDFS",
                      "IA_SUBST","IA_ATENC","IA_CONF"), collapse = ", ")),
            tags$p(tags$strong("Motius (Likert):"), style = "margin:8px 0 4px;"),
            tags$code(style = "font-size:11px;", paste(items_mot, collapse = ", ")),
            tags$p(tags$strong("Estratègies (Likert):"), style = "margin:8px 0 4px;"),
            tags$code(style = "font-size:11px;", paste(items_est, collapse = ", "))
          )
        ),
        actionButton("btn_antic", "Processar alumnat antic",
          class = "btn-success", style = "width:100%;"),
        tags$br(), tags$br(),
        downloadButton("dl_antic", "Descarregar resultats (CSV)",
          style = "width:100%;")
      ),
      column(8,
        uiOutput("antic_validacio"),
        uiOutput("antic_resum"),
        DT::dataTableOutput("antic_taula"),
        tags$hr(),
        uiOutput("antic_efa_title"),
        plotOutput("antic_efa_grafic", height = "320px"),
        uiOutput("antic_efa_peu")
      )
    )
  )
)

# ── Lector CSV robust (gestiona ; vs , i files de capçalera) ─
llegir_csv_plantilla <- function(filepath) {
  raw <- tryCatch(readLines(filepath, n = 20, warn = FALSE),
                  error = function(e) return(NULL))
  if (is.null(raw)) return(NULL)

  # Detectar separador: compte de ; vs ,
  n_semi  <- sum(nchar(raw) - nchar(gsub(";", "", raw, fixed = TRUE)))
  n_comma <- sum(nchar(raw) - nchar(gsub(",", "", raw, fixed = TRUE)))
  sep <- if (n_semi > n_comma) ";" else ","

  # Trobar la fila que conté "EDAT" (la fila real de capçaleres)
  header_idx <- which(sapply(raw, function(l) grepl("\\bEDAT\\b", l)))[1]
  skip_n <- if (is.na(header_idx) || header_idx <= 1) 0 else header_idx - 1

  tryCatch(
    read.csv(filepath, sep = sep, skip = skip_n,
             stringsAsFactors = FALSE, check.names = TRUE),
    error = function(e) NULL
  )
}

# ── SERVER ───────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── TAB 1: Alumnat nou ──────────────────────────────────────

  df_nou <- reactive({
    req(input$csv_nou)
    llegir_csv_plantilla(input$csv_nou$datapath)
  })

  output$nou_validacio <- renderUI({
    df <- df_nou(); if (is.null(df)) return(NULL)
    if (!DATA_OK)
      return(tags$div(class = "alert alert-danger",
        "El model no s'ha pogut carregar. Comprova la ruta: ",
        tags$code(PATH_RDATA)))
    manquen <- setdiff(vars_nou_raw, names(df))
    if (length(manquen) > 0)
      return(tags$div(class = "alert alert-danger",
        tags$strong("Columnes que falten: "), paste(manquen, collapse = ", ")))
    tags$div(class = "alert alert-success",
      sprintf("✔  Fitxer vàlid: %d alumnes, %d columnes.", nrow(df), ncol(df)))
  })

  res_nou <- eventReactive(input$btn_nou, {
    req(df_nou(), DATA_OK)
    df <- preparar_df(df_nou())
    manquen <- setdiff(vars_clust, names(df))
    validate(need(length(manquen) == 0,
      paste("Falten columnes al CSV:", paste(manquen, collapse = ", "),
            "| Columnes del CSV:", paste(names(df), collapse = ", "))))
    processar_batch(df, mode = "nou")
  })

  output$nou_resum <- renderUI({
    df <- res_nou(); if (is.null(df)) return(NULL)
    n <- nrow(df); n_irr <- sum(grepl("Irregular", df$Prediccio), na.rm = TRUE)
    tags$div(class = "alert alert-info", style = "margin-top:10px;",
      tags$strong(sprintf("%d alumnes analitzats | %d en risc irregular (%.1f%%)",
        n, n_irr, n_irr / n * 100)))
  })

  output$nou_taula <- DT::renderDataTable({
    df <- req(res_nou())
    cols <- intersect(c("Alumne","Prediccio","Prob_pct","u1_pct","u2_pct","Cluster"), names(df))
    etq  <- c("Alumne","Predicció","P(Regular)%","u1 Cl.1 %","u2 Cl.2 %","Cluster")[seq_along(cols)]
    DT::datatable(df[, cols], rownames = FALSE, colnames = etq,
                  options = list(pageLength = 15, scrollX = TRUE)) |>
      DT::formatStyle("Prediccio",
        backgroundColor = DT::styleEqual(
          c("Regular","Irregular"), c("#d4edda","#f8d7da")))
  })

  output$nou_grafic <- renderPlot({
    df <- req(res_nou())
    ggplot(df, aes(x = Prob_pct, fill = Prediccio)) +
      geom_histogram(bins = 20, color = "white", alpha = 0.85) +
      geom_vline(xintercept = THRESH_RF * 100, linetype = "dashed",
                 color = "grey30", linewidth = 0.9) +
      annotate("text", x = THRESH_RF * 100 + 1, y = Inf,
               label = sprintf("Llindar %.0f%%", THRESH_RF * 100),
               vjust = 2, hjust = 0, color = "grey30", size = 3.5) +
      scale_fill_manual(
        values = c("Regular" = COL_REG, "Irregular" = COL_IRR),
        na.value = "grey70", name = NULL) +
      labs(title = "Distribució de probabilitats — Alumnat nou",
           x = "P(Regular) %", y = "Alumnes") +
      theme_minimal(base_size = 12) + theme(legend.position = "top")
  })

  output$dl_nou <- downloadHandler(
    filename = function() paste0("prediccions_nou_", Sys.Date(), ".csv"),
    content  = function(file) {
      df <- res_nou(); if (!is.null(df)) write.csv(df, file, row.names = FALSE)
    }
  )

  # ── TAB 2: Alumnat antic ────────────────────────────────────

  df_antic <- reactive({
    req(input$csv_antic)
    llegir_csv_plantilla(input$csv_antic$datapath)
  })

  output$antic_validacio <- renderUI({
    df <- df_antic(); if (is.null(df)) return(NULL)
    if (!DATA_OK)
      return(tags$div(class = "alert alert-danger",
        "El model no s'ha pogut carregar. Ruta: ", tags$code(PATH_RDATA)))
    manquen_raw <- setdiff(vars_nou_raw, names(df))
    if (length(manquen_raw) > 0)
      return(tags$div(class = "alert alert-danger",
        tags$strong("Variables pre-curs que falten: "),
        paste(manquen_raw, collapse = ", ")))
    manquen_mot <- setdiff(items_mot, names(df))
    manquen_est <- setdiff(items_est, names(df))
    warns <- c(
      if (length(manquen_mot) > 0) paste("Motius sense dades:", paste(manquen_mot, collapse=", ")),
      if (length(manquen_est) > 0) paste("Estratègies sense dades:", paste(manquen_est, collapse=", "))
    )
    if (length(warns) > 0)
      return(tags$div(class = "alert alert-warning",
        tags$strong("Avís — factors parcials: "), tags$br(),
        paste(warns, collapse = " | ")))
    tags$div(class = "alert alert-success",
      sprintf("✔  Fitxer vàlid: %d alumnes, %d columnes.", nrow(df), ncol(df)))
  })

  res_antic <- eventReactive(input$btn_antic, {
    req(df_antic(), DATA_OK)
    df <- preparar_df(df_antic())
    validate(need(length(setdiff(vars_clust, names(df))) == 0,
                  "Falten les variables pre-curs al CSV."))
    processar_batch(df, mode = "antic")
  })

  output$antic_resum <- renderUI({
    df <- res_antic(); if (is.null(df)) return(NULL)
    n <- nrow(df); n_irr <- sum(grepl("Irregular", df$Prediccio), na.rm = TRUE)
    tags$div(class = "alert alert-info", style = "margin-top:10px;",
      tags$strong(sprintf("%d alumnes analitzats | %d en risc irregular (%.1f%%)",
        n, n_irr, n_irr / n * 100)))
  })

  output$antic_taula <- DT::renderDataTable({
    df <- req(res_antic())
    efa_cols  <- intersect(names(efa_factors), names(df))
    base_cols <- intersect(c("Alumne","Prediccio","Prob_pct","u1_pct","u2_pct"), names(df))
    cols_show <- c(base_cols, efa_cols)
    col_noms  <- c("Alumne","Predicció","P(Regular)%","u1 Cl.1%","u2 Cl.2%",
                   efa_labels[efa_cols])[seq_along(cols_show)]
    DT::datatable(df[, cols_show], rownames = FALSE, colnames = col_noms,
                  options = list(pageLength = 15, scrollX = TRUE)) |>
      DT::formatStyle("Prediccio",
        backgroundColor = DT::styleEqual(
          c("Regular","Irregular"), c("#d4edda","#f8d7da"))) |>
      DT::formatRound(efa_cols, digits = 2)
  })

  output$antic_efa_title <- renderUI({
    req(res_antic())
    tags$h5("Perfil de factors EFA (mitjana del grup processat)")
  })

  output$antic_efa_grafic <- renderPlot({
    df       <- req(res_antic())
    efa_cols <- intersect(names(efa_factors), names(df))
    if (length(efa_cols) == 0) return(NULL)

    df_long <- df |>
      select(Prediccio, all_of(efa_cols)) |>
      pivot_longer(all_of(efa_cols), names_to = "factor", values_to = "score") |>
      filter(!is.na(score)) |>
      group_by(factor, Prediccio) |>
      summarise(m = mean(score, na.rm = TRUE), .groups = "drop") |>
      mutate(
        etiqueta = factor(efa_labels[factor],
                          levels = rev(efa_labels[efa_cols])),
        color_bar = ifelse(efa_risc_dir[factor] == -1, COL_IRR, COL_C1)
      )

    ggplot(df_long, aes(x = etiqueta, y = m, fill = Prediccio)) +
      geom_col(position = position_dodge(0.65), width = 0.55, alpha = 0.85) +
      coord_flip() +
      scale_fill_manual(
        values = c("Regular" = COL_REG, "Irregular" = COL_IRR),
        na.value = "grey70", name = "Predicció") +
      labs(title = "Puntuació mitjana dels factors EFA per grup",
           subtitle = "Valors més alts = el factor afecta més l'alumne",
           x = NULL, y = "Puntuació ponderada (escala Likert)") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top",
            axis.text.y = element_text(size = 10))
  })

  output$antic_efa_peu <- renderUI({
    req(res_antic())
    tags$p(tags$em(
      "Factors de risc (correlació negativa amb assistència regular): ",
      tags$strong("Desmotivació pedagògica, Autogestió, IA eina d'estudi."),
      " Els factors d'estratègies no presenten correlació significativa amb l'assistència."
    ))
  })

  output$dl_antic <- downloadHandler(
    filename = function() paste0("prediccions_antic_", Sys.Date(), ".csv"),
    content  = function(file) {
      df <- res_antic(); if (!is.null(df)) write.csv(df, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
