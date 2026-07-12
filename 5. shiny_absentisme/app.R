# ============================================================
#  app.R — Predicció Absentisme Universitari (TFG FEE/UB)
#  Tab 1: Alumnat nou  → KNN fuzzy P(Irregular) + LDA (u1)
#  Tab 2: Alumnat antic → RF-A
# ============================================================

suppressPackageStartupMessages({
  library(shiny)
  library(shinythemes)
  library(dplyr)
  library(ggplot2)
  library(plotly)
  library(DT)
  library(tidyr)
})

# ── Constants ────────────────────────────────────────────────
COL_C1 <- "#4A90B8"
COL_C2 <- "#E07B54"
COL_REG <- "#27AE60"
COL_IRR <- "#E74C3C"

THRESH_RF <- 0.5687   # thresh_pr_a RF-A: P(Regular) >= 0.5687 → Regular

# ── Localitzar directori base del TFG ───────────────────────
find_tfg_dir <- function() {
  target <- file.path("2. Dades", "2. Models", "fuzzy_clustering_model_complet.RData")
  dirs <- c(getwd(), dirname(getwd()), "C:/Users/Edurne/Downloads/TFG")
  for (d in dirs) if (file.exists(file.path(d, target))) return(normalizePath(d))
  getwd()
}
TFG_DIR <- find_tfg_dir()
PATH_RDATA <- file.path(TFG_DIR, "2. Dades", "2. Models", "fuzzy_clustering_model_complet.RData")
PATH_RF <- file.path(TFG_DIR, "2. Dades", "2. Models", "model_rf_a.rds")

# ── Carregar RData principal ─────────────────────────────────
env_m <- new.env()
DATA_OK <- tryCatch({
  load(PATH_RDATA, envir = env_m); TRUE
}, error = function(e) {
  message("RData no trobat: ", PATH_RDATA, "\nError: ", e$message); FALSE
})
if (DATA_OK) { for (nm in ls(env_m)) assign(nm, get(nm, envir = env_m)) }
rm(env_m)

# Llindars carregats del RData (amb fallback si el RData és antic)
THRESH_LDA_U1 <- if (DATA_OK && exists("thresh_lda_u1")) thresh_lda_u1 else 0.368
THRESH_KNN_IRR <- if (DATA_OK && exists("knn_model")) (1 - knn_model$thresh_pr) * 100 else 43.5

# ── Model RF-A (opcional) ────────────────────────────────────
rf_model <- if (requireNamespace("ranger", quietly = TRUE) && file.exists(PATH_RF)) {
  tryCatch(readRDS(PATH_RF), error = function(e) NULL)
} else NULL

# ── Estructura factors EFA ────────────────────────────────────
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

efa_risc_dir <- c(
  MOT_DESMOTIVACIO  = -1, MOT_AUTOGESTIO    = -1, MOT_FORCA_MAJOR   = +1,
  EST_QUALITAT_DOC  = +1, EST_AVALUACIO_AC  = +1, EST_TEMPS_CLASSE  = +1,
  EST_GRUPS_REDUITS = +1, IA_EINA_ESTUDI    = -1
)

items_mot <- unique(unlist(lapply(
  efa_factors[c("MOT_DESMOTIVACIO","MOT_AUTOGESTIO","MOT_FORCA_MAJOR")], names)))
items_est <- unique(unlist(lapply(
  efa_factors[c("EST_QUALITAT_DOC","EST_AVALUACIO_AC","EST_TEMPS_CLASSE","EST_GRUPS_REDUITS")], names)))

vars_nou <- if (DATA_OK) vars_clust else character(0)
vars_antic_extra <- if (DATA_OK) c(items_mot, items_est) else character(0)
vars_antic <- if (DATA_OK) c(vars_clust, vars_antic_extra) else character(0)

vars_nou_raw <- c("EDAT","DESPL","N_ASSIG","NOTA","T_AVAL","CURS","GENERE","GRAU","DEDIC",
                  "IA_HABIT","IA_COMPR","IA_REND","IA_PDFS","IA_SUBST","IA_ATENC","IA_CONF")
grau_doble_vals <- c("Estadística","Doble Eco+Est","Doble ADE+Soc",
                     "Doble ADE+Mat","Doble ADE+Dret","Doble ADE+Qui")

# Transforma columnes RAW de la plantilla a les variables del model
preparar_df <- function(df) {
  if (all(c("NOTA_num","T_AVAL_num","CURS_1R","TREB_INTENS_num") %in% names(df))) return(df)
  df$NOTA_num <- suppressWarnings(as.numeric(df$NOTA))
  df$T_AVAL_num <- as.integer(!is.na(df$T_AVAL) & df$T_AVAL == "Continuada")
  df$CURS_1R <- as.integer(!is.na(df$CURS)   & df$CURS   == "1r")
  df$GENERE_Home <- as.integer(!is.na(df$GENERE) & df$GENERE == "Home")
  df$DOBLE_GRAU_EST <- as.integer(!is.na(df$GRAU)   & df$GRAU   %in% grau_doble_vals)
  df$TREB_INTENS_num <- as.integer(!is.na(df$DEDIC)  & df$DEDIC  %in% c("T.Parcial","T.Complet"))
  if ("IA_ATENC" %in% names(df))
    df$IA_ATENC_num <- suppressWarnings(as.numeric(df$IA_ATENC))
  df
}

# ── Càlcul scores EFA ponderats ──────────────────────────────
calcular_scores_efa <- function(fila) {
  sapply(names(efa_factors), function(fn) {
    items <- efa_factors[[fn]]
    nms <- names(items)
    vals <- suppressWarnings(as.numeric(fila[nms]))
    ok <- !is.na(vals)
    if (sum(ok) == 0) return(NA_real_)
    round(sum(vals[ok] * items[ok]) / sum(items[ok]), 3)
  })
}

# Variables RF-A (ha de coincidir amb vars_rfa del script 8. Random forest.R)
vars_rfa <- c(
  "MOT_DESMOTIVACIO", "MOT_FORCA_MAJOR",
  "EST_AVALUACIO_AC",
  "IA_SUBST_num", "IA_ATENC_num",
  "T_AVAL_num", "CURS_1R_num", "EDAT", "NOTA_num",
  "MOT_AUTOGESTIO", "DOBLE_GRAU_EST", "TREB_INTENS",
  "EST_QUALITAT_DOC", "EST_TEMPS_CLASSE", "EST_GRUPS_REDUITS",
  "IA_EINA_ESTUDI",
  "DESPL", "N_ASSIG", "GENERE_Home"
)

# ── Funcions de predicció ────────────────────────────────────
pred_rf <- function(av_rf) {
  if (is.null(rf_model)) return(NULL)
  x_df <- as.data.frame(as.list(av_rf))
  tryCatch({
    preds <- predict(rf_model, data = x_df)$predictions
    if (is.matrix(preds)) {
      cn <- colnames(preds)
      col_r <- grep("Regular|^1$", cn)[1]   # "Regular" o "1" (Y=1=Regular en codificació numèrica)
      if (is.na(col_r)) col_r <- 1          # fallback: primera columna
      prob <- preds[1, col_r]
    } else prob <- as.numeric(preds[1])
    list(prob = round(as.numeric(prob), 4),
         pred = ifelse(prob >= THRESH_RF, "Regular", "Irregular"))
  }, error = function(e) NULL)
}

pred_knn <- function(av) {
  tryCatch(predict_nou_alumne(av), error = function(e) NULL)
}

# ── Processar batch CSV ──────────────────────────────────────
processar_batch <- function(df, mode = c("nou","antic")) {
  mode <- match.arg(mode)
  n <- nrow(df)
  withProgress(message = "Processant alumnes…", value = 0, {
    rows <- lapply(seq_len(n), function(i) {
      incProgress(1 / n)
      av <- setNames(as.numeric(unlist(df[i, vars_clust])), vars_clust)

      if (mode == "nou") {
        # KNN fuzzy → P(Irregular)  |  llindar PR KNN → classificació binària
        knn_r <- tryCatch(pred_knn(av), error = function(e) NULL)
        u1_val <- if (!is.null(knn_r)) knn_r$u1 else NA_real_
        p_irr_pct <- if (!is.null(knn_r))
          round((1 - knn_r$prob_regular) * 100, 1) else NA_real_
        knn_pred <- if (!is.null(knn_r))
          ifelse(p_irr_pct >= THRESH_KNN_IRR, "Irregular", "Regular")
        else NA_character_

        data.frame(
          Alumne          = if ("id" %in% names(df)) df[i, "id"] else i,
          Prediccio       = knn_pred,
          P_Irregular_pct = p_irr_pct,
          u1_pct  = if (!is.null(knn_r)) round(u1_val * 100, 1) else NA_real_,
          u2_pct  = if (!is.null(knn_r)) round(knn_r$u2 * 100, 1) else NA_real_,
          Cluster = if (!is.null(knn_r)) knn_r$cluster_dominant else NA_integer_,
          stringsAsFactors = FALSE
        )

      } else {
        # RF-A: el CSV de la plantilla ja té els factor scores calculats
        rfa_avail <- intersect(vars_rfa, names(df))
        av_rf <- setNames(as.numeric(unlist(df[i, rfa_avail])), rfa_avail)
        rf_r <- tryCatch(pred_rf(av_rf), error = function(e) NULL)

        base <- data.frame(
          Alumne           = if ("id" %in% names(df)) df[i, "id"] else i,
          Prediccio        = if (!is.null(rf_r)) rf_r$pred else NA_character_,
          P_Irregular_pct  = if (!is.null(rf_r))
            round((1 - rf_r$prob) * 100, 1) else NA_real_,
          stringsAsFactors = FALSE
        )
        # Factor scores del CSV directament (per al gràfic EFA)
        for (nm in intersect(names(efa_factors), names(df)))
          base[[nm]] <- suppressWarnings(as.numeric(df[i, nm]))
        # Variables clau del model (per al gràfic de variables)
        for (nm in c("IA_SUBST_num","T_AVAL_num","NOTA_num","TREB_INTENS","CURS_1R_num"))
          if (nm %in% names(av_rf)) base[[nm]] <- as.numeric(av_rf[nm])
        base
      }
    })
  })
  do.call(rbind, rows)
}

# ── UI ───────────────────────────────────────────────────────
ui <- navbarPage(
  "Absentisme Universitari — TFG FEE/UB",
  theme       = shinythemes::shinytheme("flatly"),
  header      = tags$head(
    tags$link(rel = "stylesheet", href = "custom.css"),
    tags$script(HTML(
      "document.addEventListener('toggle', function(e) {",
      "  if (e.target.tagName === 'DETAILS' && e.target.open) {",
      "    setTimeout(function() { window.dispatchEvent(new Event('resize')); }, 80);",
      "  }",
      "}, true);"
    ))
  ),
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
        tags$details(style = "margin-top:12px; margin-bottom:8px;",
          tags$summary("Comprova les variables computades pel model"),
          DT::dataTableOutput("nou_vars_debug_dt")
        ),
        tags$details(style = "margin-top:8px; margin-bottom:8px;",
          tags$summary("Veure gràfic de distribució"),
          plotlyOutput("nou_grafic", height = "260px")
        ),
        tags$details(style = "margin-top:8px; margin-bottom:8px;",
          tags$summary("Perfil comparatiu dels clusters"),
          DT::dataTableOutput("nou_clusters_taula")
        )
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
        tags$details(style = "margin-top:12px; margin-bottom:8px;",
          tags$summary("Comprova les variables computades pel model"),
          DT::dataTableOutput("antic_vars_debug_dt")
        ),
        tags$details(style = "margin-top:8px; margin-bottom:8px;",
          tags$summary("Perfil EFA i variables clau del model"),
          plotlyOutput("antic_efa_grafic",  height = "340px"),
          tags$div(style = "margin-top:12px;",
            plotlyOutput("antic_vars_grafic", height = "230px")
          ),
          uiOutput("antic_efa_peu")
        )
      )
    )
  )
)

# ── Lector CSV robust ────────────────────────────────────────
llegir_csv_plantilla <- function(filepath) {
  raw <- tryCatch(readLines(filepath, n = 20, warn = FALSE),
                  error = function(e) return(NULL))
  if (is.null(raw)) return(NULL)
  n_semi <- sum(nchar(raw) - nchar(gsub(";", "", raw, fixed = TRUE)))
  n_comma <- sum(nchar(raw) - nchar(gsub(",", "", raw, fixed = TRUE)))
  sep <- if (n_semi > n_comma) ";" else ","
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

  df_nou_prep <- eventReactive(input$btn_nou, {
    req(df_nou(), DATA_OK)
    preparar_df(df_nou())
  })

  output$nou_vars_debug_dt <- DT::renderDataTable({
    df <- df_nou_prep()
    raw_show <- intersect(c("NOTA","T_AVAL","CURS","GENERE","GRAU","DEDIC"), names(df))
    comp_show <- intersect(c("NOTA_num","T_AVAL_num","CURS_1R","GENERE_Home",
                             "DOBLE_GRAU_EST","TREB_INTENS_num"), names(df))
    id_col <- if ("id" %in% names(df)) df$id else seq_len(nrow(df))
    out <- cbind(data.frame(Alumne = id_col),
                 df[, raw_show, drop = FALSE],
                 df[, comp_show, drop = FALSE])
    DT::datatable(out, rownames = FALSE,
      caption = "Variables raw → computades (entrada real al model)",
      options = list(pageLength = 15, scrollX = TRUE, dom = "tip"))
  })

  output$nou_resum <- renderUI({
    df <- res_nou(); if (is.null(df)) return(NULL)
    n <- nrow(df)
    n_irr <- sum(grepl("Irregular", df$Prediccio), na.rm = TRUE)
    tags$div(class = "alert alert-info", style = "margin-top:10px;",
      tags$strong(sprintf(
        "%d alumnes analitzats | %d en risc irregular (%.1f%%)",
        n, n_irr, n_irr / n * 100)))
  })

  output$nou_taula <- DT::renderDataTable({
    df <- req(res_nou())
    cols <- intersect(
      c("Alumne","Prediccio","P_Irregular_pct","u1_pct","u2_pct","Cluster"),
      names(df))
    etq <- c("Alumne","Predicció (KNN)","P(Irregular)%",
             "u1 Cluster 1 %","u2 Cluster 2 %","Cluster dom.")[seq_along(cols)]
    df_show <- df[, cols, drop = FALSE]
    names(df_show) <- etq
    DT::datatable(df_show, rownames = FALSE,
                  options = list(pageLength = 15, scrollX = TRUE)) |>
      DT::formatStyle("Predicció (KNN)",
        backgroundColor = DT::styleEqual(
          c("Regular","Irregular"), c("#d4edda","#f8d7da")))
  })

  output$nou_grafic <- renderPlotly({
    df <- req(res_nou())
    df_reg <- df[!is.na(df$Prediccio) & df$Prediccio == "Regular", ]
    df_irr <- df[!is.na(df$Prediccio) & df$Prediccio == "Irregular", ]
    plot_ly() |>
      add_histogram(data = df_reg, x = ~P_Irregular_pct, name = "Regular",
                    nbinsx = 20, opacity = 0.85,
                    marker = list(color = COL_REG,
                                  line = list(color = "white", width = 1))) |>
      add_histogram(data = df_irr, x = ~P_Irregular_pct, name = "Irregular",
                    nbinsx = 20, opacity = 0.85,
                    marker = list(color = COL_IRR,
                                  line = list(color = "white", width = 1))) |>
      layout(
        barmode = "stack",
        title   = list(text = "Probabilitat de ser irregular - alumnat nou\n",
                       x = 0.05, font = list(size = 14)),
        xaxis   = list(title = "P(Irregular) %"),
        yaxis   = list(title = "Alumnes"),
        legend  = list(orientation = "h", x = 0, y = 1.12),
        shapes  = list(list(
          type = "line",
          x0 = THRESH_KNN_IRR, x1 = THRESH_KNN_IRR,
          y0 = 0, y1 = 1, yref = "paper",
          line = list(color = "grey40", width = 1.5, dash = "dash")
        )),
        annotations = list(list(
          x = THRESH_KNN_IRR, y = 1, yref = "paper",
          text = sprintf("Llindar %.0f%%", THRESH_KNN_IRR),
          showarrow = FALSE, xanchor = "left", yanchor = "top",
          font = list(color = "grey40", size = 11)
        ))
      ) |>
      config(responsive = TRUE)
  })

  output$dl_nou <- downloadHandler(
    filename = function() paste0("prediccions_nou_", Sys.Date(), ".csv"),
    content  = function(file) {
      df <- res_nou(); if (!is.null(df)) write.csv(df, file, row.names = FALSE)
    }
  )

  output$nou_clusters_taula <- DT::renderDataTable({
    req(DATA_OK)
    # centers_orig no està al RData complet: reconstruir des de fcm_final + scale_params
    if (!exists("fcm_final") || is.null(fcm_final[["centers"]])) return(NULL)
    centers_sc <- as.matrix(fcm_final$centers)
    if (is.null(nrow(centers_sc)) || nrow(centers_sc) < 2) return(NULL)
    # desfer l'estandardització
    centers <- sweep(sweep(centers_sc, 2, scale_params$sd, "*"),
                     2, scale_params$mean, "+")

    vars_show <- intersect(vars_clust, colnames(centers))
    if (length(vars_show) == 0) return(NULL)

    etiquetes <- c(
      EDAT = "Edat (anys)", DESPL = "Desplaçament (min)",
      N_ASSIG = "Nre. assignatures", NOTA_num = "Nota mitjana (1–5)",
      T_AVAL_num = "Avaluació continuada (0/1)",
      CURS_1R = "1r curs (0/1)", CURS_1R_num = "1r curs (0/1)",
      IA_SUBST_num = "IA substitució classe", IA_SUBST = "IA substitució classe"
    )

    df_tab <- data.frame(
      Variable  = ifelse(vars_show %in% names(etiquetes),
                         etiquetes[vars_show], vars_show),
      `Clúster 1 — Regular`   = round(centers[1, vars_show], 2),
      `Clúster 2 — Irregular` = round(centers[2, vars_show], 2),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    df_tab$Diferència <- round(
      df_tab[["Clúster 1 — Regular"]] - df_tab[["Clúster 2 — Irregular"]], 2)

    DT::datatable(df_tab, rownames = FALSE,
      caption = "Valor mitjà per variable a cada clúster (escala original)",
      options = list(pageLength = 15, dom = "t", ordering = FALSE)) |>
      DT::formatStyle("Diferència",
        color = DT::styleInterval(c(-0.001, 0.001),
                                  c("#E74C3C", "black", "#27AE60")),
        fontWeight = "bold")
  })

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
    efa_noms <- names(efa_factors)
    efa_precalc <- all(efa_noms %in% names(df))
    if (!efa_precalc) {
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
    }
    tags$div(class = "alert alert-success",
      sprintf("✔  Fitxer vàlid: %d alumnes, %d columnes.%s",
              nrow(df), ncol(df),
              if (efa_precalc) " | Scores EFA precalculats detectats." else ""))
  })

  res_antic <- eventReactive(input$btn_antic, {
    req(df_antic(), DATA_OK)
    df <- preparar_df(df_antic())
    validate(need(length(setdiff(vars_clust, names(df))) == 0,
                  "Falten les variables pre-curs al CSV."))
    processar_batch(df, mode = "antic")
  })

  df_antic_prep <- eventReactive(input$btn_antic, {
    req(df_antic(), DATA_OK)
    preparar_df(df_antic())
  })

  output$antic_vars_debug_dt <- DT::renderDataTable({
    df <- df_antic_prep()
    raw_show <- intersect(c("NOTA","T_AVAL","CURS","GENERE","GRAU","DEDIC"), names(df))
    comp_show <- intersect(c("NOTA_num","T_AVAL_num","CURS_1R","GENERE_Home",
                             "DOBLE_GRAU_EST","TREB_INTENS_num"), names(df))
    efa_show <- intersect(names(efa_factors), names(df))
    id_col <- if ("id" %in% names(df)) df$id else seq_len(nrow(df))
    out <- cbind(data.frame(Alumne = id_col),
                 df[, raw_show, drop = FALSE],
                 df[, comp_show, drop = FALSE],
                 df[, efa_show, drop = FALSE])
    dt <- DT::datatable(out, rownames = FALSE,
      caption = "Variables raw → computades + scores EFA (entrada real al model)",
      options = list(pageLength = 15, scrollX = TRUE, dom = "tip"))
    if (length(efa_show) > 0) dt <- DT::formatRound(dt, efa_show, digits = 3)
    dt
  })

  output$antic_resum <- renderUI({
    df <- res_antic(); if (is.null(df)) return(NULL)
    n <- nrow(df)
    n_irr <- sum(grepl("Irregular", df$Prediccio), na.rm = TRUE)
    tags$div(class = "alert alert-info", style = "margin-top:10px;",
      tags$strong(sprintf(
        "%d alumnes analitzats | %d en risc irregular (%.1f%%)",
        n, n_irr, n_irr / n * 100)))
  })

  output$antic_taula <- DT::renderDataTable({
    df <- req(res_antic())
    efa_cols <- intersect(names(efa_factors), names(df))
    base_cols <- intersect(c("Alumne","Prediccio","P_Irregular_pct"), names(df))
    cols_show <- c(base_cols, efa_cols)
    col_noms <- c("Alumne","Predicció","P(Irregular)%",
                   efa_labels[efa_cols])[seq_along(cols_show)]
    df_show <- df[, cols_show, drop = FALSE]
    names(df_show) <- col_noms
    dt <- DT::datatable(df_show, rownames = FALSE,
                        options = list(pageLength = 15, scrollX = TRUE)) |>
      DT::formatStyle("Predicció",
        backgroundColor = DT::styleEqual(
          c("Regular","Irregular"), c("#d4edda","#f8d7da")))
    if (length(efa_cols) > 0)
      dt <- DT::formatRound(dt, efa_labels[efa_cols], digits = 2)
    dt
  })

  output$antic_efa_grafic <- renderPlotly({
    df <- req(res_antic())
    efa_cols <- intersect(names(efa_factors), names(df))
    if (length(efa_cols) == 0) return(NULL)

    df_long <- df |>
      select(Prediccio, all_of(efa_cols)) |>
      pivot_longer(all_of(efa_cols), names_to = "fac", values_to = "score") |>
      filter(!is.na(score), !is.na(Prediccio)) |>
      group_by(fac, Prediccio) |>
      summarise(m = round(mean(score, na.rm = TRUE), 2), .groups = "drop")

    # Lookup fora de mutate per evitar problemes de NSE dplyr amb vectors nomenats
    rows_r <- df_long[df_long$Prediccio == "Regular",   ]
    rows_i <- df_long[df_long$Prediccio == "Irregular", ]
    m_r <- setNames(rows_r$m, rows_r$fac)
    m_i <- setNames(rows_i$m, rows_i$fac)

    mr_v <- sapply(efa_cols, function(v) if (v %in% names(m_r)) m_r[[v]] else 0)
    mi_v <- sapply(efa_cols, function(v) if (v %in% names(m_i)) m_i[[v]] else 0)
    dv <- mr_v - mi_v
    lbl <- unname(efa_labels[efa_cols])

    df_diff <- data.frame(
      etiqueta = factor(lbl, levels = rev(lbl)),
      diff     = dv,
      col      = ifelse(dv >= 0, COL_REG, COL_IRR),
      tip      = sprintf("%s<br>Regular: %.2f  |  Irregular: %.2f<br><b>R − I: %+.2f</b>",
                         lbl, mr_v, mi_v, dv),
      stringsAsFactors = FALSE
    )

    plot_ly(df_diff, y = ~etiqueta, x = ~diff, type = "bar", orientation = "h",
            marker = list(color = df_diff$col), opacity = 0.85,
            text = ~tip, hovertemplate = "%{text}<extra></extra>",
            showlegend = FALSE) |>
      layout(
        title  = list(
          text = paste0("Diferència de puntuació EFA (Regular − Irregular)<br>",
                        "<span style='font-size:11px;color:", COL_REG,
                        "'>█ Regular > Irregular</span>  ",
                        "<span style='font-size:11px;color:", COL_IRR,
                        "'>█ Irregular > Regular</span>"),
          x = 0.02, font = list(size = 13)),
        xaxis  = list(title = "Regular − Irregular",
                      zeroline = TRUE, zerolinecolor = "#666", zerolinewidth = 1.5),
        yaxis  = list(title = NULL, tickfont = list(size = 10)),
        margin = list(l = 185, t = 80)
      ) |>
      config(responsive = TRUE)
  })

  output$antic_vars_grafic <- renderPlotly({
    df <- req(res_antic())
    key_vars <- c("IA_SUBST_num","T_AVAL_num","TREB_INTENS","CURS_1R_num","NOTA_num")
    key_labels <- c(IA_SUBST_num = "IA com a substitut (%)",
                    T_AVAL_num   = "Avaluació continuada (%)",
                    TREB_INTENS  = "Treballa intensament (%)",
                    CURS_1R_num  = "Primer curs (%)",
                    NOTA_num     = "Nota (÷5 × 100)")
    key_scale <- c(IA_SUBST_num = 100, T_AVAL_num = 100,
                    TREB_INTENS  = 100, CURS_1R_num = 100,
                    NOTA_num     = 20)

    avail <- intersect(key_vars, names(df))
    if (length(avail) == 0) return(NULL)

    df_long2 <- df |>
      filter(!is.na(Prediccio)) |>
      select(Prediccio, all_of(avail)) |>
      pivot_longer(all_of(avail), names_to = "variable", values_to = "val") |>
      filter(!is.na(val)) |>
      group_by(variable, Prediccio) |>
      summarise(m = mean(val, na.rm = TRUE), .groups = "drop")

    # Lookup + escalat fora de mutate per evitar problemes NSE
    rows_r2 <- df_long2[df_long2$Prediccio == "Regular",   ]
    rows_i2 <- df_long2[df_long2$Prediccio == "Irregular", ]
    m_r2 <- setNames(rows_r2$m, rows_r2$variable)
    m_i2 <- setNames(rows_i2$m, rows_i2$variable)

    mr_v2 <- sapply(avail, function(v)
      (if (v %in% names(m_r2)) m_r2[[v]] else 0) * key_scale[[v]])
    mi_v2 <- sapply(avail, function(v)
      (if (v %in% names(m_i2)) m_i2[[v]] else 0) * key_scale[[v]])
    dv2 <- mr_v2 - mi_v2
    lbl2 <- unname(key_labels[avail])

    df_diff2 <- data.frame(
      etiqueta = factor(lbl2, levels = rev(lbl2)),
      diff     = dv2,
      col      = ifelse(dv2 >= 0, COL_REG, COL_IRR),
      tip      = sprintf("%s<br>Regular: %.1f  |  Irregular: %.1f<br><b>R − I: %+.1f pp</b>",
                         lbl2, mr_v2, mi_v2, dv2),
      stringsAsFactors = FALSE
    )

    plot_ly(df_diff2, y = ~etiqueta, x = ~diff, type = "bar", orientation = "h",
            marker = list(color = df_diff2$col), opacity = 0.85,
            text = ~tip, hovertemplate = "%{text}<extra></extra>",
            showlegend = FALSE) |>
      layout(
        title  = list(
          text = paste0("Diferència de variables clau (Regular − Irregular)<br>",
                        "<span style='font-size:11px;color:", COL_REG,
                        "'>█ Regular > Irregular</span>  ",
                        "<span style='font-size:11px;color:", COL_IRR,
                        "'>█ Irregular > Regular</span>"),
          x = 0.02, font = list(size = 13)),
        xaxis  = list(title = "Regular − Irregular (punts %)",
                      zeroline = TRUE, zerolinecolor = "#666", zerolinewidth = 1.5),
        yaxis  = list(title = NULL),
        margin = list(l = 185, t = 80),
        showlegend = FALSE
      ) |>
      config(responsive = TRUE)
  })

  output$antic_efa_peu <- renderUI({
    req(res_antic())
    tags$p(tags$em(
      "Factors de risc (correlació negativa amb assistència regular): ",
      tags$strong("Desmotivació pedagògica, Autogestió, IA eina d'estudi.")
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
