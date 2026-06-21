install.packages(
  "C:/Users/Edurne/Downloads/TFG/catboost-R-windows-x86_64-1.2.10.tgz",
  repos = NULL,
  type = "source"
)

packages <- c("dplyr", "ggplot2", "tibble", "tidyr", "caret", "pROC", "PRROC")

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

lapply(packages, install_if_missing)
rm(packages)

if (!require("catboost", character.only = TRUE)) {
  stop(paste(
    "El paquet 'catboost' no esta instal·lat.",
    "Segueix les instruccions al capçalera d'aquest script.",
    sep = "\n"
  ))
}

setwd("C:/Users/edurn/Downloads/TFG")
load("2. Dades/9. Dades XGBoost.RData")
source("3. Codi/Funcions models.R")

motius_vars <- readRDS("2. Dades/motius_vars.rds")
estrategies_vars <- readRDS("2. Dades/estrategies_vars.rds")
ia_vars <- readRDS("2. Dades/ia_vars.rds")

sink("4. Outputs/10. CatBoost/10.1 Output_text_catboost.txt")
png("4. Outputs/10. CatBoost/grafic_%02d.png", width = 8, height = 6, units = "in", res = 300)

#### ============================================================ ####
####                   0. PREPARACIÓ DE DADES                     ####
#### ============================================================ ####

dades_cat <- dades_def %>%
  mutate(
    Y = as.integer(GRUP_ASSIST == "Regular (\u226580%)"),
    NOTA_num = as.numeric(NOTA),
    IA_SUBST_num = as.numeric(IA_SUBST),
    IA_ATENC_num = as.numeric(IA_ATENC),
    T_AVAL_num = as.integer(T_AVAL == "Continuada"),
    CURS_1R_num = as.integer(CURS_1R)
  ) %>%
  filter(!is.na(Y))

ia_vars <- c("IA_SUBST_num", "IA_ATENC_num", "IA_CONF_num")
vars_fa <- c("MOT_DESMOTIVACIO", "MOT_AUTOGESTIO", "MOT_FORCA_MAJOR",
             "EST_QUALITAT_DOC", "EST_AVALUACIO_AC", "EST_TEMPS_CLASSE",
             "EST_GRUPS_REDUITS", "IA_EINA_ESTUDI", "IA_SUBSTITUCIO")
vars_acad <- c("NOTA_num", "T_AVAL_num", "CURS_1R_num", "N_ASSIG","DOBLE_GRAU_EST", "TREB_INTENS")
vars_pers <- c("EDAT", "DESPL")

predictors <- c(ia_vars, vars_fa, vars_acad, vars_pers)
predictors <- predictors[predictors %in% names(dades_cat)]

dades_cat_net <- dades_cat %>%
  select(Y, all_of(predictors)) %>%
  drop_na()

cat(" ==================== PREPARACIÓ DE DADES ==================== \n")
cat(sprintf("Observacions totals: %d\n", nrow(dades_cat_net)))
cat(sprintf("Predictors inclosos: %d\n", length(predictors)))
cat(sprintf("Distribucio Y — Irregular (0): %d | Regular (1): %d\n\n",
            sum(dades_cat_net$Y == 0), sum(dades_cat_net$Y == 1)))

# Partició 80/20 compartida
idx_train <- crear_o_carregar_particio(dades_cat_net$Y)
dades_train_cat <- dades_cat_net[idx_train, ]
dades_test_cat  <- dades_cat_net[-idx_train, ]

Y_train <- dades_train_cat$Y
Y_test  <- dades_test_cat$Y

X_train_df <- as.data.frame(apply(dades_train_cat[, predictors], 2, as.numeric))
X_test_df <- as.data.frame(apply(dades_test_cat[, predictors],  2, as.numeric))

train_pool_cat <- catboost.load_pool(data = X_train_df, label = Y_train)
test_pool_cat <- catboost.load_pool(data = X_test_df,  label = Y_test)

cat(sprintf("Particio: Train = %d | Test = %d\n", length(Y_train), length(Y_test)))
cat(sprintf("  Train — Regular: %.1f%% | Irregular: %.1f%%\n",
            mean(Y_train) * 100, (1 - mean(Y_train)) * 100))
cat(sprintf("  Test  — Regular: %.1f%% | Irregular: %.1f%%\n\n",
            mean(Y_test) * 100, (1 - mean(Y_test)) * 100))

# preparem per obtenir la probabilitat correctament
extreure_prob_cat <- function(raw_pred) {
  if (is.matrix(raw_pred)) raw_pred[, ncol(raw_pred)] else as.numeric(raw_pred)
}

MIN_RECALL <- 0.60

#### ============================================================ ####
####                             1. GRID SEARCH                   ####
#### ============================================================ ####
cat("================= 1. GRID SEARCH ================ \n")

grid <- expand.grid(
  depth = c(4, 6),
  learning_rate = c(0.01, 0.03, 0.05),
  l2_leaf_reg = c(1, 5),
  random_strength = c(0.5, 1.5),
  stringsAsFactors = FALSE
)

cat(sprintf("Combinacions a avaluar: %d\n", nrow(grid)))
cat(sprintf("Recall minim acceptable: %.2f\n\n", MIN_RECALL))

set.seed(1234)
grid_results <- vector("list", nrow(grid))
folds_grid   <- caret::createFolds(Y_train, k = 5, list = TRUE)

for (i in seq_len(nrow(grid))) {
  params_i <- list(
    loss_function = "Logloss",
    eval_metric = "AUC",
    iterations = 500,
    learning_rate = grid$learning_rate[i],
    depth = grid$depth[i],
    l2_leaf_reg = grid$l2_leaf_reg[i],
    random_strength = grid$random_strength[i],
    random_seed = 1234,
    logging_level = "Silent"
  )

  aucs_folds <- numeric(5)
  for (fi in seq_along(folds_grid)) {
    vi <- folds_grid[[fi]]
    p_tr <- catboost.load_pool(as.data.frame(X_train_df[-vi, ]), Y_train[-vi])
    p_va <- catboost.load_pool(as.data.frame(X_train_df[vi,  ]), Y_train[vi])
    m_fi <- tryCatch(
      catboost.train(p_tr, test_pool = p_va, params = params_i),
      error = function(e) NULL
    )
    if (is.null(m_fi)) {
      aucs_folds[fi] <- NA_real_
    } else {
      pr_fi <- extreure_prob_cat(
        catboost.predict(m_fi, p_va, prediction_type = "Probability"))
      aucs_folds[fi] <- as.numeric(pROC::auc(
        pROC::roc(Y_train[vi], pr_fi, quiet = TRUE)))
    }
  }

  auc_i <- mean(aucs_folds, na.rm = TRUE)

  grid_results[[i]] <- data.frame(
    depth = grid$depth[i],
    learning_rate = grid$learning_rate[i],
    l2_leaf_reg = grid$l2_leaf_reg[i],
    random_strength = grid$random_strength[i],
    cv_auc = round(auc_i, 4),
    best_iter = 500L,
    stringsAsFactors = FALSE
  )

  if (i %% 12 == 0)
    cat(sprintf("  %d / %d combinacions completades...\n", i, nrow(grid)))
}

df_grid <- do.call(rbind, grid_results)
df_grid <- df_grid[order(-df_grid$cv_auc), ]

cat("\nTop 10 combinacions (ordenades per AUC CV):\n")
print(head(df_grid, 10), row.names = FALSE)

best_row <- df_grid[1, ]

cat(sprintf("\nMillors hiperparametres:\n"))
cat(sprintf("  depth = %d | learning_rate = %.3f | l2_leaf_reg = %.1f | random_strength = %.1f\n",
            best_row$depth, best_row$learning_rate,
            best_row$l2_leaf_reg, best_row$random_strength))
cat(sprintf("  CV AUC = %.4f | best_iter = %d\n\n", best_row$cv_auc, best_row$best_iter))

#### ============================================================ ####
####              2. MODEL FINAL CatBoost                         ####
#### ============================================================ ####

cat(" =============== MODEL FINAL CatBoost ================ \n")

best_params_cat <- list(
  loss_function = "Logloss",
  eval_metric = "AUC",
  iterations = 1000, # màxim; early stopping ho aturarà abans
  od_type = "Iter", # early stopping per iteracions sense millora
  od_wait = 50, # para si 50 iteracions sense millorar AUC
  learning_rate = best_row$learning_rate,
  depth = best_row$depth,
  l2_leaf_reg = best_row$l2_leaf_reg,
  random_strength = best_row$random_strength,
  random_seed = 1234,
  logging_level = "Silent"
)

set.seed(1234)
catboost_model <- catboost.train(
  learn_pool = train_pool_cat,
  params = best_params_cat
)

cat(sprintf("Iteracions del model final: %d\n\n", best_row$best_iter))

# Prediccions sobre train i test
prob_test_raw <- catboost.predict(catboost_model, test_pool_cat,  prediction_type = "Probability")
prob_train_raw <- catboost.predict(catboost_model, train_pool_cat, prediction_type = "Probability")

prob_test_cat  <- extreure_prob_cat(prob_test_raw)
prob_train_cat <- extreure_prob_cat(prob_train_raw)

roc_test_cat  <- pROC::roc(Y_test,  prob_test_cat,  quiet = TRUE)
roc_train_cat <- pROC::roc(Y_train, prob_train_cat, quiet = TRUE)
auc_test_cat <- as.numeric(pROC::auc(roc_test_cat))
auc_train_cat <- as.numeric(pROC::auc(roc_train_cat))
cat(sprintf("AUC train (in-sample): %.4f | AUC test: %.4f\n\n", auc_train_cat, auc_test_cat))

roc_df <- data.frame(
  spec_inv = 1 - roc_test_cat$specificities,
  sens = roc_test_cat$sensitivities
)

print(
  ggplot(roc_df, aes(x = spec_inv, y = sens)) +
    geom_path(color = "#4A90B8", linewidth = 1.2) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
    annotate("text", x = 0.62, y = 0.2,
             label = sprintf("AUC test = %.3f", auc_test_cat),
             size = 4.5, color = "#4A90B8") +
    labs(title = "Corba ROC — CatBoost (test)",
         x = "1 - Especificitat", y = "Sensibilitat") +
    theme_minimal(base_size = 13) +
    theme(axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          legend.text = element_text(size = 12))
)

#### ============================================================ ####
####         3. IMPORTÀNCIA DE VARIABLES                          ####
#### ============================================================ ####

cat(" ============ IMPORTANCIA DE VARIABLES ============ \n")

imp_cat <- catboost.get_feature_importance(
  model = catboost_model,
  pool = train_pool_cat,
  type = "FeatureImportance",
  plot = FALSE
)

imp_df <- tibble(
  variable = colnames(X_train_df),
  importancia = as.numeric(imp_cat)
) %>%
  arrange(desc(importancia))

cat("Top 20 variables per importancia:\n")
print(imp_df %>% slice_head(n = 20), n = 20)

print(
  ggplot(imp_df %>% slice_head(n = 20),
         aes(x = reorder(variable, importancia),
             y = importancia, fill = importancia)) +
    geom_col(alpha = 0.9) +
    geom_text(aes(label = round(importancia, 2)), hjust = -0.1, size = 3.5) +
    coord_flip() +
    scale_fill_gradient(low = "#AED6F1", high = "#1A5276", guide = "none") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = "Importancia de variables — CatBoost",
         subtitle = "Top 20 | Mesura: FeatureImportance (%)",
         x = "", y = "Importancia (%)") +
    theme_minimal(base_size = 12) +
    theme(axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          legend.text = element_text(size = 12))
)

#### ============================================================ ####
####                    4. SHAP VALUES                            ####
#### ============================================================ ####

cat(" =============== SHAP VALUES =============== \n")

shap_raw <- catboost.get_feature_importance(
  model = catboost_model,
  pool = test_pool_cat,
  type = "ShapValues",
  plot = FALSE
)
# ShapValues: matriu nrow=n_obs, ncol=n_features + 1 (última col = bias)
shap_df <- as.data.frame(shap_raw[, -ncol(shap_raw)])
names(shap_df) <- names(X_test_df)

shap_imp <- tibble(
  variable = names(shap_df),
  mean_abs_shap = colMeans(abs(shap_df))
) %>%
  arrange(desc(mean_abs_shap))

cat("Top 20 variables per importancia SHAP:\n")
print(shap_imp %>% slice_head(n = 20))

# Gràfic: importancia SHAP
print(
  ggplot(shap_imp %>% slice_head(n = 20),
         aes(x = reorder(variable, mean_abs_shap),
             y = mean_abs_shap, fill = mean_abs_shap)) +
    geom_col(alpha = 0.9) +
    geom_text(aes(label = round(mean_abs_shap, 2)), hjust = -0.1, size = 3.5) +
    coord_flip() +
    scale_fill_gradient(low = "#A9DFBF", high = "#1E8449", guide = "none") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = "Importancia SHAP — CatBoost",
         subtitle = "Top 20 | mean(|SHAP|) sobre conjunt test",
         x = "", y = "Importancia SHAP") +
    theme_minimal(base_size = 12) +
    theme(axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          legend.text = element_text(size = 12))
)

# Beeswarm (top 15)
top15_vars <- shap_imp$variable[seq_len(min(15, nrow(shap_imp)))]

shap_long <- shap_df %>%
  select(all_of(top15_vars)) %>%
  mutate(obs = row_number()) %>%
  pivot_longer(-obs, names_to = "variable", values_to = "shap") %>%
  left_join(
    X_test_df %>%
      select(all_of(top15_vars)) %>%
      mutate(obs = row_number()) %>%
      pivot_longer(-obs, names_to = "variable", values_to = "valor"),
    by = c("obs", "variable")
  ) %>%
  mutate(variable = factor(variable, levels = rev(top15_vars)))

print(
  ggplot(shap_long, aes(x = shap, y = variable, color = valor)) +
    geom_jitter(height = 0.25, size = 1.2, alpha = 0.6) +
    geom_vline(xintercept = 0, color = "grey40", linewidth = 0.8) +
    scale_color_gradient(low = "#2471A3", high = "#E74C3C",
                         name = "Valor\nde la variable") +
    labs(title = "SHAP Beeswarm — CatBoost (top 15 variables)",
         subtitle = "Color = valor de la variable | x > 0 -> augmenta P(Regular)",
         x = "Valor SHAP", y = "") +
    theme_minimal(base_size = 12) +
    theme(axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          legend.text = element_text(size = 12))
)

# Dependence plots (top 4)
top4_vars <- shap_imp$variable[seq_len(min(4, nrow(shap_imp)))]

for (v in top4_vars) {
  if (v %in% names(shap_df) && v %in% names(X_test_df)) {
    df_dep <- data.frame(valor = X_test_df[[v]], shap = shap_df[[v]])
    p_dep <- ggplot(df_dep, aes(x = valor, y = shap)) +
      geom_point(alpha = 0.5, color = "#4A90B8", size = 1.8) +
      geom_smooth(method = "loess", se = TRUE,
                  color = "#E07B54", fill = "#F0B27A", linewidth = 1) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      labs(title = sprintf("SHAP Dependence Plot — %s", v),
           subtitle = "Linia taronja = tendencia LOESS",
           x = v, y = "Valor SHAP") +
      theme_minimal(base_size = 13) +
      theme(axis.text.y = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            legend.text = element_text(size = 12))
    print(p_dep)
  }
}

#### ============================================================ ####
####              5. MÈTRIQUES DE CLASSIFICACIÓ                   ####
#### ============================================================ ####

cat(" ============== METRIQUES DE CLASSIFICACIO ============== \n")

# Llindar PR sobre probabilitats OOF del CV (5-fold, no contamina el test)
set.seed(1234)
folds_cat_oof <- caret::createFolds(Y_train, k = 5, list = TRUE)
oof_probs_cat <- rep(NA_real_, length(Y_train))

for (fi in seq_along(folds_cat_oof)) {
  vi <- folds_cat_oof[[fi]]
  
  pool_tr_f <- catboost.load_pool(
    as.data.frame(X_train_df[-vi, ]), Y_train[-vi])
  pool_va_f <- catboost.load_pool(
    as.data.frame(X_train_df[vi, ]),  Y_train[vi])
  
  params_oof <- best_params_cat
  params_oof$iterations <- 500 # max permes
  params_oof$od_type <- "Iter" # early stopping per iteracions sense millora
  params_oof$od_wait <- 20 # para si 20 iter sense millorar AUC validacio
  params_oof$eval_metric <- "AUC"
  
  m_f <- catboost.train(
    learn_pool = pool_tr_f,
    test_pool = pool_va_f,       
    params = params_oof
  )
  
  oof_probs_cat[vi] <- extreure_prob_cat(
    catboost.predict(m_f, pool_va_f, prediction_type = "Probability")
  )
}

pr_cat_oof  <- seleccionar_llindar_pr(oof_probs_cat, Y_train, MIN_RECALL)
thresh_final <- pr_cat_oof$threshold

cat(sprintf("AUPRC (OOF CV): %.4f\n", pr_cat_oof$auprc))
cat(sprintf("-> Llindar seleccionat: %.4f | recall_ok (>= %.2f): %s\n\n",
            thresh_final, MIN_RECALL,
            ifelse(pr_cat_oof$recall_ok, "SI", "NO (fallback Youden)")))

pr_cat_test <- seleccionar_llindar_pr(prob_test_cat, Y_test, MIN_RECALL)
print(
  ggplot(pr_cat_test$pr_curve, aes(x = recall, y = precision)) +
    geom_path(color = "#4A90B8", linewidth = 1) +
    geom_vline(xintercept = MIN_RECALL, linetype = "dashed",
               color = "red", linewidth = 0.8) +
    geom_point(data = data.frame(recall = pr_cat_test$recall,
               precision = ifelse(is.na(pr_cat_test$precision), 0, pr_cat_test$precision)),
               color = "#E07B54", size = 3, shape = 17) +
    labs(title = "Corba Precisio-Recall — CatBoost (test)",
         subtitle = sprintf("AUPRC = %.4f | Llindar = %.4f", pr_cat_test$auprc, thresh_final),
         x = "Recall (Sensibilitat)", y = "Precisio (PPV)") +
    theme_minimal(base_size = 13) +
    theme(axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          legend.text = element_text(size = 12))
)

# ------ Mètriques test i train ------
cat("--- Metriques sobre conjunt test ---\n")
metriques_cat <- calcular_metriques_cat(
  prob = prob_test_cat, Y_vec = Y_test,
  nom_model = "CatBoost", thresh_override = thresh_final
)
mostrar_metriques_cat(metriques_cat)

cat("--- Metriques sobre train (in-sample) ---\n")
metriques_cat_train <- calcular_metriques_cat(
  prob = prob_train_cat, Y_vec = Y_train,
  nom_model = "CatBoost (train)", thresh_override = thresh_final
)
mostrar_metriques_cat(metriques_cat_train)

# Taula resum overfitting
cat("\n--- Resum overfitting: train vs test ---\n\n")
df_ov_cat <- data.frame(
  Conjunt = c("Train", "Test"),
  AUC = c(metriques_cat_train$AUC, metriques_cat$AUC),
  Precision = c(metriques_cat_train$precision, metriques_cat$precision),
  Recall = c(metriques_cat_train$recall, metriques_cat$recall),
  F1 = c(metriques_cat_train$F1, metriques_cat$F1),
  Balanced_Acc = c(metriques_cat_train$balanced_accuracy, metriques_cat$balanced_accuracy)
)
print(df_ov_cat, row.names = FALSE)
cat("\n")

metriques_noms_ov <- c("AUC", "Precision", "Recall", "F1", "Balanced_Acc")
df_ov_long <- df_ov_cat %>%
  tidyr::pivot_longer(cols = all_of(metriques_noms_ov),
                      names_to = "Metrica", values_to = "Valor") %>%
  mutate(Metrica = factor(Metrica, levels = metriques_noms_ov))

print(
  ggplot(df_ov_long, aes(x = Metrica, y = Valor, fill = Conjunt)) +
    geom_col(position = position_dodge(width = 0.65), alpha = 0.85, width = 0.65) +
    geom_text(aes(label = round(Valor, 2)),
              position = position_dodge(width = 0.65),
              vjust = -0.4, size = 3.5, fontface = "bold") +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50") +
    scale_fill_manual(values = c("Train" = "#4A90B8", "Test" = "#E07B54")) +
    scale_y_continuous(limits = c(0, 1.05)) +
    labs(title = "CatBoost — Train vs Test",
         subtitle = sprintf("Llindar: %.4f | MIN_RECALL >= %.2f", thresh_final, MIN_RECALL),
         x = "", y = "Valor", fill = "Conjunt") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "top",
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          legend.text = element_text(size = 12))
)

#### ============================================================ ####
####         6. COMPARACIÓ GLOBAL DE MODELS                       ####
#### ============================================================ ####

cat(" ============= COMPARACIO GLOBAL DE MODELS ============= \n")

models_llista <- list()

fitxers <- c(
  Logit = "2. Dades/metriques_logit.rds",
  `RF-A` = "4. Outputs/Metriques i models/metriques_rf_a.rds",
  `RF-B` = "4. Outputs/Metriques i models/metriques_rf_b.rds",
  XGBoost = "4. Outputs/Metriques i models/metriques_xgb.rds"
)

for (nom in names(fitxers)) {
  if (file.exists(fitxers[[nom]])) {
    models_llista[[nom]] <- readRDS(fitxers[[nom]])
  }
}
models_llista[["CatBoost"]] <- metriques_cat

df_comp <- do.call(rbind, lapply(models_llista, extreure_fila))
rownames(df_comp) <- NULL

cat("Taula comparativa de models (sobre conjunt test):\n\n")
print(df_comp, row.names = FALSE)

metriques_num <- c("AUC_test", "Balanced_Acc", "F1", "Accuracy", "Precision", "Recall")

df_comp_long <- df_comp %>%
  select(Model, all_of(metriques_num)) %>%
  mutate(across(-Model, as.numeric)) %>%
  pivot_longer(-Model, names_to = "metrica", values_to = "valor") %>%
  mutate(metrica = factor(metrica, levels = metriques_num))

colors_models <- c("#4A90B8", "#E07B54", "#8E6BBF", "#2ECC71",
                   "#E74C3C", "#F39C12")[seq_len(n_distinct(df_comp_long$Model))]

print(
  ggplot(df_comp_long, aes(x = metrica, y = valor, fill = Model)) +
    geom_col(position = "dodge", alpha = 0.85) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50") +
    scale_fill_manual(values = colors_models) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(title = "Comparacio de models",
         subtitle = "Metriques sobre conjunt test",
         x = "", y = "Valor") +
    theme_minimal(base_size = 13) +
    theme(axis.text.x = element_text(angle = 25, hjust = 1, size = 12),
          axis.text.y = element_text(size = 12),
          legend.position = "bottom",
          legend.text = element_text(size = 12))
)


saveRDS(metriques_cat, "2. Dades/2. Models/metriques_catboost.rds")
saveRDS(catboost_model, "2. Dades/2. Models/model_catboost.rds")

# --- Guardar probabilitats i bbdd  ---
predictors_ok_cat <- predictors[predictors %in% names(dades_cat)]
X_all_cat <- as.data.frame(
  apply(dades_cat[, predictors_ok_cat], 2, as.numeric))
complete_cat <- complete.cases(X_all_cat)
pool_all_cat <- catboost.load_pool(data = X_all_cat[complete_cat, ])
prob_cat_tots_raw <- catboost.predict(catboost_model, pool_all_cat,
                                      prediction_type = "Probability")
prob_cat_tots <- extreure_prob_cat(prob_cat_tots_raw)
dades_def$prob_catboost <- NA_real_
dades_def$prob_catboost[complete_cat] <- prob_cat_tots
dades_def$pred_catboost <- NA_integer_
dades_def$pred_catboost[complete_cat] <- as.integer(prob_cat_tots >= thresh_final)
cat(sprintf("Llindar aplicat per pred_catboost: %.4f \n", thresh_final))
save(dades_def, file = "2. Dades/10. Dades CatBoost.RData")

sink()
dev.off()
