install.packages(
  "C:/Users/edurn/Downloads/TFG/catboost-R-windows-x86_64-1.2.10.tgz",
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

load("2. Dades/7. Dades XGBoost.RData")

source("3. Codi/Funcions models.R")

motius_vars <- readRDS("2. Dades/motius_vars.rds")
estrategies_vars <- readRDS("2. Dades/estrategies_vars.rds")
ia_vars <- readRDS("2. Dades/ia_vars.rds")

sink("4. Outputs/8.1 Output_text_catboost.txt")
pdf("4. Outputs/8.2 Output_grafics_catboost.pdf", width = 10, height = 8)

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

vars_fa <- c("MOT_DESMOTIVACIO", "MOT_AUTOGESTIO", "MOT_FORCA_MAJOR",
             "EST_QUALITAT_DOC", "EST_AVALUACIO_AC", "EST_TEMPS_CLASSE",
             "EST_GRUPS_REDUITS", "IA_EINA_ESTUDI", "IA_SUBSTITUCIO")
vars_acad <- c("NOTA_num", "T_AVAL_num", "CURS_1R_num", "N_ASSIG")
vars_pers <- c("EDAT", "DESPL")

predictors <- c(motius_vars, estrategies_vars, ia_vars,
                vars_fa, vars_acad, vars_pers)
predictors <- predictors[predictors %in% names(dades_cat)]

dades_cat_net <- dades_cat %>%
  select(Y, all_of(predictors)) %>%
  drop_na()

cat(" ==================== PREPARACIÓ DE DADES ==================== \n")
cat(sprintf("Observacions totals: %d\n", nrow(dades_cat_net)))
cat(sprintf("Predictors inclosos: %d\n", length(predictors)))
cat(sprintf("Distribucio Y — Irregular (0): %d | Regular (1): %d\n\n",
            sum(dades_cat_net$Y == 0), sum(dades_cat_net$Y == 1)))

# Partició 70 / 15 / 15
set.seed(1234)
idx_train <- createDataPartition(dades_cat_net$Y, p = 0.70, list = FALSE)
dades_rest <- dades_cat_net[-idx_train, ]
idx_val <- createDataPartition(dades_rest$Y, p = 0.50, list = FALSE)

dades_train_cat <- dades_cat_net[idx_train, ]
dades_val_cat <- dades_rest[idx_val, ]
dades_test_cat <- dades_rest[-idx_val, ]

Y_train <- dades_train_cat$Y
Y_val <- dades_val_cat$Y
Y_test <- dades_test_cat$Y

# Tot a numèric (consistència amb XGBoost)
X_train_df <- as.data.frame(apply(dades_train_cat[, predictors], 2, as.numeric))
X_val_df <- as.data.frame(apply(dades_val_cat[, predictors], 2, as.numeric))
X_test_df <- as.data.frame(apply(dades_test_cat[, predictors], 2, as.numeric))

# CatBoost pools
train_pool_cat <- catboost.load_pool(data = X_train_df, label = Y_train)
val_pool_cat <- catboost.load_pool(data = X_val_df, label = Y_val)
test_pool_cat <- catboost.load_pool(data = X_test_df, label = Y_test)

cat(sprintf("Particicio: Train = %d | Val = %d | Test = %d\n",
            length(Y_train), length(Y_val), length(Y_test)))
cat(sprintf("  Train — Regular: %.1f%% | Irregular: %.1f%%\n",
            mean(Y_train) * 100, (1 - mean(Y_train)) * 100))
cat(sprintf("  Val   — Regular: %.1f%% | Irregular: %.1f%%\n",
            mean(Y_val) * 100, (1 - mean(Y_val)) * 100))
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
  depth = c(4, 6, 8),
  learning_rate = c(0.01, 0.03, 0.05),
  l2_leaf_reg = c(1, 5, 10),
  random_strength = c(0.5, 1.5),
  stringsAsFactors = FALSE
)

cat(sprintf("Combinacions a avaluar: %d\n", nrow(grid)))
cat(sprintf("Recall minim acceptable: %.2f\n\n", MIN_RECALL))

set.seed(1234)
grid_results <- vector("list", nrow(grid))

for (i in seq_len(nrow(grid))) {
  params_i <- list(
    loss_function = "Logloss",
    eval_metric = "AUC",
    iterations = 1000,
    learning_rate = grid$learning_rate[i],
    depth = grid$depth[i],
    l2_leaf_reg = grid$l2_leaf_reg[i],
    random_strength = grid$random_strength[i],
    random_seed = 1234,
    od_type = "Iter",
    od_wait = 30,
    use_best_model = TRUE,
    logging_level = "Silent"
  )

  m_i <- catboost.train(
    learn_pool = train_pool_cat,
    test_pool = val_pool_cat,
    params = params_i
  )

  prob_val_i_raw <- catboost.predict(m_i, val_pool_cat,
                                     prediction_type = "Probability")
  prob_val_i <- extreure_prob_cat(prob_val_i_raw)

  roc_i <- roc(Y_val, prob_val_i, quiet = TRUE)
  auc_i <- as.numeric(auc(roc_i))

  pr_sel_i <- tryCatch(
    seleccionar_llindar_pr(prob_val_i, Y_val, MIN_RECALL),
    error = function(e) list(threshold = NA, precision = NA,
                             recall = NA, recall_ok = FALSE)
  )
  val_precision_i <- pr_sel_i$precision
  val_recall_i    <- pr_sel_i$recall
  val_thresh_i    <- pr_sel_i$threshold
  recall_ok_i     <- pr_sel_i$recall_ok

  grid_results[[i]] <- data.frame(
    depth = grid$depth[i],
    learning_rate = grid$learning_rate[i],
    l2_leaf_reg = grid$l2_leaf_reg[i],
    random_strength = grid$random_strength[i],
    val_auc = round(auc_i, 4),
    val_precision = round(val_precision_i, 4),
    val_recall = round(val_recall_i, 4),
    val_threshold = round(val_thresh_i, 4),
    recall_ok = recall_ok_i,
    stringsAsFactors = FALSE
  )

  if (i %% 12 == 0) {
    cat(sprintf("  %d / %d combinacions completades...\n", i, nrow(grid)))
  }
}

df_grid <- do.call(rbind, grid_results)
df_grid <- df_grid[order(-df_grid$val_auc), ]

cat("\nTop 10 combinacions (ordenades per AUC):\n")
print(head(df_grid, 10), row.names = FALSE)

best_row <- df_grid[1, ]

cat(sprintf("\nMillors hiperparametres:\n"))
cat(sprintf("  depth = %d | learning_rate = %.3f | l2_leaf_reg = %.1f | random_strength = %.1f\n",
            best_row$depth, best_row$learning_rate,
            best_row$l2_leaf_reg, best_row$random_strength))
cat(sprintf("  Val AUC = %.4f | Val Precisio = %.4f | Val Recall = %.4f | Llindar = %.4f\n",
            best_row$val_auc, best_row$val_precision,
            best_row$val_recall, best_row$val_threshold))
cat(sprintf("  recall_ok (>= %.2f): %s\n\n",
            MIN_RECALL, ifelse(best_row$recall_ok, "SI", "NO")))

#### ============================================================ ####
####              2. MODEL FINAL CatBoost                         ####
#### ============================================================ ####

cat(" =============== MODEL FINAL CatBoost ================ \n")

best_params_cat <- list(
  loss_function = "Logloss",
  eval_metric = "AUC",
  iterations = 3000,
  learning_rate = best_row$learning_rate,
  depth = best_row$depth,
  l2_leaf_reg = best_row$l2_leaf_reg,
  random_strength = best_row$random_strength,
  random_seed = 1234,
  od_type = "Iter",
  od_wait = 50,
  use_best_model = TRUE,
  logging_level = "Verbose"
)

set.seed(1234)
catboost_model <- catboost.train(
  learn_pool = train_pool_cat,
  test_pool = val_pool_cat,
  params = best_params_cat
)

n_trees <- tryCatch(catboost_model$tree_count, error = function(e) NA)
if (!is.na(n_trees)) cat(sprintf("\nIteracions finals del model: %d\n\n", n_trees))

# Prediccions sobre els tres conjunts
prob_val_raw <- catboost.predict(catboost_model, val_pool_cat,
                                 prediction_type = "Probability")
prob_test_raw <- catboost.predict(catboost_model, test_pool_cat,
                                  prediction_type = "Probability")
prob_train_raw <- catboost.predict(catboost_model, train_pool_cat,
                                   prediction_type = "Probability")

prob_val_cat <- extreure_prob_cat(prob_val_raw)
prob_test_cat <- extreure_prob_cat(prob_test_raw)
prob_train_cat <- extreure_prob_cat(prob_train_raw)

# Corba ROC
roc_test_cat <- roc(Y_test, prob_test_cat, quiet = TRUE)
roc_val_cat <- roc(Y_val, prob_val_cat, quiet = TRUE)

roc_df <- data.frame(
  spec_inv = 1 - roc_test_cat$specificities,
  sens = roc_test_cat$sensitivities
)

print(
  ggplot(roc_df, aes(x = spec_inv, y = sens)) +
    geom_path(color = "#4A90B8", linewidth = 1.2) +
    geom_abline(slope = 1, intercept = 0,
                linetype = "dashed", color = "grey50") +
    annotate("text", x = 0.62, y = 0.2,
             label = sprintf("AUC test = %.3f\nAUC val  = %.3f",
                             as.numeric(auc(roc_test_cat)),
                             as.numeric(auc(roc_val_cat))),
             size = 4.5, color = "#4A90B8") +
    labs(title = "Corba ROC — CatBoost (test)",
         x = "1 - Especificitat", y = "Sensibilitat") +
    theme_minimal(base_size = 13)
)

#### ============================================================ ####
####         3. IMPORTÀNCIA DE VARIABLES                          ####
#### ============================================================ ####

cat(" ============ IMPORTANCIA DE VARIABLES ============ \n")

imp_cat <- catboost.get_feature_importance(
  model = catboost_model,
  pool = train_pool_cat,
  type = "FeatureImportance"
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
    coord_flip() +
    scale_fill_gradient(low = "#AED6F1", high = "#1A5276", guide = "none") +
    labs(title = "Importancia de variables — CatBoost",
         subtitle = "Top 20 | Mesura: FeatureImportance (%)",
         x = "", y = "Importancia (%)") +
    theme_minimal(base_size = 13)
)

#### ============================================================ ####
####                    4. SHAP VALUES                            ####
#### ============================================================ ####

cat(" =============== SHAP VALUES =============== \n")

shap_raw <- catboost.get_feature_importance(
  model = catboost_model,
  pool = test_pool_cat,
  type = "ShapValues"
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
    coord_flip() +
    scale_fill_gradient(low = "#A9DFBF", high = "#1E8449", guide = "none") +
    labs(title = "Importancia SHAP — CatBoost",
         subtitle = "Top 20 | mean(|SHAP|) sobre conjunt test",
         x = "", y = "Importancia SHAP") +
    theme_minimal(base_size = 13)
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
    theme_minimal(base_size = 12)
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
      theme_minimal(base_size = 13)
    print(p_dep)
  }
}

#### ============================================================ ####
####              5. MÈTRIQUES DE CLASSIFICACIÓ                   ####
#### ============================================================ ####

cat(" ============== METRIQUES DE CLASSIFICACIO ============== \n")

# determinar llindar sobre validació
pr_cat_final <- seleccionar_llindar_pr(prob_val_cat, Y_val, MIN_RECALL)
thresh_final <- pr_cat_final$threshold
cat(sprintf("AUPRC: %.4f\n", pr_cat_final$auprc))
cat(sprintf("-> Llindar seleccionat: %.4f | recall_ok (>= %.2f): %s\n",
            thresh_final, MIN_RECALL,
            ifelse(pr_cat_final$recall_ok, "SI", "NO (fallback Youden)")))
if (!is.na(pr_cat_final$precision))
  cat(sprintf("   Val Precisio = %.4f | Val Recall = %.4f\n\n",
              pr_cat_final$precision, pr_cat_final$recall))
else
  cat(sprintf("   Val Recall = %.4f (Youden fallback)\n\n", pr_cat_final$recall))

# Gràfic: corba precisió-recall per al val set (PRROC)
print(
  ggplot(pr_cat_final$pr_curve, aes(x = recall, y = precision)) +
    geom_path(color = "#4A90B8", linewidth = 1) +
    geom_vline(xintercept = MIN_RECALL, linetype = "dashed",
               color = "red", linewidth = 0.8) +
    geom_point(data = data.frame(
                 recall = pr_cat_final$recall,
                 precision = ifelse(is.na(pr_cat_final$precision),
                                    0, pr_cat_final$precision)),
               color = "#E07B54", size = 3, shape = 17) +
    annotate("text", x = MIN_RECALL + 0.03, y = 0.1,
             label = sprintf("Recall minim\n= %.2f", MIN_RECALL),
             color = "red", size = 3.5) +
    labs(title = "Corba Precisio-Recall — CatBoost (val, PRROC)",
         subtitle = sprintf("AUPRC = %.4f | Llindar = %.4f",
                            pr_cat_final$auprc, thresh_final),
         x = "Recall (Sensibilitat)", y = "Precisio (PPV)") +
    theme_minimal(base_size = 13)
)

# ------ Mètriques sobre el test ------
cat("--- 5a Metriques sobre conjunt test ---\n")
metriques_cat <- calcular_metriques_cat(
  prob = prob_test_cat,
  Y_vec = Y_test,
  nom_model = "CatBoost",
  thresh_override = thresh_final
)
mostrar_metriques_cat(metriques_cat)

# ------ Mètriques sobre la validació ------
cat("--- Metriques sobre conjunt de validacio ---\n")
metriques_cat_val <- calcular_metriques_cat(
  prob = prob_val_cat,
  Y_vec = Y_val,
  nom_model = "CatBoost (validació)",
  thresh_override = thresh_final
)
mostrar_metriques_cat(metriques_cat_val)

# ------ Mètriques sobre el train ------
cat("--- Metriques sobre train ---\n")
metriques_cat_train <- calcular_metriques_cat(
  prob = prob_train_cat,
  Y_vec = Y_train,
  nom_model = "CatBoost (train)",
  thresh_override = thresh_final
)
mostrar_metriques_cat(metriques_cat_train)

# Taula resum overfitting
cat("\n--- Resum overfitting: train vs val vs test ---\n\n")
df_ov_cat <- data.frame(
  Conjunt = c("Train", "Validacio", "Test"),
  AUC = c(metriques_cat_train$AUC, metriques_cat_val$AUC, metriques_cat$AUC),
  Precision = c(metriques_cat_train$precision, metriques_cat_val$precision,
                metriques_cat$precision),
  Recall = c(metriques_cat_train$recall, metriques_cat_val$recall,
             metriques_cat$recall),
  F1 = c(metriques_cat_train$F1, metriques_cat_val$F1, metriques_cat$F1),
  Balanced_Acc = c(metriques_cat_train$balanced_accuracy,
                   metriques_cat_val$balanced_accuracy,
                   metriques_cat$balanced_accuracy)
)
print(df_ov_cat, row.names = FALSE)
cat("\n")

#### ============================================================ ####
####         6. COMPARACIÓ GLOBAL DE MODELS                       ####
#### ============================================================ ####

cat(" ============= COMPARACIO GLOBAL DE MODELS ============= \n")

models_llista <- list()

fitxers <- c(
  Logit = "2. Dades/metriques_logit.rds",
  `RF-A` = "2. Dades/metriques_rf_a.rds",
  `RF-B` = "2. Dades/metriques_rf_b.rds",
  XGBoost = "2. Dades/metriques_xgb.rds"
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
    theme(axis.text.x = element_text(angle = 25, hjust = 1),
          legend.position = "bottom")
)


saveRDS(metriques_cat, "2. Dades/metriques_catboost.rds")

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
save(dades_def, file = "2. Dades/8. Dades CatBoost.RData")

sink()
dev.off()
