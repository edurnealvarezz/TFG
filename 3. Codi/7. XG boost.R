packages <- c("dplyr", "ggplot2", "tibble", "tidyr", "xgboost", "caret", "pROC")

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

lapply(packages, install_if_missing)
rm(packages)

setwd("C:/Users/edurn/Downloads/TFG")
load("2. Dades/4. Dades EFA.RData")

motius_vars <- readRDS("2. Dades/motius_vars.rds")
estrategies_vars <- readRDS("2. Dades/estrategies_vars.rds")
ia_vars <- readRDS("2. Dades/ia_vars.rds")

sink("4. Outputs/7.1 Output_text_xgb.txt")
pdf("4. Outputs/7.2 Output_grafics_xgb.pdf", width = 10, height = 8)

#### ============================================================ ####
####                   0. PREPARACIÓ DE DADES                     ####
#### ============================================================ ####

dades_xgb <- dades_def %>%
  mutate(
    Y = as.integer(GRUP_ASSIST == "Regular (≥80%)"),
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
predictors <- predictors[predictors %in% names(dades_xgb)]

dades_xgb_net <- dades_xgb %>%
  select(Y, all_of(predictors)) %>%
  drop_na()

cat("=================================================================\n")
cat("   0. PREPARACIÓ DE DADES\n")
cat("=================================================================\n\n")
cat(sprintf("Observacions totals: %d\n", nrow(dades_xgb_net)))
cat(sprintf("Predictors inclosos: %d\n", length(predictors)))
cat(sprintf("Distribució Y — Irregular (0): %d | Regular (1): %d\n\n",
            sum(dades_xgb_net$Y == 0), sum(dades_xgb_net$Y == 1)))

# Partició 70% train | 15% val (early stopping) | 15% test (avaluació final)
# Val set separat del test per evitar data leakage en la selecció de hiperparàmetres
set.seed(1234)
idx_train <- createDataPartition(dades_xgb_net$Y, p = 0.70, list = FALSE)
dades_rest <- dades_xgb_net[-idx_train, ]
idx_val <- createDataPartition(dades_rest$Y, p = 0.50, list = FALSE)

dades_train_xgb <- dades_xgb_net[idx_train, ]
dades_val_xgb <- dades_rest[idx_val, ]
dades_test_xgb <- dades_rest[-idx_val, ]

Y_train <- dades_train_xgb$Y
Y_val <- dades_val_xgb$Y
Y_test <- dades_test_xgb$Y
X_train <- apply(dades_train_xgb[, predictors], 2, as.numeric)
X_val <- apply(dades_val_xgb[, predictors], 2, as.numeric)
X_test <- apply(dades_test_xgb[, predictors], 2, as.numeric)

dtrain <- xgb.DMatrix(X_train, label = Y_train)
dval <- xgb.DMatrix(X_val, label = Y_val)
dtest <- xgb.DMatrix(X_test, label = Y_test)

cat(sprintf("Partició: Train = %d | Val = %d | Test = %d\n",
            length(Y_train), length(Y_val), length(Y_test)))
cat(sprintf("  Train — Regular: %.1f%% | Irregular: %.1f%%\n",
            mean(Y_train) * 100, (1 - mean(Y_train)) * 100))
cat(sprintf("  Val   — Regular: %.1f%% | Irregular: %.1f%%\n",
            mean(Y_val) * 100, (1 - mean(Y_val)) * 100))
cat(sprintf("  Test  — Regular: %.1f%% | Irregular: %.1f%%\n\n",
            mean(Y_test) * 100, (1 - mean(Y_test)) * 100))

# ----------------------------------------------------------------
# Funcions de mètriques (prenen prob directament)
# ----------------------------------------------------------------
calcular_metriques_xgb <- function(prob, Y_vec, nom_model,
                                   auc_cv_mean = NA, auc_cv_sd = NA) {
  roc_obj <- roc(Y_vec, prob, quiet = TRUE)
  auc_val <- as.numeric(auc(roc_obj))

  coords_r <- coords(roc_obj, "best",
                     ret = c("threshold", "sensitivity", "specificity"),
                     best.method = "youden")
  thresh <- coords_r$threshold[1]

  pred <- as.integer(prob >= thresh)
  TP <- sum(pred == 1 & Y_vec == 1)
  TN <- sum(pred == 0 & Y_vec == 0)
  FP <- sum(pred == 1 & Y_vec == 0)
  FN <- sum(pred == 0 & Y_vec == 1)

  accuracy <- (TP + TN) / (TP + TN + FP + FN)
  precision <- ifelse(TP + FP > 0, TP / (TP + FP), NA)
  recall <- ifelse(TP + FN > 0, TP / (TP + FN), NA)
  specificity <- ifelse(TN + FP > 0, TN / (TN + FP), NA)
  f1 <- ifelse(!is.na(precision) & !is.na(recall) & (precision + recall) > 0,
               2 * precision * recall / (precision + recall), NA)
  balanced_acc <- (recall + specificity) / 2

  list(
    model = nom_model,
    n_test = length(Y_vec),
    threshold = round(thresh, 3),
    AUC = round(auc_val, 4),
    AUC_cv_mean = round(auc_cv_mean, 4),
    AUC_cv_sd = round(auc_cv_sd, 4),
    accuracy = round(accuracy, 4),
    precision = round(precision, 4),
    recall = round(recall, 4),
    specificity = round(specificity, 4),
    F1 = round(f1, 4),
    balanced_accuracy = round(balanced_acc, 4),
    TP = TP, TN = TN, FP = FP, FN = FN
  )
}

mostrar_metriques_xgb <- function(met, titol = NULL) {
  if (is.null(titol)) titol <- met$model
  cat(sprintf("\n--- Mètriques: %s ---\n", titol))
  cat(sprintf("n = %d | Llindar Youden = %.3f\n", met$n_test, met$threshold))
  if (!is.na(met$AUC_cv_mean)) {
    cat(sprintf("AUC (val CV):           %.4f ± %.4f\n",
                met$AUC_cv_mean, met$AUC_cv_sd))
  }
  cat(sprintf("AUC:                    %.4f\n", met$AUC))
  cat(sprintf("Accuracy:               %.4f\n", met$accuracy))
  cat(sprintf("Precision (PPV):        %.4f\n", met$precision))
  cat(sprintf("Recall (Sensibilitat):  %.4f\n", met$recall))
  cat(sprintf("Especificitat:          %.4f\n", met$specificity))
  cat(sprintf("F1:                     %.4f\n", met$F1))
  cat(sprintf("Balanced Accuracy:      %.4f\n\n", met$balanced_accuracy))

  cat("Matriu de confusió:\n")
  cm <- matrix(c(met$TN, met$FN, met$FP, met$TP), nrow = 2,
               dimnames = list(Observat = c("Irregular(0)", "Regular(1)"),
                               Predit = c("Irregular(0)", "Regular(1)")))
  print(cm)

  df_cm <- data.frame(
    Observat = factor(c("Irregular", "Irregular", "Regular", "Regular"),
                      levels = c("Regular", "Irregular")),
    Predit = factor(c("Irregular", "Regular", "Irregular", "Regular"),
                    levels = c("Irregular", "Regular")),
    n = c(met$TN, met$FP, met$FN, met$TP),
    etiq = c("TN", "FP", "FN", "TP")
  )

  p_cm <- ggplot(df_cm, aes(x = Predit, y = Observat, fill = n)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = paste0(etiq, "\n", n)), size = 5, fontface = "bold") +
    scale_fill_gradient(low = "#EBF5FB", high = "#2471A3", guide = "none") +
    labs(title = sprintf("Matriu de confusió — %s", titol),
         subtitle = sprintf("Llindar Youden = %.3f", met$threshold),
         x = "Valor Predit", y = "Valor Observat") +
    theme_minimal(base_size = 13) +
    theme(panel.grid = element_blank())
  print(p_cm)
}

#### ============================================================ ####
####        1. GRID SEARCH — SELECCIÓ D'HIPERPARÀMETRES           ####
#### ============================================================ ####

cat("=================================================================\n")
cat("   1. GRID SEARCH (early stopping sobre val)\n")
cat("=================================================================\n\n")

# Params fixos fora del grid
eta_fix <- 0.01
subsample_fix <- 0.6
colsample_fix <- 0.6

grid <- expand.grid(
  max_depth = c(2, 3, 4),
  min_child_weight = c(5, 10, 15),
  gamma = c(0, 1, 2),
  lambda = c(1, 3, 5),
  alpha = c(0, 0.5, 1),
  stringsAsFactors = FALSE
)

cat(sprintf("Combinacions a avaluar: %d\n", nrow(grid)))
cat(sprintf("Params fixos: eta = %.2f | subsample = %.1f | colsample_bytree = %.1f\n",
            eta_fix, subsample_fix, colsample_fix))
cat("Early stopping: 50 rondes sense millora en val AUC\n\n")

set.seed(1234)
grid_results <- vector("list", nrow(grid))

for (i in seq_len(nrow(grid))) {
  p_i <- list(
    objective = "binary:logistic",
    eval_metric = "auc",
    eta = eta_fix,
    max_depth = grid$max_depth[i],
    subsample = subsample_fix,
    colsample_bytree = colsample_fix,
    min_child_weight = grid$min_child_weight[i],
    gamma = grid$gamma[i],
    lambda = grid$lambda[i],
    alpha = grid$alpha[i]
  )

  m_i <- xgb.train(
    params = p_i,
    data = dtrain,
    nrounds = 1000,
    evals = list(train = dtrain, eval = dval),
    early_stopping_rounds = 50,
    verbose = 0
  )

  # Extreure best_iteration i val AUC directament del log (robust)
  eval_log <- m_i$evaluation_log
  auc_col <- grep("eval.*auc", names(eval_log), value = TRUE)[1]
  if (!is.na(auc_col)) {
    best_i <- which.max(eval_log[[auc_col]])
    best_score_i <- eval_log[[auc_col]][best_i]
  } else {
    best_i <- nrow(eval_log)
    best_score_i <- NA_real_
  }
  best_i <- as.integer(best_i)
  best_score_i <- as.numeric(best_score_i)

  grid_results[[i]] <- data.frame(
    max_depth = grid$max_depth[i],
    min_child_weight = grid$min_child_weight[i],
    gamma = grid$gamma[i],
    lambda = grid$lambda[i],
    alpha = grid$alpha[i],
    best_nrounds = best_i,
    val_auc = best_score_i,
    stringsAsFactors = FALSE
  )

  if (i %% 25 == 0) {
    cat(sprintf("  %d / %d combinacions completades...\n", i, nrow(grid)))
  }
}

print(best_i)
print(best_score_i)
str(best_i)
str(best_score_i)

df_grid <- do.call(rbind, grid_results)
df_grid <- df_grid[order(-df_grid$val_auc), ]

cat("\nTop 10 combinacions per AUC de validació:\n")
print(head(df_grid, 10), row.names = FALSE)

best_row <- df_grid[1, ]
best_nrounds <- best_row$best_nrounds
best_val_auc <- best_row$val_auc

cat(sprintf("\nMillors hiperparàmetres (val AUC = %.4f):\n", best_val_auc))
cat(sprintf("  max_depth = %d | min_child_weight = %d | gamma = %.1f\n",
            best_row$max_depth, best_row$min_child_weight, best_row$gamma))
cat(sprintf("  lambda = %.1f | alpha = %.1f | nrounds = %d\n\n",
            best_row$lambda, best_row$alpha, best_nrounds))

#### ============================================================ ####
####              2. MODEL FINAL XGBoost                          ####
#### ============================================================ ####

cat("=================================================================\n")
cat("   2. MODEL FINAL XGBoost (millors hiperparàmetres del grid)\n")
cat("=================================================================\n\n")

best_params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  eta = eta_fix,
  max_depth = best_row$max_depth,
  subsample = subsample_fix,
  colsample_bytree = colsample_fix,
  min_child_weight = best_row$min_child_weight,
  gamma = best_row$gamma,
  lambda = best_row$lambda,
  alpha = best_row$alpha
)

set.seed(1234)
xgb_model <- xgb.train(
  params = best_params,
  data = dtrain,
  nrounds = best_nrounds,
  evals = list(train = dtrain, eval = dval),
  early_stopping_rounds = 50,
  verbose = 1
)

best_nrounds <- xgb_model$best_iteration
if (!isTRUE(best_nrounds >= 1)) best_nrounds <- xgb_model$niter
best_val_auc <- xgb_model$best_score

cat(sprintf("\nnrounds finals: %d | Val AUC: %.4f\n\n", best_nrounds, best_val_auc))

prob_test_xgb <- predict(xgb_model, dtest)
prob_val_xgb <- predict(xgb_model, dval)
prob_train_xgb <- predict(xgb_model, dtrain)   # in-sample (optimista)

roc_xgb <- roc(Y_test, prob_test_xgb, quiet = TRUE)
roc_df <- data.frame(
  spec_inv = 1 - roc_xgb$specificities,
  sens = roc_xgb$sensitivities
)

ggplot(roc_df, aes(x = spec_inv, y = sens)) +
  geom_path(color = "#4A90B8", linewidth = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  annotate("text", x = 0.62, y = 0.2,
           label = sprintf("AUC test = %.3f\nAUC val = %.3f",
                           as.numeric(auc(roc_xgb)), best_val_auc),
           size = 4.5, color = "#4A90B8") +
  labs(title = "Corba ROC — XGBoost (test)",
       x = "1 - Especificitat", y = "Sensibilitat") +
  theme_minimal(base_size = 13)

#### ============================================================ ####
####         3. IMPORTÀNCIA DE VARIABLES                          ####
#### ============================================================ ####

cat("=================================================================\n")
cat("   3. IMPORTÀNCIA DE VARIABLES\n")
cat("=================================================================\n\n")

imp_xgb <- xgb.importance(model = xgb_model, feature_names = colnames(X_train))
cat("Top 20 variables per importància (Gain):\n")
print(imp_xgb[1:min(20, nrow(imp_xgb)), ])

df_imp <- as_tibble(imp_xgb) %>% slice_head(n = 20)

ggplot(df_imp, aes(x = reorder(Feature, Gain), y = Gain, fill = Gain)) +
  geom_col(alpha = 0.9) +
  coord_flip() +
  scale_fill_gradient(low = "#AED6F1", high = "#1A5276", guide = "none") +
  labs(title = "Importància de variables — XGBoost",
       subtitle = "Top 20 | Mesura: Gain (reducció d'impuresa per splits)",
       x = "", y = "Gain") +
  theme_minimal(base_size = 13)

#### ============================================================ ####
####                    4. SHAP VALUES (TreeSHAP)                 ####
#### ============================================================ ####

cat("=================================================================\n")
cat("   4. SHAP VALUES (TreeSHAP exacte)\n")
cat("=================================================================\n\n")

# TreeSHAP exacte built-in de xgboost
# L'última columna és el BIAS (intercept) → s'elimina
shap_matrix <- predict(xgb_model, dtest, predcontrib = TRUE)
shap_df <- as.data.frame(shap_matrix[, -ncol(shap_matrix)])
names(shap_df) <- colnames(X_test)

cat(sprintf("SHAP (TreeSHAP exacte) per a %d obs i %d variables.\n\n",
            nrow(shap_df), ncol(shap_df)))

shap_imp <- tibble(
  variable = names(shap_df),
  mean_abs_shap = colMeans(abs(shap_df))
) %>%
  arrange(desc(mean_abs_shap))

cat("Top 20 variables per importància SHAP:\n")
print(shap_imp %>% slice_head(n = 20))

# Gràfic 4a: Importància SHAP
shap_top20 <- shap_imp %>% slice_head(n = 20)

ggplot(shap_top20, aes(x = reorder(variable, mean_abs_shap),
                       y = mean_abs_shap, fill = mean_abs_shap)) +
  geom_col(alpha = 0.9) +
  coord_flip() +
  scale_fill_gradient(low = "#A9DFBF", high = "#1E8449", guide = "none") +
  labs(title = "Importància SHAP — XGBoost",
       subtitle = "Top 20 | mean(|SHAP|) sobre conjunt test",
       x = "", y = "Importància SHAP (mean |SHAP|)") +
  theme_minimal(base_size = 13)

# Gràfic 4b: Beeswarm (top 15)
top15_vars <- shap_imp$variable[seq_len(min(15, nrow(shap_imp)))]

shap_long <- shap_df %>%
  select(all_of(top15_vars)) %>%
  mutate(obs = row_number()) %>%
  pivot_longer(-obs, names_to = "variable", values_to = "shap") %>%
  left_join(
    as.data.frame(X_test) %>%
      select(all_of(top15_vars)) %>%
      mutate(obs = row_number()) %>%
      pivot_longer(-obs, names_to = "variable", values_to = "valor"),
    by = c("obs", "variable")
  ) %>%
  mutate(variable = factor(variable, levels = rev(top15_vars)))

ggplot(shap_long, aes(x = shap, y = variable, color = valor)) +
  geom_jitter(height = 0.25, size = 1.2, alpha = 0.6) +
  geom_vline(xintercept = 0, color = "grey40", linewidth = 0.8) +
  scale_color_gradient(low = "#2471A3", high = "#E74C3C",
                       name = "Valor\nde la variable") +
  labs(title = "SHAP Beeswarm — XGBoost (top 15 variables)",
       subtitle = "Color = valor de la variable | x > 0 → augmenta P(Regular)",
       x = "Valor SHAP", y = "") +
  theme_minimal(base_size = 12)

# Gràfic 4c: Dependence plots (top 4)
top4_vars <- shap_imp$variable[1:4]

for (v in top4_vars) {
  df_dep <- data.frame(valor = X_test[, v], shap = shap_df[[v]])

  p <- ggplot(df_dep, aes(x = valor, y = shap)) +
    geom_point(alpha = 0.5, color = "#4A90B8", size = 1.8) +
    geom_smooth(method = "loess", se = TRUE, color = "#E07B54",
                fill = "#F0B27A", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    labs(title = sprintf("SHAP Dependence Plot — %s", v),
         subtitle = "Línea taronja = tendència LOESS",
         x = v, y = "Valor SHAP") +
    theme_minimal(base_size = 13)

  print(p)
}

#### ============================================================ ####
####          5. MÈTRIQUES DE CLASSIFICACIÓ                       ####
#### ============================================================ ####

cat("\n=================================================================\n")
cat("   5. MÈTRIQUES DE CLASSIFICACIÓ (llindar Youden)\n")
cat("=================================================================\n\n")

# --- 5a Test (avaluació final) ---
cat("--- 5a Mètriques sobre conjunt test ---\n")

metriques_xgb <- calcular_metriques_xgb(
  prob = prob_test_xgb,
  Y_vec = Y_test,
  nom_model = "XGBoost"
)

mostrar_metriques_xgb(metriques_xgb)

# --- 5b Validació (estimació imparcial; usada per seleccionar hiperparàmetres) ---
cat("--- 5b Mètriques sobre conjunt de validació ---\n")
cat("    [Val: estimació imparcial usada en el grid search]\n\n")

metriques_xgb_val <- calcular_metriques_xgb(
  prob = prob_val_xgb,
  Y_vec = Y_val,
  nom_model = "XGBoost (val)"
)

mostrar_metriques_xgb(metriques_xgb_val)

# --- 5c Train in-sample (per diagnosi d'overfitting) ---
cat("--- 5c Mètriques sobre train (in-sample) ---\n")
cat("    [In-sample: OPTIMISTA per definició]\n\n")

metriques_xgb_train <- calcular_metriques_xgb(
  prob = prob_train_xgb,
  Y_vec = Y_train,
  nom_model = "XGBoost (train in-sample)"
)

mostrar_metriques_xgb(metriques_xgb_train)

# Taula resum overfitting (train in-sample vs val vs test)
cat("\n--- Resum overfitting: train vs val vs test ---\n")
cat("  [Val ≈ Test → model generalitza bé | Train >> Val → overfitting]\n\n")

df_ov_xgb <- data.frame(
  Conjunt = c("Train (in-sample)", "Validació", "Test"),
  AUC = c(metriques_xgb_train$AUC, metriques_xgb_val$AUC, metriques_xgb$AUC),
  Accuracy = c(metriques_xgb_train$accuracy, metriques_xgb_val$accuracy,
               metriques_xgb$accuracy),
  F1 = c(metriques_xgb_train$F1, metriques_xgb_val$F1, metriques_xgb$F1),
  Balanced_Acc = c(metriques_xgb_train$balanced_accuracy,
                   metriques_xgb_val$balanced_accuracy,
                   metriques_xgb$balanced_accuracy)
)
print(df_ov_xgb, row.names = FALSE)
cat("\n")

#### ============================================================ ####
####         6. COMPARACIÓ GLOBAL DE MODELS                       ####
#### ============================================================ ####

cat("=================================================================\n")
cat("   6. COMPARACIÓ GLOBAL DE MODELS\n")
cat("=================================================================\n\n")

models_llista <- list()

if (file.exists("2. Dades/metriques_logit.rds")) {
  models_llista[["Logit"]] <- readRDS("2. Dades/metriques_logit.rds")
}
if (file.exists("2. Dades/metriques_rf.rds")) {
  models_llista[["RF"]] <- readRDS("2. Dades/metriques_rf.rds")
}
models_llista[["XGBoost"]] <- metriques_xgb

df_comp <- do.call(rbind, lapply(models_llista, function(m) {
  cv_info <- if (!is.na(m$AUC_cv_mean)) {
    sprintf("%.4f ± %.4f", m$AUC_cv_mean, m$AUC_cv_sd)
  } else if (!is.null(m$OOB_error) && !is.na(m$OOB_error)) {
    sprintf("OOB err = %.4f", m$OOB_error)
  } else {
    "—"
  }

  data.frame(
    Model = m$model,
    AUC_CV = cv_info,
    AUC_test = m$AUC,
    Accuracy = m$accuracy,
    Precision = m$precision,
    Recall = m$recall,
    F1 = m$F1,
    Balanced_Acc = m$balanced_accuracy,
    stringsAsFactors = FALSE
  )
}))
rownames(df_comp) <- NULL

cat("Taula comparativa de models (sobre conjunt test):\n\n")
print(df_comp, row.names = FALSE)

#### ============================================================ ####
####          7. GUARDAR MÈTRIQUES PER A COMPARACIÓ               ####
#### ============================================================ ####

saveRDS(metriques_xgb, "2. Dades/metriques_xgb.rds")
cat("\n→ Mètriques guardades a: 2. Dades/metriques_xgb.rds\n\n")

cat("Vista prèvia del format de mètriques:\n")
print(as.data.frame(metriques_xgb[c("model", "n_test", "threshold",
                                     "AUC_cv_mean", "AUC", "accuracy",
                                     "precision", "recall", "F1",
                                     "balanced_accuracy")]))

sink()
dev.off()
