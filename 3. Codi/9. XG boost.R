packages <- c("dplyr", "ggplot2", "tibble", "tidyr", "xgboost", "caret", "pROC", "PRROC")

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

lapply(packages, install_if_missing)
rm(packages)

setwd("C:/Users/edurn/Downloads/TFG")
load("2. Dades/8. Dades Random Forest.RData")

source("3. Codi/Funcions models.R")

MIN_RECALL <- 0.6

motius_vars <- readRDS("2. Dades/motius_vars.rds")
estrategies_vars <- readRDS("2. Dades/estrategies_vars.rds")
ia_vars <- readRDS("2. Dades/ia_vars.rds")

sink("4. Outputs/9.1 Output_text_xgb.txt")
pdf("4. Outputs/9.2 Output_grafics_xgb.pdf", width = 10, height = 8)

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

# IA ho posem de dues formes: 1 factor i el segon factor desglosat perque ja hem
# vist als altres models que era millor predictor

ia_vars <- c("IA_SUBST_num", "IA_ATENC_num", "IA_CONF_num")
vars_fa <- c("MOT_DESMOTIVACIO", "MOT_AUTOGESTIO", "MOT_FORCA_MAJOR",
             "EST_QUALITAT_DOC", "EST_AVALUACIO_AC", "EST_TEMPS_CLASSE",
             "EST_GRUPS_REDUITS", "IA_EINA_ESTUDI")
vars_acad <- c("NOTA_num", "T_AVAL_num", "CURS_1R_num", "N_ASSIG")
vars_pers <- c("EDAT", "DESPL")

predictors <- c(ia_vars,
                vars_fa, vars_acad, vars_pers)
predictors <- predictors[predictors %in% names(dades_xgb)]

dades_xgb_net <- dades_xgb %>%
  select(Y, all_of(predictors)) %>%
  drop_na()

cat(" ============= 0. PREPARACIÓ DE DADES ============= \n")

cat(sprintf("Observacions totals: %d\n", nrow(dades_xgb_net)))
cat(sprintf("Predictors inclosos: %d\n", length(predictors)))
cat(sprintf("Distribució Y — Irregular (0): %d | Regular (1): %d\n\n",
            sum(dades_xgb_net$Y == 0), sum(dades_xgb_net$Y == 1)))

# Partició 80/20 compartida
idx_train <- crear_o_carregar_particio(dades_xgb_net$Y)
dades_train_xgb <- dades_xgb_net[idx_train, ]
dades_test_xgb  <- dades_xgb_net[-idx_train, ]

Y_train <- dades_train_xgb$Y
Y_test  <- dades_test_xgb$Y
X_train <- apply(dades_train_xgb[, predictors], 2, as.numeric)
X_test  <- apply(dades_test_xgb[, predictors],  2, as.numeric)

dtrain <- xgb.DMatrix(X_train, label = Y_train)
dtest  <- xgb.DMatrix(X_test,  label = Y_test)

cat(sprintf("Particio: Train = %d | Test = %d\n", length(Y_train), length(Y_test)))
cat(sprintf("  Train — Regular: %.1f%% | Irregular: %.1f%%\n",
            mean(Y_train) * 100, (1 - mean(Y_train)) * 100))
cat(sprintf("  Test  — Regular: %.1f%% | Irregular: %.1f%%\n\n",
            mean(Y_test) * 100, (1 - mean(Y_test)) * 100))

source("3. Codi/Funcions models.R")

#### ============================================================ ####
####                  1. SELECCIÓ D'HIPERPARÀMETRES               ####
#### ============================================================ ####

cat(" ================= GRID SEARCH ================= \n")

# paràmetres fixos fora del grid
eta_fix <- 0.01
subsample_fix <- 0.6
colsample_fix <- 0.6

# Grid reduït: 3×3×2×3 = 54 combinacions (era 3^5 = 243)
# alpha=0 quasi sempre és òptim en datasets petits; gamma s'ha reduït a 2 valors
grid <- expand.grid(
  max_depth = c(2, 3, 4),
  min_child_weight = c(5, 10, 15),
  gamma = c(0, 1),
  lambda = c(1, 3, 5),
  stringsAsFactors = FALSE
)
alpha_fix <- 0  # fixem alpha=0; redueix >4x les combinacions sense perdre qualitat

cat(sprintf("Combinacions a avaluar: %d (alpha fixat a %.1f)\n", nrow(grid), alpha_fix))
cat(sprintf("Params fixos: eta = %.2f | subsample = %.1f | colsample_bytree = %.1f\n",
            eta_fix, subsample_fix, colsample_fix))
cat("Seleccio per 5-fold CV AUC | early stopping (20 rounds sense millora)\n\n")

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
    alpha = alpha_fix
  )

  cv_i <- xgb.cv(
    params = p_i,
    data = dtrain,
    nrounds = 500,
    nfold = 5,
    stratified = TRUE,
    verbose = 0,
    showsd = FALSE,
    prediction = TRUE,
    early_stopping_rounds = 20,  # atura quan l'AUC no millora en 20 rounds
    maximize = TRUE
  )
  cv_auc_i <- max(cv_i$evaluation_log$test_auc_mean)

  # best_iteration pot ser NULL si early stopping no es dispara (millora constant)
  bi <- cv_i$best_iteration
  best_nr_i <- if (!is.null(bi) && length(bi) > 0 && !is.na(bi) && bi > 0L) {
    as.integer(bi)
  } else {
    which.max(cv_i$evaluation_log$test_auc_mean)
  }

  grid_results[[i]] <- data.frame(
    max_depth = grid$max_depth[i],
    min_child_weight = grid$min_child_weight[i],
    gamma = grid$gamma[i],
    lambda = grid$lambda[i],
    alpha = alpha_fix,
    cv_auc = cv_auc_i,
    best_nrounds = best_nr_i,
    stringsAsFactors = FALSE
  )

  if (i %% 10 == 0) {
    cat(sprintf("  %d / %d combinacions completades...\n", i, nrow(grid)))
  }
}

df_grid <- do.call(rbind, grid_results)
df_grid <- df_grid[order(-df_grid$cv_auc), ]

cat("\nTop 10 combinacions per AUC CV:\n")
print(head(df_grid, 10), row.names = FALSE)

best_row <- df_grid[1, ]

cat(sprintf("\nMillors hiperparàmetres (CV AUC = %.4f):\n", best_row$cv_auc))
cat(sprintf("  max_depth = %d | min_child_weight = %d | gamma = %.1f\n",
            best_row$max_depth, best_row$min_child_weight, best_row$gamma))
cat(sprintf("  lambda = %.1f | alpha = %.1f | best_nrounds = %d\n\n",
            best_row$lambda, best_row$alpha, best_row$best_nrounds))

#### ============================================================ ####
####              2. MODEL FINAL XGBoost                          ####
#### ============================================================ ####

cat(" ================== MODEL FINAL ==================== \n")

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

# nrounds optims del grid search (via CV), entrena sobre tot el train
nrounds_final <- best_row$best_nrounds
if (is.null(nrounds_final) || length(nrounds_final) == 0 ||
    is.na(nrounds_final) || nrounds_final < 1L) {
  nrounds_final <- 50L
  cat(sprintf("AVIS: best_nrounds no valid, usant fallback = %d\n", nrounds_final))
}
cat(sprintf("Entrenant model final amb nrounds = %d\n\n", nrounds_final))

set.seed(1234)
xgb_model <- xgb.train(
  params = best_params,
  data = dtrain,
  nrounds = nrounds_final,
  verbose = 0
)

prob_test_xgb  <- predict(xgb_model, dtest)
prob_train_xgb <- predict(xgb_model, dtrain)

roc_xgb <- pROC::roc(Y_test, prob_test_xgb, quiet = TRUE)
auc_test_xgb <- as.numeric(pROC::auc(roc_xgb))
auc_train_xgb <- as.numeric(pROC::auc(pROC::roc(Y_train, prob_train_xgb, quiet = TRUE)))
cat(sprintf("AUC train (in-sample): %.4f | AUC test: %.4f\n\n", auc_train_xgb, auc_test_xgb))

roc_df <- data.frame(spec_inv = 1 - roc_xgb$specificities, sens = roc_xgb$sensitivities)
print(
  ggplot(roc_df, aes(x = spec_inv, y = sens)) +
    geom_path(color = "#4A90B8", linewidth = 1.2) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
    annotate("text", x = 0.62, y = 0.2,
             label = sprintf("AUC test = %.3f", auc_test_xgb),
             size = 4.5, color = "#4A90B8") +
    labs(title = "Corba ROC — XGBoost (test)",
         x = "1 - Especificitat", y = "Sensibilitat") +
    theme_minimal(base_size = 13)
)

# Llindar PR sobre probabilitats OOF del CV (no contamina el test)
# tryCatch: si xgb.cv o $pred fallen, fallback a train in-sample
set.seed(1234)
pr_xgb <- tryCatch({
  cv_oof_xgb <- xgb.cv(
    params = best_params, data = dtrain,
    nrounds = nrounds_final,
    nfold = 5, stratified = TRUE, verbose = 0, prediction = TRUE
  )
  oof_probs_xgb <- cv_oof_xgb$pred
  if (is.null(oof_probs_xgb) || length(oof_probs_xgb) != length(Y_train))
    stop("OOF predictions invàlides")
  cat("Llindar calculat sobre OOF 5-fold CV.\n")
  seleccionar_llindar_pr(oof_probs_xgb, Y_train, MIN_RECALL)
}, error = function(e) {
  cat(sprintf("AVIS OOF CV: %s — usant train in-sample com a fallback.\n", conditionMessage(e)))
  seleccionar_llindar_pr(prob_train_xgb, Y_train, MIN_RECALL)
})
thresh_pr_xgb <- pr_xgb$threshold
cat(sprintf("XGBoost — AUPRC: %.4f | Llindar PR: %.4f | recall_ok: %s\n\n",
            pr_xgb$auprc, thresh_pr_xgb,
            ifelse(pr_xgb$recall_ok, "SI", "NO (fallback Youden)")))

pr_test_xgb <- seleccionar_llindar_pr(prob_test_xgb, Y_test, MIN_RECALL)
print(
  ggplot(pr_test_xgb$pr_curve, aes(x = recall, y = precision)) +
    geom_path(color = "#4A90B8", linewidth = 1) +
    geom_vline(xintercept = MIN_RECALL, linetype = "dashed",
               color = "red", linewidth = 0.8) +
    geom_point(data = data.frame(recall = pr_test_xgb$recall,
               precision = ifelse(is.na(pr_test_xgb$precision), 0, pr_test_xgb$precision)),
               color = "#E07B54", size = 3, shape = 17) +
    labs(title = "Corba Precisio-Recall — XGBoost (test)",
         subtitle = sprintf("AUPRC = %.4f | Llindar = %.4f", pr_test_xgb$auprc, thresh_pr_xgb),
         x = "Recall", y = "Precisio (PPV)") +
    theme_minimal(base_size = 13)
)

#### ============================================================ ####
####         3. IMPORTÀNCIA DE VARIABLES                          ####
#### ============================================================ ####

# es mira la importància segons les que ajuden a minimitzar la impuresa
# són les que tenen un gain més elevat

cat(" ===================== IMPORTÀNCIA DE VARIABLES ===================== \n")

imp_xgb <- xgb.importance(model = xgb_model, feature_names = colnames(X_train))
cat("Top 20 variables per importància (Gain):\n")
print(imp_xgb[1:min(20, nrow(imp_xgb)), ])

df_imp <- as_tibble(imp_xgb) %>% slice_head(n = 20)

print(
  ggplot(df_imp, aes(x = reorder(Feature, Gain), y = Gain, fill = Gain)) +
    geom_col(alpha = 0.9) +
    coord_flip() +
    scale_fill_gradient(low = "#AED6F1", high = "#1A5276", guide = "none") +
    labs(title = "Importància de variables — XGBoost",
         subtitle = "Top 20 | Mesura: Gain (reducció d'impuresa per splits)",
         x = "", y = "Gain") +
    theme_minimal(base_size = 13)
)

#### ================================================= ####
####                    4. SHAP VALUES                 ####
#### ================================================= ####

# mira quin % de probabilitat s'atribueix a cada variable

cat(" =============== SHAP VALUES ===============\n")

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

# Gràfic: Importància SHAP
shap_top20 <- shap_imp %>% slice_head(n = 20)

print(
  ggplot(shap_top20, aes(x = reorder(variable, mean_abs_shap),
                         y = mean_abs_shap, fill = mean_abs_shap)) +
    geom_col(alpha = 0.9) +
    coord_flip() +
    scale_fill_gradient(low = "#A9DFBF", high = "#1E8449", guide = "none") +
    labs(title = "Importància SHAP — XGBoost",
         subtitle = "Top 20 | mean(|SHAP|) sobre conjunt test",
         x = "", y = "Importància SHAP (mean |SHAP|)") +
    theme_minimal(base_size = 13)
)

# Gràfic: Beeswarm
# cada punt és un alumne del test, si està a la dreta -> variable ajuda a que sigui Regular
# si està a l'esquerra -> Irregular
# color vermell: valor elevat i blau: valor baix

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

print(
  ggplot(shap_long, aes(x = shap, y = variable, color = valor)) +
    geom_jitter(height = 0.25, size = 1.2, alpha = 0.6) +
    geom_vline(xintercept = 0, color = "grey40", linewidth = 0.8) +
    scale_color_gradient(low = "#2471A3", high = "#E74C3C",
                         name = "Valor\nde la variable") +
    labs(title = "SHAP Beeswarm — XGBoost (top 15 variables)",
         subtitle = "Color = valor de la variable | x > 0 → augmenta P(Regular)",
         x = "Valor SHAP", y = "") +
    theme_minimal(base_size = 12)
)

# Gràfic: Dependence plots
# mira les 4 variables més importants
# si punt > 0 -> variable ajuda a que l'alumne assisteixi més
# si punt < 0 -> valor emputxa a que l'alumne falti a classe
# mirar la linea: si baixa a mesura q augmenta variable la prob d'anar a classe disminueix

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
####               5. MÈTRIQUES DE CLASSIFICACIÓ                  ####
#### ============================================================ ####

cat(" =================== MÈTRIQUES DE CLASSIFICACIÓ =================== \n")

# ------------ Mètriques sobre el conjunt test ------------
cat(" Mètriques sobre conjunt test \n")

metriques_xgb <- calcular_metriques_xgb(
  prob = prob_test_xgb,
  Y_vec = Y_test,
  nom_model = "XGBoost",
  thresh_override = thresh_pr_xgb
)

mostrar_metriques_xgb(metriques_xgb)

# ------------ Mètriques sobre el conjunt de train ------------
cat("Mètriques sobre train (in-sample) \n\n")

metriques_xgb_train <- calcular_metriques_xgb(
  prob = prob_train_xgb,
  Y_vec = Y_train,
  nom_model = "XGBoost (train in-sample)",
  thresh_override = thresh_pr_xgb
)
mostrar_metriques_xgb(metriques_xgb_train)

# Taula resum overfitting
cat("\n--- Resum overfitting: train vs test ---\n\n")

df_ov_xgb <- data.frame(
  Conjunt      = c("Train (in-sample)", "Test"),
  AUC          = c(metriques_xgb_train$AUC,          metriques_xgb$AUC),
  Accuracy     = c(metriques_xgb_train$accuracy,      metriques_xgb$accuracy),
  F1           = c(metriques_xgb_train$F1,            metriques_xgb$F1),
  Balanced_Acc = c(metriques_xgb_train$balanced_accuracy, metriques_xgb$balanced_accuracy)
)
print(df_ov_xgb, row.names = FALSE)
cat("\n")

#### ============================================================ ####
####              6. COMPARACIÓ GLOBAL DE MODELS                  ####
#### ============================================================ ####

cat(" =================== COMPARACIÓ GLOBAL DE MODELS =================== \n")

models_llista <- list()

fitxers <- c(
  Logit = "2. Dades/metriques_logit.rds",
  `RF-A` = "2. Dades/metriques_rf_a.rds",
  `RF-B` = "2. Dades/metriques_rf_b.rds"
)

for (nom in names(fitxers)) {
  if (file.exists(fitxers[[nom]])) {
    models_llista[[nom]] <- readRDS(fitxers[[nom]])
  }
}
models_llista[["XGBoost"]] <- metriques_xgb

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
    labs(title = "Comparació de models",
         subtitle = "Mètriques sobre conjunt test",
         x = "", y = "Valor") +
    theme_minimal(base_size = 13) +
    theme(axis.text.x = element_text(angle = 25, hjust = 1),
          legend.position = "bottom")
)

saveRDS(metriques_xgb, "2. Dades/metriques_xgb.rds")

# --- Guardar probabilitats i bbdd ---
predictors_ok <- predictors[predictors %in% names(dades_xgb)]
X_all_xgb_mat <- apply(dades_xgb[, predictors_ok], 2, as.numeric)
complete_xgb <- complete.cases(X_all_xgb_mat)
dmat_all_xgb <- xgb.DMatrix(X_all_xgb_mat[complete_xgb, ])
prob_xgb_tots <- predict(xgb_model, dmat_all_xgb)
dades_def$prob_xgb <- NA_real_
dades_def$prob_xgb[complete_xgb] <- prob_xgb_tots
dades_def$pred_xgb <- NA_integer_
dades_def$pred_xgb[complete_xgb] <- as.integer(prob_xgb_tots >= thresh_pr_xgb)
cat(sprintf("Llindar aplicat per pred_xgb: %.4f \n", thresh_pr_xgb))
save(dades_def, file = "2. Dades/9. Dades XGBoost.RData")

sink()
dev.off()
