packages <- c("dplyr", "ggplot2", "tibble", "tidyr",
 "e1071", "caret", "pROC", "PRROC", "fastshap")

install_if_missing <- function(pkg) {
 if (!require(pkg, character.only = TRUE)) {
 install.packages(pkg); library(pkg, character.only = TRUE)
 }
}
lapply(packages, install_if_missing)
rm(packages)

setwd("C:/Users/edurn/Downloads/TFG")
#setwd("C:/Users/Edurne/Downloads/TFG")

load("2. Dades/10. Dades CatBoost.RData")
source("3. Codi/Funcions models.R")

motius_vars <- readRDS("2. Dades/motius_vars.rds")
estrategies_vars <- readRDS("2. Dades/estrategies_vars.rds")
ia_vars <- readRDS("2. Dades/ia_vars.rds")

sink("4. Outputs/11.1 Output_text_svm.txt")
pdf("4. Outputs/11.2 Output_grafics_svm.pdf", width = 10, height = 8)

MIN_RECALL <- 0.60


#### ============================================================ ####
####                    0. PREPARACIÓ DE DADES                    ####
#### ============================================================ ####

cat(" =============== 0. PREPARACIÓ DE DADES ===============\n")

dades_svm <- dades_def %>%
 mutate(
 Y = as.integer(GRUP_ASSIST == "Regular (≥80%)"),
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
             "EST_GRUPS_REDUITS", "IA_EINA_ESTUDI")
vars_acad <- c("NOTA_num", "T_AVAL_num", "CURS_1R_num", "N_ASSIG","DOBLE_GRAU_EST")
vars_pers <- c("EDAT", "DESPL")


predictors <- c(ia_vars, vars_fa, vars_acad, vars_pers)
predictors <- predictors[predictors %in% names(dades_svm)]

dades_svm_net <- dades_svm %>%
 dplyr::select(Y, all_of(predictors)) %>%
 drop_na()

# Partició 80/20 compartida
idx_train <- crear_o_carregar_particio(dades_svm_net$Y)
dades_train_svm <- dades_svm_net[idx_train, ]
dades_test_svm  <- dades_svm_net[-idx_train, ]

Y_train <- dades_train_svm$Y
Y_test  <- dades_test_svm$Y

X_train_raw   <- preparar_matriu_svm(dades_train_svm, predictors)
X_test_raw    <- preparar_matriu_svm(dades_test_svm, predictors)
predictors_sc <- colnames(X_train_raw)

cat(sprintf("Observacions totals: %d | Predictors originals: %d | Columnes codificades: %d\n",
 nrow(dades_svm_net), length(predictors), length(predictors_sc)))
cat(sprintf("Particio: Train = %d | Test = %d\n", length(Y_train), length(Y_test)))
cat(sprintf(" Train — Regular: %.1f%% | Irregular: %.1f%%\n",
 mean(Y_train) * 100, (1 - mean(Y_train)) * 100))
cat(sprintf(" Test  — Regular: %.1f%% | Irregular: %.1f%%\n\n",
 mean(Y_test) * 100, (1 - mean(Y_test)) * 100))

#### ============================================================ ####
####                  1. ESCALAT DE VARIABLES (escala train) ####
#### ============================================================ ####

cat(" ============= 1. ESCALAT DE VARIABLES ============= \n")

cat("IMPORTANT: mu i sd calculats EXCLUSIVAMENT sobre train.\n")
cat("Test s'escala amb els mateixos parametres (evitar leakage).\n\n")

mu_train <- colMeans(X_train_raw)
sd_train <- apply(X_train_raw, 2, sd)
sd_train[sd_train == 0] <- 1

X_train_sc <- scale(X_train_raw, center = mu_train, scale = sd_train)
X_test_sc  <- scale(X_test_raw,  center = mu_train, scale = sd_train)

vars_constants <- sum(apply(X_train_raw, 2, sd) == 0)
cat(sprintf("Variables amb sd=0 (substituida per 1): %d\n", vars_constants))
cat(sprintf("Rang X_train_sc: [%.3f, %.3f]\n\n",
 min(X_train_sc), max(X_train_sc)))

#### ============================================================ ####
####           2. GRID SEARCH MANUAL (5-fold CV, AUC)            ####
#### ============================================================ ####

cat(" =================== 2. GRID SEARCH MANUAL (5-fold CV, AUC) =================== \n")

cost_vals <- c(0.1, 1, 5, 10, 50)
gamma_vals <- c(0.001, 0.01, 0.1, 0.5, 1)
grid_svm  <- expand.grid(cost = cost_vals, gamma = gamma_vals)
grid_svm$AUC_CV_mean <- NA_real_
grid_svm$AUC_CV_sd   <- NA_real_

set.seed(1234)
folds_gs <- caret::createFolds(Y_train, k = 5, list = TRUE)

for (i in seq_len(nrow(grid_svm))) {
  auc_folds <- sapply(folds_gs, function(val_idx) {
    tr_idx <- setdiff(seq_len(length(Y_train)), val_idx)
    X_tr_f <- X_train_sc[tr_idx, , drop = FALSE]
    X_va_f <- X_train_sc[val_idx, , drop = FALSE]
    Y_tr_f <- Y_train[tr_idx]
    Y_va_f <- Y_train[val_idx]
    n_i <- sum(Y_tr_f == 0); n_r <- sum(Y_tr_f == 1)
    w_i <- (n_i + n_r) / (2 * n_i); w_r <- (n_i + n_r) / (2 * n_r)
    m_f <- tryCatch(
      svm(x = X_tr_f, y = factor(Y_tr_f, levels = c(0, 1)),
          kernel = "radial",
          cost   = grid_svm$cost[i],
          gamma  = grid_svm$gamma[i],
          class.weights = c("0" = w_i, "1" = w_r),
          probability = TRUE),
      error = function(e) NULL
    )
    if (is.null(m_f)) return(NA_real_)
    probs_va <- attr(predict(m_f, X_va_f, probability = TRUE), "probabilities")[, "1"]
    if (length(unique(Y_va_f)) < 2) return(NA_real_)
    as.numeric(pROC::auc(pROC::roc(Y_va_f, probs_va, quiet = TRUE)))
  })
  grid_svm$AUC_CV_mean[i] <- round(mean(auc_folds, na.rm = TRUE), 4)
  grid_svm$AUC_CV_sd[i]   <- round(sd(auc_folds,   na.rm = TRUE), 4)
  cat(sprintf("  cost=%.3f | gamma=%.4f | AUC_CV=%.4f (SD=%.4f)\n",
              grid_svm$cost[i], grid_svm$gamma[i],
              grid_svm$AUC_CV_mean[i], grid_svm$AUC_CV_sd[i]))
}

grid_svm_ord <- grid_svm[order(-grid_svm$AUC_CV_mean), ]
best_cost  <- grid_svm_ord$cost[1]
best_gamma <- grid_svm_ord$gamma[1]
best_auc   <- grid_svm_ord$AUC_CV_mean[1]

cat(sprintf("\nMillors hiperparametres: cost = %.3f | gamma = %.4f\n", best_cost, best_gamma))
cat(sprintf("AUC CV (5-fold, millor combinacio): %.4f\n\n", best_auc))

cat("Grid complet (ordenat per AUC CV):\n")
print(grid_svm_ord, row.names = FALSE)
cat("\n")

# Heatmap del grid search
print(
  ggplot(grid_svm, aes(x = factor(cost), y = factor(gamma), fill = AUC_CV_mean)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(AUC_CV_mean, 3)), size = 3.5, color = "white") +
    scale_fill_gradient(low = "#AED6F1", high = "#1A5276", name = "AUC CV") +
    geom_tile(data = data.frame(cost = factor(best_cost), gamma = factor(best_gamma)),
              aes(x = cost, y = gamma), fill = NA,
              color = "#E07B54", linewidth = 1.5) +
    labs(title = "Heatmap Grid Search SVM-RBF (5-fold CV, AUC)",
         subtitle = sprintf("Millors params: cost=%.1f | gamma=%.3f | AUC_CV=%.4f",
                            best_cost, best_gamma, best_auc),
         x = "Cost (C)", y = "Gamma (γ)") +
    theme_minimal(base_size = 13) +
    theme(axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          legend.text = element_text(size = 12))
)

#### ============================================================ ####
####                    3. MODEL SVM FINAL                        ####
#### ============================================================ ####

cat(" =================== 3. MODEL SVM FINAL AMB PLATT SCALING ===================\n")
cat("=================================================================\n\n")
cat("El SVM base produeix valors de decisio (distancia al hiperplan),\n")
cat("no probabilitats. Platt Scaling ajusta una regressio logistica\n")
cat("sobre aquests valors per obtenir P(Regular|x) calibrades.\n")
cat("e1071::svm amb probability=TRUE fa Platt Scaling intern (5-fold CV).\n\n")

# assignem weights per contrarestar desbalanceig i no classifiqui com a regular
n_irr <- sum(Y_train == 0); n_reg <- sum(Y_train == 1)
w_irr <- (n_irr + n_reg) / (2 * n_irr)
w_reg <- (n_irr + n_reg) / (2 * n_reg)
cat(sprintf("Case weights: Irregular=%.3f | Regular=%.3f\n\n", w_irr, w_reg))

set.seed(1234)
svm_model <- svm(
 x = X_train_sc,
 y = factor(Y_train, levels = c(0, 1)),
 kernel = "radial",
 cost = best_cost,
 gamma = best_gamma,
 class.weights = c("0" = w_irr, "1" = w_reg),
 probability = TRUE
)

cat("Resum del model SVM final:\n")
print(svm_model)
cat(sprintf("\nVectors de suport: %d (%.1f%% del train)\n\n",
 sum(svm_model$nSV), sum(svm_model$nSV) / length(Y_train) * 100))

# SVM classifica entre un costat o l'altre, amb plat scaling
# ajusta una regressió i dona probabilitats

# Probabilitats sobre train i test (Platt Scaling intern de e1071, 5-fold CV)
pred_test_obj  <- predict(svm_model, X_test_sc,  probability = TRUE)
pred_train_obj <- predict(svm_model, X_train_sc, probability = TRUE)

prob_test_svm  <- attr(pred_test_obj,  "probabilities")[, "1"]
prob_train_svm <- attr(pred_train_obj, "probabilities")[, "1"]

auc_test  <- as.numeric(pROC::auc(pROC::roc(Y_test,  prob_test_svm,  quiet = TRUE)))
auc_train <- as.numeric(pROC::auc(pROC::roc(Y_train, prob_train_svm, quiet = TRUE)))
cat(sprintf("AUC train (in-sample): %.4f | AUC test: %.4f\n\n", auc_train, auc_test))

dv_test <- as.numeric(attr(predict(svm_model, X_test_sc, decision.values = TRUE), "decision.values"))

# Grafic: decision value vs probabilitat (Platt curve)
df_platt_plot <- data.frame(
 dv = dv_test,
 prob = prob_test_svm,
 Y = factor(Y_test, labels = c("Irregular", "Regular"))
)
ggplot(df_platt_plot, aes(x = dv, y = prob, color = Y)) +
 geom_point(size = 1.8, alpha = 0.65) +
 geom_smooth(method = "glm", method.args = list(family = "binomial"),
 se = FALSE, color = "grey30", linewidth = 1, linetype = "dashed") +
 scale_color_manual(values = c("Irregular" = "#E07B54", "Regular" = "#4A90B8")) +
 labs(title = "Platt Scaling — SVM-RBF (test)",
 subtitle = "Eix x = valor de decisio SVM | Eix y = P(Regular) Platt",
 x = "Valor de decisio (distancia al hiperplan)",
 y = "P(Regular ≥ 80%)", color = NULL) +
 theme_minimal(base_size = 13) +
 theme(legend.position = "top",
 axis.text.y = element_text(size = 12),
 axis.text.x = element_text(size = 12),
 legend.text = element_text(size = 12))

#### ============================================================ ####
####                        4. LLINDAR PR                         ####
#### ============================================================ ####

cat(" =============== 4. LLINDAR PR =============== \n")
# Llindar PR sobre probabilitats OOF del CV (5-fold, no contamina el test)
set.seed(1234)
folds_svm_oof <- caret::createFolds(Y_train, k = 5, list = TRUE)
oof_probs_svm <- rep(NA_real_, length(Y_train))

for (fi in seq_along(folds_svm_oof)) {
 vi <- folds_svm_oof[[fi]]
 X_tr_f <- X_train_sc[-vi, , drop = FALSE]
 X_va_f <- X_train_sc[vi,  , drop = FALSE]
 Y_tr_f <- Y_train[-vi]
 n_irr_f <- sum(Y_tr_f == 0); n_reg_f <- sum(Y_tr_f == 1)
 w_irr_f <- (n_irr_f + n_reg_f)/(2*n_irr_f)
 w_reg_f <- (n_irr_f + n_reg_f)/(2*n_reg_f)
 m_f <- svm(x = X_tr_f, y = factor(Y_tr_f, levels = c(0,1)),
            kernel = "radial", cost = best_cost, gamma = best_gamma,
            class.weights = c("0" = w_irr_f, "1" = w_reg_f),
            probability = TRUE)
 oof_probs_svm[vi] <- attr(predict(m_f, X_va_f, probability = TRUE), "probabilities")[, "1"]
}

pr_svm <- seleccionar_llindar_pr(oof_probs_svm, Y_train, MIN_RECALL)
thresh_svm <- pr_svm$threshold

cat(sprintf("AUPRC (OOF CV): %.4f\n", pr_svm$auprc))
cat(sprintf("Llindar seleccionat: %.4f | recall_ok (>= %.2f): %s\n\n",
 thresh_svm, MIN_RECALL,
 ifelse(pr_svm$recall_ok, "SI", "NO (fallback Youden)")))

pr_test_svm <- seleccionar_llindar_pr(prob_test_svm, Y_test, MIN_RECALL)
print(
 ggplot(pr_test_svm$pr_curve, aes(x = recall, y = precision)) +
 geom_path(color = "#4A90B8", linewidth = 1) +
 geom_vline(xintercept = MIN_RECALL, linetype = "dashed",
 color = "red", linewidth = 0.8) +
 geom_point(data = data.frame(recall = pr_test_svm$recall,
 precision = ifelse(is.na(pr_test_svm$precision), 0, pr_test_svm$precision)),
 color = "#E07B54", size = 3, shape = 17) +
 labs(title = "Corba Precisio-Recall — SVM-RBF (test)",
 subtitle = sprintf("AUPRC = %.4f | Llindar = %.4f", pr_test_svm$auprc, thresh_svm),
 x = "Recall (Sensibilitat)", y = "Precisio (PPV)") +
 theme_minimal(base_size = 13) +
 theme(axis.text.y = element_text(size = 12),
 axis.text.x = element_text(size = 12),
 legend.text = element_text(size = 12))
)

#### ============================================================ ####
####                            5. SHAP VALUES                    ####
#### ============================================================ ####

cat("=============== 5. SHAP VALUES =============== \n")

pfun_svm <- function(object, newdata) {
 pred <- predict(object, newdata = as.data.frame(newdata), probability = TRUE)
 attr(pred, "probabilities")[, "1"]
}

set.seed(1234)
shap_svm <- as.data.frame(
 fastshap::explain(
 svm_model,
 X = as.data.frame(X_train_sc),
 pred_wrapper = pfun_svm,
 nsim = 200,
 newdata = as.data.frame(X_test_sc)
 )
)
names(shap_svm) <- predictors_sc

shap_imp_svm <- tibble(
 variable = predictors_sc,
 mean_abs_shap = colMeans(abs(shap_svm))
) %>% arrange(desc(mean_abs_shap))

cat("Top 15 SHAP SVM-RBF:\n")
print(shap_imp_svm %>% slice_head(n = 15))

# Grafic importancia SHAP
ggplot(shap_imp_svm %>% slice_head(n = 15),
 aes(x = reorder(variable, mean_abs_shap), y = mean_abs_shap,
 fill = mean_abs_shap)) +
 geom_col(alpha = 0.9) + coord_flip() +
 scale_fill_gradient(low = "#AED6F1", high = "#1A5276", guide = "none") +
 labs(title = "Importancia SHAP — SVM-RBF",
 subtitle = "Top 15 | mean(|SHAP|) sobre conjunt test",
 x = "", y = "Importancia SHAP") +
 theme_minimal(base_size = 13) +
 theme(axis.text.y = element_text(size = 12),
 axis.text.x = element_text(size = 12),
 legend.text = element_text(size = 12))

# SHAP Beeswarm (top 12)
top12_svm <- shap_imp_svm$variable[1:min(12, nrow(shap_imp_svm))]

shap_long_svm <- shap_svm %>%
 dplyr::select(all_of(top12_svm)) %>%
 mutate(obs = row_number()) %>%
 pivot_longer(-obs, names_to = "variable", values_to = "shap") %>%
 left_join(
 as.data.frame(X_test_sc) %>%
 setNames(predictors) %>%
 dplyr::select(all_of(top12_svm)) %>%
 mutate(obs = row_number()) %>%
 pivot_longer(-obs, names_to = "variable", values_to = "valor"),
 by = c("obs", "variable")
 ) %>%
 mutate(variable = factor(variable, levels = rev(top12_svm)))

ggplot(shap_long_svm, aes(x = shap, y = variable, color = valor)) +
 geom_jitter(height = 0.25, size = 1.2, alpha = 0.6) +
 geom_vline(xintercept = 0, color = "grey40", linewidth = 0.8) +
 scale_color_gradient(low = "#2471A3", high = "#E74C3C", name = "Valor variable") +
 labs(title = "SHAP Beeswarm — SVM-RBF (top 12)",
 subtitle = "x > 0 augmenta P(Regular) | color = valor de la variable",
 x = "Valor SHAP", y = "") +
 theme_minimal(base_size = 12) +
 theme(axis.text.y = element_text(size = 12),
 axis.text.x = element_text(size = 12),
 legend.text = element_text(size = 12))

# SHAP Dependence plots (top 4)
top4_svm <- shap_imp_svm$variable[1:4]

for (v in top4_svm) {
 df_dep <- data.frame(
 valor = as.data.frame(X_test_sc)[, v],
 shap = shap_svm[[v]]
 )
 p <- ggplot(df_dep, aes(x = valor, y = shap)) +
 geom_point(alpha = 0.5, color = "#4A90B8", size = 1.8) +
 geom_smooth(method = "loess", se = TRUE, color = "#E07B54",
 fill = "#F0B27A", linewidth = 1) +
 geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
 labs(title = sprintf("SHAP Dependence Plot SVM-RBF — %s", v),
 subtitle = "Linia taronja = tendencia LOESS | y > 0 augmenta P(Regular)",
 x = v, y = "Valor SHAP") +
 theme_minimal(base_size = 13) +
 theme(axis.text.y = element_text(size = 12),
 axis.text.x = element_text(size = 12),
 legend.text = element_text(size = 12))
 print(p)
}

#### ============================================================ ####
####                    6. MÈTRIQUES DE CLASSIFICACIÓ             ####
#### ============================================================ ####

cat("=============== 6. MÈTRIQUES DE CLASSIFICACIÓ =============== \n")

# --- 6a Test ---
cat("--- 6a Metriques sobre conjunt test ---\n")
metriques_svm <- calcular_metriques_svm(
 prob = prob_test_svm,
 Y_vec = Y_test,
 nom_model = "SVM-RBF",
 thresh_override = thresh_svm
)
mostrar_metriques_svm(metriques_svm)

# --- 6b Train (in-sample) ---
cat("\n--- 6b Metriques sobre train (in-sample) ---\n\n")
metriques_svm_train <- calcular_metriques_svm(
 prob = prob_train_svm,
 Y_vec = Y_train,
 nom_model = "SVM-RBF (train in-sample)",
 thresh_override = thresh_svm
)
mostrar_metriques_svm(metriques_svm_train)

# Taula resum overfitting
cat("\n--- Resum overfitting: train vs test ---\n\n")
df_ov_svm <- data.frame(
 Conjunt      = c("Train (in-sample)", "Test"),
 AUC          = c(metriques_svm_train$AUC,          metriques_svm$AUC),
 Accuracy     = c(metriques_svm_train$accuracy,      metriques_svm$accuracy),
 Precision    = c(metriques_svm_train$precision,     metriques_svm$precision),
 Recall       = c(metriques_svm_train$recall,        metriques_svm$recall),
 F1           = c(metriques_svm_train$F1,            metriques_svm$F1),
 Balanced_Acc = c(metriques_svm_train$balanced_accuracy, metriques_svm$balanced_accuracy)
)
print(df_ov_svm, row.names = FALSE)
cat("\n")

# Corba ROC test
roc_svm_test <- pROC::roc(Y_test, prob_test_svm, quiet = TRUE)
roc_df_test <- data.frame(spec_inv = 1 - roc_svm_test$specificities,
 sens = roc_svm_test$sensitivities)
ggplot(roc_df_test, aes(x = spec_inv, y = sens)) +
 geom_path(color = "#4A90B8", linewidth = 1.2) +
 geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
 annotate("text", x = 0.65, y = 0.25,
 label = sprintf("AUC = %.3f", metriques_svm$AUC),
 size = 5, color = "#4A90B8") +
 labs(title = "Corba ROC — SVM-RBF (test)",
 x = "1 - Especificitat", y = "Sensibilitat") +
 theme_minimal(base_size = 13) +
 theme(axis.text.y = element_text(size = 12),
 axis.text.x = element_text(size = 12),
 legend.text = element_text(size = 12))

#### ============================================================ ####
####                    7. COMPARACIÓ GLOBAL DE MODELS            ####
#### ============================================================ ####
cat("=============== 7. COMPARACIÓ GLOBAL DE MODELS =============== \n")

fitxers <- c(
 Logit = "2. Dades/metriques_logit.rds",
 `Logit Millorat` = "2. Dades/metriques_logit_millorat.rds",
 `RF-A` = "4. Outputs/Metriques i models/metriques_rf_a.rds",
 `RF-B` = "4. Outputs/Metriques i models/metriques_rf_b.rds",
 XGBoost = "4. Outputs/Metriques i models/metriques_xgb.rds",
 CatBoost = "4. Outputs/Metriques i models/metriques_catboost.rds"
)

models_llista <- list()
for (nom in names(fitxers)) {
 if (file.exists(fitxers[[nom]])) {
 models_llista[[nom]] <- readRDS(fitxers[[nom]])
 }
}
models_llista[["SVM-RBF"]] <- metriques_svm

df_comp <- do.call(rbind, lapply(models_llista, extreure_fila))
rownames(df_comp) <- NULL

cat("Taula comparativa de models (sobre conjunt test):\n\n")
print(df_comp, row.names = FALSE)

cat("\nMillor model per metrica:\n")
metriques_num <- c("AUC_test", "Accuracy", "Precision", "Recall", "F1", "Balanced_Acc")
for (m in metriques_num) {
 vals <- suppressWarnings(as.numeric(df_comp[[m]]))
 best_i <- which.max(vals)
 cat(sprintf(" %-14s: %s (%.4f)\n", m, df_comp$Model[best_i], vals[best_i]))
}
cat("\n")

# Grafic comparatiu (barres agrupades)
df_comp_long <- df_comp %>%
 dplyr::select(Model, all_of(metriques_num)) %>%
 mutate(across(-Model, as.numeric)) %>%
 pivot_longer(-Model, names_to = "metrica", values_to = "valor") %>%
 mutate(metrica = factor(metrica, levels = metriques_num),
 Model = factor(Model, levels = df_comp$Model),
 es_svm = Model == "SVM-RBF")

colors_models <- c("#4A90B8", "#5DADE2", "#E07B54", "#F0B27A",
 "#8E6BBF", "#BB8FCE", "#E74C3C")[seq_len(nrow(df_comp))]

print(ggplot(df_comp_long, aes(x = metrica, y = valor, fill = Model,
 alpha = es_svm)) +
 geom_col(position = "dodge") +
 geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50") +
 scale_fill_manual(values = colors_models) +
 scale_alpha_manual(values = c("FALSE" = 0.70, "TRUE" = 1.00), guide = "none") +
 scale_y_continuous(limits = c(0, 1)) +
 labs(title = "Comparacio de models",
 x = "", y = "Valor") +
 theme_minimal(base_size = 13) +
 theme(axis.text.x = element_text(angle = 25, hjust = 1, size = 12),
 axis.text.y = element_text(size = 12),
 legend.position = "bottom",
 legend.text = element_text(size = 12)))

# Grafic lollipop: AUC i Balanced Accuracy per model
df_lollipop <- df_comp %>%
 dplyr::select(Model, AUC_test, Balanced_Acc) %>%
 mutate(AUC_test = as.numeric(AUC_test),
 Balanced_Acc = as.numeric(Balanced_Acc),
 Model = factor(Model, levels = rev(df_comp$Model)),
 es_svm = Model == "SVM-RBF") %>%
 pivot_longer(c(AUC_test, Balanced_Acc), names_to = "metrica", values_to = "valor")

ggplot(df_lollipop, aes(x = valor, y = Model, color = metrica, shape = es_svm)) +
 geom_segment(data = df_lollipop %>%
 tidyr::pivot_wider(names_from = metrica, values_from = valor),
 aes(x = AUC_test, xend = Balanced_Acc, y = Model, yend = Model),
 color = "grey70", linewidth = 0.8, inherit.aes = FALSE) +
 geom_point(size = 3.5, alpha = 0.9) +
 scale_color_manual(values = c("AUC_test" = "#4A90B8", "Balanced_Acc" = "#E07B54"),
 labels = c("AUC test", "Balanced Accuracy")) +
 scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 18), guide = "none") +
 geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey50") +
 labs(title = "AUC i Balanced Accuracy — tots els models",
 subtitle = "Rombe = SVM-RBF",
 x = "Valor", y = "", color = NULL) +
 theme_minimal(base_size = 13) +
 theme(legend.position = "top",
 axis.text.y = element_text(size = 12),
 axis.text.x = element_text(size = 12),
 legend.text = element_text(size = 12))

#### ============================================================ ####
####                   8. GUARDAR MÈTRIQUES I BBDD                ####
#### ============================================================ ####

dir.create("4. Outputs/Metriques i models", showWarnings = FALSE, recursive = TRUE)
saveRDS(metriques_svm, "4. Outputs/Metriques i models/metriques_svm.rds")
saveRDS(svm_model,     "4. Outputs/Metriques i models/model_svm.rds")
cat("-> metriques_svm.rds i model_svm.rds guardats\n\n")

# Guardar probabilitats a dades_def (tots els obs de dades_svm)
X_all_svm <- preparar_matriu_svm(dades_svm, predictors)
complete_svm <- complete.cases(X_all_svm)
X_all_svm_sc <- scale(X_all_svm[complete_svm, ],
 center = mu_train, scale = sd_train)

pred_all_obj <- predict(svm_model, X_all_svm_sc, probability = TRUE)
prob_svm_tots <- attr(pred_all_obj, "probabilities")[, "1"]

dades_def$prob_svm <- NA_real_
dades_def$prob_svm[complete_svm] <- prob_svm_tots

dades_def$pred_svm <- NA_integer_
dades_def$pred_svm[complete_svm] <- as.integer(prob_svm_tots >= thresh_svm)

cat(sprintf("Llindar aplicat per pred_svm: %.4f\n\n",
 thresh_svm, MIN_RECALL))

save(dades_def, file = "2. Dades/11. Dades SVM.RData")

sink()
dev.off()
