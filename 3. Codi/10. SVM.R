# ================================================================
# 10. SVM (Support Vector Machine, kernel RBF) — Predicció GRUP_ASSIST
# ================================================================
# Grid search: cost × gamma (10-fold CV, e1071::tune)
# Platt Scaling per a probabilitats calibrades (probability=TRUE)
# Split: 70/15/15 train/val/test | set.seed(1234)
# MIN_RECALL = 0.40 | Llindar: max precisio (recall >= 0.40) sobre val
# ================================================================

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

load("2. Dades/8. Dades CatBoost.RData")
source("3. Codi/Funcions models.R")

motius_vars      <- readRDS("2. Dades/motius_vars.rds")
estrategies_vars <- readRDS("2. Dades/estrategies_vars.rds")
ia_vars          <- readRDS("2. Dades/ia_vars.rds")

sink("4. Outputs/10.1 Output_text_svm.txt")
pdf("4. Outputs/10.2 Output_grafics_svm.pdf", width = 10, height = 8)

MIN_RECALL <- 0.60

# ----------------------------------------------------------------
# Funcions de mètriques (format consistent amb XGBoost)
# ----------------------------------------------------------------
calcular_metriques_svm <- function(prob, Y_vec, nom_model,
                                   auc_cv_mean = NA, auc_cv_sd = NA,
                                   thresh_override = NULL) {
  roc_obj <- roc(Y_vec, prob, quiet = TRUE)
  auc_val <- as.numeric(auc(roc_obj))

  if (!is.null(thresh_override)) {
    thresh <- thresh_override
  } else {
    coords_r <- coords(roc_obj, "best",
                       ret = c("threshold", "sensitivity", "specificity"),
                       best.method = "youden")
    thresh <- coords_r$threshold[1]
  }

  pred <- as.integer(prob >= thresh)
  TP <- sum(pred == 1 & Y_vec == 1); TN <- sum(pred == 0 & Y_vec == 0)
  FP <- sum(pred == 1 & Y_vec == 0); FN <- sum(pred == 0 & Y_vec == 1)

  accuracy    <- (TP + TN) / (TP + TN + FP + FN)
  precision   <- ifelse(TP + FP > 0, TP / (TP + FP), NA)
  recall      <- ifelse(TP + FN > 0, TP / (TP + FN), NA)
  specificity <- ifelse(TN + FP > 0, TN / (TN + FP), NA)
  f1          <- ifelse(!is.na(precision) & !is.na(recall) & (precision + recall) > 0,
                        2 * precision * recall / (precision + recall), NA)
  balanced_acc <- (recall + specificity) / 2

  list(
    model = nom_model, n_test = length(Y_vec),
    threshold = round(thresh, 3), AUC = round(auc_val, 4),
    AUC_cv_mean = round(auc_cv_mean, 4), AUC_cv_sd = round(auc_cv_sd, 4),
    accuracy = round(accuracy, 4), precision = round(precision, 4),
    recall = round(recall, 4), specificity = round(specificity, 4),
    F1 = round(f1, 4), balanced_accuracy = round(balanced_acc, 4),
    TP = TP, TN = TN, FP = FP, FN = FN
  )
}

mostrar_metriques_svm <- function(met, titol = NULL) {
  if (is.null(titol)) titol <- met$model
  cat(sprintf("\n--- Mètriques: %s ---\n", titol))
  cat(sprintf("n = %d | Llindar PR = %.3f\n", met$n_test, met$threshold))
  if (!is.na(met$AUC_cv_mean))
    cat(sprintf("AUC (val set):          %.4f\n", met$AUC_cv_mean))
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
                               Predit   = c("Irregular(0)", "Regular(1)")))
  print(cm)
  df_cm <- data.frame(
    Observat = factor(c("Irregular","Irregular","Regular","Regular"),
                      levels = c("Regular","Irregular")),
    Predit   = factor(c("Irregular","Regular","Irregular","Regular"),
                      levels = c("Irregular","Regular")),
    n    = c(met$TN, met$FP, met$FN, met$TP),
    etiq = c("TN","FP","FN","TP")
  )
  p_cm <- ggplot(df_cm, aes(x = Predit, y = Observat, fill = n)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = paste0(etiq, "\n", n)), size = 5, fontface = "bold") +
    scale_fill_gradient(low = "#EBF5FB", high = "#2471A3", guide = "none") +
    labs(title = sprintf("Matriu de confusió — %s", titol),
         subtitle = sprintf("Llindar PR = %.3f", met$threshold),
         x = "Valor Predit", y = "Valor Observat") +
    theme_minimal(base_size = 13) + theme(panel.grid = element_blank())
  print(p_cm)
}

#### ============================================================ ####
####                   0. PREPARACIÓ DE DADES                     ####
#### ============================================================ ####

cat("=================================================================\n")
cat("   0. PREPARACIÓ DE DADES\n")
cat("=================================================================\n\n")

dades_svm <- dades_def %>%
  mutate(
    Y            = as.integer(GRUP_ASSIST == "Regular (≥80%)"),
    NOTA_num     = as.numeric(NOTA),
    IA_SUBST_num = as.numeric(IA_SUBST),
    IA_ATENC_num = as.numeric(IA_ATENC),
    T_AVAL_num   = as.integer(T_AVAL == "Continuada"),
    CURS_1R_num  = as.integer(CURS_1R)
  ) %>%
  filter(!is.na(Y))

vars_fa   <- c("MOT_DESMOTIVACIO", "MOT_AUTOGESTIO", "MOT_FORCA_MAJOR",
               "EST_QUALITAT_DOC", "EST_AVALUACIO_AC", "EST_TEMPS_CLASSE",
               "EST_GRUPS_REDUITS", "IA_EINA_ESTUDI", "IA_SUBSTITUCIO")
vars_acad <- c("NOTA_num", "T_AVAL_num", "CURS_1R_num", "N_ASSIG")
vars_pers <- c("EDAT", "DESPL")

predictors <- c(motius_vars, estrategies_vars, ia_vars, vars_fa, vars_acad, vars_pers)
predictors <- predictors[predictors %in% names(dades_svm)]

dades_svm_net <- dades_svm %>%
  dplyr::select(Y, all_of(predictors)) %>%
  drop_na()

# Partició 70/15/15 (consistent amb XGB i CatBoost)
set.seed(1234)
idx_train    <- createDataPartition(dades_svm_net$Y, p = 0.70, list = FALSE)
dades_rest   <- dades_svm_net[-idx_train, ]
idx_val      <- createDataPartition(dades_rest$Y, p = 0.50, list = FALSE)

dades_train_svm <- dades_svm_net[idx_train, ]
dades_val_svm   <- dades_rest[idx_val, ]
dades_test_svm  <- dades_rest[-idx_val, ]

Y_train <- dades_train_svm$Y
Y_val   <- dades_val_svm$Y
Y_test  <- dades_test_svm$Y

# ---- Codificació de variables categòriques ----
# Ordered factor (Likert 1-6) -> numeric  |  Binary factor -> 0/1
# Nominal factor (> 2 nivells) -> dummies (model.matrix, tret 1r nivell)
preparar_matriu_svm <- function(df, vars) {
  vars_ok  <- vars[vars %in% names(df)]
  col_list <- lapply(vars_ok, function(v) {
    col <- df[[v]]
    if (is.numeric(col) || is.integer(col)) {
      matrix(as.numeric(col), ncol = 1, dimnames = list(NULL, v))
    } else if (is.ordered(col)) {
      matrix(as.numeric(col), ncol = 1, dimnames = list(NULL, v))
    } else if (is.factor(col)) {
      if (nlevels(col) == 2) {
        matrix(as.integer(col) - 1L, ncol = 1, dimnames = list(NULL, v))
      } else {
        mm <- model.matrix(~ col - 1)[, -1, drop = FALSE]
        colnames(mm) <- paste0(v, "_", levels(col)[-1])
        mm
      }
    } else {
      matrix(as.numeric(col), ncol = 1, dimnames = list(NULL, v))
    }
  })
  mat <- do.call(cbind, col_list)
  storage.mode(mat) <- "numeric"
  mat
}

X_train_raw  <- preparar_matriu_svm(dades_train_svm, predictors)
X_val_raw    <- preparar_matriu_svm(dades_val_svm,   predictors)
X_test_raw   <- preparar_matriu_svm(dades_test_svm,  predictors)
predictors_sc <- colnames(X_train_raw)  # noms definitius (pot incloure dummies)

cat(sprintf("Observacions totals: %d | Predictors originals: %d | Columnes codificades: %d\n",
            nrow(dades_svm_net), length(predictors), length(predictors_sc)))
cat(sprintf("Particio: Train = %d | Val = %d | Test = %d\n",
            length(Y_train), length(Y_val), length(Y_test)))
cat(sprintf("  Train — Regular: %.1f%% | Irregular: %.1f%%\n",
            mean(Y_train) * 100, (1 - mean(Y_train)) * 100))
cat(sprintf("  Val   — Regular: %.1f%% | Irregular: %.1f%%\n",
            mean(Y_val) * 100, (1 - mean(Y_val)) * 100))
cat(sprintf("  Test  — Regular: %.1f%% | Irregular: %.1f%%\n\n",
            mean(Y_test) * 100, (1 - mean(Y_test)) * 100))

#### ============================================================ ####
####           1. ESCALAT DE VARIABLES (escala train)             ####
#### ============================================================ ####

cat("=================================================================\n")
cat("   1. ESCALAT DE VARIABLES\n")
cat("=================================================================\n\n")
cat("IMPORTANT: mu i sd calculats EXCLUSIVAMENT sobre train.\n")
cat("Val i test s'escalen amb els mateixos parametres (evitar leakage).\n\n")

mu_train <- colMeans(X_train_raw)
sd_train <- apply(X_train_raw, 2, sd)
sd_train[sd_train == 0] <- 1  # evitar divisio per zero (variables constants)

X_train_sc <- scale(X_train_raw, center = mu_train, scale = sd_train)
X_val_sc   <- scale(X_val_raw,   center = mu_train, scale = sd_train)
X_test_sc  <- scale(X_test_raw,  center = mu_train, scale = sd_train)

vars_constants <- sum(apply(X_train_raw, 2, sd) == 0)
cat(sprintf("Variables amb sd=0 (substituida per 1): %d\n", vars_constants))
cat(sprintf("Rang X_train_sc: [%.3f, %.3f]\n\n",
            min(X_train_sc), max(X_train_sc)))

#### ============================================================ ####
####       2. GRID SEARCH — cost × gamma (10-fold CV)             ####
#### ============================================================ ####

cat("=================================================================\n")
cat("   2. GRID SEARCH — cost x gamma (10-fold CV, e1071::tune)\n")
cat("=================================================================\n\n")
cat("Mesura d'error: tasa de classificacio incorrecta (10-fold CV).\n")
cat("25 combinacions × 10 folds = 250 models entrenats.\n\n")

set.seed(1234)
tune_svm <- tune(
  svm,
  train.x     = X_train_sc,
  train.y     = factor(Y_train),
  kernel      = "radial",
  ranges      = list(
    cost  = c(0.1, 1, 5, 10, 50),
    gamma = c(0.001, 0.01, 0.1, 0.5, 1)
  ),
  tunecontrol = tune.control(sampling = "cross", cross = 10)
)

best_cost  <- tune_svm$best.parameters$cost
best_gamma <- tune_svm$best.parameters$gamma
best_error <- tune_svm$best.performance

cat(sprintf("Millors hiperparametres: cost = %.3f | gamma = %.4f\n", best_cost, best_gamma))
cat(sprintf("Error CV (tasa classif.): %.4f\n\n", best_error))

cat("Grid complet (ordenat per error CV):\n")
perf_sorted <- tune_svm$performances[order(tune_svm$performances$error), ]
print(perf_sorted, row.names = FALSE)
cat("\n")

# Heatmap del grid search
ggplot(tune_svm$performances,
       aes(x = factor(cost), y = factor(gamma), fill = error)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(error, 3)), size = 3.5, color = "white") +
  scale_fill_gradient(low = "#1A5276", high = "#AED6F1",
                      name = "Error CV") +
  geom_tile(data = data.frame(cost = factor(best_cost), gamma = factor(best_gamma)),
            aes(x = cost, y = gamma), fill = NA,
            color = "#E07B54", linewidth = 1.5) +
  labs(title = "Heatmap Grid Search SVM-RBF (10-fold CV)",
       subtitle = sprintf("Millors params: cost=%.1f | gamma=%.3f | error=%.4f",
                          best_cost, best_gamma, best_error),
       x = "Cost (C)", y = "Gamma (γ)") +
  theme_minimal(base_size = 13)

#### ============================================================ ####
####              3. MODEL SVM FINAL (Platt Scaling)              ####
#### ============================================================ ####

cat("\n=================================================================\n")
cat("   3. MODEL SVM FINAL AMB PLATT SCALING\n")
cat("=================================================================\n\n")
cat("El SVM base produeix valors de decisio (distancia al hiperplan),\n")
cat("no probabilitats. Platt Scaling ajusta una regressio logistica\n")
cat("sobre aquests valors per obtenir P(Regular|x) calibrades.\n")
cat("e1071::svm amb probability=TRUE fa Platt Scaling intern (5-fold CV).\n\n")

# Pesos de classe (desbalanceig lleuger: ~57% reg / ~43% irr)
n_irr <- sum(Y_train == 0); n_reg <- sum(Y_train == 1)
w_irr <- (n_irr + n_reg) / (2 * n_irr)
w_reg <- (n_irr + n_reg) / (2 * n_reg)
cat(sprintf("Case weights: Irregular=%.3f | Regular=%.3f\n\n", w_irr, w_reg))

set.seed(1234)
svm_model <- svm(
  x             = X_train_sc,
  y             = factor(Y_train, levels = c(0, 1)),
  kernel        = "radial",
  cost          = best_cost,
  gamma         = best_gamma,
  class.weights = c("0" = w_irr, "1" = w_reg),
  probability   = TRUE   # activa Platt Scaling intern
)

cat("Resum del model SVM final:\n")
print(svm_model)
cat(sprintf("\nVectors de suport: %d (%.1f%% del train)\n\n",
            sum(svm_model$nSV), sum(svm_model$nSV) / length(Y_train) * 100))

# Obtenir probabilitats (Platt Scaling intern de e1071)
pred_val_obj   <- predict(svm_model, X_val_sc,   probability = TRUE)
pred_test_obj  <- predict(svm_model, X_test_sc,  probability = TRUE)
pred_train_obj <- predict(svm_model, X_train_sc, probability = TRUE)

prob_val_svm   <- attr(pred_val_obj,   "probabilities")[, "1"]
prob_test_svm  <- attr(pred_test_obj,  "probabilities")[, "1"]
prob_train_svm <- attr(pred_train_obj, "probabilities")[, "1"]

# --- Platt Scaling manual (fitejat sobre VAL, no sobre train) ---
cat("--- Platt Scaling manual (fitejat sobre val) ---\n\n")
cat("Diferencia clau respecte a Platt sobre train:\n")
cat("  Train: el SVM JA va veure les observacions -> Platt optimista (sobreajust).\n")
cat("  Val:   el SVM NO va veure val -> estimacio sense biaix del calibrat.\n\n")

dv_train <- as.numeric(attr(predict(svm_model, X_train_sc, decision.values = TRUE), "decision.values"))
dv_val   <- as.numeric(attr(predict(svm_model, X_val_sc,   decision.values = TRUE), "decision.values"))
dv_test  <- as.numeric(attr(predict(svm_model, X_test_sc,  decision.values = TRUE), "decision.values"))

# Ajust del model de Platt exclusivament sobre el conjunt de validació
platt_model     <- glm(Y ~ dv, data = data.frame(dv = dv_val, Y = Y_val), family = binomial)
prob_val_platt  <- predict(platt_model, newdata = data.frame(dv = dv_val),   type = "response")
prob_test_platt <- predict(platt_model, newdata = data.frame(dv = dv_test),  type = "response")
prob_train_platt<- predict(platt_model, newdata = data.frame(dv = dv_train), type = "response")

auc_e1071_val  <- as.numeric(auc(roc(Y_val,  prob_val_svm,    quiet = TRUE)))
auc_platt_val  <- as.numeric(auc(roc(Y_val,  prob_val_platt,  quiet = TRUE)))
auc_e1071_test <- as.numeric(auc(roc(Y_test, prob_test_svm,   quiet = TRUE)))
auc_platt_test <- as.numeric(auc(roc(Y_test, prob_test_platt, quiet = TRUE)))

cat(sprintf("AUC val  — e1071 (intern):  %.4f | Platt manual (val): %.4f\n", auc_e1071_val,  auc_platt_val))
cat(sprintf("AUC test — e1071 (intern):  %.4f | Platt manual (val): %.4f\n\n",auc_e1071_test, auc_platt_test))
cat("-> Usem les probabilitats de e1071 (Platt amb 5-fold CV intern, mes robust).\n\n")

# Grafic: decision value vs probabilitat (Platt curve)
df_platt_plot <- data.frame(
  dv   = dv_test,
  prob = prob_test_svm,
  Y    = factor(Y_test, labels = c("Irregular", "Regular"))
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
  theme_minimal(base_size = 13) + theme(legend.position = "top")

#### ============================================================ ####
####       4. LLINDAR PR (recall >= 0.40) — seleccionat sobre val ####
#### ============================================================ ####

cat("\n=================================================================\n")
cat("   4. LLINDAR PR (seleccionat sobre val, no test)\n")
cat("=================================================================\n\n")

pr_svm     <- seleccionar_llindar_pr(prob_val_svm, Y_val, MIN_RECALL)
thresh_svm <- pr_svm$threshold

cat(sprintf("AUPRC (val): %.4f\n", pr_svm$auprc))
cat(sprintf("Llindar seleccionat: %.4f | recall_ok (>= %.2f): %s\n\n",
            thresh_svm, MIN_RECALL,
            ifelse(pr_svm$recall_ok, "SI", "NO (fallback Youden)")))

ggplot(pr_svm$pr_curve, aes(x = recall, y = precision)) +
  geom_path(color = "#4A90B8", linewidth = 1) +
  geom_vline(xintercept = MIN_RECALL, linetype = "dashed",
             color = "red", linewidth = 0.8) +
  geom_point(data = data.frame(recall    = pr_svm$recall,
                               precision = ifelse(is.na(pr_svm$precision),
                                                  0, pr_svm$precision)),
             color = "#E07B54", size = 3, shape = 17) +
  annotate("text", x = MIN_RECALL + 0.04, y = 0.1,
           label = sprintf("Recall min\n= %.2f", MIN_RECALL),
           color = "red", size = 3.5) +
  labs(title = "Corba Precisio-Recall — SVM-RBF (val, Platt Scaling)",
       subtitle = sprintf("AUPRC = %.4f | Llindar = %.4f", pr_svm$auprc, thresh_svm),
       x = "Recall (Sensibilitat)", y = "Precisio (PPV)") +
  theme_minimal(base_size = 13)

# Corba ROC (val)
roc_svm_val <- roc(Y_val, prob_val_svm, quiet = TRUE)
roc_df_val  <- data.frame(spec_inv = 1 - roc_svm_val$specificities,
                           sens = roc_svm_val$sensitivities)
ggplot(roc_df_val, aes(x = spec_inv, y = sens)) +
  geom_path(color = "#4A90B8", linewidth = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  annotate("text", x = 0.65, y = 0.25,
           label = sprintf("AUC val = %.3f", auc_e1071_val),
           size = 5, color = "#4A90B8") +
  labs(title = "Corba ROC — SVM-RBF (val)",
       x = "1 - Especificitat", y = "Sensibilitat") +
  theme_minimal(base_size = 13)

#### ============================================================ ####
####                    5. SHAP VALUES                            ####
#### ============================================================ ####

cat("\n=================================================================\n")
cat("   5. SHAP VALUES (fastshap, nsim = 200)\n")
cat("=================================================================\n\n")

pfun_svm <- function(object, newdata) {
  pred <- predict(object, newdata = as.data.frame(newdata), probability = TRUE)
  attr(pred, "probabilities")[, "1"]
}

set.seed(1234)
shap_svm <- as.data.frame(
  fastshap::explain(
    svm_model,
    X            = as.data.frame(X_train_sc),
    pred_wrapper = pfun_svm,
    nsim         = 200,
    newdata      = as.data.frame(X_test_sc)
  )
)
names(shap_svm) <- predictors_sc

shap_imp_svm <- tibble(
  variable      = predictors_sc,
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
  theme_minimal(base_size = 13)

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
  theme_minimal(base_size = 12)

# SHAP Dependence plots (top 4)
top4_svm <- shap_imp_svm$variable[1:4]

for (v in top4_svm) {
  df_dep <- data.frame(
    valor = as.data.frame(X_test_sc)[, v],
    shap  = shap_svm[[v]]
  )
  p <- ggplot(df_dep, aes(x = valor, y = shap)) +
    geom_point(alpha = 0.5, color = "#4A90B8", size = 1.8) +
    geom_smooth(method = "loess", se = TRUE, color = "#E07B54",
                fill = "#F0B27A", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    labs(title = sprintf("SHAP Dependence Plot SVM-RBF — %s", v),
         subtitle = "Linia taronja = tendencia LOESS | y > 0 augmenta P(Regular)",
         x = v, y = "Valor SHAP") +
    theme_minimal(base_size = 13)
  print(p)
}

#### ============================================================ ####
####          6. MÈTRIQUES DE CLASSIFICACIÓ                       ####
#### ============================================================ ####

cat("\n=================================================================\n")
cat("   6. MÈTRIQUES DE CLASSIFICACIÓ (llindar PR sobre val)\n")
cat("=================================================================\n\n")

# --- 6a Test ---
cat("--- 6a Metriques sobre conjunt test ---\n")
metriques_svm <- calcular_metriques_svm(
  prob           = prob_test_svm,
  Y_vec          = Y_test,
  nom_model      = "SVM-RBF",
  thresh_override = thresh_svm
)
mostrar_metriques_svm(metriques_svm)

# --- 6b Validació ---
cat("\n--- 6b Metriques sobre conjunt de validacio ---\n")
cat("    [Val: usada per seleccionar el llindar PR]\n\n")
metriques_svm_val <- calcular_metriques_svm(
  prob           = prob_val_svm,
  Y_vec          = Y_val,
  nom_model      = "SVM-RBF (val)",
  thresh_override = thresh_svm
)
mostrar_metriques_svm(metriques_svm_val)

# --- 6c Train (in-sample) ---
cat("\n--- 6c Metriques sobre train (in-sample, OPTIMISTA) ---\n\n")
metriques_svm_train <- calcular_metriques_svm(
  prob           = prob_train_svm,
  Y_vec          = Y_train,
  nom_model      = "SVM-RBF (train in-sample)",
  thresh_override = thresh_svm
)
mostrar_metriques_svm(metriques_svm_train)

# Taula resum overfitting
cat("\n--- Resum overfitting: train vs val vs test ---\n")
cat("  [Val ≈ Test → model generalitza | Train >> Val → overfitting]\n\n")
df_ov_svm <- data.frame(
  Conjunt      = c("Train (in-sample)", "Validacio", "Test"),
  AUC          = c(metriques_svm_train$AUC, metriques_svm_val$AUC, metriques_svm$AUC),
  Accuracy     = c(metriques_svm_train$accuracy,  metriques_svm_val$accuracy,  metriques_svm$accuracy),
  Precision    = c(metriques_svm_train$precision,  metriques_svm_val$precision,  metriques_svm$precision),
  Recall       = c(metriques_svm_train$recall,     metriques_svm_val$recall,     metriques_svm$recall),
  F1           = c(metriques_svm_train$F1,          metriques_svm_val$F1,          metriques_svm$F1),
  Balanced_Acc = c(metriques_svm_train$balanced_accuracy,
                   metriques_svm_val$balanced_accuracy,
                   metriques_svm$balanced_accuracy)
)
print(df_ov_svm, row.names = FALSE)
cat("\n")

# Corba ROC test
roc_svm_test <- roc(Y_test, prob_test_svm, quiet = TRUE)
roc_df_test  <- data.frame(spec_inv = 1 - roc_svm_test$specificities,
                            sens = roc_svm_test$sensitivities)
ggplot(roc_df_test, aes(x = spec_inv, y = sens)) +
  geom_path(color = "#4A90B8", linewidth = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  annotate("text", x = 0.65, y = 0.25,
           label = sprintf("AUC = %.3f", metriques_svm$AUC),
           size = 5, color = "#4A90B8") +
  labs(title = "Corba ROC — SVM-RBF (test)",
       x = "1 - Especificitat", y = "Sensibilitat") +
  theme_minimal(base_size = 13)

#### ============================================================ ####
####            7. COMPARACIÓ GLOBAL DE MODELS                    ####
#### ============================================================ ####

cat("\n=================================================================\n")
cat("   7. COMPARACIÓ GLOBAL DE MODELS\n")
cat("=================================================================\n\n")

extreure_fila <- function(m) {
  cv_info <- tryCatch({
    if (!is.null(m$AUC_cv_mean) && !is.na(m$AUC_cv_mean))
      sprintf("%.4f ± %.4f", m$AUC_cv_mean, m$AUC_cv_sd)
    else if (!is.null(m$OOB_error) && !is.na(m$OOB_error))
      sprintf("OOB err = %.4f", m$OOB_error)
    else "—"
  }, error = function(e) "—")

  data.frame(
    Model        = m$model,
    AUC_CV       = cv_info,
    AUC_test     = round(m$AUC, 4),
    Accuracy     = round(m$accuracy, 4),
    Precision    = round(m$precision, 4),
    Recall       = round(m$recall, 4),
    F1           = round(m$F1, 4),
    Balanced_Acc = round(m$balanced_accuracy, 4),
    stringsAsFactors = FALSE
  )
}

fitxers <- c(
  Logit         = "2. Dades/metriques_logit.rds",
  `Logit Mil.`  = "2. Dades/metriques_logit_millorat.rds",
  `RF-A`        = "2. Dades/metriques_rf_a.rds",
  `RF-B`        = "2. Dades/metriques_rf_b.rds",
  XGBoost       = "2. Dades/metriques_xgb.rds",
  CatBoost      = "2. Dades/metriques_catboost.rds"
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
  cat(sprintf("  %-14s: %s (%.4f)\n", m, df_comp$Model[best_i], vals[best_i]))
}
cat("\n")

# Grafic comparatiu (barres agrupades)
df_comp_long <- df_comp %>%
  dplyr::select(Model, all_of(metriques_num)) %>%
  mutate(across(-Model, as.numeric)) %>%
  pivot_longer(-Model, names_to = "metrica", values_to = "valor") %>%
  mutate(metrica = factor(metrica, levels = metriques_num),
         Model   = factor(Model,   levels = df_comp$Model),
         es_svm  = Model == "SVM-RBF")

colors_models <- c("#4A90B8", "#5DADE2", "#E07B54", "#F0B27A",
                   "#8E6BBF", "#BB8FCE", "#E74C3C")[seq_len(nrow(df_comp))]

ggplot(df_comp_long, aes(x = metrica, y = valor, fill = Model,
                         alpha = es_svm)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50") +
  scale_fill_manual(values = colors_models) +
  scale_alpha_manual(values = c("FALSE" = 0.70, "TRUE" = 1.00), guide = "none") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "Comparacio de models: tots els models vs SVM-RBF",
       subtitle = "Metriques sobre conjunt test | SVM-RBF mes opac",
       x = "", y = "Valor") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1),
        legend.position = "bottom")

# Grafic lollipop: AUC i Balanced Accuracy per model
df_lollipop <- df_comp %>%
  dplyr::select(Model, AUC_test, Balanced_Acc) %>%
  mutate(AUC_test     = as.numeric(AUC_test),
         Balanced_Acc = as.numeric(Balanced_Acc),
         Model        = factor(Model, levels = rev(df_comp$Model)),
         es_svm       = Model == "SVM-RBF") %>%
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
  theme_minimal(base_size = 13) + theme(legend.position = "top")

#### ============================================================ ####
####          8. GUARDAR MÈTRIQUES I BBDD ENCADENADA              ####
#### ============================================================ ####

saveRDS(metriques_svm, "2. Dades/metriques_svm.rds")
cat("-> Metriques guardades a: 2. Dades/metriques_svm.rds\n\n")

# Guardar probabilitats a dades_def (tots els obs de dades_svm)
X_all_svm    <- preparar_matriu_svm(dades_svm, predictors)
complete_svm <- complete.cases(X_all_svm)
X_all_svm_sc <- scale(X_all_svm[complete_svm, ],
                       center = mu_train, scale = sd_train)

pred_all_obj   <- predict(svm_model, X_all_svm_sc, probability = TRUE)
prob_svm_tots  <- attr(pred_all_obj, "probabilities")[, "1"]

dades_def$prob_svm <- NA_real_
dades_def$prob_svm[complete_svm]  <- prob_svm_tots

dades_def$pred_svm <- NA_integer_
dades_def$pred_svm[complete_svm]  <- as.integer(prob_svm_tots >= thresh_svm)

cat(sprintf("Llindar aplicat per pred_svm: %.4f (PR recall>=%.2f, sobre val)\n\n",
            thresh_svm, MIN_RECALL))

save(dades_def, file = "2. Dades/10. Dades SVM.RData")
cat("-> dades_def amb prob_svm guardades a: 2. Dades/10. Dades SVM.RData\n\n")

sink()
dev.off()
