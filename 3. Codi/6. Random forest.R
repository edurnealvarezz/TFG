packages <- c("dplyr", "ggplot2", "tibble", "tidyr",
              "ranger", "fastshap", "caret", "pROC")

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

sink("4. Outputs/6.1 Output_text_rf.txt")
pdf("4. Outputs/6.2 Output_grafics_rf.pdf", width = 10, height = 8)

#### ============================================================ ####
####                   0. PREPARACIÓ DE DADES                     ####
#### ============================================================ ####

# Y = 1 (Regular ≥80%), 0 (Irregular <80%)
# Predictors: totes les variables Likert crues + factors EFA +
#             variables acadèmiques i personals (sense P_ASSIST)

dades_rf <- dades_def %>%
  mutate(
    Y = as.integer(GRUP_ASSIST == "Regular (≥80%)"),
    NOTA_num = as.numeric(NOTA),
    IA_SUBST_num = as.numeric(IA_SUBST),
    IA_ATENC_num = as.numeric(IA_ATENC),
    T_AVAL_num = as.integer(T_AVAL == "Continuada"),
    CURS_1R_num = as.integer(CURS_1R)
  ) %>%
  filter(!is.na(Y))

# Variables predictores (totes numèriques per al RF)
vars_fa <- c("MOT_DESMOTIVACIO", "MOT_AUTOGESTIO", "MOT_FORCA_MAJOR",
             "EST_QUALITAT_DOC", "EST_AVALUACIO_AC", "EST_TEMPS_CLASSE",
             "EST_GRUPS_REDUITS", "IA_EINA_ESTUDI", "IA_SUBSTITUCIO")
vars_acad <- c("NOTA_num", "T_AVAL_num", "CURS_1R_num", "N_ASSIG")
vars_pers <- c("EDAT", "DESPL")

predictors <- c(motius_vars, estrategies_vars, ia_vars,
                vars_fa, vars_acad, vars_pers)

# Eliminar predictors que no existeixin al dataset
predictors <- predictors[predictors %in% names(dades_rf)]

# Dataset net sense NAs en predictors ni en Y
dades_rf_net <- dades_rf %>%
  select(Y, all_of(predictors)) %>%
  drop_na()

cat("=================================================================\n")
cat("   0. PREPARACIÓ DE DADES\n")
cat("=================================================================\n\n")
cat(sprintf("Observacions totals: %d\n", nrow(dades_rf_net)))
cat(sprintf("Predictors inclosos: %d\n", length(predictors)))
cat(sprintf("Distribució Y — Irregular (0): %d | Regular (1): %d\n\n",
            sum(dades_rf_net$Y == 0), sum(dades_rf_net$Y == 1)))

# --- Partició train (80%) / test (20%) estratificada per Y ---
# Mateixa llavor i proporció que el logit per fer comparació justa
set.seed(2024)
idx_train <- createDataPartition(dades_rf_net$Y, p = 0.80, list = FALSE)
dades_train_rf <- dades_rf_net[idx_train, ]
dades_test_rf <- dades_rf_net[-idx_train, ]

Y_train <- dades_train_rf$Y
Y_test <- dades_test_rf$Y
X_train <- as.matrix(dades_train_rf[, predictors])
X_test <- as.matrix(dades_test_rf[, predictors])

cat(sprintf("Partició: Train = %d obs | Test = %d obs\n", length(Y_train), length(Y_test)))
cat(sprintf("  Train — Regular: %.1f%% | Irregular: %.1f%%\n",
            mean(Y_train) * 100, (1 - mean(Y_train)) * 100))
cat(sprintf("  Test  — Regular: %.1f%% | Irregular: %.1f%%\n\n",
            mean(Y_test) * 100, (1 - mean(Y_test)) * 100))

# ----------------------------------------------------------------
# Funcions de mètriques de classificació (adaptades per a ranger)
# Equivalent a calcular_metriques() del logit però pren prob directament
# ----------------------------------------------------------------
calcular_metriques_rf <- function(prob, Y_vec, nom_model, oob_error = NA) {
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
    AUC_cv_mean = NA,
    AUC_cv_sd = NA,
    OOB_error = round(oob_error, 4),
    accuracy = round(accuracy, 4),
    precision = round(precision, 4),
    recall = round(recall, 4),
    specificity = round(specificity, 4),
    F1 = round(f1, 4),
    balanced_accuracy = round(balanced_acc, 4),
    TP = TP, TN = TN, FP = FP, FN = FN
  )
}

mostrar_metriques_rf <- function(met, titol = NULL) {
  if (is.null(titol)) titol <- met$model
  cat(sprintf("\n--- Mètriques: %s ---\n", titol))
  cat(sprintf("n test = %d | Llindar Youden = %.3f\n", met$n_test, met$threshold))
  if (!is.na(met$OOB_error)) {
    cat(sprintf("OOB error (RF train):           %.4f  [anàleg al CV error]\n", met$OOB_error))
  }
  cat(sprintf("AUC (test):             %.4f\n", met$AUC))
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
####               1. RANDOM FOREST (ranger)                      ####
#### ============================================================ ####

cat("=================================================================\n")
cat("   1. RANDOM FOREST\n")
cat("=================================================================\n\n")

set.seed(2024)

# Tuning bàsic: num.trees = 500, mtry = sqrt(p), min.node.size = 10
n_pred <- ncol(X_train)
rf_model <- ranger(
  x = X_train,
  y = factor(Y_train, levels = c(0, 1)),
  num.trees = 500,
  mtry = floor(sqrt(n_pred)),
  min.node.size = 10,
  importance = "permutation",
  probability = TRUE,
  seed = 2024
)

cat(sprintf("num.trees = 500 | mtry = %d | min.node.size = 10\n", floor(sqrt(n_pred))))
cat(sprintf("OOB error (train): %.4f\n\n", rf_model$prediction.error))

# Predicció sobre test
prob_test_rf <- predict(rf_model, data = X_test)$predictions[, 2]
pred_test_rf <- as.integer(prob_test_rf >= 0.5)

# Matriu de confusió (llindar 0.5)
cm_rf <- table(Observat = Y_test, Predit = pred_test_rf)
colnames(cm_rf) <- c("Predit Irregular", "Predit Regular")
rownames(cm_rf) <- c("Obs Irregular", "Obs Regular")
cat("Matriu de confusió (llindar 0.5):\n")
print(cm_rf)

acc_rf <- sum(diag(cm_rf)) / sum(cm_rf)
cat(sprintf("\nExactitud (test, llindar 0.5): %.4f\n", acc_rf))

# AUC-ROC
roc_rf <- roc(Y_test, prob_test_rf, quiet = TRUE)
cat(sprintf("AUC-ROC (test):                %.4f\n\n", as.numeric(auc(roc_rf))))

# Corba ROC
roc_df <- data.frame(
  spec_inv = 1 - roc_rf$specificities,
  sens = roc_rf$sensitivities
)

ggplot(roc_df, aes(x = spec_inv, y = sens)) +
  geom_path(color = "#4A90B8", linewidth = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  annotate("text", x = 0.65, y = 0.25,
           label = sprintf("AUC = %.3f", as.numeric(auc(roc_rf))),
           size = 5, color = "#4A90B8") +
  labs(title = "Corba ROC — Random Forest",
       x = "1 - Especificitat", y = "Sensibilitat") +
  theme_minimal(base_size = 13)

#### ============================================================ ####
####         2. IMPORTÀNCIA DE VARIABLES (permutation)            ####
#### ============================================================ ####

cat("=================================================================\n")
cat("   2. IMPORTÀNCIA DE VARIABLES (permutació)\n")
cat("=================================================================\n\n")

imp_df <- tibble(
  variable = names(rf_model$variable.importance),
  importancia = rf_model$variable.importance
) %>%
  arrange(desc(importancia))

print(imp_df, n = 30)

# Top 20
imp_top <- imp_df %>% slice_head(n = 20)

ggplot(imp_top, aes(x = reorder(variable, importancia), y = importancia,
                    fill = importancia)) +
  geom_col(alpha = 0.9) +
  coord_flip() +
  scale_fill_gradient(low = "#AED6F1", high = "#1A5276", guide = "none") +
  labs(title = "Importància de variables — Random Forest",
       subtitle = "Top 20 | Mesura: disminució d'accuracy per permutació",
       x = "", y = "Importància (permutació)") +
  theme_minimal(base_size = 13)

#### ============================================================ ####
####                    3. SHAP VALUES (fastshap)                 ####
#### ============================================================ ####

cat("=================================================================\n")
cat("   3. SHAP VALUES\n")
cat("=================================================================\n\n")

# Funció de predicció per a fastshap (retorna probabilitat de classe 1)
pfun <- function(object, newdata) {
  predict(object, data = newdata)$predictions[, 2]
}

# SHAP sobre el conjunt de test
# nsim = 100: bon equilibri entre precisió i temps de còmput
set.seed(2024)
shap_vals <- explain(
  object = rf_model,
  X = X_test,
  pred_wrapper = pfun,
  nsim = 100,
  adjust = TRUE
)

shap_df <- as.data.frame(shap_vals)

cat("SHAP calculats per a", nrow(shap_df), "observacions i",
    ncol(shap_df), "variables.\n\n")

# Importància SHAP: mean(|SHAP_i|) per variable
shap_imp <- tibble(
  variable = names(shap_df),
  mean_abs_shap = colMeans(abs(shap_df))
) %>%
  arrange(desc(mean_abs_shap))

cat("Top 20 variables per importància SHAP:\n")
print(shap_imp %>% slice_head(n = 20))

# Gràfic 3a: Importància SHAP (bar chart)
shap_top20 <- shap_imp %>% slice_head(n = 20)

ggplot(shap_top20, aes(x = reorder(variable, mean_abs_shap),
                       y = mean_abs_shap, fill = mean_abs_shap)) +
  geom_col(alpha = 0.9) +
  coord_flip() +
  scale_fill_gradient(low = "#A9DFBF", high = "#1E8449", guide = "none") +
  labs(title = "Importància SHAP — Random Forest",
       subtitle = "Top 20 | mean(|SHAP|) sobre conjunt test",
       x = "", y = "Importància SHAP (mean |SHAP|)") +
  theme_minimal(base_size = 13)

# Gràfic 3b: Beeswarm / dot plot SHAP (top 15)
top15_vars <- shap_imp$variable[1:15]

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
  labs(title = "SHAP Beeswarm — Top 15 variables",
       subtitle = "Color = valor de la variable | x > 0 → augmenta P(Regular)",
       x = "Valor SHAP", y = "") +
  theme_minimal(base_size = 12)

# Gràfic 3c: Dependence plots per a les 4 variables més importants
top4_vars <- shap_imp$variable[1:4]

for (v in top4_vars) {
  df_dep <- data.frame(
    valor = X_test[, v],
    shap = shap_df[[v]]
  )

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
####         4. COMPARACIÓ RF vs LOGIT (resum)                    ####
#### ============================================================ ####

cat("=================================================================\n")
cat("   4. RESUM COMPARATIU RF vs LOGIT\n")
cat("=================================================================\n\n")

cat(sprintf("Random Forest — AUC (test): %.4f | OOB error: %.4f\n",
            as.numeric(auc(roc_rf)), rf_model$prediction.error))
cat("Logit (model 1.2) — AUC: vegeu output 5.1\n\n")

# Top 10 variables SHAP vs importància permutació
df_comp <- shap_imp %>%
  rename(rang_shap = mean_abs_shap) %>%
  slice_head(n = 10) %>%
  left_join(
    imp_df %>% rename(rang_perm = importancia),
    by = "variable"
  )

cat("Top 10 variables: SHAP vs importància per permutació:\n")
print(df_comp)

#### ============================================================ ####
####          5. MÈTRIQUES DE CLASSIFICACIÓ I COMPARACIÓ          ####
#### ============================================================ ####

cat("\n=================================================================\n")
cat("   5. MÈTRIQUES DE CLASSIFICACIÓ (llindar Youden)\n")
cat("=================================================================\n\n")

metriques_rf <- calcular_metriques_rf(
  prob = prob_test_rf,
  Y_vec = Y_test,
  nom_model = "Random Forest",
  oob_error = rf_model$prediction.error
)

mostrar_metriques_rf(metriques_rf)

# Mètriques sobre train usant prediccions OOB (out-of-bag)
# Les prediccions OOB són les de cada arbre sobre les obs que NO ha vist →
# estimació imparcial equivalent al CV; NO s'utilitzen prediccions in-bag
# (que donarien AUC ≈ 1 per definició amb RF)
cat("--- 5b Mètriques sobre train (prediccions OOB) ---\n")
cat("    [OOB = estimació imparcial; comparable directament amb test]\n\n")

prob_train_oob <- rf_model$predictions[, 2]

metriques_rf_train <- calcular_metriques_rf(
  prob = prob_train_oob,
  Y_vec = Y_train,
  nom_model = "Random Forest (OOB train)",
  oob_error = rf_model$prediction.error
)

mostrar_metriques_rf(metriques_rf_train)

# Taula comparativa train (OOB) vs test
cat("\n--- Resum overfitting: OOB train vs test ---\n\n")
df_ov_rf <- data.frame(
  Conjunt = c("Train (OOB)", "Test"),
  AUC = c(metriques_rf_train$AUC, metriques_rf$AUC),
  Accuracy = c(metriques_rf_train$accuracy, metriques_rf$accuracy),
  F1 = c(metriques_rf_train$F1, metriques_rf$F1),
  Balanced_Acc = c(metriques_rf_train$balanced_accuracy, metriques_rf$balanced_accuracy)
)
print(df_ov_rf, row.names = FALSE)
cat("\n")

# Comparació amb logit si existeix el fitxer de mètriques
if (file.exists("2. Dades/metriques_logit.rds")) {
  cat("\n--- Comparació RF vs Logit ---\n\n")
  metriques_logit <- readRDS("2. Dades/metriques_logit.rds")

  df_comp_models <- data.frame(
    Model = c(metriques_logit$model, metriques_rf$model),
    AUC_test = c(metriques_logit$AUC, metriques_rf$AUC),
    Accuracy = c(metriques_logit$accuracy, metriques_rf$accuracy),
    Precision = c(metriques_logit$precision, metriques_rf$precision),
    Recall = c(metriques_logit$recall, metriques_rf$recall),
    F1 = c(metriques_logit$F1, metriques_rf$F1),
    Balanced_Acc = c(metriques_logit$balanced_accuracy, metriques_rf$balanced_accuracy),
    stringsAsFactors = FALSE
  )
  print(df_comp_models, row.names = FALSE)
}

#### ============================================================ ####
####          6. GUARDAR MÈTRIQUES PER A COMPARACIÓ               ####
#### ============================================================ ####

# Format estàndard per comparar tots els models de classificació.
# En altres scripts: metriques_rf <- readRDS("2. Dades/metriques_rf.rds")
saveRDS(metriques_rf, "2. Dades/metriques_rf.rds")
cat("\n→ Mètriques guardades a: 2. Dades/metriques_rf.rds\n\n")

cat("Vista prèvia del format de mètriques:\n")
print(as.data.frame(metriques_rf[c("model", "n_test", "threshold",
                                    "OOB_error", "AUC", "accuracy",
                                    "precision", "recall", "F1",
                                    "balanced_accuracy")]))

sink()
dev.off()
