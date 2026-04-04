packages <- c("dplyr", "ggplot2", "tibble", "tidyr",
              "ranger", "shapr", "SHAPforxgboost",
              "fastshap", "caret", "pROC")

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

X_mat <- as.matrix(dades_rf_net[, predictors])
Y_vec <- dades_rf_net$Y

#### ============================================================ ####
####               1. RANDOM FOREST (ranger)                      ####
#### ============================================================ ####

cat("=================================================================\n")
cat("   1. RANDOM FOREST\n")
cat("=================================================================\n\n")

set.seed(2024)

# Partició train/test (70/30) estratificada per Y
idx_train <- createDataPartition(Y_vec, p = 0.70, list = FALSE)
X_train <- X_mat[idx_train, ]
Y_train <- Y_vec[idx_train]
X_test <- X_mat[-idx_train, ]
Y_test <- Y_vec[-idx_train]

cat(sprintf("Train: %d obs | Test: %d obs\n\n", length(Y_train), length(Y_test)))

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

cat(sprintf("OOB error (train): %.4f\n\n", rf_model$prediction.error))

# Predicció sobre test
prob_test <- predict(rf_model, data = X_test)$predictions[, 2]
pred_test <- as.integer(prob_test >= 0.5)

# Matriu de confusió
cm_rf <- table(Observat = Y_test, Predit = pred_test)
colnames(cm_rf) <- c("Predit Irregular", "Predit Regular")
rownames(cm_rf) <- c("Obs Irregular", "Obs Regular")
cat("Matriu de confusió (llindar 0.5):\n")
print(cm_rf)

acc_rf <- sum(diag(cm_rf)) / sum(cm_rf)
cat(sprintf("\nExactitud (test): %.4f\n", acc_rf))

# AUC-ROC
roc_rf <- roc(Y_test, prob_test, quiet = TRUE)
cat(sprintf("AUC-ROC (test):   %.4f\n\n", as.numeric(auc(roc_rf))))

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

# SHAP sobre el conjunt de test (més ràpid que tot el dataset)
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
  mutate(variable = factor(variable,
                           levels = rev(top15_vars)))

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

# Càrrega AUC del logit si existeix (del script anterior)
# Si no, es reporta només el RF
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

sink()
dev.off()
