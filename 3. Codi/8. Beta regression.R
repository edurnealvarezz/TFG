packages <- c("dplyr", "ggplot2", "tibble", "tidyr", "betareg", "caret", "lmtest")

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

sink("4. Outputs/8.1 Output_text_beta.txt")
pdf("4. Outputs/8.2 Output_grafics_beta.pdf", width = 10, height = 8)

#### ============================================================ ####
####                   0. PREPARACIÓ DE DADES                     ####
#### ============================================================ ####

# Beta regression requereix resposta estrictament en (0, 1)
# P_ASSIST és [0, 100] → escalar a [0, 1] i aplicar transformació
# Smithson & Verkuilen (2006): y* = (y*(n-1) + 0.5) / n
# Evita valors exactes a 0 i 1 sense eliminar observacions

dades_beta <- dades_def %>%
  mutate(
    NOTA_num = as.numeric(NOTA),
    IA_SUBST_num = as.numeric(IA_SUBST),
    T_AVAL_num = as.integer(T_AVAL == "Continuada"),
    CURS_1R_num = as.integer(CURS_1R)
  ) %>%
  filter(!is.na(P_ASSIST))

n_total <- nrow(dades_beta)
dades_beta <- dades_beta %>%
  mutate(
    P_prop = P_ASSIST / 100,
    P_tr = (P_prop * (n_total - 1) + 0.5) / n_total  # transformació SV
  )

cat("=================================================================\n")
cat("   0. PREPARACIÓ DE DADES\n")
cat("=================================================================\n\n")
cat(sprintf("Observacions: %d\n", n_total))
cat(sprintf("P_ASSIST: min = %.1f%% | max = %.1f%% | mitjana = %.1f%% | sd = %.1f%%\n",
            min(dades_beta$P_ASSIST), max(dades_beta$P_ASSIST),
            mean(dades_beta$P_ASSIST), sd(dades_beta$P_ASSIST)))
cat(sprintf("Valors a 0%%: %d | Valors a 100%%: %d\n",
            sum(dades_beta$P_ASSIST == 0), sum(dades_beta$P_ASSIST == 100)))
cat(sprintf("P_tr (transformada): min = %.4f | max = %.4f\n\n",
            min(dades_beta$P_tr), max(dades_beta$P_tr)))

# Histograma P_ASSIST original
print(
  ggplot(dades_beta, aes(x = P_ASSIST)) +
    geom_histogram(bins = 20, fill = "#4A90B8", alpha = 0.8, color = "white") +
    geom_vline(xintercept = mean(dades_beta$P_ASSIST),
               linetype = "dashed", color = "red", linewidth = 0.8) +
    labs(title = "Distribució de P_ASSIST",
         subtitle = sprintf("Mitjana = %.1f%% | sd = %.1f%%",
                            mean(dades_beta$P_ASSIST), sd(dades_beta$P_ASSIST)),
         x = "% Assistència", y = "Freqüència") +
    theme_minimal(base_size = 13)
)

# Partició train (80%) / test (20%) — set.seed(1234) consistent amb altres models
set.seed(1234)
idx_train <- createDataPartition(dades_beta$P_tr, p = 0.80, list = FALSE)
train_beta <- dades_beta[idx_train, ]
test_beta <- dades_beta[-idx_train, ]

cat(sprintf("Partició: Train = %d obs | Test = %d obs\n", nrow(train_beta), nrow(test_beta)))
cat(sprintf("  Train — P_ASSIST mitjana: %.1f%%\n", mean(train_beta$P_ASSIST)))
cat(sprintf("  Test  — P_ASSIST mitjana: %.1f%%\n\n", mean(test_beta$P_ASSIST)))

# ----------------------------------------------------------------
# Funció de mètriques per a regressió contínua
# ----------------------------------------------------------------
calcular_metriques_beta <- function(obs, pred_prop, nom_model) {
  # obs i pred_prop en [0,1]; convertim a escala original [0,100] per RMSE/MAE
  obs_pct <- obs * 100
  pred_pct <- pred_prop * 100

  rmse <- sqrt(mean((obs_pct - pred_pct)^2))
  mae <- mean(abs(obs_pct - pred_pct))
  r2 <- cor(obs_pct, pred_pct)^2
  bias <- mean(pred_pct - obs_pct)

  list(
    model = nom_model,
    n_test = length(obs),
    RMSE = round(rmse, 4),
    MAE = round(mae, 4),
    R2 = round(r2, 4),
    bias = round(bias, 4)
  )
}

mostrar_metriques_beta <- function(met, titol = NULL) {
  if (is.null(titol)) titol <- met$model
  cat(sprintf("\n--- Mètriques regressió: %s ---\n", titol))
  cat(sprintf("n = %d\n", met$n_test))
  cat(sprintf("RMSE:  %.4f punts percentuals\n", met$RMSE))
  cat(sprintf("MAE:   %.4f punts percentuals\n", met$MAE))
  cat(sprintf("R²:    %.4f\n", met$R2))
  cat(sprintf("Bias:  %.4f (+ = sobreestima, - = subestima)\n\n", met$bias))
}

#### ============================================================ ####
####          1. MODEL 1 — BETA REGRESSION (φ CONSTANT)          ####
#### ============================================================ ####

cat("=================================================================\n")
cat("   1. MODEL 1 — φ CONSTANT (beta regression estàndard)\n")
cat("=================================================================\n\n")

# Predictors: factors EFA (motius, estratègies, IA) + variables personals/acadèmiques
# Mateixos predictors que el logit per fer resultats comparables
formula_mu <- P_tr ~ MOT_DESMOTIVACIO + MOT_FORCA_MAJOR +
  EST_AVALUACIO_AC + IA_SUBST_num +
  T_AVAL + CURS_1R + NOTA_num + EDAT

model_1 <- betareg(formula_mu, data = train_beta, link = "logit")

cat("--- Resum model 1 (φ constant) ---\n\n")
print(summary(model_1))

cat("\nPseudo-R² (McFadden):", round(summary(model_1)$pseudo.r.squared, 4), "\n")
cat("AIC:", round(AIC(model_1), 2), "| BIC:", round(BIC(model_1), 2), "\n\n")

# Coeficients interpretats: exp(coef) = OR en el log-odds de la proporció
cat("Coeficients (escala log-odds de la proporció):\n")
print(round(coef(model_1, model = "mean"), 4))

cat("\nEffectes marginals aproximats (dy/dx a la mitjana):\n")
mu_mean <- mean(fitted(model_1))

cat(sprintf("Factor d'escala: μ(1-μ) = %.4f\n", mu_mean * (1 - mu_mean)))
cat("Multiplicar coef. per aquest factor per obtenir efecte en unitats de P_tr\n\n")

#### ============================================================ ####
####     2. MODEL 2 — BETA REGRESSION (φ VARIABLE / PRECISIÓ)    ####
#### ============================================================ ####

cat("=================================================================\n")
cat("   2. MODEL 2 — φ VARIABLE (dispersió modelada)\n")
cat("=================================================================\n\n")
cat("Hipòtesi: la precisió φ varia segons el tipus d'avaluació i el curs.\n")
cat("  Avaluació continuada → assistència més concentrada (φ alt)\n")
cat("  Primers cursos → assistència més variable (φ baix)\n\n")

# Fórmula betareg: y ~ mu_vars | phi_vars
# La part dreta de | modela log(φ)
formula_phi <- P_tr ~ MOT_DESMOTIVACIO + MOT_FORCA_MAJOR +
  EST_AVALUACIO_AC + IA_SUBST_num +
  T_AVAL + CURS_1R + NOTA_num + EDAT |
  T_AVAL + CURS_1R + NOTA_num

model_2 <- betareg(formula_phi, data = train_beta, link = "logit")

cat("--- Resum model 2 (φ variable) ---\n\n")
print(summary(model_2))

cat("\nPseudo-R² (McFadden):", round(summary(model_2)$pseudo.r.squared, 4), "\n")
cat("AIC:", round(AIC(model_2), 2), "| BIC:", round(BIC(model_2), 2), "\n\n")

cat("Coeficients de φ (log-link):\n")
print(round(coef(model_2, model = "precision"), 4))
cat("exp(coef φ) — factor multiplicatiu sobre la precisió:\n")
print(round(exp(coef(model_2, model = "precision")), 3))

#### ============================================================ ####
####     3. SELECCIÓ DE MODEL (LRT + AIC/BIC)                     ####
#### ============================================================ ####

cat("=================================================================\n")
cat("   3. SELECCIÓ DE MODEL\n")
cat("=================================================================\n\n")

# Taula resum
df_sel <- data.frame(
  Model = c("Model 1 (φ constant)", "Model 2 (φ variable)"),
  n_param_mu = c(length(coef(model_1, "mean")), length(coef(model_2, "mean"))),
  n_param_phi = c(1, length(coef(model_2, "precision"))),
  pseudo_R2 = c(round(summary(model_1)$pseudo.r.squared, 4),
                round(summary(model_2)$pseudo.r.squared, 4)),
  AIC = round(c(AIC(model_1), AIC(model_2)), 2),
  BIC = round(c(BIC(model_1), BIC(model_2)), 2)
)
print(df_sel, row.names = FALSE)

# Test de raó de versemblança (LRT): model 2 vs model 1
cat("\n--- Test LRT: Model 2 vs Model 1 ---\n\n")
lrt <- lrtest(model_1, model_2)
print(lrt)

if (lrt$`Pr(>Chisq)`[2] < 0.05) {
  cat("\n→ El model amb φ variable millora significativament (p < 0.05)\n")
  cat("  → Model seleccionat: Model 2\n\n")
  model_sel <- model_2
  nom_sel <- "Model 2 (φ variable)"
} else {
  cat("\n→ φ variable no millora significativament (p ≥ 0.05)\n")
  cat("  → Model seleccionat: Model 1 (principi de parsimònia)\n\n")
  model_sel <- model_1
  nom_sel <- "Model 1 (φ constant)"
}

#### ============================================================ ####
####          4. DIAGNÒSTICS DEL MODEL SELECCIONAT                ####
#### ============================================================ ####

cat("=================================================================\n")
cat(sprintf("   4. DIAGNÒSTICS — %s\n", nom_sel))
cat("=================================================================\n\n")

res_pearson <- residuals(model_sel, type = "pearson")
res_deviance <- residuals(model_sel, type = "deviance")
fitted_vals <- fitted(model_sel)

cat(sprintf("Residus Pearson: mean = %.4f | sd = %.4f | max|r| = %.4f\n",
            mean(res_pearson), sd(res_pearson), max(abs(res_pearson))))
cat(sprintf("Residus deviance: mean = %.4f | sd = %.4f\n\n",
            mean(res_deviance), sd(res_deviance)))

# Gràfic 4a: Fitted vs Observed
df_diag <- data.frame(
  obs = train_beta$P_tr,
  fitted = fitted_vals,
  res = res_pearson
)

print(
  ggplot(df_diag, aes(x = obs, y = fitted)) +
    geom_point(alpha = 0.5, color = "#4A90B8", size = 1.8) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                color = "red", linewidth = 0.8) +
    geom_smooth(method = "loess", se = TRUE, color = "#E07B54",
                fill = "#F0B27A", linewidth = 1) +
    labs(title = sprintf("Observats vs Ajustats — %s", nom_sel),
         subtitle = "Línia vermella = ajust perfecte | taronja = LOESS",
         x = "P_tr observada", y = "P_tr ajustada") +
    theme_minimal(base_size = 13)
)

# Gràfic 4b: Residus Pearson vs fitted
print(
  ggplot(df_diag, aes(x = fitted, y = res)) +
    geom_point(alpha = 0.5, color = "#4A90B8", size = 1.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_hline(yintercept = c(-2.5, 2.5), linetype = "dotted", color = "grey50") +
    geom_smooth(method = "loess", se = FALSE, color = "#E07B54", linewidth = 0.8) +
    labs(title = "Residus de Pearson vs Ajustats",
         subtitle = "Línies puntejades = ±2.5",
         x = "Valor ajustat", y = "Residu Pearson") +
    theme_minimal(base_size = 13)
)

# Gràfic 4c: Q-Q dels residus de deviance
qq_df <- data.frame(
  theoretical = qnorm(ppoints(length(res_deviance))),
  sample = sort(res_deviance)
)
print(
  ggplot(qq_df, aes(x = theoretical, y = sample)) +
    geom_point(alpha = 0.6, color = "#4A90B8", size = 1.5) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(title = "Q-Q plot — Residus de deviance",
         x = "Quantils teòrics (Normal)", y = "Quantils mostral") +
    theme_minimal(base_size = 13)
)

#### ============================================================ ####
####          5. MÈTRIQUES SOBRE TEST I TRAIN                     ####
#### ============================================================ ####

cat("=================================================================\n")
cat("   5. MÈTRIQUES DE REGRESSIÓ\n")
cat("=================================================================\n\n")

# Test
pred_test <- predict(model_sel, newdata = test_beta, type = "response")
met_test <- calcular_metriques_beta(test_beta$P_tr, pred_test, nom_sel)
mostrar_metriques_beta(met_test)

# Train
pred_train <- fitted(model_sel)
met_train <- calcular_metriques_beta(train_beta$P_tr, pred_train,
                                     paste0(nom_sel, " (train)"))
mostrar_metriques_beta(met_train)

# Taula overfitting
cat("--- Resum overfitting: train vs test ---\n\n")
df_ov <- data.frame(
  Conjunt = c("Train", "Test"),
  RMSE = c(met_train$RMSE, met_test$RMSE),
  MAE = c(met_train$MAE, met_test$MAE),
  R2 = c(met_train$R2, met_test$R2),
  Bias = c(met_train$bias, met_test$bias)
)
print(df_ov, row.names = FALSE)

# Gràfic 5: Observats vs Predits sobre test (escala original %)
df_test_pred <- data.frame(
  obs_pct = test_beta$P_ASSIST,
  pred_pct = pred_test * 100
)

print(
  ggplot(df_test_pred, aes(x = obs_pct, y = pred_pct)) +
    geom_point(alpha = 0.6, color = "#4A90B8", size = 2) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                color = "red", linewidth = 0.8) +
    geom_smooth(method = "loess", se = TRUE, color = "#E07B54",
                fill = "#F0B27A", linewidth = 1) +
    labs(title = sprintf("Observats vs Predits — %s (test)", nom_sel),
         subtitle = sprintf("RMSE = %.2f pp | MAE = %.2f pp | R² = %.4f",
                            met_test$RMSE, met_test$MAE, met_test$R2),
         x = "P_ASSIST observada (%)", y = "P_ASSIST predita (%)") +
    theme_minimal(base_size = 13)
)

#### ============================================================ ####
####          6. GUARDAR MÈTRIQUES PER A COMPARACIÓ               ####
#### ============================================================ ####

metriques_beta <- met_test
saveRDS(metriques_beta, "2. Dades/metriques_beta.rds")
cat("\n→ Mètriques guardades a: 2. Dades/metriques_beta.rds\n\n")

cat("Vista prèvia:\n")
print(as.data.frame(metriques_beta))

sink()
dev.off()
