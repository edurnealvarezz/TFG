packages <- c("dplyr", "ggplot2", "tidyr", "car", "pROC", "PRROC",
              "ResourceSelection", "MASS", "tibble", "caret")

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

lapply(packages, install_if_missing)
rm(packages)

#setwd("C:/Users/edurn/Downloads/TFG")
#setwd("C:/Users/Edurne/Downloads/TFG")

load("2. Dades/4. Dades EFA.RData")

sink("4. Outputs/5.1 Output_text_logit.txt")
pdf("4. Outputs/5.2 Output_grafics_logit.pdf", width = 10, height = 8)

#### ============================================================ ####
####                   0. PREPARACIÓ DE DADES                     ####
#### ============================================================ ####

# Y = 1 si Regular (≥80%), 0 si Irregular (<80%)
dades_mod <- dades_def %>%
  mutate(Y = as.integer(GRUP_ASSIST == "Regular (≥80%)")) %>%
  filter(!is.na(Y))

dades_mod$NOTA_num <- as.numeric(dades_mod$NOTA)
dades_mod$IA_SUBST_num <- as.numeric(dades_mod$IA_SUBST)
dades_mod$IA_ATENC_num <- as.numeric(dades_mod$IA_ATENC)
dades_mod <- dades_mod %>%
  mutate(
    IA_SUBST_dum = factor(IA_SUBST_num, levels = 1:6),
    IA_ATENC_dum = factor(IA_ATENC_num, levels = 1:6)
  )

cat("==== 0. DISTRIBUCIÓ GRUP_ASSIST ======= \n")
print(table(dades_mod$GRUP_ASSIST))
cat(sprintf("\nProporció Regular (≥80%%): %.1f%%\n\n", mean(dades_mod$Y) * 100))

# --- Partició train (80%) / test (20%) ---
set.seed(1234)
idx_train <- createDataPartition(dades_mod$Y, p = 0.80, list = FALSE)
dades_train <- dades_mod[idx_train, ]
dades_test <- dades_mod[-idx_train, ]

cat(sprintf("Partició: Train = %d obs | Test = %d obs\n", nrow(dades_train), nrow(dades_test)))
cat(sprintf("  Train — Regular: %.1f%% | Irregular: %.1f%%\n",
            mean(dades_train$Y) * 100, (1 - mean(dades_train$Y)) * 100))
cat(sprintf("  Test  — Regular: %.1f%% | Irregular: %.1f%%\n\n",
            mean(dades_test$Y) * 100, (1 - mean(dades_test$Y)) * 100))


# a l'arxiu Funcions models hi ha funcions per a calcular mètriques, les carreguem:

source("3. Codi/Funcions models.R")

MIN_RECALL <- 0.40


#### ============================================================ ####
####               1. MODEL COMPLET                               ####
#### ============================================================ ####

vars_fa_mot <- c("MOT_DESMOTIVACIO", "MOT_AUTOGESTIO", "MOT_FORCA_MAJOR")
vars_fa_est <- c("EST_QUALITAT_DOC", "EST_AVALUACIO_AC", "EST_TEMPS_CLASSE", "EST_GRUPS_REDUITS")
vars_fa_ia <- c("IA_EINA_ESTUDI", "IA_SUBSTITUCIO")
vars_cat <- c("T_AVAL", "CURS_1R", "GENERE", "DOBLE_GRAU_EST")
vars_num <- c("EDAT", "DESPL", "NOTA_num")

formula_completa <- as.formula(paste(
  "Y ~",
  paste(c(vars_fa_mot, vars_fa_est, vars_fa_ia, vars_cat, vars_num), collapse = " + ")
))

model_complet <- glm(formula_completa, data = dades_train, family = binomial)

cat("============== 1. MODEL LOGÍSTIC COMPLET =============\n")
cat("Variables incloses:\n")
cat(" · Factors de motius:     ", paste(vars_fa_mot, collapse = ", "), "\n")
cat(" · Factors d'estratègies: ", paste(vars_fa_est, collapse = ", "), "\n")
cat(" · Factors de IA:         ", paste(vars_fa_ia, collapse = ", "), "\n")
cat(" · Variables categòriques:", paste(vars_cat, collapse = ", "), "\n")
cat(" · Variables numèriques:  ", paste(vars_num, collapse = ", "), "\n\n")

print(summary(model_complet))
cat("\nOdds Ratios model complet (IC 95% Wald):\n")
print(round(exp(cbind(OR = coef(model_complet),
                      confint.default(model_complet))), 3))

#### ============================================================ ####
####              2. LINEALITAT EN LOG-ODDS                       ####
#### ============================================================ ####

cat("\n========= 2. LINEALITAT EN LOG-ODDS ========\n\n")

##### --- 2.1 Test de Box-Tidwell --- #####
# per mirar q hi hagi linealitat entre predictores num i log odds
# si és significatiu no és lineal

cat("--- 2.1 Test de Box-Tidwell (EDAT, DESPL, NOTA_num) ---\n\n")

dades_bt <- dades_train %>%
  filter(EDAT > 0, DESPL > 0, NOTA_num > 0) %>%
  mutate(
    EDAT_log = EDAT * log(EDAT),
    DESPL_log = DESPL * log(DESPL),
    NOTA_log = NOTA_num * log(NOTA_num)
  )

formula_bt <- update(formula_completa, . ~ . + EDAT_log + DESPL_log + NOTA_log)
model_bt <- glm(formula_bt, data = dades_bt, family = binomial)
coefs_bt <- coef(summary(model_bt))

df_bt <- data.frame(
  variable = c("EDAT", "DESPL", "NOTA_num"),
  coef_BT = coefs_bt[c("EDAT_log", "DESPL_log", "NOTA_log"), "Estimate"],
  p = coefs_bt[c("EDAT_log", "DESPL_log", "NOTA_log"), "Pr(>|z|)"],
  stringsAsFactors = FALSE
) %>%
  mutate(
    sig = dplyr::case_when(
      p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", TRUE ~ "ns"
    ),
    conclusio = ifelse(p >= 0.05, "lineal en log-odds",
                       "NO lineal → considerar transformacio")
  )
rownames(df_bt) <- NULL
print(df_bt)

##### --- 2.2 Comparació DESPL vs log(DESPL) --- #####
#normalment el temps de desplaçament és millor posar-lo en logaritme
# comparem model normal i amb logaritme.

cat("\n--- 2.2 Comparació AIC: DESPL original vs log(DESPL) ---\n\n")

dades_log_train <- dades_train %>%
  filter(DESPL > 0) %>%
  mutate(log_DESPL = log(DESPL))

formula_logDESPL <- update(formula_completa, . ~ . - DESPL + log_DESPL)
model_logDESPL <- glm(formula_logDESPL, data = dades_log_train, family = binomial)

aic_orig <- AIC(model_complet)
aic_log <- AIC(model_logDESPL)
cat(sprintf("AIC model amb DESPL: %.2f\n", aic_orig))
cat(sprintf("AIC model amb log(DESPL): %.2f\n", aic_log))
cat(sprintf("ΔAIC (orig - log): %.2f\n\n", aic_orig - aic_log))

if (aic_log < aic_orig - 2) {
  cat("→ log(DESPL) millora l'ajust (ΔAIC > 2).\n")
} else {
  cat("→ DESPL no millora amb transformació logarítmica (ΔAIC ≤ 2).\n")
  cat("  DESPL no és rellevant independentment de la forma funcional → s'elimina als models refinats.\n")
}

##### --- 2.3 Gràfics de residus parcials --- #####

cat("\n--- 2.3 Gràfics de components parcials (crPlots) ---\n\n")

par(ask = FALSE)
crPlots(model_complet,
        terms = ~ MOT_DESMOTIVACIO + MOT_AUTOGESTIO + MOT_FORCA_MAJOR +
          IA_EINA_ESTUDI + IA_SUBSTITUCIO + EDAT + DESPL + NOTA_num,
        main = "Component + Residual Plots — Model complet")

#### ============================================================ ####
####              3. MODELS REFINATS I SELECCIÓ                   ####
#### ============================================================ ####

vars_fa_est_ref <- "EST_AVALUACIO_AC"
vars_num_ref <- c("EDAT", "NOTA_num")
vars_cat <- c("T_AVAL", "CURS_1R")

##### --- 3.1 Model 1.1: Likert numèrica (IA_SUBST + IA_ATENC) --- #####

vars_ia_11 <- c("IA_SUBST_num", "IA_ATENC_num")
formula_11 <- as.formula(paste("Y ~", paste(c(vars_fa_mot, vars_fa_est_ref, vars_ia_11,
                                               vars_cat, vars_num_ref), collapse = " + ")))
model_1.1 <- glm(formula_11, data = dades_train, family = binomial)
cat("\n================== 3.1 MODEL 1.1: Likert numèrica (IA_SUBST + IA_ATENC) ================\n")
print(summary(model_1.1))

##### --- 3.2 Model 1.2: Likert numèrica (IA_SUBST) --- #####

vars_ia_12 <- "IA_SUBST_num"
formula_12 <- as.formula(paste("Y ~", paste(c(vars_fa_mot, vars_fa_est_ref, vars_ia_12,
                                               vars_cat, vars_num_ref), collapse = " + ")))
model_1.2 <- glm(formula_12, data = dades_train, family = binomial)
cat("\n================== 3.2 MODEL 1.2: Likert numèrica (IA_SUBST) ================\n")
print(summary(model_1.2))

##### --- 3.3 Model 2.1: Dummies Likert (IA_SUBST + IA_ATENC) --- #####

vars_ia_21 <- c("IA_SUBST_dum", "IA_ATENC_dum")
formula_21 <- as.formula(paste("Y ~", paste(c(vars_fa_mot, vars_fa_est_ref, vars_ia_21,
                                               vars_cat, vars_num_ref), collapse = " + ")))
model_2.1 <- glm(formula_21, data = dades_train, family = binomial)
cat("\n================== 3.3 MODEL 2.1: Dummies Likert (IA_SUBST + IA_ATENC) ================\n")
print(summary(model_2.1))

##### --- 3.4 Model 2.2: Dummies Likert (IA_SUBST) --- #####

vars_ia_22 <- "IA_SUBST_dum"
formula_22 <- as.formula(paste("Y ~", paste(c(vars_fa_mot, vars_fa_est_ref, vars_ia_22,
                                               vars_cat, vars_num_ref), collapse = " + ")))
model_2.2 <- glm(formula_22, data = dades_train, family = binomial)
cat("\n================== 3.4 MODEL 2.2: Dummies Likert (IA_SUBST) ================\n")
print(summary(model_2.2))

##### --- 3.5 Model 3: sense MOT_AUTOGESTIO --- #####

vars_fa_mot_3 <- c("MOT_DESMOTIVACIO", "MOT_FORCA_MAJOR")
formula_3 <- as.formula(paste("Y ~", paste(c(vars_fa_mot_3, vars_fa_est_ref, vars_ia_12,
                                              vars_cat, vars_num_ref), collapse = " + ")))
model_3 <- glm(formula_3, data = dades_train, family = binomial)
cat("\n================== 3.5 MODEL 3: Likert numèrica (sense MOT_AUTOGESTIO) ================\n")
print(summary(model_3))

##### --- 3.6 Taula resum comparativa (AIC/BIC sobre train) --- #####

cat("\n===== 3.6 TAULA-RESUM COMPARATIVA DE MODELS (train) =====\n\n")

llista_models <- list(
  "1.0 Complet" = model_complet,
  "1.1 Likert num (IA_SUBST + IA_ATENC)" = model_1.1,
  "1.2 Likert num (IA_SUBST)" = model_1.2,
  "2.1 Dummies (IA_SUBST + IA_ATENC)" = model_2.1,
  "2.2 Dummies (IA_SUBST)" = model_2.2,
  "3.0 Likert num sense MOT_AUTOGESTIO" = model_3
)

df_resum <- do.call(rbind, lapply(names(llista_models), function(nom) {
  m <- llista_models[[nom]]
  data.frame(
    model = nom,
    n_param = length(coef(m)),
    AIC = round(AIC(m), 2),
    BIC = round(BIC(m), 2),
    stringsAsFactors = FALSE
  )
}))
print(df_resum, row.names = FALSE)
cat("\n")

model_seleccionat <- model_3
formula_sel <- formula_3
cat("→ Model seleccionat: 3 (Likert numèrica, sense MOT_AUTOGESTIO)\n\n")

cat("\nOdds Ratios model seleccionat (IC 95% Wald):\n")
print(round(exp(cbind(OR = coef(model_seleccionat),
                      confint.default(model_seleccionat))), 3))

#### ============================================================ ####
####                          4. FIV                              ####
#### ============================================================ ####

cat("\n=================== 4. FIV ===================\n\n")

vif_res <- vif(model_seleccionat)

if (is.matrix(vif_res)) {
  cat("GVIF (factors categòrics amb > 1 gl):\n")
  print(round(vif_res, 3))
  vif_equiv <- setNames(vif_res[, "GVIF^(1/(2*Df))"]^2, rownames(vif_res))
  cat("\nGVIF equivalent a VIF:\n")
  print(round(vif_equiv, 3))
  flag_vif <- names(vif_equiv[vif_equiv > 5])
} else {
  print(round(vif_res, 3))
  vif_equiv <- vif_res
  flag_vif <- names(vif_res[vif_res > 5])
}

cat("\n⚠ Variables amb VIF > 5:")
if (length(flag_vif) == 0){
   cat(" Cap\n")
    }else 
      cat("\n ", paste(flag_vif, collapse = ", "), "\n")
    

df_vif <- tibble(variable = names(vif_equiv), VIF = as.numeric(vif_equiv))

ggplot(df_vif, aes(x = reorder(variable, VIF), y = VIF, fill = VIF > 5)) +
  geom_col(alpha = 0.85) +
  geom_hline(yintercept = 5, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_hline(yintercept = 10, linetype = "dotted", color = "darkred", linewidth = 0.7) +
  geom_text(aes(label = round(VIF, 2)), hjust = -0.1, size = 3.2) +
  coord_flip() +
  scale_fill_manual(values = c("FALSE" = "#4A90B8", "TRUE" = "#E07B54"), guide = "none") +
  labs(title = "Factor d'Inflació de la Variància (FIV)",
       subtitle = "Línia vermella = 5 | Línia fosca = 10",
       x = "", y = "VIF equivalent") +
  theme_minimal(base_size = 13)

#### ============================================================ ####
####              5. OBSERVACIONS INFLUENTS (sobre train)         ####
#### ============================================================ ####

cat("\n====== 5. OBSERVACIONS INFLUENTS ======\n\n")

n_obs <- nrow(model.frame(model_seleccionat))
k_pred <- length(coef(model_seleccionat)) - 1
row_idx <- as.integer(rownames(model.frame(model_seleccionat)))

cook_d <- cooks.distance(model_seleccionat)
lev_vals <- hatvalues(model_seleccionat)
res_pears_std <- residuals(model_seleccionat, type = "pearson") /
  sqrt(1 - hatvalues(model_seleccionat))

thresh_cook <- 4 / n_obs
thresh_lev <- 2 * (k_pred + 1) / n_obs
thresh_res <- 2.5

cat(sprintf("n train = %d | k predictors = %d\n", n_obs, k_pred))
cat(sprintf("Llindar Cook's D: 4/n = %.4f\n", thresh_cook))
cat(sprintf("Llindar leverage: 2(k+1)/n = %.4f\n", thresh_lev))
cat(sprintf("Llindar |residu Pearson std|: %.1f\n\n", thresh_res))

idx_cook <- which(cook_d > thresh_cook)
idx_lev <- which(lev_vals > thresh_lev)
idx_res <- which(abs(res_pears_std) > thresh_res)

cat(sprintf("Observacions amb Cook's D > %.4f:¡ %d\n", thresh_cook, length(idx_cook)))
cat(sprintf("Observacions amb leverage > %.4f:¡ %d\n", thresh_lev, length(idx_lev)))
cat(sprintf("Observacions amb |residu Pearson| > %.1f: %d\n\n", thresh_res, length(idx_res)))

if (length(idx_res) > 0) {
  cat("Detall observacions amb |residu Pearson std| > 2.5:\n")
  print(data.frame(
    index_orig = row_idx[idx_res],
    res_pearson_std = round(res_pears_std[idx_res], 3),
    cook_D = round(cook_d[idx_res], 4),
    leverage = round(lev_vals[idx_res], 4),
    Y_obs = dades_train$Y[row_idx[idx_res]],
    GRUP = dades_train$GRUP_ASSIST[row_idx[idx_res]]
  ))
  cat("\n")
}

if (length(idx_cook) > 0) {
  cat("Detall observacions amb Cook's D > 4/n:\n")
  print(data.frame(
    index_orig = row_idx[idx_cook],
    cook_D = round(cook_d[idx_cook], 4),
    leverage = round(lev_vals[idx_cook], 4),
    res_pearson_std = round(res_pears_std[idx_cook], 3),
    Y_obs = dades_train$Y[row_idx[idx_cook]],
    GRUP = dades_train$GRUP_ASSIST[row_idx[idx_cook]]
  ))
}

df_infl <- data.frame(
  index = row_idx,
  cook_D = cook_d, leverage = lev_vals, res_pears = res_pears_std,
  flag_cook = cook_d > thresh_cook,
  flag_res = abs(res_pears_std) > thresh_res,
  flag_lev = lev_vals > thresh_lev,
  flag_any = (cook_d > thresh_cook) | (abs(res_pears_std) > thresh_res)
)


# distancies de cook
ggplot(df_infl, aes(x = index, y = cook_D, fill = flag_cook)) +
  geom_col(alpha = 0.8) +
  geom_hline(yintercept = thresh_cook, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_text(data = dplyr::filter(df_infl, flag_cook),
            aes(label = index), vjust = -0.5, size = 3, color = "#E07B54") +
  scale_fill_manual(values = c("FALSE" = "#4A90B8", "TRUE" = "#E07B54"), guide = "none") +
  labs(title = "Distàncies de Cook (train)",
       subtitle = sprintf("Llindar = 4/n = %.4f", thresh_cook),
       x = "Índex observació", y = "Cook's D") +
  theme_minimal(base_size = 13)

#leverage vs residus de pearson
ggplot(df_infl, aes(x = leverage, y = res_pears, size = cook_D, color = flag_any)) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = c(-thresh_res, thresh_res), linetype = "dashed", color = "red") +
  geom_vline(xintercept = thresh_lev, linetype = "dashed", color = "steelblue") +
  geom_text(data = dplyr::filter(df_infl, flag_any),
            aes(label = index), vjust = -0.9, size = 3, show.legend = FALSE) +
  scale_color_manual(values = c("FALSE" = "#4A90B8", "TRUE" = "#E07B54"),
                     labels = c("Normal", "Influent / extrem"), name = "") +
  scale_size_continuous(name = "Cook's D", range = c(1, 6)) +
  labs(title = "Leverage vs Residus de Pearson estandarditzats (train)",
       subtitle = "Mida del punt proporcional a Cook's D",
       x = "Leverage (valors hat)", y = "Residu Pearson estandarditzat") +
  theme_minimal(base_size = 13)

#### ============================================================ ####
####          6. AVALUACIÓ DEL MODEL FINAL                        ####
#### ============================================================ ####

cat("\n========== 6. AVALUACIÓ DEL MODEL FINAL ==========\n\n")

# --- 6.1 Cross-validation 10-fold (AUC) sobre train ---
cat("--- 6.1 10-fold Cross-Validation (AUC) sobre train ---\n\n")

set.seed(1234)
folds <- createFolds(dades_train$Y, k = 10, list = TRUE)

cv_auc <- sapply(folds, function(test_idx) {
  m_cv <- glm(formula_sel, data = dades_train[-test_idx, ], family = binomial)
  prob_cv <- predict(m_cv, newdata = dades_train[test_idx, ], type = "response")
  as.numeric(auc(roc(dades_train$Y[test_idx], prob_cv, quiet = TRUE)))
})

cat(sprintf("AUC per fold: %s\n", paste(round(cv_auc, 4), collapse = " | ")))
cat(sprintf("AUC mitjana (CV): %.4f ± %.4f\n\n", mean(cv_auc), sd(cv_auc)))

# --- 6.2 Pseudo-R² (sobre train) ---
cat("--- 6.2 Pseudo-R² (train) ---\n\n")

model_null <- update(model_seleccionat, . ~ 1)
ll_null <- as.numeric(logLik(model_null))
ll_model <- as.numeric(logLik(model_seleccionat))

r2_mcfadden <- 1 - ll_model / ll_null
r2_cs <- 1 - exp((ll_null - ll_model) * 2 / nrow(dades_train))
r2_nagelkerke <- r2_cs / (1 - exp(ll_null * 2 / nrow(dades_train)))

cat(sprintf("McFadden R²:   %.4f\n", r2_mcfadden))
cat(sprintf("Nagelkerke R² (especial per logit): %.4f\n\n", r2_nagelkerke))

# --- 6.3 Corba Precisió-Recall i selecció de llindar (PRROC, sobre test) ---
prob_test <- predict(model_seleccionat, newdata = dades_test, type = "response")

cat("--- 6.3 Llindar per corba Precisio-Recall (PRROC) ---\n\n")
pr_logit <- seleccionar_llindar_pr(prob_test, dades_test$Y, MIN_RECALL)
thresh_pr_logit <- pr_logit$threshold
cat(sprintf("AUPRC: %.4f\n", pr_logit$auprc))
cat(sprintf("Llindar seleccionat: %.4f | recall_ok (>= %.2f): %s\n\n",
            thresh_pr_logit, MIN_RECALL,
            ifelse(pr_logit$recall_ok, "SI", "NO (fallback Youden)")))

ggplot(pr_logit$pr_curve, aes(x = recall, y = precision)) +
  geom_path(color = "#4A90B8", linewidth = 1) +
  geom_vline(xintercept = MIN_RECALL, linetype = "dashed",
             color = "red", linewidth = 0.8) +
  geom_point(data = data.frame(recall = pr_logit$recall,
                               precision = ifelse(is.na(pr_logit$precision),
                                                  0, pr_logit$precision)),
             color = "#E07B54", size = 3, shape = 17) +
  annotate("text", x = MIN_RECALL + 0.04, y = 0.1,
           label = sprintf("Recall min\n= %.2f", MIN_RECALL),
           color = "red", size = 3.5) +
  labs(title = "Corba Precisio-Recall — Logit (test)",
       subtitle = sprintf("AUPRC = %.4f | Llindar = %.4f",
                          pr_logit$auprc, thresh_pr_logit),
       x = "Recall", y = "Precisio (PPV)") +
  theme_minimal(base_size = 13)

cat("--- 6.3b Metriques sobre conjunt test ---\n")

metriques_logit <- calcular_metriques(
  model_glm = model_seleccionat,
  dades_test_df = dades_test,
  nom_model = "Logit 3",
  auc_cv_mean = mean(cv_auc),
  auc_cv_sd = sd(cv_auc),
  thresh_override = thresh_pr_logit
)

mostrar_metriques(metriques_logit)

cat("--- 6.3c Metriques sobre conjunt train ---\n")

metriques_logit_train <- calcular_metriques(
  model_glm = model_seleccionat,
  dades_test_df = dades_train,
  nom_model = "Logit 3 (train)",
  thresh_override = thresh_pr_logit
)

mostrar_metriques(metriques_logit_train)

# Taula comparativa train vs test
cat("\n--- Resum overfitting: train vs test ---\n\n")
df_ov_logit <- data.frame(
  Conjunt = c("Train", "Test"),
  AUC = c(metriques_logit_train$AUC, metriques_logit$AUC),
  Accuracy = c(metriques_logit_train$accuracy, metriques_logit$accuracy),
  Precision = c(metriques_logit_train$precision, metriques_logit$precision),
  Recall = c(metriques_logit_train$recall, metriques_logit$recall),
  F1 = c(metriques_logit_train$F1, metriques_logit$F1),
  Balanced_Acc = c(metriques_logit_train$balanced_accuracy, metriques_logit$balanced_accuracy)
)
print(df_ov_logit, row.names = FALSE)
cat("\n")

# --- 6.4 Hosmer-Lemeshow (sobre test) ---
cat("\n--- 6.4 Test de Hosmer-Lemeshow (test) ---\n\n")
# per evaluar la bondat d'ajust

hl <- hoslem.test(dades_test$Y, prob_test, g = 10)
cat(sprintf("chi² = %.4f | gl = %d | p = %.4f\n", hl$statistic, hl$parameter, hl$p.value))
cat(ifelse(hl$p.value > 0.05,
           "→ No es rebutja H0: el model s'ajusta bé (p > 0.05)\n",
           "→ Es rebutja H0: problemes d'ajust del model (p < 0.05)\n"))

# --- 6.5 Corba ROC (test) ---
roc_test <- roc(dades_test$Y, prob_test, quiet = TRUE)
roc_df <- data.frame(spec_inv = 1 - roc_test$specificities,
                     sens = roc_test$sensitivities)

ggplot(roc_df, aes(x = spec_inv, y = sens)) +
  geom_path(color = "#4A90B8", linewidth = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  annotate("text", x = 0.62, y = 0.2,
           label = sprintf("AUC test = %.3f\nAUC CV = %.3f ± %.3f",
                           metriques_logit$AUC, metriques_logit$AUC_cv_mean,
                           metriques_logit$AUC_cv_sd),
           size = 4.5, color = "#4A90B8") +
  labs(title = "Corba ROC — Model logístic (test)",
       x = "1 - Especificitat", y = "Sensibilitat") +
  theme_minimal(base_size = 13)

# --- 6.6 Odds Ratios ---
or_final <- exp(cbind(OR = coef(model_seleccionat),
                      confint.default(model_seleccionat)))
df_or <- as.data.frame(or_final) %>%
  rownames_to_column("variable") %>%
  filter(variable != "(Intercept)") %>%
  rename(OR = OR, LB = `2.5 %`, UB = `97.5 %`)

ggplot(df_or, aes(x = reorder(variable, OR), y = OR)) +
  geom_point(size = 3, color = "#4A90B8") +
  geom_errorbar(aes(ymin = LB, ymax = UB), width = 0.25, color = "#4A90B8") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.8) +
  coord_flip() +
  scale_y_log10() +
  labs(title = "Odds Ratios — Model logístic final",
       subtitle = "IC 95% Wald | Escala logarítmica",
       x = "", y = "Odds Ratio") +
  theme_minimal(base_size = 13)

#### ============================================================ ####
####          7. GUARDAR MÈTRIQUES PER A COMPARACIÓ               ####
#### ============================================================ ####

# Format estàndard per comparar tots els models de classificació.
# En altres scripts: metriques_X <- readRDS("2. Dades/metriques_X.rds")
saveRDS(metriques_logit, "2. Dades/metriques_logit.rds")

# --- Guardar model i bbdd encadenada ---
saveRDS(model_seleccionat, "2. Dades/model_logit.rds")

prob_tots_logit <- predict(model_seleccionat, newdata = dades_mod, type = "response")
dades_def$prob_logit <- NA_real_
dades_def$prob_logit[seq_len(nrow(dades_mod))] <- prob_tots_logit
save(dades_def, file = "2. Dades/5. Dades Logit.RData")
cat("\n-> Model guardat a: 2. Dades/model_logit.rds\n")
cat("-> dades_def amb prob_logit guardades a: 2. Dades/5. Dades Logit.RData\n\n")


sink()
dev.off()
