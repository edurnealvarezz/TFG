packages <- c("dplyr", "ggplot2", "tidyr", "car", "pROC",
              "ResourceSelection", "MASS", "tibble")

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

sink("4. Outputs/5.1 Output_text_logit.txt")
pdf("4. Outputs/5.2 Output_grafics_logit.pdf", width = 10, height = 8)

#### ============================================================ ####
####                   0. PREPARACIÓ DE DADES                     ####
#### ============================================================ ####

# Y = 1 si Regular (≥80%), 0 si Irregular (<80%)
# NOTA_num: factor ordinal → enter 1–5
# CURS_num: factor ordinal → enter 1–6 (no s'usa en model; sí CURS_1R)
dades_mod <- dades_def %>%
  mutate(
    Y = as.integer(GRUP_ASSIST == "Regular (≥80%)"),
    NOTA_num = as.integer(NOTA)
  ) %>%
  filter(!is.na(Y))

cat("=================================================================\n")
cat("   0. VARIABLE DEPENDENT: GRUP_ASSIST\n")
cat("=================================================================\n\n")
cat("Distribució:\n")
print(table(dades_mod$GRUP_ASSIST))
cat(sprintf("\nProporció Regular (≥80%%): %.1f%%\n\n",
            mean(dades_mod$Y) * 100))

#### ============================================================ ####
####               1. MODEL LOGÍSTIC COMPLET                      ####
#### ============================================================ ####

# Predictors seleccionats:
#  · Puntuacions factorials EFA definitiva (Motius, Estratègies, IA)
#  · Variables categòriques significatives vs GRUP_ASSIST (EDA secc. 2)
#  · Variables numèriques per contrast de linealitat (Box-Tidwell secc. 3)
vars_fa_mot <- c("FA_MOT_F1", "FA_MOT_F2", "FA_MOT_F3")
vars_fa_est <- c("FA_EST_F1", "FA_EST_F2", "FA_EST_F3", "FA_EST_F4")
vars_fa_ia <- c("FA_IA_F1", "FA_IA_F2")
vars_cat <- c("T_AVAL", "CURS_1R")
vars_num <- c("NOTA_num", "EDAT", "DESPL")

formula_completa <- as.formula(paste(
  "Y ~",
  paste(c(vars_fa_mot, vars_fa_est, vars_fa_ia,
          vars_cat, vars_num), collapse = " + ")
))

model_complet <- glm(formula_completa, data = dades_mod, family = binomial)

cat("=================================================================\n")
cat("   1. MODEL LOGÍSTIC COMPLET\n")
cat("=================================================================\n\n")
cat("Predictors inclosos:\n")
cat(" · Factors de motius:     ", paste(vars_fa_mot, collapse = ", "), "\n")
cat(" · Factors d'estratègies: ", paste(vars_fa_est, collapse = ", "), "\n")
cat(" · Factors de IA:         ", paste(vars_fa_ia, collapse = ", "), "\n")
cat(" · Variables categòriques:", paste(vars_cat, collapse = ", "), "\n")
cat(" · Variables numèriques:  ", paste(vars_num, collapse = ", "), "\n\n")

print(summary(model_complet))

# Odds Ratios amb IC 95% de Wald (perfil likelihood és lent; Wald és correcte per diagnòstic)
cat("\nOdds Ratios (IC 95% Wald):\n")
or_complet <- exp(cbind(OR = coef(model_complet),
                        confint.default(model_complet)))
print(round(or_complet, 3))

#### ============================================================ ####
####                       2. FIV / VIF                           ####
#### ============================================================ ####

cat("\n=================================================================\n")
cat("   2. FACTOR D'INFLACIÓ DE LA VARIANÇA (FIV)\n")
cat("=================================================================\n\n")

vif_res <- vif(model_complet)

# car::vif retorna GVIF per a factors amb > 1 grau de llibertat
if (is.matrix(vif_res)) {
  cat("GVIF (factors categòrics amb > 1 gl):\n")
  print(round(vif_res, 3))
  # GVIF^(1/(2*Df))^2 és l'equivalent al VIF per a 1 df
  vif_equiv <- setNames(vif_res[, "GVIF^(1/(2*Df))"]^2, rownames(vif_res))
  cat("\nGVIF equivalent a VIF (GVIF^(1/(2*Df))²):\n")
  print(round(vif_equiv, 3))
  flag_vif <- names(vif_equiv[vif_equiv > 5])
} else {
  print(round(vif_res, 3))
  vif_equiv <- vif_res
  flag_vif <- names(vif_res[vif_res > 5])
}

cat("\n⚠ Variables amb VIF > 5:")
if (length(flag_vif) == 0) cat(" Cap\n")
else cat("\n ", paste(flag_vif, collapse = ", "), "\n")

# Gràfic FIV
df_vif <- tibble(variable = names(vif_equiv),
                 VIF = as.numeric(vif_equiv))

ggplot(df_vif, aes(x = reorder(variable, VIF), y = VIF,
                   fill = VIF > 5)) +
  geom_col(alpha = 0.85) +
  geom_hline(yintercept = 5, linetype = "dashed",
             color = "red", linewidth = 0.8) +
  geom_hline(yintercept = 10, linetype = "dotted",
             color = "darkred", linewidth = 0.7) +
  geom_text(aes(label = round(VIF, 2)), hjust = -0.1, size = 3.2) +
  coord_flip() +
  scale_fill_manual(values = c("FALSE" = "#4A90B8", "TRUE" = "#E07B54"),
                    guide = "none") +
  labs(title = "Factor d'Inflació de la Variança (FIV)",
       subtitle = "Línia vermella = 5 | Línia fosca = 10",
       x = "", y = "VIF equivalent") +
  theme_minimal(base_size = 13)

#### ============================================================ ####
####              3. LINEALITAT EN LOG-ODDS                       ####
#### ============================================================ ####

cat("\n=================================================================\n")
cat("   3. LINEALITAT EN LOG-ODDS\n")
cat("=================================================================\n\n")

##### --- 3.1 Test de Box-Tidwell --- #####

# Afegim termes x·log(x) per a cada predictor continu positiu.
# Si el coeficient del terme és significatiu (p < 0.05), la variable
# no compleix la linealitat en log-odds → caldria transformació.
cat("--- 3.1 Test de Box-Tidwell ---\n")
cat("Variables testades: EDAT, DESPL, NOTA_num (totes > 0)\n\n")

dades_bt <- dades_mod %>%
  filter(EDAT > 0, DESPL > 0, NOTA_num > 0) %>%
  mutate(
    EDAT_log = EDAT * log(EDAT),
    DESPL_log = DESPL * log(DESPL),
    NOTA_log = NOTA_num * log(NOTA_num)
  )

formula_bt <- update(formula_completa, . ~ . + EDAT_log + DESPL_log + NOTA_log)
model_bt <- glm(formula_bt, data = dades_bt, family = binomial)
coefs_bt <- coef(summary(model_bt))

bt_terms <- c("EDAT_log", "DESPL_log", "NOTA_log")
vars_bt_orig <- c("EDAT", "DESPL", "NOTA_num")

df_bt <- data.frame(
  variable = vars_bt_orig,
  coef_BT = coefs_bt[bt_terms, "Estimate"],
  p = coefs_bt[bt_terms, "Pr(>|z|)"],
  stringsAsFactors = FALSE
) %>%
  mutate(
    sig = dplyr::case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ "ns"
    ),
    conclusio = ifelse(p >= 0.05,
                       "lineal en log-odds",
                       "NO lineal → considerar transformacio")
  )
rownames(df_bt) <- NULL
print(df_bt)

##### --- 3.2 Gràfics de residus parcials (Component + Residual Plots) --- #####

cat("\n--- 3.2 Gràfics de components parcials (crPlots) ---\n")
cat("Es mostren els predictors continus del model complet.\n\n")

par(ask = FALSE)
crPlots(model_complet,
        terms = ~ FA_MOT_F1 + FA_MOT_F2 + FA_MOT_F3 +
          FA_IA_F1 + FA_IA_F2 + NOTA_num + EDAT + DESPL,
        main = "Component + Residual Plots")

##### --- 3.3 Transformació log(DESPL) --- #####

# DESPL (minuts de desplaçament) sol tenir distribució asimètrica dreta.
# Comparem AIC del model amb DESPL vs log(DESPL).
cat("\n--- 3.3 Transformació log(DESPL) ---\n\n")

dades_log <- dades_mod %>%
  filter(DESPL > 0) %>%
  mutate(log_DESPL = log(DESPL))

formula_logDESPL <- update(formula_completa, . ~ . - DESPL + log_DESPL)
model_logDESPL <- glm(formula_logDESPL, data = dades_log, family = binomial)

aic_orig <- AIC(model_complet)
aic_log <- AIC(model_logDESPL)
cat(sprintf("AIC model amb DESPL:       %.2f\n", aic_orig))
cat(sprintf("AIC model amb log(DESPL):  %.2f\n", aic_log))
cat(sprintf("ΔAIC (orig - log):         %.2f\n\n", aic_orig - aic_log))

usa_log_despl <- aic_log < aic_orig - 2
if (usa_log_despl) {
  cat("→ log(DESPL) millora l'ajust (ΔAIC > 2): s'adopta la transformació.\n\n")
  model_base <- model_logDESPL
  dades_base <- dades_log
} else {
  cat("→ DESPL original és preferible o equivalent (ΔAIC ≤ 2).\n\n")
  model_base <- model_complet
  dades_base <- dades_mod
}

cat("Model base per als diagnòstics i selecció final:",
    ifelse(usa_log_despl, "log(DESPL)", "DESPL original"), "\n")

#### ============================================================ ####
####              4. OBSERVACIONS INFLUENTS                       ####
#### ============================================================ ####

cat("\n=================================================================\n")
cat("   4. OBSERVACIONS INFLUENTS\n")
cat("=================================================================\n\n")

n_obs <- nrow(model.frame(model_base))
k_pred <- length(coef(model_base)) - 1  # sense intercept
row_idx <- as.integer(rownames(model.frame(model_base)))

cook_d <- cooks.distance(model_base)
lev_vals <- hatvalues(model_base)
# Residu de Pearson estandarditzat: r_i / sqrt(1 - h_ii)
res_pears_std <- residuals(model_base, type = "pearson") /
  sqrt(1 - hatvalues(model_base))

thresh_cook <- 4 / n_obs
thresh_lev <- 2 * (k_pred + 1) / n_obs
thresh_res <- 2.5

cat(sprintf("n = %d | k predictors = %d\n", n_obs, k_pred))
cat(sprintf("Llindar Cook's D:             4/n = %.4f\n", thresh_cook))
cat(sprintf("Llindar leverage:         2(k+1)/n = %.4f\n", thresh_lev))
cat(sprintf("Llindar |residu Pearson std|: %.1f\n\n", thresh_res))

idx_cook <- which(cook_d > thresh_cook)
idx_lev <- which(lev_vals > thresh_lev)
idx_res <- which(abs(res_pears_std) > thresh_res)

cat(sprintf("Observacions amb Cook's D > %.4f:       %d\n",
            thresh_cook, length(idx_cook)))
cat(sprintf("Observacions amb leverage > %.4f:       %d\n",
            thresh_lev, length(idx_lev)))
cat(sprintf("Observacions amb |residu Pearson| > %.1f: %d\n\n",
            thresh_res, length(idx_res)))

if (length(idx_res) > 0) {
  cat("Detall observacions amb |residu Pearson std| > 2.5:\n")
  df_extrems <- data.frame(
    index_orig = row_idx[idx_res],
    res_pearson_std = round(res_pears_std[idx_res], 3),
    cook_D = round(cook_d[idx_res], 4),
    leverage = round(lev_vals[idx_res], 4),
    Y_obs = dades_base$Y[idx_res],
    GRUP = dades_base$GRUP_ASSIST[idx_res]
  )
  print(df_extrems)
  cat("\n")
}

if (length(idx_cook) > 0) {
  cat("Detall observacions amb Cook's D > 4/n:\n")
  df_cook_out <- data.frame(
    index_orig = row_idx[idx_cook],
    cook_D = round(cook_d[idx_cook], 4),
    leverage = round(lev_vals[idx_cook], 4),
    res_pearson_std = round(res_pears_std[idx_cook], 3),
    Y_obs = dades_base$Y[idx_cook],
    GRUP = dades_base$GRUP_ASSIST[idx_cook]
  )
  print(df_cook_out)
}

# --- Taula resum per a totes les observacions marcades ---
idx_tot <- sort(unique(c(idx_cook, idx_res)))
if (length(idx_tot) > 0) {
  cat("\nResum observacions marcades per ≥1 criteri:\n")
  df_tot <- data.frame(
    index_orig = row_idx[idx_tot],
    cook_D = round(cook_d[idx_tot], 4),
    leverage = round(lev_vals[idx_tot], 4),
    res_pearson = round(res_pears_std[idx_tot], 3),
    flag_cook = cook_d[idx_tot] > thresh_cook,
    flag_res = abs(res_pears_std[idx_tot]) > thresh_res,
    flag_lev = lev_vals[idx_tot] > thresh_lev
  )
  print(df_tot)
}

# --- Gràfics diagnòstics ---
df_infl <- data.frame(
  index = row_idx,
  cook_D = cook_d,
  leverage = lev_vals,
  res_pears = res_pears_std,
  fitted = fitted(model_base),
  flag_cook = cook_d > thresh_cook,
  flag_res = abs(res_pears_std) > thresh_res,
  flag_lev = lev_vals > thresh_lev,
  flag_any = (cook_d > thresh_cook) | (abs(res_pears_std) > thresh_res)
)

# Gràfic 4a: Cook's D
ggplot(df_infl, aes(x = index, y = cook_D, fill = flag_cook)) +
  geom_col(alpha = 0.8) +
  geom_hline(yintercept = thresh_cook, linetype = "dashed",
             color = "red", linewidth = 0.8) +
  geom_text(data = dplyr::filter(df_infl, flag_cook),
            aes(label = index), vjust = -0.5, size = 3, color = "#E07B54") +
  scale_fill_manual(values = c("FALSE" = "#4A90B8", "TRUE" = "#E07B54"),
                    guide = "none") +
  labs(title = "Distàncies de Cook",
       subtitle = sprintf("Llindar = 4/n = %.4f", thresh_cook),
       x = "Índex observació", y = "Cook's D") +
  theme_minimal(base_size = 13)

# Gràfic 4b: Residus de Pearson estandarditzats
ggplot(df_infl, aes(x = index, y = res_pears, color = flag_res)) +
  geom_point(size = 1.8, alpha = 0.8) +
  geom_hline(yintercept = c(-thresh_res, thresh_res),
             linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_text(data = dplyr::filter(df_infl, flag_res),
            aes(label = index), vjust = -0.8, size = 3) +
  scale_color_manual(values = c("FALSE" = "#4A90B8", "TRUE" = "#E07B54"),
                     labels = c("Normal", "Extrem (|r| > 2.5)"), name = "") +
  labs(title = "Residus de Pearson estandarditzats",
       x = "Índex observació", y = "Residu Pearson estandarditzat") +
  theme_minimal(base_size = 13)

# Gràfic 4c: Leverage vs Residus (bubble plot)
ggplot(df_infl, aes(x = leverage, y = res_pears,
                    size = cook_D, color = flag_any)) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = c(-thresh_res, thresh_res),
             linetype = "dashed", color = "red") +
  geom_vline(xintercept = thresh_lev,
             linetype = "dashed", color = "steelblue") +
  geom_text(data = dplyr::filter(df_infl, flag_any),
            aes(label = index), vjust = -0.9, size = 3,
            show.legend = FALSE) +
  scale_color_manual(values = c("FALSE" = "#4A90B8", "TRUE" = "#E07B54"),
                     labels = c("Normal", "Influent / extrem"), name = "") +
  scale_size_continuous(name = "Cook's D", range = c(1, 6)) +
  labs(title = "Leverage vs Residus de Pearson estandarditzats",
       subtitle = "Mida del punt proporcional a Cook's D",
       x = "Leverage (valors hat)", y = "Residu Pearson estandarditzat") +
  theme_minimal(base_size = 13)

#### ============================================================ ####
####        5. SELECCIÓ DEL MODEL FINAL (backward stepAIC)        ####
#### ============================================================ ####

cat("\n=================================================================\n")
cat("   5. SELECCIÓ DEL MODEL FINAL (backward stepAIC)\n")
cat("=================================================================\n\n")

model_step <- stepAIC(model_base, direction = "backward", trace = 0)

cat("Fórmula del model seleccionat per stepAIC:\n")
print(formula(model_step))
cat("\n")
print(summary(model_step))

cat("\nOdds Ratios model final (IC 95% Wald):\n")
or_final <- exp(cbind(OR = coef(model_step),
                      confint.default(model_step)))
print(round(or_final, 3))

cat("\nFIV model final:\n")
vif_step <- vif(model_step)
if (is.matrix(vif_step)) {
  vif_step_equiv <- setNames(vif_step[, "GVIF^(1/(2*Df))"]^2, rownames(vif_step))
  print(round(vif_step_equiv, 3))
} else {
  print(round(vif_step, 3))
}

#### ============================================================ ####
####               6. AVALUACIÓ DEL MODEL FINAL                   ####
#### ============================================================ ####

cat("\n=================================================================\n")
cat("   6. AVALUACIÓ DEL MODEL FINAL\n")
cat("=================================================================\n\n")

n_fit <- nrow(model.frame(model_step))
Y_fit <- model.frame(model_step)[, 1]

model_null <- update(model_step, . ~ 1)

# Pseudo-R²
ll_null <- as.numeric(logLik(model_null))
ll_model <- as.numeric(logLik(model_step))

r2_mcfadden <- 1 - ll_model / ll_null
r2_cs <- 1 - exp((ll_null - ll_model) * 2 / n_fit)
r2_cs_max <- 1 - exp(ll_null * 2 / n_fit)
r2_nagelkerke <- r2_cs / r2_cs_max

cat(sprintf("McFadden R²:   %.4f\n", r2_mcfadden))
cat(sprintf("Nagelkerke R²: %.4f\n\n", r2_nagelkerke))

# AUC-ROC
prob_pred <- predict(model_step, type = "response")
roc_obj <- roc(Y_fit, prob_pred, quiet = TRUE)
cat(sprintf("AUC-ROC: %.4f\n\n", as.numeric(auc(roc_obj))))

# Llindar òptim (Youden: max sensibilitat + especificitat - 1)
coords_roc <- coords(roc_obj, "best",
                     ret = c("threshold", "sensitivity", "specificity"),
                     best.method = "youden")
thresh_opt <- coords_roc$threshold[1]
cat(sprintf("Llindar òptim (Youden): %.3f\n", thresh_opt))
cat(sprintf("  Sensibilitat: %.3f | Especificitat: %.3f\n\n",
            coords_roc$sensitivity[1], coords_roc$specificity[1]))

# Matriu de confusió
pred_opt <- as.integer(prob_pred >= thresh_opt)
cm <- table(Observat = Y_fit, Predit = pred_opt)
colnames(cm) <- c("Predit Irregular", "Predit Regular")
rownames(cm) <- c("Obs Irregular", "Obs Regular")
cat("Matriu de confusió (llindar òptim):\n")
print(cm)

acc <- sum(diag(cm)) / sum(cm)
sens_cm <- cm["Obs Regular", "Predit Regular"] / sum(cm["Obs Regular", ])
spec_cm <- cm["Obs Irregular", "Predit Irregular"] / sum(cm["Obs Irregular", ])
cat(sprintf("\nExactitud: %.3f | Sensibilitat: %.3f | Especificitat: %.3f\n\n",
            acc, sens_cm, spec_cm))

# Test de Hosmer-Lemeshow
hl <- hoslem.test(Y_fit, prob_pred, g = 10)
cat(sprintf("Test de Hosmer-Lemeshow (g = 10): chi² = %.4f | gl = %d | p = %.4f\n",
            hl$statistic, hl$parameter, hl$p.value))
cat(ifelse(hl$p.value > 0.05,
           "→ No es rebutja H0: el model s'ajusta bé (p > 0.05)\n",
           "→ Es rebutja H0: problemes d'ajust del model (p < 0.05)\n"))

# --- Gràfic 6a: Corba ROC ---
roc_df <- data.frame(
  spec_inv = 1 - roc_obj$specificities,
  sens = roc_obj$sensitivities
)

ggplot(roc_df, aes(x = spec_inv, y = sens)) +
  geom_path(color = "#4A90B8", linewidth = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "grey50") +
  geom_point(aes(x = 1 - coords_roc$specificity[1],
                 y = coords_roc$sensitivity[1]),
             color = "#E07B54", size = 3.5) +
  annotate("text", x = 0.65, y = 0.25,
           label = sprintf("AUC = %.3f", as.numeric(auc(roc_obj))),
           size = 5, color = "#4A90B8") +
  labs(title = "Corba ROC — Model logístic final",
       x = "1 - Especificitat (Taxa de Falsos Positius)",
       y = "Sensibilitat (Taxa de Vertaders Positius)") +
  theme_minimal(base_size = 13)

# --- Gràfic 6b: Odds Ratios ---
df_or <- as.data.frame(or_final) %>%
  rownames_to_column("variable") %>%
  filter(variable != "(Intercept)") %>%
  rename(OR = OR, LB = `2.5 %`, UB = `97.5 %`)

ggplot(df_or, aes(x = reorder(variable, OR), y = OR)) +
  geom_point(size = 3, color = "#4A90B8") +
  geom_errorbar(aes(ymin = LB, ymax = UB),
                width = 0.25, color = "#4A90B8") +
  geom_hline(yintercept = 1, linetype = "dashed",
             color = "red", linewidth = 0.8) +
  coord_flip() +
  scale_y_log10() +
  labs(title = "Odds Ratios — Model logístic final",
       subtitle = "IC 95% Wald | Escala logarítmica | Línia vermella = OR 1",
       x = "", y = "Odds Ratio") +
  theme_minimal(base_size = 13)

sink()
dev.off()
