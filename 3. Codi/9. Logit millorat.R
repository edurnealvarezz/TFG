packages <- c("dplyr", "ggplot2", "tibble", "tidyr", "car", "pROC", "PRROC",
              "ResourceSelection", "caret", "marginaleffects", "data.table")

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

lapply(packages, install_if_missing)
rm(packages)


setwd("C:/Users/edurn/Downloads/TFG")
#setwd("C:/Users/Edurne/Downloads/TFG")

load("2. Dades/5. Dades Logit.RData")
#load("2. Dades/6. Dades RF.RData")
model_seleccionat <- readRDS("2. Dades/model_logit.rds")
source("3. Codi/Funcions models.R")

sink("4. Outputs/9.1 Output_text_logit_millorat.txt")
pdf("4. Outputs/9.2 Output_grafics_logit_millorat.pdf", width = 10, height = 8)

MIN_RECALL <- 0.60

#### ============================================================ ####
####                   0. PREPARACIÓ DE DADES                     ####
#### ============================================================ ####

dades_mod <- dades_def %>%
  mutate(Y = as.integer(GRUP_ASSIST == "Regular (≥80%)")) %>%
  filter(!is.na(Y))

dades_mod$NOTA_num     <- as.numeric(dades_mod$NOTA)
dades_mod$IA_SUBST_num <- as.numeric(dades_mod$IA_SUBST)

set.seed(1234)
idx_train  <- createDataPartition(dades_mod$Y, p = 0.80, list = FALSE)
dades_train <- dades_mod[idx_train, ]
dades_test  <- dades_mod[-idx_train, ]

cat(sprintf("Particio: Train = %d obs | Test = %d obs\n\n",
            nrow(dades_train), nrow(dades_test)))

formula_base <- formula(model_seleccionat)
cat("Formula base (Logit 3):\n")
print(formula_base)
cat("\n")

#### ============================================================ ####
####     1. CONTRAST ORTOGONAL IA_SUBST: LINEAL vs. QUADRATIC    ####
#### ============================================================ ####

cat("\n========== 1. CONTRAST ORTOGONAL IA_SUBST (L vs. Q) ==========\n\n")

formula_lq <- update(formula_base, . ~ . - IA_SUBST_num + poly(IA_SUBST_num, 2))
model_lq   <- glm(formula_lq, data = dades_train, family = binomial)

coef_lq <- coef(summary(model_lq))
p_L <- coef_lq["poly(IA_SUBST_num, 2)1", "Pr(>|z|)"]
p_Q <- coef_lq["poly(IA_SUBST_num, 2)2", "Pr(>|z|)"]

cat(sprintf("Component lineal   (L): z = %.3f | p = %.4f\n",
            coef_lq["poly(IA_SUBST_num, 2)1", "z value"], p_L))
cat(sprintf("Component quadratic (Q): z = %.3f | p = %.4f\n",
            coef_lq["poly(IA_SUBST_num, 2)2", "z value"], p_Q))

lrt_lq <- anova(model_seleccionat, model_lq, test = "LRT")
cat("\nLRT: model_seleccionat vs. model amb poly(IA_SUBST_num, 2):\n")
print(lrt_lq)

# Grafic de probabilitat predicta per IA_SUBST_num
ia_grid <- data.frame(IA_SUBST_num = seq(1, 6, by = 0.1))
other_vars <- setdiff(all.vars(formula_base)[-1], "IA_SUBST_num")
for (v in other_vars) {
  col <- dades_train[[v]]
  ia_grid[[v]] <- if (is.numeric(col)) median(col, na.rm = TRUE) else {
    lvl <- names(sort(table(col), decreasing = TRUE))[1]
    if (is.factor(col)) factor(lvl, levels = levels(col)) else lvl
  }
}

ia_grid$prob_lin <- predict(model_seleccionat, newdata = ia_grid, type = "response")
ia_grid$prob_lq  <- predict(model_lq, newdata = ia_grid, type = "response")

df_ia_plot <- ia_grid %>%
  dplyr::select(IA_SUBST_num, prob_lin, prob_lq) %>%
  pivot_longer(cols = c(prob_lin, prob_lq),
               names_to = "model", values_to = "prob") %>%
  mutate(model = ifelse(model == "prob_lin", "Lineal", "Lineal + Quadratic"))

ggplot(df_ia_plot, aes(x = IA_SUBST_num, y = prob, color = model, linetype = model)) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = c("Lineal" = "#4A90B8", "Lineal + Quadratic" = "#E07B54")) +
  labs(title = "Efecte de IA_SUBST_num sobre P(Regular)",
       subtitle = "Medianes de la resta de predictors",
       x = "IA_SUBST (1=Baix, 6=Alt)", y = "P(Regular >= 80%)",
       color = NULL, linetype = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

cat(sprintf("\nConclucio: component quadratic %s (p = %.4f)\n\n",
            ifelse(p_Q < 0.05, "SIGNIFICATIU -> incorporar al model", "no significatiu"),
            p_Q))

#### ============================================================ ####
####          2. TEST D'INTERACCIONS (LRT)                        ####
#### ============================================================ ####

cat("\n========== 2. TESTS D'INTERACCIONS (LRT) ==========\n\n")

formules_inter <- list(
  "MOT_DESMOTIVACIO x T_AVAL"   = update(formula_base, . ~ . + MOT_DESMOTIVACIO:T_AVAL),
  "IA_SUBST_num x NOTA_num"     = update(formula_base, . ~ . + IA_SUBST_num:NOTA_num),
  "CURS_1R x MOT_DESMOTIVACIO"  = update(formula_base, . ~ . + CURS_1R:MOT_DESMOTIVACIO),
  "MOT_FORCA_MAJOR x EDAT"      = update(formula_base, . ~ . + MOT_FORCA_MAJOR:EDAT),
  "IA_SUBST_num x CURS_1R"      = update(formula_base, . ~ . + IA_SUBST_num:CURS_1R)
)

resultats_inter <- lapply(names(formules_inter), function(nom) {
  form_i <- formules_inter[[nom]]
  model_i <- tryCatch(
    glm(form_i, data = dades_train, family = binomial),
    error = function(e) { cat(sprintf("  ERROR ajustant %s: %s\n", nom, e$message)); NULL }
  )
  if (is.null(model_i)) return(data.frame(Interaccio = nom, Chi2 = NA, gl = NA, p_LRT = NA))
  lrt_i <- anova(model_seleccionat, model_i, test = "LRT")
  data.frame(
    Interaccio = nom,
    Chi2   = round(lrt_i[2, "Deviance"], 4),
    gl     = lrt_i[2, "Df"],
    p_LRT  = round(lrt_i[2, "Pr(>Chi)"], 4)
  )
})

df_inter <- do.call(rbind, resultats_inter)
df_inter$Significativa <- ifelse(!is.na(df_inter$p_LRT) & df_inter$p_LRT < 0.05, "SI", "no")

cat("Taula de tests d'interaccio:\n\n")
print(df_inter, row.names = FALSE)
cat("\n")

# --- 2b. Test anidat per interaccions amb CURS_1R ---
cat("--- 2b. Test anidat: CURS_1R × MOT_DESMOTIVACIO vs. + IA_SUBST_num × CURS_1R ---\n\n")
model_A_curs <- tryCatch(
  glm(update(formula_base, . ~ . + MOT_DESMOTIVACIO:CURS_1R),
      data = dades_train, family = binomial),
  error = function(e) { cat("ERROR model A:", e$message, "\n"); NULL }
)
model_B_curs <- tryCatch(
  glm(update(formula_base, . ~ . + MOT_DESMOTIVACIO:CURS_1R + IA_SUBST_num:CURS_1R),
      data = dades_train, family = binomial),
  error = function(e) { cat("ERROR model B:", e$message, "\n"); NULL }
)
if (!is.null(model_A_curs) && !is.null(model_B_curs)) {
  cat("LRT anidat (base → +DESM:CURS → +IA:CURS):\n")
  print(anova(model_seleccionat, model_A_curs, model_B_curs, test = "LRT"))
  cat(sprintf("BIC base:                             %.2f\n", BIC(model_seleccionat)))
  cat(sprintf("BIC model A (+ DESM:CURS_1R):         %.2f\n", BIC(model_A_curs)))
  cat(sprintf("BIC model B (+ DESM:CURS_1R + IA:CURS): %.2f\n\n", BIC(model_B_curs)))
  cat("Interpretacio: si BIC(A) < BIC(base) i LRT A vs base p<0.05 -> afegir DESM:CURS_1R.\n")
  cat("Si LRT B vs A no es significatiu -> IA_SUBST_num:CURS_1R no aporta valor afegit.\n\n")
}

sig_inter_noms <- df_inter$Interaccio[!is.na(df_inter$p_LRT) & df_inter$p_LRT < 0.05]

if (length(sig_inter_noms) > 0) {
  cat("Interaccions significatives:\n")
  for (nom in sig_inter_noms) {
    cat(sprintf("  -> %s\n", nom))
    form_i  <- formules_inter[[nom]]
    model_i <- glm(form_i, data = dades_train, family = binomial)
    coef_i  <- coef(summary(model_i))
    inter_term <- gsub(" x ", ":", nom)
    inter_rows <- grep(inter_term, rownames(coef_i), fixed = TRUE)
    if (length(inter_rows) > 0) {
      for (rw in rownames(coef_i)[inter_rows]) {
        cat(sprintf("     %s: Coef = %.4f | SE = %.4f | z = %.3f | p = %.4f\n",
                    rw, coef_i[rw, "Estimate"], coef_i[rw, "Std. Error"],
                    coef_i[rw, "z value"], coef_i[rw, "Pr(>|z|)"]))
      }
    } else {
      cat(sprintf("     (terme %s no trobat al model; comprova noms de factors)\n", inter_term))
    }
    cat("\n")
  }
} else {
  cat("Cap interaccio significativa (alfa = 0.05).\n\n")
}

# Grafic especific per IA_SUBST_num x NOTA_num si es significativa
if ("IA_SUBST_num x NOTA_num" %in% sig_inter_noms) {
  model_ia_nota <- glm(formules_inter[["IA_SUBST_num x NOTA_num"]],
                       data = dades_train, family = binomial)
  grid_ia_nota <- expand.grid(
    IA_SUBST_num = c(1, 3, 5),
    NOTA_num = seq(min(dades_train$NOTA_num, na.rm = TRUE),
                   max(dades_train$NOTA_num, na.rm = TRUE), length.out = 50)
  )
  other_v2 <- setdiff(all.vars(formula_base)[-1], c("IA_SUBST_num", "NOTA_num"))
  for (v in other_v2) {
    col <- dades_train[[v]]
    grid_ia_nota[[v]] <- if (is.numeric(col)) median(col, na.rm = TRUE) else {
      lvl <- names(sort(table(col), decreasing = TRUE))[1]
      if (is.factor(col)) factor(lvl, levels = levels(col)) else lvl
    }
  }
  grid_ia_nota$prob <- predict(model_ia_nota, newdata = grid_ia_nota, type = "response")
  grid_ia_nota$IA_label <- factor(paste0("IA_SUBST=", grid_ia_nota$IA_SUBST_num))

  ggplot(grid_ia_nota, aes(x = NOTA_num, y = prob, color = IA_label)) +
    geom_line(linewidth = 1.1) +
    labs(title = "Interaccio IA_SUBST_num x NOTA_num",
         subtitle = "Probabilitat predicta de Regular (>=80%)",
         x = "NOTA_num", y = "P(Regular)", color = "IA_SUBST") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "top")
}

# Grafic interaccio MOT_DESMOTIVACIO x CURS_1R
if ("CURS_1R x MOT_DESMOTIVACIO" %in% sig_inter_noms && !is.null(model_A_curs)) {
  mot_vals <- seq(min(dades_train$MOT_DESMOTIVACIO, na.rm = TRUE),
                  max(dades_train$MOT_DESMOTIVACIO, na.rm = TRUE), length.out = 80)
  curs_vals <- sort(unique(dades_train$CURS_1R))

  grid_desm_curs <- expand.grid(
    MOT_DESMOTIVACIO = mot_vals,
    CURS_1R          = curs_vals
  )
  other_v3 <- setdiff(all.vars(formula_base)[-1], c("MOT_DESMOTIVACIO", "CURS_1R"))
  for (v in other_v3) {
    col <- dades_train[[v]]
    grid_desm_curs[[v]] <- if (is.numeric(col)) median(col, na.rm = TRUE) else {
      lvl <- names(sort(table(col), decreasing = TRUE))[1]
      if (is.factor(col)) factor(lvl, levels = levels(col)) else lvl
    }
  }
  grid_desm_curs$prob <- predict(model_A_curs, newdata = grid_desm_curs, type = "response")
  grid_desm_curs$CURS_label <- factor(
    ifelse(grid_desm_curs$CURS_1R == 1, "1r curs", "Altres cursos")
  )

  ggplot(grid_desm_curs, aes(x = MOT_DESMOTIVACIO, y = prob, color = CURS_label)) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(values = c("1r curs" = "#E07B54", "Altres cursos" = "#4A90B8")) +
    labs(title = "Interaccio MOT_DESMOTIVACIO x CURS_1R",
         subtitle = "Probabilitat predicta de Regular (>=80%) | resta de predictors a la mediana",
         x = "MOT_DESMOTIVACIO (factor de desmotivacio)",
         y = "P(Regular >= 80%)",
         color = NULL) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "top")
}

#### ============================================================ ####
####          3. MODEL MILLORAT                                   ####
#### ============================================================ ####

cat("\n========== 3. CONSTRUCCIO DEL MODEL MILLORAT ==========\n\n")

formula_mil <- formula_base

# Afegir component quadratic si significatiu
if (p_Q < 0.05) {
  formula_mil <- update(formula_mil, . ~ . - IA_SUBST_num + poly(IA_SUBST_num, 2))
  cat("-> Afegit poly(IA_SUBST_num, 2) (Q significatiu)\n")
}

# Afegir interaccions significatives
# Per a les interaccions amb CURS_1R, el test anidat (seccio 2b) mostra que
# model A (+ MOT_DESMOTIVACIO:CURS_1R) es preferible a model B (+ ambdues).
# Per tant, s'exclou IA_SUBST_num:CURS_1R encara que sigui significativa per separat.
inter_excloses <- c("IA_SUBST_num:CURS_1R")

if (length(sig_inter_noms) > 0) {
  for (nom in sig_inter_noms) {
    inter_term <- gsub(" x ", ":", nom)
    if (inter_term %in% inter_excloses) {
      cat(sprintf("-> Exclosa interaccio %s (test anidat: no aporta sobre DESM:CURS_1R)\n",
                  inter_term))
      next
    }
    formula_mil <- update(formula_mil, as.formula(paste(". ~ . +", inter_term)))
    cat(sprintf("-> Afegida interaccio: %s\n", inter_term))
  }
} else {
  cat("-> Cap interaccio significativa afegida.\n")
}

cat("\nFormula model millorat:\n")
print(formula_mil)
cat("\n")

model_millorat <- glm(formula_mil, data = dades_train, family = binomial)
cat("Resum model millorat:\n")
print(summary(model_millorat))

cat("\nOdds Ratios (IC 95% Wald):\n")
print(round(exp(cbind(OR = coef(model_millorat),
                      confint.default(model_millorat))), 3))

cat("\nLRT: model_seleccionat vs. model_millorat:\n")
print(anova(model_seleccionat, model_millorat, test = "LRT"))

#### ============================================================ ####
####     4. REPEATED 5x10-FOLD CV (TOTES LES METRIQUES)          ####
#### ============================================================ ####

cat("\n========== 4. Repeated 5x10-fold CV (totes les metriques) ==========\n\n")

set.seed(1234)
n_rep  <- 5
n_fold <- 10
cv_rows_mil <- vector("list", n_rep * n_fold)
k <- 0

for (r in seq_len(n_rep)) {
  folds_r <- createFolds(dades_train$Y, k = n_fold, list = TRUE)
  for (fold_idx in seq_along(folds_r)) {
    k <- k + 1
    test_idx <- folds_r[[fold_idx]]
    m_cv <- tryCatch(
      glm(formula_mil, data = dades_train[-test_idx, ], family = binomial),
      error = function(e) NULL
    )
    if (is.null(m_cv)) next
    prob_cv <- predict(m_cv, newdata = dades_train[test_idx, ], type = "response")
    Y_cv    <- dades_train$Y[test_idx]

    auc_cv_i <- tryCatch(
      as.numeric(auc(roc(Y_cv, prob_cv, quiet = TRUE))),
      error = function(e) NA_real_
    )
    pr_cv_i <- tryCatch(
      seleccionar_llindar_pr(prob_cv, Y_cv, MIN_RECALL),
      error = function(e) list(threshold = 0.5, auprc = NA_real_)
    )
    pred_cv  <- as.integer(prob_cv >= pr_cv_i$threshold)
    TP_cv <- sum(pred_cv == 1 & Y_cv == 1)
    TN_cv <- sum(pred_cv == 0 & Y_cv == 0)
    FP_cv <- sum(pred_cv == 1 & Y_cv == 0)
    FN_cv <- sum(pred_cv == 0 & Y_cv == 1)
    prec_cv   <- if (TP_cv + FP_cv > 0) TP_cv / (TP_cv + FP_cv) else NA_real_
    rec_cv    <- if (TP_cv + FN_cv > 0) TP_cv / (TP_cv + FN_cv) else NA_real_
    spec_cv   <- if (TN_cv + FP_cv > 0) TN_cv / (TN_cv + FP_cv) else NA_real_
    f1_cv     <- if (!is.na(prec_cv) & !is.na(rec_cv) & (prec_cv + rec_cv) > 0)
                   2 * prec_cv * rec_cv / (prec_cv + rec_cv) else NA_real_
    balacc_cv <- if (!is.na(rec_cv) & !is.na(spec_cv)) (rec_cv + spec_cv) / 2 else NA_real_

    cv_rows_mil[[k]] <- data.frame(
      rep = r, fold = fold_idx,
      AUC = auc_cv_i, AUPRC = pr_cv_i$auprc,
      Precision = prec_cv, Recall = rec_cv,
      F1 = f1_cv, Balanced_Acc = balacc_cv
    )
  }
}

df_cv_mil  <- do.call(rbind, cv_rows_mil)
cv_auc_mil <- df_cv_mil$AUC

metriques_cv_noms <- c("AUC", "AUPRC", "Precision", "Recall", "F1", "Balanced_Acc")
df_cv_mil_resum <- do.call(rbind, lapply(metriques_cv_noms, function(m) {
  vals <- df_cv_mil[[m]]
  data.frame(
    Metrica = m,
    Mitjana = round(mean(vals, na.rm = TRUE), 4),
    SD      = round(sd(vals,   na.rm = TRUE), 4),
    IC_2.5  = round(quantile(vals, 0.025, na.rm = TRUE), 4),
    IC_97.5 = round(quantile(vals, 0.975, na.rm = TRUE), 4)
  )
}))

cat(sprintf("Repeated %dx%d-fold CV — Model Millorat:\n\n", n_rep, n_fold))
print(df_cv_mil_resum, row.names = FALSE)
cat("\n")

#### ============================================================ ####
####     5. LLINDAR PR (Recall >= 0.60) + METRIQUES TEST         ####
#### ============================================================ ####

cat("\n========== 5. LLINDAR PR I METRIQUES SOBRE TEST ==========\n\n")

prob_test_mil <- predict(model_millorat, newdata = dades_test, type = "response")

pr_mil <- seleccionar_llindar_pr(prob_test_mil, dades_test$Y, MIN_RECALL)
thresh_pr_mil <- pr_mil$threshold

cat(sprintf("AUPRC: %.4f\n", pr_mil$auprc))
cat(sprintf("Llindar seleccionat: %.4f | recall_ok (>= %.2f): %s\n\n",
            thresh_pr_mil, MIN_RECALL,
            ifelse(pr_mil$recall_ok, "SI", "NO (fallback Youden)")))

ggplot(pr_mil$pr_curve, aes(x = recall, y = precision)) +
  geom_path(color = "#4A90B8", linewidth = 1) +
  geom_vline(xintercept = MIN_RECALL, linetype = "dashed",
             color = "red", linewidth = 0.8) +
  geom_point(data = data.frame(recall = pr_mil$recall,
                               precision = ifelse(is.na(pr_mil$precision),
                                                  0, pr_mil$precision)),
             color = "#E07B54", size = 3, shape = 17) +
  annotate("text", x = MIN_RECALL + 0.04, y = 0.1,
           label = sprintf("Recall min\n= %.2f", MIN_RECALL),
           color = "red", size = 3.5) +
  labs(title = "Corba Precisio-Recall — Logit Millorat (test)",
       subtitle = sprintf("AUPRC = %.4f | Llindar = %.4f",
                          pr_mil$auprc, thresh_pr_mil),
       x = "Recall", y = "Precisio (PPV)") +
  theme_minimal(base_size = 13)

# Referencia: Precisio del CV (estimacio sense biaix de seleccio de llindar)
prec_cv_r <- df_cv_mil_resum[df_cv_mil_resum$Metrica == "Precision", ]
cat(sprintf("Precisio CV (5x10-fold): %.4f +/- %.4f [IC95%% %.4f, %.4f]\n",
            prec_cv_r$Mitjana, prec_cv_r$SD, prec_cv_r$IC_2.5, prec_cv_r$IC_97.5))
cat("NOTA: la precisio sobre test pot estar inflada perque el llindar s'ha seleccionat\n")
cat("sobre el MATEIX conjunt de test (biaix d'optimisme). La precisio CV es mes fiable.\n\n")

# Metriques sobre test
cat("--- 5.1 Metriques sobre test ---\n")
metriques_mil <- calcular_metriques(
  model_glm = model_millorat,
  dades_test_df = dades_test,
  nom_model = "Logit Millorat",
  auc_cv_mean = mean(cv_auc_mil, na.rm = TRUE),
  auc_cv_sd = sd(cv_auc_mil, na.rm = TRUE),
  thresh_override = thresh_pr_mil
)
mostrar_metriques(metriques_mil)

cat("--- 5.2 Metriques sobre train ---\n")
metriques_mil_train <- calcular_metriques(
  model_glm = model_millorat,
  dades_test_df = dades_train,
  nom_model = "Logit Millorat (train)",
  thresh_override = thresh_pr_mil
)
mostrar_metriques(metriques_mil_train)

cat("\n--- Resum overfitting: train vs test ---\n\n")
df_ov_mil <- data.frame(
  Conjunt = c("Train", "Test"),
  AUC = c(metriques_mil_train$AUC, metriques_mil$AUC),
  Precision = c(metriques_mil_train$precision, metriques_mil$precision),
  Recall = c(metriques_mil_train$recall, metriques_mil$recall),
  F1 = c(metriques_mil_train$F1, metriques_mil$F1),
  Balanced_Acc = c(metriques_mil_train$balanced_accuracy, metriques_mil$balanced_accuracy)
)
print(df_ov_mil, row.names = FALSE)

prec_test <- metriques_mil$precision
prec_train <- metriques_mil_train$precision
if (!is.na(prec_test) && !is.na(prec_train) && prec_test > prec_train + 0.05) {
  cat(sprintf(
    "\nAVIS: precisio test (%.3f) > precisio train (%.3f): patro invers a l'overfitting classic.\n",
    prec_test, prec_train))
  cat("Causa probable: llindar seleccionat per maximitzar precisio sobre el conjunt de test.\n")
  cat(sprintf("Precisio CV (sense biaix): %.3f — usa aquest valor per a conclusions.\n", prec_cv_r$Mitjana))
}
cat("\n")

# Bootstrap CI per a Precisio
cat("--- 5.3 IC Bootstrap Precisio (B = 1000, percentil) ---\n\n")
set.seed(1234)
B <- 1000
boot_ppv_mil <- numeric(B)

for (b in seq_len(B)) {
  idx_b  <- sample(nrow(dades_test), replace = TRUE)
  dades_b <- dades_test[idx_b, ]
  prob_b  <- predict(model_millorat, newdata = dades_b, type = "response")
  pred_b  <- as.integer(prob_b >= thresh_pr_mil)
  TP_b    <- sum(pred_b == 1 & dades_b$Y == 1)
  FP_b    <- sum(pred_b == 1 & dades_b$Y == 0)
  boot_ppv_mil[b] <- if (TP_b + FP_b > 0) TP_b / (TP_b + FP_b) else NA_real_
}

boot_clean_mil <- boot_ppv_mil[!is.na(boot_ppv_mil)]
ic_low_mil  <- quantile(boot_clean_mil, 0.025)
ic_high_mil <- quantile(boot_clean_mil, 0.975)

cat(sprintf("Precisio (PPV) puntual:  %.4f\n", metriques_mil$precision))
cat(sprintf("IC Bootstrap 95%%:        [%.4f, %.4f]\n", ic_low_mil, ic_high_mil))
cat(sprintf("Amplada IC:              %.4f\n\n", ic_high_mil - ic_low_mil))

ggplot(data.frame(ppv = boot_clean_mil), aes(x = ppv)) +
  geom_histogram(bins = 40, fill = "#4A90B8", color = "white", alpha = 0.85) +
  geom_vline(xintercept = metriques_mil$precision,
             color = "#E07B54", linewidth = 1.2) +
  geom_vline(xintercept = c(ic_low_mil, ic_high_mil),
             color = "red", linewidth = 0.9, linetype = "dashed") +
  annotate("text", x = metriques_mil$precision, y = Inf,
           label = sprintf("PPV = %.3f", metriques_mil$precision),
           vjust = 1.5, hjust = -0.1, color = "#E07B54", size = 4) +
  labs(title = "Distribucio Bootstrap Precisio (PPV) — Logit Millorat (test)",
       subtitle = sprintf("IC 95%% = [%.4f, %.4f] | B = %d",
                          ic_low_mil, ic_high_mil, B),
       x = "Precisio (PPV)", y = "Frequencia") +
  theme_minimal(base_size = 13)

# Comparacio ROC: Logit base vs Logit Millorat
prob_test_base <- predict(model_seleccionat, newdata = dades_test, type = "response")
roc_base <- roc(dades_test$Y, prob_test_base, quiet = TRUE)
roc_mil  <- roc(dades_test$Y, prob_test_mil,  quiet = TRUE)

df_roc_base <- data.frame(spec_inv = 1 - roc_base$specificities,
                          sens = roc_base$sensitivities, model = "Logit base")
df_roc_mil  <- data.frame(spec_inv = 1 - roc_mil$specificities,
                          sens = roc_mil$sensitivities,  model = "Logit millorat")
df_roc_comp <- rbind(df_roc_base, df_roc_mil)

ggplot(df_roc_comp, aes(x = spec_inv, y = sens, color = model)) +
  geom_path(linewidth = 1.1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c("Logit base" = "#4A90B8", "Logit millorat" = "#E07B54")) +
  annotate("text", x = 0.55, y = 0.15,
           label = sprintf("AUC base = %.3f\nAUC millorat = %.3f",
                           as.numeric(auc(roc_base)), as.numeric(auc(roc_mil))),
           size = 4, color = "grey20") +
  labs(title = "Comparacio ROC: Logit base vs. Logit Millorat",
       x = "1 - Especificitat", y = "Sensibilitat", color = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

#### ============================================================ ####
####          6. CALIBRATION PLOT                                 ####
#### ============================================================ ####

cat("\n========== 6. CALIBRATION PLOT ==========\n\n")

# Hosmer-Lemeshow sobre test
hl_mil <- hoslem.test(dades_test$Y, prob_test_mil, g = 10)
cat(sprintf("Hosmer-Lemeshow: chi2 = %.4f | gl = %d | p = %.4f\n",
            hl_mil$statistic, hl_mil$parameter, hl_mil$p.value))
cat(ifelse(hl_mil$p.value > 0.05,
           "-> No es rebutja H0: el model s'ajusta be (p > 0.05)\n\n",
           "-> Es rebutja H0: problemes d'ajust (p < 0.05)\n\n"))

# Decile calibration plot (loess)
df_cal <- data.frame(
  prob_pred = prob_test_mil,
  Y_obs     = dades_test$Y
) %>%
  arrange(prob_pred) %>%
  mutate(decil = ntile(prob_pred, 10))

df_cal_dec <- df_cal %>%
  group_by(decil) %>%
  summarise(
    prob_mitjana = mean(prob_pred),
    prop_obs     = mean(Y_obs),
    n            = n(),
    .groups = "drop"
  )

ggplot(df_cal_dec, aes(x = prob_mitjana, y = prop_obs)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "grey50", linewidth = 0.9) +
  geom_smooth(method = "loess", se = TRUE, color = "#4A90B8",
              fill = "#AED6F1", linewidth = 1, span = 1) +
  geom_point(aes(size = n), color = "#E07B54", alpha = 0.85) +
  scale_size_continuous(range = c(2, 6), guide = "none") +
  annotate("text", x = 0.75, y = 0.15,
           label = sprintf("HL p = %.3f", hl_mil$p.value),
           size = 4, color = "grey30") +
  labs(title = "Calibration plot — Logit Millorat (test)",
       subtitle = "Decils de probabilitat predicta vs. proporció observada",
       x = "Probabilitat predicta mitjana (decil)",
       y = "Proporció observada Regular (>=80%)") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_minimal(base_size = 13)

  # Después del HL, añadir diagnóstico por decil
cat("\nDetall desajust per decil:\n")
print(df_cal_dec %>% 
  mutate(residu = prop_obs - prob_mitjana,
         flag = abs(residu) > 0.15) %>%
  dplyr::select(decil, n, prob_mitjana, prop_obs, residu, flag))

hl_6 <- hoslem.test(dades_test$Y, prob_test_mil, g = 6)
hl_8 <- hoslem.test(dades_test$Y, prob_test_mil, g = 8)
cat(sprintf("HL g=6: chi2=%.3f p=%.4f\n", hl_6$statistic, hl_6$p.value))
cat(sprintf("HL g=8: chi2=%.3f p=%.4f\n", hl_8$statistic, hl_8$p.value))

#### ============================================================ ####
####     7. EFECTES MARGINALS PROMIG (AME) — marginaleffects     ####
#### ============================================================ ####

cat("\n========== 7. EFECTES MARGINALS PROMIG (AME) ==========\n\n")

if (!requireNamespace("marginaleffects", quietly = TRUE)) install.packages("marginaleffects")
library(marginaleffects)

ame_mil <- avg_slopes(model_millorat, newdata = dades_test)
print(ame_mil)

df_ame <- as.data.frame(ame_mil)
df_ame_plot <- df_ame %>%
  filter(!grepl("Intercept", term, ignore.case = TRUE)) %>%
  dplyr::select(any_of(c("term", "estimate", "conf.low", "conf.high"))) %>%
  arrange(estimate)

ggplot(df_ame_plot, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point(size = 3, color = "#4A90B8") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.25, color = "#4A90B8") +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "red", linewidth = 0.8) +
  coord_flip() +
  labs(title = "Efectes Marginals Promig (AME) — Logit Millorat",
       subtitle = "IC 95% (delta method) | variable resposta: P(Regular >= 80%)",
       x = "", y = "AME (canvi en probabilitat)") +
  theme_minimal(base_size = 13)

#### ============================================================ ####
####     8. GUARDAR MODEL, PROBABILITATS I BBDD ENCADENADA       ####
#### ============================================================ ####

saveRDS(metriques_mil, "2. Dades/metriques_logit_millorat.rds")
saveRDS(model_millorat, "2. Dades/model_logit_millorat.rds")

prob_tots_mil <- predict(model_millorat, newdata = dades_mod, type = "response")
dades_def$prob_logit_mil <- NA_real_
dades_def$prob_logit_mil[seq_len(nrow(dades_mod))] <- prob_tots_mil

dades_def$pred_logit_mil <- NA_integer_
dades_def$pred_logit_mil[seq_len(nrow(dades_mod))] <-
  as.integer(prob_tots_mil >= thresh_pr_mil)

cat(sprintf("Llindar aplicat per pred_logit_mil: %.4f (PR recall>=%.2f)\n\n",
            thresh_pr_mil, MIN_RECALL))

save(dades_def, file = "2. Dades/9. Dades Logit Millorat.RData")
cat("-> dades_def amb prob_logit_mil guardades a: 2. Dades/9. Dades Logit Millorat.RData\n\n")

sink()
dev.off()

