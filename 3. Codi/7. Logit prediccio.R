packages <- c("dplyr", "ggplot2", "tidyr", "pROC", "PRROC",
              "ResourceSelection", "tibble", "caret")

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}
lapply(packages, install_if_missing)
rm(packages)

#setwd("C:/Users/edurn/Downloads/TFG")
setwd("C:/Users/Edurne/Downloads/TFG")

load("2. Dades/5. Dades Logit.RData")
source("3. Codi/Funcions models.R")

sink("4. Outputs/7.1 Output_text_logit_pred.txt")
pdf("4. Outputs/7.2 Output_grafics_logit_pred.pdf", width = 10, height = 8)

#### ============================================================ ####
####                   0. PREPARACIÓ DE DADES                     ####
#### ============================================================ ####

dades_mod <- dades_def %>%
  mutate(Y = as.integer(GRUP_ASSIST == "Regular (≥80%)")) %>%
  filter(!is.na(Y))

dades_mod$NOTA_num     <- as.numeric(dades_mod$NOTA)
dades_mod$IA_SUBST_num <- as.numeric(dades_mod$IA_SUBST)
dades_mod$IA_ATENC_num <- as.numeric(dades_mod$IA_ATENC)

# partició compartida per tots els models 
idx_train   <- crear_o_carregar_particio(dades_mod$Y)
dades_train <- dades_mod[idx_train, ]
dades_test  <- dades_mod[-idx_train, ]

cat("==== 0. PARTICIÓ TRAIN / TEST ====\n")
cat(sprintf("Train = %d obs | Test = %d obs\n", nrow(dades_train), nrow(dades_test)))
cat(sprintf("  Train — Regular: %.1f%% | Irregular: %.1f%%\n",
            mean(dades_train$Y) * 100, (1 - mean(dades_train$Y)) * 100))
cat(sprintf("  Test  — Regular: %.1f%% | Irregular: %.1f%%\n\n",
            mean(dades_test$Y) * 100, (1 - mean(dades_test$Y)) * 100))

MIN_RECALL <- 0.60

#### ============================================================ ####
####          1. SELECCIÓ DEL MODEL PREDICTIU (AIC sobre TRAIN)   ####
#### ============================================================ ####

cat("============== 1. CANDIDATS PREDICTIUS (AIC sobre TRAIN) =============\n\n")

mk_f <- function(...) as.formula(paste("Y ~", paste(c(...), collapse = " + ")))

vars_fa_mot     <- c("MOT_DESMOTIVACIO", "MOT_AUTOGESTIO", "MOT_FORCA_MAJOR")
vars_fa_mot_ref <- c("MOT_DESMOTIVACIO", "MOT_FORCA_MAJOR")
vars_fa_est     <- c("EST_QUALITAT_DOC", "EST_AVALUACIO_AC", "EST_TEMPS_CLASSE", "EST_GRUPS_REDUITS")
vars_fa_est_ref <- "EST_AVALUACIO_AC"
vars_fa_ia      <- c("IA_EINA_ESTUDI", "IA_SUBSTITUCIO")
vars_cat_full   <- c("T_AVAL", "CURS_1R", "GENERE", "DOBLE_GRAU_EST")
vars_cat_ref    <- c("T_AVAL", "CURS_1R")
vars_num_full   <- c("EDAT", "DESPL", "NOTA_num")
vars_num_ref    <- c("EDAT", "NOTA_num")

# Formula del model explicatiu (millor BIC de 5. Logit.R, pot incloure interaccions)
formula_exp <- tryCatch(readRDS("2. Dades/formula_logit_explicatiu.rds"), error = function(e) NULL)

candidats <- list(
  "0. Complet"                           = mk_f(vars_fa_mot, vars_fa_est, vars_fa_ia, vars_cat_full, vars_num_full),
  "1.1 Likert num (IA_SUBST + IA_ATENC)" = mk_f(vars_fa_mot, vars_fa_est_ref, "IA_SUBST_num", "IA_ATENC_num", vars_cat_ref, vars_num_ref),
  "1.2 Likert num (IA_SUBST)"            = mk_f(vars_fa_mot, vars_fa_est_ref, "IA_SUBST_num", vars_cat_ref, vars_num_ref),
  "3.0 sense MOT_AUTOGESTIO"             = mk_f(vars_fa_mot_ref, vars_fa_est_ref, "IA_SUBST_num", vars_cat_ref, vars_num_ref)
)
if (!is.null(formula_exp))
  candidats[["E. Explicatiu (BIC)"]] <- formula_exp

models_pred <- lapply(candidats, function(f)
  tryCatch(glm(f, data = dades_train, family = binomial), error = function(e) NULL)
)
models_pred <- Filter(Negate(is.null), models_pred)

df_sel <- do.call(rbind, lapply(names(models_pred), function(nom) {
  m <- models_pred[[nom]]
  data.frame(model = nom, n_param = length(coef(m)),
             AIC = round(AIC(m), 2), BIC = round(BIC(m), 2), stringsAsFactors = FALSE)
}))

print(df_sel, row.names = FALSE)

nom_aic   <- df_sel$model[which.min(df_sel$AIC)]
model_sel <- models_pred[[nom_aic]]
formula_sel <- formula(model_sel)

cat(sprintf("\n→ Model seleccionat per AIC: %s\n", nom_aic))
cat("  Fórmula:\n"); print(formula_sel); cat("\n")
print(summary(model_sel))

#### ============================================================ ####
####          2. REPEATED 5×10-fold CV (sobre TRAIN)              ####
#### ============================================================ ####

cat("\n===== 2. Repeated 5×10-fold CV (train) =====\n\n")

set.seed(1234)
n_rep <- 5; n_fold <- 10
cv_rows  <- vector("list", n_rep * n_fold)
k        <- 0
oof_probs <- rep(NA_real_, nrow(dades_train))

for (r in seq_len(n_rep)) {
  folds_r <- createFolds(dades_train$Y, k = n_fold, list = TRUE)
  for (fold_idx in seq_along(folds_r)) {
    k <- k + 1
    test_idx <- folds_r[[fold_idx]]
    m_cv <- tryCatch(
      glm(formula_sel, data = dades_train[-test_idx, ], family = binomial),
      error = function(e) NULL
    )
    if (is.null(m_cv)) next
    prob_cv <- predict(m_cv, newdata = dades_train[test_idx, ], type = "response")
    Y_cv    <- dades_train$Y[test_idx]
    if (r == 1) oof_probs[test_idx] <- prob_cv

    auc_i  <- tryCatch(as.numeric(pROC::auc(pROC::roc(Y_cv, prob_cv, quiet = TRUE))), error = function(e) NA_real_)
    pr_i   <- tryCatch(seleccionar_llindar_pr(prob_cv, Y_cv, MIN_RECALL),
                       error = function(e) list(threshold = 0.5, auprc = NA_real_))
    pred_i <- as.integer(prob_cv >= pr_i$threshold)
    TP <- sum(pred_i==1&Y_cv==1); TN <- sum(pred_i==0&Y_cv==0)
    FP <- sum(pred_i==1&Y_cv==0); FN <- sum(pred_i==0&Y_cv==1)
    prec  <- if (TP+FP>0) TP/(TP+FP) else NA_real_
    rec   <- if (TP+FN>0) TP/(TP+FN) else NA_real_
    spec  <- if (TN+FP>0) TN/(TN+FP) else NA_real_
    f1    <- if (!is.na(prec)&&!is.na(rec)&&(prec+rec)>0) 2*prec*rec/(prec+rec) else NA_real_
    balacc <- if (!is.na(rec)&&!is.na(spec)) (rec+spec)/2 else NA_real_

    cv_rows[[k]] <- data.frame(rep=r, fold=fold_idx, AUC=auc_i, AUPRC=pr_i$auprc,
                                Precision=prec, Recall=rec, F1=f1, Balanced_Acc=balacc)
  }
}

df_cv <- do.call(rbind, cv_rows)

metriques_noms <- c("AUC", "AUPRC", "Precision", "Recall", "F1", "Balanced_Acc")
df_cv_resum <- do.call(rbind, lapply(metriques_noms, function(m) {
  vals <- df_cv[[m]]
  data.frame(Metrica = m,
             Mitjana = round(mean(vals, na.rm = TRUE), 4),
             SD      = round(sd(vals, na.rm = TRUE), 4),
             IC_2.5  = round(quantile(vals, 0.025, na.rm = TRUE), 4),
             IC_97.5 = round(quantile(vals, 0.975, na.rm = TRUE), 4))
}))

cat(sprintf("Repeated %d×%d-fold CV:\n\n", n_rep, n_fold))
print(df_cv_resum, row.names = FALSE)

# Threshold OOF (1a repetició)
cc_oof   <- !is.na(oof_probs)
pr_oof   <- seleccionar_llindar_pr(oof_probs[cc_oof], dades_train$Y[cc_oof], MIN_RECALL)
thresh_oof <- pr_oof$threshold
cat(sprintf("\nThreshold OOF: %.4f | recall_ok (>=%.2f): %s\n\n",
            thresh_oof, MIN_RECALL, ifelse(pr_oof$recall_ok, "SI", "NO (Youden)")))

#### ============================================================ ####
####                         3. MÈTRIQUES TEST                    ####
#### ============================================================ ####

metriques_logit_pred <- calcular_metriques(
  model_glm    = model_sel,
  dades_test_df = dades_test,
  nom_model    = "Logit Predictiu",
  auc_cv_mean  = mean(df_cv$AUC, na.rm = TRUE),
  auc_cv_sd    = sd(df_cv$AUC, na.rm = TRUE),
  thresh_override = thresh_oof
)

cat("=== 3. MÈTRIQUES TRAIN (CV) vs TEST ===\n\n")

pr_test <- seleccionar_llindar_pr(
  predict(model_sel, newdata = dades_test, type = "response"),
  dades_test$Y, MIN_RECALL
)

metriques_noms_ord <- c("AUC", "AUPRC", "Balanced_Acc", "F1", "Precision", "Recall")
train_vals <- setNames(df_cv_resum$Mitjana, df_cv_resum$Metrica)
test_vals  <- c(
  AUC          = metriques_logit_pred$AUC,
  AUPRC        = round(pr_test$auprc, 4),
  Balanced_Acc = metriques_logit_pred$balanced_accuracy,
  F1           = metriques_logit_pred$F1,
  Precision    = metriques_logit_pred$precision,
  Recall       = metriques_logit_pred$recall
)

df_comp <- data.frame(
  Metrica  = metriques_noms_ord,
  Train_CV = round(train_vals[metriques_noms_ord], 4),
  Test     = round(test_vals[metriques_noms_ord], 4),
  Diff     = round(test_vals[metriques_noms_ord] - train_vals[metriques_noms_ord], 4),
  stringsAsFactors = FALSE
)
df_comp$Flag <- ifelse(df_comp$Train_CV - df_comp$Test > 0.05, "(!)", "")
rownames(df_comp) <- NULL

print(df_comp, row.names = FALSE)

# Grafic: Train_CV vs Test per metrica
df_comp_long <- df_comp %>%
  tidyr::pivot_longer(cols = c(Train_CV, Test), names_to = "Conjunt", values_to = "Valor") %>%
  mutate(Metrica = factor(Metrica, levels = metriques_noms_ord))

print(
  ggplot(df_comp_long, aes(x = Metrica, y = Valor, fill = Conjunt)) +
    geom_col(position = position_dodge(width = 0.65), alpha = 0.85, width = 0.65) +
    
    geom_text(
      aes(label = round(Valor, 2)),                     # Redondeamos a 2 decimales (ajusta según necesites)
      position = position_dodge(width = 0.65),          # Obliga al texto a separarse igual que las barras
      vjust = -0.4,                                     # Ajuste vertical para ponerlo justo ENCIMA de la barra
      size = 3.5,                                       # Tamaño de la fuente del número
      fontface = "bold"                                 # Opcional: poner el texto en negrita
    ) +
    
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50") +
    scale_fill_manual(values = c("Train_CV" = "#4A90B8", "Test" = "#E07B54")) +
    
    scale_y_continuous(limits = c(0, 1.05)) + 
    labs(
      title    = "Logit Predictiu — Train (CV) vs Test",
      subtitle = sprintf("Threshold OOF: %.3f | MIN_RECALL >= %.2f", thresh_oof, MIN_RECALL),
      x = "", y = "Valor", fill = "Conjunt"
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "top")
)

# Grafic: Corba ROC (test)
prob_test_lp  <- predict(model_sel, newdata = dades_test, type = "response")
roc_lp        <- pROC::roc(dades_test$Y, prob_test_lp, quiet = TRUE)
roc_df_lp     <- data.frame(spec_inv = 1 - roc_lp$specificities,
                             sens     = roc_lp$sensitivities)
print(
  ggplot(roc_df_lp, aes(x = spec_inv, y = sens)) +
    geom_path(color = "#4A90B8", linewidth = 1.2) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
    annotate("text", x = 0.65, y = 0.15,
             label = sprintf("AUC = %.4f", metriques_logit_pred$AUC),
             size = 5, color = "#4A90B8") +
    labs(title = "Corba ROC — Logit Predictiu (test)",
         x = "1 - Especificitat", y = "Sensibilitat") +
    theme_minimal(base_size = 13)
)

# Grafic: Corba PR (OOF train)
pr_oof_full <- seleccionar_llindar_pr(oof_probs[cc_oof], dades_train$Y[cc_oof], MIN_RECALL)
print(
  ggplot(pr_oof_full$pr_curve, aes(x = recall, y = precision)) +
    geom_path(color = "#4A90B8", linewidth = 1) +
    geom_vline(xintercept = MIN_RECALL, linetype = "dashed", color = "red", linewidth = 0.8) +
    geom_point(data = data.frame(recall = pr_oof_full$recall,
                                 precision = ifelse(is.na(pr_oof_full$precision), 0,
                                                    pr_oof_full$precision)),
               color = "#E07B54", size = 3, shape = 17) +
    annotate("text", x = MIN_RECALL + 0.04, y = 0.1,
             label = sprintf("Recall min\n= %.2f", MIN_RECALL),
             color = "red", size = 3.5) +
    labs(title = "Corba Precisio-Recall — Logit Predictiu (OOF train)",
         subtitle = sprintf("AUPRC = %.4f | Threshold = %.4f", pr_oof_full$auprc, thresh_oof),
         x = "Recall", y = "Precisio (PPV)") +
    theme_minimal(base_size = 13)
)

# Grafic: Distribucio metriques CV per fold
df_cv_long <- df_cv %>%
  tidyr::pivot_longer(cols = all_of(metriques_noms), names_to = "Metrica", values_to = "Valor") %>%
  mutate(Metrica = factor(Metrica, levels = metriques_noms))

print(
  ggplot(df_cv_long, aes(x = Metrica, y = Valor)) +
    geom_boxplot(fill = "#4A90B8", alpha = 0.5, outlier.shape = 21) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50") +
    labs(title = sprintf("Distribucio metriques CV (%d×%d-fold) — Logit Predictiu", n_rep, n_fold),
         x = "", y = "Valor") +
    theme_minimal(base_size = 13)
)

# Matriu de confusió (test)
cat("\n--- Matriu de confusió (test) ---\n")
cat(sprintf("Llindar aplicat: %.4f (OOF PR, recall >= %.2f)\n\n", thresh_oof, MIN_RECALL))
cm_text <- matrix(
  c(metriques_logit_pred$TN, metriques_logit_pred$FN,
    metriques_logit_pred$FP, metriques_logit_pred$TP),
  nrow = 2,
  dimnames = list(Observat = c("Irregular(0)", "Regular(1)"),
                  Predit   = c("Irregular(0)", "Regular(1)"))
)
print(cm_text)
cat(sprintf("\n  Precision (PPV): %.4f | Recall (Sens): %.4f | F1: %.4f\n\n",
            metriques_logit_pred$precision,
            metriques_logit_pred$recall,
            metriques_logit_pred$F1))

print(grafic_cm(metriques_logit_pred, "Logit Predictiu (test)"))

saveRDS(metriques_logit_pred, "2. Dades/metriques_logit_pred.rds")
saveRDS(model_sel, "2. Dades/model_logit_pred.rds")

#### ============================================================ ####
####                      4. GUARDAR DADES                        ####
#### ============================================================ ####

prob_tots <- predict(model_sel, newdata = dades_mod, type = "response")

dades_def$prob_logit <- NA_real_
dades_def$prob_logit[seq_len(nrow(dades_mod))] <- prob_tots

dades_def$pred_logit <- NA_integer_
dades_def$pred_logit[seq_len(nrow(dades_mod))] <- as.integer(prob_tots >= thresh_oof)

save(dades_def, file = "2. Dades/7. Dades Logit Predictiu.RData")

cat(sprintf("Threshold aplicat: %.4f (OOF PR, recall >= %.2f)\n\n", thresh_oof, MIN_RECALL))

sink()
dev.off()
