# ================================================================
# 16. KNN Probabilistic — Fase 6
# ================================================================
# Features: [X_sc_all (16) | u1 | u2] = 18 columnes
# Target:   GRUP_ASSIST binari (1 = Regular >=80%)
# Particio: mateixa que FCM (set.seed(2024), createDataPartition)
# ================================================================

packages <- c("dplyr", "ggplot2", "tibble", "tidyr", "caret",
              "pROC", "FNN", "e1071")

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg); library(pkg, character.only = TRUE)
  }
}
lapply(packages, install_if_missing)
rm(packages)

setwd("C:/Users/Edurne/Downloads/TFG")

load("2. Dades/fuzzy_clustering_model.RData")
load("2. Dades/4. Dades EFA.RData")

sink("4. Outputs/16.1 Output_text_knn.txt", split = TRUE)
pdf("4. Outputs/16.2 Output_grafics_knn.pdf", width = 10, height = 8)

col_clusters <- c("1" = "#4A90B8", "2" = "#E07B54")
MIN_RECALL   <- 0.60

# -----------------------------------------------------------------------
# FUNCIONS AUXILIARS
# -----------------------------------------------------------------------

proj_test <- function(X_test, centers, m) {
  n <- nrow(X_test); c_num <- nrow(centers)
  exp_val <- 2 / (m - 1)
  D <- matrix(0, nrow = n, ncol = c_num)
  for (k in seq_len(c_num))
    D[, k] <- sqrt(rowSums(
      (X_test - matrix(centers[k, ], nrow = n, ncol = ncol(X_test), byrow = TRUE))^2
    ))
  U <- matrix(0, nrow = n, ncol = c_num)
  for (i in seq_len(n)) {
    d <- D[i, ]
    if (any(d == 0)) {
      U[i, which(d == 0)[1]] <- 1
    } else {
      for (k in seq_len(c_num)) U[i, k] <- 1 / sum((d[k] / d)^exp_val)
    }
  }
  U
}

knn_probs <- function(X_tr, Y_tr, X_te, k) {
  pred <- FNN::knn(train = X_tr, test = X_te, cl = Y_tr, k = k, prob = TRUE)
  p    <- attr(pred, "prob")
  ifelse(as.character(pred) == "1", p, 1 - p)
}

ece_fn <- function(probs, Y, n_bins = 10) {
  breaks <- seq(0, 1, length.out = n_bins + 1)
  bins   <- cut(probs, breaks = breaks, include.lowest = TRUE)
  df_b   <- data.frame(prob = probs, Y = Y, bin = bins) %>%
    group_by(bin) %>%
    summarise(n = n(), conf = mean(prob), acc = mean(Y), .groups = "drop") %>%
    filter(n > 0)
  sum(df_b$n * abs(df_b$conf - df_b$acc)) / length(Y)
}

# ================================================================
cat("=================================================================\n")
cat("   FASE 6 — KNN PROBABILISTIC\n")
cat("=================================================================\n\n")
# ================================================================

# --- 6.1 Preparació ---
cat("--- 6.1 Preparació ---\n\n")

df_all <- dades_def %>%
  transmute(
    EDAT           = as.numeric(EDAT),
    DESPL          = as.numeric(DESPL),
    N_ASSIG        = as.numeric(N_ASSIG),
    NOTA_num       = as.numeric(NOTA),
    T_AVAL_num     = as.integer(T_AVAL == "Continuada"),
    CURS_1R        = as.integer(CURS_1R),
    GENERE_Home    = as.integer(GENERE == "Home"),
    DOBLE_GRAU_EST = as.integer(DOBLE_GRAU_EST),
    DEDIC_num      = as.integer(DEDIC),
    IA_HABIT       = as.integer(IA_HABIT),
    IA_COMPR       = as.integer(IA_COMPR),
    IA_REND        = as.integer(IA_REND),
    IA_PDFS        = as.integer(IA_PDFS),
    IA_SUBST       = as.integer(IA_SUBST),
    IA_ATENC       = as.integer(IA_ATENC),
    IA_CONF        = as.integer(IA_CONF),
    GRUP_ASSIST    = GRUP_ASSIST,
    P_ASSIST       = as.numeric(P_ASSIST)
  )

cc_all <- complete.cases(df_all[, vars_clust])
cat(sprintf("Obs completes per KNN: %d / %d\n\n", sum(cc_all), nrow(df_all)))

X_all_raw <- as.matrix(df_all[cc_all, vars_clust])
X_all_sc  <- scale(X_all_raw, center = scale_params$mean, scale = scale_params$sd)

U_all    <- proj_test(X_all_sc, fcm_final$centers, m_final)
hard_all <- apply(U_all, 1, which.max)
u1_all   <- U_all[, 1]
u2_all   <- U_all[, 2]

# Matriu de features KNN: [X_sc | u1 | u2] → n x 18
X_knn <- cbind(X_all_sc, u1 = u1_all, u2 = u2_all)
colnames(X_knn) <- c(vars_clust, "u1", "u2")

Y_knn <- as.integer(df_all$GRUP_ASSIST[cc_all] == "Regular (≥80%)")

# Reconstruir la mateixa particio que el model FCM
set.seed(2024)
idx_train_knn <- caret::createDataPartition(df_all$GRUP_ASSIST[cc_all], p = 0.80, list = FALSE)

X_train_knn <- X_knn[idx_train_knn, ]
X_test_knn  <- X_knn[-idx_train_knn, ]
Y_train_knn <- Y_knn[idx_train_knn]
Y_test_knn  <- Y_knn[-idx_train_knn]
hard_train_knn <- hard_all[idx_train_knn]
hard_test_knn  <- hard_all[-idx_train_knn]
u1_train_knn   <- u1_all[idx_train_knn]
u1_test_knn    <- u1_all[-idx_train_knn]
u2_test_knn    <- u2_all[-idx_train_knn]

n_train_knn <- nrow(X_train_knn)
n_test_knn  <- nrow(X_test_knn)

cat(sprintf("Train: %d obs | Test: %d obs\n", n_train_knn, n_test_knn))
cat(sprintf("Train Y: Regular=%d (%.1f%%) | Irregular=%d (%.1f%%)\n\n",
            sum(Y_train_knn), mean(Y_train_knn) * 100,
            sum(1 - Y_train_knn), mean(1 - Y_train_knn) * 100))

# --- 6.2 Seleccio de k via CV 5-fold ---
cat("--- 6.2 Seleccio de k (CV 5-fold, metrica: AUC-ROC) ---\n\n")

k_vals <- c(3, 5, 7, 9, 11)

set.seed(2024)
folds_knn <- caret::createFolds(Y_train_knn, k = 5, list = TRUE)

cv_results <- do.call(rbind, lapply(k_vals, function(k) {
  auc_folds <- sapply(folds_knn, function(val_idx) {
    tr_idx  <- setdiff(seq_len(n_train_knn), val_idx)
    probs_v <- knn_probs(X_train_knn[tr_idx, ], Y_train_knn[tr_idx],
                         X_train_knn[val_idx, ], k = k)
    if (length(unique(Y_train_knn[val_idx])) < 2) return(NA_real_)
    as.numeric(pROC::auc(pROC::roc(Y_train_knn[val_idx], probs_v, quiet = TRUE)))
  })
  data.frame(k = k, AUC_CV = round(mean(auc_folds, na.rm = TRUE), 4),
             SD_AUC = round(sd(auc_folds, na.rm = TRUE), 4))
}))

print(cv_results, row.names = FALSE)

k_optim <- cv_results$k[which.max(cv_results$AUC_CV)]
cat(sprintf("\n>>> k optim seleccionat: k=%d (AUC-CV=%.4f)\n\n",
            k_optim, max(cv_results$AUC_CV)))

# Grafic: AUC-CV vs k
print(
  ggplot(cv_results, aes(x = k, y = AUC_CV)) +
    geom_line(color = "#4A90B8", linewidth = 1.1) +
    geom_point(size = 3, color = "#4A90B8") +
    geom_errorbar(aes(ymin = AUC_CV - SD_AUC, ymax = AUC_CV + SD_AUC),
                  width = 0.3, color = "#4A90B8", alpha = 0.5) +
    geom_vline(xintercept = k_optim, linetype = "dashed", color = "#E07B54") +
    annotate("text", x = k_optim + 0.3, y = min(cv_results$AUC_CV) + 0.002,
             label = paste0("k=", k_optim), color = "#E07B54", size = 4) +
    scale_x_continuous(breaks = k_vals) +
    labs(
      title    = "Seleccio de k — KNN probabilistic (CV 5-fold)",
      subtitle = "AUC-ROC mig ± 1 SD | Particio train (80%)",
      x = "k (nombre de veins)", y = "AUC-ROC (CV)"
    ) +
    theme_minimal(base_size = 13)
)

# --- 6.3 Model KNN final ---
cat("--- 6.3 Model KNN final (k=", k_optim, ") ---\n\n", sep = "")

prob_test_knn <- knn_probs(X_train_knn, Y_train_knn, X_test_knn, k = k_optim)
prob_train_knn <- knn_probs(X_train_knn, Y_train_knn, X_train_knn, k = k_optim)

auc_knn <- as.numeric(pROC::auc(pROC::roc(Y_test_knn, prob_test_knn, quiet = TRUE)))

# Llindar Youden (maximitza sensibilitat + especificitat)
roc_obj_knn <- pROC::roc(Y_test_knn, prob_test_knn, quiet = TRUE)
youden_idx  <- which.max(roc_obj_knn$sensitivities + roc_obj_knn$specificities - 1)
thresh_knn  <- roc_obj_knn$thresholds[youden_idx]

pred_knn <- as.integer(prob_test_knn >= thresh_knn)
TP_k <- sum(pred_knn == 1 & Y_test_knn == 1)
TN_k <- sum(pred_knn == 0 & Y_test_knn == 0)
FP_k <- sum(pred_knn == 1 & Y_test_knn == 0)
FN_k <- sum(pred_knn == 0 & Y_test_knn == 1)

prec_k  <- if (TP_k + FP_k > 0) TP_k / (TP_k + FP_k) else NA_real_
rec_k   <- if (TP_k + FN_k > 0) TP_k / (TP_k + FN_k) else NA_real_
spec_k  <- if (TN_k + FP_k > 0) TN_k / (TN_k + FP_k) else NA_real_
f1_k    <- if (!is.na(prec_k) & !is.na(rec_k) & (prec_k + rec_k) > 0)
             2 * prec_k * rec_k / (prec_k + rec_k) else NA_real_
balacc_k <- if (!is.na(rec_k) & !is.na(spec_k)) (rec_k + spec_k) / 2 else NA_real_

cat(sprintf("Llindar Youden: %.3f\n", thresh_knn))
cat(sprintf("AUC-ROC:       %.4f\n", auc_knn))
cat(sprintf("Precision:     %.4f\n", prec_k))
cat(sprintf("Recall:        %.4f\n", rec_k))
cat(sprintf("Especificitat: %.4f\n", spec_k))
cat(sprintf("F1:            %.4f\n", f1_k))
cat(sprintf("Balanced Acc:  %.4f\n\n", balacc_k))
cat(sprintf("TP=%d | TN=%d | FP=%d | FN=%d\n\n", TP_k, TN_k, FP_k, FN_k))

# Comparacio amb models guardats
cat("Comparacio AUC amb altres models:\n")
fitxers_met_knn <- list(
  "Logit pred" = "2. Dades/metriques_logit_pred.rds",
  "RF-A"       = "2. Dades/metriques_rf_a.rds",
  "RF-B"       = "2. Dades/metriques_rf_b.rds",
  "XGBoost"    = "2. Dades/metriques_xgb.rds",
  "CatBoost"   = "2. Dades/metriques_catboost.rds"
)
rows_comp_knn <- do.call(rbind, lapply(names(fitxers_met_knn), function(nm) {
  f <- fitxers_met_knn[[nm]]
  if (!file.exists(f)) return(NULL)
  m <- readRDS(f)
  data.frame(Model = nm, AUC_test = round(m$AUC, 4), stringsAsFactors = FALSE)
}))
rows_comp_knn <- rbind(rows_comp_knn,
  data.frame(Model = sprintf("KNN (k=%d)", k_optim), AUC_test = round(auc_knn, 4)))
print(rows_comp_knn[order(-rows_comp_knn$AUC_test), ], row.names = FALSE)
cat("\n")

# Corba ROC
roc_df_knn <- data.frame(
  spec_inv = 1 - roc_obj_knn$specificities,
  sens     = roc_obj_knn$sensitivities
)
print(
  ggplot(roc_df_knn, aes(x = spec_inv, y = sens)) +
    geom_path(color = "#4A90B8", linewidth = 1.2) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(data = data.frame(
                 spec_inv = 1 - roc_obj_knn$specificities[youden_idx],
                 sens     = roc_obj_knn$sensitivities[youden_idx]),
               color = "#E07B54", size = 4, shape = 17) +
    annotate("text", x = 0.55, y = 0.10,
             label = sprintf("AUC = %.4f", auc_knn),
             color = "#4A90B8", size = 4) +
    labs(
      title    = sprintf("Corba ROC — KNN (k=%d, test)", k_optim),
      subtitle = "Triangle: llindar Youden",
      x = "1 - Especificitat", y = "Sensibilitat"
    ) +
    theme_minimal(base_size = 13)
)

# --- 6.4 Calibracio de probabilitats (Platt scaling) ---
cat("--- 6.4 Calibracio de probabilitats ---\n\n")

# Platt scaling: glm sobre prediccions train (in-sample)
calibration_model <- glm(
  Y ~ prob,
  data   = data.frame(Y = Y_train_knn, prob = prob_train_knn),
  family = binomial
)

prob_cal_test <- as.numeric(
  predict(calibration_model, newdata = data.frame(prob = prob_test_knn), type = "response")
)

brier_raw <- mean((prob_test_knn - Y_test_knn)^2)
brier_cal <- mean((prob_cal_test - Y_test_knn)^2)

ece_raw <- ece_fn(prob_test_knn, Y_test_knn)
ece_cal <- ece_fn(prob_cal_test, Y_test_knn)

cat(sprintf("Brier  — brut: %.4f | calibrat (Platt): %.4f\n", brier_raw, brier_cal))
cat(sprintf("ECE    — brut: %.4f | calibrat (Platt): %.4f\n\n", ece_raw, ece_cal))
cat(ifelse(brier_cal < brier_raw,
           "-> Platt millora calibracio\n\n",
           "-> Calibracio sense canvi rellevant\n\n"))

# Reliability diagram (10 bins)
n_bins <- 10
breaks <- seq(0, 1, length.out = n_bins + 1)

make_rel_df <- function(probs, Y, label) {
  bins <- cut(probs, breaks = breaks, include.lowest = TRUE)
  data.frame(prob = probs, Y = Y, bin = bins) %>%
    group_by(bin) %>%
    summarise(conf = mean(prob), acc = mean(Y), n = n(), .groups = "drop") %>%
    filter(n > 0) %>%
    mutate(tipus = label)
}

rel_df <- rbind(
  make_rel_df(prob_test_knn, Y_test_knn, "Brut"),
  make_rel_df(prob_cal_test, Y_test_knn, "Platt")
)

print(
  ggplot(rel_df, aes(x = conf, y = acc, color = tipus, size = n)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
    geom_line(aes(group = tipus), linewidth = 0.8) +
    geom_point(alpha = 0.85) +
    scale_color_manual(values = c("Brut" = "#4A90B8", "Platt" = "#E07B54")) +
    scale_size_continuous(range = c(2, 6), guide = "none") +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    annotate("text", x = 0.70, y = 0.05,
             label = sprintf("ECE brut=%.3f | ECE Platt=%.3f", ece_raw, ece_cal),
             size = 3.5, color = "grey30") +
    labs(
      title    = sprintf("Reliability Diagram — KNN (k=%d, test)", k_optim),
      subtitle = sprintf("%d bins | Brier brut=%.4f | Brier Platt=%.4f",
                         n_bins, brier_raw, brier_cal),
      x = "Probabilitat predicta (bin)", y = "Proporcio observada Regular",
      color = "Prob."
    ) +
    theme_minimal(base_size = 13) + theme(legend.position = "top")
)

# --- 6.5 Interpretacio final per observacio del test ---
cat("--- 6.5 Interpretacio final (test, n=", n_test_knn, ") ---\n\n", sep = "")

thresh_cal <- thresh_knn
pred_cal   <- as.integer(prob_cal_test >= thresh_cal)

df_interp <- data.frame(
  cluster_hard      = hard_test_knn,
  u1                = round(u1_test_knn, 3),
  u2                = round(u2_test_knn, 3),
  prob_knn_cal      = round(prob_cal_test, 3),
  GRUP_ASSIST_real  = ifelse(Y_test_knn == 1, "Regular", "Irregular"),
  encert            = pred_cal == Y_test_knn
)
print(df_interp, row.names = FALSE)
cat("\n")

acc_interp <- mean(df_interp$encert)
cat(sprintf("Precisio global (test, llindar=%.3f): %.1f%%\n\n", thresh_cal, acc_interp * 100))

# Frases interpretatives (5 exemples representatius)
cat("Exemples interpretatius:\n")
exemples <- df_interp[order(-abs(df_interp$u2 - 0.5)), ][seq_len(min(5, nrow(df_interp))), ]
for (i in seq_len(nrow(exemples))) {
  r  <- exemples[i, ]
  cl <- if (r$cluster_hard == 2) 2 else 1
  u  <- if (cl == 2) r$u2 else r$u1
  cat(sprintf(
    "  Alumne perfil Cluster %d (u%d=%.2f): prob. calibrada=%.2f -> pred. %s [real: %s]\n",
    cl, cl, u, r$prob_knn_cal,
    ifelse(r$prob_knn_cal < thresh_cal, "Irregular", "Regular"),
    r$GRUP_ASSIST_real
  ))
}
cat("\n")

# Grafic de dispersio cluster vs prob calibrada
print(
  ggplot(df_interp, aes(x = u1, y = prob_knn_cal,
                        color = factor(cluster_hard), shape = factor(GRUP_ASSIST_real))) +
    geom_point(size = 2.5, alpha = 0.75) +
    geom_hline(yintercept = thresh_cal, linetype = "dashed", color = "grey40") +
    scale_color_manual(values = col_clusters,
                       labels = c("1" = "Cluster 1", "2" = "Cluster 2"),
                       name   = "Cluster") +
    scale_shape_manual(values = c("Regular" = 16, "Irregular" = 17),
                       name   = "Y real") +
    annotate("text", x = 0.05, y = thresh_cal + 0.02,
             label = sprintf("thresh=%.3f", thresh_cal),
             size = 3.5, color = "grey40") +
    labs(
      title    = sprintf("KNN probabilistic (k=%d) — test", k_optim),
      subtitle = "x=u1 (pertenença Cluster 1) | y=prob. calibrada (Platt)",
      x = "u1", y = "P(Regular) calibrada"
    ) +
    theme_minimal(base_size = 13) + theme(legend.position = "right")
)

# ================================================================
# GUARDAR MODEL COMPLET
# ================================================================

cat("--- Guardant model complet ---\n\n")

knn_model <- list(
  k       = k_optim,
  X_train = X_train_knn,
  Y_train = Y_train_knn
)

metriques_knn <- list(
  model             = sprintf("KNN_k%d", k_optim),
  k                 = k_optim,
  n_test            = n_test_knn,
  threshold         = round(thresh_knn, 3),
  AUC               = round(auc_knn, 4),
  precision         = round(prec_k, 4),
  recall            = round(rec_k, 4),
  specificity       = round(spec_k, 4),
  F1                = round(f1_k, 4),
  balanced_accuracy = round(balacc_k, 4),
  brier_raw         = round(brier_raw, 4),
  brier_cal         = round(brier_cal, 4),
  ece_raw           = round(ece_raw, 4),
  ece_cal           = round(ece_cal, 4),
  TP = TP_k, TN = TN_k, FP = FP_k, FN = FN_k
)
saveRDS(metriques_knn, "2. Dades/metriques_knn.rds")

hard_all_full <- rep(NA_integer_, nrow(dades_def))
u1_full       <- rep(NA_real_,    nrow(dades_def))
u2_full       <- rep(NA_real_,    nrow(dades_def))
hard_all_full[cc_all] <- hard_all
u1_full[cc_all]       <- U_all[, 1]
u2_full[cc_all]       <- U_all[, 2]
dades_def$cluster_hard <- hard_all_full
dades_def$u1           <- u1_full
dades_def$u2           <- u2_full

save(
  fcm_final, scale_params, c_final, m_final,
  U_all, hard_all,
  U_train, hard_train, U_test, hard_test,
  X_train, X_test,
  y_valid_train, y_valid_test,
  p_assist_train, p_assist_test,
  vars_clust,
  knn_model, k_optim, calibration_model,
  dades_def,
  file = "2. Dades/fuzzy_clustering_model_complet.RData"
)
cat("-> metriques_knn.rds i fuzzy_clustering_model_complet.RData guardats\n\n")

dev.off()
sink()
