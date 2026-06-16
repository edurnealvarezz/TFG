packages <- c("dplyr", "ggplot2", "tibble", "tidyr", "caret",
  "pROC", "PRROC", "FNN", "e1071")

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
 install.packages(pkg); library(pkg, character.only = TRUE)
  }
}
lapply(packages, install_if_missing)
rm(packages)

setwd("C:/Users/Edurne/Downloads/TFG")
source("3. Codi/Funcions models.R")

load("2. Dades/fuzzy_clustering_model.RData")
load("2. Dades/4. Dades EFA.RData")

sink("4. Outputs/16.1 Output_text_knn.txt", split = TRUE)
pdf("4. Outputs/16.2 Output_grafics_knn.pdf", width = 10, height = 8)

col_clusters <- c("1" = "#4A90B8", "2" = "#E07B54")
MIN_RECALL <- 0.60

# -----------------------------------------------------------------------
#  FUNCIONS AUXILIARS
# -----------------------------------------------------------------------

# proj_test() s'utilitza per fer prediccions per un nou alumne
proj_test <- function(X_test, centers, m) {
  n     <- nrow(X_test)
  c_num <- nrow(centers)
  exp_v <- 2 / (m - 1)
  D     <- matrix(0, nrow = n, ncol = c_num)
  for (k in seq_len(c_num)) {
    D[, k] <- sqrt(rowSums(
      (X_test - matrix(centers[k, ], nrow = n, ncol = ncol(X_test), byrow = TRUE))^2
    ))
  }
  U <- matrix(0, nrow = n, ncol = c_num)
  for (i in seq_len(n)) {
    d <- D[i, ]
    if (any(d == 0)) {
      U[i, which(d == 0)[1]] <- 1
    } else {
      for (k in seq_len(c_num)) U[i, k] <- 1 / sum((d[k] / d)^exp_v)
    }
  }
  U
}

# Fuzzy-aware KNN: cerca k veins dins de cada cluster per separat,
# combina les probabilitats ponderades per pertinença:
# prob_final = u1 * prob_c1 + u2 * prob_c2
knn_fuzzy_probs <- function(X_tr, Y_tr, hard_tr, X_te, U_te, k) {
  get_prob_cluster <- function(cl_id) {
 idx_c <- which(hard_tr == cl_id)
 if (length(idx_c) < 1) return(rep(0.5, nrow(X_te)))
 k_eff <- min(k, length(idx_c))
 Xc <- X_tr[idx_c, , drop = FALSE]
 Yc <- factor(Y_tr[idx_c], levels = c("0", "1"))
 if (length(unique(Y_tr[idx_c])) < 2) {
 return(rep(as.numeric(as.character(Y_tr[idx_c[1]])), nrow(X_te)))
 }
 pred <- FNN::knn(train = Xc, test = X_te, cl = Yc, k = k_eff, prob = TRUE)
 p <- attr(pred, "prob")
 ifelse(as.character(pred) == "1", p, 1 - p)
  }
  prob_c1 <- get_prob_cluster(1)
  prob_c2 <- get_prob_cluster(2)
  U_te[, 1] * prob_c1 + U_te[, 2] * prob_c2
}

ece_fn <- function(probs, Y, n_bins = 10) {
  breaks <- seq(0, 1, length.out = n_bins + 1)
  bins <- cut(probs, breaks = breaks, include.lowest = TRUE)
  df_b <- data.frame(prob = probs, Y = Y, bin = bins) %>%
 group_by(bin) %>%
 summarise(n = n(), conf = mean(prob), acc = mean(Y), .groups = "drop") %>%
 filter(n > 0)
  sum(df_b$n * abs(df_b$conf - df_b$acc)) / length(Y)
}

calcular_metriques_knn <- function(prob, Y_vec, nom_model,
 auc_cv_mean = NA, auc_cv_sd = NA,
 thresh_override = NULL) {
  roc_obj <- pROC::roc(Y_vec, prob, quiet = TRUE)
  auc_val <- as.numeric(pROC::auc(roc_obj))
  if (!is.null(thresh_override)) {
 thresh <- thresh_override
  } else {
 cr <- pROC::coords(roc_obj, "best",
 ret = c("threshold", "sensitivity", "specificity"),
 best.method = "youden")
 thresh <- cr$threshold[1]
  }
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
 AUC_cv_mean = round(auc_cv_mean, 4),
 AUC_cv_sd = round(auc_cv_sd, 4),
 accuracy = round(accuracy, 4),
 precision = round(precision, 4),
 recall = round(recall, 4),
 specificity = round(specificity, 4),
 F1 = round(f1, 4),
 balanced_accuracy = round(balanced_acc, 4),
 TP = TP, TN = TN, FP = FP, FN = FN
  )
}

mostrar_metriques_knn <- function(met, titol = NULL) {
  if (is.null(titol)) titol <- met$model
  cat(sprintf("\n--- Metriques: %s ---\n", titol))
  cat(sprintf("n = %d | Llindar PR (OOF) = %.3f\n", met$n_test, met$threshold))
  if (!is.na(met$AUC_cv_mean))
 cat(sprintf("AUC (CV 5-fold):  %.4f +/- %.4f\n", met$AUC_cv_mean, met$AUC_cv_sd))
  cat(sprintf("AUC:  %.4f\n", met$AUC))
  cat(sprintf("Accuracy: %.4f\n", met$accuracy))
  cat(sprintf("Precision (PPV):  %.4f  <- metrica prioritaria\n", met$precision))
  cat(sprintf("Recall (Sens): %.4f\n", met$recall))
  cat(sprintf("Especificitat: %.4f\n", met$specificity))
  cat(sprintf("F1: %.4f\n", met$F1))
  cat(sprintf("Balanced Acc:  %.4f\n\n", met$balanced_accuracy))
  cat("Matriu de confusio:\n")
  cm <- matrix(c(met$TN, met$FN, met$FP, met$TP), nrow = 2,
 dimnames = list(Observat = c("Irregular(0)", "Regular(1)"),
 Predit = c("Irregular(0)", "Regular(1)")))
  print(cm)
}

#### ============================================================ ####
####                        0. PREPARACIO                         ####
#### ============================================================ ####

cat("================= 0. PREPARACIO =================\n\n")

df_all <- dades_def %>%
  transmute(
 EDAT  = as.numeric(EDAT),
 DESPL = as.numeric(DESPL),
 N_ASSIG  = as.numeric(N_ASSIG),
 NOTA_num = as.numeric(NOTA),
 T_AVAL_num  = as.integer(T_AVAL == "Continuada"),
 CURS_1R  = as.integer(CURS_1R),
 GENERE_Home = as.integer(GENERE == "Home"),
 DOBLE_GRAU_EST = as.integer(DOBLE_GRAU_EST),
 TREB_INTENS = as.integer(TREB_INTENS),
 IA_HABIT = as.integer(IA_HABIT),
 IA_COMPR = as.integer(IA_COMPR),
 IA_REND  = as.integer(IA_REND),
 IA_PDFS  = as.integer(IA_PDFS),
 IA_SUBST = as.integer(IA_SUBST),
 IA_ATENC = as.integer(IA_ATENC),
 IA_CONF  = as.integer(IA_CONF),
 GRUP_ASSIST = GRUP_ASSIST,
 P_ASSIST = as.numeric(P_ASSIST)
  )

cc_all <- complete.cases(df_all[, vars_clust])
cat(sprintf("Obs completes per KNN: %d / %d\n\n", sum(cc_all), nrow(df_all)))

# Assignació directa des de fuzzy_clustering_model.RData.
# X_train / X_test ja estan escalats amb scale_params (script 14).
# U_train / U_test / hard_train / hard_test ja estan calculats; evitem
# recalcular amb proj_test() sobre tots els individus.
X_knn_tr <- X_train
X_knn_te <- X_test
U_train_knn <- U_train
U_test_knn  <- U_test
hard_train_knn <- hard_train
hard_test_knn  <- hard_test
Y_train_knn <- as.integer(y_valid_train == "Regular (≥80%)")
Y_test_knn  <- as.integer(y_valid_test  == "Regular (≥80%)")

stopifnot(
  nrow(X_knn_tr) == nrow(U_train_knn),
  nrow(X_knn_te) == nrow(U_test_knn),
  length(Y_train_knn) == nrow(X_knn_tr),
  length(Y_test_knn)  == nrow(X_knn_te)
)

# Reconstrueix U_all / hard_all des de train+test recuperant els índexs
# originals (mateixa seed i partició que el script 14). Necessari per
# anotar dades_def a la secció 7; no crida proj_test().
n_all <- nrow(U_train) + nrow(U_test)
set.seed(1234)
idx_knn  <- caret::createDataPartition(df_all$GRUP_ASSIST[cc_all], p = 0.80, list = FALSE)
U_all <- matrix(0, nrow = n_all, ncol = 2)
hard_all <- integer(n_all)
U_all[idx_knn, ] <- U_train
U_all[-idx_knn, ]  <- U_test
hard_all[idx_knn]  <- hard_train
hard_all[-idx_knn] <- hard_test

n_train_knn <- nrow(X_knn_tr)
n_test_knn  <- nrow(X_knn_te)

cat(sprintf("Train: %d obs | Test: %d obs\n", n_train_knn, n_test_knn))
cat(sprintf("Train Y: Regular=%d (%.1f%%) | Irregular=%d (%.1f%%)\n",
 sum(Y_train_knn == 1), mean(Y_train_knn) * 100,
 sum(Y_train_knn == 0), mean(1 - Y_train_knn) * 100))
cat(sprintf("Train Cluster 1: %d obs | Cluster 2: %d obs\n\n",
 sum(hard_train_knn == 1), sum(hard_train_knn == 2)))

#### ============================================================ ####
####                    1. CV 5-FOLD — SELECCIO DE k              ####
#### ============================================================ ####

cat("============ 1. CV 5-FOLD — SELECCIO DE k =================\n\n")

k_vals <- c(3, 5, 7, 9, 11)

set.seed(1234)
folds_knn <- caret::createFolds(Y_train_knn, k = 5, list = TRUE)

cv_results <- do.call(rbind, lapply(k_vals, function(k_val) {
  auc_folds <- sapply(folds_knn, function(val_idx) {
    tr_idx  <- setdiff(seq_len(n_train_knn), val_idx)
    U_val <- proj_test(X_knn_tr[val_idx, ], fcm_final$centers, m_final)
    probs_v <- knn_fuzzy_probs(
      X_tr = X_knn_tr[tr_idx, ],
      Y_tr = Y_train_knn[tr_idx],
      hard_tr = hard_train_knn[tr_idx],
      X_te = X_knn_tr[val_idx, ],
      U_te = U_val,
      k = k_val
    )
    if (length(unique(Y_train_knn[val_idx])) < 2) return(NA_real_)
      as.numeric(pROC::auc(pROC::roc(Y_train_knn[val_idx], probs_v, quiet = TRUE)))
   })
  data.frame(k = k_val,
 AUC_CV = round(mean(auc_folds, na.rm = TRUE), 4),
 SD_AUC = round(sd(auc_folds, na.rm = TRUE), 4))
}))

print(cv_results, row.names = FALSE)

k_optim <- cv_results$k[which.max(cv_results$AUC_CV)]
auc_cv_opt <- cv_results$AUC_CV[cv_results$k == k_optim]
sd_cv_opt  <- cv_results$SD_AUC[cv_results$k == k_optim]
cat(sprintf("\n>>> k optim: k=%d  (AUC-CV = %.4f +/- %.4f)\n\n",
 k_optim, auc_cv_opt, sd_cv_opt))

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
  labs(title = "Seleccio de k — KNN fuzzy-aware (CV 5-fold)",
  subtitle = "AUC-ROC mig +/- 1 SD | cerca per cluster separat", 
  x = "k (veins per cluster)", y = "AUC-ROC (CV)") +
  theme_minimal(base_size = 13)
)

#### ============================================================ ####
####                2. OOF — CALIBRACIO PLATT + LLINDAR PR        ####
#### ============================================================ ####

cat("============ 2. OOF — CALIBRACIO PLATT + LLINDAR PR =================\n\n")

# Prediccions OOF amb k optim (mateixos folds que el CV)
oof_probs_raw <- numeric(n_train_knn)
for (val_idx in folds_knn) {
  tr_idx <- setdiff(seq_len(n_train_knn), val_idx)
  U_val  <- proj_test(X_knn_tr[val_idx, ], fcm_final$centers, m_final)
  oof_probs_raw[val_idx] <- knn_fuzzy_probs(
    X_tr = X_knn_tr[tr_idx, ],
    Y_tr = Y_train_knn[tr_idx],
    hard_tr = hard_train_knn[tr_idx],
    X_te = X_knn_tr[val_idx, ],
    U_te = U_val,
    k = k_optim
  )
}

# Platt scaling sobre prediccions OOF (no in-sample)
platt_model <- glm(Y ~ prob,
 data = data.frame(Y = Y_train_knn, prob = oof_probs_raw),
 family = binomial)

oof_probs_cal <- as.numeric(
  predict(platt_model, newdata = data.frame(prob = oof_probs_raw), type = "response")
)

# Llindar PR sobre OOF calibrades
pr_oof  <- seleccionar_llindar_pr(oof_probs_cal, Y_train_knn, MIN_RECALL)
thresh_knn <- pr_oof$threshold

cat(sprintf("AUC OOF (raw): %.4f\n",
 as.numeric(pROC::auc(pROC::roc(Y_train_knn, oof_probs_raw, quiet = TRUE)))))
cat(sprintf("AUC OOF (calibrat): %.4f\n",
 as.numeric(pROC::auc(pROC::roc(Y_train_knn, oof_probs_cal, quiet = TRUE)))))
cat(sprintf("Llindar PR (OOF calibrat, MIN_RECALL=%.2f): %.3f\n", MIN_RECALL, thresh_knn))
cat(sprintf("  Precision OOF: %.4f | Recall OOF: %.4f | AUPRC: %.4f\n",
 pr_oof$precision, pr_oof$recall, pr_oof$auprc))
if (!pr_oof$recall_ok)
  cat(sprintf("  AVIS: recall OOF (%.4f) < MIN_RECALL -> fallback Youden\n", pr_oof$recall))
cat("\n")

#### ============================================================ ####
####                   3. MODEL FINAL                             ####
#### ============================================================ ####

cat("================= 3. MODEL FINAL — PREDICCIONS TEST =================\n\n")

# Probabilitats test (raw + calibrades)
prob_test_raw <- knn_fuzzy_probs(
  X_tr = X_knn_tr,
  Y_tr = Y_train_knn,
  hard_tr = hard_train_knn,
  X_te = X_knn_te,
  U_te = U_test_knn,
  k = k_optim
)
prob_test_cal <- as.numeric(
  predict(platt_model, newdata = data.frame(prob = prob_test_raw), type = "response")
)

# Probabilitats train in-sample (per comparar overfitting)
prob_train_raw <- knn_fuzzy_probs(
  X_tr = X_knn_tr,
  Y_tr = Y_train_knn,
  hard_tr = hard_train_knn,
  X_te = X_knn_tr,
  U_te = U_train_knn,
  k = k_optim
)
prob_train_cal <- as.numeric(
  predict(platt_model, newdata = data.frame(prob = prob_train_raw), type = "response")
)

auc_train <- as.numeric(pROC::auc(pROC::roc(Y_train_knn, prob_train_cal, quiet = TRUE)))
auc_test  <- as.numeric(pROC::auc(pROC::roc(Y_test_knn,  prob_test_cal,  quiet = TRUE)))

cat(sprintf("AUC train (in-sample):  %.4f  [nota: optimista per KNN]\n", auc_train))
cat(sprintf("AUC CV 5-fold (OOF): %.4f +/- %.4f\n", auc_cv_opt, sd_cv_opt))
cat(sprintf("AUC test: %.4f\n\n", auc_test))

#### ============================================================ ####
####                      4. METRIQUES                            ####
#### ============================================================ ####

cat("================= 4. METRIQUES =================\n")

met_knn_train <- calcular_metriques_knn(
  prob_train_cal, Y_train_knn,
  nom_model = sprintf("KNN fuzzy train (k=%d, in-sample)", k_optim),
  auc_cv_mean = auc_cv_opt,
  auc_cv_sd = sd_cv_opt,
  thresh_override = thresh_knn
)
mostrar_metriques_knn(met_knn_train)
cat("\n")

met_knn_test <- calcular_metriques_knn(
  prob_test_cal, Y_test_knn,
  nom_model = sprintf("KNN fuzzy (k=%d)", k_optim),
  auc_cv_mean = auc_cv_opt,
  auc_cv_sd = sd_cv_opt,
  thresh_override = thresh_knn
)
mostrar_metriques_knn(met_knn_test)
cat("\n")

cat("--- Diferencial train vs test (sobreajust) ---\n")
cat(sprintf("  Delta AUC: %.4f\n",  met_knn_train$AUC - met_knn_test$AUC))
cat(sprintf("  Delta Precision: %.4f\n",  met_knn_train$precision - met_knn_test$precision))
cat(sprintf("  Delta Recall: %.4f\n\n", met_knn_train$recall - met_knn_test$recall))

#### ============================================================ ####
####                       5. GRAFICS                             ####
#### ============================================================ ####

cat("================= 5. GRÀFICS =================\n\n")

# --- 5.1 Corba ROC (test) ---
roc_obj_knn <- pROC::roc(Y_test_knn, prob_test_cal, quiet = TRUE)
roc_df_knn  <- data.frame(
  spec_inv = 1 - roc_obj_knn$specificities,
  sens  = roc_obj_knn$sensitivities
)
youden_idx <- which.max(roc_obj_knn$sensitivities + roc_obj_knn$specificities - 1)

print(
  ggplot(roc_df_knn, aes(x = spec_inv, y = sens)) +
  geom_path(color = "#4A90B8", linewidth = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(
    data = data.frame(
      spec_inv = 1 - roc_obj_knn$specificities[youden_idx],
      sens  = roc_obj_knn$sensitivities[youden_idx]),
      color = "#E07B54", size = 4, shape = 17) +
      annotate("text", x = 0.55, y = 0.10,
      label = sprintf("AUC = %.4f", auc_test),
      color = "#4A90B8", size = 4) +
      labs(title = sprintf("Corba ROC — KNN fuzzy (k=%d, test)", k_optim),
      subtitle = "Triangle: llindar Youden | probabilitats calibrades (Platt)",
      x = "1 - Especificitat", y = "Sensibilitat") +
      theme_minimal(base_size = 13)
)

# --- 5.2 Corba Precisio-Recall (test) ---
pr_test <- seleccionar_llindar_pr(prob_test_cal, Y_test_knn, MIN_RECALL)

print(
  ggplot(pr_test$pr_curve, aes(x = recall, y = precision)) +
  geom_path(color = "#4A90B8", linewidth = 1) +
  geom_vline(xintercept = MIN_RECALL, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_point(
    data = data.frame(recall = met_knn_test$recall,
    precision = met_knn_test$precision),
    color = "#E07B54", size = 3, shape = 17) +
    labs(title = sprintf("Corba Precisio-Recall — KNN fuzzy (k=%d, test)", k_optim),
    subtitle = sprintf("AUPRC = %.4f | Llindar OOF = %.3f", pr_test$auprc, thresh_knn),
    x = "Recall", y = "Precisio (PPV)") +
    theme_minimal(base_size = 13)
)

# --- 5.3 Matriu de confusio ---
print(grafic_cm(met_knn_test, sprintf("KNN fuzzy (k=%d, test)", k_optim)))

# --- 5.4 Reliability diagram (calibracio Platt) ---
brier_raw <- mean((prob_test_raw - Y_test_knn)^2)
brier_cal <- mean((prob_test_cal - Y_test_knn)^2)
ece_raw <- ece_fn(prob_test_raw, Y_test_knn)
ece_cal <- ece_fn(prob_test_cal, Y_test_knn)

cat(sprintf("Brier  — brut: %.4f | calibrat (Platt OOF): %.4f\n", brier_raw, brier_cal))
cat(sprintf("ECE — brut: %.4f | calibrat (Platt OOF): %.4f\n\n", ece_raw, ece_cal))

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
  make_rel_df(prob_test_raw, Y_test_knn, "Brut"),
  make_rel_df(prob_test_cal, Y_test_knn, "Platt")
)

print(
  ggplot(rel_df, aes(x = conf, y = acc, color = tipus, size = n)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(aes(group = tipus), linewidth = 0.8) +
  geom_point(alpha = 0.85) +
  scale_color_manual(values = c("Brut" = "#4A90B8", "Platt" = "#E07B54")) +
  scale_size_continuous(range = c(2, 6), guide = "none") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  annotate("text", x = 0.65, y = 0.05,
  label = sprintf("ECE brut=%.3f | ECE Platt=%.3f", ece_raw, ece_cal),
  size = 3.5, color = "grey30") +
  labs(title = sprintf("Reliability Diagram — KNN fuzzy (k=%d, test)", k_optim),
  subtitle = sprintf("%d bins | Brier brut=%.4f | Brier Platt=%.4f",
  n_bins, brier_raw, brier_cal),
  x = "Probabilitat predicta (bin)", y = "Proporcio observada Regular",
  color = "Prob.") +
  theme_minimal(base_size = 13) + theme(legend.position = "top")
)

# --- 5.5 Dispersio: u1 vs prob_cal, per cluster i per resultat ---
df_scatter <- data.frame(
  u1 = U_test_knn[, 1],
  u2 = U_test_knn[, 2],
  prob_cal = prob_test_cal,
  cluster_hard = factor(hard_test_knn),
  GRUP_real = ifelse(Y_test_knn == 1, "Regular", "Irregular")
)

print(
  ggplot(df_scatter, aes(x = u1, y = prob_cal,
  color = cluster_hard, shape = GRUP_real)) +
 geom_point(size = 2.5, alpha = 0.75) +
 geom_hline(yintercept = thresh_knn, linetype = "dashed", color = "grey40") +
 scale_color_manual(values = col_clusters,
  labels = c("1" = "Cluster 1", "2" = "Cluster 2"),
  name = "Cluster") +
 scale_shape_manual(values = c("Regular" = 16, "Irregular" = 17), name = "Y real") +
 annotate("text", x = 0.05, y = thresh_knn + 0.025,
 label = sprintf("thresh=%.3f", thresh_knn),
 size = 3.5, color = "grey40", hjust = 0) +
 labs(title = sprintf("KNN fuzzy-aware (k=%d) — test", k_optim),
 subtitle = "x = u1 (pertinenca Cluster 1) | y = P(Regular) calibrada (Platt)",
 x = "u1", y = "P(Regular) calibrada") +
 theme_minimal(base_size = 13) + theme(legend.position = "right")
)

# --- 5.6 Densitat P(Regular) per cluster ---
print(
  ggplot(df_scatter, aes(x = prob_cal, fill = cluster_hard)) +
 geom_density(alpha = 0.50) +
 geom_vline(xintercept = thresh_knn, linetype = "dashed", color = "grey30") +
 scale_fill_manual(values = col_clusters, name = "Cluster") +
 labs(title = sprintf("Distribucio P(Regular) calibrada per cluster — KNN (k=%d)",
 k_optim),
 subtitle = "Test set | linia puntejada: llindar PR (OOF)",
 x = "P(Regular) calibrada", y = "Densitat") +
 theme_minimal(base_size = 13)
)

#### ============================================================ ####
####                  6. COMPARACIO AMB ALTRES MODELS             ####
#### ============================================================ ####

cat("================= 6. COMPARACIO AMB ALTRES MODELS =================\n\n")

fitxers_met <- list(
  "Logit pred" = "4. Outputs/Metriques i models/metriques_logit_pred.rds",
  "RF-A"       = "4. Outputs/Metriques i models/metriques_rf_a.rds",
  "RF-B"       = "4. Outputs/Metriques i models/metriques_rf_b.rds",
  "XGBoost"    = "4. Outputs/Metriques i models/metriques_xgb.rds",
  "CatBoost"   = "4. Outputs/Metriques i models/metriques_catboost.rds",
  "SVM"        = "4. Outputs/Metriques i models/metriques_svm.rds"
)

rows_comp <- do.call(rbind, lapply(names(fitxers_met), function(nm) {
  f <- fitxers_met[[nm]]
  if (!file.exists(f)) return(NULL)
  tryCatch(extreure_fila(readRDS(f)), error = function(e) NULL)
}))

rows_comp <- rbind(rows_comp, extreure_fila(met_knn_test))
rows_comp <- rows_comp[order(-rows_comp$AUC_test), ]

print(rows_comp, row.names = FALSE)
cat("\n")

#### ============================================================ ####
####                  7. GUARDAR ####
#### ============================================================ ####

cat("================= 7. GUARDAR =================\n\n")

metriques_knn <- met_knn_test
dir.create("4. Outputs/Metriques i models", showWarnings = FALSE, recursive = TRUE)
saveRDS(metriques_knn, "4. Outputs/Metriques i models/metriques_knn.rds")
cat("-> metriques_knn.rds guardat\n")

knn_model <- list(
  k = k_optim,
  X_train = X_knn_tr,
  Y_train = Y_train_knn,
  hard_train = hard_train_knn,
  U_train = U_train_knn,
  platt_model = platt_model,
  thresh_pr = thresh_knn
)

cat("================= 7b. FUNCIO PREDICT_NOU_ALUMNE =================\n\n")

# Captura les dependències en l'entorn local perquè la funció funcioni
# de forma autònoma un cop desada al knn_model (sense necessitat de
# recarregar tot l'entorn de treball).
predict_nou_alumne <- local({
  .scale_params <- scale_params
  .fcm_centers  <- fcm_final$centers
  .fcm_m  <- m_final
  .knn_model <- knn_model # X_train, Y_train, hard_train, platt, thresh
  .proj_test <- proj_test
  .knn_fuzzy_probs <- knn_fuzzy_probs

  function(nou_alumne) {
    # nou_alumne: named numeric vector amb les mateixes variables que vars_clust

    # 1. Escalar amb els mateixos parametres del train
    x_raw <- as.numeric(nou_alumne[names(.scale_params$mean)])
    x_sc  <- (x_raw - .scale_params$mean) / .scale_params$sd
    X_mat <- matrix(x_sc, nrow = 1,
                    dimnames = list(NULL, names(.scale_params$mean)))

    # 2. Calcular pertinences fuzzy
    U_mat <- .proj_test(X_mat, .fcm_centers, .fcm_m)
    u1    <- U_mat[1, 1]
    u2    <- U_mat[1, 2]
    cluster_dominant <- ifelse(u1 >= u2, 1L, 2L)

    # 3. Probabilitat bruta via KNN fuzzy-aware
    prob_raw <- .knn_fuzzy_probs(
      X_tr    = .knn_model$X_train,
      Y_tr    = .knn_model$Y_train,
      hard_tr = .knn_model$hard_train,
      X_te    = X_mat,
      U_te    = U_mat,
      k       = .knn_model$k
    )

    # 4. Calibracio Platt
    prob_cal <- as.numeric(predict(
      .knn_model$platt_model,
      newdata = data.frame(prob = prob_raw),
      type    = "response"
    ))

    # 5. Classificacio
    prediccio <- ifelse(prob_cal >= .knn_model$thresh_pr, "Regular", "Irregular")

    # 6. Indexs dels k veins mes propers dins de cada cluster
    # Retorna indexs sobre X_train (conjunt complet de train)
    get_veins_cluster <- function(cl_id) {
      idx_c <- which(.knn_model$hard_train == cl_id)
      if (length(idx_c) < 1) return(integer(0))
      k_eff <- min(.knn_model$k, length(idx_c))
      Xc    <- .knn_model$X_train[idx_c, , drop = FALSE]
      nn    <- FNN::knn.index(data = Xc, query = X_mat, k = k_eff)
      idx_c[nn[1, ]]
    }
    veins_c1 <- get_veins_cluster(1)
    veins_c2 <- get_veins_cluster(2)

    list(
      u1               = round(u1, 4),
      u2               = round(u2, 4),
      cluster_dominant = cluster_dominant,
      prob_regular     = round(prob_cal, 4),
      prediccio        = prediccio,
      veins_cluster1   = veins_c1,
      veins_cluster2   = veins_c2
    )
  }
})

# Adjuntar la funció al model per poder-la carregar amb el .RData
knn_model$predict_nou_alumne <- predict_nou_alumne

# --- Exemple d'ús: alumne fictici amb les mitjanes del train ---
alumne_fictici <- setNames(
  colMeans(X_knn_tr) * scale_params$sd + scale_params$mean,
  names(scale_params$mean)
)

cat("Alumne fictici (mitjanes del train):\n")
print(round(alumne_fictici, 3))
cat("\n")

resultat_fictici <- predict_nou_alumne(alumne_fictici)
cat("Resultat de predict_nou_alumne():\n")
cat(sprintf("  u1 (Cluster 1):   %.4f\n", resultat_fictici$u1))
cat(sprintf("  u2 (Cluster 2):   %.4f\n", resultat_fictici$u2))
cat(sprintf("  Cluster dominant: %d\n",   resultat_fictici$cluster_dominant))
cat(sprintf("  P(Regular) cal.:  %.4f\n", resultat_fictici$prob_regular))
cat(sprintf("  Prediccio:        %s\n",   resultat_fictici$prediccio))
cat(sprintf("  Veins cluster 1 (idx train): %s\n",
            paste(resultat_fictici$veins_cluster1, collapse = ", ")))
cat(sprintf("  Veins cluster 2 (idx train): %s\n\n",
            paste(resultat_fictici$veins_cluster2, collapse = ", ")))

hard_all_full <- rep(NA_integer_, nrow(dades_def))
u1_full <- rep(NA_real_, nrow(dades_def))
u2_full <- rep(NA_real_, nrow(dades_def))
hard_all_full[cc_all] <- hard_all
u1_full[cc_all] <- U_all[, 1]
u2_full[cc_all] <- U_all[, 2]
dades_def$cluster_hard <- hard_all_full
dades_def$u1  <- u1_full
dades_def$u2  <- u2_full

save(
  fcm_final, scale_params, c_final, m_final,
  U_all, hard_all,
  U_train, hard_train, U_test, hard_test,
  X_train, X_test,
  y_valid_train, y_valid_test,
  p_assist_train, p_assist_test,
  vars_clust,
  knn_model, k_optim, platt_model,
  predict_nou_alumne,
  metriques_knn,
  dades_def,
  file = "2. Dades/fuzzy_clustering_model_complet.RData"
)

dev.off()
sink()
