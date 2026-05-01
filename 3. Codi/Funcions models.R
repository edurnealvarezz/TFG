# -------------------------------------
# Funció de mètriques de classificació
# -------------------------------------

# Helper: selecciona llindar des de la corba PR (PRROC)
# Maximitza precisió amb recall >= min_recall; fallback Youden si no assolible
seleccionar_llindar_pr <- function(prob, Y_vec, min_recall = 0.40) {
  pr_obj <- pr.curve(
    scores.class0 = prob[Y_vec == 1],
    scores.class1 = prob[Y_vec == 0],
    curve = TRUE
  )
  pr_df <- as.data.frame(pr_obj$curve)
  names(pr_df) <- c("recall", "precision", "threshold")
  pr_df <- pr_df[!is.na(pr_df$precision) & !is.na(pr_df$recall), ]
  pr_ok <- pr_df[pr_df$recall >= min_recall, ]

  if (nrow(pr_ok) > 0) {
    best_pr <- pr_ok[which.max(pr_ok$precision), ]
    list(
      threshold = best_pr$threshold,
      precision = best_pr$precision,
      recall    = best_pr$recall,
      auprc     = pr_obj$auc.integral,
      recall_ok = TRUE,
      pr_curve  = pr_df
    )
  } else {
    roc_fb <- roc(Y_vec, prob, quiet = TRUE)
    youden  <- coords(roc_fb, "best",
                      ret = c("threshold", "sensitivity", "ppv"),
                      best.method = "youden")
    list(
      threshold = youden$threshold[1],
      precision = youden$ppv[1],
      recall    = youden$sensitivity[1],
      auprc     = pr_obj$auc.integral,
      recall_ok = FALSE,
      pr_curve  = pr_df
    )
  }
}

calcular_metriques <- function(model_glm, dades_test_df, nom_model,
                               auc_cv_mean = NA, auc_cv_sd = NA,
                               thresh_override = NULL) {
  Y_test <- dades_test_df$Y
  prob <- predict(model_glm, newdata = dades_test_df, type = "response")

  roc_obj <- roc(Y_test, prob, quiet = TRUE)
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
  TP <- sum(pred == 1 & Y_test == 1)
  TN <- sum(pred == 0 & Y_test == 0)
  FP <- sum(pred == 1 & Y_test == 0)
  FN <- sum(pred == 0 & Y_test == 1)

  accuracy <- (TP + TN) / (TP + TN + FP + FN)
  precision <- ifelse(TP + FP > 0, TP / (TP + FP), NA)
  recall <- ifelse(TP + FN > 0, TP / (TP + FN), NA)
  specificity <- ifelse(TN + FP > 0, TN / (TN + FP), NA)
  f1 <- ifelse(!is.na(precision) & !is.na(recall) & (precision + recall) > 0,
               2 * precision * recall / (precision + recall), NA)
  balanced_acc <- (recall + specificity) / 2

  list(
    model = nom_model,
    n_test = length(Y_test),
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

# ----------------------------------------------------------------
# Funció per imprimir mètriques i graficar matriu de confusió
# ----------------------------------------------------------------
mostrar_metriques <- function(met, titol = NULL) {
  if (is.null(titol)) titol <- met$model
  cat(sprintf("\n--- Mètriques: %s ---\n", titol))
  cat(sprintf("n test = %d | Llindar PR = %.3f\n", met$n_test, met$threshold))
  if (!is.na(met$AUC_cv_mean)) {
    cat(sprintf("AUC (10-fold CV train): %.4f ± %.4f\n", met$AUC_cv_mean, met$AUC_cv_sd))
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
         subtitle = sprintf("Llindar PR = %.3f", met$threshold),
         x = "Valor Predit", y = "Valor Observat") +
    theme_minimal(base_size = 13) +
    theme(panel.grid = element_blank())
  print(p_cm)
}


# --------------------------
# Funcions de metriques RF
# --------------------------
calcular_metriques_rf <- function(prob, Y_vec, nom_model, oob_error = NA,
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
  cat(sprintf("\n--- Metriques: %s ---\n", titol))
  cat(sprintf("n = %d | Llindar PR = %.3f\n", met$n_test, met$threshold))
  if (!is.na(met$OOB_error))
    cat(sprintf("OOB error: %.4f\n", met$OOB_error))
  cat(sprintf("AUC:               %.4f\n", met$AUC))
  cat(sprintf("Accuracy:          %.4f\n", met$accuracy))
  cat(sprintf("Precision (PPV):   %.4f\n", met$precision))
  cat(sprintf("Recall (Sens):     %.4f\n", met$recall))
  cat(sprintf("Especificitat:     %.4f\n", met$specificity))
  cat(sprintf("F1:                %.4f\n", met$F1))
  cat(sprintf("Balanced Acc:      %.4f\n\n", met$balanced_accuracy))
  cm <- matrix(c(met$TN, met$FN, met$FP, met$TP), nrow = 2,
               dimnames = list(Observat = c("Irreg(0)", "Reg(1)"),
                               Predit = c("Irreg(0)", "Reg(1)")))
  print(cm)
}

grafic_cm <- function(met, titol = NULL) {
  if (is.null(titol)) titol <- met$model
  df_cm <- data.frame(
    Observat = factor(c("Irregular", "Irregular", "Regular", "Regular"),
                      levels = c("Regular", "Irregular")),
    Predit = factor(c("Irregular", "Regular", "Irregular", "Regular"),
                    levels = c("Irregular", "Regular")),
    n = c(met$TN, met$FP, met$FN, met$TP),
    etiq = c("TN", "FP", "FN", "TP")
  )
  ggplot(df_cm, aes(x = Predit, y = Observat, fill = n)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = paste0(etiq, "\n", n)), size = 5, fontface = "bold") +
    scale_fill_gradient(low = "#EBF5FB", high = "#2471A3", guide = "none") +
    labs(title = sprintf("Matriu de confusio — %s", titol),
         subtitle = sprintf("Llindar PR = %.3f", met$threshold),
         x = "Valor Predit", y = "Valor Observat") +
    theme_minimal(base_size = 13) +
    theme(panel.grid = element_blank())
}

# ------------------------------
# Funcions de metriques XG Boost
# ------------------------------

calcular_metriques_xgb <- function(prob, Y_vec, nom_model,
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

mostrar_metriques_xgb <- function(met, titol = NULL) {
  if (is.null(titol)) titol <- met$model
  cat(sprintf("\n--- Mètriques: %s ---\n", titol))
  cat(sprintf("n = %d | Llindar PR = %.3f\n", met$n_test, met$threshold))
  if (!is.na(met$AUC_cv_mean)) {
    cat(sprintf("AUC (val CV):           %.4f ± %.4f\n",
                met$AUC_cv_mean, met$AUC_cv_sd))
  }
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
         subtitle = sprintf("Llindar PR = %.3f", met$threshold),
         x = "Valor Predit", y = "Valor Observat") +
    theme_minimal(base_size = 13) +
    theme(panel.grid = element_blank())
  print(p_cm)
}

extreure_fila <- function(m) {
  cv_info <- tryCatch({
    if (!is.null(m$AUC_cv_mean) && length(m$AUC_cv_mean) == 1 && !is.na(m$AUC_cv_mean))
      sprintf("%.4f ± %.4f", m$AUC_cv_mean, m$AUC_cv_sd)
    else if (!is.null(m$OOB_error) && length(m$OOB_error) == 1 && !is.na(m$OOB_error))
      sprintf("OOB err = %.4f", m$OOB_error)
    else "—"
  }, error = function(e) "—")

  data.frame(
    Model = m$model,
    AUC_CV = cv_info,
    AUC_test = round(m$AUC, 4),
    Accuracy = round(m$accuracy, 4),
    Precision = round(m$precision, 4),
    Recall = round(m$recall, 4),
    F1 = round(m$F1, 4),
    Balanced_Acc = round(m$balanced_accuracy, 4),
    stringsAsFactors = FALSE
  )
}

# ------------------------------
# Funcions de metriques Cat Boost
# ------------------------------

calcular_metriques_cat <- function(prob, Y_vec, nom_model,
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
  TP <- sum(pred == 1 & Y_vec == 1)
  TN <- sum(pred == 0 & Y_vec == 0)
  FP <- sum(pred == 1 & Y_vec == 0)
  FN <- sum(pred == 0 & Y_vec == 1)

  accuracy <- (TP + TN) / (TP + TN + FP + FN)
  precision <- ifelse(TP + FP > 0, TP / (TP + FP), NA)
  recall <- ifelse(TP + FN > 0, TP / (TP + FN), NA)
  specif <- ifelse(TN + FP > 0, TN / (TN + FP), NA)
  f1 <- ifelse(!is.na(precision) & !is.na(recall) & (precision + recall) > 0,
               2 * precision * recall / (precision + recall), NA)
  balanced_acc <- (recall + specif) / 2

  list(
    model = nom_model,
    n_test = length(Y_vec),
    threshold = round(thresh, 3),
    AUC = round(auc_val, 4),
    AUC_cv_mean = NA,
    AUC_cv_sd = NA,
    accuracy = round(accuracy, 4),
    precision = round(precision, 4),
    recall = round(recall, 4),
    specificity = round(specif, 4),
    F1 = round(f1, 4),
    balanced_accuracy = round(balanced_acc, 4),
    TP = TP, TN = TN, FP = FP, FN = FN
  )
}

mostrar_metriques_cat <- function(met, titol = NULL) {
  if (is.null(titol)) titol <- met$model
  cat(sprintf("\n--- Metriques: %s ---\n", titol))
  cat(sprintf("n = %d | Llindar = %.3f\n", met$n_test, met$threshold))
  cat(sprintf("AUC:                    %.4f\n", met$AUC))
  cat(sprintf("Accuracy:               %.4f\n", met$accuracy))
  cat(sprintf("Precision (PPV):        %.4f  ← metrica prioritaria\n", met$precision))
  cat(sprintf("Recall (Sensibilitat):  %.4f\n", met$recall))
  cat(sprintf("Especificitat:          %.4f\n", met$specificity))
  cat(sprintf("F1:                     %.4f\n", met$F1))
  cat(sprintf("Balanced Accuracy:      %.4f\n\n", met$balanced_accuracy))

  cat("Matriu de confusio:\n")
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
    labs(title = sprintf("Matriu de confusio — %s", titol),
         subtitle = sprintf("Llindar = %.3f | Precision = %.4f | Recall = %.4f",
                            met$threshold, met$precision, met$recall),
         x = "Valor Predit", y = "Valor Observat") +
    theme_minimal(base_size = 13) +
    theme(panel.grid = element_blank())
  print(p_cm)
}

# ------------------------------
# Funcions de metriques SVM
# ------------------------------

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

 accuracy <- (TP + TN) / (TP + TN + FP + FN)
 precision <- ifelse(TP + FP > 0, TP / (TP + FP), NA)
 recall <- ifelse(TP + FN > 0, TP / (TP + FN), NA)
 specificity <- ifelse(TN + FP > 0, TN / (TN + FP), NA)
 f1 <- ifelse(!is.na(precision) & !is.na(recall) & (precision + recall) > 0,
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
 cat(sprintf("AUC (val set): %.4f\n", met$AUC_cv_mean))
 cat(sprintf("AUC: %.4f\n", met$AUC))
 cat(sprintf("Accuracy: %.4f\n", met$accuracy))
 cat(sprintf("Precision (PPV): %.4f\n", met$precision))
 cat(sprintf("Recall (Sensibilitat): %.4f\n", met$recall))
 cat(sprintf("Especificitat: %.4f\n", met$specificity))
 cat(sprintf("F1: %.4f\n", met$F1))
 cat(sprintf("Balanced Accuracy: %.4f\n\n", met$balanced_accuracy))
 cat("Matriu de confusió:\n")
 cm <- matrix(c(met$TN, met$FN, met$FP, met$TP), nrow = 2,
 dimnames = list(Observat = c("Irregular(0)", "Regular(1)"),
 Predit = c("Irregular(0)", "Regular(1)")))
 print(cm)
 df_cm <- data.frame(
 Observat = factor(c("Irregular","Irregular","Regular","Regular"),
 levels = c("Regular","Irregular")),
 Predit = factor(c("Irregular","Regular","Irregular","Regular"),
 levels = c("Irregular","Regular")),
 n = c(met$TN, met$FP, met$FN, met$TP),
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

# Codificació de variables categòriques 
# Variables ordinals (Likert 1-6) -> numeric
# Variables binàries -> 0/1
# Variables categòriques (> 2 nivells) -> dummies
preparar_matriu_svm <- function(df, vars) {
 vars_ok <- vars[vars %in% names(df)]
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
