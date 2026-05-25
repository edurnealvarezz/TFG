# nivell 0 (7 learners): Logit predictiu, RF, XGBoost, SVM-RBF,
# ElasticNet i KNN
# nivell 1 (meta-learner): Ridge logit (alpha=0, lower.limits=0)

packages <- c("dplyr", "ggplot2", "tibble", "tidyr", "caret",
 "pROC", "PRROC", "ranger", "xgboost", "glmnet",
 "e1071", "FNN", "ResourceSelection")

install_if_missing <- function(pkg) {
 if (!require(pkg, character.only = TRUE)) {
 install.packages(pkg); library(pkg, character.only = TRUE)
 }
}
lapply(packages, install_if_missing)
rm(packages)

#setwd("C:/Users/edurn/Downloads/TFG")
setwd("C:/Users/Edurne/Downloads/TFG")

load("2. Dades/8. Dades CatBoost.RData")
model_logit_pred <- readRDS("2. Dades/model_logit_pred.rds")
formula_pred <- formula(model_logit_pred)
source("3. Codi/Funcions models.R")

motius_vars <- readRDS("2. Dades/motius_vars.rds")
estrategies_vars <- readRDS("2. Dades/estrategies_vars.rds")
ia_vars <- readRDS("2. Dades/ia_vars.rds")

sink("4. Outputs/10.1 Output_text_superlearner.txt")
pdf("4. Outputs/10.2 Output_grafics_superlearner.pdf", width = 10, height = 8)

MIN_RECALL <- 0.60

#### ============================================================ ####
####                    0. PREPARACIÓ DE DADES                    ####
#### ============================================================ ####

cat(" ============ 0. PREPARACIO DE DADES ============\n")

dades_mod <- dades_def %>%
 mutate(
 Y = as.integer(GRUP_ASSIST == "Regular (≥80%)"),
 NOTA_num = as.numeric(NOTA),
 IA_SUBST_num = as.numeric(IA_SUBST),
 T_AVAL_num = as.integer(T_AVAL == "Continuada"),
 CURS_1R_num = as.integer(CURS_1R)
 ) %>%
 filter(!is.na(Y))

# fem la partició de les dades
set.seed(1234)
idx_train_sl <- createDataPartition(dades_mod$Y, p = 0.80, list = FALSE)
dades_train <- dades_mod[idx_train_sl, ]
dades_test <- dades_mod[-idx_train_sl, ]

# 
Feature set per als learners numerics
vars_fa <- c("MOT_DESMOTIVACIO", "MOT_AUTOGESTIO", "MOT_FORCA_MAJOR",
 "EST_QUALITAT_DOC", "EST_AVALUACIO_AC", "EST_TEMPS_CLASSE",
 "EST_GRUPS_REDUITS", "IA_EINA_ESTUDI", "IA_SUBSTITUCIO")
vars_acad <- c("NOTA_num", "T_AVAL_num", "CURS_1R_num", "N_ASSIG")
vars_pers <- c("EDAT", "DESPL")
vars_sl <- unique(c(motius_vars, estrategies_vars, ia_vars,
 vars_fa, vars_acad, vars_pers))
vars_sl <- vars_sl[vars_sl %in% names(dades_mod)]

# Subset de casos complets (usat pels learners numerics)
cc_train <- complete.cases(dades_train[, vars_sl])
cc_test <- complete.cases(dades_test[, vars_sl])
train_sl <- dades_train[cc_train, ]
test_sl <- dades_test[cc_test, ]

X_train_sl <- as.matrix(apply(train_sl[, vars_sl], 2, as.numeric))
X_test_sl <- as.matrix(apply(test_sl[, vars_sl], 2, as.numeric))
Y_train <- train_sl$Y
Y_test <- test_sl$Y
n_train <- nrow(train_sl)
n_test <- nrow(test_sl)
p_sl <- ncol(X_train_sl)

# Pesos de classe balancejats
n_irr <- sum(Y_train == 0); n_reg <- sum(Y_train == 1)
w_irr <- (n_irr + n_reg) / (2 * n_irr)
w_reg <- (n_irr + n_reg) / (2 * n_reg)
case_w <- ifelse(Y_train == 0, w_irr, w_reg)

cat(sprintf("Particio: Train = %d obs | Test = %d obs | Predictors SL = %d\n",
 n_train, n_test, p_sl))
cat(sprintf("Train Y: Irregular=%d (%.1f%%) | Regular=%d (%.1f%%)\n\n",
 n_irr, n_irr/n_train*100, n_reg, n_reg/n_train*100))
cat(sprintf("Pesos: w_irr=%.3f | w_reg=%.3f\n\n", w_irr, w_reg))

cat("\nFormula Logit predictiu:\n"); print(formula_pred); cat("\n")

#### ============================================================ ####
#### 1. FUNCIONS DELS LEARNERS LEVEL-0 ####
#### ============================================================ ####

# L1: Logit predictiu
pred_logit_mil_fn <- function(tr_df, te_df) {
 m <- tryCatch(glm(formula_mil, data = tr_df, family = binomial),
 error = function(e) NULL)
 if (is.null(m)) return(rep(NA_real_, nrow(te_df)))
 tryCatch(predict(m, newdata = te_df, type = "response"),
 error = function(e) rep(NA_real_, nrow(te_df)))
}

# L3: Random Forest (hiperparametres fixos raonables)
pred_rf_fn <- function(X_tr, Y_tr, X_te, w) {
 mtry_r <- max(1L, floor(sqrt(ncol(X_tr))))
 m <- tryCatch(
 ranger(x = X_tr, y = factor(Y_tr, levels = c(0, 1)),
 num.trees = 300, mtry = mtry_r, min.node.size = 5,
 case.weights = w, probability = TRUE, seed = 1234),
 error = function(e) NULL
 )
 if (is.null(m)) return(rep(NA_real_, nrow(X_te)))
 predict(m, data = X_te)$predictions[, "1"]
}

# L4: XGBoost (hiperparametres: max_depth=3, eta=0.1, nrounds=100)
pred_xgb_fn <- function(X_tr, Y_tr, X_te) {
 scale_pw <- sum(Y_tr == 0) / pmax(sum(Y_tr == 1), 1)
 dtrain <- xgb.DMatrix(X_tr, label = Y_tr)
 dtest_ <- xgb.DMatrix(X_te)
 params <- list(objective = "binary:logistic", eval_metric = "auc",
 max_depth = 3, eta = 0.1, subsample = 0.8,
 colsample_bytree = 0.7, scale_pos_weight = scale_pw,
 nthread = 1)
 m <- tryCatch(xgb.train(params, dtrain, nrounds = 100, verbose = 0),
 error = function(e) NULL)
 if (is.null(m)) return(rep(NA_real_, nrow(X_te)))
 predict(m, dtest_)
}

# L5: SVM-RBF (escala interna per fold)
pred_svm_fn <- function(X_tr, Y_tr, X_te) {
 mu <- colMeans(X_tr)
 sg <- apply(X_tr, 2, sd); sg[sg == 0] <- 1
 Xtr_s <- scale(X_tr, center = mu, scale = sg)
 Xte_s <- scale(X_te, center = mu, scale = sg)
 m <- tryCatch(
 e1071::svm(x = Xtr_s, y = factor(Y_tr, levels = c(0, 1)),
 kernel = "radial", probability = TRUE, cost = 1),
 error = function(e) NULL
 )
 if (is.null(m)) return(rep(NA_real_, nrow(X_te)))
 pred <- tryCatch(predict(m, Xte_s, probability = TRUE), error = function(e) NULL)
 if (is.null(pred)) return(rep(NA_real_, nrow(X_te)))
 probs <- attr(pred, "probabilities")
 if ("1" %in% colnames(probs)) probs[, "1"] else probs[, 2]
}

# L6: ElasticNet (alpha=0.5, lambda per CV interna 5-fold)
pred_enet_fn <- function(X_tr, Y_tr, X_te) {
 m <- tryCatch(
 cv.glmnet(X_tr, Y_tr, family = "binomial", alpha = 0.5,
 nfolds = 5, type.measure = "auc"),
 error = function(e) NULL
 )
 if (is.null(m)) return(rep(NA_real_, nrow(X_te)))
 as.numeric(predict(m, newx = X_te, s = "lambda.min", type = "response"))
}

# L7: KNN (k=7, escala interna per fold)
pred_knn_fn <- function(X_tr, Y_tr, X_te) {
 mu <- colMeans(X_tr)
 sg <- apply(X_tr, 2, sd); sg[sg == 0] <- 1
 Xtr_s <- scale(X_tr, center = mu, scale = sg)
 Xte_s <- scale(X_te, center = mu, scale = sg)
 out <- tryCatch(
 FNN::knn(train = Xtr_s, test = Xte_s, cl = Y_tr, k = 7, prob = TRUE),
 error = function(e) NULL
 )
 if (is.null(out)) return(rep(NA_real_, nrow(X_te)))
 p <- attr(out, "prob")
 ifelse(as.integer(as.character(out)) == 1, p, 1 - p)
}

learner_noms <- c("Logit", "LogitMil", "RF", "XGBoost", "SVM", "ElasticNet", "KNN")
n_learners <- length(learner_noms)

#### ============================================================ ####
#### 2. OOF PREDICTIONS (10-fold CV sobre train) ####
#### ============================================================ ####

cat("\n=================================================================\n")
cat(" 2. OOF PREDICTIONS (10-fold CV sobre train)\n")
cat("=================================================================\n\n")
cat("NOTA: OOF evita data leakage en el meta-learner.\n")
cat(" Cada observacio train es predita per un model que NO la va veure.\n\n")

set.seed(1234)
folds <- createFolds(Y_train, k = 10, list = TRUE)
mat_oof <- matrix(NA_real_, nrow = n_train, ncol = n_learners,
 dimnames = list(NULL, learner_noms))

for (fi in seq_along(folds)) {
 val_idx <- folds[[fi]]
 tr_idx <- setdiff(seq_len(n_train), val_idx)

 X_tr <- X_train_sl[tr_idx, ]; X_va <- X_train_sl[val_idx, ]
 Y_tr <- Y_train[tr_idx]; w_tr <- case_w[tr_idx]
 df_tr <- train_sl[tr_idx, ]; df_va <- train_sl[val_idx, ]

 mat_oof[val_idx, "LogitMil"] <- pred_logit_mil_fn(df_tr, df_va)
 mat_oof[val_idx, "RF"] <- pred_rf_fn(X_tr, Y_train[tr_idx], X_va, w_tr)
 mat_oof[val_idx, "XGBoost"] <- pred_xgb_fn(X_tr, Y_train[tr_idx], X_va)
 mat_oof[val_idx, "SVM"] <- pred_svm_fn(X_tr, Y_train[tr_idx], X_va)
 mat_oof[val_idx, "ElasticNet"] <- pred_enet_fn(X_tr, Y_train[tr_idx], X_va)
 mat_oof[val_idx, "KNN"] <- pred_knn_fn(X_tr, Y_train[tr_idx], X_va)

 cat(sprintf(" Fold %2d/10 completat\n", fi))
}

df_oof <- as.data.frame(mat_oof)
df_oof$Y <- Y_train

cat("\nAUC OOF per learner:\n")
oof_auc_tbl <- do.call(rbind, lapply(learner_noms, function(l) {
 vals <- df_oof[[l]]
 valid <- !is.na(vals)
 auc_v <- if (sum(valid) >= 10)
 as.numeric(pROC::auc(pROC::roc(df_oof$Y[valid], vals[valid], quiet = TRUE)))
 else NA_real_
 data.frame(Learner = l, AUC_OOF = round(auc_v, 4), n_valid = sum(valid))
}))
print(oof_auc_tbl[order(-oof_auc_tbl$AUC_OOF), ], row.names = FALSE)

#### ============================================================ ####
#### 3. LEVEL-0 PREDICTIONS SOBRE TEST (full train) ####
#### ============================================================ ####

cat("\n=================================================================\n")
cat(" 3. PREDICCIONS LEVEL-0 SOBRE TEST (models entrenats en tot train)\n")
cat("=================================================================\n\n")

mat_test_l0 <- matrix(NA_real_, nrow = n_test, ncol = n_learners,
 dimnames = list(NULL, learner_noms))

mat_test_l0[, "LogitMil"] <- pred_logit_mil_fn(train_sl, test_sl)
mat_test_l0[, "RF"] <- pred_rf_fn(X_train_sl, Y_train, X_test_sl, case_w)
mat_test_l0[, "XGBoost"] <- pred_xgb_fn(X_train_sl, Y_train, X_test_sl)
mat_test_l0[, "SVM"] <- pred_svm_fn(X_train_sl, Y_train, X_test_sl)
mat_test_l0[, "ElasticNet"] <- pred_enet_fn(X_train_sl, Y_train, X_test_sl)
mat_test_l0[, "KNN"] <- pred_knn_fn(X_train_sl, Y_train, X_test_sl)

cat("AUC test per learner (individual):\n")
test_auc_tbl <- do.call(rbind, lapply(learner_noms, function(l) {
 vals <- mat_test_l0[, l]
 valid <- !is.na(vals)
 auc_v <- if (sum(valid) >= 5)
 as.numeric(pROC::auc(pROC::roc(Y_test[valid], vals[valid], quiet = TRUE)))
 else NA_real_
 data.frame(Learner = l, AUC_test = round(auc_v, 4), n_valid = sum(valid))
}))
print(test_auc_tbl[order(-test_auc_tbl$AUC_test), ], row.names = FALSE)

#### ============================================================ ####
#### 4. META-LEARNER (Ridge alpha=0, lower.limits=0) ####
#### ============================================================ ####

cat("\n=================================================================\n")
cat(" 4. META-LEARNER (Ridge logit, alpha=0, coefs >= 0)\n")
cat("=================================================================\n\n")
cat("Lower.limits=0 -> combinacio convexa dels learners (interpretable).\n")
cat("Els coeficients Ridge diuen quant aporta cada model al ensemble.\n\n")

oof_complete <- complete.cases(mat_oof)
cat(sprintf("OOF files complets: %d / %d\n\n", sum(oof_complete), n_train))

X_oof_meta <- mat_oof[oof_complete, ]
Y_oof_meta <- Y_train[oof_complete]

set.seed(1234)
meta_cv <- cv.glmnet(
 x = X_oof_meta,
 y = Y_oof_meta,
 family = "binomial",
 alpha = 0,
 lower.limits = 0,
 nfolds = 10,
 type.measure = "auc",
 keep = TRUE
)

cat(sprintf("Lambda optim (lambda.min): %.6f\n", meta_cv$lambda.min))
cat(sprintf("Lambda 1se: %.6f\n\n", meta_cv$lambda.1se))

coefs_meta <- as.matrix(coef(meta_cv, s = "lambda.min"))
cat("Coeficients meta-learner (Ridge, lambda.min):\n")
print(round(coefs_meta, 4))

df_coefs <- data.frame(
 Learner = rownames(coefs_meta)[-1],
 Pes = coefs_meta[-1, 1]
) %>% arrange(desc(Pes))

ggplot(df_coefs, aes(x = reorder(Learner, Pes), y = Pes, fill = Pes)) +
 geom_col(alpha = 0.85) +
 coord_flip() +
 scale_fill_gradient(low = "#AED6F1", high = "#1A5276", guide = "none") +
 labs(title = "Pesos del meta-learner (Ridge, alpha=0)",
 subtitle = "Contribucio de cada learner Level-0 | lower.limits=0",
 x = "", y = "Coeficient Ridge") +
 theme_minimal(base_size = 13)

#### ============================================================ ####
#### 5. PREDICCIONS SUPERLEARNER SOBRE TEST + METRIQUES ####
#### ============================================================ ####

cat("\n=================================================================\n")
cat(" 5. PREDICCIONS SUPERLEARNER SOBRE TEST\n")
cat("=================================================================\n\n")

test_complete <- complete.cases(mat_test_l0)
X_test_meta <- mat_test_l0[test_complete, ]
Y_test_meta <- Y_test[test_complete]
n_test_meta <- sum(test_complete)

prob_meta_test <- as.numeric(
 predict(meta_cv, newx = X_test_meta, s = "lambda.min", type = "response")
)

# Llindar PR sobre prediccions OOF del meta-learner (evita contaminacio del test)
idx_lmin <- which.min(abs(meta_cv$lambda - meta_cv$lambda.min))
prob_meta_cv_oof <- plogis(meta_cv$fit.preval[, idx_lmin])
pr_meta_oof <- seleccionar_llindar_pr(prob_meta_cv_oof, Y_oof_meta, MIN_RECALL)
thresh_meta <- pr_meta_oof$threshold

# AUPRC i corba PR sobre test (nomes per reporting)
pr_meta <- seleccionar_llindar_pr(prob_meta_test, Y_test_meta, MIN_RECALL)

cat(sprintf("AUPRC SuperLearner (test): %.4f\n", pr_meta$auprc))
cat(sprintf("Llindar (recall>=%.2f, OOF meta): %.4f | recall_ok: %s\n\n",
 MIN_RECALL, thresh_meta, ifelse(pr_meta_oof$recall_ok, "SI", "NO (fallback)")))

ggplot(pr_meta$pr_curve, aes(x = recall, y = precision)) +
 geom_path(color = "#4A90B8", linewidth = 1) +
 geom_vline(xintercept = MIN_RECALL, linetype = "dashed",
 color = "red", linewidth = 0.8) +
 geom_point(data = data.frame(
 recall = pr_meta_oof$recall,
 precision = ifelse(is.na(pr_meta_oof$precision), 0, pr_meta_oof$precision)),
 color = "#E07B54", size = 3, shape = 17) +
 annotate("text", x = MIN_RECALL + 0.04, y = 0.1,
 label = sprintf("Recall min\n= %.2f", MIN_RECALL),
 color = "red", size = 3.5) +
 labs(title = "Corba Precisio-Recall — SuperLearner (test)",
 subtitle = sprintf("AUPRC = %.4f | Llindar = %.4f", pr_meta$auprc, thresh_meta),
 x = "Recall", y = "Precisio (PPV)") +
 theme_minimal(base_size = 13)

# Metriques de classificacio
pred_meta <- as.integer(prob_meta_test >= thresh_meta)
TP_sl <- sum(pred_meta == 1 & Y_test_meta == 1)
TN_sl <- sum(pred_meta == 0 & Y_test_meta == 0)
FP_sl <- sum(pred_meta == 1 & Y_test_meta == 0)
FN_sl <- sum(pred_meta == 0 & Y_test_meta == 1)

acc_sl <- (TP_sl + TN_sl) / n_test_meta
prec_sl <- if (TP_sl + FP_sl > 0) TP_sl / (TP_sl + FP_sl) else NA_real_
rec_sl <- if (TP_sl + FN_sl > 0) TP_sl / (TP_sl + FN_sl) else NA_real_
spec_sl <- if (TN_sl + FP_sl > 0) TN_sl / (TN_sl + FP_sl) else NA_real_
f1_sl <- if (!is.na(prec_sl) & !is.na(rec_sl) & (prec_sl + rec_sl) > 0)
 2 * prec_sl * rec_sl / (prec_sl + rec_sl) else NA_real_
balacc_sl <- if (!is.na(rec_sl) & !is.na(spec_sl)) (rec_sl + spec_sl) / 2 else NA_real_
auc_sl <- as.numeric(pROC::auc(pROC::roc(Y_test_meta, prob_meta_test, quiet = TRUE)))
brier_sl <- mean((prob_meta_test - Y_test_meta)^2)

cat(sprintf("n test (complets): %d\n", n_test_meta))
cat(sprintf("AUC: %.4f\n", auc_sl))
cat(sprintf("AUPRC: %.4f\n", pr_meta$auprc))
cat(sprintf("Precision (PPV): %.4f\n", prec_sl))
cat(sprintf("Recall: %.4f\n", rec_sl))
cat(sprintf("Especificitat: %.4f\n", spec_sl))
cat(sprintf("F1: %.4f\n", f1_sl))
cat(sprintf("Balanced Acc: %.4f\n", balacc_sl))
cat(sprintf("Brier Score: %.4f (0=perfecte, 0.25=atzar)\n\n", brier_sl))
cat(sprintf("TP=%d | TN=%d | FP=%d | FN=%d\n\n", TP_sl, TN_sl, FP_sl, FN_sl))

# Matriu de confusio
cm_sl <- matrix(c(TN_sl, FN_sl, FP_sl, TP_sl), nrow = 2,
 dimnames = list(Observat = c("Irr(0)", "Reg(1)"),
 Predit = c("Irr(0)", "Reg(1)")))
cat("Matriu de confusio:\n"); print(cm_sl); cat("\n")

df_cm_sl <- data.frame(
 Observat = factor(c("Irregular","Irregular","Regular","Regular"),
 levels = c("Regular","Irregular")),
 Predit = factor(c("Irregular","Regular","Irregular","Regular"),
 levels = c("Irregular","Regular")),
 n = c(TN_sl, FP_sl, FN_sl, TP_sl),
 etiq = c("TN","FP","FN","TP")
)
ggplot(df_cm_sl, aes(x = Predit, y = Observat, fill = n)) +
 geom_tile(color = "white", linewidth = 1) +
 geom_text(aes(label = paste0(etiq, "\n", n)), size = 5, fontface = "bold") +
 scale_fill_gradient(low = "#EBF5FB", high = "#2471A3", guide = "none") +
 labs(title = "Matriu de confusio — SuperLearner (test)",
 subtitle = sprintf("Llindar PR = %.3f", thresh_meta),
 x = "Valor Predit", y = "Valor Observat") +
 theme_minimal(base_size = 13) + theme(panel.grid = element_blank())

#### ============================================================ ####
#### 6. CALIBRACIO (Brier + Platt + Plot) ####
#### ============================================================ ####

cat("\n=================================================================\n")
cat(" 6. CALIBRACIO\n")
cat("=================================================================\n\n")

# Platt Scaling: regressi logistica sobre les probs del SL
platt_model <- glm(Y ~ prob,
 data = data.frame(prob = prob_meta_test, Y = Y_test_meta),
 family = binomial)
prob_platt <- predict(platt_model, type = "response")
brier_platt <- mean((prob_platt - Y_test_meta)^2)

cat(sprintf("Brier ABANS Platt: %.4f\n", brier_sl))
cat(sprintf("Brier DESPRES Platt: %.4f\n\n", brier_platt))
cat(ifelse(brier_platt < brier_sl,
 "-> Platt millora calibracio\n\n",
 "-> Calibracio ja bona sense Platt\n\n"))

# Calibration plot (decils)
n_decils <- min(10L, as.integer(n_test_meta %/% 5))
df_cal <- data.frame(prob_pred = prob_meta_test, Y_obs = Y_test_meta) %>%
 arrange(prob_pred) %>%
 mutate(decil = ntile(prob_pred, n_decils))

df_cal_dec <- df_cal %>%
 group_by(decil) %>%
 summarise(prob_mitjana = mean(prob_pred), prop_obs = mean(Y_obs),
 n = n(), .groups = "drop")

ggplot(df_cal_dec, aes(x = prob_mitjana, y = prop_obs)) +
 geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
 geom_smooth(method = "loess", se = TRUE, color = "#4A90B8",
 fill = "#AED6F1", linewidth = 1, span = 1) +
 geom_point(aes(size = n), color = "#E07B54", alpha = 0.85) +
 scale_size_continuous(range = c(2, 6), guide = "none") +
 annotate("text", x = 0.72, y = 0.12,
 label = sprintf("Brier = %.3f", brier_sl), size = 4, color = "grey30") +
 coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
 labs(title = "Calibration Plot — SuperLearner (test)",
 subtitle = sprintf("%d decils | prop. observada vs. predicta", n_decils),
 x = "Probabilitat predicta (decil)", y = "Proporció observada Regular") +
 theme_minimal(base_size = 13)

#### ============================================================ ####
#### 7. ANALISI DE DESACORD ENTRE LEARNERS ####
#### ============================================================ ####

cat("\n=================================================================\n")
cat(" 7. ANALISI DE DESACORD (disagreement analysis)\n")
cat("=================================================================\n\n")
cat("Alta variancia entre learners = observacio difícil / perfil mixt.\n\n")

df_disagree <- as.data.frame(X_test_meta)
colnames(df_disagree) <- learner_noms
df_disagree$variancia <- apply(X_test_meta, 1, var)
df_disagree$prob_sl <- prob_meta_test
df_disagree$pred_sl <- pred_meta
df_disagree$Y <- Y_test_meta

cat("Distribucio de la variancia entre learners (test):\n")
print(summary(df_disagree$variancia))

top_n <- ceiling(0.15 * nrow(df_disagree))
df_high <- df_disagree %>%
 slice_max(variancia, n = top_n)

cat(sprintf("\nTop %d obs amb mes desacord (15%%):\n", top_n))
print(round(df_high %>% dplyr::select(all_of(learner_noms), variancia, prob_sl, Y, pred_sl), 3),
 row.names = FALSE)

cat(sprintf("\nDistribucio Y en alt-desacord: Regular=%.1f%% | Irregular=%.1f%%\n\n",
 mean(df_high$Y) * 100, (1 - mean(df_high$Y)) * 100))
cat(sprintf("Precisio SuperLearner en alt-desacord: %.3f\n",
 mean(df_high$pred_sl == df_high$Y)))
cat(sprintf("Precisio SuperLearner en baix-desacord: %.3f\n\n",
 mean(df_disagree$pred_sl[df_disagree$variancia <= quantile(df_disagree$variancia, 0.85)] ==
 df_disagree$Y[df_disagree$variancia <= quantile(df_disagree$variancia, 0.85)])))

ggplot(df_disagree, aes(x = variancia, y = prob_sl, color = factor(Y))) +
 geom_point(size = 2, alpha = 0.75) +
 geom_hline(yintercept = thresh_meta, linetype = "dashed", color = "grey40") +
 scale_color_manual(values = c("0" = "#E07B54", "1" = "#4A90B8"),
 labels = c("0" = "Irregular", "1" = "Regular")) +
 labs(title = "Desacord entre learners Level-0 (test)",
 subtitle = "x = variancia probs L0 | y = prob SuperLearner | linia = llindar PR",
 x = "Variancia entre learners", y = "P(Regular) SuperLearner",
 color = "Y observat") +
 theme_minimal(base_size = 13) + theme(legend.position = "top")

#### ============================================================ ####
#### 8. SHAP DEL META-LEARNER (contribucions lineals) ####
#### ============================================================ ####

cat("\n=================================================================\n")
cat(" 8. SHAP DEL META-LEARNER\n")
cat("=================================================================\n\n")
cat("Per model lineal: SHAP_j = beta_j * (x_ij - mean_oof_j)\n")
cat("Interpreta quant contribueix cada learner a cada prediccio individual.\n\n")

coef_vec <- coefs_meta[-1, 1]
mean_oof <- colMeans(X_oof_meta)

shap_mat <- sweep(X_test_meta, 2, mean_oof, "-")
shap_mat <- sweep(shap_mat, 2, coef_vec, "*")

shap_imp_meta <- tibble(
 Learner = learner_noms,
 mean_abs_shap = colMeans(abs(shap_mat))
) %>% arrange(desc(mean_abs_shap))

cat("Importancia SHAP (mean |SHAP|) dels learners Level-0:\n")
print(shap_imp_meta)

ggplot(shap_imp_meta, aes(x = reorder(Learner, mean_abs_shap),
 y = mean_abs_shap, fill = mean_abs_shap)) +
 geom_col(alpha = 0.85) + coord_flip() +
 scale_fill_gradient(low = "#A9DFBF", high = "#1E8449", guide = "none") +
 labs(title = "Importancia SHAP — Meta-learner SuperLearner",
 subtitle = "mean(|SHAP|) sobre conjunt test",
 x = "", y = "Importancia SHAP") +
 theme_minimal(base_size = 13)

# Beeswarm SHAP per learner
shap_long <- as.data.frame(shap_mat) %>%
 mutate(obs = row_number()) %>%
 pivot_longer(-obs, names_to = "Learner", values_to = "shap") %>%
 left_join(
 as.data.frame(X_test_meta) %>%
 setNames(learner_noms) %>%
 mutate(obs = row_number()) %>%
 pivot_longer(-obs, names_to = "Learner", values_to = "prob_l0"),
 by = c("obs", "Learner")
 ) %>%
 mutate(Learner = factor(Learner, levels = rev(shap_imp_meta$Learner)))

ggplot(shap_long, aes(x = shap, y = Learner, color = prob_l0)) +
 geom_jitter(height = 0.25, size = 2, alpha = 0.65) +
 geom_vline(xintercept = 0, color = "grey40", linewidth = 0.8) +
 scale_color_gradient(low = "#2471A3", high = "#E74C3C", name = "Prob. L0") +
 labs(title = "SHAP Beeswarm — Meta-learner SuperLearner (test)",
 subtitle = "x > 0 augmenta P(Regular) | color = prob. predicta L0",
 x = "Valor SHAP", y = "") +
 theme_minimal(base_size = 12)

#### ============================================================ ####
#### 9. COMPARACIO GLOBAL DE MODELS ####
#### ============================================================ ####

cat("\n=================================================================\n")
cat(" 9. COMPARACIO GLOBAL DE MODELS\n")
cat("=================================================================\n\n")

fitxers_met <- list(
 "Logit" = "2. Dades/metriques_logit.rds",
 "LogitMil" = "2. Dades/metriques_logit_millorat.rds",
 "RF-A" = "2. Dades/metriques_rf_a.rds",
 "RF-B" = "2. Dades/metriques_rf_b.rds",
 "XGBoost" = "2. Dades/metriques_xgb.rds",
 "CatBoost" = "2. Dades/metriques_catboost.rds"
)

extreure_comp <- function(nom, m) {
 data.frame(
 Model = nom,
 AUC_test = round(m$AUC, 4),
 Precision = round(m$precision, 4),
 Recall = round(m$recall, 4),
 F1 = round(m$F1, 4),
 Balanced_Acc = round(m$balanced_accuracy, 4),
 stringsAsFactors = FALSE
 )
}

rows_comp <- do.call(rbind, lapply(names(fitxers_met), function(nom) {
 f <- fitxers_met[[nom]]
 if (!file.exists(f)) return(NULL)
 extreure_comp(nom, readRDS(f))
}))

row_sl <- data.frame(
 Model = "SuperLearner",
 AUC_test = round(auc_sl, 4),
 Precision = round(prec_sl, 4),
 Recall = round(rec_sl, 4),
 F1 = round(f1_sl, 4),
 Balanced_Acc = round(balacc_sl, 4),
 stringsAsFactors = FALSE
)

df_comp_final <- rbind(rows_comp, row_sl)
rownames(df_comp_final) <- NULL

cat("Taula comparativa final:\n\n")
print(df_comp_final, row.names = FALSE)

# Identificar millor model per cada metrica
cat("\nMillor model per metrica:\n")
metriques_comp <- c("AUC_test", "Precision", "Recall", "F1", "Balanced_Acc")
for (m in metriques_comp) {
 best_i <- which.max(df_comp_final[[m]])
 cat(sprintf(" %-14s: %s (%.4f)\n", m,
 df_comp_final$Model[best_i], df_comp_final[[m]][best_i]))
}

# Grafic comparatiu
colors_all <- c("#4A90B8", "#5DADE2", "#E07B54", "#F0B27A", "#8E6BBF",
 "#BB8FCE", "#E74C3C", "#2ECC71")[seq_len(nrow(df_comp_final))]

df_comp_long <- df_comp_final %>%
 pivot_longer(-Model, names_to = "metrica", values_to = "valor") %>%
 mutate(metrica = factor(metrica, levels = metriques_comp),
 Model = factor(Model, levels = df_comp_final$Model),
 SuperLearner = Model == "SuperLearner")

ggplot(df_comp_long, aes(x = metrica, y = valor, fill = Model,
 alpha = SuperLearner)) +
 geom_col(position = "dodge") +
 geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50") +
 scale_fill_manual(values = colors_all) +
 scale_alpha_manual(values = c("FALSE" = 0.70, "TRUE" = 1.00), guide = "none") +
 scale_y_continuous(limits = c(0, 1)) +
 labs(title = "Comparacio global: SuperLearner vs models individuals",
 subtitle = "Metriques sobre conjunt test | SuperLearner mes opac",
 x = "", y = "Valor") +
 theme_minimal(base_size = 13) +
 theme(axis.text.x = element_text(angle = 20, hjust = 1),
 legend.position = "bottom")

# Corba ROC comparativa Level-0 + SuperLearner
roc_list <- lapply(learner_noms, function(l) {
 vals <- mat_test_l0[test_complete, l]
 valid <- !is.na(vals)
 if (sum(valid) < 5) return(NULL)
 roc_obj <- pROC::roc(Y_test_meta[valid], vals[valid], quiet = TRUE)
 data.frame(spec_inv = 1 - roc_obj$specificities,
 sens = roc_obj$sensitivities, Model = l)
})
roc_sl_df <- local({
 r <- pROC::roc(Y_test_meta, prob_meta_test, quiet = TRUE)
 data.frame(spec_inv = 1 - r$specificities,
 sens = r$sensitivities, Model = "SuperLearner")
})
roc_all <- rbind(do.call(rbind, roc_list[!sapply(roc_list, is.null)]), roc_sl_df)

colors_roc <- c(setNames(
 scales::hue_pal()(n_learners), learner_noms),
 "SuperLearner" = "#E74C3C")

ggplot(roc_all, aes(x = spec_inv, y = sens,
 color = Model, linewidth = (Model == "SuperLearner"))) +
 geom_path() +
 geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
 scale_color_manual(values = colors_roc) +
 scale_linewidth_manual(values = c("FALSE" = 0.7, "TRUE" = 1.5), guide = "none") +
 annotate("text", x = 0.55, y = 0.10,
 label = sprintf("AUC SL = %.3f", auc_sl),
 color = "#E74C3C", size = 4) +
 labs(title = "Corbes ROC — Level-0 + SuperLearner (test)",
 x = "1 - Especificitat", y = "Sensibilitat", color = NULL) +
 theme_minimal(base_size = 13) + theme(legend.position = "right")

#### ============================================================ ####
#### 10. GUARDAR RESULTATS ####
#### ============================================================ ####

cat("\n=================================================================\n")
cat(" 10. GUARDAR RESULTATS\n")
cat("=================================================================\n\n")

metriques_sl <- list(
 model = "SuperLearner",
 n_test = n_test_meta,
 threshold = round(thresh_meta, 3),
 AUC = round(auc_sl, 4),
 auprc = round(pr_meta$auprc, 4),
 precision = round(prec_sl, 4),
 recall = round(rec_sl, 4),
 specificity = round(spec_sl, 4),
 F1 = round(f1_sl, 4),
 balanced_accuracy = round(balacc_sl, 4),
 brier = round(brier_sl, 4),
 TP = TP_sl, TN = TN_sl, FP = FP_sl, FN = FN_sl
)

saveRDS(metriques_sl, "2. Dades/metriques_superlearner.rds")
saveRDS(meta_cv, "2. Dades/model_superlearner_meta.rds")
saveRDS(df_oof, "2. Dades/oof_predictions.rds")
cat("-> metriques_superlearner.rds, model_superlearner_meta.rds, oof_predictions.rds guardats\n\n")

# Guardar probabilitats a dades_def (OOF per train, full-train per test)
dades_def$prob_superlearner <- NA_real_
dades_def$pred_superlearner <- NA_integer_

# OOF probs per als observations de train
prob_meta_oof <- as.numeric(
 predict(meta_cv, newx = X_oof_meta, s = "lambda.min", type = "response")
)
train_rows_in_mod <- idx_train_sl
train_cc_rows_in_mod <- train_rows_in_mod[cc_train]
train_oof_rows_in_mod <- train_cc_rows_in_mod[oof_complete]
dades_def$prob_superlearner[train_oof_rows_in_mod] <- prob_meta_oof
dades_def$pred_superlearner[train_oof_rows_in_mod] <- as.integer(prob_meta_oof >= thresh_meta)

# Probs (full-train models) per als observations de test
test_rows_in_mod <- setdiff(seq_len(nrow(dades_mod)), idx_train_sl)
test_cc_rows_in_mod <- test_rows_in_mod[cc_test]
test_meta_rows <- test_cc_rows_in_mod[test_complete]
dades_def$prob_superlearner[test_meta_rows] <- prob_meta_test
dades_def$pred_superlearner[test_meta_rows] <- pred_meta

cat(sprintf("Observations amb prob_superlearner: %d\n",
 sum(!is.na(dades_def$prob_superlearner))))
cat(sprintf(" -> Train (OOF): %d | Test: %d\n\n",
 sum(!is.na(dades_def$prob_superlearner[train_rows_in_mod])),
 sum(!is.na(dades_def$prob_superlearner[test_rows_in_mod]))))

save(dades_def, file = "2. Dades/10. Dades SuperLearner.RData")
cat("-> dades_def guardades a: 2. Dades/10. Dades SuperLearner.RData\n\n")

sink()
dev.off()
