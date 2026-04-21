packages <- c("dplyr", "ggplot2", "tibble", "tidyr",
              "ranger", "caret", "pROC", "PRROC", "fastshap")

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

sink("4. Outputs/6.1 Output_text_rf.txt")
pdf("4. Outputs/6.2 Output_grafics_rf.pdf", width = 10, height = 8)

#### ============================================================ ####
####                   0. PREPARACIÓ DE DADES                     ####
#### ============================================================ ####

dades_rf <- dades_def %>%
  mutate(
    Y = as.integer(GRUP_ASSIST == "Regular (≥80%)"),
    NOTA_num = as.numeric(NOTA),
    IA_SUBST_num = as.numeric(IA_SUBST),
    T_AVAL_num = as.integer(T_AVAL == "Continuada"),
    CURS_1R_num = as.integer(CURS_1R)
  ) %>%
  filter(!is.na(Y))

# Predictors RF-A: factors EFA + variables logit 
# + variables descartades pel logit
vars_rfa <- c(
  # Significatives en el logit
  "MOT_DESMOTIVACIO", "MOT_FORCA_MAJOR",
  "EST_AVALUACIO_AC",
  "IA_SUBST_num",
  "T_AVAL_num", "CURS_1R_num", "EDAT", "NOTA_num",
  # Descartades pel logit pero potencialment rellevants
  "MOT_AUTOGESTIO",
  "EST_QUALITAT_DOC", "EST_TEMPS_CLASSE", "EST_GRUPS_REDUITS",
  "IA_EINA_ESTUDI",
  "DESPL", "N_ASSIG"
)

# --- Predictors RF-B: Likert originals significatives (MW p<0.05) + categoriques ---
vars_rfb <- c(
  # Motius (MW p<0.05 a l'EDA)
  "IA_SUBST", "M_AUTON", "M_AVORR", "M_TEOR", "M_PROF",
  "M_EXAM", "M_AMICS", "M_REPET", "M_PASSIU", "M_UTIL",
  "M_SALUT", "M_FAM", "M_CV",
  # Estrategies significatives
  "E_HORA", "E_DINAM",
  # IA significatives
  "IA_HABIT", "IA_ATENC", "IA_PDFS", "IA_COMPR", "IA_REND",
  # Categoriques i numeriques
  "T_AVAL_num", "CURS_1R_num", "EDAT", "NOTA_num", "DESPL", "N_ASSIG"
)

# Filtrar predictors existents i convertir a numeric
prep_dades_rf <- function(df, vars) {
  vars <- vars[vars %in% names(df)]
  df %>%
    dplyr::select(Y, all_of(vars)) %>%
    mutate(across(-Y, as.numeric)) %>%
    drop_na()
}

dades_rfa <- prep_dades_rf(dades_rf, vars_rfa)
dades_rfb <- prep_dades_rf(dades_rf, vars_rfb)


#### ============================================================ ####
####                   0. PREPARACIÓ DE DADES                     ####
#### ============================================================ ####

cat(sprintf("RF-A (factors EFA): %d obs | %d predictors\n",
            nrow(dades_rfa), ncol(dades_rfa) - 1))
cat(sprintf("RF-B (Likert orig): %d obs | %d predictors\n\n",
            nrow(dades_rfb), ncol(dades_rfb) - 1))

cat(sprintf("Distribucio Y — Irregular (0): %d | Regular (1): %d\n",
            sum(dades_rfa$Y == 0), sum(dades_rfa$Y == 1)))
cat(sprintf("Rati de desbalanceig: %.2f\n\n",
            sum(dades_rfa$Y == 0) / sum(dades_rfa$Y == 1)))

# --- Particio train/test (80/20) ---
set.seed(1234)
idx_train_a <- createDataPartition(factor(dades_rfa$Y), p = 0.80, list = FALSE)
train_a <- dades_rfa[idx_train_a, ]
test_a <- dades_rfa[-idx_train_a, ]

idx_train_b <- createDataPartition(factor(dades_rfb$Y), p = 0.80, list = FALSE)
train_b <- dades_rfb[idx_train_b, ]
test_b <- dades_rfb[-idx_train_b, ]


# assignem més pes a la classe menys freqüent
n_irr <- sum(train_a$Y == 0)
n_reg <- sum(train_a$Y == 1)
w_irr <- (n_irr + n_reg) / (2 * n_irr)
w_reg <- (n_irr + n_reg) / (2 * n_reg)
case_weights_a <- ifelse(train_a$Y == 0, w_irr, w_reg)
case_weights_b <- ifelse(train_b$Y == 0, w_irr, w_reg)
cat(sprintf("Case weights: Irregular = %.3f | Regular = %.3f\n\n", w_irr, w_reg))

# cada individu irregular pesa 1.155

# a l'arxiu Funcions models hi ha funcions per a calcular mètriques, les carreguem:
source("3. Codi/Funcions models.R")

MIN_RECALL <- 0.40

#### ============================================================ ####
####                     1. RANDOM FOREST A                       ####
#### ============================================================ ####

cat("\n==============================================\n")
cat("              1. RANDOM FOREST A                \n")
cat("================================================\n\n")

# --------------- 1.1. Tunning d'hiperparametres ---------------

p_a <- ncol(train_a) - 1 # num predictors

# mtry cal provar amb sqrt(p), p/3, p/2 i p
mtry_vals <- unique(c(floor(sqrt(p_a)), floor(p_a / 3), floor(p_a / 2), 10, p_a))
# valors mtry= 3, 5, 7, 10 (manual) i 15
# afegeixo el 10 perque sinó hi ha molta distància entre 7 i 15

node_vals <- c(5, 10, 15) # mida minima dels nodes
num_trees <- c(10, 100, 250, 300, 500, 1000)

cat(sprintf("p = %d predictors | mtry testats: %s | min.node.size: %s | num.trees: %s\n\n",
            p_a, paste(mtry_vals, collapse = ", "),
            paste(node_vals, collapse = ", "),
            paste(num_trees, collapse = ", ")))

grid_res <- expand.grid(mtry = mtry_vals, node = node_vals, trees = num_trees)
grid_res$OOB <- NA_real_
grid_res$AUC_oob <- NA_real_

# --------------- 1.2. Exploració d'hiperparametres ---------------

set.seed(1234)
for (i in seq_len(nrow(grid_res))) {
  rf_tmp <- ranger(
    x = as.matrix(train_a[, -1]),
    y = factor(train_a$Y, levels = c(0, 1)),
    num.trees = grid_res$trees[i],
    mtry = grid_res$mtry[i],
    min.node.size = grid_res$node[i],
    case.weights = case_weights_a,
    importance = "none",
    probability = TRUE,
    seed = 1234
  )
  grid_res$OOB[i] <- round(rf_tmp$prediction.error, 4)
  prob_oob <- rf_tmp$predictions[, 2]
  grid_res$AUC_oob[i] <- round(as.numeric(auc(roc(train_a$Y, prob_oob, quiet = TRUE))), 4)
}

grid_res <- grid_res %>% arrange(desc(AUC_oob)) # ordenem per la comb que té millor auc
cat("Grid search (ordenat per AUC OOB):\n")
print(grid_res, row.names = FALSE)

best <- grid_res[1, ]
cat(sprintf("\nMillors hiperparametres: mtry = %d | min.node.size = %d | num.trees = %d | AUC OOB = %.4f\n\n",
            best$mtry, best$node, best$trees, best$AUC_oob))

# --------------- 1.3. Model final ---------------

cat(" ================== MODEL FINAL ============== \n")

set.seed(1234)
rf_a <- ranger(
  x = as.matrix(train_a[, -1]),
  y = factor(train_a$Y, levels = c(0, 1)),
  num.trees = best$trees,
  mtry = best$mtry,
  min.node.size = best$node,
  case.weights = case_weights_a,
  importance = "permutation",
  probability = TRUE,
  seed = 1234
)

# prediccio sobre el test
prob_test_a <- predict(rf_a, data = as.matrix(test_a[, -1]))$predictions[, 2]

# prediccio sobre observacions q no s'han utilitzat per entrenar cada model
prob_oob_a <- rf_a$predictions[, 2]

# calculem mètriques per train i test
pr_rfa <- seleccionar_llindar_pr(prob_test_a, test_a$Y, MIN_RECALL)
thresh_pr_a <- pr_rfa$threshold
cat(sprintf("RF-A — AUPRC: %.4f | Llindar PR: %.4f | recall_ok: %s\n\n",
            pr_rfa$auprc, thresh_pr_a,
            ifelse(pr_rfa$recall_ok, "SI", "NO (fallback Youden)")))

met_rfa_test <- calcular_metriques_rf(prob_test_a, test_a$Y, "RF-A (test)",
                                      rf_a$prediction.error,
                                      thresh_override = thresh_pr_a)
met_rfa_train <- calcular_metriques_rf(prob_oob_a, train_a$Y, "RF-A (OOB train)",
                                       thresh_override = thresh_pr_a)

mostrar_metriques_rf(met_rfa_test)
mostrar_metriques_rf(met_rfa_train)

cat("\n--- Overfitting OOB vs test ---\n\n")
df_ov_a <- data.frame(
  Conjunt = c("Train (OOB)", "Test"),
  AUC = c(met_rfa_train$AUC, met_rfa_test$AUC),
  Accuracy = c(met_rfa_train$accuracy, met_rfa_test$accuracy),
  F1 = c(met_rfa_train$F1, met_rfa_test$F1),
  Balanced_Acc = c(met_rfa_train$balanced_accuracy, met_rfa_test$balanced_accuracy)
)
print(df_ov_a, row.names = FALSE)
# no sembla q hi hagi overfitting

print(grafic_cm(met_rfa_test, "RF-A"))

# corba ROC
roc_a <- roc(test_a$Y, prob_test_a, quiet = TRUE)
roc_df_a <- data.frame(spec_inv = 1 - roc_a$specificities, sens = roc_a$sensitivities)
ggplot(roc_df_a, aes(x = spec_inv, y = sens)) +
  geom_path(color = "#4A90B8", linewidth = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  annotate("text", x = 0.65, y = 0.25,
           label = sprintf("AUC = %.3f", met_rfa_test$AUC),
           size = 5, color = "#4A90B8") +
  labs(title = "Corba ROC — RF-A", x = "1 - Especificitat", y = "Sensibilitat") +
  theme_minimal(base_size = 13)

#### ============================================================ ####
####                   2. RANDOM FOREST B                         ####
#### ============================================================ ####

cat("\n===============================================================\n")
cat("                       2. RANDOM FOREST B                        \n")
cat("=================================================================\n\n")

# --------------- 2.1. Tunning d'hiperparametres ---------------

p_b <- ncol(train_b) - 1
mtry_vals_b <- unique(c(floor(sqrt(p_b)), floor(p_b / 3), floor(p_b / 2), 20, p_b))

cat(sprintf("p = %d predictors | mtry testats: %s | min.node.size: %s | num.trees: %s\n\n",
            p_b, paste(mtry_vals_b, collapse = ", "),
            paste(node_vals, collapse = ", "),
            paste(num_trees, collapse = ", ")))

grid_b <- expand.grid(mtry = mtry_vals_b, node = node_vals, trees = num_trees)
grid_b$OOB <- NA_real_
grid_b$AUC_oob <- NA_real_

# --------------- 2.2. Exploració d'hiperparametres ---------------


set.seed(1234)
for (i in seq_len(nrow(grid_b))) {
  rf_tmp <- ranger(
    x = as.matrix(train_b[, -1]),
    y = factor(train_b$Y, levels = c(0, 1)),
    num.trees = grid_b$trees[i],
    mtry = grid_b$mtry[i],
    min.node.size = grid_b$node[i],
    case.weights = case_weights_b,
    importance = "none",
    probability = TRUE,
    seed = 1234
  )
  prob_oob_tmp <- rf_tmp$predictions[, 2]
  grid_b$AUC_oob[i] <- round(as.numeric(auc(roc(train_b$Y, prob_oob_tmp, quiet = TRUE))), 4)
}

best_b <- grid_b %>% arrange(desc(AUC_oob)) %>% slice(1)
cat(sprintf("Millors hiperparametres RF-B: mtry=%d | min.node.size=%d | num.trees=%d | AUC OOB=%.4f\n\n",
            best_b$mtry, best_b$node, best_b$trees, best_b$AUC_oob))

# --------------- 2.3. Model final ---------------

set.seed(1234)
rf_b <- ranger(
  x = as.matrix(train_b[, -1]),
  y = factor(train_b$Y, levels = c(0, 1)),
  num.trees = best_b$trees,
  mtry = best_b$mtry,
  min.node.size = best_b$node,
  case.weights = case_weights_b,
  importance = "permutation",
  probability = TRUE,
  seed = 1234
)

prob_test_b <- predict(rf_b, data = as.matrix(test_b[, -1]))$predictions[, 2]
prob_oob_b <- rf_b$predictions[, 2]

pr_rfb <- seleccionar_llindar_pr(prob_test_b, test_b$Y, MIN_RECALL)
thresh_pr_b <- pr_rfb$threshold
cat(sprintf("RF-B — AUPRC: %.4f | Llindar PR: %.4f | recall_ok: %s\n\n",
            pr_rfb$auprc, thresh_pr_b,
            ifelse(pr_rfb$recall_ok, "SI", "NO (fallback Youden)")))

met_rfb_test <- calcular_metriques_rf(prob_test_b, test_b$Y, "RF-B (test)",
                                      rf_b$prediction.error,
                                      thresh_override = thresh_pr_b)
met_rfb_train <- calcular_metriques_rf(prob_oob_b, train_b$Y, "RF-B (OOB train)",
                                       thresh_override = thresh_pr_b)

mostrar_metriques_rf(met_rfb_test)
mostrar_metriques_rf(met_rfb_train)

cat("\n--- Overfitting OOB vs test ---\n\n")
df_ov_b <- data.frame(
  Conjunt = c("Train (OOB)", "Test"),
  AUC = c(met_rfb_train$AUC, met_rfb_test$AUC),
  Accuracy = c(met_rfb_train$accuracy, met_rfb_test$accuracy),
  F1 = c(met_rfb_train$F1, met_rfb_test$F1),
  Balanced_Acc = c(met_rfb_train$balanced_accuracy, met_rfb_test$balanced_accuracy)
)
print(df_ov_b, row.names = FALSE)
# tampoc hi ha overfitting

print(grafic_cm(met_rfb_test, "RF-B"))

#### ============================================================ ####
####               3. IMPORTÀNCIA DE VARIABLES                    ####
#### ============================================================ ####

# mirem importancia de les variables amb permutacions
# mirem cuant cau l'accuracy al permutar una variable, si no cau gaire -> irrellevant
# si cau -> rellevant

cat("\n=================================================================\n")
cat("                      3. IMPORTANCIA DE VARIABLES                  \n")
cat("===================================================================\n\n")

imp_a <- tibble(variable = names(rf_a$variable.importance),
                importancia = rf_a$variable.importance,
                model = "RF-A") %>% arrange(desc(importancia))

imp_b <- tibble(variable = names(rf_b$variable.importance),
                importancia = rf_b$variable.importance,
                model = "RF-B") %>% arrange(desc(importancia))

cat("Top 15 RF-A:\n") # top 15 vas més importants
print(imp_a %>% slice_head(n = 15))

cat("\nTop 15 RF-B:\n")
print(imp_b %>% slice_head(n = 15))

ggplot(imp_a %>% slice_head(n = 15),
       aes(x = reorder(variable, importancia), y = importancia, fill = importancia)) +
  geom_col(alpha = 0.9) +
  coord_flip() +
  scale_fill_gradient(low = "#AED6F1", high = "#1A5276", guide = "none") +
  labs(title = "Importancia variables — RF-A (factors EFA)",
       subtitle = "Top 15 | mesura: disminucio accuracy per permutacio",
       x = "", y = "Importancia (permutacio)") +
  theme_minimal(base_size = 13)

ggplot(imp_b %>% slice_head(n = 15),
       aes(x = reorder(variable, importancia), y = importancia, fill = importancia)) +
  geom_col(alpha = 0.9) +
  coord_flip() +
  scale_fill_gradient(low = "#A9DFBF", high = "#1E8449", guide = "none") +
  labs(title = "Importancia variables — RF-B (Likert originals)",
       subtitle = "Top 15 | mesura: disminucio accuracy per permutacio",
       x = "", y = "Importancia (permutacio)") +
  theme_minimal(base_size = 13)

#### ============================================================ ####
####                     4. SHAP VALUES                           ####
#### ============================================================ ####

cat("\n======================================================\n")
cat("                     4. SHAP VALUES                   \n")
cat("======================================================\n\n")

# Funció de predicció per fastshap: retorna P(Regular=1)
pfun <- function(object, newdata) {
  predict(object, data = as.matrix(newdata))$predictions[, 2]
}

# --------------- 4.1. Càlcul shap ---------------

# per cada observació del test i cada variable, calcula un valor SHAP 
# = quant contribueix aquesta variable a emputxar la predicció cap a 
# Regular o Irregular respecte a la predicció mitjana

set.seed(1234)
shap_df_a <- as.data.frame(
  fastshap::explain(rf_a,
                    X = as.data.frame(train_a[, -1]),
                    pred_wrapper = pfun,
                    nsim = 100,
                    newdata = as.data.frame(test_a[, -1]))
)

set.seed(1234)
shap_df_b <- as.data.frame(
  fastshap::explain(rf_b,
                    X = as.data.frame(train_b[, -1]),
                    pred_wrapper = pfun,
                    nsim = 100,
                    newdata = as.data.frame(test_b[, -1]))
)

# --------------- 4.2. Importància SHAP global ---------------

# és la mitjana del valor absolut dels SHAP values per cada 
# variable sobre totes les observacions del test

shap_imp_a <- tibble(
  variable = names(shap_df_a),
  mean_abs_shap = colMeans(abs(shap_df_a))
) %>% arrange(desc(mean_abs_shap))

shap_imp_b <- tibble(
  variable = names(shap_df_b),
  mean_abs_shap = colMeans(abs(shap_df_b))
) %>% arrange(desc(mean_abs_shap))

cat("Top 15 SHAP RF-A:\n")
print(shap_imp_a %>% slice_head(n = 15))
cat("\nTop 15 SHAP RF-B:\n")
print(shap_imp_b %>% slice_head(n = 15))

# Grafic importancia SHAP RF-A
ggplot(shap_imp_a %>% slice_head(n = 15),
       aes(x = reorder(variable, mean_abs_shap), y = mean_abs_shap,
           fill = mean_abs_shap)) +
  geom_col(alpha = 0.9) +
  coord_flip() +
  scale_fill_gradient(low = "#AED6F1", high = "#1A5276", guide = "none") +
  labs(title = "Importancia SHAP — RF-A (factors EFA)",
       subtitle = "Top 15 | mean(|SHAP|) sobre conjunt test",
       x = "", y = "Importancia SHAP") +
  theme_minimal(base_size = 13)

# Grafic importancia SHAP RF-B
ggplot(shap_imp_b %>% slice_head(n = 15),
       aes(x = reorder(variable, mean_abs_shap), y = mean_abs_shap,
           fill = mean_abs_shap)) +
  geom_col(alpha = 0.9) +
  coord_flip() +
  scale_fill_gradient(low = "#A9DFBF", high = "#1E8449", guide = "none") +
  labs(title = "Importancia SHAP — RF-B (Likert originals)",
       subtitle = "Top 15 | mean(|SHAP|) sobre conjunt test",
       x = "", y = "Importancia SHAP") +
  theme_minimal(base_size = 13)

# --------------- 4.3. SHAP Beeswarm ---------------
# transforma els dades per poder pintar un punt per cada 
# combinació observació × variable, i pintar segons el valor 
# real de la variable.

top12_a <- shap_imp_a$variable[1:min(12, nrow(shap_imp_a))]

shap_long_a <- shap_df_a %>%
  dplyr::select(all_of(top12_a)) %>%
  mutate(obs = row_number()) %>%
  pivot_longer(-obs, names_to = "variable", values_to = "shap") %>%
  left_join(
    as.data.frame(test_a[, -1]) %>%
      dplyr::select(all_of(top12_a)) %>%
      mutate(obs = row_number()) %>%
      pivot_longer(-obs, names_to = "variable", values_to = "valor"),
    by = c("obs", "variable")
  ) %>%
  mutate(variable = factor(variable, levels = rev(top12_a)))

ggplot(shap_long_a, aes(x = shap, y = variable, color = valor)) +
  geom_jitter(height = 0.25, size = 1.2, alpha = 0.6) +
  geom_vline(xintercept = 0, color = "grey40", linewidth = 0.8) +
  scale_color_gradient(low = "#2471A3", high = "#E74C3C",
                       name = "Valor variable") +
  labs(title = "SHAP Beeswarm — RF-A (factors EFA)",
       subtitle = "x > 0 augmenta P(Regular) | color = valor de la variable",
       x = "Valor SHAP", y = "") +
  theme_minimal(base_size = 12)

# SHAP Beeswarm RF-B top 12
top12_b <- shap_imp_b$variable[1:min(12, nrow(shap_imp_b))]

shap_long_b <- shap_df_b %>%
  dplyr::select(all_of(top12_b)) %>%
  mutate(obs = row_number()) %>%
  pivot_longer(-obs, names_to = "variable", values_to = "shap") %>%
  left_join(
    as.data.frame(test_b[, -1]) %>%
      dplyr::select(all_of(top12_b)) %>%
      mutate(obs = row_number()) %>%
      pivot_longer(-obs, names_to = "variable", values_to = "valor"),
    by = c("obs", "variable")
  ) %>%
  mutate(variable = factor(variable, levels = rev(top12_b)))

ggplot(shap_long_b, aes(x = shap, y = variable, color = valor)) +
  geom_jitter(height = 0.25, size = 1.2, alpha = 0.6) +
  geom_vline(xintercept = 0, color = "grey40", linewidth = 0.8) +
  scale_color_gradient(low = "#2471A3", high = "#E74C3C",
                       name = "Valor variable") +
  labs(title = "SHAP Beeswarm — RF-B (Likert originals)",
       subtitle = "x > 0 augmenta P(Regular) | color = valor de la variable",
       x = "Valor SHAP", y = "") +
  theme_minimal(base_size = 12)

# --------------- 4.4. SHAP gràfics dependència ---------------

# per cadascuna de les 4 variables més importants
# fa un scatter plot de valor real de la variable (eix X) 
# vs valor SHAP (eix Y), amb una corba LOESS a sobre.

top4_a <- shap_imp_a$variable[1:4]

for (v in top4_a) {
  df_dep <- data.frame(
    valor = as.data.frame(test_a[, -1])[[v]],
    shap = shap_df_a[[v]]
  )
  p <- ggplot(df_dep, aes(x = valor, y = shap)) +
    geom_point(alpha = 0.5, color = "#4A90B8", size = 1.8) +
    geom_smooth(method = "loess", se = TRUE, color = "#E07B54",
                fill = "#F0B27A", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    labs(title = sprintf("SHAP Dependence Plot RF-A — %s", v),
         subtitle = "Linia taronja = tendencia LOESS | y > 0 augmenta P(Regular)",
         x = v, y = "Valor SHAP") +
    theme_minimal(base_size = 13)
  print(p)
}

#### ============================================================ ####
####        6. TAULA COMPARATIVA FINAL: Logit vs RF-A vs RF-B     ####
#### ============================================================ ####

cat("\n====================================\n")
cat("         6. TAULA COMPARATIVA FINAL   \n")
cat("======================================\n\n")

# Carregar metriques del logit si existeix
if (file.exists("2. Dades/metriques_logit.rds")) {
  met_logit <- readRDS("2. Dades/metriques_logit.rds")

  df_comp <- data.frame(
    Model = c(met_logit$model, "RF-A (factors EFA)", "RF-B (Likert orig)"),
    AUC_test = c(met_logit$AUC, met_rfa_test$AUC, met_rfb_test$AUC),
    Accuracy = c(met_logit$accuracy, met_rfa_test$accuracy, met_rfb_test$accuracy),
    Precision = c(met_logit$precision, met_rfa_test$precision, met_rfb_test$precision),
    Recall = c(met_logit$recall, met_rfa_test$recall, met_rfb_test$recall),
    F1 = c(met_logit$F1, met_rfa_test$F1, met_rfb_test$F1),
    Balanced_Acc = c(met_logit$balanced_accuracy,
                     met_rfa_test$balanced_accuracy,
                     met_rfb_test$balanced_accuracy),
    stringsAsFactors = FALSE
  )
  print(df_comp, row.names = FALSE)

  # Grafic comparatiu
  df_comp_long <- df_comp %>%
    pivot_longer(-Model, names_to = "metrica", values_to = "valor") %>%
    mutate(metrica = factor(metrica,
                            levels = c("AUC_test", "Balanced_Acc", "F1",
                                       "Accuracy", "Precision", "Recall")))

  ggplot(df_comp_long, aes(x = metrica, y = valor, fill = Model)) +
    geom_col(position = "dodge", alpha = 0.85) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50") +
    scale_fill_manual(values = c("#4A90B8", "#E07B54", "#8E6BBF")) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(title = "Comparacio de models: Logit vs RF-A vs RF-B",
         subtitle = "Metriques sobre conjunt test",
         x = "", y = "Valor") +
    theme_minimal(base_size = 13) +
    theme(axis.text.x = element_text(angle = 25, hjust = 1),
          legend.position = "bottom")
}

# sembla que el millor és el logit

saveRDS(met_rfa_test, "2. Dades/metriques_rf_a.rds")
saveRDS(met_rfb_test, "2. Dades/metriques_rf_b.rds")

# --- Guardar probabilitats i bbdd encadenada ---
vars_rfa_ok <- vars_rfa[vars_rfa %in% names(dades_rf)]
X_all_a <- dades_rf %>%
  dplyr::select(all_of(vars_rfa_ok)) %>%
  mutate(across(everything(), as.numeric))
complete_a <- complete.cases(X_all_a)
prob_rfa_tots <- rep(NA_real_, nrow(X_all_a))
prob_rfa_tots[complete_a] <- predict(
  rf_a, data = as.matrix(X_all_a[complete_a, ]))$predictions[, 2]
dades_def$prob_rf_a <- NA_real_
dades_def$prob_rf_a[seq_len(nrow(dades_rf))] <- prob_rfa_tots

vars_rfb_ok <- vars_rfb[vars_rfb %in% names(dades_rf)]
X_all_b <- dades_rf %>%
  dplyr::select(all_of(vars_rfb_ok)) %>%
  mutate(across(everything(), as.numeric))
complete_b <- complete.cases(X_all_b)
prob_rfb_tots <- rep(NA_real_, nrow(X_all_b))
prob_rfb_tots[complete_b] <- predict(
  rf_b, data = as.matrix(X_all_b[complete_b, ]))$predictions[, 2]
dades_def$prob_rf_b <- NA_real_
dades_def$prob_rf_b[seq_len(nrow(dades_rf))] <- prob_rfb_tots

save(dades_def, file = "2. Dades/6. Dades RF.RData")
cat("\n-> dades_def amb prob_rf_a i prob_rf_b guardades a: 2. Dades/6. Dades RF.RData\n")

sink()
dev.off()
