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


vars_fa <- c("MOT_DESMOTIVACIO", "MOT_AUTOGESTIO", "MOT_FORCA_MAJOR",
             "EST_QUALITAT_DOC", "EST_AVALUACIO_AC", "EST_TEMPS_CLASSE",
             "EST_GRUPS_REDUITS", "IA_EINA_ESTUDI", "IA_SUBSTITUCIO")
vars_acad <- c("NOTA_num", "T_AVAL_num", "CURS_1R_num", "N_ASSIG")
vars_pers <- c("EDAT", "DESPL")

setwd("C:/Users/edurn/Downloads/TFG")
#setwd("C:/Users/Edurne/Downloads/TFG")

load("2. Dades/8. Dades CatBoost.RData")
model_seleccionat <- readRDS("2. Dades/model_logit.rds")
source("3. Codi/Funcions models.R")

sink("4. Outputs/9.1 Output_text_logit_millorat.txt")
pdf("4. Outputs/9.2 Output_grafics_logit_millorat.pdf", width = 10, height = 8)

MIN_RECALL <- 0.6

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
####                  1. CONTRAST LINEALITAT                      ####
#### ============================================================ ####

# --------- 1.2 Contrast linealitat IA_SUBST ---------

cat("\n========== 1.1 CONTRAST LINEALITAT IA_SUBST ==========\n\n")

# primer fem el model quadràtic
formula_lq <- update(formula_base, . ~ . - IA_SUBST_num + poly(IA_SUBST_num, 2))
model_lq   <- glm(formula_lq, data = dades_train, family = binomial)

coef_lq <- coef(summary(model_lq))
p_L <- coef_lq["poly(IA_SUBST_num, 2)1", "Pr(>|z|)"] # significió component lineal
p_Q <- coef_lq["poly(IA_SUBST_num, 2)2", "Pr(>|z|)"] # significació component quadràtic

cat(sprintf("Component lineal   (L): z = %.3f | p = %.4f\n",
            coef_lq["poly(IA_SUBST_num, 2)1", "z value"], p_L))
cat(sprintf("Component quadratic (Q): z = %.3f | p = %.4f\n",
            coef_lq["poly(IA_SUBST_num, 2)2", "z value"], p_Q))

# fem LRT per comparar model base vs model quadràtic
lrt_lq <- anova(model_seleccionat, model_lq, test = "LRT")
cat("\nLRT: model_seleccionat vs. model amb poly(IA_SUBST_num, 2):\n")
print(lrt_lq)

# Grafic de probabilitat predicta per IA_SUBST_num
# fem un gràfic creant un individu promig amb la mediana de les altres variables i variem IA_SUBST_num
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

# les línies són pràctiment idèntics, el component quadràtic no aporta millora 
# i el LRT no és significatiu

cat(sprintf("\nConclusió: component quadratic %s (p = %.4f)\n\n",
            ifelse(p_Q < 0.05, "SIGNIFICATIU -> incorporar al model", "no significatiu"),
            p_Q))

# --------- 1.2 Contrast linealitat NOTA_num ---------
cat("\n========== 1.2 CONTRAST LINEALITAT NOTA_num ==========\n\n")

if ("NOTA_num" %in% all.vars(formula_base)) {
  formula_nota_q <- update(formula_base, . ~ . - NOTA_num + poly(NOTA_num, 2))
  model_nota_q   <- glm(formula_nota_q, data = dades_train, family = binomial)

  coef_nota  <- coef(summary(model_nota_q))
  p_L_nota   <- coef_nota["poly(NOTA_num, 2)1", "Pr(>|z|)"]
  p_Q_nota   <- coef_nota["poly(NOTA_num, 2)2", "Pr(>|z|)"]

  cat(sprintf("Component lineal   (L): z = %.3f | p = %.4f\n",
              coef_nota["poly(NOTA_num, 2)1", "z value"], p_L_nota))
  cat(sprintf("Component quadratic (Q): z = %.3f | p = %.4f\n",
              coef_nota["poly(NOTA_num, 2)2", "z value"], p_Q_nota))

  lrt_nota <- anova(model_seleccionat, model_nota_q, test = "LRT")
  cat("\nLRT: model_seleccionat vs. model amb poly(NOTA_num, 2):\n")
  print(lrt_nota)

  nota_grid <- data.frame(
    NOTA_num = seq(min(dades_train$NOTA_num, na.rm = TRUE),
                   max(dades_train$NOTA_num, na.rm = TRUE),
                   length.out = 100)
  )
  other_vars_nota <- setdiff(all.vars(formula_base)[-1], "NOTA_num")
  for (v in other_vars_nota) {
    col <- dades_train[[v]]
    nota_grid[[v]] <- if (is.numeric(col)) median(col, na.rm = TRUE) else {
      lvl <- names(sort(table(col), decreasing = TRUE))[1]
      if (is.factor(col)) factor(lvl, levels = levels(col)) else lvl
    }
  }

  nota_grid$prob_lin <- predict(model_seleccionat, newdata = nota_grid, type = "response")
  nota_grid$prob_q   <- predict(model_nota_q,      newdata = nota_grid, type = "response")

  df_nota_plot <- nota_grid %>%
    dplyr::select(NOTA_num, prob_lin, prob_q) %>%
    pivot_longer(cols = c(prob_lin, prob_q),
                 names_to = "model", values_to = "prob") %>%
    mutate(model = ifelse(model == "prob_lin", "Lineal", "Lineal + Quadratic"))

    ggplot(df_nota_plot, aes(x = NOTA_num, y = prob, color = model, linetype = model)) +
      geom_line(linewidth = 1.1) +
      scale_color_manual(values = c("Lineal" = "#4A90B8", "Lineal + Quadratic" = "#E07B54")) +
      labs(title = "Efecte de NOTA_num sobre P(Regular)",
           subtitle = "Medianes de la resta de predictors",
           x = "Nota (numèric)", y = "P(Regular >= 80%)",
           color = NULL, linetype = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")

  cat(sprintf("\nConclusió NOTA_num: component quadratic %s (p = %.4f)\n\n",
              ifelse(p_Q_nota < 0.05,
                     "SIGNIFICATIU -> considerar poly(2)",
                     "no significatiu"),
              p_Q_nota))
} else {
  cat("NOTA_num no es troba a formula_base — contrast omès.\n\n")
}

# tampoc és significatiu però veiem que les línies no són tan idèntiques com en el cas d'IA_SUBST_num, 

# ----------- 1.3 Contrast linealitat MOT_DESMOTIVACIO -----------
cat("\n========== 1.3 CONTRAST LINEALITAT MOT_DESMOTIVACIO ==========\n\n")

if ("MOT_DESMOTIVACIO" %in% all.vars(formula_base)) {
  formula_mot_q <- update(formula_base, . ~ . - MOT_DESMOTIVACIO + poly(MOT_DESMOTIVACIO, 2))
  model_mot_q   <- glm(formula_mot_q, data = dades_train, family = binomial)

  coef_mot <- coef(summary(model_mot_q))
  p_L_mot  <- coef_mot["poly(MOT_DESMOTIVACIO, 2)1", "Pr(>|z|)"]
  p_Q_mot  <- coef_mot["poly(MOT_DESMOTIVACIO, 2)2", "Pr(>|z|)"]

  cat(sprintf("Component lineal   (L): z = %.3f | p = %.4f\n",
              coef_mot["poly(MOT_DESMOTIVACIO, 2)1", "z value"], p_L_mot))
  cat(sprintf("Component quadratic (Q): z = %.3f | p = %.4f\n",
              coef_mot["poly(MOT_DESMOTIVACIO, 2)2", "z value"], p_Q_mot))

  lrt_mot <- anova(model_seleccionat, model_mot_q, test = "LRT")
  cat("\nLRT: model_seleccionat vs. model amb poly(MOT_DESMOTIVACIO, 2):\n")
  print(lrt_mot)

  mot_grid <- data.frame(
    MOT_DESMOTIVACIO = seq(min(dades_train$MOT_DESMOTIVACIO, na.rm = TRUE),
                           max(dades_train$MOT_DESMOTIVACIO, na.rm = TRUE),
                           length.out = 100)
  )
  other_vars_mot <- setdiff(all.vars(formula_base)[-1], "MOT_DESMOTIVACIO")
  for (v in other_vars_mot) {
    col <- dades_train[[v]]
    mot_grid[[v]] <- if (is.numeric(col)) median(col, na.rm = TRUE) else {
      lvl <- names(sort(table(col), decreasing = TRUE))[1]
      if (is.factor(col)) factor(lvl, levels = levels(col)) else lvl
    }
  }

  mot_grid$prob_lin <- predict(model_seleccionat, newdata = mot_grid, type = "response")
  mot_grid$prob_q   <- predict(model_mot_q,       newdata = mot_grid, type = "response")

  df_mot_plot <- mot_grid %>%
    dplyr::select(MOT_DESMOTIVACIO, prob_lin, prob_q) %>%
    pivot_longer(cols = c(prob_lin, prob_q),
                 names_to = "model", values_to = "prob") %>%
    mutate(model = ifelse(model == "prob_lin", "Lineal", "Lineal + Quadratic"))

    ggplot(df_mot_plot, aes(x = MOT_DESMOTIVACIO, y = prob, color = model, linetype = model)) +
      geom_line(linewidth = 1.1) +
      scale_color_manual(values = c("Lineal" = "#4A90B8", "Lineal + Quadratic" = "#E07B54")) +
      labs(title = "Efecte de MOT_DESMOTIVACIO sobre P(Regular)",
           subtitle = "Medianes de la resta de predictors",
           x = "MOT_DESMOTIVACIO (score EFA)", y = "P(Regular >= 80%)",
           color = NULL, linetype = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")

  cat(sprintf("\nConclusió MOT_DESMOTIVACIO: component quadratic %s (p = %.4f)\n\n",
              ifelse(p_Q_mot < 0.05,
                     "SIGNIFICATIU -> considerar poly(2)",
                     "no significatiu"),
              p_Q_mot))
} else {
  cat("MOT_DESMOTIVACIO no es troba a formula_base — contrast omès.\n\n")
}

# tampoc és significatiu

#### ============================================================ ####
####          2. TEST D'INTERACCIONS (LRT)                        ####
#### ============================================================ ####

# mirarem interaccions amb test de LRT

# ------ 2.1 Mirar interaccions significatives ------
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
df_inter$Significativa <- ifelse(!is.na(df_inter$p_LRT) & df_inter$p_LRT < 0.05, "Si", "No")

cat("Taula de tests d'interaccio:\n\n")
print(df_inter, row.names = FALSE)
cat("\n")

# ------ 2.2 Test niat per interaccions ------
cat("--- Test niat: CURS_1R × MOT_DESMOTIVACIO vs. + IA_SUBST_num × CURS_1R ---\n\n")
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
# p-valor

# si hi ha interaccions significatives, mostrem els coeficients i estadistics dels termes d'interaccio
if (length(sig_inter_noms) > 0) {
  cat("Interaccions significatives:\n")
  for (nom in sig_inter_noms) {
    cat(sprintf("  -> %s\n", nom))
    form_i  <- formules_inter[[nom]]
    model_i <- glm(form_i, data = dades_train, family = binomial)
    coef_i  <- coef(summary(model_i))
    inter_parts <- strsplit(gsub(" x ", ":", nom), ":")[[1]]
    inter_rows  <- which(rowSums(sapply(inter_parts, function(p)
      grepl(p, rownames(coef_i), fixed = TRUE))) == length(inter_parts))
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

# Grafic especific per IA_SUBST_num x CURS_1R si es significativa
if ("IA_SUBST_num x CURS_1R" %in% sig_inter_noms) {
  model_ia_curs <- glm(formules_inter[["IA_SUBST_num x CURS_1R"]],
                       data = dades_train, family = binomial)
  curs_vals_ia <- sort(unique(dades_train$CURS_1R))
  grid_ia_curs <- expand.grid(
    IA_SUBST_num = seq(1, 6, by = 0.1),
    CURS_1R      = curs_vals_ia
  )
  other_v2 <- setdiff(all.vars(formula_base)[-1], c("IA_SUBST_num", "CURS_1R"))
  for (v in other_v2) {
    col <- dades_train[[v]]
    grid_ia_curs[[v]] <- if (is.numeric(col)) median(col, na.rm = TRUE) else {
      lvl <- names(sort(table(col), decreasing = TRUE))[1]
      if (is.factor(col)) factor(lvl, levels = levels(col)) else lvl
    }
  }
  grid_ia_curs$prob       <- predict(model_ia_curs, newdata = grid_ia_curs, type = "response")
  grid_ia_curs$CURS_label <- factor(
    ifelse(as.character(grid_ia_curs$CURS_1R) %in% c("1", "SI", "TRUE", "1r"),
           "1r curs", "Altres cursos")
  )

    ggplot(grid_ia_curs, aes(x = IA_SUBST_num, y = prob, color = CURS_label)) +
      geom_line(linewidth = 1.2) +
      scale_color_manual(values = c("1r curs" = "#E07B54", "Altres cursos" = "#4A90B8")) +
      labs(title = "Interaccio IA_SUBST_num x CURS_1R",
           subtitle = "Probabilitat predicta de Regular (>=80%) | resta de predictors a la mediana",
           x = "IA_SUBST (1=Baix, 6=Alt)", y = "P(Regular >= 80%)",
           color = NULL) +
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

  print(
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
  )
}

# no afegim interaccio IA:CURS_1r perquè el test niuat mostra 
# que no aporta millora sobre afegir només DESM:CURS_1R

#### ============================================================ ####
####                     3. MODEL MILLORAT                        ####
#### ============================================================ ####

cat("\n========== 3. CONSTRUCCIÓ DEL MODEL MILLORAT ==========\n\n")

formula_mil <- formula_base

# Afegir component quadratic si significatiu
if (p_Q < 0.05) {
  formula_mil <- update(formula_mil, . ~ . - IA_SUBST_num + poly(IA_SUBST_num, 2))
  cat("-> Afegit poly(IA_SUBST_num, 2) (Q significatiu)\n")
}

# Afegir interaccions significatives
# s'exclou IA_SUBST_num:CURS_1R encara que sigui significativa per separat.
inter_excloses <- c("IA_SUBST_num:CURS_1R")

if (length(sig_inter_noms) > 0) {
  for (nom in sig_inter_noms) {
    inter_term <- gsub(" x ", ":", nom)
    if (inter_term %in% inter_excloses) {
      cat(sprintf("-> Exclosa interaccio %s (test niuat: no aporta sobre DESM:CURS_1R)\n",
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
####                        4. REPEATED 5x10-fold CV                   ####
#### ============================================================ ####

cat("\n========== 4. Mètriques (TRAIN) ==========\n")
cat("       Repeated 5x10-fold CV \n\n")


set.seed(1234)
n_rep  <- 5
n_fold <- 10
cv_rows_mil <- vector("list", n_rep * n_fold)
k <- 0
oof_probs_thresh <- rep(NA_real_, nrow(dades_train))

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
    if (r == 1) oof_probs_thresh[test_idx] <- prob_cv  # guardem laes prediciccions 

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

# Threshold en les prediccions OOF
cc_oof <- !is.na(oof_probs_thresh)
pr_oof_thresh <- seleccionar_llindar_pr(
  oof_probs_thresh[cc_oof], dades_train$Y[cc_oof], MIN_RECALL
)
thresh_cv_oof <- pr_oof_thresh$threshold
cat(sprintf("Threshold OOF (10-fold, 1a repeticio): %.4f | recall_ok (>= %.2f): %s\n\n",
            thresh_cv_oof, MIN_RECALL,
            ifelse(pr_oof_thresh$recall_ok, "SI", "NO (fallback Youden)")))

#### ============================================================ ####
####                     5. MÈTRIQUES (TEST)                      ####
#### ============================================================ ####

cat("\n========== 5. METRIQUES SOBRE TEST ==========\n\n")

prob_test_mil <- predict(model_millorat, newdata = dades_test, type = "response")

# Threshold seleccionat per OOF CV (no sobre test)
thresh_pr_mil <- thresh_cv_oof

# PR curve del test per visualitzacio (AUPRC es threshold-free)
pr_test_vis <- seleccionar_llindar_pr(prob_test_mil, dades_test$Y, MIN_RECALL)

# Precisio i recall al threshold OOF aplicat sobre test
pred_test_at_oof <- as.integer(prob_test_mil >= thresh_pr_mil)
TP_t <- sum(pred_test_at_oof == 1 & dades_test$Y == 1)
FP_t <- sum(pred_test_at_oof == 1 & dades_test$Y == 0)
FN_t <- sum(pred_test_at_oof == 0 & dades_test$Y == 1)
prec_at_thresh <- if (TP_t + FP_t > 0) TP_t / (TP_t + FP_t) else NA_real_
rec_at_thresh  <- if (TP_t + FN_t > 0) TP_t / (TP_t + FN_t) else NA_real_

# pr_mil: contenidor per compatibilitat amb codi posterior
pr_mil <- pr_test_vis
pr_mil$threshold <- thresh_pr_mil
pr_mil$precision <- prec_at_thresh
pr_mil$recall <- rec_at_thresh

cat(sprintf("AUPRC (test): %.4f\n", pr_test_vis$auprc))
cat(sprintf("Llindar OOF: %.4f | recall_ok (>= %.2f): %s\n",
            thresh_pr_mil, MIN_RECALL,
            ifelse(pr_oof_thresh$recall_ok, "SI", "NO (fallback Youden)")))
cat(sprintf("Precisio test al llindar OOF: %.4f | Recall test: %.4f\n\n",
            prec_at_thresh, rec_at_thresh))

print(
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
         subtitle = sprintf("AUPRC = %.4f | Llindar OOF = %.4f",
                            pr_test_vis$auprc, thresh_pr_mil),
         x = "Recall", y = "Precisio (PPV)") +
    theme_minimal(base_size = 13)
)

# Referencia: Precisio del CV (estimacio sense biaix de seleccio de llindar)
prec_cv_r <- df_cv_mil_resum[df_cv_mil_resum$Metrica == "Precision", ]
cat(sprintf("Precisio CV (5x10-fold): %.4f +/- %.4f [IC95%% %.4f, %.4f]\n",
            prec_cv_r$Mitjana, prec_cv_r$SD, prec_cv_r$IC_2.5, prec_cv_r$IC_97.5))
cat("NOTA: el llindar s'ha seleccionat per OOF CV (no sobre test).\n")

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
  cat("El llindar ve de OOF CV; comprova que el model no esta sobreajustant al train.\n")
  cat(sprintf("Precisio CV (referencia): %.3f\n", prec_cv_r$Mitjana))
}
cat("\n")

# Bootstrap IC per la precisió
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
####                        6. CALIBRATION PLOT                   ####
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
####                   7. EFECTES MARGINALS PROMIG                ####
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
####                      8. COMPARACIÓ MODELS                    ####
#### ============================================================ ####

cat("\n========== 8. COMPARACIÓ MODELS ==========\n\n")

models_llista <- list()

fitxers <- c(
  Logit = "2. Dades/metriques_logit.rds",
  `RF-A` = "2. Dades/metriques_rf_a.rds",
  `RF-B` = "2. Dades/metriques_rf_b.rds",
  XGBoost = "2. Dades/metriques_xgb.rds",
  Logit_Millorat = "2. Dades/metriques_logit_millorat.rds"
)

for (nom in names(fitxers)) {
  if (file.exists(fitxers[[nom]])) {
    models_llista[[nom]] <- readRDS(fitxers[[nom]])
  }
}
models_llista[["Logit_Millorat"]] <- metriques_mil

df_comp <- do.call(rbind, lapply(models_llista, extreure_fila))
rownames(df_comp) <- NULL

cat("Taula comparativa de models (sobre conjunt test):\n\n")
print(df_comp, row.names = FALSE)

metriques_num <- c("AUC_test", "Balanced_Acc", "F1", "Accuracy", "Precision", "Recall")

df_comp_long <- df_comp %>%
  select(Model, all_of(metriques_num)) %>%
  mutate(across(-Model, as.numeric)) %>%
  pivot_longer(-Model, names_to = "metrica", values_to = "valor") %>%
  mutate(metrica = factor(metrica, levels = metriques_num))

colors_models <- c("#4A90B8", "#E07B54", "#8E6BBF", "#2ECC71",
                   "#E74C3C", "#F39C12", "#1F3A93")[seq_len(n_distinct(df_comp_long$Model))]

print(
  ggplot(df_comp_long, aes(x = metrica, y = valor, fill = Model)) +
    geom_col(position = "dodge", alpha = 0.85) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50") +
    scale_fill_manual(values = colors_models) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(title = "Comparacio de models",
         subtitle = "Metriques sobre conjunt test",
         x = "", y = "Valor") +
    theme_minimal(base_size = 13) +
    theme(axis.text.x = element_text(angle = 25, hjust = 1),
          legend.position = "bottom")
)


#### ============================================================ ####
####               9. GUARDAR MODEL, PROBABILITATS I BBDD         ####
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

sink()
dev.off()

