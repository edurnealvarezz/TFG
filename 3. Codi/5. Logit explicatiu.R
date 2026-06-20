packages <- c("dplyr", "ggplot2", "tidyr", "car", "pROC", "PRROC",
              "ResourceSelection", "tibble", "caret", "marginaleffects", "jsonlite")

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

load("2. Dades/4. Dades EFA.RData")
source("3. Codi/Funcions models.R")

sink("4. Outputs/5. Logit explicatiu/5.1 Output_text_logit.txt")
png("4. Outputs/5. Logit explicatiu/grafic_%02d.png", width = 8, height = 6, units = "in", res = 300)


#### ============================================================ ####
####                   0. PREPARACIÓ DE DADES                     ####
#### ============================================================ ####

# Models estimats sobre TOTES les dades — objectiu: explicar, no predir
# La partició train/test s'usa NOMÉS a 5b. Logit prediccio.R

dades_mod <- dades_def %>%
  mutate(Y = as.integer(GRUP_ASSIST == "Regular (≥80%)")) %>%
  filter(!is.na(Y))

dades_mod$NOTA_num     <- as.numeric(dades_mod$NOTA)
dades_mod$IA_SUBST_num <- as.numeric(dades_mod$IA_SUBST)
dades_mod$IA_ATENC_num <- as.numeric(dades_mod$IA_ATENC)

cat("==== 0. DISTRIBUCIÓ GRUP_ASSIST ======= \n")
print(table(dades_mod$GRUP_ASSIST))
cat(sprintf("\nProporció Regular (>=80%%): %.1f%% | n = %d\n\n",
            mean(dades_mod$Y) * 100, nrow(dades_mod)))


#### ============================================================ ####
####          1. MODEL COMPLET + SELECCIÓ PER BIC                 ####
#### ============================================================ ####

cat("============== 1A. MODEL COMPLET AMB FACTORS EFA =============\n\n")

# Model A: factors EFA de IA (IA_EINA_ESTUDI, IA_SUBSTITUCIO)
formula_completa_efa <- Y ~ MOT_DESMOTIVACIO + MOT_AUTOGESTIO + MOT_FORCA_MAJOR +
  EST_QUALITAT_DOC + EST_AVALUACIO_AC + EST_TEMPS_CLASSE + EST_GRUPS_REDUITS +
  IA_EINA_ESTUDI + IA_SUBSTITUCIO +
  T_AVAL + CURS_1R + GENERE + DOBLE_GRAU_EST + TREB_INTENS +
  EDAT + DESPL + NOTA_num

model_complet_efa <- glm(formula_completa_efa, data = dades_mod, family = binomial)
print(summary(model_complet_efa))

cat("\n===== Backward step per BIC (model EFA) =====\n\n")
model_bic_efa <- step(model_complet_efa, direction = "backward",
                      k = log(nrow(dades_mod)), trace = 1)

cat("\n\n============== 1B. MODEL COMPLET AMB LIKERT IA NUMÈRICA =============\n\n")

# Model B: Likert IA_SUBST_num i IA_ATENC_num directament (1-6)
formula_completa_ia <- Y ~ MOT_DESMOTIVACIO + MOT_AUTOGESTIO + MOT_FORCA_MAJOR +
  EST_QUALITAT_DOC + EST_AVALUACIO_AC + EST_TEMPS_CLASSE + EST_GRUPS_REDUITS +
  IA_SUBST_num + IA_ATENC_num +
  T_AVAL + CURS_1R + GENERE + DOBLE_GRAU_EST + TREB_INTENS +
  EDAT + DESPL + NOTA_num

model_complet_ia <- glm(formula_completa_ia, data = dades_mod, family = binomial)
print(summary(model_complet_ia))

cat("\n===== Backward step per BIC (model Likert IA) =====\n\n")
model_bic_ia <- step(model_complet_ia, direction = "backward",
                     k = log(nrow(dades_mod)), trace = 1)

cat("\n\n===== Comparacio dels dos models BIC =====\n\n")
df_comp_bic <- data.frame(
  model = c("BIC EFA (IA_EINA + IA_SUBST factor)", "BIC Likert (IA_SUBST_num + IA_ATENC_num)"),
  n_param = c(length(coef(model_bic_efa)), length(coef(model_bic_ia))),
  AIC = c(round(AIC(model_bic_efa), 2), round(AIC(model_bic_ia), 2)),
  BIC = c(round(BIC(model_bic_efa), 2), round(BIC(model_bic_ia), 2)),
  stringsAsFactors = FALSE
)
print(df_comp_bic, row.names = FALSE)

# seleccionem el model amb menor BIC
if (BIC(model_bic_ia) <= BIC(model_bic_efa)) {
  model_bic  <- model_bic_ia
  cat("\n-> Seleccionat: model Likert IA (menor BIC)\n\n")
} else {
  model_bic  <- model_bic_efa
  cat("\n-> Seleccionat: model EFA (menor BIC)\n\n")
}
formula_bic <- formula(model_bic)

cat("--- Resum model BIC seleccionat ---\n")
print(summary(model_bic))

cat("\nOdds Ratios model BIC (IC 95% Wald):\n")
print(round(exp(cbind(OR = coef(model_bic), confint.default(model_bic))), 3))

#### ============================================================ ####
####              2. LINEALITAT EN LOG-ODDS (Box-Tidwell)         ####
#### ============================================================ ####

cat("\n========= 2. LINEALITAT EN LOG-ODDS (Box-Tidwell) ========\n\n")

# Afegim termes x*log(x) per a cada continua del model
vars_cont <- intersect(c("EDAT", "DESPL", "NOTA_num", "IA_SUBST_num", "IA_ATENC_num",
                          "MOT_DESMOTIVACIO", "MOT_AUTOGESTIO", "MOT_FORCA_MAJOR",
                          "EST_AVALUACIO_AC", "IA_EINA_ESTUDI", "IA_SUBSTITUCIO"),
                        all.vars(formula_bic))
vars_bt <- vars_cont[sapply(vars_cont, function(v) {
  x <- dades_mod[[v]]; is.numeric(x) && all(x > 0, na.rm = TRUE)
})]

if (length(vars_bt) > 0) {
  dades_bt <- dades_mod
  for (v in vars_bt) dades_bt[[paste0(v, "_log")]] <- dades_bt[[v]] * log(dades_bt[[v]])
  formula_bt <- update(formula_bic,
                       as.formula(paste(". ~ . +", paste(paste0(vars_bt, "_log"), collapse = " + "))))
  model_bt <- glm(formula_bt, data = dades_bt, family = binomial)
  coefs_bt <- coef(summary(model_bt))
  noms_bt <- paste0(vars_bt, "_log")
  df_bt <- data.frame(
    variable = vars_bt,
    coef_BT = coefs_bt[noms_bt, "Estimate"],
    p = coefs_bt[noms_bt, "Pr(>|z|)"],
    stringsAsFactors = FALSE
  ) %>% mutate(
    sig = dplyr::case_when(p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", TRUE ~ "ns"),
    conclusio = ifelse(p >= 0.05, "lineal en log-odds", "NO lineal -> transformar")
  )
  rownames(df_bt) <- NULL
  print(df_bt)
} else {
  cat("Cap variable continua positiva al model BIC.\n")
}

# Grafics component + residual (crPlots)
par(ask = FALSE)
crPlots(model_bic, main = "Component + Residual Plots - Model BIC")

#### ============================================================ ####
####          3. CONTRAST LINEALITAT VARIABLES CLAU (quadratic)   ####
#### ============================================================ ####

cat("\n========== 3. CONTRAST LINEALITAT VARIABLES CLAU ==========\n\n")

vars_quadratic <- intersect(c("IA_SUBST_num", "IA_SUBSTITUCIO", "NOTA_num", "MOT_DESMOTIVACIO"),
                             all.vars(formula_bic))
pQ <- list()

for (var_nom in vars_quadratic) {
  cat(sprintf("--- %s ---\n", var_nom))

  form_q <- update(formula_bic,
                    as.formula(sprintf(". ~ . - %s + poly(%s, 2)", var_nom, var_nom)))
  model_q <- tryCatch(glm(form_q, data = dades_mod, family = binomial),
                      error = function(e) { cat("  Error model quadratic.\n\n"); NULL })
  if (is.null(model_q)) { pQ[[var_nom]] <- NA_real_; next }

  cn <- c(sprintf("poly(%s, 2)1", var_nom), sprintf("poly(%s, 2)2", var_nom))
  cq <- coef(summary(model_q))
  pQ[[var_nom]] <- cq[cn[2], "Pr(>|z|)"]

  cat(sprintf("  Lineal (L):    z=%.3f | p=%.4f\n", cq[cn[1], "z value"], cq[cn[1], "Pr(>|z|)"]))
  cat(sprintf("  Quadratic (Q): z=%.3f | p=%.4f\n", cq[cn[2], "z value"], cq[cn[2], "Pr(>|z|)"]))
  lrt <- anova(model_bic, model_q, test = "LRT")
  cat(sprintf("  LRT: Chi2=%.3f | gl=%d | p=%.4f\n", lrt[2, "Deviance"], lrt[2, "Df"], lrt[2, "Pr(>Chi)"]))
  cat(sprintf("  Conclusio: %s\n\n",
              ifelse(pQ[[var_nom]] < 0.05, "SIGNIFICATIU -> poly(2)", "no significatiu")))

  # Grafic: efecte lineal vs quadratic
  x_seq <- seq(min(dades_mod[[var_nom]], na.rm = TRUE),
               max(dades_mod[[var_nom]], na.rm = TRUE), length.out = 100)
  grid  <- setNames(data.frame(x_seq), var_nom)
  for (v in setdiff(all.vars(formula_bic)[-1], var_nom)) {
    col <- dades_mod[[v]]
    grid[[v]] <- if (is.numeric(col) || is.integer(col)) median(as.numeric(col), na.rm = TRUE) else {
      lvl <- names(sort(table(col), decreasing = TRUE))[1]
      if (is.factor(col)) factor(lvl, levels = levels(col)) else lvl
    }
  }
  grid$prob_lin <- predict(model_bic, newdata = grid, type = "response")
  grid$prob_q <- predict(model_q, newdata = grid, type = "response")

  df_plot <- pivot_longer(grid[, c(var_nom, "prob_lin", "prob_q")],
                          cols = c("prob_lin", "prob_q"),
                          names_to = "model_tipus", values_to = "prob") %>%
    mutate(x_val = rep(grid[[var_nom]], 2),
           model_tipus = ifelse(model_tipus == "prob_lin", "Lineal", "Lineal + Quadratic"))

  print(
    ggplot(df_plot, aes(x = x_val, y = prob, color = model_tipus, linetype = model_tipus)) +
      geom_line(linewidth = 1.1) +
      scale_color_manual(values = c("Lineal" = "#4A90B8", "Lineal + Quadratic" = "#E07B54")) +
      labs(title = sprintf("Efecte de %s sobre P(Regular)", var_nom),
           subtitle = "Resta de predictors a la mediana",
           x = var_nom, y = "P(Regular >= 80%)", color = NULL, linetype = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top",
            axis.text.y = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            legend.text = element_text(size = 12))
  )
}

#### ============================================================ ####
####          4. TESTS D'INTERACCIONS (LRT)                       ####
#### ============================================================ ####

cat("\n========== 4. TESTS D'INTERACCIONS (LRT) ==========\n\n")

vars_bic <- all.vars(formula_bic)

# Interaccions a provar (les dues variables han de ser al model)
interaccions <- c(
  "MOT_DESMOTIVACIO:T_AVAL",
  "IA_SUBST_num:NOTA_num",
  "IA_SUBSTITUCIO:NOTA_num",
  "MOT_DESMOTIVACIO:CURS_1R",
  "MOT_FORCA_MAJOR:EDAT",
  "IA_SUBST_num:CURS_1R",
  "IA_SUBSTITUCIO:CURS_1R"
)

df_inter <- do.call(rbind, lapply(interaccions, function(inter) {
  parts <- strsplit(inter, ":")[[1]]
  if (!all(parts %in% vars_bic)) return(NULL)
  model_i <- tryCatch(
    glm(update(formula_bic, as.formula(paste(". ~ . +", inter))),
        data = dades_mod, family = binomial),
    error = function(e) NULL
  )
  if (is.null(model_i)) return(NULL)
  lrt <- anova(model_bic, model_i, test = "LRT")
  data.frame(
    Interaccio = inter,
    Chi2 = round(lrt[2, "Deviance"], 3),
    gl = lrt[2, "Df"],
    p_LRT = round(lrt[2, "Pr(>Chi)"], 4),
    dBIC = round(BIC(model_i) - BIC(model_bic), 2),
    Significativa = ifelse(lrt[2, "Pr(>Chi)"] < 0.05, "Si", "No"),
    stringsAsFactors = FALSE
  )
}))

print(df_inter, row.names = FALSE)
cat("\n")

# Test niat per les dues interaccions de CURS_1R
ia_var <- intersect(c("IA_SUBST_num", "IA_SUBSTITUCIO"), vars_bic)[1]
if (!is.na(ia_var) && all(c("MOT_DESMOTIVACIO", "CURS_1R") %in% vars_bic)) {
  cat(sprintf("--- Test niat: base -> +DESM:CURS_1R -> +%s:CURS_1R ---\n\n", ia_var))
  model_A <- tryCatch(
    glm(update(formula_bic, . ~ . + MOT_DESMOTIVACIO:CURS_1R), data = dades_mod, family = binomial),
    error = function(e) NULL
  )
  model_B <- tryCatch(
    glm(update(formula_bic, as.formula(sprintf(". ~ . + MOT_DESMOTIVACIO:CURS_1R + %s:CURS_1R", ia_var))),
        data = dades_mod, family = binomial),
    error = function(e) NULL
  )
  if (!is.null(model_A) && !is.null(model_B)) {
    print(anova(model_bic, model_A, model_B, test = "LRT"))
    cat(sprintf("BIC base:               %.2f\n", BIC(model_bic)))
    cat(sprintf("BIC +DESM:CURS_1R:      %.2f\n", BIC(model_A)))
    cat(sprintf("BIC +DESM:CURS+IA:CURS: %.2f\n\n", BIC(model_B)))
  }
}

# Grafics interaccions significatives
sig_inter <- df_inter$Interaccio[df_inter$Significativa == "Si"]

for (inter in sig_inter) {
  parts <- strsplit(inter, ":")[[1]]
  model_i <- glm(update(formula_bic, as.formula(paste(". ~ . +", inter))),
                 data = dades_mod, family = binomial)

  cat(sprintf("\n-> Interaccio %s:\n", inter))
  ci <- coef(summary(model_i))
  inter_rows <- rownames(ci)[sapply(rownames(ci), function(r) all(sapply(parts, function(p) grepl(p, r))))]
  for (rw in inter_rows)
    cat(sprintf("  %s: Coef=%.4f | p=%.4f\n", rw, ci[rw, "Estimate"], ci[rw, "Pr(>|z|)"]))

  var1 <- parts[1]; var2 <- parts[2]
  col2 <- dades_mod[[var2]]
  vals2 <- if (is.numeric(col2) && length(unique(col2)) > 5)
    quantile(col2, c(0.25, 0.5, 0.75), na.rm = TRUE) else sort(unique(col2))

  grid_i <- expand.grid(setNames(list(
    seq(min(dades_mod[[var1]], na.rm = TRUE), max(dades_mod[[var1]], na.rm = TRUE), length.out = 60),
    vals2), c(var1, var2)))
  for (v in setdiff(all.vars(formula_bic)[-1], c(var1, var2))) {
    col <- dades_mod[[v]]
    grid_i[[v]] <- if (is.numeric(col) || is.integer(col)) median(as.numeric(col), na.rm = TRUE) else {
      lvl <- names(sort(table(col), decreasing = TRUE))[1]
      if (is.factor(col)) factor(lvl, levels = levels(col)) else lvl
    }
  }
  grid_i$prob <- predict(model_i, newdata = grid_i, type = "response")
  grid_i$x_val <- grid_i[[var1]]
  grid_i$grp <- factor(round(grid_i[[var2]], 2))

  print(
    ggplot(grid_i, aes(x = x_val, y = prob, color = grp)) +
      geom_line(linewidth = 1.1) +
      labs(title = sprintf("Interaccio %s", inter),
           x = var1, y = "P(Regular >= 80%)", color = var2) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top",
            axis.text.y = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            legend.text = element_text(size = 12))
  )
}

#### ============================================================ ####
####             5. MODEL FINAL EXPLICATIU                        ####
#### ============================================================ ####

cat("\n========== 5. MODEL FINAL EXPLICATIU ==========\n\n")

formula_final <- formula_bic

# Termes quadratics significatius
for (var_nom in names(pQ)) {
  if (!is.na(pQ[[var_nom]]) && pQ[[var_nom]] < 0.05) {
    formula_final <- update(formula_final,
                            as.formula(sprintf(". ~ . - %s + poly(%s, 2)", var_nom, var_nom)))
    cat(sprintf("-> Afegit poly(%s, 2)\n", var_nom))
  }
}

# Interaccions significatives (dBIC <= 10, excloem IA:CURS si el test niat no el justifica)
inter_excloses <- if (!is.na(ia_var)) paste0(ia_var, ":CURS_1R") else character(0)

for (inter in sig_inter) {
  if (inter %in% inter_excloses) { cat(sprintf("-> Exclosa %s (test niat)\n", inter)); next }
  dBIC_i <- df_inter$dBIC[df_inter$Interaccio == inter]
  if (!is.na(dBIC_i) && dBIC_i > 10) { cat(sprintf("-> Exclosa %s (dBIC=+%.1f)\n", inter, dBIC_i)); next }
  formula_final <- update(formula_final, as.formula(paste(". ~ . +", inter)))
  cat(sprintf("-> Afegida interaccio: %s\n", inter))
}

if (length(sig_inter) == 0) cat("-> Cap interaccio significativa afegida.\n")

cat("\nFormula model final:\n"); print(formula_final); cat("\n")

model_final_exp <- glm(formula_final, data = dades_mod, family = binomial)
print(summary(model_final_exp))

if (!identical(formula_final, formula_bic)) {
  cat("\nLRT model BIC vs model final:\n")
  print(anova(model_bic, model_final_exp, test = "LRT"))
}

cat("\nOdds Ratios model final (IC 95% Wald):\n")
or_final <- exp(cbind(OR = coef(model_final_exp), confint.default(model_final_exp)))
print(round(or_final, 3))

df_or <- as.data.frame(or_final) %>%
  rownames_to_column("variable") %>%
  filter(variable != "(Intercept)") %>%
  rename(LB = `2.5 %`, UB = `97.5 %`)

print(
  ggplot(df_or, aes(x = reorder(variable, OR), y = OR)) +
    geom_point(size = 3, color = "#4A90B8") +
    geom_errorbar(aes(ymin = LB, ymax = UB), width = 0.25, color = "#4A90B8") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.8) +
    coord_flip() + scale_y_log10() +
    labs(title = "Odds Ratios - Model logistic final explicatiu",
         subtitle = "IC 95% Wald | Escala logaritmica", x = "", y = "Odds Ratio") +
    theme_minimal(base_size = 13) +
    theme(axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          legend.text = element_text(size = 12))
)

#### ============================================================ ####
####                          6. FIV                              ####
#### ============================================================ ####

cat("\n=================== 6. FIV ===================\n\n")

vif_res <- tryCatch(vif(model_final_exp), error = function(e) {
  cat("  VIF no calculable (interaccions presents):", e$message, "\n"); NULL
})

if (!is.null(vif_res)) {
  if (is.matrix(vif_res)) {
    vif_equiv <- setNames(vif_res[, "GVIF^(1/(2*Df))"]^2, rownames(vif_res))
    cat("GVIF (factors):\n"); print(round(vif_res, 3))
  } else {
    vif_equiv <- vif_res
    print(round(vif_res, 3))
  }
  cat("\nGVIF equivalent a VIF:\n"); print(round(vif_equiv, 3))
  flag_vif <- names(vif_equiv[vif_equiv > 5])
  cat("Variables amb VIF > 5:", if (length(flag_vif) == 0) "Cap" else paste(flag_vif, collapse = ", "), "\n")

  print(
    ggplot(tibble(variable = names(vif_equiv), VIF = as.numeric(vif_equiv)),
           aes(x = reorder(variable, VIF), y = VIF, fill = VIF > 5)) +
      geom_col(alpha = 0.85) +
      geom_hline(yintercept = 5, linetype = "dashed", color = "red", linewidth = 0.8) +
      geom_text(aes(label = round(VIF, 2)), hjust = -0.1, size = 3.2) +
      coord_flip() +
      scale_fill_manual(values = c("FALSE" = "#4A90B8", "TRUE" = "#E07B54"), guide = "none") +
      labs(title = "Factor d'Inflacio de la Variancia (FIV)", x = "", y = "VIF equivalent") +
      theme_minimal(base_size = 13) +
      theme(axis.text.y = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            legend.text = element_text(size = 12))
  )
}

#### ============================================================ ####
####              7. OBSERVACIONS INFLUENTS                       ####
#### ============================================================ ####

cat("\n====== 7. OBSERVACIONS INFLUENTS ======\n\n")

n_obs <- nrow(model.frame(model_final_exp))
k_pred <- length(coef(model_final_exp)) - 1
row_idx <- as.integer(rownames(model.frame(model_final_exp)))

cook_d <- cooks.distance(model_final_exp)
lev_vals <- hatvalues(model_final_exp)
res_pears_std <- residuals(model_final_exp, type = "pearson") / sqrt(1 - hatvalues(model_final_exp))

thresh_cook <- 4 / n_obs
thresh_lev <- 2 * (k_pred + 1) / n_obs
thresh_res <- 2.5

cat(sprintf("n=%d | k=%d | thresh Cook=%.4f | thresh lev=%.4f\n", n_obs, k_pred, thresh_cook, thresh_lev))
cat(sprintf("Cook's D > %.4f: %d obs\n", thresh_cook, sum(cook_d > thresh_cook)))
cat(sprintf("|Residu Pearson| > %.1f: %d obs\n\n", thresh_res, sum(abs(res_pears_std) > thresh_res)))

idx_res <- which(abs(res_pears_std) > thresh_res)
if (length(idx_res) > 0) {
  cat("Observacions amb |residu Pearson std| > 2.5:\n")
  print(data.frame(
    index_orig = row_idx[idx_res],
    res_pearson_std = round(res_pears_std[idx_res], 3),
    cook_D = round(cook_d[idx_res], 4),
    leverage = round(lev_vals[idx_res], 4)
  ))
  cat("\n")
}

df_infl <- data.frame(
  index    = row_idx, cook_D = cook_d, leverage = lev_vals, res_pears = res_pears_std,
  flag_any = (cook_d > thresh_cook) | (abs(res_pears_std) > thresh_res)
)

print(
  ggplot(df_infl, aes(x = leverage, y = res_pears, size = cook_D, color = flag_any)) +
    geom_point(alpha = 0.7) +
    geom_hline(yintercept = c(-thresh_res, thresh_res), linetype = "dashed", color = "red") +
    geom_vline(xintercept = thresh_lev, linetype = "dashed", color = "steelblue") +
    geom_text(data = filter(df_infl, flag_any), aes(label = index),
              vjust = -0.9, size = 3, show.legend = FALSE) +
    scale_color_manual(values = c("FALSE" = "#4A90B8", "TRUE" = "#E07B54"),
                       labels = c("Normal", "Influent"), name = "") +
    scale_size_continuous(name = "Cook's D", range = c(1, 6)) +
    labs(title = "Leverage vs Residus de Pearson estandarditzats",
         x = "Leverage", y = "Residu Pearson estandarditzat") +
    theme_minimal(base_size = 13) +
    theme(axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          legend.text = element_text(size = 12))
)

# --- 7b. Analisi de sensibilitat (sense observacions influents)) ---
if (length(idx_res) > 0) {
  cat("--- 7b. Analisi de sensibilitat (sense obs amb |Pearson std| > 2.5) ---\n\n")
  rows_excloure <- row_idx[idx_res]
  cat(sprintf("Obs excloses: %d (index original: %s)\n\n",
              length(rows_excloure), paste(rows_excloure, collapse = ", ")))

  dades_sensit <- dades_mod[-match(rows_excloure, as.integer(rownames(dades_mod))), ]
  model_sensit <- tryCatch(
    glm(formula_final, data = dades_sensit, family = binomial),
    error = function(e) { cat("Error model sensibilitat:", e$message, "\n"); NULL }
  )

  if (!is.null(model_sensit)) {
    or_full <- exp(coef(model_final_exp))
    or_sensit <- exp(coef(model_sensit))
    vars_comuns <- intersect(names(or_full), names(or_sensit))

    df_sens <- data.frame(
      variable = vars_comuns,
      OR_complet = round(or_full[vars_comuns], 3),
      OR_sensit = round(or_sensit[vars_comuns], 3),
      canvi_pct = round((or_sensit[vars_comuns] - or_full[vars_comuns]) /
                           or_full[vars_comuns] * 100, 1),
      stringsAsFactors = FALSE
    )
    df_sens$estable <- ifelse(abs(df_sens$canvi_pct) < 20, "Si", "NO - revisar")
    rownames(df_sens) <- NULL
    print(df_sens, row.names = FALSE)

    cat(sprintf("\n-> n complet=%d | n sensibilitat=%d\n", nrow(dades_mod), nrow(dades_sensit)))
    cat(ifelse(all(df_sens$estable == "Si"),
               "-> Model robust: els ORs es mantenen estables (canvi < 20%)\n\n",
               "-> ATENCIO: alguns ORs canvien materialment (canvi >= 20%). Reportar al text.\n\n"))
  }
} else {
  cat("No hi ha observacions amb |residu Pearson std| > 2.5. Analisi de sensibilitat no aplicable.\n\n")
}

#### ============================================================ ####
####              8. HOSMER-LEMESHOW + CALIBRATION PLOT           ####
#### ============================================================ ####

cat("\n========== 8. HOSMER-LEMESHOW ==========\n\n")

prob_mod <- predict(model_final_exp, type = "response")

for (g in c(6, 8, 10)) {
  hl <- hoslem.test(dades_mod$Y, prob_mod, g = g)
  cat(sprintf("HL g=%2d: chi2=%.4f | gl=%d | p=%.4f\n", g, hl$statistic, hl$parameter, hl$p.value))
}
hl10 <- hoslem.test(dades_mod$Y, prob_mod, g = 10)
cat(ifelse(hl10$p.value > 0.05, "\n-> Bon ajust (p > 0.05)\n\n", "\n-> Problemes d'ajust (p < 0.05)\n\n"))

df_cal <- data.frame(prob_pred = prob_mod, Y_obs = dades_mod$Y) %>%
  arrange(prob_pred) %>% mutate(decil = ntile(prob_pred, 10))
df_cal_dec <- df_cal %>%
  group_by(decil) %>%
  summarise(prob_mitj = mean(prob_pred), prop_obs = mean(Y_obs), n = n(), .groups = "drop")

print(df_cal_dec %>% mutate(residu = round(prop_obs - prob_mitj, 3), flag = abs(residu) > 0.15))

print(
  ggplot(df_cal_dec, aes(x = prob_mitj, y = prop_obs)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
    geom_smooth(method = "loess", se = TRUE, color = "#4A90B8",
                fill = "#AED6F1", linewidth = 1, span = 1) +
    geom_point(aes(size = n), color = "#E07B54", alpha = 0.85) +
    scale_size_continuous(range = c(2, 6), guide = "none") +
    annotate("text", x = 0.75, y = 0.15,
             label = sprintf("HL p=%.3f", hl10$p.value), size = 4, color = "grey30") +
    labs(title = "Calibration plot - Model logistic final (totes les dades)",
         x = "Probabilitat predicta (decil)", y = "Proporcio observada Regular") +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    theme_minimal(base_size = 13) +
    theme(axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          legend.text = element_text(size = 12))
)

#### ============================================================ ####
####              9. EFECTES MARGINALS PROMIG (AME)               ####
#### ============================================================ ####

cat("\n========== 9. EFECTES MARGINALS PROMIG (AME) ==========\n\n")

ame_final <- tryCatch(avg_slopes(model_final_exp, newdata = dades_mod),
                      error = function(e) { cat("Error AME:", e$message, "\n"); NULL })

if (!is.null(ame_final)) {
  print(ame_final)
  df_ame <- as.data.frame(ame_final) %>%
    filter(!grepl("Intercept", term, ignore.case = TRUE)) %>%
    dplyr::select(any_of(c("term", "estimate", "conf.low", "conf.high"))) %>%
    arrange(estimate)
  print(
    ggplot(df_ame, aes(x = reorder(term, estimate), y = estimate)) +
      geom_point(size = 3, color = "#4A90B8") +
      geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25, color = "#4A90B8") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8) +
      coord_flip() +
      labs(title = "Efectes Marginals Promig (AME) - Model logistic final",
           subtitle = "IC 95% (delta method) | variable resposta: P(Regular >= 80%)",
           x = "", y = "AME (canvi en probabilitat)") +
      theme_minimal(base_size = 13) +
      theme(axis.text.y = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            legend.text = element_text(size = 12))
  )
}

#### ============================================================ ####
####                     10. GUARDAR MODEL                        ####
#### ============================================================ ####

saveRDS(model_final_exp, "2. Dades/2. Models/model_logit_explicatiu.rds")
saveRDS(formula_final,   "2. Dades/2. Models/formula_logit_explicatiu.rds")

dades_def$prob_logit_exp <- NA_real_
dades_def$prob_logit_exp[as.integer(rownames(model.frame(model_final_exp)))] <- prob_mod

save(dades_def, file = "2. Dades/5. Dades Logit.RData")

sink()
dev.off()
