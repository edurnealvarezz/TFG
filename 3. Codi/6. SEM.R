packages <- c("lavaan", "semTools", "semPlot", "dplyr", "ggplot2", "tibble", "tidyr")
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
sink("4. Outputs/6.1 Output_text_sem.txt")
pdf("4. Outputs/6.2 Output_grafics_sem.pdf", width = 12, height = 9)

#### ============================================================ ####
####                   0. PREPARACIÓ DE DADES                     ####
#### ============================================================ ####

cat("========== 0. PREPARACIÓ DE DADES ==========\n\n")

items_desg <- c("M_TEOR", "M_PASSIU", "M_AVORR", "M_PROF", "M_AMICS")
items_fmaj <- c("M_SALUT", "M_FAM", "M_TREB")
items_aval <- c("E_ACT_AC", "E_PES_AC", "E_PART")
items_ia <- c("IA_SUBST", "IA_CONF")
items_ord <- c(items_desg, items_fmaj, items_aval, items_ia)

dades_sem <- dades_def %>%
  mutate(
    NOTA_num = as.numeric(NOTA),
    CURS_1R_d = as.integer(as.logical(CURS_1R) | as.character(CURS_1R) %in%
                                  c("1", "SI", "TRUE", "1r", "TRUE")),
    T_AVAL_num = as.integer(T_AVAL == "Continuada"),
    DOBLE_GRAU_EST = as.integer(DOBLE_GRAU_EST),
    P_ASSIST = as.numeric(P_ASSIST)
  )

# Declarar items com ordered, perquè no calculi corr pearson sinó polychoric
for (v in items_ord) {
  if (v %in% names(dades_sem))
    dades_sem[[v]] <- ordered(as.integer(dades_sem[[v]]))
}

vars_model <- c(items_ord, "P_ASSIST", "NOTA_num", "CURS_1R_d", "T_AVAL_num", "DOBLE_GRAU_EST")
vars_model <- vars_model[vars_model %in% names(dades_sem)]
dades_sem_net <- dades_sem[, vars_model, drop = FALSE]
dades_sem_net <- dades_sem_net[complete.cases(dades_sem_net), ]

cat(sprintf("Observacions totals dades_def: %d\n", nrow(dades_def)))
cat(sprintf("Observacions al model (sense NAs): %d\n", nrow(dades_sem_net)))
cat(sprintf("Parametres lliures aprox. (regla 10 obs/param): max ~%d\n\n",
            floor(nrow(dades_sem_net) / 10)))

cat("Distribucio P_ASSIST:\n")
print(summary(dades_sem_net$P_ASSIST))
cat(sprintf("SD: %.2f\n\n", sd(dades_sem_net$P_ASSIST)))

cat("CURS_1R_d:\n"); print(table(dades_sem_net$CURS_1R_d))
cat("\nT_AVAL_num:\n"); print(table(dades_sem_net$T_AVAL_num))
cat("\nDOBLE_GRAU_EST:\n"); print(table(dades_sem_net$DOBLE_GRAU_EST))
cat("\n")

#### ============================================================ ####
####                  1. MODEL DE MESURA (CFA)                    ####
#### ============================================================ ####

# connecta les variables latents (no observables) amb les variables observables
# defineixes a priori quins items mesuren cada constructe latent
# el model estima càrregues factorials (lambda)

cat("\n========== 1. MODEL DE MESURA (CFA) ==========\n\n")

# Verificacio prèvia: variables existents i nivells suficients
items_faltants <- items_ord[!items_ord %in% names(dades_sem_net)]
if (length(items_faltants) > 0) {
  cat(sprintf("AVIS: items no trobats a dades_sem_net: %s\n",
              paste(items_faltants, collapse = ", ")))
}
items_un_nivell <- sapply(items_ord[items_ord %in% names(dades_sem_net)], function(v) {
  nlevels(dades_sem_net[[v]])
})
cat("Nivells per item (minim 2):\n")
print(items_un_nivell)
items_problematic <- names(items_un_nivell)[items_un_nivell < 2]
if (length(items_problematic) > 0)
  cat(sprintf("AVIS: items amb <2 nivells (eliminar o revisar): %s\n",
              paste(items_problematic, collapse = ", ")))

# Model de mesura: cada constructe latent (factor) és mesurat per un grup d'items observables
model_cfa_str <- paste(
  "DESMOTIVACIO_PEDAG =~ M_TEOR + M_PASSIU + M_AVORR + M_PROF + M_AMICS",
  "FORCA_MAJOR =~ M_SALUT + M_FAM + M_TREB",
  "AVALUACIO_AC =~ E_ACT_AC + E_PES_AC + E_PART",
  "IA_SUBSTITUCIO =~ IA_SUBST + IA_CONF",
  sep = "\n"
)
cat("\nModel CFA:\n"); cat(model_cfa_str); cat("\n\n")

# estimem amb WLSMV (robust per dades ordinals com likert)
# std.lv = TRUE fixa var=1, millor per comparar carregues entre factors
fit_cfa <- tryCatch(
  cfa(model_cfa_str, data = dades_sem_net,
      estimator = "WLSMV", ordered = items_ord, std.lv = TRUE),
  error = function(e) { cat("ERROR CFA:", e$message, "\n"); NULL }
)

if (!is.null(fit_cfa)) {
  cat("Resum CFA (model de mesura):\n")
  summary(fit_cfa, fit.measures = TRUE, standardized = TRUE, ci = TRUE) # resum model

  cat("\nIndexs d'ajust CFA:\n")
  print(fitMeasures(fit_cfa, c("cfi", "tli", "rmsea", "rmsea.ci.lower",
                                "rmsea.ci.upper", "srmr", "wrmr")))
                                # per veure si el model és adequat abans de passar al SEM
                                # si el model

  cat("\nModindices CFA (top 15):\n")
  print(modindices(fit_cfa, sort. = TRUE, maximum.number = 15))
    # suggereix canvis al model

  # càrregues estandaritzades
  std_cfa <- standardizedSolution(fit_cfa)
  loads_cfa <- std_cfa[std_cfa$op == "=~",
                        c("lhs", "rhs", "est.std", "se", "z", "pvalue")]
  cat("\nCarregues factorials estandaritzades:\n")
  print(loads_cfa, row.names = FALSE)

  # avís càrregues baixes (considerar eliminar), (<0.4)
  loads_low <- loads_cfa[!is.na(loads_cfa$est.std) & loads_cfa$est.std < 0.40, ]
  if (nrow(loads_low) > 0) {
    cat("\nAVIS: items amb carrega < 0.40 (considerar eliminacio):\n")
    print(loads_low[, c("lhs", "rhs", "est.std")], row.names = FALSE)
  }

  # Heywood (variancies negatives) -> indica que algun item no encaixa bé al factor
  pe_cfa <- parameterEstimates(fit_cfa)
  heywood <- pe_cfa[pe_cfa$op == "~~" & pe_cfa$lhs == pe_cfa$rhs &
                      !is.na(pe_cfa$est) & pe_cfa$est < 0, ]
  if (nrow(heywood) > 0) {
    cat("\nAVIS HEYWOOD: variancies negatives (item problematic):\n")
    print(heywood[, c("lhs", "rhs", "est")], row.names = FALSE)
    cat("-> Si es IA_ATENC, elimina-la i reestima el CFA sense aquest item.\n\n")
  } else {
    cat("\nHeywood check: cap variancia negativa detectada.\n\n")
  }

  # AVE: mira que items expliquin bé el constructe (> 0.5)
  constructs_cfa <- unique(loads_cfa$lhs)
  ave_vals <- sapply(constructs_cfa, function(ct) {
    lds <- loads_cfa$est.std[loads_cfa$lhs == ct]
    lds2 <- lds^2
    sum(lds2) / (sum(lds2) + sum(1 - lds2))
  })
  cat("AVE per constructe (> 0.50 = convergencia adequada):\n")
  print(round(ave_vals, 3))

  # Fiabilitat composta (omega) -> consistència interna del constructe
  # ha de ser > 0.70
  cat("\nFiabilitat composta (omega) per constructe:\n")
  omega_res <- tryCatch(
    compRelSEM(fit_cfa),
    error = function(e) {
      tryCatch(reliability(fit_cfa),
               error = function(e2) { cat("  (no disponible)\n"); NULL })
    }
  )
  if (!is.null(omega_res)) print(round(omega_res, 3))

  # Validesa discriminant: correlacions entre constructes, mira que siguin diferents
  cors_cfa <- std_cfa[std_cfa$op == "~~" &
                        std_cfa$lhs != std_cfa$rhs &
                        std_cfa$lhs %in% constructs_cfa &
                        std_cfa$rhs %in% constructs_cfa, ]
  if (nrow(cors_cfa) > 0) {
    cat("\nCorrelacions entre constructes (r) i r^2 (ha de ser < AVE):\n")
    cors_cfa$r2 <- round(cors_cfa$est.std^2, 3)
    print(cors_cfa[, c("lhs", "rhs", "est.std", "r2")], row.names = FALSE)
  }
}

# Model de mesura final: IA_SUBSTITUCIO = IA_SUBST + IA_CONF (sense IA_ATENC)
mesura_comuns <- paste(
  "DESMOTIVACIO_PEDAG =~ M_TEOR + M_PASSIU + M_AVORR + M_PROF + M_AMICS",
  "FORCA_MAJOR =~ M_SALUT + M_FAM + M_TREB",
  "AVALUACIO_AC =~ E_ACT_AC + E_PES_AC + E_PART",
  sep = "\n"
)
items_ia_final <- items_ia   # c("IA_SUBST", "IA_CONF")
model_mesura <- paste(mesura_comuns,
                      "IA_SUBSTITUCIO =~ IA_SUBST + IA_CONF",
                      sep = "\n")
items_ord_final <- c(items_desg, items_fmaj, items_aval, items_ia_final)

#### ============================================================ ####
####               2. MODEL ESTRUCTURAL COMPLET — Model B         ####
#### ============================================================ ####

# el CFA ens diu que els 4 constructes estan bé, però expliquen per què un alumne falta a classe?

cat("\n========== 2. MODEL ESTRUCTURAL COMPLET (SEM) ==========\n\n")

a1 = efecto directo de DESMOTIVACIO_PEDAG sobre P_ASSIST
a2 = efecto de DESMOTIVACIO_PEDAG sobre AVALUACIO_AC (primera parte de la mediación)
b_aval = efecto de AVALUACIO_AC sobre P_ASSIST (segunda parte de la mediación)

#a1 = efecte total de DESMOTIVACIO_PEDAG sobre P_ASSIST (sense mediacio)
# a2 = efecte de DESMOTIVACIO_PEDAG sobre AVALUACIO_AC
# b_aval = efecte de AVALUACIO_AC sobre P_ASSIST

model_sem_b <- paste(
  model_mesura,
  "P_ASSIST ~ a1*DESMOTIVACIO_PEDAG + b_fmaj*FORCA_MAJOR + b_aval*AVALUACIO_AC +",
  "           b_ia*IA_SUBSTITUCIO + b_nota*NOTA_num + b_curs*CURS_1R_d + b_taval*T_AVAL_num +",
  "           b_dg*DOBLE_GRAU_EST",
  "AVALUACIO_AC ~ a2*DESMOTIVACIO_PEDAG",
  "DESMOTIVACIO_PEDAG ~~ FORCA_MAJOR",
  "DESMOTIVACIO_PEDAG ~~ IA_SUBSTITUCIO",
  "FORCA_MAJOR ~~ IA_SUBSTITUCIO",
  "ind_desm := a2 * b_aval",
  "tot_desm := a1 + ind_desm",
  sep = "\n"
)
cat("Model SEM-B:\n"); cat(model_sem_b); cat("\n\n")

# estimació i resultats
fit_sem_b <- tryCatch(
  sem(model_sem_b, data = dades_sem_net,
      estimator = "WLSMV", ordered = items_ord_final, std.lv = TRUE),
  error = function(e) { cat("ERROR SEM-B:", e$message, "\n"); NULL }
)

if (!is.null(fit_sem_b)) {
  cat("Resum SEM complet (Model B — amb mediacio):\n")
  summary(fit_sem_b, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

  cat("\nIndexs d'ajust SEM Model B:\n")
  print(fitMeasures(fit_sem_b, c("cfi", "tli", "rmsea", "rmsea.ci.lower",
                                  "rmsea.ci.upper", "srmr", "wrmr")))

  cat("\nCoeficients estructurals estandaritzats (relacions ~ i :=):\n")
  pe_b <- parameterEstimates(fit_sem_b, standardized = TRUE, ci = TRUE)

  struct_b <- pe_b[pe_b$op == "~", c("lhs", "rhs", "est", "se", "z",
                                      "pvalue", "ci.lower", "ci.upper", "std.all")]
  print(struct_b, row.names = FALSE)

  cat("\nEfectes indirectes i totals (mediacio):\n")
  indir_b <- pe_b[pe_b$op == ":=", c("lhs", "est", "se", "z", "pvalue",
                                       "ci.lower", "ci.upper")]
  print(indir_b, row.names = FALSE)

  cat("\nR^2 P_ASSIST i AVALUACIO_AC:\n")
  print(round(inspect(fit_sem_b, "r2"), 4))

  # Modindices per si ajust dolent
  fm_b_vals <- fitMeasures(fit_sem_b, c("cfi", "rmsea"))
  if (as.numeric(fm_b_vals["cfi"]) < 0.90) {
    cat("\nAVIS: CFI < 0.90 — modindices per millorar ajust:\n")
    print(modindices(fit_sem_b, sort. = TRUE, maximum.number = 15))
  }
}

#### ============================================================ ####
####           3. INDEXS D'AJUST I DIAGNÒSTIC                     ####
#### ============================================================ ####

cat("\n========== 3. INDEXS D'AJUST I DIAGNOSTIC ==========\n\n")

avaluar_ajust <- function(fit, nom) {
  if (is.null(fit)) { cat(sprintf("%s: model no disponible\n", nom)); return(invisible(NULL)) }
  fm <- fitMeasures(fit, c("cfi", "tli", "rmsea", "rmsea.ci.lower",
                            "rmsea.ci.upper", "srmr", "wrmr", "df", "chisq"))
  cat(sprintf("--- %s ---\n", nom))
  cat(sprintf("  CFI   = %.3f  (>=0.95 excel·lent, >=0.90 acceptable)\n", fm["cfi"]))
  cat(sprintf("  TLI   = %.3f  (>=0.95 excel·lent)\n",                   fm["tli"]))
  cat(sprintf("  RMSEA = %.3f [%.3f, %.3f]  (<=0.06 bo, <=0.08 acceptable)\n",
              fm["rmsea"], fm["rmsea.ci.lower"], fm["rmsea.ci.upper"]))
  cat(sprintf("  SRMR  = %.3f  (<=0.08 bo)\n",                           fm["srmr"]))
  cat(sprintf("  WRMR  = %.3f  (<=1.0 bo)\n",                            fm["wrmr"]))
  cat(sprintf("  chi2(%d) = %.2f\n",                                     fm["df"], fm["chisq"]))
  cfi_v <- as.numeric(fm["cfi"]); rmsea_v <- as.numeric(fm["rmsea"])
  verdict <- if (cfi_v >= 0.95 && rmsea_v <= 0.06) "EXCEL·LENT" else
             if (cfi_v >= 0.90 && rmsea_v <= 0.08) "ACCEPTABLE" else "INADEQUAT"
  cat(sprintf("  -> Veredicte: %s\n\n", verdict))
  invisible(fm)
}

avaluar_ajust(fit_cfa,   "CFA (model de mesura)")
avaluar_ajust(fit_sem_b, "SEM Model B (complet)")

#### ============================================================ ####
####              4. MODELS ALTERNATIUS                           ####
#### ============================================================ ####

cat("\n========== 4. MODELS ALTERNATIUS ==========\n\n")

# Model A: sense ruta de mediacio DESMOTIVACIO -> AVALUACIO_AC
model_sem_a <- paste(
  model_mesura,
  "P_ASSIST ~ DESMOTIVACIO_PEDAG + FORCA_MAJOR + AVALUACIO_AC +",
  "           IA_SUBSTITUCIO + NOTA_num + CURS_1R_d + T_AVAL_num + DOBLE_GRAU_EST",
  "DESMOTIVACIO_PEDAG ~~ FORCA_MAJOR",
  "DESMOTIVACIO_PEDAG ~~ IA_SUBSTITUCIO",
  "FORCA_MAJOR ~~ IA_SUBSTITUCIO",
  sep = "\n"
)

fit_sem_a <- tryCatch(
  sem(model_sem_a, data = dades_sem_net,
      estimator = "WLSMV", ordered = items_ord_final, std.lv = TRUE),
  error = function(e) { cat("ERROR SEM-A:", e$message, "\n"); NULL }
)
avaluar_ajust(fit_sem_a, "SEM Model A (sense mediacio)")

# Model C: parsimoniós — elimina predictors amb p > 0.10 de P_ASSIST
if (!is.null(fit_sem_b)) {
  pe_b_str <- parameterEstimates(fit_sem_b, standardized = TRUE)
  pa_rows <- pe_b_str[pe_b_str$op == "~" & pe_b_str$lhs == "P_ASSIST", ]
  non_sig <- pa_rows$rhs[!is.na(pa_rows$pvalue) & pa_rows$pvalue > 0.10]
  cat(sprintf("Predictors no significatius (p > 0.10) al Model B: %s\n\n",
              if (length(non_sig) == 0) "cap" else paste(non_sig, collapse = ", ")))

  if (length(non_sig) > 0 && length(non_sig) < 5) {
    pred_sig <- pa_rows$rhs[is.na(pa_rows$pvalue) | pa_rows$pvalue <= 0.10]
    pred_str <- paste(pred_sig, collapse = " + ")
    model_sem_c <- paste(
      model_mesura,
      paste("P_ASSIST ~", pred_str),
      "AVALUACIO_AC ~ DESMOTIVACIO_PEDAG",
      "DESMOTIVACIO_PEDAG ~~ FORCA_MAJOR",
      "DESMOTIVACIO_PEDAG ~~ IA_SUBSTITUCIO",
      "FORCA_MAJOR ~~ IA_SUBSTITUCIO",
      sep = "\n"
    )
    fit_sem_c <- tryCatch(
      sem(model_sem_c, data = dades_sem_net,
          estimator = "WLSMV", ordered = items_ord_final, std.lv = TRUE),
      error = function(e) { cat("ERROR SEM-C:", e$message, "\n"); NULL }
    )
    avaluar_ajust(fit_sem_c, "SEM Model C (parsimoniós)")
  } else {
    fit_sem_c <- NULL
    cat("Cap predictor a eliminar o massa predictors — Model C omès.\n\n")
  }
} else {
  fit_sem_c <- NULL
}

# Comparació models A vs B (test LRT per a models niuats)
cat("Comparació Models A i B:\n")
comp_tab <- data.frame(
  Model = c("A (sense mediacio)", "B (complet)"),
  CFI = sapply(list(fit_sem_a, fit_sem_b), function(f)
    if (!is.null(f)) round(as.numeric(fitMeasures(f, "cfi")), 3) else NA),
  RMSEA = sapply(list(fit_sem_a, fit_sem_b), function(f)
    if (!is.null(f)) round(as.numeric(fitMeasures(f, "rmsea")), 3) else NA),
  SRMR = sapply(list(fit_sem_a, fit_sem_b), function(f)
    if (!is.null(f)) round(as.numeric(fitMeasures(f, "srmr")), 3) else NA),
  df = sapply(list(fit_sem_a, fit_sem_b), function(f)
    if (!is.null(f)) as.numeric(fitMeasures(f, "df")) else NA)
)
print(comp_tab, row.names = FALSE)

if (!is.null(fit_sem_a) && !is.null(fit_sem_b)) {
  cat("\nTest de diferencia chi2 (Model A niat en B):\n")
  lrt_res <- tryCatch(
    lavTestLRT(fit_sem_a, fit_sem_b),
    error = function(e) { cat("  lavTestLRT:", e$message, "\n"); NULL }
  )
  if (!is.null(lrt_res)) print(lrt_res)
}

# Model D: com Model A però amb items IA crus en comptes del constructe latent
cat("\n--- Model D: variables IA crues com a predictors directes (sense IA_SUBSTITUCIO) ---\n")
cat("Objectiu: testar si IA_ATENC (i/o IA_SUBST, IA_CONF) son individualment significatives.\n\n")

model_sem_d <- paste(
  c(mesura_comuns,
    "P_ASSIST ~ DESMOTIVACIO_PEDAG + FORCA_MAJOR + AVALUACIO_AC +",
    "           IA_SUBST + NOTA_num + CURS_1R_d + T_AVAL_num + DOBLE_GRAU_EST",
    "DESMOTIVACIO_PEDAG ~~ FORCA_MAJOR",
    "DESMOTIVACIO_PEDAG ~~ IA_SUBST",
    "FORCA_MAJOR ~~ IA_SUBST",
    "AVALUACIO_AC ~~ IA_SUBST"),
  collapse = "\n"
)

fit_sem_d <- tryCatch(
  sem(model_sem_d, data = dades_sem_net,
      estimator = "WLSMV", ordered = items_ord_final, std.lv = TRUE),
  error = function(e) { cat("ERROR SEM-D:", e$message, "\n"); NULL }
)
avaluar_ajust(fit_sem_d, "SEM Model D (IA crues)")

if (!is.null(fit_sem_d)) {
  pe_d <- parameterEstimates(fit_sem_d, standardized = TRUE)
  pa_d <- pe_d[pe_d$op == "~" & pe_d$lhs == "P_ASSIST",
               c("rhs", "est.std", "se", "z", "pvalue")]
  colnames(pa_d) <- c("Predictor", "Beta_std", "SE", "z", "p")
  pa_d$signif <- ifelse(is.na(pa_d$p), "",
                        ifelse(pa_d$p < .001, "***",
                               ifelse(pa_d$p < .01, "**",
                                      ifelse(pa_d$p < .05, "*",
                                             ifelse(pa_d$p < .10, ".", "")))))
  cat("Coeficients estructurals estandaritzats Model D (P_ASSIST ~):\n")
  print(pa_d, row.names = FALSE)
  cat(sprintf("\nR^2 P_ASSIST (Model D): %.3f\n\n",
              as.numeric(inspect(fit_sem_d, "r2")["P_ASSIST"])))

  # Comparació índexs ajust A vs D
  comp_ad <- data.frame(
    Model  = c("A (IA latent)", "D (IA crues)"),
    CFI    = sapply(list(fit_sem_a, fit_sem_d), function(f)
                    if (!is.null(f)) round(as.numeric(fitMeasures(f, "cfi")), 3) else NA),
    RMSEA  = sapply(list(fit_sem_a, fit_sem_d), function(f)
                    if (!is.null(f)) round(as.numeric(fitMeasures(f, "rmsea")), 3) else NA),
    SRMR   = sapply(list(fit_sem_a, fit_sem_d), function(f)
                    if (!is.null(f)) round(as.numeric(fitMeasures(f, "srmr")), 3) else NA),
    df     = sapply(list(fit_sem_a, fit_sem_d), function(f)
                    if (!is.null(f)) as.numeric(fitMeasures(f, "df")) else NA)
  )
  cat("Comparació ajust Model A (IA latent) vs Model D (IA crues):\n")
  print(comp_ad, row.names = FALSE)
  cat("\n")
}

#### ============================================================ ####
####                   5. VISUALITZACIÓ                           ####
#### ============================================================ ####

cat("\n========== 5. VISUALITZACIO ==========\n\n")

fit_final <- fit_sem_a

if (!is.null(fit_final)) {
  # Coeficients estructurals estandaritzats — SEM Model A
  sd_passist <- sd(dades_def$P_ASSIST, na.rm = TRUE)
  params_finals <- parameterEstimates(fit_sem_a, standardized = TRUE, ci = TRUE) %>%
    filter(op == "~") %>%
    mutate(
      rhs = factor(rhs, levels = rhs[order(std.all)]),
      significatiu = ifelse(pvalue < 0.05, "p < 0.05", "p ≥ 0.05")
    )

  print(
    ggplot(params_finals, aes(x = std.all, y = rhs, color = significatiu)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
      geom_point(size = 3) +
      geom_errorbarh(aes(xmin = ci.lower / sd_passist,
                         xmax = ci.upper / sd_passist),
                     height = 0.25) +
      scale_color_manual(values = c("p < 0.05" = "#1A5276",
                                    "p ≥ 0.05" = "#AED6F1")) +
      labs(title = "Coeficients estructurals estandaritzats — SEM Model A",
           x = "Beta estandaritzat", y = "", color = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")
  )

  # Grafic carregues factorials CFA
  if (!is.null(fit_cfa)) {
    std_plot_cfa <- standardizedSolution(fit_cfa)
    loads_plot <- std_plot_cfa[std_plot_cfa$op == "=~",
                                c("lhs", "rhs", "est.std", "ci.lower", "ci.upper")]
    loads_plot <- loads_plot[!is.na(loads_plot$est.std), ]
    loads_plot$item <- paste0(loads_plot$lhs, ": ", loads_plot$rhs)

    print(
      ggplot(loads_plot, aes(x = est.std, y = reorder(item, est.std),
                             fill = lhs)) +
        geom_col(alpha = 0.85) +
        geom_vline(xintercept = 0.40, linetype = "dashed", color = "red",
                   linewidth = 0.7) +
        annotate("text", x = 0.41, y = 1.5, label = "llindar 0.40",
                 color = "red", size = 3, hjust = 0) +
        scale_fill_brewer(palette = "Set2") +
        labs(title = "Carregues factorials estandaritzades — CFA",
             subtitle = "Estimador WLSMV | std.lv = TRUE",
             x = "Carrega estandaritzada (lambda)", y = "", fill = "Constructe") +
        theme_minimal(base_size = 12) +
        theme(legend.position = "right")
    )
  }
}

#### ============================================================ ####
####              6. TAULA COMPARATIVA SEM vs LOGIT               ####
#### ============================================================ ####

cat("\n========== 6. TAULA COMPARATIVA SEM vs LOGIT MILLORAT ==========\n\n")

if (!is.null(fit_sem_a)) {
  pe_comp <- parameterEstimates(fit_sem_a, standardized = TRUE, ci = TRUE)
  struct_comp <- pe_comp[pe_comp$op == "~" & pe_comp$lhs == "P_ASSIST", ]

  # OR del Logit Millorat (del model final amb interaccio MOT_DESMOTIVACIO x CURS_1R)
  or_ref <- data.frame(
    Predictor = c("DESMOTIVACIO_PEDAG", "FORCA_MAJOR", "AVALUACIO_AC",
                  "IA_SUBSTITUCIO", "NOTA_num", "CURS_1R_d", "T_AVAL_num",
                  "DOBLE_GRAU_EST"),
    OR_logit = c(0.568, 1.374, 1.830, 0.701, 1.821, 2.677, 5.467, NA),
    Dir_logit = c("-", "+", "+", "-", "+", "+", "+", NA),
    stringsAsFactors = FALSE
  )

  taula_comp <- merge(
    struct_comp[, c("rhs", "std.all", "pvalue", "ci.lower", "ci.upper")],
    or_ref,
    by.x = "rhs", by.y = "Predictor",
    all.x = TRUE
  )
  taula_comp$Dir_SEM <- ifelse(!is.na(taula_comp$std.all),
                                ifelse(taula_comp$std.all > 0, "+", "-"), NA)
  taula_comp$Consistent <- ifelse(!is.na(taula_comp$Dir_SEM) &
                                    !is.na(taula_comp$Dir_logit),
                                   ifelse(taula_comp$Dir_SEM == taula_comp$Dir_logit,
                                          "SI", "NO"), "—")
  taula_comp$Beta_SEM <- round(taula_comp$std.all, 3)
  taula_comp$p_SEM <- round(taula_comp$pvalue, 4)
  taula_comp <- taula_comp[order(abs(taula_comp$Beta_SEM), decreasing = TRUE), ]

  cat("SEM beta estand. (P_ASSIST) vs Logit Millorat OR:\n\n")
  print(taula_comp[, c("rhs", "Beta_SEM", "p_SEM", "OR_logit",
                        "Dir_SEM", "Dir_logit", "Consistent")],
        row.names = FALSE)
  cat("\nNota: SEM estima efectes sobre P_ASSIST (continua 0-100).\n")
  cat("Logit estimava P(GRUP_ASSIST=Regular>=80%). Les direccions han de coincidir.\n")
  cat("\nNOTA IA_SUBSTITUCIO: el signe 'NO consistent' reflecteix suppressio estadistica,\n")
  cat("no absencia d'efecte. La correlacio r=0.591 entre IA_SUBSTITUCIO i\n")
  cat("DESMOTIVACIO_PEDAG fa que l'efecte net de IA_SUBSTITUCIO canvii de signe\n")
  cat("en presencia de DESMOTIVACIO_PEDAG. Cal interpretar ambdos predictors\n")
  cat("conjuntament, no per separat.\n")

  # Grafic comparatiu de signe i magnitud
  taula_plot <- taula_comp[!is.na(taula_comp$Beta_SEM) & !is.na(taula_comp$OR_logit), ]
  taula_plot$log_OR <- log(taula_plot$OR_logit) * ifelse(taula_plot$Dir_logit == "+", 1, -1)
  taula_plot_long <- taula_plot %>%
    dplyr::select(rhs, Beta_SEM, log_OR) %>%
    tidyr::pivot_longer(c(Beta_SEM, log_OR), names_to = "model", values_to = "coef") %>%
    mutate(model = ifelse(model == "Beta_SEM", "SEM (beta std.)", "Logit (log OR)"))

  print(
    ggplot(taula_plot_long, aes(x = coef, y = rhs, color = model, shape = model)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
      geom_point(size = 3.5, alpha = 0.9) +
      scale_color_manual(values = c("SEM (beta std.)" = "#4A90B8",
                                    "Logit (log OR)" = "#E07B54")) +
      labs(title = "Comparacio SEM vs Logit: efectes sobre assistencia",
           subtitle = "SEM = beta estandaritzat sobre P_ASSIST | Logit = log(OR) sobre P(Regular)",
           x = "Coeficient (escala diferent)", y = "", color = NULL, shape = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top")
  )
}

#### ============================================================ ####
####                  GUARDAR I TANCAR                            ####
#### ============================================================ ####

cat("\n========== GUARDAR ==========\n\n")

if (!is.null(fit_sem_a)) {
  saveRDS(fit_sem_a, "2. Dades/model_sem_complet.rds")
  cat("-> model_sem_complet.rds desat\n")
}
if (!is.null(fit_cfa)) {
  saveRDS(fit_cfa, "2. Dades/model_cfa.rds")
  cat("-> model_cfa.rds desat\n")
}

dev.off()
sink()
