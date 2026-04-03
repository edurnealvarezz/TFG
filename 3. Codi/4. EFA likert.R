packages <- c("dplyr", "ggplot2", "tidyr", "psych", "reshape2")

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

lapply(packages, install_if_missing)
rm(packages)

setwd("C:/Users/edurn/Downloads/TFG")
load("2. Dades/3. Dades EDA.RData")

motius_vars <- readRDS("2. Dades/motius_vars.rds")
estrategies_vars <- readRDS("2. Dades/estrategies_vars.rds")
ia_vars <- readRDS("2. Dades/ia_vars.rds")

col_grups <- c("Irregular (<80%)" = "#E07B54", "Regular (≥80%)" = "#4A90B8")

#### ============================================================ ####
####                     FUNCIONS AUXILIARS                       ####
#### ============================================================ ####

##### --------------------------- KMO ----------------------- #####

# Interpreta el KMO, així podem saber si és acceptable per fer EFA
# mira que les correlacions siguin fortes i no hi hagi massa variables
# que no s'expliquin per factors comuns

kmo_interp <- function(kmo) {
  case_when(
    kmo >= 0.90 ~ "excel·lent",
    kmo >= 0.80 ~ "bo",
    kmo >= 0.70 ~ "acceptable",
    kmo >= 0.60 ~ "mediocre",
    kmo >= 0.50 ~ "miserable",
    TRUE ~ "inacceptable"
  )
}

##### --------------------- FUNCIÓ EFA--------------------- #####

fer_efa_bloc <- function(vars, nom_bloc, color_bar, force_nfactors = NULL) {

  sep <- paste0(strrep("=", 60), "\n")
  cat(sep)
  cat(" BLOC:", nom_bloc, "\n")
  cat(sep, "\n")

  # --- Preparació ---
  data_raw <- dades %>%
    select(all_of(vars)) %>%
    mutate(across(everything(), as.numeric))

  data_num <- na.omit(data_raw)
  idx <- as.integer(rownames(data_num))
  n <- nrow(data_num)
  p <- ncol(data_num)

  cat("Observacions (sense NA):", n, "| Variables:", p, "\n\n")

  # --- KMO i Bartlett ---
  poly <- polychoric(data_num, correct = 0)
  R <- poly$rho

  kmo_res <- KMO(R)
  cat("KMO global:", round(kmo_res$MSA, 3),
      paste0("(", kmo_interp(kmo_res$MSA), ")"), "\n")

# Bartlett test: si p < 0.05, podem rebutjar l'hipòtesi de que R és una matriu d'identitat
# és a dir, les variables estan correlacionades i és adequat fer EFA

  bart <- cortest.bartlett(R, n = n)
  cat("Bartlett: chi2 =", round(bart$chisq, 2),
      "| gl =", bart$df,
      "| p =", format.pval(bart$p.value, digits = 3), "\n\n")

  # --- Anàlisi paral·lela ---
  # per determinar el nombre de factors a retenir
  # genera 20 matrius aleatòries i compara els valors propis reals amb els de les matrius simulades
  # només ens quedem amb els factors que tinguin un valor propi superior al de les matrius aleatòries

  pa <- fa.parallel(R, n.obs = n, fm = "pa", fa = "fa",
                    n.iter = 20, plot = FALSE)
  n_factors_pa <- max(pa$nfact, 1)
  if (!is.null(force_nfactors)) {
    n_factors <- force_nfactors
    cat("Anàlisi paral·lela → factors recomanats:", n_factors_pa,
        "| Forçat a:", n_factors, "\n\n")
  } else {
    n_factors <- n_factors_pa
    cat("Anàlisi paral·lela → factors recomanats:", n_factors, "\n\n")
  }

  # Scree plot
  n_plot <- min(p, 12)
  df_scree <- data.frame(
    factor = 1:n_plot,
    real = pa$fa.values[1:n_plot],
    sim  = pa$fa.sim[1:n_plot]
  )
  p_scree <- ggplot(df_scree, aes(x = factor)) +
    geom_line(aes(y = real, color = "Dades reals"), linewidth = 1) +
    geom_point(aes(y = real, color = "Dades reals"), size = 3) +
    geom_line(aes(y = sim, color = "Anàlisi paral·lela"),
              linetype = "dashed", linewidth = 0.8) +
    geom_point(aes(y = sim, color = "Anàlisi paral·lela"), size = 2) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
    geom_vline(xintercept = n_factors + 0.5,
               linetype = "dotted", color = "gray40") +
    annotate("text", x = n_factors + 0.6, y = max(df_scree$real) * 0.9,
             label = paste0("k = ", n_factors), hjust = 0, size = 3.5,
             color = "gray30") +
    scale_color_manual(values = c("Dades reals" = color_bar,
                                  "Anàlisi paral·lela" = "gray50"),
                       name = "") +
    scale_x_continuous(breaks = 1:n_plot) +
    labs(title = paste("Scree plot (anàlisi paral·lela) –", nom_bloc),
         subtitle = paste0("k = ", n_factors, " factors retinguts"),
         x = "Factor", y = "Valor propi") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom")
  print(p_scree)

  # --- EFA policòrica ---
  fa_res <- fa(r = R, nfactors = n_factors, n.obs = n,
               rotate = "oblimin", fm = "pa")
  # assignem scores factorials a les observacions amb regressió
  # es basa en les respostes i les càrregues factorials

  # Puntuacions factorials
  fa_scores_obj <- fa(r = data_num, nfactors = n_factors,
                      rotate = "oblimin", fm = "pa", cor = "poly",
                      scores = "regression")
  scores <- fa_scores_obj$scores

  # Noms genèrics als factors: F1, F2, ...
  fnames <- paste0("F", seq_len(n_factors))
  colnames(scores) <- fnames

  # --- Variança explicada ---
  # variància de cada variable explicada pels factors

  var_exp <- fa_res$Vaccounted
  cat("Variança explicada per factor:\n")
  print(round(var_exp, 3))
  cat("\n")

  # --- Taula de càrregues factorials (ordenada per factor primari) ---
  loads <- as.data.frame(unclass(fa_res$loadings))
  colnames(loads) <- fnames
  loads$h2 <- fa_res$communality
  loads$u2 <- fa_res$uniqueness
  loads$factor_primari <- fnames[apply(abs(loads[, fnames]), 1, which.max)]
  loads_ord <- loads %>% arrange(factor_primari, desc(abs(.data[[fnames[1]]])))

  cat("Càrregues factorials (|λ| ≥ 0.30 marquen la càrrega principal):\n")
  loads_ord %>%
    select(all_of(fnames), h2, u2) %>%
    mutate(across(everything(), ~round(.x, 3))) %>%
    print()
  cat("\n")

  # Comunalitats baixes, variables mal representades, h2 < 0.30
  baixa_h2 <- loads %>%
    filter(h2 < 0.30) %>%
    select(h2, u2) %>%
    mutate(across(everything(), ~round(.x, 3)))
  if (nrow(baixa_h2) > 0) {
    cat("⚠ Variables amb comunalitat < 0.30 (considerar eliminar):\n")
    print(baixa_h2)
    cat("\n")
  }

  # heatmap de càrregues factorials
  loads_plot <- loads[, fnames, drop = FALSE]
  loads_plot$variable <- rownames(loads_plot)

  # Ordenar per factor primari i magnitud de la càrrega
  ord_vars <- loads_ord %>%
    mutate(variable = rownames(.)) %>%
    pull(variable)

  df_loads <- loads_plot %>%
    pivot_longer(all_of(fnames), names_to = "factor", values_to = "carrega") %>%
    mutate(variable = factor(variable, levels = rev(ord_vars)),
           significant = abs(carrega) >= 0.30)

  p_heat <- ggplot(df_loads, aes(x = factor, y = variable, fill = carrega)) +
    geom_tile(color = "white", linewidth = 0.4) +
    geom_text(aes(label = ifelse(significant,
                                 sprintf("%.2f", carrega), ""),
                  fontface = ifelse(abs(carrega) >= 0.50, "bold", "plain")),
              size = 3.2, color = "gray10") +
    scale_fill_gradient2(low = "#E07B54", mid = "white", high = "#4A90B8",
                         midpoint = 0, limits = c(-1, 1),
                         name = "λ") +
    labs(title = paste("Càrregues factorials –", nom_bloc),
         subtitle = "S'indiquen les càrregues |λ| ≥ 0.30 | negreta = |λ| ≥ 0.50",
         x = "Factor", y = "") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(face = "bold"))
  print(p_heat)

  # --- Gràfic: comunalitats ---
  df_comm <- data.frame(
    variable = names(fa_res$communality),
    h2 = fa_res$communality
  ) %>% arrange(h2)

  p_comm <- ggplot(df_comm, aes(x = reorder(variable, h2), y = h2)) +
    geom_col(fill = color_bar, alpha = 0.85) +
    geom_hline(yintercept = 0.30, linetype = "dashed",
               color = "gray40", linewidth = 0.7) +
    geom_hline(yintercept = 0.50, linetype = "dashed",
               color = "gray60", linewidth = 0.7) +
    annotate("text", x = 1.2, y = 0.31, label = "0.30", size = 3, color = "gray40") +
    annotate("text", x = 1.2, y = 0.51, label = "0.50", size = 3, color = "gray60") +
    scale_y_continuous(limits = c(0, 1)) +
    coord_flip() +
    labs(title = paste("Comunalitats (h²) –", nom_bloc),
         x = "", y = "h²") +
    theme_minimal(base_size = 12)
  print(p_comm)

  list(fa = fa_res, fa_scores = fa_scores_obj, scores = scores, idx = idx,
       fnames = fnames, nom = nom_bloc)
}


# Funció auxiliar: afegir puntuacions a dades
afegir_scores <- function(dades, res, prefix) {
  for (f in res$fnames) {
    col <- paste0(prefix, "_", f)
    dades[[col]] <- NA_real_
    dades[res$idx, col] <- res$scores[, f]
  }
  dades
}

# Funció auxiliar: violin + boxplot de cada factor vs GRUP_ASSIST
fer_grafic_scores <- function(res, prefix, df = dades) {
  for (f in res$fnames) {
    col <- paste0(prefix, "_", f)
    df_f <- df %>%
      select(GRUP_ASSIST, score = all_of(col)) %>%
      filter(!is.na(score), !is.na(GRUP_ASSIST))
    mw <- wilcox.test(score ~ GRUP_ASSIST, data = df_f, exact = FALSE)
    p_lab <- ifelse(mw$p.value < 0.001, "p < 0.001",
                    paste0("p = ", round(mw$p.value, 3)))
    p_viol <- ggplot(df_f, aes(x = GRUP_ASSIST, y = score, fill = GRUP_ASSIST)) +
      geom_violin(alpha = 0.4, trim = FALSE) +
      geom_boxplot(width = 0.18, alpha = 0.85, outlier.shape = 21,
                   outlier.size = 1.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
      scale_fill_manual(values = col_grups) +
      labs(title = paste0("Puntuació ", col, " – ", res$nom),
           subtitle = paste0("Mann-Whitney: ", p_lab),
           x = "", y = "Puntuació factorial") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
    print(p_viol)
  }
}

# Gràfic resum Spearman per tots els factors d'un prefix
grafic_spearman_resum <- function(prefix, titol, df = dades) {
  fa_cols <- grep(paste0("^", prefix), names(df), value = TRUE)
  grup_num <- as.integer(df$GRUP_ASSIST == "Regular (≥80%)")
  df_corr <- lapply(fa_cols, function(col) {
    x <- df[[col]]
    mw <- wilcox.test(x ~ df$GRUP_ASSIST, exact = FALSE, na.action = na.omit)
    sp <- cor.test(x, grup_num, method = "spearman", exact = FALSE, use = "complete.obs")
    data.frame(factor = col,
               rho_spearman = round(sp$estimate, 3),
               p_mw = round(mw$p.value, 4),
               stringsAsFactors = FALSE)
  }) %>%
    bind_rows() %>%
    mutate(sig = case_when(p_mw < 0.001 ~ "***", p_mw < 0.01 ~ "**",
                           p_mw < 0.05 ~ "*", TRUE ~ "ns")) %>%
    arrange(rho_spearman)

  ggplot(df_corr,
         aes(x = reorder(factor, rho_spearman),
             y = rho_spearman,
             fill = rho_spearman > 0,
             alpha = sig != "ns")) +
    geom_col() +
    geom_hline(yintercept = 0, linewidth = 0.5, color = "gray30") +
    geom_text(aes(label = sprintf("ρ=%.2f %s", rho_spearman, sig)),
              hjust = ifelse(df_corr$rho_spearman >= 0, -0.1, 1.1), size = 3.2) +
    scale_fill_manual(values = c("TRUE" = "#4A90B8", "FALSE" = "#E07B54"),
                      guide = "none") +
    scale_alpha_manual(values = c("TRUE" = 0.9, "FALSE" = 0.35), guide = "none") +
    coord_flip() +
    expand_limits(y = c(min(df_corr$rho_spearman) - 0.2,
                        max(df_corr$rho_spearman) + 0.2)) +
    labs(title = titol,
         subtitle = "Positiu = Regular > Irregular | Opac = significatiu (p < 0.05, MW)",
         x = "", y = "Spearman ρ") +
    theme_minimal(base_size = 12)
}

##### ---------------------- OUTPUT ----------------------- #####

sink("4. Outputs/4.1 Output_text_EFA.txt")
pdf("4. Outputs/4.2 Output_grafics_EFA.pdf", width = 10, height = 8)


#### ============================================================ ####
####                 1. EFA INICIAL — TOTES LES VARIABLES         ####
#### ============================================================ ####

cat("================================================================\n")
cat("                            EFA INICIAL                         \n")
cat("================================================================\n")

res_motius_v1 <- fer_efa_bloc(motius_vars, "Motius NO assistència [v1]", "#E07B54")
res_estrat_v1 <- fer_efa_bloc(estrategies_vars, "Estratègies assistència [v1]", "#4A90B8")
res_ia_v1 <- fer_efa_bloc(ia_vars, "Ús de la IA [v1]", "#8E6BBF")


#### ============================================================ ####
####                         2. EFA MILLORADA                     ####
#### ============================================================ ####

cat("================================================================\n")
cat("                             EFA REFINADA                       \n")
cat("================================================================\n")

cat("Variables eliminades de dades: M_ACAD, E_PES_AS, IA_PREOC\n")
cat("Variables mantingudes a dades però fora de l'EFA: M_DIST, E_HORA\n\n")

##### --------- 2.1. ELIMINACIÓ VARIABLES IRRELLEVANTS ------ #####

dades_v2 <- dades %>% select(-M_ACAD, -E_PES_AS, -IA_PREOC)

motius_vars_efa <- setdiff(motius_vars, c("M_DIST", "M_ACAD"))
estrategies_vars_efa <- setdiff(estrategies_vars, c("E_HORA", "E_PES_AS"))
ia_vars_efa <- setdiff(ia_vars, "IA_PREOC")

res_motius <- fer_efa_bloc(motius_vars_efa, "Motius NO assistència [refinat]", "#E07B54")
res_estrat <- fer_efa_bloc(estrategies_vars_efa, "Estratègies assistència [refinat]", "#4A90B8")
res_ia <- fer_efa_bloc(ia_vars_efa, "Ús de la IA [refinat]", "#8E6BBF")

##### ------------- 2.2.PUNTUACIONS FACTORIALS  ---------- #####

cat(paste0(strrep("=", 60), "\n"))
cat(" 3. PUNTUACIONS FACTORIALS (EFA refinada)\n")
cat(paste0(strrep("=", 60), "\n\n"))

dades_v2 <- afegir_scores(dades_v2, res_motius, "FA_MOT")
dades_v2 <- afegir_scores(dades_v2, res_estrat, "FA_EST")
dades_v2 <- afegir_scores(dades_v2, res_ia, "FA_IA")

cat("Columnes de puntuacions afegides:\n")
cat(grep("^FA_", names(dades_v2), value = TRUE), sep = ", ")
cat("\n\nFiles amb puntuació (sense NA):\n")
cat("  Motius:      ", sum(!is.na(dades_v2$FA_MOT_F1)), "\n")
cat("  Estratègies: ", sum(!is.na(dades_v2$FA_EST_F1)), "\n")
cat("  IA:          ", sum(!is.na(dades_v2$FA_IA_F1)), "\n\n")

fer_grafic_scores(res_motius, "FA_MOT")
fer_grafic_scores(res_estrat, "FA_EST")
fer_grafic_scores(res_ia, "FA_IA")

##### ------------- 2.3. CORRELACIÓ SPEARMAN AMB GRUP_ASSIST ------ #####
print(grafic_spearman_resum("FA_MOT", "Spearman ρ factors Motius vs GRUP_ASSIST"))
print(grafic_spearman_resum("FA_EST", "Spearman ρ factors Estratègies vs GRUP_ASSIST"))
print(grafic_spearman_resum("FA_IA", "Spearman ρ factors IA vs GRUP_ASSIST"))

# taula resum
all_fa_cols <- grep("^FA_", names(dades_v2), value = TRUE)

grup_num <- as.integer(dades_v2$GRUP_ASSIST == "Regular (≥80%)")

df_fa_corr <- lapply(all_fa_cols, function(col) {
  x <- dades_v2[[col]]
  sp <- cor.test(x, grup_num, method = "spearman", exact = FALSE, use = "complete.obs")
  data.frame(factor = col,
             rho_spearman = round(sp$estimate, 3),
             p_value = round(sp$p.value, 4),
             stringsAsFactors = FALSE)
}) %>%
  bind_rows() %>%
  arrange(rho_spearman)
cat("Correlació Spearman de les puntuacions factorials amb GRUP_ASSIST:\n")
print(df_fa_corr)

#### ============================================================ ####
####                          3. EFA DEFINITIVA                   ####
#### ============================================================ ####

# cal forçar el nombre de factors, perque la variancia dels últims factors
# és molt baixa i no aporten informació útil, i a més no són significatius per diferenciar els grups

cat("================================================================\n")
cat("                                  EFA DEFINITIVA                \n")
cat("================================================================\n\n")
cat("Motius: nfactors = 3 | Estratègies: nfactors = 4 | IA: lliure\n\n")

res_motius_def <- fer_efa_bloc(motius_vars_efa, "Motius NO assistència [definitiu]",
                               "#E07B54", force_nfactors = 3)
res_estrat_def <- fer_efa_bloc(estrategies_vars_efa, "Estratègies assistència [definitiu]",
                               "#4A90B8", force_nfactors = 4)
res_ia_def <- fer_efa_bloc(ia_vars_efa, "Ús de la IA [definitiu]", "#8E6BBF")

##### --------- 3.1. PUNTUACIONS FACTORIALS DEFINITIVES ------ #####

cat("\n\n")

fer_grafic_scores(res_motius_def, "FA_MOT", df = dades_def)
fer_grafic_scores(res_estrat_def, "FA_EST", df = dades_def)
fer_grafic_scores(res_ia_def, "FA_IA", df = dades_def)

##### --------- 3.2. CORRELACIÓ SPEARMAN AMB GRUP_ASSIST ------ #####

print(grafic_spearman_resum("FA_MOT", "Spearman ρ factors Motius [definitiu] vs GRUP_ASSIST", df = dades_def))
print(grafic_spearman_resum("FA_EST", "Spearman ρ factors Estratègies [definitiu] vs GRUP_ASSIST", df = dades_def))
print(grafic_spearman_resum("FA_IA", "Spearman ρ factors IA [definitiu] vs GRUP_ASSIST", df = dades_def))

all_fa_def <- grep("^FA_", names(dades_def), value = TRUE)
grup_num_def <- as.integer(dades_def$GRUP_ASSIST == "Regular (≥80%)")

df_fa_def_corr <- lapply(all_fa_def, function(col) {
  x <- dades_def[[col]]
  sp <- cor.test(x, grup_num_def, method = "spearman", exact = FALSE, use = "complete.obs")
  data.frame(factor = col,
             rho_spearman = round(sp$estimate, 3),
             p_value = round(sp$p.value, 4),
             stringsAsFactors = FALSE)
}) %>%
  bind_rows() %>%
  arrange(rho_spearman)

cat("Correlació Spearman puntuacions definitives amb GRUP_ASSIST:\n")
print(df_fa_def_corr)

##### --------- 3.3. GUARDAR NOMS VARIABLES ------ #####

dades_def <- dades_def %>%
  rename(
    MOT_DESMOTIVACIO  = FA_MOT_F1,
    MOT_AUTOGESTIO    = FA_MOT_F2,
    MOT_FORCA_MAJOR   = FA_MOT_F3,
    EST_QUALITAT_DOC  = FA_EST_F1,
    EST_AVALUACIO_AC  = FA_EST_F2,
    EST_TEMPS_CLASSE  = FA_EST_F3,
    EST_GRUPS_REDUÏTS = FA_EST_F4,
    IA_EINA_ESTUDI    = FA_IA_F1,
    IA_SUBSTITUCIO    = FA_IA_F2
  )


sink()
dev.off()

save(dades_def, file = "2. Dades/4. Dades EFA.RData")
