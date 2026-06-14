packages <- c("fmsb", "ggplot2", "dplyr", "tidyr", "tibble",
              "scales", "gridExtra", "knitr", "e1071", "cluster", "caret")

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

sink("4. Outputs/15.1 Output_text_profiling.txt", append = TRUE, split = TRUE)
pdf("4. Outputs/15.2 Output_grafics_profiling.pdf", width = 10, height = 8)

col_clusters <- c("1" = "#4A90B8", "2" = "#E07B54")

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

calc_pc <- function(U) mean(rowSums(U^2))
calc_pe <- function(U) {
  U_s <- pmax(U, .Machine$double.eps)
  -mean(rowSums(U_s * log(U_s)))
}
calc_xb <- function(X, U, centers, m) {
  n <- nrow(X); c_num <- nrow(centers); num <- 0
  for (k in seq_len(c_num)) {
    d_sq <- rowSums((X - matrix(centers[k, ], nrow = n, ncol = ncol(X), byrow = TRUE))^2)
    num  <- num + sum(U[, k]^m * d_sq)
  }
  dists <- as.matrix(dist(centers))^2; diag(dists) <- Inf
  num / (n * min(dists))
}

fcm_best <- function(X, c_num, m, nstart = 25) {
  best_pc <- -Inf; best_fit <- NULL
  for (i in seq_len(nstart)) {
    fit <- tryCatch(
      e1071::cmeans(X, centers = c_num, iter.max = 200, dist = "euclidean", m = m),
      error = function(e) NULL
    )
    if (is.null(fit)) next
    pc <- calc_pc(fit$membership)
    if (pc > best_pc) { best_pc <- pc; best_fit <- fit }
  }
  best_fit
}

wilcox_rb <- function(x1, x2) {
  n   <- length(x1) + length(x2)
  res <- wilcox.test(x1, x2, exact = FALSE)
  z   <- qnorm(pmax(res$p.value / 2, 1e-15), lower.tail = FALSE)
  dir <- sign(mean(x1, na.rm = TRUE) - mean(x2, na.rm = TRUE))
  list(W = unname(res$statistic), p = res$p.value, r = round(dir * z / sqrt(n), 3))
}

cramers_v <- function(x, y) {
  tab  <- table(x, y)
  chi2 <- suppressWarnings(chisq.test(tab, correct = FALSE)$statistic)
  n    <- sum(tab); k <- min(nrow(tab), ncol(tab))
  sqrt(unname(chi2) / (n * (k - 1)))
}

signif_stars <- function(p) {
  ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "ns")))
}

# ================================================================
cat("=================================================================\n")
cat("   FASE 4 — PROFILING ENRIQUIT\n")
cat("=================================================================\n\n")
# ================================================================

# --- 4.1 Reassignació completa ---
cat("--- 4.1 Reassignació completa (totes les obs) ---\n\n")

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
    GRUP_ASSIST       = GRUP_ASSIST,
    P_ASSIST          = as.numeric(P_ASSIST),
    MOT_DESMOTIVACIO  = MOT_DESMOTIVACIO,
    MOT_AUTOGESTIO    = MOT_AUTOGESTIO,
    MOT_FORCA_MAJOR   = MOT_FORCA_MAJOR,
    EST_QUALITAT_DOC  = EST_QUALITAT_DOC,
    EST_AVALUACIO_AC  = EST_AVALUACIO_AC,
    EST_TEMPS_CLASSE  = EST_TEMPS_CLASSE,
    EST_GRUPS_REDUITS = EST_GRUPS_REDUITS,
    IA_EINA_ESTUDI    = IA_EINA_ESTUDI,
    IA_SUBSTITUCIO    = IA_SUBSTITUCIO
  )

cc_all <- complete.cases(df_all[, vars_clust])
cat(sprintf("Total obs: %d | Completes (clustering): %d | Excloses: %d\n\n",
            nrow(df_all), sum(cc_all), sum(!cc_all)))

X_all_raw <- as.matrix(df_all[cc_all, vars_clust])
X_all_sc  <- scale(X_all_raw, center = scale_params$mean, scale = scale_params$sd)

U_all    <- proj_test(X_all_sc, fcm_final$centers, m_final)
hard_all <- apply(U_all, 1, which.max)
max_u_all <- apply(U_all, 1, max)

df_all$cluster_hard <- NA_integer_
df_all$u1    <- NA_real_
df_all$u2    <- NA_real_
df_all$max_u <- NA_real_

df_all$cluster_hard[cc_all] <- hard_all
df_all$u1[cc_all]    <- U_all[, 1]
df_all$u2[cc_all]    <- U_all[, 2]
df_all$max_u[cc_all] <- max_u_all

dades_def$cluster_hard <- df_all$cluster_hard
dades_def$u1    <- df_all$u1
dades_def$u2    <- df_all$u2
dades_def$max_u <- df_all$max_u

df_cc <- df_all[cc_all, ]
df_cc$cluster_f <- factor(df_cc$cluster_hard)

cat(sprintf("Cluster 1: %d obs (%.1f%%) | Cluster 2: %d obs (%.1f%%)\n\n",
            sum(hard_all == 1), mean(hard_all == 1) * 100,
            sum(hard_all == 2), mean(hard_all == 2) * 100))

# --- 4.2 Taula resum per cluster ---
cat("--- 4.2 Taula resum per cluster ---\n\n")

for (cl in 1:c_final) {
  df_cl <- df_cc[df_cc$cluster_hard == cl, ]
  cat(sprintf("Cluster %d: n=%d (%.1f%% del total)\n", cl, nrow(df_cl),
              nrow(df_cl) / nrow(df_cc) * 100))
  cat(sprintf("  Regular (>=80%%): %.1f%% | Irregular: %.1f%%\n",
              mean(df_cl$GRUP_ASSIST == "Regular (≥80%)", na.rm = TRUE),
              mean(df_cl$GRUP_ASSIST != "Regular (≥80%)", na.rm = TRUE)))
  cat(sprintf("  P_ASSIST: mitjana=%.1f%% | mediana=%.1f%%\n\n",
              mean(df_cl$P_ASSIST, na.rm = TRUE),
              median(df_cl$P_ASSIST, na.rm = TRUE)))
}

chi2_res <- suppressWarnings(
  chisq.test(table(df_cc$GRUP_ASSIST, df_cc$cluster_hard), correct = FALSE)
)
v_cram <- cramers_v(df_cc$GRUP_ASSIST, df_cc$cluster_hard)
cat(sprintf("Chi2 (GRUP_ASSIST vs cluster): chi2=%.3f, df=%d, p=%.4f\n",
            chi2_res$statistic, chi2_res$parameter, chi2_res$p.value))
cat(sprintf("V de Cramer = %.3f %s\n\n", v_cram, signif_stars(chi2_res$p.value)))

pa1 <- c(p_assist_train[hard_train == 1], p_assist_test[hard_test == 1])
pa2 <- c(p_assist_train[hard_train == 2], p_assist_test[hard_test == 2])
pa1 <- pa1[!is.na(pa1)]
pa2 <- pa2[!is.na(pa2)]
mw_pa <- wilcox_rb(pa1, pa2)
cat(sprintf("Mann-Whitney P_ASSIST: W=%.0f, p=%.4f, r=%.3f %s\n\n",
            mw_pa$W, mw_pa$p, mw_pa$r, signif_stars(mw_pa$p)))

# --- 4.3 Profiling variables clustering (escala original) ---
cat("--- 4.3 Profiling variables clustering ---\n\n")

prof_clust <- do.call(rbind, lapply(vars_clust, function(v) {
  x1 <- df_cc[[v]][df_cc$cluster_hard == 1]
  x2 <- df_cc[[v]][df_cc$cluster_hard == 2]
  mw <- wilcox_rb(x1, x2)
  data.frame(
    Variable   = v,
    Cluster1   = round(mean(x1, na.rm = TRUE), 3),
    Cluster2   = round(mean(x2, na.rm = TRUE), 3),
    Diferencia = round(mean(x1, na.rm = TRUE) - mean(x2, na.rm = TRUE), 3),
    p_valor    = round(mw$p, 4),
    r_rb       = mw$r,
    Signif     = signif_stars(mw$p),
    stringsAsFactors = FALSE
  )
}))
prof_clust <- prof_clust[order(-abs(prof_clust$r_rb)), ]
print(prof_clust, row.names = FALSE)
cat("\n")

# --- 4.4 Profiling factors EFA (post-hoc) ---
cat("--- 4.4 Profiling factors EFA (post-hoc, no entren al clustering) ---\n\n")

vars_efa <- c("MOT_DESMOTIVACIO", "MOT_AUTOGESTIO", "MOT_FORCA_MAJOR",
              "EST_QUALITAT_DOC", "EST_AVALUACIO_AC", "EST_TEMPS_CLASSE",
              "EST_GRUPS_REDUITS", "IA_EINA_ESTUDI", "IA_SUBSTITUCIO")

prof_efa <- do.call(rbind, lapply(vars_efa, function(v) {
  if (!v %in% names(df_cc)) return(NULL)
  x1 <- df_cc[[v]][df_cc$cluster_hard == 1]; x1 <- x1[!is.na(x1)]
  x2 <- df_cc[[v]][df_cc$cluster_hard == 2]; x2 <- x2[!is.na(x2)]
  if (length(x1) < 3 || length(x2) < 3) return(NULL)
  mw <- wilcox_rb(x1, x2)
  data.frame(
    Factor     = v,
    Cluster1   = round(mean(x1), 3),
    Cluster2   = round(mean(x2), 3),
    Diferencia = round(mean(x1) - mean(x2), 3),
    p_valor    = round(mw$p, 4),
    r_rb       = mw$r,
    Signif     = signif_stars(mw$p),
    stringsAsFactors = FALSE
  )
}))
prof_efa <- prof_efa[order(-abs(prof_efa$r_rb)), ]
print(prof_efa, row.names = FALSE)
cat("\n")

sig_efa <- prof_efa$Factor[prof_efa$p_valor < 0.05]
cat(sprintf("Factors EFA significatius (p<0.05): %s\n\n",
            if (length(sig_efa) > 0) paste(sig_efa, collapse = ", ") else "cap"))

# --- 4.5 Grafics de profiling ---
cat("--- 4.5 Grafics ---\n\n")

# Grafic A — Radar chart
var_mins <- apply(df_cc[, vars_clust], 2, min, na.rm = TRUE)
var_maxs <- apply(df_cc[, vars_clust], 2, max, na.rm = TRUE)
norm_fn  <- function(x, lo, hi) (x - lo) / pmax(hi - lo, 1e-6)

c1_norm <- norm_fn(
  colMeans(df_cc[df_cc$cluster_hard == 1, vars_clust], na.rm = TRUE), var_mins, var_maxs
)
c2_norm <- norm_fn(
  colMeans(df_cc[df_cc$cluster_hard == 2, vars_clust], na.rm = TRUE), var_mins, var_maxs
)

radar_df <- as.data.frame(rbind(
  max      = rep(1, length(vars_clust)),
  min      = rep(0, length(vars_clust)),
  Cluster1 = c1_norm,
  Cluster2 = c2_norm
))
colnames(radar_df) <- vars_clust

par(mar = c(2, 2, 3, 2))
fmsb::radarchart(
  radar_df,
  axistype   = 1,
  pcol       = c("#4A90B8", "#E07B54"),
  pfcol      = c(adjustcolor("#4A90B8", 0.25), adjustcolor("#E07B54", 0.25)),
  plwd       = 2.5,
  cglcol     = "grey70", cglty = 1, cglwd = 0.8,
  axislabcol = "grey40",
  vlcex      = 0.72,
  title      = "Perfil FCM — variables clustering (min-max [0,1])"
)
legend("topright",
       legend = c("Cluster 1", "Cluster 2"),
       col = c("#4A90B8", "#E07B54"),
       lwd = 2, bty = "n", cex = 0.9)

# Grafic B — Barres EFA
efa_plot_df <- prof_efa %>%
  mutate(Factor = factor(Factor, levels = rev(Factor[order(abs(Diferencia))]))) %>%
  pivot_longer(cols = c(Cluster1, Cluster2),
               names_to = "Cluster", values_to = "Mitjana") %>%
  mutate(Cluster = gsub("Cluster", "Cluster ", Cluster))

signif_df <- prof_efa %>%
  mutate(
    Factor = factor(Factor, levels = levels(efa_plot_df$Factor)),
    y_pos  = pmax(Cluster1, Cluster2) + 0.04
  )

print(
  ggplot(efa_plot_df, aes(x = Mitjana, y = Factor, fill = Cluster)) +
    geom_col(position = "dodge", alpha = 0.85, width = 0.65) +
    geom_text(data = signif_df,
              aes(x = y_pos, y = Factor, label = Signif),
              inherit.aes = FALSE, size = 4.5, hjust = 0, color = "grey30") +
    scale_fill_manual(values = c("Cluster 1" = "#4A90B8", "Cluster 2" = "#E07B54")) +
    labs(
      title    = "Factors EFA per cluster — profiling post-hoc",
      subtitle = "Mitjana per cluster | *** p<0.001, ** p<0.01, * p<0.05, ns",
      x = "Puntuació mitjana factor EFA", y = "", fill = "Cluster"
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "top")
)

# Grafic C — Distribucio P_ASSIST
# Usem p_assist_train/test + hard_train/test del .RData (garantit sense NA)
df_cc_pa <- data.frame(
  P_ASSIST  = c(p_assist_train, p_assist_test),
  cluster_f = factor(c(hard_train, hard_test))
)
df_cc_pa <- df_cc_pa[!is.na(df_cc_pa$P_ASSIST), ]
mw_lbl <- sprintf("Mann-Whitney: W=%.0f, p=%.4f, r=%.3f", mw_pa$W, mw_pa$p, mw_pa$r)

print(
  ggplot(df_cc_pa, aes(x = cluster_f, y = P_ASSIST, fill = cluster_f)) +
    geom_violin(alpha = 0.45, trim = FALSE) +
    geom_boxplot(width = 0.14, alpha = 0.80, outlier.shape = 21, outlier.size = 2) +
    scale_y_continuous(breaks = seq(0, 100, 20)) +
    coord_cartesian(ylim = c(0, 100)) +
    scale_fill_manual(values = col_clusters, guide = "none") +
    scale_x_discrete(labels = c("1" = "Cluster 1", "2" = "Cluster 2")) +
    labs(
      title    = "Distribució P_ASSIST per cluster",
      subtitle = mw_lbl,
      x = "Cluster", y = "% Assistència"
    ) +
    theme_minimal(base_size = 13)
)

# Grafic D — Heatmap de nitidesa (zona ambigüitat)
n_ambig <- sum(df_cc$u1 >= 0.4 & df_cc$u1 <= 0.6)

print(
  ggplot(df_cc, aes(x = u1, y = cluster_f, color = cluster_f)) +
    annotate("rect", xmin = 0.4, xmax = 0.6, ymin = 0.35, ymax = 2.65,
             fill = "#FDEBD0", alpha = 0.55, color = "#E67E22", linetype = "dashed") +
    geom_jitter(height = 0.18, size = 2, alpha = 0.65) +
    annotate("text", x = 0.50, y = 0.62,
             label = sprintf("Ambig.\nn=%d", n_ambig),
             size = 3.2, color = "#E67E22", hjust = 0.5) +
    scale_color_manual(values = col_clusters, guide = "none") +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_y_discrete(labels = c("1" = "Cluster 1", "2" = "Cluster 2")) +
    labs(
      title    = "Nitidesa de la pertenènça fuzzy",
      subtitle = sprintf("u1 = prob. Cluster 1 | zona ambigua [0.4, 0.6]: n=%d (%.1f%%)",
                         n_ambig, n_ambig / nrow(df_cc) * 100),
      x = "u1 (probabilitat pertenènça Cluster 1)", y = "Cluster hard"
    ) +
    theme_minimal(base_size = 13)
)

# --- 4.6 Taula resum interpretativa ---
cat("--- 4.6 Taula resum interpretativa ---\n\n")

all_prof <- rbind(
  prof_clust %>% rename(Factor = Variable) %>% mutate(Tipus = "Clustering"),
  prof_efa %>% mutate(Tipus = "EFA (post-hoc)")
) %>%
  arrange(desc(abs(r_rb))) %>%
  dplyr::select(Tipus, Factor, Cluster1, Cluster2, Diferencia, p_valor, r_rb, Signif)

print(all_prof, row.names = FALSE)
cat("\n")

# ================================================================
cat("=================================================================\n")
cat("   FASE 5 — TEST c=3 (VALIDACIO OPCIONAL)\n")
cat("=================================================================\n\n")
# ================================================================

cat("--- 5.1 FCM c=3, m=1.5, nstart=25 sobre X_train ---\n\n")

set.seed(2024)
fcm_c3  <- fcm_best(X_train, c_num = 3, m = 1.5, nstart = 25)

if (!is.null(fcm_c3)) {
  U_c3    <- fcm_c3$membership
  hard_c3 <- apply(U_c3, 1, which.max)

  # --- 5.2 Metriques c=3 ---
  cat("--- 5.2 Metriques c=3 ---\n\n")
  pc_c3  <- calc_pc(U_c3)
  pe_c3  <- calc_pe(U_c3)
  xb_c3  <- calc_xb(X_train, U_c3, fcm_c3$centers, m = 1.5)
  sil_c3 <- if (length(unique(hard_c3)) > 1)
    mean(cluster::silhouette(hard_c3, dist(X_train))[, "sil_width"])
  else NA_real_

  cat(sprintf("c=3: PC=%.4f | PE=%.4f | XB=%.4f | Silhouette=%.4f\n\n",
              pc_c3, pe_c3, xb_c3, sil_c3))

  # --- 5.3 Comparacio c=2 vs c=3 ---
  cat("--- 5.3 Comparacio c=2 vs c=3 ---\n\n")

  pc_c2  <- calc_pc(U_train)
  pe_c2  <- calc_pe(U_train)
  xb_c2  <- calc_xb(X_train, U_train, fcm_final$centers, m_final)
  sil_c2 <- if (length(unique(hard_train)) > 1)
    mean(cluster::silhouette(hard_train, dist(X_train))[, "sil_width"])
  else NA_real_

  comp_c <- data.frame(
    c          = c(2, 3),
    m          = c(m_final, 1.5),
    PC         = round(c(pc_c2, pc_c3), 4),
    PE         = round(c(pe_c2, pe_c3), 4),
    XB         = round(c(xb_c2, xb_c3), 4),
    Silhouette = round(c(sil_c2, sil_c3), 4)
  )
  print(comp_c, row.names = FALSE)
  cat("\n")

  # --- 5.4 Decisio ---
  cat("--- 5.4 Decisio ---\n\n")
  sil_c2_v <- if (is.na(sil_c2)) 0 else sil_c2
  sil_c3_v <- if (is.na(sil_c3)) 0 else sil_c3

  if (sil_c3_v > sil_c2_v + 0.02) {
    cat(sprintf(
      "Silhouette(c=3)=%.4f > Silhouette(c=2)=%.4f + 0.02\n",
      sil_c3_v, sil_c2_v
    ))
    cat("NOTA: Millora detectada, pero es manté c=2 per coherencia amb el model principal.\n\n")
  } else {
    cat(sprintf(
      "Silhouette(c=3)=%.4f <= Silhouette(c=2)=%.4f + 0.02 -> Es manté c=2.\n\n",
      sil_c3_v, sil_c2_v
    ))
  }

  # --- 5.5 Centroides c=3 desescalats ---
  cat("--- 5.5 Centroides c=3 (escala original) ---\n\n")
  centers_c3_orig <- sweep(
    sweep(fcm_c3$centers, 2, scale_params$sd, "*"),
    2, scale_params$mean, "+"
  )
  rownames(centers_c3_orig) <- paste0("Cluster_", seq_len(3))
  print(round(t(centers_c3_orig), 2))
  cat("\n")

  # Projecció c=3 sobre totes les obs completes
  U_all_c3    <- proj_test(X_all_sc, fcm_c3$centers, m = 1.5)
  hard_all_c3 <- apply(U_all_c3, 1, which.max)
  cat("Distribucio c=3 (totes les obs completes):\n")
  print(table(Cluster = hard_all_c3))
  cat("\n")

} else {
  cat("ERROR: fcm_best no ha convergit per c=3.\n\n")
}

# ================================================================
# GUARDAR RESULTATS PROFILING
# ================================================================

cat("--- Guardant resultats de profiling ---\n\n")

save(
  fcm_final, scale_params, c_final, m_final,
  U_all, hard_all,
  U_train, hard_train, U_test, hard_test,
  X_train, X_test, X_all_sc,
  y_valid_train, y_valid_test,
  p_assist_train, p_assist_test,
  vars_clust, df_cc,
  dades_def,
  file = "2. Dades/fuzzy_profiling.RData"
)

dev.off()
sink()
