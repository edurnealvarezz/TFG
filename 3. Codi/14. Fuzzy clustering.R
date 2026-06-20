packages <- c(
  "e1071", "cluster", "class", "caret", "pROC",
  "ggplot2", "fmsb", "factoextra", "dplyr", "tidyr", "tibble", "scales"
)

install_if_missing <- function(pkg) {
 if (!require(pkg, character.only = TRUE)) {
 install.packages(pkg); library(pkg, character.only = TRUE)
 }
}

lapply(packages, install_if_missing)
rm(packages)

setwd("C:/Users/edurn/Downloads/TFG")
#setwd("C:/Users/Edurne/Downloads/TFG")
load("2. Dades/5. Dades Logit.RData")

sink("4. Outputs/14.1 Output_text_fuzzy.txt", split = TRUE)
pdf("4. Outputs/14.2 Output_grafics_fuzzy.pdf", width = 10, height = 7)

# -----------------------------------------------------------------------
# FUNCIONS AUXILIARS
# -----------------------------------------------------------------------

# Partition Coefficient: mesura la nitidesa dels clusters (quant més proper a 1 millor)
calc_pc <- function(U) mean(rowSums(U^2))

# Partition Entropy: mesura la incertesa de les assignacions (quant més petit millor)
calc_pe <- function(U) {
  U_safe <- pmax(U, .Machine$double.eps)
  -mean(rowSums(U_safe * log(U_safe)))
}

# Xie-Beni index (quant més petit millor):
# XB = homogeneitat intra-cluster / heterogeneitat inter-cluster

calc_xb <- function(X, U, centers, m) {
  n <- nrow(X)
  c_num <- nrow(centers)
  num <- 0
  for (k in seq_len(c_num)) {
    d_sq <- rowSums((X - matrix(centers[k, ], nrow = n, ncol = ncol(X), byrow = TRUE))^2)
    num <- num + sum(U[, k]^m * d_sq)
  }
  dists_cent <- as.matrix(dist(centers))^2
  diag(dists_cent) <- Inf
  num / (n * min(dists_cent))
}

# FCM amb 25 diferents inicialitzacions. retorna el model amb el PC més alt
# ens diu quin grau de pertinença té cada individu a cada cluster
fcm_best <- function(X, c_num, m, nstart = 25) {
  best_pc <- -Inf
  best_fit <- NULL
  for (i in seq_len(nstart)) {
    fit <- tryCatch(
      e1071::cmeans(X, centers = c_num, iter.max = 200, dist = "euclidean", m = m),
      error = function(e) NULL
    )
    if (is.null(fit)) next
    pc <- calc_pc(fit$membership)
    if (pc > best_pc) {
      best_pc <- pc
      best_fit <- fit
    }
  }
  best_fit
}

# Projecció de noves obs sobre centroides fixos del train (evita data leakage)
proj_test <- function(X_test, centers, m) {
  n <- nrow(X_test)
  c_num <- nrow(centers)
  exp_val <- 2 / (m - 1)
  D <- matrix(0, nrow = n, ncol = c_num)
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
      for (k in seq_len(c_num)) U[i, k] <- 1 / sum((d[k] / d)^exp_val)
    }
  }
  U
}

# Normalitzar
norm01 <- function(x, invert = FALSE) {
  r <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  if (invert) 1 - r else r
}

# ================================================================= #
#                       1. PREPROCESSAMENT                          # 
# ================================================================= #

cat("================ 1. PREPROCESSAMENT ================\n")

# agafem només les dades que podem observar ABANS de l'inici de curs

df_raw <- dades_def %>%
  transmute(
    # Grup A: sociodemogràfiques i acadèmiques
    EDAT           = as.numeric(EDAT),
    DESPL          = as.numeric(DESPL),
    N_ASSIG        = as.numeric(N_ASSIG),
    NOTA_num       = as.numeric(NOTA),          # ordered factor [5-5.9]…[≥9] → 1-5
    T_AVAL_num     = as.integer(T_AVAL == "Continuada"),
    CURS_1R        = as.integer(CURS_1R),
    GENERE_Home    = as.integer(GENERE == "Home"),
    DOBLE_GRAU_EST = as.integer(DOBLE_GRAU_EST),
    DEDIC_num      = as.integer(DEDIC),         # ordered factor E.Complet…T.Complet → 1-4
    # Grup B: ús de la IA (Likert 1-6)
    IA_HABIT       = as.integer(IA_HABIT),
    IA_COMPR       = as.integer(IA_COMPR),
    IA_REND        = as.integer(IA_REND),
    IA_PDFS        = as.integer(IA_PDFS),
    IA_SUBST       = as.integer(IA_SUBST),
    IA_ATENC       = as.integer(IA_ATENC),
    IA_CONF        = as.integer(IA_CONF),
    # Variables de validació (NO entren al clustering)
    GRUP_ASSIST    = GRUP_ASSIST,
    P_ASSIST       = as.numeric(P_ASSIST)
  ) %>%
  filter(complete.cases(.))

cat(sprintf("Observacions originals: %d\n", nrow(dades_def)))
cat(sprintf("Observacions completes: %d (eliminats %d per NAs)\n\n",
            nrow(df_raw), nrow(dades_def) - nrow(df_raw)))

vars_clust <- c(
  "EDAT", "DESPL", "N_ASSIG", "NOTA_num", "T_AVAL_num", "CURS_1R",
  "GENERE_Home", "DOBLE_GRAU_EST", "DEDIC_num",
  "IA_HABIT", "IA_COMPR", "IA_REND", "IA_PDFS", "IA_SUBST", "IA_ATENC", "IA_CONF"
)

X_raw <- as.matrix(df_raw[, vars_clust])

# funció paràmetres per escalar
scale_params <- list(
  mean = colMeans(X_raw),
  sd   = apply(X_raw, 2, sd)
)
X_sc <- scale(X_raw, center = scale_params$mean, scale = scale_params$sd)

cat("Paràmetres d'escala guardats (mean i sd per variable):\n")
print(round(rbind(mean = scale_params$mean, sd = scale_params$sd), 3))
cat("\n")

# 1.3 Partició estratificada train (80%) / test (20%)
set.seed(1234)
idx_train <- caret::createDataPartition(df_raw$GRUP_ASSIST, p = 0.80, list = FALSE)
X_train <- X_sc[idx_train, ]
X_test  <- X_sc[-idx_train, ]

y_valid_train  <- df_raw$GRUP_ASSIST[idx_train]
y_valid_test   <- df_raw$GRUP_ASSIST[-idx_train]
p_assist_train <- df_raw$P_ASSIST[idx_train]
p_assist_test  <- df_raw$P_ASSIST[-idx_train]

cat(sprintf("Partició: Train = %d obs | Test = %d obs\n", nrow(X_train), nrow(X_test)))
cat(sprintf("  Train — Regular: %.1f%% | Irregular: %.1f%%\n",
            mean(y_valid_train == "Regular (≥80%)") * 100,
            mean(y_valid_train != "Regular (≥80%)") * 100))
cat(sprintf("  Test  — Regular: %.1f%% | Irregular: %.1f%%\n\n",
            mean(y_valid_test == "Regular (≥80%)") * 100,
            mean(y_valid_test != "Regular (≥80%)") * 100))

# Heatmap de correlació de Pearson
cor_mat <- cor(X_train, method = "pearson")
cor_df <- as.data.frame(as.table(cor_mat)) %>%
  rename(Var1 = Var1, Var2 = Var2, corr = Freq)

print(
  ggplot(cor_df, aes(x = Var1, y = Var2, fill = corr)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      low = "#2166AC", mid = "white", high = "#B2182B",
      midpoint = 0, limits = c(-1, 1), name = "r Pearson"
    ) +
    geom_text(aes(label = sprintf("%.2f", corr)), size = 2.0) +
    labs(
      title = "Heatmap de correlació — variables d'entrada (train, z-score)",
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x  = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y  = element_text(size = 12),
      plot.title   = element_text(face = "bold"),
      legend.position = "right",
      legend.text = element_text(size = 12)
    )
)

# ========================================================== #
#           2. DETERMINACIÓ DEL NOMBRE ÒPTIM DE CLUSTERS (c)
# ========================================================== #

cat(" =========== 2. SELECCIÓ NOMBRE DE CLUSTERS ===========\n")

c_vals <- 2:5
resultats_c <- tibble(
  c          = integer(),
  PC         = numeric(),
  PE         = numeric(),
  Silhouette = numeric(),
  XB         = numeric()
)

set.seed(1234)
for (c_i in c_vals) {
  cat(sprintf("Calculant criteris per c=%d...\n", c_i))
  fit_i   <- fcm_best(X_train, c_num = c_i, m = 2, nstart = 25)
  U_i     <- fit_i$membership
  hard_i  <- apply(U_i, 1, which.max)

  pc_i  <- calc_pc(U_i)
  pe_i  <- calc_pe(U_i)
  xb_i  <- calc_xb(X_train, U_i, fit_i$centers, m = 2)
  sil_i <- if (length(unique(hard_i)) > 1) {
    mean(cluster::silhouette(hard_i, dist(X_train))[, "sil_width"])
  } else NA_real_

  resultats_c <- add_row(resultats_c,
    c = c_i, PC = pc_i, PE = pe_i, Silhouette = sil_i, XB = xb_i
  )
}

cat("\n--- Taula de criteris ---\n")
print(resultats_c %>% mutate(across(where(is.numeric), ~ round(.x, 4))))
cat("\n")

c_opt_PC <- resultats_c$c[which.max(resultats_c$PC)]
c_opt_PE <- resultats_c$c[which.min(resultats_c$PE)]
c_opt_Sil <- resultats_c$c[which.max(resultats_c$Silhouette)]
c_opt_XB <- resultats_c$c[which.min(resultats_c$XB)]

cat(sprintf("PC màxim → c=%d\n", c_opt_PC))
cat(sprintf("PE mínim → c=%d\n", c_opt_PE))
cat(sprintf("Silhouette màxim → c=%d\n", c_opt_Sil))
cat(sprintf("Xie-Beni mínim → c=%d\n\n", c_opt_XB))

vots <- table(c(c_opt_PC, c_opt_PE, c_opt_Sil, c_opt_XB)) # posem els resultats a un vector
c_final <- as.integer(names(vots)[which.max(vots)]) # mirem el que més ha sortit
cat(sprintf(">>> c òptim seleccionat: c=%d (per majoria de criteris)\n\n", c_final))

# Criteris normalitzats vs c
plot_df_c <- tibble(
  c       = rep(c_vals, 4),
  valor   = c(
    norm01(resultats_c$PC),
    norm01(resultats_c$PE,         invert = TRUE),
    norm01(resultats_c$Silhouette),
    norm01(resultats_c$XB,         invert = TRUE)
  ),
  criteri = rep(
    c("PC (↑)", "PE (↓, inv.)", "Silhouette (↑)", "XB (↓, inv.)"),
    each = length(c_vals)
  )
)

# resultats dels criteris per cada num de cluster
print(
  ggplot(plot_df_c, aes(x = c, y = valor, color = criteri, group = criteri)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 3) +
    geom_vline(xintercept = c_final, linetype = "dashed", color = "grey40") +
    annotate("text", x = c_final + 0.08, y = 0.04,
             label = paste0("c = ", c_final, " seleccionat"),
             hjust = 0, size = 3.5, color = "grey30") +
    scale_x_continuous(breaks = c_vals) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_color_manual(values = c("#E07B54", "#4A90B8", "#8E6BBF", "#5AAE61")) +
    labs(
      title    = "Selecció del nombre òptim de clusters (c) — FCM (m=2)",
      subtitle = "Criteris normalitzats a [0,1]; en tots els casos, valor més alt = millor",
      x = "Nombre de clusters (c)", y = "Valor normalitzat [0,1]",
      color = "Criteri"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title    = element_text(face = "bold"),
      legend.position = "bottom",
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      legend.text = element_text(size = 12)
    )
)

# ============================================================== #
#                    3. FUZZY C-MEANS FINAL
# ============================================================== #

cat(" ============= 3. FUZZY C-MEANS FINAL ============= \n")

# 3.1 Sensibilitat al fuzziness exponent m
# mesura quant de difús pot ser un cluster, conforme més gran sigui els individus 
# poden pertànyer a més clusters

cat("--- 3.1 Selecció de m (fuzziness exponent) ---\n")
m_vals <- c(1.5, 2.0, 2.5, 3)
resultats_m <- tibble(m = numeric(), PC = numeric(), PE = numeric(), XB = numeric())

set.seed(1234)
for (m_i in m_vals) {
  fit_i <- fcm_best(X_train, c_num = c_final, m = m_i, nstart = 25)
  pc_i  <- calc_pc(fit_i$membership)
  pe_i  <- calc_pe(fit_i$membership)
  xb_i  <- calc_xb(X_train, fit_i$membership, fit_i$centers, m_i)
  resultats_m <- add_row(resultats_m, m = m_i, PC = pc_i, PE = pe_i, XB = xb_i)
  cat(sprintf("m=%.1f | PC=%.4f | PE=%.4f | XB=%.4f\n", m_i, pc_i, pe_i, xb_i))
}

m_final <- resultats_m$m[which.max(resultats_m$PC)]
cat(sprintf("\n>>> m òptim seleccionat: m=%.1f (màxim PC)\n\n", m_final))

# 3.2 Model FCM final (millor de 25 arrencades)
cat("--- 3.2 Model FCM final ---\n")
set.seed(1234)
fcm_final <- fcm_best(X_train, c_num = c_final, m = m_final, nstart = 25)

U_train    <- fcm_final$membership
hard_train <- apply(U_train, 1, which.max) # assignació dura
max_u_train <- apply(U_train, 1, max)

cat(sprintf("c = %d | m = %.1f | nstart = 25\n\n", c_final, m_final))
cat(sprintf("PC final  = %.4f\n", calc_pc(U_train)))
cat(sprintf("PE final  = %.4f\n", calc_pe(U_train)))
cat(sprintf("XB final  = %.4f\n\n", calc_xb(X_train, U_train, fcm_final$centers, m_final)))

# distribució d'alumnes si fos amb m=1 (clustering normal)
cat("Distribució d'alumnes per cluster (hard assignment — train):\n")
print(table(Cluster = hard_train))
cat(sprintf("\nProporcions: %s\n\n",
            paste(round(prop.table(table(hard_train)) * 100, 1), collapse = "% / "), "%"))

cat("Estadística de nitidesa — max(u_ik):\n")
print(summary(max_u_train))
cat(sprintf("Obs amb max(u) > 0.60: %d (%.1f%%)\n",
            sum(max_u_train > 0.6), mean(max_u_train > 0.6) * 100))
cat(sprintf("Obs amb max(u) > 0.80: %d (%.1f%%)\n\n",
            sum(max_u_train > 0.8), mean(max_u_train > 0.8) * 100))

# Centroides en escala original per a poder interpretar-los
cat("Centroides FCM (escala original):\n")
centers_orig <- sweep(
  sweep(fcm_final$centers, 2, scale_params$sd, "*"),
  2, scale_params$mean, "+"
)
rownames(centers_orig) <- paste0("Cluster_", seq_len(c_final))
print(round(t(centers_orig), 2))
cat("\n")

# Projecció del test sobre centroides del train
U_test     <- proj_test(X_test, fcm_final$centers, m_final)
hard_test  <- apply(U_test, 1, which.max)
max_u_test <- apply(U_test, 1, max)

cat("Distribució per cluster (test — projecció sobre centroides train):\n")
print(table(Cluster = hard_test))
cat(sprintf("\nEstadística nitidesa test — max(u_ik):\n"))
print(summary(max_u_test))
cat("\n")

# distribució de probabilitats de pertinença per cluster (train)
# cal veure que els que estan més a prop del seu cluster assignat (hard)
# tinguin u_ik més altes, i els que estan més difusos tinguin u_ik més baixes
u_df <- as.data.frame(U_train) %>%
  setNames(paste0("Cluster_", seq_len(c_final))) %>%
  mutate(cluster_hard = factor(paste0("Hard: ", hard_train))) %>%
  pivot_longer(
    cols      = starts_with("Cluster_"),
    names_to  = "cluster_k",
    values_to = "u"
  )

colors_clust <- c("#E07B54", "#4A90B8", "#8E6BBF", "#5AAE61")[seq_len(c_final)]

print(
  ggplot(u_df, aes(x = u, fill = cluster_k)) +
    geom_histogram(bins = 20, alpha = 0.75, color = "white") +
    facet_grid(cluster_k ~ cluster_hard) +
    scale_fill_manual(values = colors_clust) +
    labs(
      title    = "Distribució de probabilitats de pertinença u_ik (train)",
      subtitle = "Files: cluster k analitzat · Columnes: cluster hard assignat",
      x = "u_ik (probabilitat de pertinença)", y = "Comptatge",
      fill = "Cluster k"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title      = element_text(face = "bold"),
      legend.position = "none",
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12)
    )
)

# PCA 2D colorejat per cluster hard (transparència = nitidesa)
pca_res <- prcomp(X_train, center = FALSE, scale. = FALSE)
pca_df  <- data.frame(
  PC1     = pca_res$x[, 1],
  PC2     = pca_res$x[, 2],
  cluster = factor(paste0("Cluster ", hard_train)),
  max_u   = max_u_train
)
var_exp <- round(summary(pca_res)$importance[2, 1:2] * 100, 1)

# color = a quin cluster s'assigna durament cada individu
# transparència = nitidesa de l'assignació
print(
  ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster, alpha = max_u)) +
    geom_point(size = 2.5) +
    scale_color_manual(values = colors_clust) +
    scale_alpha_continuous(range = c(0.25, 1.0), guide = "none") +
    labs(
      title    = "PCA 2D — assignació FCM (train)",
      subtitle = sprintf(
        "PC1 %.1f%% | PC2 %.1f%% variança · Transparència proporcional a max(u_ik)",
        var_exp[1], var_exp[2]
      ),
      x = paste0("PC1 (", var_exp[1], "%)"),
      y = paste0("PC2 (", var_exp[2], "%)"),
      color = "Cluster"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title      = element_text(face = "bold"),
      legend.position = "right",
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      legend.text = element_text(size = 12)
    )
)

# ========================================================= #
#                           GUARDAR MODEL
# ========================================================= #


save(
  fcm_final, scale_params, c_final, m_final,
  U_train, hard_train, U_test, hard_test,
  X_train, X_test,
  y_valid_train, y_valid_test,
  p_assist_train, p_assist_test,
  vars_clust, resultats_c, resultats_m,
  centers_orig,
  file = "2. Dades/fuzzy_clustering_model.RData"
)




dev.off()
sink()
