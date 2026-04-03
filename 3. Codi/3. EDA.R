packages <- c("dplyr", "ggplot2", "tidyr", "corrplot", "reshape2")

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

lapply(packages, install_if_missing)
rm(packages)

setwd("C:/Users/edurn/Downloads/TFG")
load("2. Dades/2. Dades tractades.RData")

motius_vars <- readRDS("2. Dades/motius_vars.rds")
estrategies_vars <- readRDS("2. Dades/estrategies_vars.rds")
ia_vars <- readRDS("2. Dades/ia_vars.rds")

#### ============================================================ ####
####            0. CREACIÓ DE VARIABLES BINÀRIES                  ####
#### ============================================================ ####

dades <- dades %>%
  mutate(
    TREB_INTENS = as.integer(DEDIC %in% c("T.Parcial", "T.Complet")),
    NOTA_ALTA = as.integer(NOTA %in% c("[8-8.9]", "≥9")),
    CURS_1R = as.integer(CURS == "1r"),
    SOBRECARREGAT = as.integer(N_ASSIG >= 12),
    DOBLE_GRAU_EST = as.integer(GRAU %in% c("Estadística", "Doble Eco+Est",
                                             "Doble ADE+Soc", "Doble ADE+Mat",
                                             "Doble ADE+Dret", "Doble ADE+Qui"))
  )


#### ============================================================ ####
####                    FUNCIONS AUXILIARS                       ####
#### ============================================================ ####

# Proporció d' empatats per una var (per saber si hem de fer kendall)
prop_ties <- function(x) {
  x <- as.numeric(x[!is.na(x)])
  n <- length(x)
  if (n < 2) return(0)
  freqs <- table(x)
  sum(choose(freqs, 2)) / choose(n, 2)
}

# Chi-quadrat + V de Cramer (avisa si hi ha cel·les amb esperada < 5)
cramer_test <- function(x, y) {
  taula <- table(x, y)
  test <- suppressWarnings(chisq.test(taula, correct = FALSE))
  n <- sum(taula)
  k <- min(nrow(taula), ncol(taula))
  v <- sqrt(as.numeric(test$statistic) / (n * (k - 1)))
  n_petita <- sum(test$expected < 5)
  avis <- if (n_petita > 0)
                paste0("AVÍS: ", n_petita, " cel·les amb esperada < 5")
              else ""
  list(chi2 = as.numeric(test$statistic),
       p = test$p.value,
       V = v,
       avis = avis)
}

# Rank-biserial (Kerby 2014): positiu = g[2] > g[1]
# g[1] = "Irregular (<80%)", g[2] = "Regular (≥80%)"
rank_biserial <- function(x, grup) {
  g <- levels(factor(grup))
  stopifnot(length(g) == 2)
  x1 <- as.numeric(x[as.character(grup) == g[1]])
  x2 <- as.numeric(x[as.character(grup) == g[2]])
  x1 <- x1[!is.na(x1)]
  x2 <- x2[!is.na(x2)]
  n1 <- length(x1); n2 <- length(x2)
  U <- wilcox.test(x1, x2, exact = FALSE)$statistic
  1 - (2 * U) / (n1 * n2)
}

# Llindar d'empats per decidir si fem kendall
LLINDAR_EMPATS <- 0.10

sig_label <- function(p) {
  case_when(p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", TRUE ~ "ns")
}

col_grups <- c("Irregular (<80%)" = "#E07B54", "Regular (≥80%)" = "#4A90B8")


sink("4. Outputs/3.1 Output_text_EDA.txt")
pdf("4. Outputs/3.2 Output_grafics_EDA.pdf", width = 10, height = 8)


#### ============================================================ ####
####         1. ASSOCIACIÓ ENTRE VARIABLES CATEGÒRIQUES           ####
#### ============================================================ ####

vars_cat <- c("GRAU", "CURS", "NOTA", "T_AVAL", "GENERE", "DEDIC","TREB_INTENS",
            "NOTA_ALTA", "CURS_1R", "SOBRECARREGAT", "DOBLE_GRAU_EST")
n_cat <- length(vars_cat)

cat("==============================================\n")
cat("  1. ASSOCIACIÓ ENTRE VARIABLES CATEGÒRIQUES  \n")
cat("==============================================\n\n")

mat_v_cat <- matrix(1, n_cat, n_cat, dimnames = list(vars_cat, vars_cat))
df_cat <- data.frame()

for (i in 1:(n_cat - 1)) {
  for (j in (i + 1):n_cat) {
    res <- cramer_test(dades[[vars_cat[i]]], dades[[vars_cat[j]]])
    mat_v_cat[i, j] <- mat_v_cat[j, i] <- round(res$V, 3)
    df_cat <- rbind(df_cat, data.frame(
      var1 = vars_cat[i],
      var2 = vars_cat[j],
      chi2 = round(res$chi2, 2),
      p_valor = round(res$p, 4),
      V = round(res$V, 3),
      sig = sig_label(res$p),
      avis = res$avis,
      stringsAsFactors = FALSE
    ))
  }
}

df_cat <- df_cat %>% arrange(desc(V))

cat("Totes les parelles ordenades per V de Cramér:\n")
print(df_cat %>% select(-avis))

avis_cat <- df_cat %>% filter(avis != "")
if (nrow(avis_cat) > 0) {
  cat("\n⚠ Parelles amb cel·les de freqüència esperada < 5:\n")
  print(avis_cat %>% select(var1, var2, avis))
}

# Heatmap Cramér's V
df_v_long <- reshape2::melt(mat_v_cat)

ggplot(df_v_long, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", value)), size = 3.2) +
  scale_fill_gradient(low = "#f5f0eb", high = "#4A90B8",
                      limits = c(0, 1), name = "V") +
  labs(title = "Cramér's V entre variables categòriques",
       x = "", y = "") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#### ============================================================ ####
####          2. VARIABLES CATEGÒRIQUES vs GRUP_ASSIST            ####
#### ============================================================ ####

cat("\n=================================================================\n")
cat("             2. VARIABLES CATEGÒRIQUES vs GRUP_ASSIST             \n")
cat("=================================================================\n\n")

df_vs_grup <- lapply(vars_cat, function(v) {
  res <- cramer_test(dades[[v]], dades$GRUP_ASSIST)
  data.frame(
    variable = v,
    chi2 = round(res$chi2, 2),
    p_valor = round(res$p, 4),
    V = round(res$V, 3),
    sig = sig_label(res$p),
    avis = res$avis,
    stringsAsFactors = FALSE
  )
}) %>%
  bind_rows() %>%
  arrange(desc(V))

cat("Resultat de cada variable vs GRUP_ASSIST:\n")
print(df_vs_grup %>% select(-avis))

avis_grup <- df_vs_grup %>% filter(avis != "")
if (nrow(avis_grup) > 0) {
  cat("\n⚠ Variables amb cel·les de freqüència esperada < 5:\n")
  print(avis_grup %>% select(variable, avis))
}

# Lollipop
ggplot(df_vs_grup, aes(x = reorder(variable, V), y = V,
                       color = sig != "ns")) +
  geom_segment(aes(xend = variable, yend = 0), linewidth = 0.8) +
  geom_point(size = 4) +
  geom_text(aes(label = sprintf("%.3f%s", V,
                                ifelse(sig != "ns", paste0(" ", sig), ""))),
            hjust = -0.2, size = 3.2) +
  scale_color_manual(values = c("TRUE" = "#4A90B8", "FALSE" = "gray60"),
                     guide = "none") +
  coord_flip() +
  expand_limits(y = max(df_vs_grup$V) + 0.1) +
  labs(title    = "Cramér's V vs GRUP_ASSIST per a cada variable",
       subtitle = "Opac = significatiu (p < 0.05)",
       x = "", y = "V de Cramér") +
  theme_minimal(base_size = 12)


#### ============================================================ ####
####        3. CORRELACIÓ SPEARMAN ENTRE VARIABLES LIKERT         ####
#### ============================================================ ####

# Kendall com a robustesa si empats > 10%

totes_likert <- c(motius_vars, estrategies_vars, ia_vars)
n_lk <- length(totes_likert)
df_likert_num <- dades %>%
  select(all_of(totes_likert)) %>%
  mutate(across(everything(), as.numeric))

cat("\n=================================================================\n")
cat(" 3. CORRELACIÓ SPEARMAN ENTRE VARIABLES LIKERT\n")
cat("=================================================================\n\n")

mat_spear <- matrix(1, n_lk, n_lk, dimnames = list(totes_likert, totes_likert))
mat_spear_p <- matrix(NA, n_lk, n_lk, dimnames = list(totes_likert, totes_likert))
df_likert_corr <- data.frame()

for (i in 1:(n_lk - 1)) {
  for (j in (i + 1):n_lk) {
    xi <- df_likert_num[[totes_likert[i]]]
    xj <- df_likert_num[[totes_likert[j]]]

    sp <- cor.test(xi, xj, method = "spearman", exact = FALSE)
    mat_spear[i, j] <- mat_spear[j, i] <- round(sp$estimate, 3)
    mat_spear_p[i, j] <- mat_spear_p[j, i] <- round(sp$p.value, 4)

    molts_empats <- prop_ties(xi) > LLINDAR_EMPATS | prop_ties(xj) > LLINDAR_EMPATS
    rho_k <- NA_real_; p_k <- NA_real_
    if (molts_empats) {
      kd <- cor.test(xi, xj, method = "kendall", exact = FALSE)
      rho_k <- round(kd$estimate, 3)
      p_k   <- round(kd$p.value, 4)
    }

    if (abs(sp$estimate) > 0.3 && sp$p.value < 0.05) {
      df_likert_corr <- rbind(df_likert_corr, data.frame(
        var1         = totes_likert[i],
        var2         = totes_likert[j],
        rho_spearman = round(sp$estimate, 3),
        p_spearman   = round(sp$p.value, 4),
        rho_kendall  = rho_k,
        p_kendall    = p_k,
        empats_elev  = molts_empats,
        stringsAsFactors = FALSE
      ))
    }
  }
}

df_likert_corr <- df_likert_corr %>%
  mutate(sig = sig_label(p_spearman)) %>%
  arrange(desc(abs(rho_spearman)))

cat("Parelles Likert significatives amb |rho| > 0.3 (ordenades per |rho|):\n")
cat("  rho_kendall s'informa quan la proporció d'empats > 10% en alguna variable\n\n")
print(df_likert_corr)

# Corrplot Spearman (|rho| > 0.2 visible)
mat_plot <- mat_spear
mat_plot[abs(mat_plot) < 0.3 & mat_plot != 1] <- 0

corrplot(mat_plot,
         method      = "color",
         type        = "lower",
         tl.cex      = 0.55,
         tl.col      = "black",
         col         = colorRampPalette(c("#E07B54", "white", "#4A90B8"))(200),
         addCoef.col = "black",
         number.cex  = 0.35,
         title       = "Spearman entre variables Likert (|rho| > 0.3 visible)",
         mar         = c(0, 0, 2, 0))


#### ============================================================ ####
####              4. VARIABLES LIKERT vs GRUP_ASSIST              ####
#### ============================================================ ####

cat("\n=================================================================\n")
cat(" 4. VARIABLES LIKERT vs GRUP_ASSIST\n")
cat("=================================================================\n\n")

# GRUP_ASSIST com a 0/1 per Spearman de direcció
grup_num <- as.integer(dades$GRUP_ASSIST == "Regular (≥80%)")

df_likert_vs_grup <- lapply(totes_likert, function(v) {
  x <- as.numeric(dades[[v]])

  mw <- wilcox.test(x ~ dades$GRUP_ASSIST, exact = FALSE)
  rb <- rank_biserial(x, dades$GRUP_ASSIST)
  sp <- cor.test(x, grup_num, method = "spearman", exact = FALSE)

  molts_empats <- prop_ties(x) > LLINDAR_EMPATS
  rho_k <- NA_real_
  if (molts_empats) {
    kd <- cor.test(x, grup_num, method = "kendall", exact = FALSE)
    rho_k <- round(kd$estimate, 3)
  }

  data.frame(
    variable     = v,
    bloc         = case_when(
      v %in% motius_vars      ~ "Motius",
      v %in% estrategies_vars ~ "Estratègies",
      v %in% ia_vars          ~ "IA"
    ),
    W            = round(mw$statistic, 1),
    p_mw         = round(mw$p.value, 4),
    sig_mw       = sig_label(mw$p.value),
    rho_spearman = round(sp$estimate, 3),
    p_spearman   = round(sp$p.value, 4),
    r_biserial   = round(rb, 3),
    rho_kendall  = rho_k,
    empats_elev  = molts_empats,
    stringsAsFactors = FALSE
  )
}) %>%
  bind_rows() %>%
  arrange(desc(abs(r_biserial)))

cat("Mann-Whitney + Spearman + Rank-biserial per a cada variable Likert:\n")
cat("  r_biserial > 0 → Regular > Irregular | rho_spearman > 0 → correlació creixent\n")
cat("  rho_kendall s'informa quan empats > 10%\n\n")
print(df_likert_vs_grup)

# Gràfic 1: rank-biserial per variable Likert
ggplot(df_likert_vs_grup,
       aes(x = reorder(variable, r_biserial),
           y = r_biserial,
           fill = bloc,
           alpha = sig_mw != "ns")) +
  geom_col() +
  geom_hline(yintercept = 0, linewidth = 0.5, color = "gray30") +
  scale_fill_manual(values = c("Motius"      = "#E07B54",
                               "Estratègies" = "#4A90B8",
                               "IA"          = "#8E6BBF")) +
  scale_alpha_manual(values = c("TRUE" = 0.9, "FALSE" = 0.3), guide = "none") +
  coord_flip() +
  labs(title    = "Rank-biserial per variable Likert vs GRUP_ASSIST",
       subtitle = "Positiu = Regular > Irregular | Opac = significatiu (p < 0.05, MW)",
       x = "", y = "Rank-biserial r", fill = "Bloc") +
  theme_minimal(base_size = 12)

# Gràfic 2: Spearman rho (direcció) per variable Likert
ggplot(df_likert_vs_grup,
       aes(x = reorder(variable, rho_spearman),
           y = rho_spearman,
           fill = bloc,
           alpha = sig_mw != "ns")) +
  geom_col() +
  geom_hline(yintercept = 0, linewidth = 0.5, color = "gray30") +
  scale_fill_manual(values = c("Motius"      = "#E07B54",
                               "Estratègies" = "#4A90B8",
                               "IA"          = "#8E6BBF")) +
  scale_alpha_manual(values = c("TRUE" = 0.9, "FALSE" = 0.3), guide = "none") +
  coord_flip() +
  labs(title    = "Spearman ρ per variable Likert vs GRUP_ASSIST",
       subtitle = "Positiu = correlació creixent amb assistència regular",
       x = "", y = "Spearman ρ", fill = "Bloc") +
  theme_minimal(base_size = 12)


#### ============================================================ ####
####  5. CORRELACIÓ SPEARMAN ENTRE VARIABLES NUMÈRIQUES          ####
#### ============================================================ ####

vars_num <- c("EDAT", "DESPL", "N_ASSIG", "P_ASSIST")
n_num <- length(vars_num)

cat("\n=================================================================\n")
cat(" 5. CORRELACIÓ SPEARMAN ENTRE VARIABLES NUMÈRIQUES\n")
cat("=================================================================\n\n")

mat_num_sp <- matrix(1, n_num, n_num, dimnames = list(vars_num, vars_num))
df_num_corr <- data.frame()

for (i in 1:(n_num - 1)) {
  for (j in (i + 1):n_num) {
    xi <- dades[[vars_num[i]]]
    xj <- dades[[vars_num[j]]]

    sp <- cor.test(xi, xj, method = "spearman", use = "complete.obs", exact = FALSE)
    mat_num_sp[i, j] <- mat_num_sp[j, i] <- round(sp$estimate, 3)

    molts_empats <- prop_ties(xi) > LLINDAR_EMPATS | prop_ties(xj) > LLINDAR_EMPATS
    rho_k <- NA_real_; p_k <- NA_real_
    if (molts_empats) {
      kd <- cor.test(xi, xj, method = "kendall", use = "complete.obs", exact = FALSE)
      rho_k <- round(kd$estimate, 3)
      p_k   <- round(kd$p.value, 4)
    }

    df_num_corr <- rbind(df_num_corr, data.frame(
      var1         = vars_num[i],
      var2         = vars_num[j],
      rho_spearman = round(sp$estimate, 3),
      p_spearman   = round(sp$p.value, 4),
      sig          = sig_label(sp$p.value),
      rho_kendall  = rho_k,
      p_kendall    = p_k,
      empats_elev  = molts_empats,
      stringsAsFactors = FALSE
    ))
  }
}

df_num_corr <- df_num_corr %>% arrange(desc(abs(rho_spearman)))

cat("Correlació Spearman entre variables numèriques:\n")
cat("  rho_kendall s'informa quan la proporció d'empats > 10% en alguna variable\n\n")
print(df_num_corr)

# Corrplot numèriques
corrplot(mat_num_sp,
         method      = "color",
         type        = "lower",
         tl.cex      = 0.9,
         tl.col      = "black",
         col         = colorRampPalette(c("#E07B54", "white", "#4A90B8"))(200),
         addCoef.col = "black",
         number.cex  = 0.8,
         title       = "Spearman entre variables numèriques",
         mar         = c(0, 0, 2, 0))


#### ============================================================ ####
####  6. VARIABLES NUMÈRIQUES vs GRUP_ASSIST                     ####
####     Mann-Whitney U + Rank-biserial                          ####
####     (P_ASSIST exclosa: és la font de GRUP_ASSIST)           ####
#### ============================================================ ####

vars_num_vs_grup <- c("EDAT", "DESPL", "N_ASSIG")

cat("\n=================================================================\n")
cat(" 6. VARIABLES NUMÈRIQUES vs GRUP_ASSIST\n")
cat("=================================================================\n\n")
cat("  (P_ASSIST s'exclou perquè GRUP_ASSIST deriva directament d'ella)\n\n")

df_num_vs_grup <- lapply(vars_num_vs_grup, function(v) {
  x  <- dades[[v]]
  mw <- wilcox.test(x ~ dades$GRUP_ASSIST, exact = FALSE)
  rb <- rank_biserial(x, dades$GRUP_ASSIST)
  data.frame(
    variable   = v,
    W          = round(mw$statistic, 1),
    p_valor    = round(mw$p.value, 4),
    sig        = sig_label(mw$p.value),
    r_biserial = round(rb, 3),
    stringsAsFactors = FALSE
  )
}) %>%
  bind_rows() %>%
  arrange(desc(abs(r_biserial)))

cat("Mann-Whitney + Rank-biserial per a cada variable numèrica:\n")
cat("  r_biserial > 0 → Regular > Irregular\n\n")
print(df_num_vs_grup)

# Lollipop rank-biserial numèriques
ggplot(df_num_vs_grup,
       aes(x = reorder(variable, r_biserial),
           y = r_biserial,
           color = sig != "ns")) +
  geom_segment(aes(xend = variable, yend = 0), linewidth = 1.2) +
  geom_point(size = 5) +
  geom_hline(yintercept = 0, color = "gray30") +
  geom_text(aes(label = sprintf("r = %.3f\np = %.4f", r_biserial, p_valor)),
            hjust = -0.15, size = 3.2) +
  scale_color_manual(values = c("TRUE" = "#4A90B8", "FALSE" = "gray60"),
                     labels = c("ns", "p < 0.05"), name = "") +
  coord_flip() +
  expand_limits(y = c(min(df_num_vs_grup$r_biserial) - 0.15,
                      max(df_num_vs_grup$r_biserial) + 0.15)) +
  labs(title    = "Rank-biserial per variable numèrica vs GRUP_ASSIST",
       subtitle = "Positiu = Regular > Irregular",
       x = "", y = "Rank-biserial r") +
  theme_minimal(base_size = 13)


sink()
dev.off()

save(dades, file = "2. Dades/3. Dades EDA.RData")
