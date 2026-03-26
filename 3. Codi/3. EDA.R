library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(corrplot)
library(ggcorrplot)
library(psych)
library(FactoMineR)
library(factoextra)
library(reshape2)
select <- dplyr::select

setwd("C:/Users/edurn/OneDrive/Escritorio/Universitat/TFG---Github/2. Dades")
dades <- read_excel("1. Preprocessing.xlsx")


motius_vars <- c("M_TREB","M_FAM","M_SALUT","M_DIST","M_AUTON","M_CV",
                 "M_EXAM","M_UTIL","M_AVORR","M_PASSIU","M_TEOR","M_PROF",
                 "M_REPET","M_ACAD","M_AMICS")

estrategies_vars <- c("E_PES_AC","E_PART","E_DINAM","E_REDU","E_CURT","E_DESC",
                      "E_CLIMA","E_EXPL","E_RITME","E_ACT_AC","E_PROP","E_HORA","E_PES_AS")

ia_vars <- c("IA_HABIT", "IA_COMPR", "IA_SUBST", "IA_CONF",
             "IA_ATENC", "IA_PREOC", "IA_REND", "IA_PDFS")

#### ============================================================ ####
####           1. CORRELACIONS VARIABLES NUMÈRIQUES               ####
#### ============================================================ ####

# --- 1.1 Pearson entre numèriques ---
vars_num <- dades %>%
  select(EDAT, DESPL, N_ASSIG, P_ASSIST) %>%
  mutate(across(everything(), as.numeric))

cor_pearson <- cor(vars_num, method = "pearson", use = "complete.obs")

ggcorrplot(cor_pearson,
           type      = "lower",
           lab       = TRUE,
           lab_size  = 4,
           colors    = c("#E07B54", "white", "#4A90B8"),
           title     = "Correlació de Pearson",
           ggtheme   = theme_minimal(base_size = 13))

# Spearman incloent Likert com a numèric
vars_spearman <- dades %>%
  select(P_ASSIST, EDAT, DESPL, N_ASSIG,
         all_of(motius_vars),
         all_of(estrategies_vars),
         all_of(ia_vars)) %>%
  mutate(across(everything(), ~ as.numeric(.x)))

cor_spearman <- cor(vars_spearman, method = "spearman", use = "complete.obs")

cor_df <- melt(cor_spearman)
cor_df <- cor_df[abs(cor_df$value) > 0.7 & cor_df$Var1 != cor_df$Var2, ] 
# només les que tenen més d'un 70%, eliminant les variables amb si mateixes

ggplot(cor_df, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", value)), size = 3) +
  scale_fill_gradient2(limits = c(-1, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlació de Pearson")

# Submatriu: només correlacions amb P_ASSIST
cor_passist <- sort(cor_spearman["P_ASSIST", -1], decreasing = TRUE)
cor_passist <- cor_passist[abs(cor_passist) > 0.2]

df_cor_passist <- data.frame(
  variable    = names(cor_passist),
  correlacio  = as.numeric(cor_passist),
  bloc        = case_when(
    names(cor_passist) %in% motius_vars      ~ "Motius",
    names(cor_passist) %in% estrategies_vars ~ "Estratègies",
    names(cor_passist) %in% ia_vars          ~ "IA",
    TRUE                                     ~ "Numèrica"
  )
)

ggplot(df_cor_passist,
       aes(x = reorder(variable, correlacio),
           y = correlacio,
           fill = bloc)) +
  geom_col(alpha = 0.85) +
  geom_hline(yintercept = 0, color = "gray30") +
  geom_hline(yintercept = c(-0.2, 0.2),
             linetype = "dashed", color = "gray60") +
  coord_flip() +
  scale_fill_manual(values = c("Motius"      = "#E07B54",
                               "Estratègies" = "#4A90B8",
                               "IA"          = "#8E6BBF",
                               "Numèrica"    = "#4CAF82")) +
  labs(title    = "Correlació de Spearman amb P_ASSIST",
       subtitle = "Línies discontinues = |r| > 0.2",
       x = "", y = "Spearman r", fill = "Bloc") +
  theme_minimal(base_size = 12)

# Heatmap complet Spearman (blocs Likert)
corrplot(cor_spearman,
         method  = "color",
         type    = "lower",
         tl.cex  = 0.6,
         tl.col  = "black",
         col     = colorRampPalette(c("#E07B54", "white", "#4A90B8"))(200),
         title   = "Correlació Spearman – Totes les variables",
         mar     = c(0, 0, 2, 0))


#### ============================================================ ####
####     BLOC 2: CORRELACIONS ENTRE BLOCS LIKERT                 ####
#### ============================================================ ####

# Mitjanes de cada bloc per individu
dades_blocs <- dades %>%
  mutate(
    MOTIUS_MEAN = rowMeans(select(., all_of(motius_vars)) %>%
                             mutate(across(everything(), as.numeric)), na.rm = TRUE),
    ESTRAT_MEAN = rowMeans(select(., all_of(estrategies_vars)) %>%
                             mutate(across(everything(), as.numeric)), na.rm = TRUE),
    IA_MEAN     = rowMeans(select(., all_of(ia_vars)) %>%
                             mutate(across(everything(), as.numeric)), na.rm = TRUE)
  )

# Correlació entre blocs i P_ASSIST
cor_blocs <- dades_blocs %>%
  select(P_ASSIST, MOTIUS_MEAN, ESTRAT_MEAN, IA_MEAN) %>%
  mutate(across(everything(), as.numeric)) %>%
  cor(method = "spearman", use = "complete.obs")

cat("\nCorrelació Spearman entre blocs i P_ASSIST:\n")
print(round(cor_blocs, 3))

ggcorrplot(cor_blocs,
           method   = "circle",
           type     = "lower",
           lab      = TRUE,
           lab_size = 5,
           colors   = c("#E07B54", "white", "#4A90B8"),
           title    = "Correlació entre blocs Likert i % Assistència",
           ggtheme  = theme_minimal(base_size = 13))

# Alpha de Cronbach per cada bloc (fiabilitat interna)
cat("\n--- Alpha de Cronbach ---\n")
alpha_motius <- psych::alpha(dades %>%
                               select(all_of(motius_vars)) %>%
                               mutate(across(everything(), as.numeric)))
cat("Motius:      ", round(alpha_motius$total$raw_alpha, 3), "\n")

alpha_estrat <- psych::alpha(dades %>%
                               select(all_of(estrategies_vars)) %>%
                               mutate(across(everything(), as.numeric)))
cat("Estratègies: ", round(alpha_estrat$total$raw_alpha, 3), "\n")

alpha_ia <- psych::alpha(dades %>%
                           select(all_of(ia_vars)) %>%
                           mutate(across(everything(), as.numeric)))
cat("IA:          ", round(alpha_ia$total$raw_alpha, 3), "\n")


#### ============================================================ ####
####     BLOC 3: TESTS ESTADÍSTICS                               ####
#### ============================================================ ####

# --- 3.1 Mann-Whitney: P_ASSIST vs variables categòriques ---
cat("\n=== Mann-Whitney / Wilcoxon ===\n")

vars_cat_bin <- c("GENERE", "T_AVAL")
for (v in vars_cat_bin) {
  grups <- split(dades$P_ASSIST, dades[[v]])
  if (length(grups) == 2) {
    test <- wilcox.test(grups[[1]], grups[[2]])
    cat(v, "→ W =", round(test$statistic, 1),
        "| p-valor =", round(test$p.value, 4), "\n")
  }
}

# --- 3.2 Kruskal-Wallis: P_ASSIST vs variables amb >2 categories ---
cat("\n=== Kruskal-Wallis ===\n")

vars_cat_multi <- c("GRAU", "CURS", "NOTA", "DEDIC")
for (v in vars_cat_multi) {
  test <- kruskal.test(P_ASSIST ~ dades[[v]], data = dades)
  cat(v, "→ H =", round(test$statistic, 2),
      "| df =", test$parameter,
      "| p-valor =", round(test$p.value, 4), "\n")
}

# --- 3.3 Chi-quadrat: GRUP_ASSIST vs categòriques ---
cat("\n=== Chi-quadrat (GRUP_ASSIST vs categòriques) ===\n")

vars_chi <- c("GRAU", "CURS", "NOTA", "T_AVAL", "GENERE", "DEDIC")
resultats_chi <- lapply(vars_chi, function(v) {
  taula <- table(dades$GRUP_ASSIST, dades[[v]])
  test  <- chisq.test(taula, simulate.p.value = TRUE, B = 2000)
  data.frame(
    variable  = v,
    chi2      = round(test$statistic, 3),
    p_valor   = round(test$p.value, 4),
    sig       = ifelse(test$p.value < 0.001, "***",
                       ifelse(test$p.value < 0.01,  "**",
                              ifelse(test$p.value < 0.05,  "*", "ns")))
  )
})

df_chi <- do.call(rbind, resultats_chi)
print(df_chi)

# --- 3.4 Mann-Whitney per cada Likert vs GRUP_ASSIST ---
cat("\n=== Mann-Whitney: cada Likert vs GRUP_ASSIST ===\n")

totes_likert <- c(motius_vars, estrategies_vars, ia_vars)

resultats_mw <- lapply(totes_likert, function(v) {
  x <- as.numeric(dades[[v]])
  g <- dades$GRUP_ASSIST
  test <- wilcox.test(x ~ g)
  r <- abs(qnorm(test$p.value / 2)) / sqrt(nrow(dades))  # effect size r
  data.frame(
    variable = v,
    bloc     = case_when(
      v %in% motius_vars      ~ "Motius",
      v %in% estrategies_vars ~ "Estratègies",
      v %in% ia_vars          ~ "IA"
    ),
    W        = round(test$statistic, 1),
    p_valor  = round(test$p.value, 4),
    effect_r = round(r, 3),
    sig      = ifelse(test$p.value < 0.001, "***",
                      ifelse(test$p.value < 0.01,  "**",
                             ifelse(test$p.value < 0.05,  "*", "ns")))
  )
})

df_mw <- do.call(rbind, resultats_mw) %>%
  arrange(p_valor)

print(df_mw)

# Gràfic effect size Likert
ggplot(df_mw, aes(x = reorder(variable, effect_r),
                  y = effect_r,
                  fill = bloc,
                  alpha = sig != "ns")) +
  geom_col() +
  geom_hline(yintercept = c(0.1, 0.3),
             linetype = "dashed", color = "gray50") +
  annotate("text", x = 2, y = 0.11, label = "petit",  size = 3, color = "gray50") +
  annotate("text", x = 2, y = 0.31, label = "mitjà",  size = 3, color = "gray50") +
  scale_fill_manual(values = c("Motius"      = "#E07B54",
                               "Estratègies" = "#4A90B8",
                               "IA"          = "#8E6BBF")) +
  scale_alpha_manual(values = c("TRUE" = 0.9, "FALSE" = 0.35),
                     guide  = "none") +
  coord_flip() +
  labs(title    = "Effect size (r) Mann-Whitney per variable Likert",
       subtitle = "Opac = significatiu | Transparent = no significatiu",
       x = "", y = "Effect size r", fill = "Bloc") +
  theme_minimal(base_size = 12)


#### ============================================================ ####
####     BLOC 4: TAULES DE CONTINGÈNCIA                          ####
#### ============================================================ ####

# --- 4.1 CURS x NOTA ---
cat("\n=== CURS x NOTA ===\n")
taula_curs_nota <- table(dades$CURS, dades$NOTA)
print(taula_curs_nota)
print(round(prop.table(taula_curs_nota, margin = 1) * 100, 1))

# --- 4.2 DEDIC x GRUP_ASSIST ---
cat("\n=== DEDIC x GRUP_ASSIST ===\n")
taula_dedic <- table(dades$DEDIC, dades$GRUP_ASSIST)
print(taula_dedic)
print(round(prop.table(taula_dedic, margin = 1) * 100, 1))

# --- 4.3 GRAU x GRUP_ASSIST ---
cat("\n=== GRAU x GRUP_ASSIST ===\n")
taula_grau <- table(dades$GRAU, dades$GRUP_ASSIST)
print(taula_grau)
print(round(prop.table(taula_grau, margin = 1) * 100, 1))

# --- 4.4 T_AVAL x NOTA ---
cat("\n=== T_AVAL x NOTA ===\n")
taula_taval_nota <- table(dades$T_AVAL, dades$NOTA)
print(taula_taval_nota)
print(round(prop.table(taula_taval_nota, margin = 1) * 100, 1))

# Visualització ballon plot DEDIC x GRUP_ASSIST
as.data.frame(taula_dedic) %>%
  rename(DEDIC = Var1, GRUP = Var2, n = Freq) %>%
  group_by(DEDIC) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = GRUP, y = DEDIC, size = n, color = prop)) +
  geom_point(alpha = 0.85) +
  geom_text(aes(label = paste0(round(prop * 100), "%")),
            color = "gray20", size = 3.2, vjust = -1.2) +
  scale_size_continuous(range = c(4, 18)) +
  scale_color_gradient(low = "#E07B54", high = "#4A90B8",
                       name = "Proporció") +
  labs(title = "Dedicació laboral × Grup d'assistència",
       x = "", y = "", size = "n") +
  theme_minimal(base_size = 13)


#### ============================================================ ####
####     BLOC 5: PCA / MCA LIKERT                                ####
#### ============================================================ ####

# --- 5.1 PCA sobre motius (numèric) ---
pca_motius <- prcomp(
  dades %>%
    select(all_of(motius_vars)) %>%
    mutate(across(everything(), as.numeric)) %>%
    na.omit(),
  scale. = TRUE
)

# Scree plot motius
fviz_eig(pca_motius,
         addlabels = TRUE,
         barfill   = "#E07B54",
         barcolor  = "white",
         linecolor = "gray40") +
  labs(title = "PCA Motius – Variància explicada per component") +
  theme_minimal(base_size = 13)

# Biplot motius
fviz_pca_biplot(pca_motius,
                repel       = TRUE,
                col.var     = "#E07B54",
                col.ind     = "#4A90B8",
                alpha.ind   = 0.4,
                label       = "var") +
  labs(title = "PCA Motius de NO assistència") +
  theme_minimal(base_size = 12)

# Variables més importants en PC1 i PC2
cat("\nLoadings PC1 i PC2 – Motius:\n")
print(round(pca_motius$rotation[, 1:2], 3))

# --- 5.2 PCA sobre estratègies ---
pca_estrat <- prcomp(
  dades %>%
    select(all_of(estrategies_vars)) %>%
    mutate(across(everything(), as.numeric)) %>%
    na.omit(),
  scale. = TRUE
)

fviz_eig(pca_estrat,
         addlabels = TRUE,
         barfill   = "#4A90B8",
         barcolor  = "white",
         linecolor = "gray40") +
  labs(title = "PCA Estratègies – Variància explicada") +
  theme_minimal(base_size = 13)

fviz_pca_var(pca_estrat,
             repel    = TRUE,
             col.var  = "#4A90B8") +
  labs(title = "PCA Estratègies d'assistència") +
  theme_minimal(base_size = 12)

# --- 5.3 PCA sobre IA ---
pca_ia <- prcomp(
  dades %>%
    select(all_of(ia_vars)) %>%
    mutate(across(everything(), as.numeric)) %>%
    na.omit(),
  scale. = TRUE
)

fviz_eig(pca_ia,
         addlabels = TRUE,
         barfill   = "#8E6BBF",
         barcolor  = "white",
         linecolor = "gray40") +
  labs(title = "PCA IA – Variància explicada") +
  theme_minimal(base_size = 13)

fviz_pca_var(pca_ia,
             repel   = TRUE,
             col.var = "#8E6BBF") +
  labs(title = "PCA Ús de la IA") +
  theme_minimal(base_size = 12)

# --- 5.4 PCA individus colorejats per GRUP_ASSIST ---
idx_complets <- which(complete.cases(
  dades %>% select(all_of(motius_vars))
))

fviz_pca_ind(pca_motius,
             geom.ind    = "point",
             col.ind     = dades$GRUP_ASSIST[idx_complets],
             palette     = c("#E07B54", "#4A90B8"),
             addEllipses = TRUE,
             ellipse.type = "confidence",
             legend.title = "",
             repel        = TRUE,
             alpha.ind    = 0.6) +
  labs(title = "PCA Motius – Individus per grup d'assistència") +
  theme_minimal(base_size = 13)

# --- 5.5 MCA sobre variables categòriques ---
dades_mca <- dades %>%
  select(GRAU, CURS, NOTA, T_AVAL, GENERE, DEDIC, GRUP_ASSIST) %>%
  mutate(across(everything(), as.factor))

mca_result <- MCA(dades_mca,
                  quali.sup = which(names(dades_mca) == "GRUP_ASSIST"),
                  graph     = FALSE)

# Mapa de categories
fviz_mca_var(mca_result,
             repel    = TRUE,
             col.var  = "contrib",
             gradient.cols = c("#4A90B8", "white", "#E07B54")) +
  labs(title = "MCA – Mapa de categories") +
  theme_minimal(base_size = 12)

# Individus per GRUP_ASSIST
fviz_mca_ind(mca_result,
             geom.ind     = "point",
             col.ind      = dades$GRUP_ASSIST,
             palette      = c("#E07B54", "#4A90B8"),
             addEllipses  = TRUE,
             ellipse.type = "confidence",
             legend.title = "",
             alpha.ind    = 0.5) +
  labs(title = "MCA – Individus per grup d'assistència") +
  theme_minimal(base_size = 13)

# Variància explicada MCA
cat("\nVariància explicada MCA (primeres 5 dimensions):\n")
print(round(mca_result$eig[1:5, ], 2))