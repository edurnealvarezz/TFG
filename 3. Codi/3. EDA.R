packages <- c("dplyr","ggplot2","tidyr","corrplot","ggcorrplot","FactoMineR",
              "factoextra","psych","reshape2","colorspace","tibble","Hmisc")

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

lapply(packages, install_if_missing)
rm(packages)

setwd("C:/Users/edurn/OneDrive/Escritorio/Universitat/TFG---Github/2. Dades")
load("2. Dades amb binaria.RData")

motius_vars <- readRDS("motius_vars.rds")
estrategies_vars <- readRDS("estrategies_vars.rds")
ia_vars <- readRDS("ia_vars.rds")

sink("C:/Users/edurn/OneDrive/Escritorio/Universitat/TFG---Github/4. Outputs/3.1 Output_text_EDA.txt")
pdf("C:/Users/edurn/OneDrive/Escritorio/Universitat/TFG---Github/4. Outputs/3.2 Output_grafics_EDA.pdf",
    width = 10, height = 8)


#### ============================================================ ####
####           1. CORRELACIONS VARIABLES NUMÈRIQUES               ####
#### ============================================================ ####

# 1.1 Spearman

vars_spearman <- c("EDAT", "DESPL", "N_ASSIG")

df_num <- dades %>%
  select(P_ASSIST, EDAT, DESPL, N_ASSIG) %>%
  mutate(across(everything(), ~ as.numeric(.x)))

cor_spearman <- cor(df_num, method = "spearman", use = "complete.obs")

cor_df <- melt(cor_spearman)
cor_df <- cor_df[cor_df$Var1 != cor_df$Var2, ] 

ggplot(cor_df, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", value)), size = 3) +
  scale_fill_gradient2(limits = c(-1, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlació de Pearson")


test_spearman <- lapply(vars_spearman, function(v) {
  test <- cor.test(dades[[v]], dades$P_ASSIST, method = "spearman")
  # mirem si la correlació és significativa amb P_ASSIST
  data.frame(
    Variable = v,
    rho = test$estimate,
    p_value = test$p.value
  )
})

test_spearman <- do.call(rbind, test_spearman)
print(test_spearman)

# no hi ha correlació significativa (al 10% tenim EDAT)

#### ============================================================ ####
####           2. ASSOCIACIONS ENTRE BLOCS LIKERT                 ####
#### ============================================================ ####

# Heatmap Spearman (blocs Likert)
df_likert <- dades %>%
  select(GRUP_ASSIST, all_of(motius_vars), all_of(estrategies_vars), all_of(ia_vars))

df_ord <- df_likert %>%
  mutate(across(-GRUP_ASSIST, ~as.numeric(.x)))

# matriz de correlaciones de Spearman
vars <- colnames(df_ord[,-1])

res <- expand.grid(var1 = vars, var2 = vars)

res <- res %>%
  rowwise() %>%
  mutate(
    test = list(cor.test(df_ord[[var1]], df_ord[[var2]], method = "kendall")),
    tau = test$estimate,
    p_value = test$p.value
  ) %>%
  select(-test)


vars_alta_cor <- which(apply(cor_spearman, 1, function(x) {
  any(abs(x[x != 1]) > 0.6, na.rm = TRUE)
}))

cor_filtrat <- cor_spearman[vars_alta_cor, vars_alta_cor]

cat("\n ==== Variables amb correlació > 0.6 amb alguna altra:", 
    nrow(cor_filtrat), "==== \n")
print(rownames(cor_filtrat))

corrplot(cor_filtrat,
         method  = "color",
         type    = "lower",
         tl.cex  = 0.8,
         tl.col  = "black",
         col     = colorRampPalette(c("#E07B54", "white", "#4A90B8"))(200),
         addCoef.col = "black",
         number.cex  = 0.7,
         title   = "Correlació Spearman – Variables amb |r| > 0.5",
         mar     = c(0, 0, 2, 0))


#### ============================================================ ####
####           3. TESTS ESTADÍSTICS                               ####
#### ============================================================ ####

# Mann-Whitney: mirar si venen de la mateixa població
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

# Kruskal-Wallis (anova)
cat("\n=== Kruskal-Wallis ===\n")

vars_cat_multi <- c("GRAU", "CURS", "NOTA", "DEDIC")
for (v in vars_cat_multi) {
  test <- kruskal.test(P_ASSIST ~ dades[[v]], data = dades)
  cat(v, "→ H =", round(test$statistic, 2),
      "| df =", test$parameter,
      "| p-valor =", round(test$p.value, 4), "\n")
}

# Chi-quadrat: associació
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

# Mann-Whitney mateixa població
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

# Gràfic effect size Likert: mesura com de gran és l'efecte
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
####             4. MCA SOBRE VARIABLES LIKERT                    ####
#### ============================================================ ####

fer_mca_bloc <- function(vars, nom_bloc, nivells, color_bar) {
  titol <- paste("\n MCA del bloc", nom_bloc, "\n")
  cat("\n")
  cat(strrep("=", nchar(titol)))
  cat(titol)
  cat(strrep("=", nchar(titol)), "\n")
  
  dades_bloc <- dades %>%
    select(all_of(vars), GRUP_ASSIST) %>%
    mutate(across(all_of(vars),
                  ~ factor(as.integer(.x), levels = 1:nivells))) %>%
    na.omit()
  
  idx <- as.integer(rownames(dades_bloc))
  grup <- dades$GRUP_ASSIST[idx]
  
  mca <- MCA(dades_bloc,
             method="Indicator",
             quali.sup = which(names(dades_bloc) == "GRUP_ASSIST"), 
             # resposta com a suplementària per poder representar-la
             ncp       = 10,
             graph     = FALSE)
  
  cat("\n ==== Variància explicada (primeres 10 dimensions sense correcció)====\n")
  eig.val <- get_eigenvalue(mca)
  print(eig.val[1:10,])
  
  # 1/p where p=12 active variables
  act.vars <- length(vars)
  threshold <- 1/act.vars
  
  # CORRECCIÓ BENZECRI
  calcular_benzecri <- function(mca_objecte, Q) {
    evals <- mca_objecte$eig[, 1]
    threshold <- 1/Q
    ejes_validos <- evals[evals > threshold]
    
    corregits <- (Q/(Q-1))^2 * (ejes_validos - threshold)^2
    percentatges <- round((corregits / sum(corregits)) * 100,4)
    acumulada   <- cumsum(percentatges)
    
    data.frame(
      Dimensio = 1:length(percentatges), 
      Var_Real = round(percentatges, 4),
      Var_Acum = round(acumulada, 4)
    )
  }
  
  cat("\n ===== Variància corregida (Benzécri) per les primeres 10 dim =====\n")
  print(calcular_benzecri(mca, act.vars)[1:10,])

  # scree plot
  p_scree <- fviz_screeplot(mca,
                            addlabels = TRUE,
                            barfill   = color_bar,
                            barcolor  = "white") +
    geom_hline(yintercept = threshold, linetype = 2, color = "red") +
    labs(title = paste("Scree Plot amb llindar 1/p")) +
    theme_minimal(base_size = 13)
  print(p_scree)
  
  # BIPLOT INDIVIDUS
  biolot <- fviz_mca_biplot(mca,
                            repel = TRUE,
                            col.var = "black", # variables en color fix
                            habillage = dades$GRUP_ASSIST, # color per grup
                            select.var = list(contrib = 10), # 10 vars amb més contrib
                            select.ind = list(contrib = 50), # 50 indiv amb més contrib
                            labelsize = 3,
    title = "Biplot MCA (1-2)")
  
  print(biplot)
  
  # MAPA DE VARIABLES
  var <- get_mca_var(mca)
  ### Pseudo-correlacio
  pseudocorr <- fviz_mca_var(mca,
               choice = "mca.cor",
               repel = TRUE,
               ggtheme = theme_minimal())
  print(pseudocorr)
  
  ### només les 10 millors
  millors_vars <- fviz_mca_var(mca,
               repel = TRUE,
               ggtheme = theme_minimal(),
               select.var = list(contrib = 10))
  
  print(millors_vars)
  
  ### qualitat de la representació
  contrib_var <- fviz_mca_var(mca, col.var = "contrib",
               gradient.cols = c("#f5f0eb", color_bar,
                                 colorspace::darken(color_bar, 0.3)),
               repel = TRUE,
               ggtheme = theme_minimal(),
               select.var = list(contrib = 15))+
    labs(title = paste("MCA", nom_bloc, "– Categories (Dim 1 vs 2)"),
                 subtitle = "Top 15 per contribució | color = contribució") +
    theme_minimal(base_size = 12)
  
  print(contrib_var)

  
  # individus per GRUP_ASSIST + el·lipses
  p_ind <- fviz_mca_ind(mca,
                        geom.ind      = "point",
                        col.ind       = grup,
                        palette       = c("#E07B54", "#4A90B8"),
                        legend.title  = "") +
    labs(title    = paste("MCA", nom_bloc, "– Individus per grup"),
         subtitle = "El·lipses de confiança 95%") +
    theme_minimal(base_size = 13)
  print(p_ind)
  
  invisible(mca)  # retorna l'objecte MCA perquè el necessitaré
}



#### ============================================================ ####
####     MCA PER CADA BLOC                                       ####
#### ============================================================ ####

mca_motius <- fer_mca_bloc(
  vars      = motius_vars,
  nom_bloc  = "Motius de NO assistència",
  nivells   = 5,
  color_bar = "#E07B54"
)

mca_estrat <- fer_mca_bloc(
  vars      = estrategies_vars,
  nom_bloc  = "Estratègies d'assistència",
  nivells   = 6,
  color_bar = "#4A90B8"
)

mca_ia <- fer_mca_bloc(
  vars      = ia_vars,
  nom_bloc  = "Ús de la IA",
  nivells   = 6,
  color_bar = "#8E6BBF"
)




#### ============================================================ ####
####     RESUM COMPARATIU DELS 3 MCA                             ####
#### ============================================================ ####

# Coordenades de GRUP_ASSIST en Dim1 per cada bloc
coord_motius <- mca_motius$quali.sup$coord["Regular (≥80%)", 1] -
  mca_motius$quali.sup$coord["Irregular (<80%)", 1]

coord_estrat <- mca_estrat$quali.sup$coord["Regular (≥80%)", 1] -
  mca_estrat$quali.sup$coord["Irregular (<80%)", 1]

coord_ia     <- mca_ia$quali.sup$coord["Regular (≥80%)", 1] -
  mca_ia$quali.sup$coord["Irregular (<80%)", 1]

cat("\n============================================\n")
cat("SEPARACIÓ GRUP_ASSIST EN DIM 1 (Regular - Irregular):\n")
cat("============================================\n")
cat("Motius:      ", round(coord_motius, 3), "\n")
cat("Estratègies: ", round(coord_estrat, 3), "\n")
cat("IA:          ", round(coord_ia,     3), "\n")
cat("\nEl bloc amb major separació discrimina millor entre grups.\n")




dades_motius <- dades %>%
  select(all_of(motius_vars),
         GRUP_ASSIST) %>%
  mutate(
    across(all_of(motius_vars), ~ factor(as.integer(.x), levels = 1:5)),
  ) %>%
  na.omit()

idx_motius <- as.integer(rownames(dades_motius))

# GRUP_ASSIST com a variable suplementària (no entra al càlcul,
# però es projecta per veure on cauen els dos grups)
mca_motius <- MCA(dades_motius,
                  quali.sup = which(names(dades_motius) == "GRUP_ASSIST"),
                  ncp       = 10,
                  graph     = FALSE)

# --- Variància explicada ---
cat("Variància explicada MCA Likert (primeres 8 dimensions):\n")
print(round(dades_motius$eig[1:8, ], 2))

fviz_screeplot(mca_likert,
               addlabels = TRUE,
               barfill   = "#4A90B8",
               barcolor  = "white") +
  labs(title = "MCA Likert – Variància explicada per dimensió") +
  theme_minimal(base_size = 13)

# --- Mapa de categories (Dim 1 vs 2) ---
# Colorejat per bloc
var_noms <- c(
  setNames(rep("Motius",      length(motius_vars)),      motius_vars),
  setNames(rep("Estratègies", length(estrategies_vars)), estrategies_vars),
  setNames(rep("IA",          length(ia_vars)),          ia_vars)
)

fviz_mca_var(mca_likert,
             repel        = TRUE,
             col.var      = "contrib",
             gradient.cols = c("#f5f0eb", "#E07B54", "#C0392B"),
             select.var   = list(contrib = 20)) +  # top 20 categories
  labs(title    = "MCA Likert – Top 20 categories per contribució",
       subtitle = "Dim 1 vs Dim 2") +
  theme_minimal(base_size = 12)

# --- Individus colorejats per GRUP_ASSIST ---
fviz_mca_ind(mca_likert,
             geom.ind     = "point",
             col.ind      = dades$GRUP_ASSIST[idx_mca],
             palette      = c("#E07B54", "#4A90B8"),
             addEllipses  = TRUE,
             ellipse.type = "confidence",
             ellipse.level = 0.95,
             alpha.ind    = 0.5,
             legend.title = "") +
  labs(title    = "MCA Likert – Individus per grup d'assistència",
       subtitle = "El·lipses de confiança 95%") +
  theme_minimal(base_size = 13)

# --- Variables suplementàries (GRUP_ASSIST) projectades ---
fviz_mca_var(mca_likert,
             choice  = "quali.sup",
             repel   = TRUE,
             col.var = "#C0392B") +
  labs(title = "MCA – Posició de GRUP_ASSIST en l'espai factorial") +
  theme_minimal(base_size = 13)

# --- Biplot: categories + individus + GRUP_ASSIST ---
fviz_mca_biplot(mca_likert,
                repel        = TRUE,
                geom.ind     = "point",
                col.ind      = dades$GRUP_ASSIST[idx_mca],
                palette      = c("#E07B54", "#4A90B8"),
                alpha.ind    = 0.3,
                select.var   = list(contrib = 15),
                col.var      = "gray40",
                arrow        = FALSE) +
  labs(title    = "MCA Likert – Biplot (top 15 categories + individus)",
       subtitle = "Individus colorejats per grup d'assistència") +
  theme_minimal(base_size = 12)

# --- Contribució de cada variable a Dim 1 i Dim 2 ---
contrib_dim1 <- mca_likert$var$contrib[, 1]
contrib_dim2 <- mca_likert$var$contrib[, 2]

df_contrib <- data.frame(
  categoria  = names(contrib_dim1),
  contrib_1  = contrib_dim1,
  contrib_2  = contrib_dim2,
  bloc       = case_when(
    gsub("_\\d+$", "", names(contrib_dim1)) %in% motius_vars      ~ "Motius",
    gsub("_\\d+$", "", names(contrib_dim1)) %in% estrategies_vars ~ "Estratègies",
    gsub("_\\d+$", "", names(contrib_dim1)) %in% ia_vars          ~ "IA",
    TRUE ~ "Altres"
  )
) %>%
  arrange(desc(contrib_1))

cat("\nTop 15 categories que més contribueixen a Dim 1:\n")
print(head(df_contrib, 15))

# Gràfic contribucions Dim 1
df_contrib %>%
  head(20) %>%
  ggplot(aes(x = reorder(categoria, contrib_1),
             y = contrib_1, fill = bloc)) +
  geom_col(alpha = 0.85) +
  coord_flip() +
  scale_fill_manual(values = c("Motius"      = "#E07B54",
                               "Estratègies" = "#4A90B8",
                               "IA"          = "#8E6BBF")) +
  labs(title = "Top 20 categories – Contribució a Dim 1",
       x = "", y = "Contribució (%)", fill = "Bloc") +
  theme_minimal(base_size = 12)

sink()
dev.off()