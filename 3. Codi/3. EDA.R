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

#setwd("C:/Users/edurn/OneDrive/Escritorio/Universitat/TFG---Github/2. Dades")
setwd("C:/Users/Edurne/OneDrive/Escritorio/Universitat/TFG---Github")
load("2.Dades/2. Dades tractades.RData")


motius_vars <- readRDS("2. Dades/motius_vars.rds")
estrategies_vars <- readRDS("2. Dades/estrategies_vars.rds")
ia_vars <- readRDS("2. Dades/ia_vars.rds")

sink("4. Outputs/3.1 Output_text_EDA.txt")
pdf("4. Outputs/3.2 Output_grafics_EDA.pdf",
    width = 10, height = 8)

#### ============================================================ ####
####     1. ASSOCIACIONS ENTRE VARIABLES CATEGÒRIQUES             ####
#### ============================================================ ####

# mesurem associació amb V de Cramer
vars_cat <- c("GRUP_ASSIST", "GRAU", "CURS", "NOTA", "T_AVAL", "GENERE", "DEDIC")
n_cat <- length(vars_cat)

mat_v <- matrix(1, n_cat, n_cat, dimnames = list(vars_cat, vars_cat))
mat_p_v <- matrix(0, n_cat, n_cat, dimnames = list(vars_cat, vars_cat))

for (i in 1:(n_cat - 1)) {
  for (j in (i + 1):n_cat) {
    taula <- table(dades[[vars_cat[i]]], dades[[vars_cat[j]]])
    test  <- chisq.test(taula, simulate.p.value = TRUE, B = 2000)
    n     <- sum(taula)
    k     <- min(nrow(taula), ncol(taula))
    v     <- round(sqrt(as.numeric(test$statistic) / (n * (k - 1))), 3)
    mat_v[i, j]   <- v
    mat_v[j, i]   <- v
    mat_p_v[i, j] <- round(test$p.value, 4)
    mat_p_v[j, i] <- round(test$p.value, 4)
  }
}

cat("\n === Matriu Cramér's V (categòriques) === \n")
print(mat_v)
cat("\n === p-valors (chi-quadrat simulat, B=2000) === \n")
print(mat_p_v)

# Heatmap Cramér's V
df_v_long <- melt(mat_v)
ggplot(df_v_long, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", value)), size = 3) +
  scale_fill_gradient(low = "#f5f0eb", high = "#4A90B8",
                      limits = c(0, 1), name = "V") +
  labs(title = "Cramér's V entre variables categòriques",
       x = "", y = "") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Taula resum de totes les parelles ordenada per V
df_parelles_v <- data.frame()
for (i in 1:(n_cat - 1)) {
  for (j in (i + 1):n_cat) {
    df_parelles_v <- rbind(df_parelles_v, data.frame(
      var1    = vars_cat[i],
      var2    = vars_cat[j],
      V       = mat_v[i, j],
      p_valor = mat_p_v[i, j]
    ))
  }
}
df_parelles_v <- df_parelles_v %>%
  mutate(sig = ifelse(p_valor < 0.001, "***",
                      ifelse(p_valor < 0.01,  "**",
                             ifelse(p_valor < 0.05,  "*", "ns")))) %>%
  filter(sig != "ns") %>%
  arrange(desc(sig))

cat("\n === Parelles categòriques significatives (ordenades per V) === \n")
print(df_parelles_v)

# Associació de cada categòrica amb GRUP_ASSIST
df_grup_cat <- data.frame(
  variable = vars_cat[vars_cat != "GRUP_ASSIST"],
  V        = round(mat_v["GRUP_ASSIST", vars_cat != "GRUP_ASSIST"], 3),
  p_valor  = round(mat_p_v["GRUP_ASSIST", vars_cat != "GRUP_ASSIST"], 4)
) %>%
  mutate(sig = ifelse(p_valor < 0.001, "***",
                      ifelse(p_valor < 0.01,  "**",
                             ifelse(p_valor < 0.05,  "*", "ns")))) %>%
  arrange(desc(V))

cat("\n === Cramér's V de les categòriques amb GRUP_ASSIST === \n")
print(df_grup_cat)



#### ============================================================ ####
####     2. ASSOCIACIONS ENTRE VARIABLES LIKERT                   ####
#### ============================================================ ####

totes_likert  <- c(motius_vars, estrategies_vars, ia_vars)
df_likert_num <- dades %>%
  select(all_of(totes_likert)) %>%
  mutate(across(everything(), as.numeric))
n_lk <- length(totes_likert)

##### ------ 2.1. Kendall tau entre parelles Likert ------ #####

mat_tau   <- matrix(1, n_lk, n_lk, dimnames = list(totes_likert, totes_likert))
mat_tau_p <- matrix(0, n_lk, n_lk, dimnames = list(totes_likert, totes_likert))

for (i in 1:(n_lk - 1)) {
  for (j in (i + 1):n_lk) {
    test <- cor.test(df_likert_num[[totes_likert[i]]],
                     df_likert_num[[totes_likert[j]]],
                     method = "kendall")
    mat_tau[i, j]   <- round(test$estimate, 3)
    mat_tau[j, i]   <- round(test$estimate, 3)
    mat_tau_p[i, j] <- round(test$p.value, 4)
    mat_tau_p[j, i] <- round(test$p.value, 4)
  }
}

# Corrplot (només mostrem |tau| > 0.2 per llegibilitat)
mat_tau_plot <- mat_tau
mat_tau_plot[abs(mat_tau_plot) < 0.2 & mat_tau_plot != 1] <- 0

corrplot(mat_tau_plot,
         method      = "color",
         type        = "lower",
         tl.cex      = 0.65,
         tl.col      = "black",
         col         = colorRampPalette(c("#E07B54", "white", "#4A90B8"))(200),
         addCoef.col = "black",
         number.cex  = 0.45,
         title       = "Kendall tau entre variables Likert (|tau| > 0.2 visible)",
         mar         = c(0, 0, 2, 0))

# Parelles fortes (|tau| > 0.3)
df_parelles_tau <- data.frame()
for (i in 1:(n_lk - 1)) {
  for (j in (i + 1):n_lk) {
    df_parelles_tau <- rbind(df_parelles_tau, data.frame(
      var1    = totes_likert[i],
      var2    = totes_likert[j],
      tau     = mat_tau[i, j],
      p_valor = mat_tau_p[i, j]
    ))
  }
}
df_parelles_tau <- df_parelles_tau %>%
  mutate(sig = ifelse(p_valor < 0.001, "***",
                      ifelse(p_valor < 0.01,  "**",
                             ifelse(p_valor < 0.05,  "*", "ns")))) %>%
  filter(abs(tau) > 0.5) %>%
  arrange(desc(abs(tau)))

cat("\n === Parelles Likert amb |tau| > 0.5 === \n")
print(df_parelles_tau)

##### ------ 2.2. Effect size r (Mann-Whitney) per Likert vs GRUP_ASSIST ------ #####

# r = |Z| / sqrt(n): mesura la magnitud de la diferència entre grups
# Rang [0,1]: ~0.1 petit, ~0.3 mitjà, ~0.5 gran
# Test: Mann-Whitney (Wilcoxon) per significació estadística

resultats_mw <- lapply(totes_likert, function(v) {
  x    <- as.numeric(dades[[v]])
  test <- wilcox.test(x ~ dades$GRUP_ASSIST)
  r    <- round(abs(qnorm(test$p.value / 2)) / sqrt(nrow(dades)), 3)
  data.frame(
    variable = v,
    bloc     = case_when(
      v %in% motius_vars      ~ "Motius",
      v %in% estrategies_vars ~ "Estratègies",
      v %in% ia_vars          ~ "IA"
    ),
    W       = round(test$statistic, 1),
    r       = r,
    p_valor = round(test$p.value, 4),
    sig     = ifelse(test$p.value < 0.001, "***",
                     ifelse(test$p.value < 0.01,  "**",
                            ifelse(test$p.value < 0.05,  "*", "ns")))
  )
})

df_mw <- do.call(rbind, resultats_mw) %>% arrange(desc(r))

cat("\n === Effect size r (Mann-Whitney) per Likert vs GRUP_ASSIST === \n")
print(df_mw)

#cap major que 0.5

# Gràfic effect size r
ggplot(df_mw, aes(x = reorder(variable, r),
                  y = r,
                  fill  = bloc,
                  alpha = sig != "ns")) +
  geom_col() +
  geom_hline(yintercept = c(0.1, 0.3, 0.5),
             linetype = "dashed", color = "gray50") +
  annotate("text", x = 1.5, y = 0.11, label = "petit",  size = 3, color = "gray50") +
  annotate("text", x = 1.5, y = 0.31, label = "mitjà",  size = 3, color = "gray50") +
  annotate("text", x = 1.5, y = 0.51, label = "gran",   size = 3, color = "gray50") +
  scale_fill_manual(values = c("Motius"      = "#E07B54",
                               "Estratègies" = "#4A90B8",
                               "IA"          = "#8E6BBF")) +
  scale_alpha_manual(values = c("TRUE" = 0.9, "FALSE" = 0.35), guide = "none") +
  coord_flip() +
  labs(title    = "Effect size r (Mann-Whitney) per Likert vs GRUP_ASSIST",
       subtitle = "Opac = significatiu (p<0.05)",
       x = "", y = "Effect size r", fill = "Bloc") +
  theme_minimal(base_size = 12)

#### ============================================================ ####
####             3. MCA SOBRE VARIABLES LIKERT                    ####
#### ============================================================ ####

##### ------ 3.1. Funció MCA ------ #####

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
             method = "Indicator",
             quali.sup = which(names(dades_bloc) == "GRUP_ASSIST"),
             ncp = 10,
             graph = FALSE)

  cat("\n ==== Variància explicada (primeres 10 dimensions sense correcció) ====\n")
  eig.val <- get_eigenvalue(mca)
  print(eig.val[1:10, ])

  act.vars <- length(vars)
  threshold <- 1 / act.vars

  # CORRECCIÓ BENZECRI
  calcular_benzecri <- function(mca_objecte, Q) {
    evals <- mca_objecte$eig[, 1]
    thr <- 1 / Q
    ejes_validos <- evals[evals > thr]
    corregits <- (Q / (Q - 1))^2 * (ejes_validos - thr)^2
    percentatges <- round((corregits / sum(corregits)) * 100, 4)
    acumulada <- cumsum(percentatges)
    data.frame(
      Dimensio = 1:length(percentatges),
      Var_Real = round(percentatges, 4),
      Var_Acum = round(acumulada, 4)
    )
  }

  cat("\n ===== Variància corregida (Benzécri) per les primeres 10 dim =====\n")
  benz <- calcular_benzecri(mca, act.vars)
  print(benz[1:min(10, nrow(benz)), ])

  # Screeplot amb variància corregida Benzécri (gràfic manual)
  p_scree <- ggplot(benz[1:min(10, nrow(benz)), ],
                    aes(x = Dimensio, y = Var_Real)) +
    geom_col(fill = color_bar, color = "white", alpha = 0.85) +
    geom_line(aes(y = Var_Acum), color = "gray40", linewidth = 0.7) +
    geom_point(aes(y = Var_Acum), color = "gray40", size = 2) +
    geom_text(aes(label = paste0(Var_Real, "%")),
              vjust = -0.4, size = 3.2) +
    scale_x_continuous(breaks = benz$Dimensio[1:min(10, nrow(benz))]) +
    labs(title    = paste("Scree Plot (Benzécri) –", nom_bloc),
         subtitle = "Barres = % variància corregida | Línia = % acumulat",
         x = "Dimensió", y = "% Variància (Benzécri)") +
    theme_minimal(base_size = 13)
  print(p_scree)

  # Gràfics per a cada parella de dimensions: (1,2), (1,3), (2,3)
  for (dims in list(c(1, 2), c(1, 3), c(2, 3))) {
    d1 <- dims[1]
    d2 <- dims[2]
    etq <- paste0("Dim ", d1, "-", d2)

    # Etiquetes dels eixos amb variància corregida (Benzécri)
    eix_x <- paste0("Dim ", d1, " (", benz$Var_Real[d1], "% Benzécri)")
    eix_y <- paste0("Dim ", d2, " (", benz$Var_Real[d2], "% Benzécri)")

    # 1. Pseudo-correlació variables-dimensions
    p_cor <- fviz_mca_var(mca,
                          choice = "mca.cor",
                          axes = dims,
                          repel = TRUE,
                          ggtheme = theme_minimal()) +
      labs(title = paste("Pseudo-correlació –", etq, "–", nom_bloc),
           x = eix_x, y = eix_y)
    print(p_cor)

    # 2. Qualitat de representació (cos2), top 20 variables
    p_cos2 <- fviz_mca_var(mca,
                           col.var = "cos2",
                           axes = dims,
                           gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                           repel = TRUE,
                           ggtheme = theme_minimal(),
                           select.var = list(contrib = 20)) +
      labs(title    = paste("Variables (cos2) –", etq, "–", nom_bloc),
           subtitle = "Top 20 per contribució | color = cos2",
           x = eix_x, y = eix_y)
    print(p_cos2)

    # 3. Biplot individus + variables colorejats per GRUP_ASSIST
    p_biplot <- fviz_mca_biplot(mca,
                                axes = dims,
                                repel = TRUE,
                                ggtheme = theme_minimal(),
                                col.var = "black",
                                habillage = grup,
                                select.ind = list(contrib = 50),
                                select.var = list(contrib = 15),
                                labelsize = 3) +
      labs(title    = paste("Biplot –", etq, "–", nom_bloc),
           subtitle = "Top 50 individus | color = GRUP_ASSIST",
           x = eix_x, y = eix_y)
    print(p_biplot)
  }

  invisible(mca)
}

##### ------ 3.2. Aplicar MCA per cada bloc ------ #####

mca_motius <- fer_mca_bloc(
  vars = motius_vars,
  nom_bloc = "Motius de NO assistència",
  nivells = 5,
  color_bar = "#E07B54"
)

mca_estrat <- fer_mca_bloc(
  vars = estrategies_vars,
  nom_bloc = "Estratègies d'assistència",
  nivells = 6,
  color_bar = "#4A90B8"
)

mca_ia <- fer_mca_bloc(
  vars = ia_vars,
  nom_bloc = "Ús de la IA",
  nivells = 6,
  color_bar = "#8E6BBF"
)

##### ------ 3.3. Resultats MCA ------ #####

# Coordenades de GRUP_ASSIST en Dim1 per cada bloc
coord_motius <- mca_motius$quali.sup$coord["Regular (≥80%)", 1] -
  mca_motius$quali.sup$coord["Irregular (<80%)", 1]

coord_estrat <- mca_estrat$quali.sup$coord["Regular (≥80%)", 1] -
  mca_estrat$quali.sup$coord["Irregular (<80%)", 1]

coord_ia     <- mca_ia$quali.sup$coord["Regular (≥80%)", 1] -
  mca_ia$quali.sup$coord["Irregular (<80%)", 1]

cat("\n============================================\n")
cat("\n ==== SEPARACIÓ GRUP_ASSIST EN DIM 1 (Regular - Irregular)==== \n")
cat("============================================\n")
cat("Motius:      ", round(coord_motius, 3), "\n")
cat("Estratègies: ", round(coord_estrat, 3), "\n")
cat("IA:          ", round(coord_ia,     3), "\n")

#motius i ia separen molt bé, estratègies no

# Variables correlacionades amb cada dimensió

ordenar_dimdesc <- function(desc, dim_num) {
  bloc <- desc[[dim_num]]$category
  if (!is.null(bloc)) {
    df_ord <- as.data.frame(bloc)
    df_ord$abs_corr <- abs(df_ord$Estimate)
    df_ord <- df_ord[order(-df_ord$abs_corr), ]
    return(df_ord)
  }
}

desc_result_motius <- dimdesc(mca_motius, axes = c(1, 2, 3))
cat("=== DIMENSIÓ 1 MOTIUS===\n")
print(ordenar_dimdesc(desc_result_motius, 1))

cat("\n=== DIMENSIÓ 2 MOTIUS===\n")
print(ordenar_dimdesc(desc_result_motius, 2))

cat("\n=== DIMENSIÓ 3 MOTIUS===\n")
print(ordenar_dimdesc(desc_result_motius, 3))


desc_result_estrat <- dimdesc(mca_estrat, axes = c(1, 2, 3))
cat("=== DIMENSIÓ 1 ESTRATÈGIES===\n")
print(ordenar_dimdesc(desc_result_estrat, 1))

cat("\n=== DIMENSIÓ 2 ESTRATÈGIES===\n")
print(ordenar_dimdesc(desc_result_estrat, 2))

cat("\n=== DIMENSIÓ 3 ESTRATÈGIES===\n")
print(ordenar_dimdesc(desc_result_estrat, 3))


desc_result_ia <- dimdesc(mca_ia, axes = c(1, 2, 3))
cat("=== DIMENSIÓ 1 IA===\n")
print(ordenar_dimdesc(desc_result_ia, 1))

cat("\n=== DIMENSIÓ 2 IA===\n")
print(ordenar_dimdesc(desc_result_ia, 2))

cat("\n=== DIMENSIÓ 3 IA===\n")
print(ordenar_dimdesc(desc_result_ia, 3))


# Guardem les primeres 3 dimensions de cada bloc
# Les files que no estaven al MCA (tenien NA en algun Likert) queden com NA

# Bloc motius
dades[, c("MCA_MOT_D1", "MCA_MOT_D2", "MCA_MOT_D3")] <- NA_real_
idx_m <- as.integer(rownames(mca_motius$ind$coord))
dades[idx_m, c("MCA_MOT_D1", "MCA_MOT_D2", "MCA_MOT_D3")] <- mca_motius$ind$coord[, 1:3]

# Bloc estratègies
dades[, c("MCA_EST_D1", "MCA_EST_D2", "MCA_EST_D3")] <- NA_real_
idx_e <- as.integer(rownames(mca_estrat$ind$coord))
dades[idx_e, c("MCA_EST_D1", "MCA_EST_D2", "MCA_EST_D3")] <- mca_estrat$ind$coord[, 1:3]

# Bloc IA
dades[, c("MCA_IA_D1", "MCA_IA_D2", "MCA_IA_D3")] <- NA_real_
idx_i <- as.integer(rownames(mca_ia$ind$coord))
dades[idx_i, c("MCA_IA_D1", "MCA_IA_D2", "MCA_IA_D3")] <- mca_ia$ind$coord[, 1:3]

cat("\n === Dimensions MCA afegides a dades === \n")
cat("Files amb coordenades motius:     ", sum(!is.na(dades$MCA_MOT_D1)), "\n")
cat("Files amb coordenades estratègies:", sum(!is.na(dades$MCA_EST_D1)), "\n")
cat("Files amb coordenades IA:         ", sum(!is.na(dades$MCA_IA_D1)),  "\n")


sink()
dev.off()