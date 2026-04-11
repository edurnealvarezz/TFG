packages <- c("dplyr", "ggplot2","tidyr","cluster","dbscan","FactoMineR","factoextra","mice")

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

lapply(packages, install_if_missing)
rm(packages)

setwd("C:/Users/edurn/Downloads/TFG")
load("2. Dades/1. Dades amb binaria.RData")


motius_vars <- readRDS("2. Dades/motius_vars.rds")
estrategies_vars <- readRDS("2. Dades/estrategies_vars.rds")
ia_vars <- readRDS("2. Dades/ia_vars.rds")

# Valor impossible: 605 minuts de desplaçament → NA
dades$DESPL[dades$DESPL == 605] <- 65
cat("Observació amb DESPL=605 convertida a 65, es considera error tipogràfic \n")

sink("4. Outputs/1.1 Output_text_preprocessing.txt")
pdf("4. Outputs/1.2 Output_grafics_preprocessing.pdf",
    width = 10, height = 8)

#### ============================================================ ####
####                        1. MISSINGS                           ####
#### ============================================================ ####
##### --------- 1.1. Exploració NA------------ #####

na_resum <- data.frame(
  variable = names(dades),
  n_na     = sapply(dades, function(x) sum(is.na(x))),
  pct_na   = round(sapply(dades, function(x) mean(is.na(x)) * 100), 2)
) %>% filter(n_na > 0) %>% arrange(desc(n_na))

cat("\n === NA per variable === \n")
print(na_resum)

ggplot(na_resum, aes(x = reorder(variable, pct_na), y = pct_na)) +
  geom_col(fill = "#E07B54", alpha = 0.85) +
  geom_text(aes(label = paste0(pct_na, "%")), hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(title = "% NAs per variable",
       x = "", y = "% NA") +
  theme_minimal(base_size = 13)

# NAs de respostes obertes, no cal fer imputació

#### ============================================================ ####
####                      PREPARACIÓ OUTLIERS                     ####
#### ============================================================ ####

dades_gower <- dades %>%
  mutate(
    EDAT     = as.numeric(EDAT),
    DESPL    = as.numeric(DESPL),
    N_ASSIG  = as.numeric(N_ASSIG),
    P_ASSIST = as.numeric(P_ASSIST),
    across(all_of(motius_vars),      ~ factor(as.integer(.x),
                                              levels = 1:5, ordered = TRUE)),
    across(all_of(estrategies_vars), ~ factor(as.integer(.x),
                                              levels = 1:6, ordered = TRUE)),
    across(all_of(ia_vars),          ~ factor(as.integer(.x),
                                              levels = 1:6, ordered = TRUE)),
    GRAU   = factor(GRAU,   ordered = FALSE),
    GENERE = factor(GENERE, ordered = FALSE),
    T_AVAL = factor(T_AVAL, ordered = FALSE),
    CURS = factor(CURS, ordered = TRUE),
    NOTA = factor(NOTA, ordered = TRUE),
    DEDIC= factor(DEDIC,ordered = TRUE)
  ) %>%
  select(
    EDAT, DESPL, N_ASSIG, P_ASSIST,
    all_of(motius_vars),
    all_of(estrategies_vars),
    all_of(ia_vars),
    GRAU, GENERE, T_AVAL, CURS, NOTA, DEDIC
  )

#### ============================================================ ####
####                            LOF                               ####
#### ============================================================ ####
dist_gower <- daisy(dades_gower, metric = "gower")
dist_matrix <- as.matrix(dist_gower)
idx_gower <- 1:nrow(dades_gower)

k_vals <- c(5, 10, 15, round(sqrt(nrow(dist_matrix))), 20) # mirem diferents valors de k
# 5, 10, 16 i 20

lof_list <- lapply(k_vals, function(k) {
  lof(dist_matrix, minPts = k)
})
names(lof_list) <- paste0("k=", k_vals)

cat("\n === Num outliers amb score > 2 per cada k === \n")
outliers_per_k <- sapply(lof_list, function(scores) sum(scores > 2))
print(outliers_per_k) # outliers amb score > 2 per cada k

cat("\n === Index outliers per cada k === \n")
for (k in k_vals) {
  scores <- lof_list[[paste0("k=", k)]]
  idx_out <- idx_gower[scores > 2]
  cat("k =", k, "→", length(idx_out), "outliers | Índexs:", idx_out, "\n")
}

# Només 153 és outlier
cat("\n === Mirem outlier === \n")
dades[153, ] %>%
  select(EDAT, DESPL, N_ASSIG, P_ASSIST, GRAU, CURS, NOTA, DEDIC, GENERE) %>%
  print()

##### -------------------- GRÀFIC  -------------------------#####
k_final=18
lof_scores <- lof(as.dist(dist_matrix), minPts = k_final)
idx_clean <- as.integer(rownames(dades_gower))
df_lof_plot <- data.frame(
  index    = idx_clean,
  score    = lof_scores,
  outlier  = lof_scores > 2
)

ggplot(df_lof_plot, aes(x = index, y = score, color = outlier)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_hline(yintercept = 2, linetype = "dashed",
             color = "red", linewidth = 0.8) +
  geom_text(data = filter(df_lof_plot, outlier),
            aes(label = index), vjust = -0.8, size = 3.2) +
  scale_color_manual(values = c("FALSE" = "#4A90B8", "TRUE" = "#E07B54"),
                     labels = c("Normal", "Outlier")) +
  labs(title = "LOF amb Distància de Gower (Likert individuals)",
       subtitle = paste0("k = ", k_final, " veïns | ",
                         ncol(dades_gower), " variables"),
       x = "Índex observació", y = "LOF Score", color = "") +
  theme_minimal(base_size = 13)


sink()
dev.off()
save(dades, file = "2. Dades/2. Dades tractades.RData")
