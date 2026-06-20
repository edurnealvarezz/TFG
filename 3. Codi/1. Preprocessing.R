packages <- c("dplyr", "ggplot2","tidyr","cluster","dbscan","FactoMineR",
              "factoextra","mice")

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

lapply(packages, install_if_missing)
rm(packages)

#setwd("C:/Users/edurn/Downloads/TFG")
load("2. Dades/0. Dades inicials.RData")


motius_vars <- readRDS("2. Dades/1. Objectes/motius_vars.rds")
estrategies_vars <- readRDS("2. Dades/1. Objectes/estrategies_vars.rds")
ia_vars <- readRDS("2. Dades/1. Objectes/ia_vars.rds")


png("4. Outputs/1. Preprocessing/grafic_%02d.png", width = 8, height = 6, units = "in", res = 300)

sink("4. Outputs/1. Preprocessing/1.1 Output_text_preprocessing.txt")

#### ============================================================ ####
####     0. CREACIÓ DE LA VARIABLE GRUP_ASSIST (tall al 80%)      ####
#### ============================================================ ####

dades <- dades %>%
  mutate(GRUP_ASSIST = factor(
    ifelse(P_ASSIST >= 80, "Regular (≥80%)", "Irregular (<80%)"),
    levels = c("Irregular (<80%)", "Regular (≥80%)")
  ))

cat("\n === Distribució GRUP_ASSIST: ===\n")
print(table(dades$GRUP_ASSIST))
print(round(prop.table(table(dades$GRUP_ASSIST)) * 100, 1))

col_grups <- c("Irregular (<80%)" = "#E07B54", "Regular (≥80%)" = "#4A90B8")


#### ============================================================ ####
####                       1. VALORS ANÒMALS                      ####
#### ============================================================ ####

# trobat al preprocessament de les dades

print(ggplot(dades, aes(y = DESPL)) +
  geom_boxplot(fill = "#5B9BD5", alpha = 0.8, outlier.shape = 21, width = 0.2) +
  labs(title = "Temps de desplaçament",
       x = "",
       y = "Minuts de desplaçament") +
  theme_minimal(base_size = 13) +
  theme(
    plot.margin = margin(100, 200, 100, 200),
    axis.text.y = element_text(size = 4.5),
    axis.text.x = element_text(size = 4.5)
  ))

# Valor impossible: 605 minuts de desplaçament → NA
dades$DESPL[dades$DESPL == 605] <- 65
cat("Observació amb DESPL=605 convertida a 65, es considera error tipogràfic \n")


#### ============================================================ ####
####                        2. MISSINGS                           ####
#### ============================================================ ####

##### --------- 2.1. Exploració NA------------ #####

na_resum <- data.frame(
  variable = names(dades),
  n_na = sapply(dades, function(x) sum(is.na(x))),
  pct_na = round(sapply(dades, function(x) mean(is.na(x)) * 100), 2)
) %>% filter(n_na > 0) %>% arrange(desc(n_na))

cat("\n === NA per variable === \n")
print(na_resum)

print(ggplot(na_resum, aes(x = reorder(variable, pct_na), y = pct_na)) +
  geom_col(fill = "#5B9BD5", alpha = 0.85, width = 0.5) +
  geom_text(aes(label = paste0(pct_na, "%")), hjust = -0.1, size = 5.5) +
  coord_flip() +
  scale_x_discrete(expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(0, 75), expand = c(0, 0)) +
  labs(title = "% NAs per variable",
       x = "", y = "% NA") +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 10)
  ))
# NAs de respostes obertes, no cal fer imputació

#### ============================================================ ####
####                             2. OUTLIERS                      ####
#### ============================================================ ####

dades_gower <- dades %>%
  mutate(
    EDAT = as.numeric(EDAT),
    DESPL = as.numeric(DESPL),
    N_ASSIG  = as.numeric(N_ASSIG),
    P_ASSIST = as.numeric(P_ASSIST),
    across(all_of(motius_vars), ~ factor(as.integer(.x),
                                              levels = 1:5, ordered = TRUE)),
    across(all_of(estrategies_vars), ~ factor(as.integer(.x),
                                              levels = 1:6, ordered = TRUE)),
    across(all_of(ia_vars), ~ factor(as.integer(.x),
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

##### --------------------- 2.1. LOF --------------------- #####

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

# gràfic
k_final=18
lof_scores <- lof(as.dist(dist_matrix), minPts = k_final)
idx_clean <- as.integer(rownames(dades_gower))
df_lof_plot <- data.frame(
  index = idx_clean,
  score = lof_scores,
  outlier = lof_scores > 2
)

print(ggplot(df_lof_plot, aes(x = index, y = score, color = outlier)) +
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
  theme_minimal(base_size = 13))

sink()
dev.off()
save(dades, file = "2. Dades/1. Dades tractades.RData")