library(dplyr)
library(ggplot2)
library(tidyr)
library(cluster)
library(dbscan)
library(FactoMineR)
library(factoextra)
library(readxl)
library(writexl)

setwd("C:/Users/edurn/OneDrive/Escritorio/Universitat/TFG---Github/2. Dades")
dades <- read_excel("0. Dades definitives.xlsx")

#### ============================================================ ####
####                           MISSINGS                           ####
#### ============================================================ ####

# NA per variable
na_resum <- data.frame(
  variable = names(dades),
  n_na     = sapply(dades, function(x) sum(is.na(x))),
  pct_na   = round(sapply(dades, function(x) mean(is.na(x)) * 100), 2)
) %>% filter(n_na > 0) %>% arrange(desc(n_na))

print(na_resum)

ggplot(na_resum, aes(x = reorder(variable, pct_na), y = pct_na)) +
  geom_col(fill = "#E07B54", alpha = 0.85) +
  geom_text(aes(label = paste0(pct_na, "%")), hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(title = "% NAs per variable",
       x = "", y = "% NA") +
  theme_minimal(base_size = 13)

# Només hi ha NA a les variables de resposta oberta. Era d'esperar.


motius_vars <- c("M_TREB","M_FAM","M_SALUT","M_DIST","M_AUTON","M_CV",
                 "M_EXAM","M_UTIL","M_AVORR","M_PASSIU","M_TEOR","M_PROF",
                 "M_REPET","M_ACAD","M_AMICS")

estrategies_vars <- c("E_PES_AC","E_PART","E_DINAM","E_REDU","E_CURT","E_DESC",
                      "E_CLIMA","E_EXPL","E_RITME","E_ACT_AC","E_PROP","E_HORA","E_PES_AS")

ia_vars <- c("IA_HABIT", "IA_COMPR", "IA_SUBST", "IA_CONF",
             "IA_ATENC", "IA_PREOC", "IA_REND", "IA_PDFS")




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

cat("Matriu distàncies Gower:", nrow(dades_gower), "x",
    nrow(dades_gower), "\n")


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

outliers_per_k <- sapply(lof_list, function(scores) sum(scores > 2))
print(outliers_per_k) # outliers amb score > 2 per cada k

for (k in k_vals) {
  scores <- lof_list[[paste0("k=", k)]]
  idx_out <- idx_gower[scores > 2]
  cat("k =", k, "→", length(idx_out), "outliers | Índexs:", idx_out, "\n")
}

# Només 153 és outlier

dades[153, ] %>%
  select(EDAT, DESPL, N_ASSIG, P_ASSIST, GRAU, CURS, NOTA, DEDIC, GENERE) %>%
  print()

##### -------------------- GRÀFIC  -------------------------#####
k_final=16
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



#### ============================================================ ####
####                        ELIMINAR: FAMD                        ####
#### ============================================================ ####


dades_famd <- dades %>%
  mutate(
    # Numèriques
    EDAT     = as.numeric(EDAT),
    DESPL    = as.numeric(DESPL),
    N_ASSIG  = as.numeric(N_ASSIG),
    P_ASSIST = as.numeric(P_ASSIST),
    across(all_of(motius_vars),      ~ factor(as.integer(.x))),
    across(all_of(estrategies_vars), ~ factor(as.integer(.x))),
    across(all_of(ia_vars),          ~ factor(as.integer(.x))),
    GRAU   = factor(GRAU),
    CURS   = factor(CURS),
    NOTA   = factor(NOTA),
    T_AVAL = factor(T_AVAL),
    GENERE = factor(GENERE),
    DEDIC  = factor(DEDIC)
  ) %>%
  select(
    EDAT, DESPL, N_ASSIG, P_ASSIST,
    all_of(motius_vars),
    all_of(estrategies_vars),
    all_of(ia_vars),
    GRAU, CURS, NOTA, T_AVAL, GENERE, DEDIC
  )


set.seed(1234)
famd_result <- FAMD(dades_famd,
                    ncp   = 20,
                    graph = FALSE)

var_exp <- famd_result$eig
cat("\nVariància explicada pels primers 15 components:\n")
print(round(var_exp[1:15, ], 2))

write_xlsx(dades, "C:/Users/edurn/OneDrive/Escritorio/Universitat/TFG---Github/2. Dades/1. Preprocessing.xlsx")