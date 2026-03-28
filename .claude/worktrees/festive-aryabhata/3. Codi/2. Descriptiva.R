library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(ggridges)
library(patchwork)
library(readxl)
select <- dplyr::select

#### ============================================================ ####
####          DESCRIPTIVA GENERAL                                 ####
#### ============================================================ ####

setwd("C:/Users/edurn/OneDrive/Escritorio/Universitat/TFG---Github/2. Dades")
dades <- read_excel("1. Preprocessing.xlsx")

dades %>%
  select(EDAT, DESPL, N_ASSIG, P_ASSIST) %>%
  summary()

dades %>%
  select(EDAT, DESPL, N_ASSIG, P_ASSIST) %>%
  summarise(across(everything(), list(
    Mitjana = ~mean(.x, na.rm = TRUE),
    Mediana = ~median(.x, na.rm = TRUE),
    sd      = ~sd(.x, na.rm = TRUE),
    min     = ~min(.x, na.rm = TRUE),
    max     = ~max(.x, na.rm = TRUE)
  ))) %>%
  pivot_longer(everything(), names_to = c("Variable", "Estadistic"),
               names_sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = Estadistic, values_from = value)

# --- 3. LIKERT: mitjanes per bloc ---
# Motius de no assistència
motius_vars <- c("M_TREB","M_FAM","M_SALUT","M_DIST","M_AUTON","M_CV",
                 "M_EXAM","M_UTIL","M_AVORR","M_PASSIU","M_TEOR","M_PROF",
                 "M_REPET","M_ACAD","M_AMICS")

dades %>%
  select(all_of(motius_vars)) %>%
  summarise(across(everything(), ~mean(as.numeric(.x), na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "Motiu", values_to = "Mitjana") %>%
  arrange(desc(Mitjana))

# Estratègies
estrategies_vars <- c("E_PES_AC","E_PART","E_DINAM","E_REDU","E_CURT","E_DESC",
                      "E_CLIMA","E_EXPL","E_RITME","E_ACT_AC","E_PROP","E_HORA","E_PES_AS")

dades %>%
  select(all_of(estrategies_vars)) %>%
  summarise(across(everything(), ~mean(as.numeric(.x), na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "Estratègia", values_to = "Mitjana") %>%
  arrange(desc(Mitjana))

# IA
ia_vars <- c("IA_HABIT", "IA_COMPR", "IA_SUBST", "IA_CONF",
             "IA_ATENC", "IA_PREOC", "IA_REND", "IA_PDFS")
dades %>%
  select(all_of(ia_vars)) %>%
  summarise(across(everything(), ~mean(as.numeric(.x), na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "Ús", values_to = "Mitjana") %>%
  arrange(desc(Mitjana))


#### ============================================================ ####
####     CREACIÓ DE LA VARIABLE GRUP_ASSIST (tall al 80%)        ####
#### ============================================================ ####

dades <- dades %>%
  mutate(GRUP_ASSIST = factor(
    ifelse(P_ASSIST >= 80, "Regular (≥80%)", "Irregular (<80%)"),
    levels = c("Irregular (<80%)", "Regular (≥80%)")
  ))

cat("Distribució GRUP_ASSIST:\n")
print(table(dades$GRUP_ASSIST))
print(round(prop.table(table(dades$GRUP_ASSIST)) * 100, 1))

col_grups <- c("Irregular (<80%)" = "#E07B54", "Regular (≥80%)" = "#4a90b8")


#### ============================================================ ####
####     GRÀFICS LIKERT PER GRUP                                 ####
#### ============================================================ ####




##### ------- 3.1. P_ASSIST VS VARIABLES NUMÈRIQUES -------- ####

# Funció auxiliar: etiqueta Spearman (ρ i p-valor)
lbl_spearman <- function(xvar, yvar) {
  d  <- dades[, c(xvar, yvar)]
  d  <- d[complete.cases(d), ]
  ct <- cor.test(as.numeric(d[[1]]), as.numeric(d[[2]]),
                 method = "spearman", exact = FALSE)
  sprintf("\u03c1 = %.2f\np = %.3f", ct$estimate, ct$p.value)
}

# P_ASSIST vs EDAT
p_edat <- ggplot(dades, aes(x = EDAT, y = P_ASSIST)) +
  geom_jitter(aes(color = GRUP_ASSIST), alpha = 0.5, size = 2, width = 0.2) +
  geom_smooth(method = "lm", color = "black", linewidth = 0.8, se = TRUE) +
  geom_hline(yintercept = 80, linetype = "dotted", color = "gray40") +
  annotate("label", x = Inf, y = Inf,
           label = lbl_spearman("EDAT", "P_ASSIST"),
           hjust = 1.05, vjust = 1.2, size = 3.2, color = "gray30",
           fill = "white", label.size = 0.2) +
  scale_color_manual(values = col_grups) +
  labs(title = "vs Edat", x = "Edat (anys)", y = "% Assistència", color = "") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

# P_ASSIST vs DESPL
p_despl <- ggplot(dades, aes(x = DESPL, y = P_ASSIST)) +
  geom_point(aes(color = GRUP_ASSIST), alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", color = "black", linewidth = 0.8, se = TRUE) +
  geom_hline(yintercept = 80, linetype = "dotted", color = "gray40") +
  annotate("label", x = Inf, y = Inf,
           label = lbl_spearman("DESPL", "P_ASSIST"),
           hjust = 1.05, vjust = 1.2, size = 3.2, color = "gray30",
           fill = "white", label.size = 0.2) +
  scale_color_manual(values = col_grups) +
  labs(title = "vs Desplaçament", x = "Minuts", y = "% Assistència", color = "") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

# P_ASSIST vs N_ASSIG (variable discreta → violin + jitter)
p_nassig <- ggplot(dades, aes(x = factor(N_ASSIG), y = P_ASSIST)) +
  geom_violin(fill = "gray85", alpha = 0.5, trim = FALSE) +
  geom_jitter(aes(color = GRUP_ASSIST), alpha = 0.5, size = 1.8, width = 0.12) +
  geom_hline(yintercept = 80, linetype = "dotted", color = "gray40") +
  annotate("label", x = Inf, y = Inf,
           label = lbl_spearman("N_ASSIG", "P_ASSIST"),
           hjust = 1.05, vjust = 1.2, size = 3.2, color = "gray30",
           fill = "white", label.size = 0.2) +
  scale_color_manual(values = col_grups) +
  labs(title = "vs Nre. Assignatures",
       x = "Nombre d'assignatures", y = "% Assistència", color = "") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

(p_edat | p_despl | p_nassig) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

##### --------- 2. GRUP_ASSIST VS VARIABLES ACADÈMIQUES ------ #####

# GRUP_ASSIST per CURS
df_curs <- dades %>%
  group_by(CURS, GRUP_ASSIST) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(CURS) %>%
  mutate(prop = n / sum(n),
         total = sum(n)) %>%
  ungroup()

df_curs_total <- df_curs %>% distinct(CURS, total)

ggplot(df_curs, aes(x = CURS, y = prop, fill = GRUP_ASSIST)) +
  geom_col(alpha = 0.9) +
  geom_text(aes(label = paste0(round(prop * 100), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 3.5, fontface = "bold") +
  geom_text(data = df_curs_total,
            aes(label = paste0("n=", total), x = CURS, y = 1.04),
            inherit.aes = FALSE, color = "gray30", size = 3.2) +
  scale_fill_manual(values = col_grups) +
  scale_y_continuous(labels = percent, limits = c(0, 1.08)) +
  labs(title = "Assistència regular per curs",
       x = "Curs", y = "Proporció", fill = "") +
  theme_minimal(base_size = 13)


# GRUP_ASSIST per GRAU
df_grau <- dades %>%
  group_by(GRAU, GRUP_ASSIST) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(GRAU) %>%
  mutate(prop = n / sum(n), total = sum(n)) %>%
  ungroup()

df_grau_total <- df_grau %>% distinct(GRAU, total)

ggplot(df_grau, aes(x = reorder(GRAU, prop * (GRUP_ASSIST == "Regular (≥70%)")),
                    y = prop, fill = GRUP_ASSIST)) +
  geom_col(alpha = 0.9) +
  geom_text(aes(label = paste0(round(prop * 100), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 3.2, fontface = "bold") +
  geom_text(data = df_grau_total,
            aes(label = paste0("n=", total), x = GRAU, y = 1.06),
            inherit.aes = FALSE, color = "gray30", size = 3) +
  scale_fill_manual(values = col_grups) +
  scale_y_continuous(labels = percent, limits = c(0, 1.12)) +
  coord_flip() +
  labs(title = "Assistència regular per grau",
       x = "", y = "Proporció", fill = "") +
  theme_minimal(base_size = 13)

# NOTA per GRUP_ASSIST
df_nota <- dades %>%
  group_by(NOTA, GRUP_ASSIST) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(NOTA) %>%
  mutate(prop = n / sum(n), total = sum(n)) %>%
  ungroup()

df_nota_total <- df_nota %>% distinct(NOTA, total)

ggplot(df_nota, aes(x = NOTA, y = prop, fill = GRUP_ASSIST)) +
  geom_col(alpha = 0.9) +
  geom_text(aes(label = paste0(round(prop * 100), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 3.5, fontface = "bold") +
  geom_text(data = df_nota_total,
            aes(label = paste0("n=", total), x = NOTA, y = 1.06),
            inherit.aes = FALSE, color = "gray30", size = 3.2) +
  scale_fill_manual(values = col_grups) +
  scale_y_continuous(labels = percent, limits = c(0, 1.12)) +
  labs(title = "Assistència regular per nota d'expedient",
       x = "Nota", y = "Proporció", fill = "") +
  theme_minimal(base_size = 13)

# T_AVAL per GRUP_ASSIST
df_taval <- dades %>%
  group_by(T_AVAL, GRUP_ASSIST) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(T_AVAL) %>%
  mutate(prop = n / sum(n), total = sum(n)) %>%
  ungroup()

df_taval_total <- df_taval %>% distinct(T_AVAL, total)

ggplot(df_taval, aes(x = T_AVAL, y = prop, fill = GRUP_ASSIST)) +
  geom_col(alpha = 0.9, width = 0.5) +
  geom_text(aes(label = paste0(round(prop * 100), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4, fontface = "bold") +
  geom_text(data = df_taval_total,
            aes(label = paste0("n=", total), x = T_AVAL, y = 1.06),
            inherit.aes = FALSE, color = "gray30", size = 3.2) +
  scale_fill_manual(values = col_grups) +
  scale_y_continuous(labels = percent, limits = c(0, 1.12)) +
  labs(title = "Assistència regular per tipus d'avaluació",
       x = "", y = "Proporció", fill = "") +
  theme_minimal(base_size = 13)


##### ---- 3. GRUP_ASSIST VS VARIABLES PERSONALS ----- #####

# DEDIC per GRUP_ASSIST amb P_ASSIST boxplot ---
ggplot(dades, aes(x = DEDIC, y = P_ASSIST, fill = DEDIC)) +
  geom_boxplot(alpha = 0.8, outlier.shape = 21) +
  geom_hline(yintercept = 70, linetype = "dashed", color = "gray40") +
  scale_fill_brewer(palette = "Oranges") +
  labs(title = "% Assistència per dedicació laboral",
       x = "", y = "% Assistència") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 15, hjust = 1))

# GENERE per GRUP_ASSIST
df_genere <- dades %>%
  group_by(GENERE, GRUP_ASSIST) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(GENERE) %>%
  mutate(prop = n / sum(n), total = sum(n)) %>%
  ungroup()

df_genere_total <- df_genere %>% distinct(GENERE, total)

ggplot(df_genere, aes(x = GENERE, y = prop, fill = GRUP_ASSIST)) +
  geom_col(alpha = 0.9, width = 0.6) +
  geom_text(aes(label = paste0(round(prop * 100), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 3.5, fontface = "bold") +
  geom_text(data = df_genere_total,
            aes(label = paste0("n=", total), x = GENERE, y = 1.06),
            inherit.aes = FALSE, color = "gray30", size = 3) +
  scale_fill_manual(values = col_grups) +
  scale_y_continuous(labels = percent, limits = c(0, 1.12)) +
  labs(title = "Assistència regular per gènere",
       x = "", y = "Proporció", fill = "") +
  theme_minimal(base_size = 13)


# heatmap CURS x DEDIC → % assistència mitjana
dades %>%
  group_by(CURS, DEDIC) %>%
  summarise(assist_mitj = mean(P_ASSIST, na.rm = TRUE),
            n = n(), .groups = "drop") %>%
  ggplot(aes(x = CURS, y = DEDIC, fill = assist_mitj)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(round(assist_mitj), "%\nn=", n)),
            size = 3, color = "white", fontface = "bold") +
  scale_fill_gradient2(low = "#E07B54", mid = "#f5f0eb",
                       high = "#4A90B8", midpoint = 70,
                       name = "% Assist.") +
  labs(title = "% Assistència mitjana per curs i dedicació laboral",
       x = "Curs", y = "") +
  theme_minimal(base_size = 13)

# heatmap GRAU x CURS → % regular
dades %>%
  group_by(GRAU, CURS) %>%
  summarise(pct_regular = mean(GRUP_ASSIST == "Regular (≥80%)") * 100,
            n = n(), .groups = "drop") %>%
  filter(n >= 3) %>%  # elimina cel·les amb menys de 3 obs
  ggplot(aes(x = CURS, y = GRAU, fill = pct_regular)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(round(pct_regular), "%")),
            size = 3, color = "white", fontface = "bold") +
  scale_fill_gradient2(low = "#E07B54", mid = "#f5f0eb",
                       high = "#4A90B8", midpoint = 60,
                       name = "% Regular") +
  labs(title = "% Assistència regular per grau i curs",
       subtitle = "Només cel·les amb n ≥ 3",
       x = "Curs", y = "") +
  theme_minimal(base_size = 13)


##### ---- 4. ESCALA DE LIKERT ----- #####

# Mitjanes motius de NO assistència per grup
dades %>%
  select(GRUP_ASSIST, all_of(motius_vars)) %>%
  mutate(across(all_of(motius_vars), as.numeric)) %>%
  group_by(GRUP_ASSIST) %>%
  summarise(across(all_of(motius_vars), ~mean(.x, na.rm = TRUE))) %>%
  pivot_longer(-GRUP_ASSIST, names_to = "motiu", values_to = "mitjana") %>%
  ggplot(aes(x = reorder(motiu, mitjana), y = mitjana, fill = GRUP_ASSIST)) +
  geom_col(position = "dodge", alpha = 0.9) +
  scale_fill_manual(values = col_grups) +
  coord_flip() +
  labs(title = "Motius de NO assistència per grup",
       subtitle = "Mitjana escala 1-5",
       x = "", y = "Mitjana", fill = "") +
  theme_minimal(base_size = 12)

# Mitjanes estratègies per grup
dades %>%
  select(GRUP_ASSIST, all_of(estrategies_vars)) %>%
  mutate(across(all_of(estrategies_vars), as.numeric)) %>%
  group_by(GRUP_ASSIST) %>%
  summarise(across(all_of(estrategies_vars), ~mean(.x, na.rm = TRUE))) %>%
  pivot_longer(-GRUP_ASSIST, names_to = "estrategia", values_to = "mitjana") %>%
  ggplot(aes(x = reorder(estrategia, mitjana), y = mitjana, fill = GRUP_ASSIST)) +
  geom_col(position = "dodge", alpha = 0.9) +
  scale_fill_manual(values = col_grups) +
  coord_flip() +
  labs(title = "Estratègies d'assistència per grup",
       subtitle = "Mitjana escala 1-6",
       x = "", y = "Mitjana", fill = "") +
  theme_minimal(base_size = 12)

# Ús de la IA per grup
dades %>%
  select(GRUP_ASSIST, all_of(ia_vars)) %>%
  mutate(across(all_of(ia_vars), as.numeric)) %>%
  group_by(GRUP_ASSIST) %>%
  summarise(across(all_of(ia_vars), ~mean(.x, na.rm = TRUE))) %>%
  pivot_longer(-GRUP_ASSIST, names_to = "ia_var", values_to = "mitjana") %>%
  ggplot(aes(x = reorder(ia_var, mitjana), y = mitjana, fill = GRUP_ASSIST)) +
  geom_col(position = "dodge", alpha = 0.9) +
  scale_fill_manual(values = col_grups) +
  coord_flip() +
  labs(title = "Ús de la IA per grup d'assistència",
       subtitle = "Mitjana escala 1-6",
       x = "", y = "Mitjana", fill = "") +
  theme_minimal(base_size = 12)


# Diferència de mitjanes (Regular - Irregular) per motius no assist
df_motius %>%
  select(-se) %>%
  pivot_wider(names_from = GRUP_ASSIST, values_from = mitjana) %>%
  mutate(diferencia = `Regular (≥80%)` - `Irregular (<80%)`) %>%
  ggplot(aes(x = reorder(variable, diferencia),
             y = diferencia,
             fill = diferencia > 0)) +
  geom_col(alpha = 0.85) +
  geom_hline(yintercept = 0, color = "gray30") +
  scale_fill_manual(values = c("TRUE" = "#4A90B8", "FALSE" = "#E07B54"),
                    labels = c("Més alt en Irregulars", "Més alt en Regulars")) +
  coord_flip() +
  labs(title = "Diferència en motius de NO assistència (Regular − Irregular)",
       subtitle = "Blau = motiu més valorat pels regulars | Taronja = pels irregulars",
       x = "", y = "Diferència de mitjanes", fill = "") +
  theme_minimal(base_size = 12)

# Diferència de mitjanes (Regular - Irregular) per estratègies

df_estrat %>%
  dplyr::select(-se) %>%
  pivot_wider(names_from = GRUP_ASSIST, values_from = mitjana) %>%
  mutate(diferencia = `Regular (≥70%)` - `Irregular (<70%)`) %>%
  ggplot(aes(x = reorder(variable, diferencia),
             y = diferencia,
             fill = diferencia > 0)) +
  geom_col(alpha = 0.85) +
  geom_hline(yintercept = 0, color = "gray30") +
  scale_fill_manual(values = c("TRUE" = "#4A90B8", "FALSE" = "#E07B54"),
                    labels = c("Més alt en Irregulars", "Més alt en Regulars")) +
  coord_flip() +
  labs(title = "Diferència en estratègies d'assistència (Regular − Irregular)",
       subtitle = "Blau = estratègia més valorada pels regulars | Taronja = pels irregulars",
       x = "", y = "Diferència de mitjanes", fill = "") +
  theme_minimal(base_size = 12)

# Diferència de mitjanes (Regular - Irregular) per IA

df_ia %>%
  dplyr::select(-se) %>%
  pivot_wider(names_from = GRUP_ASSIST, values_from = mitjana) %>%
  mutate(diferencia = `Regular (≥70%)` - `Irregular (<70%)`) %>%
  ggplot(aes(x = reorder(variable, diferencia),
             y = diferencia,
             fill = diferencia > 0)) +
  geom_col(alpha = 0.85) +
  geom_hline(yintercept = 0, color = "gray30") +
  scale_fill_manual(values = c("TRUE" = "#4A90B8", "FALSE" = "#E07B54"),
                    labels = c("Més alt en Irregulars", "Més alt en Regulars")) +
  coord_flip() +
  labs(title = "Diferència en ús de la IA (Regular − Irregular)",
       subtitle = "Blau = més valorat pels regulars | Taronja = pels irregulars",
       x = "", y = "Diferència de mitjanes", fill = "") +
  theme_minimal(base_size = 12)

