packages <- c("dplyr", "ggplot2", "tidyr", "car", "pROC",
              "ResourceSelection", "MASS", "tibble", "caret")

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

lapply(packages, install_if_missing)
rm(packages)

#setwd("C:/Users/edurn/Downloads/TFG")
#setwd("C:/Users/Edurne/Downloads/TFG")

load("2. Dades/4. Dades EFA.RData")

sink("4. Outputs/9.1 Output_text_logit_millorat.txt")
pdf("4. Outputs/9.2 Output_grafics_logit_millorat.pdf", width = 10, height = 8)

#### ============================================================ ####
####                   0. PREPARACIÓ DE DADES                     ####
#### ============================================================ ####

vars_fa_est_ref <- "EST_AVALUACIO_AC"
vars_num_ref <- c("EDAT", "NOTA_num")
vars_cat <- c("T_AVAL", "CURS_1R")
vars_fa_mot_3 <- c("MOT_DESMOTIVACIO", "MOT_FORCA_MAJOR")
vars_ia_12 <- "IA_SUBST_num"

formula <- as.formula(paste("Y ~", paste(c(vars_fa_mot_3, vars_fa_est_ref, vars_ia_12,
                                              vars_cat, vars_num_ref), collapse = " + ")))
model <- glm(formula, data = dades_train, family = binomial)

print(summary(model))