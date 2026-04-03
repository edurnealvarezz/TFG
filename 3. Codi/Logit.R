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
load("2. Dades/2. Dades tractades.RData")


motius_vars <- readRDS("2. Dades/motius_vars.rds")
estrategies_vars <- readRDS("2. Dades/estrategies_vars.rds")
ia_vars <- readRDS("2. Dades/ia_vars.rds")

sink("4. Outputs/4.1 Output_text_logit.txt")
pdf("4. Outputs/4.2 Output_grafics_logit.pdf",
    width = 10, height = 8)

