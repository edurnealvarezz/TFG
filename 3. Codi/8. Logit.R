packages <- c("dplyr", "ggplot2", "tibble", "tidyr",
              "ranger", "caret", "pROC", "fastshap")
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

lapply(packages, install_if_missing)
rm(packages)

setwd("C:/Users/edurn/Downloads/TFG")
#setwd("C:/Users/Edurne/Downloads/TFG")

load("2. Dades/4. Dades EFA.RData")

