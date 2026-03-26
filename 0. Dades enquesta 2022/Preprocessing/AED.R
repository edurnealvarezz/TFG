packages <- c("tidyverse", "caret", "data.table", "skimr", "DataExplorer", 
              "psych", "naniar", "dlookr", "missForest", "mi", "igraph", 
              "ggraph", "patchwork", "inspectdf","readxl","dplyr","ggplot2")

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

lapply(packages, install_if_missing)

rm(packages,install_if_missing)

   #================================
#### Importació de les dades ####
#================================


data <- read_excel("dades tractades.xlsx")
data <- data[, colSums(!is.na(data)) > 0]
data <- data[,2:52]

# ho convertim a factor
for(i in 1:ncol(data)){
  if(is.character(data[[i]])==TRUE){
    data[[i]] = as.factor(data[[i]])
  }
}

str(data)

data$assistencia <- as.numeric(as.character(data$assistencia))
data$Credits_matriculats <- as.numeric(as.character(data$Credits_matriculats))

hist(data$assistencia)

sum(na.omit(data$assistencia<75))

# Mirem aquestes 53 persones que no van a classe
menys_75 <- data %>% filter(assistencia < 75)
ggplot(menys_75, aes(x = "", fill = Dedicacio_estudis)) +
  geom_bar(width = 1, color = "white") +
  coord_polar(theta = "y") +
  theme_void() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    plot.margin = margin(20, 20, 20, 20)
  )



mod1 <- lm(assistencia ~ 0+ ., data)
summary(mod1)

write.c
