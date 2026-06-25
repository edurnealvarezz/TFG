# Executa aquest script UNA vegada per copiar els fitxers de dades a la carpeta data/
# Des del directori del TFG: source("5. shiny_absentisme/setup_data.R")

dir.create("5. shiny_absentisme/data", showWarnings = FALSE, recursive = TRUE)

fitxers <- list(
  "2. Dades/2. Models/fuzzy_clustering_model_complet.RData" =
    "5. shiny_absentisme/data/fuzzy_clustering_model_complet.RData",
  "2. Dades/2. Models/model_rf_a.rds" = "5. shiny_absentisme/data/model_rf_a.rds"
)

for (ori in names(fitxers)) {
  dest <- fitxers[[ori]]
  if (file.exists(ori)) {
    file.copy(ori, dest, overwrite = TRUE)
    cat(sprintf("OK  %s\n", basename(dest)))
  } else {
    cat(sprintf("--  %s (no trobat)\n", basename(ori)))
  }
}

cat("\nLlest! Obre l'app amb:\n")
cat('  shiny::runApp("5. shiny_absentisme")\n')
