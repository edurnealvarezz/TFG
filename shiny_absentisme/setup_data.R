# Executa aquest script UNA vegada per copiar els fitxers de dades a la carpeta data/
# Fes-ho des del directori del TFG: source("shiny_absentisme/setup_data.R")

dir.create("shiny_absentisme/data", showWarnings = FALSE, recursive = TRUE)

fitxers <- list(
  # RData principal (obligatori)
  "2. Dades/fuzzy_clustering_model_complet.RData" =
    "shiny_absentisme/data/fuzzy_clustering_model_complet.RData",
  # Model RF (opcional; crear-lo re-entrenant RF-A)
  # "ruta/model_rf_a.rds" = "shiny_absentisme/data/model_rf_a.rds",
  # Mètriques (opcionals; es generen en executar els scripts)
  "4. Outputs/Metriques i models/metriques_logit_pred.rds" =
    "shiny_absentisme/data/metriques_logit_pred.rds",
  "4. Outputs/Metriques i models/metriques_rf_a.rds" =
    "shiny_absentisme/data/metriques_rf_a.rds",
  "4. Outputs/Metriques i models/metriques_rf_b.rds" =
    "shiny_absentisme/data/metriques_rf_b.rds",
  "4. Outputs/Metriques i models/metriques_xgb.rds" =
    "shiny_absentisme/data/metriques_xgb.rds",
  "4. Outputs/Metriques i models/metriques_catboost.rds" =
    "shiny_absentisme/data/metriques_catboost.rds",
  "4. Outputs/Metriques i models/metriques_svm.rds" =
    "shiny_absentisme/data/metriques_svm.rds",
  "4. Outputs/Metriques i models/metriques_knn.rds" =
    "shiny_absentisme/data/metriques_knn.rds"
)

for (ori in names(fitxers)) {
  dest <- fitxers[[ori]]
  if (file.exists(ori)) {
    file.copy(ori, dest, overwrite = TRUE)
    cat(sprintf("✔  %s\n", basename(dest)))
  } else {
    cat(sprintf("–  %s (no trobat, s'ignorarà)\n", basename(ori)))
  }
}

cat("\nLlest! Obre l'app amb:\n")
cat('  shiny::runApp("shiny_absentisme")\n')
