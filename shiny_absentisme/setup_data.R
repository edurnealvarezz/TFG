# Executa aquest script UNA vegada per copiar els fitxers de dades a la carpeta data/
# Des del directori del TFG: source("shiny_absentisme/setup_data.R")

dir.create("shiny_absentisme/data", showWarnings = FALSE, recursive = TRUE)

fitxers <- list(
  "2. Dades/fuzzy_clustering_model_complet.RData" =
    "shiny_absentisme/data/fuzzy_clustering_model_complet.RData",
  "2. Dades/metriques_logit_pred.rds" = "shiny_absentisme/data/metriques_logit_pred.rds",
  "2. Dades/metriques_rf_a.rds" = "shiny_absentisme/data/metriques_rf_a.rds",
  "2. Dades/metriques_rf_b.rds" = "shiny_absentisme/data/metriques_rf_b.rds",
  "2. Dades/metriques_xgb.rds" = "shiny_absentisme/data/metriques_xgb.rds",
  "2. Dades/metriques_catboost.rds" = "shiny_absentisme/data/metriques_catboost.rds",
  "2. Dades/metriques_svm.rds" = "shiny_absentisme/data/metriques_svm.rds",
  "2. Dades/metriques_knn.rds" = "shiny_absentisme/data/metriques_knn.rds"
  # model_rf_a.rds: afegir quan el re-entreneu
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
cat('  shiny::runApp("shiny_absentisme")\n')
