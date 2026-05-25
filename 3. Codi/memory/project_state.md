---
name: Project State
description: Estado actual del pipeline de análisis del TFG — qué scripts existen y qué hacen
type: project
---

Pipeline de análisis en `3. Codi/`:

**Completados:**
1. `0. Preparació de les dades.R` — Carga Excel, renombra columnas, transforma variables (factors, Likert, etc.), guarda `0. Dades inicials.RData`
2. `1. Descriptiva.R` — Crea GRUP_ASSIST (corte 80%), estadísticas descriptivas, gráficos por grupo. Guarda `1. Dades amb binaria.RData`
3. `2. Preprocessing.R` — Imputación MICE (1 NA en DESPL), detección outliers LOF con distancia Gower. Guarda `2. Dades tractades.RData`
4. `3. EDA.R` — Asociaciones binarias vs GRUP_ASSIST (Cramér's V), correlaciones Kendall tau, MCA por bloques (Motius/Estratègies/IA). Guarda `3. Dades EDA.RData`
5. `4. EFA likert.R` — EFA policórica en 3 etapas (inicial, refinada, definitiva) con análisis paralela, scree plot, heatmap de cargas, comunalidades, scores factorials. Factores definitivos: MOT_DESMOTIVACIO/AUTOGESTIO/FORCA_MAJOR, EST_QUALITAT_DOC/AVALUACIO_AC/TEMPS_CLASSE/GRUPS_REDUITS, IA_EINA_ESTUDI/IA_SUBSTITUCIO. Guarda `4. Dades EFA.RData`
6. `5. Logit.R` — Regresión logística completa con selección de modelo (modelo 1.2 seleccionado: Likert numèrica IA_SUBST), Box-Tidwell, VIF, observaciones influyentes, CV 10-fold, métricas train+test, Hosmer-Lemeshow, ROC. Guarda `metriques_logit.rds`
7. `6. Random forest.R` — Random Forest (ranger, 500 árboles, importance=permutation) + SHAP (fastshap), métricas train OOB+test, comparación RF vs Logit. Guarda `metriques_rf.rds`

**Ficheros de datos:**
- `2. Dades/4. Dades EFA.RData` — dataset principal con factores EFA renombrados (`dades_def`)
- `2. Dades/metriques_logit.rds` — métricas del logit (test)
- `2. Dades/metriques_rf.rds` — métricas del RF (test)
- `2. Dades/motius_vars.rds`, `estrategies_vars.rds`, `ia_vars.rds` — vectores de nombres de variables

**Nota setwd:** Todos los scripts usan `setwd("C:/Users/edurn/Downloads/TFG")`. La ruta actual es `C:/Users/Edurne/Downloads/TFG` (con E mayúscula). En Windows esto normalmente no da error pero hay que tenerlo en cuenta.

**Por hacer según pipeline:**
- Análisis confirmatorio: SEM, Tobit/Double-Hurdle, regresión cuantílica
- Machine learning avanzado: XGBoost + SHAP, LPA/LCA
- Análisis de texto: BERTopic sobre EXP_POS/EXP_NEG/PROP_MOT
- Índice IRA (índice sintético de riesgo de absentismo)

**Why:** TFG académico con metodologías novedosas más allá de regresión logística estándar.
**How to apply:** Al proponer siguiente paso, partir del estado actual del pipeline. Los scripts 0–6 están completos y funcionales.
