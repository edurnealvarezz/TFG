---
name: Mètriques train+test per overfitting
description: Sempre calcular mètriques de classificació tant sobre train com sobre test per detectar overfitting/underfitting
type: feedback
---

Sempre calcular i mostrar mètriques (AUC, accuracy, F1, balanced accuracy) tant sobre el conjunt **train** com sobre el **test**, amb una taula comparativa train vs test.

**Why:** Permet detectar overfitting (train >> test) o underfitting (tots dos baixos).

**How to apply:**
- En `calcular_metriques()` (logit): cridar la funció dues vegades, una amb `dades_test` i una amb `dades_train`. No cal passar `auc_cv_mean` a la versió train.
- En `calcular_metriques_rf()` (ranger): per al train usar `rf_model$predictions[, 2]` (prediccions OOB, no in-bag) — les prediccions OOB són imparcials i comparables directament amb el test. Mai usar prediccions in-bag sobre train (donaria AUC ≈ 1 per definició).
- Afegir taula `data.frame(Conjunt, AUC, Accuracy, F1, Balanced_Acc)` amb files Train i Test.
- Guardar a RDS sempre les mètriques del test (no les de train, que son orientatives).
