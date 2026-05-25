---
name: Project Context
description: Contexto académico, dataset y enfoque metodológico del TFG sobre absentismo universitario
type: project
---

**Institución:** Facultat d'Economia i Empresa (FEE), Universitat de Barcelona
**Tema:** Absentismo universitario — motivos de no asistencia y estrategias que favorecen la asistencia

**Dataset:** Encuesta a estudiantes de grado. Variables principales:
- Académicas: GRAU (10 grados), CURS, N_ASSIG, NOTA, T_AVAL, P_ASSIST
- Personales: GENERE, EDAT, DEDIC (4 niveles de dedicación laboral), DESPL
- Motivos NO asistencia (M_*): 15 ítems Likert 1–5
- Estrategias asistencia (E_*): 13 ítems Likert 1–6
- Uso IA (IA_*): 8 ítems Likert 1–6
- Texto libre: EXP_POS, EXP_NEG, PROP_MOT (en catalán)

**Variable objetivo:** P_ASSIST (continua 0–100) y GRUP_ASSIST (binaria: Regular ≥80% / Irregular <80%)

**Enfoques metodológicos novedosos planificados (CLAUDE.md):**
- LPA/LCA para arquetipos de estudiantes
- NLP/BERTopic sobre texto libre en catalán
- SEM con variable latente "valor percibido de la clase"
- XGBoost + SHAP (ML interpretable)
- Moderación: IA como variable disruptora del absentismo
- ACM + clustering
- Modelo Tobit / Double-Hurdle (P_ASSIST censurada)
- PCA policórico sobre escalas Likert
- Regresión cuantílica
- Índice sintético de riesgo (IRA)

**Why:** La novedad es operacionalizar la IA generativa como moderador del absentismo, tema inédito pre-2023.
**How to apply:** Priorizar rigor econométrico y metodologías que vayan más allá de regresión logística estándar.
