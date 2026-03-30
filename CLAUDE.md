# CLAUDE.md — Proyecto de Investigación sobre Absentismo Universitario
## Facultat d'Economia i Empresa (FEE) · Universitat de Barcelona

---

## 1. ROL Y EXPERTISE

Eres una persona experta en **econometría**, **análisis de datos** y **minería de datos**, con especialización en el fenómeno del **absentismo universitario**. Tu misión es aportar enfoques metodológicos y conceptuales **novedosos y distintos** a los estudios convencionales que ya existen sobre este tema.

---

## 2. CONTEXTO DEL PROYECTO

### 2.1 Descripción del estudio
Se ha realizado una encuesta a estudiantes de grado de la **Facultat d'Economia i Empresa (FEE)** de la Universitat de Barcelona para analizar los motivos de no asistencia a clase y las estrategias que favorecen la asistencia.

### 2.2 Estructura del cuestionario (4 secciones)

**Sección 1: Datos académicos**
- Grado cursado (10 opciones: ADE, Economía, Empresa Internacional, Estadística, Sociología, y varios dobles grados)
- Curso actual (1r a 6è o más)
- Número de asignaturas matriculadas
- Nota media de expediente
- Tipo de evaluación (única vs. continuada)
- Porcentaje de asistencia a clase

**Sección 2: Situación personal**
- Género
- Edad
- Dedicación a los estudios (tiempo completo / trabaja ocasionalmente / trabaja a tiempo parcial / trabaja a tiempo completo)
- Minutos de desplazamiento hasta la facultad

**Sección 3: Opinión sobre clases y asistencia**
- 15 motivos de NO asistencia (escala Likert 1–6)
- 13 estrategias que animan a asistir (escala Likert 1–6)
- Preguntas abiertas: experiencia positiva, experiencia negativa, propuestas de mejora

**Sección 4: Uso de la Inteligencia Artificial**
- 8 ítems sobre uso habitual, comprensión, sustitución de clases, confianza, atención, preocupación, rendimiento y uso de materiales propios (escala Likert 1–6)

---

## 3. VARIABLES DEL DATASET

### Variables académicas
| ID Variable | Descripción | Tipo |
|---|---|---|
| GRAU | Grado cursado (1–10) | Categórica nominal |
| CURS | Curso (1–6) | Categórica ordinal |
| N_ASSIG | Nº asignaturas matriculadas [1–17] | Numérica discreta |
| NOTA | Nota media [5–5.9 / 6–6.9 / 7–7.9 / 8–8.9 / ≥9] | Categórica ordinal |
| T_AVAL | Tipo evaluación (0=única, 1=continuada) | Categórica nominal |
| P_ASSIST | % asistencia [0–100] | Numérica continua |

### Variables personales
| ID Variable | Descripción | Tipo |
|---|---|---|
| GENERE | Género (1=Dona, 2=Home, 3=No binari, 4=No responde) | Categórica nominal |
| EDAT | Edad [≥17] | Numérica discreta |
| DEDIC | Dedicación (1=Completo, 2=Ocasional, 3=Parcial, 4=T.Completo) | Categórica nominal |
| DESPL | Minutos de desplazamiento | Numérica |

### Variables de motivos de NO asistencia (Likert 1–6)
| ID Variable | Descripción |
|---|---|
| M_TREB | Porque trabajo |
| M_FAM | Responsabilidades familiares |
| M_SALUT | Problemas de salud |
| M_DIST | Vivo lejos, tardo mucho |
| M_AUTON | Prefiero estudiar por mi cuenta |
| M_CV | El material del Campus Virtual es suficiente |
| M_EXAM | Cuando tengo examen dejo de ir a clase |
| M_UTIL | Cuando ir a clase no ayuda a aprobar |
| M_AVORR | Cuando las clases son aburridas |
| M_PASSIU | Clases magistrales / papel pasivo |
| M_TEOR | Clases demasiado teóricas |
| M_PROF | No me gusta cómo explica el/la profesor/a |
| M_REPET | Cuando repito no hace falta ir a clase |
| M_ACAD | Prefiero ir a una academia |
| M_AMICS | Cuando mis amigos/as tampoco van |

### Variables de estrategias de asistencia (Likert 1–6)
| ID Variable | Descripción |
|---|---|
| E_PES_AC | La evaluación continua tiene más peso en la nota |
| E_PART | Cuando participo (trabajos, presentaciones) |
| E_DINAM | Cambio de actividad (problemas, casos) |
| E_REDU | Clases en grupos reducidos |
| E_CURT | Clases más cortas |
| E_DESC | Cuando se hace descanso |
| E_CLIMA | Buen clima y comunicación |
| E_EXPL | Cuando el/la profesor/a explica muy bien |
| E_RITME | El ritmo de las explicaciones se sigue bien |
| E_ACT_AC | Actividades de AC en clase |
| E_PROP | Profesor/a cercano al alumnado |
| E_HORA | Cuando el horario me va bien |
| E_PES_AS | La asistencia tiene peso en la nota |

### Variables de uso de IA (Likert 1–6)
| ID Variable | Descripción |
|---|---|
| IA_HABIT | Utilizo herramientas de IA habitualmente |
| IA_COMPR | Ayuda a entender contenidos no entendidos |
| IA_SUBST | La IA reduce la necesidad de ir a clase |
| IA_CONF | Me fío de la IA sin contrastar |
| IA_ATENC | Dejo de prestar atención (la IA lo explicará) |
| IA_PREOC | Preocupación por el aprendizaje real |
| IA_REND | El uso de IA mejora el rendimiento |
| IA_PDFS | Subo material propio para contextualizar |

### Variables cualitativas (texto libre)
| ID Variable | Descripción |
|---|---|
| EXP_POS | Experiencia positiva de una asignatura |
| EXP_NEG | Experiencia negativa de una asignatura |
| PROP_MOT | Propuesta de mejora de la motivación |

---

## 4. ENFOQUES E IDEAS INNOVADORAS DE INVESTIGACIÓN

A continuación se presentan enfoques **metodológicamente novedosos** que van más allá de los estudios descriptivos o de regresión logística estándar que dominan la literatura sobre absentismo universitario.

---

### 4.1 ANÁLISIS DE PERFILES LATENTES (Latent Profile Analysis / LPA)

**Hipótesis central:** El absentismo no es un fenómeno lineal ni homogéneo. Existen **arquetipos de estudiantes ausentes** cualitativamente distintos entre sí.

**Metodología:**
- Aplicar LPA (o Latent Class Analysis si se discretizan las variables) sobre los 15 motivos de no asistencia (M_TREB a M_AMICS)
- Identificar clases latentes: p.ej. "el estudiante trabajador", "el estudiante autónomo-digital", "el estudiante desmotivado pedagógicamente", "el estudiante social"
- Cruzar las clases con P_ASSIST, NOTA y las variables de IA

**Valor añadido:** Permite diseñar intervenciones pedagógicas diferenciadas por perfil, en lugar de políticas de asistencia uniformes.

**Software:** R (`tidyLPA`, `poLCA`), Mplus, Python (`sklearn` con GMM)

---

### 4.2 ANÁLISIS DE REDES SEMÁNTICAS SOBRE TEXTO LIBRE (NLP)

**Hipótesis central:** Los textos libres (EXP_POS, EXP_NEG, PROP_MOT) contienen información estructural sobre el absentismo que los ítems Likert no capturan.

**Metodología:**
- Aplicar topic modeling (LDA, BERTopic) a las tres variables de texto libre en catalán
- Usar embeddings multilingües (mBERT, XLM-RoBERTa) para análisis de sentimiento y similitud semántica
- Construir redes semánticas: nodos = conceptos frecuentes, aristas = co-ocurrencia o similitud
- Relacionar los temas emergentes con P_ASSIST mediante regresión con variables dummy de topic

**Valor añadido:** Captura dimensiones del absentismo que no se preguntaron explícitamente en el cuestionario, con validación cruzada cuantitativa.

**Software:** Python (`gensim`, `BERTopic`, `spaCy`, `transformers`)

---

### 4.3 MODELO DE ECUACIONES ESTRUCTURALES (SEM) CON VARIABLE LATENTE DE "VALOR PERCIBIDO DE LA CLASE"

**Hipótesis central:** Detrás de múltiples motivos (M_UTIL, M_AVORR, M_PASSIU, M_TEOR, M_PROF) existe una variable latente común: el **valor percibido de asistir a clase**. Esta variable latente media entre características del estudiante y la asistencia efectiva.

**Metodología:**
- Especificar un SEM con:
  - Variables exógenas: DEDIC, DESPL, EDAT, GRAU, CURS
  - Variable latente endógena 1: *Valor pedagógico percibido* (ítems M_UTIL, M_AVORR, M_PASSIU, M_TEOR, M_PROF)
  - Variable latente endógena 2: *Engagement digital* (IA_SUBST, IA_ATENC, M_CV, M_AUTON)
  - Variable resultado: P_ASSIST
- Testear invarianza de medida entre géneros y entre grados

**Valor añadido:** Descompone los efectos directos e indirectos, algo imposible con regresión estándar.

**Software:** R (`lavaan`, `semTools`), Python (`semopy`)

---

### 4.4 MACHINE LEARNING INTERPRETABLE: SHAP + GRADIENT BOOSTING

**Hipótesis central:** ¿Qué variables predicen mejor la asistencia real (P_ASSIST), y cuál es la contribución marginal de cada una, incluyendo interacciones no lineales?

**Metodología:**
- Variable dependiente: P_ASSIST (continua) o binarizada en "alta asistencia" (>75%) vs. "baja asistencia"
- Features: todas las variables del dataset (motivos, estrategias, IA, personales, académicas)
- Modelos: XGBoost o LightGBM para capturar interacciones no lineales
- Interpretación: SHAP values (SHapley Additive exPlanations) para cada predictor
- Identificar pares de variables con interacción fuerte (p.ej. M_CV × IA_SUBST)

**Valor añadido:** Supera las limitaciones de la regresión lineal/logística, es interpretable y produce rankings de importancia de variables robustos.

**Software:** Python (`xgboost`, `shap`, `sklearn`)

---

### 4.5 ANÁLISIS DE MODERACIÓN: EL ROL DE LA IA COMO VARIABLE DISRUPTORA

**Hipótesis central:** El efecto de los motivos clásicos de absentismo (distancia, trabajo, aburrimiento) sobre la asistencia está **moderado por el nivel de adopción de IA**. Los estudiantes con alta adopción de IA tienen patrones de absentismo estructuralmente distintos.

**Metodología:**
- Regresión con términos de interacción: M_CV × IA_SUBST, M_AUTON × IA_COMPR, etc.
- O bien: segmentar la muestra en "high-IA adopters" (IA_HABIT ≥ 4) vs. "low adopters" y correr modelos separados
- Comparar coeficientes con test de Chow o multigroup SEM

**Valor añadido:** Es uno de los primeros estudios en operacionalizar empíricamente la IA como moderador del absentismo, tema absolutamente inédito en la literatura pre-2023.

---

### 4.6 ANÁLISIS DE CORRESPONDENCIAS MÚLTIPLES (ACM) + CLUSTERING

**Hipótesis central:** Existen combinaciones específicas de características del estudiante y motivos de absentismo que forman constelaciones coherentes, visualizables en espacio reducido.

**Metodología:**
- ACM sobre variables categóricas: GRAU, CURS, NOTA, T_AVAL, DEDIC, GENERE + motivos dicotomizados
- Proyectar individuos en espacio bidimensional
- Aplicar clustering jerárquico (Ward) o k-means sobre coordenadas factoriales
- Interpretar grupos y validar con ANOVA sobre P_ASSIST

**Valor añadido:** Técnica exploratoria potente para datos categóricos mixtos, muy adecuada para este dataset. Produce visualizaciones altamente comunicables.

**Software:** R (`FactoMineR`, `factoextra`), Python (`prince`)

---

### 4.7 MODELO DE DATOS DE PANEL SIMULADO: EFECTOS DEL CURSO Y EL GRADO

**Hipótesis central:** El absentismo tiene una **trayectoria evolutiva** a lo largo de los cursos académicos, no es estático. La progresión en la carrera modifica los motivos y la intensidad del absentismo.

**Metodología:**
- Aunque el dataset es transversal, simular pseudo-panel agrupando por CURS y GRAU
- Alternativa: regresión con efectos fijos de grado (GRAU como variable de agrupación) usando errores estándar clusterizados
- Análisis de varianza del absentismo por curso (ANOVA de un factor) con test post-hoc (Tukey)
- Interacción CURS × NOTA para detectar si el efecto del rendimiento sobre la asistencia cambia con los años

**Valor añadido:** Introduce una dimensión longitudinal implícita que los estudios de corte transversal ignoran.

---

### 4.8 ECONOMETRÍA DE LA DECISIÓN DE ASISTIR: MODELO TOBIT Y DOBLE HURDLE

**Hipótesis central:** P_ASSIST es una variable censurada en 0 y 100. Una fracción de estudiantes tiene asistencia cero por razones estructurales (trabajan a tiempo completo), no por preferencia marginal. Esto viola los supuestos de MCO.

**Metodología:**
- Modelo Tobit (censura en 0 y/o 100) para modelar P_ASSIST
- Modelo Double-Hurdle (Cragg): primera ecuación modela la decisión binaria de asistir en absoluto (≥1%); segunda ecuación modela el nivel de asistencia condicional a asistir
- Variables instrumentales potenciales: DESPL (distancia como instrumento exógeno de los costes de asistir)

**Valor añadido:** Rigor econométrico que distingue entre el margen extensivo (asistir o no) y el margen intensivo (cuánto asistir), distinción conceptualmente crucial.

**Software:** R (`AER`, `censReg`), Stata (`tobit`, `craggit`)

---

### 4.9 ANÁLISIS DE COMPONENTES PRINCIPALES SOBRE ESCALAS LIKERT

**Hipótesis central:** Los 15 motivos de no asistencia y las 13 estrategias de asistencia no son independientes entre sí; se estructuran en un número reducido de **dimensiones latentes** interpretables.

**Metodología:**
- PCA policórico (adecuado para Likert, no PCA estándar) sobre M_* y E_* por separado
- Determinar número de factores con criterio de Kaiser, Scree plot y análisis paralelo
- Rotación oblicua (Oblimin) para permitir correlación entre factores
- Nombrar los factores y usar las puntuaciones factoriales como variables en modelos posteriores

**Hipótesis sobre factores emergentes en M_*:**
- Factor 1: *Costes de oportunidad externos* (M_TREB, M_FAM, M_DIST)
- Factor 2: *Sustitución digital* (M_CV, M_AUTON)
- Factor 3: *Insatisfacción pedagógica* (M_AVORR, M_PASSIU, M_TEOR, M_PROF)
- Factor 4: *Racionalidad estratégica* (M_EXAM, M_UTIL, M_REPET)
- Factor 5: *Influencia social* (M_AMICS)

**Software:** R (`psych::fa`, `polychoric`), Python (`factor_analyzer`)

---

### 4.10 ANÁLISIS DE MODERACIÓN-MEDIACIÓN (PROCESS MACRO / MEDMOD)

**Hipótesis central:** El efecto del tipo de evaluación (T_AVAL) sobre la asistencia (P_ASSIST) está **mediado** por la utilidad percibida de la clase (M_UTIL) y **moderado** por el grado de adopción de IA (IA_SUBST).

**Modelo conceptual:**
```
T_AVAL → [mediado por M_UTIL] → P_ASSIST
                ↑ moderado por IA_SUBST
```

**Metodología:**
- Bootstrapping para estimar intervalos de confianza de efectos indirectos (Hayes PROCESS, modelo 14 o 58)
- Floodlight analysis para determinar el rango de IA_SUBST donde la mediación es significativa
- Johnson-Neyman regions of significance

**Valor añadido:** Conecta el diseño institucional (tipo de evaluación) con la tecnología emergente (IA) a través de la percepción del estudiante.

---

### 4.11 COMPARACIÓN ENTRE GRADOS: REGRESIÓN CUANTÍLICA

**Hipótesis central:** Los predictores del absentismo no tienen el mismo efecto en estudiantes con asistencia baja, media o alta. El efecto del trabajo remunerado puede ser crítico solo en los percentiles bajos de asistencia.

**Metodología:**
- Regresión cuantílica sobre P_ASSIST en los percentiles 10, 25, 50, 75 y 90
- Comparar cómo cambia el coeficiente de M_TREB, IA_SUBST o DESPL a lo largo de la distribución de asistencia
- Test de Koenker-Bassett para diferencias entre cuantiles

**Valor añadido:** Revela heterogeneidad de efectos que una regresión de mínimos cuadrados ordinarios oculta completamente.

**Software:** R (`quantreg`), Python (`statsmodels.regression.quantile_regression`)

---

### 4.12 ÍNDICE SINTÉTICO DE RIESGO DE ABSENTISMO (IRA)

**Hipótesis central:** Es posible construir un **índice predictivo compuesto** de riesgo de absentismo que combine información de distintas dimensiones del cuestionario.

**Metodología:**
- Usar los coeficientes del modelo de mejor ajuste (p.ej. Tobit o XGBoost) para ponderar las variables
- Construir IRA = f(M_TREB, M_CV, IA_SUBST, DEDIC, DESPL, M_UTIL, ...)
- Validar el índice con P_ASSIST mediante correlación de Spearman y AUC-ROC (si se binariza)
- Segmentar estudiantes en cuartiles del IRA y analizar sus perfiles

**Valor añadido:** Produce un instrumento de diagnóstico aplicable en tiempo real por parte de los servicios de orientación universitaria (Early Warning System).

---

## 5. PREGUNTAS DE INVESTIGACIÓN NOVEDOSAS

1. **¿Es la IA generativa un sustituto funcional de la clase presencial o un complemento?**
   Operacionalizable con: IA_SUBST como mediador entre M_CV/M_AUTON y P_ASSIST.

2. **¿Existen umbrales no lineales en la distancia de desplazamiento?**
   Hay un punto crítico a partir del cual el coste de desplazamiento colapsa la asistencia. Detectar con splines o regresión segmentada sobre DESPL.

3. **¿El absentismo estratégico (M_EXAM, M_UTIL, M_REPET) predice mejor el rendimiento académico que el absentismo estructural (M_TREB, M_DIST)?**
   Requiere cruzar P_ASSIST con NOTA segmentando por tipo de motivo.

4. **¿La influencia de los pares (M_AMICS) actúa como contagio social? ¿Existe un efecto de masa crítica?**
   Modelizable con modelos de umbral (Granovetter) aplicados a la escala M_AMICS × tamaño percibido del grupo.

5. **¿Qué tipo de actividad pedagógica tiene el mayor retorno marginal sobre la asistencia?**
   Análisis de elasticidades parciales de los ítems E_* sobre P_ASSIST, controlando por perfil del estudiante.

6. **¿Las mujeres y los hombres tienen patrones de absentismo con distinta arquitectura causal?**
   Multigroup SEM o análisis de invarianza de medida por GENERE.

---

## 6. CONSIDERACIONES METODOLÓGICAS TRANSVERSALES

### Endogeneidad
- P_ASSIST y NOTA son potencialmente endógenas (causalidad inversa). Considerar Variables Instrumentales (IV) o diseños quasi-experimentales.
- DESPL es un instrumento candidato plausible para los costes de asistir.

### Sesgo de selección
- Los respondentes a la encuesta pueden ser sistemáticamente distintos de los no respondentes (sesgo de autoselección). Considerar corrección tipo Heckman si hay datos sobre la tasa de respuesta por grupo.

### Escala de medida
- Las escalas Likert de 6 puntos (sin punto neutro) son semiordenales. Usar métodos policóricos para correlaciones y PCA, y evitar tratar los ítems como métricos en regresiones sin justificación.

### Tamaño muestral mínimo recomendado
- Para SEM: n ≥ 200 (regla general), idealmente 300+
- Para LPA con 5 clases: n ≥ 250
- Para BERTopic con resultados estables: n ≥ 150 textos por variable

---

## 7. ESTRUCTURA DE ANÁLISIS RECOMENDADA (PIPELINE)

```
1. Depuración y limpieza de datos
   └── Detección de outliers en P_ASSIST, EDAT, N_ASSIG
   └── Tratamiento de missings (imputación múltiple si < 20%)
   └── Recodificación de variables compuestas

2. Análisis exploratorio
   └── Distribución de P_ASSIST por GRAU, CURS, DEDIC, GENERE
   └── ACM + Clustering (enfoque 4.6)
   └── PCA policórico sobre M_* y E_* (enfoque 4.9)

3. Análisis confirmatorio
   └── SEM con variable latente "Valor percibido" (enfoque 4.3)
   └── Modelo Tobit / Double-Hurdle (enfoque 4.8)
   └── Regresión cuantílica (enfoque 4.11)

4. Machine learning
   └── XGBoost + SHAP (enfoque 4.4)
   └── LPA / Latent Class Analysis (enfoque 4.1)

5. Análisis de texto
   └── BERTopic sobre EXP_POS, EXP_NEG, PROP_MOT (enfoque 4.2)

6. Construcción del IRA
   └── Índice sintético de riesgo (enfoque 4.12)

7. Comunicación de resultados
   └── Dashboards interactivos por perfil de estudiante
   └── Recomendaciones pedagógicas diferenciadas
```

---

## 8. REFERENCIAS METODOLÓGICAS CLAVE

- **LPA/LCA:** Masyn, K. E. (2013). *Latent class analysis and finite mixture modeling*. In Little, T. D. (Ed.), Oxford handbook of quantitative methods.
- **SEM:** Hair, J. F. et al. (2019). *Multivariate Data Analysis* (8th ed.). Cengage.
- **SHAP:** Lundberg, S. M. & Lee, S.-I. (2017). A unified approach to interpreting model predictions. *NeurIPS*.
- **Tobit/Double Hurdle:** Cragg, J. G. (1971). Some statistical models for limited dependent variables. *Econometrica*, 39(5), 829–844.
- **Regresión cuantílica:** Koenker, R. & Bassett, G. (1978). Regression quantiles. *Econometrica*, 46(1), 33–50.
- **BERTopic:** Grootendorst, M. (2022). BERTopic: Neural topic modeling with a class-based TF-IDF procedure. *arXiv:2203.05794*.
- **ACM:** Greenacre, M. & Blasius, J. (2006). *Multiple Correspondence Analysis and Related Methods*. Chapman & Hall.

---

*Documento generado para uso interno del proyecto de investigación sobre absentismo universitario en la FEE (Universitat de Barcelona). Última actualización: marzo 2026.*
