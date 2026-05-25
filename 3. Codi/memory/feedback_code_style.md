---
name: Estilo de código R
description: No alinear el operador <- con espacios extra en asignaciones
type: feedback
---

No usar alineación vertical con espacios extra en asignaciones R.

**Why:** La usuaria prefiere el estilo estándar sin alinear `<-`.

**How to apply:** Siempre asignar con un solo espacio a cada lado de `<-`, sin añadir espacios extra para alinear columnas:

❌ Mal:
```r
taula    <- table(x, y)
test     <- suppressWarnings(...)
n        <- sum(taula)
```

✅ Bien:
```r
taula <- table(x, y)
test <- suppressWarnings(...)
n <- sum(taula)
```
