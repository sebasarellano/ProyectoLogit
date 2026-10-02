# ProyectoLogit
Proyecto con Modelo Logit para identificar morosidad

# 📊 Análisis de Factores Asociados a la Morosidad (Modelos Logit y Probit)

## 📌 Descripción del Proyecto
Este proyecto analiza los factores determinantes del riesgo crediticio y la morosidad de clientes mediante la estimación y comparación de modelos de respuesta binaria (**Logit** y **Probit**). Desarrollado en **RStudio**, el estudio evalúa la capacidad predictiva de las variables financieras y socioeconómicas para identificar la probabilidad de incumplimiento en una cartera de préstamos.

---

## 🛠️ Metodología y Limpieza de Datos
1. **Filtro de Muestra:** Se acotó el análisis a una muestra depurada de **248 observaciones** correspondientes únicamente a clientes con al menos un préstamo activo[cite: 2]. Esto evita sesgos y relaciones mecánicas en el modelo, ya que un cliente sin deudas no puede incurrir en morosidad por construcción[cite: 2].
2. **Construcción de Variables Clave:**
   - **`Moroso_bin`:** Variable dependiente binaria ($1$ = moroso, $0$ = no moroso)[cite: 2].
   - **`carga_financiera`:** Ratio calculado como la relación entre la cuota mensual total y el ingreso mensual, reflejando la presión de deuda real del cliente frente a montos absolutos[cite: 2].
   - **`ingreso_miles`:** Ingreso mensual escalado en miles de USD para facilitar la legibilidad e interpretación de los coeficientes[cite: 2].
3. **Control de Multicolinealidad:** Se evaluó el factor de inflación de la varianza (VIF), descartando variables redundantes como el *score crediticio* (debido a su alta correlación con el ingreso)[cite: 2].

---

## 📈 Resultados Principales
El modelo Logit final incluyó cuatro variables explicativas clave[cite: 2]:

| Variable | Coeficiente Logit | Odds Ratio | Significancia (5%) |
| :--- | :---: | :---: | :---: |
| **Intercepto** | -5.0357 | 0.0065 | Sí[cite: 2] |
| **Ingreso mensual (miles)** | -0.3958 | 0.6731 | Sí[cite: 2] |
| **Carga financiera** | 1.6789 | 5.3597 | Sí[cite: 2] |
| **Tasa de interés promedio** | 0.2784 | 1.3210 | Sí[cite: 2] |
| **Antigüedad laboral** | -0.0100 | 0.9901 | No[cite: 2] |

### 💡 Interpretación Económica y Odds Ratios:
- **Ingreso Mensual:** Un incremento de mil dólares en los ingresos reduce las probabilidades relativas de caer en morosidad en aproximadamente un **32.7%** ($OR = 0.67$)[cite: 2].
- **Carga Financiera:** Constituye el principal factor de riesgo; un aumento de una unidad en la relación cuota-ingreso multiplica por **5.36** las probabilidades relativas de incumplimiento ($OR = 5.36$)[cite: 2].
- **Tasa de Interés:** Cada punto porcentual adicional en la tasa incrementa el riesgo relativo de mora en un **32%** ($OR = 1.32$)[cite: 2].

---

## ⚖️ Comparación Logit vs. Probit
Ambos modelos mostraron un desempeño prácticamente idéntico en términos de ajuste y consistencia de signos[cite: 2]:
- **Pseudo $R^2$ de McFadden:** Logit ($0.2024$) vs Probit ($0.2020$)[cite: 2].
- **Criterio AIC:** Logit ($283.18$) vs Probit ($283.32$)[cite: 2].
Se seleccionó el **modelo Logit** como el enfoque principal debido a su ventaja directa en la interpretación de los *Odds Ratios*[cite: 2].

---

## 🎯 Evaluación Predictiva
Utilizando un punto de corte óptimo derivado de la curva ROC ($0.5174$), el modelo alcanzó los siguientes indicadores de desempeño[cite: 2]:
- **Exactitud (Accuracy):** $72.98\%$[cite: 2]
- **Sensibilidad:** $67.83\%$[cite: 2]
- **Especificidad:** $77.44\%$[cite: 2]
- **Área bajo la curva (AUC):** $0.7895$ (demostrando una capacidad predictiva aceptable y sólida para la clasificación de riesgo)[cite: 2].

---

## 🗂️ Estructura del Repositorio
```text
📂 modelo-logit-morosidad/
├── 📄 README.md                # Documentación del proyecto
├── 📜 script_analisis.R        # Código completo en R (limpieza, modelos y gráficos)
└── 📂 data/                    # (Opcional) Datos anonimizados de clientes
