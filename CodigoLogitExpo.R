# ============================================================
# ANÁLISIS ESTADÍSTICO I - RdA2 C3
# Modelos Logit y Probit: Factores asociados a la morosidad
# Base: Clientes1.xlsx
# ============================================================

# ------------------------------------------------------------
# 1. PAQUETES
# ------------------------------------------------------------
# Instalar solo la primera vez 
install.packages("pROC")
install.packages("margins")
install.packages("caret")
install.packages("pscl")
install.packages("stargazer")
install.packages("car")

library(readxl)
library(pROC)
library(margins)
library(caret)
library(pscl)
library(stargazer)
library(car)  # para VIF

# ------------------------------------------------------------
# 2. CARGA Y LIMPIEZA DE DATOS
# ------------------------------------------------------------
clientes <- Clientes1

# Estandarizar nombres de columnas (evita problemas con espacios)
names(clientes) <- make.names(names(clientes), unique = TRUE)

# Vista previa
head(clientes)
str(clientes)

# Verificar nombres de columnas
names(clientes)

# ------------------------------------------------------------
# 3. VARIABLE DEPENDIENTE
# ------------------------------------------------------------
# Evento de interés: que el cliente sea moroso (Moroso = 1)

table(clientes$Moroso)
prop.table(table(clientes$Moroso))

# Recodificar como binaria: 1 = moroso, 0 = no moroso
clientes$Moroso_bin <- ifelse(clientes$Moroso == "Si", 1, 0)

# ------------------------------------------------------------
# 4. FILTRO: SOLO CLIENTES CON PRÉSTAMOS ACTIVOS
# ------------------------------------------------------------
# Decisión metodológica: los clientes sin préstamos (Numero_prestamos = 0)
# tienen morosidad = 0 por construcción. No pueden ser morosos sin deuda.
# Incluirlos generaría separación perfecta y distorsionaría el modelo.
# Se trabaja únicamente con los 248 clientes que tienen al menos un préstamo.

clientes_filtro <- subset(clientes, Numero_prestamos > 0)

cat("Observaciones totales:", nrow(clientes), "\n")
cat("Observaciones con préstamos:", nrow(clientes_filtro), "\n")
cat("Morosos:", sum(clientes_filtro$Moroso_bin), "\n")
cat("No morosos:", sum(clientes_filtro$Moroso_bin == 0), "\n")
cat("Proporción morosos:", mean(clientes_filtro$Moroso_bin), "\n")

# ------------------------------------------------------------
# 5. PREPARACIÓN DE VARIABLES
# ------------------------------------------------------------
# Variable cualitativa como factor
clientes_filtro$nivel_educativo <- factor(clientes_filtro$nivel_educativo,
                                          levels = c("Secundaria", "Tecnológico",
                                                     "Universitario", "Posgrado"))
# La categoría de referencia es "Secundaria" (mayor tasa de morosidad observada)

# Variable construida: carga financiera
# Mide qué proporción del ingreso mensual se destina al pago de cuotas.
# Es el indicador más directo de presión financiera mensual.
# Reemplaza a Monto.total, Saldo_total y Cuota_Total por separado,
# ya que en la muestra filtrada estas variables tienen correlación ~0 con morosidad,
# mientras que el ratio captura mejor la relación con el incumplimiento.
clientes_filtro$carga_financiera <- clientes_filtro$Cuota_Total / clientes_filtro$ingreso_mensual

# Escalar ingreso a miles para que el coeficiente sea más legible
clientes_filtro$ingreso_miles <- clientes_filtro$ingreso_mensual / 1000

# Vista previa de variables a utilizar
summary(clientes_filtro[, c("ingreso_miles", "score_crediticio", "carga_financiera",
                            "Promedio_tasa_interes", "antiguedad_laboral",
                            "nivel_educativo", "Moroso_bin")])

# ------------------------------------------------------------
# 6. ANÁLISIS DESCRIPTIVO
# ------------------------------------------------------------

# Distribución de la variable dependiente
barplot(table(clientes_filtro$Moroso),
        main = "Distribución de clientes morosos (con préstamos)",
        xlab = "Moroso",
        ylab = "Frecuencia",
        col = c("steelblue", "tomato"))

# Boxplot: score crediticio según morosidad
boxplot(score_crediticio ~ Moroso,
        data = clientes_filtro,
        main = "Score crediticio según morosidad",
        xlab = "Moroso",
        ylab = "Score crediticio",
        col = "steelblue")

# Boxplot: ingreso mensual según morosidad
boxplot(ingreso_mensual ~ Moroso,
        data = clientes_filtro,
        main = "Ingreso mensual según morosidad",
        xlab = "Moroso",
        ylab = "Ingreso mensual (USD)",
        col = "steelblue")

# Boxplot: carga financiera según morosidad
boxplot(carga_financiera ~ Moroso,
        data = clientes_filtro,
        main = "Carga financiera según morosidad",
        xlab = "Moroso",
        ylab = "Cuota / Ingreso",
        col = "steelblue")

# Boxplot: tasa de interés según morosidad
boxplot(Promedio_tasa_interes ~ Moroso,
        data = clientes_filtro,
        main = "Tasa de interés promedio según morosidad",
        xlab = "Moroso",
        ylab = "Tasa de interés (%)",
        col = "steelblue")

# Tasa de morosidad por nivel educativo
tapply(clientes_filtro$Moroso_bin, clientes_filtro$nivel_educativo, mean)

# ------------------------------------------------------------
# 7. VERIFICAR MULTICOLINEALIDAD ANTES DE ESTIMAR
# ------------------------------------------------------------
# Se verifica con un modelo lineal auxiliar para obtener el VIF.
# VIF < 5: sin problema; VIF 5-10: moderado; VIF > 10: problema severo.

modelo_vif <- lm(Moroso_bin ~ ingreso_miles + score_crediticio + carga_financiera +
                   Promedio_tasa_interes + antiguedad_laboral + nivel_educativo,
                 data = clientes_filtro)
vif(modelo_vif)

# ------------------------------------------------------------
# 8. MODELO LOGIT
# ------------------------------------------------------------
# Variables:
#   ingreso_miles         : capacidad de pago mensual
#   score_crediticio      : historial y riesgo crediticio histórico
#   carga_financiera      : presión financiera mensual (cuota/ingreso)
#   Promedio_tasa_interes : costo del crédito
#   antiguedad_laboral    : estabilidad del empleo
#   nivel_educativo       : proxy de capital humano y gestión financiera


# ------------------------------------------------------------
# ESPECIFICACIÓN FINAL DEL MODELO LOGIT
# ------------------------------------------------------------
# Se incluyen cuatro variables explicativas seleccionadas tras un proceso
# de depuración progresiva del modelo original.
#
# Variables descartadas en el proceso:
#   - score_crediticio : presentó multicolinealidad con ingreso_miles
#                        (r = 0.815, VIF = 5.02). Su información quedaba
#                        absorbida por el ingreso, perdiendo significancia
#                        estadística al estimarse conjuntamente.
#   - nivel_educativo  : en la muestra filtrada (clientes con préstamos),
#                        las diferencias en tasa de morosidad entre niveles
#                        se reducen considerablemente respecto a la muestra
#                        completa, resultando en coeficientes no significativos.
#
# Variables incluidas y justificación:
#   - ingreso_miles         : capacidad de pago mensual del cliente.
#                             A mayor ingreso, menor probabilidad de mora.
#   - carga_financiera      : proporción del ingreso comprometida en cuotas
#                             (Cuota_Total / ingreso_mensual). Indicador directo
#                             de presión financiera mensual.
#   - Promedio_tasa_interes : costo promedio del crédito. Tasas más altas
#                             elevan la carga real de la deuda.
#   - antiguedad_laboral    : proxy de estabilidad laboral e ingresos futuros.
#                             Aunque no alcanza significancia estadística al 5%,
#                             se mantiene por su respaldo teórico: mayor antigüedad
#                             implica mayor estabilidad y menor riesgo de mora.

modelo_logit <- glm(Moroso_bin ~ ingreso_miles + carga_financiera +
                      Promedio_tasa_interes + antiguedad_laboral,
                    data = clientes_filtro,
                    family = binomial(link = "logit"))

summary(modelo_logit)

# Interpretación de coeficientes:
# Coeficiente positivo → aumenta el log-odds de morosidad
# Coeficiente negativo → disminuye el log-odds de morosidad

# ------------------------------------------------------------
# 9. ODDS RATIOS - MODELO LOGIT
# ------------------------------------------------------------
# OR = exp(coeficiente)
# OR > 1: la variable aumenta las chances de ser moroso
# OR < 1: la variable reduce las chances de ser moroso

odds_ratios <- exp(coef(modelo_logit))
IC_odds <- exp(confint(modelo_logit))

tabla_or <- data.frame(
  Coeficiente = round(coef(modelo_logit), 4),
  Odds_Ratio  = round(odds_ratios, 4),
  IC_2.5      = round(IC_odds[, 1], 4),
  IC_97.5     = round(IC_odds[, 2], 4)
)
print(tabla_or)

# Efectos marginales promedio (equivalente a los coeficientes del modelo lineal de probabilidad)
# Indica cuánto cambia la probabilidad de mora ante un cambio unitario en cada variable
efectos_logit <- margins(modelo_logit)
summary(efectos_logit)
margins(modelo_logit)

# ------------------------------------------------------------
# 10. MODELO PROBIT
# ------------------------------------------------------------
#Se sacaron del modelo las variables nivel_educativo y score_crediticio por el mismo motivo
#que se sacaron del modelo Logit

modelo_probit <- glm(Moroso_bin ~ ingreso_miles + carga_financiera +
                       Promedio_tasa_interes + antiguedad_laboral,
                     data = clientes_filtro,
                     family = binomial(link = "probit"))

summary(modelo_probit)

# Interpretación de coeficientes Probit:
# El signo se interpreta igual que en Logit.
# Los valores son el efecto sobre el índice latente normal estándar.
# No se calculan odds ratios (no aplica en Probit).

# Efectos marginales Probit
efectos_probit <- margins(modelo_probit)
summary(efectos_probit)

# ------------------------------------------------------------
# 11. COMPARACIÓN LOGIT VS PROBIT
# ------------------------------------------------------------

# AIC y BIC (menor es mejor)
AIC(modelo_logit, modelo_probit)
BIC(modelo_logit, modelo_probit)

# Pseudo R2 de McFadden
pR2(modelo_logit)
pR2(modelo_probit)

# Tabla comparativa de coeficientes
stargazer(modelo_logit, modelo_probit,
          type = "text",
          title = "Comparación Logit vs Probit - Variable dependiente: Morosidad",
          column.labels = c("Logit", "Probit"),
          dep.var.labels = "Moroso (1 = Sí)",
          covariate.labels = c("Ingreso mensual (miles)",
                               "Carga financiera",
                               "Tasa de interés promedio",
                               "Antigüedad laboral"))

# Criterio de selección del modelo principal:
# - Si los signos y significancias son consistentes entre ambos → escoger Logit
#   porque permite interpretar Odds Ratios, que son más intuitivos y comunicables.

modelo_principal <- modelo_logit

# ------------------------------------------------------------
# 12. PROBABILIDADES PREDICHAS PARA TRES PERFILES
# ------------------------------------------------------------
# Se calculan para el modelo principal (Logit) y también para Probit

# Perfil 1: cliente de BAJO riesgo
# Ingreso alto, carga financiera baja, tasa baja, empleo estable
perfil_bajo <- data.frame(
  ingreso_miles         = 4.5,
  carga_financiera      = 0.20,
  Promedio_tasa_interes = 12.0,
  antiguedad_laboral    = 15
)

# Perfil 2: cliente PROMEDIO (valores medianos de la muestra filtrada)
perfil_promedio <- data.frame(
  ingreso_miles         = 2.33,
  carga_financiera      = 0.634,
  Promedio_tasa_interes = 16.81,
  antiguedad_laboral    = 9
)

# Perfil 3: cliente de ALTO riesgo
# Ingreso bajo, carga financiera alta, tasa alta, empleo inestable
perfil_alto <- data.frame(
  ingreso_miles         = 1.2,
  carga_financiera      = 1.10,
  Promedio_tasa_interes = 21.5,
  antiguedad_laboral    = 2
)

# Unir perfiles y predecir
perfiles <- rbind(perfil_bajo, perfil_promedio, perfil_alto)
perfiles$tipo_perfil <- c("Bajo riesgo", "Promedio", "Alto riesgo")

perfiles$prob_logit  <- predict(modelo_logit,  newdata = perfiles, type = "response")
perfiles$prob_probit <- predict(modelo_probit, newdata = perfiles, type = "response")

print(perfiles[, c("tipo_perfil", "prob_logit", "prob_probit")])

# Interpretación:
# La probabilidad indica la chance estimada de que el cliente incurra en mora.
# Un perfil de bajo riesgo debería arrojar una probabilidad baja (< 0.30),
# y uno de alto riesgo debería superar el 0.70.

# ------------------------------------------------------------
# 13. MATRIZ DE CONFUSIÓN
# ------------------------------------------------------------
# Punto de corte: se utiliza el umbral óptimo obtenido mediante la curva ROC,
# que maximiza simultáneamente la sensibilidad y la especificidad del modelo.
# El valor óptimo encontrado es 0.5174, que resulta en una especificidad de
# 0.774 y una sensibilidad de 0.678, equilibrando adecuadamente la detección
# de morosos y no morosos.

clientes_filtro$prob_logit <- predict(modelo_logit, type = "response")

corte <- 0.5174
cat("Punto de corte utilizado:", corte, "\n")

clientes_filtro$pred_logit <- ifelse(clientes_filtro$prob_logit >= corte, 1, 0)

# Matriz de confusión con caret
confusionMatrix(factor(clientes_filtro$pred_logit),
                factor(clientes_filtro$Moroso_bin),
                positive = "1")

# Cálculo manual para el informe
VP <- sum(clientes_filtro$Moroso_bin == 1 & clientes_filtro$pred_logit == 1)
VN <- sum(clientes_filtro$Moroso_bin == 0 & clientes_filtro$pred_logit == 0)
FP <- sum(clientes_filtro$Moroso_bin == 0 & clientes_filtro$pred_logit == 1)
FN <- sum(clientes_filtro$Moroso_bin == 1 & clientes_filtro$pred_logit == 0)

accuracy      <- (VP + VN) / (VP + VN + FP + FN)
sensibilidad  <- VP / (VP + FN)
especificidad <- VN / (VN + FP)

cat("Verdaderos Positivos (VP):", VP, "\n")
cat("Verdaderos Negativos (VN):", VN, "\n")
cat("Falsos Positivos (FP):", FP, "\n")
cat("Falsos Negativos (FN):", FN, "\n")
cat("Accuracy:     ", round(accuracy, 4), "\n")
cat("Sensibilidad: ", round(sensibilidad, 4), "\n")
cat("Especificidad:", round(especificidad, 4), "\n")

# Interpretación:
# Accuracy     : % total de clientes clasificados correctamente
# Sensibilidad : % de morosos reales detectados por el modelo (importante para riesgo)
# Especificidad: % de no morosos reales correctamente identificados


# Matriz de confusión para Probit (usando el mismo punto de corte)
clientes_filtro$pred_probit <- ifelse(clientes_filtro$prob_probit >= corte, 1, 0)

VP_p <- sum(clientes_filtro$Moroso_bin == 1 & clientes_filtro$pred_probit == 1)
VN_p <- sum(clientes_filtro$Moroso_bin == 0 & clientes_filtro$pred_probit == 0)
FP_p <- sum(clientes_filtro$Moroso_bin == 0 & clientes_filtro$pred_probit == 1)
FN_p <- sum(clientes_filtro$Moroso_bin == 1 & clientes_filtro$pred_probit == 0)

accuracy_p      <- (VP_p + VN_p) / (VP_p + VN_p + FP_p + FN_p)
sensibilidad_p  <- VP_p / (VP_p + FN_p)
especificidad_p <- VN_p / (VN_p + FP_p)

# ------------------------------------------------------------
# 14. CURVA ROC Y AUC
# ------------------------------------------------------------
# La curva ROC evalúa la capacidad discriminante del modelo
# para separar morosos de no morosos en todos los posibles cortes.
# AUC ~ 0.5: el modelo no discrimina (equivale a adivinar al azar)
# AUC 0.7-0.8: capacidad predictiva moderada
# AUC > 0.8: buena capacidad predictiva

# ROC - Logit
roc_logit <- roc(clientes_filtro$Moroso_bin, clientes_filtro$prob_logit)
cat("AUC Logit:", auc(roc_logit), "\n")

plot(roc_logit,
     main = "Curva ROC - Modelo Logit",
     col = "blue",
     lwd = 2)
legend("bottomright",
       legend = paste("AUC =", round(auc(roc_logit), 4)),
       col = "blue", lwd = 2)

# Punto óptimo de corte según la curva ROC
coords(roc_logit, "best")

# ROC - Probit (para comparar)
clientes_filtro$prob_probit <- predict(modelo_probit, type = "response")
roc_probit <- roc(clientes_filtro$Moroso_bin, clientes_filtro$prob_probit)
cat("AUC Probit:", auc(roc_probit), "\n")

plot(roc_probit,
     main = "Curva ROC - Modelo Probit",
     col = "darkgreen",
     lwd = 2)
legend("bottomright",
       legend = paste("AUC =", round(auc(roc_probit), 4)),
       col = "darkgreen", lwd = 2)

# Comparar ambas curvas en un mismo gráfico
plot(roc_logit,  col = "blue",      lwd = 2, main = "Curva ROC: Logit vs Probit")
plot(roc_probit, col = "darkgreen", lwd = 2, add = TRUE)
legend("bottomright",
       legend = c(paste("Logit  AUC =", round(auc(roc_logit),  4)),
                  paste("Probit AUC =", round(auc(roc_probit), 4))),
       col = c("blue", "darkgreen"), lwd = 2)

# ------------------------------------------------------------
# 15. TABLA RESUMEN DE INDICADORES
# ------------------------------------------------------------
indicadores <- data.frame(
  Modelo        = c("Logit", "Probit"),
  Pseudo_R2     = c(round(pR2(modelo_logit)["McFadden"], 4),
                    round(pR2(modelo_probit)["McFadden"], 4)),
  AIC           = c(round(AIC(modelo_logit), 2), round(AIC(modelo_probit), 2)),
  AUC           = c(round(auc(roc_logit), 4),    round(auc(roc_probit), 4)),
  Accuracy      = c(round(accuracy, 4),      round(accuracy_p, 4)),
  Sensibilidad  = c(round(sensibilidad, 4),  round(sensibilidad_p, 4)),
  Especificidad = c(round(especificidad, 4), round(especificidad_p, 4))
)
print(indicadores)
