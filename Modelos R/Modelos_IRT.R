# Instalar paquetes si aún no los tienes
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("eRm")
# install.packages("mirt")
# install.packages("writexl")

library(readxl)
library(dplyr)
library(eRm)
library(mirt)
library(writexl)

# 1. Leer el archivo Excel
data <- read_excel("bd_sensing_PCA.xlsx")

# 2. Separar la columna CODE
ids <- data$CODE

# 3. Extraer las respuestas (excluyendo CODE) y asegurar que sean numéricas
respuestas <- data %>%
  select(-CODE) %>%
  mutate(across(everything(), as.numeric))

# -----------------------------------------------------------------------------
# 4. MODELO PCM (Rasch politómico con eRm)
# -----------------------------------------------------------------------------

# Ajustar el modelo PCM
modelo_pcm <- PCM(respuestas)

# Calcular puntuaciones latentes (theta)
theta_pcm <- person.parameter(modelo_pcm)
theta_vals_pcm <- theta_pcm$theta.table$`Person Parameter`

# Calcular logLik, AIC y BIC para PCM
logLik_pcm <- logLik(modelo_pcm)
k_pcm <- length(unlist(thresholds(modelo_pcm)))  # número de parámetros
n_pcm <- nrow(respuestas)

AIC_pcm <- -2 * as.numeric(logLik_pcm) + 2 * k_pcm
BIC_pcm <- -2 * as.numeric(logLik_pcm) + log(n_pcm) * k_pcm

# -----------------------------------------------------------------------------
# 5. MODELO GRM (Graded Response con mirt)
# -----------------------------------------------------------------------------

# Ajustar el modelo GRM (1 dimensión latente)
modelo_grm <- mirt(respuestas, 1, itemtype = "graded")

# Calcular logLik, AIC y BIC para GRM
logLik_grm <- logLik(modelo_grm)
AIC_grm <- AIC(modelo_grm)["AIC"]
BIC_grm <- BIC(modelo_grm)["BIC"]

# Obtener puntuaciones latentes GRM
theta_vals_grm <- fscores(modelo_grm)[,1]

# -----------------------------------------------------------------------------
# 6. COMPARACIÓN DE MODELOS
# -----------------------------------------------------------------------------

cat("Comparación de modelos:\n")
cat("Modelo\t\tLogLik\t\tAIC\t\tBIC\n")
cat(sprintf("PCM\t\t%.2f\t%.2f\t%.2f\n", as.numeric(logLik_pcm), AIC_pcm, BIC_pcm))
cat(sprintf("GRM\t\t%.2f\t%.2f\t%.2f\n", as.numeric(logLik_grm), AIC_grm, BIC_grm))

# -----------------------------------------------------------------------------
# 7. Exportar resultados (opcional)
# -----------------------------------------------------------------------------

# Unir puntuaciones a IDs y guardar en Excel
resultados <- data.frame(
  CODE = ids,
  Indice_PCM = theta_vals_pcm,
  Indice_GRM = theta_vals_grm
)

write_xlsx(resultados, "resultados_rasch_vs_grm.xlsx")

# Extraer y ordenar los ítems por discriminación
coef_items <- coef(modelo_grm, simplify = TRUE)$items
discriminaciones <- coef_items[, "a1"]
sorted <- sort(discriminaciones, decreasing = TRUE)
print(sorted)