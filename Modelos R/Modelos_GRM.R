# Instalar paquetes si aún no los tienes
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("mirt")
# install.packages("writexl")

library(readxl)
library(dplyr)
library(mirt)
library(writexl)

# 1. Leer el archivo Excel
data <- read_excel("orange_plain.xlsx")

# 2. Separar columna CODE
ids <- data$CODE

# 3. Definir función para ajustar GRM por subconjunto de ítems
ajustar_grm <- function(data, patron, ids, nombre_modelo){
  # Filtrar columnas que contengan el patrón
  respuestas <- data %>%
    select(contains(patron)) %>%
    mutate(across(everything(), as.numeric))
  
  # Ajustar modelo GRM
  modelo <- mirt(respuestas, 1, itemtype = "graded")
  
  # Calcular métricas
  logLik_val <- logLik(modelo)
  AIC_val <- AIC(modelo)["AIC"]
  BIC_val <- BIC(modelo)["BIC"]
  
  # Obtener puntuaciones latentes
  theta_vals <- fscores(modelo)[,1]
  
  # Extraer discriminaciones
  coef_items <- coef(modelo, simplify = TRUE)$items
  discriminaciones <- sort(coef_items[, "a1"], decreasing = TRUE)
  
  # Mostrar resultados
  cat("\n", nombre_modelo, "\n")
  cat(sprintf("LogLik: %.2f\tAIC: %.2f\tBIC: %.2f\n", as.numeric(logLik_val), AIC_val, BIC_val))
  print(discriminaciones)
  
  # Retornar resultados
  data.frame(CODE = ids, theta = theta_vals)
}

# 4. Ajustar modelos por cada dimensión
resultado_sensing <- ajustar_grm(data, "Sensing", ids, "Modelo GRM - Sensing")
resultado_seizing <- ajustar_grm(data, "Seizing", ids, "Modelo GRM - Seizing")
resultado_transformation <- ajustar_grm(data, "Transformation", ids, "Modelo GRM - Transformation")

# 5. Unir resultados y exportar puntuaciones originales
resultados <- resultado_sensing %>%
  rename(Indice_Sensing = theta) %>%
  left_join(resultado_seizing %>% rename(Indice_Seizing = theta), by = "CODE") %>%
  left_join(resultado_transformation %>% rename(Indice_Transformation = theta), by = "CODE")

# Exportar archivo con puntuaciones originales
write_xlsx(resultados, "indices_grm_dimensiones.xlsx")

# 6. Estandarizar las puntuaciones latentes (media 0, sd 1)
resultados_estandarizados <- resultados %>%
  mutate(
    Indice_Sensing_Z = scale(Indice_Sensing),
    Indice_Seizing_Z = scale(Indice_Seizing),
    Indice_Transformation_Z = scale(Indice_Transformation)
  )

# 7. Exportar archivo con puntuaciones estandarizadas
write_xlsx(resultados_estandarizados, "indices_grm_dimensiones_estandarizados.xlsx")