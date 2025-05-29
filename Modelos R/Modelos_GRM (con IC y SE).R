# 1. Leer el archivo Excel
data <- read_excel("orange_plain.xlsx")

# 2. Separar columna CODE
ids <- data$CODE

# 3. Definir función para ajustar GRM por subconjunto de ítems con errores estándar y CIs
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
  
  # Obtener puntuaciones latentes con errores estándar
  fscores_info <- fscores(modelo, full.scores.SE = TRUE)
  theta_vals <- fscores_info[,1]
  se_vals <- fscores_info[,2]
  
  # Calcular intervalos de confianza del 95%
  lower_95 <- theta_vals - 1.96 * se_vals
  upper_95 <- theta_vals + 1.96 * se_vals
  
  # Extraer discriminaciones
  coef_items <- coef(modelo, simplify = TRUE)$items
  discriminaciones <- sort(coef_items[, "a1"], decreasing = TRUE)
  
  # Mostrar resultados
  cat("\n", nombre_modelo, "\n")
  cat(sprintf("LogLik: %.2f\tAIC: %.2f\tBIC: %.2f\n", as.numeric(logLik_val), AIC_val, BIC_val))
  print(discriminaciones)
  
  # Retornar resultados
  data.frame(
    CODE = ids,
    theta = theta_vals,
    SE = se_vals,
    IC95_Lower = lower_95,
    IC95_Upper = upper_95
  )
}

# 4. Ajustar modelos por cada dimensión
resultado_sensing <- ajustar_grm(data, "Sensing", ids, "Modelo GRM - Sensing")
resultado_seizing <- ajustar_grm(data, "Seizing", ids, "Modelo GRM - Seizing")
resultado_transformation <- ajustar_grm(data, "Transformation", ids, "Modelo GRM - Transformation")

# 5. Unir resultados y exportar puntuaciones originales
resultados <- resultado_sensing %>%
  rename_with(~ paste0("Sensing_", .), -CODE) %>%
  left_join(resultado_seizing %>% rename_with(~ paste0("Seizing_", .), -CODE), by = "CODE") %>%
  left_join(resultado_transformation %>% rename_with(~ paste0("Transformation_", .), -CODE), by = "CODE")

# Exportar archivo con puntuaciones originales y errores estándar
write_xlsx(resultados, "indices_grm_dimensiones_con_SE_IC.xlsx")

# 6. Estandarizar las puntuaciones latentes (solo theta)
resultados_estandarizados <- resultados %>%
  mutate(
    Sensing_Z = scale(Sensing_theta),
    Seizing_Z = scale(Seizing_theta),
    Transformation_Z = scale(Transformation_theta)
  )

# 7. Exportar archivo con puntuaciones estandarizadas
#write_xlsx(resultados_estandarizados, "indices_grm_dimensiones_estandarizados.xlsx")
