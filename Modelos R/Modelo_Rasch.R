# Instalar paquetes si es necesario
#install.packages("readxl")
#install.packages("eRm")
#install.packages("dplyr")

library(readxl)
library(eRm)
library(dplyr)
library(writexl)

# 1. Leer el archivo Excel
# Reemplaza la ruta según la ubicación del archivo en tu equipo
data <- read_excel("bd_sensing_PCA.xlsx")

# 2. Separar la columna CODE
ids <- data$CODE

# 3. Extraer las primeras 10 columnas de preguntas (excluyendo CODE)
respuestas <- data %>%
  select(-CODE)

# 4. Asegurarse de que todos los ítems sean numéricos
respuestas <- as.data.frame(lapply(respuestas, as.numeric))

# 5. Ajustar modelo Rasch politómico (PCM) con solo 10 ítems
modelo_pcm <- PCM(respuestas)

# 6. Calcular la variable latente (theta) por individuo
theta <- person.parameter(modelo_pcm)

# Extraer las estimaciones de theta (𝜃) por persona
theta_vals <- theta$theta.table$`Person Parameter`

# 7. Unir theta con la columna CODE
resultado <- data.frame(CODE = ids, Indice_Rasch_10_Items = theta_vals)

# 8. Mostrar o exportar resultado
print(head(resultado))
write_xlsx(resultado, "indice_rasch_todoslositems.xlsx")
