#############################################
## ACTO 3: ¿Qué generos son más rentables? ##
#############################################

# Se importan librerías
library(dplyr)
library(data.table)

# Se definen las rutas de los ficheros. Se parte del fichero de los actos 1 y 2
input_file  <- "data_processed/acto1y2.csv"
output_file <- "data_processed/acto3.csv"

# Se lee el dataset
df <- fread(input_file)

# Se calcula la mediana de ROI por género
genre_order <- df %>%
  group_by(genre_main) %>%
  summarise(median_roi = median(profitability, na.rm = TRUE)) %>%
  arrange(desc(median_roi))

# Se exportan los datos a csv para poder ser tratado en Flourish
write.csv(genre_order, output_file, row.names = FALSE)