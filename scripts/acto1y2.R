##################################################################
## ACTO 1: ¿Las mejores películas son las que más dinero ganan? ##
## ACTO 2: ¿Ha cambiado el éxito en el cine con el tiempo?      ##
##################################################################

# Se importan librerías
library(data.table)
library(dplyr)

# Se definen las rutas de los ficheros
input_file  <- "data_processed/movies_tmdb_imdb_clean.csv"
output_file <- "data_processed/acto1y2.csv"

# Se lee el dataset
movies <- fread(input_file)

# Se genera dataframe de películas con las columnas necesarias para responder a la
# pregunta del acto 1. Además, se hace un filtrado previo para no tener en cuenta
# casos que no interesan
df <- movies %>%
  mutate(
    year = as.integer(year),
    decade = ifelse(!is.na(year), paste0(floor(year/10)*10, "s"), NA),
    budget = as.numeric(budget),
    revenue = as.numeric(revenue),
    log_budget  = log10(budget),
    log_revenue = log10(revenue),
    tag = case_when(
      imdb_weighted_rating >= 7.5 & profitability >= 3  ~ "Alta calidad + muy rentable",
      imdb_weighted_rating >= 7.5 & profitability <  1  ~ "Alta calidad + poco rentable",
      imdb_weighted_rating <  6.0 & profitability >= 3  ~ "Baja calidad + muy rentable",
      imdb_weighted_rating <  6.0 & profitability <  1  ~ "Baja calidad + poco rentable",
      TRUE ~ "Intermedia"
    )
  ) %>%
  filter(
    !is.na(title),
    !is.na(imdb_weighted_rating),
    !is.na(budget), !is.na(revenue),
    budget > 1e6,
    revenue > 1e6,
    imdb_weighted_rating >= 1, imdb_weighted_rating <= 10
  ) %>%
  select(
    title, year, decade, genre_main,
    imdb_weighted_rating,
    budget, revenue, profit, profitability,
    log_budget, log_revenue,
    tag
  )

# Se observan los valores de profitability para una posterior mejor representación.
# Se ve que el 99% de los registros tienen un profitability <= 22.02, habiendo algún
# caso incluso de 93.54. Este valor es claramente un outlier que puede distorsionar
# los datos y el gráfico. Conviene filtrar, se fija un valor de 25
summary(df1$profitability)
quantile(df1$profitability, probs = c(.9, .95, .99), na.rm = TRUE)
nrow(df)

# Se filtra por profitability
df <- df %>% filter(profitability <= 25)

# Se exportan los datos a csv para poder ser tratado en Flourish
write.csv(df, output_file, row.names = FALSE)
cat("Exportado:", nrow(df), "filas a", output_file, "\n")