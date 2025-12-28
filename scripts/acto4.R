########################################################################
## ACTO 4: ¿Las películas más populares son las que más dinero ganan? ##
########################################################################

# Se importan librerías
library(data.table)
library(dplyr)

# Se definen las rutas de los ficheros
input_file  <- "data_processed/movies_tmdb_imdb_clean.csv"
output_file <- "data_processed/acto4.csv"

# Se lee el dataset
movies <- fread(input_file)

# Se seleccionan las columnas deseadas y se hace un filtrado mínimo, incluyendo
# la columna de popularidad
df <- movies %>%
  mutate(
    tag = case_when(
      imdb_weighted_rating >= 7.5 & profitability >= 3  ~ "Alta calidad + muy rentable",
      imdb_weighted_rating >= 7.5 & profitability <  1  ~ "Alta calidad + poco rentable",
      imdb_weighted_rating <  6.0 & profitability >= 3  ~ "Baja calidad + muy rentable",
      imdb_weighted_rating <  6.0 & profitability <  1  ~ "Baja calidad + poco rentable",
      TRUE ~ "Intermedia"
    )
  ) %>%
  filter(
    !is.na(popularity),
    !is.na(budget), !is.na(revenue),
    budget > 1e6,
    revenue > 1e6
  ) %>%
  select(
    title,
    year,
    decade,
    genre_main,
    imdb_weighted_rating,
    popularity,
    tmdb_vote_count,
    budget,
    revenue,
    profit,
    profitability,
    tag
  )

# Se exportan los datos a csv para poder ser tratado en Flourish
write.csv(df, output_file, row.names = FALSE)
