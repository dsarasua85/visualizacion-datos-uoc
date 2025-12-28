# Se importan librerías
library(data.table)

# Se define la ruta del fichero y se hace la lectura
in_file <- "data_processed/movies_tmdb_imdb.csv"
dt <- fread(in_file)

# Se repiten campos entre datasets de IMDB y TMDB tras hacer el merge. Se eligen campos
# "finales" (prioridad: TMDB para título/año; si no existe o está vacío, el de IMDB)
dt[, title := fifelse(!is.na(title.x) & title.x != "", title.x, title.y)]
dt[, year  := fifelse(!is.na(year.x), as.integer(year.x), as.integer(year.y))]

# Lo mismo para runtime (duración) y género principal, se prioriza TMDB y si falta usa IMDB.
# En el género, se cogen los géneros (de TMDB o IMDB), se separan por ‘ | ’ y se queda solo con el primero
dt[, runtime := fifelse(!is.na(runtime.x), as.integer(runtime.x), as.integer(runtime.y))]
dt[, genre_main := tstrsplit(fifelse(!is.na(genres) & genres != "", genres, genres_imdb), " \\| ", fixed = FALSE)[[1]]]

# Se hace una selección final de columnas tras el merge de TMDB con IMDB
out <- dt[, .(
  imdb_id,
  tmdb_id,
  title,
  year,
  decade,
  release_date,
  original_language,
  runtime,
  genre_main,
  genres_tmdb = genres,
  genres_imdb,
  budget,
  revenue,
  profit,
  profitability,
  popularity,
  tmdb_vote_average,
  tmdb_vote_count,
  imdb_rating,
  imdb_votes,
  imdb_weighted_rating,
  production_companies
)]

# Se exportan los resultados a fichero limpio
fwrite(out, "data_processed/movies_tmdb_imdb_clean.csv")
cat("Saved: data_processed/movies_tmdb_imdb_clean.csv\n")
cat("Rows:", nrow(out), "\n")