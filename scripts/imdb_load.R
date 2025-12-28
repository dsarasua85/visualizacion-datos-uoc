# Se importan librerías
library(data.table)

# Se definen las rutas de los ficheros
basics_file  <- "data_raw/title.basics.tsv"
ratings_file <- "data_raw/title.ratings.tsv"

# Se cargan los ficheros de IMDB
basics <- fread(basics_file, sep = "\t", na.strings = "\\N", showProgress = TRUE)
ratings <- fread(ratings_file, sep = "\t", na.strings = "\\N", showProgress = TRUE)

# Sólo se obtienen películas, no series ni otro contenido
basics_movies <- basics[titleType == "movie"]

# Se seleccionan columnas clave
basics_movies <- basics_movies[, .(
  imdb_id = tconst,
  title = primaryTitle,
  original_title = originalTitle,
  year = as.integer(startYear),
  runtime = as.integer(runtimeMinutes),
  genres_imdb = genres
)]

ratings <- ratings[, .(
  imdb_id = tconst,
  imdb_rating = averageRating,
  imdb_votes = numVotes
)]

# Se unen los datasets a partir del identificador imdb_id
imdb <- merge(basics_movies, ratings, by = "imdb_id", all.x = TRUE)

# Se imprime diagnóstico
cat("IMDB movies:", nrow(imdb), "\n")
cat("Con rating:", sum(!is.na(imdb$imdb_rating)), "\n")

# Se guarda dataset limpio
fwrite(imdb, "data_processed/imdb_clean.csv")