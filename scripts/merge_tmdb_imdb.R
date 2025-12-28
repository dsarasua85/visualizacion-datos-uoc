# Se importan librerías
library(data.table)

# Se definen las rutas de los ficheros
tmdb_file <- "data_tmdb/tmdb_details_1980_2024_minVotes50.csv"
imdb_file <- "data_processed/imdb_clean.csv"

# Se cargan los datasets
tmdb <- fread(tmdb_file)
imdb <- fread(imdb_file)

# Se verifica que el campo imdb_id es string en ambos datasets
tmdb[, imdb_id := as.character(imdb_id)]
imdb[, imdb_id := as.character(imdb_id)]

# Se hace merge a través de imdb_id
master <- merge(tmdb, imdb, by = "imdb_id", all.x = TRUE)

# Se calculan unas nuevas métricas
# profit: ganancia (o pérdida) de la película
# profitability: rentabilidad relativa: ganancia / presupuesto
master[, profit := revenue - budget]
master[, profitability := fifelse(budget > 0, profit / budget, NA_real_)]

# Se calcula la década de la que se trata. Se cambia el formato de year, para poder
# operar con él
master[, year := as.integer(master[["year"]])]
master[, decade := (year %/% 10) * 10]

# Se calcula otra métrica nueva: Weighted rating
# Rating ponderado por número de votos
# WR = (v/(v+m))*R + (m/(v+m))*C
# R = rating de la película
# v = número de votos
# C = rating medio del conjunto
# m = número de votos necesarios para que una nota sea fiable (se considera 1000)

C <- master[!is.na(imdb_rating), mean(imdb_rating)]
m <- 1000
master[, imdb_weighted_rating := fifelse(
  !is.na(imdb_rating) & !is.na(imdb_votes),
  (imdb_votes/(imdb_votes + m))*imdb_rating + (m/(imdb_votes + m))*C,
  NA_real_
)]

# Se exporta el fichero resultante
out_file <- "data_processed/movies_tmdb_imdb.csv"
fwrite(master, out_file)

# Se imprime diagnóstico
cat("Rows master:", nrow(master), "\n")
cat("IMDB match rate:", round(mean(!is.na(master$imdb_rating))*100, 2), "% (con rating)\n")
cat("Budget>0:", round(mean(master$budget > 0)*100, 2), "%\n")
cat("Revenue>0:", round(mean(master$revenue > 0)*100, 2), "%\n")
cat("Saved:", out_file, "\n")
