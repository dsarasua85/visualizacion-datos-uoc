# Se importan librerías
library(data.table)

# Se definen las rutas de los ficheros
master_file <- "data_processed/movies_tmdb_imdb_clean.csv"
kaggle_file <- "data_raw/movies.csv"

# Se hace la lectura de los ficheros
m <- fread(master_file)
k <- fread(kaggle_file)

# Al no disponer de imdb_id, se hace el merge a partir del título y año. Para poder
# hacer bien el merge y que el join sea robusto, se normaliza el título
normalize_title <- function(x) {
  x <- as.character(x)
  
  # Se pasa a ASCII (quita acentos) sin depender de paquetes
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  
  # Se pasa todo a minúsculas
  x <- tolower(x)
  
  # Se quita el contenido que hay entre paréntesis (ediciones, year, etc.)
  x <- gsub("\\(.*?\\)", "", x)
  
  # Se quita la puntuación y símbolos, se dejan letras/números/espacios
  x <- gsub("[^a-z0-9 ]+", " ", x)
  
  # Se reducen los múltiples espacios a uno
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  
  x
}

# Se preparan los campos clave para el merge
m[, year := as.integer(year)]
k[, year := as.integer(year)]
m[, title_key := normalize_title(title)]
k[, name_key  := normalize_title(name)]

# Se renombran las columnas de Kaggle para que no pisen a las de TMDB/IMDB
setnames(k,
         old = c("budget","gross","runtime","rating","votes","score","company","genre"),
         new = c("k_budget","k_gross","k_runtime","k_mpaa_rating","k_votes","k_score","k_company","k_genre"))

# Si hay duplicados, se elige el registro "más fuerte":
# 1) mayor k_votes, 2) mayor k_gross, 3) mayor k_score
setorder(k, name_key, year, -k_votes, -k_gross, -k_score)
k1 <- k[, .SD[1], by = .(name_key, year)]

# Se preparan los datasets para hacer el join, creando índices por título y año
setkey(m, title_key, year)
setkey(k1, name_key, year)

# se hace el join. Se añade información de Kaggle cuando la haya
out <- k1[m, on = .(name_key = title_key, year = year)]

# Se añade etiqueta de match. Si la diferencia en el campo runtime entre un dataset
# y otro es muy grande (se ha definido como 15 minutos), se marca como match débil,
# si es <= 15 minutos, se considera OK. No match para cuando no hay nada en Kaggle
out[, runtime_diff := fifelse(!is.na(k_runtime) & !is.na(runtime),
                              abs(as.integer(k_runtime) - as.integer(runtime)),
                              NA_integer_)]

out[, match_quality := fifelse(is.na(name), "no_match",
                               fifelse(!is.na(runtime_diff) & runtime_diff > 15, "weak_match", "ok_match"))]

# Se exporta resultado a fichero
out_file <- "data_processed/movies_tmdb_imdb_kaggle.csv"
fwrite(out, out_file)

# Se imprime diagnóstico
cat("Rows out:", nrow(out), "\n")
cat("Kaggle match rate (any):", round(mean(!is.na(out$name))*100, 2), "%\n")
cat("OK matches:", sum(out$match_quality == "ok_match"), "\n")
cat("Weak matches:", sum(out$match_quality == "weak_match"), "\n")
cat("No matches:", sum(out$match_quality == "no_match"), "\n")
cat("Saved:", out_file, "\n")