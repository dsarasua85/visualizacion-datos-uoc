# Descarga películas (1980-2024) desde TMDB y guarda un CSV con datos clave
# Se importan librerías
library(httr)
library(jsonlite)
library(data.table)

# Se obtiene el API Key de TMBD (almacenado ya)
api_key <- Sys.getenv("TMDB_API_KEY")
if (identical(api_key, "") || is.na(api_key)) stop("Falta TMDB_API_KEY en el entorno (ver .Renviron)")

# Se define la URL base de TMDB
base <- "https://api.themoviedb.org/3"

# Se define función para obtener datos de TMDB
tmdb_get <- function(path, query = list(), pause = 0.25) {
  # Pequeña pausa de 0.25s entre lecturas
  Sys.sleep(pause)
  
  # Se obtienen los datos
  r <- GET(
    url = paste0(base, path),
    query = c(list(api_key = api_key), query),
    user_agent("UOC-DataViz-Project/1.0 (academic)")
  )
  
  # Se controlan los errores
  if (status_code(r) != 200) {
    msg <- tryCatch(content(r, as = "text", encoding = "UTF-8"), error = function(e) "")
    stop("TMDB error ", status_code(r), " on ", path, " :: ", msg)
  }
  fromJSON(content(r, as = "text", encoding = "UTF-8"), flatten = TRUE)
}

# Se descubren las películas por año, paginando
discover_year <- function(year, min_votes = 50) {
  page <- 1
  out <- list()
  
  repeat {
    res <- tmdb_get("/discover/movie", query = list(
      primary_release_year = year,
      sort_by = "popularity.desc",
      include_adult = "false",
      include_video = "false",
      vote_count.gte = min_votes,
      page = page
    ))
    
    if (length(res$results) == 0) break
    out[[page]] <- as.data.table(res$results)
    
    if (page >= res$total_pages) break
    page <- page + 1
  }
  
  if (length(out) == 0) return(data.table())
  rbindlist(out, fill = TRUE)
}

# Se obtiene el detalle por película (budget/revenue/imdb_id/companies)
movie_details <- function(tmdb_id) {
  # Se añaden los créditos por si son necesarios luego
  d <- tmdb_get(paste0("/movie/", tmdb_id), query = list(
    append_to_response = "credits"
  ))
  
  companies <- NA_character_
  if (!is.null(d$production_companies) && nrow(d$production_companies) > 0) {
    companies <- paste(d$production_companies$name, collapse = " | ")
  }
  
  data.table(
    tmdb_id = d$id,
    imdb_id = d$imdb_id,
    title = d$title,
    original_title = d$original_title,
    release_date = d$release_date,
    year = if (!is.null(d$release_date) && nchar(d$release_date) >= 4) as.integer(substr(d$release_date, 1, 4)) else NA_integer_,
    original_language = d$original_language,
    runtime = d$runtime,
    genres = if (!is.null(d$genres) && nrow(d$genres) > 0) paste(d$genres$name, collapse = " | ") else NA_character_,
    budget = d$budget,
    revenue = d$revenue,
    popularity = d$popularity,
    tmdb_vote_average = d$vote_average,
    tmdb_vote_count = d$vote_count,
    production_companies = companies
  )
}

# Se recorren todos los años del dataset. Se aplica un filtro mínimo de 50 votos,
# garantizando así que las películas analizadas representan obras que han tenido
# exposición pública suficiente y mínima
years <- 1980:2024
min_votes <- 50
out_dir <- "data_tmdb"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Se descubren las películas por año
all_ids <- list()
for (y in years) {
  cat("Discover year:", y, "\n")
  dy <- discover_year(y, min_votes = min_votes)
  if (nrow(dy) == 0) next
  # Se obtienen los IDs únicos
  all_ids[[as.character(y)]] <- unique(dy$id)
}
ids <- unique(unlist(all_ids))
cat("Total TMDB ids:", length(ids), "\n")

# Se descargan los detalles
details_file <- file.path(out_dir, sprintf("tmdb_details_%d_%d_minVotes%d.csv", min(years), max(years), min_votes))

# En caso de re-ejecución, continúa donde se quedó
done_ids <- integer()
if (file.exists(details_file)) {
  existing <- fread(details_file)
  done_ids <- unique(existing$tmdb_id)
  cat("Continuando. Ya descargados:", length(done_ids), "\n")
}

pending <- setdiff(ids, done_ids)
cat("Pendientes:", length(pending), "\n")

# Se procesan las películas en bloques de 200 para evitar bloqueos, con pausa de 0.25s
# Se hace la escritura del fichero por bloques
chunk_size <- 200
for (i in seq(1, length(pending), by = chunk_size)) {
  chunk <- pending[i:min(i + chunk_size - 1, length(pending))]
  cat("Chunk", i, "-", min(i + chunk_size - 1, length(pending)), "\n")
  
  dt_chunk <- rbindlist(lapply(chunk, function(id) {
    tryCatch(movie_details(id), error = function(e) {
      cat("Error id", id, ":", conditionMessage(e), "\n")
      NULL
    })
  }), fill = TRUE)
  
  if (nrow(dt_chunk) > 0) {
    if (!file.exists(details_file)) {
      fwrite(dt_chunk, details_file)
    } else {
      fwrite(dt_chunk, details_file, append = TRUE)
    }
  }
}

cat("Listo. Archivo:", details_file, "\n")