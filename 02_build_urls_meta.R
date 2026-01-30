############################################################
# 02_build_urls_meta.R
# Construcción de tabla mínima de URLs y metadata. Aquí limpiamos un poquito y sacamos las urls para las replies
# Input:  data/tweets_meta.csv
# Output: data/urls_meta.csv  (delim=';')
############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

dir.create("data", showWarnings = FALSE)

tweets_meta <- read_csv("data/tweets_meta.csv", show_col_types = FALSE)

norm_handle <- function(x) {
  x %>% tolower() %>% str_trim()
}

# Mapa handle -> país
media_to_country <- c(
  "@el_pais"="España", "@elmundoes"="España", "@20m"="España", "@eldiarioes"="España", "@okdiario"="España",
  "@nytimes"="Estados Unidos", "@washingtonpost"="Estados Unidos", "@usatoday"="Estados Unidos", "@wsj"="Estados Unidos", "@nypost"="Estados Unidos",
  "@gazetesozcu"="Turquía", "@haberturk"="Turquía", "@haberler"="Turquía", "@ensonhaber"="Turquía", "@twitersondakika"="Turquía"
)

# Nota: TweetScraperR suele guardar el handle en columna username
# Si en tu CSV la columna se llama distinto, ajusta aquí.
if (!"username" %in% names(tweets_meta)) stop("No encuentro columna 'username' en data/tweets_meta.csv")

urls_meta <- tweets_meta %>%
  mutate(
    user_original = username,
    user_original_norm = norm_handle(user_original),
    pais = unname(media_to_country[user_original_norm])
  ) %>%
  transmute(
    url = url,
    fecha_original = date,
    user_original = user_original,
    pais = pais,
    tweet_original = text
  ) %>%
  filter(!is.na(url), str_detect(url, "^https?://")) %>%
  distinct(url, .keep_all = TRUE)

# Guardamos en ; para compatibilidad con tu flujo anterior
write_delim(urls_meta, "data/urls_meta.csv", delim = ";")
message("OK -> data/urls_meta.csv")

message("\nConteo por país (URLs):")
print(urls_meta %>% count(pais, sort = TRUE))