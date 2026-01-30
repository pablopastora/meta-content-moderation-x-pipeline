############################################################
# 01_scrape_tweets_meta.R
# Scraping de tuits (medios) con TweetScraperR
# Output: data/tweets_meta.csv
############################################################

suppressPackageStartupMessages({
  library(TweetScraperR)
  library(dplyr)
  library(readr)
  library(stringr)
})

dir.create("data", showWarnings = FALSE)

# ---- Config
DATE_SINCE <- "2025-01-06_00:00:00_UTC"
DATE_UNTIL <- "2025-01-20_23:59:59_UTC"   # ajusta si quieres
N_TWEETS   <- 800
TIMEOUT    <- 10

SEARCH_ANY <- "Meta Zuckerberg Facebook Instagram"
# Si quieres más precisión temática:
# SEARCH_ANY <- "(Meta OR Zuckerberg OR Facebook OR Instagram) (moderation OR censorship OR fact-checking OR verification OR community notes)"

ACCOUNTS <- c(
  # España
  "@el_pais", "@elmundoes", "@20m", "@eldiarioes", "@okdiario",
  # EEUU
  "@nytimes", "@washingtonpost", "@USATODAY", "@WSJ", "@nypost",
  # Turquía (revisa el handle final si fuese necesario)
  "@gazetesozcu", "@Haberturk", "@Haberler", "@ensonhaber", "@TwiterSonDakika"
)

# ---- Credenciales vía entorno (NO hardcode)
# Antes de ejecutar:
# Sys.setenv(USER="...", PASS="...")
xuser <- Sys.getenv("USER")
xpass <- Sys.getenv("PASS")
if (xuser == "" || xpass == "") {
  stop("Faltan USER/PASS en variables de entorno. Usa Sys.setenv(USER=..., PASS=...)")
}

# ---- Scrape
message("=== [01] Scraping tuits de medios (TweetScraperR) ===")
openTwitter()

getTweetsFullSearch(
  search_all   = NULL,
  search_exact = NULL,
  search_any   = SEARCH_ANY,
  no_search    = NULL,
  hashtag      = NULL,
  lan          = NULL,
  from         = paste(ACCOUNTS, collapse = " "),
  to           = NULL,
  men          = NULL,
  rep          = 0,
  fav          = 0,
  rt           = 0,
  timeout      = TIMEOUT,
  n_tweets     = N_TWEETS,
  since        = DATE_SINCE,
  until        = DATE_UNTIL,
  xuser        = xuser,
  xpass        = xpass,
  dir          = getwd(),
  save         = TRUE
)

# ---- Consolidación (busqueda*)
lista <- mget(ls(pattern = "^busqueda"))
tweets_meta <- bind_rows(lista) %>% distinct()

# ---- Limpieza mínima
tweets_meta <- tweets_meta %>%
  filter(!is.na(url), url != "") %>%
  distinct(url, .keep_all = TRUE)

write_csv(tweets_meta, "data/tweets_meta.csv")
message("OK -> data/tweets_meta.csv")