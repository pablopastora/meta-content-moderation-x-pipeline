############################################################
# 03_extract_replies_meta.R
# Extracción de replies con twscrapeR desde URLs (es el paso más difícil)
# Input:  data/urls_meta.csv
# Output: data/replies_meta_desde_urls.csv
############################################################

suppressPackageStartupMessages({
  library(twscrapeR)
  library(dplyr)
  library(readr)
  library(stringr)
  library(purrr)
})

dir.create("data", showWarnings = FALSE)

# ---- Variables de entorno (NO hardcode)
# Antes de ejecutar:
# Sys.setenv(
#   TW_USER="...",
#   TW_PASS="...",
#   TW_EMAIL="...",
#   TW_EMAIL_PASS="...",
#   TW_COOKIES="auth_token=...; ct0=..."
# )
tw_user       <- Sys.getenv("TW_USER")
tw_pass       <- Sys.getenv("TW_PASS")
tw_email      <- Sys.getenv("TW_EMAIL")
tw_email_pass <- Sys.getenv("TW_EMAIL_PASS")
tw_cookies    <- Sys.getenv("TW_COOKIES")

if (tw_user == "" || tw_pass == "" || tw_email == "" || tw_email_pass == "" || tw_cookies == "") {
  stop("Faltan TW_USER/TW_PASS/TW_EMAIL/TW_EMAIL_PASS/TW_COOKIES en variables de entorno.")
}

message("=== [03] Setup twscrapeR ===")
setup_twscraper()

# Añadir cuenta (si ya existe, no pasa nada)
try({
  add_account(
    username = tw_user,
    password = tw_pass,
    email = tw_email,
    email_password = tw_email_pass,
    cookies = tw_cookies
  )
}, silent = TRUE)

list_accounts()

# ---- Leer URLs
enlaces <- read_delim(
  "data/urls_meta.csv",
  delim = ";",
  trim_ws = TRUE,
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
) %>%
  filter(!is.na(url), str_detect(url, "^https?://"))

# ---- Extraer tweet_id
extract_tweet_id <- function(u) {
  str_match(u, "/status/([0-9]+)")[,2]
}

enlaces <- enlaces %>%
  mutate(original_tweet_id = extract_tweet_id(url)) %>%
  filter(!is.na(original_tweet_id))

message("URLs válidas: ", nrow(enlaces))

# ---- Función segura
get_replies_safe <- safely(function(tweet_id) {
  tweet_replies(tweet_id) %>% to_dataframe()
})

replies_list <- vector("list", nrow(enlaces))

for (i in seq_len(nrow(enlaces))) {
  cat(sprintf("[REPLIES] %d/%d  tweet_id=%s\n", i, nrow(enlaces), enlaces$original_tweet_id[i]))
  
  out <- get_replies_safe(enlaces$original_tweet_id[i])
  
  if (!is.null(out$result) && nrow(out$result) > 0) {
    replies_list[[i]] <- out$result %>%
      mutate(
        original_tweet_id = enlaces$original_tweet_id[i],
        fecha_original    = enlaces$fecha_original[i],
        user_original     = enlaces$user_original[i],
        tweet_original    = enlaces$tweet_original[i],
        url_original      = enlaces$url[i],
        pais              = enlaces$pais[i]
      )
  } else {
    replies_list[[i]] <- NULL
  }
}

replies_df <- bind_rows(replies_list) %>%
  mutate(
    text = ifelse(is.na(text), "", text),
    text = str_squish(text)
  ) %>%
  filter(text != "") %>%
  distinct(id, .keep_all = TRUE)

write_csv(replies_df, "data/replies_meta_desde_urls.csv")
message("OK -> data/replies_meta_desde_urls.csv")

message("\nConteo por país (replies):")
print(replies_df %>% count(pais, sort = TRUE))