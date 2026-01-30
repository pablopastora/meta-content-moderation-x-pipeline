############################################################
# 04_descriptive_lexical_media_posts.R
# Descriptivo + nubes de palabras + bigrams por país (posts de medios)
#
# Input:
#   data/tweets_meta.csv   (salida de 01_scrape_tweets_meta.R)
#
# Outputs:
#   figures/desc_engagement_summary.csv
#   figures/posts_by_media.csv
#   figures/posts_by_country.csv
#   figures/wordcloud_<pais>.png
#   figures/top_bigrams_<pais>.csv
#   figures/top_bigrams_<pais>.png
#
# Requisitos (CRAN):
#   dplyr, readr, stringr, lubridate, ggplot2, tidytext, stopwords, tibble
#   (opcional) SnowballC
############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(ggplot2)
  library(tidytext)
  library(tibble)
  library(stopwords)
})

dir.create("figures", showWarnings = FALSE)
dir.create("data", showWarnings = FALSE)

# ----------------------------
# 1) Cargar datos
# ----------------------------
infile <- "data/tweets_meta.csv"
if (!file.exists(infile)) stop("No encuentro data/tweets_meta.csv. Ejecuta primero 01_scrape_tweets_meta.R")

df <- read_csv(infile, show_col_types = FALSE)

# ----------------------------
# 2) Normalizar columnas esperadas
# ----------------------------
# Ajusta aquí si tu CSV usa otros nombres.
required_cols <- c("text", "username", "url")
missing <- setdiff(required_cols, names(df))
if (length(missing) > 0) {
  stop(paste0("Faltan columnas en tweets_meta.csv: ", paste(missing, collapse = ", ")))
}

# Métricas: según fuente puede variar (retweet_count vs repost_count, etc.)
# Creamos columnas estándar si existen alternativas:
df <- df %>%
  mutate(
    reply_count   = coalesce(.data$reply_count,   .data$replies, .data$replies_count),
    retweet_count = coalesce(.data$retweet_count, .data$repost_count, .data$reposts, .data$retweets),
    like_count    = coalesce(.data$like_count,    .data$likes, .data$fav_count)
  )

# Pais: si ya viene, lo usa; si no, lo intenta inferir por username (solo si coincide con tus listas)
if (!"pais" %in% names(df)) {
  message("[WARN] No existe columna 'pais' en tweets_meta.csv. Intento inferir por username con mapeo básico.")
  norm_handle <- function(x) tolower(str_trim(x))
  
  media_to_country <- c(
    "@el_pais"="España", "@elmundoes"="España", "@20m"="España", "@eldiarioes"="España", "@okdiario"="España",
    "@nytimes"="Estados Unidos", "@washingtonpost"="Estados Unidos", "@usatoday"="Estados Unidos", "@wsj"="Estados Unidos", "@nypost"="Estados Unidos",
    "@gazetesozcu"="Turquía", "@haberturk"="Turquía", "@haberler"="Turquía", "@ensonhaber"="Turquía", "@twitersondakika"="Turquía"
  )
  
  df <- df %>%
    mutate(
      username_norm = norm_handle(username),
      pais = unname(media_to_country[username_norm])
    )
}

# Quitar filas sin texto útil
df <- df %>%
  mutate(text = str_squish(coalesce(text, ""))) %>%
  filter(text != "")

# ----------------------------
# 3) DESCRIPTIVO: engagement global + N por medio/país
# ----------------------------

# 3.1 Resumen global (medias)
desc_global <- df %>%
  summarise(
    N_posts = n(),
    mean_replies  = mean(reply_count, na.rm = TRUE),
    mean_retweets = mean(retweet_count, na.rm = TRUE),
    mean_likes    = mean(like_count, na.rm = TRUE)
  )

write_csv(desc_global, "figures/desc_engagement_summary.csv")
message("[OK] figures/desc_engagement_summary.csv")

# 3.2 Posts por país
posts_by_country <- df %>%
  count(pais, name = "N_posts") %>%
  arrange(desc(N_posts))

write_csv(posts_by_country, "figures/posts_by_country.csv")
message("[OK] figures/posts_by_country.csv")

# 3.3 Posts por medio (username)
posts_by_media <- df %>%
  count(username, name = "N_posts") %>%
  arrange(desc(N_posts))

write_csv(posts_by_media, "figures/posts_by_media.csv")
message("[OK] figures/posts_by_media.csv")

# (Opcional) gráfico de posts por medio (top 15)
p_media <- posts_by_media %>%
  slice_max(N_posts, n = 15) %>%
  ggplot(aes(x = reorder(username, N_posts), y = N_posts)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL, y = "Number of posts", title = "Posting frequency by media outlet (Top 15)") +
  theme_minimal(base_size = 12)

ggsave("figures/posts_by_media_top15.png", p_media, width = 8, height = 5, dpi = 300)
message("[OK] figures/posts_by_media_top15.png")

# ----------------------------
# 4) PREPROCESADO TEXTO para nubes y bigrams
# ----------------------------

# Stopwords multilingües (ES + EN + TR). Turkey lo puedes mantener aunque luego sea cuali.
sw_es <- stopwords("es", source = "stopwords-iso")
sw_en <- stopwords("en", source = "stopwords-iso")
sw_tr <- stopwords("tr", source = "stopwords-iso")
sw_all <- unique(c(sw_es, sw_en, sw_tr))

# Función: limpieza ligera para titulares/posts
clean_text <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("https?://\\S+", " ") %>%
    str_replace_all("@\\w+", " ") %>%
    str_replace_all("#", " ") %>%
    str_replace_all("[^\\p{L}\\p{N}\\s]", " ") %>%
    str_squish()
}

df_txt <- df %>%
  mutate(text_clean = clean_text(text))

# ----------------------------
# 5) NUBES DE PALABRAS por país
# ----------------------------

# Nota: para evitar dependencias raras, genero una nube "clásica" con ggplot (tipo wordcloud simple)
# Si prefieres wordcloud2, dímelo y te lo adapto.

make_wordcloud_plot <- function(data_country, country_name, top_n = 80) {
  # Tokenizar palabras
  wc <- data_country %>%
    select(text_clean) %>%
    unnest_tokens(word, text_clean) %>%
    filter(!word %in% sw_all) %>%
    filter(str_detect(word, "^[a-záéíóúñüçöğış]+$|^[a-z0-9]+$")) %>%
    count(word, sort = TRUE) %>%
    slice_head(n = top_n)
  
  # Si hay muy poco texto (p.ej., Turquía con N bajo), no graficar
  if (nrow(wc) < 10) return(NULL)
  
  # “Pseudo-wordcloud”: tamaño por frecuencia
  ggplot(wc, aes(x = reorder(word, n), y = n)) +
    geom_col() +
    coord_flip() +
    labs(
      title = paste0("Most frequent terms (", country_name, ")"),
      x = NULL,
      y = "Frequency"
    ) +
    theme_minimal(base_size = 12)
}

countries <- df_txt %>%
  filter(!is.na(pais), pais != "") %>%
  distinct(pais) %>%
  pull(pais)

for (ct in countries) {
  dct <- df_txt %>% filter(pais == ct)
  
  p_wc <- make_wordcloud_plot(dct, ct, top_n = 40) # 40 para que sea legible
  if (is.null(p_wc)) {
    message("[WARN] No se genera nube para ", ct, " (pocos términos).")
    next
  }
  
  out_wc <- paste0("figures/wordcloud_", str_replace_all(ct, "\\s+", "_"), ".png")
  ggsave(out_wc, p_wc, width = 7, height = 5, dpi = 300)
  message("[OK] ", out_wc)
}

# ----------------------------
# 6) BIGRAMS por país (Top 10)
# ----------------------------

make_bigrams <- function(data_country, country_name, top_n = 10) {
  bi <- data_country %>%
    select(text_clean) %>%
    unnest_tokens(bigram, text_clean, token = "ngrams", n = 2) %>%
    separate(bigram, into = c("w1", "w2"), sep = " ", remove = FALSE) %>%
    filter(!is.na(w1), !is.na(w2)) %>%
    filter(!(w1 %in% sw_all), !(w2 %in% sw_all)) %>%
    filter(str_detect(w1, "^[\\p{L}\\p{N}]+$"), str_detect(w2, "^[\\p{L}\\p{N}]+$")) %>%
    count(bigram, sort = TRUE) %>%
    slice_head(n = top_n)
  
  bi
}

make_bigrams_plot <- function(bi, country_name) {
  ggplot(bi, aes(x = reorder(bigram, n), y = n)) +
    geom_col() +
    coord_flip() +
    labs(
      title = paste0("Top bigrams (", country_name, ")"),
      x = NULL,
      y = "Frequency"
    ) +
    theme_minimal(base_size = 12)
}

for (ct in countries) {
  dct <- df_txt %>% filter(pais == ct)
  
  bi <- make_bigrams(dct, ct, top_n = 10)
  
  if (nrow(bi) == 0) {
    message("[WARN] No se generan bigrams para ", ct, " (sin bigrams tras limpieza).")
    next
  }
  
  out_csv <- paste0("figures/top_bigrams_", str_replace_all(ct, "\\s+", "_"), ".csv")
  write_csv(bi, out_csv)
  message("[OK] ", out_csv)
  
  p_bi <- make_bigrams_plot(bi, ct)
  out_png <- paste0("figures/top_bigrams_", str_replace_all(ct, "\\s+", "_"), ".png")
  ggsave(out_png, p_bi, width = 7, height = 4, dpi = 300)
  message("[OK] ", out_png)
}

message("=== FIN 04_descriptive_lexical_media_posts.R ===")