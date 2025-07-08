# R/api_helpers.R

# Required packages
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)

# Base URL for the Art Institute of Chicago API
BASE_URL <- "https://api.artic.edu/api/v1"

# 1. List artworks
get_artworks <- function(page = 1,
                         limit = 100,
                         fields = c("id", "title", "artist_title", "date_display",
                                    "classification_titles", "image_id")) {
  res <- GET(
    url = paste0(BASE_URL, "/artworks"),
    query = list(
      page   = page,
      limit  = limit,
      fields = paste(fields, collapse = ",")
    )
  )
  stop_for_status(res)
  fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)$data %>%
    as_tibble()
}

# 2. Get one artwork’s details
get_artwork_details <- function(id,
                                fields = NULL) {
  url <- paste0(BASE_URL, "/artworks/", id)
  qry <- if (!is.null(fields)) list(fields = paste(fields, collapse = ",")) else NULL
  res <- GET(url = url, query = qry)
  stop_for_status(res)
  fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)$data %>%
    as_tibble()
}

# 3. Search artworks (full‐text + optional public‐domain filter)
search_artworks <- function(q,
                            is_public_domain = NULL,
                            page = 1,
                            limit = 100,
                            fields = c("id", "title", "date_display",
                                       "artist_title", "image_id")) {
  qry <- list(
    q      = q,
    page   = page,
    limit  = limit,
    fields = paste(fields, collapse = ",")
  )
  if (!is.null(is_public_domain)) {
    # API expects lowercase true/false
    qry[["query[term][is_public_domain]"]] <- tolower(is_public_domain)
  }
  res <- GET(url = paste0(BASE_URL, "/artworks/search"), query = qry)
  stop_for_status(res)
  fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)$data %>%
    as_tibble()
}

# 4. List artists
get_artists <- function(page = 1,
                        limit = 100,
                        fields = c("id", "title", "birth_date", "death_date", "nationality")) {
  res <- GET(
    url = paste0(BASE_URL, "/artists"),
    query = list(
      page   = page,
      limit  = limit,
      fields = paste(fields, collapse = ",")
    )
  )
  stop_for_status(res)
  fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)$data %>%
    as_tibble()
}

# 5. Get one artist’s details
get_artist_details <- function(id,
                               fields = NULL) {
  url <- paste0(BASE_URL, "/artists/", id)
  qry <- if (!is.null(fields)) list(fields = paste(fields, collapse = ",")) else NULL
  res <- GET(url = url, query = qry)
  stop_for_status(res)
  fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)$data %>%
    as_tibble()
}

# 6. List exhibitions
get_exhibitions <- function(page = 1,
                            limit = 100,
                            fields = c("id", "title", "date_start", "date_end", "place")) {
  res <- GET(
    url = paste0(BASE_URL, "/exhibitions"),
    query = list(
      page   = page,
      limit  = limit,
      fields = paste(fields, collapse = ",")
    )
  )
  stop_for_status(res)
  fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)$data %>%
    as_tibble()
}
