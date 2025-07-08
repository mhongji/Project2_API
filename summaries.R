# R/summaries.R

#–– Dependencies ––
library(dplyr)
library(tidyr)
library(janitor)
library(ggplot2)

# 1. Contingency table: count of artworks by classification
contingency_classification <- function(artworks_df) {
  artworks_df %>%
    unnest(classification_titles) %>%
    tabyl(classification_titles) %>%
    arrange(desc(n))
}

# 2. Numeric summary: mean & SD of creation year by classification
numeric_summary_year_by_class <- function(artworks_df) {
  artworks_df %>%
    # extract year from date_display (e.g. "1890")
    mutate(year = as.integer(substr(date_display, 1, 4))) %>%
    unnest(classification_titles) %>%
    group_by(classification_titles) %>%
    summarize(
      n         = n(),
      mean_year = mean(year, na.rm = TRUE),
      sd_year   = sd(year,   na.rm = TRUE)
    ) %>%
    arrange(desc(n))
}

# 3. Bar chart: artwork counts by classification
plot_count_by_class <- function(artworks_df) {
  df <- artworks_df %>%
    unnest(classification_titles) %>%
    count(classification_titles, sort = TRUE)
  
  ggplot(df, aes(x = reorder(classification_titles, n), y = n)) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Count of Artworks by Classification",
      x     = "Classification",
      y     = "Count"
    )
}

# 4. Boxplot: distribution of creation year by classification
plot_year_boxplot <- function(artworks_df) {
  df <- artworks_df %>%
    mutate(year = as.integer(substr(date_display, 1, 4))) %>%
    unnest(classification_titles)
  
  ggplot(df, aes(
    x = reorder(classification_titles, year, FUN = median, na.rm = TRUE),
    y = year
  )) +
    geom_boxplot() +
    coord_flip() +
    labs(
      title = "Distribution of Artwork Creation Year by Classification",
      x     = "Classification",
      y     = "Year"
    )
}

# 5. Heatmap: number of artworks per year × classification
plot_heatmap_year_class <- function(artworks_df) {
  df <- artworks_df %>%
    mutate(year = as.integer(substr(date_display, 1, 4))) %>%
    unnest(classification_titles) %>%
    count(year, classification_titles)
  
  ggplot(df, aes(x = classification_titles, y = year, fill = n)) +
    geom_tile() +
    labs(
      title = "Heatmap of Artworks by Year & Classification",
      x     = "Classification",
      y     = "Year",
      fill  = "Count"
    ) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
}

# 6. Scatter (advanced): exhibition duration vs. start year by place
plot_exhibition_duration <- function(exhibitions_df) {
  df <- exhibitions_df %>%
    mutate(
      date_start = as.Date(date_start),
      date_end   = as.Date(date_end),
      start_year = as.integer(format(date_start, "%Y")),
      duration   = as.numeric(date_end - date_start)
    )
  
  ggplot(df, aes(x = start_year, y = duration, color = place)) +
    geom_point(alpha = 0.7) +
    labs(
      title = "Exhibition Duration vs. Start Year by Place",
      x     = "Start Year",
      y     = "Duration (days)",
      color = "Place"
    )
}
