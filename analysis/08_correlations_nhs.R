# Study 2: Country-wise correlations in emotion responses (softimpute and raw data) ------------------
rm(list = ls())

library(pacman) # install.packages(pacman)
p_load(
  tidyverse,
  here,
  ggplot2
)


## Prep data ------

nhs_data <- read_csv(here("data", "tml_wwl_data.csv.gz")) %>%
  filter(corpus == "nhs")

gems25_labels <- c(
  "energetic",
  "agitated",
  "sad",
  "sentimental",
  "calm",
  "animated",
  "strong",
  "joyful",
  "tender",
  "tense",
  "allured",
  "overwhelmed",
  "dreamy",
  "fascinated",
  "bouncy",
  "tearful",
  "triumphant",
  "mellowed_softened_up",
  "affectionate",
  "soothed",
  "moved",
  "serene",
  "nostalgic",
  "filled_with_wonder",
  "transcendent"
)

gems9_labels = c(
  "wonder",
  "transcendence",
  "power",
  "tenderness",
  "nostalgia",
  "peacefulness",
  "joyful_activation",
  "sadness",
  "tension"
)


# "rcartocolor::Prism"
palette <- c(
  "#5F4690FF",
  "#1D6996FF",
  "#38A6A5FF",
  "#0F8554FF",
  "#73AF48FF",
  "#EDAD08FF",
  "#E17C05FF",
  "#CC503EFF",
  "#94346EFF",
  "#855686FF",
  "#C07ABDFF",
  "#445156FF"
)

gems9_cols <- c(
  joyful_activation = palette[1],
  nostalgia = palette[2],
  peacefulness = palette[3],
  power = palette[4],
  sadness = palette[5],
  tenderness = palette[6],
  tension = palette[7],
  transcendence = palette[8],
  wonder = palette[9]
)

pretty_labels <- c(
  joyful_activation = "Joyful Activation",
  nostalgia = "Nostalgia",
  peacefulness = "Peacefulness",
  power = "Power",
  sadness = "Sadness",
  tenderness = "Tenderness",
  tension = "Tension",
  transcendence = "Transcendence",
  wonder = "Wonder"
)


## Country-wise correlations using imputed data -----------------------

nhs_reduced_data <- readRDS(here("results/imputations", "nhs_400.rds"))

top_countries_nhs_400 <- nhs_reduced_data$country %>% unique

# Scale variables to aid modelling
for (latent_var in names(
  nhs_reduced_data %>% dplyr::select(contains("V"))
)) {
  new_name <- paste0(latent_var, "_scaled")

  nhs_reduced_data[new_name] <- scale(
    nhs_reduced_data[latent_var],
    center = TRUE,
    scale = TRUE
  )
}


nhs_v1_cor_data <- nhs_reduced_data %>%
  group_by(country, song) %>%
  summarise(mean_v1 = mean(V1_scaled, na.rm = T)) %>%
  pivot_wider(names_from = country, values_from = mean_v1)

nhs_v2_cor_data <- nhs_reduced_data %>%
  group_by(country, song) %>%
  summarise(mean_v2 = mean(V2_scaled, na.rm = T)) %>%
  pivot_wider(names_from = country, values_from = mean_v2)

# Get all possible pairs of countries
country_pairs <- combn(top_countries_nhs_400, 2, simplify = FALSE)

# For each pair of countries, correlate song-wise mean ratings
get_corr <- function(pair, data) {
  c1 <- pair[1]
  c2 <- pair[2]

  vec1 <- data %>% pull(c1)
  vec2 <- data %>% pull(c2)

  pairwise_tmp_data <- bind_cols("c1" = vec1, "c2" = vec2) %>%
    drop_na

  if (nrow(pairwise_tmp_data) < 10) {
    return(
      tibble(
        country1 = c1,
        country2 = c2,
        country = paste0(c1, "_", c2),
        correlation = NA_real_
      )
    )
  }

  r <- cor.test(
    pairwise_tmp_data[["c1"]],
    pairwise_tmp_data[["c2"]],
    method = "pearson"
  )$estimate

  tibble(
    country1 = c1,
    country2 = c2,
    country = paste0(c1, "_", c2),
    correlation = r
  )
}

# Apply get_corr function over all pairs
nhs_v1_cor_results <- map_dfr(
  country_pairs,
  get_corr,
  data = nhs_v1_cor_data
) %>%
  mutate(variable = "Valence")

nhs_v2_cor_results <- map_dfr(
  country_pairs,
  get_corr,
  data = nhs_v2_cor_data
) %>%
  mutate(variable = "Arousal")


nhs_cor_results <- bind_rows(
  nhs_v1_cor_results,
  nhs_v2_cor_results
)

#  Compute average r value for each latent dimension
nhs_mean_cors <- nhs_cor_results %>%
  group_by(variable) %>%
  summarise(mean_correlation = mean(correlation, na.rm = TRUE))


## Country-wise correlations using un-imputed data -----------------------

nhs_long_data <- nhs_data %>%
  drop_na(rating) %>%
  # Assign each gems-25 scale to relevant gems-9 factor
  mutate(
    gems_9 = case_when(
      emotion %in% c("moved", "filled_with_wonder", "allured") ~ "wonder",
      emotion %in%
        c("fascinated", "overwhelmed", "transcendent") ~ "transcendence",
      emotion %in% c("strong", "energetic", "triumphant") ~ "power",
      emotion %in%
        c("tender", "affectionate", "mellowed_softened_up") ~ "tenderness",
      emotion %in% c("nostalgic", "sentimental", "dreamy") ~ "nostalgia",
      emotion %in% c("serene", "calm", "soothed") ~ "peacefulness",
      emotion %in% c("animated", "bouncy", "joyful") ~ "joyful_activation",
      emotion %in% c("sad", "tearful") ~ "sadness",
      emotion %in% c("tense", "agitated") ~ "tension"
    )
  ) %>%
  group_by(user_id, country, language, song, gems_9) %>%
  # calculate gems-9 score by averaging all available ratings for that factor
  summarise(rating = mean(rating, na.rm = T)) %>%
  ungroup() %>%
  group_by(gems_9) %>%
  # mean centering: scores now indicate whether a song is more or less "transcendent" than average song
  mutate(rating_z = scale(rating, center = TRUE, scale = TRUE))

# function to run all possible pairwise correlations
correlator <- function(n_ppt = numeric(), data = data.frame()) {
  # Get countries with at least n_ppt number of participants
  top_countries <- data %>%
    ungroup() %>%
    dplyr::select(country, user_id) %>%
    distinct() %>%
    group_by(country) %>%
    summarise(n = n()) %>%
    filter(n >= n_ppt) %>%
    ungroup() %>%
    pull(country)

  # Create wide datasets for each gems dimension
  gems_correlation_data <- list()

  for (gems in gems9_labels) {
    print(paste0("Building data for: ", gems))

    tmp <- data %>%
      filter(gems_9 == gems) %>%
      filter(country %in% top_countries) %>%
      group_by(country, song) %>%
      summarise(mean = mean(rating_z, na.rm = T)) %>%
      pivot_wider(names_from = country, values_from = mean) %>%
      ungroup()

    gems_correlation_data[[gems]] <- tmp
  }

  country_pairs <- combn(top_countries, 2, simplify = FALSE)

  # Store results of pairwise correlations
  gems_correlation_results <- data.frame()

  # for each gems9 factor, is distribution of r values significantly > 0?
  for (gems in gems9_labels) {
    tmp_results <- map_dfr(
      country_pairs,
      get_corr,
      data = gems_correlation_data[[gems]]
    ) %>%
      mutate(
        variable = gems
      )

    gems_correlation_results <- bind_rows(gems_correlation_results, tmp_results)
  }

  return(gems_correlation_results)
}


nhs_corrs_400 <- correlator(400, nhs_long_data)

# All correlation distributions are significantly above null hyp of r <= 0
nhs_gems_mean_cors_400 <- nhs_corrs_400 %>%
  group_by(variable) %>%
  summarise(
    mean_correlation = mean(correlation, na.rm = TRUE),
    median_correlation = median(correlation, na.rm = TRUE),
    iqr = IQR(correlation, na.rm = TRUE)
  ) %>%
  mutate(ppt_n = "400")


## Noise ceiling adjustments for unimputed data ----------------------
# To correct for noise present in the data, we use a noise ceiling adjustment
# which scales up the observed correlation to an estimate of what it would be
# if data from both samples (in this case, countries) contained no sampling error.
# The noise ceiling is the square root of (1- mean(squared SE per son on a given rating dimension)/var(mean score per song on a given dimension))

nc_correlator <- function(n_ppt = numeric(), data = data.frame()) {
  # get countries above specific n
  top_countries <- data %>%
    ungroup() %>%
    select(country, user_id) %>%
    distinct() %>%
    group_by(country) %>%
    summarise(n = n(), .groups = "drop") %>%
    filter(n >= n_ppt) %>%
    pull(country)

  # calculate mean and SE of ratings per song on each emotion dimension for each country
  gems_correlation_data_adjusted <- list()
  for (gems in gems9_labels) {
    tmp <- data %>%
      filter(gems_9 == gems, country %in% top_countries) %>%
      group_by(country, song) %>%
      summarise(
        mean = mean(rating_z, na.rm = TRUE),
        mean_raw = mean(rating, na.rm = TRUE),
        se = sd(rating, na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
      ) %>%
      pivot_wider(
        names_from = country,
        values_from = c(mean, mean_raw, se),
        names_glue = "{.value}_{country}"
      )

    gems_correlation_data_adjusted[[gems]] <- tmp
  }

  # all country pairs
  country_pairs <- combn(top_countries, 2, simplify = FALSE)

  # results container
  res_tmp <- data.frame(
    variable = character(),
    country = character(),
    corr = numeric(),
    stringsAsFactors = FALSE
  )

  i <- 1

  for (gems in gems9_labels) {
    for (pair in country_pairs) {
      c1 <- pair[1]
      c2 <- pair[2]

      print(paste0("Running analyses for ", c1, " and ", c2))

      tmp_noise_ceiling <- gems_correlation_data_adjusted[[gems]] %>%
        dplyr::select(contains(c1) | contains(c2)) %>%
        drop_na()

      # skip if not enough overlapping data between countries
      if (nrow(tmp_noise_ceiling) < 10) {
        next
      }

      # mean ratings per song for each country
      x <- tmp_noise_ceiling[[paste0("mean_raw_", c1)]]
      y <- tmp_noise_ceiling[[paste0("mean_raw_", c2)]]

      # calculate unadjusted correlation
      cor_unadjusted <- suppressWarnings(cor(x, y, use = "complete.obs"))

      # calculate noise ceilings per country (this is the max observable correlation between this noisy measure and a hypothetical perfect measure of that same country)
      rel_1 <- 1 -
        (mean(tmp_noise_ceiling[[paste0("se_", c1)]]^2, na.rm = T)) /
          (var(tmp_noise_ceiling[[paste0("mean_raw_", c1)]], na.rm = T))

      rel_2 <- 1 -
        (mean(tmp_noise_ceiling[[paste0("se_", c2)]]^2, na.rm = T)) /
          (var(tmp_noise_ceiling[[paste0("mean_raw_", c2)]], na.rm = T))

      # if reliability is infinite or negative, skip
      if (!is.finite(rel_1) || !is.finite(rel_2) || rel_1 <= 0 || rel_2 <= 0) {
        print(paste0("Noise ceiling can't be calculated for ", c1, c2))
        next
      }

      nc_1 <- sqrt(rel_1)
      nc_2 <- sqrt(rel_2)

      # calculate correlation coefficient, adjusted for noise ceilings
      adj_corr <- cor_unadjusted / mean(c(nc_1, nc_2), na.rm = T)

      if (adj_corr > 1) {
        adj_corr <- 1
      }

      res_tmp[i, ] <- list(
        variable = gems,
        country = paste(c1, c2, sep = "_"),
        corr = adj_corr
      )

      i <- i + 1
    }
  }

  return(res_tmp)
}


# warnings are thrown for a few countries on specific dimensions, presumably due to very high noise. These datapoints are excluded.
nhs_gems_correlation_results_adjusted <- nc_correlator(
  n_ppt = 400,
  data = nhs_long_data
)

# calculate mean of adjusted correlations per emotion dimensions
nhs_gems_mean_cors_400_adjusted <- nhs_gems_correlation_results_adjusted %>%
  group_by(variable) %>%
  summarise(
    mean_adjusted_correlation = mean(corr, na.rm = T),
    median_adjusted_correlation = median(corr, na.rm = T),
    iqr_adjusted_correlation = IQR(corr, na.rm = T)
  )

nhs_gems_correlation_results_combined <- nhs_corrs_400 %>%
  dplyr::select(corr = correlation, variable, country) %>%
  bind_rows(
    adjusted = nhs_gems_correlation_results_adjusted,
    unadjusted = .,
    .id = "method"
  )

### mini table for inset plot

mini_table <- nhs_long_data %>%
  mutate(song = as.numeric(song)) %>%
  filter(
    country %in%
      c("Finland", "Thailand", "Argentina", "Australia") &
      gems_9 == "joyful_activation"
  ) %>%
  group_by(country, song) %>%
  summarise(mean_rating = mean(rating, na.rm = T)) %>%
  mutate(mean_rating = round(mean_rating, 1)) %>%
  pivot_wider(
    id_cols = song,
    names_from = country,
    values_from = mean_rating
  ) %>%
  slice(25:30) %>%
  rename("Song" = song) %>%
  dplyr::select(Song, "Finland", "Thailand", "Argentina", "Australia")


## save data ----------------------------------

save(
  nhs_cor_results,
  nhs_corrs_400,
  nhs_gems_correlation_results_adjusted,
  nhs_gems_correlation_results_combined,
  nhs_gems_mean_cors_400,
  nhs_gems_mean_cors_400_adjusted,
  mini_table,
  file = here("results", "08_nhs_cor_results.RData")
)
