# Study 1: Country-wise correlations in emotion responses (softimpute and raw data) ------------------
rm(list = ls())

library(pacman) # install.packages(pacman)
p_load(
  tidyverse,
  here,
  ggplot2
)


## Prep data and variables -----------------------------------------------

billboard_data <- read_csv(here("data", "tml_wwl_data.csv.gz")) %>%
  filter(corpus == "billboard")

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

billboard_reduced_data <- readRDS(here(
  "results/imputations",
  "billboard_400.rds"
))

top_countries_billboard_400 <- billboard_reduced_data$country %>% unique

# Scale variables to aid modelling
for (latent_var in names(
  billboard_reduced_data %>% dplyr::select(contains("V"))
)) {
  new_name <- paste0(latent_var, "_scaled")

  billboard_reduced_data[new_name] <- scale(
    billboard_reduced_data[latent_var],
    center = TRUE,
    scale = TRUE
  )
}

billboard_v1_cor_data <- billboard_reduced_data %>%
  group_by(country, song) %>%
  summarise(mean_v1 = mean(V1_scaled, na.rm = T)) %>%
  pivot_wider(names_from = country, values_from = mean_v1)

billboard_v2_cor_data <- billboard_reduced_data %>%
  group_by(country, song) %>%
  summarise(mean_v2 = mean(V2_scaled, na.rm = T)) %>%
  pivot_wider(names_from = country, values_from = mean_v2)

# Get all possible pairs of countries
country_pairs <- combn(top_countries_billboard_400, 2, simplify = FALSE)

# For each pair of countries, correlate song-wise mean ratings
get_corr <- function(pair, data) {
  c1 <- pair[1]
  c2 <- pair[2]

  vec1 <- data %>% pull(c1)
  vec2 <- data %>% pull(c2)

  pairwise_tmp_data <- bind_cols("c1" = vec1, "c2" = vec2) %>%
    drop_na

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
billboard_v1_cor_results <- map_dfr(
  country_pairs,
  get_corr,
  data = billboard_v1_cor_data
) %>%
  mutate(variable = "Valence")

billboard_v2_cor_results <- map_dfr(
  country_pairs,
  get_corr,
  data = billboard_v2_cor_data
) %>%
  mutate(variable = "Arousal")

billboard_cor_results <- bind_rows(
  billboard_v1_cor_results,
  billboard_v2_cor_results
)

#  Compute average r value for each latent dimension
billboard_mean_cors <- billboard_cor_results %>%
  group_by(variable) %>%
  summarise(mean_correlation = mean(correlation, na.rm = TRUE))


## Country-wise correlations using un-imputed data -----------------------

long_data <- billboard_data %>%
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
    print(paste0("Generating data for: ", gems))

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

  for (gems in gems9_labels) {
    print(paste0("Running correlation for: ", gems))

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

# Run correlations across countries with n > 400 participants
corrs_400 <- correlator(n_ppt = 400, long_data)


gems_mean_cors_400 <- corrs_400 %>%
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


gems_correlation_results_adjusted <- nc_correlator(
  n_ppt = 400,
  data = long_data
)

# calculate mean of adjusted correlations per emotion dimensions
gems_mean_cors_400_adjusted <- gems_correlation_results_adjusted %>%
  group_by(variable) %>%
  summarise(
    mean_adjusted_correlation = mean(corr, na.rm = T),
    median_adjusted_correlation = median(corr, na.rm = T),
    iqr_adjusted_correlation = IQR(corr, na.rm = T)
  )


gems_correlation_results_combined <- corrs_400 %>%
  dplyr::select(corr = correlation, variable, country) %>%
  bind_rows(
    adjusted = gems_correlation_results_adjusted,
    unadjusted = .,
    .id = "method"
  )


## Save results ------------------------------------
save(
  billboard_cor_results,
  gems_correlation_results_combined,
  gems_correlation_results_adjusted,
  gems_mean_cors_400_adjusted,
  gems_mean_cors_400,
  file = here("results", "06_billboard_cor_results.RData")
)
