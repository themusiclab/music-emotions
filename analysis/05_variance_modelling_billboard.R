# Study 1: Modelling variance in emotions (softimpute and raw data) ------------------

rm(list = ls())

library(pacman) # install.packages(pacman)
p_load(
  tidyverse,
  here,
  ggplot2,
  lme4,
  lmerTest,
  ggrepel # for scatterplots with overlapping dots
)

## Prep data for Study 1 ---------------------------------------------

billboard_data <- read_csv(here("data", "tml_wwl_data.csv.gz")) %>%
  filter(corpus == "billboard")

load(here("results", "04_weather_variance_data.rData")) # results from 04_weather_models.R

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

pretty_labels <- c(
  joyful_activation = "Joyful activation",
  nostalgia = "Nostalgia",
  peacefulness = "Peacefulness",
  power = "Power",
  sadness = "Sadness",
  tenderness = "Tenderness",
  tension = "Tension",
  transcendence = "Transcendence",
  wonder = "Wonder"
)


## Imputed data from countries with n > 400 -------------------
billboard_reduced_data <- readRDS(here(
  "results/imputations",
  "billboard_400.rds"
))

# number of countries with n > 400 participants
countries400 <- billboard_reduced_data$country %>% unique %>% length

# scale to aid modelling
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

# extract number of latent variables produced by SoftImpute (2)
latent_vars400 <- billboard_reduced_data %>%
  dplyr::select(
    starts_with("V"),
    -contains("_reversed"),
    -contains("_scaled")
  ) %>%
  ncol()

### Valence: variance components -------------------------
billboard_mod1 <- lmer(
  V1_scaled ~ (1 | user_id) + (1 | country) + (1 | song),
  billboard_reduced_data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)


billboard_mod1_tidy <- billboard_mod1 %>%
  VarCorr %>%
  as.data.frame() %>%
  dplyr::select(group = grp, variance = vcov) %>%
  mutate(prop_var = variance / sum(variance), factor = "Valence")

# the variances explained by the nesting structures are significant
ranova_billboard_mod1 <- ranova(billboard_mod1)

### Arousal: variance components -------------------------
billboard_mod2 <- lmer(
  V2_scaled ~ (1 | user_id) + (1 | country) + (1 | song),
  billboard_reduced_data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

billboard_mod2_tidy <- billboard_mod2 %>%
  VarCorr %>%
  as.data.frame() %>%
  dplyr::select(group = grp, variance = vcov) %>%
  mutate(prop_var = variance / sum(variance), factor = "Arousal")

# the variances explained by the nesting structures are significant
ranova_billboard_mod2 <- ranova(billboard_mod2)


### Figure 2: Variance components for Arousal and Valence, and for weather data ---------------

music_variance_data <- bind_rows(billboard_mod1_tidy, billboard_mod2_tidy) %>%
  mutate(
    group = factor(
      group,
      levels = c("Residual", "country", "song", "user_id"),
      labels = c("Residual", "Country", "Song", "Individual")
    )
  ) %>%
  filter(group != "Residual")


weather_variance_data <- weather_variance_data %>%
  mutate(
    group = case_when(
      group == "Individual" ~ "Station",
      TRUE ~ group
    )
  ) %>%
  mutate(
    factor = recode(
      factor,
      tavg_scaled = "Avg. Temp",
      tmax_scaled = "Max. Temp",
      dp01_scaled = "Days with rain",
      dsnd_scaled = "Days with snow",
      prcp_scaled = "Annual Precip.",
      emnt_scaled = "Ext. Min Temp"
    )
  )

# merge variance data for bar plot
plot_variance_data <- bind_rows(
  "Music Data" = music_variance_data,
  "Weather Data" = weather_variance_data,
  .id = "set"
) %>%
  filter(group != "Residual") %>%
  mutate(
    # make music data appear first
    set = factor(set, levels = c("Music Data", "Weather Data"))
  ) %>%
  # compute proportion of explained variance accounted for by each factor
  group_by(set, factor) %>%
  mutate(total_var_explained = sum(prop_var)) %>%
  mutate(scaling_factor = 100 / total_var_explained) %>%
  ungroup() %>%
  mutate(relative_prop = prop_var * scaling_factor)

# compute within vs. between variance ratio to label bars with
music_data_ratio_labels <- plot_variance_data %>%
  filter(set == "Music Data") %>%
  dplyr::select(group, factor, relative_prop) %>%
  pivot_wider(
    id_cols = factor,
    names_from = group,
    values_from = relative_prop
  ) %>%
  mutate(
    ind_ratio = round(Individual / Country),
    song_ratio = round(Song / Country)
  ) %>%
  dplyr::select(-Individual, -Song, -Country) %>%
  mutate(
    Individual = paste0(ind_ratio, ":1"),
    Song = paste0(song_ratio, ":1")
  ) %>%
  pivot_longer(cols = c(Individual, Song), names_to = "group")

plot_variance_data <- plot_variance_data %>%
  left_join(music_data_ratio_labels, by = c("factor", "group"))

# compute location for labelling the stacked barplot
label_positions <- plot_variance_data %>%
  filter(set == "Music Data") %>%
  group_by(set, factor) %>%
  mutate(
    group_ord = factor(group, levels = c("Song", "Individual", "Country"))
  ) %>%
  arrange(group_ord, .by_group = TRUE) %>%
  mutate(
    y_pos = cumsum(relative_prop) - relative_prop / 2
  ) %>%
  ungroup() %>%
  filter(!is.na(value))

# create stacked barplot (music and weather variance)
billboard_variance_barplot_combined <- ggplot() +
  # music data
  geom_col(
    data = plot_variance_data %>% filter(set == "Music Data"),
    aes(
      x = factor,
      y = relative_prop,
      fill = factor(group, levels = c("Country", "Individual", "Song"))
    ),
    position = "stack"
  ) +
  scale_fill_manual(
    name = "Popular Western Music",
    breaks = c("Country", "Individual", "Song"),
    values = c(
      "Country" = palette[12],
      "Song" = palette[11],
      "Individual" = palette[10]
    ),
    guide = guide_legend(order = 1)
  ) +
  ggnewscale::new_scale_fill() +
  # weather data
  geom_col(
    data = plot_variance_data %>% filter(set == "Weather Data"),
    aes(
      x = factor,
      y = relative_prop,
      fill = factor(group, levels = c("Country", "Station", "Date"))
    ),
    position = "stack"
  ) +
  scale_fill_manual(
    name = "Weather Data",
    breaks = c("Country", "Station", "Date"),
    values = c(
      "Date" = palette[11],
      "Station" = palette[10],
      "Country" = palette[12]
    )
  ) +
  facet_grid(set ~ ., scales = "free_y", switch = "y") +
  coord_flip() +
  theme_classic() +
  labs(
    x = NULL,
    y = "Prop. of explained variance"
  ) +
  theme(
    legend.position = "bottom",
    legend.key.size = unit(0.3, "cm"),
    legend.text = element_text(size = 6),
    text = element_text(size = 8),
    plot.margin = margin(5, 5, 5, 5),
    legend.box = "vertical"
  ) +
  geom_text(
    data = label_positions,
    aes(
      x = factor,
      y = y_pos,
      label = value
    ),
    size = 2.5,
    color = "white"
  )

## Imputed data from countries with n > 800 -------------------
billboard_reduced_data800 <- readRDS(here(
  "results/imputations",
  "billboard_800.rds"
))

# Number of countries with n > 800 participants
countries800 <- billboard_reduced_data800$country %>% unique %>% length

# Rescale columns to aid modelling
for (latent_var in names(
  billboard_reduced_data800 %>% dplyr::select(contains("V"))
)) {
  new_name <- paste0(latent_var, "_scaled")

  billboard_reduced_data800[new_name] <- scale(
    billboard_reduced_data800[latent_var],
    center = TRUE,
    scale = TRUE
  )
}

# Identify number of latent variables produced by SoftImpute
latent_vars800 <- billboard_reduced_data800 %>%
  dplyr::select(
    starts_with("V"),
    -contains("_reversed"),
    -contains("_scaled")
  ) %>%
  ncol()


### Valence: variance components -------------------------
billboard_mod1_800 <- lmer(
  V1_scaled ~ (1 | user_id) + (1 | country) + (1 | song),
  billboard_reduced_data800,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

billboard_mod1_800_tidy <- billboard_mod1_800 %>%
  VarCorr %>%
  as.data.frame() %>%
  dplyr::select(group = grp, variance = vcov) %>%
  mutate(prop_var = variance / sum(variance), factor = "Valence")

# The variances explained by the nesting structures are significant
ranova_billboard_mod1_800 <- ranova(billboard_mod1_800)

### Arousal: variance components -------------------------
billboard_mod2_800 <- lmer(
  V2_scaled ~ (1 | user_id) + (1 | country) + (1 | song),
  billboard_reduced_data800,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

billboard_mod2_800_tidy <- billboard_mod2_800 %>%
  VarCorr %>%
  as.data.frame() %>%
  dplyr::select(group = grp, variance = vcov) %>%
  mutate(prop_var = variance / sum(variance), factor = "Arousal")

# The variances explained by the nesting structures are significant
ranova_billboard_mod2_800 <- ranova(billboard_mod2_800)


## Un-imputed data and GEMS9 approach  ------------------------------

long_data <- billboard_data %>%
  drop_na(rating) %>%
  # Assign each gems-25 variable to relevant gems-9 factor
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
  summarise(rating = mean(rating)) %>%
  ungroup() %>%
  group_by(gems_9) %>%
  # mean centering: scores now indicate whether a song is more or less "transcendent" than average song
  mutate(rating_z = scale(rating, center = TRUE, scale = TRUE))


# for each gems-9 dimension, fit mixed model and extract variance components
billboard_gems_variance_mods <- list()

for (gems in gems9_labels) {
  tmp <- long_data %>%
    filter(gems_9 == gems) %>%
    drop_na(user_id, country, song, rating_z) %>%
    as.data.frame()

  print(paste0("running model for: ", gems))
  mod <- lmer(
    rating_z ~ (1 | user_id) + (1 | country) + (1 | song),
    tmp,
    control = lmerControl(optimizer = "bobyqa"),
    na.action = na.omit
  )

  billboard_gems_variance_mods[[gems]] <- mod

  name <- paste0(gems, "_ranova")
  billboard_gems_variance_mods[[name]] <- ranova(mod) %>%
    rownames_to_column %>%
    rename(p = `Pr(>Chisq)`)
}

# for each model, tidy the results and paste into one df
billboard_gems_variance_data <- data.frame()

for (gems in gems9_labels) {
  tidy_mod <- billboard_gems_variance_mods[[gems]] %>%
    VarCorr %>%
    as.data.frame() %>%
    dplyr::select(group = grp, variance = vcov) %>%
    mutate(prop_var = variance / sum(variance), factor = gems)

  billboard_gems_variance_data <- bind_rows(
    billboard_gems_variance_data,
    tidy_mod
  )
}

# compute within to between variance ratios
billboard_gems_variance_data <- billboard_gems_variance_data %>%
  filter(group != "Residual") %>%
  group_by(factor) %>%
  mutate(
    group = factor(
      group,
      levels = c("country", "song", "user_id"),
      labels = c("Country", "Song", "Individual")
    ),
    within_to_between = variance[group == "Individual"] /
      variance[group == "Country"],
    total_var_explained = sum(prop_var),
    scaling_factor = 100 / total_var_explained
  ) %>%
  ungroup() %>%
  mutate(relative_prop = prop_var * scaling_factor)


## Save results ---------------------------------------
save(
  billboard_mod1_tidy,
  billboard_mod2_tidy,
  billboard_mod1_800_tidy,
  billboard_mod2_800_tidy,
  billboard_gems_variance_data,
  billboard_variance_barplot_combined,
  weather_variance_data,
  gems9_labels,
  countries400,
  countries800,
  latent_vars400,
  latent_vars800,
  ranova_billboard_mod1,
  ranova_billboard_mod2,
  ranova_billboard_mod1_800,
  ranova_billboard_mod2_800,
  file = here("results", "05_billboard_variance_results.RData")
)
