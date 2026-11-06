# Study 2: Modelling variance in emotions (softimpute and raw data) ------------------

rm(list = ls())

library(pacman) # install.packages(pacman)
p_load(
  tidyverse,
  here,
  ggplot2,
  broom,
  ggExtra,
  lme4,
  lmerTest,
  ggrepel, # for scatterplots with overlapping dots
  emmeans # for pairwise comparisons
)

# load in billboard and variance results to use in big combined plot (figure 2)
load(here("results", "04_weather_variance_data.rData")) # results from 04_weather_models.R
load(here("results", "05_billboard_variance_results.RData")) # from 05_variance_modelling_billboard.R

## Prep data for Study 2 -------------------------------------------------------

nhs_data <- read_csv(here("data", "tml_wwl_data.csv.gz")) %>%
  filter(corpus == "nhs")

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

# Extract meta data about the original behavioural context of each song
song_data <- read_csv(here(
  "data",
  "NHSDiscography_Metadata.csv"
)) %>%
  mutate(song = (-1) * extract_numeric(song)) %>%
  dplyr::select(song, type)


## Imputed data from countries with n > 400 ---------------------

nhs_reduced_data <- readRDS(here("results/imputations", "nhs_400.rds")) %>%
  mutate(song = as.numeric(song)) %>%
  left_join(song_data, by = "song")

# Number of countries with > 400 participants
countries400_nhs <- nhs_reduced_data$country %>% unique %>% length

# Scale to aid modelling
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

# Extract number of latent variables produced by softimpute (2)
latent_vars400_nhs <- nhs_reduced_data %>%
  dplyr::select(
    starts_with("V"),
    -contains("_reversed"),
    -contains("_scaled")
  ) %>%
  ncol()

### Valence: variance components -------------------------------
nhs_mod1 <- lmer(
  V1_reversed_scaled ~ (1 | user_id) + (1 | country) + (1 | song),
  nhs_reduced_data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

nhs_mod1_tidy <- nhs_mod1 %>%
  VarCorr %>%
  as.data.frame() %>%
  dplyr::select(group = grp, variance = vcov) %>%
  mutate(prop_var = variance / sum(variance), factor = "Valence")

# The variances explained by the nesting structures are significant
ranova_res_v1_nhs <- ranova(nhs_mod1)

### Arousal: variance components -------------------------------
nhs_mod2 <- lmer(
  V2_scaled ~ (1 | user_id) + (1 | country) + (1 | song),
  nhs_reduced_data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

nhs_mod2_tidy <- nhs_mod2 %>%
  VarCorr %>%
  as.data.frame() %>%
  dplyr::select(group = grp, variance = vcov) %>%
  mutate(prop_var = variance / sum(variance), factor = "Arousal")

# The variances explained by the nesting structures are significant
ranova_res_v2_nhs <- ranova(nhs_mod2)


## Imputed data from countries with n > 800 ---------------------

nhs_reduced_data_800 <- readRDS(here("results/imputations", "nhs_800.rds")) %>%
  mutate(song = as.numeric(song)) %>%
  left_join(song_data, by = "song")

# Number of countries with > 400 participants
countries800_nhs <- nhs_reduced_data_800$country %>% unique %>% length

# Scale to aid modelling
for (latent_var in names(
  nhs_reduced_data_800 %>% dplyr::select(contains("V"))
)) {
  new_name <- paste0(latent_var, "_scaled")

  nhs_reduced_data_800[new_name] <- scale(
    nhs_reduced_data_800[latent_var],
    center = TRUE,
    scale = TRUE
  )
}

# Extract number of latent variables produced by softimpute (2)
latent_vars800_nhs <- nhs_reduced_data_800 %>%
  dplyr::select(
    starts_with("V"),
    -contains("_reversed"),
    -contains("_scaled")
  ) %>%
  ncol()

### Valence: variance components -------------------------------
nhs_mod1_800 <- lmer(
  V1_reversed_scaled ~ (1 | user_id) + (1 | country) + (1 | song),
  nhs_reduced_data_800,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

nhs_mod1_800_tidy <- nhs_mod1_800 %>%
  VarCorr %>%
  as.data.frame() %>%
  dplyr::select(group = grp, variance = vcov) %>%
  mutate(prop_var = variance / sum(variance), factor = "Valence")

# The variances explained by the nesting structures are significant
ranova_res_v1_nhs_800 <- ranova(nhs_mod1_800)

### Arousal: variance components -------------------------------
nhs_mod2_800 <- lmer(
  V2_scaled ~ (1 | user_id) + (1 | country) + (1 | song),
  nhs_reduced_data_800,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

nhs_mod2_800_tidy <- nhs_mod2_800 %>%
  VarCorr %>%
  as.data.frame() %>%
  dplyr::select(group = grp, variance = vcov) %>%
  mutate(prop_var = variance / sum(variance), factor = "Arousal")

# The variances explained by the nesting structures are significant
ranova_res_v2_nhs_800 <- ranova(nhs_mod2_800)


## Un-imputed data and GEMS9 approach  ------------------------------

long_data <- nhs_data %>%
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


# variance models
nhs_gems_variance_mods <- list()

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

  nhs_gems_variance_mods[[gems]] <- mod

  name <- paste0(gems, "_ranova")
  nhs_gems_variance_mods[[name]] <- ranova(mod) %>%
    rownames_to_column %>%
    mutate(p = `Pr(>Chisq)`)
}

# for each model, tidy the results and paste into one df
nhs_gems_variance_data <- data.frame()

for (gems in gems9_labels) {
  tidy_mod <- nhs_gems_variance_mods[[gems]] %>%
    VarCorr %>%
    as.data.frame() %>%
    dplyr::select(group = grp, variance = vcov) %>%
    mutate(prop_var = variance / sum(variance), factor = gems)
  nhs_gems_variance_data <- bind_rows(nhs_gems_variance_data, tidy_mod)
}

# compute within to between variance ratios
nhs_gems_variance_data <- nhs_gems_variance_data %>%
  mutate(
    group = factor(
      group,
      levels = c("country", "song", "user_id"),
      labels = c("Country", "Song", "Individual")
    )
  ) %>%
  filter(group != "Residual") %>%
  group_by(factor) %>%
  mutate(
    within_to_between = variance[group == "Individual"] /
      variance[group == "Country"]
  ) %>%
  group_by(factor) %>%
  mutate(total_var_explained = sum(prop_var)) %>%
  mutate(scaling_factor = 100 / total_var_explained) %>%
  ungroup() %>%
  mutate(relative_prop = prop_var * scaling_factor)


# get variance data from different datasets for plots
variance_data <- bind_rows(billboard_mod1_tidy, billboard_mod2_tidy) %>%
  mutate(
    group = factor(
      group,
      levels = c("Residual", "country", "song", "user_id"),
      labels = c("Residual", "Country", "Song", "Individual")
    )
  )


nhs_variance_data <- bind_rows(nhs_mod1_tidy, nhs_mod2_tidy) %>%
  mutate(
    group = factor(
      group,
      levels = c("Residual", "country", "song", "user_id"),
      labels = c("Residual", "Country", "Song", "Individual")
    )
  )


weather_data <- weather_variance_data %>%
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

# combine datasets for variance bar plots
icc_long_plot <- bind_rows(
  "Weather Data" = weather_data,
  "Popular" = variance_data,
  "Unfamiliar" = nhs_variance_data,
  .id = "set"
) %>%
  filter(group != "Residual") %>%
  mutate(
    # make music data appear first
    set = factor(
      set,
      levels = c("Popular", "Unfamiliar", "Weather Data")
    )
  ) %>%
  group_by(set, factor) %>%
  mutate(total_var_explained = sum(prop_var)) %>%
  mutate(scaling_factor = 100 / total_var_explained) %>%
  ungroup() %>%
  mutate(relative_prop = prop_var * scaling_factor)


## Original song contexts predict valence/arousal -----------------

cluster_data <- nhs_reduced_data %>%
  group_by(song, type) %>%
  summarise(
    Valence = mean(V1_reversed_scaled, na.rm = T),
    Arousal = mean(V2_scaled, na.rm = T)
  ) %>%
  mutate(type = as.factor(type))


### Valence is predicted by original song context: dance -----------------
valence_by_type <- lm(Valence ~ type - 1, cluster_data)
valence_by_type %>% summary
valence_by_type_emmeans <- emmeans(valence_by_type, pairwise ~ type)$contrasts

# mixed model version
lmer(V1_reversed ~ type - 1 + (1 | user_id), nhs_reduced_data) %>%
  emmeans(pairwise ~ type)

### Arousal is predicted by original song context: dance and lullaby -----------------
arousal_by_type <- lm(Arousal ~ type - 1, cluster_data)
arousal_by_type %>% summary
arousal_by_type_emmeans <- emmeans(arousal_by_type, pairwise ~ type)$contrasts

## Original song contexts predict gems-9 -----------------

cluster_data_gems <- long_data %>%
  mutate(song = as.numeric(song)) %>%
  left_join(song_data, by = "song") %>%
  group_by(song, type, gems_9) %>%
  summarise(
    mean = mean(rating_z, na.rm = T)
  ) %>%
  mutate(type = as.factor(type))

lm(
  mean ~ type - 1,
  cluster_data_gems %>% filter(gems_9 == "joyful_activation")
) %>%
  emmeans(pairwise ~ type)
lm(mean ~ type - 1, cluster_data_gems %>% filter(gems_9 == "nostalgia")) %>%
  emmeans(pairwise ~ type)
lm(mean ~ type - 1, cluster_data_gems %>% filter(gems_9 == "peacefulness")) %>%
  emmeans(pairwise ~ type)
lm(mean ~ type - 1, cluster_data_gems %>% filter(gems_9 == "power")) %>%
  emmeans(pairwise ~ type)
lm(mean ~ type - 1, cluster_data_gems %>% filter(gems_9 == "sadness")) %>%
  emmeans(pairwise ~ type)
lm(mean ~ type - 1, cluster_data_gems %>% filter(gems_9 == "tenderness")) %>%
  emmeans(pairwise ~ type)
lm(mean ~ type - 1, cluster_data_gems %>% filter(gems_9 == "tension")) %>%
  emmeans(pairwise ~ type)
lm(mean ~ type - 1, cluster_data_gems %>% filter(gems_9 == "transcendence")) %>%
  emmeans(pairwise ~ type)
lm(mean ~ type - 1, cluster_data_gems %>% filter(gems_9 == "wonder")) %>%
  emmeans(pairwise ~ type)

# cluster_data_gems_multilevel <- long_data %>%
#   mutate(song = as.numeric(song)) %>%
#   left_join(song_data, by = "song") %>%
#   mutate(type = as.factor(type)) %>%
#   ungroup() %>%
#   group_by(gems_9) %>%
#   mutate(rating_scaled = scale(rating, center = T, scale = F))
#
#
# lmer(
#   rating_z ~ type - 1 + (1 | user_id) + (1 | song),
#   cluster_data_gems_multilevel %>% filter(gems_9 == "joyful_activation")
# ) %>%
#   summary
#
# lmer(
#   rating_z ~ type - 1 + (1 | user_id) + (1 | song),
#   cluster_data_gems_multilevel %>% filter(gems_9 == "nostalgia")
# ) %>%
#   summary
#
# lmer(
#   rating_z ~ type - 1 + (1 | user_id) + (1 | song),
#   cluster_data_gems_multilevel %>% filter(gems_9 == "peacefulness")
# ) %>%
#   summary
#
# lmer(
#   rating_z ~ type - 1 + (1 | user_id) + (1 | song),
#   cluster_data_gems_multilevel %>% filter(gems_9 == "power")
# ) %>%
#   summary
#
# lmer(
#   rating_z ~ type - 1 + (1 | user_id) + (1 | song),
#   cluster_data_gems_multilevel %>% filter(gems_9 == "sadness")
# ) %>%
#   summary
#
# lmer(
#   rating_z ~ type - 1 + (1 | user_id) + (1 | song),
#   cluster_data_gems_multilevel %>% filter(gems_9 == "tenderness")
# ) %>%
#   summary
#
# lmer(
#   rating_z ~ type - 1 + (1 | user_id) + (1 | song),
#   cluster_data_gems_multilevel %>% filter(gems_9 == "tension")
# ) %>%
#   summary
#
# lmer(
#   rating_z ~ type - 1 + (1 | user_id) + (1 | song),
#   cluster_data_gems_multilevel %>% filter(gems_9 == "transcendence")
# ) %>%
#   summary
#
# lmer(
#   rating_z ~ type - 1 + (1 | user_id) + (1 | song),
#   cluster_data_gems_multilevel %>% filter(gems_9 == "wonder")
# ) %>%
#   summary
#
#
# mixed_mod <- lmer(
#   rating_scaled ~ type * gems_9 + (1 | user_id) + (1 | song),
#   cluster_data_gems_multilevel
# )
#
# em_mixed_mod <- emmeans(mixed_mod, ~ type * gems_9) %>%
#   as.data.frame() %>%
#   mutate(
#     significant = ifelse(
#       (asymp.LCL < 0 & asymp.UCL < 0) | (asymp.LCL > 0 & asymp.UCL > 0),
#       TRUE,
#       FALSE
#     )
#   )
#
# ggplot(em_mixed_mod, aes(x = gems_9, y = emmean)) +
#   facet_wrap(facets = "type") +
#   geom_col(aes(alpha = significant)) +
#   geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
#   labs(
#     title = "Relative Emotion Profiles across Song Types",
#     x = "Emotion",
#     y = "Estimated Intensity"
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 70, hjust = 1))

## Perceived song contexts predict valence/arousal ----------------

# read in form-function data from Yurdum et al.
ff_data <- read_csv(here("data", "yurdum2023_data.csv")) %>%
  group_by(song, study, songfunction) %>%
  summarise(
    mean_danc = mean(danc, na.rm = T),
    mean_baby = mean(baby, na.rm = T),
    mean_heal = mean(heal, na.rm = T),
    mean_love = mean(love, na.rm = T)
  ) %>%
  ungroup() %>%
  group_by(study) %>%
  mutate(
    mean_danc = scale(mean_danc),
    mean_baby = scale(mean_baby),
    mean_heal = scale(mean_heal),
    mean_love = scale(mean_love)
  ) %>%
  rename(type = songfunction) %>%
  mutate(
    type = case_when(
      type == "danc" ~ "Dance",
      type == "baby" ~ "Lullaby",
      type == "heal" ~ "Healing",
      type == "love" ~ "Love",
      T ~ NA
    )
  ) %>%
  drop_na(type)

# variables starting with "mean_" indicate mean ratings of how appropriate a song is for each behavioural context
ff_data <- cluster_data %>%
  left_join(ff_data, by = c("song", "type"))

ff_mods <- list()

ff_mods[["web"]][["danc_v1"]] <- lm(
  mean_danc ~ Valence,
  ff_data %>% filter(study == "web")
) %>%
  tidy()
ff_mods[["web"]][["baby_v1"]] <- lm(
  mean_baby ~ Valence,
  ff_data %>% filter(study == "web")
) %>%
  tidy()
ff_mods[["web"]][["heal_v1"]] <- lm(
  mean_heal ~ Valence,
  ff_data %>% filter(study == "web")
) %>%
  tidy()
ff_mods[["web"]][["love_v1"]] <- lm(
  mean_love ~ Valence,
  ff_data %>% filter(study == "web")
) %>%
  tidy()

ff_mods[["web"]][["danc_v2"]] <- lm(
  mean_danc ~ Arousal,
  ff_data %>% filter(study == "web")
) %>%
  tidy()
ff_mods[["web"]][["baby_v2"]] <- lm(
  mean_baby ~ Arousal,
  ff_data %>% filter(study == "web")
) %>%
  tidy()
ff_mods[["web"]][["heal_v2"]] <- lm(
  mean_heal ~ Arousal,
  ff_data %>% filter(study == "web")
) %>%
  tidy()
ff_mods[["web"]][["love_v2"]] <- lm(
  mean_love ~ Arousal,
  ff_data %>% filter(study == "web")
) %>%
  tidy()

# small scale soc participants
ff_mods[["field"]][["danc_v1"]] <- lm(
  mean_danc ~ Valence,
  ff_data %>% filter(study == "field")
) %>%
  tidy()
ff_mods[["field"]][["baby_v1"]] <- lm(
  mean_baby ~ Valence,
  ff_data %>% filter(study == "field")
) %>%
  tidy()
ff_mods[["field"]][["heal_v1"]] <- lm(
  mean_heal ~ Valence,
  ff_data %>% filter(study == "field")
) %>%
  tidy()
ff_mods[["field"]][["love_v1"]] <- lm(
  mean_love ~ Valence,
  ff_data %>% filter(study == "field")
) %>%
  tidy()

ff_mods[["field"]][["danc_v2"]] <- lm(
  mean_danc ~ Arousal,
  ff_data %>% filter(study == "field")
) %>%
  tidy()
ff_mods[["field"]][["baby_v2"]] <- lm(
  mean_baby ~ Arousal,
  ff_data %>% filter(study == "field")
) %>%
  tidy()
ff_mods[["field"]][["heal_v2"]] <- lm(
  mean_heal ~ Arousal,
  ff_data %>% filter(study == "field")
) %>%
  tidy()
ff_mods[["field"]][["love_v2"]] <- lm(
  mean_love ~ Arousal,
  ff_data %>% filter(study == "field")
) %>%
  tidy()


# original context vs perceived context
ff_data <- ff_data %>%
  mutate(
    type_danc = ifelse(type == "Dance", 1, 0),
    type_baby = ifelse(type == "Lullaby", 1, 0),
    type_heal = ifelse(type == "Healing", 1, 0),
    type_love = ifelse(type == "Love", 1, 0)
  )

lm(Arousal ~ mean_danc, ff_data %>% filter(study == "field")) %>% summary
lm(Arousal ~ type_danc, ff_data %>% filter(study == "field")) %>% summary

lm(Arousal ~ mean_baby, ff_data %>% filter(study == "field")) %>% summary
lm(Arousal ~ type_baby, ff_data %>% filter(study == "field")) %>% summary

lm(Arousal ~ mean_heal, ff_data %>% filter(study == "field")) %>% summary
lm(Arousal ~ type_heal, ff_data %>% filter(study == "field")) %>% summary

lm(Arousal ~ mean_love, ff_data %>% filter(study == "field")) %>% summary
lm(Arousal ~ type_love, ff_data %>% filter(study == "field")) %>% summary

lm(Valence ~ mean_danc, ff_data %>% filter(study == "field")) %>% summary
lm(Valence ~ type_danc, ff_data %>% filter(study == "field")) %>% summary

lm(Valence ~ mean_baby, ff_data %>% filter(study == "field")) %>% summary
lm(Valence ~ type_baby, ff_data %>% filter(study == "field")) %>% summary

lm(Valence ~ mean_heal, ff_data %>% filter(study == "field")) %>% summary
lm(Valence ~ type_heal, ff_data %>% filter(study == "field")) %>% summary

lm(Valence ~ mean_love, ff_data %>% filter(study == "field")) %>% summary
lm(Valence ~ type_love, ff_data %>% filter(study == "field")) %>% summary

ff_data_long <- ff_data %>%
  pivot_longer(
    cols = c(Valence, Arousal),
    names_to = "dimension",
    values_to = "rating"
  ) %>%
  pivot_longer(
    cols = c(mean_danc, mean_baby, mean_heal, mean_love),
    names_to = "target",
    values_to = "mean_perception_rating"
  ) %>%
  mutate(
    target = case_when(
      target == "mean_danc" ~ "Dancing",
      target == "mean_baby" ~ "Soothing infants",
      target == "mean_heal" ~ "Healing illness",
      target == "mean_love" ~ "Expressing love"
    ),
    type = as.factor((type))
  )


## Save results ----------------------------------------
save(
  nhs_mod1_tidy,
  nhs_mod2_tidy,
  nhs_mod1_800_tidy,
  nhs_mod2_800_tidy,
  ranova_res_v1_nhs,
  ranova_res_v2_nhs,
  ranova_res_v1_nhs_800,
  ranova_res_v2_nhs_800,
  latent_vars400_nhs,
  countries400_nhs,
  nhs_gems_variance_data,
  cluster_data,
  ff_data_long,
  icc_long_plot,
  valence_by_type,
  valence_by_type_emmeans,
  arousal_by_type,
  arousal_by_type_emmeans,
  ff_mods,
  cluster_data_gems,
  file = here("results", "07_nhs_variance_results.RData")
)
