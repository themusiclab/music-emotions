# language and culture replications

rm(list = ls())

library(pacman) # install.packages(pacman)
p_load(
  tidyverse,
  here,
  ggplot2,
  lme4,
  lmerTest,
  ggrepel, # for scatterplots with overlapping dots
  fuzzyjoin
)

full_data <- read_csv(here("data", "tml_wwl_data.csv.gz")) %>%
  rename("source" = id)

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

# grab language and language family codes from https://glottolog.org/meta/downloads version 5.3
glot <- read_csv(here("data", "languoid.csv")) %>%
  rename("language" = name)

full_data_withlangs <- full_data %>%
  left_join(by = "language", glot)


matched <- full_data_withlangs %>%
  group_by(language, level, family_id, parent_id, id) %>%
  summarise(n = n())

# exact matches are not always available in glottolog
# I have manually added in info for these remaining langs, using info from glottolog.org, so these are imperfect. The goal is assigning languages to a larger shared group.
# In some cases I renamed the language to a close available language, as long as the language family remained consistent (as that is the level of analysis)

matched <- full_data %>%
  mutate(
    language = case_when(
      language == "Adyghe" ~ "Abkhaz-Abaza",
      language == "Algerian" ~ "Algerian Arabic",
      language == "Apache" ~ "Apachean",
      language == "Armenian" ~ "Western Armenian",
      language == "Aymara" ~ "Aymaran",
      language == "Azeri" ~ "Western Azerbaijani",
      language == "Bantu" ~ "Bantu B20B", # is its own lang fam
      language == "Bemba" ~ "Bemba (M.40)",
      language == "Bicol" ~ "Iriga Bicolano",
      language == "Bisaya" ~ "Bisayan",
      language == "Bosnian" ~ "Bosnian Standard",
      language == "Calib" ~ "Galibi Calib",
      language == "Ch'ol" ~ "Chol",
      language == "Chinese/Cantonese/Yue" ~ "Cantonese",
      language == "Chinese/Gan" ~ "Gan Chinese",
      language == "Chinese/Hakka" ~ "Hakka Chinese",
      language == "Chinese/Hokkien" ~ "Taibei Hokkien",
      language == "Chinese/Mandarin" ~ "Mandarin Chinese",
      language == "Chinese/Min Bei" ~ "Min Bei Chinese",
      language == "Chinese/Pu Xian" ~ "Pu-Xian Chinese",
      language == "Chinese/Qiong Wen" ~ "Hainanese",
      language ==
        "Chinese/Shaozhou Tuhua" ~ "Qujiang Hakka-Shibei Shaoguan Tuhua",
      language == "Chinese/Taiwanese/Min Nan/Hokkien" ~ "Min Nan Chinese",
      language == "Chinese/Wu/Shanghainese" ~ "Wu Chinese",
      language == "Chinese/Xianghua" ~ "Waxianghua",
      language == "Chinese/Yue" ~ "Yue Chinese",
      language == "Croatian" ~ "Croatian Standard",
      language == "Farsi" ~ "Fars Persian",
      language == "Gaelic" ~ "Scottish Gaelic",
      language == "Hebrew" ~ "Standard Hebrew",
      language == "Hmong" ~ "Hmongic",
      language == "Ilongo" ~ "Ilongot",
      language == "Indonesian" ~ "Standard Indonesian",
      language == "Khmer" ~ "Khmeric",
      language == "Konkani" ~ "Northern Konkani",
      language == "Kyrgyz" ~ "Kirghiz",
      language == "Luxembourgish" ~ "Luxemburgish",
      language == "Malagasy" ~ "Tanosy Malagasy",
      language == "Malay" ~ "Standard Malay",
      language == "Ndebele" ~ "Zimbabwean Ndebele",
      language == "Nepalese" ~ "Nepali",
      language == "Newari" ~ "Newaric",
      language == "Oriya" ~ "Southern Oriya",
      language == "Punjabi" ~ "Rathi (Panjabi)",
      language == "Quechua" ~ "Quechua-Puquina",
      language == "Serbian" ~ "Serbian Standard",
      language == "Sinhalese" ~ "Sinhalaic",
      language == "Slovene" ~ "Slovenian",
      language == "Sranan" ~ "Sranan Tongo",
      language == "Swiss German" ~ "Central Alemannic",
      language == "Te Reo Māori" ~ "Maori",
      language == "Uyghur" ~ "Uighur",
      language == "Yiddish" ~ "Western Yiddish",
      language == "Chinese/Xiang/Hunanese" ~ "Xiang Chinese",
      language == "Nahuatl" ~ "Classical Nahuatl",
      language == "Sûdovian" ~ "Sudovian",
      language == "Delaware" ~ "Delawaran",
      language == "PcovariatesApiamento" ~ "Papiamento",
      language == "Inuktitut" ~ "Baffin Inuktitut",
      language == "Sardinian" ~ "Sassarese Sardinian",
      language == "Mayan" ~ "Core Mayan",
      language == "Chorti" ~ "Chorti-Cholti",
      language == "Tongan" ~ "Tonga (Chopi)",
      language == "Chinese/Min Zhong" ~ "Min Zhong Chinese",
      language == "Chinese/Min Dong" ~ "Min Dong Chinese",
      language == "Fulfulde" ~ "Borgu Fulfulde",
      language == "Klallam" ~ "Eastern Klallam",
      language == "Frankish" ~ "Old Dutch-Old Frankish",
      language == "Ojibwe" ~ "Nuclear Ojibwe",
      TRUE ~ language
    )
  ) %>%
  left_join(by = "language", glot) %>%
  dplyr::select(language, family_id, id, level) %>%
  distinct() %>%
  mutate(analysis_group = case_when(level == "family" ~ id, TRUE ~ family_id))


full_data <- full_data %>%
  left_join(matched, by = "language")


long_data <- full_data %>%
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
  group_by(user_id, corpus, country, language, analysis_group, song, gems_9) %>%
  # calculate gems-9 score by averaging all available ratings for that factor
  summarise(rating = mean(rating)) %>%
  ungroup() %>%
  group_by(gems_9) %>%
  # mean centering: scores now indicate whether a song is more or less "transcendent" than average song
  mutate(rating_z = scale(rating, center = TRUE, scale = TRUE))

## Language  replication for Billboard songs ----------
billboard_data_lang_ind <- long_data %>%
  filter(corpus == "billboard") %>%
  filter(!is.na(analysis_group))

# for each gems-9 dimension, fit mixed model and extract variance components
billboard_gems_variance_mods_lang_ind <- list()

for (gems in gems9_labels) {
  tmp <- billboard_data_lang_ind %>%
    filter(gems_9 == gems) %>%
    drop_na(user_id, language, song, rating_z) %>%
    as.data.frame()

  print(paste0("running model for: ", gems))
  mod <- lmer(
    rating_z ~ (1 | user_id) + (1 | language) + (1 | song),
    tmp,
    control = lmerControl(optimizer = "bobyqa"),
    na.action = na.omit
  )

  billboard_gems_variance_mods_lang_ind[[gems]] <- mod

  name <- paste0(gems, "_ranova")
  billboard_gems_variance_mods_lang_ind[[name]] <- ranova(mod) %>%
    rownames_to_column %>%
    rename(p = `Pr(>Chisq)`)
}

# for each model, tidy the results and paste into one df
billboard_gems_variance_data_lang_ind <- data.frame()

for (gems in gems9_labels) {
  tidy_mod <- billboard_gems_variance_mods_lang_ind[[gems]] %>%
    VarCorr %>%
    as.data.frame() %>%
    dplyr::select(group = grp, variance = vcov) %>%
    mutate(prop_var = variance / sum(variance), factor = gems)

  billboard_gems_variance_data_lang_ind <- bind_rows(
    billboard_gems_variance_data_lang_ind,
    tidy_mod
  )
}

# compute within to between variance ratios
billboard_gems_variance_data_lang_ind <- billboard_gems_variance_data_lang_ind %>%
  filter(group != "Residual") %>%
  group_by(factor) %>%
  mutate(
    group = factor(
      group,
      levels = c("language", "song", "user_id"),
      labels = c("Language", "Song", "Individual")
    ),
    within_to_between = variance[group == "Individual"] /
      variance[group == "Language"],
    total_var_explained = sum(prop_var),
    scaling_factor = 100 / total_var_explained
  ) %>%
  ungroup() %>%
  mutate(relative_prop = prop_var * scaling_factor)


## Language family replication for Billboard songs ----------
billboard_data_lang <- long_data %>%
  filter(corpus == "billboard") %>%
  filter(!is.na(analysis_group))

# for each gems-9 dimension, fit mixed model and extract variance components
billboard_gems_variance_mods_lang <- list()

for (gems in gems9_labels) {
  tmp <- billboard_data_lang %>%
    filter(gems_9 == gems) %>%
    drop_na(user_id, analysis_group, song, rating_z) %>%
    as.data.frame()

  print(paste0("running model for: ", gems))
  mod <- lmer(
    rating_z ~ (1 | user_id) + (1 | analysis_group) + (1 | song),
    tmp,
    control = lmerControl(optimizer = "bobyqa"),
    na.action = na.omit
  )

  billboard_gems_variance_mods_lang[[gems]] <- mod

  name <- paste0(gems, "_ranova")
  billboard_gems_variance_mods_lang[[name]] <- ranova(mod) %>%
    rownames_to_column %>%
    rename(p = `Pr(>Chisq)`)
}

# for each model, tidy the results and paste into one df
billboard_gems_variance_data_lang <- data.frame()

for (gems in gems9_labels) {
  tidy_mod <- billboard_gems_variance_mods_lang[[gems]] %>%
    VarCorr %>%
    as.data.frame() %>%
    dplyr::select(group = grp, variance = vcov) %>%
    mutate(prop_var = variance / sum(variance), factor = gems)

  billboard_gems_variance_data_lang <- bind_rows(
    billboard_gems_variance_data_lang,
    tidy_mod
  )
}

# compute within to between variance ratios
billboard_gems_variance_data_lang <- billboard_gems_variance_data_lang %>%
  filter(group != "Residual") %>%
  group_by(factor) %>%
  mutate(
    group = factor(
      group,
      levels = c("analysis_group", "song", "user_id"),
      labels = c("Language Family", "Song", "Individual")
    ),
    within_to_between = variance[group == "Individual"] /
      variance[group == "Language Family"],
    total_var_explained = sum(prop_var),
    scaling_factor = 100 / total_var_explained
  ) %>%
  ungroup() %>%
  mutate(relative_prop = prop_var * scaling_factor)


## without indo -------------

billboard_data_lang2 <- long_data %>%
  filter(corpus == "billboard") %>%
  filter(analysis_group != "indo1319") %>%
  filter(!is.na(analysis_group))

# for each gems-9 dimension, fit mixed model and extract variance components
billboard_gems_variance_mods_lang2 <- list()

for (gems in gems9_labels) {
  tmp <- billboard_data_lang2 %>%
    filter(gems_9 == gems) %>%
    drop_na(user_id, analysis_group, song, rating_z) %>%
    as.data.frame()

  print(paste0("running model for: ", gems))
  mod <- lmer(
    rating_z ~ (1 | user_id) + (1 | analysis_group) + (1 | song),
    tmp,
    control = lmerControl(optimizer = "bobyqa"),
    na.action = na.omit
  )

  billboard_gems_variance_mods_lang2[[gems]] <- mod

  name <- paste0(gems, "_ranova")
  billboard_gems_variance_mods_lang2[[name]] <- ranova(mod) %>%
    rownames_to_column %>%
    rename(p = `Pr(>Chisq)`)
}

# for each model, tidy the results and paste into one df
billboard_gems_variance_data_lang2 <- data.frame()

for (gems in gems9_labels) {
  tidy_mod <- billboard_gems_variance_mods_lang2[[gems]] %>%
    VarCorr %>%
    as.data.frame() %>%
    dplyr::select(group = grp, variance = vcov) %>%
    mutate(prop_var = variance / sum(variance), factor = gems)

  billboard_gems_variance_data_lang2 <- bind_rows(
    billboard_gems_variance_data_lang2,
    tidy_mod
  )
}

# compute within to between variance ratios
billboard_gems_variance_data_lang2 <- billboard_gems_variance_data_lang2 %>%
  filter(group != "Residual") %>%
  group_by(factor) %>%
  mutate(
    group = factor(
      group,
      levels = c("analysis_group", "song", "user_id"),
      labels = c("Language Family", "Song", "Individual")
    ),
    within_to_between = variance[group == "Individual"] /
      variance[group == "Language Family"],
    total_var_explained = sum(prop_var),
    scaling_factor = 100 / total_var_explained
  ) %>%
  ungroup() %>%
  mutate(relative_prop = prop_var * scaling_factor)

## Individual language replication for NHS songs ----------
nhs_data_lang <- long_data %>%
  filter(corpus == "nhs") %>%
  filter(!is.na(analysis_group))

# for each gems-9 dimension, fit mixed model and extract variance components
nhs_gems_variance_mods_lang_ind <- list()

for (gems in gems9_labels) {
  tmp <- nhs_data_lang %>%
    filter(gems_9 == gems) %>%
    drop_na(user_id, language, song, rating_z) %>%
    as.data.frame()

  print(paste0("running model for: ", gems))
  mod <- lmer(
    rating_z ~ (1 | user_id) + (1 | language) + (1 | song),
    tmp,
    control = lmerControl(optimizer = "bobyqa"),
    na.action = na.omit
  )

  nhs_gems_variance_mods_lang_ind[[gems]] <- mod

  name <- paste0(gems, "_ranova")
  nhs_gems_variance_mods_lang_ind[[name]] <- ranova(mod) %>%
    rownames_to_column %>%
    rename(p = `Pr(>Chisq)`)
}

# for each model, tidy the results and paste into one df
nhs_gems_variance_data_lang_ind <- data.frame()

for (gems in gems9_labels) {
  tidy_mod <- nhs_gems_variance_mods_lang_ind[[gems]] %>%
    VarCorr %>%
    as.data.frame() %>%
    dplyr::select(group = grp, variance = vcov) %>%
    mutate(prop_var = variance / sum(variance), factor = gems)

  nhs_gems_variance_data_lang_ind <- bind_rows(
    nhs_gems_variance_data_lang_ind,
    tidy_mod
  )
}

# compute within to between variance ratios
nhs_gems_variance_data_lang_ind <- nhs_gems_variance_data_lang_ind %>%
  filter(group != "Residual") %>%
  group_by(factor) %>%
  mutate(
    group = factor(
      group,
      levels = c("language", "song", "user_id"),
      labels = c("Language", "Song", "Individual")
    ),
    within_to_between = variance[group == "Individual"] /
      variance[group == "Language"],
    total_var_explained = sum(prop_var),
    scaling_factor = 100 / total_var_explained
  ) %>%
  ungroup() %>%
  mutate(relative_prop = prop_var * scaling_factor)


## Language replication for NHS songs ----------
nhs_data_lang <- long_data %>%
  filter(corpus == "nhs") %>%
  filter(!is.na(analysis_group))

# for each gems-9 dimension, fit mixed model and extract variance components
nhs_gems_variance_mods_lang <- list()

for (gems in gems9_labels) {
  tmp <- nhs_data_lang %>%
    filter(gems_9 == gems) %>%
    drop_na(user_id, analysis_group, song, rating_z) %>%
    as.data.frame()

  print(paste0("running model for: ", gems))
  mod <- lmer(
    rating_z ~ (1 | user_id) + (1 | analysis_group) + (1 | song),
    tmp,
    control = lmerControl(optimizer = "bobyqa"),
    na.action = na.omit
  )

  nhs_gems_variance_mods_lang[[gems]] <- mod

  name <- paste0(gems, "_ranova")
  nhs_gems_variance_mods_lang[[name]] <- ranova(mod) %>%
    rownames_to_column %>%
    rename(p = `Pr(>Chisq)`)
}

# for each model, tidy the results and paste into one df
nhs_gems_variance_data_lang <- data.frame()

for (gems in gems9_labels) {
  tidy_mod <- nhs_gems_variance_mods_lang[[gems]] %>%
    VarCorr %>%
    as.data.frame() %>%
    dplyr::select(group = grp, variance = vcov) %>%
    mutate(prop_var = variance / sum(variance), factor = gems)

  nhs_gems_variance_data_lang <- bind_rows(
    nhs_gems_variance_data_lang,
    tidy_mod
  )
}

# compute within to between variance ratios
nhs_gems_variance_data_lang <- nhs_gems_variance_data_lang %>%
  filter(group != "Residual") %>%
  group_by(factor) %>%
  mutate(
    group = factor(
      group,
      levels = c("analysis_group", "song", "user_id"),
      labels = c("Language Family", "Song", "Individual")
    ),
    within_to_between = variance[group == "Individual"] /
      variance[group == "Language Family"],
    total_var_explained = sum(prop_var),
    scaling_factor = 100 / total_var_explained
  ) %>%
  ungroup() %>%
  mutate(relative_prop = prop_var * scaling_factor)

## Language replication for NHS songs without indo ----------
nhs_data_lang2 <- long_data %>%
  filter(corpus == "nhs") %>%
  filter(analysis_group != "indo1319") %>%
  filter(!is.na(analysis_group))

# for each gems-9 dimension, fit mixed model and extract variance components
nhs_gems_variance_mods_lang2 <- list()

for (gems in gems9_labels) {
  tmp <- nhs_data_lang2 %>%
    filter(gems_9 == gems) %>%
    drop_na(user_id, analysis_group, song, rating_z) %>%
    as.data.frame()

  print(paste0("running model for: ", gems))
  mod <- lmer(
    rating_z ~ (1 | user_id) + (1 | analysis_group) + (1 | song),
    tmp,
    control = lmerControl(optimizer = "bobyqa"),
    na.action = na.omit
  )

  nhs_gems_variance_mods_lang2[[gems]] <- mod

  name <- paste0(gems, "_ranova")
  nhs_gems_variance_mods_lang2[[name]] <- ranova(mod) %>%
    rownames_to_column %>%
    rename(p = `Pr(>Chisq)`)
}

# for each model, tidy the results and paste into one df
nhs_gems_variance_data_lang2 <- data.frame()

for (gems in gems9_labels) {
  tidy_mod <- nhs_gems_variance_mods_lang2[[gems]] %>%
    VarCorr %>%
    as.data.frame() %>%
    dplyr::select(group = grp, variance = vcov) %>%
    mutate(prop_var = variance / sum(variance), factor = gems)

  nhs_gems_variance_data_lang2 <- bind_rows(
    nhs_gems_variance_data_lang2,
    tidy_mod
  )
}

# compute within to between variance ratios
nhs_gems_variance_data_lang2 <- nhs_gems_variance_data_lang2 %>%
  filter(group != "Residual") %>%
  group_by(factor) %>%
  mutate(
    group = factor(
      group,
      levels = c("analysis_group", "song", "user_id"),
      labels = c("Language Family", "Song", "Individual")
    ),
    within_to_between = variance[group == "Individual"] /
      variance[group == "Language Family"],
    total_var_explained = sum(prop_var),
    scaling_factor = 100 / total_var_explained
  ) %>%
  ungroup() %>%
  mutate(relative_prop = prop_var * scaling_factor)

## Correlation replication ------------

get_corr <- function(pair, data) {
  c1 <- pair[1]
  c2 <- pair[2]

  vec1 <- data %>% pull(c1)
  vec2 <- data %>% pull(c2)

  # Calculate Pearson correlation
  r <- cor.test(
    vec1,
    vec2,
    use = "pairwise.complete.obs",
    method = "pearson"
  )$estimate

  p <- cor.test(
    vec1,
    vec2,
    use = "pairwise.complete.obs",
    method = "pearson"
  )$p.value

  tibble(
    country1 = c1,
    country2 = c2,
    country = paste0(c1, "_", c2),
    correlation = r,
    p = p
  )
}


long_data <- full_data %>%
  filter(corpus == "billboard") %>%
  drop_na(rating) %>%
  drop_na(language) %>%
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
  summarise(rating = mean(rating)) %>%
  ungroup() %>%
  group_by(gems_9) %>%
  # mean centering: scores now indicate whether a song is more or less "transcendent" than average song
  mutate(rating_z = scale(rating, center = TRUE, scale = TRUE))

# function to run all possible pairwise correlations
# and compare distribution of r values to null hypothesis of r <= 0
correlator <- function(n_ppt = numeric(), data = data.frame()) {
  # Get languages with at least n_ppt number of participants
  top_langs <- data %>%
    ungroup() %>%
    dplyr::select(language, user_id) %>%
    distinct() %>%
    group_by(language) %>%
    summarise(n = n()) %>%
    filter(n >= n_ppt) %>%
    ungroup() %>%
    pull(language)

  # Create wide datasets for each gems dimension
  gems_correlation_data <- list()

  for (gems in gems9_labels) {
    print(paste0("Generating data for: ", gems))

    tmp <- data %>%
      filter(gems_9 == gems) %>%
      filter(language %in% top_langs) %>%
      group_by(language, song) %>%
      summarise(mean = mean(rating_z, na.rm = TRUE)) %>%
      pivot_wider(names_from = language, values_from = mean) %>%
      ungroup()

    gems_correlation_data[[gems]] <- tmp
  }

  lang_pairs <- combn(top_langs, 2, simplify = FALSE)

  # Store results of pairwise correlations
  gems_correlation_results <- data.frame()

  for (gems in gems9_labels) {
    print(paste0("Running correlation for: ", gems))

    tmp_results <- map_dfr(
      lang_pairs,
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

# Run correlations across languages with n > 400 participants
corrs_400_languages <- correlator(n_ppt = 400, long_data)

gems_mean_cors_400_languages <- corrs_400_languages %>%
  group_by(variable) %>%
  summarise(
    mean_correlation = mean(correlation, na.rm = TRUE),
    median_correlation = median(correlation, na.rm = TRUE),
    iqr = IQR(correlation, na.rm = TRUE)
  ) %>%
  mutate(ppt_n = "400")

## nhs correlations -------------

long_data_nhs <- full_data %>%
  filter(corpus == "nhs") %>%
  drop_na(rating) %>%
  drop_na(language) %>%
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
  summarise(rating = mean(rating)) %>%
  ungroup() %>%
  group_by(gems_9) %>%
  # mean centering: scores now indicate whether a song is more or less "transcendent" than average song
  mutate(rating_z = scale(rating, center = TRUE, scale = TRUE))

# Run correlations across languages with n > 400 participants
nhs_corrs_400_languages <- correlator(n_ppt = 400, long_data_nhs)

# All correlation distributions are significantly above null hyp of r <= 0
nhs_gems_mean_cors_400_languages <- nhs_corrs_400_languages %>%
  group_by(variable) %>%
  summarise(
    mean_correlation = mean(correlation, na.rm = TRUE),
    median_correlation = median(correlation, na.rm = TRUE),
    iqr = IQR(correlation, na.rm = TRUE)
  ) %>%
  mutate(ppt_n = "400")


#### save everything ----------

save(
  billboard_gems_variance_data_lang_ind,
  billboard_gems_variance_data_lang,
  billboard_gems_variance_data_lang2,
  nhs_gems_variance_data_lang_ind,
  nhs_gems_variance_data_lang,
  nhs_gems_variance_data_lang2,
  gems_mean_cors_400_languages,
  nhs_gems_mean_cors_400_languages,
  file = here("results", "10_language_replication_results.RData")
)
