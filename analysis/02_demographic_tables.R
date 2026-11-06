# Generate demographic tables ------------------------------------

rm(list = ls())

library(pacman) # install.packages(pacman)
p_load(
  tidyverse,
  here
)

processed_data <- read_csv(here("data", "tml_wwl_data.csv.gz"))

# gems variables
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

n_rows <- processed_data %>%
  dplyr::select(user_id, song, corpus, emotion) %>%
  distinct() %>%
  nrow()

# Western Popular Music
billboard_data <- processed_data %>%
  filter(corpus == "billboard")

# Unfamiliar Global Music
nhs_data <- processed_data %>%
  filter(corpus == "nhs")

### Song counts -----------

song_counts_billboard <- billboard_data %>%
  drop_na(rating) %>%
  group_by(song) %>%
  summarise(n = n()) %>%
  pull(n) %>%
  mean

song_counts_nhs <- nhs_data %>%
  drop_na(rating) %>%
  group_by(song) %>%
  summarise(n = n()) %>%
  pull(n) %>%
  mean

### Billboard demographics ----------------------------------------------

n_billboard <- billboard_data$user_id %>%
  unique %>%
  length

# gender demographics
gender_billboard <- billboard_data %>%
  dplyr::select(user_id, gender) %>%
  distinct()

df_gender_billboard <- gender_billboard %>%
  pull(gender) %>%
  table %>%
  as.data.frame

missing_gender_billboard <- is.na(gender_billboard$gender) %>% sum

# age demographics
age_mean_billboard <- billboard_data$age %>% mean(na.rm = T)
age_median_billboard <- billboard_data$age %>% median(na.rm = T)
age_sd_billboard <- billboard_data$age %>% sd(na.rm = T)
age_min_billboard <- billboard_data$age %>% min(na.rm = T)
age_max_billboard <- billboard_data$age %>% max(na.rm = T)

# country demographics
countries_billboard <- billboard_data %>%
  dplyr::select(user_id, country) %>%
  distinct() %>%
  drop_na(country) %>%
  group_by(country) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  rename("name" = country) %>%
  mutate(type = "Country")

# language demographics
languages_billboard <- billboard_data %>%
  dplyr::select(user_id, language) %>%
  drop_na(language) %>%
  distinct() %>%
  group_by(language) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  rename("name" = language) %>%
  mutate(type = "Language")


# NHS demographics ---------------------------------------------------

n_nhs <- nhs_data$user_id %>%
  unique() %>%
  length()

# gender demographics
gender_nhs <- nhs_data %>%
  dplyr::select(user_id, gender) %>%
  distinct()

df_gender_nhs <- gender_nhs %>%
  pull(gender) %>%
  table %>%
  as.data.frame

missing_gender_nhs <- is.na(gender_nhs$gender) %>% sum

# age demographics
age_mean_nhs <- nhs_data$age %>% mean(na.rm = T)
age_median_nhs <- nhs_data$age %>% median(na.rm = T)
age_sd_nhs <- nhs_data$age %>% sd(na.rm = T)
age_min_nhs <- nhs_data$age %>% min(na.rm = T)
age_max_nhs <- nhs_data$age %>% max(na.rm = T)

# country demographics
countries_nhs <- nhs_data %>%
  dplyr::select(user_id, country) %>%
  distinct() %>%
  drop_na(country) %>%
  group_by(country) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  rename("name" = country) %>%
  mutate(type = "Country")

# language demographics
languages_nhs <- nhs_data %>%
  dplyr::select(user_id, language) %>%
  drop_na(language) %>%
  distinct() %>%
  group_by(language) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  rename("name" = language) %>%
  mutate(type = "Language")


bilinguals <- processed_data %>%
  select(user_id, id, lang_other) %>%
  distinct

n_full <- processed_data$user_id %>% unique %>% length


## Table: Countries -----------------------------------------------

country_table <- processed_data %>%
  dplyr::select(user_id, country) %>%
  distinct() %>%
  drop_na(country) %>%
  group_by(country) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  rename("name" = country) %>%
  mutate(type = "Country")

split_country_table <- processed_data %>%
  dplyr::select(user_id, country, corpus) %>%
  drop_na(country) %>%
  distinct() %>%
  group_by(country, corpus) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  rename("name" = country) %>%
  mutate(type = "Country")

split_country_table_top_countries <- split_country_table %>%
  filter(n > 400) %>%
  dplyr::select("Study" = corpus, "Country" = name, n) %>%
  arrange(Study) %>%
  mutate(
    Study = ifelse(
      Study == "billboard",
      "Popular Western Music",
      "Unfamiliar Global Music"
    )
  )


## Table: Languages -----------------------------------------------

language_table <- processed_data %>%
  dplyr::select(user_id, language) %>%
  drop_na(language) %>%
  distinct() %>%
  group_by(language) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  rename("name" = language) %>%
  mutate(type = "Language")

split_language_table <- processed_data %>%
  dplyr::select(user_id, language, corpus) %>%
  drop_na(language) %>%
  distinct() %>%
  group_by(language, corpus) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  rename("name" = language) %>%
  mutate(type = "Language")

split_language_table_top_languages <- split_language_table %>%
  filter(n > 400) %>%
  dplyr::select("Study" = corpus, "Language" = name, n) %>%
  arrange(Study) %>%
  mutate(
    Study = ifelse(
      Study == "billboard",
      "Popular Western Music",
      "Unfamiliar Global Music"
    )
  )

## Table: Billboard songs metadata (artist, name, year) -------------------------

bb_songs <- billboard_data$song %>% unique

bb_meta_data <- read.csv(here("data", "billboard_meta_data.csv")) %>%
  mutate(
    name = song,
    song = sub(".*cut_(.*)\\.mp3.*", "\\1", tolower(filename))
  ) %>%
  filter(song %in% bb_songs) %>%
  dplyr::select(year, artist, name) %>%
  arrange(desc(year)) %>%
  mutate(name = gsub("\"", "", name)) %>%
  mutate(
    name = case_when(
      name == "Me, Myself & I" ~ "Me, Myself \\& I",
      name == "Suit & Tie" ~ "Suit \\& Tie",
      name == "Peaches & Cream" ~ "Peaches \\& Cream",
      TRUE ~ name
    ),
    artist = gsub("&", "and", artist),
    artist = gsub("featuring", "ft.", artist),
    artist = case_when(
      artist == "BeyoncÃ©" ~ "Beyoncé",
      artist == "Los del RÃ­o" ~ "Los del Rio",
      TRUE ~ artist
    )
  )

## big demo table for US-based participants --------

# Explicit order of demographic variable levels
variable_order <- c("race", "latino", "income")

response_levels <- list(
  race = c(
    "White",
    "Black or African American",
    "Asian",
    "Native Hawaiian or other Pacific Islander",
    "American Indian/Alaska Native",
    "More than one race",
    "Prefer not to say",
    "(Missing)"
  ),

  latino = c(
    "Yes",
    "No",
    "Prefer not to say",
    "(Missing)"
  ),

  income = c(
    "Under $10,000",
    "$10,000 to $19,999",
    "$20,000 to $29,999",
    "$30,000 to $39,999",
    "$40,000 to $49,999",
    "$50,000 to $74,999",
    "$75,000 to $99,999",
    "$100,000 to $150,000",
    "Over $150,000",
    "I'd prefer not to say",
    "(Missing)"
  )
)

demo_table <- processed_data %>%
  dplyr::select(
    user_id,
    is_mobile,
    gender,
    age,
    country,
    language,
    hearing_imp,
    music_skill,
    education,
    music_enjoy,
    music_make_time,
    music_listen_time,
    music_lessons,
    race,
    latino,
    income,
    lessons_age,
    lang_other
  ) %>%
  dplyr::distinct() %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::everything(),
      as.character
    )
  ) %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::everything(),
      ~ dplyr::na_if(.x, ",,") %>%
        tidyr::replace_na("(Missing)")
    )
  )

# this is all to ensure that variables appear in the specified order:

response_lookup <- tibble(
  Variable = c(
    rep("race", length(response_levels$race)),
    rep("latino", length(response_levels$latino)),
    rep("income", length(response_levels$income))
  ),
  Response = c(
    response_levels$race,
    response_levels$latino,
    response_levels$income
  )
) %>%
  mutate(
    display_order = dplyr::row_number()
  )

# get counts per level
us_demo_counts <- demo_table %>%
  filter(country == "USA") %>%
  dplyr::select(all_of(variable_order)) %>%
  pivot_longer(
    cols = dplyr::everything(),
    names_to = "Variable",
    values_to = "Response"
  ) %>%
  count(
    Variable,
    Response,
    name = "n"
  )

# join counts to the variable order
us_demo <- response_lookup %>%
  left_join(
    us_demo_counts,
    by = c("Variable", "Response")
  ) %>%
  mutate(
    n = tidyr::replace_na(n, 0L),
  ) %>%
  group_by(Variable) %>%
  mutate(`%` = (n / sum(n) * 100) %>% round(2)) %>%
  ungroup() %>%
  arrange(display_order) %>%
  dplyr::select(
    Variable,
    Response,
    n,
    `%`
  ) %>%
  mutate(
    Variable = case_when(
      Variable == "race" ~ "Race of participant",
      Variable == "latino" ~ "Is participant Latino?",
      Variable == "income" ~ "Participant's household income"
    )
  ) %>%
  rename("Demographic variable" = Variable)

us_count_total <- processed_data %>%
  filter(country == "USA") %>%
  dplyr::select(user_id) %>%
  distinct() %>%
  nrow()

# number of bilinguals ------------
bilinguals <- processed_data %>%
  dplyr::select(
    user_id,
    corpus,
    lang_other
  ) %>%
  distinct() %>%
  filter(!is.na(lang_other))


# Save results -----------------------------------------------------------------

save(
  list = c(
    "n_rows",
    "n_full",
    "n_billboard",
    "n_nhs",
    "df_gender_billboard",
    "df_gender_nhs",
    "age_sd_billboard",
    "age_sd_nhs",
    "age_mean_billboard",
    "age_mean_nhs",
    "age_min_nhs",
    "age_max_nhs",
    "age_min_billboard",
    "age_max_billboard",
    "age_median_billboard",
    "age_median_nhs",
    "countries_billboard",
    "countries_nhs",
    "languages_billboard",
    "languages_nhs",
    "missing_gender_billboard",
    "missing_gender_nhs",
    "song_counts_billboard",
    "song_counts_nhs"
  ),
  file = here("results", "0_ns.RData")
)


save(
  split_country_table_top_countries,
  split_language_table_top_languages,
  bb_meta_data,
  us_demo,
  us_count_total,
  bilinguals,
  file = here("results", "02_demo_tables.RData")
)
