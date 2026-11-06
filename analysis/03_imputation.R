# Data imputation -------------------------------------------
# This script takes a very long time to run (longer than overnight)
# Results are saved both as Rdata files and rds files in results/imputations

rm(list = ls())

library(pacman) # install.packages(pacman)
p_load(
  tidyverse,
  here,
  lme4,
  ggrepel, # for plotting (prevent overlapping scatterplot labels)
  softImpute, # for imputation and dimension reduction
  devtools,
  patchwork
)

set.seed(1023)

# For cross validation in softimpute. This line throws warnings but the functions we need load without issue:
source(here("analysis", "cv_soft_func.R"))

## Prep datasets and variables ----------------------------------

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
  wonder = "Wonder",
  valence = "Valence",
  arousal = "Arousal"
)


# Colors for plotting. Palette based on rcartocolor::Prism
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

# raw data
processed_data <- read_csv(here("data", "tml_wwl_data.csv.gz")) %>%
  pivot_wider(
    id_cols = c(
      "user_id",
      "is_mobile",
      "gender",
      "age",
      "country",
      "language",
      "headphone",
      "mood_free_text",
      "pick_a_mood",
      "music_enjoy",
      "lang_other",
      "song",
      "corpus",
      "familiar",
      "session_id"
    ),
    names_from = emotion,
    values_from = rating
  ) %>%
  dplyr::select(user_id:session_id, gems25_labels)

# Study 1 data
billboard_data <- processed_data %>%
  filter(corpus == "billboard")

# missingness percentages:

billboard_missingness <- colMeans(is.na(
  billboard_data %>% dplyr::select(gems25_labels)
)) %>%
  mean


# Study 2 data
nhs_data <- processed_data %>%
  filter(corpus == "nhs")


nhs_missingness <- colMeans(is.na(
  nhs_data %>% dplyr::select(gems25_labels)
)) %>%
  mean


## SoftImpute: impute missing data and reduce dimensionality, as described in Methods ---------
# We first use cross validation to find the optimal lambda value for softimpute
# Then run softimpute to impute residuals (after modelling systematic variation due to country/participant/song)

# Function output is [1] loadings, [2] optimal number of ranks/dimensions, [3] imputed and reduced data
impute_and_reduce <- function(n_ppt = numeric(), data = data.frame()) {
  # 1. Filter dataset to countries with at least n_ppt participants
  top_countries <- data %>%
    drop_na(country) %>%
    dplyr::select(country, user_id) %>%
    distinct() %>%
    group_by(country) %>%
    summarise(n = n()) %>%
    filter(n > n_ppt) %>%
    pull(country)

  data_to_impute <- data %>%
    filter(country %in% top_countries) %>%
    dplyr::select(user_id, country, song, pick_a_mood, gems25_labels) %>%
    drop_na(user_id, country, song)

  # 2. Create containers for mixed-model residuals and predicted values
  residuals_df <- data.frame(matrix(ncol = 0, nrow = nrow(data_to_impute)))
  predicted_df <- data.frame(matrix(ncol = 0, nrow = nrow(data_to_impute)))

  # 3. For each gems25 dimension, predict score based on song, participant, and country
  for (emotion in gems25_labels) {
    fit <- lmer(
      as.formula(paste(emotion, "~ (1|song) + (1|user_id) + (1|country)")),
      data = data_to_impute,
      na.action = na.exclude,
      control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6))
    )

    # Store model predictions (systematic part of variance due to song/participant/country)
    predicted_df[[emotion]] <- predict(
      fit,
      newdata = data_to_impute,
      allow.new.levels = TRUE # fills NAs with predicted values
    )

    # Store model residuals (idiosyncratic part of variance)
    residuals_df[[emotion]] <- residuals(fit)
  }

  # convert residuals and predicted values to matrices for softImpute
  predicted_to_impute_matrix <- as.matrix(predicted_df)
  residuals_to_impute_matrix <- as.matrix(residuals_df)

  # 4. Cross-validation function to choose best lambda value for softImpute (with perc.missing changed from 0.2 to 0.1)
  # Function source: https://raw.githubusercontent.com/R-miss-tastic/website/master/static/how-to/impute/CrossValidation_softImpute.R

  cv_sft <- function(y, N = 10, len = 20) {
    y <- as.matrix(y)
    Y2 <- y
    Y2[is.na(Y2)] <- 0
    d <- dim(y)
    n <- d[1]
    p <- d[2]
    m <- sum(!is.na(y))
    lambda1.max <- max(svd(Y2)$d)
    lambda1.min <- 1e-3 * lambda1.max
    grid.lambda1 <-
      exp(seq(log(lambda1.min), log(lambda1.max), length.out = len))
    ylist <-
      lapply(1:N, function(k) {
        produce_NA(as.matrix(y), perc.missing = 0.1)
      })$data.incomp
    res.cv <- lapply(1:N, function(k) {
      sapply(1:len, function(i) {
        print(i)
        yy <- produce_NA(as.matrix(y), perc.missing = 0.1)$data.incomp
        res <-
          softImpute(as.matrix(yy), lambda = grid.lambda1[i], maxit = 1000)
        u <- res$u
        d <- res$d
        v <- res$v
        if (is.null(dim(u))) {
          res <- d * u %*% t(v)
        } else {
          res <- u %*% diag(d) %*% t(v)
        }
        imp <- as.matrix(yy)
        imp[is.na(yy)] <- res[is.na(yy)]
        return(sqrt(sum((res - y)^2, na.rm = T)))
      })
    })
    res.cv <- colMeans(do.call(rbind, res.cv))
    l <- which.min(res.cv)
    lambda <- grid.lambda1[l]
    res <- as.data.frame(list(lambda = grid.lambda1, error = res.cv))
    return(res)
  }

  # 5. Run CV on the residual matrix and find lambda with minimum error
  lambda_sft <- cv_sft(residuals_to_impute_matrix, N = 10, len = 20)
  best_lambda <- lambda_sft[which(lambda_sft$error == min(lambda_sft$error)), 1]

  # 6. Fit final softImpute model on residuals using the chosen/best lambda value
  residuals_fit <- softImpute(
    residuals_to_impute_matrix,
    lambda = best_lambda,
    type = "svd",
    maxit = 1000
  )

  n_rank <- residuals_fit$d %>% length # number of ranks (data driven)

  V <- residuals_fit$v # right singular vectors ("loadings"/component directions in emotion space)
  d <- residuals_fit$d # how much variance each component explains
  u <- residuals_fit$u # each row's scores on each component

  # 7. Multiply by singular values to make loadings scale by how important they are
  loadings <- t(diag(d) %*% t(V)) # (emotion variables × components)

  # 8. Construct loadings matrix
  colnames(loadings) <- paste0("Component_", seq_len(ncol(loadings)))
  rownames(loadings) <- colnames(residuals_to_impute_matrix) # original emotion labels
  loadings_df <- as.data.frame(loadings)

  # 9. impute residuals
  imputed_residuals_matrix <- softImpute::complete(
    residuals_to_impute_matrix,
    residuals_fit
  )

  # 10. re-construct the full dataset by adding the imputed residuals back to the predicted values
  imputed_matrix <- predicted_to_impute_matrix + imputed_residuals_matrix

  # 11. project completed matrix onto the latent factors.
  reduced_data <- imputed_matrix %*%
    V %>% # reduce dataset
    as.data.frame() %>%
    bind_cols(
      country = data_to_impute$country,
      user_id = data_to_impute$user_id,
      song = data_to_impute$song
    ) %>%
    dplyr::select(user_id, country, song, starts_with("V"))

  return(list(
    loadings = loadings_df,
    n_rank = n_rank,
    reduced_data = reduced_data
  ))
} # phew we're done


## Run imputation and dimension reduction ------------------------------

# Popular Music (at two different sample sizes)
billboard_imputation_400 <- impute_and_reduce(
  n_ppt = 400,
  data = billboard_data
)
billboard_imputation_800 <- impute_and_reduce(
  n_ppt = 800,
  data = billboard_data
)

# Unfamiliar Music (at two different sample sizes)
nhs_imputation_400 <- impute_and_reduce(
  n_ppt = 400,
  data = nhs_data
)
nhs_imputation_800 <- impute_and_reduce(
  n_ppt = 800,
  data = nhs_data
)

# extract loadings
billboard_imputation_400_loadings <- billboard_imputation_400$loadings %>%
  as.data.frame()
billboard_imputation_800_loadings <- billboard_imputation_800$loadings %>%
  as.data.frame()
nhs_imputation_400_loadings <- nhs_imputation_400$loadings %>% as.data.frame()
nhs_imputation_800_loadings <- nhs_imputation_800$loadings %>% as.data.frame()

## Validating interpretations of dimensions for Popular Music corpus -------------------------

### Do the billboard loadings track Arousal and Valence? -----------
nrc <- read_delim(here("data", "NRC-VAD-Lexicon-v2.1.txt"))

vars_vad400 <- billboard_imputation_400_loadings %>%
  rownames_to_column() %>%
  dplyr::rename(term = rowname) %>%
  # some label editing necessary to get matching score. Results do not significantly change with other possible labels (e.g., mellowness instead of mellow)
  mutate(
    term = case_when(
      term == "mellowed_softened_up" ~ "mellow",
      term == "filled_with_wonder" ~ "wonder",
      term == "allured" ~ "allure",
      term == "soothed" ~ "soothing",
      TRUE ~ term
    )
  ) %>%
  left_join(nrc, by = "term")

# correlation results with Term-valence and Term-arousal values
cor.test(vars_vad400$Component_1, vars_vad400$valence) # component_1 positively correlates with valence
cor.test(vars_vad400$Component_2, vars_vad400$valence) # not sig.
cor.test(vars_vad400$Component_1, vars_vad400$arousal) # not sig.
cor.test(vars_vad400$Component_2, vars_vad400$arousal) # component_2 negatively correlates with arousal
cor.test(vars_vad400$Component_1, vars_vad400$dominance) # not sig.
cor.test(vars_vad400$Component_2, vars_vad400$dominance) # component_2 negatively correlates with arousal

# create reversed component_2 for easier interpretation
billboard_imputation_400_loadings$Component_2_reversed <- (-1) *
  billboard_imputation_400_loadings$Component_2

# correlation models for reporting in manuscript
vars_vad400 <- vars_vad400 %>%
  bind_cols(
    Component_2_reversed = billboard_imputation_400_loadings$Component_2_reversed
  )

v1_valence_billboard_400 <- cor.test(
  vars_vad400$Component_1,
  vars_vad400$valence
)

v2_arousal_billboard_400 <- cor.test(
  vars_vad400$Component_2_reversed,
  vars_vad400$arousal
)

v2_dominance_billboard_400 <- cor.test(
  vars_vad400$Component_2_reversed,
  vars_vad400$dominance
)

### Figures S1b and S1c: Popular Music loadings and NRC Valence/Arousal values ---------------------
v1_valence_billboard_plot <- ggplot(
  vars_vad400,
  aes(x = Component_1, y = valence)
) +
  geom_smooth(method = lm, se = T, color = "#855686FF") +
  geom_point(size = 1) +
  geom_text_repel(aes(x = Component_1, y = valence, label = term), size = 3) +
  theme_classic() +
  labs(x = "Loadings on Component 1", y = "Term-Valence Ratings")

v2_arousal_billboard_plot <- ggplot(
  vars_vad400,
  aes(x = Component_2_reversed, y = arousal)
) +
  geom_smooth(method = lm, se = T, color = "#855686FF") +
  geom_point(size = 1) +
  geom_text_repel(
    aes(x = Component_2_reversed, y = arousal, label = term),
    size = 3
  ) +
  theme_classic() +
  labs(x = "Loadings on Component 2", y = "Term-Arousal Ratings")

v2_dominance_billboard_plot <- ggplot(
  vars_vad400,
  aes(x = Component_2_reversed, y = dominance)
) +
  geom_smooth(method = lm, se = T, color = "#855686FF") +
  geom_point(size = 1) +
  geom_text_repel(
    aes(x = Component_2_reversed, y = dominance, label = term),
    size = 3
  ) +
  theme_classic() +
  labs(x = "Loadings on Component 2", y = "Term-Dominance Ratings")

### Figure S1a: Popular Music loadings plot (with 400 participants) -----------------------------
billboard_loadings_plot_400 <- billboard_imputation_400_loadings %>%
  as.data.frame() %>%
  rownames_to_column("emotion") %>%
  mutate(
    emotion = case_when(
      emotion == "filled_with_wonder" ~ "filled with wonder",
      emotion == "mellowed_softened_up" ~ "mellowed and softened up",
      TRUE ~ emotion
    )
  ) %>%
  # colour gems25 dimensions according to gems9 factor solution
  mutate(
    gems_9 = case_when(
      emotion %in% c("moved", "filled with wonder", "allured") ~ "wonder",
      emotion %in%
        c("fascinated", "overwhelmed", "transcendent") ~ "transcendence",
      emotion %in% c("strong", "energetic", "triumphant") ~ "power",
      emotion %in%
        c("tender", "affectionate", "mellowed and softened up") ~ "tenderness",
      emotion %in% c("nostalgic", "sentimental", "dreamy") ~ "nostalgia",
      emotion %in% c("serene", "calm", "soothed") ~ "peacefulness",
      emotion %in% c("animated", "bouncy", "joyful") ~ "joyful_activation",
      emotion %in% c("sad", "tearful") ~ "sadness",
      emotion %in% c("tense", "agitated") ~ "tension"
    )
  ) %>%
  ggplot(aes(
    x = Component_1,
    y = Component_2_reversed,
    label = emotion,
    color = gems_9
  )) +
  geom_point(size = 2) +
  geom_text_repel(aes(label = emotion), color = "black", size = 3) +
  scale_color_manual(
    values = gems9_cols,
    labels = c(
      wonder = "Wonder",
      transcendence = "Transcendence",
      power = "Power",
      tenderness = "Tenderness",
      nostalgia = "Nostalgia",
      peacefulness = "Peacefulness",
      joyful_activation = "Joyful Activation",
      sadness = "Sadness",
      tension = "Tension"
    )
  ) +
  labs(
    x = paste0("Component 1"),
    y = paste0("Component 2")
  ) +
  labs(color = '9-Factor Solution \n(Zentner et al., 2008)') +
  geom_vline(xintercept = 0, linewidth = .1) +
  geom_hline(yintercept = 0, linewidth = .1) +
  theme_classic() +
  theme(
    panel.background = element_rect(
      fill = "transparent", # avoid drawing panel outline
      colour = NA_character_
    )
  )

### Save Popular Music imputed data (and reverse V2 to create Arousal dimension) -------------
billboard_imputation_400$reduced_data <- billboard_imputation_400$reduced_data %>%
  mutate(V2_reversed = (-1) * V2)


saveRDS(
  billboard_imputation_400$reduced_data,
  here(
    "results/imputations",
    "billboard_400.rds"
  )
)


### Validating interpretations of dimensions for Popular music corpus using 800 participants -------------------------

vars_vad800 <- billboard_imputation_800_loadings %>%
  rownames_to_column() %>%
  dplyr::rename(term = rowname) %>%
  # some label editing necessary to get matching score. Results do not significantly change with other possible labels (e.g., mellowness instead of mellow)
  mutate(
    term = case_when(
      term == "mellowed_softened_up" ~ "mellow",
      term == "filled_with_wonder" ~ "wonder",
      term == "allured" ~ "allure",
      term == "soothed" ~ "soothing",
      TRUE ~ term
    )
  ) %>%
  left_join(nrc, by = "term")

cor.test(vars_vad800$Component_1, vars_vad800$valence) # component_1 positively correlated with valence
cor.test(vars_vad800$Component_2, vars_vad800$valence) # n.s.
cor.test(vars_vad800$Component_1, vars_vad800$arousal) # n.s.
cor.test(vars_vad800$Component_2, vars_vad800$arousal) # component_2 negatively correlated with arousal
cor.test(vars_vad800$Component_1, vars_vad800$dominance) # n.s.
cor.test(vars_vad800$Component_2, vars_vad800$dominance) # component_2 negatively correlated with dominance

# reverse component_2 for interpretation
billboard_imputation_800_loadings$Component_2_reversed <- (-1) *
  billboard_imputation_800_loadings$Component_2

vars_vad800 <- vars_vad800 %>%
  bind_cols(
    Component_2_reversed = billboard_imputation_800_loadings$Component_2_reversed
  )

### Figure S4a: Study 1 loadings plot (with 800 participants) -------------------------
billboard_loadings_plot_800 <- billboard_imputation_800_loadings %>%
  as.data.frame() %>%
  rownames_to_column("emotion") %>%
  mutate(
    emotion = case_when(
      emotion == "filled_with_wonder" ~ "filled with wonder",
      emotion == "mellowed_softened_up" ~ "mellowed and softened up",
      TRUE ~ emotion
    )
  ) %>%
  mutate(
    gems_9 = case_when(
      emotion %in% c("moved", "filled with wonder", "allured") ~ "wonder",
      emotion %in%
        c("fascinated", "overwhelmed", "transcendent") ~ "transcendence",
      emotion %in% c("strong", "energetic", "triumphant") ~ "power",
      emotion %in%
        c("tender", "affectionate", "mellowed and softened up") ~ "tenderness",
      emotion %in% c("nostalgic", "sentimental", "dreamy") ~ "nostalgia",
      emotion %in% c("serene", "calm", "soothed") ~ "peacefulness",
      emotion %in% c("animated", "bouncy", "joyful") ~ "joyful_activation",
      emotion %in% c("sad", "tearful") ~ "sadness",
      emotion %in% c("tense", "agitated") ~ "tension"
    )
  ) %>%
  ggplot(aes(
    x = Component_1,
    y = Component_2,
    label = emotion,
    color = gems_9
  )) +
  geom_point(size = 2) +
  geom_text_repel(aes(label = emotion), color = "black", size = 3) +
  scale_color_manual(
    values = gems9_cols,
    labels = c(
      wonder = "Wonder",
      transcendence = "Transcendence",
      power = "Power",
      tenderness = "Tenderness",
      nostalgia = "Nostalgia",
      peacefulness = "Peacefulness",
      joyful_activation = "Joyful Activation",
      sadness = "Sadness",
      tension = "Tension"
    )
  ) +
  labs(
    x = paste0("Valence"),
    y = paste0("Arousal")
  ) +
  labs(color = '9-Factor Solution \n(Zentner et al., 2008)') +
  geom_vline(xintercept = 0, linewidth = .1) +
  geom_hline(yintercept = 0, linewidth = .1) +
  theme_classic() +
  theme(
    panel.background = element_rect(
      fill = "transparent",
      colour = NA_character_
    )
  ) # necessary to avoid drawing panel outline

### Save pop music imputed data (with 800 participants — not used in manuscript.) -------------
billboard_imputation_800$reduced_data <- billboard_imputation_800$reduced_data %>%
  mutate(V2_reversed = (-1) * V2)


saveRDS(
  billboard_imputation_800$reduced_data,
  here(
    "results/imputations",
    "billboard_800.rds"
  )
)


## Validating interpretations of dimensions for Unfamiliar Music corpus (NHS tracks) -------------------------

### Do the NHS loadings track Arousal and Valence (from NRC-VAD)? -----------------
vars_vad_nhs400 <- nhs_imputation_400_loadings %>%
  rownames_to_column() %>%
  dplyr::rename(term = rowname) %>%
  # some label editing necessary to get matching score. Results do not significantly change with other possible labels (e.g., mellowness instead of mellow)
  mutate(
    term = case_when(
      term == "mellowed_softened_up" ~ "mellow",
      term == "filled_with_wonder" ~ "wonder",
      term == "allured" ~ "allure",
      term == "soothed" ~ "soothing",
      TRUE ~ term
    )
  ) %>%
  left_join(nrc, by = "term")

cor.test(vars_vad_nhs400$Component_1, vars_vad_nhs400$valence) # component_1 negatively correlates with valence
cor.test(vars_vad_nhs400$Component_2, vars_vad_nhs400$valence) # n.s.
cor.test(vars_vad_nhs400$Component_1, vars_vad_nhs400$arousal) # n.s.
cor.test(vars_vad_nhs400$Component_2, vars_vad_nhs400$arousal) # component_2 positively correlates with arousal
cor.test(vars_vad_nhs400$Component_1, vars_vad_nhs400$dominance) # n.s.
cor.test(vars_vad_nhs400$Component_2, vars_vad_nhs400$dominance) # n.s.

# reverse component_1 for interpretation
nhs_imputation_400_loadings$Component_1_reversed <- (-1) *
  nhs_imputation_400_loadings$Component_1

# correlation models for reporting in manuscript
vars_vad_nhs400 <- vars_vad_nhs400 %>%
  bind_cols(
    Component_1_reversed = nhs_imputation_400_loadings$Component_1_reversed
  )

v1_valence_nhs_400 <- cor.test(
  vars_vad_nhs400$Component_1_reversed,
  vars_vad_nhs400$valence
)

v2_arousal_nhs_400 <- cor.test(
  vars_vad_nhs400$Component_2,
  vars_vad_nhs400$arousal
)

### Figures S3b and S3c: Unfamiliar music loadings and NRC Valence/Arousal values ---------------------
v1_valence_nhs_plot <- ggplot(
  vars_vad_nhs400,
  aes(x = Component_1_reversed, y = valence)
) +
  geom_smooth(method = lm, se = T, color = "#855686FF") +
  geom_point(size = 1) +
  geom_text_repel(
    aes(x = Component_1_reversed, y = valence, label = term),
    size = 3
  ) +
  theme_classic() +
  labs(x = "Loadings on Component 1", y = "Term-Valence Ratings")

v2_arousal_nhs_plot <- ggplot(
  vars_vad_nhs400,
  aes(x = Component_2, y = arousal)
) +
  geom_smooth(method = lm, se = T, color = "#855686FF") +
  geom_point(size = 1) +
  geom_text_repel(aes(x = Component_2, y = arousal, label = term), size = 3) +
  theme_classic() +
  labs(x = "Loadings on Component 2", y = "Term-Arousal Ratings")

### Figure S3a: Unfamiliar Music loadings plot (with 400 participants) -------------------------
nhs_loadings_plot_400 <- nhs_imputation_400_loadings %>%
  as.data.frame() %>%
  rownames_to_column("emotion") %>%
  mutate(
    emotion = case_when(
      emotion == "filled_with_wonder" ~ "filled with wonder",
      emotion == "mellowed_softened_up" ~ "mellowed and softened up",
      TRUE ~ emotion
    )
  ) %>%
  mutate(
    gems_9 = case_when(
      emotion %in% c("moved", "filled with wonder", "allured") ~ "wonder",
      emotion %in%
        c("fascinated", "overwhelmed", "transcendent") ~ "transcendence",
      emotion %in% c("strong", "energetic", "triumphant") ~ "power",
      emotion %in%
        c("tender", "affectionate", "mellowed and softened up") ~ "tenderness",
      emotion %in% c("nostalgic", "sentimental", "dreamy") ~ "nostalgia",
      emotion %in% c("serene", "calm", "soothed") ~ "peacefulness",
      emotion %in% c("animated", "bouncy", "joyful") ~ "joyful_activation",
      emotion %in% c("sad", "tearful") ~ "sadness",
      emotion %in% c("tense", "agitated") ~ "tension"
    )
  ) %>%
  ggplot(aes(
    x = Component_1_reversed,
    y = Component_2,
    label = emotion,
    color = gems_9
  )) +
  geom_point(size = 2) +
  geom_text_repel(aes(label = emotion), color = "black", size = 3) +
  scale_color_manual(
    values = gems9_cols,
    labels = c(
      wonder = "Wonder",
      transcendence = "Transcendence",
      power = "Power",
      tenderness = "Tenderness",
      nostalgia = "Nostalgia",
      peacefulness = "Peacefulness",
      joyful_activation = "Joyful Activation",
      sadness = "Sadness",
      tension = "Tension"
    )
  ) +
  labs(
    x = paste0("Component 1"),
    y = paste0("Component 2")
  ) +
  labs(color = '9-Factor Solution \n(Zentner et al., 2008)') +
  geom_vline(xintercept = 0, linewidth = .1) +
  geom_hline(yintercept = 0, linewidth = .1) +
  theme_classic() +
  theme(
    panel.background = element_rect(
      fill = "transparent", # avoid drawing panel outline
      colour = NA_character_
    )
  )


### Save Unfamiliar Music imputed data ---------------------------
nhs_imputation_400$reduced_data <- nhs_imputation_400$reduced_data %>%
  mutate(V1_reversed = (-1) * V1)

saveRDS(
  nhs_imputation_400$reduced_data,
  here("results/imputations", "nhs_400.rds")
)


### Do the NHS loadings track Arousal and Valence ratings from Mehr et al. 2019?  -----------------

# Extract meta data about the original behavioural context of each song
song_data <- read_csv(here(
  "data",
  "NHSDiscography_Metadata.csv"
)) %>%
  mutate(song = (-1) * extract_numeric(song)) %>%
  dplyr::select(song, type)

# merge song data with imputed data
nhs_reduced_data <- readRDS(here("results/imputations", "nhs_400.rds")) %>%
  mutate(song = as.numeric(song)) %>%
  left_join(song_data, by = "song")

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

# Extract perceived arousal/valence from Mehr et al. 2019., measured as "happiness", "excitement" and "pleasantness"
# codebooks in Table S9 here: https://www.science.org/action/downloadSupplement?doi=10.1126%2Fscience.aax0868&file=aax0868_mehr_sm.pdf
naive_listener_data <- read_csv(here(
  "data",
  "NHSDiscography_NaiveListeners.csv"
))

# dataset containing song type, and arousal/valence scores from our data
type_by_valence_arousal_data <- nhs_reduced_data %>%
  group_by(song, type) %>%
  summarise(
    Valence = mean(V1_reversed_scaled, na.rm = T),
    Arousal = mean(V2_scaled, na.rm = T)
  ) %>%
  mutate(type = as.factor(type))

# dataset containing song type, arousal/valence scores from our data, and arousal/valence ratings from Mehr 2019 (form_exci, form_happ, form_plea)
# we expect happiness and pleasantness to track valence, and excitement to track arousal
perceived_valence_arousal_data <- naive_listener_data %>%
  dplyr::select(song, form_exci, form_happ, form_plea) %>%
  left_join(type_by_valence_arousal_data, by = "song")

explicit_ratings_plot_data <- perceived_valence_arousal_data %>%
  pivot_longer(
    cols = c(form_exci, form_happ, form_plea),
    names_to = "explicit_measure",
    values_to = "rating"
  ) %>%
  pivot_longer(
    cols = c(Valence, Arousal),
    names_to = "reduced_dim",
    values_to = "score"
  ) %>%
  mutate(
    explicit_measure = case_when(
      explicit_measure == "form_exci" ~ "Exciting",
      explicit_measure == "form_happ" ~ "Happy",
      explicit_measure == "form_plea" ~ "Pleasant"
    )
  )


exci_by_arousal <- cor.test(
  perceived_valence_arousal_data$form_exci,
  perceived_valence_arousal_data$Arousal
)

happ_by_arousal <- cor.test(
  perceived_valence_arousal_data$form_happ,
  perceived_valence_arousal_data$Arousal
)

plea_by_arousal <- cor.test(
  perceived_valence_arousal_data$form_plea,
  perceived_valence_arousal_data$Arousal
)

plea_by_valence <- cor.test(
  perceived_valence_arousal_data$form_plea,
  perceived_valence_arousal_data$Valence
)

happ_by_valence <- cor.test(
  perceived_valence_arousal_data$form_happ,
  perceived_valence_arousal_data$Valence
)

exci_by_valence <- cor.test(
  perceived_valence_arousal_data$form_exci,
  perceived_valence_arousal_data$Valence
)

perceived_valence_arousal_data <- perceived_valence_arousal_data %>%
  mutate(type = factor(type, levels = c("Dance", "Healing", "Love", "Lullaby")))

val_happ_plot <- ggplot(
  perceived_valence_arousal_data,
  aes(x = Valence, y = form_happ, color = type)
) +
  geom_smooth(aes(group = 1), color = "grey", method = lm, se = T) +
  geom_point(size = 1) +
  theme_classic() +
  labs(x = "Mean Component 1", y = "Mean Happiness") +
  scale_color_manual(
    values = c(
      "Dance" = "#5F4690FF",
      "Lullaby" = "#73AF48FF",
      "Healing" = "#CC503EFF",
      "Love" = "#EDAD08FF"
    )
  ) +
  labs(
    color = "Original Song Context"
  ) +
  geom_text(
    aes(
      x = 0,
      y = 1.5,
      label = paste0("r = ", happ_by_valence$estimate %>% round(2))
    ),
    inherit.aes = FALSE,
    size = 3
  ) +
  ylim(1, 5) +
  guides(color = guide_legend(override.aes = list(size = 5)))

val_plea_plot <- ggplot(
  perceived_valence_arousal_data,
  aes(x = Valence, y = form_plea, color = type)
) +
  geom_smooth(aes(group = 1), color = "grey", method = lm, se = T) +
  geom_point(size = 1) +
  theme_classic() +
  labs(x = "Mean Component 1", y = "Mean Pleasantness") +
  scale_color_manual(
    values = c(
      "Dance" = "#5F4690FF",
      "Lullaby" = "#73AF48FF",
      "Healing" = "#CC503EFF",
      "Love" = "#EDAD08FF"
    )
  ) +
  labs(
    color = "Original Song Context"
  ) +
  geom_text(
    aes(
      x = 0,
      y = 1.5,
      label = paste0("r = ", plea_by_valence$estimate %>% round(2))
    ),
    inherit.aes = FALSE,
    size = 3
  ) +
  ylim(1, 5) +
  guides(color = guide_legend(override.aes = list(size = 5)))

val_exci_plot <- ggplot(
  perceived_valence_arousal_data,
  aes(x = Valence, y = form_exci, color = type)
) +
  geom_smooth(aes(group = 1), color = "grey", method = lm, se = T) +
  geom_point(size = 1) +
  theme_classic() +
  labs(x = "Mean Component 1", y = "Mean Excitement") +
  scale_color_manual(
    values = c(
      "Dance" = "#5F4690FF",
      "Lullaby" = "#73AF48FF",
      "Healing" = "#CC503EFF",
      "Love" = "#EDAD08FF"
    )
  ) +
  labs(
    color = "Original Song Context"
  ) +
  geom_text(
    aes(
      x = 0,
      y = 1.5,
      label = paste0("r = ", exci_by_valence$estimate %>% round(2))
    ),
    inherit.aes = FALSE,
    size = 3
  ) +
  ylim(1, 5) +
  guides(color = guide_legend(override.aes = list(size = 5)))

aro_happ_plot <- ggplot(
  perceived_valence_arousal_data,
  aes(x = Arousal, y = form_happ, color = type)
) +
  geom_smooth(aes(group = 1), color = "grey", method = lm, se = T) +
  geom_point(size = 1) +
  theme_classic() +
  labs(x = "Mean Component 2", y = "Mean Happiness") +
  scale_color_manual(
    values = c(
      "Dance" = "#5F4690FF",
      "Lullaby" = "#73AF48FF",
      "Healing" = "#CC503EFF",
      "Love" = "#EDAD08FF"
    )
  ) +
  labs(
    color = "Original Song Context"
  ) +
  geom_text(
    aes(
      x = 0,
      y = 1.5,
      label = paste0("r = ", happ_by_arousal$estimate %>% round(2))
    ),
    inherit.aes = FALSE,
    size = 3
  ) +
  ylim(1, 5) +
  guides(color = guide_legend(override.aes = list(size = 5)))


aro_plea_plot <- ggplot(
  perceived_valence_arousal_data,
  aes(x = Arousal, y = form_plea, color = type)
) +
  geom_smooth(aes(group = 1), color = "grey", method = lm, se = T) +
  geom_point(size = 1) +
  theme_classic() +
  labs(x = "Mean Component 2", y = "Mean Pleasantness") +
  scale_color_manual(
    values = c(
      "Dance" = "#5F4690FF",
      "Lullaby" = "#73AF48FF",
      "Healing" = "#CC503EFF",
      "Love" = "#EDAD08FF"
    )
  ) +
  labs(
    color = "Original Song Context"
  ) +
  geom_text(
    aes(x = 0, y = 1.5, label = "n.s."),
    inherit.aes = FALSE,
    size = 3
  ) +
  ylim(1, 5) +
  guides(color = guide_legend(override.aes = list(size = 5)))

aro_exci_plot <- ggplot(
  perceived_valence_arousal_data,
  aes(x = Arousal, y = form_exci, color = type)
) +
  geom_smooth(aes(group = 1), color = "grey", method = lm, se = T) +
  geom_point(size = 1) +
  theme_classic() +
  labs(x = "Mean Component 2", y = "Mean Excitement") +
  scale_color_manual(
    values = c(
      "Dance" = "#5F4690FF",
      "Lullaby" = "#73AF48FF",
      "Healing" = "#CC503EFF",
      "Love" = "#EDAD08FF"
    )
  ) +
  labs(
    color = "Original Song Context"
  ) +
  geom_text(
    aes(
      x = 0,
      y = 1.5,
      label = paste0("r = ", exci_by_arousal$estimate %>% round(2))
    ),
    inherit.aes = FALSE,
    size = 3
  ) +
  ylim(1, 5) +
  guides(color = guide_legend(override.aes = list(size = 5)))

merged_explicit_rating_plot <- (val_exci_plot + val_happ_plot + val_plea_plot) /
  (aro_exci_plot + aro_happ_plot + aro_plea_plot) +
  plot_annotation(tag_levels = 'a') +
  plot_layout(
    guides = "collect"
  ) &
  theme(legend.position = 'bottom')

ggsave(
  here("viz", "figS3.pdf"),
  merged_explicit_rating_plot,
  height = 6,
  width = 5
)

### Validating interpretations of dimensions for unfamiliar music using 800 participants -------------------------

vars_vad_nhs800 <- nhs_imputation_800_loadings %>%
  rownames_to_column() %>%
  dplyr::rename(term = rowname) %>%
  # some label editing necessary to get matching score. Results do not significantly change with other possible labels (e.g., mellowness instead of mellow)
  mutate(
    term = case_when(
      term == "mellowed_softened_up" ~ "mellow",
      term == "filled_with_wonder" ~ "wonder",
      term == "allured" ~ "allure",
      term == "soothed" ~ "soothing",
      TRUE ~ term
    )
  ) %>%
  left_join(nrc, by = "term")

cor.test(vars_vad_nhs800$Component_1, vars_vad_nhs800$valence) # component_1 negatively correlates with valence
cor.test(vars_vad_nhs800$Component_2, vars_vad_nhs800$valence) # n.s.
cor.test(vars_vad_nhs800$Component_1, vars_vad_nhs800$arousal) # n.s.
cor.test(vars_vad_nhs800$Component_2, vars_vad_nhs800$arousal) # component_2 positively correlates with arousal
cor.test(vars_vad_nhs800$Component_1, vars_vad_nhs800$dominance) # n.s.
cor.test(vars_vad_nhs800$Component_2, vars_vad_nhs800$dominance) # n.s.

# reverse component_1 for interpretation
nhs_imputation_800_loadings$Component_1_reversed <- (-1) *
  nhs_imputation_800_loadings$Component_1

vars_vad_nhs800 <- vars_vad_nhs800 %>%
  bind_cols(
    Component_1_reversed = nhs_imputation_800_loadings$Component_1_reversed
  )


### Figure S4b: Study 2 loadings plot (with 800 participants) -------------------------
nhs_loadings_plot_800 <- nhs_imputation_800_loadings %>%
  as.data.frame() %>%
  rownames_to_column("emotion") %>%
  mutate(
    emotion = case_when(
      emotion == "filled_with_wonder" ~ "filled with wonder",
      emotion == "mellowed_softened_up" ~ "mellowed and softened up",
      TRUE ~ emotion
    )
  ) %>%
  mutate(
    gems_9 = case_when(
      emotion %in% c("moved", "filled with wonder", "allured") ~ "wonder",
      emotion %in%
        c("fascinated", "overwhelmed", "transcendent") ~ "transcendence",
      emotion %in% c("strong", "energetic", "triumphant") ~ "power",
      emotion %in%
        c("tender", "affectionate", "mellowed and softened up") ~ "tenderness",
      emotion %in% c("nostalgic", "sentimental", "dreamy") ~ "nostalgia",
      emotion %in% c("serene", "calm", "soothed") ~ "peacefulness",
      emotion %in% c("animated", "bouncy", "joyful") ~ "joyful_activation",
      emotion %in% c("sad", "tearful") ~ "sadness",
      emotion %in% c("tense", "agitated") ~ "tension"
    )
  ) %>%
  ggplot(aes(
    x = Component_1_reversed,
    y = Component_2,
    label = emotion,
    color = gems_9
  )) +
  geom_point(size = 2) +
  scale_color_manual(
    values = gems9_cols,
    labels = c(
      wonder = "Wonder",
      transcendence = "Transcendence",
      power = "Power",
      tenderness = "Tenderness",
      nostalgia = "Nostalgia",
      peacefulness = "Peacefulness",
      joyful_activation = "Joyful Activation",
      sadness = "Sadness",
      tension = "Tension"
    )
  ) +
  geom_text_repel(aes(label = emotion), color = "black", size = 3) +
  labs(
    x = paste0("Valence"),
    y = paste0("Arousal")
  ) +
  labs(color = '9-Factor Solution \n(Zentner et al., 2008)') +
  geom_vline(xintercept = 0, linewidth = .1) +
  geom_hline(yintercept = 0, linewidth = .1) +
  theme_classic() +
  theme(
    panel.background = element_rect(
      fill = "transparent",
      colour = NA_character_
    )
  )

### Save Unfamiliar music imputed data (with 800 participants — not used in manuscript.) -------------
nhs_imputation_800$reduced_data <- nhs_imputation_800$reduced_data %>%
  mutate(V1_reversed = -V1)

saveRDS(
  nhs_imputation_800$reduced_data,
  here("results/imputations", "nhs_800.rds")
)

## Save all results -----------------------------------------------
save(
  list = c(
    # Popular Music loadings and NRC-VAD scores
    "billboard_imputation_400_loadings",
    "billboard_imputation_800_loadings",
    "vars_vad400",
    "vars_vad800",
    "v1_valence_billboard_400",
    "v2_arousal_billboard_400",
    "v1_valence_billboard_plot",
    "v2_arousal_billboard_plot",
    "v2_dominance_billboard_plot",
    "v2_dominance_billboard_400",
    "billboard_loadings_plot_400",
    "billboard_loadings_plot_800",
    # Unfamiliar Music loadings and NRC-VAD scores
    "nhs_imputation_400_loadings",
    "nhs_imputation_800_loadings",
    "vars_vad_nhs400",
    "vars_vad_nhs800",
    "v1_valence_nhs_400",
    "v2_arousal_nhs_400",
    "v1_valence_nhs_plot",
    "v2_arousal_nhs_plot",
    "happ_by_valence",
    "plea_by_valence",
    "exci_by_valence",
    "exci_by_arousal",
    "plea_by_arousal",
    "happ_by_arousal",
    "nhs_loadings_plot_400",
    "nhs_loadings_plot_800",
    "billboard_missingness",
    "nhs_missingness"
  ),
  file = here("results", "03_imputation_results.RData")
)
