# Data imputation (tweaked) -------------------------------------------
# In code review: "CV error over all cells, not held-out (biases λ; correct object `imp` unused)."
# The original function artificially hid observed cells but calculated prediction error across all originally observed cells
# rather than those that were deliberately held out.
# the cv function in question comes from a peer-reviewed and maintained set of sources (R-miss-tastic),
# but I have re-run the imputation scripts with the suggested edits to confirm they don't change the resulting latent variables or their interpretation

rm(list = ls())

library(pacman) # install.packages(pacman)
p_load(
  tidyverse,
  here,
  lme4,
  ggrepel, # for plotting (prevent overlapping scatterplot labels)
  softImpute, # for imputation and dimension reduction
  devtools
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

# popular western music data
billboard_data <- processed_data %>%
  filter(corpus == "billboard")

# unfamiliar global music data
nhs_data <- processed_data %>%
  filter(corpus == "nhs")

## SoftImpute: impute missing data and reduce dimensionality, as described in Methods ---------
# We first use cross validation to find the optimal lambda value for softimpute
# Then run softimpute to impute residuals (after modelling systematic variation due to country/participant/song)

# EDITED IN LINE WITH CODE AUDIT: Function output is [1] loadings, [2] optimal number of ranks/dimensions, [3] imputed and reduced data
impute_and_reduce_edited <- function(n_ppt = numeric(), data = data.frame()) {
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
      allow.new.levels = TRUE
    ) # fills NAs with predicted values

    # Store model residuals (idiosyncratic part of variance)
    residuals_df[[emotion]] <- residuals(fit)
  }

  # convert residuals and predicted values to matrices for softImpute
  predicted_to_impute_matrix <- as.matrix(predicted_df)
  residuals_to_impute_matrix <- as.matrix(residuals_df)

  # 4. Cross-validation function to choose best lambda value for softImpute (with perc.missing tweaked from 0.2 to 0.1)
  # Source: https://raw.githubusercontent.com/R-miss-tastic/website/master/static/how-to/impute/CrossValidation_softImpute.R
  # edits are highlighted in code
  cv_sft_edited <- function(y, N = 10, len = 20) {
    y <- as.matrix(y)

    Y2 <- y
    Y2[is.na(Y2)] <- 0

    lambda1.max <- max(svd(Y2)$d)
    lambda1.min <- 1e-3 * lambda1.max

    grid.lambda1 <- exp(
      seq(log(lambda1.min), log(lambda1.max), length.out = len)
    )

    # Change 1 + 2:
    # Create N artificial-missingness splits once.
    # Each object retains both the incomplete data and the held-out-cell mask.
    res.cv <- lapply(seq_len(N), function(k) {
      message("CV split ", k, " of ", N)

      # Create and retain only one CV split at a time
      split <- produce_NA(as.matrix(y), perc.missing = 0.1)

      yy <- as.matrix(split$data.incomp)
      held_out <- as.matrix(split$idx_newNA)

      sapply(seq_len(len), function(i) {
        message("  Lambda ", i, " of ", len)

        fit <- softImpute(
          yy,
          lambda = grid.lambda1[i],
          maxit = 1000
        )

        u <- fit$u
        d <- fit$d
        v <- fit$v

        reconstructed <- if (is.null(dim(u))) {
          d * u %*% t(v)
        } else {
          u %*% diag(d) %*% t(v)
        }

        # Change 3: evaluate only values deliberately hidden from softImpute
        sqrt(mean(
          (reconstructed[held_out] - y[held_out])^2
        ))
      })
    })

    res.cv <- colMeans(do.call(rbind, res.cv))

    data.frame(
      lambda = grid.lambda1,
      error = res.cv
    )
  }

  # 5. Run CV on the residual matrix and find lambda with minimum error
  lambda_sft <- cv_sft_edited(residuals_to_impute_matrix, N = 10, len = 20)
  best_lambda <- lambda_sft[which(lambda_sft$error == min(lambda_sft$error)), 1]

  # 6. Fit final softImpute model on residuals using the chosen lambda value
  residuals_fit <- softImpute(
    residuals_to_impute_matrix,
    lambda = best_lambda,
    type = "svd",
    maxit = 1000
  )

  n_rank <- residuals_fit$d %>% length # number of ranks

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
}


## Run imputation and dimension reduction ------------------------------

billboard_imputation_400_edited <- impute_and_reduce_edited(
  n_ppt = 400,
  data = billboard_data
)


saveRDS(
  billboard_imputation_400_edited,
  here("results/imputations", "billboard_400_edited.rds")
)

# Study 2 (at two different sample sizes)
nhs_imputation_400_edited <- impute_and_reduce_edited(
  n_ppt = 400,
  data = nhs_data
)

saveRDS(
  nhs_imputation_400_edited,
  here("results/imputations", "nhs_400_edited.rds")
)

# extract loadings

billboard_imputation_400_loadings_edited <- billboard_imputation_400_edited$loadings %>%
  as.data.frame()

nhs_imputation_400_loadings_edited <- nhs_imputation_400_edited$loadings %>%
  as.data.frame()


## Validating interpretations of dimensions for Study 1 (Billboard data) -------------------------

### Do the billboard loadings track Arousal and Valence? -----------
nrc <- read_delim(here("data", "NRC-VAD-Lexicon-v2.1.txt"))

vars_vad400_edited <- billboard_imputation_400_loadings_edited %>%
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
cor.test(vars_vad400_edited$Component_1, vars_vad400_edited$valence) # component_1 positively correlates with valence
cor.test(vars_vad400_edited$Component_2, vars_vad400_edited$valence) # not sig.
cor.test(vars_vad400_edited$Component_1, vars_vad400_edited$arousal) # not sig.
cor.test(vars_vad400_edited$Component_2, vars_vad400_edited$arousal) # component_2 negatively correlates with arousal
cor.test(vars_vad400_edited$Component_1, vars_vad400_edited$dominance) # not sig.
cor.test(vars_vad400_edited$Component_2, vars_vad400_edited$dominance) # component_2 negatively correlates with arousal

# create reversed component_2 for easier interpretation
billboard_imputation_400_loadings_edited$Component_2_reversed <- (-1) *
  billboard_imputation_400_loadings_edited$Component_2

# correlation models for reporting in manuscript
vars_vad400_edited <- vars_vad400_edited %>%
  bind_cols(
    Component_2_reversed = billboard_imputation_400_loadings_edited$Component_2_reversed
  )

# comp1 correlates with valence
cor.test(
  vars_vad400_edited$Component_1,
  vars_vad400_edited$valence
)

# comp2 correlates with arousal
cor.test(
  vars_vad400_edited$Component_2_reversed,
  vars_vad400_edited$arousal
)

# comp2 correlates with dominance
cor.test(
  vars_vad400_edited$Component_2_reversed,
  vars_vad400_edited$dominance
)

### Figures S1b and S1c: Study 1 loadings and NRC Valence/Arousal values ---------------------
ggplot(
  vars_vad400_edited,
  aes(x = Component_1, y = valence)
) +
  geom_smooth(method = lm, se = T, color = "#855686FF") +
  geom_point(size = 1) +
  geom_text_repel(aes(x = Component_1, y = valence, label = term), size = 3) +
  theme_classic() +
  labs(x = "Loadings on Component 1", y = "Term-Valence Ratings")

ggplot(
  vars_vad400_edited,
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

ggplot(
  vars_vad400_edited,
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

### Figure S1a: Study 1 loadings plot (with 400 participants) -----------------------------
billboard_imputation_400_loadings_edited %>%
  as.data.frame() %>%
  rownames_to_column("emotion") %>%
  # colour gems25 dimensions according to gems9 factor solution
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


## Validating interpretations of dimensions for global music (NHS data) -------------------------

### Do the NHS loadings track Arousal and Valence (from NRC-VAD)? -----------------
vars_vad_nhs400_edited <- nhs_imputation_400_loadings_edited %>%
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

cor.test(vars_vad_nhs400_edited$Component_1, vars_vad_nhs400_edited$valence) # component_1 negatively correlates with valence
cor.test(vars_vad_nhs400_edited$Component_2, vars_vad_nhs400_edited$valence) # n.s.
cor.test(vars_vad_nhs400_edited$Component_1, vars_vad_nhs400_edited$arousal) # n.s.
cor.test(vars_vad_nhs400_edited$Component_2, vars_vad_nhs400_edited$arousal) # component_2 positively correlates with arousal
cor.test(vars_vad_nhs400_edited$Component_1, vars_vad_nhs400_edited$dominance) # n.s.
cor.test(vars_vad_nhs400_edited$Component_2, vars_vad_nhs400_edited$dominance) # n.s.

# reverse component_1 for interpretation
nhs_imputation_400_loadings_edited$Component_1_reversed <- (-1) *
  nhs_imputation_400_loadings_edited$Component_1

# correlation models for reporting in manuscript
vars_vad_nhs400_edited <- vars_vad_nhs400_edited %>%
  bind_cols(
    Component_1_reversed = nhs_imputation_400_loadings_edited$Component_1_reversed
  )

# comp1 correlates with valence
cor.test(
  vars_vad_nhs400_edited$Component_1_reversed,
  vars_vad_nhs400_edited$valence
)

# comp2 correlates with arousal
cor.test(
  vars_vad_nhs400_edited$Component_2,
  vars_vad_nhs400_edited$arousal
)

# n.s.
cor.test(
  vars_vad_nhs400_edited$Component_2,
  vars_vad_nhs400_edited$dominance
)

### Figures S3b and S3c: Study 2 loadings and NRC Valence/Arousal values ---------------------
ggplot(
  vars_vad_nhs400_edited,
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

ggplot(
  vars_vad_nhs400_edited,
  aes(x = Component_2, y = arousal)
) +
  geom_smooth(method = lm, se = T, color = "#855686FF") +
  geom_point(size = 1) +
  geom_text_repel(aes(x = Component_2, y = arousal, label = term), size = 3) +
  theme_classic() +
  labs(x = "Loadings on Component 2", y = "Term-Arousal Ratings")

### Figure S3a: Study 2 loadings plot (with 400 participants) -------------------------
nhs_imputation_400_loadings_edited %>%
  as.data.frame() %>%
  rownames_to_column("emotion") %>%
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
      fill = "transparent", # necessary to avoid drawing panel outline
      colour = NA_character_
    )
  )
