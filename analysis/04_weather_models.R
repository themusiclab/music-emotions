# Weather data analyses ------------------------------------------
# This script compiles the weather data, runs variance models, and also runs an imputation script equivalent to the one run on the music/emotion data

rm(list = ls())

library(pacman) # install.packages(pacman)
p_load(
  tidyverse,
  here,
  lmerTest,
  lme4
)

## GSOY compiler -------------------------------------------------
# GSOY weather data is available online, but has to first be accessed and compiled into a huge dataframe.

# We trimmed this to a smaller weather dataset by dropping data that doesn't contain relevant variables.
# The cleaned output is saved in /data

compile_gsoy = FALSE # You will need the full compiled dataset if you want to run this chunk

if (compile_gsoy) {
  gsoy_files <- list.files(here("data/gsoy-latest")) # this file was stored locally as it is huge
  gsoy_compiled <- data.frame()

  # Loop through each file and check if it contains TAVG or TMAX data, if it does, append to gsoy_compiled
  vars_to_keep <- c(
    "STATION",
    "DATE",
    "TMAX",
    "TAVG",
    "DP01",
    "DSND",
    "EVAP",
    "EMNT",
    "PRCP"
  )

  for (i in seq_along(gsoy_files)) {
    tmp <- read.csv(here("data/gsoy-latest", gsoy_files[i]))

    # keep only columns that exist in the current dataset
    keep <- intersect(vars_to_keep, names(tmp))

    # rename STATION and DATE consistently, others stay as-is
    tmp_sel <- tmp %>%
      dplyr::select(
        station = STATION,
        date = DATE,
        dplyr::all_of(setdiff(keep, c("STATION", "DATE")))
      )

    gsoy_compiled <- bind_rows(gsoy_compiled, tmp_sel)
  }
  write_csv(gsoy_compiled, here("results", "gsoy_compiled_test.csv.gz"))
}


gsoy_compiled <- read_csv(here("data", "gsoy_compiled.csv.gz")) %>%
  mutate(country_code = strtrim(station, 2))

# Percentage of non-missing data
gsoy_compiled %>% summarise(across(everything(), ~ mean(!is.na(.)) * 100))

# scale measurement data
gsoy_compiled <- gsoy_compiled %>%
  mutate(
    tavg_scaled = scale(TAVG, center = TRUE, scale = TRUE),
    tmax_scaled = scale(TMAX, center = TRUE, scale = TRUE),
    dp01_scaled = scale(DP01, center = TRUE, scale = TRUE),
    dsnd_scaled = scale(DSND, center = TRUE, scale = TRUE),
    prcp_scaled = scale(PRCP, center = TRUE, scale = TRUE),
    emnt_scaled = scale(EMNT, center = TRUE, scale = TRUE)
  )


vars_to_model <- c(
  "tavg_scaled",
  "tmax_scaled",
  "dp01_scaled",
  "dsnd_scaled",
  "prcp_scaled",
  "emnt_scaled"
)


get_variance_components <- function(var) {
  # fit same model as in emotion/music data
  form <- as.formula(paste0(
    var,
    " ~ (1|date) + (1|country_code) + (1|station)"
  ))

  model <- lmer(
    form,
    gsoy_compiled,
    control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6))
  )

  varcorr <- VarCorr(model) %>% # extract variance components
    as.data.frame() %>%
    select(group = grp, variance = vcov) %>%
    mutate(
      prop_var = variance / sum(variance), # proportion of variance explained
      factor = var
    ) %>%
    mutate(
      group = factor(
        group,
        levels = c("station", "date", "country_code", "Residual")
      ),
      group = fct_recode(
        group,
        Individual = "station",
        Date = "date",
        Country = "country_code"
      )
    )

  return(varcorr)
}

weather_variance_data <- map_df(vars_to_model, get_variance_components)

## Save results ------------------------------------------
save(
  weather_variance_data,
  file = here("results", "04_weather_variance_data.rData")
)
