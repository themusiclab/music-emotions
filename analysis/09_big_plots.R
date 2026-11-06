# Code for merging and formatting the big figures

rm(list = ls())

library(pacman) # install.packages(pacman)
p_load(
  tidyverse,
  here,
  ggplot2,
  ggExtra,
  patchwork,
  gridExtra,
  grid,
  mapproj,
  ggrepel # for scatterplots with overlapping dots
)

fp <- function(num) {
  if (num < .0001) {
    return("< .0001")
  } else if (num < .001) {
    return("< .001")
  } else if (num < .01) {
    return(paste0("= ", round(num, digits = 3)))
  } else {
    return(paste0("= ", round(num, digits = 2)))
  }
}

fsig <- function(num) {
  if (num < .001) {
    return("***")
  } else if (num < .01) {
    return("**")
  } else if (num < .05) {
    return(paste0("* = ", round(num, digits = 3)))
  } else {
    return(paste0("= ", round(num, digits = 2)))
  }
}

load(here("results", "0_ns.RData")) # from 0_data_processing.R
load(here("results", "01_maps.RData")) # from 01_map_builder.R
load(here("results", "02_demo_tables.RData")) # from 02_demographic_tables.R
load(here("results", "03_imputation_results.RData")) # from 03_imputation.R
load(here("results", "05_billboard_variance_results.RData")) # from 05_variance_modelling_billboard.R
load(here("results", "06_billboard_cor_results.RData")) # from 06_correlations_billboard.R
load(here("results", "07_nhs_variance_results.RData")) # from 07_variance_modelling_nhs.R
load(here("results", "08_nhs_cor_results.RData")) # from 08_correlation_nhs.R


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
  wonder = palette[9],
  valence = palette[12],
  arousal = palette[12]
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


common_theme <- theme(
  plot.margin = margin(5.5, 5.5, 5.5, 5.5),
  panel.background = element_rect(fill = "white", colour = NA),
  plot.background = element_rect(fill = "white", colour = NA)
)

## Figure 1 components ----------
#### Billboard cor plot ---------------

billboard_cor_results <- billboard_cor_results %>%
  rename("corr" = correlation) %>%
  mutate(method = "unadjusted")

billboard_correlation_results_combined <- bind_rows(
  gems_correlation_results_combined,
  billboard_cor_results
) %>%
  mutate(
    variable = factor(
      variable,
      levels = c(
        "joyful_activation",
        "nostalgia",
        "peacefulness",
        "power",
        "sadness",
        "tenderness",
        "tension",
        "transcendence",
        "wonder",
        "Valence",
        "Arousal"
      )
    )
  )

billboard_mean_corrs <- billboard_correlation_results_combined %>%
  group_by(variable, method) %>%
  summarise(mean_correlation = mean(corr, na.rm = T))


billboard_cor_plot_unadjusted <- ggplot(
  billboard_correlation_results_combined,
  aes(x = corr, fill = variable, alpha = method)
) +
  scale_alpha_manual(
    values = c(
      "adjusted" = 0.0,
      "unadjusted" = 0.75
    )
  ) +
  geom_density(color = NA) +
  facet_wrap(
    ~variable,
    ncol = 1,
    strip.position = "left",
    labeller = labeller(variable = pretty_labels)
  ) +
  scale_fill_manual(values = gems9_cols) +
  geom_vline(
    data = billboard_mean_corrs %>% filter(method == "unadjusted"),
    aes(xintercept = mean_correlation),
    color = "black",
    linetype = "dashed",
    linewidth = 0.3
  ) +
  geom_vline(
    aes(xintercept = 0),
    color = "lightgrey",
    linetype = "dashed",
    linewidth = 0.3
  ) +
  geom_text(
    data = billboard_mean_corrs %>% filter(method == "unadjusted"),
    aes(
      x = mean_correlation,
      y = 0,
      label = sprintf("%.2f", mean_correlation)
    ),
    vjust = -2,
    hjust = 1.2,
    size = 2.7,
    color = "black",
    inherit.aes = FALSE
  ) +
  labs(x = "Pearson's r") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0, hjust = 1)
  ) +
  xlim(-0.2, 1) +
  ggtitle("Popular Music") +
  common_theme

billboard_cor_plot_adjusted <- ggplot(
  gems_correlation_results_combined,
  aes(x = corr, fill = variable, alpha = method)
) +
  scale_alpha_manual(
    values = c(
      "adjusted" = 0.30,
      "unadjusted" = 0.85
    )
  ) +
  geom_density(color = NA) +
  facet_wrap(
    ~variable,
    ncol = 1,
    strip.position = "left",
    labeller = labeller(variable = pretty_labels)
  ) +
  scale_fill_manual(values = gems9_cols) +
  geom_vline(
    data = gems_mean_cors_400,
    aes(xintercept = mean_correlation),
    color = "black",
    linetype = "dashed",
    linewidth = 0.3
  ) +
  geom_vline(
    data = gems_mean_cors_400_adjusted,
    aes(xintercept = mean_adjusted_correlation),
    color = "grey40",
    linetype = "dashed",
    linewidth = 0.3
  ) +
  geom_vline(
    aes(xintercept = 0),
    color = "lightgrey",
    linetype = "dashed",
    linewidth = 0.3
  ) +
  geom_text(
    data = gems_mean_cors_400,
    aes(
      x = mean_correlation,
      y = 0,
      label = sprintf("%.2f", mean_correlation)
    ),
    vjust = -2,
    hjust = 1.2,
    size = 2.7,
    color = "black",
    inherit.aes = FALSE
  ) +
  geom_text(
    data = gems_mean_cors_400_adjusted,
    aes(
      x = mean_adjusted_correlation,
      y = 0,
      label = sprintf("%.2f", mean_adjusted_correlation)
    ),
    vjust = -2,
    hjust = -0.2,
    size = 2.7,
    color = "grey40",
    inherit.aes = FALSE
  ) +
  labs(x = "Pearson's r") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0, hjust = 1)
  ) +
  xlim(-0.2, 1) +
  ggtitle("Popular Music") +
  common_theme

#### NHS cor plot ---------------

nhs_cor_results <- nhs_cor_results %>%
  rename("corr" = correlation) %>%
  mutate(method = "unadjusted")

nhs_correlation_results_combined <- bind_rows(
  nhs_gems_correlation_results_combined,
  nhs_cor_results
) %>%
  mutate(
    variable = factor(
      variable,
      levels = c(
        "joyful_activation",
        "nostalgia",
        "peacefulness",
        "power",
        "sadness",
        "tenderness",
        "tension",
        "transcendence",
        "wonder",
        "Valence",
        "Arousal"
      )
    )
  )

nhs_mean_corrs <- nhs_correlation_results_combined %>%
  group_by(variable, method) %>%
  summarise(mean_correlation = mean(corr, na.rm = T))

nhs_cor_plot_unadjusted <- ggplot(
  nhs_correlation_results_combined,
  aes(x = corr, fill = variable, alpha = method)
) +
  scale_alpha_manual(
    values = c(
      "adjusted" = 0.0,
      "unadjusted" = 0.75
    )
  ) +
  geom_density(color = NA) +
  facet_wrap(
    ~variable,
    ncol = 1,
    strip.position = "left",
    labeller = labeller(variable = pretty_labels)
  ) +
  scale_fill_manual(values = gems9_cols) +
  geom_vline(
    data = nhs_mean_corrs %>% filter(method == "unadjusted"),
    aes(xintercept = mean_correlation),
    color = "black",
    linetype = "dashed",
    linewidth = 0.3
  ) +
  geom_vline(
    aes(xintercept = 0),
    color = "lightgrey",
    linetype = "dashed",
    linewidth = 0.3
  ) +
  geom_text(
    data = nhs_mean_corrs %>% filter(method == "unadjusted"),
    aes(
      x = mean_correlation,
      y = 0,
      label = sprintf("%.2f", mean_correlation)
    ),
    vjust = -2,
    hjust = 1.2,
    size = 2.7,
    color = "black",
    inherit.aes = FALSE
  ) +
  labs(x = "Pearson's r") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.placement = "outside",
    strip.text.y.left = element_blank()
  ) +
  xlim(0, 1) +
  ggtitle("Unfamiliar Music") +
  common_theme

nhs_cor_plot_adjusted <- ggplot(
  nhs_gems_correlation_results_combined,
  aes(x = corr, fill = variable, alpha = method)
) +
  scale_alpha_manual(
    values = c(
      "adjusted" = 0.30,
      "unadjusted" = 0.85
    )
  ) +
  geom_density(color = NA) +
  facet_wrap(
    ~variable,
    ncol = 1,
    strip.position = "left",
    labeller = labeller(variable = pretty_labels)
  ) +
  scale_fill_manual(values = gems9_cols) +
  geom_vline(
    data = nhs_gems_mean_cors_400,
    aes(xintercept = mean_correlation),
    color = "black",
    linetype = "dashed",
    linewidth = 0.3
  ) +
  geom_vline(
    data = nhs_gems_mean_cors_400_adjusted,
    aes(xintercept = mean_adjusted_correlation),
    color = "grey40",
    linetype = "dashed",
    linewidth = 0.3
  ) +
  geom_vline(
    aes(xintercept = 0),
    color = "lightgrey",
    linetype = "dashed",
    linewidth = 0.3
  ) +
  geom_text(
    data = nhs_gems_mean_cors_400,
    aes(
      x = mean_correlation,
      y = 0,
      label = sprintf("%.2f", mean_correlation)
    ),
    vjust = -2,
    hjust = 1.2,
    size = 2.7,
    color = "black",
    inherit.aes = FALSE
  ) +
  geom_text(
    data = nhs_gems_mean_cors_400_adjusted,
    aes(
      x = mean_adjusted_correlation,
      y = 0,
      label = sprintf("%.2f", mean_adjusted_correlation)
    ),
    vjust = -2,
    hjust = -0.2,
    size = 2.7,
    color = "grey40",
    inherit.aes = FALSE
  ) +
  labs(x = "Pearson's r") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    strip.placement = "outside",
    strip.text.y.left = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  xlim(-0.2, 1) +
  ggtitle("Unfamiliar Music") +
  common_theme

merged_cor_plots <- billboard_cor_plot_unadjusted | nhs_cor_plot_unadjusted

merged_cor_plots_adjusted <- billboard_cor_plot_adjusted | nhs_cor_plot_adjusted

ggsave(
  here("viz", "adjusted_corr_plot.pdf"),
  plot = merged_cor_plots_adjusted,
  height = 6,
  width = 6
)

#### zoomed in / inset plot --------------

tmp <- nhs_corrs_400 %>%
  filter(variable == "joyful_activation")

zoomed_plot <- ggplot(tmp) +
  geom_density(aes(x = correlation), fill = "#5F4690FF", alpha = 0.4) +
  geom_histogram(
    aes(x = correlation, y = after_stat(density)),
    fill = "#5F4690FF",
    alpha = 0.6,
    color = "#5F4690FF"
  ) +
  labs(x = "Pearson's r") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0, hjust = 1)
  ) +
  xlim(0, 1) +
  geom_vline(
    aes(xintercept = 0),
    color = "lightgrey",
    linetype = "dashed"
  ) +
  common_theme

ggsave(here("writing", "zoomed_fig.svg"), zoomed_plot, width = 4, height = 1)


# small theme for the table
mini_theme <- gridExtra::ttheme_minimal(
  base_size = 6,
  padding = unit(c(1, 1), "mm"),
  core = list(just = "left", x = 0),
  colhead = list(just = "left", x = 0)
)

mini_table_grob <- gridExtra::tableGrob(
  mini_table,
  rows = NULL,
  theme = mini_theme
)

# arrange table above zoomed plot, then wrap with a border
mini_block_inner <- gridExtra::arrangeGrob(
  mini_table_grob,
  ggplotGrob(zoomed_plot),
  ncol = 1,
  heights = c(1, 1)
)


zoomed_in_block <- patchwork::wrap_elements(mini_block_inner)

#### variance bar plot ------------
music_data_ratio_labels <- icc_long_plot %>%
  filter(set == "Popular" | set == "Unfamiliar") %>%
  dplyr::select(set, group, factor, relative_prop) %>%
  pivot_wider(
    id_cols = c(set, factor),
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

icc_long_plot <- icc_long_plot %>%
  left_join(music_data_ratio_labels, by = c("set", "factor", "group"))

label_df <- icc_long_plot %>%
  filter(set == "Popular" | set == "Unfamiliar") %>%
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

make_music_plot <- function(filter_set) {
  ggplot() +
    geom_col(
      data = icc_long_plot %>%
        dplyr::filter(set == filter_set, set %in% c("Popular", "Unfamiliar")),
      aes(
        x = factor,
        y = relative_prop,
        fill = factor(group, levels = c("Country", "Individual", "Song"))
      ),
      position = "stack"
    ) +
    scale_fill_manual(
      name = element_blank(),
      breaks = c("Country", "Individual", "Song"),
      values = c(
        "Country" = palette[12],
        "Song" = palette[11],
        "Individual" = palette[10]
      ),
      guide = guide_legend(reverse = TRUE)
    ) +
    coord_flip() +
    theme_classic() +
    labs(
      x = NULL,
      y = element_blank()
    ) +
    theme(
      legend.position = "bottom",
      legend.key.size = unit(0.3, "cm"),
      legend.text = element_text(size = 10),
      legend.margin = margin(0, 0, 0, 0),
      text = element_text(size = 10),
      legend.box = "vertical"
    ) +
    geom_text(
      data = label_df %>%
        dplyr::filter(set == filter_set, set %in% c("Popular", "Unfamiliar")),
      aes(x = factor, y = y_pos, label = value),
      size = 2.5,
      color = "white"
    ) +
    ggtitle(filter_set) +
    common_theme
}

music_plot_popular <- make_music_plot("Popular")
music_plot_unfamiliar <- make_music_plot("Unfamiliar")

variance_bar_plot <- music_plot_popular |
  music_plot_unfamiliar +
    plot_layout(
      guides = "collect"
    ) &
    theme(legend.position = 'bottom')

#### maps -----------

maps_row <- patchwork::wrap_elements(ppt_map) |
  nhs_map +
    plot_layout(widths = c(1, .5))


##### big chunk (inset + cor plots) ----------

final_chunk <- patchwork::wrap_plots(
  zoomed_in_block,
  merged_cor_plots,
  ncol = 1,
  heights = c(1, 5)
) &
  common_theme

# Wrap so this whole chunk gets a single tag
final_chunk_wrapped <- patchwork::wrap_elements(final_chunk)

#### full fig ------------

big_plot <- maps_row /
  final_chunk_wrapped /
  variance_bar_plot +
  plot_layout(
    heights = c(1, 6, .5)
  ) +
  plot_annotation(
    tag_levels = "a",
    theme = common_theme
  )


ggsave(here("viz", "big_plot.svg"), big_plot, height = 10, width = 8)


# Supplementary variance plot components ------------------

##### Weather plot -------
weather_variance_barplot <- ggplot() +
  geom_col(
    data = icc_long_plot %>%
      dplyr::filter(set == "Weather Data"),
    aes(
      x = factor,
      y = relative_prop,
      fill = factor(group, levels = c("Country", "Station", "Date"))
    ),
    position = "stack"
  ) +
  scale_fill_manual(
    name = NULL,
    breaks = c("Country", "Date", "Station"),
    values = c(
      "Date" = palette[11],
      "Station" = palette[10],
      "Country" = palette[12]
    ),
    guide = guide_legend(reverse = FALSE)
  ) +
  facet_grid(set ~ ., scales = "free_y", switch = "y") +
  coord_flip() +
  theme_classic() +
  labs(
    x = NULL,
    y = "% of explained variance"
  ) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 8),
    text = element_text(size = 10)
  )

##### nhs gems variance bar plot ---------

nhs_gems_variance_barplot <- ggplot(
  nhs_gems_variance_data,
  aes(fill = group, y = relative_prop, x = factor)
) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(x = NULL, y = NULL) +
  labs(fill = '') +
  coord_flip() +
  scale_x_discrete(labels = pretty_labels) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 8),
    text = element_text(size = 10)
  ) +
  scale_fill_manual(
    values = c(
      "Individual" = palette[10],
      "Song" = palette[11],
      "Country" = palette[12]
    )
  ) +
  theme(plot.margin = margin(10, 10, 10, 10))

##### billboard gems variance barplot ------------
billboard_gems_variance_barplot <- ggplot(
  billboard_gems_variance_data,
  aes(fill = group, y = relative_prop, x = factor)
) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(x = NULL, y = NULL) +
  labs(fill = '') +
  coord_flip() +
  scale_x_discrete(labels = pretty_labels) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 8),
    text = element_text(size = 10)
  ) +
  scale_fill_manual(
    values = c(
      "Individual" = palette[10],
      "Song" = palette[11],
      "Country" = palette[12]
    )
  ) +
  theme(plot.margin = margin(10, 10, 10, 10))

##### combined supplementary variance barplot -----

combined_variance_barplot <- billboard_gems_variance_barplot /
  nhs_gems_variance_barplot +
  weather_variance_barplot +
  plot_annotation(tag_levels = 'a') +
  plot_layout(guides = "collect")

ggsave(here("viz", "figS4.pdf"), height = 5, width = 6)

# Figure 2 cluster plot components -------------------

##### Original behavioural context and Valence/Arousal -----------------
cluster_data <- cluster_data %>%
  mutate(type = factor(type, levels = c("Dance", "Healing", "Love", "Lullaby")))


context_cluster_plot <- ggplot(
  cluster_data,
  aes(x = Valence, y = Arousal, color = type)
) +

  stat_ellipse(
    aes(fill = type),
    geom = "polygon",
    alpha = 0.1,
    color = NA,
    show.legend = FALSE
  ) +
  geom_point(size = 1.5) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_fill_manual(
    name = "Original Song Context",
    values = c(
      "Dance" = "#5F4690FF",
      "Lullaby" = "#73AF48FF",
      "Healing" = "#CC503EFF",
      "Love" = "#EDAD08FF"
    )
  ) +
  scale_color_manual(
    name = "Original Song Context",
    values = c(
      "Dance" = "#5F4690FF",
      "Lullaby" = "#73AF48FF",
      "Healing" = "#CC503EFF",
      "Love" = "#EDAD08FF"
    )
  ) +
  labs(x = "Mean Valence Rating", y = "Mean Arousal Rating") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  guides(color = guide_legend(override.aes = list(size = 5)))

context_cluster_plot_marginal <- ggMarginal(
  context_cluster_plot,
  alpha = .3,
  groupColour = TRUE,
  groupFill = TRUE,
  type = "boxplot",
  outliers = F
)

ggsave(
  here("viz", "context_cluster_plot.pdf"),
  context_cluster_plot_marginal,
  width = 6,
  height = 6
)

cluster_data_gems <- cluster_data_gems %>%
  mutate(type = factor(type, levels = c("Dance", "Healing", "Love", "Lullaby")))

ggplot(data = cluster_data_gems, aes(x = mean)) +
  geom_boxplot(aes(fill = type), outliers = FALSE) +
  facet_wrap(. ~ gems_9, labeller = labeller(gems_9 = pretty_labels)) +
  theme_classic() +
  geom_vline(
    aes(xintercept = 0),
    color = "black",
    linetype = "dashed",
    linewidth = 0.3
  ) +
  scale_fill_manual(
    name = "Original Song Context",
    values = c(
      "Dance" = "#5F4690FF",
      "Lullaby" = "#73AF48FF",
      "Healing" = "#CC503EFF",
      "Love" = "#EDAD08FF"
    )
  ) +
  theme(legend.position = "bottom")


#### perceived function plots (web) ----------------

ff_data_long <- ff_data_long %>%
  mutate(
    type = factor(type, levels = c("Dance", "Healing", "Love", "Lullaby"))
  ) %>%
  mutate(
    target = factor(
      target,
      levels = c(
        "Dancing",
        "Healing illness",
        "Expressing love",
        "Soothing infants"
      )
    )
  )

# add sig stars for tests
annot <- data.frame(
  target = c(
    "Dancing",
    "Dancing",
    "Healing illness",
    "Healing illness",
    "Expressing love",
    "Expressing love",
    "Soothing infants",
    "Soothing infants"
  ),
  dimension = c(
    "Valence",
    "Arousal",
    "Valence",
    "Arousal",
    "Valence",
    "Arousal",
    "Valence",
    "Arousal"
  ),
  x = .9,
  y = 2.8,
  label = c(
    ((ff_mods[["web"]][["danc_v1"]]$p.value)[2] %>% fsig),
    ((ff_mods[["web"]][["danc_v2"]]$p.value)[2] %>% fsig),
    ((ff_mods[["web"]][["heal_v1"]]$p.value)[2] %>% fsig),
    ((ff_mods[["web"]][["heal_v2"]]$p.value)[2] %>% fsig),
    ((ff_mods[["web"]][["love_v1"]]$p.value)[2] %>% fsig),
    ((ff_mods[["web"]][["love_v2"]]$p.value)[2] %>% fsig),
    ((ff_mods[["web"]][["baby_v1"]]$p.value)[2] %>% fsig),
    ((ff_mods[["web"]][["baby_v2"]]$p.value)[2] %>% fsig)
  )
) %>%
  mutate(
    target = factor(
      target,
      levels = c(
        "Dancing",
        "Healing illness",
        "Expressing love",
        "Soothing infants"
      )
    )
  )

perceived_function_plot_web <- ggplot(
  ff_data_long %>% filter(study == "web"),
  aes(x = rating, y = mean_perception_rating, color = type)
) +
  geom_smooth(aes(group = 1), method = "lm", color = "grey") +
  geom_point(size = 1) +
  geom_text(
    data = annot,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    size = 3
  ) +
  facet_grid(
    target ~ dimension,
    switch = "x",
    labeller = labeller(
      dimension = c(
        "Valence" = "Mean Valence Rating",
        "Arousal" = "Mean Arousal Rating"
      )
    )
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    strip.background = element_blank(),
    strip.text.y = element_text(angle = 0),
    strip.placement = "outside"
  ) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  labs(
    color = "Original Song Context",
    y = "Perceived appropriateness for...",
    x = NULL
  ) +
  scale_color_manual(
    values = c(
      "Dance" = "#5F4690FF",
      "Lullaby" = "#73AF48FF",
      "Healing" = "#CC503EFF",
      "Love" = "#EDAD08FF"
    )
  ) +
  scale_y_continuous(breaks = c(-2, 0, 2))

#### perceived function plots (field) ----------------
# add sig stars for tests
annot_field <- data.frame(
  target = c(
    "Dancing",
    "Dancing",
    "Soothing infants",
    "Soothing infants",
    "Healing illness",
    "Healing illness",
    "Expressing love",
    "Expressing love"
  ),
  dimension = c(
    "Valence",
    "Arousal",
    "Valence",
    "Arousal",
    "Valence",
    "Arousal",
    "Valence",
    "Arousal"
  ),
  x = .9,
  y = 2.7,
  label = c(
    ((ff_mods[["field"]][["danc_v1"]]$p.value)[2] %>% fsig),
    ((ff_mods[["field"]][["danc_v2"]]$p.value)[2] %>% fsig),
    ((ff_mods[["field"]][["baby_v1"]]$p.value)[2] %>% fsig),
    ((ff_mods[["field"]][["baby_v2"]]$p.value)[2] %>% fsig),
    ((ff_mods[["field"]][["heal_v1"]]$p.value)[2] %>% fsig),
    ((ff_mods[["field"]][["heal_v2"]]$p.value)[2] %>% fsig),
    ((ff_mods[["field"]][["love_v1"]]$p.value)[2] %>% fsig),
    ((ff_mods[["field"]][["love_v2"]]$p.value)[2] %>% fsig)
  )
) %>%
  mutate(
    target = factor(
      target,
      levels = c(
        "Dancing",
        "Healing illness",
        "Expressing love",
        "Soothing infants"
      )
    )
  )


perceived_function_plot_field <- ggplot(
  ff_data_long %>% filter(study == "field"),
  aes(x = rating, y = mean_perception_rating, color = type)
) +
  geom_smooth(aes(group = 1), method = "lm", color = "grey") +
  geom_point(size = 1) +
  geom_text(
    data = annot_field,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    size = 3
  ) +
  facet_grid(
    target ~ dimension,
    switch = "x",
    labeller = labeller(
      dimension = c(
        "Valence" = "Mean Valence Rating",
        "Arousal" = "Mean Arousal Rating"
      )
    )
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    strip.background = element_blank(),
    strip.text.y = element_text(angle = 0),
    strip.placement = "outside"
  ) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  labs(
    color = "Original Song Context",
    y = "Perceived appropriateness for...",
    x = NULL
  ) +
  scale_color_manual(
    values = c(
      "Dance" = "#5F4690FF",
      "Lullaby" = "#73AF48FF",
      "Healing" = "#CC503EFF",
      "Love" = "#EDAD08FF"
    )
  ) +
  scale_y_continuous(breaks = c(-2, 0, 2))

perceived_func_plots <- perceived_function_plot_web +
  perceived_function_plot_field +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(tag_levels = 'a') +
  plot_layout(
    guides = "collect"
  ) &
  theme(legend.position = 'bottom')


ggsave(here("viz", "figS5.pdf"), height = 7, width = 6)


# loadings and validating interpretation (billboard)
validation_block <- (v1_valence_billboard_plot + v2_arousal_billboard_plot)

(wrap_elements(billboard_loadings_plot_400) /
  validation_block) +
  plot_layout(heights = c(1.5, 1)) +
  plot_annotation(tag_levels = 'a')

ggsave(here("viz", "figS1.pdf"), height = 7, width = 6)

# loadings and validating interpretation (nhs)

validation_block <- v1_valence_nhs_plot + v2_arousal_nhs_plot

(wrap_elements(nhs_loadings_plot_400) /
  validation_block) +
  plot_layout(heights = c(1.5, 1)) +
  plot_annotation(tag_levels = 'a')

ggsave(here("viz", "figS2.pdf"), height = 7, width = 6)

# loadings for 800 ppt

billboard_loadings_plot_800 /
  nhs_loadings_plot_800 +
  plot_annotation(tag_levels = 'a') +
  plot_layout(guides = "collect")

ggsave(here("viz", "figS6.pdf"), height = 8, width = 6)

# Save results ------------------------------------
save(
  billboard_cor_plot_adjusted,
  nhs_cor_plot_adjusted,
  zoomed_plot,
  mini_table,
  variance_bar_plot,
  big_plot,
  weather_variance_barplot,
  nhs_gems_variance_barplot,
  combined_variance_barplot,
  context_cluster_plot_marginal,
  perceived_func_plots,
  file = here("results", "plots.RData")
)
