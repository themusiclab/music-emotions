library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(here)
library(DT)
library(tidyr)
library(readr)
library(shinycssloaders)
library(highcharter)
library(countrycode)

# RUN SETWD WHEN TROUBLESHOOTING
#setwd(here("evoke_player"))

# -------------------------
# SET UP DATA: commented out sections generate the data the app uses, they are preserved here for transparency
# -------------------------

### GENERATE GROUP MEANS: only needs to be run once

# billboard_reduced_data <- readRDS(here("results/imputations","billboard_400.rds")) %>%
#   rename(valence = V1,
#          arousal = V2_reversed)
#
# nhs_reduced_data <- readRDS(here("results/imputations","nhs_400.rds")) %>%
#   rename(valence = V1_reversed,
#          arousal = V2)
#
# full_data <- bind_rows(
#   Billboard = billboard_reduced_data,
#   NHS = nhs_reduced_data,
#   .id = "corpus"
# ) %>%
#   group_by(corpus) %>%
#   mutate(valence_scaled = scale(valence),
#          arousal_scaled = scale(arousal)) %>%
#   ungroup()
#
# # Global means
# global_means <- full_data %>%
#   group_by(corpus, song) %>%
#   summarise(Valence = mean(valence_scaled),
#             Arousal = mean(arousal_scaled),
#             .groups = "drop")

# write_csv(global_means,here("evoke_player/data","global_means_imputed.csv"))

# COUNTRY MEANS FOR AROUSAL AND VALENCE
global_means <- read_csv("./data/global_means_imputed.csv") %>%
  mutate(
    corpus = case_when(
      corpus == "NHS" ~ "nhs",
      corpus == "Billboard" ~ "billboard"
    )
  )


# bb_data <- read_csv(here("data","billboard_meta_data.csv")) %>%
#   mutate(name = song,
#          song = gsub("^Cut_|\\.mp3$", "", filename),
#          source = gsub(".mp3$", "__12s.mp3", filename),
#          source = paste0("https://www.themusiclab.org/quizzes/melo/audio/billboard_12s/", source))
# nhs_data <- read_csv(here("data","NHSDiscography_Metadata.csv")) %>%
#   mutate(source=song,
#          source= paste0("https://zenodo.org/records/7265514/files/",source,".mp3"),
#          name = song,
#          song = gsub("^NHSDiscography-", "", song) %>% as.numeric() %>% as.character(),
#          name = case_when(type=="Healing" ~ "Healing song",
#                           type=="Love" ~"Love song",
#                           type=="Dance" ~"Dance song",
#                           type=="Lullaby" ~"Lullaby",
#                           T ~ type),
#          artist = paste0(" from ", culture, " culture")) %>%
#   dplyr::select(song, type, hraf_subregion,hraf_region,name,artist,source, latitude, longitude)
#
# meta_data <- bind_rows(bb_data, nhs_data)
#
# write_csv(meta_data,here("evoke_player/data","meta_data.csv"))

# META DATA FOR SONGS
meta_data <- read_csv("./data/meta_data.csv")

# JOIN META DATA TO COUNTRY MEANS
global_means <- global_means %>%
  pivot_longer(
    cols = c(Valence, Arousal),
    names_to = "emotion",
    values_to = "rating"
  ) %>%
  left_join(meta_data, by = "song") %>%
  mutate(type = case_when(is.na(type) ~ "Popular music", TRUE ~ type))

# # Country means
# country_means <- full_data %>%
#   group_by(corpus, country, song) %>%
#   summarise(Valence = mean(valence_scaled),
#             Arousal = mean(arousal_scaled),
#             .groups = "drop") %>%
#   pivot_longer(cols = c(Valence, Arousal),
#                names_to = "emotion",
#                values_to = "rating") %>%
#   left_join(meta_data, by = "song")

# write_csv(country_means,here("evoke_player/data","country_means.csv"))
country_means <- read_csv("./data/country_means.csv")


## GEMS-9 DATA: GET COUNTRY MEANS
#processed_data <- read_csv(here("results","tml_wwl_data.csv.gz"))
#
# long_data <- processed_data %>%
#   drop_na(rating) %>%
#   group_by(corpus) %>%
#   # Assign each gems-25 scale to relevant gems-9 factor
#   mutate(gems_9 = case_when(emotion %in% c("moved","filled_with_wonder","allured") ~ "wonder",
#                             emotion %in% c("fascinated","overwhelmed","transcendent") ~ "transcendence",
#                             emotion %in% c("strong","energetic","triumphant") ~ "power",
#                             emotion %in% c("tender","affectionate","mellowed_softened_up") ~ "tenderness",
#                             emotion %in% c("nostalgic","sentimental","dreamy") ~ "nostalgia",
#                             emotion %in% c("serene","calm","soothed") ~ "peacefulness",
#                             emotion %in% c("animated","bouncy","joyful") ~ "joyful_activation",
#                             emotion %in% c("sad","tearful") ~ "sadness",
#                             emotion %in% c("tense","agitated") ~ "tension")) %>%
#   group_by(country, corpus,song,gems_9) %>%
#   # calculate gems-9 score by averaging all available ratings for that factor
#   summarise(rating = mean(rating),
#             n = n_distinct(user_id)) %>%
#   ungroup() %>%
#   group_by(gems_9) %>%
#   # mean centering: scores now indicate whether a song is more or less "transcendent" than average song
#   mutate(rating_z = as.numeric(scale(rating, center = TRUE, scale = TRUE)))
#
# write_csv(long_data,here("evoke_player/data","gems_data_for_shinyapp.csv"))
gems_data <- read_csv("./data/gems_data_for_shinyapp.csv")

gems_global_means <- gems_data %>%
  left_join(meta_data, by = "song") %>%
  group_by(corpus, song, gems_9, artist, name, source, latitude, longitude) %>%
  summarise(rating = mean(rating_z), .groups = "drop") %>%
  mutate(emotion = gems_9)

gems_country_means <- gems_data %>%
  mutate(
    emotion = gems_9,
    rating = rating_z,
    iso3 = countrycode(country, "country.name", "iso3c")
  ) %>%
  filter(!is.na(iso3)) %>%
  left_join(meta_data, by = "song")


# LOAD WORLD GEOJSON

world_hc <- jsonlite::fromJSON("./data/world.geo.json", simplifyVector = FALSE)


# SET UP UI

ui <- fluidPage(
  # prevent two audios from playing at same time
  tags$head(
    tags$script(HTML(
      "
      document.addEventListener('play', function(event) {
        const currentAudio = event.target;

        document.querySelectorAll('audio').forEach(function(audio) {
          if (audio !== currentAudio) {
            audio.pause();
            audio.currentTime = 0;  
          }
        });
      }, true);
    "
    ))
  ),

  titlePanel("Emotions Evoked by Music"),

  tabsetPanel(
    tabPanel(
      "Two-factor solution",

      sidebarLayout(
        sidebarPanel(
          selectInput(
            "corpus",
            "Pick your music:",
            choices = c(
              "Unfamiliar Music" = "nhs",
              "Popular Music" = "billboard"
            )
          ),

          uiOutput("blurb2")
        ),

        mainPanel(
          fluidRow(
            column(
              8,
              highchartOutput("twod_plot") %>%
                withSpinner(color = "#333", type = 4),
              uiOutput("selected_song_twod"),
              tags$div(
                style = "text-align: center;",
                uiOutput("audio_player_twod")
              )
            )
          )
        )
      )
    ),

    tabPanel(
      "Emotions across countries",

      sidebarLayout(
        sidebarPanel(
          selectInput(
            "corpus",
            "Pick your music:",
            choices = c(
              "Unfamiliar Music" = "nhs",
              "Popular Music" = "billboard"
            )
          ),

          selectInput(
            "emotion",
            "Pick the emotion:",
            choices = c(
              "Joyful" = "joyful_activation",
              "Sad" = "sadness",
              "Tender" = "tenderness",
              "Tense" = "tension",
              "Nostalgic" = "nostalgia",
              "Powerful" = "power",
              "Transcendent" = "transcendence",
              "Wonder" = "wonder",
              "Peaceful" = "peacefulness"
            )
          ),
          selectInput(
            "n_song",
            "Pick a number of songs to show:",
            choices = c(3, 4, 5, 6, 7, 8, 9, 10)
          ),

          uiOutput("blurb")
        ),
        mainPanel(
          fluidRow(
            column(
              8,
              uiOutput("top_songs_title"),
              DTOutput("top_items"),
              highchartOutput("rating_plot") %>%
                withSpinner(color = "#333", type = 4)
            )
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  output$blurb <- renderText({
    "Which songs induce which emotions around the world?  <br> <br> Listen to the songs that received the highest ratings across nine emotion dimensions (GEMS-9; Zentner et al. 2008). <br> <br>
    Click on a song to see how it was rated around the world. (Similar colours mean similar ratings.)"
  })

  output$blurb2 <- renderText({
    "We collapsed all emotion ratings into two dimensions that do a good job explaining people's responses.  <br> <br>
    We called the first dimension 'valence': songs that are high valence tend to be associated with typically 'positive' feelings, like feeling joyful or being moved. Songs that made people feel sad or tense score lower — although, typically 'negative' emotions like sadness are still a huge part of enjoying music!  <br> <br>
    We called the second dimension 'arousal': songs high on arousal are more bouncy and energetic, while those on the lower end are more soothing or calm.  <br> <br>
    Click a data point to hear the song."
  })

  output$top_songs_title <- renderUI({
    h1(paste("Top", input$n_song, "Songs"))
  })

  filtered_data <- reactive({
    gems_global_means %>%
      filter(corpus == input$corpus, emotion == input$emotion) %>%
      arrange(desc(rating)) %>%
      select(song, name, artist, rating, source)
  })

  output$selected_song_twod <- renderUI({
    req(input$selected_song_twod_info)

    song <- input$selected_song_twod_info

    tags$div(
      style = "
      text-align: center;
      margin-top: 10px;
      margin-bottom: 6px;
    ",
      tags$strong(song$name),
      tags$br(),
      tags$span(song$artist)
    )
  })
  output$top_items <- renderDT({
    top_df <- head(filtered_data(), n = as.numeric(input$n_song)) %>%
      mutate(Audio = paste0('<audio controls src="', source, '"></audio>'))

    datatable(
      top_df %>% select(name, artist, Audio),
      escape = FALSE,
      selection = list(mode = "single", selected = 1),
      colnames = NULL,
      options = list(
        dom = 't',
        pageLength = as.numeric(input$n_song),
        columnDefs = list(
          list(width = '150px', targets = 2)
        )
      )
    )
  })

  selected_top <- reactive({
    if (is.null(input$top_items_rows_selected)) {
      return(NULL)
    }

    head(filtered_data(), 5)[input$top_items_rows_selected, , drop = FALSE]
  })

  output$audio_player <- renderUI({
    req(selected_top()) # a song must be first selected

    tags$audio(
      src = selected_top()$source,
      type = "audio/mpeg",
      controls = TRUE
    )
  })

  filtered_from_click <- reactive({
    df <- gems_country_means %>%
      filter(corpus == input$corpus, emotion == input$emotion)

    sel <- selected_top()

    # get default song from the same data as the table
    default_song <- filtered_data() %>%
      slice(1) %>%
      pull(song)

    if (is.null(sel) || is.null(sel$song)) {
      df %>% filter(song == default_song)
    } else {
      df %>% filter(song == sel$song)
    }
  })

  # TOP MAP

  output$rating_plot <- renderHighchart({
    df <- filtered_from_click()
    req(nrow(df) > 0)

    highchart(type = "map") %>%
      hc_add_series_map(
        map = world_hc,
        df = df,
        joinBy = c("iso-a3", "iso3"),
        value = "rating",
        name = "Mean rating",
        dataLabels = list(enabled = FALSE),
        tooltip = list(
          pointFormat = "{point.name}<br>Mean rating: {point.value:.2f}"
        )
      ) %>%

      hc_add_series(
        data = df %>% filter(!is.na(latitude) & !is.na(longitude)),
        type = "mappoint",
        hcaes(
          lon = longitude,
          lat = latitude,
          name = name,
          artist = artist
        ),
        color = "black",
        marker = list(
          radius = 6,
          fillColor = "red",
          lineColor = "white"
        ),
        dataLabels = list(enabled = FALSE),
        tooltip = list(
          headerFormat = "",
          pointFormat = "<b>{point.name}</b><br>{point.artist}"
        )
      ) %>%

      hc_mapNavigation(enabled = TRUE) %>%
      hc_colorAxis(
        min = -1.2,
        max = 1.2,
        stops = color_stops(
          colors = viridisLite::viridis(10)
        )
      )
  })

  data_for_twod_plot <- reactive({
    global_means %>%
      filter(corpus == input$corpus) %>%
      pivot_wider(
        id_cols = c(song, type, artist, name, source), # include source
        names_from = emotion,
        values_from = rating
      )
  })

  type_colors <- c("#5F4690FF", "#CC503EFF", "#EDAD08FF", "#73AF48FF")

  output$twod_plot <- renderHighchart({
    tmp <- data_for_twod_plot()
    req(nrow(tmp) > 0)

    hchart(
      tmp,
      "scatter",
      hcaes(x = Valence, y = Arousal, group = type)
    ) %>%
      hc_xAxis(min = -1, max = 1, title = list(text = "Valence")) %>%
      hc_yAxis(min = -1, max = 1, title = list(text = "Arousal")) %>%
      hc_colors(type_colors) %>%
      hc_tooltip(pointFormat = "<b>{point.name}</b><br>{point.artist}") %>%
      hc_plotOptions(
        series = list(
          cursor = "pointer",
          allowPointSelect = TRUE,

          marker = list(
            states = list(
              select = list(
                enabled = TRUE,
                radius = 8,
                fillColor = "red",
                lineColor = "red",
                lineWidth = 2
              )
            )
          ),

          point = list(
            events = list(
              click = JS(
                "function() {
             Shiny.setInputValue(
               'selected_audio',
               this.source,
               {priority: 'event'}
             );

             Shiny.setInputValue(
               'selected_song_twod_info',
               {
                 name: this.name,
                 artist: this.artist
               },
               {priority: 'event'}
             );
           }"
              )
            )
          )
        )
      )
  })

  # Render the audio player when a point is clicked
  output$audio_player_twod <- renderUI({
    req(input$selected_audio)
    tags$audio(
      src = input$selected_audio,
      type = "audio/mp3",
      controls = TRUE,
      autoplay = TRUE
    )
  })
}

shinyApp(ui = ui, server = server)
