
tabpanel1 <- tabPanel(
  "Distribusi Poin",
  icon = icon("chart-simple"),
  fluidPage(
    br(),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "overviewDistCountry1",
          "Negara ke-1",
          choices = list_country,
          selected = "United Kingdom",
          multiple = FALSE
        ),
        selectInput(
          "overviewDistCountry2",
          "Negara ke-2",
          choices = list_country,
          selected = "France",
          multiple = FALSE
        ),
        selectInput(
          "overviewDistCountry3",
          "Negara ke-3",
          choices = list_country,
          selected = "Ukraine",
          multiple = FALSE
        ),
        selectInput(
          "overviewDistCountry4",
          "Negara ke-4",
          choices = list_country,
          selected = "Sweden",
          multiple = FALSE
        )
      ),
      mainPanel(
        plotlyOutput("overviewPlotDist")
      )
    )
  )
)


tabpanel2 <- tabPanel(
  "Partisipasi Negara",
  icon = icon("bars-staggered"),
  fluidPage(
    br(),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "overviewSelectCountry",
          "Negara yang ditampilkan",
          choices = list_country,
          selected = list_country_2022,
          multiple = TRUE
        ),
        sliderInput(
          "overviewSliderYears",
          "Periode",
          min = min(eurovision$year),
          max = max(eurovision$year),
          value = c(2013, 2022),
          step = 1,
          round = TRUE,
          ticks = FALSE,
          sep = ""
        ),
        width = 5
      ),
      mainPanel(
        plotlyOutput(
          "overviewPlotHeatmap"
        ),
        width = 7
      )
    )
  )
)


tabpanel3 <- tabPanel(
  "Perjalanan Kontes",
  icon = icon("chart-line"),
  fluidPage(
    br(),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "overviewJourneyCountry",
          "Negara yang ditampilkan",
          choices = list_country,
          selected = c(
            "United Kingdom",
            "Ukraine",
            "Sweden"
          ),
          multiple = TRUE
        ),
        sliderInput(
          "overviewJourneyYears",
          "Periode",
          min = min(eurovision$year),
          max = max(eurovision$year),
          value = c(1990, 2022),
          step = 1,
          round = TRUE,
          ticks = FALSE,
          sep = ""
        ),
        width = 4
      ),
      mainPanel(
        plotOutput(
          "overviewPlotJourney"
        ),
        width = 8
      )
    )
  )
)

tab1 <- tabItem(
  "tabOverview",
  titlePanel(
    title = markdown(
      glue("**_Eurovision Song Contest_** ({year_selected})"),
    )
  ),
  fluidRow(
    valueBox(
      winner_info |> pull(artist_country), "Negara Pemenang",
      icon = icon("trophy"), color = "green", width = 4
    ),
    valueBox(
      winner_info |> pull(artist), "Penyanyi / Musisi",
      icon = icon("microphone"), color = "aqua", width = 8
    )
  ),
  fluidRow(
    valueBox(
      winner_info |> pull(song), "Judul Lagu / Musik",
      icon = icon("music"), color = "blue", width = 6
    ),
    valueBox(
      winner_info |> pull(total_points), "Total Poin / Skor",
      icon = icon("star-half-stroke"), color = "teal", width = 6
    )
  ),
  hr(),
  fluidRow(
    tabBox(
      title = markdown("**Visualisasi Kontes**"),
      id = "overviewTabBox",
      tabpanel1,
      tabpanel2,
      tabpanel3,
      # selected = "Perjalanan Kontes",
      width = 12
    )
  ),
)
