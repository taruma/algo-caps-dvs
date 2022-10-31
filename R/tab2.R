tab2 <- tabItem(
  "tabExplore",
  titlePanel(
    markdown("**Eksplorasi Dataset**")
  ),

  ## ROW 1 ====
  fluidRow(
    valueBox(
      total_running_year, "Tahun Kontes Telah Berlangsung",
      icon = icon("calendar-check"), color = "green"
    ),
    valueBox(
      total_participant, "Negara Yang Telah Berpartisipasi",
      icon = icon("flag"), color = "light-blue"
    ),
    valueBox(
      total_winner_country,
      "Negara Telah Memenangkan Kontes",
      icon = icon("earth-europe"), color = "yellow"
    ),
  ),
  ## ROW 2 ====
  fluidRow(
    valueBox(
      paste(
        eurovision_grandfinal$year |> min(),
        " - ",
        eurovision_grandfinal$year |> max()
      ),
      "Periode Kontes Berlangsung",
      icon = icon("calendar-days", class = "fa-solid"), color = "maroon"
    ),
    valueBox(
      year_missing, "Tahun Kontes Ditunda",
      icon = icon("calendar-minus", class = "fa-solid"),
      color = "red"
    ),
    valueBox(
      year_multiple_winner, "Tahun Pemenang Ganda",
      icon = icon("check-double"), color = "navy"
    ),
  ),
  ## ROW 3 ====
  # PLOT OPTIONS
  fluidRow(
    box(
      title = "Pengaturan",
      column(
        3,
        sliderInput(
          "exploreSliderYear",
          "Tahun Kontes",
          min = min(eurovision$year),
          max = max(eurovision$year),
          value = 2022,
          round = TRUE,
          step = 1,
          ticks = FALSE,
          sep = "",
          # animate = animationOptions(interval = 300)
        )
      ),
      column(
        3,
        selectInput(
          "exploreSelectSection",
          "Babak",
          choices = c("grand-final")
        )
      ),
      column(
        3,
        sliderInput(
          "exploreSliderTopN",
          "Jumlah Kontestan",
          min = 1,
          max = 30,
          value = 7,
          round = TRUE,
          step = 1,
          ticks = FALSE,
          sep = ""
        )
      ),
      column(
        3,
        radioButtons(
          "exploreRadioSort",
          "Urutkan berdasarkan",
          choices = c(
            "Penampilan / No. Urut" = TRUE,
            "Poin / Skor" = FALSE
          ),
          selected = TRUE
        )
      ),
      width = 8
    ),
    box(
      boxPad(
        color = "olive",
        descriptionBlock(
          header = htmlOutput("exploreWinnerCountry"),
          text = "Negara Pemenang",
          rightBorder = FALSE
        ),
        hr(),
        descriptionBlock(
          header = htmlOutput("exploreWinnerArtist"),
          text = "Penyanyi / Musisi",
          rightBorder = FALSE
        ),
        hr(),
        descriptionBlock(
          header = htmlOutput("exploreWinnerSong"),
          text = "Judul Lagu / Musik",
          rightBorder = FALSE
        )
      ),
      width = 4,
      collapsible = TRUE,
      collapsed = TRUE,
      icon = icon("trophy", class = "fa"),
      title = "Informasi Pemenang"
    )
  ),
  ## ROW 4 ====
  # PLOT OUTPUT
  fluidRow(
    box(
      plotlyOutput("explorePlot1"),
      width = 12,
      title = textOutput("exploreTitlePlot")
    )
  )
)
