## UI ----

## HEADER ====

header <- dashboardHeader(
  # https://github.com/RinteRface/shinydashboardPlus/issues/6
  title = tagList(
    span(class = "logo-lg fs-3", "Eurovision Dashboard"),
    icon("play")
  ),
  controlbarIcon = icon("list", class = "fa")
)

## SIDEBAR ====

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview",
      tabName = "tabOverview",
      icon = icon("layer-group", class = "fa"), selected = TRUE
    ),
    menuItem("Explore",
      tabName = "tabExplore",
      icon = icon("magnifying-glass-chart", class = "fa") # , selected = TRUE
    ),
    menuItem("Dataset",
      tabName = "tabDataset",
      icon = icon("table", class = "fa")
    ),
    menuItem("Notes",
      tabName = "tabNotes",
      icon = icon("clipboard", class = "fa")
    ),
    menuItem("Source Code",
      href = "https://github.com/taruma/algo-caps-dvs",
      icon = icon("github", class = "fa")
    )
  ),
  collapsed = TRUE
)

## TAB 1 ====

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
      tabPanel("overviewTab1", "Konten Tab 1"),
      tabPanel(
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
              width = 3
            ),
            mainPanel(
              plotlyOutput(
                "overviewPlotHeatmap"
              ),
              width = 9
            )
          )
        )
      ),
      selected = "Partisipasi Negara",
      width = 12
      # height = 600
    )
  ),
)

## TAB 2 ====


## TAB 3 ====


## TAB 4 ====

tab4 <- tabItem(
  "tabNotes"
)


body <- dashboardBody(
  includeCSS("style.css"),
  setShadow(class = "small-box"),
  setShadow(class = "box"),
  setShadow(class = "pad"),
  setShadow(class = "nav-tabs-custom"),
  setZoom(class = "description-block"),
  tabItems(
    tab1, tab2, tab3, tab4
  )
)

# FINAL UI ----

ui <- dashboardPage(
  header,
  sidebar,
  body,
  footer = dashboardFooter(
    right = markdown("`-> #algoscam <-`"),
    left = tags$small(
      textOutput("footerRubric"), "points"
    )
  ),
  skin = "blue-light",
  title = "Eurovision Song Contest Dashboard",
  controlbar = myControlBar
)

# SERVER ----

server <- function(input, output, session) {
  observe({
    year_selected <- input$exploreSliderYear

    section_choices <- section_tb |>
      filter(year == year_selected) |>
      pull(section)

    section_selected <- if (year_selected <= 2003) {
      "final"
    } else {
      "grand-final"
    }

    updateSelectInput(
      session, "exploreSelectSection",
      choices = section_choices,
      selected = section_selected
    )
  })

  winner_selected <- reactive({
    eurovision |>
      filter(
        year == input$exploreSliderYear,
        section %in% c("grand-final", "final"),
        winner
      )
  })

  output$exploreWinnerCountry <- renderText(
    winner_selected() |>
      pull(artist_country) |>
      paste(collapse = "<br>")
  )

  output$exploreWinnerArtist <- renderText(
    winner_selected() |>
      pull(artist) |>
      paste(collapse = "<br>")
  )

  output$exploreWinnerSong <- renderText(
    winner_selected() |>
      pull(song) |>
      paste(collapse = "<br>")
  )

  output$explorePlot1 <- renderPlotly(
    ggplotly(
      myplot(
        input$exploreSliderYear,
        input$exploreSelectSection,
        input$exploreSliderTopN,
        input$exploreRadioSort |> as.logical()
      ),
      tooltip = "text"
    )
  )

  output$exploreTitlePlot <- renderText(
    paste(
      "Poin Tiap Negara Tahun",
      input$exploreSliderYear
    )
  )


  output$datasetTable <- renderDataTable(
    eurovision |>
      select(
        -all_of(c("event_url", "artist_url", "image_url", "country_emoji"))
      ),
    options = list(pageLength = 10, scrollX = "100%"),
    filter = "top",
    selection = "single"
  )

  output$footerRubric <- renderText({
    rubric_input <- input$rubricInput |> rubricscore()
    rubric_tab <- input$rubricTab |> rubricscore()
    rubric_render <- input$rubricRender |> rubricscore()
    rubric_deploy <- input$rubricDeploy |> rubricscore()
    rubric_ui <- input$rubricUI |> rubricscore()

    sum(
      rubric_input, rubric_tab, rubric_render, rubric_deploy, rubric_ui
    )
  })
  
  output$overviewPlotHeatmap <- renderPlotly({
    plotheatmap(
      input$overviewSelectCountry,
      input$overviewSliderYears[1],
      input$overviewSliderYears[2]
    )
  })
}

shinyApp(ui = ui, server = server)
