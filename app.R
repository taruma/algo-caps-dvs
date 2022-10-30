# algoscam

require(markdown)

library(DT)
library(shinyEffects)
library(ggplot2)
library(plotly)

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
      icon = icon("layer-group", class = "fa") # , selected = TRUE
    ),
    menuItem("Explore",
      tabName = "tabExplore",
      icon = icon("magnifying-glass-chart", class = "fa"), selected = TRUE
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
  "tabOverview"
)

## TAB 2 ====

tab2 <- tabItem(
  "tabExplore",
  
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
            "Penampilan / No. Urut" = FALSE,
            "Poin / Skor" = TRUE
          )
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
      title = "Negara Pemenang"
    )
  ),
  ## ROW 4 ====
  # PLOT OUTPUT
  fluidRow(
    box(
      plotlyOutput("explorePlot1"),
      width = 12
    )
  )
)

## TAB 3 ====

tab3 <- tabItem(
  "tabDataset",
  includeMarkdown("md/datasetInfo.md"),
  dataTableOutput("datasetTable")
)

## TAB 4 ====

tab4 <- tabItem(
  "tabNotes"
)


body <- dashboardBody(
  includeCSS("style.css"),
  setShadow(class = "small-box"),
  setShadow(class = "box"),
  setShadow(class = "pad"),
  setZoom(class = "description-block"),
  tabItems(
    tab1, tab2, tab3, tab4
  )
)

ui <- dashboardPage(
  header,
  sidebar,
  body,
  footer = dashboardFooter(
    right = markdown("`g00gL3 aj<~>4 K|>k`"),
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
        input$exploreSliderTopN
      )
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
}

shinyApp(ui = ui, server = server)
