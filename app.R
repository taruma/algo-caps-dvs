#algoscam

require(markdown)

library(DT)

## UI ----

header <- dashboardHeader(
  # https://github.com/RinteRface/shinydashboardPlus/issues/6
  title = tagList(
    span(class = "logo-lg fs-3", "Eurovision Dashboard"),
    icon("play")
  ),
  controlbarIcon = icon("list", class = "fa"),
  fixed = TRUE
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview",
      tabName = "tabOverview",
      icon = icon("layer-group", class = "fa"), selected = TRUE
    ),
    menuItem("Explore",
      tabName = "tabExplore",
      icon = icon("magnifying-glass-chart", class = "fa")
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

tab1 <- tabItem(
  "tabOverview")
tab2 <- tabItem("tabExplore", h2("Hello"))
tab3 <- tabItem(
  "tabDataset",
  includeMarkdown("md/datasetInfo.md"),
  dataTableOutput("datasetTable")
)
tab4 <- tabItem(
  "tabNotes"
)


body <- dashboardBody(
  includeCSS("style.css"),
  tabItems(
    tab1, tab2, tab3, tab4
  )
)

ui <- dashboardPage(
  header,
  sidebar,
  body,
  footer = dashboardFooter(
    left = markdown("`g00gL3 aj<~>4 K|>k`"),
    right = tags$small(
      textOutput("footerRubric"), "points"
    )
  ),
  skin = "blue-light",
  title = "Eurovision Song Contest Dashboard",
  controlbar = myControlBar
)

# SERVER ----

server <- function(input, output) {
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
    rubric_input <- input$rubricInput |> as.integer() |> sum()
    rubric_tab <- input$rubricTab |> as.integer() |> sum()
    rubric_render <- input$rubricRender |> as.integer() |> sum()
    rubric_deploy <- input$rubricDeploy |> as.integer() |> sum()
    rubric_ui <- input$rubricUI |> as.integer() |> sum()

    sum(
      rubric_input, rubric_tab, rubric_render, rubric_deploy, rubric_ui
    )
  })
}

shinyApp(ui = ui, server = server)
