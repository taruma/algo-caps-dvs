# APLIKASI SHINY UNTUK DV CAPSTONE DISURUH SAMA ALGOSCAM

require(readr)

library(dplyr)

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)

## PREP ----

eurovision <- readr::read_csv('data/eurovision.csv')

factor_columns <- c(
  "host_city", "event", "host_country", "section", "artist_country"
)

integer_columns <- c(
  "year", "running_order", "total_points", "rank"
)

eurovision <- eurovision |> 
  mutate(across(all_of(factor_columns), as.factor)) |> 
  mutate(across(all_of(integer_columns), as.integer))

## UI ----

header <- dashboardHeader(
  # https://github.com/RinteRface/shinydashboardPlus/issues/6
  title = tagList(
    span(class = "logo-lg", "Eurovision Song Contest"),
    icon("play")
  ),
  titleWidth = 300,
  controlbarIcon = icon("gears", class = "fa")
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

tab1 <- tabItem("tabOverview")
tab2 <- tabItem("tabExplore", h2("Hello"))
tab3 <- tabItem(
  "tabDataset",
  # fluidPage(
  includeMarkdown("md/datasetInfo.md"),
  dataTableOutput("datasetTable")
  # )
)
tab4 <- tabItem("tabNotes")


body <- dashboardBody(
  includeCSS("style.css"),
  tabItems(
    tab1, tab2, tab3, tab4
  )
)


# Define UI for application that draws a histogram
ui <- dashboardPage(
  header,
  sidebar,
  body,
  footer = dashboardFooter(
    left = markdown("`g00gL3 aj<~>4 K|>k`"),
    right = markdown("`... :: <=> #{}`")
  ),
  skin = "blue-light",
  title = "Eurovision Song Contest Dashboard"
)

# Define server logic required to draw a histogram
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
}

# Run the application
shinyApp(ui = ui, server = server)
