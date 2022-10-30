library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

myControlBar <- dashboardControlbar(
  controlbarMenu(
    controlbarItem(
      h1("Rubrik"),
      checkboxGroupInput(
        "rubricInput",
        "Input (reactivity)",
        c(
          "Using min. 2 different input type" = 2,
          "Choosing appropriate input type" = 2,
          "Demonstrating useful input(s)" = 2
        )
      ),
      checkboxGroupInput(
        "rubricTab",
        "Tab",
        c(
          "Using min. 3 page" = 3
        )
      ),
      checkboxGroupInput(
        "rubricRender",
        "Render plot",
        list(
          "Using interactive plot" = 1,
          "Using min. 2 plot type" = 2,
          "Choosing the appropriate plot type" = 2,
          "Demonstrating reactivity from the input" = 2,
          "Creating plots that tell a clear story" = 2
        )
      ),
      checkboxGroupInput(
        "rubricDeploy",
        "Deploy",
        c(
          "Successfully deploying to shinyapps.io" = 6
        ),
        selected = 6
      ),
      checkboxGroupInput(
        "rubricUI",
        "User Interface Appearance",
        c(
          "Have tidy page layout" = 2,
          "Have tidy plot layout" = 2,
          "Have appropriate plot tooltip" = 1,
          "Choosing right color scheme" = 1
        )
      )
    )
  ),
  skin = "light",
  overlay = TRUE,
  width = 300
)
