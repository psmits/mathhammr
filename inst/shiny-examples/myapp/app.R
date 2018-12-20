library(shiny)

ui <- fluidPage(
  titlePanel("Mathhammr dice roller"),

  sidebarLayout(
    sidebarPanel(
      helpText("Note: help text isn't a true widget,"),
      numericInput("num",
                   h3("numeric input"),
                   value = 1)
    ),
    mainPanel(
    )
  )
)

server <- function(input, output) {

}

shinyApp(ui = ui, server = server)
