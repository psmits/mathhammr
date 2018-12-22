library(shiny)

ui <- fluidPage(
  titlePanel("Mathhammr dice roller"),

  sidebarLayout(
    sidebarPanel(
      h2("Attacker's Stats"),
      numericInput("atk",
                   h3("Number of Attacks"),
                   value = 1),
      numericInput("skill",
                   h3("Relevant (WS or BS) Skill"),
                   value = 4),
      numericInput("str",
                   h3("Strength"),
                   value = 3),
      numericInput("ap",
                   h3("AP"),
                   value = 0),
      checkboxInput("reroll_atk",
                   h3("Re-roll Attack?"),
                   value = FALSE),
      numericInput("reroll_atk_lvl",
                   h4("Re-roll attacks at equal to or less than?"),
                   value = 0),
      checkboxInput("reroll_wnd",
                   h3("Re-roll Wounds?"),
                   value = FALSE),
      numericInput("reroll_wnd_lvl",
                   h4("Re-roll wounds at equal to or less than?"),
                   value = 0),

      h2("Defender's Stats"),
      numericInput("tgh",
                   h3("Toughness"),
                   value = 1),
      numericInput("save",
                   h3("Save"),
                   value = 4),
      checkboxInput("invul",
                    "Invulnerability?",
                    value = FALSE)
    ),

    mainPanel(
      h2(textOutput("attacker_var")),
      br(),
      h2(textOutput("defender_var"))
    )
  )
)

server <- function(input, output) {

  output$attacker_var <- renderText({
    paste0("Attacker has ", input$atk, " Attacks, ", input$skill, 
           "+ Skill, Strength ", input$str , " AP ",input$ap, ".")
  })
  
  output$defender_var <- renderText({
    test <- ifelse(input$invul, 'Invulnerability', '')
    paste0("Defender has ", input$tgh, " Toughness, and a ", input$save, "+", test, " Save.")
  })

}

shinyApp(ui = ui, server = server)
