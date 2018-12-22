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
      conditionalPanel(
        condition = "input.reroll_atk == true",
        numericInput("reroll_atk_lvl",
                     h4("Re-roll attacks at equal to or less than?"),
                     value = 0)),

      checkboxInput("reroll_wnd",
                   h3("Re-roll Wounds?"),
                   value = FALSE),
      conditionalPanel(
        condition = "input.reroll_wnd == true",
        numericInput("reroll_wnd_lvl",
                     h4("Re-roll wounds at equal to or less than?"),
                     value = 0)),

      h2("Defender's Stats"),
      numericInput("tgh",
                   h3("Toughness"),
                   value = 1),
      numericInput("save",
                   h3("Save"),
                   value = 4),
      checkboxInput("invul",
                    "Invulnerability?",
                    value = FALSE),
      
      checkboxInput("reroll_save",
                   h3("Re-roll Saves?"),
                   value = FALSE),
      conditionalPanel(
        condition = "input.reroll_save == true",
        numericInput("reroll_save_lvl",
                     h4("Re-roll saves at equal to or less than?"),
                     value = 0)),

      checkboxInput("fnp",
                   h3("Feel No Pain?"),
                   value = FALSE),
      conditionalPanel(
        condition = "input.fnp == true",
        numericInput("fnp_lvl",
                     h4("Feel No Pain at equal to or less than?"),
                     value = 0))
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
    rr_atk <- ifelse(input$reroll_atk, 
                     paste0(" Re-rolling attacks of ", input$reroll_atk_lvl, " or less."),
                     '')
    
    rr_wnd <- ifelse(input$reroll_wnd, 
                     paste0(" Re-rolling wounds of ", input$reroll_wnd_lvl, " or less."),
                     '')

    paste0("Attacker has ", input$atk, " Attacks, ", input$skill, 
           "+ Skill, Strength ", input$str , " AP ",input$ap, ".",
           rr_atk, rr_wnd)
  })
  
  output$defender_var <- renderText({
    invl <- ifelse(input$invul, 'Invulnerability', '')
    
    rr_save <- ifelse(input$reroll_save, 
                     paste0(" Re-rolling saves of ", input$reroll_save_lvl, " or less."),
                     '')
    
    rr_save <- ifelse(input$fnp,
                     paste0(" Feel No Pain on a ", input$reroll_save_lvl, "+."),
                     '')

    paste0("Defender has ", input$tgh, " Toughness, and a ", input$save, "+", 
           invl, " Save.", rr_save)
  })

}

shinyApp(ui = ui, server = server)
