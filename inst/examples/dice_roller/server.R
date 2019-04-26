#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(purrr)
library(glue)
library(mathhammr)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$results <- renderText({

    # attack! and calculate number of hits
    aa <- partial(to_attack,
                  skill = input$skill)

    if(input$atk_reroll_state)
      aa <- partial(aa, reroll = input$atk_reroll)

    if(input$atk_explode_state) 
      aa <- partial(aa, explode = input$atk_explode)

    if(input$atk_expand_state)
      aa <- partial(aa, 
                    expand = list(lvl = input$atk_expand_lvl,
                                  rate = input$atk_expand_rate))

    if(input$atk_trigger_state)
      aa <- partial(aa,
                    trigger = input$atk_trigger)

    # results of the attack roll
    atk <- aa(n = input$n_atk)


    # how many of those attacks wound
    ww <- partial(to_wound, 
                  str = input$atk_str,
                  tgh = input$def_tgh)

    if(input$wnd_reroll_state)
      ww <- partial(ww, reroll = input$wnd_reroll)

    if(input$wnd_explode_state) 
      ww <- partial(ww, explode = input$wnd_explode)

    if(input$wnd_expand_state)
      ww <- partial(ww, 
                    expand = list(lvl = input$wnd_expand_lvl,
                                  rate = input$wnd_expand_rate))
    
    if(input$wnd_trigger_state)
      wnd <- partial(ww,
                     trigger = input$wnd_trigger)


    # results of the wound roll 
    #   depends on attack triggers (NOT WOUND)
    if(input$atk_trigger_state) {
      wnd <- ww(n = atk$success)
    } else {
      wnd <- ww(n = atk)
    }


    # print out the results
    if(input$atk_trigger_state) {
      glue('Number of successful hits: {atk$success}.
           Number of additional attack triggers: {atk$trigger}.
           Number of successful wounds: {wnd$success}.')
    } else if(input$wnd_trigger_state) {
      glue('Number of successful hits: {atk$success}.
           Number of successful wounds: {wnd$success}.
           Number of additional wound triggers: {wnd$trigger}')
    } else if(input$atk_trigger_state & input$wnd_trigger_state) {
      glue('Number of successful hits: {atk$success}.
           Number of additional attack triggers: {atk$trigger}.
           Number of successful wounds: {wnd$success}.
           Number of additional wound triggers: {wnd$trigger}')
    } else {
      glue('Number of successful hits: {atk}.
           Number of successful wounds: {wnd}.')
    }
    
  })
}
