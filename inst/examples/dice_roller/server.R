#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(purrr)
library(glue)
library(forcats)
library(mathhammr)
source('helper.R')

# Define server logic required to draw a histogram
server <- function(input, output) {

  ###
  # ATTACKS
  roll_atk <- eventReactive(input$roll, {

    # attack! and calculate number of hits
    aa <- atk_logic(input)

    # results of the attack roll
    atk <- aa(n = input$n_atk)

    atk

  })

  ###
  # distribution of ATTACKS
  atk_dist <- eventReactive(input$roll, {

    # attack! and calculate number of hits
    aa <- atk_logic(input)

    # results of the attack roll
    atk <- rerun(1000, aa(n = input$n_atk))

    atk

  })

  ###
  # WOUNDS
  roll_wnd <- eventReactive(input$roll, {

    # how many of those attacks wound
    ww <- wnd_logic(input)

    # results of the wound roll 
    #   depends on attack triggers (NOT WOUND)
    atk_res <- roll_atk()              # n attacks
    if(input$atk_trigger_state) {
      wnd <- ww(n = atk_res$success)
    } else {
      wnd <- ww(n = atk_res)
    }

    wnd

  })

  ###
  # distribution of WOUNDS
  wnd_dist <- eventReactive(input$roll, {

    # how many of those attacks wound
    ww <- wnd_logic(input)

    # results of the wound roll 
    #   depends on attack triggers (NOT WOUND)
    atk_res <- roll_atk()              # n attacks
    if(input$atk_trigger_state) {
      wnd <- rerun(1000, ww(n = atk_res$success))
    } else {
      wnd <- rerun(1000, ww(n = atk_res))
    }

    wnd

  })


  ###
  # (failed) SAVES
  roll_sv <- eventReactive(input$roll, {
    
    nn <- roll_wnd()
    ss <- partial(to_save,
                  sv = input$def_save,
                  ap = input$atk_ap,
                  invul = input$save_invul)

    if(input$wnd_trigger_state) {
      out <- ss(n = nn$success)
    } else {
      out <- ss(n = nn)
    }

    out

  })
  
  ###
  # distribution of (failed) SAVEs
  sv_dist <- eventReactive(input$roll, {
    
    nn <- roll_wnd()
    ss <- partial(to_save,
                  sv = input$def_save,
                  ap = input$atk_ap,
                  invul = input$save_invul)

    if(input$wnd_trigger_state) {
      out <- rerun(1000, ss(n = nn$success))
    } else {
      out <- rerun(1000, ss(n = nn))
    }

    out

  })


  # textual representation of the results of the roll
  output$results <- renderText({

    atk_res <- roll_atk()
    wnd_res <- roll_wnd()
    sv_res <- roll_sv()

    # print out the results
    if(input$atk_trigger_state) {
      glue('Number of successful hits: {atk_res$success}.
           Number of additional attack triggers: {atk_restrigger}.
           Number of successful wounds: {wnd_res$success}.
           Number of unsaved wounds: {sv_res}.')
    } else if(input$wnd_trigger_state) {
      glue('Number of successful hits: {atk_res$success}.
           Number of successful wounds: {wnd_res$success}.
           Number of additional wound triggers: {wnd_res$trigger}.
           Number of unsaved wounds: {sv_res}.')
    } else if(input$atk_trigger_state & input$wnd_trigger_state) {
      glue('Number of successful hits: {atk_res$success}.
           Number of additional attack triggers: {atk_res$trigger}.
           Number of successful wounds: {wnd_res$success}.
           Number of additional wound triggers: {wnd_res$trigger}.
           Number of unsaved wounds: {sv_res}.')
    } else {
      glue('Number of successful hits: {atk_res}.
           Number of successful wounds: {wnd_res}.
           Number of unsaved wounds: {sv_res}.')
    }

  })

  
  # now build the quality plot
  # what you rolled
  res_roll <- eventReactive(input$roll, {
    tibble(atk = roll_atk(),
           wnd = roll_wnd(),
           sv = roll_sv()) %>%
    gather(key = key, value = value) %>%
    mutate(key = case_when(key == 'atk' ~ '# of Hits',
                           key == 'wnd' ~ '# of Wounds',
                           key == 'sv' ~ '# of failed Saves'),
           key = fct_inorder(key))
  })

  # 1000 other rolls
  res_dist <- eventReactive(input$roll, {
    tibble(atk = atk_dist() %>% unlist,
           wnd = wnd_dist() %>% unlist,
           sv = sv_dist() %>% unlist) %>%
    gather(key = key, value = value) %>%
    mutate(key = case_when(key == 'atk' ~ '# of Hits',
                           key == 'wnd' ~ '# of Wounds',
                           key == 'sv' ~ '# of failed Saves'),
           key = fct_inorder(key))
  })

  # spit out the comparison plot
  output$quality <- renderPlot({

    ggplot() +
      geom_bar(data = res_dist(),
               mapping = aes(x = value,
                             y = (..count..)/sum(..count..)),
               colour = 'darkgrey') +
      geom_vline(data = res_roll(),
                 mapping = aes(xintercept = value),
                 colour = 'blue',
                 size = 2) +
      facet_wrap(~ key) +
      scale_x_continuous(breaks = pretty_breaks()) +
      #theme_minimal() +
      labs(x = 'Result', y = 'Frequency')

  })

}
