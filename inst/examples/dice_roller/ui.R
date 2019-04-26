#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(

  titlePanel('40k 8th edition Dice Roller'),

  sidebarLayout(
    sidebarPanel(
      h1('Make an attack!'),

      # make it go
      actionButton('roll', 'Roll!'),

      # always  
      numericInput('n_atk',
                   h3('Total number of attacks.'),
                   value = 1),

      sliderInput('skill',
                  h3('Relevant skill value (WS or BS).'),
                  min = 2, max = 6,
                  value = 6),

      numericInput('atk_str',
                  h3('Attack strength.'),
                  value = 3),
      
      numericInput('def_tgh',
                  h3('Defender toughness.'),
                  value = 3),
      
      numericInput('def_save',
                  h3('Defender save.'),
                  value = 3),
      
      numericInput('atk_ap',
                  h3('Attack AP.'),
                  value = 0),

      # sometimes
      # rerolls are sometimes
      checkboxInput('atk_reroll_state', 
                    h4('Any rerolls on attacks?'), 
                    value = FALSE),
      conditionalPanel(
        condition = 'input.atk_reroll_state == true',
        sliderInput('atk_reroll',
                    'Reroll attack rolls of X and lower.',
                    min = 1, max = 6,
                    value = 1)
      ),
      
      # rerolls are sometimes
      checkboxInput('wnd_reroll_state', 
                    h4('Any rerolls on wounds?'), 
                    value = FALSE),
      conditionalPanel(
        condition = 'input.wnd_reroll_state == true',
        sliderInput('wnd_reroll',
                    'Reroll wound rolls of X and lower.',
                    min = 1, max = 6,
                    value = 1)
      ),
      
      # save details 
      checkboxInput('save_invul', 
                    h4('Is the Defense Save an Invulnerable?'), 
                    value = FALSE),
      
      # explosions are sometimes
      checkboxInput('atk_explode_state', 
                    h5('Do some attack rolls proc more attacks
                       (e.g. Death to the False Emperor)?'), 
                    value = FALSE),
      conditionalPanel(
        condition = 'input.atk_explode_state == true',
        sliderInput('atk_explode',
                    'Bonus attacks on rolls of X and greater.',
                    min = 1, max = 6,
                    value = 6)
      ),
                    
      # explosions are sometimes
      checkboxInput('wnd_explode_state', 
                    h5('Do some wound rolls proc more wounds?'),
                    value = FALSE),
      conditionalPanel(
        condition = 'input.wnd_explode_state == true',
        sliderInput('wnd_explode',
                    'Bonus wounds on rolls of X and greater.',
                    min = 1, max = 6,
                    value = 6)
      ),
      
      # expansions are sometimes
      checkboxInput('atk_expand_state', 
                    h5('Do some attack rolls proc additional hits \n
                       (e.g. Tesla)?'), 
                    value = FALSE),
      conditionalPanel(
        condition = 'input.atk_expand_state == true',
        sliderInput('atk_expand_lvl',
                    'Additional hits on rolls of X and greater.',
                    min = 1, max = 6,
                    value = 6)
      ),
      conditionalPanel(
        condition = 'input.atk_expand_state == true',
        numericInput('atk_expand_rate',
                    'Number of additional hits.',
                    value = 1)
      ),
      
      # expansions are sometimes
      checkboxInput('wnd_expand_state', 
                    h5('Do some wound rolls proc additional wounds?'), 
                    value = FALSE),
      conditionalPanel(
        condition = 'input.wnd_expand_state == true',
        sliderInput('wnd_expand_lvl',
                    'Additional wounds on rolls of X and greater.',
                    min = 1, max = 6,
                    value = 6)
      ),
      conditionalPanel(
        condition = 'input.wnd_expand_state == true',
        numericInput('wnd_expand_rate',
                    'Number of additional wounds',
                    value = 1)
      ),

      # misc attack triggers
      checkboxInput('atk_trigger_state', 
                    h5('Any additional attack triggers?'), 
                    value = FALSE),
      conditionalPanel(
        condition = 'input.atk_expand_state == true',
        sliderInput('atk_trigger',
                    'Additional attack triggers on rolls of X or greater.',
                    min = 1, max = 6,
                    value = 6)
      ),

      # misc wound triggers
      checkboxInput('wnd_trigger_state', 
                    h5('Any additional wound triggers?'), 
                    value = FALSE),
      conditionalPanel(
        condition = 'input.wnd_expand_state == true',
        sliderInput('wnd_trigger',
                    'Additional wound triggers on rolls of X or greater.',
                    min = 1, max = 6,
                    value = 6)
      ),

      # make it go
      actionButton('roll', 'Roll!')


    ),
    
    # tabset comparing these results to 
    # distrubition of 1000 other rolls
    # Results | Comparison
    # probably overboard -- maybe just display facet_wrap under?
    mainPanel(h1('Results'),
              h3(textOutput('results')),
              plotOutput('quality')
    )
  )
)
