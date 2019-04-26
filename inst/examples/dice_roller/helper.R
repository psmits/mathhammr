# given input 
# build up the to-attack roll function though a series partial functions
atk_logic <- function(input) {
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

  aa
}

# given input 
# build up the to-wound roll function though a series partial functions
wnd_logic <- function(input) {
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
    ww <- partial(ww,
                  trigger = input$wnd_trigger)

  ww
}
