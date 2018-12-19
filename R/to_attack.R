#' How many successful hits from an attack?
#'
#' When a unit attacks another unit, dice are rolled against the ballistic or weapon skill of the attacker.
#' Successful attacks have the opportunity to wound the defender.
#'
#' @param n integer scalar number of attacks
#' @param skill integer scalar minimum successful roll for relevant attack skill (e.g. BS 3+ would be skill = 3).
#' @param reroll integer scalar what MAXIMUM result rerolls (default NULL).
#' @param explode integer scalar what MINIMUM result explodes (default NULL).
#' @param expand named list of arguments for \code{\link{expand_dice}} (default NULL).
#' @param trigger integer vector length >= 1 which values trigger special effects (default NULL).
#'
#' @return integer scalar number of successful hits UNLESS !is.null(trigger) then named list with number of successes (success) and triggers (trigger).
#' @export
#'
#' @examples
#' # a single SM tactical attacks.
#' to_attack(n = 1, skill = 3)
#'
#' # a single SM tactical attacks while within 6" of a Captain.
#' to_attack(n = 1, skill = 3, reroll = 1)
#'
#' # a single CSM attacks with Death to the False Emperor.
#' to_attack(n = 1, skill = 3, explode = 6)
#'
#' # with expanding attacks
#' to_attack(n = 1, skill = 3, expand = list(lvl = 6, rate = 3))
#'
#' # a single SM tactical attacks with an overcharged plasma
#' to_attack(n = 1, skill = 3, trigger = 1)
to_attack <- function(n,
                      skill,
                      reroll = NULL,
                      explode = NULL,
                      expand = NULL,
                      trigger = NULL) {
  # defense
  if(!is.numeric(n)) {
    stop('Error: n must be numeric.')
  }
  if(n < 0) {
    stop('Error: Must roll a positive number of dice. n must be > 0.')
  }

  if(!is.numeric(skill)) {
    stop('Error: skill must be numeric.')
  }
  if(skill < 1) {
    stop('Error: must have min skill 1. skill must be >= 1.')
  }
  if(skill > 6) {
    stop('Error: must have max skill 6. skill must be <= 6.')
  }

  if(!is.null(reroll)) {
    if(!is.numeric(reroll)) {
      stop('Error: reroll must be numeric.')
    }
    if(reroll < 1) {
      stop('Error: reroll below 1 are invalid. just leave NULL.')
    }
    if(reroll > 6) {
      stop('Error: reroll above 6 are invalid.
           * If rerolling everything, use 6.
           * If rerolling nothing, leave NULL.')
    }
  }

  if(!is.null(explode)) {
    if(!is.numeric(explode)) {
      stop('Error: explode must be numeric.')
    }
    if(explode < 1) {
      stop('Error: explode below 1 are invalid.
           * If no exploding dice, leave NULL.
           * If all dice explode, use 6.')
    }
    if(explode > 6) {
      stop('Error: explode above 6 are invalid.
           * If all dice explode, use 6.
           * If no exploding dice, leave NULL.')
    }
  }

  if(!is.null(expand)) {
    if(!is.list(expand)) {
      stop('Error: expand expects a list.')
    }
    if(is.null(expand)) {
      stop('Error: expand expects a named list.')
    }
    if(!all(names(expand) %in% c('lvl', 'rate'))) {
      stop('Error: expand expects two named elements: lvl and rate.')
    }
    if(length(expand) != 2) {
      stop('Error: expand expects list of length 2.')
    }

    if(!is.numeric(expand$lvl)) {
      stop('Error: expand lvl must be numeric.')
    }
    if(expand$lvl > 6) {
      stop('Error: expand lvl must be equal to or less than 6.')
    }
    if(expand$lvl < 1) {
      stop('Error: expand lvl must be greater than or equal to 1.')
    }

    if(!is.numeric(expand$rate)) {
      stop('Error: rate must be greater than or equal to 1.')
    }
    if(expand$rate < 1) {
      stop('Error: rate must be greater than or equal to 1.')
    }
  }

  if(!is.null(trigger)) {
    if(!is.numeric(trigger)) {
      stop('Error: trigger must be numeric.')
    }
    if(any(trigger < 1)) {
      stop('Error: trigger must be >= 1.')
    }
    if(any(trigger > 6)) {
      stop('Error: trigger must be <= 6.')
    }
  }

  # everything starts with a dice roll
  rr <- roll_dice(n)

  # dice can only be re-rolled once, so order of operations matter
  if(!is.null(reroll)) {
    rr <- reroll_dice(x = rr, lvl = reroll)
  }

  # exploding results
  if(!is.null(explode)) {
    nr <- explode_dice(x = rr, lvl = explode)

    if(!is.null(reroll)) {
      nr <- reroll_dice(x = nr, lvl = reroll)
      # new attacks don't proc more attacks -- cannot explode again
    }

    # include the new dice
    rr <- c(rr, nr)
  }


  # given all those rolls, how many successes?
  success <- sum(rr >= skill)

  # expanding results
  if(!is.null(expand)) {
    expand$x <- rr
    more_success <- do.call(expand_dice, expand)
    success <- success + more_success
  }
  
  # given all those results, how many triggers?
  if(!is.null(trigger)) {
    nt <- trigger_dice(x = rr, trigger = trigger)
    out <- list()
    out$success <- success
    out$trigger <- nt
  } else if(is.null(trigger)) {
    out <- success
  }

  # final count
  out
}
