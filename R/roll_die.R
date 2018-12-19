#' Roll six-sided dice
#'
#' Core function to roll a d6.
#'
#' @param n int scalar number of dice to roll.
#'
#' @return numeric vector of length >= 1.
#' @export
#'
#' @examples
#' roll_dice(1)
roll_dice <- function(n) {
  # defense
  if(!is.numeric(n)) {
    stop('Error: n must be numeric')
  }

  sample(x = 1:6,
         size = n,
         replace = TRUE)
}


#' Exploding dice
#'
#' This function mimics Death to the False Emperor, Dakka Dakka Dakka, etc.
#' Some dice space new rolls on a 6+ (or even 5+).
#' This adds a new roll to the results for each explosion.
#'
#' @param x numeric vector length >= 1 of dice rolls.
#' @param lvl numeric scalar what MINIMUM result explodes; default 6+.
#'
#' @return numeric vector of new rolls.
#' @export
#'
#' @examples
#' x <- roll_dice(6)
#' # death to the false emperor
#' explode_dice(x = x, lvl = 6)
explode_dice <- function(x, lvl = 6) {
  # defense
  if(!is.numeric(x)) {
    stop('Error: n must be numeric')
  }
  if(any(x < 0)) {
    stop('Error: must be positive. x < 0')
  }

  if(!is.numeric(lvl)) {
    stop('Error: lvl must be numeric')
  }
  if(lvl > 6) {
    stop('Error: lvl must be equal to or less than 6')
  }
  if(lvl < 1) {
    stop('Error: lvl must be greater than or equal to 1')
  }

  # how many exploded
  ex <- sum(x >= lvl)

  # roll the new dice
  out <- roll_dice(ex)

  out
}


#' Re-roll effects
#'
#' Some effects cause some dice to be re-rolled e.g. SM Captain.
#' This function replaces a previous roll with a new value.
#'
#' @param x numeric vector length >= 1.
#' @param lvl numeric scalar what MAXIMUM result rerolls; default re-roll 1s.
#'
#' @return numeric vector of length == x.
#' @export
#'
#' @examples
#' x <- roll_dice(6)
#' # captain aura on to-hit
#' rr <- roll_dice(6)
#' reroll_dice(x = rr, lvl = 1)
reroll_dice <- function(x, lvl = 1) {
  # defense
  if(!is.numeric(x)) {
    stop('Error: n must be numeric')
  }
  if(any(x < 0)) {
    stop('Error: must roll a positive number of dice. no values of x < 0')
  }

  if(!is.numeric(lvl)) {
    stop('Error: lvl must be numeric')
  }
  if(lvl > 6) {
    stop('Error: lvl must be equal to or less than 6')
  }
  if(lvl < 1) {
    stop('Error: lvl must be greater than or equal to 1')
  }

  # which get re-rolled
  rr <- x <= lvl

  # replace those values with new rolls
  x[rr] <- roll_dice(sum(rr))

  x
}


#' Expands effects
#'
#' Some effects spawn additional successes on certain rolls.
#' For example, on an attack roll of 6+, this attack scores 3 hits instead of 1.
#' I call this "expands" because it increases the number of successes without generating new rolls (explodes).
#' Expanding attacks have a rate of expansion -- how many new successes proc-d on flagged roll?
#'
#' This function specifically returns the number of additionall successes, not including the initial success.
#' For example, if an attack grants 3 successes on a 6+, this function would return 2 -- the ADDITIONAL successes.
#'
#' @param x numeric vector length >= 1.
#' @param lvl numeric scalar what MINIMUM result expands; default 6+.
#' @param rate numeric scalar number of actual successes on proc (default 1)?
#'
#' @return integer scalar number additional successes (see description).
#' @export
#'
#' @examples
#' x <- roll_dice(6)
#' expand_dice(x = x, lvl = 6, rate = 3)
expand_dice <- function(x, lvl = 6, rate = 1) {
  # defense
  if(!is.numeric(x)) {
    stop('Error: n must be numeric')
  }
  if(any(x < 0)) {
    stop('Error: Must roll a positive number of dice. x cannot have values less than 0')
  }

  if(!is.numeric(lvl)) {
    stop('Error: lvl must be numeric')
  }
  if(lvl > 6) {
    stop('Error: lvl must be equal to or less than 6')
  }
  if(lvl < 1) {
    stop('Error: lvl must be greater than or equal to 1')
  }

  if(!is.numeric(rate)) {
    stop('Error: rate must be greater than or equal to 1')
  }
  if(rate < 1) {
    stop('Error: rate must be greater than or equal to 1')
  }

  # how many proc?
  n <- sum(x >= lvl)

  # expansion of those...
  out <- n * (rate - 1)                # ADDITIONAL successes (if rate is 3, then additional successes are 2)
  out <- max(0, out)                   # no negative successes!
  # this is what rate 1 produces no ADDITIONAL successes -- no expansion

  out
}

#' Trigger dice
#'
#' This function identifies how many triggers are proc-d by a roll.
#' A trigger is a specific value (or values) which are associated with special events.
#' For example, an overcharged plasma has a trigger on an attack roll of 1.
#' An overcharged plasma with a -1 to hit would trigger on attack rolls of 1 AND 2.
#'
#' @param x numeric vector length >= 1 of dice rolls.
#' @param trigger numeric vector of length >= 1. What EXACT values are special?
#'
#' @return numeric scalar number of triggers proc-d.
#' @export
#'
#' @examples
#' # trigger on a 1 (e.g. overcharged plasma)
#' rr <- roll_dice(1)
#' trigger_dice(x = rr, trigger = 1)
trigger_dice <- function(x, trigger) {
  # defense
  if(!is.numeric(x)) {
    stop('Error: n must be numeric')
  }
  if(any(x < 0)) {
    stop('Error: Must roll a positive number of dice. x cannot have values less than 0')
  }

  if(!is.numeric(trigger)) {
    stop('Error: trigger much be numeric.')
  }
  if(any(trigger < 1)) {
    stop('Error: Triggers must be >= 1.')
  }
  if(any(trigger > 6)) {
    stop('Error: Triggers must be <= 6.')
  }

  # sum triggers
  out <- sum(x %in% trigger)

  out
}
