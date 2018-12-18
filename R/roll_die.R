#' Roll six-sided dice
#'
#' Core function to roll a d6.
#'
#' @param n int scalar number of dice to roll.
#' @return numeric vector of length >= 1.
#' @examples
#' roll_die(1)
roll_die <- function(n) {
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
#' @param x numeric vector length >= 1.
#' @param lvl numeric scalar what MINIMUM result explodes; default 6+
#' @return numeric vector of length >= x.
#' @examples
#' x <- roll_die(6)
#' # death to the false emperor
#' explode_die(x = x, lvl = 6)
explode_die <- function(x, lvl = 6) {
  # how many exploded
  ex <- sum(x >= lvl)

  # add the new rolls on the end
  out <- c(x, roll_die(ex))

  out
}


#' Re-roll effects
#'
#' Some effects cause some dice to be re-rolled e.g. SM Captain.
#' This function replaces a previous roll with a new value.
#'
#' @param x numeric vector length >= 1.
#' @param lvl numeric scalar what MAXIMUM result rerolls; default re-roll 1s.
#' @return numeric vector of length == x.
#' @examples
#' x <- roll_die(6)
#' # captain aura on to-hit
#' reroll_die(x = x, lvl = 1)
reroll_die <- function(x, lvl = 1) {
  # which get re-rolled
  rr <- which(x <= lvl)

  # replace those values with new rolls
  x[rr] <- roll_die(rr)

  x
}
