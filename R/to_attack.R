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
#'
#' @return integer scalar number of successful hits.
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
to_attack <- function(n, skill, reroll = NULL, explode = NULL, expand = NULL) {
  # defense
  if(!is.numeric(n)) {
    stop('Error: n must be numeric.')
  }
  if(n < 1) {
    stop('Error: must roll at least 1 die. n < 1.')
  }

  if(!is.numeric(skill)) {
    stop('Error: skill must be numeric.')
  }
  if(skill < 1) {
    stop('Error: must have min skill 1. skill < 1.')
  }
  if(skill > 6) {
    stop('Error: must have max skill 6. skill > 6.')
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
           just leave NULL.')
    }
    if(explode > 6) {
      stop('Error: explode above 6 are invalid.
           * If exploding everything, use 6.
           * If exploding nothing, leave NULL.')
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
      stop('Error: expand expects a named list.')
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

  # final count
  success
}
