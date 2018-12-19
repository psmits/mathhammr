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
#' @export
#' @return integer scalar number of successful hits.
#' @examples
#' # a single SM tactical attacks.
#' to_attack(n = 1, skill = 3+)
#'
#' # a single SM tactical attacks while within 6" of a Captain.
#' to_attack(n = 1, skill = 3, reroll = 1)
#'
#' # a single CSM attacks with Death to the False Emperor.
#' to_attack(n = 1, skill = 3, explode = 6)
#'
#' # with expanding attacks
#' to_attack(n = 1, skill = 3, expand = list(lvl = 6, rate = 3)
to_attack <- function(n, skill, reroll = NULL, explode = NULL, expand = NULL) {
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
