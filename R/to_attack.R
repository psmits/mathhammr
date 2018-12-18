#' How many successful hits from an attack?
#'
#' When a unit attacks another unit, dice are rolled against the ballistic or weapon skill of the attacker.
#' Successful attacks have the opportunity to wound the defender.
#'
#' @param n integer scalar number of attacks
#' @param skill integer scalar minimum successful roll for relevant attack skill (e.g. BS 3+ would be skill = 3).
#' @return integer scalar number of successful hits.
to_attack <- function(n, skill) {
  rr <- roll_dice(n)

  success <- sum(rr >= skill)
  success
}
