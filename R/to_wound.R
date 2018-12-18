#' How many hits wound the target?
#'
#' After a successful hit, the attacked determines how many wounds are dealt.
#'
#' @param n integer scalar number of hits.
#' @param str integer scalar strength of the attack.
#' @param tgh integer scalar toughness of the target.
#' @return integer scalar number of wounding hits.
to_wound <- function(n, str, tgh) {
  rr <- roll_dice(n)

  # 8th uses a simple formula to determine wounds
  if(str == tgh) {                     # str equal to toughness
    success <- sum(rr >= 4)
  } else if(str > tgh & str < (2 * tgh)) { # str greater than toughness, but not twice
    success <- sum(rr >= 3)
  } else if(str >= (2 * tgh)) {        # str twice or greater than toughness
    success <- sum(rr >= 2)
  } else if(str < tgh & (2 * str) > tgh) { # str less than toughness, but not half
    success <- sum(rr >= 5)
  } else if(str <= (2 * tgh)) {        # str half or less of toughness
    success <- sum(rr >= 6)
  }

  success
}
