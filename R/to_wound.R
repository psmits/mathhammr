#' How many hits wound the target?
#'
#' After a successful hit, the attacked determines how many wounds are dealt.
#'
#' @param n integer scalar number of hits.
#' @param str integer scalar strength of the attack.
#' @param tgh integer scalar toughness of the target.
#' @param reroll integer scalar what MAXIMUM result rerolls (default NULL).
#' @param explode integer scalar what MINIMUM result explodes (default NULL).
#' @param expand named list of arguments for \code{\link{expand_dice}} (default NULL).
#' @export
#' @return integer scalar number of wounding hits.
#' @examples
#' # hit a SM tactical with a boltgun -- how many wounds?
#' to_wound(n = 1, str = 4, tgh = 4)
#'
#' # hit a SM tactical with a boltgun within 6" of a SM Lt -- how many wounds?
#' to_wound(n = 1, str = 4, tgh = 4, reroll = 1)
to_wound <- function(n, str, tgh, reroll = NULL, explode = NULL, expand = NULL) {
  # defense
  if(!is.numeric(n)) {
    stop('n must be numeric.')
  }
  if(n < 1) {
    stop('must roll at least 1 die. n < 1.')
  }

  if(!is.numeric(str)) {
    stop('str must be numeric.')
  }
  if(str < 1) {
    stop('must have min str 1. str < 1.')
  }

  if(!is.numeric(tgh)) {
    stop('tgh must be numeric.')
  }
  if(tgh < 1) {
    stop('must have min tgh 1. tgh < 1.')
  }

  if(!is.null(reroll)) {
    if(!is.numeric(reroll)) {
      stop('reroll must be numeric.')
    }
    if(reroll < 1) {
      stop('reroll below 1 are invalid. just leave NULL.')
    }
    if(reroll > 6) {
      stop('reroll above 6 are invalid.
           if rerolling everything, use 6.
           if rerolling nothing, leave NULL.')
    }
  }

  if(!is.null(explode)) {
    if(!is.numeric(explode)) {
      stop('explode must be numeric.')
    }
    if(explode < 1) {
      stop('explode below 1 are invalid.
           just leave NULL.')
    }
    if(explode > 6) {
      stop('explode above 6 are invalid.
           if exploding everything, use 6.
           if exploding nothing, leave NULL.')
    }
  }

  if(!is.null(expand)) {
    if(!is.list(expand)) {
      stop('expand expects a list.')
    }
    if(is.null(expand)) {
      stop('expand expects a named list.')
    }

    if(names(expand) %in% c('lvl', 'rate')) {
      stop('expand expects two named elements: lvl and rate.')
    }

    if(!is.numeric(expand$lvl)) {
      stop('expand lvl must be numeric.')
    }
    if(expand$lvl > 6) {
      stop('expand lvl must be equal to or less than 6.')
    }
    if(expand$lvl < 1) {
      stop('expand lvl must be greater than or equal to 1.')
    }

    if(!is.numeric(expand$rate)) {
      stop('rate must be greater than or equal to 1.')
    }
    if(expand$rate < 1) {
      stop('rate must be greater than or equal to 1.')
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
    }

    # include the new dice
    rr <- c(rr, nr)
  }

  # 8th uses a simple formula to determine successful wounds
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

  # expanding results
  if(!is.null(expand)) {
    expand$x <- rr
    more_success <- do.call(expand_dice, expand)
    success <- success + more_success
  }

  # final count
  success
}
