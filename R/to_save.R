#' Save against wound roll
#'
#' After a successful wound, the defender gets an opportunity to save and prevent that wound.
#'
#' @param n integer scalar number of wounds
#' @param sv integer scalar minimum save value (e.g. SM marine has SV 3+).
#' @param ap integer scalar armor piercing value of wound; negative and positive values accepted e.g. AP -3 and AP 3 are equivalent (default 0).
#' @param invul logical scalar is the save an invulnerability save? default FALSE.
#' @return integer scalar number of failed saves
to_save <- function(n, sv, ap = 0, invul = FALSE) {
  # defense
  if(!is.numeric(n)) {
    stop('Error: n must be numeric.')
  }
  if(n < 1) {
    stop('Error: must roll at least one die. n must be >= 1.')
  }

  if(!is.numeric(sv)) {
    stop('Error: save must be a number.')
  }
  if(sv < 2) {
    stop('Error: Cannot have a save greater than 2+. sv < 2.')
  }
  if(sv > 6) {
    warning('save value > 6 gives valid results, but will not prevent wounds.')
  }

  if(!is.numeric(ap)) {
    stop('Error: ap must be numeric.')
  }

  if(!is.null(invul)) {
    if(!is.numeric(sv)) {
      stop('Error: invul must be a number.')
    }
    if(sv < 2) {
      stop('Error: Cannot have a invul greater than 2+. sv < 2.')
    }
    if(sv > 6) {
      warning('Error: invul value > 6 gives valid results, but will not prevent wounds.')
    }
  }

  # strip AP of direction
  ap <- abs(ap)

  # determine save level -- only matters if not an invul
  if(invul == FALSE) {
    sv <- sv + ap
  }

  # roll saves
  rr <- roll_dice(n)
  fails <- sum(rr >= sv)               # this technically works even if save 7+

  fails
}


#' Feel No Pain
#'
#' After a failed save, some rules allow an additional save-after-the-save to prevent wounds individually.
#' This is sometimes refered as a shrug.
#' This function stands in place of e.g. Disgustingly Resilient.
#' 8th does not have universal rules, but this function uses the older name for convenience.
#'
#' @param n integer scalar number of un-saved wounds
#' @param fnp integer scalar minimum to ignore wound (e.g. fnp = 5 for 5+ Disgustingly Resilient)
#' @return integer scalar number of failed FNPs
to_fnp <- function(n, fnp) {
  # defense
  if(!is.numeric(n)) {
    stop('Error: n must be numeric.')
  }
  if(n < 1) {
    stop('Error: Must roll at least one die. n must be >= 1.')
  }

  if(!is.numeric(fnp)) {
    stop('Error: fnp must be a number.')
  }
  if(fnp < 2) {
    stop('Error: Cannot have a fnp greater than 2+. sv < 2.')
  }
  if(fnp > 6) {
    warning('Error: fnp value > 6 gives valid results, but will not prevent wounds.')
  }

  # this is easy
  rr <- roll_dice(n)
  fails <- sum(rr >= fnp)

  fails
}
