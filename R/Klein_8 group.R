#'@title
#'Create Klein_8 sgroupr_sgrp object
#'
#'@description the length of this group should be 8
#'@param x, an integer vector
#'@example
#'Klein_8(0:3)
new_Klein_8 <- function(x = integer()) {
  vec_assert(x, integer())
  x[x > 7L | x < 0L] <- NA_integer_
  new_sgrp(x, group = "Klein_8")
}

Klein_8 <- function(x = integer()) {
  x <- vec_cast(x, integer())
  new_Klein_8(x)
}

is_Klein_8 <- function(x) {
  result <- FALSE
  stopifnot(is_sgrp(x))
  if(attributes(x)$group == "Klein_8") {
    return(!result)
  } else return(result)
}

