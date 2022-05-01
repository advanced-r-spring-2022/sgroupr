#'@title
#'Create the C6C2 sgroupr_sgrp object
#'
#'@description The length of C6C2 should be 12
#'@param x, an integer
#'
#'
#'@example
C6C2(0:12)

new_C6C2 <- function(x = integer()) {
  vec_assert(x, integer())
  x[x > 11L | x < 0L] <- NA_integer_
  new_sgrp(x, group = "C6C2")
}

C6C2 <- function(x = integer()) {
  x <- vec_cast(x, integer())
  new_C6C2(x)
}

is_C6C2 <- function(x) {
  result <- FALSE
  stopifnot(is_sgrp(x))
  if(attributes(x)$group == "C6C2") {
    return(!result)
  } else return(result)
}

