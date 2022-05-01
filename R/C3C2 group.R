#'@title
#'Create the C3C2 sgroupr_sgrp object
#'
#'@description The length of C3C2 should be 5
#'@param x, an integer
#'
#'
#'@example
C3C2(0:5)

new_C3C2 <- function(x = integer()) {
  vec_assert(x, integer())
  x[x > 5L | x < 0L] <- NA_integer_
  new_sgrp(x, group = "C3C2")
}

#'@export
C3C2 <- function(x = integer()) {
  x <- vec_cast(x, integer())
  new_C3C2(x)
}


is_C3C2 <- function(x) {
  result <- FALSE
  stopifnot(is_sgrp(x))
  if(attributes(x)$group == "C3C2") {
    return(!result)
  } else return(result)
}

