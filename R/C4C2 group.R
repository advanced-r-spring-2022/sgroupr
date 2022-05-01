#'@title 
#'Create a C4C2 sgroupr_sgrp object
#'
#'@description The length of this group should be 8
#'@param x, an integer vector
#'@example
#'C4C2(0:7)
new_C4C2 <- function(x = integer()) {
  vec_assert(x, integer())
  x[x > 7L | x < 0L] <- NA_integer_
  new_sgrp(x, group = "C4C2")
}
 

C4C2 <- function(x = integer()) {
  x <- vec_cast(x, integer())
  new_C4C2(x)
}
 

is_C4C2 <- function(x) {
  result <- FALSE
  stopifnot(is_sgrp(x))
  if(attributes(x)$group == "C4C2") {
    return(!result)
  } else return(result)
}




