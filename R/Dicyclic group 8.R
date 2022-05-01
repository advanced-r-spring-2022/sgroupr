#'@title
#'Create Dicyclic 8 sgroupr_sgrp object
#'
#'@description the length of this group should be 8
#'@param x, an integer vector
#'@example
#'Dcyclic_8(0:3)
new_Dcyclic_8 <- function(x = integer()) {
  vec_assert(x, integer())
  x[x > 7L | x < 0L] <- NA_integer_
  new_sgrp(x, group = "Dcyclic_8")
}

Dcyclic_8 <- function(x = integer()) {
  x <- vec_cast(x, integer())
  new_Dcyclic_8(x)
}

is_Dcyclic_8 <- function(x) {
  result <- FALSE
  stopifnot(is_sgrp(x))
  if(attributes(x)$group == "Dcyclic_8") {
    return(!result)
  } else return(result)
}