#'@title
#'Create Cyclic_4 sgroupr_sgrp object
#'
#'@description the length of this group should be 3
#'@param x, an integer vector
#'@example
#'Cyclic_4(0:3)
new_Cyclic_4 <- function(x = integer()) {
  vec_assert(x, integer())
  x[x > 3L | x < 0L] <- NA_integer_
  new_sgrp(x, group = "Cyclic_4")
}

Cyclic_4 <- function(x = integer()) {
  x <- vec_cast(x, integer())
  new_Cyclic_4(x)
}


is_Cyclic_4 <- function(x) {
  result <- FALSE
  stopifnot(is_sgrp(x))
  if(attributes(x)$group == "Cyclic_4") {
    return(!result)
  } else return(result)
}

