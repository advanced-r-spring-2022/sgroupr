new_C12 <- function(x = integer()) {
  # check whether the x is integer vector
  vec_assert(x, integer())
  # For each Klein_4 group, the range should be 0:3
  x[x > 11L | x < 0L] <- NA_integer_
  # create it!
  new_sgrp(x, group = "C12")
}

#' @export
C12<- function(x = integer()) {
  # make sure the input data is integral.
  # vec_cast can change the x into the type integer
  x <- vec_cast(x, integer())
  new_C12(x)
}

#' @export
is_C12 <- function(x) {
  result <- FALSE
  stopifnot(is_sgrp(x))
  if(group(x) == "C12") {
    return(!result)
  } else return(result)
}
