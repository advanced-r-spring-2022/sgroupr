new_Q8_C2 <- function(x = integer()) {
  vec_assert(x, integer())
  new_sgrp(x, group = "Q8_C2")
}

#' @title
#' Create a Q8_C2 sgroupr_sgrp object
#' @description Q8_C2 belongs to the Multiplication Group
#' @param x An integer vector
#' @return a sgroupr_sgpr object with group Q8_C2
#' @export
#' @examples
#' Q8_C2(0:15)
Q8_C2 <- function(x = integer()) {
  x <- vec_cast(x, integer())
  new_Q8_C2(x)
}

#' @title
#' Check if a sgroupr_sgpr is Q8_C2
#' @param  x an sgroupr_sgpr object
#' @return True of False
#' @export
#' @examples
#' is_Q8_C2(Q8_C2(0:15))
is_Q8_C2 <- function(x) {
  result <- FALSE
  stopifnot(is_sgrp(x))
  if(attributes(x)$group == "Q8_C2") {
    return(!result)
  } else return(result)
}

#' @title
#' Change a vector into object of Q8_C2 group
#' @param x A vector
#' @return An Sgroupr_sgrp group belonging to Q8_C2
#' @export
#' @examples
#' x <- 0:15
#' as_Q8_C2(x)
as_Q8_C2 <- function(x) {
  as_sgrp(x, group = "Q8_C2")
}

