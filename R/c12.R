new_C12 <- function(x = integer()) {
  # check whether the x is integer vector
  vec_assert(x, integer())
  # For each Klein_4 group, the range should be 0:3
  x[x > 11L | x < 0L] <- NA_integer_
  # create it!
  new_sgrp(x, group = "C12")
}

#' @title
#' Create Cyclic 12 group
#' @param x
#' An integer vector
#' @return
#' A Cyclic 12 group of Sgroupr_sgrp object
#' @examples
#' x <- C12(0:11)
#' @export
C12<- function(x = integer()) {
  # make sure the input data is integral.
  # vec_cast can change the x into the type integer
  x <- vec_cast(x, integer())
  new_C12(x)
}

#' @title
#' Check if a sgroupr_sgpr is C12
#' @param x An sgroupr_sgpr object
#' @return True of False
#' @export
#' @examples is_C12(C12(0:3))
is_C12 <- function(x) {
  result <- FALSE
  stopifnot(is_sgrp(x))
  if(group(x) == "C12") {
    return(!result)
  } else return(result)
}

#' @title
#' Change a vector into object of C12 group
#' @param x
#' A vector
#' @return
#' An Sgroupr_sgrp group belonging to C12
#' @export
#' @examples
#' x <- 0:7
#' as_C12(x)
as_C12 <- function(x) {
  as_sgrp(x, group = "C12")
}
