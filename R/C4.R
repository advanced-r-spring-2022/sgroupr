new_C4 <- function(x = integer()) {
  # check whether the x is integer vector
  vctrs::vec_assert(x, integer())
  # For each Klein_4 group, the range should be 0:3
  # create it!
  new_sgrp(x, group = "C4")
}

#' @title
#' Create a C4 sgroupr_sgrp object
#' @param x An integer vector
#' @return a sgroupr_sgpr object with group C4
#' @export
#' @examples
#' C4(0:4)
C4 <- function(x = integer()) {
  # make sure the input data is integral.
  # vec_cast can change the x into the type integer
  x <- vec_cast(x, integer())
  new_C4(x)
}

#' @title
#' Check if a sgroupr_sgpr is C4
#' @param  x an sgroupr_sgpr object
#' @return True of False
#' @export
#' @examples is_C4(C4(0:3))
is_C4 <- function(x) {
  result <- FALSE
  stopifnot(is_sgrp(x))
  if(group(x) == "C4") {
    return(!result)
  } else return(result)
}

#' @title
#' Change a vector into object of C4 group
#' @param x
#' A vector
#' @return
#' An Sgroupr_sgrp group belonging to C4
#' @export
#' @examples
#' x <- 0:7
#' as_C4(x)
as_C4 <- function(x) {
  as_sgrp(x, group = "C4")
}

