new_C4_C4 <- function(x = integer()) {
  vec_assert(x, integer())
  x[x > 15L | x < 0L] <- NA_integer_
  new_sgrp(x, group = "C4_C4")
}

#' @title
#' Create a C4_C4 sgroupr_sgrp object
#' @param x An integer vector
#' @return a sgroupr_sgpr object with group C4_C4
#' @export
#' @examples
#' C4_C4(0:15)
C4_C4 <- function(x = integer()) {
  x <- vec_cast(x, integer())
  new_C4_C4(x)
}

#' @title
#' Check if a sgroupr_sgpr is C4_C4
#' @param  x an sgroupr_sgpr object
#' @return True of False
#' @export
#' @examples is_C4_C4(C4_C4(0:15))
is_C4_C4 <- function(x) {
  result <- FALSE
  stopifnot(is_sgrp(x))
  if (attributes(x)$group == "C4_C4") {
    return(!result)
  } else return(result)
}


#' @title
#' Change a vector into object of C4_C4 group
#' @param x
#' A vector
#' @return
#' An Sgroupr_sgrp group belonging to C4_C4
#' @export
#' @examples
#' x <- 0:15
#' as_C4_C4(x)
as_C4_C4 <- function(x) {
  as_sgrp(x, group = "C4_C4")
}
