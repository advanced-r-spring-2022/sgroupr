new_C8C2 <- function(x = integer()) {
  vec_assert(x, integer())
  x[x > 17L | x < 0L] <- NA_integer_
  new_sgrp(x, group = "C8C2")
}

#' @title
#' Create a C8C2 sgroupr_sgrp object
#' @param x An integer vector
#' @return a sgroupr_sgpr object with group C8C2
#' @export
#' @examples
#' C8C2(0:17)
C8C2 <- function(x = integer()) {
  x <- vec_cast(x, integer())
  new_C8C2(x)
}

#' @title
#' Check if a sgroupr_sgpr is C8C2
#' @param  x an sgroupr_sgpr object
#' @return True of False
#' @export
#' @examples 
#' is_C8C2(C8C2(0:17))
is_C8C2 <- function(x) {
  result <- FALSE
  stopifnot(is_sgrp(x))
  if(attributes(x)$group == "C8C2") {
    return(!result)
  } else return(result)
}

#' @title
#' Change a vector into object of C8C2 group
#' @param x
#' A vector
#' @return
#' An Sgroupr_sgrp group belonging to C8C2
#' @export
#' @examples
#' x <- 0:5
#' as_C3C2(x)
as_C8C2 <- function(x) {
  as_sgrp(x, group = "C8C2")
}

