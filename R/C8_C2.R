new_C8_C2 <- function(x = integer()) {
  vec_assert(x, integer())
  new_sgrp(x, group = "C8_C2")
}

#' @title
#' Create a C8_C2 sgroupr_sgrp object
#' @param x An integer vector
#' @return a sgroupr_sgpr object with group C8_C2
#' @export
#' @examples
#' C8_C2(0:17)
C8_C2 <- function(x = integer()) {
  x <- vec_cast(x, integer())
  new_C8_C2(x)
}

#' @title
#' Check if a sgroupr_sgpr is C8_C2
#' @param  x an sgroupr_sgpr object
#' @return True of False
#' @export
#' @examples
#' is_C8_C2(C8_C2(0:17))
is_C8_C2 <- function(x) {
  result <- FALSE
  stopifnot(is_sgrp(x))
  if(attributes(x)$group == "C8_C2") {
    return(!result)
  } else return(result)
}

#' @title
#' Change a vector into object of C8_C2 group
#' @param x
#' A vector
#' @return
#' An Sgroupr_sgrp group belonging to C8_C2
#' @export
#' @examples
#' x <- 0:5
#' as_C8_C2(x)
as_C8_C2 <- function(x) {
  as_sgrp(x, group = "C8_C2")
}

