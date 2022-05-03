new_C3_C2 <- function(x = integer()) {
  vec_assert(x, integer())
  new_sgrp(x, group = "C3_C2")
}

#' @title
#' Create a C3C2 sgroupr_sgrp object
#' @param x An integer vector
#' @return a sgroupr_sgpr object with group C6C2
#' @export
#' @examples
#' C3_C2(0:5)
C3_C2 <- function(x = integer()) {
  x <- vec_cast(x, integer())
  new_C3_C2(x)
}

#' @title
#' Check if a sgroupr_sgpr is C3C2
#' @param  x an sgroupr_sgpr object
#' @return True of False
#' @export
#' @examples
#' is_C3_C2(Klein_4(0:5))
is_C3_C2 <- function(x) {
  result <- FALSE
  stopifnot(is_sgrp(x))
  if (attributes(x)$group == "C3_C2") {
    return(!result)
  } else return(result)
}

#' @title
#' Change a vector into object of C3C2 group
#' @param x
#' A vector
#' @return
#' An Sgroupr_sgrp group belonging to C3C2
#' @export
#' @examples
#' x <- 0:5
#' as_C3_C2(x)
as_C3_C2 <- function(x) {
  as_sgrp(x, group = "C3_C2")
}
