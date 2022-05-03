new_C6_C2 <- function(x = integer()) {
  vec_assert(x, integer())
  new_sgrp(x, group = "C6_C2")
}
#' @title
#' Create a C6_C2 sgroupr_sgrp object
#' @param x An integer vector
#' @return a sgroupr_sgpr object with group C6_C2
#' @export
#' @examples
#' C6_C2(0:11)
C6_C2 <- function(x = integer()) {
  x <- vec_cast(x, integer())
  new_C6_C2(x)
}


#' @title
#' Check if a sgroupr_sgpr is C6_C2
#' @param  x an sgroupr_sgpr object
#' @return True of False
#' @export
#' @examples
#' is_C6_C2(C6_C2(0:11))
is_C6_C2 <- function(x) {
  result <- FALSE
  stopifnot(is_sgrp(x))
  if(attributes(x)$group == "C6_C2") {
    return(!result)
  } else return(result)
}

#' @title
#' Change a vector into object of C6_C2 group
#' @param x
#' A vector
#' @return
#' An Sgroupr_sgrp group belonging to C6_C2
#' @export
#' @examples
#' x <- 0:7
#' as_C6_C2(x)
as_C6_C2 <- function(x) {
  as_sgrp(x, group = "C6_C2")
}
