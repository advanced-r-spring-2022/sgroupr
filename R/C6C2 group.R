new_C6C2 <- function(x = integer()) {
  vec_assert(x, integer())
  x[x > 11L | x < 0L] <- NA_integer_
  new_sgrp(x, group = "C6C2")
}

#' @title
#' Create a C6C2 sgroupr_sgrp object
#' @param x An integer vector
#' @return a sgroupr_sgpr object with group C6C2
#' @export
#' @examples
#' C6C2(0:11)
C6C2 <- function(x = integer()) {
  x <- vec_cast(x, integer())
  new_C6C2(x)
}


#' @title
#' Check if a sgroupr_sgpr is C6C2
#' @param  x an sgroupr_sgpr object
#' @return True of False
#' @export
#' @examples 
#' is_C6C2(C6C2(0:11))
is_C6C2 <- function(x) {
  result <- FALSE
  stopifnot(is_sgrp(x))
  if(attributes(x)$group == "C6C2") {
    return(!result)
  } else return(result)
}

#' @title
#' Change a vector into object of C6C2 group
#' @param x
#' A vector
#' @return
#' An Sgroupr_sgrp group belonging to C6C2
#' @export
#' @examples
#' x <- 0:7
#' as_C6C2(x)
as_C6C2 <- function(x) {
  as_sgrp(x, group = "C6C2")
}
