new_C4_C2 <- function(x = integer()) {
  vec_assert(x, integer())
  new_sgrp(x, group = "C4_C2")
}

#' @title
#' Create a C4_C2 sgroupr_sgrp object
#' @description C4_C2 belongs to the multiplication Group
#' @param x An integer vector
#' @return a sgroupr_sgpr object with group C4_C2
#' @export
#' @examples
#' C4_C2(0:7)
C4_C2 <- function(x = integer()) {
  x <- vec_cast(x, integer())
  new_C4_C2(x)
}

#' @title
#' Check if a sgroupr_sgpr is C4_C2
#' @param  x an sgroupr_sgpr object
#' @return True of False
#' @export
#' @examples is_C4_C2(C4_C2(0:7))
is_C4_C2 <- function(x) {
  result <- FALSE
  stopifnot(is_sgrp(x))
  if (attributes(x)$group == "C4_C2") {
    return(!result)
  } else return(result)
}


#' @title
#' Change a vector into object of C4_C2 group
#' @param x A vector
#' @return An Sgroupr_sgrp group that belongs to C4_C2
#' @export
#' @examples
#' x <- 0:7
#' as_C4_C2(x)
as_C4_C2 <- function(x) {
  as_sgrp(x, group = "C4_C2")
}
