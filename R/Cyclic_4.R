new_Cyclic_4 <- function(x = integer()) {
  vec_assert(x, integer())
  x[x > 3L | x < 0L] <- NA_integer_
  new_sgrp(x, group = "Cyclic_4")
}

#' @title
#' Create a Cyclic_4 sgroupr_sgrp object
#' @description Cyclic 4 belongs to the Cyclic Group
#' @param x An integer vector
#' @return a sgroupr_sgpr object with group Cyclic_4
#' @export
#' @examples
#' Cyclic_4(0:3)
Cyclic_4 <- function(x = integer()) {
  x <- vec_cast(x, integer())
  new_Cyclic_4(x)
}

#' @title
#' Check if a sgroupr_sgpr is Cyclic_4
#' @param  x an sgroupr_sgpr object
#' @return True of False
#' @export
#' @examples
#' is_Cyclic_4(Cyclic_4(0:3))
is_Cyclic_4 <- function(x) {
  result <- FALSE
  stopifnot(is_sgrp(x))
  if (attributes(x)$group == "Cyclic_4") {
    return(!result)
  } else return(result)
}

#' @title
#' Change a vector into object of Cyclic_4 group
#' @param x A vector
#' @return An Sgroupr_sgrp group belonging to Cyclic_4
#' @export
#' @examples
#' x <- 0:3
#' as_Cyclic_4(x)
as_Cyclic_4 <- function(x) {
  as_sgrp(x, group = "Cyclic_4")
}
