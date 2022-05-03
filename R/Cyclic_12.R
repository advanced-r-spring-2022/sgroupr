new_Cyclic_12 <- function(x = integer()) {
  vec_assert(x, integer())
  x[x > 11L | x < 0L] <- NA_integer_
  new_sgrp(x, group = "Cyclic_12")
}

#' @title
#' Create Cyclic 12 group
#' @description Cyclic 12 belongs to the Cyclic Group
#' @param x An integer vector
#' @return A Cyclic 12 group of Sgroupr_sgrp object
#' @examples
#' x <- Cyclic_12(0:11)
#' @export
Cyclic_12<- function(x = integer()) {
  x <- vec_cast(x, integer())
  new_Cyclic_12(x)
}

#' @title
#' Check if a sgroupr_sgpr is C12
#' @param x An sgroupr_sgpr object
#' @return True of False
#' @export
#' @examples is_Cyclic_12(Cyclic_12(0:11))
is_Cyclic_12 <- function(x) {
  result <- FALSE
  stopifnot(is_sgrp(x))
  if (group(x) == "Cyclic_12") {
    return(!result)
  } else return(result)
}

#' @title
#' Change a vector into object of C12 group
#' @param x A vector
#' @return An Sgroupr_sgrp group belonging to C12
#' @export
#' @examples
#' x <- 0:11
#' as_Cyclic_12(x)
as_Cyclic_12 <- function(x) {
  as_sgrp(x, group = "Cyclic_12")
}
