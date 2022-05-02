new_cyclic_3 <- function(x = integer()) {
  # check whether the x is integer vector
  vctrs::vec_assert(x, integer())
  # For each Cyclic_3 group, the range should be 0:3
  x[x > 2L | x < 0L] <- NA_integer_
  # create it!
  new_sgrp(x, group = "Cyclic_3")
}

#' @title
#' Create a Cyclic_3 sgroupr_sgrp object
#'
#' @description The length of Cyclic 3 should be 0 to 2
#' @param x Is an integer vector
#' @return A sgroupr_sgpr object with group Cyclic_3
#' @export
#' @examples
#' Cyclic_3(0:2)
Cyclic_3 <- function(x = integer()) {
  # make sure the input data is integral.
  # vec_cast can change the x into the type integer
  x <- vctrs::vec_cast(x, integer())
  new_cyclic_3(x)
}

#' @title
#' Check if a sgroupr_sgpr is Cyclic 3
#' @param x An sgroupr_sgpr object
#' @return True of False
#' @export
#' @examples is_Cyclic_3(Cyclic_3(0:2))
is_Cyclic_3 <- function(x) {
  result <- FALSE
  stopifnot(is_sgrp(x))
  if(group(x) == "Cyclic_3") {
    return(!result)
  } else return(result)
}

#' @title
#' Change a vector into object of Cyclic 3 group
#' @param x
#' A vector
#' @return
#' An Sgroupr_sgrp group belonging to Cyclic 3
#' @export
#' @examples
#' x <- 0:2
#' as_Cyclic_3(x)
as_Cyclic_3 <- function(x) {
  as_sgrp(x, group = "Cyclic_3")
}
