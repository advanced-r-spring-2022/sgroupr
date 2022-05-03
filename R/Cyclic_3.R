new_Cyclic_3 <- function(x = integer()) {
  vctrs::vec_assert(x, integer())
  new_sgrp(x, group = "Cyclic_3")
}

#' @title
#' Create a Cyclic 3 sgroupr_sgrp object
#' @description Cyclic 3 belongs to the Cyclic Group
#' @param x Is an integer vector
#' @return A sgroupr_sgpr object with group Cyclic_3
#' @export
#' @examples
#' Cyclic_3(0:2)
Cyclic_3 <- function(x = integer()) {
  x <- vctrs::vec_cast(x, integer())
  new_Cyclic_3(x)
}


#' @title
#' Change a vector into object of Cyclic_3 group
#' @param x A vector
#' @return An Sgroupr_sgrp group belonging to Cyclic_3
#' @export
#' @examples
#' x <- 0:2
#' as_Cyclic_3(x)
as_Cyclic_3 <- function(x) {
  as_sgrp(x, group = "Cyclic_3")
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
