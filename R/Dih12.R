new_Dih12 <- function(x = integer()) {
  # check whether the x is integer vector
  vctrs::vec_assert(x, integer())
  x[x > 11L | x < 0L] <- NA_integer_
  # create it!
  new_sgrp(x, group = "Dih12")
}

#' @title
#' Helper function of Dih2 group
#' @param x An integer vector
#' @return a Dih12 sgroupr_sgrp
#' @export
#' @examples
#' Dih12(0:11)
Dih12 <- function(x = integer()) {
  # make sure the input data is integral.
  # vec_cast can change the x into the type integer
  x <- vec_cast(x, integer())
  new_Dih12(x)
}

#' @title
#' Check whether an sgroupr_sgrp is Dih12
#' @param x An sgroupr_sgrp
#' @return True or False
#' @export
#' @examples
#' x <- Dih12(0:11)
#' is_Dih12(x)
is_Dih12 <- function(x) {
  result <- FALSE
  stopifnot(is_sgrp(x))
  if(group(x) == "Dih12") {
    return(!result)
  } else return(result)
}

#' @title
#' Change a vector into object of Dih8 group
#' @param x
#' A vector
#' @return
#' An Sgroupr_sgrp group belonging to Dih8
#' @export
#' @examples
#' x <- 0:7
#' as_Dih12(x)
as_Dih12 <- function(x) {
  as_sgrp(x, group = "Dih12")
}

