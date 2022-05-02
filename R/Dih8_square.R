new_Dih8 <- function(x = integer()) {
  # check whether the x is integer vector
  vctrs::vec_assert(x, integer())
  # create it!
  new_sgrp(x, group = "Dih8")
}

#' @title
#' Create a Dih8 small group of sgroupr_sgrp
#' @param x
#' An integer vector
#'
#' @return
#' Return an object of sgroupr_sgrp with group Dih8
#'
#' @examples
#' Dih8(0:7)
#' @export
Dih8 <- function(x = integer()) {
  # make sure the input data is integral.
  # vec_cast can change the x into the type integer
  x <- vec_cast(x, integer())
  new_Dih8(x)
}

#' @title Test whether x is Dih8
#' @param x
#' A Object of Sgroupr_sgrp
#'
#' @return True or False
#' @export
#' @examples
#' x <- Dih8(0:7)
#' is_Dih8(x)
is_Dih8 <- function(x) {
  result <- FALSE
  stopifnot(is_sgrp(x))
  if(group(x) == "Dih8") {
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
#' as_Dih8(x)
as_Dih8 <- function(x) {
  as_sgrp(x, group = "Dih8")
}
