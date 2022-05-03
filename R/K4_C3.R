new_K4_C3 <- function(x = integer()) {
  # check whether the x is integer vector
  vctrs::vec_assert(x, integer())
  # create it!
  new_sgrp(x, group = "K4_C3")
}

#' @title
#' Create a K4_C3 small group of sgroupr_sgrp
#' @param x
#' An integer vector
#'
#' @return
#' Return an object of sgroupr_sgrp with group K4_C3
#'
#' @examples
#' K4_C3(0:11)
#' @export
K4_C3 <- function(x = integer()) {
  # make sure the input data is integral.
  # vec_cast can change the x into the type integer
  x <- vec_cast(x, integer())
  new_K4_C3(x)
}

#' @title Test whether x is K4_C3
#' @param x
#' A Object of Sgroupr_sgrp
#'
#' @return True or False
#' @export
#' @examples
#' x <- K4_C3(0:11)
#' is_K4_C3(x)
is_K4_C3 <- function(x) {
  result <- FALSE
  stopifnot(is_sgrp(x))
  if (group(x) == "K4_C3") {
    return(!result)
  } else return(result)
}


#' @title
#' Change a vector into object of K4_C3 group
#' @param x
#' A vector
#' @return
#' An Sgroupr_sgrp group belonging to K4_C3
#' @export
#' @examples
#' x <- 0:11
#' as_K4_C3(x)
as_K4_C3 <- function(x) {
  as_sgrp(x, group = "K4_C3")
}
