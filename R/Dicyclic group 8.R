new_Dcyclic_8 <- function(x = integer()) {
  vec_assert(x, integer())
  x[x > 7L | x < 0L] <- NA_integer_
  new_sgrp(x, group = "Dcyclic_8")
}

#' @title
#' Create a Dcyclic_8 sgroupr_sgrp object
#' @param x An integer vector
#' @return a sgroupr_sgpr object with group Dcyclic_8
#' @export
#' @examples
#' Dcyclic_8(0:7)
Dcyclic_8 <- function(x = integer()) {
  x <- vec_cast(x, integer())
  new_Dcyclic_8(x)
}

#' @title
#' Check if a sgroupr_sgpr is Dcyclic_8
#' @param  x an sgroupr_sgpr object
#' @return True of False
#' @export
#' @examples 
#' is_Dcyclic_8(Dcyclic_8(0:7))
is_Dcyclic_8 <- function(x) {
  result <- FALSE
  stopifnot(is_sgrp(x))
  if(attributes(x)$group == "Dcyclic_8") {
    return(!result)
  } else return(result)
}

#' @title
#' Change a vector into object of Dcyclic_8 group
#' @param x
#' A vector
#' @return
#' An Sgroupr_sgrp group belonging to Dcyclic_8
#' @export
#' @examples
#' x <- 0:7
#' as_Dcyclic_8(x)
as_Dcyclic_8 <- function(x) {
  as_sgrp(x, group = "Dcyclic_8")
}
