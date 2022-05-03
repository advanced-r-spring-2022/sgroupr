new_Dih_12 <- function(x = integer()) {
  vctrs::vec_assert(x, integer())
  new_sgrp(x, group = "Dih_12")
}

#' @title
#' Create a Dih_12 sgroupr_sgrp object
#' @description Dih_12 belongs to the dihedral Group
#' @param x An integer vector
#' @return a Dih_12 sgroupr_sgrp
#' @export
#' @examples
#' Dih_12(0:11)
Dih_12 <- function(x = integer()) {
  x <- vec_cast(x, integer())
  new_Dih_12(x)
}

#' @title
#' Check whether an sgroupr_sgrp is Dih_12
#' @param x An sgroupr_sgrp
#' @return True or False
#' @export
#' @examples
#' x <- Dih_12(0:11)
#' is_Dih_12(x)
is_Dih_12 <- function(x) {
  result <- FALSE
  stopifnot(is_sgrp(x))
  if (group(x) == "Dih_12") {
    return(!result)
  } else return(result)
}

#' @title
#' Change a vector into object of Dih_12 group
#' @param x A vector
#' @return An Sgroupr_sgrp group belonging to Dih_12
#' @export
#' @examples
#' x <- 0:11
#' as_Dih_12(x)
as_Dih_12 <- function(x) {
  as_sgrp(x, group = "Dih_12")
}
