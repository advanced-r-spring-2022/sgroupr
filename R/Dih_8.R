new_Dih_8 <- function(x = integer()) {
  # check whether the x is integer vector
  vec_assert(x, integer())
  # For each Cyclic_3 group, the range should be 0:3
  x[x > 8L | x < 0L] <- NA_integer_
  # create it!
  new_sgrp(x, group = "Dih_8")
}

#' @title
#' Create a dihedral group Dih8 (Heisenberg) sgroupr_sgrp object
#'
#' @description The length of dihedral group Dih8 (Heisenberg) should be 0 to 7
#' @param x Is an integer vector
#' @return A sgroupr_sgpr object with dihedral group Dih8 (Heisenberg)
#' @export
#' @examples
#' Dih_8(0:7)
Dih_8 <- function(x = integer()) {
  # make sure the input data is integral.
  # vec_cast can change the x into the type integer
  x <- vec_cast(x, integer())
  new_Dih_8(x)
}

#' @title
#' Check if a sgroupr_sgpr is dihedral group Dih8 (Heisenberg)
#' @param x An sgroupr_sgpr object
#' @return True of False
#' @export
#' @examples is_Dih_8(Dih_8(0:7))
is_Dih_8 <- function(x) {
  result <- FALSE
  stopifnot(is_sgrp(x))
  if(group(x) == "Dih_8") {
    return(!result)
  } else return(result)
}

#' @title
#' Change a vector into object of dihedral group Dih8 (Heisenberg) group
#' @param x
#' A vector
#' @return
#' An Sgroupr_sgrp group belonging to dihedral group Dih8 (Heisenberg)
#' @export
#' @examples
#' x <- 0:7
#' as_Dih_8(x)
as_Dih_8 <- function(x) {
  as_sgrp(x, group = "Dih_8")
}





