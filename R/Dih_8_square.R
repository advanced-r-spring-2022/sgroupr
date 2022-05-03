new_Dih_8_square <- function(x = integer()) {
  vctrs::vec_assert(x, integer())
  new_sgrp(x, group = "Dih_8_square")
}

#' @title
#' Create a Dih_8 (Square) small group of sgroupr_sgrp
#' @description Dih_8 (Square) belongs to the dihedral Group
#' @param x An integer vector
#' @return An object of sgroupr_sgrp with group Dih_8
#' @examples
#' Dih_8_square(0:7)
#' @export
Dih_8_square <- function(x = integer()) {
  x <- vec_cast(x, integer())
  new_Dih_8_square(x)
}

#' @title Test whether x is Dih_8 (Square)
#' @param x A Object of Sgroupr_sgrp
#' @return True or False
#' @export
#' @examples
#' x <- Dih_8_square(0:7)
#' is_Dih_8_square(x)
is_Dih_8_square <- function(x) {
  result <- FALSE
  stopifnot(is_sgrp(x))
  if (group(x) == "Dih_8_square") {
    return(!result)
  } else return(result)
}


#' @title
#' Change a vector into object of Dih8 group
#' @param x A vector
#' @return An Sgroupr_sgrp group belonging to Dih8
#' @export
#' @examples
#' x <- 0:7
#' as_Dih_8_square(x)
as_Dih_8_square <- function(x) {
  as_sgrp(x, group = "Dih_8_square")
}
