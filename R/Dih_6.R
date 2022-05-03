new_Dih_6 <- function(x = integer()) {
  vctrs::vec_assert(x, integer())
  new_sgrp(x, group = "Dih_6")
}

#' @title
#' Create a Dih_6 small group of sgroupr_sgrp
#' @description Dih_6 belongs to the dihedral Group
#' @param x An integer vector
#' @return An object of sgroupr_sgrp with group Dih_6
#' @examples
#' Dih_6(0:5)
#' @export
Dih_6 <- function(x = integer()) {
  x <- vctrs::vec_cast(x, integer())
  new_Dih_6(x)
}

#' @title Test whether x is Dih_6
#' @param x A Object of Sgroupr_sgrp
#' @return True or False
#' @export
#' @examples
#' x <- Dih_6(0:5)
#' is_Dih_6(x)
is_Dih_6 <- function(x) {
  result <- FALSE
  stopifnot(is_sgrp(x))
  if(group(x) == "Dih_6") {
    return(!result)
  } else return(result)
}


#' @title
#' Change a vector into object of Dih_6 group
#' @param x A vector
#' @return An Sgroupr_sgrp group belonging to Dih_6
#' @export
#' @examples
#' x <- 0:5
#' as_Dih_6(x)
as_Dih_6 <- function(x) {
  as_sgrp(x, group = "Dih_6")
}

