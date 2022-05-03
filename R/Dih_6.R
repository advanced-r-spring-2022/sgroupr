new_Dih_6 <- function(x = integer()) {
  # check whether the x is integer vector
  vctrs::vec_assert(x, integer())
  # For each Klein_4 group, the range should be 0:3
  # create it!
  new_sgrp(x, group = "Dih_6")
}

#' @title
#' Create a Dih_6 small group of sgroupr_sgrp
#' @param x
#' An integer vector
#' @return
#' Return an object of sgroupr_sgrp with group Dih_6
#' @examples
#' Dih_6(0:7)
#' @export
Dih_6 <- function(x = integer()) {
  x <- vctrs::vec_cast(x, integer())
  new_Dih_6(x)
}

#' @title Test whether x is Dih_6
#' @param x
#' A Object of Sgroupr_sgrp
#'
#' @return True or False
#' @export
#' @examples
#' x <- Dih_6(0:7)
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
#' @param x
#' A vector
#' @return
#' An Sgroupr_sgrp group belonging to Dih_6
#' @export
#' @examples
#' x <- 0:7
#' as_Dih_6(x)
as_Dih_6 <- function(x) {
  as_sgrp(x, group = "Dih_6")
}

