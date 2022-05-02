new_S3 <- function(x = integer()) {
  # check whether the x is integer vector
  vctrs::vec_assert(x, integer())
  # For each Klein_4 group, the range should be 0:3
  # create it!
  new_sgrp(x, group = "S3")
}

#' @title
#' Create a S3 small group of sgroupr_sgrp
#' @param x
#' An integer vector
#' @return
#' Return an object of sgroupr_sgrp with group S3
#' @examples
#' S3(0:7)
#' @export
S3 <- function(x = integer()) {
  x <- vctrs::vec_cast(x, integer())
  new_S3(x)
}

#' @title Test whether x is S3
#' @param x
#' A Object of Sgroupr_sgrp
#'
#' @return True or False
#' @export
#' @examples
#' x <- S3(0:7)
#' is_S3(x)
is_S3 <- function(x) {
  result <- FALSE
  stopifnot(is_sgrp(x))
  if(group(x) == "S3") {
    return(!result)
  } else return(result)
}


#' @title
#' Change a vector into object of S3 group
#' @param x
#' A vector
#' @return
#' An Sgroupr_sgrp group belonging to S3
#' @export
#' @examples
#' x <- 0:7
#' as_S3(x)
as_S3 <- function(x) {
  as_sgrp(x, group = "S3")
}

