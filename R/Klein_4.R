new_Klein_4 <- function(x = integer()) {
  vec_assert(x, integer())
  new_sgrp(x, group = "Klein_4")
}

#' @title
#' Create a Klein_4 sgroupr_sgrp object
#' @param x An integer vector
#' @return a sgroupr_sgpr object with group Klein_4
#' @export
#' @examples
#' Klein_4(0:3)
Klein_4 <- function(x = integer()) {
  x <- vec_cast(x, integer())
  new_Klein_4(x)
}

#' @title
#' Check if a sgroupr_sgpr is Klein_4
#' @param  x an sgroupr_sgpr object
#' @return True of False
#' @export
#' @examples
#' is_Klein_4(Klein_4(0:3))
is_Klein_4 <- function(x) {
  result <- FALSE
  stopifnot(is_sgrp(x))
  if (attributes(x)$group == "Klein_4") {
    return(!result)
  } else return(result)
}

#' @title
#' Change a vector into object of Klein_4 group
#' @param x
#' A vector
#' @return
#' An Sgroupr_sgrp group belonging to Klein_4
#' @export
#' @examples
#' x <- 0:3
#' as_Klein_4(x)
as_Klein_4 <- function(x) {
  as_sgrp(x, group = "Klein_4")
}
