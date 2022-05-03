new_Klein_8 <- function(x = integer()) {
  vec_assert(x, integer())
  new_sgrp(x, group = "Klein_8")
}

#' @title
#' Create a Klein_8 sgroupr_sgrp object
#' @description Klein_8 belongs to the Klein Group
#' @param x An integer vector
#' @return a sgroupr_sgpr object with group Klein_8
#' @export
#' @examples
#' Klein_8(0:7)
Klein_8 <- function(x = integer()) {
  x <- vec_cast(x, integer())
  new_Klein_8(x)
}

#' @title
#' Check if a sgroupr_sgpr is Klein_8
#' @param  x an sgroupr_sgpr object
#' @return True of False
#' @export
#' @examples
#' is_Klein_8(Klein_8(0:7))
is_Klein_8 <- function(x) {
  result <- FALSE
  stopifnot(is_sgrp(x))
  if (attributes(x)$group == "Klein_8") {
    return(!result)
  } else return(result)
}

#' @title
#' Change a vector into object of Klein_8 group
#' @param x A vector
#' @return An Sgroupr_sgrp group belonging to Klein_8
#' @export
#' @examples
#' x <- 0:7
#' as_Klein_8(x)
as_Klein_8 <- function(x) {
  as_sgrp(x, group = "Klein_8")
}

