#'@title
#'Casting
#'@description Allows explicitly to change the object type
#'@param x An object
#'@param ... Other parameters
#'@param to An object
#'@return A sgroupr object
#' @export
vec_cast.sgroupr_sgrp.sgroupr_sgrp <- function(x, to, ...) {
  new_sgrp(vctrs::vec_data(x), group = group(to))
}

#' @export
vec_cast.sgroupr_sgrp.integer <- function(x, to, ...) {
  new_sgrp(x, group = group(to))
}

#' @export
vec_cast.integer.sgroupr_sgrp <- function(x, to, ...)  vctrs::vec_data(x)
