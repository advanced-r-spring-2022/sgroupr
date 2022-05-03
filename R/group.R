#' @title
#' Extract the group attribute of a sgroupr_sgrp
#' @param x
#' An sgroupr_sgrp object
#' @return a character in lenth of 1.
#' @export
#' @examples
#' x <- Klein_4(c(0,1,2,3,4,5,6,7))
#' group(x)
group <- function(x) {
  stopifnot(is_sgrp(x))
  attr(x,"group")
}
