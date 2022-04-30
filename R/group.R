#' @export
group <- function(x) {
  stopifnot(is_sgrp(x))
  attr(x,"group")
}

#' @export
`group<-` <- function(x,group) {
  stopifnot(is_sgrp(x))
  attr(x,"group") <- group
}
