#' @export
as_sgrp <- function(x,...) {
  UseMethod("as_sgrp")
}

#' @export
as_sgrp.default <- function(x, group = NA_character_,...) {
  vec_cast(x, new_sgrp(group = group))
}

#' @export
as_sgrp.character <- function(x, group = NA_character_) {
  value <- as.integer(x)
  new_sgrp(value, group = group)
}
