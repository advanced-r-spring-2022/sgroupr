#' @export
vec_ptype_abbr.sgroupr_sgrp <- function(x, ...) {
  "sgrp"
}

# coercion
#' @export
vec_ptype2.sgroupr_sgrp.sgroupr_sgrp<- function(x, y, ...) {
  new_sgrp(group = group(y))
}

#' @export
vec_ptype2.sgroupr_sgrp.integer <- function(x, y, ...) new_sgrp(group = group(x))

#' @export
vec_ptype2.integer.sgroupr_sgrp <- function(x, y, ...) new_sgrp(group = group(y))