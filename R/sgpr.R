# Helper function for sgpr
#' Genealized small group
#'
#' @param x
#' * For `is_sgrp()`: An object to test.
#' * For `sgrp()`: A vector
#' @param group
#'
#' @return An S3 vector of class `sgroupr_sgrp`.
#' @export
#'
#' @examples
#' sgrp(c(1,2,3,4))
#'
sgrp <- function(x = integer(), group = NA_character_) {
  x <- vctrs::vec_cast(x, integer())
  group <- vctrs::vec_cast(group, character())
  new_sgrp(x, group = group)
}

# Test function
#' @export
#' @rdname sgrp
is_sgrp <- function(x) {
  inherits(x, "sgroupr_sgrp")
}

#' @export
format.sgroupr_sgrp <- function(x, ...) {
  out <- formatC(signif(vctrs::vec_data(x),3))
  # Makes nicer to print NA instead of "NA"
  out[is.na(x)] <- NA
  cat(attr(x,"group"), "\n")
  out
}

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

# cast

#' @export
vec_cast.sgroupr_sgrp.sgroupr_sgrp <- function(x,to, ...) {
  new_sgrp(vctrs::vec_data(x), group = group(to))
}

#' @export
vec_cast.sgroupr_sgrp.integer <- function(x, to, ...) {
  new_sgrp(x, group = group(to))
}

#' @export
vec_cast.integer.sgroupr_sgrp <- function(x, to, ...)  vctrs::vec_data(x)

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

# arithmetic
#' @export
vec_arith.sgroupr_sgrp <- function(op, x, y, ...) {
  UseMethod("vec_arith.sgrp", y)
}

#' @export
vec_arith.sgroupr_sgrp.default <- function(op, x, y, ...) {
  vctrs::stop_incompatible_op(op, x, y)
}

# Add addition arthimatic methos here.


# for compatibility with the S4 system
#' @export
methods::setOldClass(c("sgroupr_sgrp", "vctrs_vctr"))

