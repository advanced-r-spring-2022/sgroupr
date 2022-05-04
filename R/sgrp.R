new_sgrp <- function(x = integer(), group = character()) {
  vctrs::vec_assert(x, integer())
  vctrs::vec_assert(group, character(), size = 1)
  stopifnot(check_group(group))
  x <- check_number(x, group)
  vctrs::new_vctr(x, group = group, class = "sgroupr_sgrp")
}

# Helper function for sgpr

#' @title  Genealized small group
#' @param x
#' * For `is_sgrp()`: An object to test.
#' * For `sgrp()`: A vector
#' @param group
#' a chacter which shows which small group x is.
#'
#' @return An S3 vector of class `sgroupr_sgrp`.
#' @export
#'
#' @examples
#' sgrp(c(1,2,3,4))
sgrp <- function(x = integer(), group = NA_character_) {
  x <- vctrs::vec_cast(x, integer())
  group <- vctrs::vec_cast(group, character())
  new_sgrp(x, group = group)
}

# Test function


#' @title
#' sgrp group Check
#' @param x is an sgroupr_sgrp object
#' @return A boolean value show whether or not the object is sgroupr_sgrp group.
#' @export
#' @rdname sgrp
is_sgrp <- function(x) {
  inherits(x, "sgroupr_sgrp")
}

# Format
#' @export
format.sgroupr_sgrp <- function(x, ...) {
  out <- formatC(signif(vctrs::vec_data(x), 3))
  # Makes nicer to print NA instead of "NA"
  out[is.na(x)] <- NA
  cat(attr(x, "group"), "\n")
  out
}

# for compatibility with the S4 system
#' @export
methods::setOldClass(c("sgroupr_sgrp", "vctrs_vctr"))

#' @title
#' Chang a vector into sgroupr_sgrp object
#' @param x
#' A sgroupr_sgrp object
#' @param group the group you want to create
#' @param ... other params
#' other params
#' @export
#' @examples
#' as_sgrp(c(1L,2L,3L,4L),group = "Dih_6")
as_sgrp <- function(x, group = NA_character_, ...) {
  UseMethod("as_sgrp")
}

#' @export
as_sgrp.default <- function(x, group = NA_character_, ...) {
  vec_cast(x, new_sgrp(group = group))
}

#' @export
as_sgrp.character <- function(x, group = NA_character_, ...) {
  value <- as.integer(x)
  new_sgrp(value, group = group)
}

## just to get rid of a cran note
dummy <- function() {
  methods::is(TRUE, "logical")
}
