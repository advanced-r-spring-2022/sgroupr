new_sgrp <- function(x = integer(), group = character()) {
  vctrs::vec_assert(x,integer())
  vctrs::vec_assert(group, character(), size = 1)
  if(setequal(group,"Klein_4")) {
    data[data > 3L | data < 0L] <- NA_integer_
  }
  vctrs::new_vctr(x, group = group , class = "sgroupr_sgrp")
}

# Helper function for sgpr
#' Genealized small group
#'
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
#' @export
#' @rdname sgrp
is_sgrp <- function(x) {
  inherits(x, "sgroupr_sgrp")
}

# Format
#' @export
format.sgroupr_sgrp <- function(x, ...) {
  out <- formatC(signif(vctrs::vec_data(x),3))
  # Makes nicer to print NA instead of "NA"
  out[is.na(x)] <- NA
  cat(attr(x,"group"), "\n")
  out
}

# for compatibility with the S4 system
#' @export
methods::setOldClass(c("sgroupr_sgrp", "vctrs_vctr"))

