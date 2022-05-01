new_sgrp <- function(data = integer(), group = character()) {
  vctrs::vec_assert(data,integer())
  vctrs::vec_assert(group, character(), size = 1)
  if(setequal(group,"C3C2")) {
    data[data > 5L | data < 0L] <- NA_integer_
  } else if(setequal(group,"C6C2")) {
    data[data > 11L | data < 0L] <- NA_integer_
  } else if(setequal(group,"C4C2")) {
    data[data > 7L | data < 0L] <- NA_integer_
  } else if(setequal(group,"Cyclic_4")) {
    data[data > 3L | data < 0L] <- NA_integer_
  } else if(setequal(group,"Klein_8")) {
    data[data > 7L | data < 0L] <- NA_integer_
  } else if(setequal(group,"Dcyclic_8")) {
    data[data > 7L | data < 0L] <- NA_integer_
  }
  vctrs::new_vctr(data, group = group , class = "sgroupr_sgrp")
}

# Helper function for sgpr
#' @title
#' Generalized small group
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
  # make sure the input data is integral.
  # vec_cast can change the x into the type integer
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
#'
#' @examples
#' x <- sgrp(c(1,2,3,4))
#' is_sgrp(x)
is_sgrp <- function(x) {
  inherits(x,"sgroupr_sgrp")
}

#' @title
#' Extract the group attribute of a sgroupr_sgrp
#'
#' @param x is an sgroupr_sgrp object
#' @export
#' @examples
#' x <- Cyclic_3(c(0,1,2))
#' group(x)
group <- function(x) {
  attr(x,"group")
}