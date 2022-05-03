#' @import vctrs
#' @title
#' The arithmetic methods of small groups
#' @description
#' This defines how each arithmetic functions.
#' @param op operation such as `+`
#' @param x an sgrp
#' * For `vec_ariths.sgroupr_sgrp.sgroupr_sgrp`
#' * For `vec_arith.sgroupr_sgrp.numeric`
#' @param y another sgrp
#' @param ... Other params
#' @return a result
#' @export
#' @method vec_arith sgroupr_sgrp
vec_arith.sgroupr_sgrp <- function(op, x, y, ...) {
  UseMethod("vec_arith.sgroupr_sgrp", y)
}

#' @export
#' @method vec_arith.sgroupr_sgrp default
vec_arith.sgroupr_sgrp.default <- function(op, x, y, ...) {
  vctrs::stop_incompatible_op(op, x, y)
  }


#' @export
#' @method vec_arith.sgroupr_sgrp MISSING
vec_arith.sgroupr_sgrp.MISSING <- function(op, x, y, ...) {
  switch (
    op,
    "+" = x,
    vctrs::stop_incompatible_op()
  )
}


#' @export
#' @method vec_arith.sgroupr_sgrp sgroupr_sgrp
vec_arith.sgroupr_sgrp.sgroupr_sgrp <- function(op, x, y, ...) {
  stopifnot(group(x) == group(y))
  # create cayley table
  table <- table(x)
  return_index <- as.integer(1L + vctrs::vec_cast(y, integer()) %% nrow(table))
  data <- table[cbind(as.integer(x) + 1L, return_index)]
  switch(
    op,
    "+" = new_sgrp(data, group = group(x)),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.sgroupr_sgrp numeric
vec_arith.sgroupr_sgrp.numeric <- function(op, x, y, ...) {
  y <- as.integer(y)
  table <- table(x)
  return_index <- 1L + as.integer(y %% nrow(table))
  data <- table[cbind(as.integer(x) + 1L, return_index)]
  switch(
    op,
    "+" = new_sgrp(data, group = group(x)),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.numeric sgroupr_sgrp
vec_arith.numeric.sgroupr_sgrp <- function(op, x, y, ...) {
  x <- as.integer(x)
  table <- table(y)
  return_index <- 1L + as.integer(x %% nrow(table))
  data <- table[cbind(as.integer(y) + 1L, return_index)]
  switch(
    op,
    "+" = new_sgrp(data, group = group(y)),
    vctrs::stop_incompatible_op(op, x, y)
  )
}
