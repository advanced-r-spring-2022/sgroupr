#' @title
#' The arithmetic methods of small groups
#' @description
#' @param op operation such as `+`
#' @param x an sgrp
#' @param y another sgrp
#' @param ... Other params
#' @return a result
#' @export
vec_arith.sgroupr_sgrp <- function(op, x, y, ...) {
  UseMethod("vec_arith.sgroupr_sgrp", y)
}

#' @export
vec_arith.sgroupr_sgrp.default <- function(op, x, y, ...) {
  vctrs::stop_incompatible_op(op, x, y)
}

#' @export
vec_arith.sgroupr_sgrp.sgroupr_sgrp <- function(op, x, y, ...) {
  stopifnot(group(x) == group(y))
  # create cayley table
  table <- table(x)
  return_index <- as.integer(1L + vec_cast(y,integer()) %% nrow(table))
  x <- table[cbind(as.integer(x) + 1L, return_index)]
  switch(
    op,
    "+" = new_sgrp(x,group = group(x)),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
vec_arith.numeric.sgroupr_sgrp <- function(op, x, y, ...) {
  # if the x is float, we will discard the decimal, and convert it into the integer.
  x <- as.integer(x)
  # For each group we have special rule of adding a number.
  table <- table(y)
  # initialize the return index as 0 and  we return original if we do nothing
  # the cycle is 4
  return_index <- 1L + as.integer(x %% nrow(table))
  data <- table[cbind(as.integer(y) + 1L, return_index)]
  switch(
    op,
    "+" = new_sgrp(data, group = group(y)),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
vec_arith.sgroupr_sgrp.numeric <- function(op, x, y, ...) {
  y <- as.integer(y)
  # For each group we have special rule of adding a number.
  table <- table(x)

  return_index <- 1L + as.integer(y %% nrow(table))
  data <- table[cbind(as.integer(x) + 1L, return_index)]
  switch(
    op,
    "+" = new_sgrp(data, group = group(x)),
    stop_incompatible_op(op, x, y)
  )
}

