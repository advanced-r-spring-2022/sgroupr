#' @title
#' The arithmetic methods of small groups

#' @description
#' @param op operation such as `+`
#' @param x an sgrp
#' * For `vec_ariths.sgroupr_sgrp.sgroupr_sgrp`
#' * For `vec_arith.sgroupr_sgrp.numeric`
#' @param y another sgrp
#' @param ... Other params
#' @return a result
#' @export
vec_arith.sgroupr_sgrp <- function(op, x, y, ...) {
  UseMethod("vec_arith.sgroupr_sgrp", y)
}

#' @rdname vec_arith.sgroupr_sgrp

#' @export
vec_arith.sgroupr_sgrp.default <- function(op, x, y, ...) {
  vctrs::stop_incompatible_op(op, x, y)
}

#' @rdname vec_arith.sgroupr_sgrp
#' @export
vec_arith.sgroupr_sgrp.sgroupr_sgrp <- function(op, x, y, ...) {
  stopifnot(group(x) == group(y))
  # create cayley table
  table <- table(x)
  return_index <- as.integer(1L + vctrs::vec_cast(y,integer()) %% nrow(table))
  data <- table[cbind(as.integer(x) + 1L, return_index)]
  switch(
    op,
    "+" = new_sgrp(data, group = group(x)),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @rdname vec_arith.sgroupr_sgrp
#' @export
vec_arith.sgroupr_sgrp.numeric <- function(op, x, y, ...) {
  y <- as.integer(y)
  table <- table(x)
  return_index <- 1L + as.integer(y %% nrow(table))
  data <- table[cbind(as.integer(x) + 1L, return_index)]
  switch(
    op,
    "+" = new_sgrp(data, group = group(x)),
    vctrs::stop_incompatible_op(op, x, y)
    stop_incompatible_op(op, x, y)

  )
}

#' @rdname vec_arith.sgroupr_sgrp
#' @export
vec_arith.numeric.sgroupr_sgrp <- function(op, x, y, ...) {
  x <- as.integer(x)
  table <- table(y)
  return_index <- 1L + as.integer(x %% nrow(table))
  data <- table[cbind(as.integer(y) + 1L, return_index)]
  switch(
    op,
    "+" = new_sgrp(data, group = group(y)),
    stop_incompatible_op(op, x, y)
  )
}



