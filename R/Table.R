#' @title Cayley Table
#' 
#' @param x 
#' An sgroupr_sgrp object
#' @return 
#' A matrix for arithmetic operation
#' 
#' @example 
#' x <- Cyclic_4(0:3)
#' table(x)
#' @export
table <- function(x) {
  stopifnot(is_sgrp(x))
  table <- matrix()
  switch(
    group(x),
    "Cyclic_4" <- {
      table = matrix(c(0L, 1L, 2L, 3L,
                       1L, 2L, 3L, 0L,
                       2L, 3L, 0L, 1L,
                       3L, 0L, 1L, 2L),
                     nrow = 4, ncol = 4)
      rownames(Cl4_cayley) <- 0:3
      colnames(Cl4_cayley) <- 0:3
    }
    "Dic_8" <- {
      table = matrix(c(0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L,
                       1L, 2L, 3L, 0L, 5L, 6L, 7L, 4L,
                       2L, 3L, 0L, 1L, 6L, 7L, 4L, 5L,
                       3L, 0L, 1L, 2L, 7L, 4L, 5L, 6L,
                       4L, 7L, 6L, 5L, 2L, 1L, 0L, 3L,
                       5L, 4L, 7L, 6L, 3L, 2L, 1L, 0L,
                       6L, 5L, 4L, 7L, 0L, 3L, 2L, 1L,
                       7L, 6L, 5L, 4L, 1L, 0L, 3L, 2L),
                     nrow = 8, ncol = 8)
      rownames(Dic8_cayley) <- 0:7
      colnames(Dic8_cayley) <- 0:7
    }
    "C4C2" <- {
      table = matrix(c(0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L,
                       1L, 2L, 3L, 0L, 5L, 6L, 7L, 4L,
                       2L, 3L, 0L, 1L, 6L, 7L, 4L, 5L,
                       3L, 0L, 1L, 2L, 7L, 4L, 5L, 6L,
                       4L, 5L, 6L, 7L, 0L, 1L, 2L, 3L,
                       5L, 6L, 7L, 4L, 1L, 2L, 3L, 0L,
                       6L, 7L, 4L, 5L, 2L, 3L, 0L, 1L,
                       7L, 4L, 5L, 6L, 3L, 2L, 1L, 0L),
                     nrow = 8, ncol = 8)
      rownames(C4C2_cayley) <- 0:7
      colnames(C4C2_cayley) <- 0:7
    }
    "C3C2" <- {
      table = matrix(c(0L, 1L, 2L, 3L, 4L, 5L,
                       1L, 2L, 0L, 4L, 5L, 3L,
                       2L, 0L, 1L, 5L, 3L, 4L,
                       3L, 4L, 5L, 0L, 1L, 2L,
                       4L, 5L, 3L, 1L, 2L, 0L,
                       5L, 3L, 4L, 2L, 0L, 1L),
                     nrow = 6, ncol = 6)
      rownames(C3C2_cayley) <- 0:5
      colnames(C3C2_cayley) <- 0:5
    }
    "C6C2" <- {
      table = matrix(c(0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L,
                       1L, 2L, 3L, 4L, 5L, 0L, 7L, 8L, 9L, 10L, 11L, 6L,
                       2L, 3L, 4L, 5L, 0L, 1L, 8L, 9L, 10L, 11L, 6L, 7L, 
                       3L, 4L, 5L, 0L, 1L, 2L, 9L, 10L, 11L, 6L, 7L, 8L,
                       4L, 5L, 0L, 1L, 2L, 3L, 10L, 11L, 6L, 7L, 8L, 9L,
                       5L, 0L, 1L, 2L, 3L, 4L, 11L, 6L, 7L, 8L, 9L, 10L,
                       6L, 7L, 8L, 9L, 10L, 11L, 0L, 1L, 2L, 3L, 4L, 5L,
                       7L, 8L, 9L, 10L, 11L, 6L, 1L, 2L, 3L, 4L, 5L, 0L,
                       8L, 9L, 10L, 11L, 6L, 7L, 2L, 3L, 4L, 5L, 0L, 1L,
                       9L, 10L, 11L, 6L, 7L, 8L, 3L, 4L, 5L, 0L, 1L, 2L,
                       10L, 11L, 6L, 7L, 8L, 9L, 4L, 5L, 0L, 1L, 2L, 3L,
                       11L, 6L, 7L, 8L, 9L, 10L, 5L, 0L, 1L, 2L, 3L, 4L),
                     nrow = 12, ncol = 12)
      rownames(C6C2_cayley) <- 0:10
      colnames(C6C2_cayley) <- 0:10
    }
  )
  return(table)
}