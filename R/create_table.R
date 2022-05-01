#' @export
create_table <- function(x) {
  #stopifnot(is_sgrp(x))
  table <- matrix()
  switch(
    group(x),
    "Cyclic_3" <-  {
      table <- matrix(c(0L, 1L, 2L,
                       1L, 0L, 2L,
                       2L, 1L, 0L),
                     nrow = 3, ncol = 3)
      rownames(table) <- 0:2
      colnames(table) <- 0:2
    },
    "Cyclic_6" = {
      table <- matrix(c(0L, 1L, 2L, 3L, 4L, 5L,
                        1L, 2L, 3L, 4L, 5L, 0L,
                        2L, 3L, 4L, 5L, 0L, 1L,
                        3L, 4L, 5L, 0L, 1L, 2L,
                        4L, 5L, 0L, 1L, 2L, 3L,
                        5L, 0L, 1L, 2L, 3L, 4L),
                      nrow = 6, ncol = 6)
      rownames(table) <- 0:5
      colnames(table) <- 0:5
    },
    "Cyclic_8" = {
      table <- matrix(c(0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L,
                        1L, 2L, 3L, 4L, 5L, 6L, 7L, 0L,
                        2L, 3L, 4L, 5L, 6L, 7L, 0L, 1L,
                        3L, 4L, 5L, 6L, 7L, 0L, 1L, 2L,
                        4L, 5L, 6L, 7L, 0L, 1L, 2L, 3L,
                        5L, 6L, 7L, 0L, 1L, 2L, 3L, 4L,
                        6L, 7L, 0L, 1L, 2L, 3L, 4L, 5L,
                        7L, 0L, 1L, 2L, 3L, 4L, 5L, 6L),
                      nrow = 8, ncol = 8)
      rownames(table) <- 0:7
      colnames(table) <- 0:7
    },
    "Cyclic_16" = {
      table <- matrix(c(0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L,
                        1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 0L,
                        2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 0L, 1L,
                        3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 0L, 1L, 2L,
                        4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 0L, 1L, 2L, 3L,
                        5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 0L, 1L, 2L, 3L, 4L,
                        6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 0L, 1L, 2L, 3L, 4L, 5L,
                        7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 0L, 1L, 2L, 3L, 4L, 5L, 6L,
                        8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L,
                        9L, 10L, 11L, 12L, 13L, 14L, 15L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L,
                        10L, 11L, 12L, 13L, 14L, 15L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L,
                        11L, 12L, 13L, 14L, 15L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L,
                        12L, 13L, 14L, 15L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L,
                        13L, 14L, 15L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L,
                        14L, 15L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L,
                        15L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L),
                      nrow = 16, ncol = 16)
      rownames(table) <- 0:15
      colnames(table) <- 0:15
    },
    "Dih_8" = {
      table <- matrix(c(0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L,
                        1L, 0L, 3L, 2L, 5L, 4L, 7L, 6L,
                        2L, 3L, 0L, 1L, 7L, 6L, 5L, 4L,
                        3L, 2L, 1L, 0L, 6L, 7L, 4L, 5L,
                        4L, 5L, 6L, 7L, 0L, 1L, 2L, 3L,
                        5L, 4L, 7L, 6L, 1L, 0L, 3L, 2L,
                        6L, 7L, 4L, 5L, 3L, 2L, 1L, 0L,
                        7L, 6L, 5L, 4L, 2L, 3L, 0L, 1L),
                      nrow = 8, ncol = 8)
      rownames(table) <- 0:7
      colnames(table) <- 0:7
    },
    "Group_C4_C3" = {
      table <- matrix(c(0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L,
                        1L, 2L, 3L, 0L, 5L, 6L, 7L, 4L, 9L, 10L, 11L, 8L,
                        2L, 3L, 0L, 1L, 6L, 7L, 4L, 5L, 10L, 11L, 8L, 9L,
                        3L, 0L, 1L, 2L, 7L, 4L, 5L, 6L, 11L, 8L, 9L, 10L,
                        4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 0L, 1L, 2L, 3L,
                        5L, 6L, 7L, 4L, 9L, 10L, 11L, 8L, 1L, 2L, 3L, 0L,
                        6L, 7L, 4L, 5L, 10L, 11L, 8L, 9L, 2L, 3L, 0L, 1L,
                        7L, 4L, 5L, 6L, 11L, 8L, 9L, 10L, 3L, 0L, 1L, 2L,
                        8L, 9L, 10L, 11L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L,
                        9L, 10L, 11L, 8L, 1L, 2L, 3L, 0L, 5L, 6L, 7L, 4L,
                        10L, 11L, 8L, 9L, 2L, 3L, 0L, 1L, 6L, 7L, 4L, 5L,
                        11L, 8L, 9L, 10L, 3L, 0L, 1L, 2L, 7L, 4L, 5L, 6L),
                      nrow = 12, ncol = 12)
      rownames(table) <- 0:11
      colnames(table) <- 0:11
    }
  )
  return(table)
}
