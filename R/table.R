table <- function(x) {
  stopifnot(is_sgrp(x))
  table <- matrix()
  switch(
    group(x),
    "S3" <-  {
      table = rbind(c(0L,1L,2L,3L,4L,5L),
                     c(1L,2L,0L,4L,5L,3L),
                     c(2L,0L,1L,5L,3L,4L),
                     c(3L,5L,4L,0L,2L,1L),
                     c(4L,3L,5L,1L,0L,2L),
                     c(5L,4L,3L,2L,1L,0L))
    rownames(table) <- 0:5L
    colnames(table) <- 0:5L
    },
    "Klein_4" = {
      table <- matrix(c(0L, 1L, 2L, 3L,
                        1L, 0L, 3L, 2L,
                        2L, 3L, 0L, 1L,
                        3L, 2L, 1L, 0L),
             nrow = 4, ncol = 4)
      rownames(table) <- 0:3
      colnames(table) <- 0:3
    },
    "C4_b" = {
      rbind(c(0, 1, 2, 3),
            c(1, 2, 3, 0),
            c( -1L,0L-i, 1L, 0L+1i),
            c(0L-i, 1L, 0L+1i, -1L))
    },
    "Dih8_square" = {
      table <-
      rownames(table) <- 0:7
      colnames(table) <- 0:7
    },
    "Dic8_b" = {

    },
    "C12" = {
      table <- matrix(c(0L,1L,2L,3L,4L,5L,6L,7L,8L,9L,10L,11L,
                        1L,2L,3L,4L,5L,6L,7L,8L,9L,10L,11L,0L,
                        2L,3L,4L,5L,6L,7L,8L,9L,10L,11L,0L,1L,
                        3L,4L,5L,6L,7L,8L,9L,10L,11L,0L,1L,2L,
                        4L,5L,6L,7L,8L,9L,10L,11L,0L,1L,2L,3L,
                        5L,6L,7L,8L,9L,10L,11L,0L,1L,2L,3L,4L,
                        6L,7L,8L,9L,10L,11L,0L,1L,2L,3L,4L,5L,
                        7L,8L,9L,10L,11L,0L,1L,2L,3L,4L,5L,6L,
                        8L,9L,10L,11L,0L,1L,2L,3L,4L,5L,6L,7L,
                        9L,10L,11L,0L,1L,2L,3L,4L,5L,6L,7L,8L,
                        10L,11L,0L,1L,2L,3L,4L,5L,6L,7L,8L,9L,
                        11L,0L,1L,2L,3L,4L,5L,6L,7L,8L,9L,10L),
                      nrow = 12, ncol = 12)
      rownames(table) <- 0:11
      colnames(table) <- 0:11
    },
    "K4:C3" = {
      matrix(c(0L,1L,2L,3L,4L,5L,6L,7L,8L,9L,10L,11L,
               1L,0L,3L,1L,2L,))
    }
  )
  return(table)
}
