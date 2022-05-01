#' Cayley table for finite small group
#'
#' @param x
#' An sgroupr_sgrp object
#' @return A matrix for arithmetic operation
#' @export
#'
#' @examples
#' x <- S3(0:5)
#' table(x)
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
    "C4" = {
      rbind(c(0L,1L,2L,3L),
            c(1L,2L,3L,0L),
            c(2L,3L,0L,1L),
            c(3L,0L, 1L,2L))
    },
    "Dih8" = {
      table <- rbind(c(0L,1L,2L,3L,4L,5L,6L,7L),
                     c(1L,2L,3L,0L,5L,6L,7L,4L),
                     c(2L,3L,0L,1L,6L,7L,4L,5L),
                     c(3L,0L,1L,2L,7L,4L,5L,6L),
                     c(4L,7L,6L,5L,0L,3L,2L,1L),
                     c(5L,4L,7L,6L,1L,0L,3L,2L),
                     c(6L,5L,4L,7L,2L,1L,0L,3L),
                     c(7L,6L,5L,4L,3L,2L,1L,0L))
      rownames(table) <- 0:7
      colnames(table) <- 0:7
    },
    "Dih12" = {
      table <- matrix(c(0L,1L,2L,3L,4L,5L,6L,7L,8L,9L,10L,11L,
                        1L,2L,3L,4L,5L,0L,7L,8L,9L,10L,11L,6L,
                        2L,3L,4L,5L,0L,1L,8L,9L,10L,11L,6L,7L,
                        3L,4L,5L,0L,1L,2L,9L,10L,11L,6L,7L,8L,
                        4L,5L,0L,1L,2L,3L,10L,11L,6L,7L,8L,9L,
                        5L,0L,1L,2L,3L,4L,11L,6L,7L,8L,9L,10L,
                        6L,11L,10L,9L,8L,7L,0L,5L,4L,3L,2L,1L,
                        7L,6L,11L,10L,9L,8L,1L,0L,5L,4L,3L,2L,
                        8L,7L,6L,11L,10L,9L,2L,1L,0L,5L,4L,3L,
                        9L,8L,7L,6L,11L,10L,3L,2L,1L,0L,5L,4L,
                        10L,9L,8L,7L,6L,11L,4L,3L,2L,1L,0L,5L,
                        11L,10L,9L,8L,7L,6L,5L,4L,3L,2L,1L,0L),
                      nrow = 12, ncol = 12)
      rownames(table) <- 0:11
      colnames(table) <- 0:11
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
      table <- matrix(c(0L,1L,2L,3L,4L,5L,6L,7L,8L,9L,10L,11L,
                        1L,0L,3L,2L,5L,4L,7L,6L,9L,8L,11L,10L,
                        2L,3L,0L,1L,6L,7L,4L,5L,10L,11L,8L,9L,
                        3L,2L,1L,0L,7L,6L,5L,4L,11L,10L,9L,8L,
                        4L,5L,6L,7L,8L,9L,10L,11L,0L,1L,2L,3L,
                        5L,4L,7L,6L,9L,8L,11L,10L,1L,0L,3L,2L,
                        6L,7L,4L,5L,10L,11L,8L,9L,2L,3L,0L,1L,
                        7L,6L,5L,4L,11L,10L,9L,8L,3L,2L,1L,0L,
                        8L,9L,10L,11L,0L,1L,2L,3L,4L,5L,6L,7L,
                        9L,9L,11L,10L,1L,0L,3L,2L,5L,4L,7L,6L,
                        10L,11L,8L,9L,2L,3L,0L,1L,6L,7L,4L,5L,
                        11L,10L,9L,8L,3L,2L,1L,0L,7L,6L,5L,4L),
                      nrow = 12,ncol = 12)
      rownames(table) <- 0:11
      colnames(table) <- 0:11

    }
  )
  return(table)
}
