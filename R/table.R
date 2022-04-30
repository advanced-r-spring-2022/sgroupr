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
      rownames(kl4_cayley) <- 0:3
      colnames(kl4_cayley) <- 0:3
    },
    "C4_b" = {
      matrix()

    }

  )
  return(table)
}
