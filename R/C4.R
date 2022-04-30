new_Cyclic4 <- function(x = integer()) {
  # check whether the x is integer vector
  vctrs::vec_assert(x, integer())
  # For each Klein_4 group, the range should be 0:3
  x[x > 3L | x < 0L] <- NA_integer_
  # create it!
  new_sgrp(x, group = "C4")
}

# help function
C4 <- function(x = integer()) {
  # make sure the input data is integral.
  # vec_cast can change the x into the type integer
  x <- vctrs::vec_cast(x, integer())
  new_Cyclic4(x)
}
