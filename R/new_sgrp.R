new_sgrp <- function(x = integer(), group = character()) {
  vctrs::vec_assert(x,integer())
  vctrs::vec_assert(group, character(), size = 1)
  if(setequal(group,"Klein_4")) {
    data[data > 3L | data < 0L] <- NA_integer_
  }
  vctrs::new_vctr(x, group = group , class = "sgroupr_sgrp")
}
