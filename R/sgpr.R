
#' @export
new_sgrp <- function(data = integer(), group = character()) {
  vctrs::vec_assert(data,integer())
  vctrs::vec_assert(group, character(), size = 1)
  if(setequal(group,"Cyclic_3")) {
    data[data > 2L | data < 0L] <- NA_integer_
  } else if(setequal(group,"Cyclic_6")) {
    data[data > 5L | data < 0L] <- NA_integer_
  } else if(setequal(group,"Cyclic_8")) {
    data[data > 7L | data < 0L] <- NA_integer_
  } else if(setequal(group,"Cyclic_16")) {
    data[data > 15L | data < 0L] <- NA_integer_
  } else if(setequal(group,"Dih_8")) {
    data[data > 8L | data < 0L] <- NA_integer_
  } else if(setequal(group,"Group_C4_C3")) {
    data[data > 11L | data < 0L] <- NA_integer_
  }
  vctrs::new_vctr(data, group = group , class = "sgroupr_sgrp")
}

#' @export
sgrp <- function(x = integer(), group = NA_character_) {
  # make sure the input data is integral.
  # vec_cast can change the x into the type integer
  x <- vctrs::vec_cast(x, integer())
  group <- vctrs::vec_cast(group, character())
  new_sgrp(x, group = group)
}

#' @export
is_sgrp <- function(x) {
  inherits(x,"sgroupr_sgrp")
}

#' @export
group <- function(x) {
  attr(x,"group")
}



































