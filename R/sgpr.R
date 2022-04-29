sgrp <- function(x = integer(), group = NA_character_) {
  # make sure the input data is integral. 
  # vec_cast can change the x into the type integer
  x <- vec_cast(x, integer())
  group <- vec_cast(group, character())
  new_sgrp(x, group = group)
}