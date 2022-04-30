group <- function(x) {
  stopifnot(is_sgrp(x))
  attr(x,"group")
}
