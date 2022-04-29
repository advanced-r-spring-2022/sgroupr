# Construction function for inner use only
new_sgrp <- function(data = integer(), group = character()) {
  vctrs::vec_assert(data,integer())
  vctrs::vec_assert(group, character(), size = 1)
  if(setequal(group,"Klein_4")) {
    data[data > 3L | data < 0L] <- NA_integer_
  }
  vctrs::new_vctr(data, group = group , class = "sgrp")
}

# Help function
sgrp <- function(x = integer(), group = NA_character_) {
  x <- vctrs::vec_cast(x, integer())
  group <- vctrs::vec_cast(group, character())
  vctrs::new_sgrp(x, group = group)
}

# Test function
is_sgrp <- function(x) {
  inherits(x, "sgrp")
}

format.sgrp <- function(x, ...) {
  out <- formatC(signif(vec_data(x),3))
  # Makes nicer to print NA instead of "NA"
  out[is.na(x)] <- NA
  cat(attr(x,"group"), "\n")
  out
}

# coercion
vec_ptype2.sgrp.sgrp<- function(x, y, ...) {
  new_sgrp(group = group(y))
}
vec_ptype2.sgrp.integer <- function(x, y, ...) y
vec_ptype2.integer.sgrp <- function(x, y, ...) x

# cast
vec_cast.sgrp.sgrp <- function(x,to, ...) {
  new_sgrp(vec_data(x), group = group(to))
}
vec_cast.sgrp.integer <- function(x, to, ...) {
  new_sgrp(x, group = group(to))
}
vec_cast.integer.sgrp <- function(x, to, ...)  vec_data(x)

# other cast function
as_sgrp <- function(x,...) {
  UseMethod("as_sgrp")
}

as_sgrp.default <- function(x, group = NA_character_,...) {
  vec_cast(x, new_sgrp(group = group))
}

as_sgrp.character <- function(x, group = NA_character_) {
  value <- as.integer(x)
  new_sgrp(value, group = group)
}

# arithmetic
vec_arith.sgrp <- function(op, x, y, ...) {
  UseMethod("vec_arith.sgrp", y)
}
vec_arith.sgrp.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}


