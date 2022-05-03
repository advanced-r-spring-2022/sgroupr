test_that("sgrp_sgrp", {
  x <- Cyclic_3(0:2)
  expect_equal(vctrs::vec_c(x,x), Cyclic_3(c(0:2,0:2)))
})

test_that("integer_sgrp", {
  x <- Cyclic_3(0:2)
  expect_equal(vctrs::vec_c(1L,x), Cyclic_3(c(1L,0:2)))
})

test_that("abbr_sgrp", {
  x <- Cyclic_3(0:2)
  expect_equal(vctrs::vec_ptype_abbr(x),"sgrp" )
})
