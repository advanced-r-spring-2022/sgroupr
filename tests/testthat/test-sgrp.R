test_that("check an object is a sgroupr_sgrp", {
  expect_equal(is_sgrp(sgrp(0:3)), TRUE)
  })

test_that("sgrp", {
  expect_error(sgrp("23"))
  })
