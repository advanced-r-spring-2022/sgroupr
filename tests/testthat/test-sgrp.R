test_that("check an object is a sgroupr_sgrp", {
  expect_equal(is_sgrp(sgrp(0:3)), TRUE)
  })

test_that("sgrp", {
  expect_error(sgrp("23"))
  })

test_that("check sgroupr_sgrp", {
  x <- Klein_4(0:3)
  expect_s3_class(x,"sgroupr_sgrp")
})
