test_that("No Available", {
  expect_equal(check_group("dfa"),FALSE)
  expect_error(sgrp(0:5,"dfs"))
})
