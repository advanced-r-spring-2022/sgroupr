test_that("1 + sgrp", {
  x <- Klein_4(0:3)
  table <- table(x)
  test <- table[,2]
  names(test) <- NULL
  expect_equal(as.integer(1L + x),c(test))
  expect_error(as.integer(1L - x))
})

test_that("sgrp + 1", {
  x <- Klein_4(0:3)
  table <- table(x)
  test <- table[,2]
  names(test) <- NULL
  expect_equal(as.integer(x + 1),c(test))
  expect_error(x * 1)
})

test_that("sgrp + sgrp", {
  x <- Klein_4(0:3)
  y <- Klein_4(1)
  z <- Cyclic_4(1)
  table <- table(x)
  test <- table[,2]
  names(test) <- NULL
  expect_equal(as.integer(x + y),c(test))
  expect_error(as.integer(x * z))
  expect_error(as.integer(x * y))
})

test_that("sgrp + MISSING", {
  x <- Klein_4(0:3)
  expect_equal((+ x),x)
  expect_error(-x)
})

test_that("default",{
  x <- Klein_4(0:3)
  expect_error(vec_arith.sgroupr_sgrp.default("+",x,1))
})


