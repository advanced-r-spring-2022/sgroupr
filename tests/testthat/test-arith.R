test_that("1 + Numeric", {
  x <- Klein_4(0:3)
  table <- table(x)
  test <- table[,2]
  names(test) <- NULL
  expect_equal(as.integer(1L + x),c(test))
})
