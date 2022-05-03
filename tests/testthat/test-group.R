test_that("C3_C2", {
 x <- C3_C2(c(0:6))
 expect_equal(as.integer(x),c(0,1,2,3,4,5,NA))
 expect_equal(attr(x,"group"),"C3_C2")
 expect_equal(as_C3_C2(0:5), C3_C2(0:5))
 expect_equal(as_C3_C2(c('0','1','2','3','4','5')), C3_C2(0:5))
 expect_equal(is_C3_C2(x),TRUE)
 expect_equal(is_C3_C2(Klein_8(0:5)), FALSE)
 x <- C3_C2(c(0:5))
 expect_equal(vctrs::vec_c(x,1L),C3_C2(c(0,1,2,3,4,5,1)))
 expect_equal(vctrs::vec_cast(x,integer()),c(0L:5L))
 expect_equal(as.integer(1 + x),c(1,2,0,4,5,3))
})

test_that("C4_C3", {
  x <- C4_C3(c(0:12))
  expect_equal(as.integer(x),c(0:11,NA))
  expect_equal(attr(x,"group"),"C4_C3")
  expect_equal(as_C4_C3(0:5), C4_C3(0:5))
  expect_equal(as_C4_C3(c('0','1','2','3','4','5')), C4_C3(0:5))
  expect_equal(is_C4_C3(x),TRUE)
  expect_equal(is_C4_C3(Klein_8(0:12)), FALSE)
  expect_equal(vctrs::vec_c(x,1L), C4_C3(c(0:12,1)))
  expect_equal(vctrs::vec_cast(x,integer()),c(0L:11L,NA))
  expect_equal(as.integer(1L + x),c(1,2,3,0,5,6,7,4,9,10,11,8,NA))
})

test_that("C4_C2", {
  x <- C4_C2(c(0:8))
  expect_equal(as.integer(x),c(0:7,NA))
  expect_equal(attr(x,"group"),"C4_C2")
  expect_equal(as_C4_C2(0:5), C4_C2(0:5))
  expect_equal(as_C4_C2(c('0','1','2','3','4','5')), C4_C2(0:5))
  expect_equal(is_C4_C2(x),TRUE)
  expect_equal(is_C4_C2(Klein_8(0:12)), FALSE)
  expect_equal(vctrs::vec_c(x,1L), C4_C2(c(0:8,1)))
  expect_equal(vctrs::vec_cast(x,integer()),c(0L:7L,NA))
  table <- table(x)
  test <- table[,2]
  names(test) <- NULL
  expect_equal(as.integer(1L + x),c(test,NA))
})

test_that("C6_C2", {
  x <- C6_C2(c(0:12))
  expect_equal(as.integer(x),c(0:11,NA))
  expect_equal(attr(x,"group"),"C6_C2")
  expect_equal(as_C6_C2(0:5), C6_C2(0:5))
  expect_equal(as_C6_C2(c('0','1','2','3','4','5')), C6_C2(0:5))
  expect_equal(is_C6_C2(x),TRUE)
  expect_equal(is_C6_C2(Klein_8(0:12)), FALSE)
  expect_equal(vctrs::vec_c(x,1L), C6_C2(c(0:12,1)))
  expect_equal(vctrs::vec_cast(x,integer()),c(0L:11L,NA))
  table <- table(x)
  test <- table[,2]
  names(test) <- NULL
  expect_equal(as.integer(1L + x),c(test,NA))
})

test_that("Cyclic_3", {
  x <- Cyclic_3(c(0:3))
  expect_equal(as.integer(x),c(0:2,NA))
  expect_equal(attr(x,"group"),"Cyclic_3")
  expect_equal(as_Cyclic_3(0:2), Cyclic_3(0:2))
  expect_equal(as_Cyclic_3(c('0','1','2')), Cyclic_3(0:2))
  expect_equal(is_Cyclic_3(x),TRUE)
  expect_equal(is_Cyclic_3(Klein_8(0:12)), FALSE)
  expect_equal(vctrs::vec_c(x,1L), Cyclic_3(c(0:3,1)))
  expect_equal(vctrs::vec_cast(x,integer()),c(0L:2L,NA))
  table <- table(x)
  test <- table[,2]
  names(test) <- NULL
  expect_equal(as.integer(1L + x),c(test,NA))
})

test_that("Cyclic_4", {
  x <- Cyclic_4(c(0:4))
  expect_equal(as.integer(x),c(0:3,NA))
  expect_equal(attr(x,"group"),"Cyclic_4")
  expect_equal(as_Cyclic_4(0:2), Cyclic_4(0:2))
  expect_equal(as_Cyclic_4(c('0','1','2','3')), Cyclic_4(0:3))
  expect_equal(is_Cyclic_4(x),TRUE)
  expect_equal(is_Cyclic_4(Klein_8(0:12)), FALSE)
  expect_equal(vctrs::vec_c(x,1L), Cyclic_4(c(0:4,1)))
  expect_equal(vctrs::vec_cast(x,integer()),c(0L:3L,NA))
  table <- table(x)
  test <- table[,2]
  names(test) <- NULL
  expect_equal(as.integer(1L + x),c(test,NA))
})

test_that("Cyclic_6", {
  x <- Cyclic_6(c(0:6))
  expect_equal(as.integer(x),c(0:5,NA))
  expect_equal(attr(x,"group"),"Cyclic_6")
  expect_equal(as_Cyclic_6(0:2), Cyclic_6(0:2))
  expect_equal(as_Cyclic_6(c('0','1','2','3')), Cyclic_6(0:3))
  expect_equal(is_Cyclic_6(x),TRUE)
  expect_equal(is_Cyclic_6(Klein_8(0:12)), FALSE)
  expect_equal(vctrs::vec_c(x,1L), Cyclic_6(c(0:6,1)))
  expect_equal(vctrs::vec_cast(x,integer()),c(0L:5L,NA))
  table <- table(x)
  test <- table[,2]
  names(test) <- NULL
  expect_equal(as.integer(1L + x),c(test,NA))
})

test_that("Cyclic_8", {
  x <- Cyclic_8(c(0:8))
  expect_equal(as.integer(x),c(0:7,NA))
  expect_equal(attr(x,"group"),"Cyclic_8")
  expect_equal(as_Cyclic_8(0:2), Cyclic_8(0:2))
  expect_equal(as_Cyclic_8(c('0','1','2','3')), Cyclic_8(0:3))
  expect_equal(is_Cyclic_8(x),TRUE)
  expect_equal(is_Cyclic_8(Klein_8(0:12)), FALSE)
  expect_equal(vctrs::vec_c(x,1L), Cyclic_8(c(0:8,1)))
  expect_equal(vctrs::vec_cast(x,integer()),c(0L:7L,NA))
  table <- table(x)
  test <- table[,2]
  names(test) <- NULL
  expect_equal(as.integer(1L + x),c(test,NA))
})

test_that("Cyclic_12", {
  x <- Cyclic_12(c(0:12))
  expect_equal(as.integer(x),c(0:11,NA))
  expect_equal(attr(x,"group"),"Cyclic_12")
  expect_equal(as_Cyclic_12(0:2), Cyclic_12(0:2))
  expect_equal(as_Cyclic_12(c('0','1','2','3')), Cyclic_12(0:3))
  expect_equal(is_Cyclic_12(x),TRUE)
  expect_equal(is_Cyclic_12(Klein_8(0:12)), FALSE)
  expect_equal(vctrs::vec_c(x,1L), Cyclic_12(c(0:12,1)))
  expect_equal(vctrs::vec_cast(x,integer()),c(0L:11L,NA))
  table <- table(x)
  test <- table[,2]
  names(test) <- NULL
  expect_equal(as.integer(1L + x),c(test,NA))
})

test_that("Cyclic_16", {
  x <- Cyclic_16(c(0:16))
  expect_equal(as.integer(x),c(0:15,NA))
  expect_equal(attr(x,"group"),"Cyclic_16")
  expect_equal(as_Cyclic_16(0:2), Cyclic_16(0:2))
  expect_equal(as_Cyclic_16(c('0','1','2','3')), Cyclic_16(0:3))
  expect_equal(is_Cyclic_16(x),TRUE)
  expect_equal(is_Cyclic_16(Klein_8(0:11)), FALSE)
  expect_equal(vctrs::vec_c(x,1L), Cyclic_16(c(0:16,1)))
  expect_equal(vctrs::vec_cast(x,integer()),c(0L:15L,NA))
  table <- table(x)
  test <- table[,2]
  names(test) <- NULL
  expect_equal(as.integer(1L + x),c(test,NA))
})

test_that("Dcyclic_8", {
  x <- Dcyclic_8(c(0:8))
  expect_equal(as.integer(x),c(0:7,NA))
  expect_equal(attr(x,"group"),"Dcyclic_8")
  expect_equal(as_Dcyclic_8(0:2), Dcyclic_8(0:2))
  expect_equal(as_Dcyclic_8(c('0','1','2','3')), Dcyclic_8(0:3))
  expect_equal(is_Dcyclic_8(x),TRUE)
  expect_equal(is_Dcyclic_8(Klein_8(0:12)), FALSE)
  expect_equal(vctrs::vec_c(x,1L), Dcyclic_8(c(0:8,1)))
  expect_equal(vctrs::vec_cast(x,integer()),c(0L:7L,NA))
  table <- table(x)
  test <- table[,2]
  names(test) <- NULL
  expect_equal(as.integer(1L + x),c(test,NA))
})


test_that("Dih_6", {
  x <- Dih_6(c(0:6))
  expect_equal(as.integer(x),c(0:5,NA))
  expect_equal(attr(x,"group"),"Dih_6")
  expect_equal(as_Dih_6(0:2), Dih_6(0:2))
  expect_equal(as_Dih_6(c('0','1','2','3')), Dih_6(0:3))
  expect_equal(is_Dih_6(x),TRUE)
  expect_equal(is_Dih_6(Klein_8(0:12)), FALSE)
  expect_equal(vctrs::vec_c(x,1L), Dih_6(c(0:6,1)))
  expect_equal(vctrs::vec_cast(x,integer()),c(0L:5L,NA))
  table <- table(x)
  test <- table[,2]
  names(test) <- NULL
  expect_equal(as.integer(1L + x),c(test,NA))
})


test_that("Dih_8", {
  x <- Dih_8(c(0:8))
  expect_equal(as.integer(x),c(0:7,NA))
  expect_equal(attr(x,"group"),"Dih_8")
  expect_equal(as_Dih_8(0:2), Dih_8(0:2))
  expect_equal(as_Dih_8(c('0','1','2','3')), Dih_8(0:3))
  expect_equal(is_Dih_8(x),TRUE)
  expect_equal(is_Dih_8(Klein_8(0:12)), FALSE)
  expect_equal(vctrs::vec_c(x,1L), Dih_8(c(0:8,1)))
  expect_equal(vctrs::vec_cast(x,integer()),c(0L:7L,NA))
  table <- table(x)
  test <- table[,2]
  names(test) <- NULL
  expect_equal(as.integer(1L + x),c(test,NA))
})

test_that("Dih_8_square", {
  x <- Dih_8_square(c(0:8))
  expect_equal(as.integer(x),c(0:7,NA))
  expect_equal(attr(x,"group"),"Dih_8_square")
  expect_equal(as_Dih_8_square(0:2), Dih_8_square(0:2))
  expect_equal(as_Dih_8_square(c('0','1','2','3')), Dih_8_square(0:3))
  expect_equal(is_Dih_8_square(x),TRUE)
  expect_equal(is_Dih_8_square(Klein_8(0:12)), FALSE)
  expect_equal(vctrs::vec_c(x,1L), Dih_8_square(c(0:8,1)))
  expect_equal(vctrs::vec_cast(x,integer()),c(0L:7L,NA))
  table <- table(x)
  test <- table[,2]
  names(test) <- NULL
  expect_equal(as.integer(1L + x),c(test,NA))
})

test_that("Dih_12", {
  x <- Dih_12(c(0:12))
  expect_equal(as.integer(x),c(0:11,NA))
  expect_equal(attr(x,"group"),"Dih_12")
  expect_equal(as_Dih_12(0:2), Dih_12(0:2))
  expect_equal(as_Dih_12(c('0','1','2','3')), Dih_12(0:3))
  expect_equal(is_Dih_12(x),TRUE)
  expect_equal(is_Dih_12(Klein_8(0:12)), FALSE)
  expect_equal(vctrs::vec_c(x,1L), Dih_12(c(0:12,1)))
  expect_equal(vctrs::vec_cast(x,integer()),c(0L:11L,NA))
  table <- table(x)
  test <- table[,2]
  names(test) <- NULL
  expect_equal(as.integer(1L + x),c(test,NA))
})

test_that("K4_C3", {
  x <- K4_C3(c(0:12))
  expect_equal(as.integer(x),c(0:11,NA))
  expect_equal(attr(x,"group"),"K4_C3")
  expect_equal(as_K4_C3(0:2), K4_C3(0:2))
  expect_equal(as_K4_C3(c('0','1','2','3')), K4_C3(0:3))
  expect_equal(is_K4_C3(x),TRUE)
  expect_equal(is_K4_C3(Klein_8(0:12)), FALSE)
  expect_equal(vctrs::vec_c(x,1L), K4_C3(c(0:12,1)))
  expect_equal(vctrs::vec_cast(x,integer()),c(0L:11L,NA))
  table <- table(x)
  test <- table[,2]
  names(test) <- NULL
  expect_equal(as.integer(1L + x),c(test,NA))
})

test_that("Klein_8", {
  x <- Klein_8(c(0:8))
  expect_equal(as.integer(x),c(0:7,NA))
  expect_equal(attr(x,"group"),"Klein_8")
  expect_equal(as_Klein_8(0:2), Klein_8(0:2))
  expect_equal(as_Klein_8(c('0','1','2','3')), Klein_8(0:3))
  expect_equal(is_Klein_8(x),TRUE)
  expect_equal(is_Klein_8(Cyclic_12(0:12)), FALSE)
  expect_equal(vctrs::vec_c(x,1L), Klein_8(c(0:8,1)))
  expect_equal(vctrs::vec_cast(x,integer()),c(0L:7L,NA))
  table <- table(x)
  test <- table[,2]
  names(test) <- NULL
  expect_equal(as.integer(1L + x),c(test,NA))
})

test_that("Klein_4", {
  x <- Klein_4(c(0:4))
  expect_equal(as.integer(x),c(0:3,NA))
  expect_equal(attr(x,"group"),"Klein_4")
  expect_equal(as_Klein_4(0:2), Klein_4(0:2))
  expect_equal(as_Klein_4(c('0','1','2','3')), Klein_4(0:3))
  expect_equal(is_Klein_4(x),TRUE)
  expect_equal(is_Klein_4(Klein_8(0:12)), FALSE)
  expect_equal(vctrs::vec_c(x,1L), Klein_4(c(0:4,1)))
  expect_equal(vctrs::vec_cast(x,integer()),c(0L:3L,NA))
  table <- table(x)
  test <- table[,2]
  names(test) <- NULL
  expect_equal(as.integer(1L + x),c(test,NA))
})
