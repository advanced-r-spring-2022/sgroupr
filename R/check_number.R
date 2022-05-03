#' @title
#' Check the number of specific group
#' @param x
#' An integer vector
#' @param group
#' The group we are going to create
#'
#' @return
#' A vector that change
#' @export
#' @examples
#' check_number(0:7, group = "S3")
check_number <- function(x,group) {
  stopifnot(is.character(group))
  stopifnot(length(group) == 1)
  if(group %in% c('Dih12',"C12","K4_C3","Group_C4_C3","C6C2")) {
    x[x > 11L | x < 0L] <- NA_integer_
  } else if(group %in% c("Dih_8","Dih8_square","Cyclic_8","C4C2",
                         "Klein_8","Dcyclic_8")) {
    x[x > 7L | x < 0L] <- NA_integer_
  } else if(group %in% c("C4","Klein_4","Cyclic_4")) {
    x[x > 3L | x < 0L] <- NA_integer_
  } else if(group %in% c("S3","Cyclic_6","C3C2")) {
    x[x > 5L | x < 0L] <- NA_integer_
  } else if(setequal(group,c("Cyclic_16","C4_C4"))) {
    x[x > 15L | x< 0L] <- NA_integer_
  } else if(setequal(group,"Cyclic_3")) {
    x[x > 2L | x< 0L] <- NA_integer_
  }

  return(x)
}






