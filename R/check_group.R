#' @title
#' Check if the group has been construct
#' @param group The group the small group is
#' @return True if the specified group is included inside our package and False 
#' if it has not.
#' @export
#' @examples
#' check_group("Klein_4")
check_group <- function(group) {
  if (group %in% c("Klein_4", "Klein_8", "Cyclic_3", "Cyclic_4",
                  "Cyclic_6", "Cyclic_8", "Cyclic_12", "Cyclic_16",
                  "Dcyclic_8", "Dih_6", "Dih_8_square", "Dih_8","Dih_12",
                  "K4_C3", "C6_C2", "C4_C3", "C4_C2", "C3_C2",
                  "C4_C4", "Q8_C2","C8_C2",NA_character_)) {
    return(TRUE)
  }
  else return(FALSE)
}
