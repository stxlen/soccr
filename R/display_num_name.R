#' Diplay Number and Last Name
#'
#' This function displays name in the form #number name
#'
#' @param number The player's jersey number
#' @param name The player's name to display

display_num_name <- function(number, name) {
  display <- paste0("#",number," ",name)
  return(display)
}
