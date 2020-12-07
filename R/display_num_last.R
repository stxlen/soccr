# displays name in the form #N Lastname

display_num_last <- function(number, last_name) {
  display <- paste0("#",number," ",last_name)
  return(display)
}
