#' Initialize dimensions for use elsewhere
#'
#' @param provider Shortcut to add dimensions of provider
#' @param pitch_length Length of the pitch.
#' @param pitch_width Width of the pitch.
#' @param pitch_units Units of measurement.
#' @return Variables to be used in the future
#' @export

pitch_setup <- function(provider, length, width){
  if(tolower(provider) == "instat"){
    assign("pitch_length", 105, envir = .GlobalEnv)
    assign("pitch_width", 68, envir = .GlobalEnv)
    assign("pitch_units", "m", envir = .GlobalEnv)
  }else if(tolower(provider) == "statsbomb"){
    assign("pitch_length", 120, envir = .GlobalEnv)
    assign("pitch_width", 80, envir = .GlobalEnv)
    assign("pitch_units", "m", envir = .GlobalEnv)
  }else if(tolower(provider) == "custom"){
    assign("pitch_length", length, envir = .GlobalEnv)
    assign("pitch_width", width, envir = .GlobalEnv)
    assign("pitch_units", "m", envir = .GlobalEnv)
  }else{
    warning("Current providers are InStat or StatsBomb. You may choose custom and a length and width to make your own.")
  }
  return(list(pitch_length, pitch_width, pitch_units))
}
