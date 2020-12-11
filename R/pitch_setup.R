#' Initialize dimensions for use elsewhere
#'
#' @param provider Shortcut to add dimensions of provider
#' @param pitch_length_init Length of the pitch.
#' @param pitch_width_init Width of the pitch.
#' @param pitch_units_init Units of measurement.
#' @return Variables to be used in the future
#' @export

pitch_setup <- function(provider, length, width){
  if(tolower(provider) == "instat"){
    assign("pitch_length_init", 105, envir = .GlobalEnv)
    assign("pitch_width_init", 68, envir = .GlobalEnv)
    assign("pitch_units_init", "m", envir = .GlobalEnv)
  }else if(tolower(provider) == "statsbomb"){
    assign("pitch_length_init", 120, envir = .GlobalEnv)
    assign("pitch_width_init", 80, envir = .GlobalEnv)
    assign("pitch_units_init", "m", envir = .GlobalEnv)
  }else if(tolower(provider) == "custom"){
    assign("pitch_length_init", length, envir = .GlobalEnv)
    assign("pitch_width_init", width, envir = .GlobalEnv)
    assign("pitch_units_init", "m", envir = .GlobalEnv)
  }else{
    warning("Current providers are InStat or StatsBomb.
            You may choose \"custom\" and provide a length and width of your own.")
  }
  # add tryCatch to not return the values and only return the warning?
  return(list(pitch_length_init, pitch_width_init, pitch_units_init))
}
