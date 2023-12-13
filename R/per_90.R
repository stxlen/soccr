#' Per 90 Conversion
#'
#' This function converts a raw stat to a per 90 metric
#'
#' @param stat The stat to convert to a per 90 metric
#' @param minutes_played The total number of minutes played

per_90 <- function(stat, minutes_played){
  round(stat / minutes_played * 90, digits = 2)
}
