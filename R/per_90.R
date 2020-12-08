# converts a stat to a per 90 metric

per_90 <- function(stat, minutes_played){
  round(stat / minutes_played * 90, digits = 2)
}
