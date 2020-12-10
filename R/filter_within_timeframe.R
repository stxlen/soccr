#' Find and filter all events within a certain time of a specific event
#' For example, find all events 5 seconds prior to and including a shot
#' Right now only looks works for seconds before (lead), need to have a solution that looks after and between two times
#'
#' @param condition The event to find in dplyr fashion. Such as event == 'A'
#' @param time The column/data that contains the timestamp
<<<<<<< HEAD
#' @param lead_time The number of seconds before the event occurs
#' @param lag_time The number of seconds after an event occurs (needs to be implemented)
=======
#' @param time_filter The number of seconds before the event occurs
>>>>>>> feac73f6287e9e3fd8e55947ff6c43a8b7b907e6
#'

filter_within_timeframe <- function(.data, condition, time, lead_time = 0, lag_time = 0){
  condition <- enquo(condition)
  time <- enquo(time)
  filtered <- .data %>%
    slice(., 1:max(which(!!condition))) %>%
    group_by(., grp =  lag(cumsum(!!condition), default = 0)) %>%
    filter(.,(last(!!time) - !!time) <= lead_time & (last(!!time) - !!time) >= lag_time)
  return(filtered)
}
<<<<<<< HEAD

# Example usage
  # event_df %>% filter_within_timeframe(event == 'shot', s, 5, 0)
=======
>>>>>>> feac73f6287e9e3fd8e55947ff6c43a8b7b907e6
