#' Find and filter all events within a certain time of a specific event
#' For example, find all events 5 seconds prior to and including a shot
#' Right now only looks works for seconds before (lead), need to have a solution that looks after and between two times
#'
#' @param condition The event to find in dplyr fashion. Such as event == 'A'
#' @param time The column/data that contains the timestamp
#' @param lead_time The number of seconds before the event occurs
#' @param lag_time The number of seconds after an event occurs (needs to be implemented)
#' @param time_filter The number of seconds before the event occurs
#' @return A plot of a pitch
#' @export

filter_within_timeframe <- function(.data, condition, time, lead_time = 0, lag_time = 0){
  condition <- enquo(condition)
  time <- enquo(time)
  filtered_lead <- c()
  filtered_lag <- c()

  if(lead_time > 0){
  # filter lead events
  filtered_lead <- .data %>%
    slice(., 1:max(which(!!condition))) %>%
    group_by(., grp =  lag(cumsum(!!condition), default = 0)) %>%
    filter(.,(last({{time}}) - {{time}}) <= lead_time & (last({{time}}) - {{time}}) >= 0) %>%
    ungroup() %>%
    select(-grp)
  }

  if(lag_time > 0){
  #filter lag events
  filtered_lag <- .data %>%
    slice(min(which(!!condition)):nrow(.)) %>% # remove rows before first occurance
    group_by(grp =  lead(rev(cumsum(rev(!!condition))), default = 0), period) %>%
    filter(({{time}} - first({{time}}[!!condition])) <= lag_time) %>% # something in here doesn't work !!!!!!!!!!!!!
    ungroup() %>%
    select(-grp)
  }

  filtered <- rbind(filtered_lead, filtered_lag) %>% distinct()
  return(filtered)

}

# Example usage
  # event_df %>% filter_within_timeframe(event == 'shot', s, 5, 0)

