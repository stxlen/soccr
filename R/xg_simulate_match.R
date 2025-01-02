#' Simulate the number of goals scored given a vector of shot xG values for two teams
#'
#' @param team_a_xg_list A list containing the xG values for each shot taken by team A
#' @param team_b_xg_list A list containing the xG values for each shot taken by team B
#'
#' @return A list containing the number of goals scored by each team
#' @export
#'
xg_sim_match <- function(team_a_xg_list, team_b_xg_list){

  # Simulate goals for each team
  team_a_goals <- xg_sim_goal(unlist(team_a_xg_list))
  team_b_goals <- xg_sim_goal(unlist(team_b_xg_list))

  # Return the goals as separate lists
  return(list(team_a_goals, team_b_goals))

}
