#' Simulate the number of goals scored given a vector of shot xG values for two teams
#'
#' @param team_a_shots_xg A list containing the xG values for each shot taken by team A
#' @param team_b_shots_xg A list containing the xG values for each shot taken by team B
#'
#' @return
#' @export
#'
xg_sim_match <- function(team_a_shots_xg, team_b_shots_xg){

  # Simulate goals for each team
  team_a_goals <- xg_sim_goal(unlist(team_a_shots_xg))
  team_b_goals <- xg_sim_goal(unlist(team_b_shots_xg))

  # Return the goals as separate lists
  return(list(team_a_goals, team_b_goals))

}
