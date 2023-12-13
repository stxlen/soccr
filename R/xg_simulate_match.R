#' Simulate the number of goals scored given a vector of shot xG values for two teams
#'
#' @param team_a_shots_xg
#' @param team_b_shots_xg
#'
#' @return
#' @export
#'
#' @examples
xg_simulate_match <- function(team_a_shots_xg, team_b_shots_xg){

  # Simulate goals for each team
  team_a_goals <- xg_simulate_goal(team_a_shots_xg)
  team_b_goals <- xg_simulate_goal(team_b_shots_xg)

  # Return the goals as separate lists
  return(list(team_a_goals, team_b_goals))

}
