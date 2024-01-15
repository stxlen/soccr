#' xg_sim_goal
#' @description Simulate the number of goals scored given a vector of shot xG values
#' @param shot_xg_list List of shot xG values
#'
#' @return The simulated number of goals scored
#' @export
#'
#' @examples
#' set.seed(123)
#' # Create a data frame of shots
#' shots <- data.frame(shot = seq(1:10), xG = runif(n = 10, max = 1))
#'
#' # Simulate the number of goals scored
#' shots |> pull(xG) |> xg_sim_goal()
#'
xg_sim_goal <- function(shot_xg_list){
  #Start goal count at 0
  goals <- 0

  # For each shot, if it goes in, add a goal
  for (shot_xg in shot_xg_list){
    if (runif(1)<=shot_xg){
      goals <- goals + 1
    }
  }

  # Return the number of goals
  return(goals)

}
