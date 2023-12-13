#' Simulate matches to calculate win-draw-win probabilities
#'
#' @param team_a_shots_xg vector of shot xG values for team A
#' @param team_b_shots_xg vector of shot xG values for team B
#' @param team_a_name name of team A
#' @param team_b_name name of team B
#' @param n_sim number of simulations to run
#' @param seed seed for reproducibility
#'
#' @return
#' @export
#'
#' @examples

xg_win_prob <- function(team_a_shots_xg, team_b_shots_xg,
                        team_a_name = "team_a", team_b_name = "team_b",
                        n_sim = 10000, seed = 123){

  # Set a seed to reproduce results
  if (!missing(seed)) {
    set.seed(seed)
  }

  # Initialize empty lists
  team_a_goals <- list()
  team_b_goals <- list()

  # Run the simulation n times
  for (i in 1:n_sim) {
    # Get simulated goals for each team
    simulated_goals <- xg_simulate_match(team_a_shots_xg, team_b_shots_xg)

    # Store team-specific goals in separate lists
    team_a_goals[[i]] <- simulated_goals[[1]]
    team_b_goals[[i]] <- simulated_goals[[2]]
  }

  # Return simulation data frame
  # Combine lists into a data frame
  simulations_df <- data.frame(
    team_a_goals = unlist(team_a_goals),
    team_b_goals = unlist(team_b_goals)
  ) |>
    # Calculate the winner
    mutate(winner = case_when(
      team_a_goals > team_b_goals ~ "team_a",
      team_a_goals < team_b_goals ~ "team_b",
      team_a_goals == team_b_goals ~ "draw"
    ))

  # Return simulation data frame in two forms
  # 1 As a win-draw-win probability
  win_prob <- simulations_df |>
    count(winner) |>
    mutate(prob = n / sum(n)) |>
    arrange(case_when(
      winner == "team_a" ~ 1,
      winner == "team_b" ~ 2,
      TRUE ~ 3
    ))

  # 2 As the probability for each result
  result_prob <- simulations_df |>
    count(team_a_goals,team_b_goals) |>
    mutate(prob = n / sum(n)) |>
    arrange(desc(n))

  # Bonus to return team names for plotting purposes
  team_names <- c(team_a_name, team_b_name)

  # Return a list of the three data frames
  list(win_prob, result_prob, team_names)
}
