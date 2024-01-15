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
#' #'\dontrun{
#' df <- tibble(home_shot_xg = list(c(0.06, 0.03, 0.03, 0.02, 0.07, 0.26, 0.09, 0.08, 0.05, 0.02, 0.06, 0.22, 0.10, 0.06, 0.3, 0.24, 0.05, 0.06, 0.04, 0.21)),
#'              away_shot_xg = list(c(0.06, 0.03, 0.04, 0.06)),
#'              team_home = "BYU",
#'              team_away = "Stanford")
#'
#' sim_df <- xg_win_prob(team_a_shots_xg = home_shot_xg,
#'                       team_b_shots_xg = away_shot_xg,
#'                       team_a_name = team_home,
#'                       team_b_name = team_away,
#'                       n_sim = 10000)
#'
#' #The first dataframe contains probabilities for each result
#' sim_df[[1]]
#' ## label    n   prob team_name points
#' ## 1 team_a 8487 0.8487       BYU      3
#' ## 2 team_b  213 0.0213  Stanford      0
#' ## 3   draw 1300 0.1300      <NA>     NA
#'
#' The second dataframe contains the probabilites for every score combination - arranged by probability
#' head(sim_df[[2]])
#' ## team_a_goals team_b_goals    n   prob
#' ## 1            2            0 2378 0.2378
#' ## 2            1            0 2181 0.2181
#' ## 3            3            0 1703 0.1703
#' ## 4            0            0  862 0.0862
#' ## 5            4            0  761 0.0761
#' ## 6            2            1  517 0.0517
#' }

xg_win_prob <- function(team_a_shots_xg, team_b_shots_xg,
                        team_a_name = "team_a", team_b_name = "team_b",
                        n_sim = 10000, seed = 123){

  # Set a seed to reproduce results
  if (!missing(seed)) {
    set.seed(123)
  }

  # Unlist shots
  team_a_shots_xg <- unlist(team_a_shots_xg)
  team_b_shots_xg <- unlist(team_b_shots_xg)

  # Initialize empty lists
  team_a_goals <- list()
  team_b_goals <- list()

  # Run the simulation n times
  for (i in 1:n_sim) {
    # Get simulated goals for each team
    simulated_goals <- xg_sim_match(team_a_shots_xg, team_b_shots_xg)

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
    rename(label = winner) |>
    arrange(case_when(
      label == "team_a" ~ 1,
      label == "team_b" ~ 2,
      TRUE ~ 3
    )) |>
    # mutate(prob = n / sum(n),
    #       team_name = c(team_a_name, team_b_name, NA)
    #       )
    mutate(prob = n / sum(n)) |>
    add_column(team_name = c(team_a_name, team_b_name, NA))

  # Calculate points based on probabilities
  if(win_prob$prob[3] == max(win_prob$prob)){
    win_prob$points <- c(1,1,NA)
  } else if(win_prob$prob[1] == max(win_prob$prob)){
    win_prob$points <- c(3,0,NA)
  } else {
    win_prob$points <- c(0,3,NA)
  }

  # Calculate share of points based on probabilities
  win_prob$points_share[1] <- round((win_prob$prob[1] + (win_prob$prob[3] / 2)) * 3, 2)
  win_prob$points_share[2] <- round((win_prob$prob[2] + (win_prob$prob[3] / 2)) * 3, 2)
  win_prob$points_share[3] <- NA


  # 2 As the probability for each result
  result_prob <- simulations_df |>
    count(team_a_goals,team_b_goals) |>
    mutate(prob = n / sum(n)) |>
    arrange(desc(n))

  # Bonus to return team names for plotting purposes
  team_names <- c(team_a_name, team_b_name)



  # Return a list of the three data frames
  list(win_prob, result_prob)
}
