#' Uses xG data to simulate a season of football matches and returns the points total for each team
#'
#' @param df A data frame with four columns: team, opponent, team_xg_list, opponent_xg_list. Each row is a match.
#' @param team_a A column containing the home team or a team name.
#' @param team_b A column containing the away team or opponent name.
#' @param team_a_xg_list A column containing a list of the home team's xG eg. c(0.1, 0.1, 0.3).
#' @param team_b_xg_list A column containing a list of the away team's xG eg. c(0.1, 0.2, 0.1).
#' @param n_sim The number of simulations to run (default = 10000).
#' @param points_method The method used to calculate points. Options are "share" (default) or "full".
#'
#' @return
#' @export
#'
#' @examples
#' #' \dontrun{
#' # Create data simulating a season of 4 teams playing each team once
#' set.seed(123)
#'
#' teams <- tibble(team = c("Team A", "Team B", "Team C", "Team D"), opponent = c("Team A", "Team B", "Team C", "Team D"))
#'
#' season_data <- teams |>
#'   expand(team, opponent) |>
#'   filter(team != opponent) |>
#'   mutate(match_id = row_number()) |>
#'   rowwise() |>
#'   mutate(team_shots_xg = list(round(rnorm(sample(6:14, 1), 0.1, 0.05),2)),
#'          opp_shots_xg = list(round(rnorm(sample(6:14, 1), 0.1, 0.05),2))) |>
#'   ungroup()
#'
#' xg_full_season_sim(season_data)
#' ##  A tibble: 4 × 4
#' ##  team_name points n_games  rank
#' ##  <chr>      <dbl>   <int> <int>
#' ##1 Team D        13       6     1
#' ##2 Team A         8       6     2
#' ##3 Team B         7       6     3
#' ##4 Team C         6       6     4
#' }
xg_sim_season_pts <- function(df, team = team,
                              opponent = opponent,
                              team_xg_list = team_xg_list,
                              opp_xg_list = opp_xg_list,
                              n_sim = 10000,
                              points_method = "share"){

  season_summary <- apply(df,
                          1,
                          function(x) xg_win_prob(team_a_shots_xg = x["team_shots_xg"],
                                                  team_b_shots_xg = x["opp_shots_xg"],
                                                  team_a_name = x["team"],
                                                  team_b_name = x["opponent"],
                                                  n_sim = 10000))
  if(points_method == "full"){
    lapply(season_summary, function(x) x[[1]]) |>
      do.call(rbind, args = _) |>
      select(team_name, points) |>
      mutate(team_name = unlist(team_name)) |>
      filter(!is.na(team_name)) |>
      group_by(team_name) |>
      summarise(points = sum(points),
                n_games = n()) |>
      arrange(desc(points)) |>
      mutate(rank = rank(desc(points), ties.method = "min"))
  }else if(points_method == "share"){
    lapply(season_summary, function(x) x[[1]]) |>
      do.call(rbind, args = _) |>
      select(team_name, points_share) |>
      mutate(team_name = unlist(team_name)) |>
      filter(!is.na(team_name)) |>
      group_by(team_name) |>
      summarise(points = sum(points_share),
                n_games = n()) |>
      arrange(desc(points)) |>
      mutate(rank = rank(desc(points), ties.method = "min"))
  }else{
    stop("points_method must be either 'share' or 'full'")}

}
