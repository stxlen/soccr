#' Uses xG data to simulate a season of football matches and returns the points total for each team.
#'
#' @param df A data frame with four columns: team_a, team_b, team_a_xg_list, team_b_xg_list. Each row is a match.
#' @param team_a A column containing a team.
#' @param team_b A column containing an opponent name.
#' @param team_a_xg_list A column containing a list of team_a's xG eg. c(0.1, 0.1, 0.3).
#' @param team_b_xg_list A column containing a list of team_b's xG eg. c(0.1, 0.2, 0.1).
#' @param n_sim The number of simulations to run (default = 10000).
#' @param points_method The method used to calculate points. Options are "share" (default) or "full".
#'
#' @return A tibble with columns team_name, points, n_games, and rank.
#' @export
#'
#' @examples
#'\dontrun{
#' teams <- tibble(team = c("Team A", "Team B", "Team C", "Team D"),
#'                 opponent = c("Team A", "Team B", "Team C", "Team D"))
#'
#' season_data <- teams |>
#'   expand(team, opponent) |>
#'   filter(team != opponent) |>
#'   mutate(match_id = row_number()) |>
#'   rowwise() |>
#'   # Make B a little better than the rest and C a little worse
#'   mutate(team_xg_list = case_when(team == "Team B" ~list(abs(round(rnorm(sample(12:18, 1), 0.15, 0.08), 2))),
#'                                   team == "Team C" ~list(abs(round(rnorm(sample(6:10, 1), 0.10, 0.05), 2))),
#'                                   .default = list(abs(round(rnorm(sample(10:16, 1), 0.1, 0.05), 2)))),
#'          opp_xg_list = case_when(opponent == "Team B" ~list(abs(round(rnorm(sample(12:18, 1), 0.15, 0.08), 2))),
#'                                  opponent == "Team C" ~list(abs(round(rnorm(sample(6:10, 1), 0.10, 0.05), 2))),
#'                                  .default = list(abs(round(rnorm(sample(10:16, 1), 0.1, 0.05), 2))))) |>
#'   ungroup()
#'
#' season_data |>
#'   glimpse()
#' # Rows: 12
#' # Columns: 5
#' # $ team         <chr> "Team A", "Team A", "Te…
#' # $ opponent     <chr> "Team B", "Team C", "Te…
#' # $ match_id     <int> 1, 2, 3, 4, 5, 6, 7, 8,…
#' # $ team_xg_list <list> <0.12, 0.14, 0.16, 0.1…
#' # $ opp_xg_list  <list> <0.08, 0.09, 0.09, 0.1…
#'
#' xg_sim_season_pts(season_data,
#'                   team_a = team,
#'                   team_b = opponent,
#'                   team_a_xg_list = team_xg_list,
#'                   team_b_xg_list = opp_xg_list,
#'                   points_method  = "full")
#' # A tibble: 4 × 4
#' # team_name points n_games  rank
#' # <chr>      <dbl>   <int> <int>
#' # 1 Team B      18       6     1
#' # 2 Team A      12       6     2
#' # 3 Team D       6       6     3
#' # 4 Team C       0       6     4
#' }
xg_sim_season_pts <- function(df,
                              team_a = "team",
                              team_b = "opponent",
                              team_a_xg_list = "team_xg_list",
                              team_b_xg_list = "opp_xg_list",
                              n_sim = 10000,
                              points_method = "share") {
  # Capture column names
  team_a_col <- rlang::ensym(team_a)
  team_b_col <- rlang::ensym(team_b)
  team_a_xg_col <- rlang::ensym(team_a_xg_list)
  team_b_xg_col <- rlang::ensym(team_b_xg_list)

  # Use rowwise to iterate over rows
  season_summary <- df |>
    dplyr::rowwise() |>
    dplyr::mutate(
      match_summary = list(
        xg_win_prob(
          team_a_xg_list = !!team_a_xg_col,
          team_b_xg_list = !!team_b_xg_col,
          team_a_name = !!team_a_col,
          team_b_name = !!team_b_col,
          n_sim = n_sim
        )
      )
    ) |>
    dplyr::ungroup()

  if (points_method == "full") {
    season_summary |>
      dplyr::mutate(points = purrr::map(match_summary, ~ .x[[1]])) |>
      tidyr::unnest(points) |>
      dplyr::select(team_name, points) |>
      dplyr::filter(!is.na(team_name)) |>
      dplyr::group_by(team_name) |>
      dplyr::summarise(
        points = sum(points, na.rm = TRUE),
        n_games = dplyr::n()
      ) |>
      dplyr::arrange(dplyr::desc(points)) |>
      dplyr::mutate(rank = rank(dplyr::desc(points), ties.method = "min"))
  } else if (points_method == "share") {
    season_summary |>
      dplyr::mutate(points_share = purrr::map(match_summary, ~ .x[[1]])) |>
      tidyr::unnest(points_share) |>
      dplyr::select(team_name, points_share) |>
      dplyr::filter(!is.na(team_name)) |>
      dplyr::group_by(team_name) |>
      dplyr::summarise(
        points = sum(points_share, na.rm = TRUE),
        n_games = dplyr::n()
      ) |>
      dplyr::arrange(dplyr::desc(points)) |>
      dplyr::mutate(rank = rank(dplyr::desc(points), ties.method = "min"))
  } else {
    stop("points_method must be either 'share' or 'full'")
  }
}
