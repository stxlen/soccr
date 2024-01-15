#' Plots matrix of score probabilities
#'
#' @param xg_win_prob_df A dataframe created with xg_win_prob()
#'
#' @return
#' @export
#'
plot_score_prob <- function(xg_win_prob_df){

  custom_breaks <- function(e, num = 10){
    pretty(e, num)[round(pretty(e, num),1) %% 1 == 0]
  }

  score_matrix <- xg_win_prob_df[[2]] |>
    select(-n) |>
    # I would like to keep at minimum a 3x3 matrix,
    # so we are going to add a column to mark which results need to be kept
    mutate(keep = ifelse((team_a_goals <= 3 & team_b_goals <= 3) | prob > 0.01,
                         1, 0)) |>
    filter(keep == 1) |>
    select(-keep) |>
    # we need to fill out the matrix
    complete(team_a_goals = 0:max(team_a_goals), team_b_goals = 0:max(team_b_goals), fill = list(prob = 0)) |>
    # we are going to round the probabilities to 2 decimals and convert to a percentage
    mutate(prob_text = case_when(prob == 0 ~ "0%",
                                 prob < 0.01 ~ "<1%",
                                 TRUE ~ paste0(round(prob*100,0), "%"))) |>
    arrange(team_a_goals, team_b_goals)

  ggplot(data = score_matrix, aes(x = team_a_goals , y =  team_b_goals, fill = prob)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "darkmagenta") +
    scale_y_continuous(breaks = custom_breaks) +
    scale_x_continuous(breaks = custom_breaks) +
    # geom_text(aes(label = paste("",round(prob*100,1),"")), color = 'black', size = 6) +
    geom_text(aes(label = paste(prob_text)), color = 'black', size = 9/.pt) +
    theme_bw() +
    theme(text=element_text(size=12),
          axis.text.x = element_text(size = 12), # Team text pos/size
          plot.title = element_text(size = 14), # Title size
          plot.subtitle = element_text(size = 11, margin = margin(-5, 0, 0, 0)), # Subtitle size
          # plot.subtitle = element_text(size = 11), # Subtitle size
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          element_blank()) +
    xlab(paste0(xg_win_prob_df[[1]][1,4]," Goals")) +
    ylab(paste0(xg_win_prob_df[[1]][2,4]," Goals")) +
    ggtitle("Score Probability Matrix", subtitle = "percentage of goal combinations from xG simulation") +
    coord_fixed() +
    guides(fill="none")
}
