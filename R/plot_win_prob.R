#' Plots
#'
#' @param xg_win_prob_df A dataframe created with xg_win_prob()
#'
#' @return
#' @export
#'
plot_win_prob <- function(xg_win_prob_df){

  # Set colors for winning/losing teams
  if(abs(round(xg_win_prob_df[[1]]$prob[1] - xg_win_prob_df[[1]]$prob[2], 2)) <= 0.05){
    plot_colors <- c("black", "grey", "black")
  }else if(xg_win_prob_df[[1]]$prob[1] > xg_win_prob_df[[1]]$prob[2]){
    plot_colors <- c("darkred", "grey", "darkgreen")
  }else{
    plot_colors <- c("darkgreen", "grey", "darkred")
  }

  # Calculate center of draw bar
  # To Do: Use repel to move away from teams at the edges. Right now sets a limit how far toward edge can move
  if(xg_win_prob_df[[1]]$prob[1] < 30){
  draw_center <- 0.30
  } else if (xg_win_prob_df[[1]]$prob[1] + xg_win_prob_df[[1]]$prob[2] > 0.70){
    draw_center <- 0.70
  } else {draw_center <- xg_win_prob_df[[1]]$prob[1] + (xg_win_prob_df[[1]]$prob[3] / 2 ) }


  # Quiets the following warning:
  # Vectorized input to `element_text()` is not officially supported.
  # ℹ Results may be unexpected or may change in future versions of ggplot2.
  suppressWarnings({

  # Plot
  ggplot2::ggplot(data = xg_win_prob_df[[1]],
                  mapping = aes(x = prob, y = "dummy",
                                fill = factor(label, levels = c("team_b", "draw", "team_a")) # Order the fill reversed
  )) +
    scale_y_discrete(expand = c(0, 0, 0, 0), guide = "none") +
    geom_col(
      width = 0.9,
      alpha = 0.40
    ) +
    geom_text(
      aes(label = paste0(round(prob * 100, 0), "%")
      ),
      position = position_stack(vjust = 0.5),
      size = 11/.pt # Percent text size
    ) +
    scale_x_continuous(
      breaks = c(0, draw_center, 1),
      labels = c(glue::glue("<span style='color:{plot_colors[3]}'>{xg_win_prob_df[[1]][1,4]}</span>"),
                 "Draw",
                 glue::glue("<span style='color:{plot_colors[1]}'>{xg_win_prob_df[[1]][2,4]}</span>")),
      expand = c(0, 0),
      limits = c(0, 1),
      position = "top"
    ) +
    scale_fill_manual(values = plot_colors) +
    coord_cartesian(clip = "off") +
    theme(
      legend.position = "none",
      axis.text.x = element_text(hjust = c(0, 0.5, 1), size = 12), # Team text pos/size
      axis.ticks.x = element_blank(),
      panel.background = element_blank(),
      plot.title = element_text(size = 14, margin = margin(0, 0, 0, 0)), # Title size
      plot.subtitle = element_text(size = 11, margin = margin(0, 0, 10, 0)), # Subtitle size
      axis.text.x.top  = ggtext::element_markdown()
    ) +
    labs(
      title = "Win Probability",
      subtitle = "based on a Monte Carlo xG simulation",
      y = NULL, x = NULL
    )
  })
  }
