#' Plots
#'
#' @param xg_win_prob_df A dataframe created with xg_win_prob()
#'
#' @return
#' @export
#'
#' @examples
plot_win_prob <- function(xg_win_prob_df){

  # Set colors for winning/losing teams
  if(abs(round(xg_win_prob_df[[1]]$prob[1] - xg_win_prob_df[[1]]$prob[3], 2)) <= 0.05){
    plot_colors <- c("black", "grey", "black")
  }else if(xg_win_prob_df[[1]]$prob[1] > xg_win_prob_df[[1]]$prob[3]){
    plot_colors <- c("darkred", "grey", "darkgreen")
  }else{
    plot_colors <- c("darkgreen", "grey", "darkred")
  }

  # Quites the following warning:
  # Vectorized input to `element_text()` is not officially supported.
  # ℹ Results may be unexpected or may change in future versions of ggplot2.
  suppressWarnings({

  # Plot
  ggplot2::ggplot(data = xg_win_prob_df[[1]],
                  mapping = aes(x = prob, y = "dummy",
                                fill = factor(winner, levels = c("team_b", "draw", "team_a")) # Order the fill reversed
  )) +
    scale_y_discrete(expand = c(0, 0, 0, 0), guide = "none") +
    geom_col(
      width = 0.9,
      alpha = 0.40
    ) +
    geom_text(
      aes(label = paste0(round(prob * 100, 0), "%")
      ),
      position = position_stack(vjust = 0.5)
    ) +
    scale_x_continuous(
      breaks = c(0, .5, 1),
      labels = c(glue::glue("<span style='color:{plot_colors[3]}'>{xg_win_prob_df[[3]][1]}</span>"),
                 "Draw",
                 glue::glue("<span style='color:{plot_colors[1]}'>{xg_win_prob_df[[3]][2]}</span>")),
      expand = c(0, 0),
      limits = c(0, 1),
      position = "top"
    ) +
    scale_fill_manual(values = plot_colors) +
    coord_cartesian(clip = "off") +
    theme(
      legend.position = "none",
      axis.text.x = element_text(hjust = c(0, .5, 1), size = 28),
      axis.ticks.x = element_blank(),
      panel.background = element_blank(),
      plot.title = element_text(size = 40, margin = margin(0, 0, 0, 0)),
      plot.subtitle = element_text(size = 16, margin = margin(0, 0, 10, 0)),
      axis.text.x.top  = ggtext::element_markdown()
    ) +
    labs(
      title = "Win Probability",
      subtitle = "based on xG simulation",
      y = NULL, x = NULL
    )
  })
  }
