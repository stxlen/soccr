#' Plots
#'
#' @param xg_win_prob_df A dataframe created with xg_win_prob()
#'
#' @return A ggplot object
#' @export
#'
plot_win_prob <- function(xg_win_prob_df, team_a_color, team_b_color, cust_alpha = 1){

  # COLOR HANDLING -----------------------------------------------------------
  # Set team colors or use green/red as defaults
  if(missing(team_a_color) & missing(team_b_color)){

    # Set colors for winning/losing teams
    if(abs(round(xg_win_prob_df[[1]]$prob[1] - xg_win_prob_df[[1]]$prob[2], 2)) <= 0.05){
      plot_colors <- c("black", "grey", "black")
    }else if(xg_win_prob_df[[1]]$prob[1] > xg_win_prob_df[[1]]$prob[2]){
      plot_colors <- c("darkred", "grey", "darkgreen")
    }else{
      plot_colors <- c("darkgreen", "grey", "darkred")
    }
  } else {
    plot_colors <- c(team_b_color, "grey", team_a_color)
  }

  # Function to calculate brightness
  is_dark <- function(color, alpha = cust_alpha) {
    rgb <- grDevices::col2rgb(color) / 255  # Convert to RGB and normalize
    white <- c(1, 1, 1)  # RGB values for white

    # Blend color with white based on alpha
    blended_rgb <- (1 - alpha) * white + alpha * rgb

    # Calculate luminance (per ITU-R recommendation)
    luminance <- 0.299 * blended_rgb[1] + 0.587 * blended_rgb[2] + 0.114 * blended_rgb[3]
    return(luminance < 0.5)  # Return TRUE if dark
  }

  # Determine text color for each bar
  text_colors <- ifelse(sapply(plot_colors, is_dark), "white", "black")
  names(text_colors) <- c("team_b", "draw", "team_a")  # Match names with factor levels

  # return(text_colors)

  # TEXT HANDLING ------------------------------------------------------------

  # Calculate center of draw bar
  # To Do: Use repel to move away from teams at the edges. Right now sets a limit how far toward edge can move
  # if(xg_win_prob_df[[1]]$prob[1] < 0.30){
  # draw_center <- 0.30
  # } else if (xg_win_prob_df[[1]]$prob[1] + xg_win_prob_df[[1]]$prob[3] > 0.70){
  #   draw_center <- 0.70
  # } else {draw_center <- xg_win_prob_df[[1]]$prob[1] + (xg_win_prob_df[[1]]$prob[3] / 2 ) }

  # Calculate center of draw bar 2nd method
  # To Do: Use repel to move away from teams at the edges. Right now sets a limit how far toward edge can move
  calc_draw_center <- xg_win_prob_df[[1]]$prob[1] + (xg_win_prob_df[[1]]$prob[3] / 2 )
  if(calc_draw_center < 0.30){
    draw_center <- 0.30
  } else if (calc_draw_center > 0.70){
    draw_center <- 0.70
  } else {draw_center <- calc_draw_center}

  # Used to define minimum text position for teams
  min_text_position <- 0.05  # Adjust this to control the minimum position


  # PLOT ---------------------------------------------------------------------
  # Quiets the following warning:
  # Vectorized input to `element_text()` is not officially supported.
  # ℹ Results may be unexpected or may change in future versions of ggplot2.
  suppressWarnings({

    # Plot code
    ggplot2::ggplot(data = xg_win_prob_df[[1]],
                    mapping = aes(x = prob, y = "dummy",
                                  fill = factor(label, levels = c("team_b", "draw", "team_a"))  # Ensure correct order
                    )) +
      scale_y_discrete(expand = c(0, 0, 0, 0), guide = "none") +
      geom_col(
        width = 0.9,
        alpha = cust_alpha
      ) +
      geom_text(
        aes(label = paste0(round(prob * 100, 0), "%"),
            # x = pmin(pmax(prob / 2, 0.05), 0.95),  # Keeps text within the visible range
            color = factor(label, levels = c("team_b", "draw", "team_a"))  # Map text color to factor levels
        ),
        position = position_stack(vjust = 0.5),
        size = 11 / .pt,  # Percent text size
        alpha = 1
      ) +
      scale_color_manual(values = text_colors) +  # Apply dynamic text colors
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
        axis.text.x = element_text(hjust = c(0, 0.5, 1), size = 12),
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 14, margin = margin(0, 0, 0, 0)),
        plot.subtitle = element_text(size = 11, margin = margin(0, 0, 10, 0)),
        axis.text.x.top = ggtext::element_markdown()
      ) +
      labs(
        title = "Win Probability",
        subtitle = "based on a Monte Carlo xG simulation",
        y = NULL, x = NULL
      )
  })
  }
