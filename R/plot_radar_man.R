#' Creates a pizza plot with no preset groups or colors. The data frame must have the
#' following columns named exactly as shown: player, statistic, value, percentile, category, and color. The plot will
#' follow the order of the data frame.
#'
#' @param df A data frame with six columns: player, statistic, value, percentile, category and color. `percentile` is expected to be a number between 0 and 99.
#' @param title Text to appear as the title
#' @param subtitle Text to appear in the subtile. Use paste(..., sep = "\n") to create multiple lines.
#' @param theme_color The theme color to use: light or dark. Default is light.
#' @param pos_group The position group to use for the average line text. Not required

#'
#' @return
#' @export
#'
#' @examples
#' scaled_data <- data.frame(player = rep("Player 1", 12),
#'                           statistic = c("Stat 1", "Stat 2", "Stat 3", "Stat 4", "Stat 5", "Stat 6",
#'                                         "Stat 7", "Stat 8", "Stat 9", "Stat 10", "Stat 11", "Stat 12"),
#'                           value = c(12, 4, 9, 10, 5, 23, 15, 9, 3, 8, .79, 3),
#'                           percentile = c(60, 60, 70, 70, 90, 80, 40, 30, 50, 90, 99, 80),
#'                           category = rep(c("Attack", "Possession", "Defending", "Set Pieces"), each = 3),
#'                           color = rep(c("#008000", "#0000FF", "#FF0000", "#FFA500") ,each = 3))
#'
#' p <- plot_radar_man(scaled_data,
#'                     title = "Player A",
#'                     subtitle = paste("This Season | Compared to DMs","All Data per 90", sep = "\n"),
#'                     pos_group = "DM",
#'                     theme_color = "dark")
#' p

plot_radar_man <- function(df, title, subtitle = "", caption = "", pos_group = "", theme_color){

  # SET THEME COLORS ###########################################################################
  if (theme_color == "light" || theme_color == "") {
    fill_b <- "white"
    color_b <- "white"
    colorText <- "black"
    gridline <- "565656"
    colorLine <- "black"
  } else if (theme_color == "dark") {
    fill_b <- "black"
    color_b <- "black"
    colorText <- "white"
    gridline <- "565656"
    colorLine <- "white"
  } else {
    fill_b <- "white"
    color_b <- "white"
    colorText <- "black"
    gridline <- "565656"
    colorLine <- "black"
  }

  player_data <- df |>
    mutate(statistic = factor(statistic,
                              levels = df |> pull(statistic) |> unique()
          )
    )

  colors_map <- player_data |>
    distinct(category, color) |>
    tibble::deframe()

  ggplot(player_data, aes(statistic, percentile)) +
    geom_bar(aes(y = 100, fill = category), stat = "identity", width = 1, color = fill_b,
             alpha = 0.1, show.legend = FALSE) +
    geom_bar(stat = "identity", width = 1, aes(fill = category), color = fill_b, alpha = 1) +
    coord_polar(clip = "off") +
    geom_hline(yintercept = 25, color = colorLine, linetype = "dashed", alpha = 0.8) +
    geom_hline(yintercept = 50, color = colorLine, linetype = "dashed", alpha = 0.8) +
    geom_hline(yintercept = 75, color = colorLine, linetype = "dashed", alpha = 0.8) +
    scale_fill_manual(values = colors_map) +
    geom_label(aes(y = 90, label = value, fill = category), size = 3, color = fill_b, show.legend = FALSE) +
    geom_text(x = 0.5, y = 50, label = paste0("Avg ", pos_group), vjust = -0.5, alpha = 0.05, size = 3) +  # Place text at the first bar
    scale_y_continuous(limits = c(-20, 100)) +
    labs(fill = "",
         caption = caption,
         title = title,
         subtitle = subtitle) +
    theme_minimal() +
    theme(plot.background = element_rect(fill = fill_b, color = color_b),
          panel.background = element_rect(fill = fill_b, color = color_b),
          legend.position = "bottom",
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 12, color = colorText),
          text = element_text(color = colorText, size = 20),
          plot.title = element_text(hjust = 0.0, size = 26, color = colorText, face = "bold"),
          plot.subtitle = element_text(hjust = 0.0, size = 12, color = colorText),
          plot.caption = element_text(hjust = 0.5, size = 9, color = colorText),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    # scale_x_discrete(labels = function(x) text_wrap(x = x))
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 1))

}



