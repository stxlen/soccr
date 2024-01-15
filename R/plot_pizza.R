#' Creates a pizza plot with no preset groups or colors. The data frame must have the
#' following columns named exactly as shown: player, statistic, value, percentile, category, and color. The plot will
#' follow the order of the data frame.
#'
#' @param df A data frame with six columns: player, statistic, value, percentile, category and color. `percentile` is expected to be a number between 0 and 99.
#' @param title Text to appear as the title
#' @param subtitle Text to appear in the subtile.
#' @param caption Text to appear in the caption (bottom)
#' @param theme_color The theme color to use: light or dark. Default is light.
#' @param pos_group The position group to use for the average line text. Not required.
#' @param logo_path The path to the logo to use. Not required.
#' @param title_loc The location of the title: left, center, or right. Default is left.
#'=
#' @return A ggplot object
#' @export
#'
#' @examples
#' #' \dontrun{
#' scaled_data <- data.frame(player = rep("Alexia Putellas", 12),
#'                           statistic = c("Non-Penalty Goals", "Assists", "Shot-Creating Actions",
#'                           "Pass Completion Perc", "Progressive Passes", "Progressive Carries",
#'                           "Tackles", "Interceptions", "Aerials Won",
#'                           "Fouls Drawn","Fouls Committed", "Offsides"),
#'                           value = c(.69, 0.1, 6.2, 80, 8.1, 2.0, 0.8, 0.4, 0.8, 0.8, 0.7, 0.7),
#'                           percentile = c(85, 48, 99, 99, 99, 70, 42, 85, 21, 42, 73, 61),
#'                           category = rep(c("Attack", "Possession", "Defending", "Misc"), each = 3),
#'                           color = rep(c("#008000", "#0000FF", "#FF0000", "#FFA500") ,each = 3))
#'
#' p <- plot_radar_man(scaled_data,
#'                     title = scaled_data$player[1],
#'                     subtitle = paste("All Matches for 2023 | Compared to FWDs","All Data per 90", sep = "\n"),
#'                     caption = "Data: FBref.com | By: Sean Steele",
#'                     pos_group = "FW/AM",
#'                     theme_color = "light")
#' p
#' }

plot_pizza <- function(df, title, subtitle = "", caption = "", pos_group = "", theme_color, logo_path = "", title_loc = "left"){

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

  # SET LOGO DEFAULTS ###########################################################################

  if (is.null(logo_path)){
    title_pos <- 0
  } else if (title_loc == "center"){
    title_pos <- 0.5
  } else if (title_loc == "right"){
    title_pos <- 1
  } else {
    title_pos <- 0
  }

  # PLOT ########################################################################################

  player_data <- df |>
    mutate(statistic = factor(statistic,
                              levels = df |> pull(statistic) |> unique()
          )
    )

  colors_map <- player_data |>
    distinct(category, color) |>
    tibble::deframe()

  legend_map <-  player_data |>
    distinct(category) |>
    tibble::deframe()

  p <- ggplot(player_data, aes(statistic, percentile)) +
       geom_bar(aes(y = 100, fill = category), stat = "identity", width = 1, color = fill_b,
                alpha = 0.1, show.legend = FALSE) +
       geom_bar(stat = "identity", width = 1, aes(fill = category), color = fill_b, alpha = 1) +
       coord_polar(clip = "off") +
       geom_hline(yintercept = 25, color = colorLine, linetype = "dashed", alpha = 0.8) +
       geom_hline(yintercept = 50, color = colorLine, linetype = "dashed", alpha = 0.8) +
       geom_hline(yintercept = 75, color = colorLine, linetype = "dashed", alpha = 0.8) +
       scale_fill_manual(values = colors_map, breaks = legend_map) +
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
             legend.text = element_text(size = 9, color = colorText),
             legend.key.size = unit(0.5, "cm"),
             legend.spacing.x = unit(1, "mm"),  # Adjust spacing as needed
             axis.title.y = element_blank(),
             axis.title.x = element_blank(),
             axis.text.y = element_blank(),
             axis.text.x = element_text(size = 12, color = colorText),
             text = element_text(color = colorText, size = 20),
             plot.title = element_text(hjust = title_pos, size = 26, color = colorText, face = "bold"),
             plot.subtitle = element_text(hjust = title_pos, size = 12, color = colorText),
             plot.caption = element_text(hjust = 0.5, size = 9, color = colorText),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank()) +
       scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 1))



  # RETURN WITH OR WITHOUT A LOGO ###########################################################################

  test_grob <- TRUE

  if (logo_path == "") {

    return(p)

  } else {
    temp_plot <- tempfile("plot", fileext = ".png")
    ggsave(temp_plot, p, width = 2700, height =3000, units = "px")
    plot_add_logo(temp_plot, logo_path, logo_position = "top right", logo_scale = 12, logo_padding = 0.01)
  }

}



