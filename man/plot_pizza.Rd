% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_pizza.R
\name{plot_pizza}
\alias{plot_pizza}
\title{Creates a pizza plot with no preset groups or colors. The data frame must have the
following columns named exactly as shown: player, statistic, value, percentile, category, and color. The plot will
follow the order of the data frame.}
\usage{
plot_pizza(
  df,
  title,
  subtitle = "",
  caption = "",
  pos_group = "",
  theme_color,
  logo_path = "",
  title_loc = "left"
)
}
\arguments{
\item{df}{A data frame with six columns: player, statistic, value, percentile, category and color. \code{percentile} is expected to be a number between 0 and 99.}

\item{title}{Text to appear as the title}

\item{subtitle}{Text to appear in the subtile.}

\item{caption}{Text to appear in the caption (bottom)}

\item{pos_group}{The position group to use for the average line text. Not required.}

\item{theme_color}{The theme color to use: light or dark. Default is light.}

\item{logo_path}{The path to the logo to use. Not required.}

\item{title_loc}{The location of the title: left, center, or right. Default is left.}
}
\value{
A ggplot object
}
\description{
Creates a pizza plot with no preset groups or colors. The data frame must have the
following columns named exactly as shown: player, statistic, value, percentile, category, and color. The plot will
follow the order of the data frame.
}
\examples{
scaled_data <- data.frame(player = rep("Alexia Putellas", 12),
                          statistic = c("Non-Penalty Goals", "Assists", "Shot-Creating Actions",
                          "Pass Completion Perc", "Progressive Passes", "Progressive Carries",
                          "Tackles", "Interceptions", "Aerials Won",
                          "Fouls Drawn","Fouls Committed", "Offsides"),
                          value = c(.69, 0.1, 6.2, 80, 8.1, 2.0, 0.8, 0.4, 0.8, 0.8, 0.7, 0.7),
                          percentile = c(85, 48, 99, 99, 99, 70, 42, 85, 21, 42, 73, 61),
                          category = rep(c("Attack", "Possession", "Defending", "Misc"), each = 3),
                          color = rep(c("#008000", "#0000FF", "#FF0000", "#FFA500") ,each = 3))

p <- plot_radar_man(scaled_data,
                    title = scaled_data$player[1],
                    subtitle = paste("All Matches for 2023 | Compared to FWDs","All Data per 90", sep = "\n"),
                    caption = "Data: FBref.com | By: Sean Steele",
                    pos_group = "FW/AM",
                    theme_color = "light")
p
}
