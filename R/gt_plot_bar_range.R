#' After gtExtras::gt_plot_bar_stack. The gt_plot_bar_range takes an existing gt_tbl object and converts the existing values into a stacked barchart.
#'
#' @param gt_object An existing gt table object of class gt_tbl
#' @param column The column wherein the  stacked barchart should replace existing data. Note that the data must be represented as a list of three numeric values and an optional color value ahead of time.
#' @param team_a_name name of team A
#' @param team_b_name name of team B
#' @param n_sim number of simulations to run
#' @param seed seed for reproducibility
#'
#' @return A list of two data frames: 1) win-win-draw probabilities and 2) result probabilities
#' @export
#'
#' @examples
#' \dontrun{
#' ex_df <- tibble(
#' x = c("load", "sumad", "spd", "hsrp", "tdc"),
#' list_data = list(c(75, 50, 75, "purple"), c(150, 50, 0, "blue"), c(0, 25, 175, "red"),
#'                  c(50, 100, 50, "orange"), c(50, 50, 100, "green"))
#' )
#'
#' ex_tab <- ex_df %>%
#'   gt() %>%
#'   gt_plot_bar_range(column = list_data)
#'
#' ex_tab
#' }

gt_plot_bar_range <- function(gt_object, column) {
  stopifnot("Table must be a gt_tbl" = "gt_tbl" %in% class(gt_object))

  var_sym <- rlang::enquo(column)
  col_vals <- gt_index(gt_object, {{ column }})

  bar_fx <- function(values) {
    if (is.null(values) || length(values) < 3) return("<div></div>")

    numeric_vals <- as.numeric(values[1:3])
    middle_color <- values[4] %||% "black"  # Default to black if no color is provided

    palette <- c("lightgrey", middle_color, "lightgrey")  # First and last are always lightgrey

    df <- tibble(x = numeric_vals, fill = palette, order = 1:3) # How do I presever this order

    print(df)

    plot_out <- ggplot(df, aes(x = x, y = factor(1), fill = I(fill), group = rev(order))) +
      geom_col(width = 1, color = "white") +
      geom_vline(xintercept = c(100, 150), color = "black" ) +
      theme_void() +
      theme(legend.position = "none", plot.margin = margin(0, 0, 0, 0, "pt"))

    out_name <- tempfile(fileext = ".svg")
    ggsave(out_name, plot = plot_out, dpi = 25.4, height = 5, width = 70, units = "mm", device = "svg")

    img_plot <- readLines(out_name) %>% paste0(collapse = "") %>% gt::html()
    on.exit(file.remove(out_name), add = TRUE)

    img_plot
  }

  text_transform(gt_object, locations = cells_body({{ column }}), fn = function(x) lapply(col_vals, bar_fx))
}
