#' Add logo to a plot
#'
#' @param plot_path Location of plot image to add logo to
#' @param logo_path Location of logo image to add to plot
#' @param logo_position Position of logo on plot. Options are "top right", "top left", "bottom right", "bottom left"
#' @param logo_scale Scale of logo relative to plot width. Default is 10% of plot width
#' @param logo_padding Padding between logo and plot edge. Default is 1% of plot width
#'
#' @return An image file
#' @export
plot_add_logo <- function(plot_path, logo_path, logo_position, logo_scale = 10, logo_padding = 0.01){

  # from https://themockup.blog/posts/2019-01-09-add-a-logo-to-your-plot/
  # Requires magick R Package https://github.com/ropensci/magick

  # Can be a local or hosted image
  # pub_plot <- image_read("https://raw.githubusercontent.com/jthomasmock/tomtom/master/vignettes/basic_plot.png")
  #
  # logo <- image_read("https://www.rstudio.com/wp-content/uploads/2018/10/RStudio-Logo-Flat.png") %>%
  #   image_resize(300)
  #
  # print(pub_plot)

  # Useful error message for logo position
  if (!logo_position %in% c("top right", "top left", "bottom right", "bottom left")) {
    stop("Error Message: Uh oh! Logo Position not recognized\n  Try: logo_positon = 'top left', 'top right', 'bottom left', or 'bottom right'")
  }

  # read in raw images
  plot <- magick::image_read(plot_path)
  logo_raw <- magick::image_read(logo_path)

  # get dimensions of plot for scaling
  plot_height <- magick::image_info(plot)$height
  plot_width <- magick::image_info(plot)$width

  # default scale to 1/10th width of plot
  # Can change with logo_scale
  logo <- magick::image_scale(logo_raw, as.character(plot_width/logo_scale))

  # Get width of logo
  logo_width <- magick::image_info(logo)$width
  logo_height <- magick::image_info(logo)$height

  # Set position of logo
  # Position starts at 0,0 at top left
  # Using 0.01 for 1% - aesthetic padding
  # Use 0.0 for no padding - useful for logos that typically aren't square

  if (logo_position == "top right") {
    x_pos = plot_width - logo_width - logo_padding * plot_width
    y_pos = logo_padding * plot_height
  } else if (logo_position == "top left") {
    x_pos = logo_padding * plot_width
    y_pos = logo_padding * plot_height
  } else if (logo_position == "bottom right") {
    x_pos = plot_width - logo_width - logo_padding * plot_width
    y_pos = plot_height - logo_height - logo_padding * plot_height
  } else if (logo_position == "bottom left") {
    x_pos = logo_padding * plot_width
    y_pos = plot_height - logo_height - logo_padding * plot_height
  }

  # Compose the actual overlay
  magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))

}
