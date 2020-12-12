#' Split a field into equal parts
#'
#' @param n_columns Number of columns
#' @param n_rows Number of rows
#' @return A tibble with zone definitions
#' @export

create_grid <- function(pitch_length = pitch_length_init,
                        pitch_width = pitch_width_init,
                        n_columns=6, n_rows=5){
  grid_size <- n_columns * n_rows
  df_rows <- grid_size * 5
  grid_width <- pitch_length / n_columns
  grid_height <- pitch_width / n_rows

  # create x list
  x <- 0
  x_list <- c()
  y_list <- c()
  pos <- 1
  for(i in 1:n_columns){
    for(i in 1:n_rows){
      for(i in 1:5){
        if(i==1 | i==2 | i==5){
          output <- x
        }else{
          output <- x+grid_width
        }
        x_list[pos] <- output
        pos <- pos + 1
      }
    }
    x <- x + grid_width
  }

  # create a y list
  y <- 0
  y_list <- c()
  pos <- 1
  for(i in 1:n_rows){
    for(i in 1:n_columns){
      for(i in 1:5){
        if(i==1 | i==4 | i==5){
          output <- y
        }else{
          output <- y + grid_height
        }
        y_list[pos] <- output
        pos <- pos + 1
      }
      y <- y + grid_height
      if(y == pitch_width){
        y <- 0
      }else{}
    }
  }

  # combine them all
  pitch_grid <- tibble(zone_num = c(rep(1:grid_size, each = 5)),
                       x_list, y_list)

  return(pitch_grid)
}

# Example usage
# my_grid <- create_grid(120, 75, 5, 6)
