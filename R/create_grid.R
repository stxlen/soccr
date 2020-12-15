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
      if(y >= pitch_width - grid_height){
        y <- 0
      }else{}
    }
  }

  # combine and turn into sf object
  pitch_grid <- tibble(zone_num = c(rep(1:grid_size, each = 5)),
                       x_list, y_list) %>%
    rename(x_coord = x_list,
           y_coord = y_list) %>%
    st_as_sf(coords = c("x_coord", "y_coord")) %>%
    group_by(zone_num) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")

  # add center locations
  pitch_grid <- pitch_grid %>%
    st_coordinates() %>%
    as_tibble() %>%
    filter(row_number() %% 5 != 1) %>% ## Closed polygon repeat first values. Delete every 5th row starting from 1
    group_by(L2) %>%
    summarise(center_x = mean(X),
              center_y = mean(Y)) %>%
    rename(zone_num = L2) %>%
    left_join(pitch_grid, .)

  return(pitch_grid)
}

# Example usage
# my_grid <- create_grid(120, 75, 5, 6)
