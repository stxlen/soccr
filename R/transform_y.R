#' Transform wyscout y coordinate percent to y coordinate (yards)
#'
#' @param y_coordinate y coordinate in percent.
#' @param att_direction Attacking direction of the team. Default is "E" (East).
#' @return Transformed y coordinate
#' @export

transform_y <- function(y_coordinate, att_direction = "E"){

  transformed_y <- dplyr::case_when(
    y_coordinate <= 19 ~ (y_coordinate / 19) * 18,
    y_coordinate <= 37 & y_coordinate > 19 ~ ((y_coordinate - 19) / 18 * 12) + 18,
    y_coordinate <= 63 & y_coordinate > 37 ~ ((y_coordinate - 37) / 26) * 20 + 30,
    y_coordinate <= 81 & y_coordinate > 63 ~ ((y_coordinate - 63) / 18) * 12 + 50,
    y_coordinate <= 100 & y_coordinate > 81 ~ ((y_coordinate - 81) / 19) * 18 + 62,
    TRUE ~ NA
  )

  if(att_direction == "E"){
    break
  }else if(att_direction == "W"){
    transformed_y <- 100 - transformed_y
  }else if(att_direction == "N"){
    break
  }else if(att_direction == "S"){
    break
  }else{
    stop("att_direction must be one of E, W, N, S")
  }


  return(transformed_y)
}
