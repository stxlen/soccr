#' Creating a pitch plot using ggplot2 that is ready to plot actions on top. Numerous options allow customisation.
#'
#' @param pitch_length Length of the pitch. Always the long side. Default 105
#' @param pitch_width Width of the pitch. Default 68
#' @param pitch_units Units of measure in "meters" or "yards". Default "meters"
#' @param pitch_layout Plot the pitch vertical or horizontal. Default "horizontal"
#' @param pitch_section Full or subsection of field. "full", "att_half". Default "full"
#' @param grass_colour A string as a colour by word or #HEX code
#' @param line_colour A string as a colour by word or #HEX code
#' @param background_colour A string as a colour by word or #HEX code
#' @param goal_colour A string as a colour by word or #HEX code
#' @param goaltype String, either "line", "box" or "barcanumbers". Default "line"
#' @param middlethird Boolean. Default FALSE
#' @param BasicFeatures Boolean. Default FALSE
#' @param overlay Draw lines on top of the field. "JdeP" "grid_30" or FALSE
#' @param padding Numeric. Default = 5
#' @return A plot of a pitch
#' @export
draw_pitch <- function(pitch_length = 105,
                       pitch_width = 68,
                       pitch_units = "meters",
                       pitch_layout = "horizontal",
                       pitch_section = "full",
                       grass_colour = "#ffffff",
                       line_colour = "#A9A9A9",
                       background_colour = "#ffffff",
                       goal_colour = "#000000",
                       goaltype = "line",
                       middlethird = FALSE,
                       BasicFeatures = FALSE,
                       overlay = FALSE,
                       arcs = TRUE,
                       padding = 5){

  ## set theme for blank pitch
  theme_blankPitch = function(size=12) {
    theme(
      #axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      #axis.ticks.y=element_text(size=size),
      #   axis.ticks=element_blank(),
      axis.ticks.length=unit(0, "lines"),
      #axis.ticks.margin=unit(0, "lines"),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.background=element_rect(fill=background_colour, colour=NA),
      legend.key=element_rect(colour=background_colour,fill=background_colour),
      legend.key.size=unit(1.2, "lines"),
      legend.text=element_text(size=size),
      legend.title=element_text(size=size, face="bold",hjust=0),
      strip.background = element_rect(colour = background_colour, fill = background_colour, size = .5),
      panel.background=element_rect(fill=background_colour,colour=background_colour),
      #       panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.spacing=element_blank(),
      plot.background=element_blank(),
      #plot.margin=unit(c(0, 0, 0, 0), "lines"),
      plot.title=element_text(size=size*1.2),
      strip.text.y=element_text(colour=background_colour,size=size,angle=270),
      strip.text.x=element_text(size=size*1))}
  if(pitch_section == "full"){
    ymin <- 0 # minimum width
    ymax <- pitch_width # maximum width
    xmin <- 0 # minimum length
    xmax <- pitch_length # maximum length
  }else if (pitch_section == "att_half"){
    ymin <- 0 # minimum width
    ymax <- pitch_width # maximum width
    xmin <- (pitch_length/2) # minimum length
    xmax <- pitch_length # maximum length
  }

  # Defining features along the length (x axis)
  boxEdgeDef <- 16.5
  boxEdgeOff <- xmax - 16.5
  halfwayline <- xmax/2
  sixYardDef <- 5.5
  sixYardOff <- xmax-5.5
  penSpotDef <- 11
  penSpotOff <- xmax-11

  # Defining features along the width (y axis)
  boxEdgeLeft <- ymax/2 - 20.15 #18
  boxEdgeRight <- ymax/2 + 20.15
  sixYardLeft <- ymax/2 - 9.16
  sixYardRight <- ymax/2 + 9.16
  goalPostLeft <- ymax/2 - 3.66
  goalPostRight <- ymax/2 + 3.66
  CentreSpot <- ymax/2

  # other dimensions
  centreCirle_d <- 18.3

  ## define the circle function
  circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }

  #### create center circle ####
  center_circle <- circleFun(c(halfwayline,CentreSpot),centreCirle_d,npoints = 100)

  #### create leftD arc ####
  dArcDef <- circleFun(c(penSpotDef,CentreSpot),centreCirle_d,npoints = 1000)
  ## remove part that is in the box
  dArcDef <- dArcDef[which(dArcDef$x >= (boxEdgeDef)),]

  #### create rightD arc ####
  dArcOff <- circleFun(c(penSpotOff,CentreSpot),centreCirle_d,npoints = 1000)
  ## remove part that is in the box
  dArcOff <- dArcOff[which(dArcOff$x <= (boxEdgeOff)),]

  if(pitch_layout == "horizontal"){
    ### FIRST STAGE
    ## initiate the plot, set some boundries to the plot
    list1 <- list (xlim(c(xmin-padding,xmax+padding)),
                   ylim(c(ymin-padding,ymax+padding)),
                   # add the theme
                   theme_blankPitch()
    )

    ### ADD MIDDLE THIRD SHADING
    if(middlethird == TRUE){
      list2 <- list(geom_rect(aes(xmin=(xmax/3*1), xmax=(xmax/3*2), ymin=ymin, ymax=ymax), colour = NA, fill = "black", alpha = 0.10)
      )
    }else{
      list2 <- list()
    }

    if(BasicFeatures == TRUE){
      list3 <- list(
        # add the base rectangle of the pitch
        geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill = NA, colour = line_colour),
        # add the 18 yard box defensive
        geom_rect(aes(xmin=xmin, xmax=boxEdgeDef, ymin=boxEdgeLeft, ymax=boxEdgeRight), fill = grass_colour, colour = line_colour),
        # add the 18 yard box offensive
        geom_rect(aes(xmin=boxEdgeOff, xmax=xmax, ymin=boxEdgeLeft, ymax=boxEdgeRight), fill = grass_colour, colour = line_colour),
        # add halway line
        geom_segment(aes(x = halfwayline, y = ymin, xend = halfwayline, yend = ymax),colour = line_colour),
        arcs = FALSE
      )
    }else{
      ## initiate the plot, set some boundries to the plot
      list3 <- list(
        # add the base rectangle of the pitch
        geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill = NA, colour = line_colour),
        # add the 18 yard box defensive
        geom_rect(aes(xmin=xmin, xmax=boxEdgeDef, ymin=boxEdgeLeft, ymax=boxEdgeRight), fill = grass_colour, colour = line_colour),
        # add the 18 yard box offensive
        geom_rect(aes(xmin=boxEdgeOff, xmax=xmax, ymin=boxEdgeLeft, ymax=boxEdgeRight), fill = grass_colour, colour = line_colour),
        # add halway line
        geom_segment(aes(x = halfwayline, y = ymin, xend = halfwayline, yend = ymax),colour = line_colour),
        # add the six yard box Defensive
        geom_rect(aes(xmin=xmin, xmax=sixYardDef, ymin=sixYardLeft, ymax=sixYardRight), fill = grass_colour, colour = line_colour),
        # add the six yard box offensive
        geom_rect(aes(xmin=sixYardOff, xmax=xmax, ymin=sixYardLeft, ymax=sixYardRight), fill = grass_colour, colour = line_colour),
        # add centre circle
        geom_path(data=center_circle, aes(x=x,y=y), colour = line_colour),
        # add penalty spot left
        geom_point(aes(x = penSpotDef , y = CentreSpot), colour = line_colour, size = 0.75),
        # add penalty spot right
        geom_point(aes(x = penSpotOff , y = CentreSpot), colour = line_colour, size = 0.75),
        # add centre spot
        geom_point(aes(x = halfwayline , y = CentreSpot), colour = line_colour, size = 0.75)
      )}

    #### add goals depending on type

    ## LINE TYPE
    if(goaltype == "line"){
      list4 <-
        if(pitch_section == "full"){
          list(
            # add the goal Defensive
            geom_segment(aes(x = xmin, y = goalPostLeft, xend = xmin, yend = goalPostRight),colour = goal_colour, size = 1),
            # add the goal offensive
            geom_segment(aes(x = xmax, y = goalPostLeft, xend = xmax, yend = goalPostRight),colour = goal_colour, size = 1)
          )
        }else if(pitch_section == "att_half"){
          list(
            # add the goal offensive
            geom_segment(aes(x = xmax, y = goalPostLeft, xend = xmax, yend = goalPostRight),colour = goal_colour, size = 1)
          )
        }
      # add the goal offensive
      geom_segment(aes(x = xmax, y = goalPostLeft, xend = xmax, yend = goalPostRight),colour = goal_colour, size = 1)
    }else{}

    ## Barca Numbers TYPE
    if(goaltype == "barcanumbers"){
      list4 <- list(
        # add the goal Defensive
        geom_segment(aes(x = xmin - 0.75, y = goalPostLeft, xend = xmin - 0.75, yend = goalPostRight),colour = line_colour, size = 0.75),
        # add the goal offensive
        geom_segment(aes(x = xmax + 0.75, y = goalPostLeft, xend = xmax + 0.75, yend = goalPostRight),colour = line_colour, size = 0.75)
      )
    }else{}

    ## BOX TYPE
    if(goaltype == "box"){
      list5 <- list(
        # add the goal Defensive
        geom_rect(aes(xmin = xmin - 2 , ymin = goalPostLeft, xmax = xmin, ymax = goalPostRight), fill = grass_colour, colour = line_colour),
        # add the goal offensive
        geom_rect(aes(xmin = xmax, ymin = goalPostLeft, xmax = xmax + 2, ymax = goalPostRight), fill = grass_colour, colour = line_colour)
      )
    }else{
      list5 <- list()
    }


    ## add overlay
    if(overlay == "JdeP"){
      list6 <- list(
        # vertical tram lines
        geom_segment(aes(x = boxEdgeDef, y = boxEdgeLeft, xend = boxEdgeOff, yend = boxEdgeLeft), colour = "#941C07", alpha = 0.3),
        geom_segment(aes(x = boxEdgeDef, y = boxEdgeRight, xend = boxEdgeOff, yend = boxEdgeRight), colour = "#941C07", alpha = 0.3),
        geom_segment(aes(x = boxEdgeDef, y = CentreSpot - 9.15, xend = boxEdgeOff, yend = CentreSpot - 9.15), colour = "#941C07", alpha = 0.3),
        geom_segment(aes(x = boxEdgeDef, y = CentreSpot + 9.15, xend = boxEdgeOff, yend = CentreSpot + 9.15), colour = "#941C07", alpha = 0.3),
        # horizontal tram lines
        geom_segment(aes(x = boxEdgeDef, y = ymin, xend = boxEdgeDef, yend = ymax), colour = "#941C07", alpha = 0.3),
        geom_segment(aes(x = boxEdgeOff, y = ymin, xend = boxEdgeOff, yend = ymax), colour = "#941C07", alpha = 0.3),
        geom_segment(aes(x = (xmax/3*1), y = boxEdgeRight, xend = (xmax/3*1), yend = ymax), colour = "#941C07", alpha = 0.3),
        geom_segment(aes(x = (xmax/3*1), y = boxEdgeLeft, xend = (xmax/3*1), yend = ymin), colour = "#941C07", alpha = 0.3),
        geom_segment(aes(x = (xmax/3*2), y = boxEdgeRight, xend = (xmax/3*2), yend = ymax), colour = "#941C07", alpha = 0.3),
        geom_segment(aes(x = (xmax/3*2), y = boxEdgeLeft, xend = (xmax/3*2), yend = ymin), colour = "#941C07", alpha = 0.3)
        # add the 18 yard box defensive
        #geom_rect(aes(xmin=xmin, xmax=boxEdgeDef, ymin=boxEdgeLeft, ymax=boxEdgeRight), fill = NA, colour = line_colour) +
        # add the 18 yard box offensive
        #geom_rect(aes(xmin=boxEdgeOff, xmax=xmax, ymin=boxEdgeLeft, ymax=boxEdgeRight), fill = NA, colour = line_colour)
      )
    }else if(overlay == "grid_30"){
      list6 <- list(
        # lines along the length
        geom_segment(aes(x = xmin, y = (ymax/5*1), xend = xmax, yend = (ymax/5*1)), colour = "#941C07", alpha = 0.3),
        geom_segment(aes(x = xmin, y = (ymax/5*2), xend = xmax, yend = (ymax/5*2)), colour = "#941C07", alpha = 0.3),
        geom_segment(aes(x = xmin, y = (ymax/5*3), xend = xmax, yend = (ymax/5*3)), colour = "#941C07", alpha = 0.3),
        geom_segment(aes(x = xmin, y = (ymax/5*4), xend = xmax, yend = (ymax/5*4)), colour = "#941C07", alpha = 0.3),
        # lines along the width
        geom_segment(aes(x = (xmax/6*1), y = ymin, xend = (xmax/6*1), yend = ymax), colour = "#941C07", alpha = 0.3),
        geom_segment(aes(x = (xmax/6*2), y = ymin, xend = (xmax/6*2), yend = ymax), colour = "#941C07", alpha = 0.3),
        geom_segment(aes(x = (xmax/6*3), y = ymin, xend = (xmax/6*3), yend = ymax), colour = "#941C07", alpha = 0.3),
        geom_segment(aes(x = (xmax/6*4), y = ymin, xend = (xmax/6*4), yend = ymax), colour = "#941C07", alpha = 0.3),
        geom_segment(aes(x = (xmax/6*5), y = ymin, xend = (xmax/6*5), yend = ymax), colour = "#941C07", alpha = 0.3)
      )
    }else{
      list6 <- list()
    }

    ## add arcs
    # if(arcs == TRUE){
    #   list7 <- list(
    #     # vertical tram lines
    #     annotate("path",
    #              x = 11 + 9.15 * cos(seq(-0.3*pi, 0.3*pi, length.out = 30)),
    #              y = ymax/2 + 9.15 * sin(seq(-0.3*pi, 0.3*pi, length.out = 30)),
    #              col = line_colour),
    #     annotate("path",
    #              x = (xmax-11) - 9.15 * cos(seq(-0.3*pi, 0.3*pi, length.out = 30)),
    #              y = ymax/2 + 9.15 * sin(seq(-0.3*pi, 0.3*pi, length.out = 30)),
    #              col = line_colour)
    #   )
    # }else{
    #   list7 <- list()
    # }

    list7 <- list(# add d arc offensive
      geom_path(data=dArcOff, aes(x=x,y=y), colour = line_colour),
      # add d arc defensive
      geom_path(data=dArcDef, aes(x=x,y=y), colour = line_colour))
  }else{

    ## VERTICAL PITCH

    list1 <- list (xlim(c(ymin-padding,ymax+padding)),
                   ylim(c(xmin-padding,xmax+padding)),
                   # add the theme
                   theme_blankPitch())
    list2 <- list()

    if(BasicFeatures == TRUE){
      list3 <- list(
        # add the base rectangle of the pitch
        geom_rect(aes(xmin=ymin, xmax=ymax, ymin=xmin, ymax=xmax), fill = NA, colour = line_colour),
        # add the 18 yard box defensive
        geom_rect(aes(xmin=boxEdgeLeft, xmax=boxEdgeRight, ymin=xmin, ymax=boxEdgeDef), fill = grass_colour, colour = line_colour),
        # add the 18 yard box offensive
        geom_rect(aes(xmin=boxEdgeLeft, xmax=boxEdgeRight, ymin=boxEdgeOff, ymax=xmax), fill = grass_colour, colour = line_colour),
        # add halway line
        geom_segment(aes(x = ymin, y = halfwayline, xend = ymax, yend = halfwayline),colour = line_colour)
        # arcs = FALSE
      )
    }else{
      ## initiate the plot, set some boundries to the plot
      list3 <- list(
        # add the base rectangle of the pitch
        geom_rect(aes(xmin=ymin, xmax=ymax, ymin=xmin, ymax=xmax), fill = NA, colour = line_colour),
        # add the 18 yard box defensive
        geom_rect(aes(xmin=boxEdgeLeft, xmax=boxEdgeRight, ymin=xmin, ymax=boxEdgeDef), fill = grass_colour, colour = line_colour),
        # add the 18 yard box offensive
        geom_rect(aes(xmin=boxEdgeLeft, xmax=boxEdgeRight, ymin=boxEdgeOff, ymax=xmax), fill = grass_colour, colour = line_colour),
        # add halway line
        geom_segment(aes(x = ymin, y = halfwayline, xend = ymax, yend = halfwayline),colour = line_colour),
        # add the six yard box Defensive
        geom_rect(aes(xmin=sixYardLeft, xmax=sixYardRight, ymin=xmin, ymax=sixYardDef), fill = grass_colour, colour = line_colour),
        # add the six yard box offensive
        geom_rect(aes(xmin=sixYardLeft, xmax=sixYardRight, ymin=sixYardOff, ymax=xmax), fill = grass_colour, colour = line_colour),
        # add centre circle
        geom_path(data=center_circle, aes(x=y,y=x), colour = line_colour),
        # # add penalty spot defensive
        geom_point(aes(x = CentreSpot , y = penSpotDef), colour = line_colour),
        # add penalty spot offensive
        geom_point(aes(x = CentreSpot , y = penSpotOff), colour = line_colour),
        # # add centre spot
        geom_point(aes(x = CentreSpot , y = halfwayline), colour = line_colour)
      )}
    list4 <- list()
    list5 <- list()
    list6 <- list()
    list7 <- list(# add d arc offensive
                  geom_path(data=dArcOff, aes(x=y,y=x), colour = line_colour),
                  # add d arc defensive
                  geom_path(data=dArcDef, aes(x=y,y=x), colour = line_colour))
  }

  list8 <- list(coord_fixed()# maintain aspect ration when resized
  )
  p <- c(list1, list2, list3, list4, list5, list6, list7, list8)
  return(p)

}

# Example usage
# ggplot() + draw_pitch(pitch_layout = "vertical")
# ggplot() + draw_pitch()
# ggplot() + draw_pitch(pitch_section = "att_half")
# ggplot() + draw_pitch(pitch_layout = "vertical", BasicFeatures = TRUE)
# ggplot() + draw_pitch(pitch_layout = "vertical", pitch_section = "att_half", BasicFeatures = FALSE)
