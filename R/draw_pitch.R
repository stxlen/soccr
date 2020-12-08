#' Create a pitch plot ready for Statsbomb data
#'
#' Creating a pitch plot using ggplot2 that is ready to plot actions on top. Numerous options allow customisation.
#'
#' @param layout Plot the pitch vertical or horizontal. Default "horizontal"
#' @param grass_colour A string as a colour by word or #HEX code
#' @param line_colour A string as a colour by word or #HEX code
#' @param background_colour A string as a colour by word or #HEX code
#' @param goal_colour A string as a colour by word or #HEX code
#' @param goaltype String, either "line", "box" or "barcanumbers". Default "line"
#' @param middlethird Boolean. Default FALSE
#' @param BasicFeatures Boolean. Default FALSE
#' @param JdeP Boolean. Default FALSE
#' @param padding Numeric. Default = 5
#' @return A plot of a pitch
#' @export
draw_pitch <- function(layout = "horizontal", grass_colour = "#ffffff", line_colour = "#A9A9A9", background_colour = "#ffffff", goal_colour = "#000000", goaltype = "line", middlethird = FALSE, BasicFeatures = FALSE, JdeP = TRUE, arcs = TRUE, padding = 5){

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

  ymin <- 0 # minimum width
  ymax <- 68 # maximum width
  xmin <- 0 # minimum length
  xmax <- 105 # maximum length

  # Defining features along the length
  boxEdgeDef <- 16.5
  boxEdgeOff <- xmax - 16.5
  halfwayline <- xmax/2
  sixYardDef <- 5.5
  sixYardOff <- xmax-5.5
  penSpotDef <- 11
  penSpotOff <- xmax-11

  # Defining features along the width
  boxEdgeLeft <- ymax/2 - 20.15 #18
  boxEdgeRight <- ymax/2 + 20.15
  sixYardLeft <- ymax/2 - 9.16
  sixYardRight <- ymax/2 + 9.16
  goalPostLeft <- ymax/2 - 3.66
  goalPostRight <- ymax/2 + 3.66
  CentreSpot <- ymax/2

  # other dimensions
  centreCirle_d <- 20

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
    list4 <- list(
      # add the goal Defensive
      geom_segment(aes(x = xmin, y = goalPostLeft, xend = xmin, yend = goalPostRight),colour = goal_colour, size = 1),
      # add the goal offensive
      geom_segment(aes(x = xmax, y = goalPostLeft, xend = xmax, yend = goalPostRight),colour = goal_colour, size = 1)
    )
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


  ## add J de P
  if(JdeP == TRUE){
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

  list8 <- list(coord_fixed()# maintain aspect ration when resized
                )
  p <- c(list1, list2, list3, list4, list5, list6, list7, list8)
  return(p)

}

ggplot() + draw_pitch()