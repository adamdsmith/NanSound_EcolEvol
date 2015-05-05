#' Function to add a scale bar and (if requested) direction arrow.
#' 
#' @param ggobj The \code{ggplot} object to which the scale bar and orientation 
#'  arrow will be added
#' @param x Numeric scalar; the X coordinate of the bottom left of the full-sized scale bar bins; 
#'  see \code{n.maj.bins}.  
#' @param y Numeric scalar; the Y coordinate of the bottom left of the full-sized scale bar bins; 
#'  see \code{n.maj.bins}.
#' @param map.units Character; the units of the \code{ggobj}'s axes; defaults to meters ("m")
#' @param scalebar.units Character; the units of the \code{ggobj}'s scale bar.  Defaults to 
#'  kilometers ("km")
#' @param bin.width Numeric scalar; the width, in \code{map.units}, of each bin of the scale bar.  
#'  Default attempts to calculate a smart width for the user based on the X range of the \code{ggobj}
#'  and the number of minor and major bin widths specified.  If user elects to set it manually, 
#'  multiples of 1000 are recommended when \code{map.units} are in meters.
#' @param bin.height Numeric scalar; the height, in \code{map.units}, of the scale bar bins.  
#'  Defaults to 1/5 of the \code{bin.width}
#' @param n.maj.bins Numeric scalar; the number of full-sized bins of width \code{bin.width} to include. 
#'  Defaults to 1.
#' @param n.min.bins Numeric scalar; the number of half-sized bins of width \code{bin.width}/2 to include
#'  at the beginning of the scale bar. Defaults to 2.
#' @oaram legend.placement Character; indicates whether the user would like to allow the function
#'  to attempt a legend placement at the "bottom left" or "bottom right" of the map area.  The default
#'  is to use the user-specified \code{x} and \code{y} coordinates and not attempt a legend placement.
#'  \code{x} and \code{y} are overridden if \code{legend.placement} is changed from the default.
#' @param scale.text.pos Character; indicates desired position of scale bar text below ("bottom") or
#'  above ("top") the scale bar.  Currently not used; only below ("bottom") is supported.
#' @param scale.text.size Numeric scalar; adjusts text size of scale bar text.  Defaults to 5.
#' @param N.arrow Logical scalar; should an orientation arrow be added?
#' @param arrow.height Numeric scalar; the height of the orientation arrow.  Defaults to half of 
#'  the \code{bin.width}.  This default is overridden if the user allows the function to calculate the
#'  \code{bin.width}
#' @param arrow.size Numeric scalar; thickness of the arrow line.  Defaults to 1.
#' @param arrow.head.size Numerica scalar; size of the arrow head, in centimeters.  Defaults to 0.25.
#' @param arrow.text Character scalar; the text to superimpose on the center of the orientation 
#'  arrow.  Defaults to "N".
#' @param arrow.text.size Numeric scalar; adjusts text size of orientation arrow text.  Defaults to 6.

#' @section Warning:
#' At present, this function will not produce sensible scale bars with \code{ggobj}s that have
#' axes in geographic units (e.g., decimal degrees).

#' @return A \code{ggplot} object consisting on the input \code{ggobj} and added geometries
#'  (polygons, segments, annotations).

#' @examples

add_scale_bar <- function(ggobj, x, y, map.units = c("m", "km"), 
                          scalebar.units = c("km", "m"),
                          bin.width = NULL, bin.height = bin.width/10,
                          n.maj.bins = 1, n.min.bins = 2, 
                          legend.placement = c("", "bottom left", "bottom right"), 
                          scale.text.pos = c("bottom", "top"), scale.text.size = 5,
                          N.arrow = TRUE, arrow.height = bin.width/2, arrow.size = 1,
                          arrow.head.size = 0.25, arrow.text = "N", arrow.text.size = 6) {
  
  # Load required packages
  require(ggplot2); require(plyr); require(grid)
  
  # Modify how strings are handled 
  stringOption <- getOption("stringsAsFactors")
  options(stringsAsFactors = FALSE)
  
  # Set arguments with multiple options
  map.units <- match.arg(map.units)
  scalebar.units <- match.arg(scalebar.units)
  legend.placement <- match.arg(legend.placement)
  scale.text.pos <- match.arg(scale.text.pos)
  scale.text.pos <- match.arg(scale.text.pos)
  
  # Get X and Y range of ggobj for "smart" legend size and placement
  xrange = ggplot_build(ggobj)$panel$ranges[[1]]$x.range
  yrange = ggplot_build(ggobj)$panel$ranges[[1]]$y.range
  
  # Calculate X and Y location for legend based on `legend.placement`
  if (legend.placement == "bottom left") {
    leftX <- xrange[1]
    bottomY <- yrange[1]
    x <- leftX + 0.20 * diff(xrange)
    y <- bottomY + 0.05 * diff(yrange)
  } 
  if (legend.placement == "bottom right") {
    leftX <- xrange[1]
    bottomY <- yrange[1]
    x <- leftX + 0.80 * diff(xrange)
    y <- bottomY + 0.05 * diff(yrange)
  }  
  
  # Set "smart" bin size
  total.bw <- ifelse(map.units == "m",
                     round(diff(xrange)/3, -3),
                     round(diff(xrange)/3, 0))
  n.bin.divs <- n.min.bins + n.maj.bins * 2
  if(is.null(bin.width)) {
    min.bin.width <- total.bw/n.bin.divs
    bin.width <- min.bin.width * 2
    bin.height <- bin.width/10
    arrow.height <- total.bw/4
    }
    
  # Top of scale bar
  bin.top <- y + bin.height

  # Define coordinates for major scale bar bins

  # Empty data frame to catch coordinates
  maj.bins <- data.frame(bin = character(), x = numeric(), y = numeric(), fill = character())
    
  # Iterate through major bins
  if (n.maj.bins > 0) {
    for (bin in seq(n.maj.bins)) {
      bLeft <- c(x + (bin - 1) * bin.width, y)
      tLeft <- c(x + (bin - 1) * bin.width, bin.top)
      tRight <- c(x + bin * bin.width, bin.top)
      bRight <- c(x + bin * bin.width, y)
      allX <- c(bLeft[1], tLeft[1], tRight[1], bRight[1])
      allY <- c(bLeft[2], tLeft[2], tRight[2], bRight[2])
      
      # Add bin label and fill information
      bin.label <- rep(paste0("Major", bin), 4)
      fill <- rep(ifelse(bin %% 2 != 0, "white", "black"), 4)
      tmp.df <- data.frame(bin = bin.label, x = allX, y = allY, fill)
      maj.bins <- rbind(maj.bins, tmp.df)
    }
  }
  
  # Define coordinates for minor scale bar bins
  
  # Empty data frame to catch coordinates
  min.bins <- data.frame(bin = character(), x = numeric(), y = numeric(), fill = character())
  
  # Iterate through minor bins
  if (n.min.bins > 0) {
    for (bin in seq(n.min.bins)) {
      bRight <- c(x - (bin - 1) * bin.width/2, y)
      tRight <- c(x - (bin - 1) * bin.width/2, bin.top)
      tLeft <- c(x - bin * bin.width/2, bin.top)
      bLeft <- c(x - bin * bin.width/2, y)
      allX <- c(bLeft[1], tLeft[1], tRight[1], bRight[1])
      allY <- c(bLeft[2], tLeft[2], tRight[2], bRight[2])
      
      # Add bin label and fill information
      bin.label <- paste0("Minor", bin)
      fill <- ifelse(bin %% 2 != 0, "black", "white")
      tmp.df <- data.frame(bin = bin.label, x = allX, y = allY, fill)
      min.bins <- rbind(min.bins, tmp.df)
    }
  }
  
  # Consolidate all bin coordinates
  all.bins <- rbind(maj.bins, min.bins)
  
  # Colors for manual color scale
  color.codes <- unique(all.bins[, c("bin", "fill")])
    

  # Define coordinates and labels for scale bar annotation
  scale.text <- ddply(all.bins, .(bin), summarise,
                      x = head(x, 1),
                      y = head(y, 1)
                      )
  # Move the text below the scale bar
  scale.text$y <- scale.text$y - bin.height/5

  # Add scale bar endpoint
  if (n.maj.bins > 0) {
    scale.text <- rbind(scale.text,
                        data.frame(bin = "Endpoint", x = max(scale.text$x) + bin.width,
                                   y = mean(scale.text$y)))
  } else {
    scale.text <- rbind(scale.text,
                        data.frame(bin = "Endpoint", x = max(scale.text$x) + bin.width/2,
                                   y = mean(scale.text$y)))
  }
  
  # Adjust for match/mismatch between `map.units` and `scalebar.units`
  minX <- min(scale.text$x)
  if (map.units == scalebar.units) {
    scale.text <- mutate(scale.text,
                         label = as.character(round(x - minX, 2))
                         )
    addUnits <- scale.text[scale.text$bin == "Endpoint", "label"]
    scale.text[scale.text$bin == "Endpoint", "label"] <- paste(addUnits, scalebar.units)
  } else {
    if (map.units == "m") {
      scale.text <- mutate(scale.text,
                           label = as.character( round((x - minX) / 1000, 2))
      )
      addUnits <- scale.text[scale.text$bin == "Endpoint", "label"]
      scale.text[scale.text$bin == "Endpoint", "label"] <- paste(addUnits, scalebar.units)
    } else {
      scale.text <- mutate(scale.text,
                           label = as.character( round((x - minX) * 1000))
      )
      addUnits <- scale.text[scale.text$bin == "Endpoint", "label"]
      scale.text[scale.text$bin == "Endpoint", "label"] <- paste(addUnits, scalebar.units)
    }
  }
  
  new.plot <- ggobj + 
    
    # Add scale bar
    geom_polygon(data = all.bins, aes(x = x, y = y, group=bin, fill=fill), colour = "black") +
      scale_fill_manual(values=color.codes$fill, breaks = color.codes$bin) + 
  
    # Add scale bar annotation
    geom_text(data = scale.text, aes(x = x, y = y, label = label),
                                   vjust = 1, size = scale.text.size)
  
  # Add orientation arrow, if requested
  
  if (N.arrow) {
    # Get arrow start and end coordinates
    arrowx <- mean(c(max(all.bins$x), min(all.bins$x)))
    arrow.df <- data.frame(arrowx = arrowx,
                           xend = arrowx,
                           arrowy = max(all.bins$y) + bin.height,
                           yend = max(all.bins$y) + bin.height + arrow.height,
                           ymid = max(all.bins$y) + bin.height + arrow.height/2)
  
    new.plot <- new.plot + geom_segment(data = arrow.df, 
                                        aes(x = arrowx, xend = xend, y = arrowy, yend = yend),
                                        arrow = arrow(length = unit(arrow.head.size, "cm")),
                                        size = arrow.size) +
      annotate("text", x = arrow.df$arrowx, y = arrow.df$ymid, 
               label = arrow.text, size = arrow.text.size)
  }
  
  options(stringsAsFactors = stringOption)
  
  new.plot
  
}
  
    