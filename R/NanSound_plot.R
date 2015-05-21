NanSound_plot <- function(data = NULL, x = "x", y = "y", z = "z",
                          cut = TRUE, hotspot = NULL, effort = "obs_window",
                          segs = TRUE, plotwind = FALSE,
                          legend.title = "", label = NULL,
                          type = c("occupancy", "count")) {
  
  ## effort is not used at this point; may use it to "scale up" results
  ## from overall mean abundance to the whole segment
  
  instant_pkgs("scales")
  
  theme_set(theme_bw(base_size = 15))
  theme_update(plot.margin = unit(c(0, 0.05, -0.85, -0.85),"line"),
               panel.grid.minor = element_blank(),
               panel.grid.major= element_blank(),
               panel.border=element_rect(fill=NA),
               panel.background= element_blank(),
               axis.ticks=element_blank(),
               axis.title.x=element_blank(),
               axis.title.y=element_blank(),
               axis.text.x=element_blank(),
               axis.text.y=element_blank(),
               legend.position = 'none')
  
  type <- match.arg(type)
  
  ## Load polygons if necessary
  if (!exists("seg_poly")) seg_poly <- readOGR("../GIS/Ancillary", "seg_poly")
  if (!exists("MA")) MA <- readOGR("../GIS/Ancillary", "MA_bg")
  if (!exists("wind")) wind <- spTransform(readOGR("../GIS/Ancillary", "CW_boundary"), raster:::crs(MA))
  
  if (is.null(data)) stop("You must specify the data frame containing the predictions to display.")
  
  ## Create data frame for predictions
  tmpDat <- data.frame(x = data[, x], y = data[, y], z = data[, z])
  
  if (!is.null(hotspot)) {
    # Store "hotspot" cutoff for later
    threshold <- quantile(tmpDat$z, probs = 1 - hotspot/100)
  }
    
  if (cut) {
    breaks <- quantile(tmpDat$z, probs = c(0, 0.25, 0.5, 0.75, 1))
    if(type == "occupancy") {
      digits <- ifelse(min(breaks) < 0.1, 1, 2)
    } else {
      digits <- 0
    }
    if(grepl("mad_m", z)) digits <- 0 # Round MAD/median to nearest whole regardless
    
    tmpDat$z <- cut2(tmpDat$z, cuts = breaks, digits = digits)
    levels(tmpDat$z) <- gsub(",", ", ", gsub(" ", "", levels(tmpDat$z)))
  }
  
  # Set map x and y limits
  xlims <- c(374500, 429700)
  ylims <- c(4569125, 4615125)
  
  p <- 
    ggplot() + 
    geom_polygon(data=MA, aes(long, lat, group=group), colour = element_blank(), fill="gray85") +
    geom_tile(data=tmpDat, aes(x=x, y=y, fill=z)) +
    coord_equal() +
    coord_cartesian(xlim = xlims, ylim = ylims) + 
    theme(legend.justification=c(0,0.5), 
          legend.position=c((416000 - xlims[1])/diff(xlims), 0.5)) # enter where legend should start

  if (cut) {
    p <- p + scale_fill_brewer(legend.title, palette = "OrRd") 
  } else if (type == "occupancy") {
      p <- p + scale_fill_gradient(legend.title, low = "#fef0d9", high = "#d7301f", limits = c(0,1)) 
    } else {
      p <- p + scale_fill_gradient(legend.title, low = "#fef0d9", high = "#d7301f") 
    }
  
  # Overlay segment boundaries
  if (segs) p <- p + geom_polygon(data=seg_poly, aes(long, lat, group=group), colour = "gray30", alpha=0)
  
  # Highlight hotspots, if requested
  if (!is.null(hotspot)) {
    which_segs <- data$seg[which(data[, z] >= threshold)]
    p <- p + geom_polygon(data = subset(seg_poly, seg %in% which_segs), 
                          aes(long, lat, group=group), colour = "gray20", size=1.25, alpha=0)
  }
    
  # Plot Horseshoe Shoals wind energy zone
  if (plotwind) p <- p + geom_polygon(data=wind, aes(long, lat, group=group), 
                                      colour="black", size = 1.5, alpha=0)
  
  if (!is.null(label)) p <- p + annotate("text", x = xlims[1] + 0.02*diff(xlims), 
                                         y = ylims[2] - 0.02*diff(ylims), label = label,
                                         hjust=0, vjust=1, size=12)
  
  return(p)

}
