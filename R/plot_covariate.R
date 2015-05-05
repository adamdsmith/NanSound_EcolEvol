# Install (if necessary) and load required packages
toLoad <- c("reshape2", "plyr", "dplyr", "rgdal", 
            "grid", "lubridate", "ggplot2")
instant_pkgs(toLoad); rm(toLoad)

# Requires raster
# Load required shapefiles
if (!exists("seg_poly")) seg_poly <- readOGR("../GIS/Ancillary", "seg_poly")
if (!exists("MA")) MA <- readOGR("../GIS/Ancillary", "MA_bg")
if (!exists("wind")) wind <- spTransform(readOGR("../GIS/Ancillary", "CW_boundary"), raster:::crs(MA))

plot_covariate <- function(z = "depth", data = env.segs, x = "x", y = "y", 
                           plotwind = FALSE, segs = TRUE, winter = "winter", 
                           month = "date") {
  
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
  
  ## Create data frame
  tmpDat <- data.frame(x = data[, x], y = data[, y], z = data[, z],
                       winter = data[, winter], 
                       month = factor(month(data[, month]),
                                      levels = c(10:12, 1:4),
                                      labels = c(month.abb[10:12], month.abb[1:4])))
  
  # Check to see if the variable is dynamic and stop if not requested to display
  varybymonth <- any((tmpDat %>% group_by(x, y, winter) %>% summarise(sdev = sd(z)))$sdev > 0, na.rm=TRUE)
  varybywinter <- any((tmpDat %>% group_by(x, y, month) %>% summarise(sdev = sd(z)))$sdev > 0, na.rm=TRUE)
  if(any(varybymonth, varybywinter)) {
    dynamic = TRUE
  } else {
    dynamic = FALSE
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
  
  if(is.factor(tmpDat[, "z"])) {
    p <- p + scale_fill_brewer(z, palette = "OrRd") 
  } else {
    p <- p + scale_fill_gradient(z, low = "#fef0d9", high = "#d7301f") 
  }
  
  if (segs) p <- p + geom_polygon(data=seg_poly, aes(long, lat, group=group), colour = "gray30", alpha=0)
  
  if (plotwind) p <- p + geom_polygon(data=wind, aes(long, lat, group=group), 
                                      colour="black", size = 1.5, alpha=0)
  
  if(dynamic) {
    if(varybywinter) {
      if(varybymonth) {
        p <- p + facet_grid(winter ~ month) + theme(legend.position = "right")
      } else {
        p <- p + facet_grid(. ~ winter) + theme(legend.position = "right")
      }
    } else {
      if (varybymonth) p <- p + facet_grid(. ~ month) + theme(legend.position = "right")
    }
  }
  
  p
  
}