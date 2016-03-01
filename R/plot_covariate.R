# Install (if necessary) and load required packages
toLoad <- c("reshape2", "plyr", "dplyr", "rgdal", 
            "grid", "lubridate", "ggplot2")
sapply(toLoad, require, character.only = TRUE); rm(toLoad)

# Requires raster
# Load required data shapefiles
if (!exists("env.segs")) load("../Data/ducks&environment.RData")
if (!exists("seg_poly")) seg_poly <- readOGR("../GIS/Ancillary", "seg_poly", verbose=FALSE)
if (!exists("MA")) MA <- readOGR("../GIS/Ancillary", "MA_bg", verbose=FALSE)
if (!exists("wind")) wind <- spTransform(readOGR("../GIS/Ancillary", "CW_boundary", verbose=FALSE),
                                         raster:::crs(MA))

plot_covariate <- function(z = "depth", data = env.segs, x = "x", y = "y", 
                           plotwind = FALSE, segs = TRUE, agg.seg = c(NA, "mean", "sum"),
                           winter = "winter", month = "date", legend.title = NULL,
                           legend.size = 10, scale = FALSE, diverge = FALSE) {
  
  theme_set(theme_bw(base_size = legend.size))
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
  
  agg.seg <- match.arg(agg.seg)
  
  ## Create data frame
  tmpDat <- data.frame(x = data[, x], y = data[, y], z = data[, z], seg = data[, "seg"],
                       winter = data[, winter], 
                       month = factor(month(data[, month]),
                                      levels = c(10:12, 1:4),
                                      labels = c(month.abb[10:12], month.abb[1:4])))
  if (scale) tmpDat$z <- as.numeric(scale(tmpDat$z))
  
  # Check to see if the variable is dynamic and stop if not requested to display
  varybymonth <- any((tmpDat %>% group_by(x, y, winter) %>% summarise(sdev = sd(z)))$sdev > 0, na.rm=TRUE)
  varybywinter <- any((tmpDat %>% group_by(x, y, month) %>% summarise(sdev = sd(z)))$sdev > 0, na.rm=TRUE)
  if(any(varybymonth, varybywinter)) {
    dynamic = TRUE
    } else {
      dynamic = FALSE
    }
  
  if (dynamic & !is.na(agg.seg)) {
    tmpDat <- arrange(unique(data[, c("seg", x, y, z)]), seg); names(tmpDat)[2:4] <- c("x", "y", "z")
    tmpDat <- ddply(tmpDat, .(x, y, seg), summarise,
                    z = do.call(agg.seg, list(z)))
  }
       
  # Set map x and y limits
  xlims <- c(374500, 429700)
  ylims <- c(4569125, 4615125)
  
  p <- 
    ggplot() + 
    geom_polygon(data=MA, aes(long, lat, group=group), colour = NA, fill="gray85") +
    geom_tile(data=tmpDat, aes(x=x, y=y, fill=z)) +
    coord_equal() +
    coord_cartesian(xlim = xlims, ylim = ylims) + 
    theme(legend.justification=c(0,0.5), 
          legend.position=c((416000 - xlims[1])/diff(xlims), 0.5)) # enter where legend should start
  
  if (is.null(legend.title)) {
      if(is.na(agg.seg)) {
        legend.title <- Cap(z)
      } else {
        legend.title <- paste0(Cap(z), "(", agg.seg, ")")
      }
  } else {
    legend.title <- legend.title
  }
  
  if(is.factor(data[, z])) {
    p <- p + scale_fill_brewer(legend.title, palette = "OrRd") 
  } else {
    if (diverge) {
      p <- p + scale_fill_gradient2(legend.title, low = "black", high = "red")
    } else {
      p <- p + scale_fill_gradient(legend.title, low = "#fef0d9", high = "#d7301f") 
    }
  }
  
  if (segs) p <- p + geom_polygon(data=seg_poly, aes(long, lat, group=group), colour = "gray30", fill = NA)
  
  if (plotwind) p <- p + geom_polygon(data=wind, aes(long, lat, group=group), 
                                      colour="black", size = 1.5, fill=NA)
  
  if(dynamic & is.na(agg.seg)) {
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