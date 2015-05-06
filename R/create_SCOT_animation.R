instant_pkgs("animation")
# A LaTex compiler must be installed

theme_set(theme_bw(base_size = 19))
theme_update(plot.margin = unit(c(0.25, 0.05, -0.85, -0.85),"line"),
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

## Load polygons if necessary
if (!exists("seg_poly")) seg_poly <- readOGR("../GIS/Ancillary", "seg_poly")
if (!exists("MA")) MA <- readOGR("../GIS/Ancillary", "MA_bg")
if (!exists("wind")) wind <- spTransform(readOGR("../GIS/Ancillary", "CW_boundary"), raster:::crs(MA))

# Set map x and y limits
xlims <- c(374500, 425700)
ylims <- c(4569125, 4615125)

make_anim <- function(tmpDat) {
    
    threshold <- quantile(tmpDat$ltabund, probs = 0.98)
    which_segs <- tmpDat$seg[which(tmpDat$ltabund >= threshold)]
    
    tmpDat$ltabund <- cut2(tmpDat$ltabund, cuts = c(0, 10, 25, 100, 500000), digits = 0)
    levels(tmpDat$ltabund) <- c("< 10", "10 - 25", "26 - 100", "> 100")
    
    p <- ggplot() + ggtitle("Scoter estimated overall abundance: winter 2005-2006") +
      geom_polygon(data=MA, aes(long, lat, group=group), colour = element_blank(), fill="gray85") +
      geom_tile(data=tmpDat, aes(x=x, y=y, fill=ltabund)) +
      coord_equal() +
      coord_cartesian(xlim = xlims, ylim = ylims) + 
      theme(legend.justification=c(0,0.5), 
            legend.position=c((416000 - xlims[1])/diff(xlims), 0.5)) + # enter where legend should start
      scale_fill_brewer("Predicted\nabundance", palette = "OrRd") +
      geom_polygon(data=seg_poly, aes(long, lat, group=group), colour = "gray30", alpha=0) +
      geom_polygon(data=wind, aes(long, lat, group=group),
                   colour="black", size = 1.5, alpha=0) +
      geom_polygon(data = subset(seg_poly, seg %in% which_segs), 
                   aes(long, lat, group=group), colour = "gray20", size=1.25, alpha=0) +
      annotate("text", x = xlims[1] + 0.02*diff(xlims), y = ylims[2] - 0.02*diff(ylims), 
               label = format(unique(tmpDat$date), format = "%d %b %Y"), hjust=0, vjust=1, size=10)
    print(p)
    
}

saveLatex({

  lapply(dlply(scot_anim, .(date)), make_anim)

  }, interval = 0.5, ani.dev = "pdf", ani.type = "pdf", 
  ani.width = 8, ani.height = 7, latex.filename = "SCOT_2005_animation",
  documentclass = paste("\\documentclass{article}", 
                        "\\usepackage[margin=0.5in,landscape]{geometry}", sep = "\n"),
  ani.opts="controls, width=0.825\\linewidth")
