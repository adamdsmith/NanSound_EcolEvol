library(rgdal); library(ggplot2); library(grid)
seg_poly <- readOGR("../GIS/Ancillary", "seg_poly", verbose=FALSE)
MA <- readOGR("../GIS/Ancillary", "MA_bg", verbose=FALSE)
wind <- spTransform(readOGR("../GIS/Ancillary", "CW_boundary", verbose=FALSE), raster:::crs(MA))
transects <- readOGR("../GIS/Ancillary", "transects", verbose=FALSE)
ec <- readOGR("../GIS/Ancillary", "NE_US_poly", verbose=FALSE)

theme_set(theme_bw())
theme_update(line = element_blank(),
             text = element_blank(),
             axis.ticks.length = unit(0, "cm"),
             axis.ticks.margin = unit(0.01, "cm"), 
             legend.position = "none", 
             panel.margin = unit(0, "lines"), 
             plot.margin = unit(c(0, 0, -0.5, -0.5),"lines"), 
             complete = TRUE)

# Detailed study area plot
detail <- 
  ggplot() + 
  geom_polygon(data=MA, aes(long, lat, group=group), colour = NA, fill="gray85") +
  geom_path(data=transects, aes(long, lat, group=group), colour = "gray50", alpha = 0.5) + 
  geom_polygon(data=seg_poly, aes(long, lat, group=group), colour = "black", fill = NA) +
  geom_polygon(data=wind, aes(long, lat, group=group), colour="black", size = 1.5, fill = NA) +
  coord_equal() +
  scale_x_continuous("", limits = c(363350, 423200), expand = c(0, 0)) + 
  scale_y_continuous("", limits = c(4565650, 4625100), expand = c(0, 0)) + 
  theme(panel.border=element_blank())

# Add helpful geographic labels
geog_labels <- data.frame(x = c(402118, 410428, 368384),
                          y = c(4616079, 4568500, 4581922),
                          name = c("Cape Cod", "Nantucket", "Martha's\nVineyard"))
detail <- detail +
  geom_text(data=geog_labels, aes(x = x, y = y, label = name), vjust = 0.5, size = 5)


# Add scale bar and orientation arrow
all_bins <- data.frame(bin = rep(1L:3L, each = 4),
                       x = c(375320, 375320, 385320, 385320, 370320, 370320, 375320, 375320, 
                             365320, 365320, 370320, 370320),
                       y = c(4568622.5, 4569622.5, 4569622.5, 4568622.5, 4568622.5, 4569622.5, 
                             4569622.5, 4568622.5, 4568622.5, 4569622.5, 4569622.5, 4568622.5),
                       fill = rep(c("white", "black", "white"), each = 4))
bin_text <- data.frame(x = c(375320, 370320, 365320, 385320),
                       y = c(4568422.5, 4568422.5, 4568422.5, 4568422.5),
                       label = c("10", "5", "0", "20 km"), stringsAsFactors = FALSE)
detail <- detail + geom_polygon(data = all_bins, aes(x, y, group = bin, fill = fill), colour = "black") +
  scale_fill_manual(values = c("black", "white")) +
  geom_text(data = bin_text, aes(x, y, label = label), vjust = 1, size = 5)

# Get arrow start and end coordinates
arrow_df <- data.frame(arrowx = 375320, xend = 375320,
                       arrowy = 4570623, yend = 4575623,
                       ymid = 4573123)

detail <- detail + geom_segment(data = arrow_df,
                      aes(x = arrowx, xend = xend, y = arrowy, yend = yend),
                      arrow = arrow(length = unit(0.25, "cm")),
                                    size = 1) +
  annotate("text", x = arrow_df$arrowx, y = arrow_df$ymid,
           label = "N", size = 6)

# Inset
# Define study area boundary
study_area <- data.frame(xmin=-70.63, xmax=-69.90, ymin=41.22, ymax=41.70)

# Create inset plot
inset <- 
  ggplot() + 
#  geom_rect(aes(xmin =-75.03, xmax = -68.55, ymin = 38.89, ymax = 45.37), 
#            fill = "white") +
  geom_polygon(data=ec, aes(long, lat, group=group), fill = "gray75", colour = "black") +
  coord_equal() +
    coord_cartesian(xlim = c(-75.03, -68.55), ylim = c(38.89, 45.37)) +
#  scale_x_continuous(limits = , expand = c(0, 0)) + 
#  scale_y_continuous(limits = , expand = c(0, 0)) +
  geom_rect(data = study_area, 
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            colour="black", fill = NA, size=1)  +
  theme(plot.background = element_rect(fill = "transparent", colour = NA))
  
# Print (or save) it
png(file="./Figures/Nantucket_study_area.png",w=6.5,h=6.5, res=600, units = "in")
grid.newpage()
v1<-viewport() #plot area for the main map
#plot area for the inset map
v2<-viewport(w = 0.33, h = 0.33, x = 0, y = 1, just = c(0, 1), gp = gpar(fill=0)) 
print(detail,vp=v1) 
print(inset,vp=v2)
dev.off()
