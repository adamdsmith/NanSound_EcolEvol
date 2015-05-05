seg_poly <- readOGR("../GIS/Ancillary", "seg_poly", verbose=FALSE)
MA <- readOGR("../GIS/Ancillary", "MA_bg", verbose=FALSE)
wind <- spTransform(readOGR("../GIS/Ancillary", "CW_boundary", verbose=FALSE), raster:::crs(MA))
transects <- readOGR("../GIS/Ancillary", "transects", verbose=FALSE)
ec <- readOGR("../GIS/Ancillary", "ec_lines", verbose=FALSE)

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
  geom_polygon(data=MA, aes(long, lat, group=group), colour = element_blank(), fill="gray85") +
  geom_path(data=transects, aes(long, lat, group=group), colour = "gray50", alpha=0.5) + 
  geom_polygon(data=seg_poly, aes(long, lat, group=group), colour = "black", alpha=0) +
  geom_polygon(data=wind, aes(long, lat, group=group), colour="red", size = 1.5, alpha=0) +
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
source("../R/add_scale_bar.R")
detail <- add_scale_bar(detail, legend.placement = "bottom left")
               
# Inset
# Define study area boundary
study_area <- data.frame(xmin=-70.63, xmax=-69.90, ymin=41.22, ymax=41.70)

# Create inset plot
inset <- 
  ggplot() + 
  geom_rect(aes(xmin =-75.03, xmax = -68.55, ymin = 38.89, ymax = 45.37), fill = "white") +
  geom_path(data=ec, aes(long, lat, group=group), colour = "black", fill="white") +
  coord_equal() +
  scale_x_continuous(limits = c(-75.03, -68.55), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(38.89, 45.37), expand = c(0, 0)) +
  geom_rect(data = study_area, 
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            colour="red", alpha=0, size=1)  +
  theme(plot.background = element_rect(fill = "transparent", colour = NA))
  
# Print (or save) it
# tiff(file="../Figures/Nantucket_study_area.tif",w=6.5,h=6.5, res=900, units = "in", compression = "lzw")
# png(file="./Figures/Nantucket_study_area.png",w=6.5,h=6.5, res=900, units = "in")
grid.newpage()
v1<-viewport() #plot area for the main map
v2<-viewport(w = 0.33, h = 0.33, x = 0, y = 1, just = c(0, 1), gp = gpar(fill=0)) #plot area for the inset map
print(detail,vp=v1) 
print(inset,vp=v2)
#dev.off()