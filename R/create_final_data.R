# Load some useful aliases and functions
devtools::source_gist(9216061) # various.R

# Loading required packages
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(plyr, lubridate, reshape, sp, maptools, raster, 
               rgdal, foreign, parallel, psych, ggplot2)

# Load segments, Massachusetts outline, Cape Wind project boundary and 'plotvar' function
source("../R/plotvar.R")

# Plot them to check
plot(seg_poly)
plot(MA, add=TRUE)
plot(spTransform(wind, crs(MA)), border = "red", add=TRUE)

# Loading chlorophyll-a & dissolved organic matter
chla <- raster("../GIS/Environmental/Final/chla.tif")
cdom  <-  raster("../GIS/Environmental/Final/cdom.tif")
chl_dom <- stack(chla, cdom)
chl_dom <- disaggregate(chl_dom, c(14, 10))

# Loading sediment grain size (phi scale)
phi <- raster("../GIS/Environmental/Final/meanphi.tif")
phi <- disaggregate(phi, 4) # increase resolution to facilitate calculation of mean

# Loading Nantucket Sound bathymetry
ns_bath <- raster("../GIS/Environmental/Final/ns_bath.tif")
# Reducing res of original (to ~ 100 m square) to improve speed
#ns_bath <- aggregate(ns_bath, c(12, 9)) 
#writeRaster(ns_bath, "../GIS/Environmental/Final/ns_bath", format="GTiff", overwrite = TRUE)

# Loading seafloor surface and planimetric area
# NAs were causing trouble, so replace with zeros
ns_SA <- raster("../GIS/Environmental/Final/ns_sa.tif")
ns_SA[is.na(ns_SA)] <- 0
ns_PA <- raster("../GIS/Environmental/Final/ns_pa.tif")
ns_PA[is.na(ns_PA)] <- 0

# Loading tidal velocity mean and standard deviation, sea bottom temperature (May - Oct),
# & monthly sea surface temperature (FVCOM)
ns_tidebmean <- raster("../GIS/Environmental/Final/tidebmean.tif")
ns_tidesmean <- raster("../GIS/Environmental/Final/tidesmean.tif")
ns_tidesd <- raster("../GIS/Environmental/Final/tidesd.tif")
bTemp_files <- list.files(path = "../GIS/Environmental/Final/bTemp/", full.names = TRUE, 
                          pattern = 'bTemp')
sTemp_files <- list.files(path = "../GIS/Environmental/Final/sTemp/", full.names = TRUE, 
                          pattern = 'sTemp')
tide_temps <- stack(c(bTemp_files, sTemp_files), ns_tidebmean, ns_tidesmean, ns_tidesd)
tide_temps <- disaggregate(tide_temps, 10) # increase res to facilitate calculation of mean

# Loading distance to land
ns_d2land <- raster("../GIS/Environmental/Final/ns_d2land.tif")

### Covariate extraction from segment buffers

# The loop iterates through geographically-associated subsets of segments,
# which lessens the raster extent to be loaded, reducing processing time
# This takes about 22 min to run on 4 cores (Intel? Xeon? Processor E5520, 12GB RAM)

# Set up parallelization
beginCluster()

# Create dataframe to receive zonal stats from loop
env.segs <- data.frame()

seg_poly <- spTransform(seg_poly, crs(chl_dom))
segids <- 1:nrow(seg_poly)
process.list <- split(segids, ceiling(seq_along(segids)/65))

system.time(for (i in 1:length(process.list)) {
  tmp_poly <- seg_poly[which(seg_poly$seg %in% process.list[[i]]), ]
  
  # Chlorophyll-a (mg/m^3) and chromophoric dissolved organic material (absorption m-1)
  chldom <- extract(chl_dom, tmp_poly, fun=mean, na.rm=TRUE)
  tmp_poly@data <- cbind(tmp_poly@data, chldom)
  
  # Hard bottom probability (obsolete)
#  tmp_poly <- spTransform(tmp_poly, crs(hbp))
#  hardness <- extract(hbp, tmp_poly, fun=mean, na.rm=TRUE)
#  tmp_poly@data <- cbind(tmp_poly@data, hardness)
  
  # Sediment grain size (phi; equivalent to -log2(diameter [mm]))
  tmp_poly <- spTransform(tmp_poly, crs(phi))
  meanphi <- extract(phi, tmp_poly, fun=mean, na.rm=TRUE)
  tmp_poly@data <- cbind(tmp_poly@data, meanphi)
  
  # Nantucket Sound bathymetry (m)
  tmp_poly <- spTransform(tmp_poly, crs(ns_bath))
  depth <- extract(ns_bath, tmp_poly, fun=mean, na.rm=TRUE)
  tmp_poly@data <- cbind(tmp_poly@data, depth)
  
#  # Sea floor 'roughness'
#  tmp_poly <- spTransform(tmp_poly, crs(ns_slope))
#  roughness <- extract(ns_slope, tmp_poly, fun=sd, na.rm=TRUE)
#  tmp_poly@data <- cbind(tmp_poly@data, roughness)
  
  # Sea floor surace area:planimetric area ratio 
  tmp_poly <- spTransform(tmp_poly, crs(ns_SA))
  SA <- extract(ns_SA, tmp_poly, fun=sum) 
  PA <- extract(ns_PA, tmp_poly, fun=sum) 
  SAR <- SA/PA 
  tmp_poly@data <- cbind(tmp_poly@data, SAR)
  
  # Mean bottom and surface tidal velocity (2003-2005; m/s)
  # SD of epibenthic tidal velocity (2003-2005; m/s)
  # Sea bottom temperature during mussel spawn 
  # Monthly sea surface temperature (FVCOM)
  tmp_poly <- spTransform(tmp_poly, crs(tide_temps))
  tide.temps <- extract(tide_temps, tmp_poly, fun=mean, na.rm=TRUE)
  tmp_poly@data <- cbind(tmp_poly@data, tide.temps)

  # Distance to land (km)
  tmp_poly <- spTransform(tmp_poly, crs(ns_d2land))
  d2land <- extract(ns_d2land, tmp_poly, fun=mean, na.rm=TRUE) / 1000
  tmp_poly@data <- cbind(tmp_poly@data, d2land)

env.segs <- rbind(env.segs, tmp_poly@data)
})

# Close cluster
endCluster()

# Sort by segment
env.segs <- arrange(env.segs, seg)

# Load full survey and effort record, add information for segments that were never 
# surveyed (n = 47), and join with environmental data
survey.hist <- read.dbf("../GIS/Ancillary/seg_xsect.dbf", as.is = TRUE)

# Consolidate by segment and date (i.e., several instances in which a transect intersected
# a segment twice during the same survey)
survey.hist <- ddply(survey.hist, .(seg, start_dt), summarise,
                     length = sum(length))

# Add unsurveyed segments (this is to make sure dynamic data is associated with every segment)
survey.dates <- unique(survey.hist[, 'start_dt']) # all survey dates
n.surveys <- length(survey.dates)
tmp.survey.hist <- expand.grid(seg = 1:504, start_dt = survey.dates, length = 0,
                               stringsAsFactors = FALSE)
unsurveyed <- tmp.survey.hist[which(with(tmp.survey.hist, interaction(seg, start_dt)) %notin% 
                                      with(survey.hist, interaction(seg, start_dt))), ]
survey.hist <- rbind(survey.hist, unsurveyed)

env.segs <- join(survey.hist, env.segs, by = "seg") # right join preserves unsurveyed segments

# Format date, add month (mo) to facilitate association with dynamic variables,
# add 'time' variable (relative date within a winter season) and center location
env.segs <- mutate(env.segs, 
                   date = ymd(start_dt),
                   mo = month(start_dt), 
                   winter = factor(ifelse(month(start_dt) > 5, year(start_dt), 
                                          year(start_dt) - 1)),
                   time = as.integer(date - mdy(paste0("12-31-", winter))),
                   xkm = (x - median(x)) / 1000,
                   ykm = (y - median(y)) / 1000)

# Dynamic data (monthly or yearly changes)
# Monthly sea surface temperature
sstm_lu <- unique(arrange(melt(env.segs[, c("seg", grep("sTemp", names(env.segs), 
                                                        value = TRUE))],
                                 id="seg"), variable, seg))
sstm_lu <- mutate(sstm_lu, 
                    yr = as.numeric(substr(variable, 7, 10)),
                    mo = as.numeric(substr(variable, 12, 13)),
                    winter = factor(ifelse(mo > 5, yr, yr - 1)))

# Average seasonal (Nov - Mar) SST
# Note winter 2002 and 2006 winters incomplete but they are not used
sstw_lu <- ddply(sstm_lu[sstm_lu$mo %in% c(11:12, 1:3), ], .(seg, winter), summarize,
                 sstw = mean(value))

# Epibenthic sea temperature (May - Oct)
btemp_lu <- arrange(melt(env.segs[, c("seg", grep("bTemp", names(env.segs), value = TRUE))], 
                         id="seg"), seg)
btemp_lu <- btemp_lu[!duplicated(btemp_lu), ]
btemp_lu <- mutate(btemp_lu, 
                    winter = as.numeric(substr(variable, 7, 10)))

# North Atlantic Oscillation (winter PC index of Hurrell) look up table
nao_lu <- read.csv("../GIS/Environmental/Final/nao.csv", header=TRUE)

# Match segment with its corresponding dynamic data
env.segs <- within(env.segs, {
  
  # Montly SST (for relative temperature calculation, below)
  SSTm <- sstm_lu$value[match(interaction(seg, year(date), mo), 
                              interaction(sstm_lu$seg, sstm_lu$yr, sstm_lu$mo))]
  
  # Mean winter SST
  SSTw <- sstw_lu$sstw[match(interaction(seg, winter), 
                              interaction(sstw_lu$seg, sstw_lu$winter))]
  
  # Mean mussel spawing/settling (May - Oct) sea bottom temperature
  SBT <- btemp_lu$value[match(interaction(winter, seg), 
                              interaction(btemp_lu$winter, btemp_lu$seg))]
  
  # Winter NAO index (PC time series of the leading empirical orthogonal function of 
  # sea level pressure anomalies over the Atlantic sector)
  NAOw <- nao_lu$NAO_PC[match(winter, nao_lu$winter)]  
})

# Calculate for each segment the deviation from monthly temperature (relative temperature)
env.segs <- ddply(env.segs, .(winter, mo), mutate,
                  SSTrel = SSTm - mean(SSTm))

# Calculate for each segment the potential for seasonal thermal stratification
env.segs <- mutate(env.segs, 
                   strat = log10(-depth / tidesmean ^ 3))

# Clean up
rm(list = c("sstm_lu", "sstw_lu", "btemp_lu", "nao_lu", "tmp_poly"))

# Now reduce to final effort & environmental data set
env.segs <- env.segs[, c("seg", "x", "y", "xkm", "ykm", "date", "length", "winter", "time",
                         "SSTw", "SSTm", "SSTrel", "SBT", "chla", "cdom", "meanphi", "depth", 
                         "d2land", "SAR", "tidebmean", "tidesd", "strat", "NAOw", "ferry")]

# Example uses of the 'plotvar' function
plotvar("depth", plotwind = TRUE)
plotvar("meanphi")
plotvar("d2land")
plotvar("tidebmean")
plotvar("tidesd")
plotvar("SAR")
plotvar("strat")
plotvar("ferry", plotwind = TRUE)
plotvar("SBT")
plotvar("SSTm")
plotvar("SSTw")
plotvar("NAOw")
plotvar("chla")
plotvar("cdom")
plotvar("SSTrel")

# Check relationships among covariates
testVIF <- unique(env.segs[, c("seg", "time", "SSTw", "SSTm", "SSTrel", "SBT", "chla", 
                               "cdom", "meanphi", "depth", "d2land", "SAR", "tidebmean",
                               "tidesd", "strat")])
                  
# First, a quick figure of relationships
pdf("../Exploratory_output/covariate_r_matrix.pdf", width = 10.5, height = 8, paper = "USr")
pairs.panels(testVIF[, -1], scale = TRUE, pch = 12)
dev.off()

# Second, testing VIF of predictor variables
corvif(testVIF[, -1]) # tidebmean and strat strongly related...

# Load, manipulate, and save observation data
obs <- read.dbf("../GIS/Survey/obs_poly.dbf", as.is = TRUE)

# Lump scoter species
scoters <- c("SUSC", "WWSC", "BLSC", "UNSC")
obs <- mutate(obs, group = ifelse(spp_cd %in% scoters, "SCOT", spp_cd),
              obs_dt = ymd(obs_dt))

# Aggregate observations to dates and transect segments
obs.final <- ddply(obs, .(obs_dt, seg, group), summarise,
                   count = sum(count))
obs.final <- mutate(obs.final, date = obs_dt, obs_dt = NULL)

# Save final environmental and observation data
save(env.segs, obs.final, file = "../Data/ducks&environment.RData")

