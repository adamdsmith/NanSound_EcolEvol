if (!exists("seg_poly")) seg_poly <- readOGR("../GIS/Ancillary", "seg_poly")

sTemp_files <- list.files(path = "../GIS/Environmental/Final/sTemp/", full.names = TRUE, 
                          pattern = 'sTemp')
sst <- raster::stack(sTemp_files)
sst <- raster::disaggregate(sst, 10) # increase resolution to facilitate calculation of mean

# Create dataframe to receive zonal stats from loop
all_ssts <- data.frame()

tmp_poly <- spTransform(seg_poly, raster::crs(sst))
segids <- 1:nrow(tmp_poly)
process.list <- split(segids, ceiling(seq_along(segids)/65))

raster::beginCluster()
for (i in 1:length(process.list)) {
  tmp_poly2 <- tmp_poly[which(tmp_poly$seg %in% process.list[[i]]), ]
  stemps <- raster::extract(sst, tmp_poly2, fun=mean, na.rm=TRUE)
  tmp_poly2@data <- cbind(tmp_poly2@data, stemps)
  all_ssts <- rbind(all_ssts, tmp_poly2@data)
}
raster::endCluster()

# Dynamic data (monthly or yearly changes)
# Monthly sea surface temperature
sstm_lu <- unique(arrange(melt(all_ssts[, c("seg", grep("sTemp", names(all_ssts), value = TRUE))],
                               id="seg"), variable, seg))
sstm_lu <- mutate(sstm_lu, 
                  yr = as.numeric(substr(variable, 7, 10)),
                  mo = as.numeric(substr(variable, 12, 13)),
                  winter = factor(ifelse(mo > 5, yr, yr - 1)))

# Add relevant months/years to all_ssts
all_ssts <- expand.grid.df(all_ssts, 
                           data.frame(expand.grid(mo = c(11:12, 1:4), year = 2003:2006)))

# Montly SST
all_ssts <- within(all_ssts, {
  SSTm <- sstm_lu$value[match(interaction(seg, year, mo), 
                            interaction(sstm_lu$seg, sstm_lu$yr, sstm_lu$mo))]
})
all_ssts <- all_ssts[, c("seg", "mo", "year", "SSTm")]

# Calculate for each segment the deviation from monthly temperature (relative temperature)
# and put it on the appropriate scale (i.e., center it)
all_ssts <- ddply(all_ssts, .(mo, year), mutate,
                  SSTrel = SSTm - mean(SSTm))

all_ssts <- mutate(all_ssts,
                   SSTm = (SSTm - covar_avgs["SSTm"]) / covar_sds["SSTm"],
                   SSTrel = (SSTrel - covar_avgs["SSTrel"]) / covar_sds["SSTrel"],
                   winter = factor(ifelse(mo > 5, year, year - 1)))

# Keep only relevant winters
all_ssts <- droplevels(subset(all_ssts, winter %in% c("2003", "2004", "2005")))

save(all_ssts, file = "../Data/all_SSTs.Rda")
