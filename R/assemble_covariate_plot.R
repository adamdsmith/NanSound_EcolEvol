## load accessory function
source("../R/ggplot_effects.R")

vars <- c("NAOw", "time", "depth", "time, depth", "meanphi", "SAR", "tidebmean", "tidesd", 
          "strat", "d2land", "chla", "cdom", "chla, cdom", 
          "SBT", "SSTm", "SSTw", "SSTrel", "time, SSTrel",
          "ferry", "y2004", "y2005", 
          "xkm, ykm", 
          "obs_window")
varTypes <- c("uni", "uni", "uni", "bi", "uni", "uni", "uni", "uni", 
              "uni", "uni", "uni", "uni", "bi",
              "uni", "uni", "uni", "uni", "bi",
              "fac", "fac", "fac",
              "bi", 
              "uni")

## OCCUPANCY
# COEI
keep <- c("SSTm", "d2land", "cdom", "meanphi", "SAR", "y2005", "xkm, ykm", "obs_window")
drop <- which(!(vars %in% keep))
varTypes_coei <- varTypes
varTypes_coei[drop] <- ""
coei_occ_plot <- ggplot_effects(COEIocc, data = coei, vars = vars, varTypes = varTypes_coei,
                                ylims=c(-2.4, 1.7))

# SCOT
keep <- c("time", "SSTw", "SSTm", "depth", "d2land", "cdom", "meanphi", "SAR", "xkm, ykm", "obs_window")
drop <- which(!(vars %in% keep))
varTypes_scot <- varTypes
varTypes_scot[drop] <- ""
scot_occ_plot <- ggplot_effects(SCOTocc, data = scot, vars = vars, varTypes = varTypes_scot,
                                ylims=c(-2.4, 1.7))

# LTDU
keep <- c("time", "time, depth", "SSTm", "d2land", "tidesd", "y2004", "xkm, ykm", "obs_window")
drop <- which(!(vars %in% keep))
varTypes_ltdu <- varTypes
varTypes_ltdu[drop] <- ""
ltdu_occ_plot <- ggplot_effects(LTDUocc, data = ltdu, vars = vars, varTypes = varTypes_ltdu, 
                                ylims=c(-2.4, 1.7))

## CONDITIONAL COUNT - MU & SIGMA FOR EACH SPECIES
# COEI
keep <- c("SSTrel", "depth", "cdom", "meanphi", "SAR", "ferry")
drop <- which(!(vars %in% keep))
varTypes_coei <- varTypes
varTypes_coei[drop] <- ""
coei_mu_plot <- ggplot_effects(COEIcc, parameter = "mu", vars = vars, varTypes = varTypes_coei, 
                               ylims = c(-1, 1.5))

keep <- c("SSTm", "depth", "tidebmean", "tidesd", "y2004", "xkm, ykm")
drop <- which(!(vars %in% keep))
varTypes_coei <- varTypes
varTypes_coei[drop] <- ""
coei_sigma_plot <- ggplot_effects(COEIcc, parameter = "sigma", vars = vars, varTypes = varTypes_coei, 
                                  ylims = c(-1.1, 1.35))

#SCOT
keep <- c("time", "SSTrel", "d2land", "meanphi", "xkm, ykm")
drop <- which(!(vars %in% keep))
varTypes_scot <- varTypes
varTypes_scot[drop] <- ""
scot_mu_plot <- ggplot_effects(SCOTcc, parameter = "mu", vars = vars, varTypes = varTypes_scot,
                               ylims = c(-1, 1.5))

keep <- c("SSTrel", "meanphi", "SAR", "tidesd", "y2004", "y2005")
drop <- which(!(vars %in% keep))
varTypes_scot <- varTypes
varTypes_scot[drop] <- ""
scot_sigma_plot <- ggplot_effects(SCOTcc, parameter = "sigma", vars = vars, varTypes = varTypes_scot, 
                                  ylims = c(-1.1, 1.35))

#LTDU
keep <- c("SSTm", "time, depth", "xkm, ykm")
drop <- which(!(vars %in% keep))
varTypes_ltdu <- varTypes
varTypes_ltdu[drop] <- ""
ltdu_mu_plot <- ggplot_effects(LTDUcc, parameter = "mu", vars = vars, varTypes = varTypes_ltdu, 
                               ylims = c(-1, 1.5))

keep <- c("strat")
drop <- which(!(vars %in% keep))
varTypes_ltdu <- varTypes
varTypes_ltdu[drop] <- ""
ltdu_sigma_plot <- ggplot_effects(LTDUcc, parameter = "sigma", vars = vars, varTypes = varTypes_ltdu, 
                                  ylims = c(-1.1, 1.35))

## CREATE "PLOTS" WITH ROW LABELS
labels <- c("North Atlantic Oscillation (Dec - Mar)", "Time (day of season)", "Water depth", 
            "Water depth x time", "Sediment grain size (phi)", 
            "Sea floor surface area:planimetric area", "Epibenthic tidal velocity (mean)", 
            "Epibenthic tidal velocity (standard deviation)", 
            "Water column stratification potential (summer)", "Distance to land", 
            "Chlorophyll-a (Chl-a)", "Chromomorphic dissolved organic matter (CDOM)", "Chl-a x CDOM", 
            "Sea bottom temperature (May - Oct)",
            "Sea surface temperature (SST; monthly)", "SST (mean; Nov - Mar)", 
            "SST (relative to other segments)",  "SST (relative) x time", 
            "Ferry route (within 1 km)", "Winter (2004)", "Winter (2005)",  
            "Northing x easting", "Survey effort (transect area within segment)")

label_plots <- vector("list", length(labels))
names(label_plots) <- vars

for (i in 1:length(labels)) {
  label <- labels[i]
  tmpDat <- data.frame()
  p <- ggplot(tmpDat) + geom_blank() + xlim(0, 1) + ylim(0, 1) + 
    annotate("text", x = Inf, y = 0.5, label = label, hjust=1, vjust=0.5) + 
    theme(panel.border = element_blank())
  label_plots[[i]] <- p
}

## CREATE "PLOTS" WITH COLUMN (SPECIES) HEADINGS
spp <- c("COEI", "SCOT", "LTDU")
spp_plots <- vector("list", length(spp))

for (i in 1:length(spp)) {
  sp <- spp[i]
  tmpDat <- data.frame()
  p <- ggplot(tmpDat) + geom_point() + xlim(0, 1) + ylim(0, 1) + 
    annotate("text", x = 0.5, y = 0, label = sp, hjust=0.5, vjust=0, size=4) + 
    theme(panel.border = element_blank())
  spp_plots[[i]] <- p
}

## MAKE BLANK "PLOTS" FOR USE IN FINAL FIGURE (SPACING)
blank_plots <- ggplot_effects(data=0, vars=rep("blank", 24), varTypes = rep("blank", 24)) #data and vars irrelevant

## CREATE FILE
tiff(file = "./Figures/Covariate_plot(pre-edits).tif", width = 19 * 0.5, height = 24.5 * 0.5, 
     units = "in", res = 600, compression="lzw")
hts <- c(1.5, rep(1, 23))
grid.arrange(do.call(arrangeGrob, c(blank_plots[1], label_plots, list(nrow=24, heights=hts))), 
             do.call(arrangeGrob, c(spp_plots[1], coei_occ_plot, list(nrow=24, heights=hts))),
             do.call(arrangeGrob, c(spp_plots[2], scot_occ_plot, list(nrow=24, heights=hts))),
             do.call(arrangeGrob, c(spp_plots[3], ltdu_occ_plot, list(nrow=24, heights=hts))),
             do.call(arrangeGrob, c(blank_plots, list(nrow=24, heights=hts))),
             do.call(arrangeGrob, c(spp_plots[1], coei_mu_plot, list(nrow=24, heights=hts))),
             do.call(arrangeGrob, c(spp_plots[2], scot_mu_plot, list(nrow=24, heights=hts))),
             do.call(arrangeGrob, c(spp_plots[3], ltdu_mu_plot, list(nrow=24, heights=hts))),
             do.call(arrangeGrob, c(blank_plots, list(nrow=24, heights=hts))),
             do.call(arrangeGrob, c(spp_plots[1], coei_sigma_plot, list(nrow=24, heights=hts))),
             do.call(arrangeGrob, c(spp_plots[2], scot_sigma_plot, list(nrow=24, heights=hts))),
             do.call(arrangeGrob, c(spp_plots[3], ltdu_sigma_plot, list(nrow=24, heights=hts))),
             ncol = 12, widths = c(9, 1, 1, 1, 0.5, 1, 1, 1, 0.5, 1, 1, 1))
dev.off()
