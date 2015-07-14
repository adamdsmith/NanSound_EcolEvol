## load accessory function
source("../R/ggplot_effects.R")

vars <- c("time", "depth", "time, depth", "meanphi", "SAR", "tidebmean", "tidesd", 
          "strat", "chla", "cdom", "chla, cdom", 
          "SBT", "SSTm", "SSTw", "SSTrel", "time, SSTrel", "NAOw", "d2land",
          "ferry", "y2004", "y2005", 
          "xkm, ykm", 
          "obs_window")
varTypes <- c("uni", "uni", "bi", "uni", "uni", "uni", "uni", 
              "uni", "uni", "uni", "bi",
              "uni", "uni", "uni", "uni", "bi", "uni", "uni",
              "fac", "fac", "fac",
              "bi", 
              "uni")

## OCCUPANCY
## Load occupancy models for each species and rename to avoid repeatedly loading these models
load("C:/Users/Adam/OneDrive/NanSound/Results_coei/zero.rda")
#load("../Results_coei/zero.Rda")
COEIocc <- zero

load("C:/Users/Adam/OneDrive/NanSound/Results_scot/zero.rda")
#load("../Results_scot/zero.Rda")
SCOTocc <- zero

load("C:/Users/Adam/OneDrive/NanSound/Results_ltdu/zero.rda")
#load("../Results_ltdu/zero.Rda")
LTDUocc <- zero

# COEI
keep <- c("SSTm", "SBT", "d2land", "cdom", "meanphi", "SAR", "y2005", "xkm, ykm", "obs_window")
drop <- which(!(vars %in% keep))
varTypes_coei <- varTypes
varTypes_coei[drop] <- ""
coei_occ_plot <- ggplot_effects(COEIocc, data = coei, vars = vars, varTypes = varTypes_coei,
                                ylims=c(-2.4, 1.7))

# SCOT
keep <- c("time", "SSTw", "SSTm", "SSTrel", "depth", "d2land", "cdom", "meanphi", "SAR", "xkm, ykm", "obs_window")
drop <- which(!(vars %in% keep))
varTypes_scot <- varTypes
varTypes_scot[drop] <- ""
scot_occ_plot <- ggplot_effects(SCOTocc, data = scot, vars = vars, varTypes = varTypes_scot,
                                ylims=c(-2.4, 1.7))

# LTDU
keep <- c("time", "time, depth", "SSTm", "d2land", "meanphi", "tidesd", "y2004", "xkm, ykm", "obs_window")
drop <- which(!(vars %in% keep))
varTypes_ltdu <- varTypes
varTypes_ltdu[drop] <- ""
ltdu_occ_plot <- ggplot_effects(LTDUocc, data = ltdu, vars = vars, varTypes = varTypes_ltdu, 
                                ylims=c(-2.4, 1.7))

rm("zero", "COEIocc", "SCOTocc")
gc(reset=TRUE)

## CONDITIONAL COUNT - MU & SIGMA FOR EACH SPECIES
## Load conditional count models for each species and rename to avoid repeatedly loading these models
load("C:/Users/Adam/OneDrive/NanSound/Results_coei/hurdle.rda")
#load("../Results_coei/hurdle.Rda")
COEIcc <- hurdle

load("C:/Users/Adam/OneDrive/NanSound/Results_scot/hurdle.rda")
#load("../Results_scot/hurdle.Rda")
SCOTcc <- hurdle

load("C:/Users/Adam/OneDrive/NanSound/Results_ltdu/hurdle.rda")
#load("../Results_ltdu/hurdle.Rda")
LTDUcc <- hurdle

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

rm("hurdle", "COEIcc", "SCOTcc", "LTDUcc", "LTDUocc")
gc(reset=TRUE)

## CREATE "PLOTS" WITH ROW LABELS
labels <- c("Day~of~season~(italic(scriptstyle(time)))",
            "Bathymetry~(italic(scriptstyle(depth)))",
            "italic(depth)~scriptstyle(x)~italic(time)",
            "Sediment~grain~size~(italic(scriptstyle(meanphi)))", 
            "Sea~floor~surface~area:planimetric~area~(italic(scriptstyle(SAR)))",
            "Epibenthic~tidal~velocity~(mean*';'~italic(scriptstyle(tidebmean)))", 
            "Epibenthic~tidal~velocity~(standard~deviation*';'~italic(scriptstyle(tidesd)))", 
            "Water~column~stratification~potential~(italic(scriptstyle(strat)))",
            "Chlorophyll-a~(italic(scriptstyle(chla)))",
            "Chromomorphic~dissolved~organic~matter~(italic(scriptstyle(cdom)))",
            "italic(chla)~scriptstyle(x)~italic(cdom)",
            "Sea~bottom~temperature~(May~-~Oct*';'~italic(scriptstyle(SBT)))",
            "Sea~surface~temperature~(monthly*';'~italic(scriptstyle(SST[m])))",
            "Sea~surface~temperature~(Nov~-~Mar*';'~italic(scriptstyle(SST[w])))",
            "Sea~surface~temperature~(relative*';'~italic(scriptstyle(SST[rel])))",
            "italic(SST[rel])~scriptstyle(x)~italic(time)",
            "North~Atlantic~Oscillation~(Dec~-~Mar*';'~italic(scriptstyle(NAO[w])))", 
            "Distance~to~land~(italic(scriptstyle(d2land)))",
            "Ferry~route~(within~1~km*';'~italic(scriptstyle(ferry)))",
            "Winter~2004~(italic(scriptstyle(y[2004])))",
            "Winter~2005~(italic(scriptstyle(y[2005])))", 
            "Northing~(italic(scriptstyle(ykm)))~scriptstyle(x)~Easting~(italic(scriptstyle(xkm)))",
            "Survey~effort~(scriptstyle(obs_window))")

label_plots <- vector("list", length(labels))
names(label_plots) <- vars

for (i in 1:length(labels)) {
  label <- labels[i]
  tmpDat <- data.frame()
  p <- ggplot(tmpDat) + geom_blank() + xlim(0, 1) + ylim(0, 1) + 
    annotate("text", x = Inf, y = 0.5, label = label,
             parse=T, hjust=1, vjust=0.5) + 
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
