# Assumes the first two code chunkls from NanSound_main_document.Rmd have been sourced
library(ggplot2)
library(viridis)
source("../R/predict.gamlssHurdle.R")

if (!exists("env.segs")) load("../Data/ducks&environment.RData")

# Subset environmental data to those segments actually surveyed on a given date
surveyed.segs <- subset(env.segs, length > 0)

# Ignore certain variables
ignore <- names(surveyed.segs) %in% c('date', 'seg', 'count', 'x', 'y', 'winter', 'ferry')
surveyed.segs <- surveyed.segs[, !ignore]

# Convert transect length to survey area
surveyed.segs$obs_window <- surveyed.segs$length * 91.44/1000 * 2
surveyed.segs$length <- NULL

# Generate named vector of means and SDs of time (for scaling)
covar_avgs <- apply(surveyed.segs, 2, mean)
covar_sds <- apply(surveyed.segs, 2, sd)

## Sum of observed sea duck counts, by species (group), for 30 surveys
coei_obs <- ddply(coei, .(date, winter), summarise,
                  total_effort = sum(obs_window * covar_sds["obs_window"] + covar_avgs["obs_window"]),
                  total_count = sum(count))

scot_obs <- ddply(scot, .(date, winter), summarise,
                  total_effort = sum(obs_window * covar_sds["obs_window"] + covar_avgs["obs_window"]),
                  total_count = sum(count))

ltdu_obs <- ddply(ltdu, .(date, winter), summarise,
                  total_effort = sum(obs_window * covar_sds["obs_window"] + covar_avgs["obs_window"]),
                  total_count = sum(count))

## Load occupancy and conditional count models models for each species and rename
load("../Results_coei/zero.Rda")
COEIocc <- zero

load("../Results_coei/hurdle.Rda")
COEIcc <- hurdle

# Sum of predicted sea duck counts, by species (group), for 30 surveys
coei_test <- data.frame(coei, predict.gamlssHurdle(COEIocc, COEIcc, coei))
coei_test <- ddply(coei_test, .(date, winter), summarise,
                   total_effort = sum(obs_window * covar_sds["obs_window"] + covar_avgs["obs_window"]),
                   est_count = sum(exp(add_pred)))
coei_compare <- left_join(coei_obs, coei_test)

# SCOT
load("../Results_scot/zero.Rda")
SCOTocc <- zero

load("../Results_scot/hurdle.Rda")
SCOTcc <- hurdle

scot_test <- data.frame(scot, predict.gamlssHurdle(SCOTocc, SCOTcc, scot))
scot_test <- ddply(scot_test, .(date, winter), summarise,
                   total_effort = sum(obs_window * covar_sds["obs_window"] + covar_avgs["obs_window"]),
                   est_count = sum(exp(add_pred)))
scot_compare <- left_join(scot_obs, scot_test)

# LTDU
load("../Results_ltdu/zero.Rda")
LTDUocc <- zero

load("../Results_ltdu/hurdle.Rda")
LTDUcc <- hurdle

ltdu_test <- data.frame(ltdu, predict.gamlssHurdle(LTDUocc, LTDUcc, ltdu))
ltdu_test <- ddply(ltdu_test, .(date, winter), summarise,
                   total_effort = sum(obs_window * covar_sds["obs_window"] + covar_avgs["obs_window"]),
                   est_count = sum(exp(add_pred)))
ltdu_compare <- left_join(ltdu_obs, ltdu_test)

all_ducks <- data.frame(spp = rep(c("COEI", "SCOT", "LTDU"), each=30),
                      rbind(coei_compare, scot_compare, ltdu_compare))
all_ducks$spp <- factor(all_ducks$spp, levels = c("COEI", "SCOT", "LTDU"))

#ggplot(all_ducks, aes(x=log(est_count), y=log(total_count))) + 
p <- ggplot(all_ducks, aes(x=total_count, y=est_count)) +
    coord_equal() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
    geom_point(aes(shape = spp, fill = spp), size = 2.5) + 
    scale_fill_manual("", values = viridis(3)) +
    scale_shape_manual("", values = c(21, 22, 24)) +
    scale_x_log10("Observed abundance", limits = c(40, 10^5), breaks = 10^(2:5), expand = c(0,0)) + 
    scale_y_log10("Predicted abundance", limits = c(40, 10^5), breaks = 10^(2:5), expand = c(0,0)) +
    theme_classic() + 
    theme(legend.position = c(0, 1), legend.justification = c(0.1, 0.75),
          legend.direction = "vertical",
          axis.line.x = element_line(color="black"), # These two lines are workaround for ggplot2 bug
          axis.line.y = element_line(color="black")) 

## CREATE FILE
#png(file = "./Figures/Predicted_abundance_observed_abundance.png", height = 107/1.25, width = 107, units = "mm", res = 600)
p
dev.off()
