instant_pkgs("lubridate")

## Create standardized (when appropriate) environmental data on which to predict from
## selected models for each species

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

# Don't copy over env.segs
standardized.segs <- env.segs

# Standardize appropriate variables
for (i in seq(ncol(standardized.segs))) {
  cur_var <- names(standardized.segs)[i]
  if (cur_var %in% names(covar_avgs)) {
    standardized.segs[, cur_var] <- 
      (standardized.segs[, cur_var] - covar_avgs[cur_var]) / covar_sds[cur_var]
  }
}

# Factorize ferry variable
standardized.segs$ferry <- as.factor(standardized.segs$ferry)

# Make data sets comparable (i.e., weighted equally) for each year
# To do this, we select 10 evenly spaced dates from the time periods surveyed
# each year (~15 November through ~1 April).  Care must be taken to make sure
# the appropriate temporal variables are associated with each date

# Set dates
pred_dates <- mdy("11012005") 
yday(pred_dates) <- 305 + seq(0, 150, 2)

# Add winter and month to associate with temporally-changing variables
pred_data <- data.frame(date = pred_dates,
                        mo = month(pred_dates),
                        winter = factor(ifelse(month(pred_dates) > 5, 
                                               year(pred_dates), year(pred_dates) - 1),
                                        levels = c("2003", "2004", "2005")))

# Add time on scale used in this study
pred_data <- mutate(pred_data,
                    time = as.integer(difftime(date, as.POSIXct("2005-12-31"), units="days")),
                    time = (time - covar_avgs["time"]) / covar_sds["time"],
                    obs_window = ((1.5 * 91.44/1000 * 2) - covar_avgs["obs_window"]) / covar_sds["obs_window"],
                    int = 1,
                    y2004 = factor(winter == 2004, levels = c(FALSE, TRUE), labels = c("no", "yes")),
                    y2005 = factor(winter == 2005, levels = c(FALSE, TRUE), labels = c("no", "yes")))

# Isolate variables that change yearly and perhaps in space, and join
yearly_vars <- unique(standardized.segs[, c("seg", "winter", "SSTw", "NAOw", "SBT")])

# Conveniently (sarcasm), monthly sea surface temperature (and thus also SSTrel) are
# missing because surveys did not occur in Jan 2005 or 2006, or April 2006
# We have to add them here from a special script
# Not run, takes a while; loading object instead
# source("../R/retrieve_SSTs.R")
load("../Data/all_SSTs.Rda")

# Isolate variables that change only in space, not over time and create data frame
spatial_vars <- unique(standardized.segs[, c("seg", "x", "y", "xkm", "ykm", "chla", "cdom", "tidebmean",
                                             "tidesd", "strat", "SAR", "ferry", "d2land", "depth",
                                             "meanphi")])

# Join dates with spatial variables, then link with temporally changing variables
pred_data <- expand.grid.df(pred_data, spatial_vars)
pred_data <- left_join(pred_data, yearly_vars)
pred_data <- left_join(pred_data, all_ssts)
scot_anim <- pred_data


load("C:/Users/Adam/OneDrive/NanSound/Results_scot/zero.rda")
#load("../Results_scot/zero.Rda")
load("C:/Users/Adam/OneDrive/NanSound/Results_scot/hurdle.rda")
#load("../Results_scot/hurdle.Rda")
SCOTocc <- zero
SCOTcc <- hurdle

scot_anim$occup <- predict(SCOTocc, newdata=scot_anim, type = "response")
scot_anim$ltabund <- exp(predict.gamlssHurdle(SCOTocc, SCOTcc, scot_anim)$add_pred)

rm("zero", "hurdle", "SCOTocc", "SCOTcc")
gc(reset = TRUE)