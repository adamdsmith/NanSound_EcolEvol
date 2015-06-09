require(plyr)

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

# Sum of predicted sea duck counts, by species (group), for 30 surveys
coei_test <- data.frame(coei, predict.gamlssHurdle(COEIocc, COEIcc, coei))
coei_test <- ddply(coei_test, .(date, winter), summarise,
                   total_effort = sum(obs_window * covar_sds["obs_window"] + covar_avgs["obs_window"]),
                   est_count = sum(exp(add_pred)))
coei_compare <- left_join(coei_obs, coei_test)

scot_test <- data.frame(scot, predict.gamlssHurdle(SCOTocc, SCOTcc, scot))
scot_test <- ddply(scot_test, .(date, winter), summarise,
                   total_effort = sum(obs_window * covar_sds["obs_window"] + covar_avgs["obs_window"]),
                   est_count = sum(exp(add_pred)))
scot_compare <- left_join(scot_obs, scot_test)

ltdu_test <- data.frame(ltdu, predict.gamlssHurdle(LTDUocc, LTDUcc, ltdu))
ltdu_test <- ddply(ltdu_test, .(date, winter), summarise,
                   total_effort = sum(obs_window * covar_sds["obs_window"] + covar_avgs["obs_window"]),
                   est_count = sum(exp(add_pred)))
ltdu_compare <- left_join(ltdu_obs, ltdu_test)

all_ducks <- data.frame(spp = rep(c("COEI", "SCOT", "LTDU"), each=30),
                      rbind(coei_compare, scot_compare, ltdu_compare))

#ggplot(all_ducks, aes(x=log(est_count), y=log(total_count))) + 
p <- ggplot(all_ducks, aes(x=total_count, y=est_count)) +
  coord_equal() +
  geom_point(aes(group=spp, shape = spp, size=spp)) + 
  scale_shape_manual(values = c(21, 22, 24), guide = guide_legend(title = NULL)) +
  scale_size_manual(values = c(2,2,1.75), guide = guide_legend(title = NULL)) +
  geom_abline(intercept=0, slope=1, linetype="dashed") + 
  theme_classic() +
  scale_x_continuous("Observed abundance") + scale_y_continuous("Predcited abundance") +
  theme(legend.position = c(1, 1), legend.justification = c(1, 0.75),
        legend.direction = "vertical")

## CREATE FILE
#png(file = "./Figures/Predicted_abundance_observed_abundance.png", height = 6.5/1.38753, width = 6.5, units = "in", res = 600)
p
#dev.off()
