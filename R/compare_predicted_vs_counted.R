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
all_ducks$spp <- factor(all_ducks$spp, levels = c("COEI", "SCOT", "LTDU"))

#ggplot(all_ducks, aes(x=log(est_count), y=log(total_count))) + 
p <- ggplot(all_ducks, aes(x=total_count, y=est_count)) +
  coord_equal() +
  geom_point(aes(group=spp, shape = spp, size=spp, fill = spp), alpha=0.75) + 
  scale_fill_brewer(type = "qual", palette="Set2", guide = guide_legend(title = NULL)) +
  scale_shape_manual(values = c(21, 22, 24), guide = guide_legend(title = NULL)) +
  scale_size_manual(values = c(2,2,1.75), guide = guide_legend(title = NULL)) +
  geom_abline(intercept=0, slope=1, linetype="dashed") + 
  theme_classic() +
  scale_x_log10("Observed abundance", limits=c(50,10^5), breaks=10^(1:5)) + 
  scale_y_log10("Predicted abundance", limits=c(50,10^5), breaks=10^(1:5)) +
  theme(legend.position = c(0, 1), legend.justification = c(0.1, 0.75),
        legend.direction = "vertical")

## CREATE FILE
#png(file = "./Figures/Predicted_abundance_observed_abundance.png", height = 5.5/1.38753, width = 5.5, units = "in", res = 600)
p
#dev.off()
