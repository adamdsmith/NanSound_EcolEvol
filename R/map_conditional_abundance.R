## MAKE BLANK "PLOTS" FOR USE AS SPACING IN THE FINAL FIGURE
# Specification of `data` and `vars` irrelevant, so long as not NULL or non-existent variable
blank_plots <- ggplot_effects(data=0, vars=rep("blank", 2), varTypes = rep("blank", 2)) 

## Species conditional abundance plots
# COEI
coei_c_abund <- NanSound_plot(data=coei_final, z = "c_abund", type = "count", hotspot=2,
                             label = "COEI", legend.title = "Conditional\nabundance\n(median)")
coei_c_abund_mad_m <- NanSound_plot(data=coei_final, z = "c_abund_mad_m", type = "count", hotspot=2,
                                 legend.title = "Conditional\nabundance\n(MAD/median)")

# SCOT
scot_c_abund <- NanSound_plot(data=scot_final, z = "c_abund", type = "count", hotspot=2,
                             label = "SCOT", legend.title = "Conditional\nabundance\n(median)")
scot_c_abund_mad_m <- NanSound_plot(data=scot_final, z = "c_abund_mad_m", type = "count", hotspot=2,
                                 legend.title = "Conditional\nabundance\n(MAD/median)")

# LTDU
ltdu_c_abund <- NanSound_plot(data=ltdu_final, z = "c_abund", hotspot=2, type = "count", 
                             label = "LTDU", legend.title = "Conditional\nabundance\n(median)")
ltdu_c_abund_mad_m <- NanSound_plot(data=ltdu_final, z = "c_abund_mad_m", type = "count", hotspot=2,
                                 legend.title = "Conditional\nabundance\n(MAD/median)")

# Assemble them
png(file = "./Figures/conditional_abundance_maps.png", width = 18.5, height = 10, 
    units = "in", res = 600)
grid.arrange(do.call(arrangeGrob, list(coei_c_abund,coei_c_abund_mad_m, nrow=2)),
             do.call(arrangeGrob, c(blank_plots, list(nrow=2))),
             do.call(arrangeGrob, list(scot_c_abund, scot_c_abund_mad_m, nrow=2)),
             do.call(arrangeGrob, c(blank_plots, list(nrow=2))),
             do.call(arrangeGrob, list(ltdu_c_abund, ltdu_c_abund_mad_m, nrow=2)),
             ncol = 5, widths = c(1.2, 0.05, 1.2, 0.05, 1.2))
dev.off()
