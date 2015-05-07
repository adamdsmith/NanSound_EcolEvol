## MAKE BLANK "PLOTS" FOR USE AS SPACING IN THE FINAL FIGURE
# Specification of `data` and `vars` irrelevant, so long as not NULL or non-existent variable
blank_plots <- ggplot_effects(data=0, vars=rep("blank", 2), varTypes = rep("blank", 2)) 

## Species conditional overdispersion plots
# COEI
coei_c_overd <- NanSound_plot(data=coei_final, z = "c_overd", type = "count", hotspot=2,
                             label = "COEI", legend.title = "Conditional\noverdispersion\n(median)")
coei_c_overd_mad <- NanSound_plot(data=coei_final, z = "c_overd_mad", type = "count", hotspot=2,
                                 legend.title = "Conditional\noverdispersion\n(MAD)")

# SCOT
scot_c_overd <- NanSound_plot(data=scot_final, z = "c_overd", type = "count", hotspot=2,
                             label = "SCOT", legend.title = "Conditional\noverdispersion\n(median)")
scot_c_overd_mad <- NanSound_plot(data=scot_final, z = "c_overd_mad", type = "count", hotspot=2,
                                 legend.title = "Conditional\noverdispersion\n(MAD)")

# LTDU
ltdu_c_overd <- NanSound_plot(data=ltdu_final, z = "c_overd", hotspot=2, type = "count", 
                             label = "LTDU", legend.title = "Conditional\noverdispersion\n(median)")
ltdu_c_overd_mad <- NanSound_plot(data=ltdu_final, z = "c_overd_mad", type = "count", hotspot=2,
                                 legend.title = "Conditional\noverdispersion\n(MAD)")

# Assemble them
png(file = "./Figures/conditional_overdispersion_maps.png", width = 18.5, height = 10, 
    units = "in", res = 600)
grid.arrange(do.call(arrangeGrob, list(coei_c_overd,coei_c_overd_mad, nrow=2)),
             do.call(arrangeGrob, c(blank_plots, list(nrow=2))),
             do.call(arrangeGrob, list(scot_c_overd, scot_c_overd_mad, nrow=2)),
             do.call(arrangeGrob, c(blank_plots, list(nrow=2))),
             do.call(arrangeGrob, list(ltdu_c_overd, ltdu_c_overd_mad, nrow=2)),
             ncol = 5, widths = c(1.2, 0.05, 1.2, 0.05, 1.2))
dev.off()
