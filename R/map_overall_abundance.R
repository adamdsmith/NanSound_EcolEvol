## MAKE BLANK "PLOTS" FOR USE AS SPACING IN THE FINAL FIGURE
# Specification of `data` and `vars` irrelevant, so long as not NULL or non-existent variable
blank_plots <- ggplot_effects(data=0, vars=rep("blank", 2), varTypes = rep("blank", 2)) 

## Species overall abundance plots
# COEI
coei_lt_abund <- NanSound_plot(data=coei_final, z = "lt_abund", type = "count", hotspot=2,
                              label = "COEI", legend.title = "Abundance\n(median)", plotwind = TRUE)
coei_lt_abund_mad_m <- NanSound_plot(data=coei_final, z = "lt_abund_mad_m", type = "count", hotspot=2,
                                  legend.title = "Abundance\n(MAD/median)", plotwind = TRUE)

# SCOT
scot_lt_abund <- NanSound_plot(data=scot_final, z = "lt_abund", type = "count", hotspot=2,
                              label = "SCOT", legend.title = "Abundance\n(median)", plotwind = TRUE)
scot_lt_abund_mad_m <- NanSound_plot(data=scot_final, z = "lt_abund_mad_m", type = "count", hotspot=2,
                                  legend.title = "Abundance\n(MAD/median)", plotwind = TRUE)

# LTDU
ltdu_lt_abund <- NanSound_plot(data=ltdu_final, z = "lt_abund", type = "count", hotspot=2,
                              label = "LTDU", legend.title = "Abundance\n(median)", plotwind = TRUE)
ltdu_lt_abund_mad_m <- NanSound_plot(data=ltdu_final, z = "lt_abund_mad_m", type = "count", hotspot=2,
                                  legend.title = "Abundance\n(MAD/median)", plotwind = TRUE)

# Assemble them
png(file = "./Figures/overall_abundance_maps.png", width = 18.5, height = 10, 
    units = "in", res = 600)
grid.arrange(do.call(arrangeGrob, list(coei_lt_abund, coei_lt_abund_mad_m, nrow=2)),
             do.call(arrangeGrob, c(blank_plots, list(nrow=2))),
             do.call(arrangeGrob, list(scot_lt_abund, scot_lt_abund_mad_m, nrow=2)),
             do.call(arrangeGrob, c(blank_plots, list(nrow=2))),
             do.call(arrangeGrob, list(ltdu_lt_abund, ltdu_lt_abund_mad_m, nrow=2)),
             ncol = 5, widths = c(1.2, 0.05, 1.2, 0.05, 1.2))
dev.off()
