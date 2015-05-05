## MAKE BLANK "PLOTS" FOR USE AS SPACING IN THE FINAL FIGURE
# Specification of `data` and `vars` irrelevant, so long as not NULL or non-existent variable
blank_plots <- ggplot_effects(data=0, vars=rep("blank", 2), varTypes = rep("blank", 2)) 

## Species occupancy plots
# COEI
coei_occ <- NanSound_plot(data=coei_final, z = "occup", hotspot=2, label = "COEI",
                          legend.title = "Occupancy\n(median)")
coei_occ_mad <- NanSound_plot(data=coei_final, z = "occup_mad", hotspot=2,
                              legend.title = "Occupancy\n(MAD)")

# SCOT
scot_occ <- NanSound_plot(data=scot_final, z = "occup", hotspot=2,
                          legend.title = "Occupancy\n(median)", label = "SCOT")
scot_occ_mad <- NanSound_plot(data=scot_final, z = "occup_mad", hotspot=2,
                              legend.title = "Occupancy\n(MAD)")

# LTDU
ltdu_occ <- NanSound_plot(data=ltdu_final, z = "occup", hotspot=2, label = "LTDU",
                         legend.title = "Occupancy\n(median)")
ltdu_occ_mad <- NanSound_plot(data=ltdu_final, z = "occup_mad", hotspot=2,
                              legend.title = "Occupancy\n(MAD)")

# Assemble them
png(file = "./Figures/occupancy_maps.png", width = 18.5, height = 10, units = "in", res = 600)
grid.arrange(do.call(arrangeGrob, list(coei_occ, coei_occ_mad, nrow=2)),
             do.call(arrangeGrob, c(blank_plots, list(nrow=2))),
             do.call(arrangeGrob, list(scot_occ, scot_occ_mad, nrow=2)),
             do.call(arrangeGrob, c(blank_plots, list(nrow=2))),
             do.call(arrangeGrob, list(ltdu_occ, ltdu_occ_mad, nrow=2)),
             ncol = 5, widths = c(1.2, 0.05, 1.2, 0.05, 1.2))
dev.off()
