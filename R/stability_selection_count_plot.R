source("../R/tidy_baselearner_functions.R")

## Load occupancy models to get baselearner names
load("../Results_coei/hurdle.Rda")

labels <- sapply(hurdle, function(x) tidy_baselearner_names_plot(x, which = ""))
labels <- paste0(c(labels), " [", rep(colnames(labels), each = nrow(labels)), "]")
#png(file="./Figures/stability_selection_count.png",w=9, h=5.25, res=1200, units = "in")
png(file="./Figures/stability_selection_count_reduced.png",w=9, h=5.25, res=150, units = "in")
par(mar=c(4,10,1.5,1))
layout(matrix(1:3, nrow=1))
plot(coei_stabs_hurdle, np = 48, labels = labels, main="Common Eider")
plot(scot_stabs_hurdle, np = 48, labels = labels, main="Scoters")
plot(ltdu_stabs_hurdle, np = 48, labels = labels, main="Long-tailed Duck")
dev.off()

rm("hurdle")
gc(reset=TRUE)
