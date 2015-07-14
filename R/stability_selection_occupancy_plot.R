source("../R/tidy_baselearner_functions.R")

## Load occupancy models to get baselearner names
load("C:/Users/Adam/OneDrive/NanSound/Results_coei/zero.rda")
#load("../Results_coei/zero.Rda")
COEIocc <- zero

labels <- tidy_baselearner_names_plot(COEIocc, which = "")
png(file="./Figures/stability_selection_occupancy.png",w=9, h=5.5, res=1200, units = "in")
par(mar=c(4,9,1.5,1))
layout(matrix(1:3, nrow=1))
plot(coei_stabs_zero, np = 48, labels = labels, main="Common Eider")
plot(scot_stabs_zero, np = 48, labels = labels, main="Scoters")
plot(ltdu_stabs_zero, np = 48, labels = labels, main="Long-tailed Duck")
dev.off()

rm("COEIocc", "zero")
gc(reset=TRUE)