## Load occupancy bootstrapping results for each species and rename 
load("../Results_coei/cvr_zero.Rda", verbose=TRUE)
cvr_COEIocc <- cvr_zero

load("../Results_scot/cvr_zero.Rda", verbose=TRUE)
cvr_SCOTocc <- cvr_zero

load("../Results_ltdu/cvr_zero.Rda", verbose=TRUE)
cvr_LTDUocc <- cvr_zero

rm(cvr_zero)

png(file="./Figures/early_stopping_occupancy.png",w=6.5, h=2, res=1200, units = "in")
par(mar = c(3.5, 3, 1.5, 1))
layout(matrix(1:3, nrow=1))
plot(cvr_COEIocc, main = "Common Eider", xlab="", ylab="")
title(ylab = "Negative binomial likelihood", line=2)
plot(cvr_SCOTocc, main = "Scoter", ylab="", xlab="")
title(xlab = "Number of boosting iterations", line=2.5)
plot(cvr_LTDUocc, main = "Long-tailed Duck", xlab="", ylab="")
dev.off()