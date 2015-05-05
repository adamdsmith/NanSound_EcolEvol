## Load count model bootstrapping results for each species and rename
load("../Results_coei/cvr_hurdle.Rda", verbose=TRUE)
cvr_COEIcc <- cvr_hurdle

load("../Results_scot/cvr_hurdle.Rda", verbose=TRUE)
cvr_SCOTcc <- cvr_hurdle

load("../Results_ltdu/cvr_hurdle.Rda", verbose=TRUE)
cvr_LTDUcc <- cvr_hurdle

rm(cvr_hurdle)

png(file="./Figures/early_stopping_count.png",w=6.5, h=2, res=1200, units = "in")
par(mar = c(3.5, 3, 1.5, 1))
layout(matrix(1:3, nrow=1))
plot(cvr_COEIcc, main = "Common Eider", xlab="", ylab="")
title(ylab = expression(paste("Number of boosting iterations (", sigma, ")")), line=2)
plot(cvr_SCOTcc, main = "Scoter", ylab="", xlab="")
title(xlab = expression(paste("Number of boosting iterations (", mu, ")")), line=2.5)
plot(cvr_LTDUcc, main = "Long-tailed Duck", xlab="", ylab="")
dev.off()