dir.create("../Results_coei/", showWarnings = FALSE)

pdf("../Results_coei/coei.pdf", width = 12)
mc.cores <- 25
mc.cores2 <- 5

################################################################################
## Load libraries and data
################################################################################

# Need specific versions of packages for the saved models to run
pacman::p_load(devtools)
if (!requireNamespace("mboost", quietly = TRUE))
  devtools::install_version("mboost", version = "2.4-2", repos = "http://cran.us.r-project.org")
if (packageVersion("mboost") != "2.4.2")
  devtools::install_version("mboost", version = "2.4-2", repos = "http://cran.us.r-project.org")
if (!requireNamespace("gamboostLSS", quietly = TRUE))
  devtools::install_version("gamboostLSS", version = "1.2-0", repos = "http://cran.us.r-project.org")
if (packageVersion("gamboostLSS") != "1.2.0")
  devtools::install_version("gamboostLSS", version = "1.2-0", repos = "http://cran.us.r-project.org")
pacman::p_load(stabs)

if (!file.exists("../Data/data_coei.Rda")) {
    # Bring in final data
    load("../Data/ducks&environment.RData", verbose=TRUE)

    # Subset observations for common eider (COEI)
    # The same will be done for scoters (SCOT) and long-tailed ducks (LTDU)
    obs.coei <- subset(obs.final, group == "COEI")

    # Subset environmental data to those segments actually surveyed on a given date,
    # i.e., transect length within a segment > 0
    env.coei <- subset(env.segs, length > 0)

    # Consolidate observations with corresponding environmental variables and
    # replace resulting NA counts with zeros (0)
    # '<NA>' in group column is irrelevant
    coei <- within(merge(env.coei, obs.coei, all = TRUE), {
      count <- ifelse(is.na(count), 0, count)
    })

    # Make ferry a factor
    coei$ferry <- as.factor(coei$ferry)

    # Drop group
    coei$group <- NULL

    # add observation window (dependent on length) a covariate
    coei$obs_window <- coei$length * 91.44/1000 * 2

    # Overview of all variables in data set
    pacman::p_load(papeR)
    coei <- as.labeled.data.frame(coei)
    #pdf("../Output/Exploratory/coei_variables.pdf")
    #plot(coei)
    #dev.off()

    # Extract certain variables
    nms <- names(coei) %in% c('date', 'seg', 'count', 'length')
    fac <- sapply(coei, is.factor)

    # Center continuous covariates and add explicit intercept in case we want to decompose splines
    coei <- data.frame(coei[, nms],
                       scale(coei[, !nms & !fac], center = TRUE, scale = TRUE),
                       coei[, fac],
                       int = 1)

    # Add separate dummy variables for winter == 2004 and winter == 2005
    coei$y2004 <- factor(coei$winter == 2004, labels = c("no", "yes"))
    coei$y2005 <- factor(coei$winter == 2005, labels = c("no", "yes"))
    coei[sample(1:nrow(coei), 10), c("winter", "y2004", "y2005")] # ok!
    save("coei", file = "../Data/data_coei.Rda")
} else {
    load("../Data/data_coei.Rda", verbose = TRUE)
}
summary(coei)

################################################################################
## Set up boosting models
################################################################################

# Boosting control parameters
ctrl <- boost_control(mstop = 1000, nu = 0.3, trace = TRUE)

# Set up formula
source("../R/model_formula_decomp.R", echo = TRUE, max.deparse.length = 10000)

################################################################################
## Hurdle negative binomial model
################################################################################

##################
## Hurdle Part

pacman::p_load(gamlss.tr)
## generates: dNBItr pNBItr qNBItr rNBItr NBItr
gen.trun(0, family = "NBI")

## now one can use the newly built family
if (!file.exists("../Results_coei/hurdle.Rda")) {
    coei_zerotrunc <- coei[coei$count > 0,]
    ## offset <- coei_zerotrunc$length * 91.44/1000 * 2
    # We cannot specify this offset in the model as it becomes very small and
    # hence the outcome, which is then count/offset becomes incredibly huge
    # Instead: Use length as covariate
    hurdle <- gamboostLSS(coei_fm2,
                          data = coei_zerotrunc, control = ctrl,
                          families = as.families("NBItr"))
    save("hurdle", file = "../Results_coei/hurdle.Rda")
} else {
    load("../Results_coei/hurdle.Rda", verbose=TRUE)
}

## Now, we need to cross-validate the model. Be careful, this really takes some
## time. You should run this on a compute cluster only. (I could do this once we
## are more or less sure about the model).

## example for a cross-validation grid
grid <- make.grid(max = c(mu = 20000, sigma = 1000),
                  length.out = c(20, 15),
                  min = c(200, 20))
#plot(grid)
if (!file.exists("../Results_coei/cvr_hurdle.Rda")) {
    set.seed(1907)
    suppressWarnings(cvr_hurdle <- cvrisk(hurdle, grid = grid,
                         folds = cv(model.weights(hurdle), type = "subsampling"),
                         mc.cores = mc.cores))
    save("cvr_hurdle", file = "../Results_coei/cvr_hurdle.Rda")
} else {
    load("../Results_coei/cvr_hurdle.Rda", verbose=TRUE)
}

plot(cvr_hurdle)

if (any(mstop(cvr_hurdle) != mstop(hurdle))) {
    hurdle[mstop(cvr_hurdle)]
    save("hurdle", file = "../Results_coei/hurdle.Rda")
} else {
    load("../Results_coei/hurdle.Rda", verbose=TRUE)
}

## selected base-learners:
lapply(coef(hurdle), names)

# pdf("effect_estimates_mu.pdf")
# plot(hurdle, parameter = "mu", which = "bspatial")
# dev.off()

# pdf("effect_estimates_sigma.pdf")
# plot(hurdle, parameter = "sigma")
# dev.off()

## ## stabsel with q = 15
## if (!file.exists("stabs_hurdle.Rda")) {
##     set.seed(2202)
##     suppressWarnings(stabs_hurdle <- stabsel(hurdle, mstop = 20000,
##                                              q = 15, PFER = 2, # eval = FALSE,
##                                              mc.cores = mc.cores2))
##     save("stabs_hurdle", file = "stabs_hurdle.Rda")
## } else {
##     load("stabs_hurdle.Rda", verbose=TRUE)
## }
##
## ## stabsel with q = 25
## if (!file.exists("stabs_hurdle_q25.Rda")) {
##     set.seed(2202)
##     suppressWarnings(stabs_hurdle_q25 <- stabsel(hurdle, mstop = 20000,
##                                              q = 25, PFER = 2, # eval = FALSE,
##                                              mc.cores = mc.cores2))
##     save("stabs_hurdle_q25", file = "stabs_hurdle_q25.Rda")
## } else {
##     load("stabs_hurdle_q25.Rda", verbose=TRUE)
## }

## stabsel with q = 35
if (!file.exists("../Results_coei/stabs_hurdle_q35.Rda")) {
    set.seed(2202)
    suppressWarnings(stabs_hurdle_q35 <- stabsel(hurdle, mstop = 20000,
                                             q = 35, PFER = 2, # eval = FALSE,
                                             mc.cores = mc.cores2))
    save("stabs_hurdle_q35", file = "../Results_coei/stabs_hurdle_q35.Rda")
} else {
    load("../Results_coei/stabs_hurdle_q35.Rda", verbose=TRUE)
}

##################
## Zero Part

rm(list = c("hurdle", "cvr_hurdle"))

if (!file.exists("../Results_coei/zero.Rda")) {
    coei_zero <- coei
    coei_zero$count <- factor(coei_zero$count > 0, labels = c("0", ">0"))
    zero <- gamboost(coei_fm2, data = coei_zero, control = ctrl,
                     family = Binomial())
    save("zero", file = "../Results_coei/zero.Rda")
} else {
    load("../Results_coei/zero.Rda", verbose=TRUE)
}

if (!file.exists("../Results_coei/cvr_zero.Rda")) {
    set.seed(1907)
    suppressWarnings(cvr_zero <- cvrisk(zero, grid = 1:10000,
                       folds = cv(model.weights(zero), type = "subsampling"),
                       mc.cores = mc.cores))
    save("cvr_zero", file = "../Results_coei/cvr_zero.Rda")
} else {
    load("../Results_coei/cvr_zero.Rda", verbose=TRUE)
}
plot(cvr_zero)

if (mstop(cvr_zero) != mstop(zero)) {
    zero[mstop(cvr_zero)]
    save("zero", file = "../Results_coei/zero.Rda")
} else {
    load("../Results_coei/zero.Rda", verbose=TRUE)
}
## it looks like the model converges towards the LS solution, i.e. early
## stopping does not "work"/apply here.

## selected base-learners:
names(coef(zero))

# pdf("effect_estimates_zero.pdf")
# plot(zero)
# dev.off()

## if (!file.exists("stabs_zero.Rda")) {
##     set.seed(2202)
##     suppressWarnings(stabs_zero <- stabsel(zero, q = 15, PFER = 2, # eval = FALSE,
##                                            mc.cores = mc.cores2))
##     save("stabs_zero", file = "stabs_zero.Rda")
## } else {
##     load("stabs_zero.Rda", verbose=TRUE)
## }

if (!file.exists("../Results_coei/stabs_zero_q35.Rda")) {
    set.seed(2202)
    suppressWarnings(stabs_zero_q35 <- stabsel(zero, q = 35, PFER = 2, # eval = FALSE,
                                               mc.cores = mc.cores2))
    save("stabs_zero_q35", file = "../Results_coei/stabs_zero_q35.Rda")
} else {
    load("../Results_coei/stabs_zero_q35.Rda", verbose=TRUE)
}

dev.off()
