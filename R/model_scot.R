pdf("../Results_scot/scot.pdf", width = 12)
mc.cores <- 13
mc.cores2 <- 5

################################################################################
## Load libraries and data
################################################################################

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


if (!file.exists("../Data/data_scot.Rda")) {
    # Bring in final data
    load("../Data/ducks&environment.RData", verbose=TRUE)

    # Subset observations for common eider (COEI)
    # The same will be done for scoters (SCOT) and long-tailed ducks (LTDU)
    obs.scot <- subset(obs.final, group == "SCOT")

    # Subset environmental data to those segments actually surveyed on a given date,
    # i.e., transect length within a segment > 0
    env.scot <- subset(env.segs, length > 0)

    # Consolidate observations with corresponding environmental variables and
    # replace resulting NA counts with zeros (0)
    # '<NA>' in group column is irrelevant
    scot <- within(merge(env.scot, obs.scot, all = TRUE), {
      count <- ifelse(is.na(count), 0, count)
    })

    # Make ferry a factor
    scot$ferry <- as.factor(scot$ferry)

    # Drop group
    scot$group <- NULL

    # add observation window (dependent on length) a covariate
    scot$obs_window <- scot$length * 91.44/1000 * 2

    # Overview of all variables in data set
    pacman::p_load(papeR)
    scot <- as.labeled.data.frame(scot)
    #pdf("../Output/Exploratory/scot_variables.pdf")
    #plot(scot)
    #dev.off()

    # Extract certain variables
    nms <- names(scot) %in% c('date', 'seg', 'count', 'length')
    fac <- sapply(scot, is.factor)

    # Center continuous covariates and add explicit intercept in case we want to decompose splines
    scot <- data.frame(scot[, nms],
                       scale(scot[, !nms & !fac], center = TRUE, scale = TRUE),
                       scot[, fac],
                       int = 1)

    # Add separate dummy variables for winter == 2004 and winter == 2005
    scot$y2004 <- factor(scot$winter == 2004, labels = c("no", "yes"))
    scot$y2005 <- factor(scot$winter == 2005, labels = c("no", "yes"))
    scot[sample(1:nrow(scot), 10), c("winter", "y2004", "y2005")] # ok!
    save("scot", file = "../Data/data_scot.Rda")
} else {
    load("../Data/data_scot.Rda", verbose = TRUE)
}
summary(scot)

################################################################################
################################################################################
################################################################################




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
if (!file.exists("../Results_scot/hurdle.Rda")) {
    scot_zerotrunc <- scot[scot$count > 0,]
    ## offset <- scot_zerotrunc$length * 91.44/1000 * 2
    # We cannot specify this offset in the model as it becomes very small and
    # hence the outcome, which is then count/offset becomes incredibly huge
    # Instead: Use length as covariate
    hurdle <- gamboostLSS(scot_fm2,
                          data = scot_zerotrunc, control = ctrl,
                          families = as.families("NBItr"))
    save("hurdle", file = "../Results_scot/hurdle.Rda")
} else {
    scot_zerotrunc <- scot[scot$count > 0,]
    load("../Results_scot/hurdle.Rda", verbose=TRUE)
    mstop(hurdle) <- c(2000, 200)
}

## Now, we need to cross-validate the model. Be careful, this really takes some
## time. You should run this on a compute cluster only. (I could do this once we
## are more or less sure about the model).

## example for a cross-validation grid
grid <- make.grid(max = c(mu = 50000, sigma = 4000),
                  length.out = c(20, 10),
                  min = c(2000, 1000))
#plot(grid)
if (!file.exists("../Results_scot/cvr_hurdle.Rda")) {
    set.seed(1907)
    suppressWarnings(cvr_hurdle <- cvrisk(hurdle, grid = grid,
                         folds = cv(model.weights(hurdle), type = "subsampling"),
                         mc.cores = mc.cores))
    save("cvr_hurdle", file = "../Results_scot/cvr_hurdle.Rda")
} else {
    load("../Results_scot/cvr_hurdle.Rda", verbose=TRUE)
}

plot(cvr_hurdle)
mstop(cvr_hurdle)

if (any(mstop(cvr_hurdle) != mstop(hurdle))) {
    hurdle[mstop(cvr_hurdle)]
    save("hurdle", file = "../Results_scot/hurdle.Rda")
} else {
    load("../Results_scot/hurdle.Rda", verbose=TRUE)
}

## selected base-learners:
lapply(coef(hurdle), names)

## stabsel with q = 35
if (!file.exists("../Results_scot/stabs_hurdle_q35.Rda")) {
    set.seed(2202)
    suppressWarnings(stabs_hurdle_q35 <- stabsel(hurdle, mstop = 20000,
                                             q = 35, PFER = 2, # eval = FALSE,
                                             mc.cores = mc.cores2))
    save("stabs_hurdle_q35", file = "../Results_scot/stabs_hurdle_q35.Rda")
} else {
    load("../Results_scot/stabs_hurdle_q35.Rda", verbose=TRUE)
}

##################
## Zero Part

rm(list = c("hurdle", "cvr_hurdle"))

if (!file.exists("../Results_scot/zero.Rda")) {
    scot_zero <- scot
    scot_zero$count <- factor(scot_zero$count > 0, labels = c("0", ">0"))
    zero <- gamboost(scot_fm2, data = scot_zero, control = ctrl,
                     family = Binomial())
    save("zero", file = "../Results_scot/zero.Rda")
} else {
    load("../Results_scot/zero.Rda", verbose=TRUE)
}

if (!file.exists("../Results_scot/cvr_zero.Rda")) {
    set.seed(1907)
    suppressWarnings(cvr_zero <- cvrisk(zero, grid = 1:10000,
                       folds = cv(model.weights(zero), type = "subsampling"),
                       mc.cores = mc.cores))
    save("cvr_zero", file = "../Results_scot/cvr_zero.Rda")
} else {
    load("../Results_scot/cvr_zero.Rda", verbose=TRUE)
}
plot(cvr_zero)

if (mstop(cvr_zero) != mstop(zero)) {
    zero[mstop(cvr_zero)]
    save("zero", file = "../Results_scot/zero.Rda")
} else {
    load("../Results_scot/zero.Rda", verbose=TRUE)
}
## it looks like the model converges towards the LS solution, i.e. early
## stopping does not "work"/apply here.

## selected base-learners:
names(coef(zero))

if (!file.exists("../Results_scot/stabs_zero_q35.Rda")) {
    set.seed(2202)
    suppressWarnings(stabs_zero_q35 <- stabsel(zero, q = 35, PFER = 2, # eval = FALSE,
                                               mc.cores = mc.cores2))
    save("stabs_zero_q35", file = "../Results_scot/stabs_zero_q35.Rda")
} else {
    load("../Results_scot/stabs_zero_q35.Rda", verbose=TRUE)
}

dev.off()
