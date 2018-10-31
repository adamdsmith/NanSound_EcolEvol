pdf("../Results_ltdu/ltdu.pdf", width = 12)
mc.cores <- 25
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

if (!file.exists("../Data/data_ltdu.Rda")) {
    # Bring in final data
    load("../Data/ducks&environment.RData", verbose=TRUE)

    # Subset observations for common eider (COEI)
    # The same will be done for scoters (SCOT) and long-tailed ducks (LTDU)
    obs.ltdu <- subset(obs.final, group == "LTDU")

    # Subset environmental data to those segments actually surveyed on a given date,
    # i.e., transect length within a segment > 0
    env.ltdu <- subset(env.segs, length > 0)

    # Consolidate observations with corresponding environmental variables and
    # replace resulting NA counts with zeros (0)
    # '<NA>' in group column is irrelevant
    ltdu <- within(merge(env.ltdu, obs.ltdu, all = TRUE), {
      count <- ifelse(is.na(count), 0, count)
    })

    # Make ferry a factor
    ltdu$ferry <- as.factor(ltdu$ferry)

    # Drop group
    ltdu$group <- NULL

    # add observation window (dependent on length) a covariate
    ltdu$obs_window <- ltdu$length * 91.44/1000 * 2

    # Overview of all variables in data set
    pacman::p_load(papeR)
    ltdu <- as.labeled.data.frame(ltdu)
    #pdf("../Output/Exploratory/ltdu_variables.pdf")
    #plot(ltdu)
    #dev.off()

    # Extract certain variables
    nms <- names(ltdu) %in% c('date', 'seg', 'count', 'length')
    fac <- sapply(ltdu, is.factor)

    # Center continuous covariates and add explicit intercept in case we want to decompose splines
    ltdu <- data.frame(ltdu[, nms],
                       scale(ltdu[, !nms & !fac], center = TRUE, scale = TRUE),
                       ltdu[, fac],
                       int = 1)

    # Add separate dummy variables for winter == 2004 and winter == 2005
    ltdu$y2004 <- factor(ltdu$winter == 2004, labels = c("no", "yes"))
    ltdu$y2005 <- factor(ltdu$winter == 2005, labels = c("no", "yes"))
    ltdu[sample(1:nrow(ltdu), 10), c("winter", "y2004", "y2005")] # ok!
    save("ltdu", file = "../Data/data_ltdu.Rda")
} else {
    load("../Data/data_ltdu.Rda", verbose = TRUE)
}
summary(ltdu)


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
if (!file.exists("../Results_ltdu/hurdle.Rda")) {
    ltdu_zerotrunc <- ltdu[ltdu$count > 0,]
    ## offset <- ltdu_zerotrunc$length * 91.44/1000 * 2
    # We cannot specify this offset in the model as it becomes very small and
    # hence the outcome, which is then count/offset becomes incredibly huge
    # Instead: Use length as covariate
    hurdle <- gamboostLSS(ltdu_fm2,
                          data = ltdu_zerotrunc, control = ctrl,
                          families = as.families("NBItr"))
    save("hurdle", file = "../Results_ltdu/hurdle.Rda")
} else {
    load("../Results_ltdu/hurdle.Rda", verbose=TRUE)
}

## Now, we need to cross-validate the model. Be careful, this really takes some
## time. You should run this on a compute cluster only. (I could do this once we
## are more or less sure about the model).

## example for a cross-validation grid
grid <- make.grid(max = c(mu = 20000, sigma = 1000),
                  length.out = c(20, 15),
                  min = c(200, 20))
#plot(grid)
if (!file.exists("../Results_ltdu/cvr_hurdle.Rda")) {
    set.seed(1907)
    suppressWarnings(cvr_hurdle <- cvrisk(hurdle, grid = grid,
                         folds = cv(model.weights(hurdle), type = "subsampling"),
                         mc.cores = mc.cores))
    save("cvr_hurdle", file = "../Results_ltdu/cvr_hurdle.Rda")
} else {
    load("../Results_ltdu/cvr_hurdle.Rda", verbose=TRUE)
}

plot(cvr_hurdle)

if (any(mstop(cvr_hurdle) != mstop(hurdle))) {
    hurdle[mstop(cvr_hurdle)]
    save("hurdle", file = "../Results_ltdu/hurdle.Rda")
} else {
    load("../Results_ltdu/hurdle.Rda", verbose=TRUE)
}

## selected base-learners:
lapply(coef(hurdle), names)

## stabsel with q = 35
if (!file.exists("../Results_ltdu/stabs_hurdle_q35.Rda")) {
    set.seed(2202)
    suppressWarnings(stabs_hurdle_q35 <- stabsel(hurdle, mstop = 20000,
                                             q = 35, PFER = 2, # eval = FALSE,
                                             mc.cores = mc.cores2))
    save("stabs_hurdle_q35", file = "../Results_ltdu/stabs_hurdle_q35.Rda")
} else {
    load("../Results_ltdu/stabs_hurdle_q35.Rda", verbose=TRUE)
}

##################
## Zero Part

rm(list = c("hurdle", "cvr_hurdle"))

if (!file.exists("../Results_ltdu/zero.Rda")) {
    ltdu_zero <- ltdu
    ltdu_zero$count <- factor(ltdu_zero$count > 0, labels = c("0", ">0"))
    zero <- gamboost(ltdu_fm2, data = ltdu_zero, control = ctrl,
                     family = Binomial())
    save("zero", file = "../Results_ltdu/zero.Rda")
} else {
    load("../Results_ltdu/zero.Rda", verbose=TRUE)
}

if (!file.exists("../Results_ltdu/cvr_zero.Rda")) {
    set.seed(1907)
    suppressWarnings(cvr_zero <- cvrisk(zero, grid = 1:10000,
                       folds = cv(model.weights(zero), type = "subsampling"),
                       mc.cores = mc.cores))
    save("cvr_zero", file = "../Results_ltdu/cvr_zero.Rda")
} else {
    load("../Results_ltdu/cvr_zero.Rda", verbose=TRUE)
}
plot(cvr_zero)

if (mstop(cvr_zero) != mstop(zero)) {
    zero[mstop(cvr_zero)]
    save("zero", file = "../Results_ltdu/zero.Rda")
} else {
    load("../Results_ltdu/zero.Rda", verbose=TRUE)
}
## it looks like the model converges towards the LS solution, i.e. early
## stopping does not "work"/apply here.

## selected base-learners:
names(coef(zero))

if (!file.exists("../Results_ltdu/stabs_zero_q35.Rda")) {
    set.seed(2202)
    suppressWarnings(stabs_zero_q35 <- stabsel(zero, q = 35, PFER = 2, # eval = FALSE,
                                               mc.cores = mc.cores2))
    save("stabs_zero_q35", file = "../Results_ltdu/stabs_zero_q35.Rda")
} else {
    load("../Results_ltdu/stabs_zero_q35.Rda", verbose=TRUE)
}

dev.off()
