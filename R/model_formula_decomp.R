################################################################################
## Model formula with decomposition
################################################################################

center <- TRUE
df <- 1

coei_fm <- count ~

#########################
## Continuous variables:
    bols(int, intercept = FALSE) +

    ## a 'day of season' variable to capture within-season changes in abundance
    bols(time, intercept = FALSE) +
    bbs(time, df = df, center = center) +

    ## average sea surface temperature (Nov-Mar)
    bols(SSTw, intercept = FALSE) +
    bbs(SSTw, df = df, center = center) +

    ## monthly sea surface temperature
    bols(SSTm, intercept = FALSE) +
    bbs(SSTm, df = df, center = center) +

    ## monthly sea surface temperature relative to other locations in the study
    ## area
    bols(SSTrel, intercept = FALSE) +
    bbs(SSTrel, df = df, center = center) +

    ## use of relative water temperatures may change seasonally (e.g., relative
    ## cool early in the season to relative warm areas late in the season)
    # bols(SSTrel, intercept = FALSE) +  ## already defined above
    # bols(time, intercept = FALSE) + ## already defined above
    bols(SSTrel, by = time, intercept = FALSE) +
    bbs(SSTrel, time, df = df, center = center) +

    ## sea bottom temperature (May-Oct) which can affect mussel (prey) settling
    ## and growth
    bols(SBT, intercept = FALSE) +
    bbs(SBT, df = df, center = center) +

    ## winter (Dec-Mar) North Atlantic Oscillation index; only takes three
    ## unique values so 'bbs' likely not practical?
    bols(NAOw, intercept = FALSE) +

    ## water depth
    bols(depth, intercept = FALSE) +
    bbs(depth, df = df, center = center) +

    ## ducks occasionally shift to deeper waters as resources in shallow waters
    ## are depleted
    # bols(depth, intercept = FALSE) + ## already defined above
    # bols(time, intercept = FALSE) + ## already defined above
    bols(depth, by = time, intercept = FALSE) +
    bbs(depth, time, df = df, center = center) +

    ## distance (km) to nearest land
    bols(d2land, intercept = FALSE) +
    bbs(d2land, df = df, center = center) +

    ## geometric mean of chlorophyll a
    bols(chla, intercept = FALSE) +
    bbs(chla, df = df, center = center) +

    ## geometric mean of chromophoric dissoloved organic material
    bols(cdom, intercept = FALSE) +
    bbs(cdom, df = df, center = center) +

    ## CDOM can skew other derived satellite products and cause overestimation
    ## of chla;
    # bols(cdom, intercept = FALSE) + ## already defined above
    bbs(cdom, chla, df = df, center = center) +

    ## COMMENT: I'm not sure if relationship warrants an interaction or simple
    ## additive effects

    ## sediment grain size
    bols(meanphi, intercept = FALSE) +
    bbs(meanphi, df = df, center = center) +

    ## ratio of seafloor surface area to planimetric area
    bols(SAR, intercept = FALSE) +
    bbs(SAR, df = df, center = center) +

    ## average epibenthic tidal velocity
    bols(tidebmean, intercept = FALSE) +
    bbs(tidebmean, df = df, center = center) +

    ## standard deviation of epibenthic tidal velocity
    bols(tidesd, intercept = FALSE) +
    bbs(tidesd, df = df, center = center) +

    ## potential for thermal stratification in the summer; highly correlated
    ## with tidebmean (r = -0.86)
    bols(strat, intercept = FALSE) +
    bbs(strat, df = df, center = center) +

#############################
## Classification variables:

    ## dichotomous; is a given segment within 1 km of the Cape Cod - Nantucket
    ## ferry route
    bols(ferry, intercept = FALSE) +

    ## (winter; 2 dummy variables): yearly differences in abundance
    bols(y2004, intercept = FALSE) +
    bols(y2005, intercept = FALSE) +

##########################
## Spatial heterogeneity:

    ## modeling spatial autocorrelation and unmeasured environmental covariates.
    bols(xkm, intercept = FALSE) +
    bols(ykm, intercept = FALSE) +
    bols(xkm, by = ykm, intercept = FALSE) +
    bspatial(xkm, ykm, df = df, center = center) +

# QUESTION: how do we evaluate whether (and if so, prevent) this spatial smooth
# from forming combinations of one or more relevant covariates (above) and thus
# masking their effect? Does it make sense to create some measure of space that
# is orthogonal to our included covariates, as suggested by Kuhn and Dormann
# (2012; J. Biogeogr. 39:995-998)?

#################################
## Spatiotemporal heterogeneity:

    bols(xkm, by = time, intercept = FALSE) +
    bols(ykm, by = time, intercept = FALSE) +
    bols(xkm, by = ykm, intercept = FALSE) %X% bols(time, intercept = FALSE) + ### ???
    bspatial(xkm, ykm, by = time, df = df, center = center)

# (18) COMMENT/QUESTION: The goal is to capture changes in duck spatial
# distribution over the course of the winter due, presumably, to depletion of
# prey resources. I'm not sure this gets at what we want, though, since it's
# only an interaction with spatial pattern attributable largely to unmeasured
# variables. Perhaps interactions between time and specific environmental
# variables that may change in importance over the course of the winter?

## REMOVE THIS, SEE EMAIL A.S., 02.09.2014
##     ## (winter, two dummy variables by = winter dummy variable): similar concerns to (18)
##     bols(xkm, by = y2004, intercept = FALSE) +
##     bols(ykm, by = y2004, intercept = FALSE) +
##     bols(xkm, by = ykm, intercept = FALSE) %X% bols(y2004, intercept = FALSE) + ### ???
##     bspatial(xkm, ykm, by = y2004, df = df, center = center) +
##
##     bols(xkm, by = y2005, intercept = FALSE) +
##     bols(ykm, by = y2005, intercept = FALSE) +
##     bols(xkm, by = ykm, intercept = FALSE) %X% bols(y2005, intercept = FALSE) + ### ???
##     bspatial(xkm, ykm, by = y2005, df = df, center = center)
##
## ##############################################
## ## Nonstationary (spatially varying) effects:
## # COMMENT: I'm not sure if we need concern ourselves with this given the
## # relatively small scale of our study area. Thoughts? The only environmental
## # covariates that I've reasoned may have a different effect in one part of the
## # study area than another is depth (due to, e.g., differences in bottom sediment
## # type, grain size, etc.)


## add observation window as covariate
coei_fm2 <- as.formula(paste(paste(deparse(coei_fm), collapse = ""),
                             "+ bols(obs_window, intercept = FALSE) + bbs(obs_window, df = df, center = center)"))


## use same model for scoters
scot_fm <- coei_fm
scot_fm2 <- coei_fm2

## use same model for longtailed ducks
ltdu_fm <- coei_fm
ltdu_fm2 <- coei_fm2
