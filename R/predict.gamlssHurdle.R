## Function to generate the overall mean regression relationship for
## the hurdle model fitted using mboost/gamboostLSS
instant_pkgs(c("gamboostLSS", "gamlss.tr"))

## generates: dNBItr pNBItr qNBItr rNBItr NBItr
capture.output(gen.trun(0, family = "NBI"), file="NUL")

## See equation 6 (p. 6) in: Zeileis et al. 2008. J Stat Soft 27(8):1-25
predict.gamlssHurdle <- function(zero, hurdle, newdata,
                                 variance = FALSE, residuals = FALSE,
                                 append = FALSE) {
  # variance: requests calculation of variance for fitted value
  # residuals: calculates scaled (E(Y)/sd(Y)) Pearson-type residual for fitted value
  # append: appends these calculations to the input data set

  if (residuals) variance <- TRUE

  add_pred <- gamboostLSS:::predict.mboostLSS(hurdle, newdata = newdata)
  log_p1_zero <- log(mboost:::predict.mboost(zero, type = "response", newdata = newdata))
  log_p_gt_0_count <- pNBI(0, mu = exp(add_pred$mu), sigma = exp(add_pred$sigma),
                           lower.tail = FALSE, log.p = TRUE)

  # Partition for ease of interpretation
  fitted <- exp(add_pred$mu + log_p1_zero - log_p_gt_0_count)
  mu <- exp(add_pred$mu)
  sigma <- exp(add_pred$sigma)

  # If a "count" variable is present in data, assume comparison is wanted
  if ("count" %in% colnames(newdata)) {
    tmpDat <- data.frame(obs_count = newdata$count,
                         add_pred = add_pred$mu + log_p1_zero - log_p_gt_0_count)
  } else {
    if (residuals) stop("Can't calculate residuals without an observed 'count' variable")
    tmpDat <- data.frame(add_pred = add_pred$mu + log_p1_zero - log_p_gt_0_count)
  }

  if (variance) {
    tmpDat$variance <- fitted * (1 + mu + sigma * mu - fitted)
  }

  if (residuals) tmpDat$scaled_resid <- with(tmpDat, (obs_count - exp(add_pred)) / sqrt(variance))

  if (append) tmpDat <- data.frame(newdata, tmpDat[, -1])

  return(tmpDat)

}