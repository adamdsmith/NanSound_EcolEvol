pseudoR2 <- function(zero, hurdle, data, engine = c("both", "pscl", "glmmADMB")) {

  engine <- match.arg(engine)

  # Get observered counts
  obs_count <- data$count

  # Create indicators of observed zero or non-zero
  zeros <- data$count == 0
  nonzeros <- data$count > 0

  # Fit null, two ways available
  # Complete hurdle model using pscl package or separately using glmmADMB
  # Final log-likelihoods for null model differ very slightly
  # Can't load pscl package because `hurdle` function overwrites gamboostLSS count model
  if (engine %in% c("pscl", "both") ) {

    full.0 <- pscl:::hurdle(count ~ 1, data = data, dist = "negbin")
    L.0 <- as.numeric(logLik(full.0))

    if (engine == "both") L.0.pscl <- L.0

  }

  if (engine %in% c("glmmADMB", "both")) {

    # Way # 2; separately and combine with glmmADMB
    library("glmmADMB")

    # Dichotomize count for logistic model
    data_zero <- data
    data_zero$count <- factor(data_zero$count > 0, labels = c("0", ">0"))

    # Fit the null zero and conditional count models
    zero.0 <- glm(count ~ 1, data = data_zero, family = binomial)
    count.0 <- glmmadmb(count ~ 1, data = subset(data, count > 0), family = "truncnbinom")

    # Get mu and sigma from null models
    mu.0 <- predict(count.0, newdata = data, type="response")
    sigma.0 <- 1/count.0$alpha

    # Calculate fitted values for complete hurdle model
    log_p1_zero <- log(predict(zero.0, type = "response", newdata = data))
    log_pgt0_count <- pNBI(0, mu = mu.0, sigma = sigma.0,
                           lower.tail = FALSE, log.p = TRUE)

    # Calculate observation level log-likelihoods for null model
    llzero.0 <- log(1 - predict(zero.0, type="response"))

    # Calculate observation level log-likelihoods for null conditional count models
    llcount.0 <- lgamma(data$count + 1/sigma.0) - lgamma(1/sigma.0) - lgamma(data$count + 1) + # NB ll
      data$count * log(sigma.0*mu.0) - (data$count + 1/sigma.0) * log(1 + sigma.0*mu.0) - # NB ll
      log(1 - (1 + sigma.0*mu.0)^(-1/sigma.0)) # adjustment for truncated distribution

    # Calculate total log-likelihoods for null (L.0) model
    p1_zero.0 <- predict(zero.0, type="response")
    L.0 <- sum(zeros * llzero.0 + nonzeros * (log(p1_zero.0) + llcount.0))

  }

  # Calculate observation level log-likelihoods for final zero models
  llzero.final <- log(1 - predict(zero, type = "response"))

  # Calculate observation level log-likelihoods for final conditional count models
  # First, get mu and sigma from final boosted GAMLSS model
  mu.final <- predict(hurdle, parameter="mu", type="response", newdata=data)
  sigma.final <- predict(hurdle, parameter="sigma", type="response", newdata=data)

  llcount.final <- lgamma(data$count + 1/sigma.final) - lgamma(1/sigma.final) - lgamma(data$count + 1) + # NB ll
    data$count * log(sigma.final*mu.final) - (data$count + 1/sigma.final) * log(1 + sigma.final*mu.final) - # NB ll
    log(1 - (1 + sigma.final*mu.final)^(-1/sigma.final)) # adjustment for truncated distribution

  # Calculate total log-likelihoods for final (L.final) model
  p1_zero.final <- predict(zero, type="response", newdata=data)
  L.final <- sum(zeros * llzero.final + nonzeros * (log(p1_zero.final) + llcount.final))

  # Calculate pseudo-R2 sensu Nagelkerke (1991; Biometrika 78:691-692)
  n <- nrow(data)
  pseudoR2 <- (1 - exp(-2/n * (L.final - L.0))) / ( 1 - exp(2/n * L.0))

  if (engine == "both") {
    pseudoR2.pscl <- (1 - exp(-2/n * (L.final - L.0.pscl))) / ( 1 - exp(2/n * L.0.pscl))
    tmpDat <- data.frame(engine = c("pscl", "glmmADMB"),
                         pseudoR2 = c(pseudoR2.pscl, pseudoR2))
    pseudoR2 <- tmpDat
  }

  return(pseudoR2)

}
