pseudoR2 <- function(zero, hurdle, data) {
    
    if (!requireNamespace("pscl", quietly = TRUE))
        install.packages("pscl", quiet = TRUE)
    
    # Get observered counts
    obs_count <- data$count
    
    # Create indicators of observed zero or non-zero
    zeros <- data$count == 0
    nonzeros <- data$count > 0
    
    # Fit null, two ways available
    # Complete hurdle model using pscl package or separately using glmmADMB
    # Final log-likelihoods for null model differ very slightly
    # Can't load pscl package because `hurdle` function overwrites gamboostLSS count model
    full.0 <- pscl:::hurdle(count ~ 1, data = data, dist = "negbin")
    L.0 <- as.numeric(logLik(full.0))
    
    # Calculate observation level log-likelihoods for final zero models
    llzero.final <- log(1 - mboost:::predict.mboost(zero, type = "response"))
    
    # Calculate observation level log-likelihoods for final conditional count models
    # First, get mu and sigma from final boosted GAMLSS model
    mu.final <- gamboostLSS:::predict.mboostLSS(hurdle, parameter="mu", type="response", newdata=data)
    sigma.final <- gamboostLSS:::predict.mboostLSS(hurdle, parameter="sigma", type="response", newdata=data)
    
    llcount.final <- lgamma(data$count + 1/sigma.final) - lgamma(1/sigma.final) - lgamma(data$count + 1) + # NB ll
        data$count * log(sigma.final*mu.final) - (data$count + 1/sigma.final) * log(1 + sigma.final*mu.final) - # NB ll
        log(1 - (1 + sigma.final*mu.final)^(-1/sigma.final)) # adjustment for truncated distribution
    
    # Calculate total log-likelihoods for final (L.final) model
    p1_zero.final <- mboost:::predict.mboost(zero, type="response", newdata=data)
    L.final <- sum(zeros * llzero.final + nonzeros * (log(p1_zero.final) + llcount.final))
    
    # Calculate pseudo-R2 sensu Nagelkerke (1991; Biometrika 78:691-692)
    n <- nrow(data)
    pseudoR2 <- (1 - exp(-2/n * (L.final - L.0))) / ( 1 - exp(2/n * L.0))
    
    return(pseudoR2)
    
}
