## Function to generate the overall mean regression relationship for 
## the hurdle model fitted using mboost/gamboostLSS

## generates: dNBItr pNBItr qNBItr rNBItr NBItr
capture.output(gen.trun(0, family = "NBI"), file="NUL")

## See equation 6 (p. 6) in: Zeileis et al. 2008. J Stat Soft 27(8):1-25
predict.gamlssHurdle <- function(zero, hurdle, newdata) {
  
  add_pred <- predict(hurdle, newdata = newdata)
  log_p1_zero <- log(predict(zero, type = "response", newdata = newdata))
  log_p_gt_0_count <- pNBI(0, mu = exp(add_pred$mu), sigma = exp(add_pred$sigma),
                           lower.tail = FALSE, log.p = TRUE)

  # If a "count" variable is present in data, assume comparison is wanted
  if ("count" %in% colnames(newdata)) {
    return(data.frame(obs_count = newdata$count,
                      add_pred = add_pred$mu + log_p1_zero - log_p_gt_0_count))
  } else {
    return(data.frame(add_pred = add_pred$mu + log_p1_zero - log_p_gt_0_count))
  }
}