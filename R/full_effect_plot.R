full_effect_plot <- function(mod, data = NULL, var, varType = c("uni", "bi", "st", "fac"), 
                             parameter = NULL, ylims = NULL,
                             xlab = "", ylab = "") {
  
  toLoad <- c("ggplot2", "grid", "RColorBrewer", "lubridate", "gamboostLSS")
  instant_pkgs(toLoad); rm(toLoad)
  
  varType <- match.arg(varType)
  
  if (is.null(data)) data <- attr(mod, "data")
    
  if (!is.null(parameter)) mod <- mod[[parameter]]
  
  all_vars <- c("NAOw", "time", "depth", "time, depth", "meanphi", "SAR", 
                "tidebmean", "tidesd", "strat", "d2land", "chla", "cdom", 
                "chla, cdom", "SBT", "SSTm", "SSTw", "SSTrel", "time, SSTrel",
                "ferry", "y2004", "y2005", "xkm, ykm", "obs_window")
  
  if (varType == "uni") {
    
    bls <- extract(mod, what = "bnames", which = paste0("(", var))
    bls <- bls[!grepl(paste(c(all_vars[all_vars != var], "by =", "%X%"), collapse = "|"), bls)]
    getBLs <- which(names(mod$baselearner) %in% bls)
    
    x <- data.frame(seq(min(data[, var]), max(data[, var]), length.out = 50))
    names(x) <- var
    
    y <- rowSums(predict(mod, which = getBLs, newdata = x))
    
    # Create data frame and return covariate to original scale
    tmpDat <- data.frame(x = x[, var] * covar_sds[var] + covar_avgs[var],
                         y = y - mean(y))
    
    # Data (on original scale) for rug plot
    rugDat <- data.frame(x = data[, var] * covar_sds[var] + covar_avgs[var], 
                         y = sample(tmpDat$y, nrow(data), replace=T))
    
    p <- ggplot(tmpDat, aes(x, y)) + geom_line(color = "black", size = 1) +
      geom_rug(data = rugDat, sides = "b") + xlab(xlab)
    
    # Modify Y-axis label for count model
    if (!is.null(parameter)) {
      p <- p + ylab(paste0("Partial contribution\nto the additive predictor (", parameter, ")"))
    } else  {
      p <- p + ylab("Partial contribution\nto the additive predictor")
    }
    
    # If standardizing y-axis for comparability
    if (!is.null(ylims)) p <- p + ylim(ylims)
    
    } else if(varType == "bi") {
      
      splVars <- unlist(strsplit(var, ", "))
      
      bls <- vector()
      for (v in splVars) {
        tmp <- extract(mod, what = "bnames", which = paste0("(", v))
        bls <- c(bls, tmp)
      }
      if (!identical(splVars, c("xkm", "ykm"))) bls <- bls[!grepl("%X%", bls)]
      
      # Hack for LTDU occupancy to show only partial effect of depth x time interaction
      # given that time effect is also included; I should somehow incorporate this possibility
      # but don't have the time at the moment
      if (is.null(parameter)) {
        ltdu_zero <- ifelse(as.character(mod$call)[3] == "ltdu_zero", TRUE, FALSE)
      } else {
        ltdu_zero <- FALSE
      }
      if (all(ltdu_zero, identical(splVars, c("time", "depth")))) {
        bls <- bls[3:6]
      }
      
      getBLs <- which(names(mod$baselearner) %in% bls)
      
      grid <- lapply(data[, splVars], function(x) seq(min(x), max(x), length = 50))
      xy <- expand.grid(grid)
      if (identical(splVars, c("xkm", "ykm"))) xy$time <- 0
      z <- rowSums(predict(mod, which = getBLs, newdata = xy))
      
      # Create data frame and return to original scale
      tmpDat <- data.frame(x = xy[, splVars[1]] * covar_sds[splVars[1]] + covar_avgs[splVars[1]],
                           y = xy[, splVars[2]] * covar_sds[splVars[2]] + covar_avgs[splVars[2]],
                           z = z - mean(z))
      
      if (!identical(splVars, c("xkm", "ykm"))) {
        rugDat <- data.frame(x = data[, splVars[1]] * covar_sds[splVars[1]] + covar_avgs[splVars[1]], 
                             y = data[, splVars[2]] * covar_sds[splVars[2]] + covar_avgs[splVars[2]])
        plotRugs <- TRUE
      } else { plotRugs <- FALSE }
      
      p <- ggplot(tmpDat, aes(x = x, y = y)) + 
        geom_tile(aes(fill=cut(z, breaks = log(c(1/1000,1/5,1/2,2/3,1/1.25,1,1.25,1.5,2,5,1000)),
                               labels = c("< -1.61", "(-1.61,-0.69]", "(-0.69,-0.41]", "(-0.41,-0.22]", 
                                          "(-0.22,0]", "(0,0.22]", "(0.22,0.41]", "(0.41,0.69]", 
                                          "(0.69,1.61]", "> 1.61"),
                               ordered_result = TRUE))) +
        xlab(xlab) + ylab(ylab) +
        scale_fill_manual("Partial contribution,\nadditive predictor",
                          values = rev(brewer.pal(n=10, "RdGy")), drop=FALSE) +
        theme(legend.position = "right")
      
      if (plotRugs) {
        p <- p + geom_rug(data = rugDat)
      } else { p <- p + coord_equal()}
      
    } else if(varType == "st") {
      
      splVars <- unlist(strsplit(var, ", "))
      
      bls <- vector()
      for (v in splVars) {
        tmp <- extract(mod, what = "bnames", which = paste0("(", v))
        bls <- c(bls, tmp)
      }
      
      getBLs <- which(names(mod$baselearner) %in% bls)
      
      grid <- lapply(data[, splVars], function(x) seq(min(x), max(x), length = 50))
      
      # Add dates to display
      dates <-mdy(c("11152003", "01012004", "02142004", "04012004")) # year irrelevant 
      grid[["time"]] <- (as.integer(dates - mdy("12-31-2003")) - covar_avgs["time"]) / covar_sds["time"]
      date_lu <- data.frame(date = format(dates, format = "%d %B"), time = grid[["time"]])
      xytime <- expand.grid(grid)
      z <- rowSums(predict(mod, which = getBLs, newdata = xytime))
      
      # Create data frame and return to original scale
      tmpDat <- data.frame(x = xytime[, splVars[1]] * covar_sds[splVars[1]] + covar_avgs[splVars[1]],
                           y = xytime[, splVars[2]] * covar_sds[splVars[2]] + covar_avgs[splVars[2]],
                           time = xytime[, "time"], 
                           z = z - mean(z))
      tmpDat <- left_join(tmpDat, date_lu)
      tmpDat$date <- factor(tmpDat$date, levels = c("15 November", "01 January",
                                                    "14 February", "01 April"))
 
      p <- ggplot(tmpDat, aes(x = x, y = y)) + 
        geom_tile(aes(fill=cut(z, breaks = log(c(1/1000,1/5,1/2,2/3,1/1.25,1,1.25,1.5,2,5,1000)),
                               labels = c("< -1.61", "(-1.61,-0.69]", "(-0.69,-0.41]", "(-0.41,-0.22]", 
                                          "(-0.22,0]", "(0,0.22]", "(0.22,0.41]", "(0.41,0.69]", 
                                          "(0.69,1.61]", "> 1.61"),
                               ordered_result = TRUE))) + coord_equal() +
        facet_wrap(~ date, nrow=2) +
        xlab(xlab) + ylab(ylab) +
        scale_fill_manual("Partial contribution,\nadditive predictor",
                          values = rev(brewer.pal(n=10, "RdGy")), drop=FALSE) +
        theme(legend.position = "right")  
      
    } else { # factor variables
      
      bls <- extract(mod, what = "bnames", which = paste0("(", var))
      bls <- bls[!grepl(paste(c(all_vars[all_vars != var], "%X%"), collapse = "|"), bls)]
      getBLs <- which(names(mod$baselearner) %in% bls)
      
      x <- data.frame(factor(levels(data[, var])))
      names(x) <- var
      
      y <- rowSums(predict(mod, which = getBLs, newdata = x))
      
      tmpDat <- data.frame(x = x[, var], y = y)
      
      levels(tmpDat$x) <- c("no", "yes")
            
      p <- ggplot(tmpDat, aes(x = x, y = y)) + 
        geom_bar(colour = "black", fill = "white", stat="identity") + 
        xlab(xlab) + ylab("Partial contribution \nto the additive predictor")
      
    }

  return(p)
  
}
