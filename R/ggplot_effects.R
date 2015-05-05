ggplot_effects <- function(mod, data = NULL, vars, varTypes = rep("uni", length(vars)), 
                           parameter = NULL, ylims = NULL) {
  
  require(ggplot2); require(grid); require("RColorBrewer")
  
  theme_set(theme_bw(base_size = 18))
  theme_update(plot.margin = unit(c(0, 0.05, -0.85, -0.85),"line"),
               panel.grid.minor = element_blank(),
               panel.grid.major= element_blank(),
               panel.border=element_rect(fill=NA),
               panel.background= element_blank(),
               axis.ticks=element_blank(),
               axis.title.x=element_blank(),
               axis.title.y=element_blank(),
               axis.text.x=element_blank(),
               axis.text.y=element_blank(),
               legend.position = 'none')
  
  if (is.null(data)) data <- attr(mod, "data")
  
  if (!is.null(parameter)) mod <- mod[[parameter]]
  
  partial_plots <- vector("list", length(vars))
  names(partial_plots) <- vars
  
  for (i in 1:length(vars)) {
    
    varType <- varTypes[i]
    
    if (varType == "uni") {
      
      bls <- extract(mod, what = "bnames", which = paste0("(", vars[i]))
      bls <- bls[!grepl(paste(c(vars[-i], "by =", "%X%"), collapse = "|"), bls)]
      getBLs <- which(names(mod$baselearner) %in% bls)
      
      x <- data.frame(seq(min(data[, vars[i]]), max(data[, vars[i]]), length.out = 50))
      names(x) <- vars[i]

      y <- rowSums(predict(mod, which = getBLs, newdata = x))
      
      tmpDat <- data.frame(x = x[, vars[i]], y = y - mean(y))
      p <- ggplot(tmpDat, aes(x, y)) + geom_line(color = "black", size = 1)  
      
      # If standardizing y-axis for comparability
      if (!is.null(ylims)) p <- p + ylim(ylims)
      
    } else if(varType == "bi") {
      
      splVars <- unlist(strsplit(vars[i], ", "))

      bls <- vector()
      for (var in splVars) {
        tmp <- extract(mod, what = "bnames", which = paste0("(", var))
        bls <- c(bls, tmp)
      }
      if (!identical(splVars, c("xkm", "ykm"))) bls <- bls[!grepl("%X%", bls)]
      
      # Hack for LTDU occupancy to show only partial effect of depth x time interaction
      # given that time effect is also included; I should somehow incorporate this possibility
      # but don't have the time at the moment
      if (all(identical(mod, LTDUocc), identical(splVars, c("time", "depth")))) {
        bls <- bls[3:6]
      }
            
      getBLs <- which(names(mod$baselearner) %in% bls)
      
      grid <- lapply(data[, splVars], function(x) seq(min(x), max(x), length = 50))
      xy <- expand.grid(grid)
      if (identical(splVars, c("xkm", "ykm"))) xy$time <- 0
      z <- rowSums(predict(mod, which = getBLs, newdata = xy))
      tmpDat <- data.frame(xy[, 1:2], z = z - mean(z)); names(tmpDat) <- c("x", "y", "z")
      
      p <- ggplot(tmpDat, aes(x = x, y = y)) + 
        geom_tile(aes(fill=cut(z, breaks = log(c(1/1000,1/5,1/2,2/3,1/1.25,1,1.25,1.5,2,5,1000)), 
                               ordered_result = TRUE))) + 
        scale_fill_manual(values = rev(brewer.pal(n=10, "RdGy")), drop=FALSE)
      
    } else if(varType == "fac") {
      
      bls <- extract(mod, what = "bnames", which = paste0("(", vars[i]))
      bls <- bls[!grepl(paste(c(vars[-i], "%X%"), collapse = "|"), bls)]
      getBLs <- which(names(mod$baselearner) %in% bls)
      
      x <- data.frame(factor(levels(data[, vars[i]])))
      names(x) <- vars[i]
      
      y <- rowSums(predict(mod, which = getBLs, newdata = x))
      
      tmpDat <- data.frame(x = x[, vars[i]], y = y)
      isPos <- tmpDat$y[which(x == "yes" | x == 1)] > 0
      
      if (!is.null(ylims)) {
        tmpDat <- expand.grid(x = -1:1, y = ylims)
      } else {
        tmpDat <- expand.grid(x = -1:1, y = -1:1)
      }
      
      p <- ggplot(tmpDat, aes(x = x, y = y)) + geom_blank() + 
        geom_segment(aes(x = -0.5, xend = 0.5, y = mean(y), yend = mean(y)), size = 1.2)
      
      if(isPos) {
        
        p <- p + geom_segment(aes(x = 0, xend = 0,
                                  y = mean(y) - (max(y) - min(y))/4,
                                  yend = mean(y) + (max(y) - min(y))/4), size = 1.2)
        
      }
      
    } else {
      
      p <- ggplot(data.frame(x=1, y=1), aes(x,y)) + geom_blank()

      if(varType == "") {
      
        p <- p + theme(panel.border=element_rect(fill=NA, colour = "gray80"))
      
        }
      
      if (varType == "blank") {
        
        p <- p + theme(panel.border = element_blank())
          
      }
    
    }
    
    partial_plots[[i]] <- p  
    
  }
  
  return(partial_plots)
  
}
