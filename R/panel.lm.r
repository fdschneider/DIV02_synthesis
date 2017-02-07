
panel.lm <- function(x, y,  col = par("col"), bg = NA, pch = par("pch"), 
                     cex = 1,span = 2/3,col.smooth = "red",  iter = 3,  ...)
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok))
    model <- lm(y~x)
  significant <- summary(model)$coefficients[2,4] < 0.01
  abline(model, col = col.smooth, lwd = c(1,2)[significant+1],lty = c(2,1)[significant+1])
  mtext(round(summary(model)$coefficients[2,4],4), line = -1, cex = 0.5, col = col.smooth)
}


panel.region <- function(x, y, col = par("col"), bg = NA, pch = par("pch"), 
                         cex = 1,span = 2/3, col.smooth = "red",  iter = 3,  split = lui$Exploratory, ...) {

  gg <- colorRampPalette(c( par("col"), "grey50"))
  colvec <- rainbow(length(levels(split)))
  points(x, y, pch = pch, 
         col = colvec[as.integer(split)], 
         bg = bg, cex = cex)
  for(r in levels(split)) {
    i = match(r, levels(split))
    x_r <- x[split == r]
    y_r <- y[split == r]
    ok <- is.finite(x_r) & is.finite(y_r)
    if (any(ok))
      model <- lm(y_r~x_r)
    significant <- summary(model)$coefficients[2,4] < 0.01
    abline(model, col = colvec[i], 
           lwd = c(1,2)[significant+1],
           lty = c(2,1)[significant+1])
    mtext(paste0(r, ": p = ",round(summary(model)$coefficients[2,4],3)), 
          line = -i-1, cex = 0.8, col = colvec[i], adj = 0)
  }
}



plot_by <- function(x,y, by, cols = colorRampPalette(c("red3", "blue", "black")), ...) {
  colvec <- cols(length(levels(by)))
  
  plot(x,y, pch = 20,col = colvec[as.integer(by)], ...)
  
  for(r in levels(by)) {
    i = match(r, levels(by))
    x_r <- x[by == r]
    y_r <- y[by == r]
    ok <- is.finite(x_r) & is.finite(y_r)
    if (any(ok)) {
      model <- lm(y_r~x_r)
      significant <- summary(model)$coefficients[2,4] < 0.01
      abline(model, col = colvec[i], 
             lwd = c(1,2)[significant+1],
             lty = c(2,1)[significant+1])
      mtext(paste0(r, ": p = ",round(summary(model)$coefficients[2,4],3), ", R² = ", round(summary(model)[]$r.squared, 2)), 
            line = -i-1, cex = 0.8, col = colvec[i], adj = 0)
    }
    
  }
  
}
