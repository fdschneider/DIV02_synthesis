#' calculate and plot ordination and map land use 
#'
#' @param x 
#' @param i 
#' @param y 
#' @param traits 
#' @param log 
#' @param landuse 
#'
#' @return
#' @import vegan
#' @export
#'
#' @examples
#' 
#' trait_pca(cwm_plants, log = TRUE)
#' 
trait_pca <- function(x, 
                      i = 1, #column/position of plotID
                      y = 2, #column/position of year
                      traits = 3:length(x), 
                      log = FALSE,
                      cex = 0.6,
                      landuse = lui,
                      envir = topo_soil,
                      choices = c(1,2),
                      show = c("G_std", "M_std", "F_std", "LUI"), 
                      col = "#00000050", 
                      ...
) {
  
  rownames(x) <- paste0(x[,y],"_",x[,i])
  rownames(landuse) <- paste0(landuse$Year,"_", landuse$EP.Plotid)

  env_dd <- envir[match(rep(envir$EP_PlotID, times = length(levels(landuse$Year))), envir$EP_PlotID),]
  env_dd$Year <- rep(levels(landuse$Year), each = length(unique(envir$EP_PlotID)))
  rownames(env_dd) <- paste0(env_dd$Year,"_", env_dd$EP_PlotID)
  
  lui_dd <- landuse[match(rownames(x),rownames(landuse) ), show]
  env_dd <- env_dd[match(rownames(x),rownames(env_dd) ), c(3:6)]
  
  dd0 <- na.omit(cbind(x[,traits],lui_dd, env_dd))
  
  lui_dd <- dd0[,c("G_std", "M_std", "F_std", "LUI")]
  env_dd <- dd0[,c("pH", "elevation", "slope", "TWI")]
  
  dd <- dd0[,-c(-7:0+length(dd0))]
  if(log) dd <- log(dd)
  
  pca_dd <- rda(dd, scale = TRUE)
  eig_rel <- round(eigenvals(pca_dd)/sum(eigenvals(pca_dd))*100,1)
  axislabels <- paste0("PC", 1:length(eig_rel), " (", eig_rel, "%)")
  
  biplot(pca_dd, scaling = -1, cex = cex,  
         xlab = axislabels[choices[1]],
         ylab = axislabels[choices[2]],
         col = c(col, "red"),
         ...
           ) -> fig
  points(fig, "sites", pch = 20, col = col, choices = choices)
  text(fig, "species", col="red", cex= cex, choices = choices)
  
  covars <- envfit(pca_dd, lui_dd[,show], permu=999)
  plot(covars, cex = cex, col = "darkblue", choices = choices)
  
  covars2 <- envfit(pca_dd, env_dd[,], permu=999)
  plot(covars2, cex = cex, col = "darkcyan", choices = choices)
  
  return(pca_dd)
}

