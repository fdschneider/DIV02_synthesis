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
                      show = c("G_std", "M_std", "F_std", "LUI"), 
                      col = "#00000020"
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
  biplot(pca_dd, scaling = -1, cex = cex, 
         xlab = paste0("PC1 (", eig_rel[1], "%)"),
         ylab = paste0("PC2 (", eig_rel[2], "%)"),
         col = c(col, "red")
           ) -> fig
  points(fig, "sites", pch = 20, col = col)
  text(fig, "species", col="red", cex= cex)
  
  covars <- envfit(pca_dd, lui_dd[,show], permu=999)
  plot(covars, cex = cex, col = "darkblue")
  
  covars2 <- envfit(pca_dd, env_dd[,], permu=999)
  plot(covars2, cex = cex, col = "darkcyan")
  
  return(pca_dd)
}

