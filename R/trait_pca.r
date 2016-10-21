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
                      landuse = lui,
                      show = c("G_std", "M_std", "F_std", "LUI")
) {
  
  rownames(x) <- paste0(x[,y],"_",x[,i])
  rownames(landuse) <- paste0(landuse$Year,"_", landuse$EP.Plotid)
  lui_dd <- landuse[match(rownames(x),rownames(landuse) ), c("G_std", "M_std", "F_std", "LUI")]
  
  dd0 <- na.omit(cbind(x[,traits],lui_dd))
  
  lui_dd <- dd0[,c("G_std", "M_std", "F_std", "LUI")]
  
  dd <- dd0[,-c(-3:0+length(dd0))]
  if(log) dd <- log(dd)
  
  pca_plant <- rda(dd, scale = TRUE)
  biplot(pca_plant, scaling = -1) -> fig
  points(fig, "sites", pch = 20, col = "#00000020")
  text(fig, "species", col="red", cex=0.6)
  
  covars <- envfit(pca_plant, lui_dd[,show], permu=999)
  plot(covars, cex = 0.6)
  
  return(pca_plant)
}

