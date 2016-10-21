#' Plot distribution of trait space as a 3D plot with convex hull
#'
#' @param x 
#' @param y 
#' @param z 
#' @param col 
#'
#' @return
#' @export
#'

crystalplot <- function(x,y,z, col = NULL) {
  
  plot3d(x,y,z, col = col )
  
  ps <- data.frame(x,y,z)
  ts.surf <- t(convhulln(ps))
  rgl.triangles(ps[ts.surf,1],ps[ts.surf,2],ps[ts.surf,3],col="blue",alpha=.2,
                color = c("blue"), shininess = 200, texenvmap = TRUE)
  
}

