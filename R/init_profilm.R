init_profilm <- function(left, right, top, bottom, fmin, fmax){
  
  comp <- list()
  
  
  x_center <- abs(right - left)/2 + min(right,left)
  y_center <- abs(top - bottom)/2 + min(top,bottom)
  
  center <- c(x_center, y_center)
  names(center) <- c("x","y")
  
  comp$center <- center
  
  comp$scan_length <- abs(fmax - fmin)
  comp$grid_width <- abs(right - left)
  comp$grid_height <- abs(top- bottom)
  comp$fmin <- fmin
  
  return(comp)
  
}
