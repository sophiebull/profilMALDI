##' Match resolution of Profilometry data to MALDI MSI data
##' 
##' A function to match the resolution of the raw profilometry data obtained from the ProFilm 3D Optical Profilometer to the raw mass spectral data from MALDI MSI data. 
##' 
##' @param data matrix; The matrix containing the topological data
##' @param xres numeric; A positive integer value representing the resolution across the x-axis.
##' @param yres numeric; A positive integer value representing the resolution across the y-axis.
##' 
##' @return Returns a list of 1. An array of values representing surface topography (heights) and 2. The pixel size (um). 
##'
##' @author Sophie Castel <sophie.castel@@ontariotechu.net>
##' @references https://github.com/castels/profilMALDI
##' @keywords data txt convert read

# -----------------------------------------------------------------------
# Last Updated: April 6, 2021
# Author: Sophie Castel
# Title: profilMALDI: Match resolution of Profilometry data to MALDI MSI data

# -----------------------------------------------------------------------

change_res <- function(data, xres = ncol(data$surface), yres = nrow(data$surface)){
  
  ##################
  # LOGICAL CHECKS
  ##################
  
    if(!(is.numeric(xres) & xres > 0 & xres %% 1 == 0)){
      stop(paste0("Specified x-resolution must be a positive integer."))
    } 
    
    if(!(is.numeric(yres) & yres > 0 & yres %% 1 == 0)){
      stop(paste0("Specified y-resolution must be a positive integer."))
    } 
  
  pixel_size <- data$pixel_size
  field_width <- data$field_width
  field_height <- data$field_height
  
  # Initialize empty matrix
  
  #surface_matrix <- matrix(rep(0, xres*yres), nrow = yres, ncol = xres)
  
  # Determine if increasing or decreasing resolution along each axis
  
  xres_orig <- ncol(data$surface)
  yres_orig <- nrow(data$surface)
  
  # Adjustment factors
  
  x_adjust <- xres/xres_orig
  y_adjust <- yres/yres_orig
  
  # Increasing resolution
  
  if(x_adjust > 1){ 
    message(paste0("Increasing x resolution from ", xres_orig, " to ",xres,". (Field width = ", field_width," um)"))
  }
  
  if(y_adjust > 1){ 
    message(paste0("Increasing y resolution from ", yres_orig, " to ",yres,". (Field height = ", field_height," um)"))
  }
  
  # Decreasing resolution
  
  if(x_adjust < 1){ 
    message(paste0("Decreasing x resolution from ", xres_orig, " to ",xres,". (Field width = ", field_width," um)"))
  }
  
  if(y_adjust < 1){ 
    message(paste0("Decreasing y resolution from ", yres_orig, " to ",yres,". (Field height = ", field_height," um)"))
  }
  
  # No change
  
  if(x_adjust == 1){
    message(paste0("Maintaining x resolution of ", xres,". (Field width = ", field_width," um)"))
  }
  
  if(y_adjust == 1){
    message(paste0("Maintaining z resolution of ", yres,". (Field height = ", field_height," um)"))
  }
  
  
  #################
  # RESIZE MATRIX
  #################
  
  # Author credit: Vyga (https://stackoverflow.com/questions/11123152/function-for-resizing-matrices-in-r)
  
  rescale <- function(x, newrange=range(x)){
    xrange <- range(x)
    mfac <- (newrange[2]-newrange[1])/(xrange[2]-xrange[1])
    newrange[1]+(x-xrange[1])*mfac
  }
  
  ResizeMat <- function(mat, ndim=dim(mat)){
    if(!require(fields)) stop("`fields` required.")
    
    # input object
    odim <- dim(mat)
    obj <- list(x= 1:odim[1], y=1:odim[2], z= mat)
    
    # output object
    ans <- matrix(NA, nrow=ndim[1], ncol=ndim[2])
    ndim <- dim(ans)
    
    # rescaling
    ncord <- as.matrix(expand.grid(seq_len(ndim[1]), seq_len(ndim[2])))
    loc <- ncord
    loc[,1] = rescale(ncord[,1], c(1,odim[1]))
    loc[,2] = rescale(ncord[,2], c(1,odim[2]))
    
    # interpolation
    ans[ncord] <- interp.surface(obj, loc)
    
    ans
  }
  
  
  resized <- ResizeMat(mat = data$surface, ndim = c(yres, xres) )

  
  # Output side-by-side plot of changes

  #col_orig <- colour_values(data$surface, palette = "viridis")
  #col_orig <- matrix(col_orig, ncol = xres_orig, nrow = yres_orig)
  
  #col_resized <- colour_values(resized, palette = "viridis")
  #col_resized <- matrix(col_resized, ncol = xres, nrow = yres)
  
  data_orig <- melt(data$surface)
  data_resized <- melt(resized)
  
  names(data_orig) <- c("row_id", "col_id", "value")
  names(data_resized) <- c("row_id", "col_id", "value")
  
  p_orig <- ggplot(data_orig, aes(x = col_id, y = row_id, fill = value, label = value)) + 
    geom_tile() + 
    #geom_text(col = "black") + 
    scale_fill_gradientn(colors = c("white","red"), values = c(0,1)) + 
    scale_y_continuous(trans = "reverse") + 
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank()) + 
    ggtitle(paste0("original (", xres_orig, " x ", yres_orig, ")"))
  
  p_resized <- ggplot(data_resized, aes(x = col_id, y = row_id, fill = value, label = value)) + 
    geom_tile() + 
    #geom_text(col = "black") + 
    scale_fill_gradientn(colors = c("white","red"), values = c(0,1)) + 
    scale_y_continuous(trans = "reverse")+ 
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank()) + 
    ggtitle(paste0("resized (", xres, " x ", yres, ")"))
  
  grid.arrange(p_orig, p_resized, ncol = 2)
  
  r_data <- list()
  r_data$resized <- resized
  r_data$pixel_size <- "figure_out"
  
  
  return(resized)


  
}
