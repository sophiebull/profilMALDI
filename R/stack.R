##' Stack Time-Ordered Surface Profile Scans
##' 
##' A function to match the resolution of the raw profilometry data obtained from the ProFilm 3D Optical Profilometer to the raw mass spectral data from MALDI MSI data. 
##' 
##' @param data_list list; A list of datasets representing each surface profile scan
##' @param xres numeric; A positive integer value representing the resolution across the x-axis.
##' @param yres numeric; A positive integer value representing the resolution across the y-axis.
##' @param display logical; TRUE = Display side-by-side tile plots of original and resized data.
##' 
##' @return Returns the resized matrix of data and displays side-by-side plots visualizing the adjusted resolution.
##'
##' @author Sophie Castel <sophie.castel@@ontariotechu.net>
##' @references https://github.com/castels/profilMALDI
##' @keywords data txt convert read

# -----------------------------------------------------------------------
# Last Updated: April 12, 2021
# Author: Sophie Castel
# Title: profilMALDI: Stack Time-Ordered Surface Profile Scans

# -----------------------------------------------------------------------

stack <- function(dir, standard_pos = "lowerRight", standard_thickness, tolerance = 3, scale.row = 200, scale.col = 200){
  
  standard_positions <- c("upperLeft", "upperRight", "lowerLeft", "lowerRight")
  
  ##################
  # LOGICAL CHECKS
  ##################
  
  if(!standard_pos %in% standard_positions){
    stop(paste0("General position of the external standard must be one of: ", paste(standard_positions, collapse = ", "),"."))
  }

  # Removing terminating forward slash (if any)
  
  dir <- sub("/$", "", dir)
  
  
  ###################################
  # Function to load file from a path
  ###################################
  
  load_obj <- function(f)
  {
    env <- new.env()
    nm <- load(f, env)[1]
    env[[nm]]
  }
  
  ############################
  # Compress files into a list
  ############################
  
  files <- list.files(dir, pattern=".txt", full.names = TRUE, recursive = TRUE)
  
  message("General warning: Importing files in order of appearance in the directory.  Please use a naming convention that ensures the file order represents the time order.")
  
  data_list <- list()
  
  for(i in 1:length(files)){
    data_list[[i]] <- read_profilm(file = files[i])
  }
  
  ###############################
  # Checking conforming datasets
  ###############################
  
  dims <- list()
  pixel_sizes <- list()
  
  dims <- lapply(data_list, FUN = function(x) dim(x$surface))
  
  logic_dims <- 
  if(length(unique(dims)) != 1){
    stop("Data set ______ has non-conforming dimension. Adjusting size:")
  }
  
  pixel_sizes <- unique(lapply(data_list, FUN = function(x) x$pixel_size))
  
  if(length(unique(pixel_sizes)) != 1){
    stop("Data set ____ has non-conforming pixel size.  Please consider converting using change_res().")
  }
  
  ###########################
  # Add Padding for Rotation
  ###########################
  
  # Just increase padding by a lot (nothing specific)
  
  padded_list <- lapply(data_list, FUN = function(x){
    scale(x = x$surface, scale.row = scale.row, scale.col = scale.col)
  })
  
  # determine dimensions
  
  dim_padded_list <- lapply(padded_list, FUN = function(x){
    dims <- dim(x)
    names(dims) <- c("rows","columns")
    return(dims)
  })
  
  
  ##################################
  # Align external standard (rotate)
  ##################################
  
  # perform rotation correction, save in a list
  
  rotated_list <- lapply(padded_list, FUN = function(x){
    rotate(x = x, standard_thickness = standard_thickness, tolerance = tolerance, standard_pos = standard_pos)
    })
  
  # determine dimensions
  
  dim_rotated_list <- lapply(rotated_list, FUN = function(x){
    dims <- dim(x)
    names(dims) <- c("rows","columns")
    return(dims)
  })
  
  # determine scale factors
  adj_list <- mapply("/", dim_padded_list, dim_rotated_list, SIMPLIFY = FALSE)
  
  adj_list <- lapply(adj_list, FUN = function(x){
    adjs <- round(x*100)
    names(adjs) <- c("r.factor", "c.factor")
    return(adjs)
  })
  
  #########################################
  # Remove Padding to match original scale
  #########################################
  
  vec.scale.row <- lapply(adj_list, FUN = function(x){x["r.factor"]}) # vector of row scale adjustments
  vec.scale.col <- lapply(adj_list, FUN = function(x){x["c.factor"]}) # vector of col scale adjustments 
  
  reverted_list <- list()
  
  for(i in 1:length(files)){
    reverted_list[[i]] <- scale(x = rotated_list[[i]], scale.row = vec.scale.row[[i]], scale.col = vec.scale.col[[i]])  
  }


  # cut out anything above the standard reference (beyond the zero boundary)
  # start searching from bottom, up and left
  
  # Perform rotation to match
  
  # Trim ends
  
  # lateral translations
  
}