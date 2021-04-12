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

stack <- function(dir, standard_pos = "lowerRight", standard_thickness, tolerance = 3){
  
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
  # Locate external standard
  ###########################
  
  # initialize list of empty matrices
  
  standard_list <- vector(mode = "list", length = length(files))
  standard_list <- lapply(standard_list, FUN = function(x) x <- matrix(rep(0, nrow*ncol), nrow = nrow, ncol = ncol))
  
  # populate with standard heights
  
  upper <- standard_thickness + tolerance
  lower <- standard_thickness - tolerance
  
  
  for(i in 1:length(files)){

    logic <- (data_list[[i]]$surface > lower) & (data_list[[i]]$surface < upper)
    standard_list[[i]][logic] <- data_list[[i]]$surface[logic]
    
  }
  
  # apply window 
  
  nrow <- unlist(unique(dims))[1]
  ncol <- unlist(unique(dims))[2]
  
  ref_nrow <- floor(nrow/2)
  ref_ncol <- floor(ncol/2)
  
  # Window where external standard is approximately located 
  
  if(standard_pos == "upperLeft"){
    standard_window <- list(rows = 1:ref_nrow, cols = 1:ref_ncol)
  }
  else if(standard_pos == "upperRight"){
    standard_window <- list(rows = 1:ref_nrow, cols = ref_ncol:ncol)
  }
  else if(standard_pos == "lowerLeft"){
    standard_window <- list(rows = ref_nrow:nrow, cols = 1:ref_ncol)
  }
  else if(standard_pos == "lowerRight"){
    standard_window <- list(rows = ref_nrow:nrow, cols = ref_ncol:ncol)
  }
  
  # Create list of windows
  window_list <- list()
  
  for(i in 1:length(files)){
    window_list[[i]] <- standard_list[[1]]$surface[standard_window$rows, standard_window$cols]
  }
  
  # cut out anything above the standard reference (beyond the zero boundary)
  # start searching from bottom, up and left
  
  # Perform rotation to match
  
  # Trim ends
  
  # lateral translations
  
}