##' Read Raw Profilometry Files
##' 
##' A function to convert raw profilometry files obtained from the ProFilm 3D Optical Profilometer.
##' 
##' @param file character; The filepath to the local \code{.txt} data that is to be read.
##' 
##' @return Returns a list of 1. An array of values representing surface topography (heights) and 2. The pixel size (um). 
##'
##' @author Sophie Castel <sophie.castel@@ontariotechu.net>
##' @references https://github.com/castels/profilMALDI
##' @keywords data txt convert read

# -----------------------------------------------------------------------
# Last Updated: April 6, 2021
# Author: Sophie Castel
# Title: profilMALDI: Read Raw Profilometry Files
# -----------------------------------------------------------------------

read_profilm <- function(file){
  
  ##################
  # LOGICAL CHECKS
  ##################
  
  if(!is.character(file)){
    stop(paste0("Filepath must be a character string."))
  }
  
  #if(! file %in% list.files(path = "~/")){
  #  paste0("Specifed filepath not found.")
  #}

  
  #######################
  # Importing file as txt
  #######################
  
  profilm_txt <- readLines(con = file, n = 4)  # need to make sure there is an EOL character at the end of the txt file in order for this to work... 
  
  #######################
  # Parsing meta data
  #######################
  
  meta <- as.numeric(unlist(strsplit(profilm_txt[2], split="\t")))
  names(meta) <- unlist(strsplit(profilm_txt[1], split = "\t"))
  
  # defaults
  xres<- meta[1]
  yres <- meta[2]
  pixel_size <- meta[3]
  
  ##############################
  # Creating a matrix of values
  ##############################
  
  surface_matrix <- matrix(as.numeric(unlist(strsplit(profilm_txt[4], split = "\t"))), 
                           nrow = yres, ncol = xres, byrow = TRUE)
  
  
  #########
  # RETURN
  #########
  
  data <- list()
  
  data$surface <- surface_matrix
  data$pixel_size <- unname(pixel_size)
  data$field_width <- unname(pixel_size)*unname(xres)
  data$field_height <- unname(pixel_size)*unname(yres)
  
  return(data)
  
}
