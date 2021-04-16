rotate <- function(x, standard_thickness, tolerance, standard_pos, direction = "clockwise"){
  
  standard_positions <- c("upperLeft", "upperRight", "lowerLeft", "lowerRight")
  directions <- c("clockwise", "counterclockwise")
  
  if(direction == "clockwise"){
    direction = 1
  } 
  else if(direction == "counterclockwise"){
    direction = -1
  }
  
  if(!standard_pos %in% standard_positions){
    stop(paste0("Please choose a standard position from the following options: ", paste(standard_positions, collapse = ", ")))
  }
  
  upper <- standard_thickness + tolerance
  lower <- standard_thickness - tolerance
  
  ncol <- ncol(x)
  nrow <- nrow(x)
  
  ###########################
  # Locate external standard
  ###########################
  
  ##
  
  # NEED TO TRIM EXCESS BECAUSE THE ALGORITHM BELOW WILL AUTOMATICALLY SELECT THE CORNERS BECAUSE OF THE PADDING
  
  ##
  
  x_trim <- trim(x, standard_pos = standard_pos)
  
  # Identify row and column where first zero appears
  # (this indicates the inner corner of the standard)
  
  traces <- trace(x = x_trim, standard_pos = standard_pos)
  
  col_id <- traces$col_id
  acol_id <- traces$acol_id
  sign_c <- traces$sign_c
  
  row_id <- traces$row_id
  arod_id <- traces$arow_id
  sign_r <- traces$sign_r
  
  pos <- x_trim[row_id, col_id]
  
  
  while(pos != 0){
    
    row_id <- row_id + sign_r*1
    col_id <- col_id + sign_c*1
    
    pos <- x_trim[row_id, col_id]
    
  }
  
  # Determine required degree of rotation
  
  sum_row <- sum(x_trim[row_id, acol_id:col_id ])
  
  len <- length(acol_id:col_id) # length of inner arm
  tot_lower <- len*lower # if every value along inner arm was standard thickness
  
  rot_deg <- 0 # might also need to rotate counter clockwise
  
  while(!(sum_row < 0.20*tot_lower)){ # inner arm contains less than 20% of thickness values
    
    rot_deg <- rot_deg + direction*1 # -1 for counter clockwise, +1 for clockwise
    
    rot <- rotate.matrix(x_trim, angle = rot_deg, method = "bilinear")

    rot_trim <- trim(x = rot, standard_pos = standard_pos)
    
    # Find new first zero value in inner corner
    
    traces <- trace(x = rot_trim, standard_pos = standard_pos)
    
    col_id <- traces$col_id
    acol_id <- traces$acol_id
    sign_c <- traces$sign_c
    
    row_id <- traces$row_id
    arod_id <- traces$arow_id
    sign_r <- traces$sign_r
    
    pos <- rot_trim[row_id, col_id]
    
    
    while(pos != 0){
      
      row_id <- row_id + sign_r*1
      col_id <- col_id + sign_c*1
      
      pos <- rot_trim[row_id, col_id]
      
    }
    
    # Condition to check that sum of entire row is zero
    sum_row <- sum(rot_trim[row_id, acol_id:col_id])
    
    len <- length(acol_id:col_id) # length of inner arm
    tot_lower <- len*lower
    
  }
  
  # Now apply the rotation to the padded data
  
  rot <- rotate.matrix(x, angle = rot_deg, method = "bilinear")
  
  # Need to implement a smart tool to decide whether to rotate clockwise or counter clockwise....
  
  rot_list <- list()
  rot_list$rot_deg <- rot_deg
  rot_list$matrix <- rot
  
  return(rot_list)
}







