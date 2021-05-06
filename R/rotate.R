rotate <- function(x, standard_thickness, tolerance, standard_pos, threshold){
  
  standard_positions <- c("upperLeft", "upperRight", "lowerLeft", "lowerRight")
  directions <- c("clockwise", "counterclockwise")
  
  if(!standard_pos %in% standard_positions){
    stop(paste0("Please choose a standard position from the following options: ", paste(standard_positions, collapse = ", ")))
  }
  
  # Determine acceptable threshold
  
  upper <- standard_thickness + threshold
  lower <- standard_thickness - threshold
  
  ncol <- ncol(x)
  nrow <- nrow(x)
  
  ###########################################
  # Locate inner corner of external standard
  ###########################################
  
  # Trim excess padding
  
  x_trim <- trim(x, standard_pos = standard_pos)
  
  # Identify row and column of inner corner
  
  traces <- trace(x = x_trim, standard_pos = standard_pos)
  
  clockwise <- traces$clockwise
  
  col_id <- traces$col_id
  acol_id <- traces$acol_id
  
  row_id <- traces$row_id
  arod_id <- traces$arow_id
  
  ############################################################
  # Determine required degree of rotation using trimmed matrix
  ############################################################
  
  # Here's where the computer decides whether to rotate clockwise or counterclockwise.
  
  # For now, we'll work with lower Right and need to rotate clockwise.  Then check:
  
  # Check alignment of horizontal
  sum_row <- sum(x_trim[row_id, acol_id:col_id ])
  
  # Check alignment of vertical
  sum_col <- sum(x_trim[row_id:arow_id, col_id])

  
  # Threshold to accept alignment
  
  len_h <- length(acol_id:col_id) # length of inner arm, horizontal
  len_v <- length(arow_id:row_id) # length of inner arm, vertical
  
  tot_lower_h <- len_h*lower
  tot_lower_v <- len_v*lower # if every value along inner arm was standard thickness
  
  ######################################################
  # Rotate trimmed matrix until alignment is acceptable
  ######################################################
  
  
  if(clockwise){
    direction = 1
    arm = sum_col
  } 
  
  else if(!clockwise){
    direction = -1
    arm = sum_row
  }
  
  rot_deg <- 0
  
  while(!(arm < tolerance*tot_lower)){ # inner arm contains less than 30% of thickness values
    
    # Increase degree of rotation by one
    
    rot_deg <- rot_deg + direction*1 # -1 for counter clockwise, +1 for clockwise
    
    # Rotate trimmed matrix
    rot <- rotate.matrix(x_trim, angle = rot_deg, method = "bilinear")
    
    # Trim new rotated matrix
    rot_trim <- trim(x = rot, standard_pos = standard_pos)
    
    # Identify location of new inner corner
    traces <- trace(x = rot_trim, standard_pos = standard_pos)
    
    col_id <- traces$col_id
    acol_id <- traces$acol_id
    
    row_id <- traces$row_id
    arod_id <- traces$arow_id
    
    # Determine new acceptance threshold
    clockwise <- traces$clockwise
    
    if(clockwise){
      # Check alignment of vertical
      arm <- sum(rot_trim[row_id:arow_id, col_id]) # sum_col
      len_v <- length(arow_id:row_id) # length of inner arm, vertical
      tot_lower <- len_v*lower
    }
    
    if(!clockwise){
      # Check alignment of horizontal
      arm <- sum(rot_trim[row_id, acol_id:col_id]) # sum_row
      len_h <- length(acol_id:col_id) # length of inner arm, horizontal
      tot_lower <- len_h*lower
    }
    
    # Iterate until alignment is in acceptable range
    
    # Save rot_deg value
  }
  
  ###################################################
  # Apply the appropriate rot_deg to the padded data
  ###################################################
  
  message(paste0("Degree of rotation: ", rot_deg))
  
  rot <- rotate.matrix(x, angle = rot_deg, method = "bilinear")
  
  # Need to implement a smart tool to decide whether to rotate clockwise or counter clockwise....
  # did this in TRACE
  
  rot_list <- list()
  rot_list$rot_deg <- rot_deg
  rot_list$matrix <- rot
  
  
  return(rot_list)
}




