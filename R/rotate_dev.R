
### ONLY WORKS FOR SMALL CORRECTIONS
# TRY INCREASING PADDING NOT ZEROES BUT WHAT THE EDGES ARE. AND THEN USE ONLY THE INNER PART TO ROTATE. THIS PREVENT ZEROES ON EDGES

rotate <- function(x, standard_thickness, tolerance, threshold, standard_pos){
  
  standard_positions <- c("upperLeft", "upperRight", "lowerLeft", "lowerRight")
  directions <- c("clockwise", "counterclockwise")
  
  if(!standard_pos %in% standard_positions){
    stop(paste0("Please choose a standard position from the following options: ", paste(standard_positions, collapse = ", ")))
  }
  
  # Determine acceptable threshold
  
  upper <- standard_thickness + tolerance
  lower <- standard_thickness - tolerance
  
  ncol <- ncol(x)
  nrow <- nrow(x)
  
  ###########################################
  # Locate inner corner of external standard
  ###########################################
  
  # Trim excess padding
  
  # x_trim <- trim(x, standard_pos = standard_pos)
  
  # Identify row and column of inner corner
  
  traces <- trace(x = x, standard_pos = standard_pos)
  
  clockwise <- traces$clockwise
  
  col_id <- traces$col_id
  acol_id <- traces$acol_id
  
  row_id <- traces$row_id
  arow_id <- traces$arow_id
  
  ############################################################
  # Determine required degree of rotation using trimmed matrix
  ############################################################
  
  # Here's where the computer decides whether to rotate clockwise or counterclockwise.
  
  # For now, we'll work with lower Right and need to rotate clockwise.  Then check:
  
  # Check alignment of horizontal
  arm_row <- x[(row_id+1):(row_id+4), acol_id:col_id ]
  arm_row <- apply(arm_row, MARGIN = 2, FUN = median)
  
  # Check alignment of vertical
  arm_col <- x[row_id:arow_id, (col_id+1):(col_id+4)]
  arm_col <- apply(arm_col, MARGIN = 1, FUN = median)
  
  # Threshold to accept alignment
  # Accept if every element in 'arm' is within the threshold
  logic_row <- arm_row > lower & arm_row < upper
  logic_col <- arm_col > lower & arm_col < upper
  
  ######################################################
  # Rotate trimmed matrix until alignment is acceptable
  ######################################################
  
  
  if(clockwise){
    direction = 1
    arm = sum(logic_row)/length(logic_row) # % of elements TRUE
  } 
  
  else if(!clockwise){
    direction = -1
    arm = sum(logic_col)/length(logic_col) # % of elements TRUE
  }
  
  rot_deg <- 0
  
  plot_profilm(data = x)
  
  while(arm < threshold){ # inner arm has every value within the threshold
    
    # Increase degree of rotation by one
    
    rot_deg <- rot_deg + direction*1 # -1 for counter clockwise, +1 for clockwise
    
    # Rotate original matrix by rot_deg
    rot <- rotate.matrix(x, angle = rot_deg, method = "NN")
    
    # Fill in edge zeroes
    # for lower right
    # horizontal bound 
    # hb = ncol(rot)
    # vertical bound
    # vb = nrow(rot)
    
    # rot_filled <- rot
    
    #  if(all(rot_filled[,hb] > lower & rot_filled[,hb] < upper)){
    #    message("right edge is already filled")
    #  }
      
    #  if(any(rot_filled[,hb] < lower)){
    #    rot_filled[,hb] <- rep(standard_thickness, nrow(rot_filled))
    #  }
      
    #  if(all(rot_filled[vb,] > lower & rot_filled[vb,] < upper)){
    #    message("bottom edge is already filled")
    #  }
        
    #  if(any(rot_filled[vb,] < lower)){
    #      rot_filled[vb,] <- rep(standard_thickness, ncol(rot_filled))
    #  }
    
    # Rotate indicator matrix to find new trace
    rot_ind <- rotate.matrix(x = traces$indicator, angle = rot_deg, method = "NN")
    ind_pos <- which(rot_ind == 10, arr.ind = TRUE)
    
    # Trim rotated matrix
    rot_trim <- trim(rot, standard_pos = standard_pos)
    
    # Find corresponding inner corner of indicator matrix
    row_remove_index <- rot_trim$row_remove_index
    col_remove_index <- rot_trim$col_remove_index
    
    ind_trim <- rot_ind[-(row_remove_index), -(col_remove_index)]
    
    ind_trim_pos <- which(ind_trim == 10, arr.ind = TRUE)
    
    rot_trim <- rot_trim$matrix
    
    col_id <- ind_trim_pos[2]
    acol_id <- 1
    
    row_id <- ind_trim_pos[1]
    arow_id <- nrow(rot_trim)
    
    
    
    # Determine new acceptance threshold
    clockwise <- traces$clockwise
    
    if(clockwise){
      # Check alignment of horizontal
      arm_row <- rot_trim[(row_id+1):(row_id+4), acol_id:col_id ]
      arm_row <- apply(arm_row, MARGIN = 2, FUN = median)
      
      logic_row <- arm_row > lower & arm_row < upper
      arm = sum(logic_row)/length(logic_row)
      
      plot_profilm(data = rot)
      rot_deg
      arm
    }
    
    plot_profilm(data = rot)
    rot_deg
    arm
    
    if(!clockwise){
      # Check alignment of vertical
      arm_col <- rot_trim[row_id:arow_id, (col_id+1):(col_id+4)]
      arm_col <- apply(arm_col, MARGIN = 1, FUN = median)
      
      logic_col <- arm_col > lower & arm_col < upper
      arm = sum(logic_col)/length(logic_col)
      
      plot_profilm(data = rot)
      rot_deg
      arm
    }
    
    
    
    # Iterate until alignment is in acceptable range

    # Save rot_deg value
  }
  
  ###################################################
  # Apply the appropriate rot_deg to the padded data
  ###################################################

  
  message(paste0("Degree of rotation: ",rot_deg))
  message(paste0("% of values in threshold:", round(arm, 2)))
  
  rot <- rotate.matrix(x, angle = rot_deg, method = "NN")

  rot_list <- list()
  rot_list$rot_deg <- rot_deg
  rot_list$matrix <- rot
  
  plot_profilm(data= rot)
  
  return(rot_list)
}





