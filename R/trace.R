trace <- function(x, standard_pos){
  
  # Initialize parameters
  
  if(standard_pos == "upperLeft"){
    col_id <- 1
    acol_id <- ncol(x)

    row_id <- 1
    arow_id <- nrow(x)
  }
  
  if(standard_pos == "upperRight"){
    col_id <- ncol(x)
    acol_id <- 1

    row_id <- 1
    arow_id <- nrow(x)
  } 
  
  if(standard_pos == "lowerLeft"){
    col_id <- 1
    acol_id <- ncol(x)

    row_id <- nrow(x)
    arow_id <- 1
    }
  
  if(standard_pos == "lowerRight"){
    col_id <- ncol(x)
    acol_id <- 1

    row_id <- nrow(x)
    arow_id <- 1
    }
  
  # SEARCH FOR INNER CORNER
  
  pos <- x[row_id, col_id]
  
  if(pos == 0){
    # The outer corner is a zero
    z_row_id = row_id
    z_col_id = col_id
  }

  col_row <- function(col_id, acol_id, row_id, arow_id){
    
    # Search for the inner corner
    for(col in col_id:acol_id){
      for(row in row_id:arow_id){
        
        pos <- x[row, col]
        
        z_row_id <- row
        z_col_id <- col
        
        if(pos == 0){
          break
        }
      }
      
      if(pos == 0){
        break
      }
    }
    
    
      ids <- list()
      
      ids$row_id <- z_row_id
      ids$col_id <- z_col_id
      
      return(ids)
    }
    
    row_col <- function(col_id, acol_id, row_id, arow_id){
      
      for(row in row_id:arow_id){
        for(col in col_id:acol_id){
          
          pos <- x[row, col]
          
          z_row_id <- row
          z_col_id <- col
          
          if(pos == 0){
            break
          }
        }
        if(pos == 0){
          break
        }
      }
      
      ids <- list()
      
      ids$row_id <- z_row_id
      ids$col_id <- z_col_id
      
      return(ids)
    }
  
  ############################
  # SEARCH FOR INNER CORNER
  ###########################
  
  if(pos != 0){
    if(standard_pos == "lowerRight"){
     
      ids <- col_row(col_id = col_id, acol_id = acol_id, row_id = row_id, arow_id = arow_id)
      
      z_row_id <- ids$row_id
      z_col_id <- ids$col_id
      
      clockwise = TRUE
    
      if(!(z_row_id %in% row_id:(nrow(x)/2))){
          
          clockwise = FALSE 
          
          ids <- row_col(col_id = col_id, acol_id = acol_id, row_id = row_id, arow_id = arow_id)
          
          z_row_id <- ids$row_id
          z_col_id <- ids$col_id
      }
    }
    
    
    if(standard_pos == "lowerLeft"){
      
      ids <- col_row(col_id = col_id, acol_id = acol_id, row_id = row_id, arow_id = arow_id)
      
      z_row_id <- ids$row_id
      z_col_id <- ids$col_id
      
      clockwise = FALSE
      
      if(!(z_row_id %in% row_id:(nrow(x)/2))){
        
        clockwise = TRUE 
        
        ids <- row_col(col_id = col_id, acol_id = acol_id, row_id = row_id, arow_id = arow_id)
        
        z_row_id <- ids$row_id
        z_col_id <- ids$col_id
      }
    
    }
    
    if(standard_pos == "upperRight"){
      
      ids <- col_row(col_id = col_id, acol_id = acol_id, row_id = row_id, arow_id = arow_id)
      
      z_row_id <- ids$row_id
      z_col_id <- ids$col_id
      
      clockwise = FALSE
      
      if(!(z_row_id %in% row_id:(nrow(x)/2))){
        
        clockwise = TRUE 
        
        ids <- row_col(col_id = col_id, acol_id = acol_id, row_id = row_id, arow_id = arow_id)
        
        z_row_id <- ids$row_id
        z_col_id <- ids$col_id
      }
      
    }
    
    if(standard_pos == "upperLeft"){
      
      ids <- col_row(col_id = col_id, acol_id = acol_id, row_id = row_id, arow_id = arow_id)
      
      z_row_id <- ids$row_id
      z_col_id <- ids$col_id
      
      clockwise = TRUE
      
      if(!(z_row_id %in% row_id:(nrow(x)/2))){
        
        clockwise = FALSE
        
        ids <- row_col(col_id = col_id, acol_id = acol_id, row_id = row_id, arow_id = arow_id)
        
        z_row_id <- ids$row_id
        z_col_id <- ids$col_id
      }
      
    }
  }
  
  in_list <- list()
  in_list$col_id <- z_col_id
  in_list$acol_id <- acol_id
  in_list$row_id <- z_row_id
  in_list$arow_id <- arow_id
  in_list$clockwise <- clockwise
  

  return(in_list)
  
}

# THIS ALGORITHM BREAKS WHEN YOU NEED TO ROTATE COUNTER CLOCKWISE.

# Determine appropriate direction of rotation by searching for the first zero and checking
# if it is in the lower half of the standard (assuming lowerRight). If not, then the direction
# is counter clockwise.  Instead, go by rows

# NEED TO TRIM APRIL 16 2021 BEFORE OFFICE HOUR DIDN"T GET TO IT