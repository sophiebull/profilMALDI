trace <- function(x, standard_pos){
  
  if(standard_pos == "upperLeft"){
    col_id <- 1
    acol_id <- ncol(x)
    sign_c <- 1
    
    row_id <- 1
    arow_id <- nrow(x)
    sign_r <- 1
    
  }
  
  if(standard_pos == "upperRight"){
    col_id <- ncol(x)
    acol_id <- 1
    sign_c <- -1
    
    row_id <- 1
    arow_id <- nrow(x)
    sign_r <- 1
  } 
  
  if(standard_pos == "lowerLeft"){
    col_id <- 1
    acol_id <- ncol(x)
    sign_c <- 1
    
    row_id <- nrow(x)
    arow_id <- 1
    sign_r <- -1
    
    
    #col_id <- col_id + sign_c*1
  }
  
  if(standard_pos == "lowerRight"){
    col_id <- ncol(x)
    acol_id <- 1
    sign_c <- -1
    
    row_id <- nrow(x)
    arow_id <- 1
    sign_r <- -1
    
    }
  
  # SEARCH FOR INNER CORNER
  
  pos <- x[row_id, col_id]
  
  if(pos == 0){
    # The outer corner is a zero
    z_row_id = row_id
    z_col_id = col_id
  }
  
  
  else if(pos != 0){
    
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
    
  }
  
  in_list <- list()
  in_list$col_id <- z_col_id
  in_list$acol_id <- acol_id
  in_list$sign_c <- sign_c
  in_list$row_id <- z_row_id
  in_list$arow_id <- arow_id
  in_list$sign_r <- sign_r
  
  return(in_list)
  
}
