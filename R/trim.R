trim <- function(x, standard_pos = standard_pos){
  
  if(standard_pos == "upperLeft"){
    #scan down and to the right
    .col_id <- 1
    .acol_id <- ncol(x)
    .sign_c <- 1
    
    .row_id <- 1
    .nrow_id <- nrow(x)
    .sign_r <- 1
  }
  
  if(standard_pos == "upperRight"){
    # scan down and to the left
    .col_id <- ncol(x)
    .acol_id <- 1
    .sign_c <- -1
    
    .row_id <- 1
    .arow_id <- nrow(x)
    .sign_r <- 1
  } 
  
  if(standard_pos == "lowerLeft"){
    # scan up and to the right
    .col_id <- 1
    .acol_id <- ncol(x)
    .sign_c <- 1
    
    .row_id <- nrow(x)
    .arow_id <- 1
    .sign_r <- -1
  }
  
  if(standard_pos == "lowerRight"){
    # scan up and to the left
    .col_id <- ncol(x)
    .acol_id <- 1
    .sign_c <- -1
    
    .row_id <- nrow(x)
    .arow_id <- 1
    .sign_r <- -1
  }
  
  # Start with rows
  row_id <- .row_id
  r_pos <- x[row_id, ncol(x)/2]
  
  while(r_pos == 0){
    row_id <- row_id + .sign_r*1
    r_pos <- x[row_id, ncol(x)/2]
  }
  
  # Stop when row_id reaches the point where r_pos is no longer zero-valued
  
  # Next columns
  
  col_id <- .col_id
  c_pos <- x[row_id, col_id]
  
  while(c_pos == 0){
    col_id <- col_id + .sign_c*1
    c_pos <- x[row_id, col_id]
  }
  
  # Stop when col_id reaches the point where c_pos is no longer zero-valued
  # Trimming
  
  row_remove_index <- .row_id:row_id 
  col_remove_index <- .col_id:col_id

  trimmed <- x[-(row_remove_index), -(col_remove_index)]
  
  plot_profilm(data = trimmed, threeD = FALSE)
  
  #################
  # OPPOSITE SIDE
  #################
  
  if(standard_pos == "upperLeft"){
    #scan down and to the right
    .acol_id <- ncol(trimmed)
    .nrow_id <- nrow(trimmed)
  }
  
  if(standard_pos == "upperRight"){
    # scan down and to the left
    .col_id <- ncol(trimmed)
    .arow_id <- nrow(trimmed)
  } 
  
  if(standard_pos == "lowerLeft"){
    # scan up and to the right
    .acol_id <- ncol(trimmed)
    .row_id <- nrow(trimmed)
  }
  
  if(standard_pos == "lowerRight"){
    # scan up and to the left
    .col_id <- ncol(trimmed)
    .row_id <- nrow(trimmed)
  }
  
  # Other row side
  
  arow_id <- .arow_id

  ar_pos <- trimmed[arow_id, .col_id]
  
  while(ar_pos == 0){
    arow_id <- arow_id + (-1)*.sign_r
    ar_pos <- trimmed[arow_id, .col_id]
  }
  
  # Stop when row is no longer zero valued
  
  # Other column side
  
  acol_id <- .acol_id
  
  ac_pos <- trimmed[.row_id, acol_id]
  
  while(ac_pos == 0){
    acol_id <- acol_id + (-1)*.sign_c
    ac_pos <- trimmed[.row_id, acol_id]
  }
  
  
  row_remove_index <- .arow_id:arow_id
  col_remove_index <- .acol_id:acol_id
  
  trimmed <- trimmed[-(row_remove_index), -(col_remove_index)]
  
  plot_profilm(data = trimmed, threeD = FALSE)
  
  return(trimmed) 
}
