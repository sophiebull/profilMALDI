scale <- function(x, scale.row = 100, scale.col = 100){
  
  nrow <- nrow(x)
  ncol <- ncol(x)
  
  # SCALING
  
  new.row <- round(nrow*(scale.row/100))
  new.col <- round(ncol*(scale.col/100))
  
  if((new.col - ncol)/2 > 0){
    col.pad <- ceiling( (new.col - ncol)/2 )
  }
  
  if((new.col - ncol)/2 < 0){
    col.pad <- floor( (new.col - ncol)/2 ) 
  }
  
  if((new.row - nrow)/2 > 0){
    row.pad <- ceiling( (new.row - nrow)/2 )
  }
  
  if((new.row - nrow)/2 < 0){
    row.pad <- floor( (new.row - nrow)/2 )
  }
  
  # WARNINGS
  
  if(((new.col - ncol)/2) %% 1 != 0){
    warning("Warning: Column padding factor does not evenly scale.  Left and right column padding will be uneven.")
  }
  
  if(((new.row - nrow)/2) %% 1 != 0){
    warning("Warning: Row padding factor does not evenly scale.  Top and bottom row padding will be uneven.")
  }
  
  # INITIALIZE NEW MATRIX
  
  scale_mat <- matrix(rep(0, new.row*new.col), nrow = new.row, ncol = new.col)
  
  # POPULATE NEW MATRIX
  
  # Row replace index
  row_id_expand <- (row.pad + 1):(row.pad+nrow)
  row_id_delete <- (-row.pad +1):(nrow+row.pad)
  
  # Check that index length matches new.row
  
  #logic_r <- length(row_id_expand) != new.row | length(row_id_delete) != new.row

  #if(logic_r){
  # row_id_expand <- (row.pad + 1):(row.pad+1+new.row)
  # row_id_delete <- (-row.pad + 1):
     
  #}
  
  # Column replace index
  
  col_id_expand <- (col.pad + 1):(col.pad+ncol)
  col_id_delete <- (-col.pad +1):(ncol+col.pad)
  
  # Check that index length matches new.col
  
  #logic_c <- length(col_id_expand) != new.col | length(col_id_delete) != new.col
  
  #if(logic_c){
    
  #}

  
  
  
  # Expand rows, Expand columns
  if(scale.row > 100 & scale.col > 100){
    scale_mat[row_id_expand, col_id_expand] <- x 
  }
  
  # Expand rows, Preserve columns
  if(scale.row > 100 & scale.col == 100){
    scale_mat[row_id_expand, ] <- x
  }
  
  # Expand rows, Delete columns
  if(scale.row > 100 & scale.col < 100){
    scale_mat[row_id_expand, ] <- x[, col_id_delete]
  }
  
  
  
  # Preserve rows, Expand columns
  if(scale.row == 100 & scale.col > 100){
    scale_mat[ , col_id_expand] <- x
  }
  
  # Preserve rows, Preserve columns
  if(scale.row == 100 & scale.col == 100){
    scale_mat <- x
  }
  
  # Preserve rows, Delete columns
  if(scale.row == 100 & scale.col < 100){
    scale_mat <- x[ , col_id_delete] 
  }
  
  
  
  # Delete rows, expand columns
  if(scale.row < 100 & scale.col > 100){
    scale_mat[, col_id_expand]  <- x[row_id_delete,]
  }
  
  # Delete rows, Preserve columns
  if(scale.row < 100 & scale.col == 100){
    scale_mat <- x[row_id_delete]
  }
  
  # Delete rows, delete columns
  if(scale.row < 100 & scale.col < 100){
    scale_mat <- x[row_id_delete, col_id_delete]
  }
  
  return(scale_mat)
}
