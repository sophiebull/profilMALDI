##' Plot Optical Profilometry Data
##' 
##' A function to plot optical profilometry data obtained from the ProFilm 3D Optical Profilometer.
##' 
##' @param data matrix; The matrix containing the topological data.
##' @param threeD logical; TRUE displays a three-dimensional contour plot (rendered by R Package \code{plot_ly}).
##' @param tolerance numeric; The degree of tolerance for defining the contours in the two-dimensional plot.
##' @param palette character; The color palette to use.
##' @param contour_col character; The colour to use for the contours in the two-dimensional plot
##' 
##' @details 
##' 
##' Use code \code{color_palettes()} to see the available color palette options.
##' 
##' 
##' @return Returns a surface plot representing surface topography (heights).
##'
##' @author Sophie Castel <sophie.castel@@ontariotechu.net>
##' @references https://github.com/castels/profilMALDI
##' @keywords plot optical profilometry surface profile

# -----------------------------------------------------------------------
# Last Updated: April 6, 2021
# Author: Sophie Castel
# Title: profilMALDI: Plot Profilometry Data
# -----------------------------------------------------------------------

plot_profilm <- function(data, threeD = TRUE, tolerance = 6, palette = "inferno", contour_col = "white"){
  
  palettes <- color_palettes()
  
  if(!palette %in% palettes){
    message(paste0("Palette '", palette, "' not found - defaulting to 'inferno'.  Please use color_palettes() for available palettes."))
    palette <- "inferno"  
  }
  
  if(!is.logical(threeD)){
    stop("Argument 'threeD' must be a logical element: TRUE = 3D plot, FALSE = 2D plot")
  }
  
  if(!is.numeric(tolerance)){
    stop("Please set the tolerance to be a numeric value.")
  }
  
  # Extract colours from palette
  colorscale <- color_values(1:20, palette = palette)

  z <- data$surface 
  
  xres <- ncol(z)
  yres <- nrow(z)
  z_max <- max(z)
  

  if(threeD){
    
    # Define axes
    
    axx <- list(nticks = 10,
                range = c(1,xres),
                title = "x",
                showgrid = F) 
    
    axy <- list(nticks = 10,
                range = c(1,yres),
                title = "y",
                showgrid = F) 
    
    
    axz <- list(nticks = 10,
                range = c(1,z_max),
                title = "height",
                showgrid = F) 
    
    
    # Plotly
    
    #colorscale = list(seq(0,1,length.out=xres*yres), palette)
    
    
    p <- plot_ly(z = z, colors = colorscale) %>% add_surface(
      
      contours = list(z = list(show = TRUE,
                               usecolormap = TRUE,
                               highlightcolor = "#ff0000",
                               project = list(z = TRUE)
      )
      )
    ) %>%
      
      layout(scene = list(xaxis = axx,
                          yaxis = axy,
                          zaxis = axz)
      ) 
  
  }
  
  else if(!threeD){
    
    # reshape data
    
    z <- melt(z)
    names(z) <- c("row_id", "col_id", "value")
    
    
    # ggplot
    
    p <- ggplot(z, aes(x = col_id, y = row_id, z = value, fill = value, label = value)) + 
      geom_tile() + 
      #geom_text(col = "black") + 
      scale_fill_gradientn(colors = colorscale, values = c(0,1)) + 
      scale_y_continuous(trans = "reverse") + 
      theme(legend.position = "none",
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank()) + 
      ggtitle(paste0("Field: (", xres, " x ", yres, ")")) + 
      geom_contour(aes(x = col_id, y = row_id, z = value), color = contour_col, bins = tolerance)
    
  }
  
  
  

  

    
  return(p)
  
  
}


