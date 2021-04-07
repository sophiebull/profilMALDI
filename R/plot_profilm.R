##' Plot Optical Profilometry Data
##' 
##' A function to plot optical profilometry data obtained from the ProFilm 3D Optical Profilometer.
##' 
##' @param z matrix; The matrix containing the topological data.
##' @param palette character; The color palette to use.
##' 
##' @details 
##' 
##' Color palette options:
##' 
##' \begin{itemize}
##' 
##' \item Greys
##' \item YlGnBu
##' \item Greens
##' \item YlOrRd
##' \item Bluered
##' \item RdBu
##' \item Reds
##' \item Blues
##' \item Picnic
##' \item Rainbow
##' \item Portland
##' \item Jet
##' \item Hot
##' \item Blackbody
##' \item Earth
##' \item Electric
##' \item Viridis
##' \item Cividis
##' 
##' \end{itemize}
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

plot_profilm <- function(z, palette = "Blackbody"){
  
  palettes <-  c("Greys", "YlGnBu", "Greens", "YlOrRd", "Bluered", "RdBu", "Reds", 
                 "Blues", "Picnic", "Rainbow", "Portland", "Jet", "Hot", "Blackbody", 
                 "Earth", "Electric", "Viridis", "Cividis")
  
  if(!palette %in% palettes){
    message(paste0("Palette '", palette, "' not found - defaulting to 'Blackbody'.  Please see Details for colorscale options."))
    palette <- "Blackbody"  
  }
  
  
  xres <- ncol(z)
  yres <- nrow(z)
  z_max <- max(z)
  
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
  
  p <- plot_ly(z = z, colorscale = palette) %>% add_surface(
    
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
  

    
    
  
  return(p)
}


