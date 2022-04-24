# 0. Library and fonts management
library(rayshader) 
library(rayvista)

# 1. Map composition and rendering
## Defines the central coordinates of the map
lat <- -13.004261797755436
long <- -38.52864647866323

## Generates the 3D map of the surface of the city
{
  rayvista::plot_3d_vista(
    ### Defines the bounding box of the map (center, radius and shape)
    lat = lat, long = long, radius = 1400, baseshape = "hex",
    ### Rotation in z-axis and azimuth and zoom
    theta = -70, phi = 45, zoom = 0.6, 
    ### Scale of exaggeration of elevations
    zscale = 5,
    ### Detail of the map
    overlay_detail = 16,
    ### Size of the canvas for the map
    windowsize = 1100
  )
  Sys.sleep(1)
  
  ## Renders a preview in the plot window (lower quality)
  # rayshader::render_snapshot()
  
  ## Renders a detailed map
  rayshader::render_highquality("2022/day20/base.png",
                                lightdirection = 30,
                                clear = TRUE)
  
  ## Closes the rgl window
  rgl::rgl.close()
}
