# 0. Library and fonts management
library(tidyverse)
library(showtext)
library(sf)
library(hues)
library(colorspace)
library(ggforce)
library(patchwork)

## Adding Google Fonts
sysfonts::font_add_google(name = "Grenze Gotisch", family = "Grenze Gotisch")
goth <- "Grenze Gotisch"

## Allows the use of the downloaded Google Font
## To see the results updated, it's needed to call windows() or save the image
showtext::showtext_opts(dpi = 320)
showtext::showtext_auto()

########### ATTENTION ##########################################################
## If you want to make your own stained glass, I recommend you make changes ####
## only to the number of points (n_pts), the seed and the colors. The colors ###
## can be modified by changing the number of colors in the palette (n_clr), ####
## and the parameters for generating the window palette (palpar). These ########
## parameters control the hue, chroma and luminance of the colors randomly #####
## searched by the iwanthue algorithm. Alternatively, you can define your own ##
## set of colors. Just write the number of base colors (n_clr) and put the hex #
## colors in the object 'palette' (line) 82. ###################################
################################################################################

## Defines some layout constants
r <- 0.3 ### Radio of the top of the window frame 
x0 <- 0.5 ### Center of the top of the window frame (x)
y0 <- 0.7 ### Center of the top of the window frame (y)
n_pts <- 200 ### Number of points to try to put inside the window
seed <- 13 ### Random seed used to create the points
n_clr <- 7 ### Number of colors in the window palette
### Parameters for generating the window palette (min and max)
palpar <- c(0, 40, #### Hue
            40, 100, #### Chroma
            15, 80) #### Luminance
lnhgt <- 1 ### Height of the text lines
width <- 25 ### Plot width in inches
height <- 10 ### Plot height in inches

# 1. Data download, load and handling
## Creates the window frame
cicvec <- seq(0.201,0.799,0.01) ### Vector of the circular section
frame <- cbind(
  c(0.2,0.2,
    cicvec,
    0.8,0.8), ### x coordinates
  c(0,0.7,
    (sqrt((r^2) - (cicvec-x0)^2)+y0),
    0.7,0) ### y coordinates
)

### Creates a tibble version of the frame
frame_tib <- tibble(
  x = frame[,1],
  y = frame[,2]
)

## Creates a closed version of the frame and converts it to a polygon
frame_closed <- frame %>% 
  rbind(c(0.2,0)) %>% 
  list() %>% 
  sf::st_polygon()

## Creates random data points uniformly distributed between 0 and 1
set.seed(seed)
df <- tibble(x = runif(n_pts), y = runif(n_pts))

## Creates a new sf object to hold the created points
df_pts <- sf::st_as_sf(df, coords = c("y","x"))

## Keeps only the random points that are within the frame
contained <- sf::st_contains(frame_closed, df_pts)
df <- df %>% dplyr::slice(contained[[1]])

## Creates a random palette for the window using the iwanthue algorithm.
## See https://medialab.github.io/iwanthue/ for help
set.seed(seed)
palette <- hues::iwanthue(n = n_clr,
                          hmin = palpar[1], hmax = palpar[2],
                          cmin = palpar[3], cmax = palpar[4],
                          lmin = palpar[5], lmax = palpar[6],
                          random = TRUE)

## Creates a function that takes a random color from the palette of the window
## and outputs them with a random lightness/darkness
lights <- function(size) {
  
  ### Samples colors for every point
  clr = sample(palette, size = size, replace = TRUE)
  
  ### Takes random values for the lightness/darkness
  intense = runif(size, max = 0.8)
  
  ### Gets the half of the number of points (rounded)
  rnd = round(size/2)
  
  ### Applies the lightness/darkness to the halves of the vector
  c(colorspace::lighten(col = clr[1:rnd], amount = intense[1:rnd]),
    colorspace::darken(col = clr[(rnd+1):size], amount = intense[(rnd+1):size]))
  
}

## Applies random colors to the remaining points
set.seed(seed)
df <- df %>% dplyr::mutate(fill = lights(n()))

## Defines the coordinates for the titles changing
titles <- tibble(
  x = 0,
  y = c(0.95, 0.6, 0.07),
  size = c(73, 14, 12),
  label = c(
    "Voronoi Vitrail",
    
    
'This stained glass (or "vitrail" in French) is composed by taking random
points with coordinates between 0 and 1 and within the frame of the window.
An algorithm of Voronoi tesselation divides the frame in sections based on
the distance between the points. Each section is randomly colored. The same
seed is used in every step of randomness. Its value is printed in the window.
You can make your own glass by changing the seed, quant. of points and color.',

"Art by Ícaro Bernardes (@IcaroBSC)"
  )
)

# 2. Generates the plot
## Create the window
window <- df %>% 
  ggplot() +
  
  ### Places a background white contour to highlight the window
  geom_polygon(aes(x = x, y = y), color = "white",
               size = 20, data = frame_tib) +
  
  ### Places the Voronoi tiles
  geom_voronoi_tile(aes(x = x, y = y, group = -1L, fill = I(fill)),
                    color = "black", size = 1.5, bound = frame) +
  
  ### Places the value of the seed as text
  geom_text(aes(x = 0.5, y = 0.5, label = seed), alpha = 0.02, color = "white",
            size = 100, family = goth) +
  
  ### Guarantees limits and proportions on the plot
  coord_equal(xlim = c(0.1,0.9), ylim = c(0,1)) +
  
  ### Eliminates elements of the plot
  theme_void()

## Creates the main plot
p <- titles %>% 
  ggplot() +
  
  ### Places the titles
  geom_text(aes(x = x, y = y, label = label, size = I(size)), family = goth,
            hjust = 0, vjust = 1, color = "white", lineheight = lnhgt) +
  
  ### Guarantees limits and proportions on the plot
  coord_equal(xlim = c(0,width/10), ylim = c(0,height/10)) +
  
  ### Customizes and eliminates elements of the plot
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black")
  ) +
  
  ### Places the window
  patchwork::inset_element(window, bottom = 0.05, top = 0.95,
                           left = 0.7, right = 1)

## Saves the plot
ggsave("2022/day10/day10.png", plot = p, dpi = "retina",
       width = width, height = height)

