# 0. Library and fonts management
library(tidyverse)
library(ragg)
library(scales)
library(ggfx)

## Defining the plot font (installed beforehand from Google Fonts)
hand <- "Gloria Hallelujah"

## Defines some layout constants
lnhgt <- 1.2

# 1. Data download, load and handling
rawdata <- readr::read_csv('2022/day23/data.csv')

## Renames and keeps only relevant data
df <- rawdata %>% 
  dplyr::rename(Price = "Price for Light – Fouquet and Pearson (2012)") %>% 
  dplyr::select(Year, Price)

## Converts the data to lumen-hour per pound
df <- df %>% dplyr::mutate(Energy = 1000000/Price)

## Creates a new variable in 0-1 scale
df <- df %>% dplyr::mutate(scaled = scales::rescale(Energy))

## Defines lightness of the tiles
fun_clr <- grDevices::colorRamp(c("#020202", "white"))

## Applies the color function
df <- df %>% 
  dplyr::mutate(color = grDevices::rgb(do.call(fun_clr, list(scaled)), maxColorValue = 255))

## Defines coordinates for the titles
titles <- tibble(
  x = c(1740, 2006, 2006),
  y = c(1,0.74,0.6),
  size = c(53,7.5,4),
  label = c(
    "Let there be",
    
    "The tiles bellow show how much luminous energy (in lumen-hours) was possible to buy with one British Pound in the UK.\nThrough most of the country's history, energy was a luxurious resource. Cost was adjusted for inflation and expressed in values for the year 2000.",
    
    "Data from: Fouquet and Pearson (2012) – The long run demand for lighting: elasticities and rebound effects in different phases of economic\ndevelopment. Economics of Energy and Environmental Policy, v.1, pp.83-100 by way of Our World in Data. Graphic by: Ícaro Bernardes (@IcaroBSC)"
  )
)

## Defines coordinates for the notes
notes <- tibble(
  x = 1920,
  y = 0.25,
  size = 6,
  label = c(
    "Candles, gas and kerosene were providers of light for a long time.\nEach technolgy increased availability of artificial lights for the general public but prices were still very high.\nThen, in the 1920s electricity became very cheap. Since then it is the main source of energy for lighting in the UK."
  )
)

# 2. Generates the plot
## Creates the plot
p <- df %>% 
  ggplot() +
  
  ### Places the tiles
  ggfx::with_blur(
    geom_tile(aes(x = Year, y = 0.25, fill = I(color)), height = 0.5),
    sigma = 2
  ) +
  
  ### Places the titles
  geom_text(aes(x = x, y = y, label = label, size = I(size)), color = "white",
            family = hand, hjust = 1, vjust = 1, lineheight = lnhgt, data = titles) +
  
  ### Places the word "LIGHT!" with a glow around it
  ggfx::with_outer_glow(
    geom_text(aes(x = 2006, y = 1, label = "LIGHT!"), color = "white",
              family = hand, hjust = 1, vjust = 1, size = 53),
    colour = "#f0f0f0",
    expand = 16,
    sigma = 6
  ) +
  
  ### Places some notes on the data
  geom_text(aes(x = x, y = y, label = label, size = I(size)), color = "white",
            family = hand, hjust = 1, lineheight = lnhgt, data = notes) +
  
  ### Defines limits for the y-axis
  coord_cartesian(ylim = c(0,1)) +
  
  ### Defines breaks for the x-axis
  scale_x_continuous(breaks = seq(1300, 2000, 100)) +
  
  ### Eliminates and customizes plot elements
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    plot.margin = margin(t = 20, r = 0, b = 20, l = 0),
    axis.text.x = element_text(color = "white", family = hand, size = 30)
  )

## Saves the plot
ggsave("2022/day23/day23.png", plot = p, dpi = "retina",
       width = 24, height = 10, device = ragg::agg_png, res = 320)

