# 0. Library and fonts management
library(tidyverse)
library(patchwork)
library(ggwaffle)
library(ragg)

## Defines some layout constants
lnhgt <- 1.1 ### To set the lineheight
xmax <- 24 ### Max limit for the x-axis
ymax <- 15 ### Max limit for the y-axis
width <- 12 ### Image width
height <- width*ymax/xmax ### Image height

# 1. Data handling
## Defines percentages of the categories
Vpct <- c(Agriculture = 50, Forests = 37, Shrub = 11,
           `Urban and built-up land` = 1, Freshwater = 1)

## Defines the icons for the categories
df <- tibble(
  icon = c(rep("👩‍🌾",Vpct[1]),rep("🌲",Vpct[2]),rep("🌿",Vpct[3]),
           rep("🏙",Vpct[4]),rep("💧",Vpct[5])),
  order = c(rep(1,Vpct[1]),rep(2,Vpct[2]),rep(3,Vpct[3]),
            rep(4,Vpct[4]),rep(5,Vpct[5]))
)

## Generates coordinates for the icons
waffle_data <- df %>% 
  ggwaffle::waffle_iron(ggwaffle::aes_d(group = order), rows = 10) %>% 
  dplyr::left_join(df, by = c("group" = "order"))

## Define coordinates for the titles
titles <- tibble(
  x = 0.7,
  y = ymax - c(0,2.5,3.7),
  size = c(30,9,5),
  label = c(
    "Feed the World",
    
    "Half of the world’s habitable land is used for agriculture",
    
    "Data from: Our World in Data through FAO (UN) | Graphic by: Ícaro Bernardes (@IcaroBSC)"
  )
)

## Define coordinates for the legend texts
legends_texts <- tibble(
  x = 14,
  y = c(9.5,9.5,7.5,7.5,5.5,5.5,3.5,3.5,1.5,1.5) + rep(c(+0.26,-0.5), 5),
  size = rep(c(10,5), 5),
  label = c(
    "Agriculture",
    "50% (51M km²)",
    
    "Forests",
    "37% (39M km²)",
    
    "Shrub",
    "11% (12M km²)",
    
    "Urban and built-up land",
    "1% (1.5M km²)",
    
    "Freshwater",
    "1% (1.5M km²)"
  )
)

## Define coordinates for the legend icons
legends_icons <- tibble(
  x = c(13.7),
  y = seq(9.5, 1.5, -2),
  label = c(
    "👩‍🌾",
    "🌲",
    "🌿",
    "🏙",
    "💧"
  )
)

# 2. Pictogram making
p <- ggplot(NULL, aes(x = x, y = y)) + 
  
  ## Places the emojis in waffle form
  geom_text(aes(label = icon), family = "Apple Color Emoji",
            size = 6, data = waffle_data) +
  
  ## Places the titles
  geom_text(aes(label = label, size = I(size)), family = "Libre Baskerville",
            hjust = 0, vjust = 1, data = titles) +
  
  ## Places the legend texts
  geom_text(aes(label = label, size = I(size)), family = "Libre Baskerville",
            hjust = 0, data = legends_texts) +
  
  ## Places the legend icons
  geom_text(aes(label = label), family = "Apple Color Emoji",
            hjust = 1, size = 15, data = legends_icons) +
  
  ## Defines the axes limits and makes that units on both have the same dimension
  coord_equal(xlim = c(1,xmax), ylim = c(1,ymax)) + 
  
  ## Customizes elements of the plot
  ggwaffle::theme_waffle() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

## Saves the plot
ggsave("2022/day02/day02.png", plot = p, dpi = "retina",
       width = width, height = height, device = ragg::agg_png, res = 320)

