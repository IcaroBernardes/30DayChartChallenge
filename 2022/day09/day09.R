# 0. Library and fonts management
library(tidyverse)
library(showtext)
library(ggtext)
library(geomtextpath)
library(ggfx)
library(colorspace)
library(ggboy)

## Adding Google Fonts
sysfonts::font_add_google(name = "VT323", family = "VT323")
mono <- "VT323"

## Allows the use of the downloaded Google Font
## To see the results updated, it's needed to call windows() or save the image
showtext::showtext_opts(dpi = 320)
showtext::showtext_auto()

## Defines some layout constants
lnhgt <- 1
xdom <- seq(0.5, 2, 0.01)

# 1. Data download, load and handling
## Creates the distributions
distros <- c("beta","norm","f","exp","t","cauchy")
set.seed(42)
df <- tibble(
  x = xdom,
  t = stats::dt(x, df = 100, ncp = 2),
  beta = stats::dbeta(x, shape1 = 2, shape2 = 2),
  cauchy = stats::dcauchy(x, location = 0, scale = 1),
  norm = stats::dnorm(x, mean = 1.9, sd = 0.3),
  exp = stats::dexp(x, rate = 1),
  f = stats::df(x, df1 = 500, df2 = 10)
) %>% 
  tidyr::pivot_longer(cols = -x,
                      names_to = "group",
                      values_to = "y")

## Gives colors to the mountains (focus in luminosity, not the acutal colors)
clr <- tibble(
  group = distros,
  color = colorspace::lighten("black", seq(0.5, 0.2, length.out = length(distros)))
)
df <- df %>%
  dplyr::left_join(clr) %>% 
  dplyr::mutate(group = factor(group, levels = distros))

## Creates the path for the labels
labels <- df %>% 
  dplyr::group_by(group) %>% 
  dplyr::slice(which.max(y):(which.max(y)+20)) %>% 
  dplyr::ungroup()

## Creates the title of the plot
title <- c("
<span style='font-size:15px;'>THE SUNRISE OF STATISTICAL DISTRIBUTIONS</span><br>
<span style='font-size:8px;'>GRAPHIC BY: ÍCARO BERNARDES (@IcaroBSC)</span>
")

# 2. Generates the plot
## Creates the base plot
base <- df %>% 
  ggplot() + 
  
  ### Places the sun with a glow around it
  ggfx::with_outer_glow(
    x = annotate("point", x = 1.5, y = 0.5, size = 5,
                 color = colorspace::lighten("black", 0.9)),
    colour = colorspace::lighten("black", 0.8),
    sigma = 20
  ) +
  
  ### Places the mountains
  geom_area(aes(x = x, y = y, fill = I(color), group = group),
            position = "identity", color = NA) +
  
  ### Eliminates the extra space of axes expansion
  coord_cartesian(expand = FALSE)+

  ### Eliminates plot elements
  theme_void() +
  theme(
    plot.background = element_rect(fill = colorspace::lighten("black", 0.6),
                                   color = NA)
  )

## Saves the base plot
path <- "2022/day09/base.png"
ggsave(path, plot = base, dpi = "retina",
       width = 128, height = 112, units = "px")

## Creates the main plot
p <- path %>% 
  
  ### Converts to the Game Boy palette
  ggboy(palette = 3) + 
  
  ### Places the title
  labs(caption = title) +
  
  ### Customizes plot elements 
  theme(
    text = element_text(family = mono),
    plot.background = element_rect(fill = "#0f380f", color = NA),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "npc"),
    plot.caption = ggtext::element_markdown(
      hjust = 0.5, color = "white", lineheight = 1,
      margin = margin(t = 0, r = 0, b = 0.05, l = 0, unit = "npc"))
  )

## Saves the plot
ggsave("2022/day09/day09.png", plot = p, dpi = "retina",
       width = 128*8, height = 112*8, units = "px")

