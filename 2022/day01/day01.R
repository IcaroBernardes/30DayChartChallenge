# 0. Library and fonts management
library(tidyverse)
library(ggforce)
library(patchwork)
library(showtext)
library(ggtext)
library(colorspace)

## Adding Google Fonts
sysfonts::font_add_google(name = "Roboto", family = "Roboto")
sans <- "Roboto"

## Allows the use of the downloaded Google Font
## To see the results updated, it's needed to call windows() or save the image
showtext::showtext_opts(dpi = 320)
showtext::showtext_auto()

## Defines some layout constants
lnhgt <- 1.1 ### To set the lineheight

# 1. Plots production
## Creates the pie charts
p1 <- ggplot(NULL) +
  ggforce::geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
                            amount = c(1,99),
                            fill = I(c("#F0D81B","#403EA3"))),
                        stat = 'pie', color = NA) +
  coord_fixed() +
  theme_void()

p2 <- ggplot(NULL) +
  ggforce::geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
                            amount = c(71.7,28.3),
                            fill = I(c("#403EA3","#F0D81B"))),
                        stat = 'pie', color = NA) +
  coord_fixed() +
  theme_void()

## Defines coordinates for all texts
texts <- tibble(
  x = c(rep(0.03,3),rep(0.255,2),rep(0.75,2)),
  y = c(0.98,0.87,0.79,rep(0.5,2),rep(0.5,2)),
  hjust = c(rep(0,3),rep(c(1,0),2)),
  vjust = c(rep(1,3),rep(0.5,4)),
  color = c(rep("black",3),rep("white",2),"black","white"),
  fontface = c("bold",rep("plain",2),rep(c("bold","plain"),2)),
  size = c(10,3,2,rep(c(8,4),2)),
  label = c(
    "Vice-champion of inequality",
    
    "Brazil will feel at home in this 2022 World Cup as in 2019 the country<br>
    lost only to Qatar in the matter of income concentration among the most rich.",
    
    "Data from: G1 through Pnud 2019 (IBGE) | Graphic by: Ícaro Bernardes (@IcaroBSC)",
    
    "The 1%",
    
    "most rich in<br> Brazil have",
    
    "28.3%",
    
    "of all income<br> in the country"
  )
)

## Creates the main plot
p <- ggplot(NULL) +
  
  ### Places the texts
  ggtext::geom_richtext(aes(x = x, y = y, label = label, size = I(size),
                            hjust = hjust, vjust = vjust, color = I(color),
                            fontface = I(fontface)),
                        lineheight = lnhgt, family = sans,
                        label.color = NA, fill = NA, data = texts) +
  
  ### Eliminates unnecessary plot elements and defines a background color
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#C9F0CB", color = NA)
  ) +
  
  ### Defines unitary limits for the plot and cuts expansion of axes
  coord_cartesian(xlim = c(0,1), ylim = c(0,1), expand = FALSE) +
  
  patchwork::inset_element(p1, left = 0, right = 0.5,
                           bottom = 0, top = 0.77, on_top = FALSE) +
  patchwork::inset_element(p2, left = 0.5, right = 1,
                           bottom = 0, top = 0.77, on_top = FALSE)


## Saves the plot
ggsave("day01/day01.png", plot = p, dpi = "retina",
       width = 6, height = 4)

