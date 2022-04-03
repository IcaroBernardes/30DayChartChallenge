# 0. Library and fonts management
library(tidyverse)
library(patchwork)
library(ggwaffle)
library(ragg)

## Defines some layout constants
lnhgt <- 1.1 ### To set the lineheight

# 1. Pictograms making
## First scale
### Defines percentages of the categories
Vpct1 <- c(Land = 29, Ocean = 71)

### Defines the icons for the categories
df1 <- tibble(
  icon = c(rep("🌍",Vpct1[1]),rep("🌊",Vpct1[2])),
  order = c(rep(1,Vpct1[1]),rep(2,Vpct1[2]))
)

### Generates coordinates for the icons
waffle_data1 <- ggwaffle::waffle_iron(df1, aes_d(group = order), rows = 10) %>% 
  dplyr::left_join(df1, by = c("group" = "order"))

### Creates the pictogram
p1 <- ggplot(waffle_data1, aes(x, y)) + 
  geom_text(aes(label = icon), family = 'Apple Color Emoji', size = 3) +
  coord_equal(xlim = c(-1,10), ylim = c(-1,10)) + 
  theme_waffle() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

## Second scale
Vpct2 <- c(`Habitable land` = 71, Glaciers = 10, `Barren land` = 19)

### Defines the icons for the categories
df2 <- tibble(
  icon = c(rep("🗾",Vpct2[1]),rep("❄",Vpct2[2]),rep("🏜",Vpct2[3])),
  order = c(rep(1,Vpct2[1]),rep(2,Vpct2[2]),rep(3,Vpct2[3]))
)

### Generates coordinates for the icons
waffle_data2 <- ggwaffle::waffle_iron(df2, aes_d(group = order), rows = 10) %>% 
  dplyr::left_join(df2, by = c("group" = "order"))

### Creates the pictogram
p2 <- ggplot(waffle_data2, aes(x, y)) + 
  geom_text(aes(label = icon), family = 'Apple Color Emoji', size = 3) +
  coord_equal(xlim = c(-1,10), ylim = c(-1,10)) + 
  theme_waffle() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

## Third scale
Vpct3 <- c(Agriculture = 50, Forests = 37, Shrub = 11,
           `Urban and built-up land` = 1, Freshwater = 1)

### Defines the icons for the categories
df3 <- tibble(
  icon = c(rep("👩‍🌾",Vpct3[1]),rep("🌲",Vpct3[2]),rep("🌿",Vpct3[3]),
           rep("🏙",Vpct3[4]),rep("💧",Vpct3[5])),
  order = c(rep(1,Vpct3[1]),rep(2,Vpct3[2]),rep(3,Vpct3[3]),
            rep(4,Vpct3[4]),rep(5,Vpct3[5]))
)

### Generates coordinates for the icons
waffle_data3 <- ggwaffle::waffle_iron(df3, aes_d(group = order), rows = 10) %>% 
  dplyr::left_join(df3, by = c("group" = "order"))

### Creates the pictogram
p3 <- ggplot(waffle_data3, aes(x, y)) + 
  geom_text(aes(label = icon), family = 'Apple Color Emoji', size = 3) +
  coord_equal(xlim = c(-1,10), ylim = c(-1,10)) + 
  theme_waffle() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

## Fourth scale
Vpct4 <- c(`Livestock: meat and dairy` = 77, Crops = 23)

### Defines the icons for the categories
df4 <- tibble(
  icon = c(rep("🐮",Vpct4[1]),rep("🍉",Vpct4[2])),
  order = c(rep(1,Vpct4[1]),rep(2,Vpct4[2]))
)

### Generates coordinates for the icons
waffle_data4 <- ggwaffle::waffle_iron(df4, aes_d(group = order), rows = 10) %>% 
  dplyr::left_join(df4, by = c("group" = "order"))

### Creates the pictogram
p4 <- ggplot(waffle_data4, aes(x, y)) + 
  geom_text(aes(label = icon), family = 'Apple Color Emoji', size = 3) +
  coord_equal(xlim = c(-1,10), ylim = c(-1,10)) + 
  theme_waffle() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

##

legends <- tibble(
  x = 0.51,
  y = seq(0.7, 0.1, -0.2),
  label = c(
    "🌍 Land: 29% (149M km²)\n🌊 Ocean: 71% (361M km²)",
    
    "🗾 Habitable land: 71% (104M km²)\n❄ Glaciers: 10% (15M km²)\n🏜 Barren land: 10% (15M km²)",
    
    "👩‍🌾 Agriculture: 50% (51M km²)\n🌲 Forests: 37% (39M km²)\n🌿 Shrub: 11% (12M km²)\n🏙 Urban and built-up land: 1% (1.5M km²)\n💧 Freshwater: 1% (1.5M km²)",
    
    "🐮 Livestock - meat and dairy: 77% (40M km²)\n🍉 Crops: 23% (11M km²)"
    
  )
)

## Creates the main plot
p <- ggplot(NULL) +
  
  ### Places the legends
  geom_text(aes(x = x, y = y, label = label), lineheight = lnhgt,
            hjust = 0, size = 4, data = legends) +
  
  
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  
  theme_void() +
  theme(
    plot.background = element_rect(color = "black")
  ) +
  
  ### Places
  patchwork::inset_element(p1, left = 0, right = 0.5,
                           bottom = 0.6, top = 0.8,
                           on_top = FALSE) +
  patchwork::inset_element(p2, left = 0, right = 0.5,
                           bottom = 0.4, top = 0.6,
                           on_top = FALSE) +
  patchwork::inset_element(p3, left = 0, right = 0.5,
                           bottom = 0.2, top = 0.4,
                           on_top = FALSE) +
  patchwork::inset_element(p4, left = 0, right = 0.5,
                           bottom = 0, top = 0.2,
                           on_top = FALSE)

## Saves the plot
ggsave("2022/day02/day02.png", plot = p, dpi = "retina",
       width = 7, height = 12)

