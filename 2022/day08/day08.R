# 0. Library and fonts management
library(tidyverse)
library(showtext)
library(glue)
library(lubridate)
library(polite)
library(rvest)
library(geomtextpath)
library(colorspace)
library(ggtext)
library(ggforce)

## Adding Google Fonts
sysfonts::font_add_google(name = "VT323", family = "VT323")
mono <- "VT323"

## Allows the use of the downloaded Google Font
## To see the results updated, it's needed to call windows() or save the image
showtext::showtext_opts(dpi = 320)
showtext::showtext_auto()

## Defines some layout constants
lnhgt <- 4
base_clr <- "#A4876F" #765A40
clr_sky <- c(
  colorspace::lighten(base_clr, c(0.9,0.8,0.7,0.5,0.3,0.1)),
  colorspace::darken("#22DAE0", c(0.5,0.7,0.9))
  )

# 1. Data download, load and handling
## !!!!!!!!!!!!!!! ATENTION !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
## If you don't want to scrap the data again, load the RDS (skip lines 32-71)
df <- readRDS("2022/day08/scrap.rds")

## Connects to the w3counter website and announces our presence
session <- polite::bow("https://www.w3counter.com/globalstats.php")

## Creates a tibble to hold the data to be scrapped
df <- tidyr::expand_grid(
  year = 2007:2022,
  month = 1:12
) %>% 
  dplyr::group_by(year, month) %>% 
  tidyr::nest()

## Creates a function to query and scrape data (top browsers and values)
## for each pair month/year from the website
scrapper <- function(data, year, month) {
  
  result = session %>% 
    polite::scrape(query = list(year = year, month = month))
  
  browsers = result %>% 
    rvest::html_elements(".lab") %>% 
    rvest::html_text()
  
  values = result %>% 
    rvest::html_elements(".value") %>% 
    rvest::html_text()
  
  add = tibble(
    browsers = browsers,
    values = values
  )
  
}

## Applies the function and unrolls the data and converts the values to numeric
df <- df %>% 
  dplyr::mutate(data = purrr::pmap(list(data, year, month), scrapper)) %>%
  tidyr::unnest(data) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(values = stringr::str_remove(values, "%"),
                values = as.numeric(values))

## Saves the data in a rds file for posterity
saveRDS(df, "2022/day08/scrap.rds")

## Gets the mean share of each browser in the whole series.
## Uses it to define the order of placement of the "mountains"
mean_order <- df %>% 
  dplyr::group_by(browsers) %>% 
  dplyr::summarise(mean = mean(values)) %>% 
  dplyr::arrange(desc(mean)) %>% 
  dplyr::pull(browsers)

## Define the colors of the "mountains"
mount_clr <- tibble(
  browsers = mean_order,
  color = colorspace::darken(base_clr, amount = seq(0.1, 0.95, length.out = 5))
)
df_final <- df %>% 
  dplyr::left_join(mount_clr)

## Applies the order to the browsers and creates a date using year and month
df_final <- df_final %>%
  dplyr::mutate(browsers = factor(browsers, levels = mean_order),
                date = lubridate::ym(glue::glue("{year}-{month}")))

## Smooth the time series using lowess
df_final <- df_final %>% 
  dplyr::group_by(browsers) %>% 
  dplyr::mutate(values = stats::lowess(date, values, f = 1/20, iter = 4)$y) %>% 
  dplyr::ungroup()

## Creates a function to filter specific rows for each group in a tibble
selector <- function(data, id) {
  
  id = id %>% dplyr::pull(rows)
  
  data %>% 
    tibble::rownames_to_column() %>% 
    dplyr::mutate(rowname = as.numeric(rowname)) %>%
    dplyr::filter(rowname %in% id)
  
}

## Defines some subsets of the data to use in the label of the "mountains"
labels <- tibble(
  browsers = sort(rep(mean_order, 50)),
  rows = c(
    67:136,
    20:59,
    21:60,
    127:176,
    125:174
  )
) %>%
  dplyr::group_by(browsers) %>% 
  tidyr::nest(id = rows)
labels <- df_final %>% 
  dplyr::group_by(browsers) %>% 
  tidyr::nest() %>% 
  dplyr::left_join(labels) %>% 
  dplyr::mutate(data = purrr::map2(data, id, selector)) %>% 
  tidyr::unnest(data) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(date, values, browsers, color)

## Define coordinates for the "sky" gradient
gradient <- tibble(
  x = mean(df_final$date),
  y = seq(20,100,10),
  color = clr_sky
)

## Creates the title of the plot
title <- c("
<span style='font-size:120px;'>LANDSCAPE OF THE BROWSER WARS</span><br>
<span style='font-size:45px;'>BROWSERS MARKET SHARE SMOOTHED WITH LOWESS</span><br>
<span style='font-size:30px;'>DATA FROM: W3COUNTER | GRAPHIC BY: ÍCARO BERNARDES (@IcaroBSC)</span>
")

# 2. Generates the plot
## Creates the main plot
p <- df_final %>% 
  ggplot() + 
  
  ### Places the background "sky"
  ggforce:: geom_link2(aes(x = x, y = y, color = I(color), group = 1),
                       size = 700, n = 400, data = gradient) +
  
  ### Places the mountains
  geom_area(aes(x = date, y = values, fill = I(color), group = browsers),
            position = "identity", color = NA) +
  
  ### Places the curved labels
  geomtextpath::geom_textpath(aes(x = date, y = values+2.5, label = browsers,
                                  color = I(colorspace::darken(color, 0.5))),
                              text_only = TRUE, text_smoothing  = 98, hjust = 0,
                              size = 14, family = mono, data = labels) +
  
  ### Eliminates the extra space of axes expansion
  coord_cartesian(expand = FALSE) +
  
  ### Places the title
  labs(caption = title) +
  
  ### Customizes plot theme elements
  theme(
    text = element_text(family = mono),
    plot.background = element_rect(fill = "black", color = NA),
    plot.margin = margin(t = 50, r = 150, b = 50, l = 150, unit = "pt"),
    plot.caption = ggtext::element_markdown(
      hjust = 0.5, color = "white", lineheight = lnhgt,
      margin = margin(t = 40, r = 0, b = 0, l = 0, unit = "pt")),
    axis.text.y = element_blank(),
    axis.text.x = element_text(
      size = 40, color = colorspace::lighten(base_clr, 0.7),
      margin = margin(t = 30, r = 0, b = 0, l = 0, unit = "pt")),
    axis.ticks = element_blank()
  )

## Saves the plot
ggsave("2022/day08/day08.png", plot = p, dpi = "retina",
       width = 24, height = 16)

