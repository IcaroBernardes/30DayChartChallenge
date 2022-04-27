# 0. Library and fonts management
library(tidyverse)
library(ragg)
library(glue)
library(epoxy)
library(ggtext)
library(geomtextpath)
library(sf)
library(rnaturalearthhires)
library(rnaturalearth)
library(rmapshaper)
library(colorspace)
library(scales)
library(ggfx)
library(cowplot)

## Defines fonts to use (installed on the machine)
serif <- "Bitter"
sans <- "Mulish"

## Defines some layout constants
lnhgt <- 1 ### To set the lineheight
x0 <- c(0.25,0.75) ### x-axis coordinates for the maps and bars
y0 <- 0.23 ### y-axis coordinates for the maps and bars
dims_bars <- 0.9 ### Width and height of the bar plots
dims_maps <- 0.12 ### Width and height of the bar plots

# 1. Data download, load and handling
rawdata <- readr::read_csv2("2022/day18/data.csv")

## Gathers the percentages in one variable, eliminates lines with missing data
## and takes negative values and brings them to zero
df <- rawdata %>% 
  na.exclude() %>% 
  tidyr::pivot_longer(cols = -Country,
                      names_to = "Category",
                      values_to = "Percentage") %>% 
  dplyr::mutate(Percentage = ifelse(Percentage < 0, 0, Percentage))

## Keeps some of the categories
categs <- c("Taxes on goods and services",
            "Taxes on income, profits and capital gains")
df <- df %>% dplyr::filter(Category %in% categs)

## Orders the data for the facets
df <- df %>% 
  dplyr::group_by(Category) %>% 
  dplyr::arrange(Category, desc(Percentage), Country) %>% 
  dplyr::mutate(Rank = row_number()) %>% 
  dplyr::ungroup()

## Defines the color palettes for each fuel type
palette <- tibble(
  Category = unique(df$Category),
  base_clr = c("#4087B3","#FF1200"),
) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(
    fun_clr = c(grDevices::colorRamp(c(colorspace::lighten(base_clr, 0.3),
                                       base_clr,
                                       colorspace::darken(base_clr, 0.3))))
  ) %>% 
  dplyr::ungroup()

## Joins the functions to the data
df <- df %>% 
  dplyr::left_join(palette)

## Creates labels for the categories
ctg <- palette %>% 
  dplyr::select(Category, base_clr) %>% 
  dplyr::mutate(
    x = 45,
    y = -3,
    label = stringr::str_wrap(Category, width = 17)
  )

## Creates a variable to scale the percentage
## between 0 and 1 inside each category
df <- df %>% 
  dplyr::group_by(Category) %>% 
  dplyr::mutate(scaled = scales::rescale(Percentage)) %>% 
  dplyr::ungroup() 

## Defines the fill colors for each percentage
df <- df %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(
    color = grDevices::rgb(do.call(unlist(fun_clr), list(scaled)), maxColorValue = 255)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-base_clr, -fun_clr, -scaled)

## Creates labels for the countries
styled <- "<span style ='font-size:10pt'>"
df <- df %>% 
  dplyr::mutate(label = ifelse(
    Rank > 26,
    glue::glue("{Country} | {styled}{format(Percentage, nsmall = 1)}%</span>"),
    glue::glue("{styled}{format(Percentage, nsmall = 1)}%</span> | {Country}")
  ))

## Indicates which category is bigger for each country
df <- df %>% 
  dplyr::group_by(Country) %>% 
  dplyr::arrange(Country, Category) %>% 
  dplyr::mutate(Bigger = which.max(Percentage),
                Rank_big = Rank[which.max(Percentage)]) %>% 
  dplyr::ungroup()

## Gets the shapes of the countries with the highest pct for each category
top_countries <- df %>%
  dplyr::filter(Rank == 1) %>% 
  dplyr::mutate(geounit = ifelse(Country == "Czech Republic", "Czechia", Country))
top_countries <- rnaturalearth::ne_countries(
  returnclass ="sf",
  scale = "large",
  country = top_countries$geounit
) %>%
  dplyr::right_join(top_countries, by = c("name_long" = "Country")) %>% 
  dplyr::select(name_long, Category, Percentage, color, geometry) %>% 
  dplyr::arrange(Category)

## Simplifies the shapes (greatly reduces object memory size)
geom <- sf::st_geometry(top_countries) %>% 
  rmapshaper::ms_simplify(keep = 0.1)
sf::st_geometry(top_countries) <- geom

## Defines coordinates for the titles
cnt1 <- glue::glue("<span style='color:{palette$base_clr[2]};'>{c('the US','Canada','Switzerland')}</span>")
cnt1 <- glue::glue("{cnt1&}", .transformer = epoxy::epoxy_style("collapse", syntax = "html"))
cnt2 <- glue::glue("<span style='color:{palette$base_clr[1]};'>{c('Latvia','Hungary','Estonia')}</span>")
cnt2 <- glue::glue("{cnt2&}", .transformer = epoxy::epoxy_style("collapse", syntax = "html"))
titles <- tibble(
  x = 0.07,
  y = c(0.95, 0.82, 0.6),
  size = c(70, 12.5, 10),
  label = c(
    "Taxing Conditions",
    
    glue::glue(
      "Standardization and cooperation are some of the key principles that guide the<br>
      Organisation for Economic Co-operation and Development. Tax issues are one of<br>
      the biggest concerns of the OECD. The bars bellow show how much the two main<br>
      sources of taxes represent in the total of taxes collected by each country.<br>
      For each country the category with the highest percentage also have in<br>
      their bar a line that shows the percentage of the smaller category.<br><br>
      Countries like {cnt1} rely more in taxing gains,<br>
      while countries like {cnt2} tax consume more."),
    
    'Data: OECD (2022), "Revenue Statistics: Comparative tables", OECD Tax Statistics (database)
    <br>Graphic by: Ícaro Bernardes (@IcaroBSC)'
  )
)

# 2. Generates the plot
## Creates the maps in separated objects
lims <- list(
  c(-78,-65),
  c(8,13)
)
for (i in 1:dim(top_countries)[1]) {
  
  aux <- top_countries %>%
    dplyr::slice(i) %>% 
    ggplot() +
    geom_sf(aes(fill = I(color)), color = NA) +
    theme_void() +
    coord_sf(xlim = lims[[i]])
  
  assign(glue::glue("map{i}"), aux)
  
}

## Creates the bars in separated objects
for (i in 1:n_distinct(df$Category)) {
  
  aux <- df %>% 
    dplyr::filter(Category == categs[i]) %>% 
    ggplot() +
    
    ### Places a line and point to link the top country with its shape 
    annotate("segment", x = 1, xend = 1, y = 0, yend = -6,
             size = 1.2, linetype = "dotted") +
    annotate("point", x = 1, y = -6, size = 2.5) +
    
    ### Creates a reference layer with the bars
    ggfx::as_reference(
      geom_col(aes(x = Rank, y = Percentage, fill = I(color))),
      id = "cols"
    ) +
    
    ### Places text that show the percentages and
    ### makes it transparent in the regions where the bars are
    ggfx::with_blend(
      x = geom_textpath(aes(x = Rank, y = 3, label = label),
                        angle = 90, hjust = 0, family = serif,
                        size = 6, rich = TRUE),
      bg_layer = "cols",
      blend_type = "xor"
    ) +
    
    ### Places tiles to highlight the values of bigger taxes shares
    geom_tile(aes(x = Rank_big, y = Percentage, fill = I(color)),
              width = 0.9, height = 0.3, alpha = 0.7,
              data = df %>% dplyr::filter(Category != categs[i], Bigger == i)) +
    
    ### Places the titles of the categories
    geom_text(aes(x = x, y = y, label = label, color = I(base_clr)),
              size = 16, family = sans, hjust = 1, vjust = 0,
              lineheight = lnhgt, fontface = "bold",
              data = ctg %>% dplyr::filter(Category == categs[i])) +
    
    ### Defines limits for the plot
    scale_x_continuous(limits = c(0,51), expand = expansion(0,0)) +
    scale_y_continuous(limits = c(-20,65), expand = expansion(0,0)) +
    
    ### Makes the coordinate system as circular
    coord_polar() +
    
    ### Eliminates and customizes plot elements
    theme_void() +
    theme(
      strip.text = element_blank(),
      panel.spacing = unit(0, "pt")
    )
  
  assign(glue::glue("bar{i}"), aux)
  
}

## Creates the main plot
p <- cowplot::ggdraw() +
  
  ### Places the maps and bars
  cowplot::draw_plot(bar1,
                     x = x0[1], y = y0,
                     width = dims_bars, height = dims_bars,
                     hjust = 0.5, vjust = 0.5) +
  cowplot::draw_plot(map1,
                     x = x0[1], y = y0,
                     width = dims_maps, height = dims_maps,
                     hjust = 0.5, vjust = 0.5) +
  cowplot::draw_plot(bar2,
                     x = x0[2], y = y0,
                     width = dims_bars, height = dims_bars,
                     hjust = 0.5, vjust = 0.5) +
  cowplot::draw_plot(map2,
                     x = x0[2], y = y0,
                     width = dims_maps, height = dims_maps,
                     hjust = 0.5, vjust = 0.5) +
  
  ### Places th titles
  ggtext::geom_richtext(aes(x = x, y = y, label = label, size = I(size)),
                        family = sans, vjust = 1, hjust = 0, lineheight = lnhgt,
                        label.color = NA, fill = NA, data = titles)

## Saves the plot
ggsave("2022/day18/day18.png", plot = p, dpi = "retina", bg = "#eef2e6",
       width = 26, height = 23, device = ragg::agg_png, res = 320)


