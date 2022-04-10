# 0. Library and fonts management
library(tidyverse)
library(showtext)
library(readxl)
library(glue)
library(patchwork)
library(tidygeocoder)
library(countrycode)
library(osrm)
library(ggtext)
library(ggbeeswarm)
library(geomtextpath)
library(cols4all)

## Adding Google Fonts
sysfonts::font_add_google(name = "Forum", family = "Forum")
font <- "Forum"

## Allows the use of the downloaded Google Font
## To see the results updated, it's needed to call windows() or save the image
showtext::showtext_opts(dpi = 320)
showtext::showtext_auto()

## Defines some layout constants
lnhgt <- 0.9

# 1. Data download, load and handling
## Loads the data and gets the region of the countries as defined
## for the World Bank Development Indicators (legacy division)
rawdata <- readxl::read_excel("2022/day07/data.xlsx") %>% 
  dplyr::mutate(region = countrycode::countrycode(country,
                                                  origin = "country.name",
                                                  destination = "region23"))

## Highlights the Italian cities in the region variable and
## converts it to a factor
coord <- rawdata %>% 
  dplyr::mutate(region = ifelse(country == "Italy", "Italy", region),
                region = stringr::str_replace(region, " ", "<br>"),
                region = factor(region, levels = c("Eastern<br>Europe",
                                                   "Northern<br>Europe",
                                                   "Southern<br>Europe",
                                                   "Western<br>Europe",
                                                   "Italy")))

## Gets the geographical coordinates using OSM
coord <- coord %>% 
  tidygeocoder::geocode(address = city, method = "osm", limit = 1)

## Eliminates cities that weren't found in the geocoding process
coord <- coord %>% na.exclude()

## Separates Rome from the other cities
rome <- coord %>% dplyr::filter(city == "ROMA")
coord <- coord %>% dplyr::filter(city != "ROMA")

## Creates an id variable
rome <- rome %>% dplyr::mutate(id = 0)
coord <- coord %>% dplyr::mutate(id = 1:n())

## Gets the travel time (in minutes) from the cities to Rome by car
## using the OSM API. Made in batches in order to not overwhelm the server.
time <- NULL
for (i in 1:5) {
  
  start <- (1+(i-1)*100)
  end <- i*100
  
  lista <- osrm::osrmTable(src = coord %>% dplyr::select(id, long, lat) %>% dplyr::slice(start:end),
                           dst = rome %>% dplyr::select(id, long, lat),
                           osrm.profile = "car")
  
  time <- rbind(time, lista$durations)
  
  Sys.sleep(1)
  
}

## Joins the data and eliminates cities without time
df <- coord %>%
  dplyr::mutate(time = time) %>%
  na.exclude()

## Defines the color scale of the regions
clr <- cols4all::c4a(palette = "misc.r4", n = 5)

## Defines the coordinates for the titles
titles <- tibble(
  x = 0.05,
  y = c(0.95,0.75,0.26,0.1),
  size = c(46,160,14.1,14.3),
  label = c(
    "ALL ROADS LEAD TO",
    "ROME",
    "Distribution of the travel times (in minutes) by car to Rome from some\nof the top 500 biggest european cities in the start of the XXI Century.",
    "Data from: citymayors.com | Graphic by: Ícaro Bernardes (@IcaroBSC)"
  )
)

# 2. Generates the plot
## Creates the swarmplot
swarm <- df %>% 
  ggplot() +
  
  ### Places the swarm
  ggbeeswarm::geom_beeswarm(aes(x = region, y = time, color = region)) +
  
  ### Highlights a distant Italian city
  annotate("point", x = "Italy", y = 1290, size = 5, shape = 1, stroke = 1) +
  geomtextpath::geom_textcurve(aes(x = 3.7, xend = 5, y = 3700, yend = 1400),
                               curvature = -0.4, hjust = 0,
                               label = "Cagliari is an unusually distant Italian city") +
  
  ### Defines a log scale for the time
  scale_y_log10(labels = ~glue::glue("{.}<br><span style='font-size:20px;'>minutes</span>")) +
  
  ### Applies the colors to the points
  scale_color_discrete(type = clr, guide = "none") +
  
  ### Eliminates and customizes plot elements
  theme_void() +
  theme(
    text = element_text(family = font),
    axis.text = ggtext::element_markdown(size = 20)
  )

## Creates the main plot
p <- ggplot(titles) +
  
  ### Places the titles
  geom_text(aes(x = x, y = y, label = label, size = I(size)),
            hjust = 0, vjust = 1, family = font, lineheight = lnhgt) +
  
  ### Defines limits for the plot
  coord_cartesian(xlim = c(0,1), ylim = c(0,1), expand = FALSE) +
  
  ### Eliminates and customizes plot elements
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#f0e1cc", color = NA)
  ) +
  
  ### Places the swarm
  patchwork::inset_element(swarm, top = 0.95, bottom = 0.05,
                           left = 0.65, right = 0.95)

## Saves the plot
ggsave("2022/day07/day07.png", plot = p, dpi = "retina",
       width = 29, height = 10)

