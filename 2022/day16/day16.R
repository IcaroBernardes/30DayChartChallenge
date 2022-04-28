# 0. Library and fonts management
library(tidyverse)
library(glue)
library(gt)
library(gtExtras)
library(magick)

## Defines some layout constants
H_earth <- 40 ### Height of Earth icon on the table

# 1. Data download, load and handling
## Gets the most recent Human Development Index data 
hdi <- readr::read_csv("2022/day16/hdi.csv") %>% 
  dplyr::filter(Year == 2017) %>% 
  dplyr::rename("Country" = "Entity",
                "HDI" = "Human Development Index (UNDP)") %>% 
  dplyr::select(Country, HDI)

## Gets the data on consume of the Earth
consume <- readr::read_csv2("2022/day16/consume.csv")

## Joins the data
df <- consume %>% 
  dplyr::select(Country, req_earths) %>% 
  dplyr::left_join(hdi)

## Gets the countries which have not been matched
miss_consume <- df %>% dplyr::filter(is.na(HDI)) %>% dplyr::select(-HDI)
miss_hdi <- hdi %>% dplyr::filter(!(Country %in% df$Country))

## Join some of the previously not matched countries
miss_hdi$Country[c(2,4,5,8,10:12,17,18,20,21,25:29,32,33)] <- c(
  "Brunei Darussalam","Czech Republic","Congo, Democratic Republic of",
  "Iran, Islamic Republic of","Korea, Democratic People's Republic of",
  "Lao People's Democratic Republic","Libyan Arab Jamahiriya",
  "Republic of Moldova","Republic of North Macedonia",
  "State of Palestine","Russian Federation","Korea, Republic of",
  "Syrian Arab Republic","Tanzania, United Republic of","Timor-Leste",
  "United States of America","Venezuela, Bolivarian Republic of","Viet Nam"
)
miss_df <- miss_consume %>% 
  dplyr::left_join(miss_hdi) %>% 
  stats::na.exclude()
df <- df %>% 
  stats::na.exclude() %>% 
  rbind(miss_df)

## Converts the number of required Earth to numeric
df <- df %>% dplyr::mutate(req_earths = as.numeric(req_earths))

## Selects some countries for the table and renames some
cnt <- c("Australia","Brazil","China","France","Germany","India","Italy",
         "Japan","Portugal","Russian Federation","Spain","Switzerland",
         "United Kingdom","United States of America")
df <- df %>% 
  dplyr::filter(Country %in% cnt) %>% 
  dplyr::mutate(Country = case_when(Country == "Russian Federation" ~ "Russia",
                                    Country == "United Kingdom" ~ "U.K.",
                                    Country == "United States of America" ~ "U.S.A.",
                                    TRUE ~ Country)) %>% 
  dplyr::arrange(desc(req_earths))

## Adds data on global consumption
df <- df %>% 
  tibble::add_row(Country = "World", req_earths = 1.8)

## Loads the Earth icon
earth <- magick::image_read("2022/day16/icons/earth_1.0.png")

## Loads a white circle for support
circle <- magick::image_read("2022/day16/icons/circle.png") %>% 
  magick::image_trim() %>% 
  magick::image_resize("120")

## Creates versions of the icon that correspond to fractions of the Earth
for (i in seq(0.1, 0.9, 0.1)) {
  
  ### Calculates the crop of the Earth
  crop <- 120*i
  geom <- glue::glue("120x119-{crop}")
  
  ### Crops the Earth
  earth_aux <- magick::image_crop(earth, geom) 
  
  ### Reestablishes the size of the icon
  aux <- magick::image_composite(circle, earth_aux, gravity = "west")
  
  ### Save the icon
  image_write(image = aux,
              path = glue::glue("2022/day16/icons/earth_{1-i}.png"))
}

## Associates Earth icons to each country
df <- df %>% 
  dplyr::mutate(
    E1 = case_when(
      floor(req_earths) >= 1 ~ glue::glue("2022/day16/icons/earth_1.0.png"),
      1-req_earths < 1 ~ glue::glue("2022/day16/icons/earth_{req_earths}.png"),
      TRUE ~ glue::glue("2022/day16/icons/circle.png")
    ),
    E2 = case_when(
      floor(req_earths) >= 2 ~ glue::glue("2022/day16/icons/earth_1.0.png"),
      2-req_earths < 1 ~ glue::glue("2022/day16/icons/earth_{round(req_earths-1, digits = 1)}.png"),
      TRUE ~ glue::glue("2022/day16/icons/circle.png")
    ),
    E3 = case_when(
      floor(req_earths) >= 3 ~ glue::glue("2022/day16/icons/earth_1.0.png"),
      3-req_earths < 1 ~ glue::glue("2022/day16/icons/earth_{round(req_earths-2, digits = 1)}.png"),
      TRUE ~ glue::glue("2022/day16/icons/circle.png")
    ),
    E4 = case_when(
      floor(req_earths) >= 4 ~ glue::glue("2022/day16/icons/earth_1.0.png"),
      4-req_earths < 1 ~ glue::glue("2022/day16/icons/earth_{round(req_earths-3, digits = 1)}.png"),
      TRUE ~ glue::glue("2022/day16/icons/circle.png")
    ),
    E5 = case_when(
      floor(req_earths) >= 5 ~ glue::glue("2022/day16/icons/earth_1.0.png"),
      5-req_earths < 1 ~ glue::glue("2022/day16/icons/earth_{round(req_earths-4, digits = 1)}.png"),
      TRUE ~ glue::glue("2022/day16/icons/circle.png")
    ),
    E6 = case_when(
      floor(req_earths) >= 6 ~ glue::glue("2022/day16/icons/earth_1.0.png"),
      6-req_earths < 1 ~ glue::glue("2022/day16/icons/earth_{round(req_earths-5, digits = 1)}.png"),
      TRUE ~ glue::glue("2022/day16/icons/circle.png")
    )
  )

## Associates flags to each country
df <- df %>% 
  dplyr::mutate(flag = tolower(Country),
                flag = stringr::str_remove_all(flag, "[:punct:]"),
                flag = glue::glue("2022/day16/icons/{flag}.png"))

## Reorders the data
df <- df %>% 
  dplyr::select(flag, Country, HDI, req_earths, E1:E6)

## Makes and saves the table
df %>% 
  gt::gt() %>% 
  
  ### Places the title and subtitle
  gt::tab_header(
    title = gt::html("<div style='margin-top:15px;'>How many Earths would we need</div>"),
    subtitle = gt::html("if everyone lived like U.S.A. residents?")
  ) %>% 
  
  ### Places the source notes
  gt::tab_source_note(
    source_note = gt::html("<div style='margin-top:10px;'>Required Earths data (2019): National Footprint and Biocapacity Accounts 2022<br>HDI data (2017): United Nations Development Programme<br>Table reproduction by: Ícaro Bernardes</div>")
  ) %>% 
  
  ### Places the flags
  gtExtras::gt_img_rows(columns = flag, img_source = "local", height = 30) %>% 
  
  ### Places the Earths
  gtExtras::gt_img_rows(columns = E1, img_source = "local", height = H_earth) %>% 
  gtExtras::gt_img_rows(columns = E2, img_source = "local", height = H_earth) %>% 
  gtExtras::gt_img_rows(columns = E3, img_source = "local", height = H_earth) %>% 
  gtExtras::gt_img_rows(columns = E4, img_source = "local", height = H_earth) %>% 
  gtExtras::gt_img_rows(columns = E5, img_source = "local", height = H_earth) %>% 
  gtExtras::gt_img_rows(columns = E6, img_source = "local", height = H_earth) %>% 
  
  ### Divides the table in two groups and orders them
  gt::tab_row_group(
    label = gt::html("<hr style='border:0.5px solid #025091;'>"),
    rows = 1:14,
    id = "countries"
  ) %>%
  gt::tab_row_group(
    label = gt::html("<hr style='border:0.5px solid #025091;width:95%;'>"),
    rows = 15,
    id = "world"
  ) %>%
  gt::row_group_order(
    groups = c("countries","world")
  ) %>% 
  
  ### Colors the HDI column
  gtExtras::gt_color_rows(
    columns = "HDI",
    palette = "ggsci::blue_material",
    type = "continuous"
  ) %>% 
  
  ### Formats the text of missing value in the HDI column
  gt::fmt_missing(
    columns = "HDI",
    missing_text = "---"
  ) %>% 
  
  ### Defines width of the HDI and required Earths columns
  gt::cols_width(
    flag ~ px(45),
    Country ~ px(170),
    HDI ~ px(70),
    req_earths ~ px(70),
    num_range("E",1:6) ~ px(1.5*H_earth)
  ) %>% 
  
  ### Styles font alignment in the header and body
  gt::tab_style(
    style = gt::cell_text(align = "left"),
    locations = list(
      gt::cells_body(),
      gt::cells_title()
    )
  ) %>% 
  ### Styles weight of title
  gt::tab_style(
    style = gt::cell_text(weight = "bolder"),
    locations = gt::cells_title(groups = "title")
  ) %>%
  ### Styles borders of the cells of the body of the table
  gt::tab_style(
    style = gt::cell_borders(weight = px(0)),
    locations = gt::cells_body()
  ) %>% 
  ### Styles size of the names of the countries
  gt::tab_style(
    style = gt::cell_text(size = px(26)),
    locations = gt::cells_body(columns = "Country")
  ) %>% 
  ### Styles size and font alignment  of the values of required Earths and HDI
  gt::tab_style(
    style = gt::cell_text(size = px(19), align = "center"),
    locations = gt::cells_body(columns = c("HDI","req_earths"))
  ) %>% 
  ### Styles borders of the cells of the top of the groups
  gt::tab_style(
    style = gt::cell_borders(weight = px(0)),
    locations = gt::cells_row_groups()
  ) %>% 
  ### Styles the color of the source notes
  gt::tab_style(
    style = gt::cell_text(color = "#333333"),
    locations = gt::cells_source_notes()
  ) %>% 
  ### Styles the table further
  gt::tab_options(
    table.font.names = "Century Gothic",
    table.font.color = "#025091",
    table.border.top.width = 0,
    table.border.bottom.width = 0,
    
    table_body.border.bottom.width = 0,
    
    heading.border.bottom.color = "white",
    heading.title.font.size = 32,
    heading.subtitle.font.size = 26,

    column_labels.hidden = TRUE,
    
    source_notes.font.size = 15
    ) %>% 
  
  ### Saves the table as image
  gt::gtsave("2022/day16/day16.png")
