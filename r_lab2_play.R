
library(tidycensus)
library(tidyverse)
library(kableExtra)
library(sf)

census_api_key('4b917a3ebce557bc497aebb5aba1b04f0ff9c5ba',overwrite = FALSE)

variables2017 <- load_variables(2017,'acs5')
variables2009 <- load_variables(2009,'acs5')

write.csv(variables,'variables.csv')

acs_variables <- c('B05003_001', #Total Population
                   'B05003B_001', #African American Pop
                   'B05003A_001', #White Population
                   'B05003C_001', #Alaskan and Native American
                   'B05003D_001', #Asian
                   'B05003E_001', #Native Hawian or Pacfic Islander
                   'B05003F_001', #Other
                   'B05003G_001', #Two Races
                   'B19013_001', #Median Income
                   'B25058_001') #Rent 

dc2017tracts <- get_acs('tract',
                        variables = acs_variables,
                        year = 2017,
                        state = 'DC',
                        geometry = TRUE,
                        output = 'wide') %>%
  rename('Total_pop' = 'B05003_001E',
         'Black_pop' = 'B05003B_001E',
         'White_pop' = 'B05003A_001E',
         'Alaskan_native_pop' = 'B05003C_001E',
         'Asian_pop'='B05003D_001E',
         'Native_hawian_pop'='B05003E_001E',
         'Other_pop'='B05003F_001E',
         'Two_races_pop'='B05003G_001E',
         'Median_income'='B19013_001E',
         'Median_rent'='B25058_001E') %>%
  st_transform('EPSG:2248') %>%
  dplyr::select(-NAME, -starts_with("B0"), -starts_with("B1")) %>%
  mutate(pct_white = (White_pop / Total_pop) * 100,
         pct_nonwhite = ((Black_pop + Alaskan_native_pop + Asian_pop + Native_hawian_pop + Other_pop + Two_races_pop) / Total_pop) * 100)

dc2009tracts <- get_acs('tract',
                        variables = acs_variables,
                        year = 2009,
                        state = 'DC',
                        geometry = TRUE,
                        output = 'wide') %>%
  rename('Total_pop' = 'B05003_001E',
         'Black_pop' = 'B05003B_001E',
         'White_pop' = 'B05003A_001E',
         'Alaskan_native_pop' = 'B05003C_001E',
         'Asian_pop'='B05003D_001E',
         'Native_hawian_pop'='B05003E_001E',
         'Other_pop'='B05003F_001E',
         'Two_races_pop'='B05003G_001E',
         'Median_income'='B19013_001E',
         'Median_rent'='B25058_001E') %>%
  st_transform('EPSG:2248') %>%
  dplyr::select(-NAME, -starts_with("B0"), -starts_with("B1")) %>%
  mutate(pct_white = (White_pop / Total_pop) * 100,
         pct_nonwhite = ((Black_pop + Alaskan_native_pop + Asian_pop + Native_hawian_pop + Other_pop + Two_races_pop) / Total_pop) * 100)

dc_boundary <- st_union(dc2017tracts)

dcmetro <- st_read('https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/Transportation_Rail_Bus_WebMercator/MapServer/51/query?outFields=*&where=1%3D1&f=geojson') %>%
  st_transform('EPSG:2248')
dcmetro <- dcmetro[st_intersects(dcmetro, dc_boundary) %>% lengths > 0, ]

ggplot()+
  geom_sf(data = dc_boundary)+
  geom_sf(data = dcmetro)