
library(tidycensus)
library(tidyverse)
library(kableExtra)
library(sf)

census_api_key('4b917a3ebce557bc497aebb5aba1b04f0ff9c5ba',overwrite = FALSE)

variables2017 <- load_variables(2017,'acs5')
variables2009 <- load_variables(2009,'acs5')

acs_variables2017 <- c(
                   'B19013_001', #Median Household Income
                   'B25058_001', #Rent
                   'B27011_003', #Total Working Population - not in 2009
                   'B08126_014', #Number of People Working in Public Adminstration - not in 2009
                   'B08301_010', #Number of People Commuting in any form of public transit
                   'B25026_001') #Total Pop

acs_variables2009 <- c(
  'B19013_001', #Median Household Income
  'B25058_001', #Rent
  'B24080_001', #Total Working Population - not in 2009 #This data is at the block level in 2017, will tidycensus aggregate for us to tract?
  'C24030_028', #Male Population in Public Administration
  'B08301_010', #Number of People Commuting in any form of public transit
  'B25026_001', #Total Pop
  'C24030_055') #Female Population in Public Administration

dc2017tracts <- get_acs('tract',
                        variables = acs_variables2017,
                        year = 2017,
                        state = 'DC',
                        geometry = TRUE,
                        output = 'wide') %>%
  rename('Median_income'='B19013_001E',
         'Median_rent'='B25058_001E',
         'Working_Pop' = 'B27011_003E',
         'public_admin_pop' = 'B08126_014E',
         'commute_public_transit' = 'B08301_010E',
         'total_pop' = 'B25026_001E',) %>%
  st_transform('EPSG:2248') %>%
  dplyr::select(-NAME, -starts_with("B0"), -starts_with("B1"), -starts_with("B2"),-starts_with("C2"))

dc2009tracts <- get_acs('tract',
                        variables = acs_variables2009,
                        year = 2009,
                        state = 'DC',
                        geometry = TRUE,
                        output = 'wide') %>%
  rename('Median_income'='B19013_001E',
         'Median_rent'='B25058_001E',
         'Working_Pop' = 'B24080_001E',
         'male_public_admin_pop' = 'C24030_028E',
         'female_public_admin_pop' = 'C24030_055E',
         'commute_public_transit' = 'B08301_010E',
         'total_pop' = 'B25026_001E') %>%
  st_transform('EPSG:2248') %>%
  dplyr::select(-NAME, -starts_with("B0"), -starts_with("B1"), -starts_with("B2"),-starts_with("C2"))

dc_boundary <- st_union(dc2017tracts)

dcmetro <- st_read('https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/Transportation_Rail_Bus_WebMercator/MapServer/51/query?outFields=*&where=1%3D1&f=geojson') %>%
  st_transform('EPSG:2248')
dcmetro <- dcmetro[st_intersects(dcmetro, dc_boundary) %>% lengths > 0, ]

ggplot()+
  geom_sf(data = dc_boundary)+
  geom_sf(data = dcmetro)