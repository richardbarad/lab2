
library(tidycensus)
library(tidyverse)
library(kableExtra)
library(sf)

census_api_key('4b917a3ebce557bc497aebb5aba1b04f0ff9c5ba',overwrite = FALSE)

variables2017 <- load_variables(2017,'acs5')
variables2009 <- load_variables(2009,'acs5')

variables_join <- inner_join(variables2017,variables2009, by='name','suffix'=c('_2017','_2009')) %>%
  dplyr::filter(geography == 'tract')

acs_variables <- c(
                   'B25119_001', #Median Household Income
                   'B25058_001', #Rent
                   'B08130_001', #Total Working Population
                   'B08130_016', #Total Population Commuting on Public Transit
                   'B17009_002', #Total Population with Income Below Poverty Level
                   'B17009_001') #Total Population


options(scipen=999) #
options(tigris_class = "sf")

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

palette5 <- c("#f0f9e8","#bae4bc","#7bccc4","#43a2ca","#0868ac")


dc2017tracts <- get_acs('tract',
                        variables = acs_variables,
                        year = 2017,
                        state = 'DC',
                        geometry = TRUE,
                        output = 'wide') %>%
  rename('median_income'='B25119_001E',
         'median_rent'='B25058_001E',
         'working_pop' = 'B08130_001E',
         'transit_pop' = 'B08130_016E',
         'poverty_pop' = 'B17009_002E',
         'total_pop' = 'B17009_001E',) %>%
  st_transform('EPSG:2248') %>%
  mutate(pct_transit = (transit_pop / working_pop) * 100,
         pct_poverty = (poverty_pop / total_pop) * 100,
         year = '2017') %>%
  dplyr::select(-NAME, -starts_with("B0"), -starts_with("B1"), -starts_with("B2"))

dc2009tracts <- get_acs('tract',
                        variables = acs_variables,
                        year = 2009,
                        state = 'DC',
                        geometry = TRUE,
                        output = 'wide') %>%
  rename('median_income'='B25119_001E',
         'median_rent'='B25058_001E',
         'working_pop' = 'B08130_001E',
         'transit_pop' = 'B08130_016E',
         'poverty_pop' = 'B17009_002E',
         'total_pop' = 'B17009_001E',) %>%
  st_transform('EPSG:2248') %>%
  mutate(pct_transit = (transit_pop / working_pop) * 100,
         pct_poverty = (poverty_pop / total_pop) * 100,
         year = '2009') %>%
  dplyr::select(-NAME, -starts_with("B0"), -starts_with("B1"), -starts_with("B2"))

census_bind = rbind(dc2009tracts, dc2017tracts)

dc_boundary <- st_union(dc2017tracts)

dcmetro <- st_read('https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/Transportation_Rail_Bus_WebMercator/MapServer/51/query?outFields=*&where=1%3D1&f=geojson') %>%
  st_transform('EPSG:2248')
dcmetro <- dcmetro[st_intersects(dcmetro, dc_boundary) %>% lengths > 0, ]

ggplot()+
  geom_sf(data = census_bind, aes(fill = q5(median_income)))+
  geom_sf(data = dcmetro)+
  facet_wrap(~year)+
  scale_fill_manual(values = palette5,
                    name = "Median Income\n(Quintile Breaks)")+
  mapTheme()