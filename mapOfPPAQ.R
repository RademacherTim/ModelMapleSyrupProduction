#===============================================================================
# Script to make a map of all producers in the database
#-------------------------------------------------------------------------------

# load dependencies
#-------------------------------------------------------------------------------
if (!existsFunction ('%>%')) library ('tidyverse')
if (!existsFunction ('ggplot')) library ('ggplot2')
if (!exists ('worldHiresMapEnv')) library ('mapdata')
if (!existsFunction ('st_read')) library ('sf')
if (!existsFunction ('readTIFF')) library ('tiff')
if (!exists ('production')) source ('matchClimAndProdData.R') 

# get meteorological station locations
# downloaded from https://open.canada.ca/data/en/dataset/9764d6c6-3044-450c-ac5a-383cedbfef17
#-------------------------------------------------------------------------------
metStations <- read_csv ("../data/swob-xml_station_list.csv", 
                         col_types = cols ()) # TR - Change this for the actual locations from the BioSIM database

# get shapefile for red and sugar maple distributions from US Forest service
#-------------------------------------------------------------------------------
disACRU <- sf::st_read  ('../data/distribution/little1991/ACRU/litt316av.shp',
                         stringsAsFactors = FALSE, quiet = TRUE)
disACSH <- sf::st_read  ('../data/distribution/little1991/ACSA/litt318av.shp',
                         stringsAsFactors = FALSE, quiet = TRUE)

# set the coordinate system to Albers equal area projection with US Forest 
# Service parameters from https://www.fs.fed.us/nrs/atlas/littlefia/albers_prj.txt
#-------------------------------------------------------------------------------
USFS_CRS <- 
  '+proj=aea +lat_1=38.0 +lat_2=42.0 +lat_0=40.0 +lon_0=-82.0 +x_0=0 +y_0=0'
sf::st_crs (disACRU) <- USFS_CRS
sf::st_crs (disACSH) <- USFS_CRS

# convert coordinate system to WGS84
#-------------------------------------------------------------------------------
disACRU_ll <- disACRU %>% 
  sf::st_transform (crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")
#st_transform (crs = '+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45')
disACSH_ll <- disACSH %>% 
  sf::st_transform (crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")
#st_transform (crs = '+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45')

# load map biomass map from Beaudoin et al. (2014)
#-------------------------------------------------------------------------------
disACSA_be <- readTIFF ('../data/distribution/beaudoin2014/Beaudoin_etal_2014_Acer/NFI_MODIS250m_kNN_Species_Acer_Sac_v0.tif')
disACSA_be [which (disACSA_be < -1e6)] <- NA

# get map data for USA and Canada
#-------------------------------------------------------------------------------
usa <- map_data ("usa")
canada <- map_data ("worldHires", "Canada")

# make a map of municipalities with "producteur et productrices acéricole"
#-------------------------------------------------------------------------------
municipalPPAQ <- production %>% group_by (municipality, lon, lat) %>%
  summarise (n = n_distinct (uniqueID), .groups = 'drop') %>% 
  mutate (nClass = ifelse (n < 10, 1, 
                           ifelse (n < 20, 1.2, 
                                   ifelse (n < 40, 1.6, 
                                           ifelse (n < 80, 2.2, 3)))))
PPAQmap <- ggplot () + 
  geom_polygon (data = usa, 
                aes (x = long, y = lat, group = group), 
                fill = "white", 
                color = "#333333") +  
  geom_polygon (data = canada, aes (x = long, y = lat, group = group), 
                fill = "white", color = "#333333") +
  #geom_raster (raster (disACSA_be), aes (fill = values)) 
  #geom_sf (data = disACRU_ll, colour = '#901c3b99', fill = "transparent", size = 1) + # TR - change this to contours
  geom_sf (data = disACSH_ll, colour = '#f3bd4899', fill = "transparent", size = 1) + # TR - change this to contours
  # TR - add the Beaudoin map in the background
  xlab ("Longitude") + ylab ("Latitude") +
  ggtitle ('Producteur et productrices acéricole du Québec') +
  geom_point (data = metStations, 
              aes (x = Longitude, y = Latitude), 
              shape = 23, 
              size = 1.3, 
              color = "#A4103499", 
              fill = "#A4103499") +
  geom_point (data = municipalPPAQ, 
              aes (x = lon, y = lat, size = nClass, fill = '#91b9a4'), 
              fill = '#91b9a455', 
              color = "#33333366", 
              shape = 21) + 
  coord_sf (xlim = c (-79, -63.5),  ylim = c (45, 51)) +
  theme_minimal (12) + theme (legend.position = 'bottom') +
  guides (size = guide_legend (title = 'Number of producers (n)')) +
  scale_size_continuous (labels = c ('< 10','< 20', '< 40', '< 80','\u2265 80'),
                         breaks = c (1,1.2,1.6,2.2, 3))
PPAQmap

#===============================================================================
