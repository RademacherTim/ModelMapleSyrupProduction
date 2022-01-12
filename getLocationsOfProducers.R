#========================================================================================
# Create list of coordinates of all municipalities for which we have to simulate data
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
if (!exists ('production')) source ('readProductionData.R')
if (!existsFunction ('dms2char')) library ('sp')

# list of unique municipalities with producers
#----------------------------------------------------------------------------------------
listOfMunicipalities <- production %>% 
  select (municipality, region) %>% 
  unique () %>% 
  filter (!is.na (municipality) & !is.na (region))

# get list of municipalities from the government of Quebec
#----------------------------------------------------------------------------------------
muniCoord <- readxl::read_excel (path = path.expand ('../data/ISQ_code_geo_20211116.xlsx'),
  sheet = 'ISQ_code_geo_20211116',
  col_names = c ('municipalID',         # unique identifier for municipality
                 'municipalStatus',     # status of the municipality (M = Municipality, V = Ville, NO = non-organised territory, CT = Canton, R = Reserve indienne, VL = village, P = Paroisse, ) 
                 'name',                # name of municipality
                 'population',          # estimated population of municipality
                 'dateOfIncorporation', # date of incorporation of the municipality
                 'cartographicRef',     # cartographic reference 
                 'latitude',            # latitude of municipality            (DMS)
                 'longitude',           # longitude of municipality           (DMS)
                 'landSurface',         # terrestrial surface of municipality (km2)
                 'totalSurface',        # total surface area of municipality  (km2)
                 'regionalDivision',    # type of regional division
                 'regionalCode',        # code for regional division
                 'region'),             # name of the administrative region
  skip = 1)

# decompose the latitudes and longitudes in degrees, minutes, and seconds
#----------------------------------------------------------------------------------------
DSep <- substr (muniCoord$latitude,  3,  3) [1]
MSep <- substr (muniCoord$latitude,  7,  7) [1]
SSep <- substr (muniCoord$latitude, 11, 11) [1]

# create DMS object of latitude and longitude and convert to decimal
#----------------------------------------------------------------------------------------
muniCoord$lat <- as.numeric (char2dms (from = muniCoord$latitude, 
                                       chd = DSep, chm = MSep, chs = SSep))
muniCoord$lon <- -as.numeric (char2dms (from = muniCoord$longitude, 
                                        chd = DSep, chm = MSep, chs = SSep))

# Select only municipalities with producers
#----------------------------------------------------------------------------------------
producerLocations <- tibble (municipalID = NA, municipalStatus = NA, name = NA, 
                             population = NA, dateOfIncorporation = NA, 
                             cartographicRef = NA, latitude = NA, longitude = NA, 
                             landSurface = NA, totalSurface = NA, regionalDivision = NA, 
                             regionalCode = NA, region = NA, lat = NA, lon = NA)
for (r in 1: dim (listOfMunicipalities) [1]) {
  i <- which (listOfMunicipalities$municipality [r] == muniCoord$name &
              listOfMunicipalities$region [r] == muniCoord$region)
  
  if (length (i) >= 2) {
    print (paste (i, listOfMunicipalities$municipality [r], 
                  listOfMunicipalities$region [r], muniCoord$municipalStatus [i]))
    if ('R' %in% muniCoord$municipalStatus [i]) {
      i <- which (listOfMunicipalities$municipality [r] == muniCoord$name &
                    listOfMunicipalities$region [r] == muniCoord$region &
                    muniCoord$municipalStatus != "R")
    } else if ('P' %in% muniCoord$municipalStatus [i]) {
      i <- which (listOfMunicipalities$municipality [r] == muniCoord$name &
                    listOfMunicipalities$region [r] == muniCoord$region &
                    muniCoord$municipalStatus == "P")
    } else if ('VL' %in% muniCoord$municipalStatus [i]) {
      i <- which (listOfMunicipalities$municipality [r] == muniCoord$name &
                    listOfMunicipalities$region [r] == muniCoord$region &
                    muniCoord$municipalStatus != "VL")
    } else if ('CT' %in% muniCoord$municipalStatus [i]) {
      i <- which (listOfMunicipalities$municipality [r] == muniCoord$name &
                    listOfMunicipalities$region [r] == muniCoord$region &
                    muniCoord$municipalStatus == "CT")
    }
  }
  if (length (i) == 1) {
    producerLocations <- producerLocations %>% add_row (muniCoord [i, ])  
  } else if (length (i) == 2) {
    stop ("There are two municipalities!")
    producerLocations <- producerLocations %>% add_row (muniCoord [i, ])
  } else {
    stop ("Stop!")
  }
}
# delete empty first row to fall back to 643 locations
producerLocations <- producerLocations [-1, ]

# write file with locations for BioSIM
#----------------------------------------------------------------------------------------
producerLocations <- producerLocations %>% select (name, lat, lon, region) %>% 
  mutate (KeyID = 1:dim (producerLocations)[1]) %>% relocate (KeyID) %>%
  rename (Name = name, Latitude = lat, Longitude = lon)
write_csv (producerLocations, "../data/producerLocations.csv")
#========================================================================================