#========================================================================================
# Script to match maple syrup producer data with climate data 
#
# Questions:
# - Generally works but need to figure out what is going on with multiple matches
# - Need to integrate region into the matching 
# - Need to deal with cases when there are two municipalities with the same name in the same region
#----------------------------------------------------------------------------------------

# source dependencies
#----------------------------------------------------------------------------------------
if (!exists ('production')) source ('readProductionData.R')
if (!existsFunction ('dms2char')) library ('sp')

# read file with locations of the municipalities from données Québec
#----------------------------------------------------------------------------------------
muniCoord <- readxl::read_excel (path = path.expand ('../data/ISQ_code_geo_20211029.xls'),
  sheet = 'ISQ_code_geo_20211029',
  col_names = c ('municipalID',      # unique identifier for municipality
                 'name',             # name of municipality
                 'population',       # estimated population of municipality
                 'cartographicRef',  # cartographic reference 
                 'latitude',         # latitude of municipality            (DMS)
                 'longitude',        # longitude of municipality           (DMS)
                 'landSurface',      # terrestrial surface of municipality (km2)
                 'totalSurface',     # total surface area of municipality  (km2)
                 'regionalDivision', # type of regional division
                 'regionalCode',     # code for regional division
                 'region'),          # name of the administrative region
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
muniCoord$lon <- as.numeric (char2dms (from = muniCoord$longitude, 
                                       chd = DSep, chm = MSep, chs = SSep))

# loop over production data to get coordinates of municipality
#----------------------------------------------------------------------------------------
production$lat <- NA
production$lon <- NA
for (r in 1:dim (production) [1]) {
  # check whether this row has already been associated with coordinates
  #--------------------------------------------------------------------------------------
  if (!is.na (production$lat [r])) next 
    
  # get indices of the municipality's coordinates
  #--------------------------------------------------------------------------------------
  indices <- grep (pattern = production$municipality [r], 
                   x = muniCoord$name, ignore.case = TRUE)
  
  # associate municipality's latitude and longitude with producer
  #--------------------------------------------------------------------------------------
  if (length (indices) != 0) {
    production$lat [which (production$municipality == production$municipality [r])] <- 
      muniCoord$lat [indices]
    production$lon [which (production$municipality == production$municipality [r])] <- 
      muniCoord$lon [indices]
  } else {
    print (paste ('Row: ',r,' Municipality: ', production$municipality [r]), sep = '')
  }
}

# initialise spatial resolution for climate data
#-------------------------------------------------------------------------------
res <- 0.25

# directory name with climate data
#-------------------------------------------------------------------------------
dirString <- '/Volumes/TREE LAB001/data/climate/princeton/'

# initial possible climates for climate data 
#-------------------------------------------------------------------------------
dates <- seq (from = as_date ('1948-01-01'), 
              to   = as_date ('2016-12-31'), by = 1)

#========================================================================================
