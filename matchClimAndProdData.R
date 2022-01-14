#========================================================================================
# Script to match maple syrup producer data with climate data 
#
# Questions:
# - TR Generally works but need to figure out what is going on with multiple matches
# - TR Need to integrate region into the matching 
# - TR Need to deal with cases when there are two municipalities with the same name in the same region
#----------------------------------------------------------------------------------------

# source dependencies
#----------------------------------------------------------------------------------------
if (!exists ('production')) source ('readProductionData.R')
if (!existsFunction ('dms2char')) library ('sp')

# read file with locations of the municipalities from données Québec
# downloaded from https://statistique.quebec.ca/pls/hcp/hcp225_fichr_seqnt.hcp225_f1?pvcLangue=fr
# at the 16th of November 2021
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

# loop over production data to get coordinates of municipalities with producers
#----------------------------------------------------------------------------------------
production$lat <- NA
production$lon <- NA
for (r in 1:dim (production) [1]) {
  
  # check whether this row has already been associated with coordinates
  #--------------------------------------------------------------------------------------
  if (!is.na (production$lat [r])) next 
  if (is.na (production$municipality [r])) next
    
  # get indices of the municipality's coordinates
  #--------------------------------------------------------------------------------------
  iName <- grep (pattern = paste0 ("^", production$municipality [r], "$"), 
                 x = muniCoord$name)
  iRegion <- grep (pattern = paste0 ("^",production$region [r], "$"),
                   x = muniCoord$region)
  #i <- iRegion [which (iName == iRegion)]
  
  if (length (iName) >= 2) {
    print (paste (iName, production$municipality [r], 
                  production$region [r], muniCoord$municipalStatus [iName]))
    if ('R' %in% muniCoord$municipalStatus [iName]) {
      i <- which (production$municipality [r] == muniCoord$name &
                    production$region [r] == muniCoord$region &
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
  
  # associate municipality's latitude and longitude with producer
  #--------------------------------------------------------------------------------------
  if (length (i) == 1) {
    production$lat [which (production$municipality == production$municipality [r])] <- 
      muniCoord$lat [i]
    production$lon [which (production$municipality == production$municipality [r])] <- 
      muniCoord$lon [i]
  } else {
    print (paste ("Row: ",r ," Municipality: ", production$municipality [r], 
                  " Region: ",production$region [r], " Indices: ",iName, sep = ''))
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

# need to actually match the production and climate data
#-------------------------------------------------------------------------------

#========================================================================================