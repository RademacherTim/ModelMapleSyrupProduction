#========================================================================================
# Script to match maple syrup producer data with climate data 
#
# To-do list:
#----------------------------------------------------------------------------------------

# source dependencies
#----------------------------------------------------------------------------------------
if (!exists ('production')) source ('readProductionData.R')
if (!existsFunction ('dms2char')) library ('sp')
if (!exists ("climData")) climData <- read_csv ("../data/siteDerivedMeteorologicalVariables.csv")

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
muniCoord$lat <- as.numeric (sp::char2dms (from = muniCoord$latitude, 
                                           chd = DSep, chm = MSep, chs = SSep))
muniCoord$lon <- -as.numeric (sp::char2dms (from = muniCoord$longitude, 
                                            chd = DSep, chm = MSep, chs = SSep))

# loop over production data to get coordinates of municipalities with producers
#----------------------------------------------------------------------------------------
production$lat <- NA
production$lon <- NA
for (r in 1:dim (production) [1]) {
  
  # check whether this row has already been associated with coordinates
  #--------------------------------------------------------------------------------------
  if (!is.na (production$lat [r])) next 
  
  # jump rows that do not have a name for municipality or region
  #--------------------------------------------------------------------------------------
  if (is.na (production$municipality [r]) | is.na (production$region [r])) next
    
  # get indices of the municipality's coordinates for the right name and region
  #--------------------------------------------------------------------------------------
  i <- grep (pattern = paste0 ("^", production$municipality [r], " ", production$region [r], "$"), 
             x = paste (muniCoord$name, muniCoord$region))
  
  # check whether the name exists twice or more often in the region
  #--------------------------------------------------------------------------------------
  if (length (i) >= 2) {
    # following deals with the exceptions of Cacouna, Bas-Saint-Laurent ("M" and "R")
    if ('R' %in% muniCoord$municipalStatus [i]) {
      i <- which (production$municipality [r] == muniCoord$name &
                  production$region [r] == muniCoord$region &
                  muniCoord$municipalStatus != "R")
    # following deals with the exceptions of:
    # - Notre-Dame-du-Bon-Conseil, Centre-du-Québec ("VL" and "P")
    # - Plessisville, Centre-du-Québec ("V" and "P")
    # - Disraeli, Chaudière-Appalaches ("V" and "P")
    } else if ('P' %in% muniCoord$municipalStatus [i]) {
      i <- which (production$municipality [r] == muniCoord$name &
                  production$region [r] == muniCoord$region &
                  muniCoord$municipalStatus == "P")
    # following deals with the exceptions of:
    # - Notre-Dame-du-Bon-Conseil, Centre-du-Québec ("VL" and "P"), already dealt with in previous
    # - Saint-Célestin, Centre-du-Québec ("VL" and "M")
    # - Hemmingford, Montérégie ("CT" and "VL")
    } else if ('VL' %in% muniCoord$municipalStatus [i]) {
      i <- which (production$municipality [r] == muniCoord$name &
                  production$region [r] == muniCoord$region &
                  muniCoord$municipalStatus != "VL")
    # following deals with the exceptions of:
    # - Bedford, Estrie ("V" and "CT")
    # - Hatley, Estrie ("M" and "CT")
    # - Stanstead, Estrie ("V" and "CT")
    # - Valcourt, Estrie ("V" and "CT")
    # - Hemmingford, Montérégie ("CT" and "VL"), already dealt with in the previous
    } else if ('CT' %in% muniCoord$municipalStatus [i]) {
      i <- which (production$municipality [r] == muniCoord$name &
                  production$region [r] == muniCoord$region &
                  muniCoord$municipalStatus == "CT")
      #print (paste (r, production$municipality [r], production$region [r]))
      #print (muniCoord$municipalStatus [i])
    }
  }
  
  # associate municipality's latitude and longitude with producer
  #--------------------------------------------------------------------------------------
  if (length (i) == 1) {
    condition <- which (production$municipality == production$municipality [r] &
                        production$region == production$region [r])
    production$lat [condition] <- muniCoord$lat [i]
    production$lon [condition] <- muniCoord$lon [i]
  } else {
    stop ("Error")
    #print (paste (r, production$municipality [r], production$region [r], sep = ''))
  }
}

# temporary clean index variables
#----------------------------------------------------------------------------------------
rm (r, i)

# add climate data to the production tibble for each site
#----------------------------------------------------------------------------------------
production$FT <- NA; production$maxCFT <- NA
production$tMean <- NA; production$tGrow <- NA; production$tWint <- NA
production$tSpri <- NA
production$pTota <- NA; production$sTota <- NA; production$pGrow <- NA
production$pWint <- NA; production$sWint <- NA; production$pSpri <- NA
production$sSpri <- NA; production$snowD <- NA
production$DoyGDD <- NA; production$wSpri <- NA; production$aSpri <- NA
production$rGrow <- NA; production$rSpri <- NA
for (s in 1:dim (climData) [1]) {
  
  # get indices of production sites for this municipality and year
  #-----------------------------------------------------------------------------
  i <- which (round (production$lat, 4) == round (climData$Latitude [s], 4) &
              round (production$lon, 4) == round (climData$Longitude [s], 4) & 
              production$municipality == climData$Name [s] &
              production$year == climData$Year [s])
  
  # associate annual climate and production data
  #-----------------------------------------------------------------------------
  production [i, 15:dim (production) [2]] <- climData [s, 6:dim (climData) [2]]
}

# save production data with appended climate data to speed up processing
#----------------------------------------------------------------------------------------
write_csv (production, "../data/productionData.csv")

#========================================================================================