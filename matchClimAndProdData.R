#========================================================================================
# Script to match maple syrup producer data with climate data 
#
# Questions:
# - 
#----------------------------------------------------------------------------------------

# source dependencies
#----------------------------------------------------------------------------------------
if (!exists ('production')) source ('readProductionData.R')
if (!existsFunction ('dms2char')) library ('sp')

# read file with locations of the municipalities from données Québec
#----------------------------------------------------------------------------------------
municipalCoordinates <- readxl::read_excel (path = '../data/MunicipalitéDuQuébec.xlsx',
                                            sheet = 'coordonnées',
                                            col_names = c ('municipalID', # unique identifier for the municipality
                                                           'name',        # name of municipality
                                                           'latitude',    # latitude of municipality
                                                           'longitude'),  # longitude of municipality
                                  skip = 1)
# decompose the latitudes and longitudes in degrees, minutes, and seconds
#----------------------------------------------------------------------------------------

#========================================================================================