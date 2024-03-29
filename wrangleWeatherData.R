#========================================================================================
# Script to read and re-organise the simulated climate data (BioSIM 11) and generate the
# siteMeteorologies.csv file.
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
if (!existsFunction ("read_csv")) library ("readr")
if (!existsFunction ("%>%")) library ("tidyverse")

# read producerLocations file to obtain the number of sites
#----------------------------------------------------------------------------------------
municipalities <- read_csv ("../data/producerLocations.csv", col_types = cols ())

# read original output file from BioSIM 11 with 50 replications per site
#----------------------------------------------------------------------------------------
# 2004-2021 had five leap years (i.e., 2004, 2008, 2012, 2016, 2020) out of 17 years 
# hence the average days per year were (365 + 5/17)
# (365 + 5/17) days per year * 17 years * 50 replications = 310 500
# number of lines per site = 310 500
nLines1 <- 310500
# 643 sites * 310 500 lines per site = 199 651 500 lines in the file

# For the file with only weather for 2021 
# 365 days per year * 1 year * 50 replications = 18 250
nLines2 <- 18250
# 643 site * 18250 lines per site = 11 552 250 lines in the file

# ascribe names 
#----------------------------------------------------------------------------------------
colNames <- c ("KeyID","Name","Latitude","Longitude","Elevation","Region","State",
               "Country","P","Replication","Year","Month","Day","Minimum Air Temperature",
               "Air Temperature","Maximum Air Temperature","Total Precipitation",
               "Relative Humidity","Wind Speed at 10 meters","Wind Direction",
               "Solar Radiation","Atmospheric Pressure","Snow Precipitation",
               "Snow Depth Accumulation","Wind Speed at 2 meters")

# turn warnings into errors
#----------------------------------------------------------------------------------------
options (warn = 2)

# loop over each site
#----------------------------------------------------------------------------------------
time0 <- Sys.time ()
for (s in 1:length (municipalities$Name)) {
  
  # read 2005 to 2020 data
  temp1 <- read_csv (file = "../data/Export (WeatherGeneration2004-2020).csv", 
                     col_names = colNames,
                     col_types = 'icdddccciiiiidddddddddddd',
                     skip = ifelse (s == 1, 1, (s-1) * nLines1 + 1),
                     n_max = nLines1) %>% 
    select (-c (KeyID, State, Country, P)) 
  
  # average over replications
  temp1 <- temp1 %>% 
    group_by (Name, Latitude, Longitude, Elevation, Year, Month, Day) %>% 
    summarise (tmin = mean (`Minimum Air Temperature`),
               temp = mean (`Air Temperature`),
               tmax = mean (`Maximum Air Temperature`),
               prec = mean (`Total Precipitation`),
               snow = mean (`Snow Precipitation`),
               snowDepth = mean (`Snow Depth Accumulation`),
               RH = mean (`Relative Humidity`),
               wind02 = mean (`Wind Speed at 2 meters`),
               wind10 = mean (`Wind Speed at 10 meters`),
               wind = mean (`Wind Direction`),
               rad = mean (`Solar Radiation`),
               pres = mean (`Atmospheric Pressure`),
               .groups = "keep")
  
  # read 2021 climate data
  temp2 <- read_csv (file = "../data/Export (WeatherGeneration2021).csv", 
                     col_names = colNames,
                     col_types = 'icdddccciiiiidddddddddddd',
                     skip = ifelse (s == 1, 1, (s-1) * nLines2 + 1),
                     n_max = nLines2) %>%
    select (-c (KeyID, State, Country, P))
  
  # average over replications
  temp2 <- temp2 %>% 
    group_by (Name, Latitude, Longitude, Elevation, Year, Month, Day) %>% 
    summarise (tmin = mean (`Minimum Air Temperature`),
               temp = mean (`Air Temperature`),
               tmax = mean (`Maximum Air Temperature`),
               prec = mean (`Total Precipitation`),
               snow = mean (`Snow Precipitation`),
               snowDepth = mean (`Snow Depth Accumulation`),
               RH = mean (`Relative Humidity`),
               wind02 = mean (`Wind Speed at 2 meters`),
               wind10 = mean (`Wind Speed at 10 meters`),
               wind = mean (`Wind Direction`),
               rad = mean (`Solar Radiation`),
               pres = mean (`Atmospheric Pressure`),
               .groups = "keep")
  
  # combined climate data into one tibble
  if (s == 1) {
    tmp <- rbind (temp1, temp2)
  } else {
    tmp <- rbind (tmp, temp1, temp2)
  }
  
  # print feedback every 30 sites
  if (s %% 100 == 0) print (s)
}
time1 <- Sys.time ()
# N.B.: This currently takes about 4.56 hours for 643 sites.

# write csv file with mean daily climate data
write_csv (tmp, file = "../data/siteMeteorologies.csv")
# 643 municipalities * (365 days per year * 13 years + 366 days per year * 5 years) = 
# 643 * (4745 + 1830) = 643 * 6575 = 4 227 725 lines
#========================================================================================