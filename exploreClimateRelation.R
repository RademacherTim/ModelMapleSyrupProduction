#========================================================================================
# Script to explore basic relationship between average yield per tap and climatic 
# variables as simulated for the producers municipalities using BioSIM
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
if (!existsFunction ("read_csv")) library ("readr")
if (!exists ("metData")) metData <- read_csv ("../data/siteClimate.csv", 
                                              col_types = "cdddiiidddddddddddd")
if (!exists ("production")) source ("readProductionData.R")
if (!existsFunction ("yday")) library ("lubridate")

# create day of year variable
#----------------------------------------------------------------------------------------
metData <- metData %>% 
  mutate (Date = lubridate::as_date (paste (Year, Month, Day, sep = "-")), .after = Day) %>% 
  mutate (Doy = lubridate::yday (Date), .after = Date) 

# extract the number of municipalities
#----------------------------------------------------------------------------------------
nMuni <- metData %>% select (Name, Latitude, Longitude) %>% unique ()

# calculate the number of days with spring freeze-thaw cycles per year
#----------------------------------------------------------------------------------------
freezeThaw <- metData %>% group_by (Name, Latitude, Longitude, Elevation, Year) %>% 
  filter (Doy <= 150) %>% # look only at days in spring before the end of May
  summarise (FreezeThaw = sum (tmin < -1 & tmax >= 1), .groups = "keep") # count number of days with freeze-thaw cycle

# calculate the maximum number of consecutive spring freeze-thaw cycles per year  
#----------------------------------------------------------------------------------------
maxFreezeThaw <- tibble (site = NA, Year = rep (2005:2021, ))
for (r in 1:dim (metData) [1]) {
  s <- 
}

#========================================================================================