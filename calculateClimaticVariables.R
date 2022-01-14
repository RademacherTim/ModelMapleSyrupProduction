#========================================================================================
# Script to explore basic relationship between average yield per tap and climatic 
# variables as simulated for the producers municipalities using BioSIM
#----------------------------------------------------------------------------------------

# to-do list: 
#----------------------------------------------------------------------------------------
# - add 2004 to the weather simulation to include previous year variables
# - improve temporal associations with actually biological processes
# - add administrative region to the metData and climData tibbles

# load dependencies
#----------------------------------------------------------------------------------------
if (!existsFunction ("read_csv")) library ("readr")
if (!exists ("metData")) metData <- read_csv ("../data/siteClimate.csv", 
                                              col_types = "cdddiiidddddddddddd")
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
climData <- metData %>% group_by (Name, Latitude, Longitude, Elevation, Year) %>% 
  filter (Doy <= 150) %>% # look only at days in spring before the end of May
  summarise (FT = sum (tmin < -1 & tmax > 1), .groups = "keep") # count number of days with freeze-thaw cycle

# calculate the maximum number of consecutive daily freeze-thaw cycles per year  
#----------------------------------------------------------------------------------------
metData$cFT <- NA
for (r in 1:dim (metData) [1]) {
  
  # save and reset global counter at end of the year
  if (metData$Doy [r] < metData$Doy [r - 1]) l <- 0
  
  # increase counter local counter, if day has a freeze thaw cycle
  if (metData$tmin [r] < -1 & metData$tmax [r] > 1) {
    l <- l + 1
  # reset local counter, if it not a freeze-thaw cycle
  } else {
    l <- 0
  }
  
  # add local counter to the metData
  metData$cFT [r] <- l
}

# get the maximum number of consecutive daily freeze-that for each year
#----------------------------------------------------------------------------------------
tmp <- metData %>% group_by (Name, Latitude, Longitude, Elevation, Year) %>% 
  summarise (maxCFT = max (cFT, na.rm = TRUE), .groups = "keep") 
climData <- climData %>% add_column (maxCFT = tmp$maxCFT)

# calculate mean annual temperature, mean summer temperature (JJA), and mean winter 
# temperature (JFM)
#----------------------------------------------------------------------------------------
# N.B.: TR - Should make this biologically more relevant with the temperatures preceding 
# the actual sugaring season
tmp1 <- metData %>% group_by (Name, Latitude, Longitude, Elevation, Year) %>% 
  summarise (tmean = mean (temp, na.rm = TRUE), .groups = "keep") 
tmp2 <- metData %>% group_by (Name, Latitude, Longitude, Elevation, Year) %>% 
  filter (Doy >= ifelse (Year %% 4 == 0, 153, 152) & 
          Doy <= ifelse (Year %% 4 == 0, 244, 243)) %>%
  summarise (tmean = mean (temp, na.rm = TRUE), .groups = "keep") 
tmp3 <- metData %>% group_by (Name, Latitude, Longitude, Elevation, Year) %>% 
  filter (Doy <= ifelse (Year %% 4 == 0, 91, 90)) %>%
  summarise (tmean = mean (temp, na.rm = TRUE), .groups = "keep") 
climData <- climData %>% 
  add_column (tmean = tmp1$tmean,
              tmJJA = tmp2$tmean,
              tmJFM = tmp3$tmean)

# calculate total annual precipitation, annual snow fall
#----------------------------------------------------------------------------------------
tmp4 <- metData %>% group_by (Name, Latitude, Longitude, Elevation, Year) %>% 
  summarise (prec = sum (prec, na.rm = TRUE), 
             snow = sum (snow, na.rm = TRUE), .groups = "keep")
tmp5 <- metData %>% group_by (Name, Latitude, Longitude, Elevation, Year) %>% 
  filter (Doy <= ifelse (Year %% 4 == 0, 91, 90)) %>%
  summarise (preJFM = sum (prec, na.rm = TRUE), 
             snoJFM = sum (snow, na.rm = TRUE), .groups = "keep")
tmp5 <- metData %>% group_by (Name, Latitude, Longitude, Elevation, Year) %>% 
  filter (Doy <= ifelse (Year %% 4 == 0, 91, 90)) %>%
  summarise (preJFM = sum (prec, na.rm = TRUE), 
             snoJFM = sum (snow, na.rm = TRUE), .groups = "keep")
climData <- climData %>% 
  add_column (prec = tmp4$prec,
              snow = tmp4$snow)
#========================================================================================