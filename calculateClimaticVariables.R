#========================================================================================
# Script to explore basic relationship between average yield per tap and climatic 
# variables as simulated for the producers municipalities using BioSIM
#----------------------------------------------------------------------------------------

# to-do list: 
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
if (!existsFunction ("%>%")) library ("tidyverse")
if (!existsFunction ("read_csv")) library ("readr")
if (!exists ("metData")) metData <- read_csv ("../data/siteMeteorologies.csv", 
                                              col_types = "cdddiiidddddddddddd")
if (!existsFunction ("yday")) library ("lubridate")
if (!existsFunction ("raster")) library ("raster")

# create day of year variable
#----------------------------------------------------------------------------------------
metData <- metData %>% 
  mutate (Date = lubridate::as_date (paste (Year, Month, Day, sep = "-")), .after = Day) %>% 
  mutate (Doy = lubridate::yday (Date), .after = Date) 

# extract the number of municipalities
#----------------------------------------------------------------------------------------
nMuni <- metData %>% dplyr::select (Name, Latitude, Longitude) %>% distinct ()

# calculate the number of days with spring freeze-thaw cycles per year
#----------------------------------------------------------------------------------------
climData <- metData %>% group_by (Name, Latitude, Longitude, Elevation, Year) %>% 
  filter (Doy <= 150) %>% # look only at days in spring before the end of May
  summarise (FT = sum (tmin < -1 & tmax > 1), .groups = "keep") # count number of days with freeze-thaw cycle

# calculate the maximum number of consecutive daily freeze-thaw cycles per year  
#----------------------------------------------------------------------------------------
metData <- metData %>% mutate (FT = tmin < -1 & tmax > 1)
metData$cFT <- NA
time1 <- Sys.time ()
for (r in 1:dim (metData) [1]) {
  
  # jump iteration if we are later than end of May
  if (metData$Doy [r] > 150) next
  
  # save and reset global counter at beginning of the year
  if (metData$Doy [r] == 1) {
    metData$cFT [r] <- ifelse (metData$FT [r], 1, 0)
  # increase counter, if day has a freeze thaw cycle, or reset to zero
  } else {
    metData$cFT [r] <- ifelse (metData$FT [r], metData$cFT [r - 1] + 1, 0)
  }
 
  # give update
  if (r %% 100000 == 0) print (paste (r, Sys.time ()))
  
}
time2 <- Sys.time ()
time2 - time1
# N.B.: This takes 4 hours 

# get the maximum number of consecutive daily freeze-thaw cycles for each year
#----------------------------------------------------------------------------------------
tmp <- metData %>% group_by (Name, Latitude, Longitude, Elevation, Year) %>% 
  summarise (maxCFT = max (cFT, na.rm = TRUE), .groups = "keep") 
climData <- climData %>% add_column (maxCFT = tmp$maxCFT)

# calculate mean annual temperature, mean growing season temperature (MJJASO), mean winter 
# temperature (NDJ), and mean sugaring season temperature (FMA)
#----------------------------------------------------------------------------------------
# mean annual temperature
tmp1 <- metData %>% group_by (Name, Latitude, Longitude, Elevation, Year) %>% 
  summarise (tmean = mean (temp, na.rm = TRUE), .groups = "keep") 
# mean growing season temperature (MJJASO)
tmp2 <- metData %>% group_by (Name, Latitude, Longitude, Elevation, Year) %>% 
  filter (Doy >= ifelse (Year %% 4 == 0, 122, 121) &    # after 1 May
          Doy <= ifelse (Year %% 4 == 0, 305, 304)) %>% # before 31 Oct
  summarise (tmean = mean (temp, na.rm = TRUE), .groups = "keep") 
# mean winter temperature (NDJ)
metData <- metData %>% 
  mutate (wYear = ifelse (Doy >= ifelse (Year %% 4 == 0, 306, 305), Year + 1, Year))
tmp3 <- metData %>% group_by (Name, Latitude, Longitude, Elevation, wYear) %>%
  filter (Doy >= ifelse (Year %% 4 == 0, 306, 305) | # after 1 Nov
          Doy <= 31) %>%                             # before 31 Jan
  summarise (tmean = mean (temp, na.rm = TRUE), .groups = "keep") %>%
  rename (Year = wYear) %>% filter (Year != 2022)
# mean sugaring season temperature (FMA)
tmp4 <- metData %>% group_by (Name, Latitude, Longitude, Elevation, Year) %>%
  filter (Doy >= 32 & # after 1 Feb
          Doy <= ifelse (Year %% 4 == 0, 121, 120)) %>% # before 30 Apr
  summarise (tmean = mean (temp, na.rm = TRUE), .groups = "keep")
climData <- climData %>% 
  add_column (tMean = tmp1$tmean,
              tGrow = tmp2$tmean,
              tWint = tmp3$tmean,
              tSpri = tmp4$tmean)

# set 2004 winter temperature to NA, because it does not include Nov and Dec 2003
#----------------------------------------------------------------------------------------
climData <- climData %>% mutate (tWint = replace (tWint, Year == 2004, NA))

# calculate total annual precipitation, growing season precipitation (MJJASO), preceding 
# winter precipitation and snow fall (NDJ), and spring precipitation and snow fall (FMA)
#----------------------------------------------------------------------------------------
# annual precipitation and snow fall
tmp5 <- metData %>% group_by (Name, Latitude, Longitude, Elevation, Year) %>% 
  summarise (prec = sum (prec, na.rm = TRUE), 
             snow = sum (snow, na.rm = TRUE), .groups = "keep")
# growing season precipitation
tmp6 <- metData %>% group_by (Name, Latitude, Longitude, Elevation, Year) %>% 
  filter (Doy >= ifelse (Year %% 4 == 0, 122, 121) &    # after 1 May
          Doy <= ifelse (Year %% 4 == 0, 305, 304)) %>% # before 31 Oct
  summarise (prec = sum (prec, na.rm = TRUE), .groups = "keep")
# winter precipitation and snow fall
tmp7 <- metData %>% group_by (Name, Latitude, Longitude, Elevation, wYear) %>% 
  filter (Doy >= ifelse (Year %% 4 == 0, 306, 305) | # after 1 Nov
          Doy <= 31) %>%                             # before 31 Jan
  summarise (prec = sum (prec, na.rm = TRUE), 
             snow = sum (snow, na.rm = TRUE), .groups = "keep") %>%
  rename (Year = wYear) %>% filter (Year != 2022)
# spring precipitation and snow fall
tmp8 <- metData %>% group_by (Name, Latitude, Longitude, Elevation, wYear) %>% 
  filter (Doy >= 32 & # after 1 Feb
          Doy <= ifelse (Year %% 4 == 0, 121, 120)) %>% # before 30 Apr
  summarise (prec = sum (prec, na.rm = TRUE), 
             snow = sum (snow, na.rm = TRUE), .groups = "keep") 
climData <- climData %>% 
  add_column (prec  = tmp5$prec,
              snow  = tmp5$snow,
              pGrow = tmp6$prec,
              pWint = tmp7$prec,
              sWint = tmp7$snow,
              pSpri = tmp8$prec,
              sSpri = tmp8$snow)

# calculate mean snow depth for February
#----------------------------------------------------------------------------------------
tmp9 <- metData %>% group_by (Name, Latitude, Longitude, Elevation, wYear) %>%
 filter (lubridate::month (Date) == 2) %>%
  summarise (snowD = mean (snowDepth, na.rm = TRUE), .groups = "keep")
climaData <- climData %>%
  add_column (snowD = tmp9$snowD)

# calculate growing degree days for each day and cumulative sum
#----------------------------------------------------------------------------------------
GDDThres <- 5.0 # temperature threshold above which growing degree days are accumulated
metData <- metData %>% mutate (GDD = ifelse (temp - GDDThres > 0, temp - GDDThres, 0)) 
metData <- metData %>% group_by (Name, Latitude, Longitude, Elevation, Year) %>%
  mutate (GDDsum = cumsum (GDD)) 
  
# get day of year, when growing degree threshold is reached (DOY75 roughly corresponds to 
# the heat sum necessary for bud break in sugar maple according to )
#----------------------------------------------------------------------------------------
GDDcumThres <- 75
tmp10 <- metData %>% group_by (Name, Latitude, Longitude, Elevation, Year) %>%
  filter (GDDsum >= GDDcumThres) %>%
  summarise (DoyGDD = min (Doy))
climData <- climData %>% 
  add_column (DoyGDD = tmp10$DoyGDD)

# calculate the mean wind speed at 10m above the surface during the sugaring season (FMA)
#----------------------------------------------------------------------------------------
tmp11 <- metData %>% group_by (Name, Latitude, Longitude, Elevation, Year) %>%
  filter (lubridate::month (Date) %in% 2:4) %>%
  summarise (windS = mean (wind10, na.rm = TRUE), .groups = "keep")
climData <- climData %>%
  add_column (snowD = tmp11$windS)

# calculate the mean atmospheric pressure during the sugaring season (FMA)
#----------------------------------------------------------------------------------------
tmp12 <- metData %>% group_by (Name, Latitude, Longitude, Elevation, Year) %>%
  filter (lubridate::month (Date) %in% 2:4) %>%
  summarise (presA = mean (pres, na.rm = TRUE), .groups = "keep")
climData <- climData %>%
  add_column (presA = tmp12$presA)

# calculate the mean down-welling shortwave radiation during the sugaring season (FMA)
#----------------------------------------------------------------------------------------
tmp13 <- metData %>% group_by (Name, Latitude, Longitude, Elevation, Year) %>%
  filter (lubridate::month (Date) %in% 2:4) %>%
  summarise (solRa = mean (pres, na.rm = TRUE), .groups = "keep")
climData <- climData %>%
  add_column (rSpri = tmp13$solRa)

# calculate the mean down-welling shortwave radiation during the growing season (MJJASO)
#----------------------------------------------------------------------------------------
tmp14 <- metData %>% group_by (Name, Latitude, Longitude, Elevation, Year) %>%
  filter (lubridate::month (Date) %in% 5:10) %>%
  summarise (solRa = mean (pres, na.rm = TRUE), .groups = "keep")
climData <- climData %>%
  add_column (rGrow = tmp14$solRa)

# write csv file with derived meteorological variables
#----------------------------------------------------------------------------------------
write_csv (climData, "../data/siteDerivedMeteorologicalVariables.csv")
#========================================================================================