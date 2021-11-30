# Read Statistics Canada and USDA-NASS data of long-term production of maple products in syrup equivalent

# load dependencies
#----------------------------------------------------------------------------------------
if (!existsFunction ('read_csv')) library ('tidyverse')

# colours for USA states
#----------------------------------------------------------------------------------------
colUSA <- c ('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5',
             '#d9d9d9')

# not clear whether statistics canada works in canadian (4.54609 liters) or imperial gallons (3.78532 liters)
# N.B.: I assume Canadian gallons for now
#----------------------------------------------------------------------------------------
gal2L <- 4.54609
USgal2L <- 3.78532

# read csv file with production data in gallons
# 
#----------------------------------------------------------------------------------------
dataCanada <- read_csv ('../data/statisticsCanada/32100354.csv',
                        col_types = c ('dccccicddcdcc'),
                        col_names = c (
                          "REF_DATE","GEO","DGUID","Description","UOM","UOM_ID",
                          "SCALAR_FACTOR","SCALAR_ID","VECTOR","COORDINATE","VALUE",
                          "STATUS","SYMBOL","TERMINATED","DECIMALS"), skip = 1)

# read csv file for the USA data from the USDA-NASS 
# Downloaded from quickstats on the 30th Nov 2021: https://quickstats.nass.usda.gov/results/2481633A-9543-33D2-A9FE-990FC20E5462 
#----------------------------------------------------------------------------------------
dataUSA <- read_csv ('../data/USDA_NASS/594BB147-48D2-3BAE-B349-1A7F4FFED981.csv',
                     col_types = cols ()) %>%
  select (-c (4, 8:15)) %>%
  filter (`Data Item` == 'MAPLE SYRUP - PRODUCTION, MEASURED IN GALLONS', Domain == 'TOTAL') %>% 
  group_by (State) %>%
  arrange (Year) %>%
  filter (Value != '(D)') %>% # filter out all lines without values
  mutate (V = as.numeric (gsub (',','', Value)) * USgal2L) # convert Value to actual number

# select only values of total production
#----------------------------------------------------------------------------------------
dataCanada <-  dataCanada %>% 
  dplyr::filter (substr (Description, 1, 5) == 'Maple') 

# plot time series for total maple syrup production of Canada, the USA and Québec
#----------------------------------------------------------------------------------------
png (file = '../fig/longTermNationalProductionTrends.png', width = 750, height = 400)
par (mar = c (3, 5, 1, 1))
plot (x = dataCanada$REF_DATE [dataCanada$GEO == 'Canada'],
      y = dataCanada$VALUE [dataCanada$GEO == 'Canada'] * gal2L / 1e3, typ = 'l', 
      axes = 'FALSE', xlab = '', ylab = 'Maple syrup production (million liters)', 
      xlim = c (1920, 2025), ylim = c (0, 63), col = colours [3], lwd = 2)
axis (side = 1, at = seq (1920, 2020, by = 10))
axis (side = 2, las = 1)
abline (v = 1989.92, col = colours [1])
lines (x = dataCanada$REF_DATE [dataCanada$GEO == 'Quebec'],
       y = dataCanada$VALUE [dataCanada$GEO == 'Quebec'] * gal2L / 1e3,
       col = '#003399', lwd = 2)
lines (x = dataCanada$REF_DATE [dataCanada$GEO == 'Ontario'],
       y = dataCanada$VALUE [dataCanada$GEO == 'Ontario'] * gal2L / 1e3,
       col = '#D00C27', lwd = 2)
lines (x = dataUSA$Year [dataUSA$State == 'VERMONT'], 
       y = dataUSA$V [dataUSA$State == 'VERMONT'] / 1e6, 
       col = colUSA [7], lwd = 2)

# add lines for total USA production
#----------------------------------------------------------------------------------------
USAprod <- dataUSA %>% group_by (Year) %>% summarise (totalProd = sum (V) / 1e6)
lines (x = USAprod$Year,
       y = USAprod$totalProd,
       col = '#666666', lwd = 2)

# add lines for PPAQ data
#----------------------------------------------------------------------------------------
# According to the MAPAQ 4 liters of syrup are equivalent to 11.6566 pounds, thus
pou2L <- 4 / 11.6566
annualP <- production %>% group_by (year) %>% 
  summarise (totalP = sum (totalProduction), .groups = 'drop') %>%
  mutate (totalP = totalP * pou2L)
lines (x = annualP$year, y = annualP$totalP / 1e6, lty = 2, col = colours [1], lwd = 2)

# add legend 
#----------------------------------------------------------------------------------------
legend (1920, 60, 
        legend = c ('Canada', 'Québec (StatCan)', 'Québec (PPAQ)', 'USA','Vermont','Ontario'),
        col = c (colours [3],'#003399', colours [1],'#666666', colUSA [7],'#D00C27'), 
        box.lty = 0, lty = c (1, 1, 2, 1, 1, 1), lwd = 2)
dev.off ()

# plot time series for total maple syrup production of Canadian provinces (except for 
# Québec) and USA states
#----------------------------------------------------------------------------------------
png (file = '../fig/longTermProvincialProductionTrends.png', width = 750, height = 400)
par (mar = c (3, 5, 1, 1))
plot (x = dataCanada$REF_DATE [dataCanada$GEO == 'Canada'],
      y = dataCanada$VALUE [dataCanada$GEO == 'Canada'] * gal2L / 1e3, typ = 'l', 
      axes = 'FALSE', xlab = '', ylab = 'Maple syrup production (million liters)', 
      xlim = c (1920, 2025), ylim = c (0, 3.9), col = 'white', lwd = 2)
axis (side = 1, at = seq (1920, 2020, by = 10))
axis (side = 2, las = 1)
lines (x = dataCanada$REF_DATE [dataCanada$GEO == 'Ontario'],
       y = dataCanada$VALUE [dataCanada$GEO == 'Ontario'] * gal2L / 1e3,
       col = '#D00C27', lwd = 2)
lines (x = dataCanada$REF_DATE [dataCanada$GEO == 'Nova Scotia'],
       y = dataCanada$VALUE [dataCanada$GEO == 'Nova Scotia'] * gal2L / 1e3,
       col = '#52AC66', lwd = 2)
lines (x = dataCanada$REF_DATE [dataCanada$GEO == 'New Brunswick'],
       y = dataCanada$VALUE [dataCanada$GEO == 'New Brunswick'] * gal2L / 1e3,
       col = '#FFCD00', lwd = 2)
lines (x = dataUSA$Year [dataUSA$State == 'MAINE'], 
       y = dataUSA$V [dataUSA$State == 'MAINE'] / 1e6, 
       col = colUSA [1], lwd = 2)
lines (x = dataUSA$Year [dataUSA$State == 'MICHIGAN'], 
       y = dataUSA$V [dataUSA$State == 'MICHIGAN'] / 1e6, 
       col = colUSA [2], lwd = 2)
lines (x = dataUSA$Year [dataUSA$State == 'NEW HAMPSHIRE'], 
       y = dataUSA$V [dataUSA$State == 'NEW HAMPSHIRE'] / 1e6, 
       col = colUSA [3], lwd = 2)
lines (x = dataUSA$Year [dataUSA$State == 'NEW YORK'], 
       y = dataUSA$V [dataUSA$State == 'NEW YORK'] / 1e6, 
       col = colUSA [4], lwd = 2)
lines (x = dataUSA$Year [dataUSA$State == 'OHIO'], 
       y = dataUSA$V [dataUSA$State == 'OHIO'] / 1e6, 
       col = colUSA [5], lwd = 2)
lines (x = dataUSA$Year [dataUSA$State == 'PENNSYLVANIA'], 
       y = dataUSA$V [dataUSA$State == 'PENNSYLVANIA'] / 1e6, 
       col = colUSA [6], lwd = 2)
lines (x = dataUSA$Year [dataUSA$State == 'WISCONSIN'], 
       y = dataUSA$V [dataUSA$State == 'WISCONSIN'] / 1e6, 
       col = colUSA [8], lwd = 2)

# sum all states with less than 0.5 million liters of production into others
OTHERS <- c ('CONNECTICUT','ILLINOIS','INDIANA','MASSACHUSETTS','MINNESOTA')
dataOthers <- dataUSA %>% filter (State %in% OTHERS) %>% group_by (Year) %>% 
  summarise (totalProd = sum (V))
lines (x = dataOthers$Year, 
       y = dataOthers$totalProd / 1e6, 
       col = colUSA [9], lwd = 2)

# add legend 
#----------------------------------------------------------------------------------------
legend (1960, 4.1, 
        legend = c ('Ontario','Nova Scotia','New Brunswick','Maine','Michigan',
                    'New Hampshire','New York','Ohio','Pennsylvania','Wisconsin','Others'),
        col = c ('#D00C27','#52AC66','#FFCD00', colUSA [c (1:6, 8:9)]), 
        box.lty = 0, lty = 1, lwd = 2, cex = 0.9, bg = 'transparent')
dev.off ()

# number of taps for USA states and PPAQ (no data from Stats Canada)
#----------------------------------------------------------------------------------------
# read csv file for the USA data from the USDA-NASS 
# Downloaded from quickstats on the 30th Nov 2021: https://quickstats.nass.usda.gov/results/2481633A-9543-33D2-A9FE-990FC20E5462 
#----------------------------------------------------------------------------------------
dataUSAByState <- read_csv ('../data/USDA_NASS/594BB147-48D2-3BAE-B349-1A7F4FFED981.csv',
                            col_types = cols ()) %>%
  select (-c (4, 8:15)) %>%
  filter (`Data Item` == 'MAPLE SYRUP - NUMBER OF TAPS', Domain == 'TOTAL') %>% 
  group_by (State) %>%
  arrange (Year) %>%
  filter (Value != '(D)') %>% # filter out all lines without values
  mutate (V = as.numeric (gsub (',','', Value))) # convert Value to actual number

# plot time series for total maple syrup production of Québec (PPAQ) and USA states
#----------------------------------------------------------------------------------------
png (file = '../fig/longTermProvincialNumberOfTaps.png', width = 750, height = 400)
par (mar = c (3, 5, 1, 1))
plot (x = dataUSAByState$Year [dataUSAByState$State == 'CONNECTICUT'],
      y = dataUSAByState$V [dataUSAByState$State == 'CONNECTICUT'] / 1e6, typ = 'l', 
      axes = 'FALSE', xlab = '', ylab = 'Number of taps (thousands)', 
      xlim = c (1995, 2025), ylim = c (0, 6), col = 'white', lwd = 2)
axis (side = 1, at = seq (1995, 2020, by = 5))
axis (side = 2, las = 1)
lines (x = dataUSAByState$Year [dataUSA$State == 'MAINE'], 
       y = dataUSAByState$V [dataUSA$State == 'MAINE'] / 1e6, 
       col = colUSA [1], lwd = 2)
lines (x = dataUSAByState$Year [dataUSA$State == 'MICHIGAN'], 
       y = dataUSAByState$V [dataUSA$State == 'MICHIGAN'] / 1e6, 
       col = colUSA [2], lwd = 2)
lines (x = dataUSAByState$Year [dataUSA$State == 'NEW HAMPSHIRE'], 
       y = dataUSAByState$V [dataUSA$State == 'NEW HAMPSHIRE'] / 1e6, 
       col = colUSA [3], lwd = 2)
lines (x = dataUSAByState$Year [dataUSA$State == 'NEW YORK'], 
       y = dataUSAByState$V [dataUSA$State == 'NEW YORK'] / 1e6, 
       col = colUSA [4], lwd = 2)
lines (x = dataUSAByState$Year [dataUSA$State == 'OHIO'], 
       y = dataUSAByState$V [dataUSA$State == 'OHIO'] / 1e6, 
       col = colUSA [5], lwd = 2)
lines (x = dataUSAByState$Year [dataUSA$State == 'PENNSYLVANIA'], 
       y = dataUSAByState$V [dataUSA$State == 'PENNSYLVANIA'] / 1e6, 
       col = colUSA [6], lwd = 2)
lines (x = dataUSAByState$Year [dataUSA$State == 'VERMONT'], 
       y = dataUSAByState$V [dataUSA$State == 'VERMONT'] / 1e6, 
       col = colUSA [7], lwd = 2)
lines (x = dataUSAByState$Year [dataUSA$State == 'WISCONSIN'], 
       y = dataUSAByState$V [dataUSA$State == 'WISCONSIN'] / 1e6, 
       col = colUSA [8], lwd = 2)

# sum all states with less than 0.5 million liters of production into others
OTHERS <- c ('CONNECTICUT','ILLINOIS','INDIANA','MASSACHUSETTS','MINNESOTA')
dataOthers <- dataUSA %>% filter (State %in% OTHERS) %>% group_by (Year) %>% 
  summarise (totalProd = sum (V))
lines (x = dataOthers$Year, 
       y = dataOthers$totalProd / 1e6, 
       col = colUSA [9], lwd = 2)

# add legend 
#----------------------------------------------------------------------------------------
legend (1995, 6, 
        legend = c ('Maine','Michigan','New Hampshire','New York','Ohio','Pennsylvania',
                    'Vermont','Wisconsin','Others'),
        col = colUSA, 
        box.lty = 0, lty = 1, lwd = 2, cex = 0.9, bg = 'transparent')
dev.off ()
