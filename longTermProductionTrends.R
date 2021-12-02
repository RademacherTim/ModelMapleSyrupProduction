# Read Statistics Canada and USDA-NASS data of long-term production of maple products in syrup equivalent

# load dependencies
#----------------------------------------------------------------------------------------
if (!existsFunction ('read_csv'))    library ('tidyverse')
if (!existsFunction ('weightedSd')) (library ('matrixStats'))

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

# select only values of total production
#----------------------------------------------------------------------------------------
dataCanada <-  dataCanada %>% 
  dplyr::filter (substr (Description, 1, 5) == 'Maple') 

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
states <- c ('MAINE','MICHIGAN','NEW HAMPSHIRE','NEW YORK','OHIO','PENNSYLVANIA',
             'VERMONT','WISCONSIN')
for (s in c (1:6, 8)) {
  lines (x = dataUSA$Year [dataUSA$State == states [s]], 
         y = dataUSA$V [dataUSA$State == states [s]] / 1e6, 
         col = colUSA [s], lwd = 2)
}

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
for (s in 1:8) {
  lines (x = dataUSAByState$Year [dataUSAByState$State == states [s]], 
         y = dataUSAByState$V [dataUSAByState$State == states [s]] / 1e6, 
         col = colUSA [s], lwd = 2)
}

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

# below is all to plot the approximate price of maple syrup over time
#----------------------------------------------------------------------------------------

# read historic currency exchange rates from Statistics Canada
# downloaded from https://www150.statcan.gc.ca/t1/tbl1/en/cv.action?pid=1010000901 on the 
# 1st of December 2021
#----------------------------------------------------------------------------------------
USD2CAD <- read_csv (file = '../data/statisticsCanada/1010000901-eng.csv', skip = 9, 
                     col_types = 'cddddddd') [2, ] %>% # only working with the "noon spot rate, average" for now
  pivot_longer (cols = 2:800,
                names_to = c ('month', 'Year'),
                names_sep = ' ', 
                values_to = 'rate') %>% group_by (Year) %>%
  summarise (meanRate = mean (rate)) %>%
  mutate (Year = as.numeric (Year))

# recent currency exchange rates from the Bank of Canada
# downloaded from https://www.bankofcanada.ca/rates/exchange/annual-average-exchange-rates/#download
# on the 2nd of December 2021
#----------------------------------------------------------------------------------------
tmp4 <- read_csv (file = '../data/statisticsCanada/FX_RATES_ANNUAL-sd-2017-01-01.csv', 
                 skip = 39, col_types = 'Ddddd') %>% 
  mutate (Year = lubridate::year (date)) %>%
  select (Year, FXAUSDCAD) %>% rename (meanRate = FXAUSDCAD)
USD2CAD <- rbind (USD2CAD, tmp4)

# get data for prices in the USA
#----------------------------------------------------------------------------------------
dataUSA <- read_csv ('../data/USDA_NASS/594BB147-48D2-3BAE-B349-1A7F4FFED981.csv',
                     col_types = cols ()) %>%
  select (-c (4, 8:15)) %>%
  filter (`Data Item` == 'MAPLE SYRUP - PRICE RECEIVED, MEASURED IN $ / GALLON') %>% 
  group_by (State) %>%
  arrange (Year) %>%
  filter (Value != '(D)') %>%
  left_join (USD2CAD, by = 'Year') %>% 
  mutate (V = (as.numeric (Value) / USgal2L) / meanRate)

# prices in the USA
#----------------------------------------------------------------------------------------
png (file = '../fig/longTermPricesPerLiterUSA.png', width = 750, height = 400)
par (mar = c (3, 5, 1, 5))
plot (x = dataUSA$Year [dataUSA$State == 'CONNECTICUT'], 
      y = dataUSA$V  [dataUSA$State == 'CONNECTICUT'], typ = 'l',
      xlim = c (1992, 2021), ylim = c (0, 18), axes = FALSE,
      ylab = 'Price (Canadian-$ per liter)', col = 'white')
axis (side = 1)
axis (side = 2, las = 1, at = seq (0, 18, by = 6))
# add line for each state
for (s in 1:8) {
  lines (x = dataUSA$Year [dataUSA$State == states [s]], 
         y = dataUSA$V [dataUSA$State == states [s]], 
         col = colUSA [s], lwd = 2)
}

# average all states with less than 0.5 million liters of production into others
#----------------------------------------------------------------------------------------
dataOthers <- dataUSA %>% filter (State %in% OTHERS) %>% group_by (Year) %>% 
  summarise (V = mean (V))
lines (x = dataOthers$Year, 
       y = dataOthers$V, 
       col = colUSA [9], lwd = 2)

# add legend 
#----------------------------------------------------------------------------------------
legend (2008, 7.5, 
        legend = c ('Maine','Michigan','New Hampshire','New York','Ohio','Pennsylvania',
                    'Vermont','Wisconsin','Others'),
        col = colUSA, 
        box.lty = 0, lty = 1, lwd = 2, cex = 0.7, bg = 'transparent')

# add mean exchange rate
#----------------------------------------------------------------------------------------
par (new = TRUE)
par (mar = c (3, 5, 1, 5))
plot (x = USD2CAD$Year [USD2CAD$Year >= 1992], y =  1 / USD2CAD$meanRate [USD2CAD$Year >= 1992], 
      xlim = c (1992, 2021), ylim = c (0, 0.97), lty = 3,
      axes = FALSE, typ = 'l', xlab = '', ylab = '', col = '#666666')
axis (side = 4, las = 1)
mtext (side = 4, line = 3, text = 'Exchange rate')
dev.off ()

# read csv file with production data in gallons
# 
#----------------------------------------------------------------------------------------
dataCanada <- read_csv ('../data/statisticsCanada/32100354.csv',
                        col_types = c ('dccccicddcdcc'),
                        col_names = c (
                          "REF_DATE","GEO","DGUID","Description","UOM","UOM_ID",
                          "SCALAR_FACTOR","SCALAR_ID","VECTOR","COORDINATE","VALUE",
                          "STATUS","SYMBOL","TERMINATED","DECIMALS"), skip = 1)

# select only gross and total production to plot the average value (aka price)
#----------------------------------------------------------------------------------------
tmp1 <- dataCanada %>% 
  dplyr::filter (substr (Description, 1, 5) == 'Gross') %>%
  select (1:2, 11)
tmp2 <- dataCanada %>% 
  dplyr::filter (substr (Description, 1, 5) == 'Maple', GEO != 'Canada') %>% 
  group_by (REF_DATE) %>% 
  mutate (P = VALUE * gal2L) %>% select (1:2, P) %>% 
  group_by (REF_DATE) %>% 
  left_join (tmp1 %>% filter (GEO != 'Canada'), by = c ('REF_DATE', 'GEO')) %>% 
  summarise (meanPrice = weighted.mean (VALUE / P, P, na.rm = TRUE),
             sdPrice = weightedSd (VALUE / P, P, na.rm = TRUE)) %>%
  rename (Year = REF_DATE)
dataCa <- dataCanada %>% 
  dplyr::filter (substr (Description, 1, 5) == 'Maple', GEO == 'Canada') %>%
  dplyr::select (1,11) %>%
  left_join (tmp1 %>% filter (GEO == 'Canada'), by = c ('REF_DATE')) %>% 
  rename (Year = REF_DATE, totalProd = VALUE.x, grossValue = VALUE.y) %>%
  mutate (totalProd = totalProd * gal2L,
          meanPrice = grossValue / totalProd) 

png (file = '../fig/longTermPrices.png', width = 750, height = 400)
par (mar = c (3, 5, 1, 1))
plot (x = dataCa$Year, y = dataCa$meanPrice, 
      typ = 'l', axes = 'FALSE', xlab = '', ylab = 'Price ($-CAD per liter)', 
      xlim = c (1920, 2025), ylim = c (0, 13.0), col = colours [3], lwd = 2)
axis (side = 1)
axis (side = 2, las = 1)

# add USA average price
#----------------------------------------------------------------------------------------
# get data for prices in the USA
tmp3 <- read_csv ('../data/USDA_NASS/594BB147-48D2-3BAE-B349-1A7F4FFED981.csv',
                  col_types = cols ()) %>%
  select (-c (4, 8:15)) %>%
  filter (`Data Item` == 'MAPLE SYRUP - PRODUCTION, MEASURED IN GALLONS', Domain == 'TOTAL') %>% 
  group_by (State) %>%
  arrange (Year) %>%
  filter (Value != '(D)') %>% # filter out all lines without values
  mutate (P = as.numeric (gsub (',','', Value)) * USgal2L) %>% # convert Value to actual number
  select (Year, State, P)
dataUSA <- read_csv ('../data/USDA_NASS/594BB147-48D2-3BAE-B349-1A7F4FFED981.csv',
                     col_types = cols ()) %>%
  select (-c (4, 8:15)) %>%
  filter (`Data Item` == 'MAPLE SYRUP - PRICE RECEIVED, MEASURED IN $ / GALLON') %>% 
  group_by (State) %>%
  arrange (Year) %>%
  filter (Value != '(D)') %>%
  left_join (USD2CAD, by = 'Year') %>% 
  mutate (V = (as.numeric (Value) / USgal2L) / meanRate) %>% 
  select (Year, State, V) %>% 
  left_join (tmp3, by = c ('Year', 'State'))
dataUS <- dataUSA %>% group_by (Year) %>% 
  summarise (meanPrice = weighted.mean (V, P), # weighted.mean (V, )
             sdPrice = weightedSd (V, P)) # weightedSd (V, )
polygon (x = c (dataUS$Year, rev (dataUS$Year)),
         y = c (dataUS$meanPrice - dataUS$sdPrice, 
                rev (dataUS$meanPrice + dataUS$sdPrice)), lty = 0, col = '#66666666')
lines (x = dataUS$Year, y = dataUS$meanPrice, col = '#666666', lwd = 2)
#abline (v = 1989.92, col = colours [1])
polygon (x = c (tmp2$Year, rev (tmp2$Year)), 
         y = c (tmp2$meanPrice + tmp2$sdPrice, rev (tmp2$meanPrice - tmp2$sdPrice)), 
         lty = 0, 
         col = addAlpha (colours [3], 50))
lines (x = tmp2$Year,
       y = tmp2$meanPrice, col = colours [3], lwd = 2)
dev.off ()
