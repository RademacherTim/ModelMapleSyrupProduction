# Read statistics Canada data of long-term production of maple products in syrup equivalent

if (!existsFunction ('read_csv')) library ('tidyverse')

# read csv file with production data in gallons
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

# not clear whether statistics canada works in canadian (4.54609 liters) or imperial gallons (3.78532 liters)
# N.B.: I assume canadian gallons for now
gal2L <- 4.54609

# plot time series for total maple syrup production of Canada and its provinces
#----------------------------------------------------------------------------------------
par (mar = c (3, 5, 1, 1))
plot (x = dataCanada$REF_DATE [dataCanada$GEO == 'Canada'],
      y = dataCanada$VALUE [dataCanada$GEO == 'Canada'] * gal2L / 1e3, typ = 'l', 
      axes = 'FALSE', xlab = '', ylab = 'Maple syrup production (million liters)', 
      xlim = c (1920, 2025), ylim = c (0, 63), col = '#666666', lwd = 2)
axis (side = 1, at = seq (1920, 2020, by = 10))
axis (side = 2, las = 1)
lines (x = dataCanada$REF_DATE [dataCanada$GEO == 'Quebec'],
       y = dataCanada$VALUE [dataCanada$GEO == 'Quebec'] * gal2L / 1e3,
       col = '#003399', lwd = 2)
lines (x = dataCanada$REF_DATE [dataCanada$GEO == 'Ontario'],
       y = dataCanada$VALUE [dataCanada$GEO == 'Ontario'] * gal2L / 1e3,
       col = '#D00C27', lwd = 2)
lines (x = dataCanada$REF_DATE [dataCanada$GEO == 'Nova Scotia'],
       y = dataCanada$VALUE [dataCanada$GEO == 'Nova Scotia'] * gal2L / 1e3,
       col = '#52AC66', lwd = 2)
lines (x = dataCanada$REF_DATE [dataCanada$GEO == 'New Brunswick'],
       y = dataCanada$VALUE [dataCanada$GEO == 'New Brunswick'] * gal2L / 1e3,
       col = '#FFCD00', lwd = 2)

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
        legend = c ('Canada', 'Québec (StatCan)', 'Québec (PPAQ)', 'Ontario', 'Nova Scotia', 'New Brunswick'),
        col = c ('#666666','#003399', colours [1],'#D00C27','#52AC66','#FFCD00'), 
        box.lty = 0, lty = c (1, 1, 2, 1, 1, 1), lwd = 2)
