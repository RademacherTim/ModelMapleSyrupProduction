#========================================================================================
# Script to read maple syrup producer data provided 
#
# Questions:
# - Would it be possible to get more data on grades of syrup?
# - Does the production include all production or the contigent supplied to the PPAQ? 
#----------------------------------------------------------------------------------------

# load dependencies 
#----------------------------------------------------------------------------------------
if (!existsFunction ('read_excel')) library ('readxl')
if (!existsFunction ('%>%')) library ('tidyverse')

# define some fun colours and a function to add opacity to a colour
#----------------------------------------------------------------------------------------
colours <- c ('#CC7240','#94452E','#B8AB9E')
addAlpha <- function (col,       # color name
                      percent) { # % transparency
    
  # get RGB values for named colour
  rgb.val <- col2rgb (col)
    
  # make new colour using input colour as base and alpha set by transparency
  colAlpha <- rgb (rgb.val [1], rgb.val [2], rgb.val [3],
                   max = 255,
                   alpha = (100 - percent) * 255 / 100)
    
  # return the colour with alpha channel
  invisible (colAlpha)
}

# read file 
#----------------------------------------------------------------------------------------
production <- readxl::read_excel (path = '../data/PPAQ/20211019_MR_InfoProd.xlsx',
                                  sheet = '20211019_MR_InfoProd',
                                  col_names = c ('uniqueID',        # unique identifier for producer
                                                 'year',            # calendar year AD
                                                 'numberOfTaps',    # total number of taps for the year 
                                                 'totalProduction', # total production [lbs of syrup]
                                                 'municipality'),   # municipality in which the sugar bush can be found
                                  skip = 1)

# calculate mean production per tap (lb of syrup per tap)
# NB: a tree can have multiple taps this is necessarily a conservative estimate of mean 
# production per tree
#----------------------------------------------------------------------------------------
production <- production %>% 
  mutate (meanProduction = totalProduction / numberOfTaps)

# get some basic statistics and plots
#----------------------------------------------------------------------------------------
length (unique (production$uniqueID)) # number of producers
# 10756 different producers contributed to the data set

range (production$year) # length of historic record
# they started recording in 2005 and have data until 2021

png ('../fig/producersPerYear.png', width = 700, height = 400)
par (mar = c (3, 6, 1, 1))
hist (production$year, xlab = '', ylab = '', main = '', las = 1, 
      col = addAlpha (colours [1], 30))
mtext (side = 2, line = 4, text = 'Number of producers')
dev.off ()

# how many people reported no taps?
sum (production$numberOfTaps == 0)
# 788 instances of no tapping at all
# 0 instances of negative number of taps 

# how many people reported less than 100 taps?
sum (production$numberOfTaps < 100)
# 799 instances of less than 100 taps

# how many people reported no production?
sum (production$totalProduction == 0)
# 6422 instances of no tapping at all
# 0 instances of negative total production

# plot number of taps to get an idea of company size
png ('../fig/numberOfTapsPerProducers.png', width = 700, height = 400)
par (mar = c (5, 5, 1, 1))
producerSize <- production %>% group_by (uniqueID) %>% 
  summarise (meanNumberOfTaps = mean (numberOfTaps)) %>% 
  select (meanNumberOfTaps) %>% unlist ()
hist (producerSize / 1000, col = addAlpha (colours [1], 30), main = '', las = 1,
      xlab = 'Number of taps per producers (thousands)', breaks = seq (0, 240, by = 1))
dev.off ()
png ('../fig/numberOfTapsPerProducersCropped.png', width = 700, height = 400)
par (mar = c (5, 5, 1, 1))
hist (producerSize / 1000, col = addAlpha (colours [1], 30), main = '', las = 1,
      xlab = 'Number of taps per producers (thousands)', breaks = seq (0, 240, by = 0.2),
      xlim = c (0, 50))
dev.off ()
# mean number of taps
mean (production$numberOfTaps)
# 6518 taps on average
median (production$numberOfTaps)
# 3500 is the median number of taps

# plot number of taps over time
png ('../fig/numberOfTapsOverTime.png', width = 700, height = 400)
annualProduction <- production %>% group_by (year) %>% 
  summarise (meanNumberOfTaps = mean (numberOfTaps) / 1000,
             sdNumberOfTaps   = sd   (numberOfTaps) / 1000,
             nNumberOfTaps    = n (),
             seNumberOfTaps   = sdNumberOfTaps / sqrt (nNumberOfTaps))
par (mar = c (3, 5, 1, 1))
plot (x = jitter (production$year),
      y = production$numberOfTaps / 1000, pch = 19, col = addAlpha (colours [1], 96),
      xlab = '', ylab = 'Number of taps (thousands)', axes = FALSE, ylim = c (0.1, 240),
      log = 'y')
axis (side = 1, seq (2005, 2020, by = 5))
axis (side = 2, at = c (1, 5, 10, 20, 40, 80, 160), las = 1)
segments (x0 = 2004.5, x1 = 2021.5, y0 = mean (production$numberOfTaps / 1000), lwd = 3, 
          col = addAlpha (colours [3], 60))
segments (x0 = 2005:2021, 
          y0 = annualProduction$meanNumberOfTaps - 2*annualProduction$seNumberOfTaps, 
          y1 = annualProduction$meanNumberOfTaps + 2*annualProduction$seNumberOfTaps,
          lwd = 2, col = colours [2])
lines (x = 2005:2021, 
       y = annualProduction$meanNumberOfTaps, col = colours [2], lwd = 2)
dev.off ()

# number of taps over time
png ('../fig/numberOfTapsOverTimeBoxPlot.png', width = 700, height = 400)
boxplot (numberOfTaps / 1000 ~ year, data = production, ylim = c (0, 20), outpch = 19,
         outcol = addAlpha (colours [1], 90),
         col = addAlpha (colours [1], 30), ylab = 'Number of taps', las = 1, axes = FALSE)
axis (side = 1, labels = 2005:2021, at = 1:17)
axis (side = 2, at = seq (0, 20, by = 5), las = 1)
dev.off ()

# histogram of total production (proxy for company size)
producerSize <- production %>% group_by (uniqueID) %>% 
  summarise (meanTotalProduction = mean (totalProduction)) %>% 
  select (meanTotalProduction) %>% unlist ()
hist (producerSize / 1000, col = addAlpha (colours [1], 30), main = '', las = 1, lty = 0,
      xlab = 'Total production (thousands of pounds of syrup)', breaks = seq (0, 650, by = 2))
png ('../fig/meanTotalProductionCropped.png')
hist (producerSize / 1000, col = addAlpha (colours [1], 30), main = '', las = 1,
      xlab = 'Total production (thousands of pounds of syrup)', breaks = seq (0, 650, by = 1), 
      lty = 1, xlim = c (0, 60))
dev.off ()

# mean total production
annualProduction <- production %>% group_by (year) %>% 
  summarise (meanTotalProduction = mean (totalProduction) / 1000,
             sdTotalProduction   = sd   (totalProduction) / 1000,
             nTotalProduction    = n (),
             seTotalProduction   = sdTotalProduction / sqrt (nTotalProduction))
png ('../fig/totalProductionOverTime.png', width = 700, height = 400)
par (mar = c (3,5,1,1))
plot (x = annualProduction$year,
      y = annualProduction$meanTotalProduction, axes = FALSE, pch = 19,
      ylim = c (0, 30), ylab = 'Mean of total production (thousand pounds of syrup)',
      col = colours [1])
segments (x0 = 2005:2021, 
          y0 = annualProduction$meanTotalProduction - 3*annualProduction$seTotalProduction,
          y1 = annualProduction$meanTotalProduction + 3*annualProduction$seTotalProduction,
          col = colours [1])
axis (side = 1, at = 2005:2021)
axis (side = 2, at = seq (0, 30, by = 5), las = 1)
dev.off ()

# plot across-producer mean of mean production per tap over time
# plot sum of taps over time
# add fits to all data
#========================================================================================