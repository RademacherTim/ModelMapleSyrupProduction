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
hist (production$numberOfTaps)

# plot number of taps over time
annualProduction <- production %>% group_by (year) %>% 
  summarise (meanTotalProduction = mean (numberOfTaps) / 1000,
             sdTotalProduction   = sd   (numberOfTaps) / 1000,
             nTotalProduction    = n (),
             seTotalProduction   = sdTotalProduction / sqrt (nTotalProduction))
par (mar = c (3, 5, 1, 1))
plot (x = jitter (production$year),
      y = production$numberOfTaps / 1000, pch = 19, col = addAlpha (colours [1], 95),
      xlab = '', ylab = 'Number of taps (thousands)', axes = FALSE, ylim = c (0.1, 240),
      log = 'y')
axis (side = 1, seq (2005, 2020, by = 5))
axis (side = 2, at = c (1, 5, 10, 20, 40, 80, 160), las = 1)
segments (x0 = 2004.5, x1 = 2021.5, y0 = mean (production$numberOfTaps / 1000), lwd = 3, 
          col = addAlpha (colours [3], 60))
segments (x0 = 2005:2021, 
          y0 = annualProduction$meanTotalProduction - annualProduction$sdTotalProduction, 
          y1 = annualProduction$meanTotalProduction + annualProduction$sdTotalProduction,
          lwd = 2, col = colours [2])
lines (x = 2005:2021, 
       y = annualProduction$meanTotalProduction, col = colours [2], lwd = 2)
points (x = 2005:2021, 
        y = annualProduction$meanTotalProduction, col = colours [2], pch = 19)

#========================================================================================