#========================================================================================
# Script to read maple syrup producer data provided 
#
# Questions:
# - Would it be possible to get more data on grades of syrup?
# - Does the production include all production or the contingent supplied to the PPAQ? 
#----------------------------------------------------------------------------------------

# load dependencies 
#----------------------------------------------------------------------------------------
if (!existsFunction ('read_excel')) library ('readxl')
if (!existsFunction ('%>%')) library ('tidyverse')
if (!existsFunction ('brewer.pal')) library ('RColorBrewer')

# define some fun colours and a function to add opacity to a colour
#----------------------------------------------------------------------------------------
colours <- c ('#CC7240','#94452E','#B8AB9E')
palette <- colorRampPalette (brewer.pal (9, 'YlOrBr'))
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

# read production data file 
#----------------------------------------------------------------------------------------
production <- readxl::read_excel (path = '../data/PPAQ/20211105_MR_InfoProd_cleaned_provisional.xlsx',
                                  sheet = '20211105_MR_InfoProd_cleaned',
                                  col_names = c ('uniqueID',        # unique identifier for producer
                                                 'year',            # calendar year AD
                                                 'PPAQstatus',      # status of PPAQ membership
                                                 'numberOfTaps',    # total number of taps for the year 
                                                 'totalProduction', # total production [lbs of syrup]
                                                 'municipalityRaw', # municipality of the sugar bush
                                                 'municipality',    # corrected municipality of the sugar bush can be found   
                                                 'region',          # adimistrative region of Québec
                                                 'comment'),        # comment on the data 
                                  skip = 1)

# calculate mean production per tap (lb of syrup per tap)
# NB: a tree can have multiple taps this is necessarily a conservative estimate of mean 
# production per tree
#----------------------------------------------------------------------------------------
production <- production %>% 
  mutate (meanProduction = totalProduction / numberOfTaps)
production [which (is.infinite (production$meanProduction)), ]
# 788 instances the number of taps is zero but production was above zero,
# I assume the number of taps was simply not reported and set it to NA in the following.
# NB - I could investigate the imputation of those numbers
production$meanProduction [which (is.infinite (production$meanProduction))] <- NA
production [which (production$totalProduction == 0), ]
# 6422 instances of no total production, which need to be removed to avoid bias in region means
production$meanProduction [which (production$totalProduction == 0)] <- NA

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

# remove the instances of no reported tapping
production <- production [-which (production$numberOfTaps == 0), ]

# how many people reported less than 100 taps?
sum (production$numberOfTaps < 100)
# 799 instances of less than 100 taps

# how many people reported no production?
sum (production$totalProduction == 0)
# 6422 instances of no production
# 0 instances of negative total production

# remove instances of no production sold to PPAQ
production <- production [-which (production$totalProduction == 0), ]

# create a new column with production in liters 
# Unit conversions according to MAPAQ (26th of November): https://www.mapaq.gouv.qc.ca/fr/Publications/Unitesconversion.pdf
# Duchesne et Houle (2014) used 344 ml of syrup per pound from a government report of Canada called "Canadian Maple Products Situation and Trends 2006-2007"
production$totalP <- production$totalProduction * 0.3431532 
production$meanP <- production$meanProduction * 0.3431532

# plot number of taps to get an idea of company size
PLOT <- FALSE
if (PLOT) {
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
    summarise (meanNumberOfTaps = mean (numberOfTaps) / 100,
               sdNumberOfTaps   = sd   (numberOfTaps) / 100,
               nNumberOfTaps    = n (),
               seNumberOfTaps   = sdNumberOfTaps / sqrt (nNumberOfTaps))
  par (mar = c (3, 5, 1, 1))
  plot (x = jitter (production$year),
        y = production$numberOfTaps / 100, pch = 19, col = addAlpha (colours [1], 96),
        xlab = '', ylab = 'Number of taps (hundreds)', axes = FALSE, ylim = c (1, 2400),
        log = 'y')
  axis (side = 1, seq (2005, 2020, by = 5))
  axis (side = 2, at = c (1, 10, 50, 100, 200, 400, 800, 1600), las = 1)
  segments (x0 = 2004.5, x1 = 2021.5, y0 = mean (production$numberOfTaps / 100), lwd = 3, 
            col = addAlpha (colours [3], 60))
  segments (x0 = 2005:2021, 
            y0 = rep (1, 17), 
            y1 = annualProduction$meanNumberOfTaps + annualProduction$sdNumberOfTaps,
            lwd = 1.5, col = colours [2])
  lines (x = 2005:2021, 
         y = annualProduction$meanNumberOfTaps, col = colours [2], lwd = 2)
  dev.off ()
  
  # number of taps over time
  png ('../fig/numberOfTapsOverTimeBoxPlot.png', width = 700, height = 400)
  boxplot (numberOfTaps / 1000 ~ year, data = production, ylim = c (0, 20), outpch = 19,
           outcol = addAlpha (colours [1], 90),
           col = addAlpha (colours [1], 30), ylab = 'Number of taps (tohusands)', 
           las = 1, axes = FALSE)
  axis (side = 1, labels = 2005:2021, at = 1:17)
  axis (side = 2, at = seq (0, 20, by = 5), las = 1)
  dev.off ()
  
  # histogram of mean total production by producers over time (proxy for company size)
  producerSize <- production %>% group_by (uniqueID) %>% 
    summarise (meanTotalProduction = mean (totalProduction)) %>% 
    select (meanTotalProduction) %>% unlist ()
  hist (producerSize / 100, col = addAlpha (colours [1], 30), main = '', las = 1, lty = 0,
        xlab = 'Total production (hundreds of pounds of syrup)', breaks = seq (0, 6500, by = 2))
  png ('../fig/meanTotalProductionCropped.png')
  par (mar = c (5, 3, 1, 1))
  hist (producerSize / 100, col = addAlpha (colours [1], 30), main = '', las = 1,
        xlab = 'Total production (hundreds of pounds of syrup)', breaks = seq (0, 6500, by = 1), 
        lty = 0, xlim = c (0, 600))
  dev.off ()
  
  # plot mean production
  annualProduction <- production %>% group_by (year) %>% 
    summarise (meanTotalProduction = mean (totalProduction) / 1000,
               sdTotalProduction   = sd   (totalProduction) / 1000,
               nTotalProduction    = n (),
               seTotalProduction   = sdTotalProduction / sqrt (nTotalProduction))
  png ('../fig/meanProductionOverTime.png', width = 700, height = 400)
  par (mar = c (3,5,1,1))
  plot (x = annualProduction$year,
        y = annualProduction$meanTotalProduction, axes = FALSE, pch = 19,
        ylim = c (0, 30), ylab = 'Mean of production (thousand pounds of syrup)',
        col = colours [1])
  segments (x0 = 2005:2021, 
            y0 = annualProduction$meanTotalProduction - 3*annualProduction$seTotalProduction,
            y1 = annualProduction$meanTotalProduction + 3*annualProduction$seTotalProduction,
            col = colours [1])
  axis (side = 1, at = 2005:2021)
  axis (side = 2, at = seq (0, 30, by = 5), las = 1)
  dev.off ()
  
  # plot sum of taps over time
  numberOfTaps <- production %>% group_by (year) %>% 
    summarise (sumNumberOfTaps = sum (numberOfTaps),
               sdNumberOfTaps = sd (numberOfTaps, na.rm = TRUE), 
               .groups = 'drop')
  png ('../fig/numberOfTapsOverTime.png', width = 700, height = 400)
  par (mar = c (3,5,1,1))
  plot (x = numberOfTaps$year,
        y = numberOfTaps$sumNumberOfTaps / 1e6,
        ylab = 'Number of taps (millions)', las = 1, ylim = c (30, 50), axes = FALSE)
  lines (x = numberOfTaps$year,
         y = numberOfTaps$sumNumberOfTaps / 1e6, col = colours [1])
  polygon (x = c (numberOfTaps$year, rev (numberOfTaps$year)),
           y = c ((numberOfTaps$sumNumberOfTaps - numberOfTaps$sdNumberOfTaps) / 1e6, 
                  rev ((numberOfTaps$sumNumberOfTaps + numberOfTaps$sdNumberOfTaps) / 1e6)),
           col = addAlpha (colours [1], 80), lty = 0)
  points (x = numberOfTaps$year,
          y = numberOfTaps$sumNumberOfTaps / 1e6, col = colours [1], pch = 19)
  axis (side = 1)
  axis (side = 2, las = 1)
  dev.off ()
  
  # plot across-producer mean of mean production per tap over time
  meanAnnualProduction <- production %>% group_by (year) %>%
    summarise (meanAnnualProduction = mean (meanProduction, na.rm = TRUE), 
               sdAnnualProduction = sd (meanProduction, na.rm = TRUE), .groups = 'drop')
  png ('../fig/meanProductionOverTime.png', width = 700, height = 400)
  par (mar = c (3, 5, 1, 1))
  plot (x = meanAnnualProduction$year,
        y = meanAnnualProduction$meanAnnualProduction,
        ylab = 'Mean production (pounds of syrup per tap)', las = 1, ylim = c (0, 5), axes = FALSE)
  lines (x = meanAnnualProduction$year,
         y = meanAnnualProduction$meanAnnualProduction, col = colours [1])
  polygon (x = c (meanAnnualProduction$year, rev (meanAnnualProduction$year)),
           y = c (meanAnnualProduction$meanAnnualProduction - meanAnnualProduction$sdAnnualProduction, 
                  rev (meanAnnualProduction$meanAnnualProduction + meanAnnualProduction$sdAnnualProduction)),
           col = addAlpha (colours [1], 80), lty = 0)
  points (x = meanAnnualProduction$year,
          y = meanAnnualProduction$meanAnnualProduction, col = colours [1], pch = 19)
  axis (side = 1)
  axis (side = 2, las = 1)
  dev.off ()
  
  # plot annual histograms of number of taps for each producer
  png ('../fig/annualNumberOfTapsDistribution.png', width = 700, height = 400)
  par (mar = c (5, 6, 1, 1))
  plot (NULL,
        xlim = c (0, 30000), ylim = c (-0.000015, 0.0002), axes = FALSE, main = '', 
        col = palette (17) [1], ylab = 'Density (‰)', xlab = 'Number of taps')
  abline (h = 0)
  abline (h = -0.00001, col = '#88888888')
  for (y in 2005:2021) {
    lines (x = density (production$numberOfTaps [production$year == y]),
           col = palette (17) [y - 2004])
    points (x = mean (production$numberOfTaps [production$year == y]),
            y = -0.00001,
            col = palette (17) [y - 2004],
            cex = 1, pch = 19)
  }
  legend (x = 25000, y = 0.0002, legend = 2005:2021, lty = 1, col = palette (17), 
          box.lty = 0, cex = 0.85)
  axis (side = 1)
  axis (side = 2, las = 1, at = seq (0, 0.0002, len = 5), labels = c (0.0, 0.05, 0.10, 0.15, 0.20))
  dev.off ()
  
  # plot annual production over time
  png ('../fig/totalProductionOverTime.png', width = 700, height = 400)
  par (mar = c (3, 5, 1, 1))
  annualProduction <- production %>% group_by (year) %>% 
    summarise (sumProduction = sum (totalProduction))
  plot (x = annualProduction$year,
        y = annualProduction$sumProduction, pch = 19, col = colours [1],
        ylim = c (40.0e6, 150.0e6), axes = FALSE, ylab = 'Total production (millions of pounds of syrup)')
  lines (x = annualProduction$year,
         y = annualProduction$sumProduction, pch = 19, col = colours [1])
  axis (side = 1)
  axis (side = 2, at = seq (50.0e6, 150.0e6, by = 10e6), labels = seq (50, 150, by = 10), las = 1)
  linearFit <- lm (sumProduction ~ year, data = annualProduction)
  abline (linearFit, col = colours [3], lwd = 2)
  summary (linearFit)
  dev.off ()
}

# What is the largest producer?
production [which (max (production$numberOfTaps) == production$numberOfTaps), ]
#========================================================================================