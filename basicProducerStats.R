#========================================================================================
# Script to explore the producer data by region and see the impact of using all data or 
# only data with certainty.
#----------------------------------------------------------------------------------------

# source production data and make sure it was geo-referenced
#----------------------------------------------------------------------------------------
if (!exists ('production')) source ('matchClimAndProdData.R') 
if (!existsFunction ('%>%')) library ('tidyverse')


# colour palette for the regions 
#----------------------------------------------------------------------------------------
regionColours <- c ('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462',
                    '#b3de69','#fccde5','#d9d9d9','#bc80bd','#ccebc5','#ffed6f','#666666')

# create subset of data with only entries that have no comments (i.e., are certain in geo-referencing) 
#----------------------------------------------------------------------------------------
productionSub <- production %>% filter (is.na (comment))
dim (productionSub) [1]
dim (production) [1] - dim (productionSub) [1]

# plot number of producers by region over the years
#----------------------------------------------------------------------------------------
producersByRegion <- productionSub %>% count (region, year)
regions <- unique (producersByRegion$region) [which (!is.na (unique (producersByRegion$region)))]
max (producersByRegion$n)
png (file = '../fig/numberOfProducersByRegionOverTime.png', width = 700, height = 400)
par (mfrow = c (2, 1))
par (mar = c (0, 5, 1, 1))
plot (NULL, xlim = c (2005, 2021), ylim = c (2200, 3300), xlab = '', ylab = '',
      axes = FALSE)
axis (side = 2, las = 1)
for (r in regions) {
  tmp <- producersByRegion %>% filter (region == r)
  lines (x = tmp$year, y = tmp$n, lwd = 2, col = regionColours [which (regions == r)])
}
legend (y = 3000, x = 2015, box.lty = 0, 
        col = regionColours [c (5, 6, 4, 2, 11, 3, 8, 9, 10, 7, 12, 1, 13)], 
        legend = regions [c (5, 6, 4, 2, 11, 3, 8, 9, 10, 7, 12, 1, 13)], lty = 1, 
        lwd = 2, bg = 'transparent', cex = 0.6)
par (mar = c (3, 5, 0, 1))
plot (NULL, xlim = c (2005, 2021), ylim = c (0, 1100), xlab = '', ylab = '',
      axes = FALSE)
axis (side = 1)
axis (side = 2, las = 1)
for (r in regions) {
  tmp <- producersByRegion %>% filter (region == r)
  lines (x = tmp$year, y = tmp$n, lwd = 2, col = regionColours [which (regions == r)])
}
mtext (side = 2, line = 3.5, at = 1200, text = 'Number of producers')
dev.off ()

# Plot number of taps by region
#----------------------------------------------------------------------------------------
tapsByRegion <- productionSub %>% group_by (region, year) %>% 
  summarise (nTaps = sum (numberOfTaps), .groups = 'drop')
png (file = '../fig/numberOfTapsByRegionOverTime.png', width = 700, height = 400)
par (mfrow = c (2, 1))
par (mar = c (0, 5, 1, 1))
plot (NULL, xlim = c (2005, 2021), ylim = c (12, 18), xlab = '', ylab = '',
      axes = FALSE)
axis (side = 2, las = 1)
for (r in regions) {
  tmp <- tapsByRegion %>% filter (region == r)
  lines (x = tmp$year, y = tmp$nTaps / 1e6, lwd = 2, col = regionColours [which (regions == r)])
}
legend (y = 15.5, x = 2015, box.lty = 0, 
        col = regionColours [c (5, 6, 4, 2, 11, 3, 8, 9, 10, 7, 12, 1, 13)], 
        legend = regions [c (5, 6, 4, 2, 11, 3, 8, 9, 10, 7, 12, 1, 13)], lty = 1, 
        lwd = 2, bg = 'transparent', cex = 0.5)
par (mar = c (3, 5, 0, 1))
plot (NULL, xlim = c (2005, 2021), ylim = c (0, 8.5), xlab = '', ylab = '',
      axes = FALSE)
axis (side = 1)
axis (side = 2, las = 1)
for (r in regions) {
  tmp <- tapsByRegion %>% filter (region == r)
  lines (x = tmp$year, y = tmp$nTaps / 1e6, lwd = 2, col = regionColours [which (regions == r)])
}
mtext (side = 2, line = 3.5, at = 9, text = 'Number of taps (millions)')
dev.off ()

# Total production by region over time
#----------------------------------------------------------------------------------------
productionByRegion <- productionSub %>% group_by (region, year) %>% 
  summarise (totalProduction = sum (totalProduction), .groups = 'drop')
png (file = '../fig/totalProductionByRegionOverTime.png', width = 700, height = 400)
par (mfrow = c (1, 1))
par (mar = c (3, 5, 1, 1))
plot (NULL, xlim = c (2005, 2021), ylim = c (0, 54), xlab = '',
      axes = FALSE, ylab = 'Total production (millions of pounds of syrup)')
axis (side = 2, las = 1)
axis (side = 1)
for (r in regions) {
  tmp <- productionByRegion %>% filter (region == r)
  lines (x = tmp$year, y = tmp$totalProduction / 1e6, lwd = 2, col = regionColours [which (regions == r)])
}
legend (y = 52, x = 2004.5, box.lty = 0, 
        col = regionColours [c (5, 6, 4, 2, 11, 3, 8, 9, 10, 7, 12, 1, 13)], 
        legend = regions [c (5, 6, 4, 2, 11, 3, 8, 9, 10, 7, 12, 1, 13)], lty = 1, 
        lwd = 2, bg = 'transparent', cex = 0.7)
dev.off ()

# Mean production by region over time
#----------------------------------------------------------------------------------------
productionByRegion <- productionSub %>% group_by (region, year) %>% 
  summarise (sdProduction = sd (meanProduction, na.rm = TRUE), 
             meanProduction = mean (meanProduction, na.rm = TRUE), 
             .groups = 'drop')
png (file = '../fig/meanProductionByRegionOverTime.png', width = 700, height = 400)
par (mfrow = c (1, 1))
par (mar = c (3, 5, 1, 1))
plot (NULL, xlim = c (2005, 2021), ylim = c (0, 5), xlab = '',
      axes = FALSE, ylab = 'Mean production (pounds of syrup per tap)')
axis (side = 2, las = 1)
axis (side = 1)
for (r in regions) {
  tmp <- productionByRegion %>% filter (region == r)
  polygon (x = c (2005:2021, 2021:2005),
           y = c (tmp$meanProduction - tmp$sdProduction, 
                  rev (tmp$meanProduction + tmp$sdProduction)),
           col = addAlpha (regionColours  [which (regions == r)], 95), 
           lty = 0)
  lines (x = tmp$year, y = tmp$meanProduction, lwd = 2, col = regionColours [which (regions == r)])
}
globalProduction <- productionSub %>% group_by (year) %>% 
  summarise (sdProduction = sd (meanProduction, na.rm = TRUE), 
             meanProduction = mean (meanProduction, na.rm = TRUE), 
             .groups = 'drop')
# polygon (x = c (2005:2021, 2021:2005),
#          y = c (globalProduction$meanProduction - globalProduction$sdProduction, 
#                 rev (globalProduction$meanProduction + globalProduction$sdProduction)),
#          col = addAlpha ('#999999', 95), 
#          lty = 0)
lines (x = 2005:2021, globalProduction$meanProduction, lwd = 4, col = '#999999')
legend (y = 5, x = 2004.5, box.lty = 0, 
        col = regionColours [c (5, 6, 4, 2, 11, 3, 8, 9, 10, 7, 12, 1)], 
        legend = regions [c (5, 6, 4, 2, 11, 3, 8, 9, 10, 7, 12, 1)], lty = 1, 
        lwd = 2, bg = 'transparent', cex = 0.6)
dev.off ()

# Plot number of taps per producer by region over time
#----------------------------------------------------------------------------------------
tapsByRegion <- productionSub %>% group_by (region, year) %>% 
  summarise (nTaps = mean (numberOfTaps, na.rm = TRUE) / 1e3, 
             n = n_distinct (uniqueID),
             seTaps = (sd (numberOfTaps, na.rm = TRUE) / sqrt (n)) / 1e3, .groups = 'drop')
png (file = '../fig/numberOfTapsPerProducerByRegionOverTime.png', width = 700, height = 400)
par (mfrow = c (1, 1))
par (mar = c (3, 5, 1, 1))
plot (NULL, xlim = c (2005, 2021), ylim = c (0, 35), xlab = '', ylab = '',
      axes = FALSE)
axis (side = 1)
axis (side = 2, las = 1)
for (r in regions) {
  tmp <- tapsByRegion %>% filter (region == r)
  if (r != tail (regions, 1)) {
    polygon (x = c (2005:2021, 2021:2005),
           y = c (tmp$nTaps - tmp$seTaps, 
                  rev (tmp$nTaps + tmp$seTaps)),
           col = addAlpha (regionColours  [which (regions == r)], 80), 
           lty = 0)
  }
  lines (x = tmp$year, y = tmp$nTaps, lwd = 2, col = regionColours [which (regions == r)])
}
globalTaps <- productionSub %>% group_by (year) %>% 
  summarise (nTaps = mean (numberOfTaps, na.rm = TRUE) / 1e3, 
             n = n_distinct (uniqueID),
             seTaps = (sd (numberOfTaps, na.rm = TRUE) / sqrt (n)) / 1e3, .groups = 'drop')
#lines (x = 2005:2021, globalTaps$nTaps, lwd = 3, col = '#999999')
legend (y = 35, x = 2005, box.lty = 0, 
        col = regionColours [c (5, 6, 4, 2, 11, 3, 8, 9, 10, 7, 12, 1, 13)], 
        legend = regions [c (5, 6, 4, 2, 11, 3, 8, 9, 10, 7, 12, 1, 13)], lty = 1, 
        lwd = 2, bg = 'transparent', cex = 0.6)
mtext (side = 2, line = 3.5, text = 'Number of taps per producers (thousands)')
dev.off ()

# Make map of all producers
#----------------------------------------------------------------------------------------

#========================================================================================

