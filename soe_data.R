### Learning to explore the SOE report data ### 
### 2/5/25 ###

library(tidyverse)
library(raster)

## Another method might be ecodata ## 
# https://github.com/NOAA-EDAB/ecodata 
remotes::install_github("noaa-edab/ecodata",build_vignettes=TRUE)

#### Max temperature at the sea bottom... 
#the idea here is to take the survey lat lons and then generate points from which to extract BT. 

# https://jorgemfa.medium.com/easy-to-use-marine-climate-layers-for-ecological-modelling-83950cd7a75f 
# Load package
library(sdmpredictors)

#example code
# Download data layer (Maximum Temperature at the sea bottom)
temp.max.bottom <- load_layers("BO2_tempmax_bdmax")
# Crop raster to fit the North Atlantic
ne.atlantic.ext <- extent(-100, 45, 30.75, 72.5)
temp.max.bottom.crop <- crop(temp.max.bottom, ne.atlantic.ext)
# Generate a nice color ramp and plot the map
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127"))
plot(temp.max.bottom.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(cex.sub = 1.25, sub = "Maximum temperature at the sea bottom (ºC)")

datasets <- list_datasets(terrestrial = FALSE, marine = TRUE)
View(datasets)
layers <- list_layers(datasets)
View(layers)

#BioOracle which is one of the sources for the sdm data is not providing annual data or data earlier than 2000. 
#Marspec is more promising. 
