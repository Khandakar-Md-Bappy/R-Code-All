install.packages("terra")
library(terra)
install.packages("phenops")
install.packages("LSTrends")
# You already have raster and terra installed
install.packages(c("terra", "raster", "sf", "dplyr", "ggplot2", "tidyr", "purrr", "lubridate", "stringr", "rlang", "units"))
library(terra)
library(greenbrown)
setwd("D:/Merge NDVI")
list.files()
# Load your NDVI stack
ndvi_stack <- rast("D:/Merge NDVI")

# Temporal smoothing (if needed)
ndvi_smooth <- smoothNDVI(ndvi_stack)

# Extract phenology metrics
phenology <- PhenoTrs(ndvi_smooth, method = "Deriv", trs = 0.2)  # 20% threshold

# Save phenology results
writeRaster(phenology$SOS, "sos_map.tif", format="GTiff")
writeRaster(phenology$EOS, "eos_map.tif", format="GTiff")
writeRaster(phenology$LOS, "los_map.tif", format="GTiff")
