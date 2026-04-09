library(terra)
library(SPEI)

# Define paths
precip_path <- "G:/FINAL USES DATA/ALL DATA/CHRIPS/CHRIPS_Preci_Without_mask/"
pet_path    <- "G:/ML_Papers_Data/All_Index_Data/New_Data/Without_Mask/ERA5_PET/"
shapefile   <- "G:/Ningxia hui shp/Ningxia_Hui.shp"
output_path <- "G:/ML_Papers_Data/All_Index_Data/New_Data/With_Mask/SPEI-3/"

# Validate paths and files
if (!dir.exists(precip_path) || length(list.files(precip_path, pattern = "\\.tif$")) == 0) {
  stop("Precipitation path invalid or no .tif files found.")
}
if (!dir.exists(pet_path) || length(list.files(pet_path, pattern = "\\.tif$")) == 0) {
  stop("PET path invalid or no .tif files found.")
}
if (!file.exists(shapefile)) stop("Shapefile not found.")
if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)

# Load shapefile
ningxia <- vect(shapefile)
if (!is.valid(ningxia)) {
  ningxia <- makeValid(ningxia)
  cat("Shapefile geometry repaired.\n")
}

# Load raster file paths
prec_files <- sort(list.files(precip_path, pattern = "\\.tif$", full.names = TRUE))
pet_files  <- sort(list.files(pet_path, pattern = "\\.tif$", full.names = TRUE))

# Load rasters
prec_stack <- rast(prec_files)
pet_stack  <- rast(pet_files)

# Validate temporal consistency
date_seq <- seq(as.Date("2001-01-01"), as.Date("2020-12-01"), by = "month")
if (length(prec_files) != length(date_seq) || length(pet_files) != length(date_seq)) {
  stop("Number of raster files does not match expected time period (2001–2020).")
}

# Check CRS alignment
if (crs(ningxia) != crs(prec_stack)) {
  ningxia <- project(ningxia, crs(prec_stack))
  cat("Shapefile projected to match raster CRS.\n")
}

# Check geometry alignment
if (!compareGeom(prec_stack, pet_stack, stopOnError = FALSE)) {
  cat("Warning: Precipitation and PET rasters have different geometries. Resampling PET.\n")
}

# Resample PET to match precipitation
pet_stack <- resample(pet_stack, prec_stack, method = "bilinear")

# Set layer names (YYYY_MM)
layer_names <- format(date_seq, "%Y_%m")
names(prec_stack) <- layer_names
names(pet_stack)  <- layer_names

# Crop and mask to Ningxia Hui
prec_crop <- mask(crop(prec_stack, ningxia), ningxia)
pet_crop  <- mask(crop(pet_stack, ningxia), ningxia)

# Calculate water balance
wb <- prec_crop - pet_crop

# Function to calculate SPEI-3
calc_spei3 <- function(ts) {
  if (all(is.na(ts)) || sum(!is.na(ts)) < 12) return(rep(NA, length(ts)))
  out <- tryCatch({
    spei(ts, scale = 3)$fitted
  }, error = function(e) {
    rep(NA, length(ts))
  })
  return(out)
}

# Apply SPEI-3 calculation
terraOptions(memfrac = 0.8)  # Optimize memory usage
spei3_stack <- app(wb, calc_spei3)

# Assign names to output stack
names(spei3_stack) <- layer_names

# Export monthly GeoTIFFs
for (i in 1:nlyr(spei3_stack)) {
  month_name <- names(spei3_stack)[i]
  out_file <- paste0(output_path, "SPEI3_", month_name, ".tif")
  writeRaster(spei3_stack[[i]], filename = out_file, overwrite = TRUE)
  cat("Exported:", out_file, "\n")
}