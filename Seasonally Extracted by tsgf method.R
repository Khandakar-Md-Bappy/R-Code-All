install.packages("fs")

# Load required library
library(fs) # For file system operations

# Define input folders
#input_folders <- c(
  "path/to/VCI",
  "path/to/TCI",
  "path/to/VHI",
  "path/to/SMDI",
  "path/to/EDI",
  "path/to/PAI",
  "path/to/PCI",
  "path/to/SPI_3",
  "path/to/SPI_6",
  "path/to/SPEI_3",
  "path/to/SPEI_6"
#)

# Define output folder
#output_folder <- "path/to/combined_folder/"

# Create output folder if it does not exist
#if (!dir.exists(output_folder)) {
  #dir.create(output_folder, recursive = TRUE)
#}

# Loop through each input folder and copy files to the output folder
#for (folder in input_folders) {
  # Get list of files in the folder
  #files <- list.files(folder, pattern = "\\.tif$", full.names = TRUE)
  
  # Copy files to output folder
#  file_copy(files, output_folder, overwrite = TRUE)
#}

# Print message
#cat("All files have been copied to:", output_folder)





#How to separte the data by seasonally by tsgf (temporal smoothing and gap-filling) method.
# Libraries and Setup
library(greenbrown)  # For phenology metrics
library(raster)      # For handling raster data
library(zoo)         # For time series manipulation
library(terra)       # For efficient raster operations

# Set paths
ndvi_path <- "path/to/ndvi/tiff/files/"  # NDVI TIFF files
drought_indices_path <- "path/to/drought/indices/tiff/files/"  # Drought indices TIFF files
output_path <- "path/to/output/phenology/"

#NDVI Processing and Phenology Extraction
# Load NDVI data
ndvi_stack <- stack(list.files(ndvi_path, pattern = "\\.tif$", full.names = TRUE))

# Temporal smoothing and gap-filling
ndvi_smoothed <- greenbrown::SmoothNDVI(ndvi_stack, method = "TSGF")

# Calculate phenology metrics
phenology <- Phenology(ndvi_smoothed, method = "Deriv")

# Save SOS, EOS, LOS
writeRaster(phenology$SOS, file.path(output_path, "SOS.tif"), overwrite = TRUE)
writeRaster(phenology$EOS, file.path(output_path, "EOS.tif"), overwrite = TRUE)
writeRaster(phenology$LOS, file.path(output_path, "LOS.tif"), overwrite = TRUE)

#Drought Indices Analysis
# Load drought indices
drought_files <- list.files(drought_indices_path, pattern = "\\.tif$", full.names = TRUE)
drought_stack <- stack(drought_files)

# Define phenological stages
stages <- list(
  SOS = c(4, 5),  # April-May
  LOS = c(5, 8),  # May-August
  EOS = c(9, 10)  # September-October
)

# Extract drought values for each stage
stage_indices <- lapply(names(stages), function(stage) {
  months <- stages[[stage]]
  drought_stage <- drought_stack[[months]]
  mean_drought <- calc(drought_stage, mean, na.rm = TRUE)
  writeRaster(mean_drought, file.path(output_path, paste0(stage, "_Drought.tif")), overwrite = TRUE)
  return(mean_drought)
})

# Save stage indices
names(stage_indices) <- names(stages)
saveRDS(stage_indices, file = file.path(output_path, "drought_indices_by_stage.RDS"))


