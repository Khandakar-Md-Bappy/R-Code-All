# Install and load the required library

library(terra)

# Define the input and output directories
input_folder <- "G:/FINAL USES DATA/ALL DATA/Precipitation Data/PAI/"
output_folder <- "G:/FINAL USES DATA/Reprojected_All data/Reprojected_PAI-500m/"

# Get a list of all TIFF files in the input folder
tif_files <- list.files(input_folder, pattern = "\\.tif$", full.names = TRUE)

# Define the target CRS (UTM Zone 48N for Ningxia Hui region)
utm_crs <- "EPSG:32648"  # UTM Zone 48N

# Loop through each TIFF file, reproject, and save with the same name in the output folder
for (tif_file in tif_files) {
  
  # Load the TIFF file
  raster_data <- rast(tif_file)
  
  # Reproject the raster to UTM Zone 48N with a resolution of 500 meters
  reprojected_raster <- project(raster_data, utm_crs, res=500)
  
  # Construct the output file path with the same file name as the input
  output_file <- file.path(output_folder, basename(tif_file))
  
  # Save the reprojected raster
  writeRaster(reprojected_raster, output_file, overwrite=TRUE)
  
  # Print confirmation
  print(paste("Reprojected and saved:", basename(tif_file)))
}
