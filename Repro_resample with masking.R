# Install and load the required library
library(terra)

# Define the input and output directories
input_folder <- "G:/FINAL USES DATA/New Data Collected/SPI ALL/SPI_12/New folder/"
output_folder <- "G:/FINAL USES DATA/Reprojected_All data/Repo_Resam_SPI_3,6,12/SPI-12/"
boundary_shapefile <- "G:/Ningxia hui shp/Ningxia_Hui.shp"  # Path to the shapefile

# Get a list of all TIFF files in the input folder
tif_files <- list.files(input_folder, pattern = "\\.tif$", full.names = TRUE)

# Define the target CRS (UTM Zone 48N for Ningxia Hui region)
utm_crs <- "EPSG:32648"  # UTM Zone 48N

# Load the boundary shapefile
boundary <- vect(boundary_shapefile)

# Ensure the boundary shapefile is in the target CRS
boundary <- project(boundary, utm_crs)

# Loop through each TIFF file, reproject, mask, and save with the same name in the output folder
for (tif_file in tif_files) {
  
  # Load the TIFF file
  raster_data <- rast(tif_file)
  
  # Reproject the raster to UTM Zone 48N with a resolution of 500 meters
  reprojected_raster <- project(raster_data, utm_crs, res=500)
  
  # Mask the reprojected raster using the boundary shapefile
  masked_raster <- mask(reprojected_raster, boundary)
  
  # Construct the output file path with the same file name as the input
  output_file <- file.path(output_folder, basename(tif_file))
  
  # Save the masked raster
  writeRaster(masked_raster, output_file, overwrite=TRUE)
  
  # Print confirmation
  print(paste("Reprojected, masked, and saved:", basename(tif_file)))
}
