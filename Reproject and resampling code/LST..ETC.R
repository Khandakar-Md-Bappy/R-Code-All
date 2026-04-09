# Install and load the required libraries
install.packages("terra")
library(terra)

# Define the input and output directories for VCI data
input_folder <- "G:/FINAL USES DATA/Data/LST_Monthly_-MODIS_061_MOD11A2/LST Data/LST_Mask_Monthly/"
output_folder <- "G:/FINAL USES DATA/Reprojected_All data/Reprojected_LST_500m/"

# Create the output folder if it doesn't exist
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# Get a list of all TIFF files in the input folder (filtering for 2001-2020)
tif_files <- list.files(input_folder, pattern = "\\.tif$", full.names = TRUE)

# Filter for the years 2001-2020
tif_files <- tif_files[grepl("200[1-9]|201[0-9]|2020", basename(tif_files))]

# Check if files are being loaded correctly
if (length(tif_files) == 0) {
  stop("No TIFF files found in the specified folder for the years 2001-2020. Please check the folder path and file names.")
}


# Define the target CRS (UTM Zone 48N for Ningxia Hui region)
utm_crs <- "EPSG:32648"  # UTM Zone 48N

# Define the target resolution (500 meters)
target_resolution <- 500

# Loop through each TIFF file, reproject, and save with the same name in the output folder
for (tif_file in tif_files) {
  
  # Load the TIFF file
  print(paste("Processing file:", tif_file))  # Print the name of the current file
  raster_data <- rast(tif_file)
  
  # Reproject the raster to UTM Zone 48N
  reprojected_raster <- project(raster_data, utm_crs, method = "bilinear")
  
  # Resample the raster to the target resolution of 500 meters
  resampled_raster <- resample(reprojected_raster, rast(ext(reprojected_raster), res = target_resolution))
  
  # Construct the output file path with the same file name as the input
  output_file <- paste0(output_folder, basename(tif_file))
  
  # Save the reprojected and resampled raster with the new resolution
  writeRaster(resampled_raster, output_file, overwrite = TRUE)
  
  # Print the file name and check the new CRS and resolution to confirm
  print(paste("Reprojected and saved:", basename(tif_file), "to", output_file))
  print(crs(resampled_raster))  # Check the CRS
  print(res(resampled_raster))  # Check the resolution
}

print("All files from 2001 to 2020 have been reprojected and resampled.")
