# Install required packages
install.packages("raster")
install.packages("terra")

# Load required library
# Load required library
library(terra)
# Load necessary libraries
library(raster)
library(dplyr)

# Define input and output directories
input_dir <- "G:/FINAL USES DATA/Precipitation Mask data/"
output_dir_tif <- "G:/FINAL USES DATA/Data/Output_PAI_TIFF/"
output_dir_csv <- "G:/FINAL USES DATA/Data/Output_PAI_CSV/"

# Set threshold for precipitation in mm/day
threshold <- 1

# Get a list of all the TIFF files in the input directory (files from 2001 to 2021)
precip_files <- list.files(input_dir, pattern = "*.tif$", full.names = TRUE)

# Loop through each file (representing a time period like a year or month)
for (file in precip_files) {
  # Load the precipitation data for the current file
  precip_data <- raster(file)
  
  # Create binary raster (1 for precipitation >= threshold, 0 for less)
  precip_binary <- precip_data >= threshold
  
  # Extract the year/month from the filename (you may need to adjust this depending on your file naming convention)
  # For example, if filenames are like "precip_2001.tif", you can extract the year as follows:
  file_name <- basename(file)
  year <- sub(".*_(\\d{4}).tif", "\\1", file_name)
  
  # Construct the output file paths
  output_file_tif <- file.path(output_dir_tif, paste0("PAI_", year, ".tif"))
  output_file_csv <- file.path(output_dir_csv, paste0("PAI_", year, ".csv"))
  
  # Save the binary precipitation mask to a TIFF file
  writeRaster(precip_binary, output_file_tif, format = "GTiff", overwrite = TRUE)
  
  # Optional: If you want to calculate PAI as percentage, do the following:
  # Calculate the total area covered by precipitation (PAI in square meters)
  cell_area <- res(precip_data)[1] * res(precip_data)[2]  # Assuming square grid cells in meters
  
  # Count the number of cells with precipitation above the threshold
  num_precip_cells <- sum(values(precip_binary), na.rm = TRUE)
  
  # Calculate total number of cells (for normalization purposes)
  total_cells <- ncell(precip_data)
  
  # Calculate the PAI as a percentage (optional)
  pai_percentage_raster <- precip_binary * (num_precip_cells / total_cells) * 100
  
  # Save the PAI percentage raster as a TIFF file
  writeRaster(pai_percentage_raster, output_file_tif, format = "GTiff", overwrite = TRUE)
  
  # Optional: Save PAI as a CSV file with cell values (if needed)
  # Convert the raster to a data frame
  raster_values <- as.data.frame(precip_binary, xy = TRUE)
  write.csv(raster_values, output_file_csv, row.names = FALSE)
  
  # Print progress
  cat("Processed file:", file_name, "\n")
  cat("Saved PAI output as TIFF to:", output_file_tif, "\n")
  cat("Saved PAI output as CSV to:", output_file_csv, "\n")
}

# End of loop
cat("All files processed successfully.\n")
