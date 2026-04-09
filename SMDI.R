# Install and load necessary libraries

library(terra)

# Define input and output directories
input_dir <- "G:/FINAL USES DATA/New Data Collected/Soil Moisture/Soil_Moisture_New/"  # Input directory
output_dir <- "G:/FINAL USES DATA/New Data Collected/Soil Moisture/SMDI/"   # Output directory

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Load all soil moisture TIFF files as a raster stack
soil_moisture_files <- list.files(input_dir, pattern = "\\.tif$", full.names = TRUE)
soil_stack <- rast(soil_moisture_files)  # Create a raster stack

# Calculate long-term mean and standard deviation across all layers
mean_soil_moisture <- mean(soil_stack, na.rm = TRUE)
stddev_soil_moisture <- app(soil_stack, fun = sd, na.rm = TRUE)

# Function to calculate SMDI for each raster layer
calculate_smdi <- function(soil_layer, mean_layer, stddev_layer) {
  (soil_layer - mean_layer) / stddev_layer
}

# Loop through each layer and save the SMDI output with the same name
for (i in 1:nlyr(soil_stack)) {
  # Calculate SMDI for the current layer
  smdi <- calculate_smdi(soil_stack[[i]], mean_soil_moisture, stddev_soil_moisture)
  
  # Get the input file name without the path
  input_name <- basename(soil_moisture_files[i])
  
  # Construct the output file name (prefix "SMDI_" to the original file name)
  output_file <- file.path(output_dir, paste0("SMDI_", input_name))
  
  # Save the SMDI raster to the output directory
  writeRaster(smdi, output_file, overwrite = TRUE)
  
  # Print a message for each saved file
  cat("Saved:", output_file, "\n")
}

# Print completion message
cat("SMDI calculation completed. Files saved in:", output_dir, "\n")
