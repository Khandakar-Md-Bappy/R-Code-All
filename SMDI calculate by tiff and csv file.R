# Step 1: Install necessary packages (if not already installed)
install.packages(c("terra", "dplyr"))

library(terra)
library(dplyr)


# Define the input and output directories
input_folder <- "G:/FINAL USES DATA/Reprojected_All data/Reprojected_SM_250m/"
csv_output_folder <- "G:/FINAL USES DATA/SMDI_Output/"
tif_output_folder <- "G:/FINAL USES DATA/SMDI_TIFF_Output/"

# Create subfolders for each phenology stage
sos_folder <- paste0(csv_output_folder, "SOS/")
los_folder <- paste0(csv_output_folder, "LOS/")
eos_folder <- paste0(csv_output_folder, "EOS/")

# Create directories for TIFF output
dir.create(sos_folder, showWarnings = FALSE, recursive = TRUE)
dir.create(los_folder, showWarnings = FALSE, recursive = TRUE)
dir.create(eos_folder, showWarnings = FALSE, recursive = TRUE)
dir.create(tif_output_folder, showWarnings = FALSE, recursive = TRUE)

# Step 3: Get a list of all TIFF files in the input folder
tif_files <- list.files(input_folder, pattern = "\\.tif$", full.names = TRUE)

# Initialize a vector to store mean soil moistures
mean_soil_moistures <- numeric(length(tif_files))

# Ensure to process only the available files
for (i in seq_along(tif_files)) {
  raster_data <- rast(tif_files[i])
  
  # Calculate mean soil moisture using the global function
  mean_soil_moisture <- global(raster_data, fun = 'mean', na.rm = TRUE)$mean
  mean_soil_moistures[i] <- mean_soil_moisture
}

# Create a data frame with dates from 2001 to 2020
date_seq <- seq(as.Date("2001-01-01"), as.Date("2020-12-31"), by = "months")
smdi_results <- data.frame(Date = date_seq, Mean_Soil_Moisture = mean_soil_moistures)

# Step 6: Calculate SMDI
mean_sm <- mean(smdi_results$Mean_Soil_Moisture, na.rm = TRUE)
sd_sm <- sd(smdi_results$Mean_Soil_Moisture, na.rm = TRUE)
smdi_results$SMDI <- (smdi_results$Mean_Soil_Moisture - mean_sm) / sd_sm

# Save individual monthly SMDI output as TIFF files
for (i in 1:nrow(smdi_results)) {
  # Create a raster for each month based on the extent of the first raster
  base_raster <- rast(tif_files[1])  # Use the first raster to get extent and CRS
  monthly_raster <- base_raster
  values(monthly_raster) <- smdi_results$SMDI[i]  # Assign SMDI value
  
  # Format the date for the filename
  date_str <- format(smdi_results$Date[i], "%Y-%m")
  
  # Save the monthly SMDI raster as a TIFF file
  writeRaster(monthly_raster, filename = paste0(tif_output_folder, "smdi_results_", date_str, ".tif"), overwrite = TRUE)
}

# Print message confirming completion
print("Processing completed and outputs saved in respective folders.")
