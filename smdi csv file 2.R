# Step 1: Install necessary packages (if not already installed)
install.packages(c("terra", "dplyr"))

# Step 2: Load libraries
library(terra)
library(dplyr)

# Step 3: Set the path to your directory containing TIFF files
tif_directory <- "G:/FINAL USES DATA/New Data Collected/Soil Moisture/SMDI//"

# Step 4: List all TIFF files in the directory
tif_files <- list.files(path = tif_directory, pattern = "\\.tif$", full.names = TRUE)

# Step 5: Load all soil moisture TIFF files into a SpatRaster
soil_moisture_raster <- rast(tif_files)

# Step 6: Create a data frame to store results
dates <- seq(as.Date("2001-01-01"), as.Date("2020-12-31"), by = "months")
smdi_results <- data.frame(Date = dates)

# Step 7: Loop through each layer to calculate mean soil moisture
for (i in 1:nlyr(soil_moisture_raster)) {
  layer <- soil_moisture_raster[[i]]
  
  # Calculate the mean soil moisture across all pixels
  mean_soil_moisture <- global(layer, fun = 'mean', na.rm = TRUE)$mean
  
  # Store the result
  smdi_results[i, "Mean_Soil_Moisture"] <- mean_soil_moisture
}

# Step 8: Calculate SMDI
mean_sm <- mean(smdi_results$Mean_Soil_Moisture, na.rm = TRUE)
sd_sm <- sd(smdi_results$Mean_Soil_Moisture, na.rm = TRUE)

# Calculate SMDI
smdi_results$SMDI <- (smdi_results$Mean_Soil_Moisture - mean_sm) / sd_sm

# Step 9: Export the results to a CSV file
output_csv_path <- "G:/FINAL USES DATA/SMDI_Output/smdi_results_2001_2020.csv"
write.csv(smdi_results, output_csv_path, row.names = FALSE)

# Optional: View the results
print(smdi_results)



##

