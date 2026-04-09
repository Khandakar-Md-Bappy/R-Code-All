# Load required libraries
library(raster)

# Directory for input (precipitation data) and output (PAI result)
precip_dir <- "G:/FINAL USES DATA/Precipitation Mask data/"  # Directory containing your .tif files
output_dir_tif <- "G:/FINAL USES DATA/Precipitation Anomaly Index/"  # Directory to save output PAI files

# Ensure the output directory exists (create if necessary)
if (!dir.exists(output_dir_tif)) {
  dir.create(output_dir_tif, recursive = TRUE)  # Create parent directories if missing
  cat("Created output directory:", output_dir_tif, "\n")
} else {
  cat("Output directory already exists:", output_dir_tif, "\n")
}

# List all .tif files (monthly precipitation data) in the directory
precip_files <- list.files(precip_dir, pattern = "\\.tif$", full.names = TRUE)

# Load the precipitation data into a RasterStack (each layer corresponds to one month of data)
precip_raster_stack <- stack(precip_files)

# Ensure the raster has 240 layers (12 months * 20 years = 240 layers)
if (nlayers(precip_raster_stack) != 240) {
  stop("The number of layers in the precipitation raster does not match 240 months (12 months * 20 years)!")
}

# Define years and months
years <- 2001:2020
months <- 1:12

# Step 1: Calculate the historical average precipitation for each month (1-12) across all years (2001-2020)
historical_avg_list <- list()

for (month in months) {
  # Extract the layers corresponding to the current month across all years (e.g., Jan 2001, Jan 2002, ...)
  month_layers <- precip_raster_stack[[seq(month, nlayers(precip_raster_stack), by = 12)]]
  
  # Calculate the long-term average precipitation for this month (2001-2020)
  historical_avg <- calc(month_layers, fun = mean, na.rm = TRUE)
  
  # Store the historical average in the list
  historical_avg_list[[month]] <- historical_avg
}

# Step 2: Calculate the Precipitation Anomaly Index (PAI) for each month and year (2001-2020)
for (year in years) {
  for (month in months) {
    
    # Calculate the corresponding layer index for the current year and month
    month_layer_index <- (year - 2001) * 12 + month
    
    # Extract the current month's precipitation data
    precip_layer <- precip_raster_stack[[month_layer_index]]
    
    # Extract the historical average for this month (from the pre-calculated list)
    historical_avg <- historical_avg_list[[month]]
    
    # Step 3: Calculate the Precipitation Anomaly Index (PAI) for the current month
    pai_layer <- (precip_layer - historical_avg) / historical_avg * 100  # Percentage anomaly
    
    # Step 4: Save the PAI as a .tif file for the current year and month
    output_filename <- paste0(output_dir_tif, "Precipitation_Anomaly_Index_", year, "_", sprintf("%02d", month), ".tif")
    
    # Save the PAI raster as a .tif file
    tryCatch({
      writeRaster(pai_layer, filename = output_filename, format = "GTiff", overwrite = TRUE)
      cat("PAI for", year, "-", sprintf("%02d", month), "saved to", output_filename, "\n")
    }, error = function(e) {
      cat("Error saving PAI for", year, "-", sprintf("%02d", month), ": ", e$message, "\n")
    })
  }
}
