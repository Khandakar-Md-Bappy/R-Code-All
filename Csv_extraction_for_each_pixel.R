# Load required libraries
library(terra)   # For handling raster data

# Define the input folder containing the TIFF files
tiff_folder <- "G:/FINAL USES DATA/Seasonally Extract/1. Seasonal_Data/1.First_Seasonal_SOS/SPI-12"  # Replace with your folder path

# Define the output CSV file path for the combined data
output_csv_file <- "G:/FINAL USES DATA/Seasonally Extract/1. Seasonal_Data/1.First_Seasonal_SOS/1.First_Seasonal_csv/SPI-12.csv"  # Replace with your output file path

# Get the list of TIFF files in the folder
tiff_files <- list.files(tiff_folder, pattern = "\\.tif$", full.names = TRUE)

# Check the list of files found
cat("Files found:", length(tiff_files), "files.\n")
print(basename(tiff_files))

# Initialize an empty data frame to store all results
combined_data <- data.frame()

# Loop through each TIFF file
for (file in tiff_files) {
  # Read the raster
  raster <- rast(file)
  
  # Check CRS (Coordinate Reference System) and ensure it's UTM Zone 48N
  if (crs(raster) != "EPSG:32648") {  # EPSG:32648 is UTM Zone 48N
    raster <- project(raster, "EPSG:32648")
  }
  
  # Extract X, Y coordinates and raster values
  raster_data <- as.data.frame(raster, xy = TRUE, na.rm = TRUE)  # Extract coordinates and values
  
  # Add a date column (use year from the file name)
  year <- gsub(".*([0-9]{4}).*", "\\1", basename(file))  # Extract year (e.g., 2001)
  
  # Debugging: Print the extracted year
  cat("Processing year:", year, "\n")
  
  month_day <- "04-01"  # Assuming the data corresponds to April 1st for SOS
  raster_data$Date <- paste(year, month_day, sep = "-")
  
  # Rename the value column to the year
  colnames(raster_data)[3] <- "Value"
  
  # Append the current file's data to the combined data frame
  combined_data <- rbind(combined_data, raster_data)
}

# Reorder columns to match your requirement: Date, x, y, Value (2001 to 2020 vertically)
combined_data <- combined_data[, c("Date", "x", "y", "Value")]

# Save the combined data frame to a CSV file
write.csv(combined_data, output_csv_file, row.names = FALSE)

cat("Combined CSV file saved to:", output_csv_file, "\n")
