# Load required libraries
library(terra)   # For handling raster data

# Define the main input folder containing subfolders for each index
main_folder <- "E:/FINAL USES DATA/Seasonally Extract/1. Seasonal_Data/1.First_Seasonal_SOS/"  # Replace with your main folder path

# Define the output CSV file path for the combined data
output_csv_file <- "E:/FINAL USES DATA/Seasonally Extract/1. Seasonal_Data/1.First_Seasonal_SOS/1.First_Seasonal_csv/Combined_Indices.csv"  # Replace with your output file path

# Get the list of subfolders (one for each index)
indices_folders <- list.dirs(main_folder, full.names = TRUE, recursive = FALSE)

# Initialize an empty list to store data frames for all indices
all_indices_data <- list()

# Loop through each index folder
for (index_folder in indices_folders) {
  # Get the index name from the folder name
  index_name <- basename(index_folder)
  
  # Get the list of TIFF files in the current folder
  tiff_files <- list.files(index_folder, pattern = "\\.tif$", full.names = TRUE)
  
  # Initialize an empty data frame to store data for the current index
  index_data <- data.frame()
  
  # Loop through each TIFF file in the current folder
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
    month_day <- "04-01"  # Assuming the data corresponds to April 1st for SOS
    raster_data$Date <- paste(year, month_day, sep = "-")
    
    # Rename the value column to the index name
    colnames(raster_data)[3] <- index_name
    
    # Append the current file's data to the index data frame
    index_data <- rbind(index_data, raster_data)
  }
  
  # Store the index data in the list
  all_indices_data[[index_name]] <- index_data
}

# Merge all index data frames by Date, X, and Y coordinates
combined_data <- Reduce(function(x, y) merge(x, y, by = c("Date", "x", "y"), all = TRUE), all_indices_data)

# Save the combined data frame to a CSV file
write.csv(combined_data, output_csv_file, row.names = FALSE)

cat("Combined CSV file saved to:", output_csv_file, "\n")
