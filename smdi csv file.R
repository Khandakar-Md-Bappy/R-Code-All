library(terra)

# Define input directory and output file
input_dir <- "G:/FINAL USES DATA/New Data Collected/Soil Moisture/SMDI/"  # Update with your input directory
output_csv <- "G:/FINAL USES DATA/New Data Collected/Soil Moisture/smdi_csv/smdi_monthly_means.csv"  # Output CSV file path

# Check if output directory exists, if not, create it
output_dir <- dirname(output_csv)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Load all SMDI TIFF files (assuming they are named consistently)
smdi_files <- list.files(input_dir, pattern = "\\.tif$", full.names = TRUE)

# Check if any files are found
if (length(smdi_files) == 0) {
  stop("No TIFF files found in the specified directory.")
}

# Load the raster stack
smdi_stack <- rast(smdi_files)

# Initialize an empty data frame to store results
smdi_results <- data.frame(Date = character(0), Mean_SMDI = numeric(0))

# Loop through each raster layer (each file corresponds to one month of data)
for (i in 1:nlyr(smdi_stack)) {
  # Extract values from the current raster layer
  smdi_values <- values(smdi_stack[[i]])
  
  # Remove NA values
  smdi_values <- smdi_values[!is.na(smdi_values)]
  
  # Extract year and month from the filename
  file_name <- basename(smdi_files[i])
  # Assuming the file name format is "smdi_YYYY_MM.tif"
  parts <- strsplit(file_name, "_")[[1]]
  year <- as.integer(parts[2])
  month <- as.integer(parts[3])
  
  # Combine year and month into a single Date column in "YYYY/MM" format
  date_str <- sprintf("%04d/%02d", year, month)
  
  # Calculate the mean SMDI value for this layer (month)
  mean_smdi <- mean(smdi_values)
  
  # Add the result to the main data frame
  smdi_results <- rbind(smdi_results, data.frame(Date = date_str, Mean_SMDI = mean_smdi))
}

# Save the combined SMDI results (monthly mean) to a CSV file
write.csv(smdi_results, output_csv, row.names = FALSE)

cat("SMDI monthly mean values extracted and saved to:", output_csv, "\n")
