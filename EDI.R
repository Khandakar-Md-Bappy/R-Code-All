#install.packages("raster")
#install.packages("sf")
#install.packages("dplyr")

install.packages("tools")

# Load required libraries
library(raster)
library(sf)
library(dplyr)
library(tools) # For file_path_sans_ext

# Define file paths
pet_dir <- "G:/FINAL USES DATA/ALL DATA/PET Data_MODIS006MOD16A2/PET_Mask_Data/"
et_dir <- "G:/FINAL USES DATA/ALL DATA/ET_MODIS006MOD16A2/ET_Mask_Data/"
output_file <- "G:/FINAL USES DATA/New Data Collected/EDI/Combined_EDI.csv"

# List all PET and ET files
pet_files <- list.files(pet_dir, pattern = "\\.tif$", full.names = TRUE)
et_files <- list.files(et_dir, pattern = "\\.tif$", full.names = TRUE)

# Ensure PET and ET files are aligned
if (length(pet_files) != length(et_files)) {
  stop("The number of PET and ET files must be the same.")
}

# Function to extract year and month from filenames
get_year_month <- function(filename) {
  match <- regmatches(filename, regexpr("\\d{4}_\\d{2}", filename))
  if (length(match) == 1) {
    split_match <- unlist(strsplit(match, "_"))
    year <- split_match[1]
    month <- split_match[2]
    return(c(year = year, month = month))
  } else {
    stop("Filename does not contain a valid date in 'YYYY_MM' format: ", filename)
  }
}

# Initialize an empty data frame for combined output
combined_df <- data.frame()

# Calculate EDI for each file and append to combined data frame
for (i in seq_along(pet_files)) {
  # Load PET and ET rasters
  pet_raster <- raster(pet_files[i])
  et_raster <- raster(et_files[i])
  
  # Calculate EDI
  edi_raster <- et_raster / pet_raster
  
  # Handle cases where PET is zero
  edi_raster[is.infinite(edi_raster)] <- NA
  
  # Convert EDI raster to data frame
  edi_df <- as.data.frame(edi_raster, xy = TRUE, na.rm = TRUE)
  
  # Extract year and month
  date_info <- get_year_month(basename(pet_files[i]))
  year <- date_info["year"]
  month <- date_info["month"]
  
  # Add year and month columns to the data frame
  edi_df <- edi_df %>%
    mutate(Year = year, Month = month) %>%
    select(Year, Month, x, y, layer) # Rearrange columns
  
  # Append to the combined data frame
  combined_df <- bind_rows(combined_df, edi_df)
  
  print(paste("Processed EDI for:", year, month))
}

# Save the combined data frame to a single CSV file
write.csv(combined_df, output_file, row.names = FALSE)

print(paste("Combined EDI saved to:", output_file))
