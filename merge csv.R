# Load required packages
library(raster)
library(tidyverse)
library(lubridate)

# Set working directory and paths
input_dir <- "G:/ML_Papers_Data/All_Index_Data/New_Data/With_Mask/1.More_Indices for uses/SOIL_M"
output_dir <- "G:/ML_Papers_Data/All_Index_Data/New_Data/With_Mask/All_csv"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
output_csv <- file.path(output_dir, "SMDI_monthly_means.csv")

# List TIFF files (expecting SOIL_M_YYYY_MM.tif)
tiff_files <- list.files(input_dir, pattern = "SOIL_M_\\d{4}_\\d{2}\\.tif$", full.names = TRUE)

# Check if files exist
if (length(tiff_files) == 0) {
  stop("No TIFF files found in ", input_dir)
}

# Initialize lists for results
months <- vector("character", length(tiff_files))
mean_smdi <- vector("numeric", length(tiff_files))
file_count <- 0

# Read all TIFFs to compute long-term median and MAD
soil_stack <- stack(tiff_files)
cat("Loaded", nlayers(soil_stack), "TIFF files\n")

# Compute median and MAD across all months (per pixel)
soil_values <- values(soil_stack)
median_sm <- apply(soil_values, 1, function(x) median(x, na.rm = TRUE))
mad_sm <- apply(soil_values, 1, function(x) median(abs(x - median(x, na.rm = TRUE)), na.rm = TRUE))

# Create rasters for median and MAD
median_raster <- raster(soil_stack[[1]])
values(median_raster) <- median_sm
mad_raster <- raster(soil_stack[[1]])
values(mad_raster) <- mad_sm

# Process each TIFF file
for (tiff in tiff_files) {
  tryCatch({
    # Extract year and month from filename (e.g., SOIL_M_2001_01.tif)
    file_name <- basename(tiff)
    parts <- str_split(file_name, "_")[[1]]
    if (length(parts) != 4) {
      cat("Skipping", file_name, ": Invalid filename format\n")
      next
    }
    year <- as.integer(parts[3])
    month <- as.integer(str_replace(parts[4], "\\.tif", ""))
    
    # Validate year and month
    if (is.na(year) || is.na(month) || year < 2001 || year > 2020 || month < 1 || month > 12) {
      cat("Skipping", file_name, ": Invalid year or month\n")
      next
    }
    
    # Create date
    month_date <- as.Date(sprintf("%d-%02d-01", year, month))
    
    # Read raster
    r <- raster(tiff)
    
    # Calculate SMDI: (SM_i - median) / MAD, scaled to -4 to +4
    smdi <- (r - median_raster) / mad_raster
    smdi <- 10 * smdi - 5  # Scale to approximate -4 to +4 range
    
    # Compute mean of valid pixels
    smdi_values <- values(smdi)
    valid_values <- smdi_values[is.finite(smdi_values)]
    
    # Log range for debugging
    if (length(valid_values) > 0) {
      min_val <- min(valid_values)
      max_val <- max(valid_values)
      cat("Range for", file_name, ": Min =", min_val, ", Max =", max_val, "\n")
    }
    
    # Calculate mean SMDI
    mean_value <- if (length(valid_values) > 0) mean(valid_values) else NA
    if (!is.finite(mean_value)) mean_value <- NA
    
    # Warn if outside typical range (-4 to 4)
    if (!is.na(mean_value) && (mean_value < -4 || mean_value > 4)) {
      cat("Warning: Mean for", file_name, "=", mean_value, "is outside typical range (-4 to 4)\n")
    }
    
    # Store results
    file_count <- file_count + 1
    months[file_count] <- as.character(month_date)
    mean_smdi[file_count] <- mean_value
    cat("Processed", file_name, ": Mean SMDI =", mean_value, "\n")
    
  }, error = function(e) {
    cat("Error processing", file_name, ":", conditionMessage(e), "\n")
  })
}

# Create data frame
results <- tibble(
  Month = as.Date(months[1:file_count]),
  SMDI = mean_smdi[1:file_count]
)

# Ensure all months from 2001-01 to 2020-12
all_months <- seq(as.Date("2001-01-01"), as.Date("2020-12-01"), by = "month")
expected_df <- tibble(Month = all_months)
results <- expected_df %>% 
  left_join(results, by = "Month")

# Save to CSV
write_csv(results, output_csv)
cat("CSV saved:", output_csv, "\n")
cat("Processed", file_count, "out of 240 expected months\n")

# Check for missing data
missing <- sum(is.na(results$SMDI))
if (missing > 0) {
  cat("Warning:", missing, "months have missing SMDI values\n")
}