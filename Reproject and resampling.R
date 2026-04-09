library(terra)

# Define input and output base directories
input_base <- "G:/ML_Papers_Data/All_Index_Data/Spatio/All_Normalized_Data/"
output_base <- "G:/ML_Papers_Data/All_Index_Data/Spatio/All_Stacked_Data/"

# Ensure output base directory exists
if (!dir.exists(output_base)) {
  dir.create(output_base, recursive = TRUE)
}

# Define the 8 specific subfolders
subfolders <- c("ESI_N", "NDVI_N", "PCI_N", "SMCI_N", "SPEI-3_N", "TCI_N", "VCI_N", "VHI_N")
subfolder_paths <- file.path(input_base, subfolders)

# Check if all subfolders exist
existing_folders <- subfolder_paths[dir.exists(subfolder_paths)]
if (length(existing_folders) != 8) {
  cat("Warning: Found", length(existing_folders), "of 8 expected subfolders.\n")
  cat("Missing:", setdiff(subfolder_paths, existing_folders), "\n")
}

# Loop through each subfolder
for (subfolder in existing_folders) {
  tryCatch({
    # Get folder name (e.g., 'ESI_N')
    folder_name <- basename(subfolder)
    
    # Create corresponding output folder (e.g., 'ESI_Stacked')
    output_folder <- file.path(output_base, paste0(gsub("_N$", "", folder_name), "_Stacked"))
    if (!dir.exists(output_folder)) {
      dir.create(output_folder, recursive = TRUE)
    }
    
    # Get list of TIFF files in the subfolder
    tif_files <- list.files(subfolder, pattern = "\\.tif$", full.names = TRUE)
    
    # Check if any TIFFs were found
    if (length(tif_files) == 0) {
      cat("No TIFF files found in", folder_name, "\n")
      next
    }
    
    # Read and stack TIFFs
    raster_list <- lapply(tif_files, function(f) {
      r <- rast(f)
      names(r) <- gsub("\\.tif$", "", basename(f))  # Set layer name
      return(r)
    })
    stacked_raster <- rast(raster_list)
    
    # Define output stacked TIFF path
    stack_output <- file.path(output_folder, paste0("stacked_", tolower(gsub("_N$", "", folder_name)), ".tif"))
    
    # Save the stacked raster
    writeRaster(stacked_raster, stack_output, overwrite = TRUE)
    
    # Verify output
    cat("Stacked and saved:", stack_output, "\n")
    cat("Folder:", folder_name, "\n")
    cat("Number of layers:", nlyr(stacked_raster), "\n")
    cat("CRS:", crs(stacked_raster, describe = TRUE)$code, "\n")
    cat("Resolution:", res(stacked_raster), "\n")
    
  }, warning = function(w) {
    cat("Warning processing folder", folder_name, ":", w$message, "\n")
  }, error = function(e) {
    cat("Error processing folder", folder_name, ":", e$message, "\n")
  })
}

# Final message
cat("Processing complete.\n")