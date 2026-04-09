# --- Load Required Library ---
#install.packages("terra")
library(terra)

# --- Define Input & Output Paths ---
input_folder <- "G:/ML_Papers_Data/All_Index_Data/All_Resam_Repro_Data/SPEI-3/"
output_folder <- "G:/ML_Papers_Data/All_Index_Data/All_Normalized_Data/SPEI-3/"

# --- Create Output Folder If Not Exists ---
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# --- List TIFF Files ---
tif_files <- list.files(input_folder, pattern = "\\.tif$", full.names = TRUE)
if (length(tif_files) == 0) stop("❌ No TIFF files found in the input folder.")

cat("📂 Normalizing the following NDVI files:\n")
print(tif_files)

# --- Loop: Normalize Each File ---
for (tif in tif_files) {
  cat("🔄 Processing:", basename(tif), "\n")
  
  r <- rast(tif)
  
  # Calculate min and max
  r_min <- global(r, "min", na.rm = TRUE)[1,1]
  r_max <- global(r, "max", na.rm = TRUE)[1,1]
  
  # Normalize to [0, 1]
  if (!is.na(r_min) && !is.na(r_max) && r_max != r_min) {
    r_norm <- (r - r_min) / (r_max - r_min)
  } else {
    warning(paste("⚠️ Skipping normalization for:", basename(tif), "- invalid min/max."))
    r_norm <- r
  }
  
  # Output file path
  output_path <- file.path(output_folder, basename(tif))
  
  # Save normalized raster
  writeRaster(r_norm, output_path, overwrite = TRUE)
  cat("✅ Saved:", output_path, "\n")
}

cat("🎉 Normalization complete. All NDVI files scaled to [0, 1].\n")
