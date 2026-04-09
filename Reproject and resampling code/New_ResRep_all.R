# --- Install & Load Required Library ---
library(terra)

# --- Define Input & Output Paths ---
input_folder <- "G:/ML_Papers_Data/All_Index_Data/All_Mask_Data/VCI/"
output_folder <- "G:/ML_Papers_Data/All_Index_Data/All_Resam_Repro_Data/Res&Rep_VCI-500/"

# --- Create Output Folder If Not Exists ---
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# --- List TIFF Files ---
tif_files <- list.files(input_folder, pattern = "\\.tif$", full.names = TRUE)
if (length(tif_files) == 0) stop("❌ No TIFF files found in the input folder.")

cat("📂 Files to be processed:\n")
print(tif_files)

# --- Define Target CRS and Resolution ---
utm_crs <- "EPSG:32648"     # UTM Zone 48N
target_res <- 500           # 500 meter resolution

# --- Loop: Reproject + Resample + Save ---
cat("\n🚀 Starting reprojection and resampling...\n")
for (tif in tif_files) {
  cat("🔄 Processing:", basename(tif), "\n")
  
  r <- rast(tif)
  
  # Reproject and resample in one step
  r_proj <- project(r, utm_crs, res = target_res, method = "bilinear")
  
  # Output file path
  output_path <- file.path(output_folder, basename(tif))
  
  # Save output
  writeRaster(r_proj, output_path, overwrite = TRUE)
  cat("✅ Saved:", output_path, "\n")
}

cat("🎉 All files reprojected to UTM Zone 48N and resampled to 500m.\n")
