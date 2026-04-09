# --- Ensure Libraries Are Loaded ---
install_and_load <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}
required_packages <- c("terra", "tidyverse")
invisible(lapply(required_packages, install_and_load))

# --- Set Paths ---
input_folder <- "G:/ML_Papers_Data/All_Index_Data/All_Mask_Data/Monthly_SM_Mask/"  # Input SM folder
output_folder <- "G:/ML_Papers_Data/All_Index_Data/SMCI/"  # Output SMCI folder

# --- Create Output Directory If Not Exists ---
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

# --- Load All TIFFs ---
sm_files <- list.files(input_folder, pattern = "\\.tif$", full.names = TRUE)
if (length(sm_files) == 0) {
  stop("No TIFF files found in the input folder.")
}

# --- Check File Count ---
dates <- seq(as.Date("2001-01-01"), as.Date("2020-12-01"), by = "month")
if (length(sm_files) != length(dates)) {
  warning("Number of SM files (", length(sm_files), 
          ") does not match expected months (", length(dates), "). Check file order.")
}

cat("Loading raster stack...\n")
sm_stack <- rast(sm_files)

# --- Calculate Min and Max ---
cat("Calculating pixel-wise min and max...\n")
sm_min <- app(sm_stack, fun = min, na.rm = TRUE)
sm_max <- app(sm_stack, fun = max, na.rm = TRUE)

# --- Calculate SMCI ---
cat("Calculating SMCI...\n")
denominator <- sm_max - sm_min
smci_stack <- terra::ifel(denominator == 0, 0.5, (sm_stack - sm_min) / denominator)
smci_stack <- clamp(smci_stack, lower = 0, upper = 1)  # Ensure 0-1 range

# --- Save Each Layer ---
cat("Saving output TIFFs...\n")
for (i in 1:nlyr(smci_stack)) {
  month_str <- format(dates[i], "%Y_%m")
  output_path <- file.path(output_folder, paste0("SMCI_", month_str, ".tif"))
  writeRaster(smci_stack[[i]], output_path, overwrite = TRUE)
  cat("Saved:", output_path, "\n")
}

cat("✅ SMCI processing completed.\n")