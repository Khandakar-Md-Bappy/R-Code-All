# --- Install & Load Required Libraries ---
install_and_load <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}
required_packages <- c("terra", "tidyverse")
invisible(lapply(required_packages, install_and_load))

# --- Set Input and Output Paths ---
et_folder <- "G:/ML_Papers_Data/All_Index_Data/All_Mask_Data/ET&PET/ET_Mask_Data/"     # Path to ET data
pet_folder <- "G:/ML_Papers_Data/All_Index_Data/All_Mask_Data/ET&PET/PET_Mask_Data/"   # Path to PET data
output_folder <- "G:/ML_Papers_Data/All_Index_Data/All_Mask_Data/ESI/"  # Output folder

# --- Create Output Folder If Not Exists ---
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

# --- List ET and PET Files (Assumed monthly aligned) ---
et_files <- list.files(et_folder, pattern = "\\.tif$", full.names = TRUE)
pet_files <- list.files(pet_folder, pattern = "\\.tif$", full.names = TRUE)

# --- Check File Count Alignment ---
if (length(et_files) != length(pet_files)) {
  stop("❌ ET and PET file counts do not match.")
}

# --- Dates (Monthly from 2001 to 2020) ---
dates <- seq(as.Date("2001-01-01"), as.Date("2020-12-01"), by = "month")

cat("📊 Calculating raw ESI...\n")
for (i in seq_along(et_files)) {
  et <- rast(et_files[i])
  pet <- rast(pet_files[i])
  
  # Avoid division by zero
  pet[pet == 0] <- NA
  
  # Calculate raw ESI (ET / PET)
  esi <- et / pet
  
  # Save output
  month_str <- format(dates[i], "%Y_%m")
  output_path <- file.path(output_folder, paste0("ESI_", month_str, ".tif"))
  writeRaster(esi, output_path, overwrite = TRUE)
  cat("✅ Saved:", output_path, "\n")
}

cat("🎉 All raw ESI files saved successfully.\n")
