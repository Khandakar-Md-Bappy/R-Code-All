library(terra)

# Define paths
output_base <- "G:/ML_Papers_Data/All_Index_Data/Spatio/All_Stacked_Data/"
indices <- c("esi", "ndvi", "pci", "smci", "spei-3", "tci", "vci", "vhi")

# Load stacks
stacks <- lapply(indices, function(idx) {
  rast(file.path(output_base, paste0(idx, "_Stacked/stacked_", idx, ".tif")))
})

# Choose reference stack (e.g., ESI)
ref_stack <- stacks[[1]]

# Align all stacks to reference
aligned_stacks <- lapply(stacks, function(s) {
  # Compare extent, resolution, and dimensions
  extent_match <- identical(as.vector(ext(s)), as.vector(ext(ref_stack)))
  res_match <- identical(res(s), res(ref_stack))
  dim_match <- nrow(s) == nrow(ref_stack) && ncol(s) == ncol(ref_stack)
  
  if (!extent_match || !res_match || !dim_match) {
    cat("Aligning", names(s)[1], "to reference\n")
    project(s, ref_stack, method = "bilinear")  # Reproject to match
  } else {
    s
  }
})

# Verify alignment
cat("Verifying alignment:\n")
alignment_check <- lapply(aligned_stacks, function(s) {
  c(ncell = ncell(s), extent = paste(round(as.vector(ext(s)), 2), collapse = ","), res = paste(res(s), collapse = ","))
})
names(alignment_check) <- indices
print(alignment_check)

# Option 1: Extract all values (if memory allows)
tryCatch({
  pixel_data <- do.call(cbind, lapply(aligned_stacks, values))
  coords <- xyFromCell(ref_stack, 1:ncell(ref_stack))
  data_df <- data.frame(x = coords[, 1], y = coords[, 2], pixel_data)
  
  # Name columns
  colnames(data_df)[3:ncol(data_df)] <- unlist(lapply(indices, function(idx) {
    paste0(idx, "_", names(rast(file.path(output_base, paste0(idx, "_Stacked/stacked_", idx, ".tif")))))
  }))
  
  # Remove NAs
  data_df <- data_df[complete.cases(pixel_data), ]
  
  # Save
  write.csv(data_df, "G:/ML_Papers_Data/ml_input.csv", row.names = FALSE)
  cat("Data saved to ml_input.csv\n")
}, error = function(e) {
  cat("Error extracting all values:", e$message, "\nTrying sampling approach...\n")
  
  # Option 2: Sample pixels if memory fails
  set.seed(123)
  sample_cells <- sample(1:ncell(ref_stack), min(10000, ncell(ref_stack)))
  pixel_data <- do.call(cbind, lapply(aligned_stacks, function(s) values(s)[sample_cells, ]))
  coords <- xyFromCell(ref_stack, sample_cells)
  data_df <- data.frame(x = coords[, 1], y = coords[, 2], pixel_data)
  
  # Name columns
  colnames(data_df)[3:ncol(data_df)] <- unlist(lapply(indices, function(idx) {
    paste0(idx, "_", names(rast(file.path(output_base, paste0(idx, "_Stacked/stacked_", idx, ".tif")))))
  }))
  
  # Remove NAs
  data_df <- data_df[complete.cases(pixel_data), ]
  
  # Save
  write.csv(data_df, "G:/ML_Papers_Data/ml_input_sampled.csv", row.names = FALSE)
  cat("Sampled data saved to ml_input_sampled.csv\n")
})