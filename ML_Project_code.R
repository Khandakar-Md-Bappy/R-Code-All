# Load required packages
library(raster)
library(randomForest)
library(xgboost)
library(e1071)
library(caret)
library(ggplot2)
library(corrplot)
library(dplyr)
library(reshape2)
library(gridExtra)

# Step 1: Load and Preprocess TIFF Data
data_dir <- "G:/ML_Papers_Data/All_Index_Data/Spatio/All_Stacked_Data/"
indices <- c("SPEI-3", "ESI", "NDVI", "PCI", "SMCI", "TCI", "VCI", "VHI")

# Get all TIFF files
tif_files <- list.files(data_dir, pattern = "\\.tif$", full.names = TRUE)
cat("Total TIFF files found:", length(tif_files), "\n")

# Extract file information
file_info <- data.frame(
  File = tif_files,
  Name = basename(tif_files)
) %>%
  mutate(
    # Try to extract index and year-month (flexible pattern)
    Index = sub("_Stacked_.*", "", Name),
    YearMonth = sub(".*_Stacked_([0-9]{4}_[0-9]{2})\\.tif", "\\1", Name),
    Year = as.integer(sub("_\\d{2}", "", YearMonth)),
    Month = as.integer(sub("\\d{4}_", "", YearMonth))
  )

# Print files that didn’t match the pattern
invalid_files <- file_info[is.na(file_info$Year) | is.na(file_info$Month), ]
if (nrow(invalid_files) > 0) {
  cat("Files with invalid naming (couldn’t parse Year/Month):\n")
  print(invalid_files$Name)
}

# Filter valid files and indices
file_info <- file_info[!is.na(file_info$Year) & !is.na(file_info$Month) & file_info$Index %in% indices, ]
cat("Valid files after filtering:", nrow(file_info), "\n")

# Summarize files per index
cat("\nFiles per index:\n")
print(table(file_info$Index))

# Summarize files per year-month
year_month_counts <- file_info %>%
  group_by(Year, Month) %>%
  summarise(Count = n(), .groups = "drop")
cat("\nYear-Month combinations and index counts:\n")
print(year_month_counts)

# Identify complete sets (all 8 indices)
complete_sets <- year_month_counts %>%
  filter(Count == 8)
cat("\nComplete sets (8 indices per year-month):", nrow(complete_sets), "\n")
if (nrow(complete_sets) == 0) {
  stop("No complete sets of 8 indices found. Check file names or missing indices.")
}

# Initialize data frame
all_data <- data.frame()

# Process each complete year-month
for (i in 1:nrow(complete_sets)) {
  year <- complete_sets$Year[i]
  month <- complete_sets$Month[i]
  
  # Get files for this year-month
  files_subset <- file_info %>%
    filter(Year == year, Month == month)
  
  # Load rasters
  pixel_data_list <- list()
  for (idx in indices) {
    file_row <- files_subset %>% filter(Index == idx)
    if (nrow(file_row) != 1) {
      warning(paste("Missing file for", idx, "in", year, month))
      next
    }
    r <- raster(file_row$File)
    pixel_data_list[[idx]] <- getValues(r)
  }
  
  # Check pixel count consistency
  pixel_lengths <- sapply(pixel_data_list, length)
  if (length(unique(pixel_lengths)) != 1) {
    warning(paste("Inconsistent pixel counts for", year, month))
    next
  }
  
  # Combine into data frame
  pixel_data <- as.data.frame(pixel_data_list)
  colnames(pixel_data) <- indices
  pixel_data$Year <- year
  pixel_data$Month <- month
  
  # Append
  all_data <- rbind(all_data, pixel_data)
}

# Remove NA rows
all_data <- na.omit(all_data)
cat("\nData dimensions after cleaning:", dim(all_data), "\n")

# Check SPEI-3 distribution
cat("SPEI-3 distribution:\n")
print(summary(all_data$`SPEI-3`))
cat("Negative values (drought):", sum(all_data$`SPEI-3` < 0), "\n")

# Step 2: Prepare Features and Target
X <- all_data[, c("ESI", "NDVI", "PCI", "SMCI", "TCI", "VCI", "VHI")]
y <- all_data$`SPEI-3`

# Stratified train-test split
set.seed(42)
bins <- cut(y, breaks = 5, labels = FALSE)
train_index <- createDataPartition(bins, p = 0.8, list = FALSE)
X_train <- X[train_index, ]
X_test <- X[-train_index, ]
y_train <- y[train_index]
y_test <- y[-train_index]

# Scale features
preprocess <- preProcess(X_train, method = c("center", "scale"))
X_train_scaled <- predict(preprocess, X_train)
X_test_scaled <- predict(preprocess, X_test)

cat("Training set size:", nrow(X_train_scaled), "\n")
cat("Testing set size:", nrow(X_test_scaled), "\n")

# Step 3: Hyperparameter Tuning
# Random Forest
rf_grid <- expand.grid(mtry = c(2, 4, 6))
rf_control <- trainControl(method = "cv", number = 5)
rf_tune <- train(X_train_scaled, y_train, method = "rf", tuneGrid = rf_grid, trControl = rf_control, ntree = 200)
best_rf <- rf_tune$finalModel
cat("Best RF mtry:", rf_tune$bestTune$mtry, "\n")

# XGBoost
xgb_grid <- expand.grid(
  nrounds = c(100, 200),
  max_depth = c(3, 6),
  eta = c(0.01, 0.1),
  gamma = 0,
  colsample_bytree = 0.7,
  min_child_weight = 1,
  subsample = 0.8
)
xgb_control <- trainControl(method = "cv", number = 5)
xgb_tune <- train(X_train_scaled, y_train, method = "xgbTree", tuneGrid = xgb_grid, trControl = xgb_control)
best_xgb <- xgb_tune$finalModel
cat("Best XGBoost params:\n")
print(xgb_tune$bestTune)

# SVM
svm_grid <- expand.grid(C = c(0.1, 1, 10), sigma = c(0.01, 0.1, 1))
svm_control <- trainControl(method = "cv", number = 5)
svm_tune <- train(X_train_scaled, y_train, method = "svmRadial", tuneGrid = svm_grid, trControl = svm_control)
best_svm <- svm_tune$finalModel
cat("Best SVM params: C =", svm_tune$bestTune$C, ", sigma =", svm_tune$bestTune$sigma, "\n")

# Step 4: Train Models and Evaluate
models <- list("Random Forest" = best_rf, "XGBoost" = best_xgb, "SVM" = best_svm)
predictions <- list()

for (name in names(models)) {
  if (name == "Random Forest") {
    y_pred <- predict(models[[name]], X_test_scaled)
  } else if (name == "XGBoost") {
    y_pred <- predict(models[[name]], as.matrix(X_test_scaled))
  } else {
    y_pred <- predict(models[[name]], X_test_scaled)
  }
  predictions[[name]] <- y_pred
  
  # Calculate metrics
  r2 <- cor(y_test, y_pred)^2
  rmse <- sqrt(mean((y_test - y_pred)^2))
  mae <- mean(abs(y_test - y_pred))
  cat(sprintf("%s - R²: %.3f, RMSE: %.3f, MAE: %.3f\n", name, r2, rmse, mae))
  
  # Scatterplot
  min_val <- min(min(y_test), min(y_pred)) - 0.1
  max_val <- max(max(y_test), max(y_pred)) + 0.1
  df_plot <- data.frame(Observed = y_test, Predicted = y_pred)
  p <- ggplot(df_plot, aes(x = Observed, y = Predicted)) +
    geom_point(alpha = 0.5) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    labs(title = sprintf("%s Predictions vs Observations\nR²=%.3f, RMSE=%.3f", name, r2, rmse),
         x = "Observed Value", y = "Predicted Value") +
    coord_cartesian(xlim = c(min_val, max_val), ylim = c(min_val, max_val)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  print(p)
  
  # Print first three values
  cat(sprintf("\n%s - First Three Observed vs Predicted Values:\n", name))
  for (i in 1:min(3, length(y_test))) {
    cat(sprintf("  Observed: %.3f, Predicted: %.3f\n", y_test[i], y_pred[i]))
  }
}

# Store predictions
y_pred_rf <- predictions[["Random Forest"]]
y_pred_xgb <- predictions[["XGBoost"]]
y_pred_svm <- predictions[["SVM"]]

# Step 5: Weighted Ensemble
weights <- list("Random Forest" = 0.5, "XGBoost" = 0.3, "SVM" = 0.2)
y_pred_ensemble <- weights[["Random Forest"]] * y_pred_rf + 
  weights[["XGBoost"]] * y_pred_xgb + 
  weights[["SVM"]] * y_pred_svm

# Evaluate ensemble
r2_ensemble <- cor(y_test, y_pred_ensemble)^2
rmse_ensemble <- sqrt(mean((y_test - y_pred_ensemble)^2))
mae_ensemble <- mean(abs(y_test - y_pred_ensemble))
bias_ensemble <- mean(y_pred_ensemble - y_test)
cat(sprintf("\nWeighted Ensemble - R²: %.3f, RMSE: %.3f, MAE: %.3f, Bias: %.3f\n", 
            r2_ensemble, rmse_ensemble, mae_ensemble, bias_ensemble))

# Ensemble scatterplot
min_val <- min(min(y_test), min(y_pred_ensemble)) - 0.1
max_val <- max(max(y_test), max(y_pred_ensemble)) + 0.1
df_plot <- data.frame(Observed = y_test, Predicted = y_pred_ensemble)
p <- ggplot(df_plot, aes(x = Observed, y = Predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = sprintf("Weighted Ensemble Predictions vs Observations\nR²=%.3f, RMSE=%.3f", 
                       r2_ensemble, rmse_ensemble),
       x = "Observed Value", y = "Predicted Value") +
  coord_cartesian(xlim = c(min_val, max_val), ylim = c(min_val, max_val)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
print(p)

# Print first three ensemble values
cat("\nWeighted Ensemble - First Three Observed vs Predicted Values:\n")
for (i in 1:min(3, length(y_test))) {
  cat(sprintf("  Observed: %.3f, Predicted: %.3f\n", y_test[i], y_pred_ensemble[i]))
}

# Step 6: Boxplot of Predictions
pred_df <- data.frame(
  Random_Forest = y_pred_rf,
  XGBoost = y_pred_xgb,
  SVM = y_pred_svm,
  Ensemble = y_pred_ensemble
)
box_data <- reshape2::melt(pred_df, variable.name = "Model", value.name = "Predicted")
p <- ggplot(box_data, aes(x = Model, y = Predicted)) +
  geom_boxplot() +
  labs(title = "Boxplot of Predicted Values by Model", y = "Predicted Value") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
print(p)

# Step 7: Feature Importance
# Random Forest
rf_imp <- importance(best_rf)
df_rf_imp <- data.frame(Feature = rownames(rf_imp), Importance = rf_imp[, 1])
p_rf <- ggplot(df_rf_imp, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Random Forest Feature Importance", x = "Feature", y = "Importance") +
  theme_minimal()

# XGBoost
xgb_imp <- xgb.importance(model = best_xgb)
p_xgb <- ggplot(xgb_imp, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "XGBoost Feature Importance", x = "Feature", y = "Gain") +
  theme_minimal()

# Plot side by side
gridExtra::grid.arrange(p_rf, p_xgb, ncol = 2)

# Step 8: Correlation Heatmap
corr_matrix <- cor(all_data[, indices])
corrplot(corr_matrix, method = "color", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black", 
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         title = "Correlation Heatmap of Indices", mar = c(0, 0, 1, 0))

# Step 9: Drought Years Time Series
all_data$Date <- as.Date(paste(all_data$Year, all_data$Month, "01", sep = "-"))
all_data <- all_data[order(all_data$Date), ]

p <- ggplot(all_data, aes(x = Date, y = `SPEI-3`)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_ribbon(aes(ymin = pmin(`SPEI-3`, 0), ymax = 0), fill = "red", alpha = 0.3) +
  labs(title = "SPEI-3 Time Series with Drought Periods (Ningxia Hui Area)",
       x = "Year", y = "SPEI-3") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
print(p)

# Identify drought years
drought_years <- all_data %>%
  group_by(Year) %>%
  summarise(Avg_SPEI3 = mean(`SPEI-3`)) %>%
  filter(Avg_SPEI3 < 0) %>%
  pull(Year)
cat("Drought years (average SPEI-3 < 0):", drought_years, "\n")