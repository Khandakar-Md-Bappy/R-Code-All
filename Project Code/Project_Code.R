# Required libraries
library(sf)          # Replaces rgdal
library(terra)       # Alternative for raster/sp
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(h2o)

# Setup
rm(list = ls())
dataFolder <- "G:/FINAL USES DATA/Seasonally Extract/Seasonal_Indices/Se_SOS/Se_SOS_csv/"

# Load data for drought indices
PAI <- read.csv(paste0(dataFolder, "PAI.csv"))
EDI <- read.csv(paste0(dataFolder, "EDI.csv"))
NDVI <- read.csv(paste0(dataFolder, "NDVI.csv"))
PCI <- read.csv(paste0(dataFolder, "PCI.csv"))
SPEI_3 <- read.csv(paste0(dataFolder, "SPEI-3.csv"))
SPEI_6 <- read.csv(paste0(dataFolder, "SPEI-6.csv"))
SPEI_12 <- read.csv(paste0(dataFolder, "SPEI-12.csv"))
SPI_3 <- read.csv(paste0(dataFolder, "SPI-3.csv"))
SPI_6 <- read.csv(paste0(dataFolder, "SPI-6.csv"))
SPI_12 <- read.csv(paste0(dataFolder, "SPI-12.csv"))
VHI <- read.csv(paste0(dataFolder, "VHI.csv"))
TCI <- read.csv(paste0(dataFolder, "TCI.csv"))
VCI <- read.csv(paste0(dataFolder, "VCI.csv"))
SMDI <- read.csv(paste0(dataFolder, "SMDI.csv"))

# Prepare training data
drought_data <- data.frame(
  PAI = PAI$PAI,
  PCI = PCI$PCI,
  VCI = VCI$VCI,
  TCI = TCI$TCI,
  EDI = EDI$EDI,
  SPI_3 = SPI_3$SPI_3,
  SPI_6 = SPI_6$SPI_6,
  SPI_12 = SPI_12$SPI_12,
  SPEI_3 = SPEI_3$SPEI_3,
  SPEI_6 = SPEI_6$SPEI_6,
  SPEI_12 = SPEI_12$SPEI_12,
  SMDI = SMDI$SMDI  # Response variable
)

# Ensure all columns are numeric
drought_data <- mutate_all(drought_data, as.numeric)

# Define response and predictor variables
y <- "SMDI"  # Response variable
x <- setdiff(names(drought_data), y)  # Predictor variables

# Initialize H2O
h2o.init(nthreads = -1, max_mem_size = "1g")

# Convert data to H2O frame
df <- as.h2o(drought_data)

# Split data into train, validation, and test sets
splits <- h2o.splitFrame(df, c(0.75, 0.125), seed = 1234)
train  <- h2o.assign(splits[[1]], "train.hex")
valid  <- h2o.assign(splits[[2]], "valid.hex")
test   <- h2o.assign(splits[[3]], "test.hex")

# Define and train the Deep Learning (DFNN) model
dl_model <- h2o.deeplearning(
  model_id = "Deep_Learning_Model",
  training_frame = train,
  validation_frame = valid,
  x = x,
  y = y,
  standardize = TRUE,
  activation = "RectifierWithDropout",
  hidden = c(200, 200, 200),
  hidden_dropout_ratios = c(0.2, 0.1, 0.1),
  stopping_tolerance = 0.001,
  epochs = 10,
  adaptive_rate = TRUE,
  l1 = 1e-6,
  l2 = 1e-6,
  reproducible = TRUE,
  max_w2 = 10,
  nfolds = 10,
  fold_assignment = "Random",
  keep_cross_validation_fold_assignment = TRUE,
  seed = 125,
  variable_importances = TRUE
)

# Model evaluation
print(h2o.mse(dl_model, train = TRUE, valid = TRUE, xval = TRUE))
print(h2o.rmse(dl_model, train = TRUE, valid = TRUE, xval = TRUE))

# Predict on test data
g.predict <- as.data.frame(h2o.predict(dl_model, newdata = test))
test_df <- as.data.frame(test)  # Convert test data to dataframe
test_df$SMDI_pred <- g.predict$predict  # Add predictions

# Save predictions
write.csv(test_df, paste0(dataFolder, "Predicted_SMDI.csv"), row.names = FALSE)

# Visualize performance
ggplot(test_df, aes(x = SMDI_pred, y = SMDI)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "Prediction of SMDI", x = "Predicted SMDI", y = "Actual SMDI")

# Shutdown H2O after completion
h2o.shutdown(prompt = FALSE)







# 2.With the shape file 

## Required libraries
library(rgdal)
library(raster)
library(sp)
library(plyr)
library(dplyr)
library(RStoolbox)
library(ggplot2)
library(RColorBrewer)
library(h2o)

# Setup
rm(list = ls())
dataFolder <- "E:\\RECENT_MY\\R_project\\Drought_indices\\Depend\\Se_1//"

# Load the shapefile (make sure you have .shp, .shx, and .dbf files in the same folder)
shapefile_path <- "path/to/your/shapefile.shp"  # Update the path to your shapefile
shape <- readOGR(shapefile_path)

# Load data for drought indices (PAI, PCI, VCI, TCI, EDI, SPI, SPEI) as CSV or raster data
PAI <- read.csv(paste0(dataFolder, "PAI.csv"))
PCI <- read.csv(paste0(dataFolder, "PCI.csv"))
VCI <- read.csv(paste0(dataFolder, "VCI.csv"))
TCI <- read.csv(paste0(dataFolder, "TCI.csv"))
EDI <- read.csv(paste0(dataFolder, "EDI.csv"))
SPI_3 <- read.csv(paste0(dataFolder, "SPI_3.csv"))
SPI_6 <- read.csv(paste0(dataFolder, "SPI_6.csv"))
SPI_12 <- read.csv(paste0(dataFolder, "SPI_12.csv"))
SPEI_3 <- read.csv(paste0(dataFolder, "SPEI_3.csv"))
SPEI_6 <- read.csv(paste0(dataFolder, "SPEI_6.csv"))
SPEI_12 <- read.csv(paste0(dataFolder, "SPEI_12.csv"))

# Prepare training data
# Assuming your data is combined into one dataframe
drought_data <- data.frame(
  PAI = PAI$PAI,
  PCI = PCI$PCI,
  VCI = VCI$VCI,
  TCI = TCI$TCI,
  EDI = EDI$EDI,
  SPI_3 = SPI_3$SPI_3,
  SPI_6 = SPI_6$SPI_6,
  SPI_12 = SPI_12$SPI_12,
  SPEI_3 = SPEI_3$SPEI_3,
  SPEI_6 = SPEI_6$SPEI_6,
  SPEI_12 = SPEI_12$SPEI_12,
  SMDI = read.csv(paste0(dataFolder, "SMDI.csv"))$SMDI # Assuming you have SMDI data
)

# Prepare the response and predictor variables
y <- "SMDI"  # Response variable
x <- setdiff(names(drought_data), y)  # Predictor variables

# Initialize H2O cluster for machine learning models
localH2o <- h2o.init(nthreads = -1, max_mem_size = "1g")

# Convert data to H2O frame
df <- as.h2o(drought_data)

# Split data into train, validation, and test sets
splits <- h2o.splitFrame(df, c(0.75, 0.125), seed = 1234)
train  <- h2o.assign(splits[[1]], "train.hex")
valid  <- h2o.assign(splits[[2]], "valid.hex")
test   <- h2o.assign(splits[[3]], "test.hex")

# Define and train the Deep Learning (DFNN) model
dl_model <- h2o.deeplearning(
  model_id = "Deep_Learning_Model",
  training_frame = train,
  validation_frame = valid,
  x = x,
  y = y,
  standardize = TRUE,
  activation = "RectifierWithDropout",
  hidden = c(200, 200, 200),
  hidden_dropout_ratios = c(0.2, 0.1, 0.1),
  stopping_tolerance = 0.001,
  epochs = 10,
  adaptive_rate = TRUE,
  l1 = 1e-6,
  l2 = 1e-6,
  reproducible = TRUE,
  max_w2 = 10,
  nfolds = 10,
  fold_assignment = "Random",
  keep_cross_validation_fold_assignment = TRUE,
  seed = 125,
  variable_importances = TRUE
)

# Model evaluation
print(h2o.mse(dl_model, train = TRUE, valid = TRUE, xval = TRUE))
print(h2o.rmse(dl_model, train = TRUE, valid = TRUE, xval = TRUE))

# Predict on test data
g.predict <- as.data.frame(h2o.predict(object = dl_model, newdata = test))
test$SMDI_pred <- g.predict$predict

# Output prediction results
write.csv(g.predict, paste0(dataFolder, "Predicted_SMDI.csv"))

# Visualize performance
ggplot(g.predict, aes(x = SMDI_pred, y = test$SMDI)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "Prediction of SMDI", x = "Predicted SMDI", y = "Actual SMDI")

# Mask the predicted raster with shapefile
# Create a raster for the predicted SMDI from the Deep Learning model
dl_raster <- rasterFromXYZ(cbind(test$x, test$y, test$SMDI_pred))

# Apply the shapefile mask to the raster
masked_dl_raster <- mask(dl_raster, shape)

# Save the masked raster as a TIFF file
writeRaster(masked_dl_raster, filename = paste0(dataFolder, "Masked_Predicted_SMDI_DL.tif"), format = "GTiff", overwrite = TRUE)

# Plot the result with the shapefile overlayed
plot(masked_dl_raster, main = "Predicted SMDI (Deep Learning) with Shapefile Overlay")
plot(shape, add = TRUE, border = "black")

# Shutdown H2O after completion
h2o.shutdown(prompt = FALSE)

