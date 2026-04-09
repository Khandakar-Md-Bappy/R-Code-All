install.packages("SPEI")
library(SPEI)
# Load your data
data <- read.csv("D:/Data/Me.csv")

# View the first few rows
head(data)
# Assuming 'Precipitation' and 'PET' columns exist in your CSV
data$Water_Balance <- data$Precipitation - data$PET
# SPEI for a 3-month timescale
# Install and load necessary libraries


# Calculate SPEI for 3, 6, and 12-month timescales
spei_3 <- spei(data$Water_Balance, 3)$fitted   # SPEI 3-month
spei_6 <- spei(data$Water_Balance, 6)$fitted   # SPEI 6-month
spei_12 <- spei(data$Water_Balance, 12)$fitted # SPEI 12-month

# Add the SPEI values to the original data
data$SPEI_3 <- spei_3
data$SPEI_6 <- spei_6
data$SPEI_12 <- spei_12

# Save the updated data with SPEI columns to a CSV file
write.csv(data, "D:/Data/spei_results.csv", row.names = FALSE)

# Confirmation message
cat("SPEI 3, 6, and 12-month data has been saved to 'D:/Data/spei_results.csv'")

data$SPEI_3_normalized <- scale(data$SPEI_3)
data$SPEI_6_normalized <- scale(data$SPEI_6)
data$SPEI_12_normalized <- scale(data$SPEI_12)

# Save the updated data with normalized SPEI columns to a CSV file
write.csv(data, "D:/Data/normalized_spei_results.csv", row.names = FALSE)

# Confirmation message
cat("Normalized SPEI 3, 6, and 12-month data has been saved to 'D:/Data/normalized_spei_results.csv'")

