# Load necessary libraries
library(readr)
library(caret)

# Load and preprocess training data
setwd("C:/Users/praka/OneDrive/Desktop/assignment")
train_data <-('housing_train.csv')
# Perform data preprocessing steps

# Split the data
set.seed(42)  # for reproducibility
split <- createDataPartition(train_data$Price, p = 0.8, list = FALSE)
train_set <- train_data[split, ]
val_set <- train_data[-split, ]

# Create a training set (adjust column names accordingly)
train_set <- housing_data[, c("Rooms", "Type", "Method", "SellerG", "Distance", "Postcode", "Bedroom2", "Bathroom", "Car", "Landsize", "BuildingArea", "Price")]

# Evaluate the model on the validation set
predictions <- predict(model, newdata = val_set)
mae <- mean(abs(val_set$Price - predictions))
print(paste('Mean Absolute Error:', mae))

# Load and preprocess test data
test_data <- read_csv('housing_test.csv')
# Perform the same data pre-processing steps

# Predict on the test data
test_predictions <- predict(model, newdata = test_data)





setwd("C:/Users/praka/OneDrive/Desktop/assignment")
train_data <-('housing_train.csv')

# Load your data into a data frame
housing_test <- read.csv(file_path)

# Check the number of unique values in 'postcode'
unique_postcodes <- length(unique(housing_test$postcode))

cat("Number of unique values in 'postcode' in housing_test:", unique_postcodes, "\n")

# Assuming your data frame is named 'data' and contains a column 'postcode'
unique_postcodes <- housing_test.csv(data$postcode)

cat("Number of unique values in 'postcode':", unique_postcodes, "\n")
# Load your data into a data frame
housing_test <- read.csv('housing_test.csv')

# Check the number of unique values in 'postcode'
unique_postcodes <- length(unique(housing_test$postcode))

cat("Number of unique values in 'postcode' in housing_test:", unique_postcodes, "\n")
shapiro.test(housing_test$distance)