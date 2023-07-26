# 1. SETUP ----
## Default view columns
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)

## Load in libraries ----
library(tidyverse)
library(data.table)
library(caret)
library(readr)
library(glmnet)

# 2. DATA CLEANING ----
## Check if "my_data.csv" exists, if it exists then load it, if not create and load it
if (file.exists("my_data.csv")) {
  df <- read_csv("my_data.csv")
} else {
  ## Read in in the dataset ----
  df <- read_csv("MelanomaData.csv")
  
  ## Drop NAs ----
  df <- df %>% filter(!is.na(adult_fg_so))
  
  ## Cleaning the data, drop NA rows
  df <- df %>% select(-checked_6_month_other47, -adult_religion6_other, -adult_fg_so, -child_under_18_relation12___1, -checked_6_month_other47)
  
  df <- df %>%
    mutate_if(is.numeric, ~if_else(is.na(.), 0, .))
  
  na_counts <- df %>%
    summarise(across(everything(), ~ sum(is.na(.))))
  
  print(na_counts)
  
  ## Format variables ----
  ## Of the six child protective behaviors, create a new column using `mutate`
  ## new column should be a total of the number of behaviors taken up by the child
  ## This will be your outcome/y variable
  ## Make sure your count is a factor
  df <- df %>%
    rowwise() %>%
    mutate(Sum_Columns = sum(c_across(c(children_outdoors81, children_outdoors82, children_outdoors83, children_outdoors84, children_outdoors85, children_outdoors86, children_outdoors87, children_outdoors88))))
  
  print(df)
  
  ## Viewing new Column
  column_data <- df$Sum_Columns
  print(column_data)
  
  ## Save my_data_frame as a CSV file
  write_csv(df, path = "my_data.csv")
}


# Convert selected variables to factors
df <- df %>%
  mutate(
    across(c(
      adult_gender2, adult_gender3, adult_gender4, adult_gender5, adult_gender6, adult_gender7,
      child_under_18_relation12___3, child_under_18_relation12___4, child_under_18_relation12___5,
      child_under_18_relation12___6, child_under_18_relation12___7, child_relation_other12,
      health_care_coverage14, outdoor31, outdoor32, outdoor33, outdoor34, outdoor35, outdoor36,
      outdoor37, outdoor38, checked_your_skin_6_months47, children_outside_weekday79,
      children_outside_weekend80, children_outdoors81, children_outdoors82, children_outdoors83,
      children_outdoors84, children_outdoors85, children_outdoors86, children_outdoors87,
      children_outdoors88, total_sunburn, children_check_skin92, checked_body_parts94___1,
      checked_body_parts94___2, checked_body_parts94___3, checked_body_parts94___4,
      checked_body_parts94___5, checked_body_parts94___6, checked_body_parts94___7,
      checked_body_parts94___8, checked_body_parts94___9, checked_body_parts94___10,
      checked_body_parts94___11, checked_body_parts94___12, checked_body_parts94___13,
      checked_body_parts94___14, easy_difficult101, easy_difficult102, easy_difficult103,
      easy_difficult104, easy_difficult105, easy_difficult106, easy_difficult107,
      easy_difficult108, easy_difficult109, easy_difficult110, easy_difficult111,
      easy_difficult112, affect_sunscreen115, affect_sunscreen116, affect_sunscreen117,
      affect_sunscreen118, affect_sunscreen119, affect_sunscreen120, affect_sunscreen121,
      affect_sunscreen122, affect_sunscreen123, affect_sunscreen124, affect_sunscreen125,
      affect_sunscreen126, affect_sunscreen127, affect_sunscreen128, affect_outdoors131,
      affect_outdoors132, affect_outdoors133, affect_outdoors134, affect_outdoors135,
      affect_outdoors136, affect_outdoors137, affect_outdoors138, affect_outdoors139,
      affect_outdoors140, affect_outdoors141, self_exams_children162, self_exams_children163,
      self_exams_children164, self_exams_children165, self_exams_children166,
      self_exams_children167, self_exams_children168, self_exams_children169,
      self_exams_children170, self_exams_children171, self_exams_children172,
      self_exams_children173, self_exams_children174, kid_gender, valid_pastsixmonths29,
      total_body_sites_kidselfreport, total_body_sites_parentreportonkid,
      checked_body_parts94___1, checked_body_parts94___2, checked_body_parts94___3,
      checked_body_parts94___4, checked_body_parts94___5, checked_body_parts94___6,
      checked_body_parts94___7, checked_body_parts94___8, checked_body_parts94___9,
      checked_body_parts94___10, checked_body_parts94___11, checked_body_parts94___12,
      checked_body_parts94___13, checked_body_parts94___14, protectiveclothing58,
      protectiveclothing59, protectiveclothing60, protectiveclothing61, protectiveclothing62,
      protectiveclothing63, protectiveclothing64, protectiveclothing65, protectiveclothing66,
      protectiveclothing67, protectiveclothing68, protectiveclothing69, protectiveclothing70,
      protectiveclothing71, sunscreen73, sunscreen74, sunscreen75, sunscreen76, sunscreen77,
      sunscreen78, sunscreen79, sunscreen80, sunscreen81, sunscreen82, sunscreen83,
      sunscreen84, sunscreen85, sunscreen86, avoidsun88, avoidsun89, avoidsun90, avoidsun91,
      avoidsun92, avoidsun93, avoidsun94, avoidsun95, avoidsun96, avoidsun97, avoidsun98,
      avoidsunelse, examining100, examining101, examining102, examining103, examining104,
      examining105, examining106, examining107, examining108, examining109, examining110,
      examining111, examining112, examining113, examiningelse, SHS38, SHS88
    ), factor)
  )

# Convert to numeric
df <- df %>%
  mutate(
    adult_age1 = as.numeric(adult_age1),
    total_sunburn = as.numeric(total_sunburn)
  )
# 3. MODELING ----

## Set a seed
set.seed(123)

## Split the data into training and test sets ----
train_index <- createDataPartition(df$Sum_Columns, p = .8, list = FALSE)
train <- df[train_index, ]
test <- df[-train_index, ]

# Check and remove constant columns in the training set
train <- train[sapply(train, function(x) length(unique(x)) > 1)]
# Check and remove constant columns in the test set
test <- test[sapply(test, function(x) length(unique(x)) > 1)]


# Prepare the data for glmnet ----
x_train <- model.matrix(Sum_Columns ~ ., data = train)[,-1]
y_train <- train$Sum_Columns

x_test <- model.matrix(Sum_Columns ~ ., data = test)[,-1]
y_test <- test$Sum_Columns

# Get the names of columns in both datasets
train_cols <- colnames(x_train)
test_cols <- colnames(x_test)

# Find the common columns
common_cols <- intersect(train_cols, test_cols)

# Subset both datasets to exclude the common columns
x_train <- x_train[, (colnames(x_train) %in% common_cols)]
x_test <- x_test[, (colnames(x_test) %in% common_cols)]


# FIT MODEL ----
# Fit a Poisson model using lasso regression
fit <- glmnet(x_train, y_train, family = "poisson", alpha = 1)

# Tune the model using cv.glmnet
cv_fit <- cv.glmnet(x_train, y_train, family = "poisson", alpha = 1)

# Optimal lambda value
lambda_best <- cv_fit$lambda.min

# Refit the model using the optimal lambda value
fit_best <- glmnet(x_train, y_train, family = "poisson", alpha = 1, lambda = lambda_best)

# 4. PERFORMANCE ASSESSMENT ----

# Predict on the test set
predictions <- predict(fit_best, s = lambda_best, newx = x_test, type = "response")

# Calculate the mean absolute error
mae <- mean(abs(y_test - predictions))

# Print the mean absolute error
print(paste("Mean Absolute Error:", mae))

# 5. PLOTS ----

# Plot the cross-validation error
old_par <- par(mar = c(4, 4, 1, 1)) 
plot(cv_fit)
par(old_par)


# Plot the variable coefficients in the final model
plot(fit_best, xvar = "lambda", label = TRUE)

# Predictions
actual_values <- y_test

# Plotting the actual vs predicted values
plot(actual_values, predictions, main="Predicted vs Actual",
     xlab="Actual", ylab="Predicted", pch=19)
abline(a=0, b=1, col="red") # adds a y=x line

