# 1. SETUP ----
## Set working directories ----

## Install Packages
install.packages("data.table")
install.packages("caret")
install.packages("glmnet")



## Load in libraries ----
library(tidyverse)
library(randomForest)
library(haven)
library(data.table)
library(caret)
library(readr)
library(glmnet)

# 2. DATA CLEANING ----
## Read in in the dataset ----
df <- read_sav("SPARK Data.sav")

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
write_csv(df, path = "C:/Users/logan/Downloads/Melanoma/my_data.csv")

## Make sure your count is a factor
library(dplyr)

# Assuming you have a data frame called 'df'

# Convert selected variables to factors
df <- df %>%
  mutate(
    across(c(
      adult_gender2, adult_gender, adult_gender4, adult_gender5, adult_gender6, adult_gender7,
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
set.seed(666)

## Split data into training and testing datasets ----
df$id <- 1:nrow(df)

train <- df %>% dplyr::sample_frac(0.70)
test  <- dplyr::anti_join(df, train, by = 'id')

## Train_x and test_x
train_x <- train %>% select(-c("Sum_Columns", "id"))
test_x <- test %>% select(-c("Sum_Columns", "id"))

## Train_y and test_y
train_y <- train %>% select(c("Sum_Columns"))
test_y <- test %>% select(c("Sum_Columns"))

## Convert train_y and test_y to a vector
train_y <- unlist(train_y)
test_y <- unlist(test_y)
      
## Fit initial random forest model with your outcome being the new total variable ----
rf_model <- randomForest(
  x = train_x,
  y = train_y,
  xtest = test_x,
  ytest = test_y,
  importance = TRUE,
  ntree = 5000
)

## Tune model ----
mtry <- tuneRF(
  x = train_x,
  y = train_y,
  xtest = test_x,
  ytest = test_y,
  ntreeTry = 5000,
  stepFactor = 1.5,
  improve = 0.01,
  trace = TRUE,
  plot = TRUE
)

# The code below will save the best value for the mtry and print it out
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

## Fit final model ----
rf_final_model <-
  randomForest(
    x = train_x,
    y = train_y,
    mtry = 6,
    importance = TRUE,
    ntree = 3000
  )

## Check accuracy ----
summary(rf_final_model)

# 4. FIGURES ----

## Feature importance ----
rf_features <- as.data.frame(varImp( rf_final_model))
```

## Rename the column name to rf_imp
colnames(rf_features) <- "rf_imp"

## convert rownames to column
rf_features$feature <- rownames(rf_features)

## Selecting only relevant columns for mapping
features <- rf_features %>% dplyr::select(c(feature, rf_imp))




## Other descriptives ----
## Once you know your important features, examine the distribution of variables on the y axis, and the x-axis is count of 

# LM Reggression Model

## Fit the linear regression model
my_lm_model <- lm(Sum_Columns ~ adult_age1, adult_education_level3, adult_income5, data = df)
summary(my_lm_model)


# Lasso Regresssion Model

## Load in data
set.seed(123)
n <- 100
p <- 5

y <- df

x <- data.matrix(df)

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_model) 

