# 1. SETUP ----
## Set working directories ----

## Install Packages
install.packages("data.table")


## Load in libraries ----
library(tidyverse)
library(randomForest)
library(haven)
library(data.table)

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
## Of the six child protective behaviors, create a new column using `transmute`
df <- df %>%
  mutate(Protective_Barrier_Sum = rowSums(select(., starts_with("children_outdoors")), na.rm = TRUE))

print(df)

## new column should be a total of the number of behaviors taken up by the child
## This will be your outcome/y variable
## Make sure your count is a factor
df <- df %>%
  mutate(children_barrier = Protective_Barrier_Sum$children_barrier)

print(df)

# 3. MODELING ----

## Set a seed
set.seed(666)

## Split data into training and testing datasets ----
df$id <- 1:nrow(df)
train <- df %>% dplyr::sample_frac(0.70)
test  <- dplyr::anti_join(df, train, by = 'id')

## Fit initial random forest model with your outcome being the new total variable ----

## Tune model ----

## Fit final model ----

## Check accuracy ----

# 4. FIGURES ----

## Feature importance ----

## Other descriptives ----
## Once you know your important features, examine the distribution of variables on the y axis, and the x-axis is count of 

# Train_x and test_x
train_x <- train %>% select(-c("hospital_death", "id"))
test_x <- test %>% select(-c("hospital_death", "id"))

# Train_y and test_y
train_y <- train %>% select(c("hospital_death"))
test_y <- test %>% select(c("hospital_death"))

# Convert train_y and test_y to a vector
train_y <- unlist(train_y)
test_y <- unlist(test_y)

```


## Random Forest Model

### 1. Train a simple model 

Fit a randomforest model called `m1` that predicts hospital death.
Reminder, we use the `lm()` function to fit linear regression models, and `glm()` function to fit logistic regression models.

```{r}
# load the randomForest library
library(randomForest)
```

```{r}
# fit a simple model with x = train_x, y = train_y, xtest = test_x, ytest = test_y, importance = TRUE, and ntree = 5000

rf_model <- randomForest(
  x = train_x,
  y = train_y,
  xtest = test_x,
  ytest = test_y,
  importance = TRUE,
  ntree = 5000
)

```



### 2. Model Tuning

There are a few "hyperparameters" to tune, like the number of variables that each tree should consider during the learning process, which is called `mtry`. We can tune the model using the `tuneRF()` function in the `randomForest` package. 

```{r}
# find the best value for the mtry hyperparameter. Set the x, y, xtest, ytest as before. Set the ntreeTry value to 500 (it will build 500 trees per try), stepFactor to 1.5, improve = 0.01, trace = TRUE, and plot = TRUE 
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

```

We can also tune the number of trees itself. The random forest model you fit earlier, rf_model should have an error rate associated with a certain number of trees after which the error stabilizes for some time. The code below will find the iteration with the lowest error rate, and save it as `best_nrounds`. 
```{r}
rf_res_df <-
  data.frame(
    TRAINING_ERROR = rf_model$err.rate[,1],
    ITERATION = c(1:5000)
  ) %>%
  mutate(MIN = TRAINING_ERROR == min(TRAINING_ERROR))

best_nrounds <- rf_res_df %>%
  filter(MIN) %>%
  pull(ITERATION)

best_nrounds
```


Finally, fit your final model using the best `mtry` and `ntree` value you found

```{r}
rf_final_model <-
  randomForest(
    x = train_x,
    y = train_y,
    mtry = 6,
    importance = TRUE,
    ntree = 3000
  )

rf_final_model
```

## Feature Importance

A cool feature of random forests is that they are explainable, and it is easy to figure out what variables were the most informative for prediction.

Use the `varImp()` function to extract the variable importance from `rf_final_ model`. The code below will save it as a dataframe for plotting

```{r}
library(caret)
rf_features <- as.data.frame(varImp( rf_final_model))
```

The features extracted have a weird format (take a peek at the dataframe to see it) and so the few lines of code below make it plottable.
```{r}
## Rename the column name to rf_imp
colnames(rf_features) <- "rf_imp"

## convert rownames to column
rf_features$feature <- rownames(rf_features)

## Selecting only relevant columns for mapping
features <- rf_features %>% dplyr::select(c(feature, rf_imp))
```


Now, use `ggplot` to plot the variable importance in the `features` dataframe
The rf_imp should be on the x axis, feature should be on the y axis.
Use geom_point
```{r}
### Plot the feature importance
plot <- features %>%
  ggplot(aes(x =  rf_imp, y = feature , color = "#2E86AB")) +
  # Creates a point for the feature importance
  geom_point(position = position_dodge(0.5)) 

print(plot)
```

You can make this a little better with some ggplot arguments. 
```{r}
plot +
  # Connecting line between 0 and the feature
  geom_linerange(aes(xmin = 0, xmax = rf_imp),
                 linetype = "solid",
                 position = position_dodge(.5)) +
  # Vertical line at 0
  geom_vline(xintercept = 0,
             linetype = "solid",
             color = "grey70") +
  # Adjust the scale if you need to based on your importance
  scale_x_continuous(limits = c(-1, 5)) +
  # Label the x and y axes
  labs(x = "Importance", y = "Feature") +
  # Make the theme pretty
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(family = "serif")) +
  guides(color = guide_legend(title = NULL)) +
  # Plot them in order of importance
  scale_y_discrete(limits = features$feature[order(features$rf_imp, decreasing = FALSE)])
```

