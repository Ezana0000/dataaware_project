# 1. SETUP ----

## Set working directory ----
getwd()
## Load libraries ----
install.packages("randomForest")
install.packages("ggplot2")

library(randomForest)
library(ggplot2)
library(tidyverse)




## Read in data ----
## This will work if your working directory is set correctly
df <- load("df_combined.rda")


# 2. MODELING ----

##
df <- na.omit(df)
print(df_combined)

# Check data types of columns before conversion
sapply(df_combined, class)



## Set Seed ----
set.seed(123)
## Split dataset ----
df_combined$id <- 1:nrow(df_combined)
# Split the dataset into train and test
train <- df_combined %>% dplyr::sample_frac(0.70)
test  <- dplyr::anti_join(df_combined, train, by = 'id')

# Train_x and test_x
train_x <- train %>% select(-c("class", "id"))
test_x <- test %>% select(-c("class", "id"))

# Train_y and test_y
train_y <- train %>% select(c("class"))
test_y <- test %>% select(c("class"))

# Convert train_y and test_y to a vector
train_y <- unlist(train_y)
test_y <- unlist(test_y)


## Fit initial model ----
library(randomForest)

rf_model <- randomForest(
  x = train_x,
  y = train_y,
  xtest = test_x,
  ytest = test_y,
  importance = TRUE,
  ntree = 5000
)

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

## Tune model ----
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

## Fit final model ----
rf_final_model <-
  randomForest(
    x = train_x,
    y = train_y,
    mtry = 22,
    importance = TRUE,
    ntree = 3000
  )

rf_final_model


## Check accuracy ----
# Use the final random forest model to make predictions on the test data
predictions <- predict(rf_final_model, test_x)

# Compare the predictions with the actual test labels (test_y)
accuracy <- sum(predictions == test_y) / length(test_y)

# Print the accuracy
print(paste("Model Accuracy: ", round(accuracy * 100, 2), "%"))



# 4. FIGURES ---- 

## Scatterplot of variables ----
## Feature importance plot from random forest model ----
##
library(caret)
rf_features <- as.data.frame(varImp( rf_final_model))

## Rename the column name to rf_imp
colnames(rf_features) <- "rf_imp"

## convert rownames to column
rf_features$feature <- rownames(rf_features)

## Selecting only relevant columns for mapping
features <- rf_features %>% dplyr::select(c(feature, rf_imp))

### Plot the feature importance
plot <- features %>%
  ggplot(aes(x =  rf_imp, y = feature , color = "#2E86AB")) +
  # Creates a point for the feature importance
  geom_point(position = position_dodge(0.5)) 

print(plot)
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
  scale_x_continuous(limits = c(0, 1)) +
  # Label the x and y axes
  labs(x = "Importance", y = "Feature") +
  # Make the theme pretty
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(family = "serif")) +
  guides(color = guide_legend(title = NULL)) +
  # Plot them in order of importance
  scale_y_discrete(limits = features$feature[order(features$rf_imp, decreasing = FALSE)])

## Boxplot of important features ----


