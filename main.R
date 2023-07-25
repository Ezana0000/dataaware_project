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

# Drop the column named "column_to_drop"
df_combined <- df_combined %>%
  mutate(class = as.factor(class))

# Print the updated data frame
print(df_combined)


# 2. MODELING ----

# Check data types of columns before conversion
# sapply(df_combined, class)

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


# ## Fit initial model ----
# rf_model <- randomForest(
#   x = train_x,
#   y = train_y,
#   xtest = test_x,
#   ytest = test_y,
#   importance = TRUE,
#   ntree = 5000
# )
# 
# # find the best value for the mtry hyperparameter. Set the x, y, xtest, ytest as before. Set the ntreeTry value to 500 (it will build 500 trees per try), stepFactor to 1.5, improve = 0.01, trace = TRUE, and plot = TRUE 
# mtry <- tuneRF(
#   x = train_x,
#   y = train_y,
#   xtest = test_x,
#   ytest = test_y,
#   ntreeTry = 5000,
#   stepFactor = 1.5,
#   improve = 0.01,
#   trace = TRUE,
#   plot = TRUE
# )
# 
# # The code below will save the best value for the mtry and print it out
# best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
# print(mtry)
# print(best.m)
# 
# ## Tune model ----
# control <- trainControl(method='repeatedcv', 
#                         number=10, 
#                         repeats=3, 
#                         search='grid')
# #create tunegrid with 15 values from 1:15 for mtry to tunning model. Our train function will change number of entry variable at each split according to tunegrid. 
# tunegrid <- expand.grid(.mtry = (1:15)) 
# 
# rf_gridsearch <- train(Class ~ ., 
#                        data = df_combined,
#                        method = 'rf',
#                        metric = 'Accuracy',
#                        tuneGrid = tunegrid)
# print(rf_gridsearch)

# Create a vector of unique user IDs
users_all <- unique(df_combined$person)

# Initialize vectors for probabilities, predictions and actual class with NA
prob_rf <- pred_rf <- actual <- rep(NA, length(users_all))

# Loop over each unique user
for (i in seq_along(users_all)) {
 
  # Create a boolean mask for selecting data of the current user
  indices_user_select <- df_combined$person == users_all[i]
 
  # Train a Random Forest model using all data except for the current user's
  mod <- randomForest(
    class ~ hour, person, xyz.mean, xyz.absolute.deviation, xyz.standard.deviation, xyz.max.deviation, xyz.PSD.1, xyz.PSD.3, xyz.PSD.6, xyz.PSD.10, azimuth.mean, azimuth.absolute.deviation, azimuth.standard.deviation, azimuth.max.deviation, pitch.mean, pitch.absolute.deviation, pitch.standard.deviation, pitch.max.deviation, roll.mean, roll.absolute.deviation, roll.standard.deviation, roll.max.deviation, id,
    data = df_combined[-indices_user_select,],
    ntree = 1000,
    importance = TRUE,
    do.trace = 10
  )
  
 
  # Use the trained model to predict the class of the current user's data,
  # then calculate the mean probability that the predicted class is "PD"
  prob <- mean(predict(mod, df_combined[indices_user_select,], type = "response") == "PD")
 
  # Create a prediction for the current user: if the calculated probability is
  # more than 0.5, it predicts "PD"; otherwise, it predicts "Control"
  pred <- ifelse(prob > 0.5, "PD", "Control")
 
  # Store the calculated probability, prediction, and actual class for the current user
  prob_rf[i] <- prob
  pred_rf[i] <- pred
  actual[i] <- unique(as.character(df_combined$class[indices_user_select]))
}



# Create a DataFrame with the calculated probabilities, the predictions,
# and the actual classes for all unique users
data.frame(prob_rf, pred_rf, actual)




## Fit final model ----
rf_final_model <-
  randomForest(
    x = train_x,
    y = train_y,
    mtry = 11,
    importance = TRUE,
    ntree = 1000
  )

rf_final_model





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


