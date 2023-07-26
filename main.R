# 1. SETUP ----

## Set working directory ----
getwd()
setwd("/Users/ezanaenquobahrie/Desktop/dataaware_project")
## Load libraries ----
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

# 
# # 2. MODELING ----
# 
# # Check data types of columns before conversion
# # sapply(df_combined, class)
# 
# ## Set Seed ----
# set.seed(123)
# ## Split dataset ----
# df_combined$id <- 1:nrow(df_combined)
# # Split the dataset into train and test
# train <- df_combined %>% dplyr::sample_frac(0.70)
# test  <- dplyr::anti_join(df_combined, train, by = 'id')
# 
# # Train_x and test_x
# train_x <- train %>% select(-c("class", "id"))
# test_x <- test %>% select(-c("class", "id"))
# 
# # Train_y and test_y
# train_y <- train %>% select(c("class"))
# test_y <- test %>% select(c("class"))
# 
# # Convert train_y and test_y to a vector
# train_y <- unlist(train_y)
# test_y <- unlist(test_y)
# 
# train_x2 <- train_x %>% select(-c(person, hour))
# test_x2 <- test_x %>% select(-c(person, hour))
# 
# ## Fit final model ----
# rf_final_model <-
#   randomForest(
#     x = train_x2,
#     y = train_y,
#     xtest = test_x2,
#     ytest = test_y,
#     mtry = 11,
#     importance = TRUE,
#     ntree = 5000,
#     do.trace = 10,
#   )
# 
# rf_final_model
# plot(rf_final_model)
# 
# save(rf_final_model,file = "RF_MODEL.RData")

load("RF_MODEL.RData")

rf_final_model
# 4. FIGURES ---- 
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
  geom_point(position = position_dodge(1)) 

print(plot)

plot +
  # Connecting line between 0 and the feature
  geom_linerange(aes(xmin = 0, xmax = rf_imp),
                 linetype = "solid",
                 position = position_dodge(.5)) +
  # Vertical line at 0
  geom_vline(xintercept = 0,
             linetype = "solid",
             color = "red") +
  # Adjust the scale if you need to based on your importance
  scale_x_continuous(limits = c(-50, 200)) +
  # Label the x and y axes
  labs(x = "Importance", y = "Feature") +
  # Make the theme pretty
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(family = "serif")) +
  guides(color = guide_legend(title = NULL)) +
  # Plot them in order of importance
  scale_y_discrete(limits = features$feature[order(features$rf_imp, decreasing = FALSE)])
## Scatterplot of variables ----
## Feature importance plot from random forest model ----
library(caret)
rf_features <- as.data.frame(varImp( rf_final_model))
### Rename the column name to rf_imp
colnames(rf_features) <- "rf_imp"

### convert rownames to column
rf_features$feature <- rownames(rf_features)

### Selecting only relevant columns for mapping
features <- rf_features %>% dplyr::select(c(feature, rf_imp))

### Plot the feature importance
plot <- features %>%
  ggplot(aes(x =  rf_imp, y = feature , color = "red")) +
  ### Creates a point for the feature importance
  geom_boxplot(position = position_dodge(1)) 

print(plot)

plot +
  ### Connecting line between 0 and the feature
  geom_linerange(aes(xmin = 0, xmax = rf_imp),
                 linetype = "solid",
                 position = position_dodge(.5)) +
  ### Vertical line at 0
  geom_vline(xintercept = 0,
             linetype = "solid",
             color = "red") +
  ### Adjust the scale if you need to based on your importance
  scale_x_continuous(limits = c(0, 120)) +
  ### Label the x and y axes
  labs(x = "Importance", y = "Feature") +
  ### Make the theme pretty
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(family = "serif")) +
  guides(color = guide_legend(title = NULL)) +
  ### Plot them in order of importance
  scale_y_discrete(limits = features$feature[order(features$rf_imp, decreasing = FALSE)])
## Boxplot of important features ----
# Load required libraries
#library(ggplot2)
#library(dplyr)

# Assuming you have a data frame named 'features' with columns 'rf_imp' and 'feature'

# Create the bar plot
plot <- features %>%
  ggplot(aes(x = reorder(feature, rf_imp), y = rf_imp, fill = "lightblue")) +
  geom_bar(stat = "identity") +
  # geom_segment(aes(xend = reorder(feature, rf_imp), yend = 0), linetype = "solid", color = "black") +
  geom_hline(yintercept = 0, linetype = "solid", color = "lightblue") +
  coord_flip() +
  scale_fill_manual(values = "lightblue") +
  labs(x = "Feature", y = "Importance") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(family = "serif"))

# Print the bar plot
print(plot)


##



# Load required libraries
library(pheatmap)

# Assuming you have a data frame named 'df_combined' with the relevant columns
# Replace 'xyz.mean', 'roll.mean', 'xyz.PSD.6', 'pitch.max.deviation', 'xyz.PSD.1', 'azimuth.mean', 'xyz.PSD.3', 'xyz.PSD.10'
# with the actual column names you want to include in the heatmap

# Select the relevant columns for the heatmap
selected_columns <- c("xyz.mean", "roll.mean", "xyz.PSD.6", "pitch.max.deviation",
                      "xyz.PSD.1", "azimuth.mean", "xyz.PSD.3", "xyz.PSD.10")

# Subset the data frame with the selected columns
selected_data <- df_combined[, selected_columns]

# Compute the correlation matrix
correlation_matrix <- cor(selected_data, use = "pairwise.complete.obs")

# Create the heatmap
pheatmap(correlation_matrix, 
         fontsize_row = 8, fontsize_col = 8, cellwidth = 25, cellheight = 15,
         color = colorRampPalette(c("blue", "white", "red"))(100),
         main = "Heatmap of Pairwise Correlation", 
         fontsize = 12, angle_col = 45)

