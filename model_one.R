#random forest model 
source("rf_prepare.R")


set.seed(5000000)
#load the packages we need 
#library(kernelshap) 
library(treeshap) #to apply the tree SHAP algorithm
library(ranger) #for the random forest model
library(shapviz) #for the shap visualisation
library(dplyr) #to do some data engineering
library(ggplot2) # for plotting
#library(pdp) # to visualize partial dependence plots




# Set a random seed so that same sample can be reproduced in future runs

# Randomly sample indices for the training set
ix = sample(nrow(df), 0.7 * nrow(df))

# Create training and testing datasets
train = df[ix, ]
test = df[-ix, ]

#store train and test set as csv to ensure reproducibility
#write.csv(train, "train_data.csv", row.names = FALSE)
#write.csv(test, "test_data.csv", row.names = FALSE)

# Confirm that the total number of rows matches the original dataset
print(nrow(train) + nrow(test) == nrow(df))

# Fitting the random forest model
original_model <- ranger(
  formula = median_house_value ~ ., 
  data = train, 
  importance = "permutation", 
  max.depth = 4, 
  mtry = 4,
  num.trees = 500
)

#check performance (OOB error and so on)
print(original_model)

# check feature importance
importance_ranger = original_model$variable.importance

# look at the values
print(importance_ranger)


#
####to get a more nuanced view lets plot the feature importance

# Create a data set for the plot
importance_df <- data.frame(Feature = names(importance_ranger), Importance = importance_ranger)

# Plotting 
png("Pfi.png", width = 1600, height = 1200, res = 150)
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col() +
  coord_flip() +  # Flip to make it horizontal
  labs(title = "Feature Importance in Random Forest Model",
       x = "Features",
       y = "Importance") +
  theme_minimal()
dev.off()
#median income clearly the most important feature




#What about the performance on unseen data 
#predictions for test data
testPred = predict(original_model, data = test)

#compare with the actual values
actuals <- test$median_house_value
predictions <- testPred$predictions

# calculate MSE
mse_rf = mean((predictions - actuals)^2)

# calculate rmse
test_rmse = sqrt(mse_rf)
test_rmse
# Correct calculation of MAE
#MAE = mean(abs(predictions - actuals))
#MAE


#some plotting options for performance
# Calculating residuals
residuals <- predictions - actuals

# Plotting residuals
ggplot(data = data.frame(Actuals = actuals, Residuals = residuals), aes(x = Actuals, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals Plot", x = "Actual House Values", y = "Residuals") +
  theme_minimal()

#residual plot is normally distributed
ggplot(data = data.frame(Residuals = residuals), aes(x = Residuals)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency") +
  theme_minimal()

#predictions vs actuals
ggplot(data = data.frame(Actuals = actuals, Predictions = predictions), aes(x = Actuals, y = Predictions)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predictions vs Actuals", x = "Actual House Values", y = "Predicted House Values") +
  theme_minimal()

##############################
# Extra analysis to assess how good my model is 
# Calculate and print summary statistics for median house values
summary_stats <- summary(cleaned_housing$median_house_value)
print(summary_stats)


# Calculate mean and median of the house values
mean_value_mae <- mean(cleaned_housing$median_house_value)
median_value_mae <- median(cleaned_housing$median_house_value)

# Calculate MAE as a percentage of mean and median
mae_percentage_mean <- (MAE / mean_value_mae) * 100
mae_percentage_median <- (MAE / median_value_mae) * 100

print(paste("MAE as a percentage of the mean: ", round(mae_percentage_mean, 2), "%"))
print(paste("MAE as a percentage of the median: ", round(mae_percentage_median, 2), "%"))

# Calculate errors
errors <- predictions - actuals

# Plot error distribution
hist(errors, breaks = 50, main = "Distribution of Prediction Errors", xlab = "Prediction Error")
#############

########
#SHAP analysis
#now lets see what tree SHAP detects 

#lets explain our predictions with TreeSHAP
unified_model = ranger.unify(original_model,test[-14])

#use package treeshap: https://cran.r-project.org/web/packages/treeshap/index.html
shaps <- treeshap(unified_model,  test[-14] , interactions = FALSE)
shp = shapviz(shaps, X=test) # for visualization https://cran.r-project.org/web/packages/shapviz/index.html


#Global scale
#SHAP importance plots
png("importance.png", width = 1600, height = 1200, res = 150)
sv_importance(shp)
dev.off()

#importance with numerical values
png("importance_num.png", width = 1600, height = 1200, res = 150)
sv_importance(shp, show_numbers = TRUE)
dev.off()

#beeswarm plots
png("importance_bee.png", width = 1600, height = 1200, res = 150)
sv_importance(shp, kind="bee")
dev.off()


#variable importance plot with the original package (to compare)
png("original_importance.png", width = 1600, height = 1200, res = 150)
plot_feature_importance(shaps, max_vars = 13)
dev.off()


#
#SHAP dependece plots

#longitude, the feature with the strongest interaction is selected on default
png("dependnece.png", width = 1600, height = 1200, res = 150)
sv_dependence(shp, v = "longitude", color_var = "auto")
dev.off()


#households
png("dependnece_households.png", width = 1600, height = 1200, res = 150)
sv_dependence(shp, v = "households", color_var = "auto")
dev.off()

#island
png("dependnece_island.png", width = 1600, height = 1200, res = 150)
sv_dependence(shp, v = "island", color_var = "auto")
dev.off()

#mean rooms
png("dependnece_rooms.png", width = 1600, height = 1200, res = 150)
sv_dependence(shp, v = "mean_rooms", color_var = "inland")
dev.off()

#median income
png("dependnece_income.png", width = 1600, height = 1200, res = 150)
sv_dependence(shp, v = "median_income", color_var = "auto")
dev.off()

###
#real feature dependence plots
png("dependnece_incomereal.png", width = 1600, height = 1200, res = 150)
plot_interaction(shaps, "median_income", "inland")
dev.off()

png("dependnece_roomreal.png", width = 1600, height = 1200, res = 150)
plot_interaction(shaps, "mean_rooms", "inland")
dev.off()

png("dependnece_longreal.png", width = 1600, height = 1200, res = 150)
plot_interaction(shaps, "longitude", "inland")
dev.off()


#Local analysis
# for observation three
png("waterfallt.png", width = 1600, height = 1200, res = 150)
sv_waterfall(shp,row_id = 3)
dev.off()

#force plot another visualization tool
sv_force(shp,row_id = 3)

png("waterfallte.png", width = 1600, height = 1200, res = 150)
plot_contribution(shaps, obs = 3, min_max = c(0, 300000))
dev.off()




