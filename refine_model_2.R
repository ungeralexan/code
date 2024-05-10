#refine model 
#remove the features that have been detected as unimportant
features_to_remove <- c("island", "households", "mean_bedrooms","nearBay","nearOcean")  # start with the least important features

# Update the dataset by removing these features
df_2 <- df[, !(names(df) %in% features_to_remove)]

# Create new training and testing datasets without the removed features
train_reduced_2 = df_2[ix, ]
test_reduced_2 = df_2[-ix, ]

set.seed(21000)

model_2 <- ranger(
  formula = median_house_value ~ ., 
  data = train_reduced_2, 
  importance = "permutation", 
  max.depth = 4, 
  mtry = 4,
  num.trees = 500
)
print(model_2)

#
#performance on unseen data
testPred_2 = predict(model_2, data = test_reduced_2)

# Extract the actual and predicted values
actuals_2 <- test_reduced_2$median_house_value
predictions_2 <- testPred_2$predictions

# Calculate the MSE and RMSE for the reduced model
mse_rf_reduced_2 = mean((predictions_2 - actuals_2)^2)
rmse_reduced_2 = sqrt(mse_rf_reduced_2)

# Output the RMSE to see if there is an improvement
print(rmse_reduced_2)

#yes we get a better prediction performance


########################
#refine model second time 
features_to_remove <- c("island", "households", "mean_bedrooms","housing_median_age","nearOcean","nearBay")  # start with the least important features

# Update the dataset by removing these features
cleaned_housing_pfi <- df[, !(names(df) %in% features_to_remove)]

# Create new training and testing datasets without the removed features
train_reduced_pfi = cleaned_housing_pfi[ix, ]
test_reduced_pfi = cleaned_housing_pfi[-ix, ]

set.seed(21000)

fit_model_pfi <- ranger(
  formula = median_house_value ~ ., 
  data = train_reduced_pfi, 
  importance = "permutation", 
  max.depth = 4, 
  mtry = 4,
  num.trees = 500
)

testPred_pfi = predict(fit_model_pfi, data = test_reduced_pfi)

# Extract the actual and predicted values
actuals_pfi <- test_reduced_pfi$median_house_value
predictions_pfi <- testPred_pfi$predictions

# Calculate the MSE and RMSE for the reduced model
mse_rf_reduced_pfi = mean((predictions_pfi - actuals_pfi)^2)
rmse_reduced_pfi = sqrt(mse_rf_reduced_pfi)

# Output the RMSE to see if there is an improvement
print(rmse_reduced_pfi)
