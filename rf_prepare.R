#1) prepare the data for the "model_one" file
#and for the "visualize_improve" file


#clear the workspace
rm(list=ls())
library(tidyverse)
library(dplyr)
library(reshape2)
#read in the data set from kaggle : https://www.kaggle.com/datasets/camnugent/california-housing-prices
house= read.csv("housing.csv")

#first look at the data
head(house)



#checke for missing observations
sum(is.na(house))
#there are 207 observations where we detect missing entries 


#lets see where those observations are missing
countNAs <- function(data) {
  na_count <- sapply(data, function(y) sum(length(which(is.na(y)))))
  return(na_count)
}


na_counts <- countNAs(house)
print(na_counts)


#omit those missing entries for the total bedrooms variable as the data set is large enough
house = na.omit(house)

#check the distribution in the data 
summary(house)
#Income reaches form 0.5-15
#Median house value has some outliers


#
#feature engineering
str(house)
glimpse(house)
#now lets take a look at the ocean proximity, as it is a categorial feature
#Therefore, it's temporarily removed from the main dataset (housing_num) to prepare the data for models that require numeric input.
drop_vector = c('ocean_proximity')
house_num =  house[ , !(names(house) %in% drop_vector)]



#now build the special data frame for the ocean proximity variable
new_var = unique(house$ocean_proximity)

new_var_house = data.frame(ocean_proximity = house$ocean_proximity)

for(i in new_var){
  new_var_house[,i] = rep(0, times= nrow(new_var_house))
}
head(new_var_house)

#the filled ocean proximity table
for(i in 1:length(new_var_house$ocean_proximity)){
  ct = as.character(new_var_house$ocean_proximity[i])
  new_var_house[,ct][i] = 1
}

head(new_var_house)
#One hot encoding for Ocean Proximity feature
#which transforms the categorical variable into a format that can be provided to machine learning algorithms, by creating a binary column for each category and marking the presence of the category with a 1

column_names = names(new_var_house) #retrieves the names of all columns in new_var_house.
keep = column_names[column_names != 'ocean_proximity'] #identifies the columns to keep, essentially all columns except the original ocean_proximity column, since it's already been encoded into separate columns.
new_var_house = select(new_var_house,one_of(keep)) #to keep only the specified columns in new_var_house. This effectively removes the original ocean_proximity column, leaving only the one-hot encoded columns.

tail(new_var_house)


#now lets change our variables (to reduce correlations see visualizing)

#now lets face new bedroom variables 
house$mean_population = house$population/house$households # the average population per household
house$mean_bedrooms = house$total_bedrooms/house$households #average number of bedrooms per household
house$mean_rooms = house$total_rooms/house$households # the average number of rooms per household 


#drop the old features
drop_again = c('total_bedrooms', 'total_rooms', 'population')

house = house[ , !(names(house) %in% drop_again)] # updates the housing dataframe to keep only the columns not listed in drops.

head(house) 



#Now merge everything together

drops_3 = c('ocean_proximity','median_house_value')
house_num =  house[ , !(names(house) %in% drops_3)] #updates housing_num to include only the columns not listed in drops_1. This leaves you with a dataset of purely numerical features without the target variable.

head(house_num)


#create a new dataset called df, that we will use
df = cbind(new_var_house, house_num, median_house_value=house$median_house_value)

head(df)
str(df)



#here you could change the datasize 
#and as we have a humongous data set lets choose only 0.2 percent of the data
#subset_size <- floor(0.99 * nrow(cleaned_housing))


#sampled_indices <- sample(nrow(cleaned_housing), size = subset_size)


#cleaned_housing <- cleaned_housing[sampled_indices, ]
#house is now the new data set we will use for our prediction model



#change the column names for the RF: 
#we need to changes variable names of our columns for the ranger function
new_column_names <- c("nearBay", "lessoneOcean", "inland", "nearOcean","island")


colnames(df)[1:5] <- new_column_names



#check now descriptives again
summary(df)