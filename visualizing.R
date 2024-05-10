# this script is independent from rf_prepare
#Here we observe the distributions of the varibales
# and calculate the correlations before and after feature engineering
library(ggplot2)
rm(list=ls())
house = read.csv("housing.csv")


#how many missing observations do we have
sum(is.na(house))
#there are 207 

#Lets see which features have missing observations
#function
countNAs <- function(data) {
  na_count <- sapply(data, function(y) sum(length(which(is.na(y)))))
  return(na_count)
}


na_counts <- countNAs(house)
print(na_counts)


#Omit the missing values in the total bedrooms variable
house = na.omit(house)

#what abou the categorial Ocean Proximity variable
# Calculating the frequency of each category
category_counts <- table(house$ocean_proximity)
category_data <- as.data.frame(category_counts)
names(category_data) <- c("Category", "Frequency")

png("ocean_prox.png", width = 1600, height = 1200, res = 150)
# Creating the bar plot with labels
ggplot(category_data, aes(x = Category, y = Frequency, fill = Category)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) + # "identity" to use actual frequencies
  geom_text(aes(label = Frequency), vjust = -0.3, color = "black", size = 3.5) + # Adding text labels on bars
  scale_fill_brewer(palette = "Set1") + # Different colors for each category
  labs(title = "Frequency of Categories in Ocean Proximity",
       x = "Ocean Proximity",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Improve x-axis label readability
dev.off()

#now check the distribution of the remaining numerical features
#therefore we first drop the ocean proximity feature to get just a numerical data set
drop_vector = c('ocean_proximity')
house_num =  house[ , !(names(house) %in% drop_vector)]




#now plot the map 
library(leaflet)


coords= house %>% select(longitude,latitude)

themap = coords %>%
  as.matrix() %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(radius = 1)
png("map.png", width = 1600, height = 1200, res = 150)
themap
dev.off()

#create one plot to show all the histograms
png("distributions_num.png", width = 1600, height = 1200, res = 150)
ggplot(data = melt(house_num), mapping = aes(x = value)) + 
  geom_histogram(bins = 50) + facet_wrap(~variable, scales = 'free_x')
dev.off()

#now lets do a correlation analysis with this data 
#we will be using the Spearman correlation as there is no clear normal distribution visible
cor_one_variables = house_num
cor_matrix <- cor(cor_one_variables, method="spearman")  # or use method="spearman" if the data is not normally distributed

library(corrplot)
# Visualize the correlation matrix
png("spearman_one.png", width = 1600, height = 1200, res = 150)
corrplot(cor_matrix, method="number", type="lower")
dev.off()

#correlations two
#check the correlations among the acomodation variables, only for me
corr_accom <- cor_one_variables[, !names(cor_one_variables) %in% c("longitude", "latitude", "housing_median_age", "median_income", "median_house_value")]

cor_matrix_sec <- cor(corr_accom, method="spearman")  # or use method="spearman" if the data is not normally distributed

png("corr_accom.png", width = 1600, height = 1200, res = 150)
corrplot(cor_matrix_sec, method="number", type="lower")
dev.off()





#now lets mitigate the correlations with feature engineering
#create the new acomodation variables to reduce correlations by averaging with the household feature
house_num$mean_population = house$population/house$households # the average population per household
house_num$mean_bedrooms = house$total_bedrooms/house$households #average number of bedrooms per household
house_num$mean_rooms = house$total_rooms/house$households # the average number of rooms per houshold 



# drop the old acomodation features
drop_the_old = c('total_bedrooms', 'total_rooms', 'population')

house_num = house_num[ , !(names(house_num) %in% drop_the_old)] # updates the housing dataframe to keep only the columns not listed in drops.

#check whether the new features are introduced and the old are ommited 
head(house_num) 

#now lets calculate the correlations for our new features to see whether we effectively reduced correlations
#we will be using the Spearman correlation as there is no clear normal distribution visible
cor_newvar = house_num
cor_matrix <- cor(cor_newvar, method="spearman")  # or use method="spearman" if the data is not normally distributed

library(corrplot)
# Visualize the correlation matrix
png("spearman_two.png", width = 1600, height = 1200, res = 150)
corrplot(cor_matrix, method="number", type="lower")
dev.off()

