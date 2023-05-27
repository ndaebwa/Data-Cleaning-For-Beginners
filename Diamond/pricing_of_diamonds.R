# install.packages("readxl") # nolint
## Packages
library(readxl)
library(tidyverse)
library(DescTools)
library(corrplot)
library(psych)

## Import an view the data set
# An excel file
data <- read_excel("D:/Github/Charles/R/DataScience/pricingofdiamonds.xlsx", sheet = 1) # nolint
data

# data <- data[,-1] # nolint
data <- subset(data, select = -1)
data
## the other ways of reading data sets in R include;
## - using the read function for csv
## read.csv, read.table.

## - using the openxlsx and xlsx package, we can also use read.xlsx to read excel files. # nolint
class(data)


### Dealing with missing values
#- outliers can be delt with using different methods,
# *- deleting outliers, through omiting ie `na.omit`
# *- impututing for the missing values - filling the missing values with appropreate values basing on the type of data, # nolint

# getting to know the type of data
# ID - double
# carat - double
# cut - character
# colour - character
# clarity - character
# depth - double
# price - double
# x - double
# y - double
# P - character
# PC - character
column_type <- typeof(data$PC)
print(column_type)

# checking for the missing values
missing_values <- data %>% summarise_all(funs(sum(is.na(.))))
missing_values

## Grouping the data types
double_data <- data %>% select_if(is.double)
character_data <- data %>% select_if(is.character)

double_data
# using ggplot to plot the histogram for the double data
double_data_hist <- function(feature) {
    ggplot(double_data, aes_string(x = feature)) +
        geom_histogram()
}
double_data_hist(data$carat)
double_data_hist(data$price)
double_data_hist(data$x)
double_data_hist(data$y)
double_data_hist(data$depth)

### Imputing for the missing values

## Since we are only having two features with missing values, we are going to impute only for those # nolint
# the commented imputations show what would be done with they had missing values

# filling the missing values with the mean
# data$carat[is.na(data$carat)] <- median(data$carat, na.rm = TRUE) # nolint
# data$price[is.na(data$price)] <- median(data$price, na.rm = TRUE) # nolint
# data$x[is.na(data$x)] <- median(data$x, na.rm = TRUE) # nolint
# data$y[is.na(data$y)] <- median(data$y, na.rm = TRUE) # nolint
# data$depth[is.na(data$depth)] <- mean(data$depth, na.rm = TRUE) # nolint
# # imputing for the missing values in the character data with the mode
# data$cut[is.na(data$cut)] <- names(which.max(table(data$cut))) # nolint
# data$colour[is.na(data$colour)] <- names(which.max(table(data$colour))) # nolint
# data$clarity[is.na(data$clarity)] <- names(which.max(table(data$clarity))) # nolint

data$P[is.na(data$P)] <- names(which.max(table(data$P)))
data$PC[is.na(data$PC)] <- names(which.max(table(data$PC)))

# Checking after removing the missing values
data_no_missing_values <- data %>% summarise_all(funs(sum(is.na(.))))
data_no_missing_values
## Visiualizing after removing the missing values for the character data

ggplot(character_data, aes(x = cut)) +
    geom_bar(fill = "blue")
ggplot(character_data, aes(x = colour)) +
    geom_bar(fill = "red")
ggplot(character_data, aes(x = clarity)) +
    geom_bar(fill = "green")
ggplot(character_data, aes(x = P)) +
    geom_bar(fill = "brown")
ggplot(character_data, aes(x = PC)) +
    geom_bar(fill = "orange")


## Dealing with the outliers.
# using the boxplot to visualize the outliers
boxplot(data$carat)
boxplot(data$price)
boxplot(data$x)
boxplot(data$y)
boxplot(data$depth)

## a general boxplot for all the double data
boxplot(double_data)

## using the IQR to remove the outliers

# carat

carat_quatiles <- quantile(data$carat, probs = c(.25, .75), na.rm = TRUE)
H <- 1.5 * IQR(data$carat, na.rm = TRUE)
data <- data[which(data$carat > carat_quatiles[1] - H & data$carat < carat_quatiles[2] + H), ] # nolint
boxplot(data$carat)

ggplot(data, aes(x = "", y = carat)) +
    geom_boxplot()

# price
IQR_price <- IQR(data$price) # nolint
IQR_price

# finding the lower and lower quatiles
price_quatiles <- quantile(data$price, probs = c(.25, .75), na.rm = FALSE)
price_lower_quartile <- price_quatiles[1] - (1.5 * IQR_price)
price_upper_quartile <- price_quatiles[2] + (1.5 * IQR_price)
price_lower_quartile
price_upper_quartile

# filtering out the outliers for price
data <- data[which(data$price < price_lower_quartile | data$price > price_upper_quartile),] # nolint
boxplot(data$price)

# x
IQR_x <- IQR(data$x, na.rm = TRUE) # nolint

# finding the lower and lower quatiles
x_quatiles <- quantile(data$x, probs = c(.25, .75), na.rm = FALSE)
x_lower_quartile <- x_quatiles[1] - (1.5 * IQR_x)
x_upper_quartile <- x_quatiles[2] + (1.5 * IQR_x)
x_lower_quartile
x_upper_quartile

# filtering out the outliers for x
data <- data[which(data$x < x_lower_quartile | data$x > x_upper_quartile),] # nolint
boxplot(data$x)

# y
IQR_y <- IQR(data$y) # nolint

# finding the lower and lower quatiles
y_quatiles <- quantile(data$y, probs = c(.25, .75), na.rm = FALSE)

y_lower_quartile <- y_quatiles[1] - (1.5 * IQR_y)
y_upper_quartile <- y_quatiles[2] + (1.5 * IQR_y)
y_lower_quartile
y_upper_quartile

# filtering out the outliers for y
data <- data[which(data$y < y_lower_quartile | data$y > y_upper_quartile),]
boxplot(data$y)

# depth
IQR_depth <- IQR(data$depth) # nolint

# finding the lower and lower quatiles
depth_quatiles <- quantile(data$depth, probs = c(.25, .75), na.rm = FALSE)

depth_lower_quartile <- depth_quatiles[1] - (1.6 * IQR_depth)
depth_upper_quartile <- depth_quatiles[2] + (1.5 * IQR_depth)
depth_lower_quartile
depth_upper_quartile

# filtering out the outliers for depth
data <- data[which(data$depth < depth_lower_quartile | data$depth > depth_upper_quartile),] # nolint
boxplot(data$depth)

# boxplot for the double data after removing the outliers
current_double_data <- data %>% select_if(is.double)
boxplot(current_double_data)

# find the relationship between the variables

# For categorical data we can use the chi-square test
double_relationship <- function(feature1, feature2) {
    ggplot(data, aes_string(x = feature1, y = feature2)) +
        geom_point()
}

double_relationship("carat", "price")
double_relationship("carat", "x")
double_relationship("carat", "depth")
double_relationship("carat", "y")
double_retationship("x", "price")
double_relationship("x", "depth")
double_relationship("x", "y")
double_relationship("depth", "price")
double_relationship("depth", "y")
double_relationship("y", "price")

# this can also be displayed using a correlation matrix.
correlation_matrix <- cor(double_data)
correlation_matrix

# corr.test(double_data) #This is also another way of displaying the correlation matrix # nolint

# 2. for categorical data with continuous data we can use the ANOVA test
cat_cont_relationship <- function(feature1, feature2) {
    ggplot(data, aes_string(x = feature1, y = feature2)) +
        geom_boxplot()
}

# Aprroach 1
cat_cont_anova <- function(feature1, feature2) {
    anova <- aov(data[[feature1]] ~ data[[feature2]])
    summary(anova)
}
# Approach 2
perform_anova <- function(data, dependent_var, independent_var) {
    anoa_model <- aov(as.formula(paste(dependent_var, "~", independent_var)), data = data) # nolint
    anova_result <- summary(anoa_model)
    print(anova_result)
}

cat_cont_relationship("cut", "price")
cat_cont_anova("price", "cut")
perform_anova(data, "price", "cut")

cat_cont_relationship("color", "price")
cat_cont_anova("price", "colour")

cat_cont_relationship("clarity", "price")
cat_cont_anova("price", "clarity")

cat_cont_relationship("cut", "carat")
cat_cont_anova("carat", "cut")

cat_cont_relationship("colour", "carat")
cat_cont_anova("carat", "color")

cat_cont_relationship("clarity", "carat")
cat_cont_anova("carat", "clarity")


# The relationship between categorical and continious variables can be gotten by
# displaying the covariation between one and the other using a frequency polygon. # nolint
ggplot(data = data, mapping = aes(x = price)) +
    geom_freqpoly(mapping = aes(color = PC), binwidth = 500)

