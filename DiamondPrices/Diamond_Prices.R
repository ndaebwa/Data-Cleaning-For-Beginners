#libraries
library(tidyverse)
library(ggplot2)
library(readxl)

#importing the dataset
data <- read_excel("D:/Github/Charles/R/DiamondPices/DiamondPricesData.xlsx")
data

## Analysing the dataset.
## The types
# price - double
# carat - double
# cut - character
# color - character
# clarity - character
# deppth - double
# table - double
# x- double
# y - double
# z- double

type <- typeof(data$z)# nolint
type

## Finding the number of missing values in each feature
missing_values <- data %>% summarise_all(funs(sum(is.na(.))))
missing_values

## Since we have with depth missing values in our dataset, we shall  make any imputations. # nolint
ggplot(data, aes(x = depth)) +
  geom_histogram()
# since the graph is not skrewed, we are going to use mean when imputiting.
data$depth[is.na(data$depth)] <- mean(data$depth, na.rm = TRUE) # nolint

## checking if the missing values are removed.
missing_values_check <- data %>% summarise_all(funs(sum(is.na(.))))
missing_values_check

## beaking up the dataset into, only the categorical and continious data
continious_data <- data %>% select_if(is.double)
continious_data

categorical_data <- data %>% select_if(is.character)
categorical_data

#plotting so that we can see the outliers

continious_box <- function(variable, name) {
  ggplot(data, aes(x = "", y = variable)) +
    geom_boxplot() +
    ggtitle(paste("Box plot for", name, "before removing outliers"))
}

continious_box(data$price, "price")
continious_box(data$carat, "carat")
continious_box(data$depth, "depth")
continious_box(data$x, "x")
continious_box(data$table, "table")
continious_box(data$z, "z")
continious_box(data$y, "y")

### Using interquatile range to remove outliers

# plots
box_no_outlier <- function(data, variable, name) {
  ggplot(data, aes(x = "",  y = variable)) +
    geom_boxplot() +
    ggtitle(paste("Box plot of ", name, "without outliers"))

}

#price
IQR_price <- IQR(data$price) # nolint
IQR_price

# quatiles
price_quantiles <- quantile(data$price, probs = c(.25, .75), na.rm = FALSE)
price_quantiles

price_lower_quantile <- price_quantiles[1] - (1.5 * IQR_price)
price_upper_quantile <- price_quantiles[2] + (1.5 * IQR_price)

price_lower_quantile
price_upper_quantile

# filtering out the outliers
price_outlier <- subset(data, data$price<price_lower_quantile | data$price> price_upper_quantile) # nolint
price_outlier

box_no_outlier(price_outlier, price_outlier$price, "price")

#carat
IQR_carat <- IQR(data$carat) # nolint
IQR_carat

# quatiles
carat_quantiles <- quantile(data$carat, probs = c(.25, .75), na.rm = FALSE)
carat_quantiles

carat_lower_quantile <- carat_quantiles[1] - (1.5 * IQR_carat)
carat_upper_quantile <- carat_quantiles[2] + (3.5 * IQR_carat)

carat_lower_quantile
carat_upper_quantile

# filtering out the outliers
carat_outlier <- subset(data,data$carat<carat_lower_quantile | data$carat> carat_upper_quantile) # nolint
carat_outlier

box_no_outlier(carat_outlier, carat_outlier$carat, "carat")

#depth
IQR_depth <- IQR(data$depth) # nolint
IQR_depth

# quatiles
depth_quantiles <- quantile(data$depth, probs = c(.25, .75), na.rm = FALSE)
depth_quantiles

depth_lower_quantile <- depth_quantiles[1] - (2.5 * IQR_depth)
depth_upper_quantile <- depth_quantiles[2] + (2.5 * IQR_depth)

depth_lower_quantile
depth_upper_quantile

# filtering out the outliers
depth_outlier <- subset(data,data$depth<depth_lower_quantile | data$depth> depth_upper_quantile) # nolint
depth_outlier

box_no_outlier(depth_outlier, depth_outlier$depth, "depth")

#table
IQR_table <- IQR(data$table) # nolint
IQR_table

# quatiles
table_quantiles <- quantile(data$table, probs = c(.25, .75), na.rm = FALSE)
table_quantiles

table_lower_quantile <- table_quantiles[1] - (3.5 * IQR_table)
table_upper_quantile <- table_quantiles[2] + (2.5 * IQR_table)

table_lower_quantile
table_upper_quantile

# filtering out the outliers
table_outlier <- subset(data,data$table<table_lower_quantile | data$table> table_upper_quantile) # nolint
table_outlier

box_no_outlier(table_outlier, table_outlier$table, "table")

#x
IQR_x <- IQR(data$x) # nolint
IQR_x

# quatiles
x_quantiles <- quantile(data$x, probs = c(.25, .75), na.rm = FALSE)
x_quantiles

x_lower_quantile <- x_quantiles[1] - (1.5 * IQR_x)
x_upper_quantile <- x_quantiles[2] + (1.5 * IQR_x)

x_lower_quantile
x_upper_quantile

# filtering out the outliers
x_outlier <- subset(data, data$x < x_lower_quantile | data$x > x_upper_quantile)
x_outlier

box_no_outlier(x_outlier, x_outlier$x, "x")

#y
IQR_y <- IQR(data$y) # nolint
IQR_y

# quatiles
y_quantiles <- quantile(data$y, probs = c(.25, .75), na.rm = FALSE)
y_quantiles

y_lower_quantile <- y_quantiles[1] - (0.09 * IQR_y)
y_upper_quantile <- y_quantiles[2] + (0.09 * IQR_y)

y_lower_quantile
y_upper_quantile

# filtering out the outliers
y_outlier <- subset(data, data$y < y_lower_quantile | data$y > y_upper_quantile)
y_outlier

box_no_outlier(y_outlier, y_outlier$y, "y")

#z
IQR_z <- IQR(data$z) # nolint
IQR_z

# quatiles
z_quantiles <- quantile(data$z, probs = c(.25, .75), na.rm = FALSE)
z_quantiles

z_lower_quantile <- z_quantiles[1] - (1.5 * IQR_z)
z_upper_quantile <- z_quantiles[2] + (1.3 * IQR_z)

z_lower_quantile
z_upper_quantile

# filtering out the outliers
z_outlier <- subset(data, data$z < z_lower_quantile | data$z > z_upper_quantile)
z_outlier

box_no_outlier(z_outlier, z_outlier$z, "z")