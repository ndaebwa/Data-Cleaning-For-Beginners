## who_covid19 dataset data cleaning
library(tidyverse)
library(ggplot2)
library(dplyr)

## Reading the dataset
data <- read_csv("C:/Users/Hello/Desktop/DS Python and R/Datasets/who_covid19.csv") # nolint
data

### understanding the dataset
#- 1. How many rows and columns are there in the dataset?
dim(data)

# - 2. The datatype for the unduvidual columns
str(data)
#         Name = col_character(), # nolint
#   ..   `WHO Region` = col_character(),
#   ..   `Cases - cumulative total` = col_double(),
#   ..   `Cases - cumulative total per 100000 population` = col_double(),
#   ..   `Cases - newly reported in last 7 days` = col_double(),
#   ..   `Cases - newly reported in last 7 days per 100000 population` = col_double(),# nolint
#   ..   `Cases - newly reported in last 24 hours` = col_double(),
#   ..   `Deaths - cumulative total` = col_double(),
#   ..   `Deaths - cumulative total per 100000 population` = col_double(),
#   ..   `Deaths - newly reported in last 7 days` = col_double(),
#   ..   `Deaths - newly reported in last 7 days per 100000 population` = col_double(), #nolint
#   ..   `Deaths - newly reported in last 24 hours` = col_double()

# Since the name of the columns are long, we can rename them with shorter names
colnames(data) <- c(
    "Country", "Region", "Cumulative total cases", "Cumulative total cases per 100000 population", # nolint
    "New cases in 7 days", # nolint
    "New cases in 7 days per 100000 population", "New cases in the 24 hours", "Total cumulative deaths", # nolint
    "Total cumulative deaths per 100000 population", "Deaths in 7 days", # nolint
    "Deaths in 7 days per 100000 population", "Deaths in 24 hours"
) # nolint
###
### Checking for missing values###
misssing_values <- data %>% summarise_all(funs(sum(is.na(.))))
misssing_values

# we can seen that we have only one missing value form any variable that has any missin values. #nolint
# we can drop the missing value
data <- na.omit(data)

# checking whether we have any remaining missing values
sum(is.na(data))

### Checking for outliers
# we will check for outliers in the numerical variables
# we will use boxplots to check for outliers

numeric_features <- data %>% select_if(is.double)
numeric_features

# plotting
box_plot <- function() {
    for (z in names(numeric_features)) {
        boxplot(numeric_features[[z]], main = z)
    }
}
box_plot()

# we can see that we have outliers in the following variables
# Removing outliers
removing_outliers <- function(data, columns) {
    for (column in columns) {
        iqr_z <- IQR(data[[column]])
        quantiles <- quantile(data[[column]], probs = c(.25, .75), na.rm = TRUE) # nolint
        q1 <- quantiles[1] - 1.5 * iqr_z
        q2 <- quantiles[2] + 1.5 * iqr_z
        data <- data[which((data[[column]] >= q1) & (data[[column]] <= q2)), ] # nolint
        # data <- data[which((data[[column]] >= lower_quantile) & (data[[column]] <= upper_quantile)), ] # nolint
    }
    return(data)
}

processed_data <- removing_outliers(numeric_features, c(
    "Cumulative total cases", "Cumulative total cases per 100000 population", # nolint
    "New cases in 7 days", # nolint
    "New cases in 7 days per 100000 population", "New cases in the 24 hours", "Total cumulative deaths", # nolint
    "Total cumulative deaths per 100000 population", "Deaths in 7 days", # nolint
    "Deaths in 7 days per 100000 population", "Deaths in 24 hours"
)) # nolint
boxplot(processed_data[["Cumulative total cases"]], main = "Cumulative total cases") # nolint
boxplot(processed_data[["Cumulative total cases per 100000 population"]], main = "Cumulative total cases per 100000 population") # nolint
