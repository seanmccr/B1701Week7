# ----- B1700 Assignment 1 | 27.10.23 | 202383028-----

# ----- 4.1 Data Preparation -----
##### 4.1.1 Loading packages #####
# Code to load packages to library
library(ggplot2)
library(tidyverse)
library(dplyr)

##### 4.1.2 Import remote file and check import #####
# Code to read the csv file and show first ten rows
my_data <- read.csv("https://www.dropbox.com/scl/fi/sl5uzrjkfaconpxausb4o/raw_data.csv?rlkey=20ewcyfvhmi2q4b9wx7poi3zz&dl=1")

# Code to show variable type given to each variable
head(my_data, n = 10)
str(my_data)

##### 4.1.3 Cleaning Data #####
# Code to remove 'NA' returns and the 'X' variable
my_data_02 <- na.omit(my_data)
my_data_03 <- subset(my_data_02, select = -c(X))

##### 4.1.4 Renaming Variables #####
# Code to rename the variables in the dataset
my_data_03 <- my_data_03 %>%
  rename(game_id = ID,
         opposition_id = opposing.team.name,
         home_game = at.home..,
         season_id = seas,
         home_score = score,
         oppos_score = AWAYcore,
         var_01 = A,
         var_02 = var2,
         possess_pcent = our_level_of_possession)
summary(my_data_03)

##### 4.1.5 Define Variable Types #####
# Code to change variable type to factor
my_data_03$season_id <- as.factor(my_data_03$season_id)

# Code to change variable type of another variable to factor
my_data_03$opposition_id <- as.factor(my_data_03$opposition_id)
str(my_data_03)

##### 4.1.6 Detect and Remove Observations w/Outliers #####
# Code to remove outliers from 'home_score' and 'oppos_score' variables
remove_outliers <- function(df, columns, z_threshold = 2) {
  df_cleaned <- df
  for (col in columns) {
    z_scores <- scale(df[[col]])
    outliers <- abs(z_scores) > z_threshold
    df_cleaned <- df_cleaned[!outliers, ]
  }
  df_cleaned
}
my_data_04 <- remove_outliers(my_data_03, c("home_score", "oppos_score"), 2)

# Code to show the range of the particular variables in each dataset
cat("Range of home_score in my_data_03:", range(my_data_03$home_score), "\n")
cat("Range of home_score in my_data_04:", range(my_data_04$home_score), "\n")

##### 4.1.7 Variable based on existing variables #####
# Code to create a new variable
my_data_04$goaldiff <- my_data_04$home_score - my_data_04$oppos_score

# Code to provide mean and standard deviation of 'goaldiff' variable
mean(my_data_04$goaldiff)
sd(my_data_04$goaldiff)

##### 4.1.8 Clean Environment #####
# Code to clean environment
rm(my_data, my_data_02, my_data_03, remove_outliers)


# ----- 4.2 Data Exploration -----

##### 4.2.1 Print Descriptive Statistics #####
# Code to provide certain descriptive statistics
mean(my_data_04$home_score)
median(my_data_04$home_score)
sd(my_data_04$home_score)
min(my_data_04$home_score)
max(my_data_04$home_score)

##### 4.2.2 Create Histogram #####
# Code to create a histogram of goal difference
gdplot <- ggplot(my_data_04, aes(x = goaldiff)) +
  geom_histogram()
gdplot

##### 4.2.3 Boxplot of Group #####
# Code to create a boxplot of season and home score
boxhomescore <- ggplot(my_data_04, aes(x = season_id, y = home_score)) +
  geom_boxplot()
boxhomescore

##### 4.2.4 Heatmap of Variables #####
# This is code forr the creation of a heatmap with home score, possesion percentage and opposition score
heatmapmd4 <- ggplot(my_data_04, aes(possess_pcent, home_score)) + 
  geom_tile(width = 1, height = 1, aes(fill = oppos_score)) +
  scale_fill_gradient(low = "white", high = "green") +
  labs(title = "Heatmap", x = "Percentage Possession", y = "Home Score") +
  theme_minimal()
heatmapmd4

##### 4.2.5 Scatterplot w/Linear Regression and name/label #####
# This is code for the creation of a scatterplot with regression line:
scattermyd4 <- ggplot(my_data_04, aes(x = possess_pcent, y = home_score)) +
  geom_point() + 
  labs(title = "Relationship between Percentage Possesion and Home Score", x = "Possession Percentage", y = "Home Score") +
  geom_smooth(method = 'lm') +
  theme_test()
scattermyd4









  