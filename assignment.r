# Assignment 3: Using Data
#
# Before you get started:
# - Set your working directory to "source file location" using the Session menu
# - Run the following line of code to delete all variables in your workspace
#     (This will make it easier to test your script)
rm(list = ls())


### Built in R Data ###########################################################

# In this section, you'll work with the variable `Titanic`, a data set which is
# built into the R environment.
# This data set actually loads in a format called a *table*
# See https://cran.r-project.org/web/packages/data.table/data.table.pdf
# Use the `is.data.frame()` function to test if it is a table.
is.data.frame(Titanic)

# Create a variable `titanic_df` by converting `Titanic` into a data frame;
# you can use the `data.frame()` function or `as.data.frame()`
titanic_df <- data.frame(Titanic, stringsAsFactors = FALSE)

# It's important to understand the _meaning_ of each column before analyzing it
# Using comments below, describe what information is stored in each column
# For categorical variables, list all possible values
# Class: [1st, 2nd, 3rd, Crew]
# Sex: [Either 'Male' or 'Female']
# Age: [Either 'Adult' or 'Child']
# Survived: [Either 'Yes' or 'No']
# Freq: [The number of people in each categorical variable]


# Create a variable `children` that is a data frame containing only the rows
# from `titanic_df` with information about children on the Titanic
# Hints:
# - Filter rows using a vector of boolean values (like vector filtering)
# - See chapter 10.2.3
children <- titanic_df[titanic_df$Age == "Child", ]

# Create a variable `num_children` that is the total number of children.
# Hint: Remember the `sum()` function!
num_children <- sum(titanic_df$Freq)

# Create a variable `most_lost` that is the *row* from `titanic_df` with the
# largest absolute number of losses (people who did not survive)
# You can use multiple lines of code if you find that helpful
# to create this variable
# Hint: Filter for those who did not survive, then look for the row
who_did_not_survive <- titanic_df[titanic_df$Survived == "No", ]
most_lost <- who_did_not_survive[who_did_not_survive$Freq == 
                                   max(who_did_not_survive$Freq), ]

# Define a function called `survival_rate()` that takes in two arguments which
# must be in *the following order*:
# - a ticket class (e.g., "1st", "2nd"), and
# - the dataframe itself (it's good practice to explicitly pass in data frames)

# This function should return a sentence that states the *survival rate*
# (# survived / # in group) of adult men and "women and children" in that
# ticketing class.
# It should read (for example):
# >"Of Crew class, 87% of women and children survived and 22% of men survived."
#
# This is a complicated function! We recommend the following approach:
# - Filter for all rows representing the given ticketing class and save the
#   new data frame to a variable
# - Using this data frame, filter for all rows representing Adult Males
# - Find the total number of men and total number of male survivors to
#   calculate the survival rate
# - Likewise, use the data frame to filter for all Children and Adult Females
# - Perform the above calculation for this group as well
#
# Other approaches are also acceptable, please comment to explain what you do!
survival_rate <- function(ticket_class, data_frame) {
  #new overall data frame of given ticketing class
  given_class <- data_frame$Class == ticket_class
  #new data frame of adult males in given ticketing class
  adult_males_in_class <- given_class$Age == "Adult" & given_class$Sex == "Male"
  #variable for total men
  total_adult_males <- sum(adult_males_in_class$Freq)
  #variable for total male survivors
  total_male_survivors <- adult_males_in_class$Survivors == "Yes"
  #variable for survival rate of males
  male_survival_rate <- round((total_male_survivors/adult_males_in_class) * 100)
  #variable for total women and children
  women_and_children_in_class <- sum(given_class$Freq) - total_adult_males
  #total women and children and total women/children survivors
  total_women_and_children_survivors <- women_and_children_in_class$Survivors == "Yes"
  #variable for survival rate of women and children
  women_and_children_survival_rate <- round((total_women_and_children_survivors/
                                             women_and_children_in_class) * 100)
  #sentence stating survival rate
  sentence <- paste0("Of", sep = " ", ticket_class, sep = " ", "class,", 
                     sep = " ", women_and_children_survival_rate, "% of women
                     and children survived and", sep = " ", men_survival_rate, 
                     "% of men survived.")
  sentence
}

# Create variables `first_survived`, `second_survived`, `third_survived` and
# `crew_survived` by passing each class and the `titanic_df` data frame
# to your `survival_rate` function
first_survived <- survival_rate("1st", titanic_df)
second_survived <- survival_rate("2nd", titanic_df)
third_survived <- survival_rate("3rd", titanic_df)
crew_survived <- survival_rate("Crew", titanic_df)

# What notable differences do you observe in the survival rates across classes?
# Note at least 2 observations.
# [The higher social class has the larger survival rate. Adults were 
# significantly more likely to survive compared to children across all classes.]


# What notable differences do you observe in the survival rates between the
# women and children versus the men in each group?
# Note at least 2 observations.
# [Women and children survivors among all classes were significantly more than men.
# The survival rate for women and children in the 1st class was about 2.5 times
# more than men in the 1st class.]


### Reading in Data ###########################################################

# In this section, you'll work with .csv data of life expectancy by country
# First, download the csv file of `Life Expectancy` data from GapMinder:
# https://www.gapminder.org/data/
# You should save the .csv file into your `data` directory


# Before getting started, explore the GapMinder website to better understand
# the *original* source of the data (e.g., who calculated these estimates)
# Place a brief summary of the each data source here (e.g., 1 - 2 sentences
# per data source)
# [The 'Life Expectancy' data has three main sources that are combined together 
#  to span from 1800-2099. The v7 source by Mattias Lindgren covers the time
# period from 1800-1970 and this source itself is compiled from 100 sources on 
# huge, fatal disasters. THe IHME source covers the time period from 1970-2016
# which takes information from the Global Burden of Disease Study of 2017 done
# at the University of Washington. The UN source covers the time period from
# 2017-2099 and is based on the 'World Population Prospects 2019' file.]


# Using the `read.csv` function, read the life_expectancy_years.csv file into
# a variable called `life_exp`.
life_exp <- read.csv("data/life_expectancy_years.csv", )

# Write a function `get_col_mean()` that takes a column name and a data frame
# and returns the mean of that column. Make sure to properly handle NA values
# Hint: `mean()` takes in an argument called `na.rm`
get_col_mean <- function(column_name, life_exp) {
  mean(data_frame[[column_name]], na.rm = T)
}

# Create a list `col_means` that has the mean value of each column in the
# data frame (except the `Country` column). You should use your function above.
# Hint: Use an `*apply` function (lapply, sapply, etc.)
relevant_col <- colnames(life_exp)[1:300(life_exp)]
col_means <- as.list(sapply(relevant_col, get_col_mean))

# Create a variable `avg_diff` that is the difference in average country life
# expectancy between 1800 and 2018
avg_diff <- get_col_mean("2018", life_exp) - get_col_mean("1800", life_exp)

# Create a column `life_exp$change` that is the change in life
# expectancy from 2000 to 2018. Increases in life expectancy should
# be *positive*
life_exp$change <- life_exp$"2018" - life_exp$"2000"

# Create a variable `most_improved` that is the *name* of the country
# with the largest gain in life expectancy. Make sure to filter NA values
# Hint: `max()` takes in an argument called `na.rm`
most_improved <- life_exp[life_exp$change == max(life_exp$change, na.rm = T), "country"]

# Create a variable `num_small_gain` that has the *number* of countries
# whose life expectance has improved less than 1 year between 2000 and 2018
# Make sure to filter NA values
# Hint: Lookup `is.na()`
num_small_gain <- nrow(!is.na(life_exp[life_exp$change > 0 & life_exp$change < 1, ]))

# Write a function `country_change()` that takes in a country's name,
# two years as numbers (not strings), and the `life_exp` data frame
# Parameters should be written *in the above order*
# It should return the phrase:
# "Between YEAR1 and YEAR2, the life expectancy in COUNTRY went DIRECTION by
# SOME_YEARS years".
# Make sure to properly indictate the DIRECTION as "up" or "down"
# Hint: Use an if/else statement to help compute DIRECTION
country_change <- function(country_name, year_one, year_two, life_exp) {

  year_one_life_exp <- life_exp[life_exp$country == country_name, year_one]
  year_two_life_exp <- life_exp[life_exp$country == country_name, year_two]
  absolute_difference <- abs(life_exp[life_exp$country == country_name, year_one]
                             - life_exp[life_exp$country == country_name, year_two])
  
  if (year_one_life_exp > year_two_life_exp) {
    sentence <- paste0("Between", sep = " ", year_one, sep = " ", "and", sep = " "
                     , year_two, ", the life expectancy in", sep = " ", country_name
                     , sep = " ", "went up by", sep = " ", absolute_difference, sep = " ",
                     "years.")
  } else if (year_two_life_exp > year_one_life_exp) {
    sentence <- paste0("Between", sep = " ", year_one, sep = " ", "and", sep = " "
                     , year_two, ", the life expectancy in", sep = " ", country_name
                     , sep = " ", "went down by", sep = " ", absolute_difference, sep = " ",
                     "years.")
  } else if (year_one_life_exp == year_two_life_exp) {
    sentence <- paste0("Between", sep = " ", year_one, sep = " ", "and", sep = " "
                     , year_two, ", the life expectancy in", sep = " ", country_name
                     , sep = " ", "stayed the same.")
  }
  sentence
}

# Using your `country_change()` function, create a variable `sweden_change`
# that is the change in life expectancy from 1960 to 1990 in Sweden
sweden_change <- country_change("Sweden", 1960, 1990)

# Write a function `compare_change()` that takes in two country names and your
# `life_exp` data frame as parameters, and returns a sentence that describes
# their change in life expectancy from 2000 to 2018 (the `change` column)
# For example, if you passed the values "China", and "Bolivia" to you function,
# It would return this:
# "The country with the bigger change in life expectancy was China (gain=6.9),
#  whose life expectancy grew by 0.6 years more than Bolivia's (gain=6.3)."
# Make sure to round your numbers to one digit (though only after calculations)
# Hint: Use an if/else statement to paste the countries in the correct order
compare_change <- function(country_one, country_two, life_exp) {
  
  country_one_change <- round(life_exp[life_exp$country == country_one, "difference"], 1)
  country_two_change <- round(life_exp[life_exp$country == country_two, "difference"], 1)
  absolute_difference <- abs(country_two_change - country_one_change)
  
  if (country_one_change > country_two_change) {
    sentence <- paste0("The country with the bigger change in life expectancy
                     was", sep = " ", country_one, sep = " ", "(gain=", 
                     country_one_change,"), whose life expectancy grew by", 
                     sep = " ", absolute_difference, sep = " ", "years more than", sep =
                     " ", country_two, "'s (gain=", sep = " ", 
                     country_two_change, ").")
  } else if (country_two_change > country_one_change) {
    sentence <- paste0("The country with the bigger change in life expectancy
                     was", sep = " ", country_two, sep = " ", "(gain=", 
                       country_two,"), whose life expectancy grew by", 
                     sep = " ", absolute_difference, sep = " ", "years more than", sep =
                     " ", country_one, "'s (gain=", sep = " ", 
                     country_one_change, ").")
  } else if (country_one_change == country_two_change) {
    sentence <- paste0("Country One and Country Two had the same amount of 
                       change", sep = " ", change, "years, in their life expectancies")
  }
  sentence
}

# Using your `bigger_change()` function, create a variable `usa_or_france`
# that describes who had a larger gain in life expectancy (the U.S. or France)
usa_or_france <- compare_change("United States.", "France")

# Write your `life_exp` data.frame to a new .csv file to your
# data/ directory with the filename `life_exp_with_change.csv`.
# Make sure not to write row names.
write.csv(life_exp, "data/life_exp_with_change.csv", row.names = F)

