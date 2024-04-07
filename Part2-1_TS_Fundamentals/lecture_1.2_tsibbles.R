# ====================================
# Time Series Analysis with R
# gwendolin.wilke@fhnw.ch
# This script contains code used in 
# Part 2, Lecture 1.2 Time Series in R
# ==================================== 


# ----- Loading fpp3

#install.packages("fpp3")
library(fpp3)

# data sets are included in the fpp3 package
data(package='fpp3')

# data sets are included in the tsibble package
data(package='tsibble')

# data sets are included in the tsibbledata package
data(package='tsibbledata')




# ----- Creating tsibble Objects

#### Define a time series object using tsibble()
y <- tsibble(
  Year = 2015:2019,
  Observation = c(123, 39, 78, 52, 110),
  index = Year
)

#### Define a time series object using as_tsibble

# Define a tibble (not a time series yet!)
z <- tibble(
  Month = c("2019 Jan", "2019 Feb", "2019 Mar", "2019 Apr", "2019 May"),
  Observation = c(50, 23, 34, 30, 25)
)

# Make it a monthly time series
z %>% 
  mutate(Month = yearmonth(Month)) %>%   # Convert to yearmonth class
  as_tsibble(index = Month) # Convert to tsibble


# ----- tsibble Objects with Multiple Time Series

# Example: olympic_running
olympic_running # the key indicates the variables that uniquely determine the different time series
olympic_running %>% print(n=100) # to see more than 10 rows of the tibble, we use print() or View()
olympic_running %>% arrange(Year) # sort by Year to see that a time index can occur more than once

olympic_running %>% distinct(Sex) # use distinct() to see the unique values of each key variable
olympic_running %>% distinct(Length)
olympic_running %>% distinct(Length, Sex) # we can also see the unique combinations of key variables


# ----- Using dplyr and tydr for tsibbles.

#### Example: PBS data
PBS 
PBS %>% distinct(Concession)
PBS %>% distinct(Type)
PBS %>% distinct(ATC1)
PBS %>% distinct(ATC2)

#### Assume we ask: “What is the total costs of prescriptions with ATC2 index A10?”

# First, we filter for ATC2 index A10:
PBS %>% 
  filter(ATC2 == "A10") 

# Use distinct to see that ACT2 index A10 only occurs in combination with ACT1 index A. 
# This explains the huge reduction in the number of time series included.
PBS %>% 
  filter(ATC2 == "A10") %>% 
  distinct(ATC1, ATC2) 

# We can plot the 4 time series using autoplot():
PBS %>% 
  filter(ATC2 == "A10") %>% 
  autoplot(Cost)

# In order to simplify our analysis, we may want to select only the Cost column:
PBS %>% 
  filter(ATC2 == "A10") %>%  
  select(Cost) 

# Now we use summarize() to calculate the total costs:
PBS %>% 
  filter(ATC2 == "A10") %>% 
  select(Cost) %>% 
  summarise(TotalC = sum(Cost)) 

# We can use autoplot() to plot the aggregated time series:
PBS  %>% 
  filter(ATC2 == "A10")  %>%  
  select(Cost) %>% 
  summarise(TotalC = sum(Cost)) %>% 
  autoplot(TotalC) 

# Finally, we use mutate() to create a new column with total costs scaled to Million $, 
# and we save the resulting tsibble so that we can reuse later.
#   Note: We use the *right assignment* operator (->) here! 
PBS |>
  filter(ATC2 == "A10") |>
  select(Month, Concession, Type, Cost) |>
  summarise(TotalC = sum(Cost)) |>
  mutate(Cost = TotalC / 1e6) -> a10 

saveRDS(a10, file = "a10.RDS")

# Plot the scaled version of a10
autoplot(a10, Cost) +
  labs(y = "$ (millions)",
       title = "Australian antidiabetic drug sales")

# ----- Reading csv files

# Example: prison_population.csv
prison <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv")

# Since the data is quarterly, we don't need the full date. 
# We convert it into a quarterly time object using yearquarter()
# and remove the original date column
prison %>% 
  mutate(Quarter = yearquarter(Date))  %>% 
  select(-Date) 

# To get an idea of the single time series contained in the tsibble, 
#use print() or View()
View(prison) 

# Every time index seems to be uniquely determined by a combination of State, Gender, Legal and Indigenous.
# We try setting them as key variable - if it does not work, as_tsibble() will give an error.
prison <- prison %>% 
  mutate(Quarter = yearquarter(Date))  %>%  # Since the data is quarterly, we don't need the full date.
  select(-Date) %>%  # And we can remove the original date column
  as_tsibble(key = c(State, Gender, Legal, Indigenous), # We define the key variables
             index = Quarter) # And the index variable

prison
