##############################################
# Time Series Analysis with R (TSAR)
# PART 2: Time Series Analysis
# Self-Study 1.2: Creating tsibble objects
# gwendolin.wilke@fhnw.ch
##############################################

#### 1. The USgas package

install.packages("USgas")
library(USgas)

us_tsibble <- us_total %>% 
  as_tsibble(index=year, 
             key=state)

# For each state
us_tsibble %>% 
  filter(state %in% c("Maine", "Vermont", "New Hampshire", "Massachusetts",
                      "Connecticut", "Rhode Island")) %>% 
  autoplot(y/1e3) +
  labs(y = "billion cubic feet")



#### 2. tourism.xlsx 

library(readxl)
my_tourism <- readxl::read_excel("tourism.xlsx") %>% 
  mutate(Quarter = yearquarter(Quarter)) %>% 
  as_tsibble(
    index = Quarter,
    key = c(Region, State, Purpose)
  )

my_tourism
tourism

my_tourism %>% 
  as_tibble() %>% 
  summarise(Trips = mean(Trips), .by=c(Region, Purpose)) %>% 
  filter(Trips == max(Trips))

state_tourism <- my_tourism %>% 
  group_by(State) %>% 
  summarise(Trips = sum(Trips)) %>% 
  ungroup()

state_tourism

