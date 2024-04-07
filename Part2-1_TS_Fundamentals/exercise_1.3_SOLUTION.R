##############################################
# Time Series Analysis with R (TSAR)
# PART 2: Time Series Analysis
# Exercise 1.3: Time Plots, Seasonal Plots, and ACF Plots
# gwendolin.wilke@fhnw.ch
##############################################

#### 1. aus_arrivals data

aus_arrivals %>% 
  autoplot(Arrivals)
# Generally the number of arrivals to Australia is increasing over the entire series, 
# with the exception of Japanese visitors which begin to decline after 1995. 
# The series appear to have a seasonal pattern which varies proportionately to the number of arrivals. Interestingly, the number of visitors from NZ peaks sharply in 1988. The seasonal pattern from Japan appears to change substantially.

aus_arrivals %>%  
  gg_season(Arrivals, labels = "both")
# The seasonal pattern of arrivals appears to vary between each country. 
# In particular, arrivals from the UK appears to be lowest in Q2 and Q3, and 
# increase substantially for Q4 and Q1. Whereas for NZ visitors, the lowest 
# period of arrivals is in Q1, and highest in Q3. Similar variations can be seen for Japan and US.

aus_arrivals %>%  
  gg_subseries(Arrivals)
# The subseries plot reveals more interesting features. 
# It is evident that whilst the UK arrivals is increasing, most of this increase is seasonal. 
# More arrivals are coming during Q1 and Q4, whilst the increase in Q2 and Q3 is less extreme. 
# The growth in arrivals from NZ and US appears fairly similar across all quarters. 
# There exists an unusual spike in arrivals from the US in 1992 Q3.

# Unusual observations:
# - 2000 Q3: Spikes from the US (Sydney Olympics arrivals)
# - 2001 Q3-Q4 are unusual for US (9/11 effect)
# - 1991 Q3 is unusual for the US (Gulf war effect?)

#### 2. Matching time plots and ACF plots

# 1-B, 2-A, 3-D, 4-C

