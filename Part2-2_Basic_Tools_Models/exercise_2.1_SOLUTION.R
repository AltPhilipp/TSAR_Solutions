##############################################
# Time Series Analysis with R (TSAR)
# PART 2: Time Series Analysis
# Exercise 2.1: Transformations and Adjustments
# gwendolin.wilke@fhnw.ch
##############################################

library(fpp3)


# ------- 1. Transformations

#### United States GDP from global_economy

us_economy <- global_economy |>
  filter(Country == "United States")

# Trend appears exponential, a log transformation would be useful.
us_economy |>
  autoplot(GDP)

# Try a log transformation to the GDP 
#   Remember: A Box-Cox transformation with lambda = 0 is equivalent to a log transformation.)
# Yet, the log transformation appears slightly too strong.
us_economy |>
  autoplot(box_cox(GDP, 0))

# try lambda = 0.3
# This looks pretty good, the trend is now almost linear.
us_economy |>
  autoplot(box_cox(GDP, 0.3))

# Let's see what the guerrero method suggests as the best lambda
#   It says lambda = 0.282 - very close to 0.3! :-)
us_economy |>
  features(GDP, features = guerrero)

# let's see how it looks:
us_economy |>
  autoplot(box_cox(GDP, 0.2819714))
# More or less the same. 
# Box-Cox transformations are usually not very sensitive to the choice of lambda.



## Slaughter of Victorian "Bulls, bullocks and steers"

vic_bulls <- aus_livestock |>
  filter(State == "Victoria", Animal == "Bulls, bullocks and steers")

vic_bulls |>
  autoplot(Count)

# -   Variation in the series appears to vary slightly with the number of bulls slaughtered in Victoria.
# -   A transformation may be useful.

# Try a log transformation (equivalent to box-cox with lambda = 0.)
# -   The log transformation appears to normalise most of the variation. 
vic_bulls |>
  autoplot(log(Count))

# Let's double-check with guerrero's method.
# -   Pretty close, guerrero suggests lambda = -0.045. 
#      This is close enough to zero, so it is probably best to just use a log
#      transformation (allowing better interpretations).
vic_bulls |>
  features(Count, features = guerrero)



### Victorian Electricity Demand

vic_elec |>
  autoplot(Demand)

# -   Seasonal patterns for *time of day* hidden due to density of ink.
# -   *Day-of-week* seasonality just visible.
# -   *Time-of-year* seasonality is clear with increasing variance in winter and high skewness in summer.

# Try a log transformation
vic_elec |>
  autoplot(box_cox(Demand, 0))

# -   A log transformation makes the variance more even and reduces the skewness.
# -   Guerrero's method doesn't work here as there are several types of seasonality.



### Australian Gas production

aus_production |>
  autoplot(Gas)

# -   Variation in seasonal pattern grows proportionally to the amount of
#     gas produced in Australia. A transformation should work well here.

# Try a log transformation
# -   The log transformation appears slightly too strong, where the
#     variation in periods with smaller gas production is now larger than
#     the variation during greater gas production.
aus_production |>
  autoplot(box_cox(Gas, 0))

# Ask guerrero
# -   Guerrero's method agrees by selecting a slightly weaker
#     transformation. 
aus_production |>
  features(Gas, features = guerrero)

# Let's see how it looks.
#     Looking good! The variation is now constant across the series.
aus_production |>
  autoplot(box_cox(Gas, 0.1095))



# ------- 2. Why is a Box-Cox transformation unhelpful for the canadian_gas data?
  
canadian_gas |>
  autoplot(Volume) +
  labs(
    x = "Year", y = "Gas production (billion cubic meters)",
    title = "Monthly Canadian gas production"
  )

# -   Here the variation in the series is not proportional to the amount
#     of gas production in Canada.
# -   When small and large amounts of gas is being produced, we can
#     observe small variation in the seasonal pattern.
# -   However, between 1975 and 1990 the gas production is moderate, and
#     the variation is large.
# -   Power transformations (like the Box-Cox transformation) require the
#     variability of the series to vary proportionately to the level of
#     the series.



