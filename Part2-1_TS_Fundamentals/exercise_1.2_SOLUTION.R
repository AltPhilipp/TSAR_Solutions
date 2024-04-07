##############################################
# Time Series Analysis with R (TSAR)
# PART 2: Time Series Analysis
# Exercise 1.2: Exploring tsibble objects
# gwendolin.wilke@fhnw.ch
##############################################


#### 1. Bricks, Lynx and Close 

## Bricks

aus_production 
# The observations are quarterly.

aus_production |> autoplot(Bricks) 
# An upward trend is apparent until 1980, after which the number of clay bricks being 
# produced starts to decline. A seasonal pattern is evident in this data. 
# Some sharp drops in some quarters can also be seen.

## Lynx

interval(pelt)
# Observations are made once per year.

pelt |> autoplot(Lynx)
# Canadian lynx trappings are cyclic, as the extent of peak trappings 
# is unpredictable, and the spacing between the peaks is irregular but approximately 10 years.

## Close

gafa_stock
# Interval is daily. Looking closer at the data, we can see that the index is a Date variable. It also appears that observations occur only on trading days, creating lots of implicit missing values.

gafa_stock |>
  autoplot(Close)
# Stock prices for these technology stocks have risen for most of the series, until mid-late 2018.
# The four stocks are on different scales, so they are not directly comparable. A plot with faceting would be better.

gafa_stock |>
  ggplot(aes(x=Date, y=Close, group=Symbol)) +
  geom_line(aes(col=Symbol)) +
  facet_grid(Symbol ~ ., scales='free')
# The downturn in the second half of 2018 is now very clear, with Facebook taking a big drop (about 20%) 
# in the middle of the year.
# The stocks tend to move roughly together, as you would expect with companies in the same industry.


#### 2. gafa_stock 

gafa_stock |>
  group_by(Symbol) |>
  filter(Close == max(Close)) |>
  ungroup() |>
  select(Symbol, Date, Close)



